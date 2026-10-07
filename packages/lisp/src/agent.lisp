;;;; agent.lisp --- the native AxAgent: construction, runs and host boundaries.
;;;;
;;;; An agent is three AxGen stages (distiller, executor, responder) driven by
;;;; portable Core code. Core decides everything observable: which stage runs
;;;; when, how the actor prompt is built, what a context field is, when a
;;;; runtime session is created, which discovery or delegation call is allowed,
;;;; how a completion payload is normalized, and what the action log records.
;;;; None of that is decided here, and this file contains no stage ordering,
;;;; context policy, discovery policy or delegation policy of its own.
;;;;
;;;; What is here is the other half: the parts a portable IR cannot express.
;;;;
;;;;   * the object a caller holds -- AGENT and the AX-AGENT class, its
;;;;     stages, and the Lisp-facing accessors
;;;;   * the stage adapters, which let Core forward an AxGen or a child agent
;;;;     without knowing what either one is
;;;;   * the closures a run installs on a code runtime, and the guards that
;;;;     stop a callback outliving its run or crossing threads
;;;;   * the host callbacks an agent is configured with: memory and skill
;;;;     search, the load and use observers, audio transcription, and
;;;;     callable invocation
;;;;   * pause, resume, cancellation and cleanup of a run's runtime session
;;;;   * the context metrics collector, which folds the public context event
;;;;     stream into one run's compression summary
;;;;
;;;; Model-written code is never evaluated in this image. A run that executes
;;;; code hands it to a CODE-RUNTIME, and the only runtime shipped with Ax for
;;;; Lisp puts it in a separate process (see agent-runtime.lisp).

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; The one boundary that loads after this file
;;; ------------------------------------------------------------------
;;;
;;; Core itself is loaded before this file, so its entry points need no
;;; declaration: a name Core did not emit must be a compile error here, not
;;; something a declaration hides.
;;;
;;; The first three are MCP's, in src/mcp.lisp, which loads after this file
;;; because an agent is the thing MCP attaches to. They are only called when
;;; the options actually ask for an execution context, so an agent without
;;; MCP never reaches that layer at all.
;;;
;;; Nothing else is declared. gen.lisp and optimize.lisp both load before this
;;; file, so a program hook or an optimizer generic that is missing must be a
;;; loud error here rather than something a declaration hides. They are
;;; the mutation and streaming points the background-agent and streaming
;;; stage adapters need, and they are requested from the Gen worker. An ftype
;;; of plain FUNCTION asserts only that the name is a function, so it cannot
;;; weaken or contradict the argument list Gen finally publishes; remove each
;;; line as its generic lands.

(declaim (ftype function
                resolve-execution-context
                execution-context-initialize
                execution-context-runtime-modules))

;;; ------------------------------------------------------------------
;;; Clarification
;;; ------------------------------------------------------------------

(define-condition agent-clarification-error (ax-error)
  ((clarification :initarg :clarification :initform :null :reader agent-clarification
                  :documentation "The question the actor asked, as Core shaped it.")
   (state :initarg :state :initform :null :reader agent-clarification-state
          :documentation "The runtime state as the actor left it, so a caller
can answer and resume instead of starting again.")
   (payload :initarg :payload :initform :null :reader agent-clarification-payload
            :documentation "The whole askClarification completion payload."))
  (:documentation
   "The agent stopped to ask the caller a question instead of answering.

A normal outcome rather than a failure: handle it, get the answer, and run
the agent again with the clarification state restored."))

(defun %clarification-message (clarification payload)
  (let ((text (cond ((hash-table-p clarification)
                     (let ((question (jget clarification "question")))
                       (if (eq question :null) (jget clarification "message") question)))
                    (t clarification))))
    (if (or (null text) (eq text :null))
        (axllm/core::core-js-text payload)
        (axllm/core::core-js-text text))))

;;; ------------------------------------------------------------------
;;; The agent
;;; ------------------------------------------------------------------

(defclass ax-agent ()
  ((options :initarg :options :accessor agent-options
            :documentation "The configuration Core reads, as a JSON object.")
   (state :reader agent-core-state
          :documentation "Core's agent state record. Core owns its contents.")
   (signature :reader agent-signature)
   (distiller :reader agent-distiller)
   (executor :reader agent-executor)
   (responder :reader agent-responder)
   (llm-query :reader agent-llm-query
              :documentation "The focused sub-query the actor can await.")
   (execution-context :initform nil :reader agent-execution-context
                      :documentation "The MCP and UCP clients this agent runs
with, when it was built with any. A run may name another one.")
   (stage-mode :initform "plain" :accessor %agent-stage-mode)
   (stage-sets :initform (make-hash-table :test #'equal) :reader %agent-stage-sets
               :documentation "One stage triple per actor mode, built on first
use and kept, so moving between modes does not rebuild prompts.")
   (optimized-components :initform (object) :reader %agent-optimized-components)
   (playbook :initform nil :reader agent-playbook-handle
             :documentation "The evolving context playbook, when one is attached.")
   (playbook-config :initform :null :accessor %agent-playbook-config)
   (playbook-target :initform "actor" :accessor %agent-playbook-target)
   (playbook-apply :initform t :accessor %agent-playbook-apply)
   (playbook-bases :initform (make-hash-table :test #'eq) :reader %agent-playbook-bases
                   :documentation "Each bound stage's instruction before any
playbook was written into it, so rebinding a stage never composes twice."))
  (:documentation
   "An Ax agent: a signature, three stages and Core's state record.

Build one with AGENT. Run it with AGENT-FORWARD, or with FORWARD, which
treats an agent and an AxGen alike so an agent can be a stage of another
agent."))

(defmethod print-object ((agent ax-agent) stream)
  (print-unreadable-object (agent stream :type t)
    (format stream "~a"
            (or (ignore-errors (signature-string (agent-signature agent))) "unbuilt"))))

(defun %option (options &rest keys)
  "The first of KEYS present in OPTIONS, or :NULL.

Agent options arrive in either spelling -- Ax's published camelCase or
Core's snake_case -- so every read tries both rather than picking one."
  (when (hash-table-p options)
    (dolist (key keys)
      (let ((value (jget options key)))
        (unless (eq value :null)
          (return-from %option value)))))
  :null)

(defparameter +execution-context-option-keys+
  '("executionContext" "mcpExecutionContext" "inheritedExecutionContext" "mcp" "ucp")
  "The option keys that ask for live protocol clients.

Checked before anything in src/mcp.lisp is called, so an agent configured
without MCP or UCP never reaches that layer. MCP owns what each key means;
this only decides whether to ask.")

(defun %execution-context-requested-p (options)
  (and (hash-table-p options)
       (some (lambda (key) (nth-value 1 (gethash key options)))
             +execution-context-option-keys+)))

(defun %resolve-run-context (options &optional parent)
  "The execution context OPTIONS asks for, PARENT's, or NIL."
  (when (or (%execution-context-requested-p options) parent)
    (resolve-execution-context (if (hash-table-p options) options (object)) parent)))

(defun %attach-execution-context (options)
  "Initialize the context OPTIONS names and add its runtime modules.

Returns (values options context). The clients are initialized once, here,
rather than on the first call that needs one, so a server that cannot be
reached fails while the agent is being built instead of mid-run."
  (let ((context (%resolve-run-context options)))
    (if (null context)
        (values options nil)
        (progn
          (execution-context-initialize context)
          (let ((updated (axllm/core::agent-append-runtime-modules
                          options
                          (coerce (execution-context-runtime-modules context) 'vector))))
            (%set-key updated "executionContext" context)
            (values updated context))))))

(defun %state-get (agent key &optional (fallback :null))
  (axllm/core::core-get (agent-core-state agent) key fallback))

(defun %stage-instruction (record key)
  (let ((text (axllm/core::core-get record key "")))
    (if (eq text :null) "" (axllm/core::core-js-text text))))

(defun %make-stage (signature id instruction max-retries &optional actor-p)
  "One AxGen stage for SIGNATURE, named ID and carrying INSTRUCTION.

The ids are the ones every port uses, because an optimizable component is
addressed by them: a component map written against another port's agent
has to apply here unchanged. The rest of a stage's prompt shaping is
Core's and travels with each call rather than being baked in, so the same
stage rebuilt in another actor mode is still the same program."
  (let ((options (object)))
    ;; Actor system prompts describe the stable signature, even before a
    ;; skill, memory or guidance value is loaded. Core still omits absent
    ;; values from the user turn. This is the same AxGen option TS uses.
    (when actor-p
      (%set-key options "includeOptionalInputFieldsInSystemPrompt" 'yason:true))
    (if (integerp max-retries)
        (ax signature :id id :instruction instruction :max-retries max-retries :options options)
        (ax signature :id id :instruction instruction :options options))))

(defun %actor-stage-retries (agent)
  "The validation budget an actor stage gets.

One correction turn, as every other port gives the distiller and the
executor, unless the caller set a budget. The default matters: an actor
stage that silently took AxGen's larger budget would spend provider calls
the agent loop never accounted for."
  (let ((value (%option (agent-options agent) "validation_retries" "validationRetries")))
    (if (integerp value) value 1)))

(defun %responder-stage-retries (agent)
  "The validation budget the responder gets: the caller's, or AxGen's own.

The responder answers the caller, so it keeps the generator's ordinary
retry budget rather than the actor's single turn."
  (let ((value (%option (agent-options agent) "validation_retries" "validationRetries")))
    (and (integerp value) value)))

(defun %build-stage-set (agent record)
  "The distiller, executor and responder for one actor mode.

RECORD is Core's stage record for that mode: the three signatures and the
three instructions."
  (let* ((actor-retries (%actor-stage-retries agent))
         (responder-retries (%responder-stage-retries agent))
         (responder (%make-stage (axllm/core::core-get record "responder_signature"
                                                       (%state-get agent "responder_signature"))
                                 "task.root.responder"
                                 (%stage-instruction record "responder_description")
                                 responder-retries)))
    ;; Core publishes the citation check but calls it from nowhere: it is an
    ;; assertion, so the host has to install it. Without it a model could cite
    ;; evidence it never saw and the answer would be returned as if the
    ;; citation had been checked, which is the one thing citations exist to
    ;; prevent. Core answers "nothing to check" until a run collects the valid
    ;; keys, so installing it unconditionally costs a disabled agent nothing.
    (add-assert responder
                (lambda (output)
                  (let ((message (axllm/core::agent-citation-assert
                                  (agent-core-state agent) output)))
                    (if (or (eq message :null) (null message)) t message))))
    (list (%make-stage (axllm/core::core-get record "distiller_signature")
                       "ctx.root.actor"
                       (%stage-instruction record "distiller_description") actor-retries t)
          (%make-stage (axllm/core::core-get record "executor_signature")
                       "task.root.actor"
                       (%stage-instruction record "executor_description") actor-retries t)
          responder)))

(defun %rebuild-from-signature (agent new-signature)
  "Rebuild AGENT's stages for NEW-SIGNATURE from a fresh Core state.

Used by the constructor and by anything that changes the agent's shape: a
new signature, or a new child agent in its namespace."
  (setf (slot-value agent 'state)
        (axllm/core::agent-factory new-signature (agent-options agent)))
  (setf (slot-value agent 'signature) (%state-get agent "signature"))
  (let ((record (object "distiller_signature" (%state-get agent "distiller_signature")
                        "executor_signature" (%state-get agent "executor_signature")
                        "responder_signature" (%state-get agent "responder_signature")
                        "distiller_description" (%state-get agent "distiller_description" "")
                        "executor_description" (%state-get agent "executor_description" "")
                        "responder_description" (%state-get agent "responder_description" ""))))
    (destructuring-bind (distiller executor responder) (%build-stage-set agent record)
      (setf (slot-value agent 'distiller) distiller
            (slot-value agent 'executor) executor
            (slot-value agent 'responder) responder)))
  (setf (%agent-stage-mode agent)
        (if (axllm/core::core-true-p (%state-get agent "runtime_enabled" 'yason:false))
            "runtime"
            "plain"))
  (clrhash (%agent-stage-sets agent))
  (setf (gethash (%agent-stage-mode agent) (%agent-stage-sets agent))
        (list (agent-distiller agent) (agent-executor agent) (agent-responder agent)))
  (setf (slot-value agent 'llm-query)
        (%make-stage (%state-get agent "llm_query_signature"
                                 "task:string, context:json -> answer:string")
                     "rlm.llmquery"
                     (axllm/core::core-js-text (%state-get agent "llm_query_description" ""))
                     1))
  ;; The stages are new objects, so a playbook already attached has to be
  ;; written into the new target rather than left pointing at a dead stage.
  (%rebind-playbook agent)
  agent)

(defun %use-stage-mode (agent options)
  "Point AGENT at the stage set this run's actor mode needs.

Core resolves the mode from the run's runtime, the constructor's, or
neither. A mode's stages are built once and kept; returning to a mode
re-applies the instructions Core holds now, and any optimized components
applied since."
  (let* ((record (axllm/core::agent-use-stage-mode (agent-core-state agent) options))
         (mode (axllm/core::core-js-text (axllm/core::core-get record "mode" "plain"))))
    (unless (equal mode (%agent-stage-mode agent))
      (let ((stages (gethash mode (%agent-stage-sets agent))))
        (if (null stages)
            (setf stages (%build-stage-set agent record)
                  (gethash mode (%agent-stage-sets agent)) stages)
            (progn
              (program-set-instruction (first stages)
                                       (%stage-instruction record "distiller_description"))
              (program-set-instruction (second stages)
                                       (%stage-instruction record "executor_description"))
              (program-set-instruction (third stages)
                                       (%stage-instruction record "responder_description"))))
        (when (plusp (hash-table-count (%agent-optimized-components agent)))
          (dolist (stage stages)
            (program-apply-optimized-components stage (%agent-optimized-components agent))))
        (setf (slot-value agent 'distiller) (first stages)
              (slot-value agent 'executor) (second stages)
              (slot-value agent 'responder) (third stages)
              (%agent-stage-mode agent) mode)
        (%rebind-playbook agent)))
    mode))

(defun agent (signature &key options)
  "Create an agent for SIGNATURE.

SIGNATURE is signature text or a parsed signature. OPTIONS is a JSON
object of Ax agent configuration -- context fields, actor mode, runtime,
functions, skills, memories, discovery, delegation, citations, context
policy -- read by Core, which decides what each one means.

  (agent \"question:string -> answer:string\")

  (agent \"question:string -> answer:string\"
         :options (object \"runtime\" (object \"language\" \"JavaScript\")))"
  (let ((options (cond ((null options) (object))
                       ((hash-table-p options) options)
                       (t (error 'ax-error
                                 :message (format nil "agent: :options must be a JSON object, got ~s"
                                                  options))))))
    (multiple-value-bind (options context) (%attach-execution-context options)
      (let ((agent (make-instance 'ax-agent :options options)))
        (setf (slot-value agent 'execution-context) context
              (%agent-playbook-config agent) (jget options "playbook"))
        (%rebuild-from-signature agent signature)
        ;; A configured playbook attaches after the stages exist, because it
        ;; writes itself into one of them.
        (unless (or (eq (%agent-playbook-config agent) :null)
                    (json-false-p (%agent-playbook-config agent)))
          (%attach-configured-playbook agent))
        agent))))

;;; ------------------------------------------------------------------
;;; Run-scoped host callables
;;; ------------------------------------------------------------------
;;;
;;; When a run has an executable in-process runtime, the actor's code can call
;;; back into this image: each of Core's qualified callable names, plus the
;;; built-in llmQuery. Both are closures over this run's state, client and
;;; options, so they must stop working the moment the run ends -- a runtime
;;; that outlives the run, through a reused session or a worker answering
;;; late, could otherwise reach a client the caller has moved on from.
;;;
;;; Two guards, the same ones every other port applies: a callback belongs to
;;; one run, and it must arrive on the thread that owns that run.

(defstruct (agent-invocation (:conc-name %invocation-))
  state sub-gen client options (owner (%current-run-thread)) (active t))

(defun %check-invocation (invocation)
  (unless (%invocation-active invocation)
    (error 'ax-error :message "Agent invocation belongs to a closed run"))
  (unless (eql (%invocation-owner invocation) (%current-run-thread))
    (error 'ax-error :message "Agent runtime callbacks must execute on the owning run thread"))
  invocation)

(defun %invocation-callable (invocation qualified)
  "A one-argument function the runtime can expose under QUALIFIED."
  (lambda (arguments)
    (%check-invocation invocation)
    (axllm/core::agent-runtime-invoke-callable (%invocation-state invocation) qualified arguments)))

(defun %invocation-llm-query (invocation)
  "The built-in llmQuery primitive, bound to this run's client."
  (lambda (params)
    (%check-invocation invocation)
    (axllm/core::agent-run-llm-query (%invocation-sub-gen invocation)
                                     (%invocation-client invocation)
                                     params
                                     (%invocation-options invocation))))

(defun %close-invocation (invocation)
  "Retire INVOCATION: its callables refuse later calls and drop the client."
  (when invocation
    (setf (%invocation-active invocation) nil
          (%invocation-client invocation) nil
          (%invocation-sub-gen invocation) nil
          (%invocation-options invocation) nil))
  nil)

(defun %install-run-callables (agent runtime client options)
  "Install this run's host callables on RUNTIME and return the invocation.

NIL when there is nothing to install: a runtime behind a process boundary
owns its own callables, and Core describes them in the session globals
instead."
  (when (and runtime (runtime-supports-callables-p runtime))
    (let ((invocation (make-agent-invocation :state (agent-core-state agent)
                                             :sub-gen (agent-llm-query agent)
                                             :client client
                                             :options options)))
      (let ((names (axllm/core::agent-runtime-callable-names (agent-core-state agent))))
        (loop for qualified across (if (%array-p names) names (%new-array))
              do (let ((name (axllm/core::core-js-text qualified)))
                   (runtime-register-callable runtime name
                                              (%invocation-callable invocation name)))))
      (runtime-register-callable runtime "llmQuery" (%invocation-llm-query invocation))
      invocation)))

;;; ------------------------------------------------------------------
;;; Run control
;;; ------------------------------------------------------------------
;;;
;;; A run control hears the run's lifecycle at its own path. Whoever owns the
;;; control object owns its shape, so this only announces, through the Core
;;; host bridge; a control that cannot take an event hears nothing. Run
;;; telemetry must never be able to fail a run.

(defun %emit-run-event (control kind path &optional extra)
  (when (and control (not (eq control :null)))
    (let ((event (object "type" kind "path" path)))
      (when (hash-table-p extra) (axllm/core::core-map-update event extra))
      (handler-case (axllm/core::core-host-call control "emit" (vector event))
        (error () nil))))
  nil)

(defun %run-path (options)
  (let ((path (%option options "execution_path" "executionPath")))
    (if (eq path :null) "root" (axllm/core::core-js-text path))))

;;; ------------------------------------------------------------------
;;; Runs
;;; ------------------------------------------------------------------

(defvar *agent-control-scopes* nil)

(defun %agent-stage-options (options)
  "Give each native stage a persistent cursor over this run's control."
  (let ((control (jget options "control")))
    (if (and *agent-control-scopes* (typep control 'run-control))
        (let* ((path (%run-path options))
               (key (cons control path))
               (scope (or (gethash key *agent-control-scopes*)
                          (setf (gethash key *agent-control-scopes*)
                                (make-instance 'agent-control-scope :control control :path path))))
               (copy (axllm/core::core-map-merge options (object))))
          (%set-key copy "control" scope)
          copy)
        options)))

(defun %run-agent (agent client values options sink)
  "One run of AGENT, streaming to SINK when given.

Everything observable belongs to Core. This installs the run's host
callables, announces the lifecycle, hands control to Core and -- however
the run ends -- retires the callables so none of them survives it."
  (let* ((*agent-control-scopes* (make-hash-table :test #'equal))
         (options (if (hash-table-p options)
                      (axllm/core::core-map-merge options (object))
                      (object)))
         (values (if (hash-table-p values) values (object)))
         (control (jget options "control"))
         (path (%run-path options)))
    (%use-stage-mode agent options)
    (%apply-run-context agent options)
    (let* ((configured (jget (agent-options agent) "runtime"))
           (per-run (jget options "runtime"))
           (runtime (let ((chosen (if (eq per-run :null) configured per-run)))
                      (if (eq chosen :null) nil chosen)))
           (invocation (%install-run-callables agent runtime client options))
           (settled nil))
      ;; AxGen wraps each stage's client at its request boundary. Wrapping the
      ;; root as well would consume stage updates before those stages see them.
      (%emit-run-event control "started" path)
      (unwind-protect
           (handler-case
             (let ((output (if sink
                               (axllm/core::agent-streaming-forward
                                (agent-core-state agent)
                                (agent-distiller agent) (agent-executor agent)
                                (agent-responder agent)
                                client values options sink)
                               (axllm/core::agent-forward
                                (agent-core-state agent)
                                (agent-distiller agent) (agent-executor agent)
                                (agent-responder agent)
                                client values options))))
               (%notify-citations agent)
               ;; Core returns the assembled responder output after a stream
               ;; completes. Failed or interrupted streams never reach here.
               (%learn-playbook-failures agent output)
               (setf settled t)
               (%emit-run-event control "completed" path)
               output)
             (error (condition)
               (setf settled t)
               (%emit-run-event control "failed" path
                                (object "error" (princ-to-string condition)))
               (error condition)))
        (unless settled
          ;; A Lisp nonlocal sink exit bypasses Core's exception handler.
          ;; Release the same run-owned references and session it releases
          ;; on an error, so this agent can be used again after early stop.
          (%set-key (agent-core-state agent) "forward_active" 'yason:false)
          (%set-key (agent-core-state agent) "active_client" :null)
          (%set-key (agent-core-state agent) "active_forward_options" :null)
          (axllm/core::agent-runtime-close-session
           (agent-core-state agent) (%state-get agent "runtime_session"))
          (%emit-run-event control "aborted" path))
        (%close-invocation invocation)
        ;; A worker holds the host callables this run registered. Retiring them
        ;; with the invocation means a later host_call naming one is refused
        ;; rather than served against a closure whose run is over.
        (axllm::runtime-retire-callables runtime)))))

(defun %apply-run-context (agent options)
  "Give this run the protocol clients it asks for, in place in OPTIONS.

A run may name its own execution context, which wins over the one the
agent was built with. Core decides what that does to the actor prompt --
which modules appear, and under which names -- so this initializes the
clients, hands Core the modules, and then re-reads the stage instructions
Core composed, because the runtime actor prompt names the modules."
  (let ((call-context (%resolve-run-context options (agent-execution-context agent))))
    (when (or call-context
              (axllm/core::core-true-p (%state-get agent "mcp_run_context_active" 'yason:false)))
      (let ((modules (if call-context
                         (progn (execution-context-initialize call-context)
                                (coerce (execution-context-runtime-modules call-context) 'vector))
                         (%new-array))))
        (when call-context
          (%set-key options "executionContext" call-context))
        (axllm/core::core-map-update
         options
         (axllm/core::agent-apply-run-context (agent-core-state agent)
                                              (agent-options agent)
                                              options
                                              modules))
        (when (axllm/core::core-true-p (%state-get agent "runtime_enabled" 'yason:false))
          (program-set-instruction (agent-distiller agent)
                                   (%stage-instruction (agent-core-state agent)
                                                       "distiller_description"))
          (program-set-instruction (agent-executor agent)
                                   (%stage-instruction (agent-core-state agent)
                                                       "executor_description"))
          (program-set-instruction (agent-responder agent)
                                   (%stage-instruction (agent-core-state agent)
                                                       "responder_description"))
          ;; Context composition replaced the stage instructions, so the
          ;; remembered playbook bases must follow those new instructions.
          (dolist (stage (list (agent-executor agent) (agent-responder agent)))
            (remhash stage (%agent-playbook-bases agent)))
          (%rebind-playbook agent)))))
  options)

(defun %notify-citations (agent)
  "Hand the run's citations to the configured callback, if there is one."
  (let* ((citations (jget (agent-options agent) "citations"))
         (callback (let ((value (%option citations "onCitations" "on_citations")))
                     (and (functionp value) value))))
    (when callback
      (handler-case (funcall callback (%state-get agent "last_citations" (%new-array)))
        (error () nil))))
  nil)

(defun agent-forward (agent client values &key options)
  "Run AGENT against CLIENT with VALUES and return its output object.

VALUES is a JSON object of the signature's input values. OPTIONS is a JSON
object of per-run options: a runtime, a run control, an execution path,
use observers, and anything else Core reads for one run.

Signals AGENT-CLARIFICATION-ERROR when the agent asks the caller a
question instead of answering."
  (check-type agent ax-agent)
  (%run-agent agent client values options nil))

(defun agent-streaming-forward (agent client values sink &key options)
  "Run AGENT and hand each responder delta to SINK as it arrives.

SINK takes one JSON envelope in Ax's delta shape -- \"version\", \"index\"
and \"delta\". Merge each index's deltas (strings and arrays append,
anything else replaces) and start over when the version changes. The
distiller and the executor run first, without streaming, as in every other
port.

Unlike the Python port this pushes to SINK on the calling thread instead
of returning a generator: a caller who wants to pull can run the agent in
a thread of its own, rather than having this file pick a threading model.
Returns the run's output object, as AGENT-FORWARD does."
  (check-type agent ax-agent)
  (unless (functionp sink)
    (error 'ax-error :message "agent-streaming-forward: sink must be a function of one argument"))
  (%run-agent agent client values options sink))

;;; ------------------------------------------------------------------
;;; Pause, resume and cleanup of a runtime session
;;; ------------------------------------------------------------------

(defun agent-test (agent runtime code &key context-values options)
  "Run CODE once in a fresh session of RUNTIME and return its step result.

The way to try actor code without a model: Core builds the globals from
CONTEXT-VALUES, runs one step, normalizes the result and closes the
session."
  (check-type agent ax-agent)
  (axllm/core::agent-runtime-test (agent-core-state agent) runtime
                                  (axllm/core::core-js-text code)
                                  (or context-values (object))
                                  (or options (object))))

(defun agent-execute-actor-step (agent runtime code &key values options)
  "Run one actor step of CODE in AGENT's session, creating it if needed.

The session persists between calls, so a later step sees what an earlier
one bound."
  (check-type agent ax-agent)
  (axllm/core::agent-runtime-build-globals (agent-core-state agent) (or values (object)))
  (axllm/core::agent-runtime-execute-step (agent-core-state agent) runtime
                                          (%state-get agent "runtime_session")
                                          (axllm/core::core-js-text code)
                                          (or options (object))))

(defun agent-inspect-runtime (agent &key options)
  "A readable view of AGENT's runtime session globals."
  (check-type agent ax-agent)
  (axllm/core::agent-runtime-inspect-state (agent-core-state agent)
                                           (%state-get agent "runtime_session")
                                           (or options (object))))

(defun agent-export-session-state (agent &key options)
  "AGENT's runtime session as a snapshot: the pause half of pause and resume."
  (check-type agent ax-agent)
  (axllm/core::agent-runtime-export-session-state (agent-core-state agent)
                                                  (%state-get agent "runtime_session")
                                                  (or options (object))))

(defun agent-restore-session-state (agent snapshot &key options)
  "Put SNAPSHOT back into AGENT's runtime session: the resume half."
  (check-type agent ax-agent)
  (axllm/core::agent-runtime-restore-session-state (agent-core-state agent)
                                                   (%state-get agent "runtime_session")
                                                   (or snapshot (object))
                                                   (or options (object))))

(defun agent-close-runtime-session (agent)
  "Close AGENT's runtime session and release the worker holding it."
  (check-type agent ax-agent)
  (axllm/core::agent-runtime-close-session (agent-core-state agent)
                                           (%state-get agent "runtime_session")))

;;; ------------------------------------------------------------------
;;; State, observability and policy
;;; ------------------------------------------------------------------

(defun agent-state (agent)
  "AGENT's minimal portable state, for a round trip through another port."
  (check-type agent ax-agent)
  (axllm/core::agent-get-state (agent-core-state agent)))

(defun agent-set-state (agent state)
  "Restore AGENT from a state produced by AGENT-STATE."
  (check-type agent ax-agent)
  (axllm/core::agent-set-state (agent-core-state agent) (or state (object))))

(defun agent-export-runtime-state (agent)
  "Everything AGENT holds about its runtime: globals, action log, provenance."
  (check-type agent ax-agent)
  (axllm/core::agent-export-runtime-state (agent-core-state agent)))

(defun agent-restore-runtime-state (agent snapshot)
  "Restore AGENT's runtime state from a snapshot."
  (check-type agent ax-agent)
  (axllm/core::agent-restore-runtime-state (agent-core-state agent) (or snapshot (object))))

(defun %refresh-observability (agent)
  (axllm/core::merge-agent-chat-log (agent-core-state agent)
                                    (agent-distiller agent) (agent-executor agent)
                                    (agent-responder agent))
  (axllm/core::merge-agent-usage (agent-core-state agent)
                                 (agent-distiller agent) (agent-executor agent)
                                 (agent-responder agent))
  agent)

(defun agent-chat-log (agent)
  "Every request and response of AGENT's runs so far, stage by stage."
  (check-type agent ax-agent)
  (%refresh-observability agent)
  (%state-get agent "chat_log" (%new-array)))

(defun agent-action-log (agent)
  "What the actor did: each step, its result and its effect."
  (check-type agent ax-agent)
  (%state-get agent "action_log" (%new-array)))

(defun agent-trace (agent)
  "AGENT's trace: the events of its runs, in order, for replay."
  (check-type agent ax-agent)
  (%refresh-observability agent)
  (axllm/core::agent-export-trace (agent-core-state agent)))

(defun agent-replay-trace (agent trace &key fixtures)
  "Replay TRACE against FIXTURES and report where it diverges."
  (check-type agent ax-agent)
  (axllm/core::agent-replay-trace (or trace (object)) (or fixtures (object))))

(defun agent-usage (agent)
  "Token usage across AGENT's stages."
  (check-type agent ax-agent)
  (%refresh-observability agent)
  (%state-get agent "usage" (object)))

(defun agent-runtime-contract (agent)
  "The runtime contract AGENT's actor prompt promises: language, primitives."
  (check-type agent ax-agent)
  (%state-get agent "runtime_contract" (object)))

(defun agent-policy (agent)
  "The policy decisions in force for AGENT."
  (check-type agent ax-agent)
  (%state-get agent "policy" (object)))

(defun agent-policy-registry (agent)
  "Every policy AGENT could apply, and whether it is enabled."
  (check-type agent ax-agent)
  (%state-get agent "policy_registry" (object)))

(defun agent-callable-inventory (agent)
  "The callables AGENT exposes to its actor, grouped by namespace."
  (check-type agent ax-agent)
  (%state-get agent "callable_inventory" (%new-array)))

(defun agent-discovery-catalog (agent)
  "The compact catalog the actor prompt carries, before any discover call."
  (check-type agent ax-agent)
  (%state-get agent "discovery_catalog" (%new-array)))

(defun agent-discover (agent request)
  "Load the full docs REQUEST names into AGENT's next actor turn."
  (check-type agent ax-agent)
  (axllm/core::agent-discover (agent-core-state agent) (or request (object))))

(defun agent-recall (agent request)
  "Load the memories REQUEST names into AGENT's next actor turn."
  (check-type agent ax-agent)
  (axllm/core::agent-recall (agent-core-state agent) (or request (%new-array))))

(defun agent-used (agent id &key (reason "") (stage "executor"))
  "Record that AGENT used the loaded memory, skill or module ID."
  (check-type agent ax-agent)
  (axllm/core::agent-used (agent-core-state agent)
                          (object "id" id "reason" reason "stage" stage)
                          stage))

(defun agent-invoke-callable (agent qualified-name &key args options)
  "Call one of AGENT's callables directly, as its actor would."
  (check-type agent ax-agent)
  (axllm/core::agent-execute-callable (agent-core-state agent)
                                      (object "qualified_name" qualified-name
                                              "args" (or args (object)))
                                      (or options (object))))

;;; ------------------------------------------------------------------
;;; Shape and instructions
;;; ------------------------------------------------------------------

(defun agent-set-signature (agent signature)
  "Give AGENT a new signature and rebuild its stages."
  (check-type agent ax-agent)
  (%rebuild-from-signature agent signature))

(defun agent-add-child (agent namespace name child)
  "Expose CHILD to AGENT's actor as NAMESPACE.NAME."
  (check-type agent ax-agent)
  (check-type child ax-agent)
  (setf (agent-options agent)
        (axllm/core::agent-register-child (agent-options agent) namespace name
                                          child (agent-signature child)))
  (%rebuild-from-signature agent (agent-signature agent)))

(defun agent-instruction (agent)
  "AGENT's standing instruction."
  (check-type agent ax-agent)
  (let ((text (%state-get agent "stage_instruction" "")))
    (if (eq text :null) "" (axllm/core::core-js-text text))))

(defun %reinstruct-actor (agent composed)
  "Give AGENT's actor COMPOSED as its instruction, keeping any playbook.

The playbook is composed onto the instruction a stage had when it was
first bound, so replacing that instruction makes the remembered base
stale: writing COMPOSED straight onto the stage would drop the playbook
block, and a configured playbook would silently stop reaching the model
the moment a caller changed an instruction. Forgetting the base and
binding again recomposes the playbook onto the new text."
  (program-set-instruction (agent-executor agent) composed)
  (remhash (agent-executor agent) (%agent-playbook-bases agent))
  (%rebind-playbook agent)
  agent)

(defun agent-set-instruction (agent instruction)
  "Replace AGENT's standing instruction."
  (check-type agent ax-agent)
  (let ((composed (axllm/core::agent-set-instruction (agent-core-state agent)
                                                     (axllm/core::core-js-text instruction))))
    (%set-key (agent-options agent) "instruction" (%state-get agent "stage_instruction" ""))
    (%reinstruct-actor agent composed)))

(defun agent-add-actor-instruction (agent addendum)
  "Add ADDENDUM to what AGENT's actor is told, keeping what came before."
  (check-type agent ax-agent)
  (let ((composed (axllm/core::agent-add-actor-instruction (agent-core-state agent)
                                                           (axllm/core::core-js-text addendum))))
    (%set-key (agent-options agent) "instructionAddenda"
              (%state-get agent "instruction_addenda" (%new-array)))
    (%reinstruct-actor agent composed)))

;;; ------------------------------------------------------------------
;;; Optimizer-facing surface
;;; ------------------------------------------------------------------

(defun agent-optimizer-metadata (agent)
  "What an optimizer needs to know about AGENT without running it."
  (check-type agent ax-agent)
  (axllm/core::agent-optimizer-metadata (agent-core-state agent)))

(defun agent-optimizable-components (agent)
  "AGENT's optimizable components, including its stages'."
  (check-type agent ax-agent)
  (let ((children (%new-array)))
    (dolist (stage (list (agent-distiller agent) (agent-executor agent) (agent-responder agent)))
      (let ((components (program-optimizable-components stage)))
        (loop for component across (if (%array-p components) components (%new-array))
              do (vector-push-extend component children))))
    (axllm/core::agent-get-optimizable-components (agent-core-state agent) children)))

;;; ------------------------------------------------------------------
;;; The playbook
;;; ------------------------------------------------------------------
;;;
;;; A playbook is an evolving set of rules the agent writes for itself from
;;; its own failures. The optimizer owns the machinery -- the ACE driver, its
;;; Reflector and Curator, and how a rule is curated -- and this owns the
;;; agent half: which stage the rendered playbook is written into, keeping
;;; that writing idempotent across stage rebuilds, and what one finished run
;;; teaches it.
;;;
;;; The handle holds the agent, not a stage, because a playbook is judged on
;;; the answer a run really produced; the stage is recorded separately as the
;;; component the rendered text is attached to. Those are two decisions and
;;; the optimizer keeps them apart, so this does too.

(defparameter +playbook-stage-components+
  '(("actor" . "task.root.actor::instruction")
    ("responder" . "task.root.responder::instruction"))
  "Which component a playbook target names.

The ids are the stage ids every port uses, so a playbook bound here and one
bound in another port attach to the same component.")

(defun %playbook-stage (agent)
  "The stage a playbook writes into: the responder, or the actor."
  (if (equal (%agent-playbook-target agent) "responder")
      (agent-responder agent)
      (agent-executor agent)))

(defun %playbook-compose-instruction (base rendered)
  "BASE and RENDERED as one instruction, blank parts dropped.

Two newlines between them, as every port joins them, so a rendered
playbook reads as its own block rather than running into the task text."
  (let ((parts (remove-if (lambda (part) (zerop (length (axllm/core::core-string-trim part))))
                          (list (axllm/core::core-js-text (if (eq base :null) "" base))
                                (axllm/core::core-js-text (if (eq rendered :null) "" rendered))))))
    (format nil "~{~a~^~%~%~}" parts)))

(defun %bind-playbook-stage (agent handle)
  "Point HANDLE's rendered text at the stage AGENT's target names.

A stage keeps the instruction it had when it was first bound, so binding
it again -- a kept stage set coming back into use -- composes the playbook
onto the original text rather than onto itself."
  (let ((stage (%playbook-stage agent)))
    (when (%agent-playbook-apply agent)
      (multiple-value-bind (base present) (gethash stage (%agent-playbook-bases agent))
        (unless present
          (setf base (axllm/core::core-js-text (or (ignore-errors (generator-instruction stage)) ""))
                (gethash stage (%agent-playbook-bases agent)) base))
        (program-set-instruction stage (%playbook-compose-instruction base (ace-render handle)))))
    stage))

(defun %rebind-playbook (agent)
  "Write the playbook into the stage again after the stages were rebuilt."
  (let ((handle (agent-playbook-handle agent)))
    (when handle (%bind-playbook-stage agent handle)))
  agent)

(defun %playbook-seed-playbook (seed)
  "SEED as a playbook structure, whether it is one or a snapshot holding one."
  (let ((inner (and (hash-table-p seed) (jget seed "playbook"))))
    (if (and inner (not (eq inner :null))) inner seed)))

(defun %load-playbook-seed (handle seed)
  "Install SEED as HANDLE's starting playbook.

The optimizer publishes PLAYBOOK-LOAD for this. The fallback does the same
thing by hand -- set the initial playbook, then reset onto it -- for a tree
where the driver has not grown that function yet; it is checked at run time
rather than read time so this file loads either way. Delete the fallback
once every tree has the loader."
  (let ((loader (find-symbol "PLAYBOOK-LOAD" "AXLLM")))
    (if (and loader (fboundp loader))
        (funcall loader handle (%playbook-seed-playbook seed))
        (progn (setf (ace-initial-playbook handle) (%playbook-seed-playbook seed))
               (ace-reset handle))))
  handle)

(defun agent-playbook (agent &key options client teacher)
  "AGENT's playbook, attaching one on first use.

OPTIONS is the playbook configuration: \"target\" is \"actor\" or
\"responder\", \"apply\" false keeps the playbook out of the prompt, and
\"studentAI\" or CLIENT is the client its Reflector and Curator run
against. Calling this again without options returns the attached
playbook; calling it again with options is an error, because two
configurations for one playbook cannot both be in force."
  (check-type agent ax-agent)
  (let ((existing (agent-playbook-handle agent)))
    (when existing
      (when (and (hash-table-p options) (plusp (hash-table-count options)))
        (error 'ax-error
               :message "agent-playbook: this agent already has a playbook; call it without options to use it."))
      (return-from agent-playbook existing)))
  (let* ((options (if (hash-table-p options)
                      (axllm/core::core-map-merge options (object))
                      (object)))
         (target (let ((given (jget options "target" "actor")))
                   (if (eq given :null) "actor" (axllm/core::core-js-text given))))
         (student (or client
                      (let ((value (%option options "studentAI" "student_ai" "student" "client" "ai")))
                        (unless (eq value :null) value))
                      (let ((value (%option (agent-options agent) "ai" "client")))
                        (unless (eq value :null) value)))))
    (unless student
      (error 'ax-error
             :message "agent-playbook: a student client is required when the agent has no default one."))
    (unless (assoc target +playbook-stage-components+ :test #'string=)
      (error 'ax-error
             :message (format nil "agent-playbook: target must be \"actor\" or \"responder\", got ~s"
                              target)))
    (setf (%agent-playbook-target agent) target
          (%agent-playbook-apply agent) (not (json-false-p (jget options "apply" 'yason:true))))
    ;; The target is recorded in the options rather than passed as its own
    ;; argument. That is where the optimizer's driver keeps it and where
    ;; PLAYBOOK-TARGET reads it from, so the same call works whether or not
    ;; the driver has grown a :target keyword yet.
    (%set-key options "target"
              (cdr (assoc target +playbook-stage-components+ :test #'string=)))
    (let ((handle (make-playbook
                   :program agent
                   :student student
                   :teacher teacher
                   :options options)))
      (setf (slot-value agent 'playbook) handle)
      (%bind-playbook-stage agent handle)
      handle)))

(defun %attach-configured-playbook (agent)
  "Attach the playbook AGENT's own configuration asks for.

A numeric \"seed\" belongs to the optimizer, so only a snapshot or a bare
playbook is loaded here; Core decides which one the configuration carries."
  (let ((config (%agent-playbook-config agent)))
    (when (and (hash-table-p config))
      (let ((options (axllm/core::core-map-merge config (object))))
        (unless (nth-value 1 (gethash "maxReflectorRounds" options))
          (%set-key options "maxReflectorRounds" 1))
        (let ((seed (axllm/core::agent-playbook-config-seed options))
              (handle (agent-playbook agent :options options)))
          (unless (eq seed :null)
            (%load-playbook-seed handle seed)
            ;; Attaching bound an empty playbook to the stage, so the stage has
            ;; to be written again now that the seed's rules are in it.
            (%bind-playbook-stage agent handle))
          handle)))))

(defun %playbook-failure-feedback (signals)
  "The feedback text one run's failure signals become."
  (with-output-to-string (out)
    (write-line "Agent run failures to avoid:" out)
    (loop for signal across signals
          do (format out "- [~a] ~a: ~a~%"
                     (axllm/core::core-js-text (jget signal "kind" ""))
                     (axllm/core::core-js-text (jget signal "signature" ""))
                     (axllm/core::core-js-text (jget signal "detail" ""))))
    (write-string "Curate ONE bounded avoidance rule into failures_to_avoid." out)))

(defun %learn-playbook-failures (agent output)
  "Teach AGENT's playbook what this run got wrong.

Signals already covered by a rule are dropped, and the report is capped
after the dedupe rather than before it, so a fresh signature cannot be
starved by twelve already-covered ones. An evaluated run teaches nothing,
and a failure to learn never fails the run that produced it."
  (let ((handle (agent-playbook-handle agent))
        (config (%agent-playbook-config agent)))
    (when (and handle (hash-table-p config)
               (not (axllm/core::core-true-p (%state-get agent "playbook_learning_paused" 'yason:false))))
      (let ((learn (jget config "learn" 'yason:true)))
        (unless (json-false-p learn)
          (handler-case
              (let* ((learn-config (if (hash-table-p learn) learn (object)))
                     (minimum (let ((value (%option learn-config "minSignals" "min_signals")))
                                (if (integerp value) value 1)))
                     (raw (%state-get agent "failure_signals" (%new-array)))
                     (signals (if (%array-p raw) raw (%new-array))))
                (when (>= (length signals) minimum)
                  (let ((covered (axllm/core::agent-collect-covered-failure-signatures
                                  (ace-artifact handle)))
                        (kept (%new-array)))
                    (loop for signal across signals
                          do (let ((signature (axllm/core::core-js-text (jget signal "signature" ""))))
                               (when (or (json-false-p (jget learn-config "dedupe" 'yason:true))
                                         (not (axllm/core::core-true-p
                                               (axllm/core::core-contains covered signature))))
                                 (when (< (length kept) 12)
                                   (vector-push-extend signal kept)))))
                    (when (plusp (length kept))
                      (let* ((before (encode-json (ace-playbook handle)))
                             (signatures (%new-array)))
                        (loop for signal across kept
                              do (vector-push-extend (jget signal "signature" "") signatures))
                        (let* ((feedback (%playbook-failure-feedback kept))
                               (result (ace-apply-online-update
                                        handle
                                        (object "example"
                                                (object "task" (let ((task (jget (agent-options agent)
                                                                                 "instruction" "agent run")))
                                                                 (if (eq task :null) "agent run" task))
                                                        "failureSignatures" signatures)
                                                "prediction" (if (hash-table-p output) output (object))
                                                "feedback" feedback)))
                               (callback (let ((value (%option config "onUpdate" "on_update")))
                                           (and (functionp value) value))))
                          (%bind-playbook-stage agent handle)
                          (when callback
                            (handler-case
                                (funcall callback
                                         (object "status" (if (equal before (encode-json (ace-playbook handle)))
                                                              "unchanged"
                                                              "updated")
                                                 "signals" kept
                                                 "feedback" feedback
                                                 "snapshot" (ace-artifact handle)
                                                 "result" result))
                              (error () nil)))))))))
            (error () nil))))))
  nil)

(defun agent-evaluate-optimization-task (agent client task &key options)
  "Run AGENT once for one optimizer task and return Core's prediction.

TASK is the optimizer's task object; its \"input\" is the run's values, or
the task itself when it carries them directly. OPTIONS is the optimizer's
option object: a \"runtime\" there runs the task unless \"forward_options\"
already names one, because an agent may hold only a runtime descriptor.

The prediction is Core's: this marks the state, runs the agent once,
turns however the run ended into a completion record, and lets Core build
the prediction from the marks. The three ways a run can end -- an answer,
a clarification, an error -- are all predictions, not failures, because an
optimizer scores them all. Run-end playbook learning is paused for the
duration, as it is in every other port: an evaluated run must not teach
the agent the thing it is being measured on."
  (check-type agent ax-agent)
  (let* ((options (if (hash-table-p options) options (object)))
         (forward-options (let ((given (%option options "forward_options" "forwardOptions")))
                            (if (hash-table-p given)
                                (axllm/core::core-map-merge given (object))
                                (object))))
         (runtime (jget options "runtime")))
    (when (and (not (eq runtime :null)) (eq (jget forward-options "runtime") :null))
      (%set-key forward-options "runtime" runtime))
    (let ((marks (axllm/core::agent-eval-marks (agent-core-state agent)))
          (completion nil))
      (%set-key (agent-core-state agent) "playbook_learning_paused" 'yason:true)
      (unwind-protect
           (handler-case
               (let ((output (agent-forward agent client
                                            (let ((input (jget task "input")))
                                              (if (hash-table-p input) input task))
                                            :options forward-options)))
                 (setf completion (object "type" "final" "output" output)))
             (agent-clarification-error (condition)
               (setf completion (object "type" "askClarification"
                                        "clarification" (agent-clarification condition))))
             (error (condition)
               (setf completion (object "type" "error"
                                        "message" (princ-to-string condition)))))
        (axllm/core::core-map-delete (agent-core-state agent) "playbook_learning_paused"))
      (axllm/core::build-agent-run-prediction (agent-core-state agent) marks completion
                                              (agent-usage agent) (agent-trace agent)))))

(defun agent-function-calls (agent)
  "The tool and runtime calls AGENT's last rollout made.

Core shapes each call, so an optimizer scoring expectedActions against
this port sees the same records it would from any other."
  (check-type agent ax-agent)
  (axllm/core::agent-eval-function-calls (%state-get agent "function_call_traces" (%new-array))))

(defun agent-apply-optimized-components (agent component-map)
  "Apply COMPONENT-MAP to AGENT and to every stage set it holds."
  (check-type agent ax-agent)
  (let ((updates (if (hash-table-p component-map) component-map (object))))
    (axllm/core::core-map-update (%agent-optimized-components agent) updates)
    (dolist (stage (list (agent-distiller agent) (agent-executor agent) (agent-responder agent)))
      (program-apply-optimized-components stage updates))
    (let ((composed (axllm/core::agent-apply-optimized-components (agent-core-state agent) updates)))
      (axllm/core::core-map-update (agent-options agent) (%state-get agent "options" (object)))
      (program-set-instruction (agent-executor agent) composed)))
  agent)

;;; ------------------------------------------------------------------
;;; The agent as a program
;;; ------------------------------------------------------------------
;;;
;;; An agent is a program, so a parent agent, a flow or an optimizer treats it
;;; exactly as it treats an AxGen. These are the agent's answers to the hooks
;;; AxGen defines.

(defmethod forward ((program ax-agent) client inputs &optional options)
  (agent-forward program client inputs :options options))

(defmethod program-chat-log ((program ax-agent)) (agent-chat-log program))
(defmethod program-usage ((program ax-agent)) (agent-usage program))
(defmethod program-traces ((program ax-agent)) (agent-trace program))
(defmethod program-set-instruction ((program ax-agent) text)
  (agent-set-instruction program text))
(defmethod program-optimizable-components ((program ax-agent))
  (agent-optimizable-components program))
(defmethod program-apply-optimized-components ((program ax-agent) component-map)
  (agent-apply-optimized-components program component-map))

;;; The optimizer's own three questions. Their generic functions live in
;;; optimize.lisp, which loads before this file, so these are the agent's
;;; answers rather than new protocol.
;;;
;;; PROGRAM-FUNCTION-CALLS has no default on purpose: a program that can call
;;; tools and reports nothing would score as though it called nothing, which
;;; quietly corrupts every expectedActions and forbiddenActions score. An
;;; agent can call tools, so it answers.
;;;
;;; The kind is "axagent", which is what reaches an engine as
;;; request.programKind and an artifact as provenance.sourceProgramKind. The
;;; shared optimizer fixtures pin that spelling, so it is a wire value rather
;;; than a label: "agent" would be rejected by every port's artifact check.
;;;
;;; PROGRAM-SET-DEMOS is deliberately not answered. An agent has no demo
;;; store -- its stages do -- and the refusing default is the truthful
;;; reply; answering it with a no-op would let an optimizer believe demos
;;; were applied.

(defmethod program-kind ((program ax-agent)) "axagent")

(defmethod program-function-calls ((program ax-agent))
  (agent-function-calls program))

(defmethod program-optimizer-trace ((program ax-agent))
  (agent-trace program))

(defmethod program-evaluate-task ((program ax-agent) client task &key options)
  "Run one optimizer task against PROGRAM and return Core's prediction.

The optimizer reaches an agent through the program protocol rather than by
name, because optimize.lisp loads first: a driver that called this file's
function directly could not be compiled. The prediction is the same one
AGENT-EVALUATE-OPTIMIZATION-TASK produces, so the evolve driver reads the
real action log rather than a summary built for it."
  (agent-evaluate-optimization-task program client task :options options))

(defmethod axllm/core::core-host-get ((target ax-agent) key &optional (fallback :null))
  (let ((key (axllm/core::core-js-text key)))
    (cond ((string= key "signature") (agent-signature target))
          ((string= key "options") (agent-options target))
          ((string= key "state") (agent-core-state target))
          ((string= key "instruction") (agent-instruction target))
          (t fallback))))

(defmethod axllm/core::core-host-call ((target ax-agent) method args)
  (let ((method (axllm/core::core-js-text method)))
    (cond ((string= method "forward")
           (agent-forward target (%host-arg args 0) (%host-arg args 1)
                          :options (let ((run-options (%host-arg args 2)))
                                     (and (hash-table-p run-options) run-options))))
          ((string= method "get_chat_log") (agent-chat-log target))
          ((string= method "get_usage") (agent-usage target))
          ((string= method "get_traces") (agent-trace target))
          ((string= method "set_instruction") (agent-set-instruction target (%host-arg args 0 "")))
          ((string= method "get_optimizable_components") (agent-optimizable-components target))
          ((string= method "apply_optimized_components")
           (agent-apply-optimized-components target (%host-arg args 0)))
          (t (error 'ax-error :message (format nil "unknown agent host method: ~a" method))))))

;;; ------------------------------------------------------------------
;;; Context metrics
;;; ------------------------------------------------------------------
;;;
;;; An agent announces what it did to its own prompt through the public
;;; context event stream. This folds one run's events into the headline
;;; context-compression numbers: how large the mutable prompt grew, how much
;;; the compactions removed, how often pressure was high, and what the run
;;; cost in tokens.
;;;
;;; It reads only published events, so it measures whichever context policy
;;; is in force without knowing which one, and decides nothing about
;;; retention itself.

(defclass context-metrics-collector ()
  ((series :initform (%new-array) :reader %metrics-series)
   (checkpoints :initform 0 :accessor %metrics-checkpoints)
   (tombstones :initform 0 :accessor %metrics-tombstones)
   (compactions :initform 0 :accessor %metrics-compactions)
   (original-chars :initform 0 :accessor %metrics-original-chars)
   (rendered-chars :initform 0 :accessor %metrics-rendered-chars)
   (peak-chars :initform 0 :accessor %metrics-peak-chars)
   (final-chars :initform 0 :accessor %metrics-final-chars)
   (pressure-counts :initform (object "ok" 0 "watch" 0 "critical" 0)
                    :reader %metrics-pressure-counts))
  (:documentation
   "Accumulates one agent run's context telemetry.

Pass CONTEXT-METRICS-HANDLER's result as the agent's \"onContextEvent\"
option, then call CONTEXT-METRICS-SUMMARY with AGENT-USAGE once the run
returns."))

(defun make-context-metrics-collector ()
  "A fresh context metrics collector."
  (make-instance 'context-metrics-collector))

(defun %metrics-count (event key)
  (let ((value (jget event key 0)))
    (if (realp value) (round value) 0)))

(defun context-metrics-observe (collector event)
  "Fold one context event into COLLECTOR.

Unrecognised event kinds are ignored on purpose, as in every other port:
telemetry Ax adds later must not break a collector written before it."
  (check-type collector context-metrics-collector)
  (when (hash-table-p event)
    (let ((kind (axllm/core::core-js-text (jget event "kind" ""))))
      (cond
        ((string= kind "budget_check")
         (let ((chars (%metrics-count event "mutablePromptChars"))
               (pressure (axllm/core::core-js-text (jget event "pressure" "ok"))))
           (setf (%metrics-peak-chars collector) (max (%metrics-peak-chars collector) chars)
                 (%metrics-final-chars collector) chars)
           ;; Only the three published pressures are counted; an unknown one
           ;; is still a turn, so it keeps its place in the series.
           (when (nth-value 1 (gethash pressure (%metrics-pressure-counts collector)))
             (incf (gethash pressure (%metrics-pressure-counts collector))))
           (vector-push-extend
            (object "stage" (axllm/core::core-js-text (jget event "stage" "executor"))
                    "turn" (%metrics-count event "turn")
                    "pressure" pressure
                    "mutablePromptChars" chars
                    "effectiveBudgetChars" (%metrics-count event "effectiveBudgetChars")
                    "actionLogEntryCount" (%metrics-count event "actionLogEntryCount"))
            (%metrics-series collector))))
        ((string= kind "checkpoint_created") (incf (%metrics-checkpoints collector)))
        ((string= kind "tombstone_created") (incf (%metrics-tombstones collector)))
        ((string= kind "action_compacted")
         (incf (%metrics-compactions collector))
         (incf (%metrics-original-chars collector) (%metrics-count event "originalChars"))
         (incf (%metrics-rendered-chars collector) (%metrics-count event "renderedChars"))))))
  nil)

(defun context-metrics-handler (collector)
  "COLLECTOR as a one-argument function, for the \"onContextEvent\" option."
  (check-type collector context-metrics-collector)
  (lambda (event) (context-metrics-observe collector event)))

(defun %usage-entries (usage)
  "USAGE flattened to a list of per-request usage records.

Agent usage is published either as a flat array or as the actor and
responder split, so both are accepted."
  (cond ((%array-p usage) (coerce usage 'list))
        ((hash-table-p usage)
         (let ((actor (jget usage "actor"))
               (responder (jget usage "responder")))
           (if (or (%array-p actor) (%array-p responder))
               (append (if (%array-p actor) (coerce actor 'list) '())
                       (if (%array-p responder) (coerce responder 'list) '()))
               '())))
        (t '())))

(defun %usage-tokens (entry key)
  (let ((tokens (jget entry "tokens")))
    (if (hash-table-p tokens)
        (let ((value (jget tokens key 0)))
          (if (realp value) value 0))
        0)))

(defun context-metrics-summary (collector &optional usage)
  "COLLECTOR's summary, with USAGE's tokens folded in.

The compaction ratio is the share of compacted characters the agent
removed, (original - rendered) / original, and zero when nothing was
compacted, so an untouched run reports 0 rather than nothing at all."
  (check-type collector context-metrics-collector)
  (let ((cumulative 0) (prompt 0) (completion 0))
    (dolist (entry (%usage-entries usage))
      (when (hash-table-p entry)
        (incf cumulative (%usage-tokens entry "totalTokens"))
        (incf prompt (%usage-tokens entry "promptTokens"))
        (incf completion (%usage-tokens entry "completionTokens"))))
    (let* ((original (%metrics-original-chars collector))
           (ratio (if (plusp original)
                      (/ (float (- original (%metrics-rendered-chars collector)) 1d0)
                         (float original 1d0))
                      0))
           (series (%new-array)))
      (loop for sample across (%metrics-series collector)
            do (vector-push-extend sample series))
      (object "turns" (length (%metrics-series collector))
              "peakMutablePromptChars" (%metrics-peak-chars collector)
              "finalMutablePromptChars" (%metrics-final-chars collector)
              "checkpoints" (%metrics-checkpoints collector)
              "tombstones" (%metrics-tombstones collector)
              "compactions" (%metrics-compactions collector)
              "totalOriginalChars" original
              "totalRenderedChars" (%metrics-rendered-chars collector)
              "compactionRatio" ratio
              "pressureCounts" (axllm/core::core-map-merge (%metrics-pressure-counts collector)
                                                           (object))
              "cumulativeTokens" cumulative
              "promptTokens" prompt
              "completionTokens" completion
              "series" series))))

;;; ------------------------------------------------------------------
;;; The agent intrinsics generated Core code calls
;;; ------------------------------------------------------------------
;;;
;;; One function per intrinsic.agent.* in ir/axcore/agent.axir that is not a
;;; runtime session op. Each is a boundary to something a portable IR cannot
;;; name: a program, a Lisp function the caller supplied, or a provider call.

(in-package #:axllm/core)

(defun %without-keys (source keys)
  "SOURCE without KEYS, keeping the order of the keys that remain."
  (let ((out (core-new-map)))
    (when (hash-table-p source)
      (dolist (key (axllm::%object-keys source))
        (unless (member key keys :test #'string=)
          (core-set out key (gethash key source)))))
    out))

(defun %core-list (value)
  "VALUE as a Core list, or an empty one when it is not a list."
  (if (axllm::%array-p value) value (core-new-list)))

(defun %inventory-handler (state qualified)
  "The handler the agent's callable inventory records for QUALIFIED, if any."
  (loop for group across (%core-list (core-get state "callable_inventory"))
        do (loop for entry across (%core-list (core-get group "callables"))
                 do (when (core-value-equal (core-get entry "qualified_name") qualified)
                      (let ((handler (core-get entry "handler")))
                        (when (functionp handler)
                          (return-from %inventory-handler handler))))))
  nil)

(defun core-agent-stage-forward (stage client values options)
  "Forward one stage -- an AxGen or a child agent -- the same way.

A child agent carries its parent's MCP inheritance policy through here.
The policy is Core's; this only derives the execution context it names and
removes the two keys that were meant for the boundary, not for the child."
  (let ((options (if (typep stage 'axllm::ax-agent)
                     (if (hash-table-p options) options (core-new-map))
                     (axllm::%agent-stage-options
                      (if (hash-table-p options) options (core-new-map))))))
    (if (and (typep stage 'axllm::ax-agent)
             (core-true-p (core-map-contains options "mcpInheritanceFromParent")))
        (let ((context (core-get options "executionContext"))
              (policy (core-get options "mcpInheritanceFromParent"))
              (forwarded (%without-keys options '("executionContext" "mcpInheritanceFromParent"))))
          (unless (eq context :null)
            ;; Deriving is the MCP layer's own operation, not a host-object
            ;; method: an execution context answers no "derive" through the
            ;; bridge, so going that way failed every delegation that carried
            ;; an inheritance policy.
            (core-set forwarded "inheritedExecutionContext"
                      (axllm::execution-context-derive
                       context (if (eq policy :null) "all" policy))))
          (axllm::forward stage client (if (hash-table-p values) values (core-new-map)) forwarded))
        (axllm::forward stage client (if (hash-table-p values) values (core-new-map)) options))))

(defun core-agent-native-stage-forward (stage state client values options selected)
  "Forward STAGE with the background-agent callables SELECTED names added.

The extra callables are typed tools for this one call: they are added to
the stage, the call records which of them ran, and the stage is handed
back exactly as it was, so a later call does not inherit them."
  (let ((tools (core-new-list))
        (options (axllm::%agent-stage-options
                  (if (hash-table-p options) options (core-new-map)))))
    (loop for descriptor across (%core-list selected)
          do (let* ((qualified (core-get descriptor "qualified_name"))
                    (source (agent-callable-implementation state qualified)))
               (unless (and (hash-table-p source) (functionp (axllm:tool-handler source)))
                 (axllm::tool-fail
                  (format nil "Background agent callable '~a' needs a typed tool implementation"
                          (core-js-text qualified))))
               (vector-push-extend
                (axllm:tool :name (core-js-text (core-get descriptor "native_name"))
                            :description (core-js-text (core-get descriptor "description" ""))
                            :parameters (core-get source "parameters")
                            :handler (axllm:tool-handler source))
                tools)))
    (let* ((original (axllm::program-tools stage))
           (previous (axllm::program-function-call-traces stage))
           (extended (core-new-list)))
      (loop for item across (%core-list original) do (vector-push-extend item extended))
      (loop for item across tools do (vector-push-extend item extended))
      (axllm::program-set-tools stage extended)
      (axllm::program-clear-function-call-traces stage)
      (unwind-protect
           (axllm::forward stage client
                           (if (hash-table-p values) values (core-new-map))
                           (if (hash-table-p options) options (core-new-map)))
        (let ((records (axllm::program-function-call-traces stage))
              (merged (core-new-list)))
          (loop for item across (%core-list previous) do (vector-push-extend item merged))
          (loop for item across (%core-list records) do (vector-push-extend item merged))
          (axllm::program-set-tools stage original)
          (axllm::program-set-function-call-traces stage merged)
          (agent-record-native-calls state selected records
                                     (if (hash-table-p options) options (core-new-map))))))))

(defun core-agent-stage-streaming-forward (stage state client values options sink)
  "Stream STAGE's deltas to SINK, each through the agent's citation handling.

Core decides what a delta carries -- with hidden citations it leaves the
citation field out -- so every envelope goes through Core before the
caller sees it."
  ;; The sink travels in the options, which is how gen publishes
  ;; program-streaming-forward, and every envelope goes through Core first so a
  ;; hidden citation leaves the delta before the caller sees it.
  (let* ((call-options (axllm::%agent-stage-options
                       (core-map-merge (if (hash-table-p options) options (core-new-map))
                                       (core-new-map))))
         (control (core-get call-options "control")))
    (core-set call-options "sink"
              (lambda (envelope)
                (let ((returned nil) (failed nil))
                  (unwind-protect
                       (handler-bind ((error (lambda (condition)
                                               (declare (ignore condition))
                                               (setf failed t))))
                         (prog1 (funcall sink (agent-stream-citation-delta state envelope))
                           (setf returned t)))
                    (when (and (not returned) (not failed)
                               (typep control 'axllm::agent-control-scope))
                      (setf (axllm::%scope-interrupted control) t))))))
    (axllm::program-streaming-forward stage client
                                      (if (hash-table-p values) values (core-new-map))
                                      call-options)))

(defun core-agent-program-forward (signature program-options client values options)
  "Forward a one-off AxGen: the context map's distiller and cartographer."
  (let* ((instruction (core-get program-options "instruction" ""))
         (program (axllm:ax signature
                            :instruction (if (eq instruction :null) "" (core-js-text instruction)))))
    (axllm::forward program client
                    (if (hash-table-p values) values (core-new-map))
                    (axllm::%agent-stage-options
                     (if (hash-table-p options) options (core-new-map))))))

(defun core-agent-stage-chat-log (stage)
  "STAGE's chat log, or an empty list when it keeps none."
  (%core-list (ignore-errors (axllm::program-chat-log stage))))

(defun core-agent-stage-usage (stage)
  "STAGE's usage records.

A stage that reports usage directly is used as is; otherwise the usage is
collected from its chat log, which is where a stage that records only
requests keeps it."
  (let ((usage (ignore-errors (axllm::program-usage stage))))
    (cond ((and (axllm::%array-p usage) (plusp (length usage))) usage)
          ((and (hash-table-p usage) (plusp (hash-table-count usage))) usage)
          (t (let ((items (core-new-list)))
               (loop for entry across (%core-list (ignore-errors (axllm::program-chat-log stage)))
                     do (let ((entry-usage (core-get entry "usage")))
                          (when (core-true-p entry-usage)
                            (vector-push-extend entry-usage items))))
               items)))))

(defun core-agent-stage-traces (stage)
  "STAGE's traces, or an empty list when it keeps none."
  (%core-list (ignore-errors (axllm::program-traces stage))))

(defun core-agent-clarification-error (payload state)
  "The clarification condition for PAYLOAD, for Core to signal.

Core raises this rather than returning it, so a caller who does not handle
a clarification sees it as the error it is."
  (let* ((args (%core-list (core-get payload "args")))
         (clarification (if (plusp (length args)) (aref args 0) payload)))
    (make-condition 'axllm::agent-clarification-error
                    :message (axllm::%clarification-message clarification payload)
                    :clarification clarification
                    :state (core-get state "runtime_state" (core-new-map))
                    :payload payload)))

(defun %scripted-search-result (scripted searches concatenate)
  "SCRIPTED's answer for SEARCHES: the joined key, then each key, then \"*\".

A scripted catalog is how every port drives memory and skill search
without a live store, and this lookup order is part of that contract.
CONCATENATE collects each search's hits instead of taking the first,
because one skill turn may load several documents."
  (cond
    ((axllm::%array-p scripted) scripted)
    ((hash-table-p scripted)
     (multiple-value-bind (joined found) (gethash (core-string-join "|" searches) scripted)
       (if found
           joined
           (let ((out (core-new-list)))
             (loop for item across (%core-list searches)
                   do (let ((hit (core-get scripted (core-js-text item))))
                        (cond ((and concatenate (axllm::%array-p hit))
                               (loop for entry across hit do (vector-push-extend entry out)))
                              ((and (not concatenate) (not (eq hit :null)))
                               (return-from %scripted-search-result hit)))))
             (if (plusp (length out))
                 out
                 (%core-list (core-get scripted "*")))))))
    (t (core-new-list))))

(defun core-agent-memory-search (state searches already-loaded)
  "The host's answer to a recall: a callback, else a scripted catalog."
  (let* ((options (core-get state "options" (core-new-map)))
         (callback (axllm::%option options "on_memories_search" "onMemoriesSearch")))
    (if (functionp callback)
        (%core-list (funcall callback searches already-loaded))
        (%scripted-search-result (core-coalesce (core-get options "memory_search_results")
                                                (core-get options "memorySearchResults"))
                                 searches nil))))

(defun core-agent-skill-search (state searches)
  "The host's answer to a skill search: a callback, else a scripted catalog."
  (let* ((options (core-get state "options" (core-new-map)))
         (callback (axllm::%option options "on_skills_search" "onSkillsSearch")))
    (if (functionp callback)
        (%core-list (funcall callback searches))
        (%scripted-search-result (core-coalesce (core-get options "skill_search_results")
                                                (core-get options "skillSearchResults"))
                                 searches t))))

(defparameter +agent-observer-options+
  '(("loaded_memories" "on_loaded_memories" "onLoadedMemories")
    ("loaded_skills" "on_loaded_skills" "onLoadedSkills")
    ("used_memories" "on_used_memories" "onUsedMemories")
    ("used_skills" "on_used_skills" "onUsedSkills"))
  "The published load and use observers, and the options they answer to.")

(defun core-agent-observer-notify (state forward-options kind payload)
  "Tell the configured observer about KIND.

A use observer may be given per run, which then wins over the
constructor's; a load observer is a constructor option only. An observer
that signals is ignored: telling a caller what happened must not change
what happened."
  (let* ((kind (core-js-text kind))
         (names (assoc kind +agent-observer-options+ :test #'string=)))
    (when names
      (let* ((constructor-options (core-get state "options" (core-new-map)))
             (per-run (and (core-true-p (core-string-starts-with kind "used_"))
                           (let ((value (axllm::%option forward-options
                                                        (second names) (third names))))
                             (and (functionp value) value))))
             (callback (or per-run
                           (let ((value (axllm::%option constructor-options
                                                        (second names) (third names))))
                             (and (functionp value) value)))))
        (when callback
          (handler-case (funcall callback (%core-list payload))
            (error () nil))))))
  :null)

(defun core-agent-transcribe (client request options)
  "Turn CLIENT's audio input into text before the agent loop sees it."
  (axllm::ax-transcribe client request
                        (if (hash-table-p options) options (core-new-map))))

(defun %scripted-callable-result (agent-options request qualified)
  (let ((scripted (core-coalesce (core-get agent-options "callable_results")
                                 (core-get agent-options "callableResults"))))
    (if (hash-table-p scripted)
        (core-coalesce (core-get scripted (core-js-text qualified))
                       (core-coalesce (core-get scripted (core-js-text (core-get request "name" "")))
                                      (core-get scripted "*")))
        :null)))

(defun %native-callable-handler (implementation)
  "IMPLEMENTATION's native protocol handler, or NIL.

A protocol tool keeps its handler under the keyword key :HANDLER, the same
convention records use for :RECORD, so it never reaches JSON output and
never collides with a string key. That keyword is also what tells a
protocol tool from an Ax tool, whose handler is under the string key, and
the two are invoked differently: a protocol handler takes the execution
context as well as the arguments."
  (when (hash-table-p implementation)
    (let ((handler (gethash :handler implementation)))
      (and (functionp handler) handler))))

(defun core-agent-callable-invoke (state request options)
  "Invoke one of the agent's callables and report its status and value.

A typed tool is validated and invoked through the tool layer, a plain
handler is called with the arguments, and a scripted result answers a
fixture. An unknown name is an error result, never a silent empty value."
  (let* ((agent-options (core-get state "options" (core-new-map)))
         (qualified (core-coalesce (core-get request "qualified_name")
                                   (core-get request "name" "")))
         (arguments (core-get request "args" (core-new-map)))
         (implementation (agent-callable-implementation state qualified))
         (inventory-handler (%inventory-handler state qualified))
         (native-handler (%native-callable-handler implementation)))
    (cond
      ;; A protocol tool published by a server. Its schema is the server's and
      ;; is passed through untouched: validating it here would reject schemas
      ;; the tool layer's fail-closed checker cannot express, and the server
      ;; is the only authority on its own arguments. Its handler takes the
      ;; execution context as a second argument, which is how client identity
      ;; and cancellation survive into the call.
      (native-handler
       (axllm:object "status" "ok"
                     "value" (funcall native-handler arguments
                                      (core-coalesce (core-get options "tool_context")
                                                     (core-get options "toolContext")))))
      ((and (hash-table-p implementation) (functionp (axllm:tool-handler implementation)))
       (multiple-value-bind (result problems) (axllm:invoke-tool implementation arguments)
         (if problems
             (axllm:object "status" "error"
                           "error" (core-string-join " " (coerce problems 'vector)))
             (axllm:object "status" "ok" "value" result))))
      ((functionp (core-get implementation "handler"))
       (axllm:object "status" "ok"
                     "value" (funcall (core-get implementation "handler") arguments)))
      (inventory-handler
       (axllm:object "status" "ok" "value" (funcall inventory-handler arguments)))
      (t
       (let ((result (%scripted-callable-result agent-options request qualified)))
         (cond
           ((eq result :null)
            (axllm:object "status" "error"
                          "error" (core-string-format "unknown callable: {}" qualified)))
           ((hash-table-p result)
            (let ((copied (core-map-merge result (core-new-map))))
              (if (core-true-p (core-get copied "error"))
                  (axllm:object "status" "error" "error" (core-get copied "error"))
                  (progn
                    (unless (core-true-p (core-map-contains copied "status"))
                      (core-set copied "status" "ok"))
                    copied))))
           (t (axllm:object "status" "ok" "value" result))))))))
