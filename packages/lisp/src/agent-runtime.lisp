;;;; agent-runtime.lisp --- the agent's executable code-runtime boundary.
;;;;
;;;; AxAgent's actor stages can write code instead of prose. Core owns what
;;;; the globals are, when a session is created, how one step's result is
;;;; normalized and when the session is exported, restored or closed. This
;;;; file owns the other side of that line: the protocol a host runtime
;;;; implements, the envelopes it answers with, and one concrete runtime --
;;;; a separate operating-system process speaking JSON lines over its own
;;;; standard input and output.
;;;;
;;;; A process is the only runtime shipped here on purpose. Actor code comes
;;;; from a language model, so it is untrusted input: this file never passes
;;;; it to READ, EVAL or COMPILE. The code crosses a process boundary and is
;;;; run by whatever engine the operator chose -- the QuickJS or Pyodide
;;;; protocol servers Ax already ships, or any other program that answers
;;;; the same ops. Choosing that program, and sandboxing it, is the
;;;; operator's decision and stays outside this file.
;;;;
;;;; The protocol, one JSON object per line in each direction:
;;;;
;;;;   -> {"id":"1","op":"capabilities","payload":{}}
;;;;   <- {"id":"1","ok":true,"result":{"language":"JavaScript",...}}
;;;;   -> {"id":"2","op":"create_session","payload":{"globals":{},"options":{}}}
;;;;   <- {"id":"2","ok":true,"session_id":"s1","result":{"session_id":"s1"}}
;;;;   -> {"id":"3","op":"execute","session_id":"s1",
;;;;       "payload":{"code":"final({})","options":{}}}
;;;;   <- {"id":"3","ok":true,"session_id":"s1","result":{"type":"final","args":[{}]}}
;;;;   <- {"id":"3","ok":false,"error":{"category":"timeout","message":"..."}}
;;;;
;;;; ops: capabilities, create_session, execute, inspect_globals,
;;;; snapshot_globals, patch_globals, close, shutdown.
;;;;
;;;; Every request is correlated by id and, for session ops, by session_id;
;;;; a crossed id, a crossed session, a non-object response, unparsable text
;;;; or a closed pipe is an error naming what went wrong, never a value the
;;;; agent loop would mistake for a result.
;;;;
;;;; Host calls are an optional extension, negotiated and off by default. A
;;;; worker that answers `capabilities' with "host_calls": true may, while it
;;;; is running an execute, ask the host to run a callable the host registered
;;;; for that run. A worker that does not advertise it is unchanged in every
;;;; respect, and RUNTIME-SUPPORTS-CALLABLES-P stays false for it, so the
;;;; servers written before this exist keep working untouched.
;;;;
;;;;   -> {"id":"4","op":"create_session",
;;;;       "payload":{"globals":{},"options":{},"host_calls":["crm.lookup"]}}
;;;;   -> {"id":"5","op":"execute","session_id":"s1",
;;;;       "payload":{"code":"const r = await crm.lookup({id:'c-7'}); final({})"}}
;;;;   <- {"op":"host_call","callback_id":"cb1","request_id":"5",
;;;;       "session_id":"s1","name":"crm.lookup","params":{"id":"c-7"}}
;;;;   -> {"id":"cb1","ok":true,"result":{"tier":"gold"}}
;;;;   <- {"id":"5","ok":true,"session_id":"s1","result":{"type":"final",...}}
;;;;
;;;; A host call carries its own callback id, the id of the request it
;;;; interrupted and its session, and the host answers in the same envelope
;;;; every other reply uses. It serves only a name registered for the run that
;;;; is in flight: a frame naming another request, another session, an
;;;; unregistered name, or a name whose run has been retired is refused with
;;;; an error envelope rather than executed. Callbacks do not extend the
;;;; execute's deadline -- the caller asked for one operation within one
;;;; bound, and a worker calling back repeatedly must not be able to hold the
;;;; run open indefinitely while no single wait ever expires.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Errors
;;; ------------------------------------------------------------------

(define-condition runtime-protocol-error (ax-error)
  ((category :initarg :category :initform "runtime"
             :reader runtime-protocol-error-category
             :documentation
             "The failure's Ax category: \"runtime\", \"timeout\",
\"session_closed\", \"abort\", \"user_error\", \"unavailable\" or
\"protocol\". Core reads it to decide whether a step can be retried in a
fresh session, so it is data rather than prose."))
  (:documentation
   "A runtime step or protocol exchange that failed.

Signalled by the protocol ops that must fail loudly -- capabilities,
create_session, inspect, snapshot, patch -- and caught by SESSION-EXECUTE,
which turns it into an error envelope so one bad step does not end the
run."))

(defun %runtime-fail (category format-control &rest arguments)
  (error 'runtime-protocol-error
         :category category
         :message (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; Envelopes
;;; ------------------------------------------------------------------
;;;
;;; What a session's EXECUTE answers with. Core normalizes these into step
;;; results, so the key names and the category strings are a contract with
;;; the other ports, not a local choice.

(defun envelope-result (value)
  "A step that produced VALUE and asked for nothing."
  (object "kind" "result" "result" value))

(defun envelope-error (message &optional (category "runtime"))
  "A step that failed, with MESSAGE and an Ax failure CATEGORY."
  (object "kind" "error"
          "is_error" 'yason:true
          "error_category" (axllm/core::core-js-text category)
          "error" (axllm/core::core-js-text message)))

(defun envelope-session-closed (&optional (message "session closed"))
  "A step that found its session gone. Core restarts one fresh session."
  (envelope-error message "session_closed"))

(defun envelope-timeout (&optional (message "execution timed out"))
  "A step that outran its budget."
  (envelope-error message "timeout"))

(defun %completion-args (arguments)
  "ARGUMENTS as a completion payload's args array.

One array argument is the array of arguments, matching every other port:
final([a, b]) and final(a, b) carry the same two values."
  (let ((out (%new-array)))
    (if (and (= (length arguments) 1) (%array-p (first arguments)))
        (loop for item across (first arguments) do (vector-push-extend item out))
        (dolist (item arguments) (vector-push-extend item out)))
    out))

(defun envelope-final (&rest arguments)
  "The actor's final answer."
  (object "type" "final" "args" (%completion-args arguments)))

(defun envelope-ask-clarification (&rest arguments)
  "The actor asking the caller a question instead of answering."
  (object "type" "askClarification" "args" (%completion-args arguments)))

(defun envelope-discover (request)
  "An effect-only step asking for full docs of what REQUEST names."
  (object "kind" "discover" "discover" request))

(defun envelope-recall (request)
  "An effect-only step asking for the memories REQUEST names."
  (object "kind" "recall" "recall" request))

(defun envelope-used (request &key reason stage)
  "A step reporting that it used a loaded memory, skill or module.

REQUEST is either the record or a bare id."
  (let ((payload (if (hash-table-p request)
                     (axllm/core::core-map-merge request (%new-object))
                     (object "id" request))))
    (when reason (%set-key payload "reason" reason))
    (when stage (%set-key payload "stage" stage))
    (object "kind" "used" "used" payload)))

(defun envelope-status (status-type &optional (message "") extra)
  "A step reporting progress. EXTRA, when given, is merged after message."
  (let ((payload (object "type" status-type "message" message)))
    (when (hash-table-p extra)
      (axllm/core::core-map-update payload extra))
    (object "kind" "status" "status" payload)))

(defun envelope-guide-agent (guidance &optional triggered-by)
  "A step steering the next turn rather than producing a value."
  (let ((payload (object "type" "guide_agent" "guidance" guidance)))
    (when triggered-by (%set-key payload "triggeredBy" triggered-by))
    payload))

;;; ------------------------------------------------------------------
;;; Capabilities
;;; ------------------------------------------------------------------

(defun runtime-capabilities (&key (inspect t) (snapshot t) (patch t) (abort nil)
                                  (language "JavaScript") (usage-instructions ""))
  "A runtime's capability object, in the shape every Ax port reports.

INSPECT, SNAPSHOT and PATCH say whether the session can show, export and
restore its globals; ABORT says whether a running step can be cancelled.
A runtime that answers false must also refuse the matching op, so a caller
is never told a capability exists and then handed a silent no-op."
  (object "inspect" (json-boolean inspect)
          "snapshot" (json-boolean snapshot)
          "patch" (json-boolean patch)
          "abort" (json-boolean abort)
          "language" (axllm/core::core-js-text language)
          "usage_instructions" (axllm/core::core-js-text usage-instructions)))

;;; ------------------------------------------------------------------
;;; The runtime and session protocol
;;; ------------------------------------------------------------------
;;;
;;; A host runtime is an instance of CODE-RUNTIME and its sessions are
;;; instances of CODE-SESSION. Subclassing is the contract: Core asks
;;; RUNTIME-EXECUTABLE-P whether the thing it was handed can run code, and
;;; a plain JSON object describing a runtime (`{"language":"Python"}`) must
;;; answer no. Duck typing cannot draw that line, because every JSON object
;;; would pass it.

(defclass code-runtime ()
  ()
  (:documentation
   "A host that can run actor code.

Subclass this and implement RUNTIME-CREATE-SESSION. RUNTIME-LANGUAGE and
RUNTIME-USAGE-INSTRUCTIONS are optional; their defaults are JavaScript and
no instructions, which is what the other ports fall back to."))

(defclass code-session ()
  ()
  (:documentation
   "One live session of a CODE-RUNTIME: the scope the actor's globals live
in across steps.

Implement SESSION-EXECUTE. The state methods -- SESSION-INSPECT-GLOBALS,
SESSION-SNAPSHOT-GLOBALS, SESSION-PATCH-GLOBALS -- are optional, and their
defaults report the capability as missing instead of pretending."))

(defgeneric runtime-executable-p (runtime)
  (:documentation "Whether RUNTIME can actually run code.")
  (:method ((runtime t)) nil)
  (:method ((runtime code-runtime)) t))

(defgeneric runtime-language (runtime)
  (:documentation "RUNTIME's code language, as the actor prompt names it.")
  (:method ((runtime code-runtime)) "JavaScript"))

(defgeneric runtime-usage-instructions (runtime)
  (:documentation "RUNTIME's own prompt guidance, or \"\" when it has none.")
  (:method ((runtime code-runtime)) ""))

(defgeneric runtime-create-session (runtime globals options)
  (:documentation
   "A new CODE-SESSION of RUNTIME with GLOBALS already bound.

GLOBALS and OPTIONS are JSON objects built by Core: the inputs, context,
callables and primitives the actor may use, and the session options
(reserved names, timeout, trace and session ids).")
  (:method ((runtime code-runtime) globals options)
    (declare (ignore globals options))
    (%runtime-fail "runtime" "~a does not implement runtime-create-session"
                   (type-of runtime))))

(defgeneric runtime-supports-callables-p (runtime)
  (:documentation
   "Whether RUNTIME accepts host callables through RUNTIME-REGISTER-CALLABLE.

A runtime that runs in this process can call back into Lisp and answers
true. A runtime behind a pipe cannot: its own program owns its callables,
and Core instead describes them in the session globals.")
  (:method ((runtime t)) nil))

(defgeneric runtime-register-callable (runtime name function)
  (:documentation
   "Make FUNCTION callable as NAME inside RUNTIME's sessions.

FUNCTION takes one JSON value and returns one. Only called when
RUNTIME-SUPPORTS-CALLABLES-P is true.")
  (:method ((runtime t) name function)
    (declare (ignore function))
    (%runtime-fail "runtime" "~a does not accept the host callable ~a"
                   (type-of runtime) name)))

(defgeneric runtime-shutdown (runtime)
  (:documentation "Release everything RUNTIME holds. Safe to call twice.")
  (:method ((runtime code-runtime)) (object "shutdown" 'yason:true)))

(defgeneric session-execute (session code options)
  (:documentation
   "Run one step of CODE in SESSION and return its envelope.

This never signals for a failure of the code itself: a thrown error, a
timeout, an abort or a closed session all come back as error envelopes, so
Core can log the step and decide what happens next.")
  (:method ((session code-session) code options)
    (declare (ignore code options))
    (%runtime-fail "runtime" "~a does not implement session-execute"
                   (type-of session))))

(defgeneric session-closed-p (session)
  (:documentation "Whether SESSION has been closed.")
  (:method ((session code-session)) nil))

(defgeneric session-inspect-globals (session options)
  (:documentation
   "A readable view of SESSION's globals, for the runtime-state summary.")
  (:method ((session code-session) options)
    (declare (ignore options))
    "[runtime state inspection unavailable: runtime session does not implement inspect-globals]"))

(defgeneric session-snapshot-globals (session options)
  (:documentation
   "SESSION's globals as a restorable snapshot: version, entries, bindings.")
  (:method ((session code-session) options)
    (declare (ignore options))
    (%runtime-fail "unavailable"
                   "session-snapshot-globals is required to export AxAgent state; ~a does not implement it"
                   (type-of session))))

(defgeneric session-patch-globals (session snapshot options)
  (:documentation "Write SNAPSHOT's bindings back into SESSION.")
  (:method ((session code-session) snapshot options)
    (declare (ignore snapshot options))
    (%runtime-fail "unavailable"
                   "session-patch-globals is required to restore AxAgent state; ~a does not implement it"
                   (type-of session))))

(defgeneric session-export-state (session options)
  (:documentation "SESSION's state for a later SESSION-RESTORE-STATE.")
  (:method ((session code-session) options)
    (session-snapshot-globals session options)))

(defgeneric session-restore-state (session snapshot options)
  (:documentation "Put SNAPSHOT back into SESSION.")
  (:method ((session code-session) snapshot options)
    (session-patch-globals session snapshot options)))

(defgeneric session-close (session)
  (:documentation "End SESSION. Safe to call twice.")
  (:method ((session code-session)) (object "closed" 'yason:true)))

;;; ------------------------------------------------------------------
;;; Locks
;;; ------------------------------------------------------------------
;;;
;;; One pipe carries every request, so two threads sharing a runtime must
;;; not interleave their lines: the second reader would take the first
;;; reader's response. The lock is recursive because a session op is
;;; written in terms of a runtime request.

(defun %make-runtime-lock (name)
  #+sb-thread (sb-thread:make-mutex :name name)
  #-sb-thread (progn name nil))

(defmacro %with-runtime-lock ((place) &body body)
  #+sb-thread `(sb-thread:with-recursive-lock (,place) ,@body)
  #-sb-thread `(progn ,place ,@body))

(defun %current-run-thread ()
  "An identity for the calling thread, for the owning-thread checks."
  #+sb-thread sb-thread:*current-thread*
  #-sb-thread :single-threaded)

;;; ------------------------------------------------------------------
;;; The process runtime
;;; ------------------------------------------------------------------

(defparameter +default-runtime-timeout-seconds+ 30
  "How long one protocol request may take before the worker is killed.

A runtime engine that stops answering would otherwise hang the agent run
for good: a model can write an endless loop, and a process behind a pipe
has no other way to be interrupted. NIL waits forever, which is only
reasonable for a worker a human is watching.")

(defclass process-runtime (code-runtime)
  ((process :initarg :process :reader process-runtime-process)
   (command :initarg :command :reader process-runtime-command
            :documentation "The argv this worker was launched with.")
   (language :initarg :language :initform "JavaScript" :reader %process-language)
   (timeout :initarg :timeout :initform +default-runtime-timeout-seconds+
            :reader process-runtime-timeout)
   (lock :initform (%make-runtime-lock "ax-process-runtime") :reader %process-lock)
   (counter :initform 0 :accessor %process-counter)
   (dead-reason :initform nil :accessor %process-dead-reason)
   (capabilities :initform nil :accessor %process-capabilities
                 :documentation "The worker's capabilities answer, read once.")
   (callables :initform (make-hash-table :test #'equal) :reader %process-callables
              :documentation "Host functions this worker may call back into,
by qualified name. Only populated when the worker advertised hostCalls, and
emptied when the invocation that registered them is retired."))
  (:documentation
   "A runtime in a separate process, speaking the JSON-line protocol.

The worker program is the operator's choice: Ax's QuickJS or Pyodide
protocol servers, or any other program implementing the same ops. Actor
code is written to that process's standard input and never evaluated
here."))

(defclass process-session (code-session)
  ((runtime :initarg :runtime :reader process-session-runtime)
   (id :initarg :id :reader process-session-id)
   (closed :initform nil :accessor %process-session-closed))
  (:documentation "One session of a PROCESS-RUNTIME, named by its session id."))

(defun make-process-runtime (command &key cwd env (language "JavaScript")
                                          (timeout +default-runtime-timeout-seconds+))
  "Launch COMMAND as a runtime protocol worker.

COMMAND is a list of argv strings, or one string split on spaces. CWD is
the worker's working directory. ENV is an alist of extra environment
variables, applied by running the command through `env' rather than by
editing this process's environment. TIMEOUT bounds one request in seconds;
NIL waits forever.

The worker is not started through a shell, so nothing in COMMAND, CWD or
ENV is expanded or re-parsed."
  (let* ((argv (etypecase command
                 (string (remove "" (uiop:split-string command :separator " ") :test #'string=))
                 (list (mapcar #'axllm/core::core-js-text command))))
         (argv (if env
                   (append (list "env")
                           (mapcar (lambda (pair)
                                     (format nil "~a=~a" (car pair)
                                             (axllm/core::core-js-text (cdr pair))))
                                   env)
                           argv)
                   argv)))
    (when (null argv)
      (error 'ax-error :message "make-process-runtime: command is empty"))
    (unless (or (null timeout) (and (realp timeout) (plusp timeout)))
      (error 'ax-error
             :message (format nil "make-process-runtime: :timeout must be a positive number or nil, got ~s"
                              timeout)))
    (make-instance 'process-runtime
                   :command argv
                   :language language
                   :timeout timeout
                   :process (uiop:launch-program argv
                                                 :input :stream
                                                 :output :stream
                                                 :error-output :stream
                                                 :directory cwd
                                                 :external-format :utf-8))))

(defun %process-exit-code (runtime)
  "RUNTIME's worker exit code, or NIL while it is still running.

Waits briefly, as the other ports do: a worker that just closed its pipe
usually has not been reaped yet, and its exit code is the most useful part
of the error message."
  (let ((process (process-runtime-process runtime)))
    (if (uiop:process-alive-p process)
        (progn
          (sleep 0.05)
          (if (uiop:process-alive-p process) nil (uiop:wait-process process)))
        (uiop:wait-process process))))

(defun %process-stderr-text (runtime)
  "Whatever the worker wrote to standard error, once it has exited.

Only read after exit: the stream has no end while the worker lives, so
reading it early would block."
  (let ((stream (uiop:process-info-error-output (process-runtime-process runtime))))
    (when (and stream (open-stream-p stream))
      (handler-case
          (let ((text (with-output-to-string (out)
                        (loop for line = (read-line stream nil nil)
                              while line do (write-line line out)))))
            (string-trim '(#\Space #\Tab #\Newline #\Return) text))
        (error () nil)))))

(defun %kill-process-runtime (runtime reason)
  "Stop RUNTIME's worker and refuse every later request, naming REASON.

Called when the channel can no longer be trusted: after a timeout a late
response would be read as the answer to the next request, so the pipe is
abandoned rather than resynchronised."
  (setf (%process-dead-reason runtime) reason)
  (let ((process (process-runtime-process runtime)))
    (dolist (stream (list (uiop:process-info-input process)
                          (uiop:process-info-output process)))
      (when (and stream (open-stream-p stream))
        (ignore-errors (close stream))))
    (when (uiop:process-alive-p process)
      (ignore-errors (uiop:terminate-process process :urgent t)))
    (ignore-errors (uiop:wait-process process)))
  reason)

(defun %read-protocol-line (stream seconds)
  "One line from STREAM, or :TIMEOUT when SECONDS elapse first."
  (if (and seconds (plusp seconds))
      #+sbcl (handler-case (sb-ext:with-timeout seconds (read-line stream nil nil))
               (sb-ext:timeout () :timeout))
      #-sbcl (read-line stream nil nil)
      (read-line stream nil nil)))

(defun %closed-without-response-message (runtime)
  (let ((code (%process-exit-code runtime)))
    (if (null code)
        "runtime protocol process closed without a response"
        (let ((stderr (%process-stderr-text runtime)))
          (if (and stderr (plusp (length stderr)))
              (format nil "runtime protocol process closed without a response (exit code ~d): ~a"
                      code stderr)
              (format nil "runtime protocol process closed without a response (exit code ~d)"
                      code))))))

(defparameter +host-only-option-keys+
  '("runtime" "control" "mcp" "ucp" "mcpContext" "mcp_context"
    "executionContext" "execution_context"
    "inheritedExecutionContext" "inherited_execution_context"
    "mcpExecutionContext" "mcp_execution_context"
    "functions" "abortSignal" "abort_signal" "cancellation" "eventContext"
    "event_context" "protocol" "sink" "logger" "tracer" "meter")
  "Option keys whose values live in this image and never cross a pipe.

These are the keys Core itself treats as host-held -- the same set
`agent_stage_options' withholds from a stage -- plus the observer and
transport handles a caller may pass. A worker is given the code and the
values, not the objects this process uses to run it.")

(defun %json-value-p (value)
  "Whether VALUE is something JSON can carry."
  (or (stringp value) (realp value) (null value)
      (eq value :null) (eq value 'yason:true) (eq value 'yason:false)
      (eq value t)
      (hash-table-p value)
      (and (vectorp value) (not (stringp value)))))

(defun %sendable (value path)
  "VALUE prepared for the pipe, or a protocol error naming what cannot go.

A host object under a known host-only key is dropped, because Core puts it
in the options for this side's use and the worker was never going to read
it. Anything else that cannot be encoded is refused with its path rather
than removed: a value the caller put there is theirs, and silently losing
it would hand the worker a different request from the one that was made.

An array is never compacted. Dropping one element would move every later
one, so a position the code indexes by would quietly mean something else;
an element that cannot cross is an error about that element."
  (cond
    ((hash-table-p value)
     (let ((out (%new-object)))
       (maphash
        (lambda (key inner)
          (let ((key (axllm/core::core-js-text key)))
            (cond
              ((%json-value-p inner)
               (%set-key out key (%sendable inner (format nil "~a.~a" path key))))
              ((member key +host-only-option-keys+ :test #'string=)
               nil)
              (t
               (%runtime-fail "protocol"
                              "runtime protocol payload value at ~a.~a cannot cross a process boundary: ~a"
                              path key (type-of inner))))))
        value)
       out))
    ((and (vectorp value) (not (stringp value)))
     (let ((out (%new-array)))
       (loop for inner across value
             for index from 0
             do (unless (%json-value-p inner)
                  (%runtime-fail "protocol"
                                 "runtime protocol payload value at ~a[~a] cannot cross a process boundary: ~a"
                                 path index (type-of inner)))
                (vector-push-extend (%sendable inner (format nil "~a[~a]" path index)) out))
       out))
    (t value)))

(defun %host-call-frame-p (value)
  "Whether VALUE is a worker-initiated host call rather than a response."
  (and (hash-table-p value)
       (equal (axllm/core::core-js-text (jget value "op" "")) "host_call")))

(defun %serve-host-call (runtime input frame request-id session-id)
  "Answer one worker-initiated host call on INPUT.

The frame has to name the request it interrupted and the session it
belongs to, and a name registered for that invocation. Anything else is
refused with an error envelope rather than served: a worker that can ask
the host to run an arbitrary name, out of band or after the run that
registered it is over, is a worse failure than a refused call."
  (let* ((callback-id (jget frame "callback_id"))
         (claimed-request (jget frame "request_id"))
         (claimed-session (jget frame "session_id"))
         (name (jget frame "name"))
         ;; llmQuery accepts arrays. Preserve JSON values instead of replacing
         ;; every non-object argument with an empty object.
         (params (jget frame "params"))
         (reply (object))
         (refusal
           (cond
             ((or (eq callback-id :null) (equal callback-id ""))
              "host call carried no callback_id")
             ((not (equal (axllm/core::core-js-text claimed-request) request-id))
              (format nil "host call names request ~s while ~s is in flight"
                      (axllm/core::core-js-text claimed-request) request-id))
             ((not (equal (axllm/core::core-js-text claimed-session)
                          (or session-id "")))
              (format nil "host call names session ~s while ~s is in flight"
                      (axllm/core::core-js-text claimed-session) (or session-id "")))
             ((eq name :null) "host call carried no name")
             ((null (gethash (axllm/core::core-js-text name)
                             (%process-callables runtime)))
              (format nil "no host callable named ~a is registered for this invocation"
                      (axllm/core::core-js-text name)))
             (t nil))))
    (%set-key reply "id" (if (eq callback-id :null) :null callback-id))
    (if refusal
        (progn (%set-key reply "ok" 'yason:false)
               (%set-key reply "error" (object "category" "protocol" "message" refusal)))
        (handler-case
            (let ((value (funcall (gethash (axllm/core::core-js-text name)
                                           (%process-callables runtime))
                                  params)))
              (%set-key reply "ok" 'yason:true)
              (%set-key reply "result" (%sendable (if (eq value :null) :null value)
                                                  "host_call.result")))
          (error (condition)
            (%set-key reply "ok" 'yason:false)
            (%set-key reply "error"
                      (object "category" "runtime"
                              "message" (substitute #\Space #\Newline
                                                    (princ-to-string condition)))))))
    (handler-case (progn (write-line (encode-json reply) input)
                         (force-output input))
      (error ()
        (%kill-process-runtime runtime (%closed-without-response-message runtime))
        (%runtime-fail "session_closed" "~a" (%process-dead-reason runtime))))
    (axllm/core::core-js-text (if (eq callback-id :null) "" callback-id))))

(defun %await-protocol-response (runtime input output id session-id op)
  "The worker's response line for request ID, serving host calls on the way.

A host call does not reset the clock. The caller asked for one operation
within one deadline, and a worker that called back a thousand times would
otherwise hold the run open forever while never exceeding any single
wait."
  (let* ((timeout (process-runtime-timeout runtime))
         (deadline (and timeout (plusp timeout)
                        (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second)))))
    (loop
      (let* ((remaining (and deadline
                             (/ (- deadline (get-internal-real-time))
                                internal-time-units-per-second)))
             (line (if (and remaining (not (plusp remaining)))
                       :timeout
                       (%read-protocol-line output remaining))))
        (when (eq line :timeout)
          (let ((reason (format nil "runtime protocol request timed out after ~a second(s) (op ~a)"
                                timeout op)))
            (%kill-process-runtime runtime reason)
            (%runtime-fail "timeout" "~a" reason)))
        (when (null line)
          (let ((reason (%closed-without-response-message runtime)))
            (%kill-process-runtime runtime reason)
            (%runtime-fail "session_closed" "~a" reason)))
        (let ((parsed (handler-case (parse-json line) (error () nil))))
          (if (%host-call-frame-p parsed)
              (%serve-host-call runtime input parsed id session-id)
              (return line)))))))

(defun %protocol-request (runtime op session-id payload)
  "Send one OP to RUNTIME and return its response object.

Signals RUNTIME-PROTOCOL-ERROR when the worker reports a failure, when the
exchange cannot be trusted, or when the request outruns the timeout."
  (%with-runtime-lock ((%process-lock runtime))
    (let ((reason (%process-dead-reason runtime)))
      (when reason
        (%runtime-fail "session_closed" "~a" reason)))
    (let* ((id (princ-to-string (incf (%process-counter runtime))))
           (process (process-runtime-process runtime))
           (input (uiop:process-info-input process))
           (output (uiop:process-info-output process))
           (message (object "id" id "op" op)))
      (when session-id (%set-key message "session_id" session-id))
      (%set-key message "payload" (%sendable (or payload (%new-object)) "payload"))
      (unless (and input output (open-stream-p input) (open-stream-p output))
        (%runtime-fail "session_closed" "runtime protocol process is closed"))
      ;; Encoding is this side's work and a failure here means the request was
      ;; never sent, so it must not be reported as the worker closing: that
      ;; blamed a healthy process for a local bug and hid which op could not
      ;; be serialized.
      (let ((text (handler-case (encode-json message)
                    (error (condition)
                      (%runtime-fail "protocol"
                                     "runtime protocol request could not be encoded (op ~a): ~a"
                                     op (substitute #\Space #\Newline
                                                    (princ-to-string condition)))))))
        (handler-case
            (progn (write-line text input)
                   (force-output input))
          (runtime-protocol-error (condition) (error condition))
          (error ()
            (%kill-process-runtime runtime (%closed-without-response-message runtime))
            (%runtime-fail "session_closed" "~a" (%process-dead-reason runtime)))))
      (let ((line (%await-protocol-response runtime input output id session-id op)))
        (let ((response (handler-case (parse-json line)
                          (error (condition)
                            (%runtime-fail "protocol"
                                           "runtime protocol invalid JSON response: ~a"
                                           (substitute #\Space #\Newline
                                                       (princ-to-string condition)))))))
          (unless (hash-table-p response)
            (%runtime-fail "protocol" "runtime protocol response must be an object"))
          (unless (equal (axllm/core::core-js-text (jget response "id" "")) id)
            (%runtime-fail "protocol" "runtime protocol response id mismatch"))
          (when session-id
            (let ((answered (jget response "session_id")))
              (unless (or (eq answered :null)
                          (equal (axllm/core::core-js-text answered) session-id))
                (%runtime-fail "protocol" "runtime protocol session_id mismatch"))))
          (when (json-false-p (jget response "ok"))
            (let ((failure (jget response "error")))
              (%runtime-fail (if (hash-table-p failure)
                                 (axllm/core::core-js-text (jget failure "category" "runtime"))
                                 "runtime")
                             "~a"
                             (if (hash-table-p failure)
                                 (axllm/core::core-js-text (jget failure "message" "runtime protocol error"))
                                 "runtime protocol error"))))
          response)))))

(defun %protocol-result (response)
  (let ((result (jget response "result")))
    (if (eq result :null) (%new-object) result)))

(defmethod runtime-language ((runtime process-runtime))
  (%process-language runtime))

(defun %process-capabilities-of (runtime)
  "RUNTIME's capabilities, asked once and remembered.

A worker that cannot answer is treated as one that advertises nothing,
which is what an older server is: it keeps every optional extension off
rather than failing the run."
  (or (%process-capabilities runtime)
      (setf (%process-capabilities runtime)
            (handler-case
                (let ((result (%protocol-result
                               (%protocol-request runtime "capabilities" nil nil))))
                  (if (hash-table-p result) result (%new-object)))
              (runtime-protocol-error () (%new-object))))))

(defmethod runtime-supports-callables-p ((runtime process-runtime))
  "Whether this worker advertised that it can call back into the host.

Negotiated, never assumed: the extension exists only for a worker that
says it implements it, so every server written before it keeps the old
behaviour, where the worker owns its own callables and Core describes
them in the session globals instead."
  (let* ((capabilities (%process-capabilities-of runtime))
         (snake (jget capabilities "host_calls"))
         (value (if (eq snake :null) (jget capabilities "hostCalls") snake)))
    (and (not (eq value :null)) (axllm/core::core-true-p value) t)))

(defmethod runtime-register-callable ((runtime process-runtime) name function)
  "Let this worker call FUNCTION back as NAME during its own execute.

Refused when the worker never advertised hostCalls, because a name
registered against a worker that will not call it would look available to
Core and silently never run."
  (unless (runtime-supports-callables-p runtime)
    (%runtime-fail "runtime" "~a does not accept the host callable ~a"
                   (type-of runtime) name))
  (unless (functionp function)
    (%runtime-fail "runtime" "the host callable ~a must be a function" name))
  (%with-runtime-lock ((%process-lock runtime))
    (setf (gethash (axllm/core::core-js-text name) (%process-callables runtime)) function))
  runtime)

(defun runtime-retire-callables (runtime)
  "Forget every host callable RUNTIME holds.

The run that registered them is over, so a later host_call naming one is
reaching for an invocation that no longer exists and is refused rather
than served against a stale closure."
  (when (typep runtime 'process-runtime)
    (%with-runtime-lock ((%process-lock runtime))
      (clrhash (%process-callables runtime))))
  runtime)

(defmethod runtime-usage-instructions ((runtime process-runtime))
  "The worker's own prompt guidance, or \"\" when it reports none.

A worker that cannot answer `capabilities' is not a failure here: the
prompt simply carries no runtime instructions, which is what a runtime
without them does."
  (handler-case
      (let ((result (%protocol-result (%protocol-request runtime "capabilities" nil nil))))
        (axllm/core::core-js-text (jget result "usage_instructions" "")))
    (runtime-protocol-error () "")))

(defmethod runtime-create-session ((runtime process-runtime) globals options)
  (let* ((payload (object "globals" (or globals (%new-object))
                          "options" (or options (%new-object))))
         (names (let ((out (%new-array)))
                  (maphash (lambda (name function)
                             (declare (ignore function))
                             (vector-push-extend name out))
                           (%process-callables runtime))
                  (sort out #'string<)))
         (response (progn
                     ;; A worker can only call back into names it knows about,
                     ;; so the session is told which ones exist. Sent only when
                     ;; there are any, so a worker without the extension sees
                     ;; the payload it always saw.
                     (when (plusp (length names))
                       (%set-key payload "host_calls" names))
                     (%protocol-request runtime "create_session" nil payload)))
         (direct (jget response "session_id"))
         (result (jget response "result"))
         (id (cond ((not (eq direct :null)) direct)
                   ((hash-table-p result) (jget result "session_id"))
                   (t :null))))
    (when (or (eq id :null) (and (stringp id) (zerop (length id))))
      (%runtime-fail "protocol" "runtime protocol did not return a session_id"))
    (make-instance 'process-session
                   :runtime runtime
                   :id (axllm/core::core-js-text id))))

(defmethod runtime-shutdown ((runtime process-runtime))
  "Ask the worker to stop, then make sure it has.

A worker that ignores the request, or that already died, must not be left
behind as an orphan holding the agent's file descriptors."
  (unless (%process-dead-reason runtime)
    (ignore-errors (%protocol-request runtime "shutdown" nil nil))
    (%kill-process-runtime runtime "runtime protocol worker was shut down"))
  (object "shutdown" 'yason:true))

(defmethod session-closed-p ((session process-session))
  (or (%process-session-closed session)
      (and (%process-dead-reason (process-session-runtime session)) t)))

(defun %session-request (session op payload)
  (%protocol-request (process-session-runtime session) op (process-session-id session) payload))

(defmethod session-execute ((session process-session) code options)
  (if (session-closed-p session)
      (envelope-session-closed)
      (handler-case
          (%protocol-result
           (%session-request session "execute"
                             (object "code" (axllm/core::core-js-text code)
                                     "options" (or options (%new-object)))))
        (runtime-protocol-error (condition)
          (envelope-error (ax-error-message condition)
                          (runtime-protocol-error-category condition))))))

(defmethod session-inspect-globals ((session process-session) options)
  (%protocol-result (%session-request session "inspect_globals" (or options (%new-object)))))

(defmethod session-snapshot-globals ((session process-session) options)
  (%protocol-result (%session-request session "snapshot_globals" (or options (%new-object)))))

(defmethod session-patch-globals ((session process-session) snapshot options)
  (%protocol-result
   (%session-request session "patch_globals"
                     (object "globals" (or snapshot (%new-object))
                             "options" (or options (%new-object))))))

(defmethod session-close ((session process-session))
  (if (%process-session-closed session)
      (object "closed" 'yason:true)
      (let ((result (handler-case (%protocol-result (%session-request session "close" nil))
                      (runtime-protocol-error () (object "closed" 'yason:true)))))
        (setf (%process-session-closed session) t)
        result)))

;;; ------------------------------------------------------------------
;;; Run control
;;; ------------------------------------------------------------------
;;;
;;; A run control is the caller's handle on a run in progress: it hears the
;;; lifecycle, it can stop the run, and it can steer the next stage. Core
;;; drives all three, so the object is a host boundary rather than policy --
;;; which path an event carries, and when a steering update is applied, are
;;; Core's decisions.
;;;
;;; Cancellation is cooperative on purpose. Core asks CORE-RUN-CONTROL-ABORTED
;;; between turns and between stages, so a stopped run unwinds at a boundary
;;; with its session closed and its logs intact. Interrupting a thread would
;;; leave a half-written action log and an orphaned runtime worker, which is
;;; the thing the process runtime's own cleanup exists to avoid.

(defparameter +run-control-thinking-levels+
  '("none" "minimal" "low" "medium" "high" "highest")
  "The thinking budgets a run control may ask a provider for.

The list is the provider contract, so a level outside it is refused here
rather than sent on to be ignored: a caller who asked for more thinking and
silently got the default has been told something untrue.")

(defparameter +run-control-root-path+ "root"
  "The path an update targets when the caller names none.

A root update reaches the whole run, which is what an unscoped steer means.")

(defclass run-control ()
  ((events :initform (%new-array) :reader run-control-events
           :documentation "Every lifecycle event, in the order it arrived.")
   (aborted :initform nil :reader run-control-aborted-p)
   (reason :initform :null :reader run-control-reason)
   (pending :initform '() :accessor %control-pending)
   (counter :initform 0 :accessor %control-counter
            :documentation "The last update id handed out. Ids are monotonic
because a queued or applied event names the update it refers to, and a
caller matching those events needs the id to be stable and unique.")
   (listener :initarg :listener :initform nil :reader %control-listener)
   (lock :initform (%make-runtime-lock "ax-run-control") :reader %control-lock))
  (:documentation
   "A caller's handle on one run.

Pass it as the run's \"control\" option. It collects the lifecycle events
Core and the agent emit, it stops the run at the next boundary when
RUN-CONTROL-ABORT is called, and it carries steering updates to the next
stage. A listener, if given, sees each event as it happens."))

(defun make-run-control (&key listener)
  "A run control. LISTENER, when given, is called with each event."
  (make-instance 'run-control :listener listener))

(defun run-control-abort (control &optional (reason "aborted by the caller"))
  "Stop CONTROL's run at the next stage or turn boundary.

Returns true the first time and false afterwards: a run can only be
stopped once, and a second call must not overwrite the first reason. The
first one announces itself at the root path, so a caller watching the
lifecycle learns the run ended even though nothing has failed."
  (check-type control run-control)
  (let ((first (%with-runtime-lock ((%control-lock control))
                 (if (run-control-aborted-p control)
                     nil
                     (progn (setf (slot-value control 'aborted) t
                                  (slot-value control 'reason) (axllm/core::core-js-text reason))
                            t)))))
    (when first
      (run-control-emit control (object "type" "aborted" "path" +run-control-root-path+)))
    first))

(defun %control-enqueue (control update target)
  "Queue UPDATE for TARGET and return it, with its id filled in.

The id and the target are the caller's handle on one update: an update
with neither could not be matched to the queued and applied events it
causes, and could not be scoped to a stage.

An aborted run refuses the update rather than queueing it. The run will
never reach another stage boundary, so a queued update would wait for a
consumer that is not coming, and the caller would be told their steering
landed when nothing will ever read it.

Queueing announces itself, because the queued event is how a caller
learns the update was accepted and which id to watch for when it is
applied. The announcement happens after the queue lock is released, so a
listener is never called with the control locked."
  (let ((queued (axllm/core::core-map-merge update (object))))
    (%with-runtime-lock ((%control-lock control))
      (when (run-control-aborted-p control)
        (error 'ax-error :message "run control is aborted: it takes no further updates"))
      (%set-key queued "id" (incf (%control-counter control)))
      (unless (axllm/core::core-true-p (axllm/core::core-map-contains queued "target"))
        (%set-key queued "target" target))
      (setf (%control-pending control) (append (%control-pending control) (list queued))))
    (run-control-emit control
                      (object "type" "queued"
                              "path" (axllm/core::core-get queued "target" target)
                              "updateId" (axllm/core::core-get queued "id" :null)))
    queued))

(defun run-control-enqueue (control update &key (target +run-control-root-path+))
  "Queue a prepared UPDATE object for TARGET.

The low-level entry point, for an update shape Core understands and this
file does not need to: it fills in the id and the target and nothing else."
  (check-type control run-control)
  (unless (hash-table-p update)
    (error 'ax-error :message "run-control-enqueue: update must be a JSON object"))
  (%control-enqueue control update (axllm/core::core-js-text target)))

(defun run-control-steer (control text &key (target +run-control-root-path+))
  "Steer CONTROL's run with TEXT, for TARGET and everything under it.

Empty steering is refused rather than queued: an update that says nothing
would still consume its turn and produce a queued and an applied event, so
a caller would be told their guidance landed when it did not."
  (check-type control run-control)
  (let ((text (axllm/core::core-js-text text)))
    (when (zerop (length (axllm/core::core-string-trim text)))
      (error 'ax-error :message "run-control-steer: steering text must not be empty"))
    (%control-enqueue control (object "type" "steer" "text" text)
                      (axllm/core::core-js-text target))))

(defun run-control-set-thinking-token-budget (control level
                                              &key (target +run-control-root-path+))
  "Ask CONTROL's run for thinking budget LEVEL, for TARGET and below.

LEVEL must be one the provider contract names; anything else is refused
here, because a budget the provider drops is worse than one never asked
for."
  (check-type control run-control)
  (let ((level (axllm/core::core-js-text level)))
    (unless (member level +run-control-thinking-levels+ :test #'string=)
      (error 'ax-error
             :message (format nil "run-control-set-thinking-token-budget: level must be one of ~{~a~^, ~}, got ~s"
                              +run-control-thinking-levels+ level)))
    (%control-enqueue control (object "type" "thinking" "level" level)
                      (axllm/core::core-js-text target))))

(defun run-control-emit (control event)
  "Record EVENT on CONTROL and hand it to the listener.

A listener that signals is ignored: a caller watching a run must not be
able to fail it."
  (check-type control run-control)
  (%with-runtime-lock ((%control-lock control))
    (vector-push-extend event (run-control-events control)))
  (let ((listener (%control-listener control)))
    (when (functionp listener)
      (handler-case (funcall listener event) (error () nil))))
  event)

(defun %update-reaches-p (update path)
  "Whether UPDATE's target reaches PATH.

Core owns the rule, so the match is read from it rather than copied here:
an update reaches its own target and the nodes under it and never reaches
back up, and the separator is part of the test, so \"root/left\" reaches
\"root/left/child\" but not \"root/leftover\". The provider boundary reads
the same Core function for the same decision, and two copies of a routing
rule that drifted apart would send one stage's steering to another."
  (let* ((target (axllm/core::core-get update "target" +run-control-root-path+))
         (target (if (or (eq target :null) (not (stringp target)))
                     +run-control-root-path+
                     target)))
    (axllm/core::core-true-p
     (axllm/core::chat-session-target-matches target (axllm/core::core-js-text path)))))

(defun run-control-take-pending (control &optional path)
  "CONTROL's queued updates for PATH, removed from the queue.

Without PATH the whole queue is taken, which is what a run with one stage
wants. With PATH only the updates that reach it are removed, so one
stage's turn cannot swallow a sibling's steering -- the scoping the
protocol promises, rather than a drain that happens to work when there is
only one consumer.

Taking is this host bridge's convenience, not the reference's own model:
there, a consumer reads with RUN-CONTROL-PENDING and remembers the last
id it saw, so a root update stays readable for a descendant that has not
run yet. A consumer that may share a run with another stage should read
by cursor for that reason; taking is for a single consumer that wants the
queue emptied behind it."
  (check-type control run-control)
  (%with-runtime-lock ((%control-lock control))
    (let ((pending (%control-pending control)))
      (if (null path)
          (progn (setf (%control-pending control) '())
                 (coerce pending 'vector))
          (let ((path (axllm/core::core-js-text path))
                (taken '())
                (kept '()))
            (dolist (update pending)
              (if (%update-reaches-p update path)
                  (push update taken)
                  (push update kept)))
            (setf (%control-pending control) (nreverse kept))
            (coerce (nreverse taken) 'vector))))))

(defun run-control-pending (control &key path (after 0))
  "CONTROL's queued updates for PATH with an id above AFTER, left in place.

A reader rather than a taker: a caller deciding whether to wait, or a
boundary looking ahead, must be able to see the queue without consuming
it."
  (check-type control run-control)
  (%with-runtime-lock ((%control-lock control))
    (let ((out (%new-array))
          (path (and path (axllm/core::core-js-text path))))
      (dolist (update (%control-pending control))
        (let ((id (axllm/core::core-get update "id" 0)))
          (when (and (or (null path) (%update-reaches-p update path))
                     (or (not (realp id)) (> id after)))
            (vector-push-extend update out))))
      out)))

(defun run-control-pending-count (control &optional path)
  "How many updates CONTROL is holding for PATH, consuming nothing."
  (check-type control run-control)
  (%with-runtime-lock ((%control-lock control))
    (if (null path)
        (length (%control-pending control))
        (count-if (lambda (update) (%update-reaches-p update (axllm/core::core-js-text path)))
                  (%control-pending control)))))

(defmethod axllm/core::core-host-get ((target run-control) key &optional (fallback :null))
  (let ((key (axllm/core::core-js-text key)))
    (cond ((string= key "aborted") (json-boolean (run-control-aborted-p target)))
          ((string= key "reason") (run-control-reason target))
          ((string= key "events") (run-control-events target))
          ((or (string= key "pending_count") (string= key "pendingCount"))
           (run-control-pending-count target))
          (t fallback))))

(defmethod axllm/core::core-host-call ((target run-control) method args)
  (let ((method (axllm/core::core-js-text method)))
    (cond ;; Core spells it "emit" and gen spells it "_emit"; both mean the
          ;; same thing to a control, and refusing one of them would make a
          ;; nested program's lifecycle silently vanish.
          ((or (string= method "emit") (string= method "_emit"))
           (run-control-emit target (%host-arg args 0)))
          ((string= method "abort")
           (json-boolean (run-control-abort target
                                            (let ((reason (%host-arg args 0)))
                                              (if (eq reason :null) "aborted by the caller" reason)))))
          ((or (string= method "take_pending") (string= method "takePending"))
           (let ((path (%host-arg args 0)))
             (run-control-take-pending target (unless (eq path :null) path))))
          ((string= method "pending")
           (let ((path (%host-arg args 0))
                 (after (%host-arg args 1 0)))
             (run-control-pending target
                                  :path (unless (eq path :null) path)
                                  :after (if (realp after) after 0))))
          ((or (string= method "pending_count") (string= method "pendingCount"))
           (let ((path (%host-arg args 0)))
             (run-control-pending-count target (unless (eq path :null) path))))
          ((string= method "steer")
           (let ((given (%host-arg args 0))
                 (path (%host-arg args 1)))
             ;; Core and a provider boundary both hand back a whole update
             ;; object when they re-queue one; a caller steering by hand
             ;; passes the text.
             (if (hash-table-p given)
                 (run-control-enqueue target given
                                      :target (if (eq path :null)
                                                  (axllm/core::core-get given "target"
                                                                        +run-control-root-path+)
                                                  path))
                 (run-control-steer target given
                                    :target (if (eq path :null) +run-control-root-path+ path)))))
          ((or (string= method "set_thinking_token_budget")
               (string= method "setThinkingTokenBudget"))
           (let ((path (%host-arg args 1)))
             (run-control-set-thinking-token-budget
              target (%host-arg args 0)
              :target (if (eq path :null) +run-control-root-path+ path))))
          ((string= method "enqueue")
           (run-control-enqueue target (%host-arg args 0)))
          (t (%runtime-fail "runtime" "unknown run control host method: ~a" method)))))

(defclass agent-control-scope ()
  ((control :initarg :control :reader %scope-control)
   (path :initarg :path :reader %scope-path)
   (cursor :initform 0 :accessor %scope-cursor)
   (interrupted :initform nil :accessor %scope-interrupted))
  (:documentation "One agent stage's cursor over the caller's control updates.
Core owns path matching. Reading rather than draining lets a root update reach
each later stage once, without replaying it on the same stage's next turn."))

(defun %scope-pending (scope)
  (run-control-pending (%scope-control scope) :path (%scope-path scope)
                       :after (%scope-cursor scope)))

(defmethod axllm/core::core-host-get ((scope agent-control-scope) key &optional (fallback :null))
  (if (member key '("pending_count" "pendingCount") :test #'equal)
      (length (%scope-pending scope))
      (axllm/core::core-host-get (%scope-control scope) key fallback)))

(defmethod axllm/core::core-host-call ((scope agent-control-scope) method args)
  (cond
    ((member method '("take_pending" "takePending") :test #'equal)
     (let ((updates (%scope-pending scope)))
       (loop for update across updates
             do (setf (%scope-cursor scope) (max (%scope-cursor scope) (jget update "id" 0))))
       updates))
    ((and (member method '("emit" "_emit") :test #'equal)
          (%scope-interrupted scope)
          (equal (jget (%host-arg args 0) "type") "failed"))
     ;; A sink that exits nonlocally did not fail generation. The generator's
     ;; unwind notification is classified at this host streaming boundary.
     (run-control-emit (%scope-control scope)
                      (object "type" "aborted" "path" (%scope-path scope))))
    (t (axllm/core::core-host-call (%scope-control scope) method args))))

;;; ------------------------------------------------------------------
;;; The host-object bridge
;;; ------------------------------------------------------------------
;;;
;;; Generated Core code holds a runtime and a session as opaque values and
;;; reaches them through CORE-HOST-GET, CORE-HOST-SET and CORE-HOST-CALL,
;;; the generic functions declared beside the other Core boundaries in
;;; core-runtime.lisp. These methods are this file's half of that: they say
;;; what a runtime and a session answer, and nothing else in Core needs to
;;; know that either one is a CLOS object rather than a JSON map.

(defmethod axllm/core::core-host-get ((target code-runtime) key &optional (fallback :null))
  (let ((key (axllm/core::core-js-text key)))
    (cond ((string= key "language") (runtime-language target))
          ((or (string= key "usage_instructions") (string= key "usageInstructions"))
           (runtime-usage-instructions target))
          ((string= key "executable") (json-boolean (runtime-executable-p target)))
          ((or (string= key "supports_callables") (string= key "supportsCallables"))
           (json-boolean (runtime-supports-callables-p target)))
          (t fallback))))

(defmethod axllm/core::core-host-get ((target code-session) key &optional (fallback :null))
  (let ((key (axllm/core::core-js-text key)))
    (if (string= key "closed")
        (json-boolean (session-closed-p target))
        fallback)))

(defun %host-arg (args index &optional (fallback :null))
  (if (and (%array-p args) (< index (length args))) (aref args index) fallback))

(defmethod axllm/core::core-host-call ((target code-runtime) method args)
  (let ((method (axllm/core::core-js-text method)))
    (cond ((string= method "create_session")
           (runtime-create-session target (%host-arg args 0) (%host-arg args 1)))
          ((string= method "get_usage_instructions") (runtime-usage-instructions target))
          ((string= method "get_language") (runtime-language target))
          ((string= method "register_callable")
           (runtime-register-callable target
                                      (axllm/core::core-js-text (%host-arg args 0))
                                      (%host-arg args 1)))
          ((string= method "shutdown") (runtime-shutdown target))
          (t (%runtime-fail "runtime" "unknown runtime host method: ~a" method)))))

(defmethod axllm/core::core-host-call ((target code-session) method args)
  (let ((method (axllm/core::core-js-text method)))
    (cond ((string= method "execute")
           (session-execute target (%host-arg args 0 "") (%host-arg args 1)))
          ((string= method "inspect_globals") (session-inspect-globals target (%host-arg args 0)))
          ((string= method "snapshot_globals") (session-snapshot-globals target (%host-arg args 0)))
          ((string= method "patch_globals")
           (session-patch-globals target (%host-arg args 0) (%host-arg args 1)))
          ((string= method "export_state") (session-export-state target (%host-arg args 0)))
          ((string= method "restore_state")
           (session-restore-state target (%host-arg args 0) (%host-arg args 1)))
          ((string= method "close") (session-close target))
          (t (%runtime-fail "runtime" "unknown runtime session host method: ~a" method)))))

;;; ------------------------------------------------------------------
;;; The runtime intrinsics generated Core code calls
;;; ------------------------------------------------------------------
;;;
;;; One function per intrinsic.agent.runtime.* in ir/axcore/agent.axir. Each
;;; is a boundary: it checks that the host object can do what Core is about
;;; to ask, and performs no agent policy of its own.

(in-package #:axllm/core)

(defun core-agent-runtime-is-executable (runtime)
  "Whether RUNTIME can run code, as opposed to merely describing one.

Core uses this to tell a runtime descriptor -- the JSON `{\"language\":
\"Python\"}` an agent may be configured with -- from a host runtime that
can actually execute a step."
  (core-bool (axllm::runtime-executable-p runtime)))

(defun core-agent-runtime-create-session (runtime globals options)
  (unless (axllm::runtime-executable-p runtime)
    (axllm::%runtime-fail "runtime" "agent runtime does not implement the code-runtime protocol"))
  (let ((session (axllm::runtime-create-session runtime
                                               (if (hash-table-p globals) globals (core-new-map))
                                               (if (hash-table-p options) options (core-new-map)))))
    (when (null session)
      (axllm::%runtime-fail "runtime" "agent runtime returned no session"))
    session))

(defun core-agent-runtime-execute (session code options)
  (unless (typep session 'axllm::code-session)
    (axllm::%runtime-fail "session_closed" "agent code session is not active"))
  (axllm::session-execute session (core-js-text code)
                         (if (hash-table-p options) options (core-new-map))))

(defun core-agent-runtime-inspect (session options)
  (if (typep session 'axllm::code-session)
      (axllm::session-inspect-globals session (if (hash-table-p options) options (core-new-map)))
      "[runtime state inspection unavailable: no runtime session]"))

(defun core-agent-runtime-export-state (session options)
  (unless (typep session 'axllm::code-session)
    (axllm::%runtime-fail "unavailable" "no runtime session to export"))
  (axllm::session-export-state session (if (hash-table-p options) options (core-new-map))))

(defun core-agent-runtime-restore-state (session snapshot options)
  (unless (typep session 'axllm::code-session)
    (axllm::%runtime-fail "unavailable" "no runtime session to restore"))
  (axllm::session-restore-state session
                               (if (hash-table-p snapshot) snapshot (core-new-map))
                               (if (hash-table-p options) options (core-new-map))))

(defun core-agent-runtime-close (session)
  (if (typep session 'axllm::code-session)
      (let ((result (axllm::session-close session)))
        (if (hash-table-p result) result (axllm::object "closed" 'yason:true)))
      (axllm::object "closed" 'yason:true)))

(defun core-run-control-aborted (control)
  "Whether CONTROL has been cancelled.

Core asks this before each actor step and before each stage, so a run can
be stopped between turns without the host interrupting a thread. A run
control is the caller's object: a JSON control reports cancellation under
\"aborted\", and a host control object answers the same key through the
CORE-HOST-GET bridge, which is what CORE-GET falls back to. No control at
all is not cancelled."
  (core-bool (and control
                  (not (eq control :null))
                  (core-true-p (core-get control "aborted" 'yason:false)))))

(defun core-agent-runtime-language (runtime)
  "RUNTIME's code language: a descriptor's own, else the host runtime's,
else JavaScript, which is the default every port shares."
  (let ((language (cond ((hash-table-p runtime) (core-get runtime "language"))
                        ((typep runtime 'axllm::code-runtime) (axllm::runtime-language runtime))
                        (t :null))))
    (let ((text (if (eq language :null) "" (core-string-trim language))))
      (if (plusp (length text)) text "JavaScript"))))

(defun core-agent-runtime-usage-instructions (runtime)
  "RUNTIME's own prompt guidance, or \"\" when it has none."
  (let ((text (cond ((hash-table-p runtime)
                     (core-coalesce (core-get runtime "usageInstructions")
                                    (core-get runtime "usage_instructions")))
                    ((typep runtime 'axllm::code-runtime)
                     (axllm::runtime-usage-instructions runtime))
                    (t :null))))
    (if (eq text :null) "" (core-js-text text))))

;;; ------------------------------------------------------------------
;;; The Docker session adapter
;;; ------------------------------------------------------------------

(in-package #:axllm)

;;; An optional host boundary, not a code runtime: a tool an agent can call to
;;; run a shell command inside a container, over the Docker Engine HTTP API.
;;; It is separate from CODE-RUNTIME on purpose. A code runtime runs the
;;; actor's own program and keeps a session's globals between steps; this runs
;;; one shell command and returns its output, and the container is the unit of
;;; reuse rather than a language scope.
;;;
;;; Dependency-bearing and opt-in. Nothing here starts a daemon, and nothing
;;; here is reached unless a caller builds a session and asks for it. The API
;;; endpoint is the caller's: a local socket proxy, a remote engine, or a test
;;; double. Every request goes through one transport closure, so a caller can
;;; point the session at any of those without this file knowing which.

(defparameter +docker-tag-label+ "com.example.tag"
  "The label Ax tags its containers with, so a session can find its own again.

The name matches the other ports exactly: a container tagged by one port
must be found by another, and changing the label would silently orphan
every container already running.")

(defparameter +default-docker-api-url+ "http://localhost:2375"
  "The Docker Engine HTTP endpoint used when a caller names none.")

(define-condition docker-error (ax-error) ()
  (:documentation "A Docker Engine request that failed, or a session with no container."))

(defun docker-fail (format-control &rest arguments)
  (error 'docker-error :message (apply #'format nil format-control arguments)))

(defclass docker-session ()
  ((api-url :initarg :api-url :initform +default-docker-api-url+ :reader docker-session-api-url)
   (container-id :initform nil :accessor docker-session-container-id
                 :documentation "The container this session is attached to, or NIL.")
   (transport :initarg :transport :reader %docker-transport
              :documentation "A function of (method url headers body) returning
\(values body status reason). The single place this file touches the network."))
  (:documentation
   "A session attached to one Docker container.

Create one with MAKE-DOCKER-SESSION, attach it to a container with
DOCKER-CREATE-CONTAINER, DOCKER-FIND-OR-CREATE-CONTAINER or
DOCKER-CONNECT-TO-CONTAINER, then run commands with
DOCKER-EXECUTE-COMMAND. DOCKER-SESSION-TOOL turns it into a tool an agent
can call."))

(defmethod print-object ((session docker-session) stream)
  (print-unreadable-object (session stream :type t)
    (format stream "~a~@[ container ~a~]"
            (docker-session-api-url session)
            (docker-session-container-id session))))

(defun %decode-docker-body (body)
  "A Docker response body as text, whether Drakma gave octets or a string."
  (cond ((stringp body) body)
        ((null body) "")
        ((and (vectorp body) (every (lambda (byte) (typep byte '(unsigned-byte 8))) body))
         (handler-case (sb-ext:octets-to-string (coerce body '(vector (unsigned-byte 8)))
                                                :external-format :utf-8)
           (error () (docker-fail "Docker response body was not valid UTF-8."))))
        (t (docker-fail "Unexpected Docker response body representation."))))

(defun make-docker-transport (&key (timeout 30))
  "The default bounded Docker HTTP transport.

Redirects are not followed and the timeout is enforced here, so a daemon
that stops answering cannot hang an agent run.

The URL arrives already percent-encoded, as it does in every other port,
so the request is sent with the URI preserved. Without that, Drakma
encodes it a second time and an image name like `alpine:3' reaches the
daemon as the literal text `alpine%3A3'."
  (lambda (method url headers body)
    (handler-case
        (sb-ext:with-timeout timeout
          (multiple-value-bind (response status)
              (drakma:http-request url
                                   :method method
                                   :preserve-uri t
                                   :additional-headers headers
                                   :content-type (if body "application/json" nil)
                                   :content body
                                   :external-format-out :utf-8
                                   :external-format-in :utf-8
                                   :connection-timeout timeout
                                   :redirect nil
                                   :force-binary nil
                                   :want-stream nil)
            (values (%decode-docker-body response) status
                    (format nil "HTTP ~a" status))))
      (sb-ext:timeout ()
        (docker-fail "Docker request timed out after ~a second(s)." timeout))
      (docker-error (condition) (error condition))
      (error (condition)
        (docker-fail "Docker request failed: ~a"
                     (substitute #\Space #\Newline (princ-to-string condition)))))))

(defun make-docker-session (&key (api-url +default-docker-api-url+) transport (timeout 30))
  "A Docker session against API-URL.

TRANSPORT replaces the HTTP layer with a function of (method url headers
body) returning (values body status reason); this is how a test drives the
adapter against a loopback double instead of a daemon."
  (make-instance 'docker-session
                 :api-url (axllm/core::core-js-text api-url)
                 :transport (or transport (make-docker-transport :timeout timeout))))

(defun %docker-url (session endpoint)
  (let ((base (docker-session-api-url session)))
    (concatenate 'string
                 (string-right-trim "/" base)
                 (if (and (plusp (length endpoint)) (char= (char endpoint 0) #\/))
                     endpoint
                     (concatenate 'string "/" endpoint)))))

(defun %docker-request (session endpoint &key (method :get) body)
  "One Docker Engine request. Returns (values text status reason)."
  (funcall (%docker-transport session)
           method
           (%docker-url session endpoint)
           (if body (list (cons "content-type" "application/json")) '())
           body))

(defun %docker-ok-p (status)
  (and (integerp status) (<= 200 status 299)))

(defun %docker-json (text what)
  (handler-case (parse-json text)
    (ax-error (condition) (error condition))
    (error () (docker-fail "~a: Docker returned invalid JSON." what))))

(defun %docker-container (session what)
  (or (docker-session-container-id session)
      (docker-fail "~a: no container created or connected." what)))

(defun %url-encode (text)
  "TEXT percent-encoded for one query-string value."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets (axllm/core::core-js-text text)
                                                   :external-format :utf-8)
          for character = (code-char byte)
          do (if (or (alphanumericp character) (find character "-_.~"))
                 (write-char character out)
                 (format out "%~2,'0X" byte)))))

(defun docker-pull-image (session image-name)
  "Pull IMAGE-NAME and wait for the pull to finish."
  (check-type session docker-session)
  (multiple-value-bind (text status reason)
      (%docker-request session (format nil "/images/create?fromImage=~a" (%url-encode image-name))
                       :method :post)
    (declare (ignore text))
    (unless (%docker-ok-p status)
      (docker-fail "Failed to pull image: ~a" reason)))
  nil)

(defun docker-list-containers (session &optional all)
  "The daemon's containers, as a JSON array. ALL includes stopped ones."
  (check-type session docker-session)
  (multiple-value-bind (text status reason)
      (%docker-request session (format nil "/containers/json?all=~a" (if all "true" "false")))
    (unless (%docker-ok-p status)
      (docker-fail "Failed to list containers: ~a" reason))
    (%docker-json text "list containers")))

(defun docker-create-container (session &key image-name volumes do-not-pull-image tag)
  "Create a container from IMAGE-NAME and attach SESSION to it.

VOLUMES is a list of (host-path . container-path) conses. TAG labels the
container so DOCKER-FIND-OR-CREATE-CONTAINER can reuse it later. The image
is pulled first unless DO-NOT-PULL-IMAGE."
  (check-type session docker-session)
  (unless do-not-pull-image
    (docker-pull-image session image-name))
  (let ((binds (%new-array))
        (labels (object)))
    (dolist (volume volumes)
      (vector-push-extend (format nil "~a:~a" (car volume) (cdr volume)) binds))
    (when tag (%set-key labels +docker-tag-label+ (axllm/core::core-js-text tag)))
    (let ((config (object "Image" (axllm/core::core-js-text image-name)
                          "Tty" 'yason:true
                          "OpenStdin" 'yason:false
                          "AttachStdin" 'yason:false
                          "AttachStdout" 'yason:false
                          "AttachStderr" 'yason:false
                          "HostConfig" (object "Binds" binds)
                          "Labels" labels)))
      (multiple-value-bind (text status reason)
          (%docker-request session "/containers/create" :method :post :body (encode-json config))
        (unless (%docker-ok-p status)
          (docker-fail "Failed to create container: ~a" reason))
        (let ((data (%docker-json text "create container")))
          (let ((id (jget data "Id")))
            (when (eq id :null)
              (docker-fail "Docker created a container without an Id."))
            (setf (docker-session-container-id session) (axllm/core::core-js-text id)))
          data)))))

(defun docker-find-or-create-container (session &key image-name volumes do-not-pull-image tag)
  "Reuse a container labelled TAG, or create one.

Returns an object with \"Id\" and \"isNew\". When several containers carry
the tag one is chosen at random, as in every other port, so concurrent
agents spread across them instead of all crowding the first."
  (check-type session docker-session)
  (let ((matching (%new-array)))
    (loop for container across (docker-list-containers session t)
          do (let ((container-labels (jget container "Labels")))
               (when (and (hash-table-p container-labels)
                          (equal (axllm/core::core-js-text
                                  (jget container-labels +docker-tag-label+ ""))
                                 (axllm/core::core-js-text tag)))
                 (vector-push-extend container matching))))
    (if (plusp (length matching))
        (let* ((selected (aref matching (random (length matching))))
               (id (axllm/core::core-js-text (jget selected "Id"))))
          (docker-connect-to-container session id)
          (object "Id" id "isNew" 'yason:false))
        (let ((created (docker-create-container session
                                                :image-name image-name
                                                :volumes volumes
                                                :do-not-pull-image do-not-pull-image
                                                :tag tag)))
          (object "Id" (axllm/core::core-js-text (jget created "Id")) "isNew" 'yason:true)))))

(defun docker-connect-to-container (session container-id)
  "Attach SESSION to CONTAINER-ID, failing when the daemon does not know it."
  (check-type session docker-session)
  (multiple-value-bind (text status reason)
      (%docker-request session (format nil "/containers/~a/json"
                                       (axllm/core::core-js-text container-id)))
    (declare (ignore text))
    (unless (%docker-ok-p status)
      (docker-fail "Failed to connect to container: ~a" reason)))
  (setf (docker-session-container-id session) (axllm/core::core-js-text container-id))
  nil)

(defun docker-start-container (session)
  "Start the container SESSION is attached to."
  (check-type session docker-session)
  (let ((id (%docker-container session "start container")))
    (multiple-value-bind (text status reason)
        (%docker-request session (format nil "/containers/~a/start" id) :method :post)
      (declare (ignore text))
      (unless (%docker-ok-p status)
        (docker-fail "Failed to start container: ~a" reason))))
  nil)

(defun %docker-container-info (session container-id)
  (multiple-value-bind (text status reason)
      (%docker-request session (format nil "/containers/~a/json" container-id))
    (unless (%docker-ok-p status)
      (docker-fail "Failed to get container info: ~a" reason))
    (%docker-json text "container info")))

(defun %docker-container-status (info)
  (let ((state (jget info "State")))
    (if (hash-table-p state)
        (axllm/core::core-js-text (jget state "Status" ""))
        "")))

(defun %await-docker-running (session container-id timeout poll-interval)
  "Wait until CONTAINER-ID is running, or fail when TIMEOUT elapses."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (string= (%docker-container-status (%docker-container-info session container-id))
                     "running")
        (return-from %await-docker-running nil))
      (when (>= (get-internal-real-time) deadline)
        (docker-fail "Timeout waiting for container to start."))
      (sleep poll-interval))))

(defun docker-container-logs (session)
  "The container's combined standard output and error, as text."
  (check-type session docker-session)
  (let ((id (%docker-container session "container logs")))
    (multiple-value-bind (text status reason)
        (%docker-request session (format nil "/containers/~a/logs?stdout=true&stderr=true" id))
      (unless (%docker-ok-p status)
        (docker-fail "Failed to read container logs: ~a" reason))
      text)))

(defun docker-execute-command (session command &key (start-timeout 30) (poll-interval 1))
  "Run COMMAND in the container through `sh -c' and return its output.

A container that is not running is started first and waited for, so a
reused container that has since stopped still answers."
  (check-type session docker-session)
  (let ((id (%docker-container session "execute command")))
    (unless (string= (%docker-container-status (%docker-container-info session id)) "running")
      (docker-start-container session)
      (%await-docker-running session id start-timeout poll-interval))
    (let ((create-body (encode-json
                        (object "Cmd" (vector "sh" "-c" (axllm/core::core-js-text command))
                                "AttachStdout" 'yason:true
                                "AttachStderr" 'yason:true))))
      (multiple-value-bind (text status reason)
          (%docker-request session (format nil "/containers/~a/exec" id)
                           :method :post :body create-body)
        (unless (%docker-ok-p status)
          (docker-fail "Failed to create exec instance: ~a" reason))
        (let ((exec-id (jget (%docker-json text "create exec") "Id")))
          (when (eq exec-id :null)
            (docker-fail "Docker created an exec instance without an Id."))
          (multiple-value-bind (output start-status start-reason)
              (%docker-request session (format nil "/exec/~a/start"
                                               (axllm/core::core-js-text exec-id))
                               :method :post
                               :body (encode-json (object "Detach" 'yason:false
                                                          "Tty" 'yason:false)))
            (unless (%docker-ok-p start-status)
              (docker-fail "Failed to start exec instance: ~a" start-reason))
            output))))))

(defun docker-stop-containers (session &key tag remove (timeout 10))
  "Stop the containers labelled TAG, and remove them when REMOVE.

Without TAG every container the daemon reports is a target, so a caller
that means to touch only its own must pass the tag it created them with.
A container that will not stop or will not be removed is skipped and left
out of the result rather than ending the sweep."
  (check-type session docker-session)
  (let ((results (%new-array)))
    (loop for container across (docker-list-containers session t)
          do (let* ((container-labels (jget container "Labels"))
                    (container-tag (and (hash-table-p container-labels)
                                        (jget container-labels +docker-tag-label+)))
                    (id (axllm/core::core-js-text (jget container "Id"))))
               (when (or (null tag)
                         (and container-tag
                              (not (eq container-tag :null))
                              (equal (axllm/core::core-js-text container-tag)
                                     (axllm/core::core-js-text tag))))
                 (let ((stopped t))
                   (when (string= (%docker-container-status container) "running")
                     (multiple-value-bind (text status reason)
                         (%docker-request session
                                          (format nil "/containers/~a/stop?t=~a" id timeout)
                                          :method :post)
                       (declare (ignore text reason))
                       (if (%docker-ok-p status)
                           (vector-push-extend (object "Id" id "Action" "stopped") results)
                           (setf stopped nil))))
                   (when (and stopped remove)
                     (multiple-value-bind (text status reason)
                         (%docker-request session (format nil "/containers/~a" id)
                                          :method :delete)
                       (declare (ignore text reason))
                       (when (%docker-ok-p status)
                         (vector-push-extend (object "Id" id "Action" "removed") results))))))))
    results))

(defun docker-session-tool (session &key (start-timeout 30) (poll-interval 1))
  "SESSION as a tool an agent can call to run shell commands.

The name and description match the other ports, so a prompt written
against one works against this one."
  (check-type session docker-session)
  (tool :name "commandExecution"
        :description "Use this function to execute shell commands, scripts, and programs. This function enables interaction with the file system, running system utilities, and performing tasks that require a shell interface."
        :parameters (object "type" "object"
                            "properties" (object "command"
                                                 (object "type" "string"
                                                         "description" "Shell command to execute. eg. `ls -l` or `echo \"Hello, World!\"`."))
                            "required" (vector "command"))
        :handler (lambda (arguments)
                   (docker-execute-command session (jget arguments "command")
                                           :start-timeout start-timeout
                                           :poll-interval poll-interval))))
