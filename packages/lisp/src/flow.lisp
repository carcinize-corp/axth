;;;; flow.lisp --- native AxFlow for the Common Lisp port.
;;;;
;;;; Scope: the whole AxFlow surface, carried by Core.  Nothing in this file
;;;; re-implements a graph algorithm: the execution plan, the barrier rules,
;;;; the parallel-group merge order, branch/while/feedback execution, the
;;;; cache key and lookup, `.returns()` projection, optimizer components and
;;;; the Mermaid parser/renderer all live in ir/axcore/flow.axir and reach
;;;; this file only through generated Core functions in AXLLM/CORE.
;;;;
;;;; What is native here, and only this:
;;;;
;;;;   * the FLOW object and its builder methods, which assemble the Core
;;;;     step records Core then owns;
;;;;   * FLOW-CALLABLE, the wrapper that lets a plain Lisp function be a
;;;;     map/derive/predicate node.  A callback never sees the live flow
;;;;     state: it is handed a clone, as the TypeScript and Python
;;;;     references do, so a mutating callback cannot corrupt the run;
;;;;   * the two `flow.*` native boundaries Core calls out to:
;;;;     CORE-FLOW-CACHING-FUNCTION and CORE-FLOW-DISPATCH-GROUP.  The
;;;;     second one is the real parallel group: one SBCL thread per node,
;;;;     isolated state, reports merged in plan order by Core, cancellation
;;;;     with a bounded drain on the first failure;
;;;;   * FORWARD for a flow, a method on the generic the generator defines.
;;;;
;;;; JSON values follow the package model exactly (see the AXLLM docstring):
;;;; objects are string-keyed EQUAL hash tables with key order, arrays are
;;;; vectors, null is :NULL, booleans are YASON:TRUE / YASON:FALSE.  NIL is
;;;; never a JSON value, and no function here produces one.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Errors
;;; ------------------------------------------------------------------

(define-condition flow-error (ax-error)
  ()
  (:documentation "A flow construction or execution failure raised natively.

Core's own flow failures arrive as AX-ERROR from CORE.RAISE; this condition
covers the native boundary: a bad builder argument, an unusable callable, or
a parallel group that could not be completed."))

(defun flow-fail (format-control &rest arguments)
  (error 'flow-error :message (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; JSON value cloning
;;; ------------------------------------------------------------------

(defun %flow-clone (value)
  "A deep copy of VALUE's JSON structure, sharing every host object.

Objects and arrays are copied, with key order preserved; strings, numbers,
booleans, :NULL and every non-JSON object (a program, a client, a run
control, a FLOW-CALLABLE, a function) are shared by identity.  That is the
distinction the parallel group and the callback boundary both need: state is
copied so two threads cannot see each other's writes, while a program is an
opaque handle that must stay the same object."
  (cond ((hash-table-p value)
         (let ((out (%new-object)))
           ;; Record keys first, in order, then the internal record marker,
           ;; which is a keyword and never part of the key order.
           (dolist (key (%object-keys value))
             (%set-key out key (%flow-clone (gethash key value))))
           (multiple-value-bind (marker found) (gethash :record value)
             (when found (setf (gethash :record out) marker)))
           out))
        ((and (vectorp value) (not (stringp value)))
         (let ((out (%new-array)))
           (loop for item across value do (vector-push-extend (%flow-clone item) out))
           out))
        (t value)))

(defun %flow-object (value &optional (what "value"))
  "VALUE as a JSON object, or a flow error naming WHAT."
  (cond ((hash-table-p value) value)
        ((eq value :null) (%new-object))
        ((null value) (%new-object))
        (t (flow-fail "~a must be a JSON object (a string-keyed hash table), got ~s" what value))))

(defun %flow-array (value &optional (what "value"))
  "VALUE as a JSON array: a vector, a list, or nothing."
  (cond ((and (vectorp value) (not (stringp value))) value)
        ((eq value :null) (%new-array))
        ((null value) (%new-array))
        ((consp value)
         (let ((out (%new-array)))
           (dolist (item value) (vector-push-extend item out))
           out))
        (t (flow-fail "~a must be a JSON array (a vector or a list), got ~s" what value))))

;;; ------------------------------------------------------------------
;;; Callables
;;; ------------------------------------------------------------------

(defclass flow-callable ()
  ((function :initarg :function :reader flow-callable-function)
   (name :initarg :name :initform nil :reader flow-callable-name))
  (:documentation
   "A plain Lisp function used as a flow node or predicate.

Core invokes it with the \"call\" method and one argument, the flow state.
The function receives a clone of that state, so it may mutate what it is
given without reaching the run's own state, exactly as the reference ports
hand their callbacks a copy."))

(defun flow-callable (function &key name)
  "Wrap FUNCTION as a flow map/derive node or branch/loop predicate.

FUNCTION takes one argument, the flow state as a JSON object, and returns a
JSON object (a map or derive node) or a value (a predicate)."
  (unless (or (functionp function) (and (symbolp function) (fboundp function)))
    (flow-fail "flow-callable: ~s is not a function" function))
  (make-instance 'flow-callable :function function :name name))

(defmethod print-object ((callable flow-callable) stream)
  (print-unreadable-object (callable stream :type t)
    (format stream "~@[~a~]" (flow-callable-name callable))))

;;; ------------------------------------------------------------------
;;; The flow object
;;; ------------------------------------------------------------------

(defclass flow ()
  ((state :initarg :state :accessor flow-state))
  (:documentation
   "An Ax flow: a graph of program, map, derive and control nodes.

The object holds exactly one thing, the Core flow record, and every
operation on it is a Core call.  FLOW-STATE is that record; it is the same
value Core reads and writes, so a builder method's effect is visible in it
immediately."))

(defun flow-p (value)
  (typep value 'flow))

(defparameter +flow-mermaid-literals+
  '(("mermaidPercent" . "%")
    ("mermaidOpenBrace" . "{")
    ("mermaidCloseBrace" . "}"))
  "Literal characters Core's Mermaid renderer reads from the flow record.

Core's string templates cannot carry a bare %, { or }, so the host supplies
them, as every other port does.")

(defun %flow-install-mermaid-literals (state)
  (dolist (entry +flow-mermaid-literals+)
    (axllm/core::core-set state (car entry) (cdr entry)))
  state)

(defun %flow-arguments (arguments)
  "FLOW's arguments as (values source bindings).

The lambda list is &REST rather than an optional followed by a keyword,
which is a style warning in Common Lisp and a build failure here.  A flow's
source is an options object or a Mermaid string, never a keyword, so the two
forms cannot be confused."
  (let ((source :null)
        (bindings :null))
    (when (and arguments (not (keywordp (first arguments))))
      (setf source (pop arguments)))
    (loop while arguments
          do (let ((key (pop arguments)))
               (unless arguments
                 (flow-fail "flow: option ~s has no value" key))
               (let ((value (pop arguments)))
                 (if (eq key :bindings)
                     (setf bindings value)
                     (flow-fail "flow: unknown option ~s; flow takes :bindings" key)))))
    (values source bindings)))

(defun flow (&rest arguments)
  "Create a flow.

With no argument, or with an options object, build an empty flow:

  (flow)
  (flow (object \"id\" \"qa.flow\"))

With a Mermaid document string, compile that document into a flow.  BINDINGS
supplies the document's nodes and conditions:

  (flow \"flowchart TD ...\"
        :bindings (object \"nodes\" (object \"qa\" (ax \"question:string -> answer:string\"))
                          \"conditions\" (object \"again\" (flow-callable #'done-p))))

A node binding may be a program, a signature string, or a FLOW-CALLABLE, in
which case the node becomes a map step.  A condition binding is a
FLOW-CALLABLE or a Core data predicate object."
  (multiple-value-bind (source bindings) (%flow-arguments arguments)
    (if (stringp source)
        (let* ((resolved (%flow-normalize-bindings bindings))
               (state (axllm/core::flow-from-mermaid source resolved))
               (object (make-instance 'flow :state (%flow-install-mermaid-literals state))))
          (%flow-hydrate-mermaid-steps (axllm/core::core-get state "steps") resolved)
          object)
        (let ((state (axllm/core::flow-factory (%flow-object source "flow options"))))
          (make-instance 'flow :state (%flow-install-mermaid-literals state))))))

(defmethod print-object ((object flow) stream)
  (print-unreadable-object (object stream :type t)
    (let ((state (flow-state object)))
      (format stream "~a ~a step(s)"
              (axllm/core::core-get state "program_id" "root.flow")
              (length (%flow-array (axllm/core::core-get state "steps")))))))

;;; ------------------------------------------------------------------
;;; Mermaid bindings
;;; ------------------------------------------------------------------

(defun %flow-normalize-bindings (bindings)
  "BINDINGS with every bare Lisp function wrapped as a FLOW-CALLABLE."
  (let* ((source (%flow-object bindings "flow bindings"))
         (out (%new-object)))
    (dolist (key (%object-keys source))
      (%set-key out key (gethash key source)))
    (dolist (section '("nodes" "conditions"))
      (let ((table (%flow-object (axllm/core::core-get out section) section))
            (normalized (%new-object)))
        (dolist (name (%object-keys table))
          (let ((value (gethash name table)))
            (%set-key normalized name
                      (if (functionp value) (flow-callable value :name name) value))))
        (%set-key out section normalized)))
    out))

(defun %flow-hydrate-mermaid-steps (steps bindings)
  "Replace each compiled step's placeholder program with its binding.

Core compiles a Mermaid document into steps that name their node and carry
the node's signature text; the host supplies the actual program.  A
FLOW-CALLABLE binding makes the node a map step, a string becomes a
generator, and a step Core left holding signature text becomes a generator
for that signature."
  (let ((nodes (%flow-object (axllm/core::core-get bindings "nodes") "mermaid node bindings")))
    (loop for step across (%flow-array steps)
          for name = (axllm/core::core-get step "name" "")
          for binding = (axllm/core::core-get nodes name)
          do (cond ((typep binding 'flow-callable)
                    (axllm/core::core-set step "kind" "map")
                    (axllm/core::core-set step "program" binding))
                   ((stringp binding)
                    (axllm/core::core-set step "program" (ax binding)))
                   ((not (eq binding :null))
                    (axllm/core::core-set step "program" binding))
                   (t
                    (let ((program (axllm/core::core-get step "program")))
                      (when (and (stringp program)
                                 (equal (axllm/core::core-get step "kind" "execute") "execute"))
                        (axllm/core::core-set step "program" (ax program))))))
             (let ((nested (axllm/core::core-get
                            (%flow-object (axllm/core::core-get step "options") "step options")
                            "steps")))
               (unless (eq nested :null)
                 (%flow-hydrate-mermaid-steps nested bindings))))
    steps))

;;; ------------------------------------------------------------------
;;; Step construction
;;; ------------------------------------------------------------------

(defun %flow-step-options (options)
  (%flow-clone (%flow-object options "step options")))

(defun %flow-program-signature-text (program)
  "PROGRAM's signature text, or NIL when it has none."
  (typecase program
    (generator (signature-string (generator-signature program)))
    (t nil)))

;;; Attaching a program to a node does not change the program's identity.
;;;
;;; An earlier version of this file renamed a nested flow that still carried
;;; Core's default id to root.<node>.  That was wrong on three counts, and
;;; the record is here so it is not reinvented:
;;;
;;;   * neither reference port does it.  Python's AxFlow never re-ids a
;;;     nested flow; its conformance runner names one root.<node> when it
;;;     *builds* it.  TypeScript's AxProgram.register does rename a child,
;;;     but unconditionally and to <parent id>.<child name>, which would make
;;;     the nested flow root.flow.nested, not the root.nested the shared
;;;     fixtures record.  So neither rule is the one the fixtures encode;
;;;   * it bought no correctness.  Core's PROGRAM-PREFIX-COMPONENT already
;;;     makes a child's component id <owner>.<node>::<child id>, so two
;;;     children carrying the same id never collide after prefixing;
;;;   * it could not be made honest and still satisfy the fixture.  Core's
;;;     default id is the constant root.flow, so the id alone cannot tell
;;;     "nobody chose one" from "the caller chose root.flow"; once the rule
;;;     respects an explicit id, a host that always passes one -- as the
;;;     optimizer's fixture runner does -- is correctly left alone, and the
;;;     expected nested id has to come from where it comes from in Python:
;;;     the host choosing root.<node> at construction.
;;;
;;; A nested flow's id is therefore the caller's, always.

(defun flow-step (kind name program &optional options)
  "Build one Core flow step record.

This is the step value a branch's or a loop's \"steps\" option takes, and the
value FLOW-EXECUTE and friends add to a flow.  KIND is one of \"execute\",
\"derive\", \"map\", \"branch\", \"while\", \"feedback\", \"parallel\" or
\"parallelMerge\"."
  (unless (stringp kind)
    (flow-fail "flow-step: kind must be a string, got ~s" kind))
  (unless (stringp name)
    (flow-fail "flow-step: name must be a string, got ~s" name))
  (let ((opts (%flow-step-options options))
        (node (cond ((null program) :null)
                    ((functionp program) (flow-callable program :name name))
                    (t program))))
    ;; As in the reference ports, an execute step records its program's
    ;; signature text so a Mermaid rendering can show it.
    (when (and (equal kind "execute")
               (not (axllm/core::core-true-p (axllm/core::core-map-contains opts "signatureText"))))
      (let ((text (%flow-program-signature-text node)))
        (when text (axllm/core::core-set opts "signatureText" text))))
    (axllm/core::flow-step kind name node opts)))

(defun %flow-add (flow kind name program options)
  (axllm/core::flow-add-step (flow-state flow) (flow-step kind name program options))
  flow)

(defun flow-execute (flow name program &optional options)
  "Add a program node: run PROGRAM with the flow state and spread its output.

The node writes {NAME}Result and, when PROGRAM has a signature, each of its
output fields, so a later node can read them directly."
  (when (null program)
    (flow-fail "flow-execute: step ~s needs a program" name))
  (%flow-add flow "execute" name program options))

(defun flow-derive (flow name mapper &optional options)
  "Add a derive node: map one state field, element-wise when it is an array.

MAPPER is a function or FLOW-CALLABLE.  It receives the state with the
current element under \"__item\" and returns a state carrying the derived
value under \"__derived\"."
  (%flow-add flow "derive" name mapper options))

(defun flow-map (flow name mapper &optional options)
  "Add a map node: transform the whole state with a Lisp function.

MAPPER receives a clone of the state and returns a JSON object, which is
written to {NAME}Result and merged into the state."
  (%flow-add flow "map" name mapper options))

(defun flow-branch (flow name predicate branches &optional options)
  "Add a branch node: run the branch whose \"when\" matches PREDICATE's value.

PREDICATE is a function, a FLOW-CALLABLE or a Core data predicate object.
BRANCHES is a sequence of objects with \"when\" and \"steps\" (steps built by
FLOW-STEP)."
  (let ((opts (%flow-step-options options)))
    (axllm/core::core-set opts "predicate" (%flow-predicate predicate))
    (axllm/core::core-set opts "branches" (%flow-branches branches))
    (axllm/core::flow-add-step (flow-state flow) (flow-step "branch" name nil opts))
    flow))

(defun %flow-predicate (predicate)
  (cond ((null predicate) :null)
        ((eq predicate :null) :null)
        ((functionp predicate) (flow-callable predicate))
        (t predicate)))

(defun %flow-branches (branches)
  (let ((out (%new-array)))
    (loop for branch across (%flow-array branches "flow branches")
          do (let ((entry (%new-object))
                   (source (%flow-object branch "flow branch")))
               (%set-key entry "when" (axllm/core::core-get source "when"))
               (%set-key entry "steps" (%flow-array (axllm/core::core-get source "steps")
                                                   "flow branch steps"))
               (vector-push-extend entry out)))
    out))

(defun flow-while (flow name condition steps &key (max-iterations 100) options)
  "Add a while node: run STEPS while CONDITION holds, at most MAX-ITERATIONS.

Exceeding MAX-ITERATIONS is an error, as in every other port."
  (let ((opts (%flow-step-options options)))
    (axllm/core::core-set opts "condition" (%flow-predicate condition))
    (axllm/core::core-set opts "steps" (%flow-array steps "flow while steps"))
    (axllm/core::core-set opts "maxIterations" max-iterations)
    (axllm/core::flow-add-step (flow-state flow) (flow-step "while" name nil opts))
    flow))

(defun flow-feedback (flow name condition steps &key (max-iterations 10) label options)
  "Add a feedback node: re-run STEPS while CONDITION holds, counting iterations.

Unlike a while node, reaching MAX-ITERATIONS stops the loop instead of
failing, and the iteration count is kept in the state under
_feedback_{LABEL}_iterations."
  (let ((opts (%flow-step-options options)))
    (axllm/core::core-set opts "condition" (%flow-predicate condition))
    (axllm/core::core-set opts "steps" (%flow-array steps "flow feedback steps"))
    (axllm/core::core-set opts "maxIterations" max-iterations)
    (axllm/core::core-set opts "label" (or label name))
    (axllm/core::flow-add-step (flow-state flow) (flow-step "feedback" name nil opts))
    flow))

(defun flow-parallel (flow name results &optional options)
  "Add an explicit parallel node holding RESULTS for a later merge node."
  (let ((opts (%flow-step-options options)))
    (axllm/core::core-set opts "parallelResults" (%flow-array results "flow parallel results"))
    (axllm/core::flow-add-step (flow-state flow) (flow-step "parallel" name nil opts))
    flow))

(defun flow-parallel-merge (flow name &optional options)
  "Add an explicit merge node that collects the preceding parallel results."
  (%flow-add flow "parallelMerge" name nil options))

(defun flow-node-extended (flow name base-signature &key extended-signature options)
  "Add a program node for BASE-SIGNATURE, or EXTENDED-SIGNATURE when given.

This is Ax's `nx`: one node whose signature is the base signature widened
with extra fields, usually an internal reasoning field."
  (let ((signature (or extended-signature base-signature)))
    (unless (stringp signature)
      (flow-fail "flow-node-extended: step ~s needs a signature string" name))
    (flow-execute flow name (ax signature) options)))

(defun flow-nx (flow name base-signature &key extended-signature options)
  "Alias for FLOW-NODE-EXTENDED, matching Ax's short form."
  (flow-node-extended flow name base-signature
                      :extended-signature extended-signature :options options))

(defun flow-returns (flow returns)
  "Project the final state through RETURNS: output key -> dotted state path."
  (axllm/core::flow-set-returns (flow-state flow) (%flow-object returns "flow returns"))
  flow)

;;; ------------------------------------------------------------------
;;; Demos
;;; ------------------------------------------------------------------

(defun flow-set-demos (flow demos)
  "Attach optimizer demos to the flow.

An array of demo records is validated against the flow's own program ids: a
record naming a node the flow does not have is an error rather than a demo
silently dropped.  An object maps node name to that node's demos and is
passed to each node's program."
  (let* ((state (flow-state flow))
         (steps (%flow-array (axllm/core::core-get state "steps"))))
    (cond
      ((and (vectorp demos) (not (stringp demos)))
       (let* ((owner (axllm/core::core-get state "program_id" "root.flow"))
              (known (list owner "root"))
              (unknown '()))
         (loop for step across steps
               for name = (axllm/core::core-get step "name" "")
               when (plusp (length name))
                 do (push (format nil "~a.~a" owner name) known)
                    (push (format nil "root.~a" name) known))
         (loop for demo across demos
               for id = (axllm/core::core-get (%flow-object demo "flow demo") "programId")
               do (when (and (stringp id) (not (member id known :test #'string=))
                             (not (member id unknown :test #'string=)))
                    (push id unknown)))
         (when unknown
           (flow-fail "Unknown program ID(s) in demos: ~{~a~^, ~}" (sort unknown #'string<)))
         (axllm/core::core-set state "demos" (%flow-clone demos))))
      (t
       (let ((table (%flow-object demos "flow demos")))
         (dolist (name (%object-keys table))
           (let ((step (find name steps :test #'equal
                                        :key (lambda (s) (axllm/core::core-get s "name" "")))))
             (unless step
               (flow-fail "unknown flow node in demos: ~a" name))
             (let ((program (axllm/core::core-get step "program")))
               (unless (eq program :null)
                 ;; The node's own demos go to the node's program, through the
                 ;; same generic an optimizer uses.
                 (program-set-demos program (gethash name table))))))
         (axllm/core::core-set state "demos" (%flow-clone table)))))
    flow))

;;; ------------------------------------------------------------------
;;; Inspection
;;; ------------------------------------------------------------------

(defun flow-plan (flow)
  "The flow's execution plan: total steps, parallel groups and each group."
  (axllm/core::flow-plan (flow-state flow)))

(defun flow-traces (flow)
  "The trace events the last run recorded, in order."
  (%flow-array (axllm/core::core-get (flow-state flow) "traces")))

(defun flow-chat-log (flow)
  "The last run's chat log, each entry named by the node that produced it."
  (%flow-array (axllm/core::core-get (flow-state flow) "chat_log")))

(defun flow-usage (flow)
  "The last run's token usage, by node name."
  (%flow-object (axllm/core::core-get (flow-state flow) "usage")))

(defun flow-components (flow)
  "The flow's optimizable components: its graph plan and each node's own."
  (axllm/core::flow-get-optimizable-components (flow-state flow)))

(defun flow-apply-components (flow component-map)
  "Apply an optimizer's COMPONENT-MAP to the flow and its nodes."
  (axllm/core::flow-apply-optimized-components
   (flow-state flow) (%flow-object component-map "component map"))
  flow)

;;; Core also carries @flow_snapshot_components, @flow_restore_components,
;;; @flow_evaluate_optimization and @flow_optimize_with.  This port
;;; deliberately does not expose them, and the reason is in those bodies
;;; rather than in taste:
;;;
;;;   flow_snapshot_components   = flow_get_optimizable_components
;;;                                then optimization_component_current_map
;;;   flow_restore_components    = flow_apply_optimized_components
;;;   flow_evaluate_optimization = normalize dataset, snapshot, apply, run,
;;;                                restore
;;;
;;; OPTIMIZE-PROGRAM already does exactly that through the program protocol:
;;; it snapshots with OPTIMIZATION-COMPONENT-CURRENT-MAP over
;;; PROGRAM-OPTIMIZABLE-COMPONENTS and restores with
;;; PROGRAM-APPLY-OPTIMIZED-COMPONENTS in an unwind-protect, which are the
;;; same two Core calls reached through the methods above.  So there is no
;;; flow state the single entry point misses -- ir/conformance/axoptimize's
;;; flow-evaluate-rollback is the fixture that would catch it if there were.
;;; A second path over the same Core functions would add no capability and
;;; two paths can disagree about a candidate map.
;;;
;;; This is reversible on one trigger: a caller or fixture that wants a
;;; standalone flow-side optimize, the way Python's AxFlow carries its own
;;; optimize_with.  That is an API question, not a correctness one, and thin
;;; wrappers over the four Core functions would be the answer.

(defun flow-mermaid (flow &optional options)
  "The flow as a Mermaid flowchart.

A flow compiled from a Mermaid document re-renders its own document, so
parsing and rendering round-trip; a flow built with the builder is rendered
from its steps."
  (axllm/core::flow-to-mermaid (flow-state flow) (%flow-object options "mermaid options")))

;;; ------------------------------------------------------------------
;;; Running
;;; ------------------------------------------------------------------

(defmethod forward ((program flow) client values &optional options)
  "Run the flow against CLIENT with VALUES, returning the projected output.

The cache is read before anything else, as the TypeScript reference does, so
a stored output records neither a trace nor a request.  OPTIONS is the call's
option object; it is merged over the flow's own options for each node, and
carries the run's abort flags, run control, trace label, caching function and
auto-parallel override.

Returns (values output usage), as every program's FORWARD does; a flow's
usage is keyed by node name."
  (let* ((state (flow-state program))
         (opts (%flow-clone (%flow-object options "forward options")))
         (input (%flow-object values "forward values"))
         (lookup (axllm/core::flow-cache-lookup-impl state input opts)))
    (if (axllm/core::core-true-p (axllm/core::core-get lookup "hit" 'yason:false))
        ;; A cache hit records neither a trace nor a request, so the usage it
        ;; reports is the usage the flow still holds: none for this call.
        (values (axllm/core::core-get lookup "value") (flow-usage program))
        (let ((control (axllm/core::core-get opts "control"))
              (path (%flow-run-path opts))
              (output nil))
          (axllm/core::core-set opts "_ax_flow_cache_lookup" lookup)
          ;; The flow's own lifecycle, at its own path. Each node reports at
          ;; <path>/<node>, which is the node's program's business, so the two
          ;; together describe the run without either repeating the other.
          (%flow-emit control "started" path nil)
          ;; The flow reports failed after the stack has unwound, so a node
          ;; that failed has already reported at its own path: the events read
          ;; outside-in on the way down and inside-out on the way back, which
          ;; is the order every port records.
          (handler-case
              (setf output (axllm/core::flow-forward state client input opts))
            (error (condition)
              (%flow-emit control "failed" path (princ-to-string condition))
              (error condition)))
          (%flow-emit control "completed" path nil)
          (values output (flow-usage program))))))

(defun %flow-emit (control type path error)
  "Report one flow lifecycle event to CONTROL, when the run has one."
  (unless (or (null control) (eq control :null))
    (let ((event (%new-object)))
      (%set-key event "type" type)
      (%set-key event "path" path)
      (when error (%set-key event "error" error))
      (axllm/core::core-host-call control "_emit" (vector event))))
  control)

(defun flow-streaming-forward (flow client values &optional options)
  "The flow's output as a one-element delta stream.

A flow has no partial output of its own: its nodes stream internally and the
flow yields one final delta, which is what the reference ports do."
  (let ((delta (%new-object))
        (out (%new-array)))
    (%set-key delta "version" 1)
    (%set-key delta "index" 0)
    (%set-key delta "delta" (forward flow client values options))
    (vector-push-extend delta out)
    out))

;;; ------------------------------------------------------------------
;;; Host methods Core and the other layers reach the flow through
;;; ------------------------------------------------------------------

(defmethod axllm/core::core-host-call ((target flow-callable) method args)
  "Invoke a user callback.

Core calls \"call\" with the flow state.  The callback is handed a clone, so
a callback that mutates what it receives cannot reach the run's state."
  (unless (equal method "call")
    (flow-fail "flow callable has no method ~a" method))
  (unless (plusp (length args))
    (flow-fail "flow callable \"call\" needs the flow state"))
  (let ((result (funcall (flow-callable-function target) (%flow-clone (aref args 0)))))
    (if (null result) (%new-object) result)))

;;; A map or derive node's program is a FLOW-CALLABLE, and Core walks every
;;; step when it collects or applies optimizable components.  A callback has
;;; no prompt and no signature, so it answers that it owns nothing rather
;;; than leaving the walk without an applicable method.

(defmethod program-signature ((program flow-callable))
  :null)

(defmethod program-optimizable-components ((program flow-callable))
  (%new-array))

(defmethod program-apply-optimized-components ((program flow-callable) component-map)
  (declare (ignore component-map))
  program)

(defmethod program-chat-log ((program flow-callable)) (%new-array))

(defmethod program-usage ((program flow-callable)) (%new-array))

(defmethod program-traces ((program flow-callable)) (%new-array))

(defmethod program-set-instruction ((program flow-callable) text)
  (declare (ignore text))
  program)

;;; A flow is an Ax program, so it answers the program generics the
;;; generator defines.  These are what Core's program/agent intrinsics reach,
;;; which is how a flow can be a node of another flow.

(defmethod program-signature ((program flow))
  "A flow declares no signature of its own, so a node that runs one is a
barrier: Core cannot infer what it reads or writes."
  :null)

(defmethod program-chat-log ((program flow))
  (flow-chat-log program))

(defmethod program-usage ((program flow))
  (flow-usage program))

(defmethod program-traces ((program flow))
  (flow-traces program))

(defmethod program-optimizable-components ((program flow))
  (flow-components program))

(defmethod program-apply-optimized-components ((program flow) component-map)
  (flow-apply-components program component-map)
  program)

(defmethod program-kind ((program flow))
  "The kind Core itself recorded, so the host cannot disagree with the record."
  (axllm/core::core-get (flow-state program) "program_kind" "axflow"))

(defmethod program-set-demos ((program flow) demos)
  (flow-set-demos program demos)
  program)

(defun %flow-program-nodes (flow)
  "FLOW's steps whose program is an Ax program rather than a callback.

A map, derive, branch or loop node runs a Lisp callback and makes no provider
or tool call, so it can hide none; only these nodes can."
  (loop for step across (%flow-array (axllm/core::core-get (flow-state flow) "steps"))
        for program = (axllm/core::core-get step "program")
        unless (or (eq program :null) (typep program 'flow-callable))
          collect (cons (axllm/core::core-get step "name" "") program)))

(defmethod program-function-calls ((program flow))
  "Every call the flow's nodes made during the last run, in node order.

A node's calls are its own, so they are taken from the node and tagged with
the node that made them; a nested flow contributes its nodes' calls the same
way.  A node that cannot report its calls is named rather than counted as
having made none, because a silently short history would mis-score every
action-adjusted result."
  (let ((out (%new-array)))
    (loop for (name . node) in (%flow-program-nodes program)
          do (unless (compute-applicable-methods #'program-function-calls (list node))
               (flow-fail "flow node ~a (~a) cannot report the calls it made; implement PROGRAM-FUNCTION-CALLS for it"
                          name (type-of node)))
             (loop for call across (%flow-array (program-function-calls node))
                   do (let ((entry (%flow-clone call)))
                        (when (and (hash-table-p entry)
                                   (not (nth-value 1 (gethash "node" entry))))
                          (%set-key entry "node" name))
                        (vector-push-extend entry out))))
    out))

(defmethod program-set-instruction ((program flow) text)
  "A flow has no prompt of its own; the instruction goes to each node.

A node whose program does not take an instruction is left alone, so a flow of
map and derive nodes is not an error."
  (loop for step across (%flow-array (axllm/core::core-get (flow-state program) "steps"))
        for node = (axllm/core::core-get step "program")
        do (unless (eq node :null)
             (handler-case (program-set-instruction node text)
               (error () nil))))
  program)

(defmethod axllm/core::core-host-call ((target flow) method args)
  "The flow methods Core reaches by name rather than through a generic."
  (cond ((equal method "forward")
         (forward target (aref args 0) (aref args 1)
                  (if (> (length args) 2) (aref args 2) nil)))
        ;; A flow has no signature of its own, so a step that runs one is a
        ;; barrier: Core cannot infer what it reads or writes.
        ((equal method "signature") :null)
        ((equal method "set_demos") (flow-set-demos target (aref args 0)) target)
        ((or (equal method "owned_worker_factory") (equal method "ownedWorkerFactory"))
         (%flow-owned-factory target))
        ((equal method "to_string") (flow-mermaid target))
        (t (flow-fail "flow has no method ~a" method))))

;;; ------------------------------------------------------------------
;;; flow.* native boundary: the caching function
;;; ------------------------------------------------------------------

;;; A caching function takes a key and an optional value: called with the key
;;; alone it reads, returning the stored value or :NULL; called with a value
;;; it writes.  A flow's constructor takes none, matching the reference ports:
;;; the function comes from the call's options, else from Ax's own
;;; "cachingFunction" global.  There is no second flow-only global.

(defun axllm/core::core-flow-caching-function (options)
  "The caching function for this run: the call's, else Ax's global."
  (let ((opts (if (hash-table-p options) options (%new-object))))
    (dolist (key '("cachingFunction" "caching_function"))
      (let ((value (axllm/core::core-get opts key)))
        (unless (eq value :null)
          (return-from axllm/core::core-flow-caching-function value))))
    (get-global "cachingFunction")))

;;; ------------------------------------------------------------------
;;; flow.* native boundary: the parallel group
;;; ------------------------------------------------------------------

(defclass flow-cancellation ()
  ((cancelled :initform nil :accessor flow-cancellation-cancelled-p)
   (reason :initform :null :accessor flow-cancellation-reason)
   (lock :initform (sb-thread:make-mutex :name "flow-cancellation")
         :reader flow-cancellation-lock))
  (:documentation
   "The cancellation a dispatched flow node is given.

The group cancels every sibling as soon as one node fails or the parent run
is stopped, so a long-running node is not left to finish work whose result is
already discarded."))

(defun flow-cancel (token &optional (reason "Flow group cancelled"))
  (sb-thread:with-mutex ((flow-cancellation-lock token))
    (unless (flow-cancellation-cancelled-p token)
      (setf (flow-cancellation-cancelled-p token) t
            (flow-cancellation-reason token) reason)))
  token)

(defun flow-cancelled-p (token)
  (and (typep token 'flow-cancellation)
       (sb-thread:with-mutex ((flow-cancellation-lock token))
         (flow-cancellation-cancelled-p token))))

(defmethod axllm/core::core-host-call ((target flow-cancellation) method args)
  (cond ((equal method "cancel")
         (flow-cancel target (if (plusp (length args)) (aref args 0) "Flow group cancelled")))
        ((equal method "cancelled") (axllm/core::core-bool (flow-cancelled-p target)))
        (t (flow-fail "flow cancellation has no method ~a" method))))

(defmethod axllm/core::core-host-get ((target flow-cancellation) key &optional (fallback :null))
  (cond ((equal key "cancelled") (axllm/core::core-bool (flow-cancelled-p target)))
        ((equal key "reason") (flow-cancellation-reason target))
        (t fallback)))

(defparameter +flow-group-drain-seconds+ 0.1d0
  "How long a cancelled group waits for its remaining nodes to settle.

A node that has not reported by then is recorded as cancelled with its path,
so a stuck node produces a named failure rather than a hang.")

(defun %flow-host-call-safe (target method &rest args)
  "Call TARGET's METHOD, or return :NULL when it has no such method.

A program or client that does not offer owned workers is the ordinary case,
not an error: the group falls back to running its nodes one after another."
  (handler-case (axllm/core::core-host-call target method (coerce args 'vector))
    (error () :null)))

(defun %flow-owned-factory (object)
  "A factory that produces an owned copy of OBJECT, or :NULL.

A flow can be cloned for a worker only when every one of its program nodes
can be, because a worker that shared a program with the parent would write
the parent's conversation state from another thread."
  (if (typep object 'flow)
      (let ((factories '()))
        (loop for step across (%flow-array (axllm/core::core-get (flow-state object) "steps"))
              for index from 0
              for program = (axllm/core::core-get step "program")
              do (unless (eq program :null)
                   (let ((factory (%flow-host-call-safe program "owned_worker_factory")))
                     (when (eq factory :null) (return-from %flow-owned-factory :null))
                     (push (cons index factory) factories))))
        (let ((snapshot (flow-state object)))
          (lambda ()
            (let* ((state (%flow-clone snapshot))
                   (owned (make-instance 'flow :state state))
                   (steps (%flow-array (axllm/core::core-get state "steps"))))
              (dolist (entry factories)
                (axllm/core::core-set (aref steps (car entry)) "program" (funcall (cdr entry))))
              owned))))
      (%flow-host-call-safe object "owned_worker_factory")))

(defstruct (flow-task (:constructor %make-flow-task))
  position
  index
  plan
  step
  program
  client
  cancellation
  (report nil))

(defun %flow-group-tasks (flow client plans)
  "The tasks for this group, or NIL when it cannot run on worker threads.

Every node in the group must be a program node whose program and whose
client can both be cloned; anything else runs serially so the fallback is a
real execution path, not a silent degradation."
  (let ((client-factory (%flow-host-call-safe client "owned_worker_factory"))
        (steps (%flow-array (axllm/core::core-get flow "steps")))
        (tasks '()))
    (when (eq client-factory :null)
      (return-from %flow-group-tasks nil))
    (loop for plan across (%flow-array plans)
          for position from 0
          do (let* ((index (round (axllm/core::core-get plan "stepIndex" 0)))
                    (step (if (< -1 index (length steps)) (aref steps index) :null)))
               (unless (and (hash-table-p step)
                            (equal (axllm/core::core-get step "kind" "execute") "execute"))
                 (return-from %flow-group-tasks nil))
               (let ((program-factory (%flow-host-call-safe
                                       (axllm/core::core-get step "program")
                                       "owned_worker_factory")))
                 (when (eq program-factory :null)
                   (return-from %flow-group-tasks nil))
                 (push (%make-flow-task :position position
                                        :index index
                                        :plan plan
                                        :step step
                                        :program (funcall program-factory)
                                        :client (funcall client-factory)
                                        :cancellation (make-instance 'flow-cancellation))
                       tasks))))
    (nreverse tasks)))

(defun %flow-worker-step (step program)
  "STEP cloned for a worker, with PROGRAM as its own owned program.

Programs belonging to other nodes are opaque handles the worker never runs,
so only the selected node's program is replaced."
  (let ((out (%flow-clone step)))
    (axllm/core::core-set out "program" program)
    out))

(defun %flow-worker-flow (flow index program)
  (let* ((out (%flow-clone flow))
         (steps (%flow-array (axllm/core::core-get out "steps"))))
    (when (< -1 index (length steps))
      (axllm/core::core-set (aref steps index) "program" program))
    out))

(defun %flow-worker-options (options cancellation)
  (let ((out (%flow-clone (if (hash-table-p options) options (%new-object)))))
    (axllm/core::core-set out "cancellation" cancellation)
    out))

(defun %flow-error-report (message)
  (let ((report (%new-object)))
    (%set-key report "error" message)
    (%set-key report "traces" (%new-array))
    (%set-key report "chat_log" (%new-array))
    (%set-key report "usage" (%new-object))
    report))

(defun axllm/core::core-flow-dispatch-group (flow client plans state options)
  "Run one parallel group's nodes on SBCL threads, in isolation.

Each node gets its own thread, its own owned program and client, its own
clone of the group's starting state and its own cancellation.  Reports come
back in plan order, so Core merges them in step order and the result does not
depend on which thread finished first.

Returns :NULL when the group cannot be dispatched -- a non-program node, or a
program or client that cannot be cloned -- and Core then runs the group
serially, recording that it did.

The first failure cancels the remaining nodes and the group waits a bounded
drain for them; a node that has not reported by then is recorded as cancelled
with its own path, so a group never hangs on a node whose result is already
discarded."
  (let ((tasks (%flow-group-tasks flow client plans)))
    (when (null tasks)
      (return-from axllm/core::core-flow-dispatch-group :null))
    (let* ((count (length tasks))
           (reports (make-array count :initial-element nil))
           (lock (sb-thread:make-mutex :name "flow-dispatch-group"))
           (settled (sb-thread:make-waitqueue))
           (parent-cancellation (%flow-run-cancellation options))
           (path (%flow-run-path options))
           (threads '()))
      (flet ((deliver (position report)
               (sb-thread:with-mutex (lock)
                 (unless (aref reports position)
                   (setf (aref reports position) report))
                 (sb-thread:condition-broadcast settled)))
             (cancel-all (reason)
               (dolist (task tasks) (flow-cancel (flow-task-cancellation task) reason))))
        (dolist (task tasks)
          (let* ((position (flow-task-position task))
                 (index (flow-task-index task))
                 (program (flow-task-program task))
                 (worker-flow (%flow-worker-flow flow index program))
                 (worker-step (%flow-worker-step (flow-task-step task) program))
                 (worker-plan (%flow-clone (flow-task-plan task)))
                 (worker-state (%flow-clone state))
                 (worker-options (%flow-worker-options options (flow-task-cancellation task)))
                 (worker-client (flow-task-client task)))
            (push (sb-thread:make-thread
                   (lambda ()
                     (deliver
                      position
                      (handler-case
                          (axllm/core::flow-execute-owned-worker
                           worker-flow worker-step worker-plan worker-client
                           worker-state worker-options)
                        (error (condition)
                          (%flow-error-report (princ-to-string condition))))))
                   :name (format nil "ax-flow-~a" (axllm/core::core-get worker-step "name" "node")))
                  threads)))
        (let ((deadline nil))
          (unwind-protect
               (loop
                 (sb-thread:with-mutex (lock)
                   (when (every #'identity reports) (return))
                   (sb-thread:condition-wait settled lock :timeout 0.02d0))
                 (let ((failed (sb-thread:with-mutex (lock)
                                 (some (lambda (report)
                                         (and report
                                              (not (eq (axllm/core::core-get report "error") :null))))
                                       reports))))
                   (when (or failed (flow-cancelled-p parent-cancellation))
                     (cancel-all "Flow group cancelled")
                     (unless deadline
                       (setf deadline (+ (get-internal-real-time)
                                         (* +flow-group-drain-seconds+
                                            internal-time-units-per-second))))))
                 (when (and deadline (>= (get-internal-real-time) deadline))
                   (sb-thread:with-mutex (lock)
                     (loop for position from 0 below count
                           for task in tasks
                           do (unless (aref reports position)
                                (setf (aref reports position)
                                      (%flow-error-report
                                       (format nil "Flow cancelled; unresolved node: ~a/~a"
                                               path
                                               (axllm/core::core-get (flow-task-step task) "name" "")))))))
                   (return)))
            (cancel-all "Flow group cancelled")))
        ;; A node that finished cleanly hands its owned program back, so the
        ;; flow keeps the conversation the worker actually had.
        (let ((steps (%flow-array (axllm/core::core-get flow "steps")))
              (out (%new-array)))
          (loop for task in tasks
                for report = (aref reports (flow-task-position task))
                do (when (eq (axllm/core::core-get report "error") :null)
                     (let ((index (flow-task-index task)))
                       (when (< -1 index (length steps))
                         (axllm/core::core-set (aref steps index) "program" (flow-task-program task)))))
                   (vector-push-extend report out))
          out)))))

(defun %flow-run-path (options)
  (let ((opts (if (hash-table-p options) options (%new-object))))
    (let ((snake (axllm/core::core-get opts "execution_path" "root")))
      (axllm/core::core-get opts "executionPath" snake))))

(defun %flow-run-cancellation (options)
  "The run's cancellation, under any of the three names a caller may use."
  (let ((opts (if (hash-table-p options) options (%new-object))))
    (dolist (key '("cancellation" "cancellationToken" "cancellation_token") :null)
      (let ((value (axllm/core::core-get opts key)))
        (unless (eq value :null) (return value))))))
