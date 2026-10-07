;;;; flow-runtime.lisp --- what the shared fixtures cannot observe.
;;;;
;;;; The conformance suite proves AxFlow's semantics against the recorded
;;;; cross-port expectations.  It cannot prove that a parallel group really
;;;; runs on two threads, that one node cannot see another's state, that a
;;;; failure cancels its siblings and the group still settles, or that a
;;;; mutating callback cannot corrupt the run.  Those are properties of this
;;;; port's native boundary, and each test here observes the property itself
;;;; rather than the presence of a method:
;;;;
;;;;   * concurrency is proved by a rendezvous.  Each node waits for the
;;;;     other to arrive before it returns, with a timeout.  Serial execution
;;;;     cannot satisfy it, so the test fails rather than passes slowly.
;;;;   * isolation is proved by each node writing the same key and reading
;;;;     what it wrote, while the merged result follows plan order.
;;;;   * cancellation is proved by the surviving node observing its own
;;;;     cancellation, and by a node that ignores cancellation producing a
;;;;     named unresolved-node failure instead of a hang.
;;;;   * recovery is proved by running the same flow again after a failure.
;;;;
;;;; The programs here are plain Ax programs: they answer the program
;;;; generics and offer an owned-worker factory, which is all the flow's
;;;; dispatcher asks of a node.

(defpackage #:axllm/flow-runtime-tests
  (:use #:cl)
  (:export #:run-flow-runtime-tests))

(in-package #:axllm/flow-runtime-tests)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(define-condition runtime-failure (error)
  ((detail :initarg :detail :reader runtime-failure-detail))
  (:report (lambda (condition stream) (write-string (runtime-failure-detail condition) stream))))

(defun fail (format-control &rest arguments)
  (error 'runtime-failure :detail (apply #'format nil format-control arguments)))

(defun expect (ok format-control &rest arguments)
  (unless ok (apply #'fail format-control arguments))
  t)

(defun expect-json (actual expected label)
  (unless (axllm/core::core-value-equal actual expected)
    (fail "~a mismatch~%    expected: ~a~%    actual:   ~a"
          label (ax:encode-json expected) (ax:encode-json actual))))

(defvar *tests* '())

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *tests* (append (remove ',name *tests*) (list ',name)))
     ',name))

;;; ------------------------------------------------------------------
;;; A program that runs a Lisp function, and can be owned by a worker
;;; ------------------------------------------------------------------

(defclass test-program ()
  ((body :initarg :body :reader test-program-body)
   (label :initarg :label :initform "node" :reader test-program-label)
   (calls :initarg :calls :initform (list 0) :reader test-program-calls))
  (:documentation
   "A program node whose behavior is a Lisp function of (state options).

An owned copy shares the function and the call counter -- that is the point
of the counter -- but is a distinct program object, so the dispatcher's
requirement that no two threads share a program is still satisfied."))

(defun test-program (body &key (label "node"))
  (make-instance 'test-program :body body :label label))

(defun test-program-call-count (program)
  (first (test-program-calls program)))

(defmethod ax:forward ((program test-program) client values &optional options)
  (declare (ignore client))
  (incf (first (test-program-calls program)))
  (funcall (test-program-body program) values (if (hash-table-p options) options (ax:object))))

(defmethod ax:program-chat-log ((program test-program)) (vector))
(defmethod ax:program-usage ((program test-program)) (ax:object))
(defmethod ax:program-traces ((program test-program)) (vector))
(defmethod ax:program-optimizable-components ((program test-program)) (vector))
(defmethod ax:program-apply-optimized-components ((program test-program) component-map)
  (declare (ignore component-map))
  program)

(defmethod axllm/core::core-host-call ((target test-program) method args)
  (cond ((or (equal method "owned_worker_factory") (equal method "ownedWorkerFactory"))
         (lambda ()
           (make-instance 'test-program
                          :body (test-program-body target)
                          :label (test-program-label target)
                          :calls (test-program-calls target))))
        ((equal method "signature") :null)
        ((equal method "forward")
         (ax:forward target (aref args 0) (aref args 1)
                     (if (> (length args) 2) (aref args 2) nil)))
        (t (fail "test program has no method ~a" method))))

(defclass test-client ()
  ((owned :initarg :owned :initform t :reader test-client-owned-p))
  (:documentation
   "A client stand-in. With :owned NIL it offers no owned worker, which is how
the serial fallback is exercised as a real execution path."))

(defmethod axllm/core::core-host-call ((target test-client) method args)
  (declare (ignore args))
  (cond ((or (equal method "owned_worker_factory") (equal method "ownedWorkerFactory"))
         (if (test-client-owned-p target)
             (lambda () (make-instance 'test-client :owned t))
             :null))
        (t (fail "test client has no method ~a" method))))

(defclass recording-control ()
  ((events :initform (make-array 0 :adjustable t :fill-pointer 0) :reader control-events))
  (:documentation "A run control that records the lifecycle events it is sent."))

(defmethod axllm/core::core-host-call ((target recording-control) method args)
  (cond ((or (equal method "_emit") (equal method "emit"))
         (vector-push-extend (ax:object "path" (ax:jget (aref args 0) "path")
                                        "type" (ax:jget (aref args 0) "type"))
                             (control-events target))
         :null)
        ((equal method "aborted") ax:false)
        (t (fail "recording control has no method ~a" method))))

(defmethod axllm/core::core-host-get ((target recording-control) key &optional (fallback :null))
  (if (equal key "aborted") ax:false fallback))

(defclass aborted-control ()
  ((events :initform (make-array 0 :adjustable t :fill-pointer 0) :reader control-events))
  (:documentation
   "A run control that is already aborted and records what the flow reports."))

(defmethod axllm/core::core-host-call ((target aborted-control) method args)
  (cond ((or (equal method "_emit") (equal method "emit"))
         (vector-push-extend (aref args 0) (control-events target))
         :null)
        ((equal method "aborted") ax:true)
        (t (fail "aborted control has no method ~a" method))))

(defmethod axllm/core::core-host-get ((target aborted-control) key &optional (fallback :null))
  (if (equal key "aborted") ax:true fallback))

;;; ------------------------------------------------------------------
;;; Shared shapes
;;; ------------------------------------------------------------------

(defun parallel-flow (left right &key (reads (vector "question")))
  "A flow whose two program nodes share one parallel group."
  (let ((flow (ax:flow (ax:object "id" "runtime.flow"))))
    (ax:flow-execute flow "left" left
                     (ax:object "reads" reads "writes" (vector "leftResult") "isBarrier" ax:false))
    (ax:flow-execute flow "right" right
                     (ax:object "reads" reads "writes" (vector "rightResult") "isBarrier" ax:false))
    flow))

(defun group-size (flow)
  (let ((groups (ax:jget (ax:flow-plan flow) "groups")))
    (reduce #'max (map 'list (lambda (group) (length (ax:jget group "steps"))) groups))))

(defun trace-kinds (flow)
  (map 'list (lambda (event) (ax:jget event "kind")) (ax:flow-traces flow)))

;;; ------------------------------------------------------------------
;;; Concurrency
;;; ------------------------------------------------------------------

(deftest test-parallel-group-runs-nodes-at-the-same-time
  ;; Each node waits for the other to arrive. Running the group one node at a
  ;; time cannot satisfy this, so a serial dispatcher times out and fails.
  (let* ((arrived-left (sb-thread:make-semaphore :name "left"))
         (arrived-right (sb-thread:make-semaphore :name "right"))
         (timeout 5)
         (rendezvous
           (lambda (mine theirs answer)
             (lambda (state options)
               (declare (ignore state options))
               (sb-thread:signal-semaphore mine)
               (unless (sb-thread:wait-on-semaphore theirs :timeout timeout)
                 (error "the sibling node never arrived; the group did not run concurrently"))
               (ax:object "answer" answer))))
         (flow (parallel-flow
                (test-program (funcall rendezvous arrived-left arrived-right "l") :label "left")
                (test-program (funcall rendezvous arrived-right arrived-left "r") :label "right"))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer" "right" "rightResult.answer"))
    (expect (= (group-size flow) 2) "the two independent nodes must share one parallel group")
    (let ((output (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))))
      (expect-json output (ax:object "left" "l" "right" "r") "concurrent parallel group output"))
    (expect (not (member "flow_parallel_fallback" (trace-kinds flow) :test #'equal))
            "a dispatched group must not record the serial fallback")))

(deftest test-parallel-nodes-do-not-share-state
  ;; Both nodes write and then read the same key. Each must read back its own
  ;; value, which is only true when each has its own state.
  (let* ((gate (sb-thread:make-semaphore))
         (body (lambda (mine)
                 (lambda (state options)
                   (declare (ignore options))
                   (setf (gethash "seen" state) mine)
                   ;; Give the sibling every chance to overwrite a shared key.
                   (sb-thread:signal-semaphore gate)
                   (sb-thread:wait-on-semaphore gate :timeout 5)
                   (ax:object "answer" (gethash "seen" state)))))
         (flow (parallel-flow (test-program (funcall body "left"))
                              (test-program (funcall body "right")))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer" "right" "rightResult.answer"))
    (let ((output (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))))
      (expect-json output (ax:object "left" "left" "right" "right")
                   "each parallel node must see only its own state"))))

(deftest test-parallel-group-merges-in-plan-order-not-finish-order
  ;; The slower node is the earlier one in the plan, so finish order and plan
  ;; order disagree. Both spread "answer"; the later step must still win.
  (let* ((flow (parallel-flow
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (sleep 0.15)
                                (ax:object "answer" "from left")))
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (ax:object "answer" "from right"))))))
    (ax:flow-returns flow (ax:object "answer" "answer" "left" "leftResult.answer"))
    (let ((output (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))))
      (expect-json output (ax:object "answer" "from right" "left" "from left")
                   "a parallel group merges its reports in plan order"))))

;;; ------------------------------------------------------------------
;;; Cancellation and recovery
;;; ------------------------------------------------------------------

(deftest test-failed-node-cancels-its-sibling
  (let* ((sibling-saw-cancellation nil)
         (failed (sb-thread:make-semaphore))
         (flow (parallel-flow
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (sb-thread:signal-semaphore failed)
                                (error "left exploded")))
                (test-program
                 (lambda (state options)
                   (declare (ignore state))
                   (sb-thread:wait-on-semaphore failed :timeout 5)
                   (let ((token (ax:jget options "cancellation")))
                     ;; Wait for the group to pass the cancellation on, then
                     ;; stop as a cooperative node must.
                     (loop repeat 200
                           until (ax:flow-cancelled-p token)
                           do (sleep 0.005))
                     (setf sibling-saw-cancellation (ax:flow-cancelled-p token)))
                   (ax:object "answer" "r"))))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer"))
    (handler-case
        (progn (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))
               (fail "expected the failing node to fail the flow"))
      (runtime-failure (condition) (error condition))
      (error (condition)
        (let ((text (princ-to-string condition)))
          (expect (search "left exploded" text)
                  "the group's failure must name the node's own error, got: ~a" text))))
    (expect sibling-saw-cancellation
            "a failed node must cancel its siblings so they stop doing discarded work")))

(deftest test-unresolved-node-is-reported-not-waited-on
  ;; A node that ignores its cancellation must not hold the group open: the
  ;; drain ends and the node is reported, by path, as cancelled.
  (let* ((release (sb-thread:make-semaphore))
         (flow (parallel-flow
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (error "left exploded")))
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (sb-thread:wait-on-semaphore release :timeout 10)
                                (ax:object "answer" "late"))))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer"))
    (let ((started (get-internal-real-time)))
      (handler-case
          (progn (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))
                 (fail "expected the group to fail"))
        (runtime-failure (condition) (error condition))
        (error (condition)
          (let ((text (princ-to-string condition))
                (elapsed (/ (- (get-internal-real-time) started)
                            internal-time-units-per-second)))
            (expect (search "unresolved node" text)
                    "an uncancellable node must be reported as unresolved, got: ~a" text)
            (expect (search "root/right" text)
                    "the unresolved node must be named by its path, got: ~a" text)
            (expect (< elapsed 5)
                    "the group must settle on its own drain, not wait for the node (~as)" elapsed))))
      (sb-thread:signal-semaphore release))))

(deftest test-caller-cancellation-stops-the-group
  ;; A cancellation the caller already holds, under any of its three names,
  ;; reaches every worker and ends the group.
  (dolist (key '("cancellation" "cancellationToken" "cancellation_token"))
    (let* ((seen (list nil))
           (flow (parallel-flow
                  (test-program (lambda (state options)
                                  (declare (ignore state))
                                  (let ((token (ax:jget options "cancellation")))
                                    (loop repeat 400
                                          until (ax:flow-cancelled-p token)
                                          do (sleep 0.005))
                                    (setf (first seen) (ax:flow-cancelled-p token)))
                                  (ax:object "answer" "l")))
                  (test-program (lambda (state options)
                                  (declare (ignore state options))
                                  (ax:object "answer" "r")))))
           (token (ax:flow-cancel (make-instance 'ax:flow-cancellation) "caller stopped")))
      (ax:flow-returns flow (ax:object "left" "leftResult.answer"))
      (handler-case
          (ax:forward flow (make-instance 'test-client) (ax:object "question" "q")
                      (ax:object key token))
        (runtime-failure (condition) (error condition))
        (error () nil))
      (expect (first seen)
              "a caller cancellation under ~s must reach the group's workers" key))))

(deftest test-run-control-abort-stops-the-flow-before-its-group
  (let* ((ran (list nil))
         (control (make-instance 'aborted-control))
         (flow (parallel-flow
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (setf (first ran) t)
                                (ax:object "answer" "l")))
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (setf (first ran) t)
                                (ax:object "answer" "r"))))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer"))
    (handler-case
        (progn (ax:forward flow (make-instance 'test-client) (ax:object "question" "q")
                           (ax:object "control" control))
               (fail "expected an aborted run control to stop the flow"))
      (runtime-failure (condition) (error condition))
      (error (condition)
        (expect (search "Flow aborted" (princ-to-string condition))
                "an aborted control must stop the flow, got: ~a" condition)))
    (expect (not (first ran)) "no node may run once the control is aborted")
    (expect (find "failed" (map 'list (lambda (e) (ax:jget e "type")) (control-events control))
                  :test #'equal)
            "the aborted flow must report failed on its control, got ~a"
            (map 'list (lambda (e) (ax:jget e "type")) (control-events control)))))

(deftest test-flow-reports-failed-after-its-node-does
  ;; Lifecycle events read outside-in on the way down and inside-out on the
  ;; way back: the flow must not report failed before the node that failed.
  (let* ((control (make-instance 'recording-control))
         (flow (ax:flow (ax:object "id" "events.flow"))))
    (ax:flow-execute flow "first"
                     (test-program
                      (lambda (state options)
                        (declare (ignore state))
                        (let ((node (ax:jget options "control"))
                              (path (ax:jget options "executionPath" "root/first")))
                          (axllm/core::core-host-call
                           node "_emit" (vector (ax:object "type" "started" "path" path)))
                          (unwind-protect (error "node exploded")
                            (axllm/core::core-host-call
                             node "_emit" (vector (ax:object "type" "failed" "path" path)))))))
                     (ax:object "reads" (vector "question") "writes" (vector "firstResult")))
    (ax:flow-returns flow (ax:object "answer" "firstResult.answer"))
    (handler-case
        (progn (ax:forward flow (make-instance 'test-client) (ax:object "question" "q")
                           (ax:object "control" control))
               (fail "expected the node's failure to fail the flow"))
      (runtime-failure (condition) (error condition))
      (error () nil))
    (expect-json (control-events control)
                 (vector (ax:object "path" "root" "type" "started")
                         (ax:object "path" "root/first" "type" "started")
                         (ax:object "path" "root/first" "type" "failed")
                         (ax:object "path" "root" "type" "failed"))
                 "flow and node lifecycle events in order")))

(deftest test-worker-cannot-reach-a-nested-object-in-the-parent-state
  ;; Isolation has to be deep: a worker that mutates a nested object or an
  ;; array inside its state must not change the state the group started from.
  (let* ((flow (parallel-flow
                (test-program (lambda (state options)
                                (declare (ignore options))
                                (setf (gethash "tag" (ax:jget state "profile")) "left")
                                (setf (aref (ax:jget state "tags") 0) "left")
                                (ax:object "answer" (ax:jget (ax:jget state "profile") "tag"))))
                (test-program (lambda (state options)
                                (declare (ignore options))
                                (setf (gethash "tag" (ax:jget state "profile")) "right")
                                (setf (aref (ax:jget state "tags") 0) "right")
                                (ax:object "answer" (ax:jget (ax:jget state "profile") "tag"))))
                :reads (vector "profile" "tags")))
         (profile (ax:object "tag" "start"))
         (tags (make-array 1 :adjustable t :fill-pointer 1 :initial-element "start"))
         (input (ax:object "profile" profile "tags" tags)))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer" "right" "rightResult.answer"))
    (let ((output (ax:forward flow (make-instance 'test-client) input)))
      (expect-json output (ax:object "left" "left" "right" "right")
                   "each worker reads back its own nested write")
      (expect-json profile (ax:object "tag" "start")
                   "the caller's nested object must be untouched")
      (expect (equal (aref tags 0) "start")
              "the caller's array must be untouched, got ~s" (aref tags 0)))))

(deftest test-flow-nests-as-a-program-node
  ;; A flow answers the program generics, so an outer flow can run it as a
  ;; node: its chat log, usage and traces are folded in under the node name.
  (let ((inner (ax:flow (ax:object "id" "inner.flow")))
        (outer (ax:flow (ax:object "id" "outer.flow"))))
    (ax:flow-map inner "shout"
                 (lambda (state)
                   (ax:object "answer" (string-upcase (ax:jget state "question")))))
    (ax:flow-returns inner (ax:object "answer" "answer"))
    (ax:flow-execute outer "nested" inner
                     (ax:object "reads" (vector "question") "writes" (vector "nestedResult")))
    (ax:flow-returns outer (ax:object "answer" "nestedResult.answer"))
    (expect (plusp (length (ax:program-optimizable-components inner)))
            "a nested flow exposes its own optimizable components")
    (let ((output (ax:forward outer (make-instance 'test-client) (ax:object "question" "hi"))))
      (expect-json output (ax:object "answer" "HI") "a nested flow runs as a node")
      (expect (find "flow_child_trace" (trace-kinds outer) :test #'equal)
              "the outer flow records the nested flow's traces, got ~a" (trace-kinds outer)))))

(deftest test-flow-recovers-after-a-failed-group
  (let* ((explode (list t))
         (flow (parallel-flow
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (when (first explode) (error "left exploded"))
                                (ax:object "answer" "l")))
                (test-program (lambda (state options)
                                (declare (ignore state options))
                                (ax:object "answer" "r"))))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer" "right" "rightResult.answer"))
    (handler-case (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))
      (runtime-failure (condition) (error condition))
      (error () nil))
    (setf (first explode) nil)
    (let ((output (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))))
      (expect-json output (ax:object "left" "l" "right" "r")
                   "a flow must run cleanly after a failed group")
      (expect (not (find "flow_parallel_fallback" (trace-kinds flow) :test #'equal))
              "the recovered run must still dispatch its group")
      (expect (equal (first (trace-kinds flow)) "flow_start")
              "a new run starts a fresh trace list, got ~a" (trace-kinds flow)))))

;;; ------------------------------------------------------------------
;;; Serial fallback
;;; ------------------------------------------------------------------

(deftest test-group-without-owned-workers-runs-serially-and-says-so
  (let* ((order '())
         (record (lambda (name answer)
                   (lambda (state options)
                     (declare (ignore state options))
                     (push name order)
                     (ax:object "answer" answer))))
         (flow (parallel-flow (test-program (funcall record "left" "l"))
                              (test-program (funcall record "right" "r")))))
    (ax:flow-returns flow (ax:object "left" "leftResult.answer" "right" "rightResult.answer"))
    (let ((output (ax:forward flow (make-instance 'test-client :owned nil)
                              (ax:object "question" "q"))))
      (expect-json output (ax:object "left" "l" "right" "r") "serial fallback output")
      (expect (equal (reverse order) '("left" "right"))
              "the fallback runs the group's nodes in plan order, got ~a" (reverse order))
      (expect (member "flow_parallel_fallback" (trace-kinds flow) :test #'equal)
              "the fallback must record why it did not dispatch, got ~a" (trace-kinds flow)))))

;;; ------------------------------------------------------------------
;;; Callback isolation
;;; ------------------------------------------------------------------

(deftest test-map-callback-cannot-reach-the-run-state
  (let* ((flow (ax:flow (ax:object "id" "callback.flow"))))
    (ax:flow-map flow "sneaky"
                 (lambda (state)
                   ;; A callback that mutates what it is given must not change
                   ;; the flow's own state; only its return value counts.
                   (setf (gethash "question" state) "clobbered")
                   (remhash "keep" state)
                   (ax:object "seen" (ax:jget state "question"))))
    (ax:flow-returns flow (ax:object "question" "question" "keep" "keep" "seen" "seen"))
    (let ((output (ax:forward flow (make-instance 'test-client)
                              (ax:object "question" "original" "keep" "kept"))))
      (expect-json output (ax:object "question" "original" "keep" "kept" "seen" "clobbered")
                   "a mutating callback must only affect the clone it was handed"))))

(deftest test-derive-callback-sees-each-item-in-isolation
  (let ((flow (ax:flow (ax:object "id" "derive.flow"))))
    (ax:flow-derive flow "upper"
                    (lambda (state)
                      (let ((item (ax:jget state "__item")))
                        ;; Mutating the item state must not leak into the next
                        ;; item's state.
                        (setf (gethash "__item" state) "clobbered")
                        (ax:object "__derived" (string-upcase item))))
                    (ax:object "reads" (vector "names")))
    (ax:flow-returns flow (ax:object "upper" "upper"))
    (let ((output (ax:forward flow (make-instance 'test-client)
                              (ax:object "names" (vector "ada" "grace")))))
      (expect-json output (ax:object "upper" (vector "ADA" "GRACE"))
                   "derive maps each element from its own item state"))))

;;; ------------------------------------------------------------------
;;; The optimizer's program contract
;;; ------------------------------------------------------------------

(defclass calling-program (test-program)
  ((calls-made :initarg :calls-made :initform (vector) :reader program-calls-made))
  (:documentation "A node program that can report the tool calls it made."))

(defmethod ax:program-function-calls ((program calling-program))
  (program-calls-made program))

(deftest test-flow-reports-its-kind-to-the-optimizer
  ;; The kind the optimizer sends as request.programKind. It is the kind Core
  ;; itself wrote into the flow record, not a second spelling kept in the host.
  (expect (equal (ax:program-kind (ax:flow)) "axflow")
          "a flow's optimizer kind is \"axflow\", got ~s" (ax:program-kind (ax:flow)))
  (expect (equal (ax:jget (ax:flow-state (ax:flow)) "program_kind") "axflow")
          "and it is the value Core recorded"))

(deftest test-attaching-a-nested-flow-does-not-change-its-identity
  ;; A node's program keeps the id the caller gave it, and a flow nobody named
  ;; keeps Core's default. Core's default is the constant root.flow, so the id
  ;; alone cannot tell "nobody chose one" from "the caller chose root.flow";
  ;; neither is renamed, so the two cannot be confused.
  (let ((outer (ax:flow (ax:object "id" "outer.flow")))
        (unnamed (ax:flow))
        (chose-the-default (ax:flow (ax:object "id" "root.flow")))
        (named (ax:flow (ax:object "id" "chosen.flow"))))
    (dolist (child (list unnamed chose-the-default named))
      (ax:flow-map child "step" (lambda (state) (declare (ignore state)) (ax:object))))
    (ax:flow-execute outer "a" unnamed
                     (ax:object "reads" (vector "question") "writes" (vector "aResult")))
    (ax:flow-execute outer "b" chose-the-default
                     (ax:object "reads" (vector "question") "writes" (vector "bResult")))
    (ax:flow-execute outer "c" named
                     (ax:object "reads" (vector "question") "writes" (vector "cResult")))
    (expect (equal (ax:jget (ax:flow-state unnamed) "program_id") "root.flow")
            "an unnamed nested flow keeps Core's default, got ~s"
            (ax:jget (ax:flow-state unnamed) "program_id"))
    (expect (equal (ax:jget (ax:flow-state chose-the-default) "program_id") "root.flow")
            "an explicit root.flow survives attachment, got ~s"
            (ax:jget (ax:flow-state chose-the-default) "program_id"))
    (expect (equal (ax:jget (ax:flow-state named) "program_id") "chosen.flow")
            "a named nested flow keeps its own id, got ~s"
            (ax:jget (ax:flow-state named) "program_id"))
    ;; A nested flow's own id therefore comes from its construction, which is
    ;; where every port puts it, and the parent prefixes it with <owner>.<node>.
    (let ((ids (map 'list (lambda (c) (ax:jget c "id")) (ax:flow-components outer))))
      (expect (member "outer.flow.c::chosen.flow::graph-plan" ids :test #'equal)
              "the nested flow's graph component is prefixed, got ~a" ids))))

(deftest test-two-children-sharing-an-id-do-not-collide-after-prefixing
  ;; This is why renaming a child would buy nothing: Core's prefix already
  ;; separates two children that carry the same id, including two unnamed
  ;; flows both holding Core's default.
  (let ((outer (ax:flow (ax:object "id" "root.flow")))
        (left (ax:flow (ax:object "id" "shared")))
        (right (ax:flow (ax:object "id" "shared"))))
    (ax:flow-map left "step" (lambda (state) (declare (ignore state)) (ax:object)))
    (ax:flow-map right "step" (lambda (state) (declare (ignore state)) (ax:object)))
    (ax:flow-execute outer "a" left
                     (ax:object "reads" (vector "question") "writes" (vector "aResult")))
    (ax:flow-execute outer "b" right
                     (ax:object "reads" (vector "question") "writes" (vector "bResult")))
    (let ((ids (map 'list (lambda (c) (ax:jget c "id")) (ax:flow-components outer))))
      (expect (member "root.flow.a::shared::graph-plan" ids :test #'equal)
              "the first child's component is prefixed by its node, got ~a" ids)
      (expect (member "root.flow.b::shared::graph-plan" ids :test #'equal)
              "the second child's component is prefixed by its node, got ~a" ids)
      (expect (= (length ids) (length (remove-duplicates ids :test #'equal)))
              "and no two component ids are equal, got ~a" ids))))

(deftest test-flow-aggregates-its-nodes-tool-calls
  (let ((flow (ax:flow (ax:object "id" "calls.flow"))))
    (ax:flow-execute flow "lookup"
                     (make-instance 'calling-program
                                    :body (lambda (state options)
                                            (declare (ignore state options))
                                            (ax:object "answer" "ok"))
                                    :calls-made (vector (ax:object "name" "search" "status" "ok")))
                     (ax:object "reads" (vector "question") "writes" (vector "lookupResult")))
    (ax:flow-execute flow "report"
                     (make-instance 'calling-program
                                    :body (lambda (state options)
                                            (declare (ignore state options))
                                            (ax:object "answer" "done"))
                                    :calls-made (vector (ax:object "name" "write" "status" "ok")))
                     (ax:object "reads" (vector "lookupResult") "writes" (vector "reportResult")))
    (let ((calls (ax:program-function-calls flow)))
      ;; Node order, each call tagged with the node that made it, and the call's
      ;; own "name" untouched so action scoring still matches.
      (expect-json (map 'vector (lambda (call) (ax:jget call "name")) calls)
                   (vector "search" "write") "aggregated call names in node order")
      (expect-json (map 'vector (lambda (call) (ax:jget call "node")) calls)
                   (vector "lookup" "report") "each call names the node that made it"))))

(deftest test-flow-names-a-node-that-cannot-report-its-calls
  ;; Reporting an empty history for a node that cannot answer would make an
  ;; action-adjusted score look clean, so the node is named instead.
  (let ((flow (ax:flow (ax:object "id" "opaque.flow"))))
    (ax:flow-execute flow "opaque" (test-program (lambda (state options)
                                                   (declare (ignore state options))
                                                   (ax:object "answer" "x")))
                     (ax:object "reads" (vector "question") "writes" (vector "opaqueResult")))
    (handler-case
        (progn (ax:program-function-calls flow)
               (fail "expected the flow to name the node that cannot report its calls"))
      (runtime-failure (condition) (error condition))
      (error (condition)
        (expect (search "opaque" (princ-to-string condition))
                "the failure must name the node, got: ~a" condition)))))

(deftest test-flow-callbacks-are-not-counted-as-callers
  ;; A map or derive node runs a Lisp callback and can make no provider or tool
  ;; call, so it never blocks the aggregate.
  (let ((flow (ax:flow (ax:object "id" "callbacks.flow"))))
    (ax:flow-map flow "shout" (lambda (state) (ax:object "answer" (ax:jget state "question"))))
    (expect-json (ax:program-function-calls flow) (vector)
                 "a flow of callbacks reports no calls and refuses nothing")))

(deftest test-flow-accepts-optimizer-demos-and-returns-itself
  (let ((flow (ax:flow (ax:object "id" "root.flow"))))
    (ax:flow-execute flow "qa" (ax:ax "question:string -> answer:string"))
    (expect (eq (ax:program-set-demos flow (vector (ax:object "programId" "root.qa" "traces" (vector))))
                flow)
            "program-set-demos returns the program the optimizer handed it")
    (expect-json (ax:jget (ax:flow-state flow) "demos")
                 (vector (ax:object "programId" "root.qa" "traces" (vector)))
                 "the demos are installed on the flow")
    ;; A demo naming a node the flow does not have is refused, not dropped.
    (handler-case
        (progn (ax:program-set-demos flow (vector (ax:object "programId" "root.missing")))
               (fail "expected an unknown program id to be refused"))
      (runtime-failure (condition) (error condition))
      (error (condition)
        (expect (search "root.missing" (princ-to-string condition))
                "the refusal must name the unknown program id, got: ~a" condition)))))

;;; ------------------------------------------------------------------
;;; Cache
;;; ------------------------------------------------------------------

(defun counting-cache ()
  "An in-memory caching function plus its read and write logs."
  (let ((store (make-hash-table :test #'equal))
        (reads '())
        (writes '()))
    (values (lambda (key &optional (value nil value-supplied))
              (if value-supplied
                  (progn (push key writes) (setf (gethash key store) value) :null)
                  (progn (push key reads)
                         (multiple-value-bind (hit found) (gethash key store)
                           (if found hit :null)))))
            (lambda () (reverse reads))
            (lambda () (reverse writes)))))

(deftest test-cache-miss-then-hit-skips-the-node
  (multiple-value-bind (cache reads writes) (counting-cache)
    (let* ((program (test-program (lambda (state options)
                                    (declare (ignore options))
                                    (ax:object "answer" (ax:jget state "question")))))
           (flow (ax:flow (ax:object "id" "cache.flow"))))
      (ax:flow-execute flow "qa" program
                       (ax:object "reads" (vector "question") "writes" (vector "qaResult")))
      (ax:flow-returns flow (ax:object "answer" "qaResult.answer"))
      (let ((options (ax:object "cachingFunction" cache))
            (client (make-instance 'test-client)))
        (expect-json (ax:forward flow client (ax:object "question" "q") options)
                     (ax:object "answer" "q") "first call output")
        (expect (= (test-program-call-count program) 1) "the first call runs the node")
        (expect-json (ax:forward flow client (ax:object "question" "q") options)
                     (ax:object "answer" "q") "cached call output")
        (expect (= (test-program-call-count program) 1)
                "a cache hit must not run the node again, ran ~a time(s)"
                (test-program-call-count program))
        (expect (= (length (funcall reads)) 2) "both calls read the cache")
        (expect (= (length (funcall writes)) 1) "only the miss writes the cache")
        ;; Different inputs are a different key, so the node runs again.
        (ax:forward flow client (ax:object "question" "other") options)
        (expect (= (test-program-call-count program) 2)
                "a different input must miss the cache")))))

(deftest test-cache-key-ignores-input-key-order
  (multiple-value-bind (cache reads writes) (counting-cache)
    (declare (ignore reads))
    (let* ((program (test-program (lambda (state options)
                                    (declare (ignore options))
                                    (ax:object "answer" (ax:jget state "a")))))
           (flow (ax:flow (ax:object "id" "cache-order.flow"))))
      (ax:flow-execute flow "qa" program
                       (ax:object "reads" (vector "a" "b") "writes" (vector "qaResult")))
      (ax:flow-returns flow (ax:object "answer" "qaResult.answer"))
      (let ((options (ax:object "cachingFunction" cache))
            (client (make-instance 'test-client)))
        (ax:forward flow client (ax:object "a" "1" "b" "2") options)
        (ax:forward flow client (ax:object "b" "2" "a" "1") options)
        (expect (= (length (funcall writes)) 1)
                "the cache key must not depend on input key order, wrote ~a time(s)"
                (length (funcall writes)))
        (expect (= (test-program-call-count program) 1)
                "the reordered input must hit the cache")))))

(deftest test-global-caching-function-is-the-fallback
  (multiple-value-bind (cache reads writes) (counting-cache)
    (declare (ignore reads))
    (let* ((program (test-program (lambda (state options)
                                    (declare (ignore options))
                                    (ax:object "answer" (ax:jget state "question")))))
           (flow (ax:flow (ax:object "id" "cache-global.flow")))
           (previous (ax:get-global "cachingFunction")))
      (ax:flow-execute flow "qa" program
                       (ax:object "reads" (vector "question") "writes" (vector "qaResult")))
      (ax:flow-returns flow (ax:object "answer" "qaResult.answer"))
      (unwind-protect
           (progn
             (ax:set-global "cachingFunction" cache)
             (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))
             (ax:forward flow (make-instance 'test-client) (ax:object "question" "q"))
             (expect (= (length (funcall writes)) 1)
                     "a run with no call option uses the process-wide cache")
             (expect (= (test-program-call-count program) 1)
                     "the second run must hit the process-wide cache"))
        (ax:set-global "cachingFunction" previous)))))

(deftest test-cache-read-error-does-not-fail-the-run
  (let* ((program (test-program (lambda (state options)
                                  (declare (ignore options))
                                  (ax:object "answer" (ax:jget state "question")))))
         (flow (ax:flow (ax:object "id" "cache-error.flow"))))
    (ax:flow-execute flow "qa" program
                     (ax:object "reads" (vector "question") "writes" (vector "qaResult")))
    (ax:flow-returns flow (ax:object "answer" "qaResult.answer"))
    (let ((options (ax:object "cachingFunction"
                              (lambda (key &optional (value nil value-supplied))
                                (declare (ignore key value))
                                (if value-supplied :null (error "cache unavailable"))))))
      (expect-json (ax:forward flow (make-instance 'test-client) (ax:object "question" "q") options)
                   (ax:object "answer" "q")
                   "a cache read failure must be ignored, as in every other port"))))

;;; ------------------------------------------------------------------
;;; Mermaid output is a real rendering of a real builder flow
;;; ------------------------------------------------------------------

(deftest test-builder-flow-renders-and-parses-back
  (let ((flow (ax:flow (ax:object "id" "mermaid.flow"))))
    ;; critique reads summarize's own result key, so the rendering carries the
    ;; dependency as an edge and the document is a complete graph.
    (ax:flow-execute flow "summarize" (ax:ax "documentText:string -> summaryText:string")
                     (ax:object "reads" (vector)))
    (ax:flow-execute flow "critique" (ax:ax "summaryText:string -> critiqueText:string")
                     (ax:object "reads" (vector "summarizeResult")))
    (let ((rendered (ax:flow-mermaid flow)))
      (expect (search "flowchart" rendered) "a rendered flow is a Mermaid flowchart: ~a" rendered)
      (expect (search "summarize --> critique" rendered)
              "the rendering carries the dependency as an edge: ~a" rendered)
      (let ((reparsed (ax:flow rendered)))
        (expect-json (map 'vector (lambda (step) (ax:jget step "name"))
                          (remove "execute" (ax:jget (ax:flow-plan reparsed) "steps")
                                  :test-not #'equal
                                  :key (lambda (step) (ax:jget step "kind"))))
                     (vector "summarize" "critique")
                     "the rendered document parses back to the same node order")
        (expect-json (ax:flow-mermaid reparsed) rendered
                     "re-rendering the parsed document reproduces it exactly")))))

;;; ------------------------------------------------------------------
;;; Entry point
;;; ------------------------------------------------------------------

(defun run-flow-runtime-tests ()
  "Run the native flow runtime tests. Returns T when all of them pass."
  (let ((passed 0)
        (failures '()))
    (dolist (test *tests*)
      (handler-case (progn (funcall test) (incf passed))
        (error (condition) (push (cons test (princ-to-string condition)) failures))))
    (setf failures (nreverse failures))
    (format t "~&flow runtime: ~d passed, ~d failed (of ~d tests)~%"
            passed (length failures) (length *tests*))
    (dolist (failure failures)
      (format t "~&  FAIL ~a~%    ~a~%" (car failure) (cdr failure)))
    (null failures)))
