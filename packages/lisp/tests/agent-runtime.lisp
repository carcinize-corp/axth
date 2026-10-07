;;;; agent-runtime.lisp --- tests for the agent's runtime and host boundaries.
;;;;
;;;; Entry point for the repository runner:
;;;;
;;;;   (axllm/tests-agent:run-agent-runtime-tests)   ; => (values passed failed)
;;;;
;;;; What is covered here needs no generated Core: the runtime envelopes, the
;;;; JSON-line protocol client against a real worker process, the native
;;;; request timeout and the worker cleanup it forces, session reentry under
;;;; concurrency, pause and resume through a real JavaScript engine, the
;;;; Core host-object bridge, the Docker session adapter against a loopback
;;;; HTTP double, and the context metrics collector.
;;;;
;;;; No network egress, no credentials, no Docker daemon. The protocol worker
;;;; is python3 or node running a script from this directory; the Docker
;;;; double is a loopback HTTP server in this image.

(defpackage #:axllm/tests-agent
  (:use #:cl)
  (:local-nicknames (#:ax #:axllm) (#:core #:axllm/core))
  (:documentation
   "Tests for packages/lisp/src/agent-runtime.lisp and agent.lisp.

Names from the library are written AX::NAME on purpose: these tests exercise
the surface as the parent package will export it, and spelling the package
makes it obvious which side of the boundary each name belongs to.")
  (:export #:run-agent-runtime-tests
           #:run-agent-conformance
           #:json-equal
           #:assert-json-subset
           #:assert-json-equal
           #:assert-json-list-subset
           #:make-scripted-runtime
           #:scripted-runtime-sessions
           #:scripted-runtime-executed
           #:scripted-runtime-create-requests
           #:scripted-runtime-execute-options
           #:protocol-worker-command
           #:javascript-worker-command))

(in-package #:axllm/tests-agent)

;;; ------------------------------------------------------------------
;;; Test framework
;;; ------------------------------------------------------------------

(define-condition test-failure (error)
  ((text :initarg :text :reader test-failure-text))
  (:report (lambda (condition stream) (write-string (test-failure-text condition) stream))))

(defvar *runtime-tests* '())

(define-condition test-skipped (condition)
  ((reason :initarg :reason :reader test-skipped-reason))
  (:documentation
   "Signalled by a test that cannot run because a named gap upstream of this
package blocks it. A skip is reported and counted separately: it is not a
pass, and it is not a failure of this package either."))

(defun skip-test (reason)
  (signal 'test-skipped :reason reason)
  (error 'test-failure :text (format nil "unreachable: skip not handled (~a)" reason)))

(defun agent-buildable-p ()
  "Whether an agent can be constructed at all in this image.

Core's actor stages end in a code output field, and gen.lisp's supported
field types do not include it yet, so every agent construction fails until
that lands. A test that needs an agent says so rather than appearing to
pass."
  (handler-case (progn (ax::agent "question:string -> answer:string") t)
    (error () nil)))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *runtime-tests*
           (append (remove ',name *runtime-tests* :key #'car) (list (cons ',name #',name))))
     ',name))

(defun expect (ok description)
  (unless ok (error 'test-failure :text description))
  t)

(defun expect-equal (actual expected description)
  (expect (equal actual expected)
          (format nil "~a (expected ~s, got ~s)" description expected actual)))

(defun expect-contains (haystack needle description)
  (expect (and (stringp haystack) (search needle haystack))
          (format nil "~a (~s not found in ~s)" description needle haystack)))

(defmacro expect-signals (type needle description &body body)
  "Run BODY, require a TYPE condition whose report contains NEEDLE."
  (let ((condition (gensym)))
    `(handler-case (progn ,@body
                          (error 'test-failure
                                 :text (format nil "~a (nothing signalled)" ,description)))
       (,type (,condition)
         (expect-contains (princ-to-string ,condition) ,needle ,description)
         ,condition))))

;;; ------------------------------------------------------------------
;;; JSON comparison
;;; ------------------------------------------------------------------

(defun json-equal (left right)
  "Whether LEFT and RIGHT are the same JSON value.

Object key order is not part of equality -- it is rendering order, and two
objects with the same entries are the same value -- but it is part of
encoding, which ENCODE-JSON tests cover separately."
  (cond ((and (ax::%object-p left) (ax::%object-p right))
         (let ((left-keys (ax::%object-keys left))
               (right-keys (ax::%object-keys right)))
           (and (= (length left-keys) (length right-keys))
                (every (lambda (key)
                         (and (nth-value 1 (gethash key right))
                              (json-equal (gethash key left) (gethash key right))))
                       left-keys))))
        ((and (ax::%array-p left) (ax::%array-p right))
         (and (= (length left) (length right))
              (every #'json-equal left right)))
        ((and (stringp left) (stringp right)) (string= left right))
        ((and (realp left) (realp right)) (= left right))
        (t (eql left right))))

(defun json-subset-p (actual expected)
  "Whether ACTUAL contains EXPECTED.

An expected object requires its keys and says nothing about the others, so
a fixture can pin the part it cares about. An expected array is compared
exactly, as the other ports do: inside a subset, a list is a value, and a
missing or extra element is a different value."
  (cond ((ax::%object-p expected)
         (and (ax::%object-p actual)
              (every (lambda (key)
                       (and (nth-value 1 (gethash key actual))
                            (json-subset-p (gethash key actual) (gethash key expected))))
                     (ax::%object-keys expected))))
        ((ax::%array-p expected) (json-equal actual expected))
        (t (json-equal actual expected))))

(defun json-list-subset-p (actual expected)
  "Whether ACTUAL holds each item of EXPECTED, in order, as a subset.

The shape every port uses for a log: the fixture names some entries in the
order they must appear, and the entries between them are not constrained."
  (and (ax::%array-p actual)
       (let ((start 0))
         (every (lambda (wanted)
                  (loop for index from start below (length actual)
                        when (json-subset-p (aref actual index) wanted)
                          do (setf start (1+ index)) (return t)
                        finally (return nil)))
                (if (ax::%array-p expected) (coerce expected 'list) '())))))

(defun assert-json-list-subset (actual expected description)
  (expect (json-list-subset-p actual expected)
          (format nil "~a~%  expected entries: ~a~%  actual list:      ~a"
                  description
                  (ignore-errors (ax:encode-json expected))
                  (ignore-errors (ax:encode-json actual)))))

(defun assert-json-subset (actual expected description)
  (expect (json-subset-p actual expected)
          (format nil "~a~%  expected subset: ~a~%  actual:          ~a"
                  description
                  (ignore-errors (ax:encode-json expected))
                  (ignore-errors (ax:encode-json actual)))))

(defun assert-json-equal (actual expected description)
  (expect (json-equal actual expected)
          (format nil "~a~%  expected: ~a~%  actual:   ~a"
                  description
                  (ignore-errors (ax:encode-json expected))
                  (ignore-errors (ax:encode-json actual)))))

(defun jparse (text) (ax:parse-json text))

;;; ------------------------------------------------------------------
;;; The worker processes
;;; ------------------------------------------------------------------

(defun %tests-file (name)
  (namestring (asdf:system-relative-pathname "axllm" (concatenate 'string "tests/" name))))

(defun protocol-worker-command ()
  "The python3 runtime protocol worker in this directory."
  (list "python3" (%tests-file "runtime-protocol-server.py")))

(defun javascript-worker-command ()
  "The node runtime protocol worker in this directory: a real JS engine."
  (list "node" (%tests-file "runtime-javascript-server.mjs")))

(defun make-protocol-runtime (&key (mode "normal") (timeout 20))
  (ax::make-process-runtime (protocol-worker-command)
                            :env (list (cons "AXIR_RUNTIME_PROTOCOL_FIXTURE_MODE" mode))
                            :timeout timeout))

(defmacro with-protocol-runtime ((name &rest options) &body body)
  `(let ((,name (make-protocol-runtime ,@options)))
     (unwind-protect (progn ,@body)
       (ignore-errors (ax::runtime-shutdown ,name)))))

(defun runtime-process-alive-p (runtime)
  (uiop:process-alive-p (ax::process-runtime-process runtime)))

;;; ------------------------------------------------------------------
;;; A scripted runtime, for tests that must not depend on a worker
;;; ------------------------------------------------------------------
;;;
;;; The deterministic stand-in every port uses to drive Core's session
;;; handling: each execute takes the next scripted step, which can pin the
;;; code it expects, patch the session's globals and return any envelope.
;;; A capability turned off here is refused, not silently answered, so a
;;; fixture can prove the fallback path.

(defclass scripted-runtime (ax::code-runtime)
  ((script :initarg :script :accessor scripted-runtime-script)
   (language :initarg :language :initform "JavaScript" :reader scripted-runtime-language)
   (usage :initarg :usage :initform "" :reader scripted-runtime-usage)
   (capabilities :initarg :capabilities :initform nil :reader scripted-runtime-capabilities)
   (sessions :initform '() :accessor scripted-runtime-sessions)
   (executed :initform (ax::%new-array) :reader scripted-runtime-executed)
   (create-requests :initform (ax::%new-array) :reader scripted-runtime-create-requests)
   (execute-options :initform (ax::%new-array) :reader scripted-runtime-execute-options)))

(defclass scripted-session (ax::code-session)
  ((runtime :initarg :runtime :reader scripted-session-runtime)
   (globals :initarg :globals :accessor scripted-session-globals)
   (create-options :initarg :create-options :reader scripted-session-create-options)
   (closed :initform nil :accessor scripted-session-closed)))

(defun make-scripted-runtime (&key script (language "JavaScript") (usage "") capabilities)
  "A scripted runtime. SCRIPT is a list of step objects.

A step may carry \"expected_code\" (the code it insists on),
\"bindings_patch\" (globals it writes), \"close_before_result\" (a session
that dies mid-step) and \"result\" (the envelope it answers with).
CAPABILITIES is a JSON object turning \"inspect\", \"snapshot\" or
\"patch\" off."
  (make-instance 'scripted-runtime
                 :script (coerce (or script '()) 'list)
                 :language language
                 :usage usage
                 :capabilities capabilities))

(defun %capability-enabled-p (runtime key)
  (let ((capabilities (scripted-runtime-capabilities runtime)))
    (or (not (ax::%object-p capabilities))
        (let ((value (ax:jget capabilities key)))
          (or (eq value :null) (ax:json-true-p value))))))

(defmethod ax::runtime-language ((runtime scripted-runtime)) (scripted-runtime-language runtime))
(defmethod ax::runtime-usage-instructions ((runtime scripted-runtime)) (scripted-runtime-usage runtime))

(defmethod ax::runtime-create-session ((runtime scripted-runtime) globals options)
  (vector-push-extend (ax:object "globals" (or globals (ax:object))
                                 "options" (or options (ax:object)))
                      (scripted-runtime-create-requests runtime))
  (let ((session (make-instance 'scripted-session
                                :runtime runtime
                                :globals (core::core-map-merge (or globals (ax:object)) (ax:object))
                                :create-options (or options (ax:object)))))
    (setf (scripted-runtime-sessions runtime)
          (append (scripted-runtime-sessions runtime) (list session)))
    session))

(defmethod ax::session-closed-p ((session scripted-session)) (scripted-session-closed session))

(defmethod ax::session-execute ((session scripted-session) code options)
  (if (scripted-session-closed session)
      (ax::envelope-session-closed)
      (let ((runtime (scripted-session-runtime session)))
        (when (null (scripted-runtime-script runtime))
          (error 'test-failure :text (format nil "scripted runtime exhausted on ~s" code)))
        (let* ((step (pop (scripted-runtime-script runtime)))
               (expected (ax:jget step "expected_code")))
          (unless (eq expected :null)
            (expect-equal code expected "scripted runtime executed the expected code"))
          (vector-push-extend code (scripted-runtime-executed runtime))
          (vector-push-extend (or options (ax:object)) (scripted-runtime-execute-options runtime))
          (let ((patch (ax:jget step "bindings_patch")))
            (when (ax::%object-p patch)
              (core::core-map-update (scripted-session-globals session) patch)))
          (when (ax:json-true-p (ax:jget step "close_before_result"))
            (setf (scripted-session-closed session) t))
          (let ((result (ax:jget step "result")))
            (if (eq result :null)
                (ax::envelope-result (scripted-session-globals session))
                result))))))

(defmethod ax::session-inspect-globals ((session scripted-session) options)
  (declare (ignore options))
  (if (%capability-enabled-p (scripted-session-runtime session) "inspect")
      (core::core-map-merge (scripted-session-globals session) (ax:object))
      "[runtime state inspection unavailable: runtime session does not implement inspect-globals]"))

(defmethod ax::session-snapshot-globals ((session scripted-session) options)
  (declare (ignore options))
  (unless (%capability-enabled-p (scripted-session-runtime session) "snapshot")
    (ax::%runtime-fail "unavailable"
                       "session-snapshot-globals is required to export AxAgent state"))
  (let ((globals (scripted-session-globals session))
        (entries (ax::%new-array)))
    (dolist (key (ax::%object-keys globals))
      (vector-push-extend (ax:object "name" key
                                     "type" (string-downcase
                                             (princ-to-string (type-of (gethash key globals))))
                                     "preview" (core::core-js-text (gethash key globals)))
                          entries))
    (ax:object "version" 1
               "entries" entries
               "bindings" (core::core-map-merge globals (ax:object))
               "globals" (core::core-map-merge globals (ax:object))
               "closed" (ax:json-boolean (scripted-session-closed session)))))

(defmethod ax::session-patch-globals ((session scripted-session) snapshot options)
  (unless (%capability-enabled-p (scripted-session-runtime session) "patch")
    (ax::%runtime-fail "unavailable"
                       "session-patch-globals is required to restore AxAgent state"))
  (let* ((snapshot (if (ax::%object-p snapshot) snapshot (ax:object)))
         (bindings (let ((value (ax:jget snapshot "bindings")))
                     (if (ax::%object-p value) value
                         (let ((globals (ax:jget snapshot "globals")))
                           (if (ax::%object-p globals) globals snapshot))))))
    (setf (scripted-session-globals session) (core::core-map-merge bindings (ax:object)))
    (let ((closed (ax:jget snapshot "closed")))
      (setf (scripted-session-closed session) (ax:json-true-p closed)))
    (ax::session-snapshot-globals session options)))

(defmethod ax::session-close ((session scripted-session))
  (setf (scripted-session-closed session) t)
  (ax:object "closed" ax:true))

;;; A session that implements nothing but execute, so the protocol's own
;;; fallbacks are the thing under test.

(defclass bare-runtime (ax::code-runtime) ())
(defclass bare-session (ax::code-session) ())

(defmethod ax::runtime-create-session ((runtime bare-runtime) globals options)
  (declare (ignore globals options))
  (make-instance 'bare-session))

(defmethod ax::session-execute ((session bare-session) code options)
  (declare (ignore options))
  (ax::envelope-result (ax:object "echo" code)))

;;; ------------------------------------------------------------------
;;; Envelopes
;;; ------------------------------------------------------------------

(deftest test-runtime-envelopes-match-the-published-shapes
  (assert-json-equal (ax::envelope-result (ax:object "answer" "ok"))
                     (jparse "{\"kind\":\"result\",\"result\":{\"answer\":\"ok\"}}")
                     "result envelope")
  (assert-json-equal (ax::envelope-error "boom")
                     (jparse "{\"kind\":\"error\",\"is_error\":true,\"error_category\":\"runtime\",\"error\":\"boom\"}")
                     "error envelope defaults to the runtime category")
  (assert-json-equal (ax::envelope-session-closed "closed")
                     (jparse "{\"kind\":\"error\",\"is_error\":true,\"error_category\":\"session_closed\",\"error\":\"closed\"}")
                     "session_closed envelope")
  (assert-json-equal (ax::envelope-timeout "slow")
                     (jparse "{\"kind\":\"error\",\"is_error\":true,\"error_category\":\"timeout\",\"error\":\"slow\"}")
                     "timeout envelope")
  (assert-json-equal (ax::envelope-final (ax:object "answer" "ok"))
                     (jparse "{\"type\":\"final\",\"args\":[{\"answer\":\"ok\"}]}")
                     "final envelope")
  (assert-json-equal (ax::envelope-ask-clarification (ax:object "question" "Which one?"))
                     (jparse "{\"type\":\"askClarification\",\"args\":[{\"question\":\"Which one?\"}]}")
                     "askClarification envelope")
  (assert-json-equal (ax::envelope-discover (ax:object "tools" (vector "docs")))
                     (jparse "{\"kind\":\"discover\",\"discover\":{\"tools\":[\"docs\"]}}")
                     "discover envelope")
  (assert-json-equal (ax::envelope-recall "prefs")
                     (jparse "{\"kind\":\"recall\",\"recall\":\"prefs\"}")
                     "recall envelope")
  (assert-json-equal (ax::envelope-used "mem-1" :reason "relevant" :stage "executor")
                     (jparse "{\"kind\":\"used\",\"used\":{\"id\":\"mem-1\",\"reason\":\"relevant\",\"stage\":\"executor\"}}")
                     "used envelope from a bare id")
  (assert-json-equal (ax::envelope-used (ax:object "id" "mem-2" "score" 3))
                     (jparse "{\"kind\":\"used\",\"used\":{\"id\":\"mem-2\",\"score\":3}}")
                     "used envelope keeps a record's own fields and adds nothing")
  (assert-json-equal (ax::envelope-status "success" "loaded")
                     (jparse "{\"kind\":\"status\",\"status\":{\"type\":\"success\",\"message\":\"loaded\"}}")
                     "status envelope")
  (assert-json-equal (ax::envelope-guide-agent "Use the loaded docs." "tools.review")
                     (jparse "{\"type\":\"guide_agent\",\"guidance\":\"Use the loaded docs.\",\"triggeredBy\":\"tools.review\"}")
                     "guide_agent envelope")
  (assert-json-equal (ax::envelope-guide-agent "Only guidance.")
                     (jparse "{\"type\":\"guide_agent\",\"guidance\":\"Only guidance.\"}")
                     "guide_agent without a trigger leaves triggeredBy out"))

(deftest test-completion-arguments-treat-one-array-as-the-argument-list
  ;; final([a, b]) and final(a, b) carry the same two values in every port, and
  ;; a single non-array argument must stay one argument rather than be spread.
  (assert-json-equal (ax::envelope-final (vector "a" "b"))
                     (ax::envelope-final "a" "b")
                     "one array argument is the argument list")
  (assert-json-equal (ax::envelope-final (ax:object "answer" "ok"))
                     (jparse "{\"type\":\"final\",\"args\":[{\"answer\":\"ok\"}]}")
                     "a single object argument stays one argument")
  (assert-json-equal (ax::envelope-final)
                     (jparse "{\"type\":\"final\",\"args\":[]}")
                     "no arguments is an empty argument list"))

(deftest test-runtime-capabilities-object
  (assert-json-equal (ax::runtime-capabilities :inspect nil :snapshot t :patch nil :abort t
                                               :language "Python"
                                               :usage-instructions "Use safe globals only.")
                     (jparse "{\"inspect\":false,\"snapshot\":true,\"patch\":false,\"abort\":true,\"language\":\"Python\",\"usage_instructions\":\"Use safe globals only.\"}")
                     "capabilities report every flag as a JSON boolean")
  (assert-json-equal (ax::runtime-capabilities)
                     (jparse "{\"inspect\":true,\"snapshot\":true,\"patch\":true,\"abort\":false,\"language\":\"JavaScript\",\"usage_instructions\":\"\"}")
                     "capability defaults: state yes, abort no, JavaScript"))

;;; ------------------------------------------------------------------
;;; The protocol client against a real worker
;;; ------------------------------------------------------------------

(deftest test-protocol-roundtrip-against-a-worker-process
  (with-protocol-runtime (runtime)
    (assert-json-subset (ax::runtime-usage-instructions runtime)
                        "fixture protocol runtime"
                        "capabilities carry the worker's usage instructions")
    (let ((session (ax::runtime-create-session runtime
                                               (ax:object "inputs" (ax:object "question" "adapter"))
                                               (ax:object "reservedNames" (vector "inputs" "final" "respond")
                                                          "timeoutMs" 123))))
      (assert-json-subset (ax::session-execute session "final()"
                                               (ax:object "abort" ax:true
                                                          "sessionId" "runtime-protocol-session"
                                                          "timeout" 7
                                                          "traceId" "runtime-protocol-trace"))
                          (jparse "{\"type\":\"final\",\"args\":[{\"answer\":\"fixture\"}]}")
                          "execute returns the worker's completion payload")
      (assert-json-subset (ax::session-inspect-globals session (ax:object))
                          (jparse "{\"answer\":\"fixture\",\"inputs\":{\"question\":\"adapter\"},\"__create_options\":{\"reservedNames\":[\"inputs\",\"final\",\"respond\"],\"timeoutMs\":123},\"__last_execute_options\":{\"abort\":true,\"sessionId\":\"runtime-protocol-session\",\"timeout\":7,\"traceId\":\"runtime-protocol-trace\"}}")
                          "inspect shows the create and execute options the client sent")
      (assert-json-subset (ax::session-snapshot-globals session (ax:object))
                          (jparse "{\"bindings\":{\"answer\":\"fixture\"}}")
                          "snapshot carries the session bindings")
      (assert-json-subset (ax::session-patch-globals session
                                                     (ax:object "bindings" (ax:object "answer" "patched"
                                                                                      "safe" ax:true))
                                                     (ax:object))
                          (jparse "{\"bindings\":{\"answer\":\"patched\",\"safe\":true}}")
                          "patch replaces the session bindings")
      (assert-json-subset (ax::session-close session)
                          (jparse "{\"closed\":true}")
                          "close reports the session closed"))))

(deftest test-protocol-execute-failures-become-categorised-envelopes
  ;; A failing step must not end the run: each category comes back as an
  ;; envelope Core can log and act on.
  (dolist (case '(("timeout()" "timeout" "fixture timeout")
                  ("sessionClosed()" "session_closed" "fixture session closed")
                  ("abort()" "abort" "fixture abort")
                  ("userError()" "user_error" "fixture user error")))
    (with-protocol-runtime (runtime)
      (let* ((session (ax::runtime-create-session runtime (ax:object) (ax:object)))
             (result (ax::session-execute session (first case) (ax:object))))
        (assert-json-equal result
                           (ax:object "kind" "error"
                                      "is_error" ax:true
                                      "error_category" (second case)
                                      "error" (third case))
                           (format nil "~a is reported as ~a" (first case) (second case)))))))

(deftest test-protocol-transport-failures-name-what-went-wrong
  (with-protocol-runtime (runtime)
    (expect-signals ax::runtime-protocol-error "unknown runtime protocol op"
                    "an unknown op is refused by the worker"
      (ax::%protocol-request runtime "unknown_op" nil nil)))
  (with-protocol-runtime (runtime :mode "id_mismatch")
    (expect-signals ax::runtime-protocol-error "response id mismatch"
                    "a crossed request id is caught rather than mistaken for the answer"
      (ax::%protocol-request runtime "capabilities" nil nil)))
  (with-protocol-runtime (runtime :mode "malformed_json")
    (expect-signals ax::runtime-protocol-error "runtime protocol invalid JSON response"
                    "an unparsable line is a protocol error"
      (ax::%protocol-request runtime "capabilities" nil nil)))
  (with-protocol-runtime (runtime :mode "eof")
    (expect-signals ax::runtime-protocol-error "closed without a response"
                    "a worker that closes its pipe is reported, not waited on"
      (ax::%protocol-request runtime "capabilities" nil nil)))
  (with-protocol-runtime (runtime :mode "nonzero")
    (let ((condition (expect-signals ax::runtime-protocol-error "exit code 7"
                                     "a worker that exits names its exit code"
                      (ax::%protocol-request runtime "capabilities" nil nil))))
      (expect-contains (princ-to-string condition) "fixture stderr before nonzero exit"
                       "the worker's standard error is carried into the message")))
  (with-protocol-runtime (runtime :mode "session_mismatch")
    (let ((session (ax::runtime-create-session runtime (ax:object) (ax:object))))
      (let ((result (ax::session-execute session "final()" (ax:object))))
        (assert-json-subset result
                            (jparse "{\"error_category\":\"protocol\"}")
                            "an answer for another session is refused")
        (expect-contains (ax:jget result "error") "session_id mismatch"
                         "the session mismatch is named"))))
  (with-protocol-runtime (runtime :mode "unavailable")
    (let ((session (ax::runtime-create-session runtime (ax:object) (ax:object))))
      (expect-signals ax::runtime-protocol-error "inspectGlobals unavailable"
                      "a capability the worker refuses signals instead of returning a value"
        (ax::session-inspect-globals session (ax:object))))))

;;; ------------------------------------------------------------------
;;; Timeout, cleanup and reentry
;;; ------------------------------------------------------------------

(deftest test-request-timeout-kills-the-worker-and-poisons-the-channel
  ;; A model can write an endless loop, and a process behind a pipe has no
  ;; other way to be interrupted. After the timeout the worker must be gone,
  ;; and the channel must stay refused: a late answer would otherwise be read
  ;; as the reply to the next request.
  (let ((runtime (make-protocol-runtime :timeout 1)))
    (unwind-protect
         (let* ((session (ax::runtime-create-session runtime (ax:object) (ax:object)))
                (started (get-internal-real-time))
                (result (ax::session-execute session "stall()" (ax:object)))
                (elapsed (/ (float (- (get-internal-real-time) started))
                            internal-time-units-per-second)))
           (assert-json-subset result (jparse "{\"kind\":\"error\",\"error_category\":\"timeout\"}")
                               "a stalled step becomes a timeout envelope")
           (expect-contains (ax:jget result "error") "timed out after 1"
                            "the timeout names its own budget")
           (expect (< elapsed 10)
                   (format nil "the timeout ended the wait promptly (took ~,2fs)" elapsed))
           (expect (not (runtime-process-alive-p runtime))
                   "the stalled worker process was terminated, not left running")
           (expect (integerp (uiop:wait-process (ax::process-runtime-process runtime)))
                   "the terminated worker was reaped, so it is not a zombie")
           (assert-json-subset (ax::session-execute session "final()" (ax:object))
                               (jparse "{\"error_category\":\"session_closed\"}")
                               "the poisoned channel refuses later steps")
           (expect-signals ax::runtime-protocol-error "timed out"
                           "a new session on a dead runtime is refused with the reason"
             (ax::runtime-create-session runtime (ax:object) (ax:object)))
           (expect (ax::session-closed-p session)
                   "a session of a dead runtime reports itself closed"))
      (ignore-errors (ax::runtime-shutdown runtime)))
    ;; Reentry: the failure belongs to that worker, not to the adapter.
    (with-protocol-runtime (fresh)
      (let ((session (ax::runtime-create-session fresh (ax:object) (ax:object))))
        (assert-json-subset (ax::session-execute session "echo recovered" (ax:object))
                            (jparse "{\"type\":\"final\",\"args\":[{\"echo\":\"recovered\"}]}")
                            "a fresh runtime works after another one timed out")))))

(deftest test-shutdown-cleans-up-the-worker-and-is-idempotent
  (let ((runtime (make-protocol-runtime)))
    (ax::runtime-create-session runtime (ax:object) (ax:object))
    (expect (runtime-process-alive-p runtime) "the worker is running before shutdown")
    (assert-json-equal (ax::runtime-shutdown runtime) (ax:object "shutdown" ax:true)
                       "shutdown reports success")
    (expect (not (runtime-process-alive-p runtime)) "shutdown left no worker process")
    (assert-json-equal (ax::runtime-shutdown runtime) (ax:object "shutdown" ax:true)
                       "shutdown twice is not an error")))

(deftest test-session-reentry-gives-a-fresh-scope
  ;; Closing a session must not close the runtime, and the next session must
  ;; not inherit the last one's globals.
  (with-protocol-runtime (runtime)
    (let ((first-session (ax::runtime-create-session runtime (ax:object) (ax:object))))
      (assert-json-subset (ax::session-execute first-session "count()" (ax:object))
                          (jparse "{\"args\":[{\"counter\":1}]}") "first step counts once")
      (assert-json-subset (ax::session-execute first-session "count()" (ax:object))
                          (jparse "{\"args\":[{\"counter\":2}]}") "the session kept its counter")
      (ax::session-close first-session)
      (assert-json-subset (ax::session-execute first-session "count()" (ax:object))
                          (jparse "{\"error_category\":\"session_closed\"}")
                          "a closed session refuses further steps")
      (let ((second-session (ax::runtime-create-session runtime (ax:object) (ax:object))))
        (assert-json-subset (ax::session-execute second-session "count()" (ax:object))
                            (jparse "{\"args\":[{\"counter\":1}]}")
                            "reentry on the same runtime starts from a fresh scope")))))

(deftest test-concurrent-sessions-do-not-cross-their-answers
  ;; Every request shares one pipe. Without the runtime lock a second thread
  ;; would read the first thread's response; the ids and session ids are what
  ;; prove each answer went back to its own caller.
  (with-protocol-runtime (runtime)
    (let* ((worker-count 4)
           (steps 5)
           (results (make-array worker-count :initial-element nil))
           (threads
             (loop for index from 0 below worker-count
                   collect (let ((index index))
                             (sb-thread:make-thread
                              (lambda ()
                                (handler-case
                                    (let ((session (ax::runtime-create-session runtime (ax:object)
                                                                               (ax:object)))
                                          (seen '()))
                                      (dotimes (step steps)
                                        (let* ((token (format nil "w~a-s~a" index step))
                                               (result (ax::session-execute
                                                        session
                                                        (format nil "echo ~a" token)
                                                        (ax:object))))
                                          (push (ax:jget (ax:jget (ax:jget result "args") 0) "echo")
                                                seen)))
                                      (setf (aref results index) (nreverse seen)))
                                  (error (condition)
                                    (setf (aref results index) (princ-to-string condition)))))
                              :name (format nil "ax-agent-runtime-test-~a" index))))))
      (dolist (thread threads) (sb-thread:join-thread thread :default nil))
      (loop for index from 0 below worker-count
            do (expect-equal (aref results index)
                             (loop for step from 0 below steps
                                   collect (format nil "w~a-s~a" index step))
                             (format nil "worker ~a received exactly its own answers" index))))))

;;; ------------------------------------------------------------------
;;; A real JavaScript engine behind the same protocol
;;; ------------------------------------------------------------------

(deftest test-javascript-profile-runs-code-and-survives-pause-and-resume
  ;; The QuickJS and Pyodide profiles are protocol servers, so what has to
  ;; hold here is that the client drives a genuine engine: code really runs,
  ;; the session's globals persist between steps, and a snapshot taken from
  ;; one session restores into the next -- pause and resume across a session
  ;; boundary, which is what an agent does when it is interrupted.
  (let ((runtime (ax::make-process-runtime (javascript-worker-command)
                                           :language "JavaScript" :timeout 20)))
    (unwind-protect
         (let ((session (ax::runtime-create-session runtime
                                                    (ax:object "inputs" (ax:object "question" "node"))
                                                    (ax:object "reservedNames" (vector "inputs")))))
           (assert-json-equal (ax::session-execute session
                                                   "final({ answer: inputs.question })"
                                                   (ax:object))
                              (jparse "{\"type\":\"final\",\"args\":[{\"answer\":\"node\"}]}")
                              "the engine ran the code and saw the injected inputs")
           (ax::session-execute session "counter = 41" (ax:object))
           (assert-json-equal (ax::session-execute session "counter = counter + 1; final({ counter })"
                                                   (ax:object))
                              (jparse "{\"type\":\"final\",\"args\":[{\"counter\":42}]}")
                              "a global bound by one step is visible to the next")
           (assert-json-equal (ax::session-execute session "askClarification('Which one?')"
                                                   (ax:object))
                              (jparse "{\"type\":\"askClarification\",\"args\":[\"Which one?\"]}")
                              "the engine's askClarification primitive produces the completion")
           (assert-json-subset (ax::session-execute session "throw new Error('boom')" (ax:object))
                               (jparse "{\"kind\":\"error\",\"error_category\":\"runtime\",\"error\":\"boom\"}")
                               "a thrown error is an error envelope, not a crash")
           (let ((snapshot (ax::session-snapshot-globals session (ax:object))))
             (assert-json-subset snapshot (jparse "{\"version\":1,\"bindings\":{\"counter\":42}}")
                                 "the snapshot carries the engine's own globals")
             (expect (not (nth-value 1 (gethash "inputs" (ax:jget snapshot "bindings"))))
                     "injected globals are not part of the user snapshot")
             (ax::session-close session)
             (let ((resumed (ax::runtime-create-session runtime (ax:object) (ax:object))))
               (assert-json-equal
                (ax::session-execute resumed
                                     "final({ counter: typeof counter === 'undefined' ? null : counter })"
                                     (ax:object))
                (jparse "{\"type\":\"final\",\"args\":[{\"counter\":null}]}")
                "a fresh session really is fresh before the restore")
               (ax::session-restore-state resumed snapshot (ax:object))
               (assert-json-equal (ax::session-execute resumed "final({ counter })" (ax:object))
                                  (jparse "{\"type\":\"final\",\"args\":[{\"counter\":42}]}")
                                  "the restored session resumed where the first one paused"))))
      (ignore-errors (ax::runtime-shutdown runtime)))))

;;; ------------------------------------------------------------------
;;; Protocol fallbacks
;;; ------------------------------------------------------------------

(deftest test-session-state-protocol-fallbacks-are-honest
  (let* ((runtime (make-instance 'bare-runtime))
         (session (ax::runtime-create-session runtime (ax:object) (ax:object))))
    (expect (ax::runtime-executable-p runtime) "a code-runtime subclass can run code")
    (expect (not (ax::runtime-executable-p (ax:object "language" "Python")))
            "a runtime descriptor object cannot run code")
    (expect (not (ax::runtime-supports-callables-p runtime))
            "a runtime that did not opt in takes no host callables")
    (expect-contains (ax::session-inspect-globals session (ax:object))
                     "runtime state inspection unavailable"
                     "a session without inspect says so instead of returning empty globals")
    (expect-signals ax::runtime-protocol-error "required to export AxAgent state"
                    "a session without snapshot refuses the export"
      (ax::session-export-state session (ax:object)))
    (expect-signals ax::runtime-protocol-error "required to restore AxAgent state"
                    "a session without patch refuses the restore"
      (ax::session-restore-state session (ax:object) (ax:object)))
    (assert-json-equal (ax::session-close session) (ax:object "closed" ax:true)
                       "the default close reports the session closed")))

(deftest test-process-runtime-takes-no-host-callables
  ;; The stdio protocol has no callback channel, so the worker owns its
  ;; callables and the agent must not be told otherwise.
  (with-protocol-runtime (runtime)
    (expect (not (ax::runtime-supports-callables-p runtime))
            "a process runtime reports that it takes no host callables")
    (expect-signals ax::runtime-protocol-error "does not accept the host callable"
                    "registering one anyway is refused"
      (ax::runtime-register-callable runtime "search" (lambda (params) params)))))

(deftest test-run-control-collects-the-lifecycle-and-stops-once
  ;; The caller's handle on a run: it hears the lifecycle, it stops the run at
  ;; the next boundary, and it carries steering updates until Core asks.
  (let* ((seen (ax::%new-array))
         (control (ax::make-run-control :listener (lambda (event)
                                                    (vector-push-extend event seen)))))
    (expect (not (ax::run-control-aborted-p control)) "a new control is not aborted")
    (expect-equal (core::core-run-control-aborted control) ax:false
                  "Core reads a live control as not cancelled")
    (ax::run-control-emit control (ax:object "type" "started" "path" "root"))
    (ax::run-control-emit control (ax:object "type" "completed" "path" "root"))
    (assert-json-equal (ax::run-control-events control)
                       (vector (ax:object "type" "started" "path" "root")
                               (ax:object "type" "completed" "path" "root"))
                       "the control kept both events in order")
    (assert-json-equal seen (ax::run-control-events control)
                       "the listener saw exactly what the control recorded")
    ;; Steering updates queue until a consumer asks, and taking empties them.
    ;; An update carries its own id and target, because the queued and applied
    ;; events name the update they refer to by id.
    (ax::run-control-steer control "prefer short answers")
    (ax::run-control-steer control "cite the source")
    (expect-equal (ax::run-control-pending-count control) 2 "both updates are queued")
    (assert-json-equal (ax::run-control-take-pending control)
                       (vector (ax:object "type" "steer" "text" "prefer short answers"
                                          "id" 1 "target" "root")
                               (ax:object "type" "steer" "text" "cite the source"
                                          "id" 2 "target" "root"))
                       "taking the queue returns the updates in order, with monotonic ids")
    (expect-equal (ax::run-control-pending-count control) 0 "taking the queue empties it")
    (assert-json-equal (ax::run-control-take-pending control) (vector)
                       "taking an empty queue is empty, not an error")
    ;; Queueing announces itself: a caller watching the run learns the update
    ;; was accepted and which id to match when it is applied.
    (assert-json-equal seen
                       (vector (ax:object "type" "started" "path" "root")
                               (ax:object "type" "completed" "path" "root")
                               (ax:object "type" "queued" "path" "root" "updateId" 1)
                               (ax:object "type" "queued" "path" "root" "updateId" 2))
                       "each queued update announced itself with its own id")
    ;; Stopping is one-shot, so a second caller cannot overwrite the reason.
    (expect (ax::run-control-abort control "caller stopped it") "the first abort takes")
    (expect (not (ax::run-control-abort control "a different reason"))
            "a second abort is refused")
    (expect-equal (ax::run-control-reason control) "caller stopped it"
                  "the first reason is the one kept")
    (expect-equal (core::core-run-control-aborted control) ax:true
                  "Core reads a stopped control as cancelled")
    (assert-json-equal (aref (ax::run-control-events control) 4)
                       (ax:object "type" "aborted" "path" "root")
                       "the first abort announced itself at the root path")
    (expect-equal (length (ax::run-control-events control)) 5
                  "the refused second abort announced nothing")
    ;; An aborted run takes no further updates: it will never reach another
    ;; boundary, so a queued update would wait for a consumer that is not
    ;; coming.
    (expect-signals ax:ax-error "aborted"
                    "steering an aborted run is refused, not silently queued"
      (ax::run-control-steer control "one more thing"))
    (expect-equal (ax::run-control-pending-count control) 0
                  "the refused update did not reach the queue")))

(deftest test-a-worker-calls-back-into-the-host-it-negotiated-with
  ;; The real engine, calling real host functions mid-execute. A dotted name
  ;; has to read in the code the way it reads in the registry, and a host
  ;; function that signals has to reach the code as a failed call rather than
  ;; as a value.
  (let ((runtime (ax::make-process-runtime (javascript-worker-command)
                                           :language "JavaScript" :timeout 20)))
    (unwind-protect
         (progn
           (expect (ax::runtime-supports-callables-p runtime)
                   "a worker advertising host_calls accepts host callables")
           (ax::runtime-register-callable
            runtime "crm.lookup"
            (lambda (params) (ax:object "tier" (ax:jget params "id"))))
           (ax::runtime-register-callable
            runtime "llmQuery" (lambda (params) (declare (ignore params)) "sub-answer"))
           (ax::runtime-register-callable
            runtime "breaks" (lambda (params) (declare (ignore params))
                               (error "the host callable failed")))
           (let ((session (ax::runtime-create-session runtime (ax:object) (ax:object))))
             (assert-json-equal
              (ax::session-execute session
                                   "const r = await crm.lookup({id:'c-7'}); final('ok', {t: r.tier})"
                                   (ax:object))
              (jparse "{\"type\":\"final\",\"args\":[\"ok\",{\"t\":\"c-7\"}]}")
              "a dotted host callable runs and its result reaches the code")
             (assert-json-equal
              (ax::session-execute session
                                   "const q = await llmQuery([{query:'x'}]); final('ok', {q})"
                                   (ax:object))
              (jparse "{\"type\":\"final\",\"args\":[\"ok\",{\"q\":\"sub-answer\"}]}")
              "a flat host callable runs the same way")
             ;; A host callable that signals is a failed call, not a value: the
             ;; code must be able to tell them apart.
             (assert-json-equal
              (ax::session-execute session
                                   "try { await breaks({}); final('ok', {}) } catch (e) { final('caught', { message: String(e.message) }) }"
                                   (ax:object))
              (jparse "{\"type\":\"final\",\"args\":[\"caught\",{\"message\":\"the host callable failed\"}]}")
              "a host callable that signals reaches the code as a rejection carrying its reason")
             ;; A name nobody registered is not reachable at all.
             (assert-json-subset
              (ax::session-execute session "await nope({})" (ax:object))
              (jparse "{\"is_error\":true}")
              "an unregistered name is not callable")
             ;; Once the run that registered them is over, the names are gone.
             (ax::runtime-retire-callables runtime)
             (let ((result (ax::session-execute
                            session "const r = await crm.lookup({id:'c-7'}); final('ok', {})"
                            (ax:object))))
               (expect-contains (core::core-js-text (ax:jget result "error" ""))
                                "no host callable named crm.lookup is registered"
                                "a retired invocation's callable is refused, not served"))))
      (ignore-errors (ax::runtime-shutdown runtime)))))

(deftest test-a-host-call-must-name-the-request-and-session-in-flight
  ;; A cooperative worker always correlates correctly, so these frames are
  ;; built by hand: the host must refuse a call that arrives for another
  ;; request, another session, or with no callback id, because a worker able
  ;; to ask the host to run a name out of band is the whole risk the
  ;; correlation exists to remove.
  (let ((runtime (ax::make-process-runtime (javascript-worker-command)
                                           :language "JavaScript" :timeout 20))
        (served '()))
    (unwind-protect
         (progn
           (ax::runtime-register-callable
            runtime "ran" (lambda (params) (declare (ignore params))
                            (push :called served) (ax:object "ok" ax:true)))
           (let ((frame (lambda (&key (id "cb1") (request "7") (session "s1") (name "ran"))
                          (let ((out (ax:object "op" "host_call" "name" name
                                                "params" (ax:object))))
                            (unless (null id) (ax::%set-key out "callback_id" id))
                            (ax::%set-key out "request_id" request)
                            (ax::%set-key out "session_id" session)
                            out))))
             ;; Each refusal is observed through the reply the host writes.
             (flet ((reply-for (f)
                      (let ((stream (make-string-output-stream)))
                        (ax::%serve-host-call runtime stream f "7" "s1")
                        (jparse (string-trim '(#\Newline) (get-output-stream-string stream))))))
               (assert-json-subset (reply-for (funcall frame))
                                   (jparse "{\"id\":\"cb1\",\"ok\":true}")
                                   "a correctly correlated call is served")
               (expect-equal (length served) 1 "and it really ran the host function")
               (let ((crossed (reply-for (funcall frame :request "9"))))
                 (expect-equal (ax:jget crossed "ok") ax:false
                               "a call naming another request is refused")
                 (expect-contains (core::core-js-text
                                   (ax:jget (ax:jget crossed "error") "message" ""))
                                  "names request" "and the refusal says why"))
               (let ((crossed (reply-for (funcall frame :session "s9"))))
                 (expect-equal (ax:jget crossed "ok") ax:false
                               "a call naming another session is refused"))
               (let ((anonymous (reply-for (funcall frame :id nil))))
                 (expect-equal (ax:jget anonymous "ok") ax:false
                               "a call with no callback id is refused"))
               (let ((unknown (reply-for (funcall frame :name "not-registered"))))
                 (expect-equal (ax:jget unknown "ok") ax:false
                               "a call naming an unregistered function is refused"))
               (expect-equal (length served) 1
                             "no refused call ever reached the host function"))))
      (ignore-errors (ax::runtime-shutdown runtime)))))

(deftest test-a-worker-without-the-capability-takes-no-callables
  ;; The extension is negotiated. The python worker in this directory does not
  ;; advertise host_calls, so it must look exactly as it did before the
  ;; extension existed.
  (with-protocol-runtime (runtime)
    (expect (not (ax::runtime-supports-callables-p runtime))
            "a worker that does not advertise host_calls accepts no callables")
    (expect-signals ax::runtime-protocol-error "does not accept the host callable"
                    "registering one against it is refused"
      (ax::runtime-register-callable runtime "search" (lambda (params) params)))))

(deftest test-conformance-refuses-a-root-missing-a-suite
  ;; The gate reads the conformance counts as evidence, and a count cannot
  ;; say how much of the inventory it covered. A root holding one suite --
  ;; a broken checkout, or AXIR_CONFORMANCE_DIR pointed one level wrong --
  ;; used to return a clean (1 0 0 0) over a single file.
  (let* ((root (merge-pathnames (format nil "ax-conformance-inventory-~a/" (random 100000))
                                (uiop:temporary-directory)))
         (present (merge-pathnames "axagent/" root)))
    (unwind-protect
         (progn
           (ensure-directories-exist present)
           (with-open-file (out (merge-pathnames "only-one.json" present)
                                :direction :output :if-exists :supersede)
             (write-string "{\"kind\":\"agent_forward\",\"name\":\"only-one\"}" out))
           (let ((previous (uiop:getenv "AXIR_CONFORMANCE_DIR")))
             (unwind-protect
                  (progn
                    (sb-posix:setenv "AXIR_CONFORMANCE_DIR" (namestring root) 1)
                    ;; The other suite is absent entirely.
                    (expect-signals test-failure "axagent-real/"
                                    "a root missing a whole suite is refused, not quietly run"
                      (%fixture-files))
                    ;; Present but empty is the same claim: nothing to run is
                    ;; not the same as nothing to say.
                    (ensure-directories-exist (merge-pathnames "axagent-real/" root))
                    (expect-signals test-failure "missing or empty"
                                    "a suite directory with no fixtures is refused too"
                      (%fixture-files))
                    ;; And a complete root is accepted, so the guard refuses an
                    ;; incomplete inventory rather than everything.
                    (with-open-file (out (merge-pathnames "axagent-real/one.json" root)
                                         :direction :output :if-exists :supersede)
                      (write-string "{\"kind\":\"agent_runtime_real\",\"name\":\"one\"}" out))
                    (expect-equal (length (%fixture-files)) 2
                                  "both suites together are what a run covers"))
               (if (and previous (plusp (length previous)))
                   (sb-posix:setenv "AXIR_CONFORMANCE_DIR" previous 1)
                   (sb-posix:unsetenv "AXIR_CONFORMANCE_DIR")))))
      (ignore-errors (uiop:delete-directory-tree root :validate t)))))

(deftest test-run-control-scopes-an-update-to-its-target-and-below
  ;; An update reaches its own target and the nodes under it, and never reaches
  ;; back up. The separator is part of the test: a bare prefix match would hand
  ;; "root/left"'s steering to "root/leftover", an unrelated sibling.
  (let ((control (ax::make-run-control)))
    (ax::run-control-steer control "root change")
    (ax::run-control-set-thinking-token-budget control "high" :target "root/left")
    (expect-equal (length (ax::run-control-pending control :path "root/right")) 1
                  "a sibling sees the root update and not the other node's")
    (expect-equal (length (ax::run-control-pending control :path "root/left/child")) 2
                  "a descendant of the targeted node sees both")
    (expect-equal (length (ax::run-control-pending control :path "root/leftover")) 1
                  "a name that merely starts the same way is not a descendant")
    (expect-equal (length (ax::run-control-pending control :path "root/left" :after 2)) 0
                  "a consumer that has already seen both updates is handed neither")
    (expect-equal (length (ax::run-control-pending control :path "root/left" :after 1)) 1
                  "a cursor hands back only what the consumer has not seen")
    (expect-equal (ax::run-control-pending-count control "root/leftover") 1
                  "the count scopes the same way the reader does")
    ;; Reading leaves the queue alone, so a root update is still there for a
    ;; descendant that has not run yet.
    (expect-equal (ax::run-control-pending-count control) 2
                  "reading the queue consumed nothing")
    ;; Taking scoped to one node leaves the sibling's steering behind.
    (assert-json-equal (ax::run-control-take-pending control "root/left")
                       (vector (ax:object "type" "steer" "text" "root change"
                                          "id" 1 "target" "root")
                               (ax:object "type" "thinking" "level" "high"
                                          "id" 2 "target" "root/left"))
                       "taking for a node takes the root update and its own")
    (expect-equal (ax::run-control-pending-count control) 0
                  "nothing targeted elsewhere was left behind in this run")))

(deftest test-run-control-refuses-an-update-that-says-nothing
  ;; An update that cannot be acted on is refused where the caller can still
  ;; see it, rather than queued and quietly dropped by the provider.
  (let ((control (ax::make-run-control)))
    (expect-signals ax:ax-error "must not be empty"
                    "empty steering text is refused"
      (ax::run-control-steer control "   "))
    (expect-signals ax:ax-error "level must be one of"
                    "a thinking budget outside the provider contract is refused"
      (ax::run-control-set-thinking-token-budget control "exhaustive"))
    (expect-equal (ax::run-control-pending-count control) 0
                  "neither refused update reached the queue")
    ;; Every level the contract names is accepted, so the check cannot be a
    ;; stricter list than the providers actually support.
    (dolist (level ax::+run-control-thinking-levels+)
      (ax::run-control-set-thinking-token-budget control level))
    (expect-equal (ax::run-control-pending-count control)
                  (length ax::+run-control-thinking-levels+)
                  "each contract level queued an update")))

(deftest test-core-drives-a-run-control-through-the-host-bridge
  ;; Core holds a control as an opaque value, so everything it does to one
  ;; goes through the bridge. A listener that signals must not fail the run.
  (let ((control (ax::make-run-control :listener (lambda (event)
                                                   (declare (ignore event))
                                                   (error "a watching caller broke")))))
    (core::core-host-call control "emit" (vector (ax:object "type" "started" "path" "root")))
    (assert-json-equal (core::core-host-get control "events")
                       (vector (ax:object "type" "started" "path" "root"))
                       "the event was recorded even though the listener signalled")
    (expect-equal (core::core-host-get control "aborted") ax:false
                  "Core reads the abort flag through the bridge")
    ;; A provider boundary hands back a whole update object when it re-queues
    ;; one; the control fills in the id and target it needs to be matched
    ;; later, and leaves the rest of the shape alone.
    (core::core-host-call control "steer" (vector (ax:object "guidance" "slow down")))
    (expect-equal (core::core-host-get control "pending_count") 1
                  "Core reads the pending count through the bridge")
    ;; A caller steering by hand passes the text instead, and a stage scopes it.
    (core::core-host-call control "steer" (vector "answer in one line" "root/responder"))
    (core::core-host-call control "set_thinking_token_budget" (vector "low" "root/responder"))
    (expect-equal (core::core-host-call control "pending_count" (vector "root/other")) 1
                  "a sibling stage sees only the unscoped update")
    (assert-json-equal (core::core-host-call control "pending" (vector "root/responder" 1))
                       (vector (ax:object "type" "steer" "text" "answer in one line"
                                          "id" 2 "target" "root/responder")
                               (ax:object "type" "thinking" "level" "low"
                                          "id" 3 "target" "root/responder"))
                       "Core reads past a cursor through the bridge without consuming")
    (expect-equal (core::core-host-get control "pending_count") 3
                  "reading through the bridge consumed nothing")
    (assert-json-equal (core::core-host-call control "take_pending" (vector "root/responder"))
                       (vector (ax:object "guidance" "slow down" "id" 1 "target" "root")
                               (ax:object "type" "steer" "text" "answer in one line"
                                          "id" 2 "target" "root/responder")
                               (ax:object "type" "thinking" "level" "low"
                                          "id" 3 "target" "root/responder"))
                       "Core takes the queue for one stage through the bridge")
    (expect-equal (core::core-host-get control "pending_count") 0
                  "the stage's turn emptied what reached it")
    (expect-equal (core::core-host-call control "abort" (vector "Core stopped it")) ax:true
                  "Core stops the run through the bridge")
    (expect-equal (core::core-host-get control "reason") "Core stopped it"
                  "the reason Core gave is the one kept")
    (expect-signals ax::runtime-protocol-error "unknown run control host method"
                    "an unknown control method is named, not ignored"
      (core::core-host-call control "rewind" (vector)))))

;;; ------------------------------------------------------------------
;;; The Core host-object bridge
;;; ------------------------------------------------------------------

(deftest test-core-reaches-a-runtime-and-a-session-through-the-host-bridge
  (with-protocol-runtime (runtime)
    (expect-equal (core::core-host-get runtime "language") "JavaScript"
                  "Core reads a runtime's language through the bridge")
    (expect-equal (core::core-host-get runtime "usage_instructions") "fixture protocol runtime"
                  "Core reads a runtime's usage instructions through the bridge")
    (expect-equal (core::core-host-get runtime "nothing-like-this" "fallback") "fallback"
                  "an unknown key yields the caller's fallback")
    (let ((session (core::core-host-call runtime "create_session"
                                       (vector (ax:object) (ax:object)))))
      (expect (typep session 'ax::code-session) "create_session returns a session")
      (assert-json-subset (core::core-host-call session "execute" (vector "final()" (ax:object)))
                          (jparse "{\"type\":\"final\"}")
                          "Core executes a step through the bridge")
      (expect-equal (core::core-host-get session "closed") ax:false
                    "a live session reports itself open")
      (core::core-host-call session "close" (vector))
      (expect-equal (core::core-host-get session "closed") ax:true
                    "a closed session reports itself closed")
      (expect-signals ax::runtime-protocol-error "unknown runtime session host method"
                      "an unknown session method is named, not ignored"
        (core::core-host-call session "teleport" (vector))))))

(deftest test-core-runtime-language-and-usage-intrinsics
  (expect-equal (core::core-agent-runtime-language (ax:object "language" "Python")) "Python"
                "a runtime descriptor's language wins")
  (expect-equal (core::core-agent-runtime-language (ax:object)) "JavaScript"
                "a descriptor with no language falls back to JavaScript")
  (expect-equal (core::core-agent-runtime-language (ax:object "language" "   ")) "JavaScript"
                "a blank language is no language")
  (expect-equal (core::core-agent-runtime-language (make-scripted-runtime :language "Python")) "Python"
                "a host runtime reports its own language")
  (expect-equal (core::core-agent-runtime-usage-instructions
                 (ax:object "usageInstructions" "camel"))
                "camel" "a descriptor's camelCase usage instructions are read")
  (expect-equal (core::core-agent-runtime-usage-instructions
                 (ax:object "usage_instructions" "snake"))
                "snake" "a descriptor's snake_case usage instructions are read")
  (expect-equal (core::core-agent-runtime-usage-instructions (ax:object)) ""
                "no usage instructions is the empty string")
  (expect-equal (core::core-agent-runtime-is-executable (ax:object "language" "Python")) ax:false
                "a descriptor is not executable")
  (expect-equal (core::core-agent-runtime-is-executable (make-scripted-runtime)) ax:true
                "a host runtime is executable")
  (expect-signals ax::runtime-protocol-error "does not implement the code-runtime protocol"
                  "Core refuses to create a session on a descriptor"
    (core::core-agent-runtime-create-session (ax:object "language" "Python") (ax:object) (ax:object)))
  (expect-signals ax::runtime-protocol-error "code session is not active"
                  "Core refuses to execute without a session"
    (core::core-agent-runtime-execute :null "final()" (ax:object)))
  (expect-contains (core::core-agent-runtime-inspect :null (ax:object))
                   "no runtime session"
                   "inspecting without a session says so")
  (assert-json-equal (core::core-agent-runtime-close :null) (ax:object "closed" ax:true)
                     "closing without a session is still closed"))

(deftest test-scripted-runtime-drives-the-session-intrinsics
  (let* ((runtime (make-scripted-runtime
                   :script (list (ax:object "expected_code" "final()"
                                            "bindings_patch" (ax:object "answer" "scripted")
                                            "result" (ax::envelope-final
                                                      (ax:object "answer" "scripted"))))))
         (session (core::core-agent-runtime-create-session runtime
                                                         (ax:object "inputs" (ax:object "q" "x"))
                                                         (ax:object "timeoutMs" 50))))
    (assert-json-equal (core::core-agent-runtime-execute session "final()" (ax:object "traceId" "t1"))
                       (jparse "{\"type\":\"final\",\"args\":[{\"answer\":\"scripted\"}]}")
                       "the scripted step answered its envelope")
    (assert-json-subset (ax:jget (scripted-runtime-create-requests runtime) 0)
                        (jparse "{\"globals\":{\"inputs\":{\"q\":\"x\"}},\"options\":{\"timeoutMs\":50}}")
                        "the create request recorded the globals and options Core sent")
    (assert-json-subset (ax:jget (scripted-runtime-execute-options runtime) 0)
                        (jparse "{\"traceId\":\"t1\"}")
                        "the execute options reached the session")
    (assert-json-subset (core::core-agent-runtime-export-state session (ax:object))
                        (jparse "{\"version\":1,\"bindings\":{\"answer\":\"scripted\"}}")
                        "the export carries the patched globals")
    (assert-json-subset (core::core-agent-runtime-restore-state
                         session (ax:object "bindings" (ax:object "answer" "restored")) (ax:object))
                        (jparse "{\"bindings\":{\"answer\":\"restored\"}}")
                        "the restore replaced the globals")
    (assert-json-equal (core::core-agent-runtime-close session) (ax:object "closed" ax:true)
                       "the close reported the session closed"))
  (let* ((runtime (make-scripted-runtime :capabilities (ax:object "snapshot" ax:false
                                                                  "patch" ax:false)))
         (session (core::core-agent-runtime-create-session runtime (ax:object) (ax:object))))
    (expect-signals ax::runtime-protocol-error "required to export AxAgent state"
                    "a runtime that reports no snapshot capability refuses the export"
      (core::core-agent-runtime-export-state session (ax:object)))
    (expect-signals ax::runtime-protocol-error "required to restore AxAgent state"
                    "a runtime that reports no patch capability refuses the restore"
      (core::core-agent-runtime-restore-state session (ax:object) (ax:object)))))

(defclass cancelling-control ()
  ((aborted :initform nil :accessor control-aborted))
  (:documentation "A host run control, to prove the cancellation bridge."))

(defmethod core::core-host-get ((target cancelling-control) key &optional (fallback :null))
  (if (equal key "aborted") (ax:json-boolean (control-aborted target)) fallback))

(deftest test-run-control-cancellation-boundary
  ;; Core asks this between turns, so a run stops at a step boundary rather
  ;; than by interrupting a thread. Absent and present-but-false must both
  ;; read as "keep going", or a run with a control could never start.
  (expect-equal (core::core-run-control-aborted :null) ax:false
                "no run control is not cancelled")
  (expect-equal (core::core-run-control-aborted nil) ax:false
                "an absent run control is not cancelled")
  (expect-equal (core::core-run-control-aborted (ax:object)) ax:false
                "a control that says nothing is not cancelled")
  (expect-equal (core::core-run-control-aborted (ax:object "aborted" ax:false)) ax:false
                "a control that says false is not cancelled")
  (expect-equal (core::core-run-control-aborted (ax:object "aborted" ax:true)) ax:true
                "a control that says true is cancelled")
  (expect-equal (core::core-run-control-aborted "not a control") ax:false
                "a value that is not a control is not cancelled")
  ;; A host control object answers through the same bridge Core uses for a
  ;; runtime or a session, so a caller's own control class needs no wrapper.
  (let ((control (make-instance 'cancelling-control)))
    (expect-equal (core::core-run-control-aborted control) ax:false
                  "a host control starts un-cancelled")
    (setf (control-aborted control) t)
    (expect-equal (core::core-run-control-aborted control) ax:true
                  "a cancelled host control reports through the host bridge")))

(deftest test-callable-invocation-tells-a-protocol-tool-from-an-ax-tool
  ;; Three shapes reach the callable boundary and must not be confused.
  ;;
  ;; A protocol tool published by an MCP or UCP server keeps its handler
  ;; under the keyword key :HANDLER and carries the server's own JSON
  ;; Schema. Running it through the Ax tool validator would reject schemas
  ;; that validator cannot express, so it must be called directly, and it
  ;; takes the execution context as a second argument.
  ;;
  ;; An Ax tool keeps its handler under the string key and must be
  ;; validated, because its schema is Ax's promise to the model.
  (let* ((seen (list))
         (native (ax:object "name" "remote"
                            "description" "A server tool"
                            ;; A schema the Ax tool validator refuses: the
                            ;; wrong implementation would reject this call.
                            "parameters" (jparse "{\"type\":\"object\",\"properties\":{\"q\":{\"type\":\"string\",\"pattern\":\"^a\"}},\"oneOf\":[{\"required\":[\"q\"]}]}")))
         )
    (setf (gethash :handler native)
          (lambda (arguments context)
            (push (list :native arguments context) seen)
            (ax:object "title" "Docs")))
    (let ((result (core::%native-callable-handler native)))
      (expect (functionp result) "a protocol tool's keyword handler is found"))
    (expect (null (core::%native-callable-handler
                   (ax:object "name" "plain" "handler" (lambda (arguments) arguments))))
            "a string-keyed handler is not mistaken for a protocol handler")
    (expect (null (core::%native-callable-handler (ax:object "name" "inert")))
            "a record with no handler at all is not a protocol tool")
    (expect (null (core::%native-callable-handler "not a record"))
            "a non-record is not a protocol tool")
    ;; Called directly, with the context, and with the schema untouched.
    (let ((value (funcall (core::%native-callable-handler native)
                          (ax:object "q" "zebra") :null)))
      (assert-json-equal value (ax:object "title" "Docs")
                         "the protocol handler ran without schema validation")
      (assert-json-equal (second (first seen)) (ax:object "q" "zebra")
                         "the protocol handler saw the arguments unchanged")
      (expect-equal (third (first seen)) :null
                    "the protocol handler was given an execution context argument"))
    ;; The Ax tool path still validates: a tool whose arguments are wrong is
    ;; refused rather than invoked.
    (let ((spec (ax:tool :name "local_echo"
                         :parameters (ax:object "type" "object"
                                                "properties" (ax:object "text" (ax:object "type" "string"))
                                                "required" (vector "text"))
                         :handler (lambda (arguments) (ax:jget arguments "text")))))
      (multiple-value-bind (result problems) (ax:invoke-tool spec (ax:object "text" "hi"))
        (expect-equal problems nil "a valid Ax tool call passes validation")
        (expect-equal result "hi" "the Ax tool handler ran"))
      (multiple-value-bind (result problems) (ax:invoke-tool spec (ax:object))
        (declare (ignore result))
        (expect (and problems t) "an invalid Ax tool call is refused before the handler")))
    (expect-equal (length seen) 1 "the protocol handler ran exactly once")))

;;; ------------------------------------------------------------------
;;; The Docker session adapter
;;; ------------------------------------------------------------------
;;;
;;; Against a scripted loopback HTTP server, not a daemon. Nothing here installs or
;;; starts Docker, and none of this is evidence that a live engine behaves
;;; the same way: what it proves is the request each method sends, the result
;;; it reports, and what it refuses.

(defstruct (scripted-docker (:conc-name scripted-docker-))
  (containers (list)) (pulled (list)) (execs (list)) (stopped (list)) (removed (list))
  (started (list)) (created (list)))

(defun %scripted-container (id &key (status "running") tag)
  (ax:object "Id" id
             "Names" (vector (concatenate 'string "/" id))
             "Image" "alpine:3"
             "State" (ax:object "Status" status
                                "Running" (ax:json-boolean (string= status "running")))
             "Status" status
             "Labels" (if tag
                          (ax:object ax::+docker-tag-label+ tag)
                          (ax:object))))

(defun %path-of (request-line)
  (let* ((parts (uiop:split-string request-line :separator " "))
         (method (first parts))
         (target (or (second parts) "")))
    (values method target)))

(defun %segment-after (target prefix)
  "The path segment of TARGET that follows PREFIX, up to the next slash."
  (let ((start (search prefix target)))
    (when start
      (let* ((from (+ start (length prefix)))
             (rest (subseq target from))
             (slash (position #\/ rest))
             (query (position #\? rest)))
        (subseq rest 0 (or (and slash query (min slash query)) slash query))))))

(defun %scripted-docker-responder (state)
  "A scripted Docker Engine: enough of the API for the adapter's own requests."
  (lambda (request stream)
    (multiple-value-bind (method target) (%path-of (ax:jget request "requestLine"))
      (labels ((json (status value) (ax::%write-http-response stream status (ax:encode-json value)))
               (text (status value) (ax::%write-http-response stream status value))
               (find-container (id)
                 (find id (scripted-docker-containers state)
                       :key (lambda (container) (ax:jget container "Id")) :test #'equal)))
        (cond
          ((and (string= method "POST") (search "/images/create" target))
           (push target (scripted-docker-pulled state))
           (json 200 (ax:object)))

          ((and (string= method "POST") (string= target "/containers/create"))
           (let* ((config (jparse (ax:jget request "body")))
                  (id (format nil "c~a" (1+ (length (scripted-docker-created state)))))
                  (labels-object (ax:jget config "Labels"))
                  (tag (and (ax::%object-p labels-object)
                            (let ((value (ax:jget labels-object ax::+docker-tag-label+)))
                              (unless (eq value :null) value)))))
             (push config (scripted-docker-created state))
             (push (%scripted-container id :status "created" :tag tag)
                   (scripted-docker-containers state))
             (json 201 (ax:object "Id" id))))

          ((and (string= method "GET") (search "/containers/json" target))
           (json 200 (coerce (reverse (scripted-docker-containers state)) 'vector)))

          ((and (string= method "GET") (search "/json" target)
                (search "/containers/" target))
           (let ((container (find-container (%segment-after target "/containers/"))))
             (if container (json 200 container) (json 404 (ax:object "message" "no such container")))))

          ((and (string= method "POST") (search "/start" target)
                (search "/containers/" target))
           (let ((id (%segment-after target "/containers/")))
             (push id (scripted-docker-started state))
             (let ((container (find-container id)))
               (if container
                   (progn (ax::%set-key container "State"
                                        (ax:object "Status" "running" "Running" ax:true))
                          (ax::%set-key container "Status" "running")
                          (json 200 (ax:object)))
                   (json 404 (ax:object "message" "no such container"))))))

          ((and (string= method "POST") (search "/exec" target)
                (search "/containers/" target))
           (push (jparse (ax:jget request "body")) (scripted-docker-execs state))
           (json 201 (ax:object "Id" "exec-1")))

          ((and (string= method "POST") (search "/exec/" target) (search "/start" target))
           (text 200 "total 0"))

          ((and (string= method "GET") (search "/logs" target))
           (text 200 (format nil "line one~%line two")))

          ((and (string= method "POST") (search "/stop" target))
           (push (%segment-after target "/containers/") (scripted-docker-stopped state))
           (json 200 (ax:object)))

          ((string= method "DELETE")
           (push (%segment-after target "/containers/") (scripted-docker-removed state))
           (json 200 (ax:object)))

          (t (json 500 (ax:object "message" (format nil "unexpected ~a ~a" method target)))))))))

(defmacro with-scripted-docker ((state-var session-var &rest session-options) &body body)
  `(let* ((,state-var (make-scripted-docker))
          (server (ax::start-loopback-server (%scripted-docker-responder ,state-var))))
     (declare (ignorable ,state-var))
     (unwind-protect
          (let ((,session-var (ax::make-docker-session
                               :api-url (ax::loopback-url server) ,@session-options)))
            ,@body)
       (ax::stop-loopback-server server))))

(deftest test-docker-session-creates-tags-and-runs-a-command
  (with-scripted-docker (state session)
    (let ((created (ax::docker-create-container session
                                                :image-name "alpine:3"
                                                :volumes '(("/host/work" . "/work"))
                                                :tag "ax-lisp-test")))
      (expect-equal (ax:jget created "Id") "c1" "create returns the container id")
      (expect-equal (ax::docker-session-container-id session) "c1"
                    "the session attached itself to the new container")
      (expect (find-if (lambda (target) (search "fromImage=alpine%3A3" target))
                       (scripted-docker-pulled state))
              "the image was pulled with its name percent-encoded")
      (let ((config (first (scripted-docker-created state))))
        (assert-json-subset config
                            (jparse "{\"Image\":\"alpine:3\",\"Tty\":true,\"OpenStdin\":false,\"HostConfig\":{\"Binds\":[\"/host/work:/work\"]},\"Labels\":{\"com.example.tag\":\"ax-lisp-test\"}}")
                            "the create request carried the image, binds and tag")))
    (expect-equal (ax::docker-execute-command session "ls -l" :start-timeout 2 :poll-interval 0.01)
                  "total 0" "the command's output is returned")
    (expect-equal (scripted-docker-started state) '("c1")
                  "a container that was not running was started first")
    (assert-json-subset (first (scripted-docker-execs state))
                        (jparse "{\"Cmd\":[\"sh\",\"-c\",\"ls -l\"],\"AttachStdout\":true,\"AttachStderr\":true}")
                        "the command ran through sh -c with both streams attached")
    (expect-equal (ax::docker-container-logs session) (format nil "line one~%line two")
                  "the container logs are returned as text, newline intact")))

(deftest test-docker-session-skips-the-pull-when-asked
  (with-scripted-docker (state session)
    (ax::docker-create-container session :image-name "alpine:3" :do-not-pull-image t)
    (expect-equal (scripted-docker-pulled state) '()
                  "do-not-pull-image means no image was pulled")))

(deftest test-docker-find-or-create-reuses-a-tagged-container
  (with-scripted-docker (state session)
    (push (%scripted-container "existing-1" :status "running" :tag "ax-lisp-reuse")
          (scripted-docker-containers state))
    (let ((found (ax::docker-find-or-create-container session
                                                      :image-name "alpine:3"
                                                      :do-not-pull-image t
                                                      :tag "ax-lisp-reuse")))
      (assert-json-equal found (ax:object "Id" "existing-1" "isNew" ax:false)
                         "the tagged container was reused")
      (expect-equal (ax::docker-session-container-id session) "existing-1"
                    "the session attached to the container it found")
      (expect-equal (scripted-docker-created state) '()
                    "nothing was created when a tagged container already existed"))
    (let ((fresh (ax::docker-find-or-create-container session
                                                      :image-name "alpine:3"
                                                      :do-not-pull-image t
                                                      :tag "ax-lisp-other")))
      (assert-json-equal fresh (ax:object "Id" "c1" "isNew" ax:true)
                         "a different tag created a new container"))))

(deftest test-docker-stop-containers-is-scoped-by-tag
  (with-scripted-docker (state session)
    (push (%scripted-container "mine-1" :status "running" :tag "ax-lisp-scope")
          (scripted-docker-containers state))
    (push (%scripted-container "mine-2" :status "exited" :tag "ax-lisp-scope")
          (scripted-docker-containers state))
    (push (%scripted-container "someone-elses" :status "running" :tag "not-ours")
          (scripted-docker-containers state))
    (push (%scripted-container "untagged" :status "running") (scripted-docker-containers state))
    (let ((results (ax::docker-stop-containers session :tag "ax-lisp-scope" :remove t)))
      ;; Only the tagged containers are touched; an already-exited one is
      ;; removed without a stop, and nothing else is disturbed.
      (assert-json-equal results
                         (vector (ax:object "Id" "mine-1" "Action" "stopped")
                                 (ax:object "Id" "mine-1" "Action" "removed")
                                 (ax:object "Id" "mine-2" "Action" "removed"))
                         "the sweep reports what it stopped and removed")
      (expect-equal (sort (copy-list (scripted-docker-stopped state)) #'string<) '("mine-1")
                    "only the running tagged container was stopped")
      (expect-equal (sort (copy-list (scripted-docker-removed state)) #'string<) '("mine-1" "mine-2")
                    "only the tagged containers were removed"))))

(deftest test-docker-session-refuses-to-act-without-a-container
  (with-scripted-docker (state session)
    (expect-signals ax::docker-error "no container created or connected"
                    "starting without a container is refused"
      (ax::docker-start-container session))
    (expect-signals ax::docker-error "no container created or connected"
                    "reading logs without a container is refused"
      (ax::docker-container-logs session))
    (expect-signals ax::docker-error "no container created or connected"
                    "executing without a container is refused"
      (ax::docker-execute-command session "ls"))
    (expect-signals ax::docker-error "Failed to connect to container"
                    "connecting to a container the daemon does not know is refused"
      (ax::docker-connect-to-container session "nope"))))

(deftest test-docker-session-tool-matches-the-published-function
  (with-scripted-docker (state session)
    (ax::docker-create-container session :image-name "alpine:3" :do-not-pull-image t)
    (let ((spec (ax::docker-session-tool session :start-timeout 2 :poll-interval 0.01)))
      (expect-equal (ax:jget spec "name") "commandExecution"
                    "the tool keeps the name every other port publishes")
      (assert-json-equal (ax:jget spec "parameters")
                         (jparse "{\"type\":\"object\",\"properties\":{\"command\":{\"type\":\"string\",\"description\":\"Shell command to execute. eg. `ls -l` or `echo \\\"Hello, World!\\\"`.\"}},\"required\":[\"command\"]}")
                         "the tool's parameter schema matches the published one")
      (multiple-value-bind (result problems)
          (ax:invoke-tool spec (ax:object "command" "ls -l"))
        (expect-equal problems nil "valid arguments pass validation")
        (expect-equal result "total 0" "the tool ran the command in the container"))
      (multiple-value-bind (result problems)
          (ax:invoke-tool spec (ax:object))
        (declare (ignore result))
        (expect (and problems t) "a missing command is rejected before the daemon is called")))))

(deftest test-playbook-writes-itself-into-the-target-stage-once
  ;; The agent half of a playbook: which stage the rendered rules are written
  ;; into, and that writing it again never composes it onto itself.
  (unless (agent-buildable-p)
    (skip-test "gen.lisp has no \"code\" output field type, so no agent can be constructed"))
  (let* ((client (ax:ai :name "openai" :model "gpt-6-luna" :api-key "playbook-test"
                        :transport (lambda (url headers body)
                                     (declare (ignore url headers body))
                                     (error 'test-failure
                                            :text "the playbook must not call the model to attach"))))
         (agent (ax::agent "question:string -> answer:string"))
         (handle (ax::agent-playbook agent :options (ax:object "target" "responder")
                                           :client client))
         (stage (ax::agent-responder agent))
         (base (ax::generator-instruction stage)))
    ;; Read through the driver's options, which is where the target lives and
    ;; where playbook-target reads it from once that reader lands.
    (expect-equal (ax:jget (ax::ace-options handle) "target")
                  "task.root.responder::instruction"
                  "the handle records the component the rendered playbook attaches to")
    (expect (eq (ax::agent-playbook-handle agent) handle)
            "the agent keeps the handle it attached")
    (expect (eq (ax::agent-playbook agent) handle)
            "asking again without options returns the attached playbook")
    (expect-signals ax:ax-error "already has a playbook"
                    "asking again with options is refused rather than silently ignored"
      (ax::agent-playbook agent :options (ax:object "target" "actor")))
    ;; Writing it again must be idempotent: the stage keeps the instruction it
    ;; had when first bound, so a kept stage set coming back into use does not
    ;; accumulate copies of the playbook.
    (ax::%rebind-playbook agent)
    (ax::%rebind-playbook agent)
    (expect-equal (ax::generator-instruction stage) base
                  "rebinding twice leaves the stage instruction unchanged")
    ;; An unknown target is refused by name, not quietly treated as the actor.
    (let ((other (ax::agent "question:string -> answer:string")))
      (expect-signals ax:ax-error "target must be"
                      "an unknown playbook target is refused"
        (ax::agent-playbook other :options (ax:object "target" "distiller") :client client)))))

(deftest test-playbook-attaches-from-the-agent-configuration
  ;; A playbook named in the agent's own options attaches during construction
  ;; and loads the seed Core finds in that configuration.
  (unless (agent-buildable-p)
    (skip-test "gen.lisp has no \"code\" output field type, so no agent can be constructed"))
  (let* ((client (ax:ai :name "openai" :model "gpt-6-luna" :api-key "playbook-test"
                        :transport (lambda (url headers body)
                                     (declare (ignore url headers body))
                                     (error 'test-failure :text "no model call expected"))))
         ;; The playbook structure every port shares, taken from
         ;; ir/conformance/axagent/playbook-config-ts-bare-seed.json so the
         ;; test exercises the real shape rather than an invented one.
         (seed (jparse "{\"version\":1,\"sections\":{\"failures_to_avoid\":[{\"id\":\"failures-to-avoid-00001\",\"section\":\"failures_to_avoid\",\"content\":\"Check the live evidence before answering.\",\"helpfulCount\":0,\"harmfulCount\":0,\"createdAt\":\"2026-07-15T00:00:00.000Z\",\"updatedAt\":\"2026-07-15T00:00:00.000Z\"}]},\"stats\":{\"bulletCount\":1,\"helpfulCount\":0,\"harmfulCount\":0,\"tokenEstimate\":10},\"updatedAt\":\"2026-07-15T00:00:00.000Z\"}"))
         (agent (ax::agent "question:string -> answer:string"
                           :options (ax:object "ai" client
                                               "playbook" (ax:object "target" "responder"
                                                                     "playbook" seed))))
         (handle (ax::agent-playbook-handle agent)))
    (expect (and handle t) "a configured playbook attached during construction")
    (expect-contains (ax::ace-render handle) "Check the live evidence before answering."
                     "the seed Core found in the configuration was loaded")
    (expect-contains (ax::generator-instruction (ax::agent-responder agent))
                     "Check the live evidence before answering."
                     "the rendered playbook reached the responder's prompt")
    (expect-equal (ax:jget (ax:jget (ax::ace-playbook handle) "stats") "bulletCount") 1
                  "the loaded playbook carries the seed's one rule")))

;;; ------------------------------------------------------------------
;;; Context metrics
;;; ------------------------------------------------------------------

(deftest test-context-metrics-summarise-one-run
  (let* ((collector (ax::make-context-metrics-collector))
         (observe (ax::context-metrics-handler collector)))
    ;; Three actor turns at rising pressure, one checkpoint, one tombstone and
    ;; two compactions. The expected numbers are worked out from the events,
    ;; not read back from the collector: 900 of 1200 compacted characters are
    ;; removed, so the ratio is 0.75; the peak is the largest mutable prompt
    ;; (8000) even though the run ended smaller (3000).
    (funcall observe (ax:object "kind" "budget_check" "stage" "distiller" "turn" 1
                                "pressure" "ok" "mutablePromptChars" 2000
                                "effectiveBudgetChars" 10000 "actionLogEntryCount" 1))
    (funcall observe (ax:object "kind" "budget_check" "stage" "executor" "turn" 2
                                "pressure" "watch" "mutablePromptChars" 8000
                                "effectiveBudgetChars" 10000 "actionLogEntryCount" 4))
    (funcall observe (ax:object "kind" "action_compacted" "stage" "executor" "turn" 2
                                "mode" "compact" "reason" "pressure"
                                "originalChars" 1000 "renderedChars" 200))
    (funcall observe (ax:object "kind" "checkpoint_created" "stage" "executor" "turn" 2
                                "coveredTurns" (vector 1 2) "reason" "over_budget"))
    (funcall observe (ax:object "kind" "checkpoint_cleared" "stage" "executor" "turn" 3
                                "coveredTurns" (vector 1 2) "reason" "under_budget"))
    (funcall observe (ax:object "kind" "tombstone_created" "stage" "executor" "turn" 3
                                "resolvedByTurn" 2 "source" "model" "summaryChars" 40))
    (funcall observe (ax:object "kind" "action_compacted" "stage" "executor" "turn" 3
                                "mode" "distill" "reason" "superseded"
                                "originalChars" 200 "renderedChars" 100))
    (funcall observe (ax:object "kind" "relevance_ranking" "stage" "executor"
                                "domain" "skills" "taskChars" 12
                                "shortlist" (vector) "suppressed" ax:true))
    (funcall observe (ax:object "kind" "budget_check" "stage" "executor" "turn" 3
                                "pressure" "critical" "mutablePromptChars" 3000
                                "effectiveBudgetChars" 10000 "actionLogEntryCount" 2))
    (let ((summary (ax::context-metrics-summary
                    collector
                    (ax:object "actor" (vector (ax:object "tokens" (ax:object "promptTokens" 100
                                                                              "completionTokens" 20
                                                                              "totalTokens" 120)))
                               "responder" (vector (ax:object "tokens" (ax:object "promptTokens" 30
                                                                                  "completionTokens" 5
                                                                                  "totalTokens" 35)))))))
      (assert-json-equal summary
                         (jparse "{\"turns\":3,\"peakMutablePromptChars\":8000,\"finalMutablePromptChars\":3000,\"checkpoints\":1,\"tombstones\":1,\"compactions\":2,\"totalOriginalChars\":1200,\"totalRenderedChars\":300,\"compactionRatio\":0.75,\"pressureCounts\":{\"ok\":1,\"watch\":1,\"critical\":1},\"cumulativeTokens\":155,\"promptTokens\":130,\"completionTokens\":25,\"series\":[{\"stage\":\"distiller\",\"turn\":1,\"pressure\":\"ok\",\"mutablePromptChars\":2000,\"effectiveBudgetChars\":10000,\"actionLogEntryCount\":1},{\"stage\":\"executor\",\"turn\":2,\"pressure\":\"watch\",\"mutablePromptChars\":8000,\"effectiveBudgetChars\":10000,\"actionLogEntryCount\":4},{\"stage\":\"executor\",\"turn\":3,\"pressure\":\"critical\",\"mutablePromptChars\":3000,\"effectiveBudgetChars\":10000,\"actionLogEntryCount\":2}]}")
                         "the summary of one run"))))

(deftest test-context-metrics-handle-an-empty-run-and-a-flat-usage-array
  (let ((collector (ax::make-context-metrics-collector)))
    (assert-json-equal (ax::context-metrics-summary collector)
                       (jparse "{\"turns\":0,\"peakMutablePromptChars\":0,\"finalMutablePromptChars\":0,\"checkpoints\":0,\"tombstones\":0,\"compactions\":0,\"totalOriginalChars\":0,\"totalRenderedChars\":0,\"compactionRatio\":0,\"pressureCounts\":{\"ok\":0,\"watch\":0,\"critical\":0},\"cumulativeTokens\":0,\"promptTokens\":0,\"completionTokens\":0,\"series\":[]}")
                       "a run with no events reports zeros, and a ratio of 0 rather than nothing")
    (ax::context-metrics-observe collector (ax:object "kind" "action_compacted"
                                                      "originalChars" 0 "renderedChars" 0))
    (expect-equal (ax:jget (ax::context-metrics-summary collector) "compactionRatio") 0
                  "a compaction of nothing still has a ratio of 0, not a division by zero")
    (assert-json-subset (ax::context-metrics-summary
                         collector
                         (vector (ax:object "tokens" (ax:object "totalTokens" 7 "promptTokens" 5))
                                 (ax:object)))
                        (jparse "{\"cumulativeTokens\":7,\"promptTokens\":5,\"completionTokens\":0}")
                        "a flat usage array is accepted, and an entry without tokens counts zero")))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(deftest test-agent-control-cursors-preserve-sibling-steering
  (let* ((control (ax::make-run-control))
         (left (make-instance 'ax::agent-control-scope :control control :path "root/left"))
         (right (make-instance 'ax::agent-control-scope :control control :path "root/right")))
    (ax::run-control-steer control "Everyone")
    (ax::run-control-steer control "Only left" :target "root/left")
    (expect-equal (core::core-host-get left "pending_count") 2 "left sees root and local updates")
    (expect-equal (length (core::core-host-call left "take_pending" #())) 2 "left reads both updates")
    (expect-equal (length (core::core-host-call left "take_pending" #())) 0 "left does not replay updates")
    (let ((updates (core::core-host-call right "take_pending" #())))
      (expect-equal (length updates) 1 "right still sees root update, not left update")
      (expect-equal (ax:jget (aref updates 0) "text") "Everyone" "the original steer survives"))
    (ax::run-control-steer control "New root")
    (expect-equal (length (core::core-host-call left "take_pending" #())) 1 "left sees later updates")
    (expect-equal (length (core::core-host-call right "take_pending" #())) 1 "right sees later updates")))

(deftest test-agent-control-scope-preserves-errors
  (let* ((control (ax::make-run-control))
         (scope (make-instance 'ax::agent-control-scope :control control :path "root/responder")))
    (core::core-host-call scope "emit" (vector (ax:object "type" "failed" "path" "root/responder")))
    (expect-equal (ax:jget (aref (ax::run-control-events control) 0) "type") "failed"
                  "an ordinary generator failure stays a failure")
    (setf (ax::%scope-interrupted scope) t)
    (core::core-host-call scope "emit" (vector (ax:object "type" "failed" "path" "root/responder")))
    (expect-equal (ax:jget (aref (ax::run-control-events control) 1) "type") "aborted"
                  "a nonlocal consumer exit is an interruption, not a validation failure")))

(deftest test-per-call-runtime-keeps-responder-evidence-input
  (let* ((agent (ax::agent "question:string -> answer:string"))
         (runtime (make-scripted-runtime)))
    (ax::%use-stage-mode agent (ax:object "runtime" runtime))
    (let ((signature (ax::signature-string
                      (ax::generator-signature (ax::agent-responder agent)))))
      (expect-contains signature "contextData" "runtime-mode responder must receive actor evidence"))
    (expect-equal
     (ax::signature-string (ax::generator-signature (ax::agent-responder agent)))
     (ax::signature-string
      (ax::parse-signature (ax:jget (ax::agent-core-state agent) "responder_signature")))
     "the mode switch uses Core's responder signature, not the agent's public signature")))

(defun run-agent-runtime-tests ()
  "Run every agent runtime test. Returns (values passed failed skipped).

A test blocked by a named gap outside this package is reported as skipped
and counted apart from both passes and failures, so the suite neither
flatters itself nor goes red for someone else's missing surface."
  (let ((passed 0) (failed 0) (skipped 0))
    (dolist (entry *runtime-tests*)
      (let ((skip nil))
        (block one-test
          (handler-bind ((test-skipped
                           (lambda (condition)
                             (setf skip (test-skipped-reason condition))
                             (return-from one-test))))
            (handler-case
                (progn (funcall (cdr entry))
                       (incf passed)
                       (format t "~&ok   ~a~%" (car entry)))
              (error (condition)
                (incf failed)
                (format t "~&FAIL ~a~%     ~a~%" (car entry) condition)))))
        (when skip
          (incf skipped)
          (format t "~&skip ~a~%     ~a~%" (car entry) skip))))
    (format t "~&agent runtime: ~a passed, ~a failed, ~a skipped~%" passed failed skipped)
    (values passed failed skipped)))
