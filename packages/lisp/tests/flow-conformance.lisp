;;;; flow-conformance.lisp --- the shared AxFlow conformance suite, natively.
;;;;
;;;; Every fixture under ir/conformance/axflow runs here, against the same
;;;; recorded expectations the TypeScript, Python, Java and C++ ports run.
;;;; Nothing is compared against this implementation's own output.
;;;;
;;;; Three fixture kinds appear in that directory, and all three run:
;;;;
;;;;   flow                 build the flow from its steps, optionally check
;;;;                        the plan, then run it and check output, request
;;;;                        count, request text, chat log, trace kinds and
;;;;                        subsets, usage, components and run-control events
;;;;   flow_mermaid         compile a Mermaid document and re-render it, or
;;;;                        render a builder-made flow, or require the
;;;;                        recorded compilation error
;;;;   flow_cache_sequence  several forward calls on one flow sharing one
;;;;                        cache: each call's output, deltas, request count,
;;;;                        and every cache read and write
;;;;
;;;; A fixture kind this file does not handle fails naming the kind.  A
;;;; fixture is never counted as passing because nothing ran: each assertion
;;;; comes from a key the fixture actually records, and a fixture that
;;;; records none of them fails as unasserted.

(defpackage #:axllm/flow-tests
  (:use #:cl)
  (:export #:run-flow-conformance-tests #:run-flow-harness-tests))

(in-package #:axllm/flow-tests)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(define-condition flow-fixture-failure (error)
  ((detail :initarg :detail :reader flow-fixture-failure-detail))
  (:report (lambda (condition stream)
             (write-string (flow-fixture-failure-detail condition) stream))))

(defun fail (format-control &rest arguments)
  (error 'flow-fixture-failure :detail (apply #'format nil format-control arguments)))

(defun conformance-directory ()
  (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
    (if (and override (plusp (length override)))
        (uiop:ensure-directory-pathname override)
        (asdf:system-relative-pathname "axllm" "../../ir/conformance/"))))

(defun fixture-files ()
  (sort (directory (merge-pathnames "axflow/*.json" (conformance-directory)))
        #'string< :key #'namestring))

(defun read-fixture (path)
  (ax:parse-json (uiop:read-file-string path)))

(defun show (value)
  (if (stringp value) (format nil "~s" value) (ax:encode-json value)))

(defun same-value-p (left right)
  (axllm/core::core-value-equal left right))

(defun truthy (value)
  (axllm/core::core-true-p value))

(defun present-p (fixture key)
  (nth-value 1 (gethash key fixture)))

(defun jref (object key &optional (default :null))
  (ax:jget object key default))

(defun as-vector (value)
  (cond ((and (vectorp value) (not (stringp value))) value)
        ((eq value :null) (vector))
        ((null value) (vector))
        (t (fail "expected a JSON array, got ~a" (show value)))))

(defun as-object (value)
  (cond ((hash-table-p value) value)
        ((or (eq value :null) (null value)) (ax:object))
        (t (fail "expected a JSON object, got ~a" (show value)))))

(defun assert-equal (actual expected label)
  (unless (same-value-p actual expected)
    (fail "~a mismatch~%    expected: ~a~%    actual:   ~a" label (show expected) (show actual))))

(defun subset-problem (actual expected label)
  "NIL when ACTUAL satisfies the EXPECTED subset, else a description."
  (cond ((hash-table-p expected)
         (if (not (hash-table-p actual))
             (format nil "~a expected an object subset, got ~a" label (show actual))
             (loop for key in (axllm::%object-keys expected)
                   do (multiple-value-bind (value found) (gethash key actual)
                        (if (not found)
                            (return (format nil "~a missing key ~s" label key))
                            (let ((problem (subset-problem value (gethash key expected)
                                                           (format nil "~a.~a" label key))))
                              (when problem (return problem))))))))
        ((and (vectorp expected) (not (stringp expected)))
         (unless (same-value-p actual expected)
           (format nil "~a mismatch~%    expected: ~a~%    actual:   ~a"
                   label (show expected) (show actual))))
        (t (unless (same-value-p actual expected)
             (format nil "~a expected ~a, got ~a" label (show expected) (show actual))))))

(defun assert-subset (actual expected label)
  (let ((problem (subset-problem actual expected label)))
    (when problem (fail "~a" problem))))

(defun assert-list-subset (actual expected label)
  "Every expected item matches a later actual item, in order."
  (let ((items (as-vector actual))
        (start 0))
    (loop for wanted across (as-vector expected)
          do (let ((matched nil))
               (loop for index from start below (length items)
                     do (unless (subset-problem (aref items index) wanted
                                                (format nil "~a[~d]" label index))
                          (setf start (1+ index) matched t)
                          (return)))
               (unless matched
                 (fail "~a is missing the expected item ~a~%    actual: ~a"
                       label (show wanted) (show items)))))))

;;; ------------------------------------------------------------------
;;; A scripted provider
;;; ------------------------------------------------------------------

(defstruct (scripted (:conc-name scripted-))
  queue
  speak-queue
  (requests '())
  (speak-requests '())
  (speak-results '())
  (speak-wire-requests '()))

(defclass scripted-provider-client (axllm::provider-client)
  ((script :initarg :script :reader client-script)))

(defmethod ax:ax-speak :around ((client scripted-provider-client) request &optional options)
  (declare (ignore options))
  ;; Observe the real provider boundary; normalization still runs in production.
  (let ((script (client-script client)))
    (push (axllm::%flow-clone request) (scripted-speak-requests script))
    (let ((result (call-next-method)))
      (push (axllm::%flow-clone result) (scripted-speak-results script))
      result)))

(defun usage-numbers (usage)
  "A fixture usage record as prompt, completion and total counts."
  (let* ((usage (as-object usage))
         (prompt (jref usage "prompt_tokens" 0))
         (completion (jref usage "completion_tokens" 0))
         (total (jref usage "total_tokens" :null)))
    (list (if (realp prompt) prompt 0)
          (if (realp completion) completion 0)
          (if (realp total) total (+ (if (realp prompt) prompt 0)
                                     (if (realp completion) completion 0))))))

(defun response-results (response)
  "A fixture response's results, in either shape the fixtures use.

Most fixtures record one response as \"content\" plus optional \"usage\"; some
record a \"results\" array of samples, each with its own content and finish
reason."
  (let ((results (jref response "results")))
    (if (and (vectorp results) (not (stringp results)))
        results
        (vector response))))

(defun scripted-body (response)
  "One fixture response as an OpenAI chat-completion body, or a scripted failure.

A fixture that records an \"error\" scripts the service itself failing, as the
other ports' scripted clients do: the recorded message is the error, not a
provider response body. It is returned as a closure so the client raises it
rather than this runner reading a redacted condition back out of a real HTTP
reply."
  (let ((failure (jref response "error")))
    (when (hash-table-p failure)
      (return-from scripted-body
        (lambda ()
          (error 'ax:provider-error
                 :kind (intern (string-upcase (jref failure "type" "response")) :keyword)
                 :message (jref failure "message" "")
                 :provider "openai")))))
  (destructuring-bind (prompt completion total) (usage-numbers (jref response "usage"))
    (let ((choices (make-array 0 :adjustable t :fill-pointer 0)))
      (loop for result across (response-results response)
            for index from 0
            do (vector-push-extend
                (ax:object "index" (let ((recorded (jref result "index" index)))
                                     (if (realp recorded) recorded index))
                           "finish_reason" (let ((reason (jref result "finish_reason" "stop")))
                                             (if (stringp reason) reason "stop"))
                           "message" (ax:object "role" "assistant"
                                                "content" (jref result "content" "")))
                choices))
      (ax:encode-json
       (ax:object "choices" choices
                  "usage" (ax:object "prompt_tokens" prompt
                                     "completion_tokens" completion
                                     "total_tokens" total))))))

(defun scripted-client (responses &optional speak-responses)
  "Replay separate chat and speech queues. Returns (values client script)."
  (let ((script (make-scripted :queue (map 'list #'scripted-body (as-vector responses))
                               :speak-queue (coerce (as-vector speak-responses) 'list))))
    (values (change-class
             (ax:ai :name "openai"
                   :model "gpt-5.4-mini"
                   :api-key "sk-test-dummy-key"
                   :transport (lambda (url headers body)
                                (declare (ignore headers))
                                (cond
                                  ((uiop:string-suffix-p url "/audio/speech")
                                   (push (ax:parse-json body) (scripted-speak-wire-requests script))
                                   (let ((next (or (pop (scripted-speak-queue script))
                                                   (fail "the fixture sent more speech requests than it scripted"))))
                                     (when (hash-table-p (jref next "error"))
                                       (error "~a" (jref (jref next "error") "message")))
                                     ;; The native binary transport returns base64 plus Content-Type.
                                     (values (jref next "data") 200
                                             (list (cons "content-type" (jref next "mimeType"))))))
                                  ((uiop:string-suffix-p url "/chat/completions")
                                   (push body (scripted-requests script))
                                   (let ((next (if (scripted-queue script)
                                                   (pop (scripted-queue script))
                                                   (fail "the fixture sent more provider requests than it scripted"))))
                                     (if (functionp next)
                                         (funcall next)
                                         (values next 200))))
                                  (t (fail "unexpected scripted provider URL: ~a" url)))))
             'scripted-provider-client :script script)
            script)))

(defun request-count (script)
  (length (scripted-requests script)))

(defun request-text (script)
  (format nil "~{~a~^ ~}" (reverse (scripted-requests script))))

;;; ------------------------------------------------------------------
;;; Fixture mappers and predicates
;;; ------------------------------------------------------------------

(defun state-path (state field &optional (default :null))
  "STATE's value at the dotted path FIELD."
  (if (or (eq field :null) (not (stringp field)) (zerop (length field)))
      default
      (let ((current state))
        (dolist (part (uiop:split-string field :separator "."))
          (setf current (if (hash-table-p current) (ax:jget current part default) default)))
        current)))

(defun number-or (value fallback)
  (if (realp value) value fallback))

(defun spec-mapper (spec)
  "A fixture mapper spec as a flow callable, matching the reference ports."
  (let ((spec (as-object spec)))
    (ax:flow-callable
     (lambda (state)
       (let ((out (ax:object))
             (op (jref spec "op" "set")))
         (dolist (key (axllm::%object-keys state))
           (setf (gethash key out) (gethash key state))
           (axllm::%record-key out key))
         (cond
           ((string= op "set")
            (let ((values (as-object (jref spec "values"))))
              (dolist (key (axllm::%object-keys values))
                (axllm::%set-key out key (gethash key values)))))
           ((string= op "increment")
            (let ((field (jref spec "field")))
              (axllm::%set-key out field (+ (number-or (state-path out field 0) 0)
                                            (number-or (jref spec "by" 1) 1)))))
           ((string= op "append")
            (let* ((field (jref spec "field"))
                   (value-field (jref spec "valueField"))
                   (value (if (stringp value-field)
                              (state-path out value-field)
                              (jref spec "value"))))
              (let ((current (as-vector (state-path out field (vector))))
                    (next (make-array 0 :adjustable t :fill-pointer 0)))
                (loop for item across current do (vector-push-extend item next))
                (vector-push-extend value next)
                (axllm::%set-key out field next))))
           ((string= op "copy")
            (axllm::%set-key out (jref spec "to") (state-path out (jref spec "from"))))
           ((string= op "upper")
            (let ((from (jref spec "from" "__item"))
                  (to (jref spec "to" "__derived")))
              (axllm::%set-key out to (string-upcase (axllm/core::core-js-text
                                                     (state-path out from ""))))))
           (t (fail "fixture mapper op ~s is not implemented" op)))
         out)))))

(defun spec-predicate (spec)
  "A fixture predicate spec as a flow callable, matching the reference ports."
  (if (hash-table-p spec)
      (ax:flow-callable
       (lambda (state)
         (let ((op (jref spec "op" "truthy"))
               (field (jref spec "field")))
           (cond ((string= op "truthy") (axllm/core::core-truthy (state-path state field)))
                 ((string= op "field") (state-path state field))
                 ((string= op "lt")
                  (axllm/core::core-bool (< (number-or (state-path state field 0) 0)
                                            (number-or (jref spec "value" 0) 0))))
                 ((string= op "eq")
                  (axllm/core::core-eq (state-path state field) (jref spec "value")))
                 ((string= op "always") (axllm/core::core-truthy (jref spec "value" ax:true)))
                 (t (fail "fixture predicate op ~s is not implemented" op))))))
      (ax:flow-callable (lambda (state) (declare (ignore state)) (axllm/core::core-truthy spec)))))

;;; ------------------------------------------------------------------
;;; Building a flow from a fixture
;;; ------------------------------------------------------------------

(defun fixture-signature (step fixture)
  (let ((extended (jref step "extended_signature")))
    (cond ((stringp extended) extended)
          ((stringp (jref step "signature")) (jref step "signature"))
          ((stringp (jref fixture "signature")) (jref fixture "signature"))
          (t "question:string -> answer:string"))))

(defun constant-output-callable (step)
  (let ((output (jref step "output")))
    (ax:flow-callable (lambda (state) (declare (ignore state)) (as-object output)))))

(defun build-step (step fixture)
  (let* ((step (as-object step))
         (kind (jref step "kind" "execute"))
         (name (jref step "name"))
         (options (as-object (jref step "options"))))
    (unless (stringp name)
      (fail "fixture step has no name: ~a" (show step)))
    (cond
      ((or (string= kind "map") (string= kind "derive"))
       (ax:flow-step kind name
                     (if (present-p step "mapper")
                         (spec-mapper (jref step "mapper"))
                         (constant-output-callable step))
                     options))
      ((string= kind "branch")
       (let ((opts (ax:object)))
         (dolist (key (axllm::%object-keys options))
           (axllm::%set-key opts key (gethash key options)))
         (axllm::%set-key opts "predicate"
                          (spec-predicate (if (present-p step "predicate")
                                              (jref step "predicate")
                                              (jref options "predicate"))))
         (let ((branches (make-array 0 :adjustable t :fill-pointer 0)))
           (loop for branch across (as-vector (if (present-p step "branches")
                                                  (jref step "branches")
                                                  (jref options "branches")))
                 do (let ((entry (ax:object))
                          (steps (make-array 0 :adjustable t :fill-pointer 0)))
                      (loop for child across (as-vector (jref branch "steps"))
                            do (vector-push-extend (build-step child fixture) steps))
                      (axllm::%set-key entry "when" (jref branch "when"))
                      (axllm::%set-key entry "steps" steps)
                      (vector-push-extend entry branches)))
           (axllm::%set-key opts "branches" branches))
         (ax:flow-step "branch" name nil opts)))
      ((or (string= kind "while") (string= kind "feedback"))
       (let ((opts (ax:object)))
         (dolist (key (axllm::%object-keys options))
           (axllm::%set-key opts key (gethash key options)))
         (axllm::%set-key opts "condition"
                          (spec-predicate (if (present-p step "condition")
                                              (jref step "condition")
                                              (jref options "condition"))))
         (let ((steps (make-array 0 :adjustable t :fill-pointer 0)))
           (loop for child across (as-vector (if (present-p step "steps")
                                                 (jref step "steps")
                                                 (jref options "steps")))
                 do (vector-push-extend (build-step child fixture) steps))
           (axllm::%set-key opts "steps" steps))
         (ax:flow-step kind name nil opts)))
      ((or (string= kind "parallel") (string= kind "parallelMerge"))
       (ax:flow-step kind name nil options))
      (t
       (let ((program
               (cond ((equal (jref step "program") "flow")
                      (build-flow (ax:object "flow_options"
                                             (if (present-p step "flow_options")
                                                 (jref step "flow_options")
                                                 (ax:object "id" (jref step "program_id"
                                                                       (format nil "root.~a" name))))
                                             "steps" (as-vector (jref step "steps"))
                                             "returns" (as-object (jref step "returns"))
                                             "signature" (fixture-signature step fixture)
                                             "_node_control" (jref fixture "_node_control"))))
                     (t (ax:ax (fixture-signature step fixture)))))
             (step-options (ax:object)))
         (let ((forward-options (as-object (jref step "forward_options"))))
           (dolist (key (axllm::%object-keys forward-options))
             (axllm::%set-key step-options key (gethash key forward-options))))
         (dolist (key (axllm::%object-keys options))
           (axllm::%set-key step-options key (gethash key options)))
         (when (truthy (jref step "constructor_control" ax:false))
           (axllm::%set-key step-options "control" (jref fixture "_node_control")))
         (ax:flow-step kind name program step-options))))))

(defun build-flow (fixture)
  (let ((object (ax:flow (if (present-p fixture "flow_options")
                             (as-object (jref fixture "flow_options"))
                             (ax:object "id" (jref fixture "program_id" "root.flow"))))))
    (loop for step across (as-vector (jref fixture "steps"))
          do (axllm/core::flow-add-step (ax:flow-state object) (build-step step fixture)))
    (when (present-p fixture "returns")
      (ax:flow-returns object (as-object (jref fixture "returns"))))
    (when (present-p fixture "demos")
      (ax:flow-set-demos object (jref fixture "demos")))
    object))

;;; ------------------------------------------------------------------
;;; Run controls
;;; ------------------------------------------------------------------

(defclass recording-control ()
  ((events :initform (make-array 0 :adjustable t :fill-pointer 0) :reader control-events)
   (aborted :initform nil :accessor control-aborted-p))
  (:documentation
   "A run control that records the lifecycle events a flow and its nodes emit.

Fixtures pin the events and their paths, so the recorder keeps them in the
order they arrive."))

(defparameter +lifecycle-event-types+ '("started" "completed" "failed" "aborted")
  "The event types the fixtures pin; a steer or tool event is not one of them.")

(defmethod axllm/core::core-host-call ((target recording-control) method args)
  (cond ((or (equal method "_emit") (equal method "emit"))
         ;; Record path and type only, as every port's fixture recorder does,
         ;; so an event's extra detail is not pinned by the shared fixtures.
         (let ((event (aref args 0)))
           (when (member (jref event "type") +lifecycle-event-types+ :test #'equal)
             (vector-push-extend (ax:object "path" (jref event "path")
                                            "type" (jref event "type"))
                                 (control-events target))))
         :null)
        ((equal method "abort") (setf (control-aborted-p target) t) :null)
        ((equal method "aborted") (axllm/core::core-bool (control-aborted-p target)))
        (t (fail "run control has no method ~a" method))))

(defmethod axllm/core::core-host-get ((target recording-control) key &optional (fallback :null))
  (cond ((equal key "aborted") (axllm/core::core-bool (control-aborted-p target)))
        ((equal key "events") (control-events target))
        (t fallback)))

(defun control-event-list (control)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for event across (control-events control) do (vector-push-extend event out))
    out))

;;; ------------------------------------------------------------------
;;; kind: flow
;;; ------------------------------------------------------------------

(defun assert-control-events (fixture flow-control node-control asserted)
  (when (present-p fixture "expected_control_events")
    (assert-equal (control-event-list flow-control)
                  (jref fixture "expected_control_events") "flow run control events")
    (setf (car asserted) t))
  (when (present-p fixture "expected_node_control_events")
    (assert-equal (control-event-list node-control)
                  (jref fixture "expected_node_control_events") "node run control events")
    (setf (car asserted) t)))

(defun run-flow-fixture (fixture)
  (let* ((asserted (list nil))
         (node-control (make-instance 'recording-control))
         (flow-control (make-instance 'recording-control))
         (fixture (let ((copy (ax:object)))
                    (dolist (key (axllm::%object-keys fixture))
                      (axllm::%set-key copy key (gethash key fixture)))
                    (axllm::%set-key copy "_node_control" node-control)
                    copy))
         (failure nil)
         (flow (handler-case (build-flow fixture)
                 (error (condition)
                   ;; A fixture can pin a construction failure, such as a demo
                   ;; naming a node the flow does not have.
                   (setf failure condition)
                   nil)))
         script
         output)
    (when failure
      (let ((wanted (jref fixture "expected_error_contains")))
        (unless (and (stringp wanted) (search wanted (princ-to-string failure)))
          (error failure))
        (return-from run-flow-fixture)))
    (when (present-p fixture "expected_plan")
      (assert-equal (ax:flow-plan flow) (jref fixture "expected_plan") "flow plan")
      (setf (car asserted) t))
    (when (present-p fixture "expected_plan_subset")
      (assert-subset (ax:flow-plan flow) (jref fixture "expected_plan_subset") "flow plan")
      (setf (car asserted) t))
    (when (equal (jref fixture "operation") "plan")
      (unless (car asserted)
        (fail "a plan fixture must record expected_plan or expected_plan_subset"))
      (return-from run-flow-fixture))
    (multiple-value-bind (client built-script)
        (scripted-client (jref fixture "responses") (jref fixture "speak_responses"))
      (setf script built-script)
      (let ((options (ax:object)))
        (let ((forward-options (as-object (jref fixture "forward_options"))))
          (dolist (key (axllm::%object-keys forward-options))
            (axllm::%set-key options key (gethash key forward-options))))
        (when (truthy (jref fixture "control" ax:false))
          (axllm::%set-key options "control" flow-control))
        (handler-case
            (setf output (if (equal (jref fixture "operation") "streaming")
                             (ax:flow-streaming-forward flow client (as-object (jref fixture "input")) options)
                             (ax:forward flow client (as-object (jref fixture "input")) options)))
          (error (condition) (setf failure condition)))))
    (cond
      ((present-p fixture "expected_error_contains")
       (let ((wanted (jref fixture "expected_error_contains")))
         (unless failure
           (fail "expected the flow to fail with ~s, it returned ~a" wanted (show output)))
         (let ((text (princ-to-string failure)))
           (unless (search wanted text)
             (fail "expected a flow error containing ~s, got: ~a" wanted text)))
         (setf (car asserted) t))
       (assert-control-events fixture flow-control node-control asserted))
      (failure (error failure))
      (t
       (when (present-p fixture "expected_output")
         (assert-equal output (jref fixture "expected_output") "flow output")
         (setf (car asserted) t))
       (when (present-p fixture "expected_streaming_output")
         (assert-equal output (jref fixture "expected_streaming_output") "flow streaming output")
         (setf (car asserted) t))
       (assert-control-events fixture flow-control node-control asserted)
       (when (present-p fixture "expected_request_count")
         (let ((wanted (jref fixture "expected_request_count")))
           (unless (= (request-count script) wanted)
             (fail "expected ~a provider request(s), got ~a" wanted (request-count script))))
         (setf (car asserted) t))
       (when (present-p fixture "expected_speak_requests")
         (assert-equal (coerce (reverse (scripted-speak-requests script)) 'vector)
                       (jref fixture "expected_speak_requests") "flow speech requests")
         (assert-equal (length (scripted-speak-wire-requests script))
                       (length (as-vector (jref fixture "expected_speak_requests")))
                       "flow speech transport calls")
         (setf (car asserted) t))
       (when (present-p fixture "speak_responses")
         (assert-equal (coerce (reverse (scripted-speak-results script)) 'vector)
                       (jref fixture "speak_responses") "flow normalized speech results")
         (setf (car asserted) t))
       (when (present-p fixture "expected_request_contains")
         (let ((text (request-text script)))
           (loop for item across (as-vector (jref fixture "expected_request_contains"))
                 do (let ((needle (axllm/core::core-js-text item)))
                      (unless (search needle text)
                        (fail "flow request is missing ~s: ~a" needle text)))))
         (setf (car asserted) t))
       (when (present-p fixture "expected_chat_log_subset")
         (assert-list-subset (ax:flow-chat-log flow) (jref fixture "expected_chat_log_subset")
                             "flow chat log")
         (setf (car asserted) t))
       (when (present-p fixture "expected_trace_kinds")
         (assert-equal (map 'vector (lambda (event) (jref event "kind")) (ax:flow-traces flow))
                       (jref fixture "expected_trace_kinds") "flow trace kinds")
         (setf (car asserted) t))
       (when (present-p fixture "expected_trace_subset")
         (assert-list-subset (ax:flow-traces flow) (jref fixture "expected_trace_subset")
                             "flow traces")
         (setf (car asserted) t))
       (when (present-p fixture "expected_usage_subset")
         (assert-subset (ax:flow-usage flow) (jref fixture "expected_usage_subset") "flow usage")
         (setf (car asserted) t))
       (when (present-p fixture "expected_components_subset")
         (assert-list-subset (ax:flow-components flow) (jref fixture "expected_components_subset")
                             "flow components")
         (setf (car asserted) t))))
    (unless (car asserted)
      (fail "fixture records no expectation this runner checks; it would pass without running"))))

;;; ------------------------------------------------------------------
;;; kind: flow_mermaid
;;; ------------------------------------------------------------------

(defun mermaid-bindings (fixture)
  (let ((conditions (ax:object)))
    (loop for name across (as-vector (jref fixture "condition_names"))
          do (axllm::%set-key conditions name
                              (ax:flow-callable (lambda (state) (declare (ignore state)) ax:false)
                                                :name name)))
    (ax:object "conditions" conditions)))

(defun flow-direction (flow)
  "FLOW's Mermaid direction, from the AST Core parsed."
  (let ((ast (ax:jget (ax:flow-state flow) "mermaidAst")))
    (if (hash-table-p ast) (jref ast "direction") :null)))

(defun run-flow-mermaid-roundtrip (fixture bindings)
  "Check a Mermaid document's render, its re-render, and its direction.

The three expectations are independent. expected_rendered pins what the
document renders to, expected_rerendered pins what that rendering renders to
when parsed again -- the two differ for at least one fixture, so collapsing
them hides whichever one is not checked -- and expected_direction pins the
direction Core parsed out of the source document.

expected_direction is deliberately not re-checked after the round trip. Every
fixture that records a direction other than TD (graph RL, flowchart LR,
flowchart BT) records an expected_rendered of \"flowchart TD\": the canonical
rendering normalises direction on purpose. So the source direction is a parse
expectation, and the normalisation is already pinned by the rendering itself.
Asserting the direction survived the round trip would contradict the
fixtures."
  (unless (present-p fixture "expected_rendered")
    (fail "a mermaid roundtrip fixture must record expected_rendered"))
  (let* ((first (ax:flow (jref fixture "document") :bindings bindings))
         (rendered (ax:flow-mermaid first)))
    (assert-equal rendered (jref fixture "expected_rendered") "flow mermaid render")
    (assert-equal (ax:flow-mermaid (ax:flow rendered :bindings bindings))
                  (if (present-p fixture "expected_rerendered")
                      (jref fixture "expected_rerendered")
                      (jref fixture "expected_rendered"))
                  "flow mermaid canonical roundtrip")
    (when (present-p fixture "expected_direction")
      (assert-equal (flow-direction first) (jref fixture "expected_direction")
                    "flow mermaid direction"))))

(defun run-flow-mermaid-fixture (fixture)
  (let ((operation (jref fixture "operation"))
        (bindings (mermaid-bindings fixture)))
    ;; Explicit dispatch: an operation this runner does not implement fails
    ;; naming it, rather than falling through to the roundtrip path and
    ;; appearing to have been checked.
    (unless (or (eq operation :null)
                (member operation '("error" "builder_render" "roundtrip") :test #'equal))
      (fail "mermaid fixture operation ~s is not handled by the Lisp AxFlow suite; implement it or remove the claim"
            operation))
    (cond
      ((equal operation "error")
       (let ((wanted (jref fixture "expected_error_contains" "")))
         (handler-case
             (let ((built (ax:flow (jref fixture "document" "") :bindings bindings)))
               (fail "expected the Mermaid document to be rejected with ~s, it compiled to ~a"
                     wanted (show (ax:flow-plan built))))
           (flow-fixture-failure (condition) (error condition))
           (error (condition)
             (let ((text (princ-to-string condition)))
               (unless (search wanted text)
                 (fail "expected a Mermaid error containing ~s, got: ~a" wanted text)))))))
      ((equal operation "builder_render")
       (let ((flow (ax:flow)))
         (loop for step across (as-vector (jref fixture "builder_steps"))
               do (ax:flow-execute flow (jref step "name") (ax:ax (jref step "signature"))
                                   (if (present-p step "reads")
                                       (ax:object "reads" (jref step "reads"))
                                       (ax:object))))
         (assert-equal (ax:flow-mermaid flow) (jref fixture "expected_rendered")
                       "flow mermaid builder render")))
      (t (run-flow-mermaid-roundtrip fixture bindings)))))

;;; ------------------------------------------------------------------
;;; kind: flow_cache_sequence
;;; ------------------------------------------------------------------

(defun run-flow-cache-sequence-fixture (fixture)
  (let* ((store (make-hash-table :test #'equal))
         (reads '())
         (writes (make-array 0 :adjustable t :fill-pointer 0))
         (cache-in (jref fixture "cache_in" "call"))
         (flow (build-flow fixture))
         (outputs (make-array 0 :adjustable t :fill-pointer 0))
         (deltas (make-array 0 :adjustable t :fill-pointer 0))
         (requests (make-array 0 :adjustable t :fill-pointer 0))
         (errors (make-array 0 :adjustable t :fill-pointer 0))
         (any-error nil)
         (caching-function
           (lambda (key &optional (value nil value-supplied))
             (cond (value-supplied
                    (let ((write-error (jref fixture "cache_write_error")))
                      (when (stringp write-error) (error write-error)))
                    (vector-push-extend (axllm::%flow-clone value) writes)
                    (setf (gethash key store) (axllm::%flow-clone value))
                    :null)
                   (t
                    (push key reads)
                    (let ((read-error (jref fixture "cache_read_error")))
                      (when (stringp read-error) (error read-error)))
                    (multiple-value-bind (hit found) (gethash key store)
                      (if found (axllm::%flow-clone hit) :null)))))))
    (multiple-value-bind (client script) (scripted-client (jref fixture "responses"))
      (let ((previous (ax:get-global "cachingFunction")))
        (when (equal cache-in "global")
          (ax:set-global "cachingFunction" caching-function))
        (unwind-protect
             (loop for call across (as-vector (jref fixture "calls"))
                   do (let ((before (request-count script))
                            (options (ax:object))
                            (input (as-object (jref call "input"))))
                        (when (equal cache-in "call")
                          (axllm::%set-key options "cachingFunction" caching-function))
                        (when (truthy (jref call "control" ax:false))
                          (axllm::%set-key options "control" (make-instance 'recording-control)))
                        (when (truthy (jref call "reverse_input_keys" ax:false))
                          (let ((reversed (ax:object)))
                            (dolist (key (reverse (axllm::%object-keys input)))
                              (axllm::%set-key reversed key (gethash key input)))
                            (setf input reversed)))
                        (vector-push-extend :null errors)
                        (handler-case
                            (if (equal (jref call "kind") "streaming_forward")
                                (let ((stream (ax:flow-streaming-forward flow client input options)))
                                  (vector-push-extend (jref (aref stream (1- (length stream))) "delta")
                                                      outputs)
                                  (vector-push-extend stream deltas))
                                (progn
                                  (vector-push-extend (ax:forward flow client input options) outputs)
                                  (vector-push-extend :null deltas)))
                          (error (condition)
                            (setf any-error t)
                            (setf (aref errors (1- (length errors)))
                                  (first (uiop:split-string (princ-to-string condition)
                                                            :separator (string #\Newline))))
                            (vector-push-extend :null outputs)
                            (vector-push-extend :null deltas)))
                        (vector-push-extend (- (request-count script) before) requests)))
          (ax:set-global "cachingFunction" previous)))
      (when (or any-error (present-p fixture "expected_errors"))
        (assert-equal errors (jref fixture "expected_errors") "flow cache sequence errors"))
      (assert-equal outputs (jref fixture "expected_outputs") "flow cache sequence outputs")
      (assert-equal deltas (jref fixture "expected_deltas") "flow cache sequence deltas")
      (assert-equal requests (jref fixture "expected_requests") "flow cache sequence requests per call")
      (let ((wanted (jref fixture "expected_request_count")))
        (unless (and (realp wanted) (= (request-count script) wanted))
          (fail "expected ~a provider request(s), got ~a" wanted (request-count script))))
      (let ((wanted (jref fixture "expected_cache_gets")))
        (unless (and (realp wanted) (= (length reads) wanted))
          (fail "expected ~a cache read(s), got ~a" wanted (length reads))))
      (assert-equal writes (jref fixture "expected_cache_sets") "flow cache writes"))))

;;; ------------------------------------------------------------------
;;; Dispatch
;;; ------------------------------------------------------------------

(defparameter +fixture-runners+
  '(("flow" . run-flow-fixture)
    ("flow_mermaid" . run-flow-mermaid-fixture)
    ("flow_cache_sequence" . run-flow-cache-sequence-fixture))
  "The AxFlow fixture kinds this suite claims, and how each one runs.")

(defun fixture-category (fixture)
  "The conformance-receipt category this fixture's expectation falls under.
Read off the fixture itself, so it cannot drift with the runner's control flow."
  (if (or (present-p fixture "expected_error_contains")
          (equal (jref fixture "operation") "error")
          (some (lambda (entry) (not (eq entry :null)))
                (as-vector (jref fixture "expected_errors"))))
      :validation-error
      :semantic))

(defun run-fixture (fixture)
  (let* ((kind (jref fixture "kind"))
         (runner (cdr (assoc kind +fixture-runners+ :test #'equal))))
    (unless runner
      (fail "fixture kind ~s is not handled by the Lisp AxFlow suite; implement it or remove the claim"
            kind))
    (funcall runner fixture)))

;;; ------------------------------------------------------------------
;;; Negative harness tests
;;; ------------------------------------------------------------------
;;;
;;; A conformance runner can pass for the wrong reason, and a green suite is
;;; exactly the condition that hides it. These tests perturb a real fixture in
;;; one field at a time and require the runner to fail: an assertion that no
;;; perturbation can break is not checking anything.

(defun fixture-with (fixture &rest key-values)
  "FIXTURE with KEY-VALUES replaced, as a fresh object."
  (let ((out (ax:object)))
    (dolist (key (axllm::%object-keys fixture))
      (axllm::%set-key out key (gethash key fixture)))
    (loop for (key value) on key-values by #'cddr
          do (axllm::%set-key out key value))
    out))

(defun fixture-without (fixture key)
  (let ((out (ax:object)))
    (dolist (existing (axllm::%object-keys fixture))
      (unless (equal existing key)
        (axllm::%set-key out existing (gethash existing fixture))))
    out))

(defun expect-fixture-rejected (fixture label)
  "Require RUN-FIXTURE to fail on FIXTURE. Returns NIL on success, else a report."
  (handler-case
      (progn (run-fixture fixture)
             (format nil "~a: the runner accepted a fixture it should have rejected" label))
    (flow-fixture-failure () nil)
    (error (condition)
      ;; Any failure is acceptable here, but a Lisp-level error usually means
      ;; the harness tripped over itself rather than detecting the fault.
      (declare (ignore condition))
      nil)))

(defun harness-fixture (name)
  (read-fixture (merge-pathnames (format nil "axflow/~a.json" name)
                                 (conformance-directory))))

(defun run-flow-harness-tests ()
  "Prove the mermaid runner rejects each single-field perturbation."
  (let* ((base (harness-fixture "mermaid-supported-shapes"))
         (checks
           (list
             ;; The real fixture must still pass, or the perturbations below
             ;; prove nothing.
             (handler-case (progn (run-fixture base) nil)
               (error (condition)
                 (format nil "unperturbed mermaid-supported-shapes failed: ~a" condition)))
             ;; 1. expected_rendered must be asserted in its own right. Before
             ;; this was fixed, expected_rerendered replaced it for both checks,
             ;; so an impossible expected_rendered passed.
             (expect-fixture-rejected
              (fixture-with base "expected_rendered" "impossible expected output")
              "expected_rendered")
             ;; 2. expected_rerendered must be asserted separately too.
             (expect-fixture-rejected
              (fixture-with base "expected_rerendered" "impossible expected output")
              "expected_rerendered")
             ;; 3. expected_direction was ignored entirely.
             (expect-fixture-rejected
              (fixture-with base "expected_direction" "INVALID")
              "expected_direction")
             ;; 4. An unknown operation fell through to the roundtrip path.
             (expect-fixture-rejected
              (fixture-with base "operation" "unknown-op")
              "unknown operation")
             ;; 5. All three perturbations at once, which is the exact case
             ;; the review reported passing.
             (expect-fixture-rejected
              (fixture-with base
                            "expected_direction" "INVALID"
                            "expected_rendered" "impossible expected output"
                            "operation" "unknown-op")
              "the reported combination")
             ;; 6. A fixture that records no rendering must not pass by
             ;; asserting nothing.
             (expect-fixture-rejected
              (fixture-without base "expected_rendered")
              "missing expected_rendered")
             ;; 7. An unknown fixture kind must still be refused.
             (expect-fixture-rejected
              (fixture-with base "kind" "flow_unknown_kind")
              "unknown fixture kind")))
         (problems (remove nil checks)))
    (format t "~&axflow harness: ~d passed, ~d failed (of ~d checks)~%"
            (- (length checks) (length problems)) (length problems) (length checks))
    (dolist (problem problems)
      (format t "~&  FAIL ~a~%" problem))
    (null problems)))

(defun run-flow-conformance-tests ()
  "Run every ir/conformance/axflow fixture. Returns T when all of them pass."
  (let ((files (fixture-files))
        (passed 0)
        (failures '()))
    (when (null files)
      (format t "~&axflow: no fixtures found under ~a~%" (conformance-directory))
      (return-from run-flow-conformance-tests nil))
    (dolist (path files)
      (let ((name (pathname-name path)))
        (handler-case
            (let ((fixture (read-fixture path)))
              (run-fixture fixture)
              ;; Record only a fixture that fully passed, keyed by its disk
              ;; filename. A no-op unless the full gate enabled the report.
              (axllm/conformance:record-result "axflow" path (fixture-category fixture))
              (incf passed))
          (error (condition)
            (push (cons name (princ-to-string condition)) failures)))))
    (setf failures (nreverse failures))
    (format t "~&axflow: ~d passed, ~d failed (of ~d fixtures)~%"
            passed (length failures) (length files))
    (dolist (failure failures)
      (format t "~&  FAIL ~a~%    ~a~%" (car failure) (cdr failure)))
    ;; The harness checks run with the suite: a fixture count means nothing
    ;; without evidence that the assertions behind it can fail.
    (let ((harness (run-flow-harness-tests)))
      (and (null failures) harness))))
