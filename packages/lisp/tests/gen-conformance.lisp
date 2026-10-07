;;;; gen-conformance.lisp --- the shared axgen and axprogram fixtures.
;;;;
;;;; Entry point for the repository runner:
;;;;
;;;;   (axllm:run-gen-conformance)        ; => (values passed failed not-claimed)
;;;;   (axllm:run-gen-conformance-or-die) ; exits non-zero on failure
;;;;
;;;; These are the same JSON fixtures every Ax port runs, read from
;;;; ir/conformance/axgen and ir/conformance/axprogram.  Nothing here is a
;;;; port-local restatement of them: a fixture's expectations are read from the
;;;; file and asserted against what this package actually produces.
;;;;
;;;; Three rules make the result mean something.
;;;;
;;;; A fixture kind this runner does not dispatch is an ERROR, not a skip, so a
;;;; new kind in the shared suite cannot arrive unnoticed.
;;;;
;;;; A fixture is CLAIMED only when this runner implements every key it carries.
;;;; A claimed fixture must pass.  A fixture carrying a key the runner cannot
;;;; assert is reported as NOT CLAIMED, by name, with the keys that made it so;
;;;; it is never counted as a pass.  That is the difference between "we do not
;;;; do this yet" and "we checked nothing and said nothing".
;;;;
;;;; An expectation is asserted semantically: the output values, the request
;;;; bodies, the recorded tool calls, the traces and the error text, compared
;;;; against the fixture's own expected values.  No assertion here passes merely
;;;; because a key exists.

(defpackage #:axllm/gen-conformance
  (:use #:cl #:axllm)
  (:documentation
   "The shared axgen and axprogram fixture runner.

Its own package, so its helpers cannot collide with another suite's: two runners
that shared a failure helper would also share its behaviour, and a change made
for one gate would silently change the other.")
  ;; The service generics and the public API come through :use.  Only the
  ;; internals this suite reaches for are imported, and each one is a deliberate
  ;; reach into AXLLM rather than a name this package happens to spell the same.
  (:import-from #:axllm
                #:%present #:%object-keys #:%new-array #:%integer-or-zero #:%blankp
                #:add-assert #:add-field-transform #:set-stop-functions
                #:ax-error-message-text #:generator-memory #:+test-openai-model+))

(in-package #:axllm/gen-conformance)

(define-condition fixture-failure (error)
  ((text :initarg :text :reader fixture-failure-text))
  (:report (lambda (c s) (write-string (fixture-failure-text c) s))))

(defun %fixture-fail (format-control &rest arguments)
  (error 'fixture-failure :text (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; Loading
;;; ------------------------------------------------------------------

(defun %conformance-root ()
  "The fixture tree, overridden by AXIR_CONFORMANCE_DIR (or legacy AX_IR_ROOT)."
  (let ((override (or (uiop:getenv "AXIR_CONFORMANCE_DIR")
                      (uiop:getenv "AX_IR_ROOT"))))
    (cond ((and override (plusp (length override)))
           (uiop:ensure-directory-pathname override))
          ;; From this file's own location, so the suite runs whether or not the
          ;; system was loaded through ASDF.
          ((ignore-errors (asdf:system-source-directory "axllm"))
           (merge-pathnames "../../ir/conformance/"
                            (asdf:system-source-directory "axllm")))
          (t (merge-pathnames "../../../ir/conformance/"
                              (or *compile-file-truename* *load-truename*
                                  #p"/home/user/workspace/repo/packages/lisp/tests/x.lisp"))))))

(defun %load-fixtures (suite)
  "Every fixture in SUITE, as (name . object), sorted by name."
  (let ((directory (merge-pathnames (concatenate 'string suite "/")
                                    (%conformance-root)))
        (out '()))
    (dolist (path (sort (directory (merge-pathnames "*.json" directory))
                        #'string< :key #'namestring))
      (let ((fixture (handler-case (parse-json (uiop:read-file-string path))
                       (error (condition)
                         (%fixture-fail "~a is not readable JSON: ~a"
                                        (file-namestring path) condition)))))
        (unless (hash-table-p fixture)
          (%fixture-fail "~a is not a JSON object." (file-namestring path)))
        (push (list (or (%present (jget fixture "name")) (pathname-name path))
                    fixture
                    (file-namestring path))
              out)))
    (nreverse out)))

;;; ------------------------------------------------------------------
;;; What this runner claims
;;; ------------------------------------------------------------------

(defparameter +fixture-metadata-keys+
  '("name" "kind" "source" "description")
  "Keys that describe a fixture rather than drive or check a run.")

(defparameter +forward-input-keys+
  '("signature" "input" "responses" "tools" "forward_options" "options"
    "result_picker_index" "signature_spec" "features" "speak_responses"
    "assertions" "field_transforms" "field_processors" "feedback_processors"
    "streaming_processors" "streaming_assertions" "stop_functions"
    "control" "constructor_control" "control_steer" "constructor_cancellation" "call_cancellation"
    "examples" "demos" "function_result_formatter" "call_function_result_formatter"
    "global_function_result_formatter" "requires_lone_surrogates" "client" "native_session")
  "Keys this runner uses to drive a `forward' fixture.")

(defparameter +forward-expectation-keys+
  '("expected_output" "expected_request_count" "expected_error_contains"
    "expected_error_cause_contains" "expected_tool_calls" "expected_request_contains"
    "expected_request_not_contains" "expected_request_roles" "expected_trace"
    "expected_function_traces_subset" "expected_memory_history_subset"
    "expected_memory_history_count" "expected_chat_log_subset" "expected_prompt_contains"
    "expected_request" "expected_chat_options_subset" "expected_chat_prompt"
    "expected_chat_prompt_contains" "expected_last_request_tail" "expected_step_requests"
    "expected_speak_requests" "expected_processor_calls" "expect_chat_path"
    "expected_generate_error" "expected_control_events" "expected_memory_function_results"
    "expected_memory_function_stored_results" "expected_tool_extras"
    "expected_deprecations" "expected_picker_samples" "expected_session_log"
    "expected_session_tool_results")
  "Expectations this runner asserts for a `forward' fixture.")

(defparameter +streaming-expectation-keys+
  '("expected_deltas" "stop_after_deltas")
  "Keys a `streaming_forward' fixture adds on top of the forward expectations.")

(defparameter +program-contract-keys+
  '("program" "signature" "options" "expected_component_ids"
    "program_id" "steps" "expected_components_subset")
  "Keys this runner uses for a `program_contract' fixture.")

(defparameter +deferred-kinds+
  '(("cache_sequence" . "multi-call cache sequences are not driven yet")
    ("date_field_value" . "the date value tables are checked by the signature suite, not here")
    ("date_input" . "the date input tables are checked by the signature suite, not here")
    ("stream" . "stream folding fixtures are not driven yet")
    ("signature_error" . "signature errors are checked by the signature suite")
    ("prompt" . "prompt rendering is checked by the prompt suite")
    ("json_schema" . "schema generation is checked by the schema suite"))
  "Kinds this runner deliberately does not run yet, each with the reason.

A kind in neither this list nor the dispatch below is an error: the shared suite
must not be able to grow a kind that silently counts as covered.")

;;; ------------------------------------------------------------------
;;; The fixture as a service
;;; ------------------------------------------------------------------
;;;
;;; A fixture describes normalized completions: content, a thought, function
;;; calls, usage, a finish reason, or an injected failure.  That is what a
;;; service hands a generator, so the fixture is played back as a service rather
;;; than re-encoded into one provider's wire format and parsed again.  Provider
;;; normalization has its own suite; this one is about what the generator does
;;; with a completion.

(defclass fixture-service ()
  ((responses :initarg :responses :accessor fixture-service-responses)
   (requests :initform '() :accessor fixture-service-requests)
   (options :initform '() :accessor fixture-service-options)
   (features :initarg :features :initform nil :reader fixture-service-features)
   (on-request :initform nil :accessor fixture-on-request)
   (speak-responses :initarg :speak-responses :initform nil :accessor fixture-speak-responses)
   (speak-requests :initform '() :accessor fixture-speak-requests)))

(defun %fixture-call-arguments (call)
  "A fixture function call's parameters, as the wire carries them: a string."
  (let ((params (if (nth-value 1 (gethash "params" call))
                    (jget call "params")
                    (jget call "args"))))
    (cond ((stringp params) params)
          ((or (null params) (eq params :null)) "{}")
          (t (encode-json params)))))

(defun %fixture-tool-calls (completion)
  "COMPLETION's function calls, normalized as this port's provider layer would."
  (let ((calls (or (%present (jget completion "function_calls"))
                   (%present (jget completion "tool_calls")))))
    (if (and calls (vectorp calls) (not (stringp calls)))
        (map 'vector
             (lambda (call)
               (if (hash-table-p call)
                   (let ((nested (%present (jget call "function"))))
                     ;; Core's own call shape, so the provider-side flattening is the
                     ;; one the generator sees rather than a shortcut taken here.
                     (object "id" (or (%present (jget call "id")) "")
                             "type" (or (%present (jget call "type")) "function")
                             "function"
                             (object "name" (if nested (jget nested "name") (jget call "name"))
                                     "params" (%fixture-call-arguments (or nested call)))))
                   call))
             calls)
        (%new-array))))

(defparameter +fixture-finish-failures+
  '(("length" . "Max tokens reached before completion")
    ("content_filter" . "Model refused the request"))
  "Finish reasons that end a run, with the reason a service reports.")

(defun %fixture-completions (results)
  "Several fixture completions as one Core chat response."
  (let ((merged (%fixture-completion (aref results 0))))
    (setf (gethash "results" merged)
          (map 'vector (lambda (completion)
                         (aref (jget (%fixture-completion completion) "results") 0))
               results))
    (loop for index from 0 below (length (jget merged "results"))
          do (setf (gethash "index" (aref (jget merged "results") index)) index))
    merged))

(defun %fixture-completion (completion)
  "One fixture completion as Core's chat response, which is ax-chat's contract."
  (let ((usage (%present (jget completion "usage"))))
    (object "results"
            (vector (object "index" (or (%present (jget completion "index")) 0)
                            "content" (or (%present (jget completion "content")) "")
                            "function_calls" (%fixture-tool-calls completion)
                            "thought" (or (%present (jget completion "thought")) "")
                            "finish_reason"
                            (or (%present (jget completion "finish_reason")) "stop")))
            "model_usage"
            (object "ai" "openai" "model" +test-openai-model+
                    "tokens"
                    (object "prompt_tokens"
                            (if (hash-table-p usage)
                                (%integer-or-zero (jget usage "prompt_tokens")) 0)
                            "completion_tokens"
                            (if (hash-table-p usage)
                                (%integer-or-zero (jget usage "completion_tokens")) 0)
                            "total_tokens"
                            (if (hash-table-p usage)
                                (%integer-or-zero (jget usage "total_tokens")) 0))))))

(defun %fixture-raw-calls (completion)
  "COMPLETION's function calls as they came off the wire, unnormalized."
  (let ((calls (or (%present (jget completion "function_calls"))
                   (%present (jget completion "tool_calls")))))
    (if (and calls (vectorp calls) (not (stringp calls)))
        calls
        (%new-array))))

(defun %fixture-reject-unusable-calls (completion result-index)
  "Refuse COMPLETION when a function call's shape is unusable.

A real service refuses the whole response before a normalized call escapes, so
the double does too: these fixtures pin that message and pin that no tool ran,
and a double that normalized them away could not fail the way they describe."
  (let ((problems (tool-call-problems (%fixture-raw-calls completion) result-index)))
    (when (plusp (length problems))
      (let ((first (aref problems 0)))
        (error 'provider-error
               :kind :response
               :provider "openai"
               :message (jget first "message"))))))

(defclass fixture-stream ()
  ((chunks :initarg :chunks :accessor fixture-stream-chunks)
   (closed :initform nil :accessor fixture-stream-closed-p))
  (:documentation "One streamed turn: the fixture's chunks, handed over one at a time.

The handle holds the chunks that are left rather than an index into a vector it
has already produced, so a consumer that stops reading leaves the rest unread and
that is observable."))

(defmethod ax-stream ((service fixture-service) request &optional options)
  (push options (fixture-service-options service))
  (push (parse-json (encode-json request)) (fixture-service-requests service))
  (when (fixture-on-request service)
    (funcall (fixture-on-request service) (length (fixture-service-requests service))))
  (let ((response (pop (fixture-service-responses service))))
    (unless response
      (%fixture-fail "the fixture ran out of responses after ~a request(s)"
                     (length (fixture-service-requests service))))
    (%fixture-signal-injected-failure response)
    (let ((chunks (%present (jget response "stream"))))
      (make-instance 'fixture-stream
                     :chunks (if (and chunks (vectorp chunks) (not (stringp chunks)))
                                 (coerce chunks 'list)
                                 ;; A fixture that describes a whole completion
                                 ;; rather than chunks is one chunk: the same
                                 ;; turn, arriving all at once.
                                 (list (%fixture-completion response)))))))

(defmethod ax-stream-next ((handle fixture-stream))
  (when (fixture-stream-closed-p handle)
    (%fixture-fail "the generator read from a stream it had already closed"))
  (let ((chunk (pop (fixture-stream-chunks handle))))
    (when chunk (%fixture-signal-injected-failure chunk))
    (or chunk :null)))

(defmethod ax-stream-close ((handle fixture-stream))
  (setf (fixture-stream-closed-p handle) t))

(defun %fixture-signal-injected-failure (response)
  "Signal RESPONSE's injected provider failure, if it carries one.

A fixture can describe an unhealthy service instead of a completion, and that has
to behave the same whether the turn is streamed or not."
  (let ((failure (%present (jget response "error"))))
    (when failure
      (error 'provider-error
             :kind (let ((type (%present (jget failure "type"))))
                     (cond ((equal type "status") :status)
                           ((equal type "timeout") :timeout)
                           ((equal type "network") :network)
                           ((equal type "refusal") :refusal)
                           (t :response)))
             :provider "openai"
             :status (%present (jget failure "status"))
             :message (or (%present (jget failure "message")) "fixture failure")))))

(defmethod ax-chat ((service fixture-service) request &optional options)
  (push options (fixture-service-options service))
  (push (parse-json (encode-json request)) (fixture-service-requests service))
  (when (fixture-on-request service)
    (funcall (fixture-on-request service) (length (fixture-service-requests service))))
  (let ((response (pop (fixture-service-responses service))))
    (unless response
      (%fixture-fail "the fixture ran out of responses after ~a request(s)"
                     (length (fixture-service-requests service))))
    ;; A fixture can inject a provider failure instead of a completion, which is
    ;; how the retry and refusal fixtures describe an unhealthy service.
    (%fixture-signal-injected-failure response)
    (when (%present (jget response "stream"))
      (return-from ax-chat
        (axllm/core::fold-chat-response-stream (jget response "stream"))))
    (let ((results (%present (jget response "results"))))
      (if (and results (vectorp results) (not (stringp results)))
          ;; Several completions in one response is multi-sample generation, which
          ;; the generator now asks for and Core parses; each one is checked and
          ;; carried, so a picker chooses among real candidates.
          response
          (%fixture-completion response)))))

(defclass priced-fixture-service (fixture-service)
  ((provider :initarg :provider :reader fixture-provider)))

(defmethod ax-chat :before ((service priced-fixture-service) request &optional options)
  ;; Exercise the provider's real preflight gate, before recording a request.
  (axllm::%provider-prepare-request (fixture-provider service) request options))

(defmethod ax-service-name ((service fixture-service)) "openai")

(defmethod ax-options ((service fixture-service)) (object "model" +test-openai-model+))

(defclass native-fixture-service (fixture-service)
  ((sessions :initarg :sessions :accessor fixture-sessions)
   (session-log :initform (%new-array) :reader fixture-session-log)
   (session-results :initform (%new-array) :reader fixture-session-results)))

(defmethod ax-features :around ((service native-fixture-service) &optional model)
  (declare (ignore model))
  (axllm/core::core-map-merge (call-next-method) (object "asyncTools" true)))

(defmethod axllm/core::core-host-get ((service native-fixture-service) key &optional (fallback :null))
  (if (equal key "open_chat_session")
      (lambda (request options)
        (push (parse-json (encode-json request)) (fixture-service-requests service))
        (push options (fixture-service-options service))
        (vector-push-extend (object "op" "open") (fixture-session-log service))
        (when (fixture-on-request service)
          (funcall (fixture-on-request service) (length (fixture-service-requests service))))
        (let* ((script (coerce (or (pop (fixture-sessions service))
                                    (%fixture-fail "scripted sessions exhausted")) 'list))
               (events (coerce (pop script) 'list))
               (submitted (%new-array)) (closed nil))
          (object
           "model" "scripted-session"
           "next" (lambda ()
                    (when closed (%fixture-fail "reading a closed session"))
                    (let ((event (or (pop events) (%fixture-fail "scripted session exhausted"))))
                      (object "type" (jget event "type") "response_id" (jget event "response_id")
                              "response" (object "results" (jget event "results" #())))))
           "submit" (lambda (results)
                      (loop for result across results do
                        (vector-push-extend (jget result "function_id") submitted)
                        (vector-push-extend
                         (object "call_id" (jget result "function_id") "result" (jget result "result")
                                 "is_error" (jget result "is_error" false))
                         (fixture-session-results service))))
           "continue" (lambda ()
                        (vector-push-extend (object "op" "continue" "call_ids" submitted)
                                            (fixture-session-log service))
                        (setf submitted (%new-array))
                        (when events (%fixture-fail "continued before consuming the response"))
                        (setf events (coerce (or (pop script) (%fixture-fail "no continuation scripted")) 'list)))
           "steer" (lambda (text)
                     (vector-push-extend (object "op" "steer" "text" text) (fixture-session-log service))
                     "next-response")
           "thinking" (lambda (level)
                        (vector-push-extend (object "op" "thinking" "level" level) (fixture-session-log service))
                        "next-response")
           "close" (lambda ()
                     (unless closed
                       (setf closed t)
                       (vector-push-extend (object "op" "close") (fixture-session-log service)))))))
      fallback))

(defmethod ax-speak ((service fixture-service) request &optional options)
  (declare (ignore options))
  (push request (fixture-speak-requests service))
  (let ((response (pop (fixture-speak-responses service))))
    (unless response (%fixture-fail "speech fixture exhausted"))
    (%fixture-signal-injected-failure response)
    response))

(defmethod ax-features ((service fixture-service) &optional model)
  "What this double claims to support.

Pinned rather than left to a profile: a double that answers in one dialect while
a fixture names a model whose profile speaks another reads empty content with no
error at all, which is the worst way for a runner to be wrong."
  (declare (ignore model))
  (or (fixture-service-features service)
      (object "functions" true
          "streaming" false
          "structured_outputs" true)))

(defun %fixture-request-text (service index)
  "The INDEXth request this service received, as JSON text."
  (let ((requests (reverse (fixture-service-requests service))))
    (when (>= index (length requests))
      (%fixture-fail "expected at least ~a request(s), saw ~a" (1+ index) (length requests)))
    (encode-json (nth index requests))))

(defun %fixture-request (service index)
  (let ((requests (reverse (fixture-service-requests service))))
    (when (>= index (length requests))
      (%fixture-fail "expected at least ~a request(s), saw ~a" (1+ index) (length requests)))
    (nth index requests)))

(defun %fixture-tools (fixture recorded &optional extras-recorded)
  "The fixture's tools, each recording its invocation into RECORDED."
  (let ((specs (%present (jget fixture "tools")))
        (out '()))
    (when (and specs (vectorp specs) (not (stringp specs)))
      (loop for spec across specs
            do (let* ((name (jget spec "name"))
                      (args (%present (jget spec "args")))
                      (result (if (nth-value 1 (gethash "result" spec))
                                  (jget spec "result")
                                  :null))
                      (failure (%present (jget spec "error")))
                      (properties (object))
                      (required '()))
                 (when (hash-table-p args)
                   (dolist (key (%object-keys args))
                     (let* ((field (gethash key args))
                            (type (or (%present (jget field "type")) "string")))
                       (setf (gethash key properties) (object "type" type))
                       (when (%present (jget field "description"))
                         (setf (gethash "description" (gethash key properties)) (jget field "description")))
                       (dolist (bound '("min" "max"))
                         (when (nth-value 1 (gethash bound field))
                           (setf (gethash (if (equal type "string")
                                              (if (equal bound "min") "minLength" "maxLength")
                                              (if (equal bound "min") "minimum" "maximum"))
                                          (gethash key properties))
                                 (gethash bound field))))
                       (unless (json-true-p (jget field "optional"))
                         (push key required)))))
                 (push (tool :name name
                             :description (or (%present (jget spec "description")) name)
                             :parameters (object "type" "object"
                                                 "properties" properties
                                                 "required" (coerce (nreverse required) 'vector))
                             :handler
                             (let ((name name) (result result) (failure failure)
                                   (record-extras (json-true-p (jget spec "record_extras"))))
                               (lambda (arguments &optional extras)
                                 (vector-push-extend (object "name" name "args" arguments)
                                                     recorded)
                                 (when (and extras-recorded record-extras)
                                   (let ((selected (object)))
                                     (dolist (pair '(("sessionId" "session_id")
                                                     ("executionPath" "execution_path")
                                                     ("eventContext" "event_context")))
                                       (let ((value (or (%present (jget extras (first pair)))
                                                        (%present (jget extras (second pair))))))
                                         (when value (setf (gethash (first pair) selected) value))))
                                     (vector-push-extend (object "name" name "extras" selected) extras-recorded)))
                                 (when failure (error "~a" failure))
                                 result)))
                       out))))
    (nreverse out)))

;;; ------------------------------------------------------------------
;;; Semantic comparison
;;; ------------------------------------------------------------------

(defun %json-equal (left right)
  "JSON structural equality, so two readings of the same value compare equal."
  (cond ((and (hash-table-p left) (hash-table-p right))
         (let ((left-keys (sort (%object-keys left) #'string<))
               (right-keys (sort (%object-keys right) #'string<)))
           (and (equal left-keys right-keys)
                (every (lambda (key) (%json-equal (gethash key left) (gethash key right)))
                       left-keys))))
        ((and (vectorp left) (not (stringp left)) (vectorp right) (not (stringp right)))
         (and (= (length left) (length right))
              (every #'%json-equal (coerce left 'list) (coerce right 'list))))
        ((and (stringp left) (stringp right)) (string= left right))
        ((and (realp left) (realp right) (not (eq left t)) (not (eq right t)))
         (= left right))
        (t (eql left right))))

(defun %json-subset-p (actual expected)
  "True when EXPECTED is contained in ACTUAL.

An object is descended key by key, because a fixture names only the keys it
cares about.  A list is compared by value, because that is what every other
port's subset assertion does: a usage entry with an extra key is a different
entry, not a superset of one."
  (cond ((hash-table-p expected)
         (and (hash-table-p actual)
              (every (lambda (key)
                       (multiple-value-bind (value present) (gethash key actual)
                         (and present (%json-subset-p value (gethash key expected)))))
                     (%object-keys expected))))
        ((and (vectorp expected) (not (stringp expected)))
         (and (vectorp actual) (not (stringp actual))
              (= (length actual) (length expected))
              (every #'%json-subset-p (coerce actual 'list) (coerce expected 'list))))
        (t (%json-equal actual expected))))

(defun %expect-json-equal (actual expected what)
  (unless (%json-equal actual expected)
    (%fixture-fail "~a~%  expected ~a~%  actual   ~a" what
                   (encode-json expected) (encode-json actual))))

(defun %expect-json-subset (actual expected what)
  (unless (%json-subset-p actual expected)
    (%fixture-fail "~a~%  expected (subset) ~a~%  actual            ~a" what
                   (encode-json expected) (encode-json actual))))

(defun %expect-list-subset (actual expected what)
  "Match each expected record against an actual record, as the shared runner does."
  (loop for item across expected
        unless (find-if (lambda (record) (%json-subset-p record item)) actual)
          do (%fixture-fail "~a lacks record ~a in ~a" what
                            (encode-json item) (encode-json actual))))

(defun %failure-text (condition)
  "CONDITION's message exactly as it reads, newlines included.

A fixture's expected text carries the newlines Core put there, so collapsing
them here would compare two strings that only look alike when printed."
  (if (typep condition 'axllm:ax-error)
      (axllm:ax-error-message condition)
      (princ-to-string condition)))

(defun %expect-contains (haystack needle what)
  (unless (and (stringp haystack) (search needle haystack))
    (%fixture-fail "~a~%  expected to contain ~s~%  in ~s" what needle haystack)))

;;; ------------------------------------------------------------------
;;; forward
;;; ------------------------------------------------------------------

(defclass fixture-picker ()
  ((index :initarg :index :reader fixture-picker-index)
   (samples :initarg :samples :initform nil :reader fixture-picker-samples)))

(defmethod axllm/core::core-host-call ((picker fixture-picker) method args)
  "The picker Core calls to choose a sample.

A fixture names the index it wants realized, so the double answers that index and
nothing else; Core checks it is in range."
  (if (equal method "call")
      (progn
        (let ((payload (aref args 0)))
          (when (fixture-picker-samples picker)
            (loop for sample across (jget payload "results" #()) do
              (vector-push-extend sample (fixture-picker-samples picker)))))
        (let ((payload (aref args 0)))
          (unless (equal (%present (jget payload "type")) "fields")
            (%fixture-fail "a result picker is handed a fields payload, got ~a"
                           (encode-json payload)))
          (unless (plusp (length (%present (jget payload "results"))))
            (%fixture-fail "a result picker is handed the samples to choose from")))
        (fixture-picker-index picker))
      (call-next-method)))

(defun %fixture-forward-options (fixture &optional picker-samples)
  "The fixture's forward options, as `forward' reads them."
  (let ((options (let ((given (%present (jget fixture "forward_options"))))
                   (if (hash-table-p given)
                       (axllm/core::core-map-merge (object) given)
                       (object))))
        (picker-index (%present (jget fixture "result_picker_index"))))
    (when picker-index
      (setf (gethash "resultPicker" options)
            (make-instance 'fixture-picker :index picker-index :samples picker-samples)))
    options))

(defun %fixture-signature (fixture)
  (if (%present (jget fixture "signature_spec"))
      (signature-from-spec (jget fixture "signature_spec"))
      (parse-signature (jget fixture "signature"))))

(defun %fixture-cancellation (spec)
  (let ((token (axllm::cancellation-token)))
    (when (json-true-p (jget spec "cancelled"))
      (axllm::cancel token (jget spec "reason" "fixture cancellation")))
    token))

(defun %fixture-formatter (spec)
  (lambda (result)
    (declare (ignore result))
    (when (%present (jget spec "throws")) (error 'ax-error :message (jget spec "throws")))
    (jget spec "text" "")))

(defun %fixture-processor (spec calls)
  (let ((returned 0))
    (lambda (value context)
      (vector-push-extend (object "field" (jget spec "field") "value" value
                                  "done" (jget context "done")) calls)
      (when (%present (jget spec "throws"))
        (error 'ax-error :message (jget spec "throws")))
      (cond ((and (json-true-p (jget spec "when_done"))
                  (not (json-true-p (jget context "done")))) :null)
            ((and (%present (jget spec "times")) (>= returned (jget spec "times"))) :null)
            (t (let ((value (if (json-true-p (jget spec "echo")) value (jget spec "returns"))))
                 (unless (eq value :null) (incf returned))
                 value))))))

(defun %configure-fixture-generator (gen fixture processor-calls)
  (when (%present (jget fixture "examples")) (axllm::set-examples gen (jget fixture "examples")))
  (when (%present (jget fixture "demos")) (axllm::set-demos gen (jget fixture "demos")))
  (when (%present (jget fixture "function_result_formatter"))
    (setf (gethash "function_result_formatter" (axllm::generator-base-options gen))
          (%fixture-formatter (jget fixture "function_result_formatter"))))
  (loop for spec across (jget fixture "assertions" #()) do
    (add-assert gen
                (let ((spec spec))
                  (lambda (output)
                    (when (%present (jget spec "throw"))
                      (error 'ax-error :message (jget spec "throw")))
                    (let ((value (if (%present (jget spec "field"))
                                     (jget output (jget spec "field")) output)))
                      (cond ((nth-value 1 (gethash "return" spec))
                             (let ((value (jget spec "return"))) (if (eq value :null) t value)))
                            ((%present (jget spec "contains"))
                             (json-boolean (and (stringp value) (search (jget spec "contains") value))))
                            ((nth-value 1 (gethash "equals" spec))
                             (json-boolean (%json-equal value (jget spec "equals"))))
                            (t t)))))
                :message (%present (jget spec "message"))))
  (dolist (key '("field_processors" "field_transforms"))
    (loop for spec across (jget fixture key #()) do
      (add-field-transform gen (jget spec "field")
                           (let ((op (or (%present (jget spec "processor")) (jget spec "op"))))
                             (cond ((equal op "uppercase") #'string-upcase)
                                   ((equal op "lowercase") #'string-downcase)
                                   ((equal op "trim") #'axllm/core::core-string-trim)
                                   (t (%fixture-fail "unknown field transform ~a" op)))))))
  (setf (axllm::generator-feedback-processors gen)
        (loop for spec across (jget fixture "feedback_processors" #())
              collect (object "field" (jget spec "field")
                              "processor" (%fixture-processor spec processor-calls))))
  (setf (gethash "streaming_field_processors" (axllm::generator-base-options gen))
        (map 'vector (lambda (spec) (object "field" (jget spec "field")
                                           "processor" (%fixture-processor spec processor-calls)))
             (jget fixture "streaming_processors" #())))
  (setf (gethash "streaming_assertions" (axllm::generator-base-options gen))
        (jget fixture "streaming_assertions" #()))
  (when (%present (jget fixture "stop_functions"))
    (set-stop-functions gen (jget fixture "stop_functions"))))

(defun %run-forward-fixture (fixture &key streaming)
  "Drive one `forward' fixture and assert every expectation it carries.

With STREAMING, the run goes through `program-streaming-forward' with a capturing
sink and the fixture's `expected_deltas' are asserted as well.  Everything else is
the same assertion set deliberately: a streamed run has to reach the same
outputs, the same request count and the same failures as an unstreamed one, and
driving it through a separate copy of these checks is how that stops being true."
  (let* ((signature (%fixture-signature fixture))
         (responses (%present (jget fixture "responses")))
         (recorded (make-array 0 :adjustable t :fill-pointer 0))
         (tool-extras (%new-array))
         (picker-samples (%new-array))
         (deprecations (%new-array))
         (axllm/core::*ai-warnings-shown* (make-hash-table :test #'equal))
         (axllm/core::*ai-warning-sink* (lambda (text) (vector-push-extend text deprecations)))
         (tools (%fixture-tools fixture recorded tool-extras))
         (service (apply #'make-instance
                          (cond ((%present (jget fixture "native_session")) 'native-fixture-service)
                                ((%present (jget fixture "client")) 'priced-fixture-service)
                                (t 'fixture-service))
                                 :features (%present (jget fixture "features"))
                                 :speak-responses (coerce (jget fixture "speak_responses" #()) 'list)
                                 :responses (if (and responses (vectorp responses)
                                                     (not (stringp responses)))
                                                (coerce responses 'list)
                                                '())
                          (cond ((%present (jget fixture "native_session"))
                                 (list :sessions (coerce (jget fixture "native_session") 'list)))
                                ((%present (jget fixture "client"))
                            (list :provider (provider :name "openai" :api-key "test-only"
                                                :model (jget (jget fixture "client") "model")
                                                :options (jget (jget fixture "client") "options")))))))
         (gen (ax signature :tools tools
                            :options (let ((given (%present (jget fixture "options"))))
                                       (if (hash-table-p given) given (object)))))
         (inputs (let ((given (%present (jget fixture "input"))))
                   (if (hash-table-p given) given (object))))
         (deltas (make-array 0 :adjustable t :fill-pointer 0))
         (processor-calls (%new-array))
         (control-events (%new-array))
         (control (when (or (json-true-p (jget fixture "control"))
                            (json-true-p (jget fixture "constructor_control")))
                    (axllm::make-run-control
                     :listener (lambda (event)
                                 (when (or (%present (jget fixture "control_steer"))
                                           (member (jget event "type") '("started" "completed" "failed" "aborted") :test #'equal))
                                   (vector-push-extend (object "type" (jget event "type") "path" (jget event "path")) control-events))))))
         (outputs nil)
         (old-formatter (axllm::get-global "functionResultFormatter"))
         (failure nil))
    (unless signature (%fixture-fail "no signature"))
    (when (json-true-p (jget fixture "requires_lone_surrogates"))
      (unless (= (char-code (char (parse-json "\"\\ud83d\"") 0)) #xd83d)
        (%fixture-fail "JSON decoding lost a lone UTF-16 surrogate")))
    (%configure-fixture-generator gen fixture processor-calls)
    (when (json-true-p (jget fixture "constructor_control"))
      (setf (gethash "control" (axllm::generator-base-options gen)) control))
    (when (%present (jget fixture "constructor_cancellation"))
      (setf (gethash "cancellation" (axllm::generator-base-options gen))
            (%fixture-cancellation (jget fixture "constructor_cancellation"))))
    (when (%present (jget fixture "control_steer"))
      (let ((steer (jget fixture "control_steer")))
        (setf (fixture-on-request service)
              (lambda (number)
                (when (= number (jget steer "during_request"))
                  (axllm::run-control-steer control (jget steer "text")))))))
    (unwind-protect
    (handler-case
        (let ((options (%fixture-forward-options fixture picker-samples)))
          (when (%present (jget fixture "call_function_result_formatter"))
            (setf (gethash "function_result_formatter" options)
                  (%fixture-formatter (jget fixture "call_function_result_formatter"))))
          (when (%present (jget fixture "global_function_result_formatter"))
            (axllm::set-global "functionResultFormatter" (%fixture-formatter (jget fixture "global_function_result_formatter"))))
          (when (json-true-p (jget fixture "control")) (setf (gethash "control" options) control))
          (when (%present (jget fixture "call_cancellation"))
            (setf (gethash "cancellation" options) (%fixture-cancellation (jget fixture "call_cancellation"))))
          (if streaming
              (progn
                (setf (gethash "sink" options)
                      (lambda (envelope)
                        (vector-push-extend envelope deltas)
                        (if (and (%present (jget fixture "stop_after_deltas"))
                                 (>= (length deltas) (jget fixture "stop_after_deltas")))
                            false true)))
                (setf outputs (program-streaming-forward gen service inputs options)))
              (setf outputs (forward gen service inputs options))))
      (error (condition) (setf failure condition)))
      (axllm::set-global "functionResultFormatter" old-formatter))
    (let ((requests (length (fixture-service-requests service))))
      (let ((expected-error (%present (jget fixture "expected_error_contains"))))
        (cond (expected-error
               (unless failure
                 (%fixture-fail "expected a failure containing ~s, the run completed with ~a"
                                expected-error (encode-json (or outputs (object)))))
               (%expect-contains (%failure-text failure) expected-error
                                 "the failure text"))
              (failure (%fixture-fail "the run failed: ~a" failure))))
      (let ((expected-cause (%present (jget fixture "expected_error_cause_contains"))))
        (when expected-cause
          (%expect-contains (%failure-text (typecase failure
                                             (axllm::ax-generate-error (axllm::ax-generate-error-cause failure))
                                             (provider-error (axllm::provider-error-cause failure)))) expected-cause
                            "the failure cause text")))
      (when (json-true-p (jget fixture "expected_generate_error"))
        (unless (typep failure 'axllm::ax-generate-error)
          (%fixture-fail "expected typed AxGenerateError, got ~s" failure)))
      (when (json-true-p (jget fixture "expect_chat_path"))
        (unless (plusp requests) (%fixture-fail "expected a chat request")))
      (when (typep service 'native-fixture-service)
        (dolist (entry (list (list "expected_session_log" (fixture-session-log service))
                            (list "expected_session_tool_results" (fixture-session-results service))))
          (when (nth-value 1 (gethash (first entry) fixture))
            (%expect-json-equal (second entry) (jget fixture (first entry)) (first entry)))))
      (dolist (entry (list (list "expected_speak_requests" (coerce (reverse (fixture-speak-requests service)) 'vector))
                          (list "expected_tool_extras" tool-extras)
                          (list "expected_picker_samples" picker-samples)
                          (list "expected_deprecations" deprecations)
                          (list "expected_control_events" control-events)
                          (list "expected_processor_calls" processor-calls)))
        (when (nth-value 1 (gethash (first entry) fixture))
          (%expect-json-equal (second entry) (jget fixture (first entry)) (first entry))))
      (when (%present (jget fixture "expected_request"))
        (%expect-json-subset (%fixture-request service 0) (jget fixture "expected_request") "first request"))
      (when (%present (jget fixture "expected_chat_options_subset"))
        (%expect-json-subset (car (last (fixture-service-options service)))
                            (jget fixture "expected_chat_options_subset") "chat options"))
      (when (%present (jget fixture "expected_chat_prompt"))
        (%expect-json-equal (jget (%fixture-request service 0) "chat_prompt")
                           (jget fixture "expected_chat_prompt") "chat prompt"))
      (loop for needle across (jget fixture "expected_chat_prompt_contains" #()) do
        (%expect-contains (encode-json (jget (%fixture-request service 0) "chat_prompt")) needle "chat prompt"))
      (loop for check across (jget fixture "expected_step_requests" #()) do
        (let ((request (%fixture-request service (jget check "index" 0))))
          (when (%present (jget check "request"))
            (%expect-json-subset request (jget check "request") "step request"))
          (when (%present (jget check "function_names"))
            (%expect-json-equal (map 'vector (lambda (fn) (jget fn "name")) (jget request "functions" #()))
                               (jget check "function_names") "step functions"))))
      (let ((tail (%present (jget fixture "expected_last_request_tail"))))
        (when tail
          (let* ((prompt (jget (%fixture-request service (1- requests)) "chat_prompt"))
                 (actual (map 'vector (lambda (message)
                                        (let ((out (object)))
                                          (dolist (key '("role" "content"))
                                            (when (nth-value 1 (gethash key message))
                                              (setf (gethash key out) (jget message key)))) out))
                              (subseq prompt (- (length prompt) (length tail))))))
            (%expect-json-equal actual tail "last request tail"))))
      (let ((expected (%present (jget fixture "expected_output"))))
        (when expected
          (%expect-json-equal (or outputs (object)) expected "the output values")))
      (let ((expected (%present (jget fixture "expected_request_count"))))
        (when expected
          (unless (= requests expected)
            (%fixture-fail "expected ~a provider request(s), made ~a" expected requests))))
      (let ((expected (%present (jget fixture "expected_deltas"))))
        (when expected
          (%expect-json-equal (coerce deltas 'vector) expected
                              "the deltas handed to the sink")))
      (let ((expected (%present (jget fixture "expected_tool_calls"))))
        (when expected
          (%expect-json-equal (coerce recorded 'vector) expected "the tool calls that ran")))
      (let ((expected (%present (jget fixture "expected_request_contains"))))
        (when (and expected (vectorp expected) (not (stringp expected)))
          (let ((text (%fixture-request-text service (1- requests))))
            (loop for needle across expected
                  do (%expect-contains text needle "the last request")))))
      (let ((expected (%present (jget fixture "expected_request_not_contains"))))
        (when (and expected (vectorp expected) (not (stringp expected)))
          (let ((text (%fixture-request-text service (1- requests))))
            (loop for needle across expected
                  do (when (search needle text)
                       (%fixture-fail "the last request must not contain ~s" needle))))))
      (let ((expected (%present (jget fixture "expected_request_roles"))))
        (when (and expected (vectorp expected) (not (stringp expected)))
          ;; One entry per request, in order, so a fixture pins how the
          ;; conversation grew rather than only how it ended.
          (let ((roles (map 'vector
                            (lambda (request)
                              (map 'vector (lambda (m) (jget m "role"))
                                   (jget request "chat_prompt")))
                            (coerce (reverse (fixture-service-requests service)) 'vector))))
            (%expect-json-equal roles expected "the message roles of each request"))))
      (let ((expected (%present (jget fixture "expected_prompt_contains"))))
        (when expected
          (%expect-contains (%fixture-request-text service 0) expected "the first request")))
      (let ((expected (%present (jget fixture "expected_trace"))))
        (when expected
          (let ((traces (program-traces gen)))
            (when (zerop (length traces)) (%fixture-fail "no trace was recorded"))
            (%expect-json-subset (aref traces 0) expected "the recorded trace"))))
      (let ((expected (%present (jget fixture "expected_function_traces_subset"))))
        (when expected
          (%expect-list-subset (program-function-call-traces gen) expected
                               "the recorded function-call traces")))
      (let ((expected (%present (jget fixture "expected_memory_history_subset"))))
        (when expected
          (%expect-list-subset (coerce (memory-history (generator-memory gen)) 'vector) expected
                               "the memory history")))
      (dolist (entry '(("expected_memory_function_results" "result_text")
                       ("expected_memory_function_stored_results" "result")))
        (when (%present (jget fixture (first entry)))
          (let ((actual (%new-array)))
            (dolist (item (memory-history (generator-memory gen)))
              (when (equal (jget item "role") "function")
                (loop for result across (jget item "results") do
                  (vector-push-extend (jget result (second entry)) actual))))
            (%expect-json-equal actual (jget fixture (first entry)) (first entry)))))
      (let ((expected (%present (jget fixture "expected_memory_history_count"))))
        (when expected
          (let ((count (length (memory-history (generator-memory gen)))))
            (unless (= count expected)
              (%fixture-fail "expected ~a memory item(s), recorded ~a" expected count)))))
      (let ((expected (%present (jget fixture "expected_chat_log_subset"))))
        (when expected
          (%expect-list-subset (program-chat-log gen) expected "the chat log"))))))

;;; ------------------------------------------------------------------
;;; program_contract
;;; ------------------------------------------------------------------

(defun %run-program-contract-fixture (fixture)
  "Drive one `program_contract' fixture."
  (let ((program (%present (jget fixture "program"))))
    (when (equal program "flow")
      (let ((flow (axllm:flow (object "id" (jget fixture "program_id")))))
        (loop for step across (jget fixture "steps") do
          (unless (equal (jget step "kind") "execute")
            (%fixture-fail "unsupported contract step ~a" (jget step "kind")))
          (axllm:flow-execute flow (jget step "name") (ax (jget step "signature"))))
        (%expect-list-subset (program-optimizable-components flow)
                             (jget fixture "expected_components_subset") "flow components")
        (return-from %run-program-contract-fixture)))
    (unless (equal program "axgen")
      (%fixture-fail "this runner implements the axgen program contract, not ~a" program))
    (let* ((options (let ((given (%present (jget fixture "options"))))
                      (if (hash-table-p given) given (object))))
           (gen (ax (%present (jget fixture "signature"))
                    :id (%present (jget options "id"))))
           (expected (%present (jget fixture "expected_component_ids"))))
      (when expected
        (%expect-json-equal
         (map 'vector (lambda (component) (jget component "id"))
              (program-optimizable-components gen))
         expected
         "the optimizable component ids")))))

;;; ------------------------------------------------------------------
;;; Dispatch
;;; ------------------------------------------------------------------

(defun %run-date-values-fixture (fixture)
  (loop for case across (jget fixture "cases") do
    (let ((field (axllm/core::core-map-merge (object) (jget case "field")))
          (result nil) (failure nil))
      (setf (gethash "parse_dates" field) (jget fixture "parse_dates"))
      (handler-case (setf result (axllm/core::stream-field-value-impl field (jget case "text")))
        (error (c) (setf failure c)))
      (if (%present (jget case "expected_error"))
          (%expect-json-equal (and failure (%failure-text failure)) (jget case "expected_error") "date error")
          (progn
            (when failure (%fixture-fail "unexpected date error: ~a" failure))
            (let ((actual (object "has" (jget result "has"))))
              (when (json-true-p (jget result "has"))
                (setf (gethash "value" actual) (jget result "value")))
              (%expect-json-equal actual (jget case "expected") "date parsed value")))))))

(defun %native-dates (value)
  (cond ((hash-table-p value)
         (cond ((%present (jget value "$date")) (local-time:parse-timestring (jget value "$date")))
               ((%present (jget value "$date_only"))
                (local-time:parse-timestring (concatenate 'string (jget value "$date_only") "T00:00:00Z")))
               (t (let ((out (object)))
                    (dolist (key (%object-keys value)) (setf (gethash key out) (%native-dates (jget value key)))) out))))
        ((and (vectorp value) (not (stringp value))) (map 'vector #'%native-dates value))
        (t value)))

(defun %run-stream-fixture (fixture)
  (when (%present (jget fixture "text_signature"))
    (let ((fields (signature-fields (parse-signature (jget fixture "text_signature")) :side :output))
          (text ""))
      (loop for chunk across (jget fixture "stream_events") do
        (setf text (concatenate 'string text chunk))
        (axllm/core::parse-text-output-fields-impl text fields false))
      (let ((output (axllm/core::parse-text-output-fields-impl text fields true)))
        (axllm/core::validate-output fields output)
        (%expect-json-equal output (jget fixture "expected_text_output") "streamed text extraction"))))
  (when (%present (jget fixture "structured_states"))
    (loop for case across (jget fixture "route_cases" #()) do
      (%expect-json-equal (axllm/core::stream-extraction-route (jget case "has_complex_fields"))
                         (jget case "expected") "stream route"))
    (let ((previous (object)) (emitted (%new-array)))
      (loop for state across (jget fixture "structured_states") do
        (let ((result (axllm/core::stream-structured-delta
                       (jget fixture "field_specs") (jget state "parsed_values") previous
                       (jget state "partial_array_incomplete"))))
          (%expect-json-equal (jget result "delta") (jget state "expected_delta") "structured delta")
          (%expect-json-equal (jget result "full_values") (jget state "expected_full_values") "structured values")
          (loop for item across (jget (jget result "delta") (jget fixture "tracked_array_field") #()) do
            (vector-push-extend item emitted))
          (setf previous (axllm/core::core-map-merge previous (jget result "full_values")))))
      (%expect-json-equal previous (jget fixture "expected_final_values") "final structured values")
      (%expect-json-equal emitted (jget fixture "expected_emitted_items") "emitted structured items")))
  (let ((chunks (%new-array)) (failure nil) (folded "")
        (assertions (jget fixture "streaming_assertions" #())))
    (handler-case
        (loop for event across (jget fixture "stream_events" #()) do
          (vector-push-extend event chunks)
          (setf folded (axllm/core::fold-stream chunks))
          (loop for spec across assertions do
            (let* ((field (object "name" (jget spec "field") "type" (object "name" "string")))
                   (gen (ax "query:string -> answer:string" :options (object "streaming_assertions" (vector spec))))
                   (state (object "curr_field" field "s" 0)))
              (axllm/core::stream-check-assertions-impl gen state folded false))))
      (error (c) (setf failure c)))
    (if (%present (jget fixture "expected_error_contains"))
        (progn (unless failure (%fixture-fail "stream should fail"))
               (%expect-contains (%failure-text failure) (jget fixture "expected_error_contains") "stream failure"))
        (progn (when failure (%fixture-fail "unexpected stream error: ~a" failure))
               (%expect-json-equal folded (jget fixture "expected_folded" "") "stream fold")))))

(defun %run-cache-fixture (fixture)
  (let* ((store (object)) (reads 0) (writes (%new-array))
         (cache (lambda (key &optional (value nil supplied))
                  (if supplied
                      (progn
                        (when (%present (jget fixture "cache_write_error"))
                          (error 'ax-error :message (jget fixture "cache_write_error")))
                        (vector-push-extend (parse-json (encode-json value)) writes)
                        (setf (gethash key store) (parse-json (encode-json value))))
                      (progn
                        (incf reads)
                        (when (%present (jget fixture "cache_read_error"))
                          (error 'ax-error :message (jget fixture "cache_read_error")))
                        (let ((value (gethash key store :null)))
                          (if (eq value :null) :null (parse-json (encode-json value))))))))
         (where (jget fixture "cache_in" "call"))
         (options (axllm/core::core-map-merge (object) (jget fixture "options" (object))))
         (service (make-instance 'fixture-service :responses (coerce (jget fixture "responses" #()) 'list)
                                 :features (%present (jget fixture "features"))
                                 :speak-responses (coerce (jget fixture "speak_responses" #()) 'list)))
         (outputs (%new-array)) (deltas (%new-array)) (counts (%new-array)) (errors (%new-array))
         (old-global (axllm::get-global "cachingFunction")))
    (when (equal where "constructor") (setf (gethash "caching_function" options) cache))
    (when (json-true-p (jget fixture "constructor_control"))
      (setf (gethash "control" options) (axllm::make-run-control)))
    (let ((gen (ax (%fixture-signature fixture) :options options)))
      (when (%present (jget fixture "result_picker_index"))
        (setf (gethash "resultPicker" (axllm::generator-base-options gen))
              (make-instance 'fixture-picker :index (jget fixture "result_picker_index"))))
      (unwind-protect
           (progn
             (when (equal where "global") (axllm::set-global "cachingFunction" cache))
             (loop for call across (jget fixture "calls") do
               (let* ((before (length (fixture-service-requests service)))
                      (call-options (axllm/core::core-map-merge (object) (jget call "forward_options" (object))))
                      (input (jget call "input" (object)))
                      (streaming (equal (jget call "kind") "streaming_forward"))
                      (emitted (%new-array)) (failure :null) (out :null))
                 (when (equal where "call") (setf (gethash "caching_function" call-options) cache))
                 (when (json-true-p (jget call "control"))
                   (setf (gethash "control" call-options) (axllm::make-run-control)))
                 (when (json-true-p (jget call "reverse_input_keys"))
                   (let ((reversed (object)))
                     (dolist (key (reverse (%object-keys input))) (setf (gethash key reversed) (jget input key)))
                     (setf input reversed)))
                 (setf (gethash "sink" call-options) (lambda (e) (vector-push-extend e emitted)))
                 (handler-case
                     (setf out (if streaming (program-streaming-forward gen service input call-options)
                                   (forward gen service input call-options)))
                   (error (c) (setf failure (first (uiop:split-string (%failure-text c) :separator '(#\Newline))))))
                 (vector-push-extend out outputs)
                 (vector-push-extend failure errors)
                 (vector-push-extend (if streaming emitted :null) deltas)
                 (vector-push-extend (- (length (fixture-service-requests service)) before) counts))))
        (axllm::set-global "cachingFunction" old-global)))
    (dolist (entry (list (list "expected_outputs" outputs) (list "expected_deltas" deltas)
                        (list "expected_requests" counts) (list "expected_cache_sets" writes)
                        (list "expected_request_count" (length (fixture-service-requests service)))
                        (list "expected_cache_gets" reads)))
      (%expect-json-equal (second entry) (jget fixture (first entry)) (first entry)))
    (%expect-json-equal errors (jget fixture "expected_errors" (make-array (length errors) :initial-element :null))
                       "cache call errors")
    (when (%present (jget fixture "expected_speak_requests"))
      (%expect-json-equal (coerce (reverse (fixture-speak-requests service)) 'vector)
                         (jget fixture "expected_speak_requests") "cached speech requests"))))

(defun %unclaimed-keys (fixture handled)
  "The fixture keys this runner does not implement."
  (sort (remove-if (lambda (key)
                     (or (member key +fixture-metadata-keys+ :test #'string=)
                         (member key handled :test #'string=)))
                   (%object-keys fixture))
        #'string<))

(defun %run-fixture (fixture)
  "Run FIXTURE.  Returns (values status detail).

STATUS is :semantic when every expectation the fixture carries was asserted and
held, :validation-error when the fixture expected a failure and got it, or
:not-claimed with the keys that are not implemented."
  (let ((kind (%present (jget fixture "kind"))))
    (cond
      ((equal kind "signature_error")
       (let ((failure (handler-case (progn (%fixture-signature fixture) nil) (error (c) c))))
         (unless failure (%fixture-fail "signature should fail"))
         (%expect-contains (%failure-text failure) (jget fixture "expected_error_contains") "signature error"))
       (values :validation-error nil))
      ((equal kind "json_schema")
       (%expect-json-equal (json-schema (%fixture-signature fixture) :side :output :title "Schema")
                          (jget fixture "expected_schema") "JSON schema")
       (values :semantic nil))
      ((equal kind "prompt")
       (let ((text (encode-json (render-prompt (%fixture-signature fixture) (jget fixture "input")))))
         (loop for needle across (jget fixture "expected_prompt_contains") do
           (%expect-contains text needle "rendered prompt")))
       (values :semantic nil))
      ((equal kind "date_input")
       (loop for case across (jget fixture "cases") do
         (let ((prompt (render-prompt (%fixture-signature fixture) (%native-dates (jget case "values")))))
           (%expect-json-equal (jget (aref prompt (1- (length prompt))) "content")
                              (jget case "expected_user_content") "native date input")))
       (values :semantic nil))
      ((equal kind "stream") (%run-stream-fixture fixture) (values :semantic nil))
      ((equal kind "date_field_value")
       (%run-date-values-fixture fixture) (values :semantic nil))
      ((equal kind "cache_sequence")
       (%run-cache-fixture fixture) (values :semantic nil))
      ((equal kind "forward")
       (let ((unclaimed (%unclaimed-keys fixture (append +forward-input-keys+
                                                         +forward-expectation-keys+))))
         (cond
           (unclaimed (values :not-claimed (format nil "~{~a~^, ~}" unclaimed)))
           ;; A fixture written against `signature_spec' needs the builder, which
           ;; is a different surface from the signature string this drives.
           ((not (or (%present (jget fixture "signature")) (%present (jget fixture "signature_spec"))))
            (values :not-claimed "no signature string"))
           (t (%run-forward-fixture fixture)
              (if (%present (jget fixture "expected_error_contains"))
                  (values :validation-error nil)
                  (values :semantic nil))))))
      ((equal kind "streaming_forward")
       (let ((unclaimed (%unclaimed-keys fixture (append +forward-input-keys+
                                                         +forward-expectation-keys+
                                                         +streaming-expectation-keys+))))
         (cond
           (unclaimed (values :not-claimed (format nil "~{~a~^, ~}" unclaimed)))
           ((not (or (%present (jget fixture "signature")) (%present (jget fixture "signature_spec"))))
            (values :not-claimed "no signature string"))
           (t (%run-forward-fixture fixture :streaming t)
              (if (%present (jget fixture "expected_error_contains"))
                  (values :validation-error nil)
                  (values :semantic nil))))))
      ((equal kind "program_contract")
       (let ((unclaimed (%unclaimed-keys fixture +program-contract-keys+)))
         (if unclaimed
             (values :not-claimed (format nil "~{~a~^, ~}" unclaimed))
             (progn (%run-program-contract-fixture fixture) (values :semantic nil)))))
      ((assoc kind +deferred-kinds+ :test #'equal)
       (values :not-claimed (cdr (assoc kind +deferred-kinds+ :test #'equal))))
      (t
       ;; Not a kind this runner knows, and not one it has declared it skips.
       (%fixture-fail "unknown fixture kind ~s; add a dispatch arm or declare it deferred"
                      kind)))))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun run-suite (&key (stream *standard-output*) (verbose nil))
  "Run the shared axgen and axprogram fixtures.

Returns (values passed failed not-claimed).  A claimed fixture that fails is a
failure; a fixture this runner does not claim is reported by name and counted
separately, never as a pass."
  (let ((passed 0) (failed 0) (not-claimed 0)
        (classifications '()))
    (dolist (suite '("axgen" "axprogram"))
      (dolist (entry (%load-fixtures suite))
        (destructuring-bind (name fixture file) entry
          (handler-case
              (multiple-value-bind (status detail) (%run-fixture fixture)
                (case status
                  (:not-claimed
                   (incf not-claimed)
                   (push (list name "explicitly-not-claimed" detail) classifications)
                   (when verbose (format stream "skip ~a (~a)~%" name detail)))
                  (:validation-error
                   (incf passed)
                   ;; Only a fixture that ran to completion is recorded, and it is
                   ;; recorded under its file name, so the gate can reconcile the
                   ;; receipt against the directory and catch a run that exited
                   ;; without executing anything.
                   (axllm/conformance:record-result suite file :validation-error)
                   (push (list name "validation-error" nil) classifications)
                   (when verbose (format stream "ok   ~a~%" name)))
                  (t
                   (incf passed)
                   (axllm/conformance:record-result suite file :semantic)
                   (push (list name "semantic" nil) classifications)
                   (when verbose (format stream "ok   ~a~%" name)))))
            (error (condition)
              (incf failed)
              (push (list name "failed" (princ-to-string condition)) classifications)
              (format stream "FAIL ~a~%     ~a~%" name
                      (substitute #\Space #\Newline (princ-to-string condition))))))))
    (format stream "~&axgen/axprogram: ~a passed, ~a failed, ~a not claimed~%"
            passed failed not-claimed)
    (finish-output stream)
    (values passed failed not-claimed (nreverse classifications))))

(defun run-or-die ()
  "Run the suite as a gate.  Returns T, or signals.

A gate has to fail on three different kinds of nothing: a fixture that failed, a
fixture this runner does not claim, and an empty inventory.  The last one matters
most, because a suite that found no fixtures reports no failures and would
otherwise look like a pass.  This signals rather than exiting the image, so an
ASDF run reports it as a failed test operation instead of killing the process."
  (dolist (suite '("axgen" "axprogram"))
    (let ((found (length (%load-fixtures suite))))
      (when (zerop found)
        (error 'fixture-failure
               :text (format nil "no ~a fixtures were found under ~a; the suite checked nothing"
                             suite (%conformance-root))))))
  (multiple-value-bind (passed failed not-claimed) (run-suite)
    (when (zerop passed)
      (error 'fixture-failure :text "no axgen or axprogram fixture passed"))
    (unless (zerop failed)
      (error 'fixture-failure
             :text (format nil "~a axgen/axprogram fixture(s) failed" failed)))
    (unless (zerop not-claimed)
      (error 'fixture-failure
             :text (format nil "~a axgen/axprogram fixture(s) are not claimed; ~
a fixture this port does not run is not a pass" not-claimed)))
    t))

;;; ------------------------------------------------------------------
;;; Public entry points
;;; ------------------------------------------------------------------

(in-package #:axllm)

(export '(run-gen-conformance run-gen-conformance-or-die))

(defun run-gen-conformance (&rest arguments)
  "Run the shared axgen and axprogram fixtures.

Returns (values passed failed not-claimed classifications)."
  (apply #'axllm/gen-conformance::run-suite arguments))

(defun run-gen-conformance-or-die ()
  "Run the shared axgen and axprogram fixtures as a gate.  Returns T, or signals."
  (axllm/gen-conformance::run-or-die))
