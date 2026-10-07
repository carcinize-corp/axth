;;;; ai-conformance.lisp --- the shared axai fixtures, run natively.
;;;;
;;;; Entry points:
;;;;
;;;;   (axllm/ai-conformance:run-ai-conformance-tests)        ; (values passed failed)
;;;;   (axllm/ai-conformance:run-ai-conformance-tests-or-die) ; exits non-zero on failure
;;;;
;;;; The fixtures in ir/conformance/axai are derived from the TypeScript
;;;; reference, so they, not this file, decide what correct means.  Each one is
;;;; run through the real provider client with a scripted transport, and the
;;;; assertions are semantic: the exact request body that went on the wire, the
;;;; exact normalized output, the request count, and the exact error class,
;;;; status and message where the fixture states them.
;;;;
;;;; Coverage is reported, not claimed.  The runner writes
;;;; conformance-coverage.json next to this file, classifying every fixture it
;;;; saw as `semantic', `validation-error', `transport-boundary' or
;;;; `explicitly-not-claimed', so a kind this port does not yet execute shows
;;;; up as unclaimed instead of silently passing.  There is deliberately no
;;;; catch-all arm that lets an unknown kind pass.

(defpackage #:axllm/ai-conformance
  (:use #:cl)
  (:export #:run-ai-conformance-tests #:run-ai-conformance-tests-or-die
           #:conformance-directory #:write-coverage-report))

(in-package #:axllm/ai-conformance)

;;; ------------------------------------------------------------------
;;; Fixture loading
;;; ------------------------------------------------------------------

(defun conformance-directory ()
  "The ir/conformance/axai directory.

AXIR_CONFORMANCE_DIR overrides the tree, so the runner can be pointed at a
different checkout without editing it."
  (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
    (if (and override (plusp (length override)))
        (merge-pathnames "axai/" (uiop:ensure-directory-pathname override))
        (merge-pathnames
         "ir/conformance/axai/"
         (uiop:pathname-parent-directory-pathname
          (uiop:pathname-parent-directory-pathname
           (asdf:system-source-directory "axllm")))))))

(defun load-fixtures ()
  "Every fixture as (pathname . parsed), sorted so a run is deterministic.

The pathname is kept, not just the title: the conformance receipt is keyed by
the fixture's filename on disk, and a fixture whose \"name\" differs from its
filename would be recorded under an id the inventory does not contain."
  (let ((files (sort (directory (merge-pathnames "*.json" (conformance-directory)))
                     #'string< :key #'namestring)))
    (loop for file in files
          for parsed = (handler-case (axllm:parse-json (uiop:read-file-string file))
                         (error (condition)
                           (error "~a is not readable JSON: ~a" file condition)))
          collect (cons file parsed))))

;;; ------------------------------------------------------------------
;;; Assertions
;;; ------------------------------------------------------------------

(define-condition fixture-failed (error)
  ((text :initarg :text :reader fixture-failed-text))
  (:report (lambda (c s) (write-string (fixture-failed-text c) s))))

(defun fail (format-control &rest arguments)
  (error 'fixture-failed :text (apply #'format nil format-control arguments)))

(defun json-equal (actual expected)
  "Whether two JSON values are the same, by Core's own value equality."
  (axllm/core::core-value-equal actual expected))

(defun json-subset-p (actual expected path)
  "Whether ACTUAL matches every key EXPECTED states, recursively.

A fixture's expectation names the fields that must appear with those values;
it does not forbid a field it does not mention.  Returns T, or signals with
the path that disagreed so a failure names the field rather than dumping two
whole objects."
  (cond
    ((and (hash-table-p expected) (hash-table-p actual))
     (dolist (key (axllm::%object-keys expected) t)
       (multiple-value-bind (value found) (gethash key actual)
         (unless found
           (fail "~a.~a is missing; expected ~a" path key
                 (axllm:encode-json (gethash key expected))))
         (json-subset-p value (gethash key expected) (format nil "~a.~a" path key)))))
    ((and (vectorp expected) (not (stringp expected))
          (vectorp actual) (not (stringp actual)))
     (unless (= (length expected) (length actual))
       (fail "~a has ~a element(s); expected ~a~%  actual:   ~a~%  expected: ~a"
             path (length actual) (length expected)
             (axllm:encode-json actual) (axllm:encode-json expected)))
     (loop for index from 0 below (length expected)
           do (json-subset-p (aref actual index) (aref expected index)
                             (format nil "~a[~a]" path index)))
     t)
    ((json-equal actual expected) t)
    (t (fail "~a is ~a; expected ~a" path
             (axllm:encode-json actual) (axllm:encode-json expected)))))

(defun json-path-absent-p (object path)
  "Whether the dotted PATH is absent from OBJECT.

A fixture's expected_transport_json_absent names a field the request must not
carry, which is how the reference states that a setting was dropped rather
than sent with a default."
  (let ((cursor object))
    (dolist (segment (uiop:split-string path :separator "."))
      (let ((index (parse-integer segment :junk-allowed t)))
        (cond
          ((and index (vectorp cursor) (not (stringp cursor)))
           (if (< index (length cursor))
               (setf cursor (aref cursor index))
               (return-from json-path-absent-p t)))
          ((hash-table-p cursor)
           (multiple-value-bind (value found) (gethash segment cursor)
             (if found (setf cursor value) (return-from json-path-absent-p t))))
          (t (return-from json-path-absent-p t)))))
    nil))

;;; ------------------------------------------------------------------
;;; The scripted transport
;;; ------------------------------------------------------------------

(defstruct (wire (:conc-name wire-)) queue (calls '()))

(defun fixture-responses (fixture)
  "The fixture's HTTP envelopes or bare native response bodies."
  (let ((responses (axllm::%present (axllm:jget fixture "transport_responses"))))
    (when responses (coerce responses 'list))))

(defun scripted-wire (responses)
  "Returns (values transport wire).  The transport records every call."
  (let ((wire (make-wire :queue (copy-list responses))))
    (values
     (lambda (url headers body)
       (push (list url headers body axllm::*provider-http-method*) (wire-calls wire))
       (let ((next (if (wire-queue wire)
                       (pop (wire-queue wire))
                       (fail "the provider made more requests than the fixture scripted"))))
         (cond
           ((consp next) (values (cdr next) (car next)))
           ((stringp next) (values next 200))
           (t
            (when (gethash "network_error" next)
              (axllm::provider-fail :network
                                    (format nil "Network Error: ~a" (gethash "network_error" next))
                                    :retryable t))
            (let ((body (axllm:jget next "json" (axllm:jget next "body" next))))
              (values (if (stringp body) body (axllm:encode-json body))
                      (axllm:jget next "status" 200)
                      (axllm:jget next "headers" (axllm:object))))))))
     wire)))

(defun streaming-wire (transport)
  (lambda (url headers request)
    (multiple-value-bind (body status response-headers) (funcall transport url headers request)
      (let ((offset 0) (bytes (sb-ext:string-to-octets body :external-format :utf-8)))
        (values (lambda ()
                  (when (< offset (length bytes))
                    (let ((end (min (length bytes) (+ offset 7))))
                      (prog1 (subseq bytes offset end) (setf offset end)))))
                status (lambda () nil) response-headers body)))))

(defun wire-call (wire n)
  (let ((calls (reverse (wire-calls wire))))
    (unless (< n (length calls))
      (fail "expected at least ~a transport call(s), saw ~a" (1+ n) (length calls)))
    (nth n calls)))

(defun wire-count (wire) (length (wire-calls wire)))

;;; ------------------------------------------------------------------
;;; Building the client a fixture describes
;;; ------------------------------------------------------------------

(defun fixture-option-object (fixture key)
  (let ((value (axllm::%present (axllm:jget fixture key))))
    (and (hash-table-p value) value)))

(defparameter +endpoint-option-keys+
  '("resource_name" "deployment_name" "region" "project_id" "path" "endpoint"
    "primary_index" "routing" "processing")
  "Top-level fixture fields that are really service options.

A deployment profile such as azure-openai or vertex-ai needs its endpoint
settings before Core can resolve a descriptor at all, and the fixtures state
them at the top level rather than inside service_options.")

(defun fixture-service-options (fixture)
  "The service options the fixture configures, including its endpoint fields."
  (let ((options (axllm::%new-object)))
    (let ((declared (fixture-option-object fixture "service_options")))
      (when declared
        (dolist (key (axllm::%object-keys declared))
          (axllm::%set-key options key (gethash key declared)))))
    (dolist (key +endpoint-option-keys+)
      (let ((value (axllm::%present (axllm:jget fixture key))))
        (when value
          (axllm::%set-key options key value)
          ;; Core reads both spellings, and a fixture states only one.
          (axllm::%set-key options (axllm/core::core-string-lower-camel
                                   (axllm/core::core-string-split key "_")) value))))
    options))

(defvar *credential-requests* nil)

(defun fixture-credential-provider (fixture)
  "The credential callback the fixture asks for, if any.

A fixture that states an error makes the callback fail, which is how the
reference checks that a credential failure is reported rather than becoming an
unauthenticated request."
  (let ((spec (fixture-option-object fixture "credential_provider_fixture")))
    (when spec
      (let ((failure (axllm::%present-string (axllm:jget spec "error")))
            (headers (axllm:jget spec "headers" #())) (index 0))
        (lambda (context)
          (push context *credential-requests*)
          (when failure (error failure))
          (if (hash-table-p headers) headers
              (prog1 (if (plusp (length headers)) (aref headers (min index (1- (length headers)))) (axllm:object))
                (incf index))))))))

(defun build-client (fixture transport &key streaming-transport)
  "A provider client configured exactly as the fixture states."
  (let ((profile (axllm::%present-string (axllm:jget fixture "provider")))
        ;; A fixture that sets no_api_key is checking what happens with only
        ;; the environment, so it must not be handed a key.
        (no-key (axllm/core::core-true-p (axllm:jget fixture "no_api_key" 'yason:false))))
    (axllm::provider
     :profile profile
     :model (axllm::%present-string (axllm:jget fixture "model"))
     :embed-model (axllm::%present-string (axllm:jget fixture "embed_model"))
     :api-key (cond ((axllm::%present-string (axllm:jget fixture "api_key")))
                    (no-key nil)
                    (t "test-key"))
     :base-url (axllm::%present-string (axllm:jget fixture "base_url"))
     :api-version (axllm::%present-string (axllm:jget fixture "api_version"))
     :options (fixture-service-options fixture)
     :model-config (fixture-option-object fixture "model_config")
     :transport transport
     :streaming-transport (or streaming-transport (and transport (streaming-wire transport)))
     :credential-provider (fixture-credential-provider fixture))))

(defmacro with-fixture-environment ((fixture) &body body)
  "Run BODY with the fixture's env entries set, restoring them afterwards.

A null value means the variable must be unset, which is how a fixture checks
that one provider's key is not read for another."
  (let ((f (gensym)) (saved (gensym)) (name (gensym)) (value (gensym)) (entries (gensym)))
    `(let* ((,f ,fixture)
            (,entries (fixture-option-object ,f "env"))
            (,saved '()))
       (when ,entries
         (dolist (,name (axllm::%object-keys ,entries))
           (push (cons ,name (uiop:getenv ,name)) ,saved)
           (let ((,value (axllm::%present (gethash ,name ,entries))))
             (if (stringp ,value)
                 (sb-posix:setenv ,name ,value 1)
                 (sb-posix:unsetenv ,name)))))
       (unwind-protect (progn ,@body)
         (dolist (,name ,saved)
           (if (cdr ,name)
               (sb-posix:setenv (car ,name) (cdr ,name) 1)
               (sb-posix:unsetenv (car ,name))))))))

(defun fixture-request (fixture)
  (or (fixture-option-object fixture "request")
      (let ((text (axllm::%present-string (axllm:jget fixture "request_json"))))
        (and text (axllm:parse-json text)))
      (axllm:object)))

;;; ------------------------------------------------------------------
;;; Shared assertions over a recorded call
;;; ------------------------------------------------------------------

(defun check-transport-request (fixture wire)
  "Compare the recorded request with the fixture's expectation."
  (let ((expected (fixture-option-object fixture "expected_transport_request")))
    (when expected
      (destructuring-bind (url headers body &optional (method "POST")) (wire-call wire 0)
        (let ((expected-url (axllm::%present-string (axllm:jget expected "url"))))
          (when expected-url
            (unless (equal url expected-url)
              (fail "request URL is ~s; expected ~s" url expected-url))))
        (let ((expected-method (axllm::%present-string (axllm:jget expected "method"))))
          (when (and expected-method (not (equal expected-method method)))
            (fail "Request method ~a, expected ~a" method expected-method)))
        (let ((expected-headers (fixture-option-object expected "headers")))
          (when expected-headers
            (dolist (name (axllm::%object-keys expected-headers))
              (let ((actual (cdr (assoc name headers :test #'string-equal)))
                    (wanted (axllm/core::core-js-text (gethash name expected-headers))))
                (unless (equal actual wanted)
                  (fail "header ~a is ~s; expected ~s" name actual wanted))))))
        (let ((expected-json (axllm::%present (axllm:jget expected "json" (axllm:jget expected "data")))))
          (when (hash-table-p expected-json)
            (json-subset-p (axllm:parse-json body) expected-json "request")))))
    (let ((absent (axllm::%present (axllm:jget fixture "expected_transport_json_absent"))))
      (when (and absent (plusp (length absent)))
        (let ((sent (axllm:parse-json (third (wire-call wire 0)))))
          (map nil (lambda (path)
                     (unless (json-path-absent-p sent path)
                       (fail "request still carries ~a, which the fixture says is dropped"
                             path)))
               absent))))
    (let ((contains (axllm::%present (axllm:jget fixture "expected_transport_wire_json_contains"))))
      (when (and contains (plusp (length contains)))
        (let ((text (third (wire-call wire 0))))
          (map nil (lambda (needle)
                     (unless (search needle text)
                       (fail "request wire JSON does not contain ~s" needle)))
               contains))))
    (loop for expected across (axllm:jget fixture "expected_transport_requests" #()) for index from 0
          do (check-transport-request (axllm:object "expected_transport_request" expected)
                                      (make-wire :calls (list (wire-call wire index)))))
    (let ((count (axllm::%present (axllm:jget fixture "expected_transport_request_count"))))
      (when (integerp count)
        (unless (= (wire-count wire) count)
          (fail "the provider made ~a request(s); the fixture expects ~a"
                (wire-count wire) count))))))

(defun check-output (fixture actual)
  (let ((expected (fixture-option-object fixture "expected_output")))
    (when expected
      (json-subset-p actual expected "output"))))

(defun check-error (fixture condition)
  "Compare a signalled condition with the fixture's error expectation."
  (when (gethash "expected_error_request" fixture)
    (let ((actual (and (typep condition 'axllm:provider-error) (axllm::provider-error-request condition)))
          (expected (gethash "expected_error_request" fixture)))
      (unless (and (hash-table-p actual) (= (hash-table-count actual) (hash-table-count expected)))
        (fail "Error request keys differ: ~a" actual))
      (json-subset-p actual expected "error request")))
  (let ((expected-type (axllm::%present-string (axllm:jget fixture "expected_error_type")))
        (expected-text (axllm::%present-string (axllm:jget fixture "expected_error_contains")))
        (expected-status (axllm::%present (axllm:jget fixture "expected_status")))
        (excludes (axllm::%present (axllm:jget fixture "expected_error_excludes"))))
    (when expected-text
      (unless (search expected-text (princ-to-string condition))
        (fail "the failure is ~s; it does not contain ~s"
              (princ-to-string condition) expected-text)))
    (when excludes
      (map nil (lambda (needle)
                 (when (search needle (format nil "~a ~a ~a" condition
                                               (if (typep condition 'axllm:provider-error) (axllm:encode-json (axllm::provider-error-response-body condition)) "")
                                               (if (typep condition 'axllm:provider-error) (axllm:encode-json (axllm::provider-error-request condition)) "")))
                   (fail "the failure must not mention ~s" needle)))
           excludes))
    (when (integerp expected-status)
      (let ((actual (and (typep condition 'axllm:provider-error)
                         (axllm:provider-error-status condition))))
        (unless (eql actual expected-status)
          (fail "the failure reports status ~a; expected ~a" actual expected-status))))
    (when expected-type
      (let ((kind (and (typep condition 'axllm:provider-error)
                       (axllm:provider-error-kind condition))))
        (unless (equal (expected-kind-for expected-type) kind)
          (fail "the failure is kind ~a; the fixture's ~a maps to ~a"
                kind expected-type (expected-kind-for expected-type)))))))

(defparameter +error-type-kinds+
  '(("AxAIServiceStatusError" . :status)
    ("AxAIServiceAuthenticationError" . :auth)
    ("AxAIServiceNetworkError" . :network)
    ("AxAIServiceTimeoutError" . :timeout)
    ("AxAIServiceResponseError" . :response)
    ("AxAIServiceStreamTerminatedError" . :stream)
    ("AxAIServiceAbortedError" . :aborted)
    ("AxAIRefusalError" . :refusal)
    ("AxUnsupportedCapabilityError" . :unsupported))
  "The reference's error classes and the provider-error kind each maps to.")

(defun expected-kind-for (type)
  (or (cdr (assoc type +error-type-kinds+ :test #'string=))
      (fail "the fixture expects error class ~a, which this port does not map" type)))

;;; ------------------------------------------------------------------
;;; Fixture kinds
;;;
;;; Dispatch is explicit. A kind with no arm is reported as not claimed; it
;;; never passes by falling through.
;;; ------------------------------------------------------------------

(defun fixture-expects-failure-p (fixture)
  "Whether the fixture states that the call must fail.

A kind such as ai_chat still carries an error expectation when the reference
rejects the request, for example a forced tool choice a profile cannot
express, so the arm has to read the expectation rather than the kind."
  (or (axllm::%present-string (axllm:jget fixture "expected_error_type"))
      (axllm::%present-string (axllm:jget fixture "expected_error_contains"))))

(defun run-expected-failure (fixture runner-name wire thunk)
  "Run THUNK and require the failure the fixture states."
  (handler-case
      (let ((output (funcall thunk)))
        (fail "~a succeeded with ~a; the fixture expects a failure"
              runner-name (axllm:encode-json output)))
    (fixture-failed (condition) (error condition))
    (error (condition)
      (check-error fixture condition)
      (check-transport-request fixture wire)
      (values :validation-error condition))))

(defun run-ai-chat (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let ((client (build-client fixture transport)))
      (if (fixture-expects-failure-p fixture)
          (run-expected-failure
           fixture "chat" wire
           (lambda () (axllm::ax-chat client (fixture-request fixture)
                                      (fixture-option-object fixture "options"))))
          (let ((output (axllm::ax-chat client (fixture-request fixture)
                                        (fixture-option-object fixture "options"))))
            (check-transport-request fixture wire)
            (check-output fixture output)
            (when (gethash "expected_estimated_cost" fixture)
              (let ((cost (axllm:ax-estimated-cost client (axllm:jget output "model_usage" (axllm:jget output "modelUsage")))))
                (unless (< (abs (- cost (gethash "expected_estimated_cost" fixture))) 1d-12)
                  (fail "Estimated cost ~a, expected ~a" cost (gethash "expected_estimated_cost" fixture)))))
            (values :semantic output))))))

(defun run-request-validation (fixture)
  "A fixture with no provider checks Core's provider-neutral request
validation, not a provider call."
  (handler-case
      (progn (axllm/core::validate-chat-request (fixture-request fixture))
             (fail "the request validated; the fixture expects a failure"))
    (fixture-failed (condition) (error condition))
    (error (condition)
      (check-error fixture condition)
      (values :validation-error condition))))

(defun run-ai-error (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let ((client (build-client fixture transport)))
      (handler-case
          (let ((output (funcall
                         (cond ((equal (axllm:jget fixture "method") "speak") #'axllm:ax-speak)
                               ((equal (axllm:jget fixture "method") "transcribe") #'axllm:ax-transcribe)
                               ((equal (axllm:jget fixture "method") "embed") #'axllm:ax-embed)
                               (t #'axllm:ax-chat))
                         client (fixture-request fixture)
                         (fixture-option-object fixture "options"))))
            (fail "the request succeeded with ~a; the fixture expects a failure"
                  (axllm:encode-json output)))
        (fixture-failed (condition) (error condition))
        (error (condition)
          (check-error fixture condition)
          (check-transport-request fixture wire)
          (values :validation-error condition))))))

(defun run-ai-embed (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let* ((client (build-client fixture transport))
           (output (axllm::ax-embed client (fixture-request fixture)
                                    (fixture-option-object fixture "options"))))
      (check-transport-request fixture wire)
      (check-output fixture output)
      (values :semantic output))))

(defun run-ai-transcribe (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let* ((client (build-client fixture transport))
           (output (axllm::ax-transcribe client (fixture-request fixture)
                                         (fixture-option-object fixture "options"))))
      (check-transport-request fixture wire)
      (check-output fixture output)
      (values :semantic output))))

(defun run-ai-speak (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let* ((client (build-client fixture transport))
           (output (axllm::ax-speak client (fixture-request fixture)
                                    (fixture-option-object fixture "options"))))
      (check-transport-request fixture wire)
      (check-output fixture output)
      (values :semantic output))))

(defun fixture-stream-body (fixture)
  "The raw server-sent-events text the fixture scripts."
  (let ((responses (axllm::%present (axllm:jget fixture "transport_responses"))))
    (unless (and responses (plusp (length responses)))
      (fail "the fixture scripts no streamed response"))
    (let* ((first (aref responses 0))
           (body (axllm::%present (axllm:jget first "body")))
           (json (axllm::%present (axllm:jget first "json"))))
      (values (cond ((stringp body) body)
                    ((stringp json) json)
                    (json (axllm:encode-json json))
                    (t (fail "the scripted streamed response has no body")))
              (or (axllm::%present (axllm:jget first "status")) 200)))))

(defparameter +stream-chunk-bytes+ 7
  "How many bytes the scripted streaming transport hands over at a time.

Deliberately small and not a line length: a provider's chunk boundary falls
wherever the network puts it, so feeding the body in small slices exercises
the decoder's line splitting and its incremental UTF-8 across boundaries. A
single-blob feed would pass even with both of those broken.")

(defun scripted-streaming-transport (body status)
  "Returns (values transport wire) for a streamed response.

The transport answers the body in small byte slices, like a real socket."
  (let ((wire (make-wire)))
    (values
     (lambda (url headers request-body)
       (push (list url headers request-body) (wire-calls wire))
       (let* ((bytes (sb-ext:string-to-octets body :external-format :utf-8))
              (offset 0))
         (values
          (lambda ()
            (when (< offset (length bytes))
              (let ((end (min (length bytes) (+ offset +stream-chunk-bytes+))))
                (prog1 (subseq bytes offset end)
                  (setf offset end)))))
          status
          (lambda () nil))))
     wire)))

(defun run-ai-stream (fixture)
  "Run a streamed turn and compare every normalized chunk, in order."
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
      (let ((client (build-client fixture transport)))
        (if (fixture-expects-failure-p fixture)
            (run-expected-failure
             fixture "stream" wire
             (lambda ()
               (axllm::%collect-stream-chunks
                (axllm::ax-stream client (fixture-request fixture)
                                  (fixture-option-object fixture "options")))))
            (let ((chunks (axllm::%collect-stream-chunks
                           (axllm::ax-stream client (fixture-request fixture)
                                             (fixture-option-object fixture "options")))))
              (check-transport-request fixture wire)
              (let ((expected (axllm::%present (axllm:jget fixture "expected_output"))))
                (cond
                  ((and expected (vectorp expected) (not (stringp expected)))
                   (json-subset-p chunks expected "chunks"))
                  ((hash-table-p expected)
                   ;; A fixture that states one object expects the folded
                   ;; response, not the chunk sequence.
                   (json-subset-p (axllm/core::fold-chat-response-stream chunks)
                                  expected "folded"))))
              (values :semantic chunks))))))

(defun run-ai-realtime (fixture)
  "A realtime fixture, run against Core without opening a socket.

Realtime is a request builder, an input builder and an event normalizer, all
Core's, plus a URL.  None of that needs a WebSocket, so these fixtures are
exercised with no optional dependency at all: the transport only matters for a
live provider, and a fixture scripts its events.

Every expectation the fixture states is checked, and at least one must be
present or the fixture would pass without asserting anything."
  (let* ((profile (axllm::%present-string (axllm:jget fixture "provider")))
         (model (or (axllm::%present-string (axllm:jget fixture "model"))
                    (axllm::%present-string (axllm:jget (axllm/core::provider-descriptor profile) "defaultModel")) ""))
         (request (fixture-request fixture))
         (options (fixture-option-object fixture "options"))
         (checked 0))
    (flet ((expectation (key) (axllm::%present (axllm:jget fixture key))))
      ;; A fixture that expects a refusal states it against whichever builder
      ;; it also expects, so the error arms run first.
      (let ((failure (axllm::%present-string (axllm:jget fixture "expected_error_contains"))))
        (cond
          (failure
           (handler-case
               (progn
                 (when (expectation "expected_setup")
                   (axllm/core::provider-build-realtime-audio-setup
                    profile request (or options :null)))
                 (when (expectation "expected_input")
                   (axllm/core::provider-build-realtime-audio-input profile request))
                 (fail "the realtime request was built; the fixture expects a failure"))
             (fixture-failed (condition) (error condition))
             (error (condition)
               (check-error fixture condition)
               (incf checked)
               (return-from run-ai-realtime (values :validation-error condition)))))
          (t
           (let ((expected-setup (expectation "expected_setup")))
             (when expected-setup
               (json-subset-p (axllm/core::provider-build-realtime-audio-setup
                               profile request (or options :null))
                              expected-setup "setup")
               (incf checked)))
           (let ((expected-input (expectation "expected_input")))
             (when expected-input
               (json-subset-p (axllm/core::provider-build-realtime-audio-input profile request)
                              expected-input "input")
               (incf checked)))
           (let ((expected-url (axllm::%present-string (axllm:jget fixture "expected_ws_url"))))
             (when expected-url
               ;; Core answers {url, headers}: the credential may belong in
               ;; either, and reading only a bare string would miss a provider
               ;; that puts the key in a header instead of the query.
               (let* ((resolved (axllm/core::provider-realtime-ws-url
                                 profile model
                                 (or (axllm::%present-string (axllm:jget fixture "api_key"))
                                     "test-key")
                                 (or (fixture-service-options fixture) :null)))
                      (actual (if (hash-table-p resolved)
                                  (axllm::%present-string (axllm:jget resolved "url"))
                                  resolved)))
                 (unless (equal actual expected-url)
                   (fail "realtime URL is ~s; expected ~s" actual expected-url))
                 (incf checked))))
           (let ((events (expectation "events"))
                 (expected-output (expectation "expected_output")))
             (when (and events expected-output)
               ;; One shared state object across the stream, as a live session
               ;; would carry: a per-event state would lose the turn's
               ;; accumulated transcript and silently pass the first event only.
               (let ((state (axllm::%new-object))
                     (produced (axllm::%new-array)))
                 (map nil (lambda (event)
                            (let ((chunk (axllm/core::provider-normalize-realtime-event
                                          profile event state profile model)))
                              (unless (eq chunk :null)
                                (vector-push-extend chunk produced))))
                      events)
                 (json-subset-p produced expected-output "events")
                 (incf checked))))))
        (when (zerop checked)
          (fail "the fixture states no realtime expectation this runner checks"))
        (values :semantic checked)))))

(defun run-model-catalog-runtime (fixture)
  "The model catalogue, narrowed by the fixture's model type.

CHECK-CLONE matters: the reference hands back a copy, so a caller that mutates
the catalogue must not corrupt the next caller's view.  A shared structure
would pass an equality check and still be wrong."
  (let* ((model-type (axllm::%present-string (axllm:jget fixture "model_type")))
         (catalog (axllm::supported-ai-models model-type)))
    (let* ((openai (find "openai" catalog :test #'equal
                         :key (lambda (entry) (axllm:jget entry "name"))))
           (models (if openai (axllm:jget openai "models") #()))
           (types (sort (remove-duplicates
                         (map 'list (lambda (model) (axllm:jget model "type")) models)
                         :test #'equal) #'string<)))
      (check-output fixture
                    (axllm:object "catalog" catalog "providerCount" (length catalog)
                                  "providerNames" (map 'vector (lambda (p) (axllm:jget p "name")) catalog)
                                  "modelCount" (loop for p across catalog sum (length (axllm:jget p "models")))
                                  "openaiFirstModel" (if (plusp (length models)) (axllm:jget (aref models 0) "name") :null)
                                  "openaiModelTypes" (coerce types 'vector))))
    (when (axllm/core::core-true-p (axllm:jget fixture "check_clone" 'yason:false))
      (let ((entries catalog))
        (unless (and entries (vectorp entries) (plusp (length entries)))
          (fail "the catalogue is empty, so the clone check would prove nothing"))
        ;; Mutate the copy we were given, then ask again.
        (axllm::%set-key (aref entries 0) "displayName" "mutated by the clone check")
        (let* ((again-entries (axllm::supported-ai-models model-type)))
          (when (equal (axllm::%present-string
                        (axllm:jget (aref again-entries 0) "displayName"))
                       "mutated by the clone check")
            (fail "the catalogue is shared, not copied: mutating one caller's ~
copy changed the next caller's view")))))
    (values :semantic catalog)))

(defun run-model-catalog-audit (fixture)
  (let ((summary (axllm::model-catalog-summary)))
    (check-output fixture summary)
    (values :semantic summary)))

(defun run-provider-registry (fixture)
  "The profile registry, plus every alias the fixture states.

The alias table is the part worth asserting: a registry that listed the right
profiles but resolved \"claude\" or \"azure\" to the wrong one would still look
correct."
  (let ((registry (axllm/core::provider-profile-registry))
        (aliases (fixture-option-object fixture "alias_expectations")))
    (check-output fixture registry)
    (when aliases
      (dolist (alias (axllm::%object-keys aliases))
        (let ((expected (axllm/core::core-js-text (gethash alias aliases)))
              (actual (axllm/core::provider-normalize-profile alias)))
          (unless (equal actual expected)
            (fail "alias ~s resolves to ~s; expected ~s" alias actual expected)))))
    (values :semantic registry)))

(defun run-ai-unsupported (fixture)
  "An operation a provider cannot express has to be refused, not attempted.

The transport is deliberately a function that fails if it is ever called, so a
fixture that reached the wire instead of being refused fails loudly rather than
passing on the error message alone."
  (let* ((method (axllm::%present-string (axllm:jget fixture "method")))
         (reached (list nil))
         (transport (lambda (url headers body)
                      (declare (ignore url headers body))
                      (setf (first reached) t)
                      (values "{}" 200)))
         (client (build-client fixture transport))
         (request (fixture-request fixture))
         (options (fixture-option-object fixture "options")))
    (handler-case
        (progn
          (cond ((equal method "transcribe") (axllm::ax-transcribe client request options))
                ((equal method "speak") (axllm::ax-speak client request options))
                ((equal method "embed") (axllm::ax-embed client request options))
                ((or (null method) (equal method "chat"))
                 (axllm::ax-chat client request options))
                (t (fail "the fixture names method ~s, which this runner does not drive"
                         method)))
          (fail "~a succeeded; the fixture expects a refusal" (or method "chat")))
      (fixture-failed (condition) (error condition))
      (error (condition)
        (check-error fixture condition)
        (when (first reached)
          (fail "the request reached the transport before being refused"))
        (values :validation-error condition)))))

(defun fixture-service-features (fixture)
  "The feature maps the fixture's scripted services advertise, in order."
  (let ((services (axllm::%present (axllm:jget fixture "services"))))
    (unless (and services (vectorp services) (not (stringp services)))
      (fail "the fixture scripts no services"))
    (map 'vector
         (lambda (service)
           (let ((entry (axllm::%new-object)))
             (axllm::%set-key entry "features"
                              (or (axllm::%present (axllm:jget service "features"))
                                  (axllm::%new-object)))
             (dolist (key (list "name" "model" "provider" "id"))
               (let ((value (axllm::%present (axllm:jget service key))))
                 (when value (axllm::%set-key entry key value))))
             entry))
         services)))

(defclass fixture-service ()
  ((spec :initarg :spec :reader service-spec)
   (queue :initarg :queue :accessor service-queue)
   (calls :initform (axllm::%new-array) :reader service-calls)
   (request :initform nil :accessor service-request)
   (options :initform (axllm:object) :accessor service-options)
   (last-chat :initform :null :accessor service-last-chat)
   (last-embed :initform :null :accessor service-last-embed)
   (last-config :initform :null :accessor service-last-config)))
(defun fixture-services (fixture)
  (map 'vector (lambda (spec)
                 (make-instance 'fixture-service :spec spec :queue (coerce (axllm:jget spec "responses" #()) 'list)))
       (axllm:jget fixture "services")))
(defmethod axllm:ax-service-name ((service fixture-service)) (axllm:jget (service-spec service) "name" "fixture"))
(defmethod axllm:ax-id ((service fixture-service))
  (axllm:jget (service-spec service) "id" (concatenate 'string (axllm:ax-service-name service) "-id")))
(defmethod axllm:ax-features ((service fixture-service) &optional model)
  (declare (ignore model)) (axllm:jget (service-spec service) "features" (axllm:object)))
(defmethod axllm::ax-model-list ((service fixture-service)) (axllm:jget (service-spec service) "modelList"))
(defmethod axllm::ax-last-chat-model ((service fixture-service)) (service-last-chat service))
(defmethod axllm::ax-last-embed-model ((service fixture-service)) (service-last-embed service))
(defmethod axllm::ax-last-model-config ((service fixture-service)) (service-last-config service))
(defmethod axllm:ax-options ((service fixture-service)) (service-options service))
(defmethod (setf axllm:ax-options) (options (service fixture-service)) (setf (service-options service) options))
(defmethod axllm:ax-metrics ((service fixture-service))
  (axllm:jget (service-spec service) "metrics" (axllm:object "service" (axllm:ax-service-name service) "calls" (length (service-calls service)))))
(defmethod axllm:ax-estimated-cost ((service fixture-service) &optional usage)
  (declare (ignore usage)) (axllm:jget (service-spec service) "estimatedCost" 0))
(defmethod axllm::ax-validate-request ((service fixture-service) request)
  (when (equal (axllm:ax-service-name service) "Typesafe")
    (axllm/core::provider-validate-chat-request "typesafe" request (axllm:object))))
(defmethod axllm:ax-chat ((service fixture-service) request &optional options)
  (let* ((resolved (axllm/core::resolve-model-key
                    (service-options service) request (or options :null)
                    (axllm:jget (service-spec service) "model" "fixture-chat") 'yason:false))
         (request (axllm:jget resolved "request"))
         (options (axllm:jget resolved "options")))
    (setf (service-request service) request
          (service-last-chat service) (axllm:jget request "model" (axllm:jget (service-spec service) "model" "fixture-chat"))
          (service-last-config service) (axllm/core::merge-model-config (axllm:object)
                                         (axllm:jget request "model_config" (axllm:jget request "modelConfig")) options))
    (vector-push-extend (axllm:object "method" "chat" "opt" options) (service-calls service))
    (if (service-queue service)
        (let* ((response (pop (service-queue service))) (error-spec (gethash "error" response)))
          (when error-spec
            (axllm::provider-fail (intern (string-upcase (axllm:jget error-spec "type")) :keyword)
                                  (axllm:jget error-spec "message") :status (axllm::%present (axllm:jget error-spec "status"))
                                  :retryable (axllm/core::core-true-p (axllm/core::is-retryable-status (axllm:jget error-spec "status" 500)))))
          (axllm:jget response "response" response))
        (axllm:object "results" (vector (axllm:object "index" 0 "content" (format nil "~a chat" (axllm:ax-service-name service))))))))
(defmethod axllm:ax-embed ((service fixture-service) request &optional options)
  (setf (service-last-embed service) (axllm:jget request "embed_model" (axllm:jget request "embedModel" (axllm:jget (service-spec service) "embed_model" "fixture-embed"))))
  (vector-push-extend (axllm:object "method" "embed" "opt" (or options (axllm:object))) (service-calls service))
  (axllm:object "embeddings" #(#(1 2)) "modelUsage" (axllm:object "ai" (axllm:ax-service-name service))))
(defmethod axllm:ax-transcribe ((service fixture-service) request &optional options)
  (declare (ignore request))
  (vector-push-extend (axllm:object "method" "transcribe" "opt" (or options (axllm:object))) (service-calls service))
  (axllm:object "text" (format nil "~a transcript" (axllm:ax-service-name service))))
(defmethod axllm:ax-speak ((service fixture-service) request &optional options)
  (declare (ignore request))
  (vector-push-extend (axllm:object "method" "speak" "opt" (or options (axllm:object))) (service-calls service))
  (axllm:object "audio" "pcm"))

(defun run-provider-router (fixture)
  (let* ((services (fixture-services fixture))
         (router (axllm::provider-router (aref services (axllm:jget fixture "primary_index" 0))
                                         (map 'list (lambda (i) (aref services i)) (axllm:jget fixture "alternative_indices" #()))
                                         :routing (axllm:jget fixture "routing" (axllm:object))
                                         :processing (axllm:jget fixture "processing" (axllm:object))))
         (request (fixture-request fixture))
         (recommendation (axllm::provider-routing-recommendation router request))
         (actual (axllm:object "recommendation" recommendation
                               "validation" (axllm::provider-routing-validation router request)
                               "stats" (axllm::provider-routing-stats router))))
    (setf (gethash "provider" recommendation) (axllm:jget recommendation "providerName"))
    (when (gethash "forwardedContent" (axllm:jget fixture "expected_output"))
      (axllm:ax-chat router request)
      (let ((sent (service-request (axllm::routing-current router))))
        (setf (gethash "forwardedContent" actual)
              (axllm:jget (aref (axllm:jget sent "chatPrompt" (axllm:jget sent "chat_prompt")) 0) "content"))))
    (check-output fixture actual)
    (values :semantic actual)))

(defun run-balancer (fixture)
  (let ((services (fixture-services fixture))
        (best-effort (axllm:object "storeGets" 0 "storeObserves" 0 "eventCalls" 0))
        (configuration (axllm::copy-runtime-options (axllm:jget fixture "options" (axllm:object)))))
    (when (axllm/core::core-true-p (axllm:jget fixture "adaptive_best_effort"))
      (let ((strategy (axllm:jget configuration "strategy")))
        (setf (gethash "routeKey" strategy) (lambda (service index) (declare (ignore service index)) "best-effort-route")
              (gethash "statsStore" strategy)
              (axllm:object "get" (lambda (key) (declare (ignore key))
                                   (incf (gethash "storeGets" best-effort)) (error "store read failed"))
                            "observe" (lambda (key observation) (declare (ignore key observation))
                                       (incf (gethash "storeObserves" best-effort)) (error "store write failed")))
              (gethash "onRoutingEvent" strategy)
              (lambda (event) (declare (ignore event))
                (incf (gethash "eventCalls" best-effort)) (error "event hook failed")))))
    (handler-case
        (let ((balancer (axllm::balancer services configuration))
              (outputs (axllm:object)))
          (loop for op across (axllm:jget fixture "operations")
                for name = (axllm:jget op "name")
                for request = (axllm:jget op "request" (axllm:object))
                for options = (axllm:jget op "options" (axllm:object))
                do (cond
                     ((equal name "set_options") (setf (axllm:ax-options balancer) options))
                     ((equal name "chat") (setf (gethash name outputs) (axllm:ax-chat balancer request options)))
                     ((equal name "stream") (setf (gethash name outputs) (axllm::%collect-stream-chunks (axllm:ax-stream balancer request options))))
                     ((equal name "embed") (setf (gethash name outputs) (axllm:ax-embed balancer request options)))
                     ((equal name "transcribe") (setf (gethash name outputs) (axllm:ax-transcribe balancer request options)))
                     ((equal name "speak") (setf (gethash name outputs) (axllm:ax-speak balancer request options)))
                     ((equal name "adaptive_store")
                      (let ((store (axllm::balancer-stats-store)))
                        (loop for write across (axllm:jget op "writes")
                              do (axllm::balancer-store-observe store (axllm:jget write "key") (axllm:jget write "observation")))
                        (setf (gethash name outputs)
                              (axllm:object "states" (map 'vector (lambda (key) (axllm::balancer-store-get store key))
                                                         (axllm:jget op "reads"))))))
                     ((equal name "adaptive_stats")
                      (let ((stats (axllm::balancer-route-stats))
                            (axllm/core::*math-random-values* (coerce (axllm:jget op "random_values") 'list)))
                        (loop for observation across (axllm:jget op "observations")
                              do (setf stats (axllm::balancer-observe-route stats observation)))
                        (let* ((health (axllm::balancer-sample-health stats (axllm:jget op "deadline_ms")))
                               (score (axllm::balancer-adaptive-score (axllm:jget op "estimated_cost")
                                         (axllm:jget op "bad_outcome_cost") (axllm:jget health "failureProbability")
                                         (axllm:jget health "deadlineMissProbability"))))
                          (flet ((rounded (value) (/ (round (* value 1d9)) 1d9)))
                            (dolist (key '("failureEwma" "logLatencyMean" "logLatencyM2"))
                              (setf (gethash key stats) (rounded (gethash key stats))))
                            (dolist (key (axllm::%object-keys health))
                              (setf (gethash key health) (rounded (gethash key health))))
                            (setf (gethash name outputs) (axllm:object "stats" stats "health" health "score" (rounded score)))))))
                     (t (fail "Unsupported balancer operation ~a" name))))
          (when (fixture-expects-failure-p fixture) (fail "Balancer did not reject operation"))
          (let ((actual (axllm:object "outputs" outputs "id" (axllm:ax-id balancer) "name" (axllm:ax-service-name balancer)
                                     "lastChat" (axllm::ax-last-chat-model balancer) "lastEmbed" (axllm::ax-last-embed-model balancer)
                                     "lastConfig" (axllm::ax-last-model-config balancer) "options" (axllm:ax-options balancer)
                                     "metrics" (axllm:ax-metrics balancer) "modelList" (axllm::ax-model-list balancer)
                                     "serviceCalls" (coerce (loop for s across services when (plusp (length (service-calls s))) collect (service-calls s)) 'vector))))
            (when (gethash "features" (axllm:jget fixture "expected_output"))
              (setf (gethash "features" actual) (axllm:ax-features balancer)))
            (when (axllm/core::core-true-p (axllm:jget fixture "adaptive_best_effort"))
              (setf (gethash "bestEffort" actual) best-effort))
            (check-output fixture actual)
            (values :semantic actual)))
      (fixture-failed (c) (error c))
      (error (c)
        (unless (fixture-expects-failure-p fixture) (error c))
        (check-error fixture c)
        (values :validation-error c)))))

(defun run-provider-descriptor (fixture)
  "A descriptor fixture asserts Core's own descriptor, reached the way the
client reaches it."
  (let* ((profile (axllm::%present-string (axllm:jget fixture "provider")))
         (descriptor (if (gethash "options" fixture)
                         (axllm/core::provider-resolve-descriptor profile (gethash "options" fixture))
                         (axllm/core::provider-descriptor profile))))
    (check-output fixture descriptor)
    (values :semantic descriptor)))

(defun run-provider-features (fixture)
  (let* ((profile (axllm::%present-string (axllm:jget fixture "provider")))
         (features (axllm/core::provider-resolve-features
                    profile
                    (or (axllm::%present-string (axllm:jget fixture "model")) "")
                    (or (fixture-option-object fixture "service_options") (axllm:object)))))
    (check-output fixture features)
    (values :semantic features)))

(defun assert-json-equal (actual expected label)
  (unless (json-equal actual expected)
    (fail "~a: actual ~a, expected ~a" label (axllm:encode-json actual) (axllm:encode-json expected))))

(defun run-typesafe-native (fixture)
  (multiple-value-bind (transport wire)
      (scripted-wire (list (cons 200 (axllm:encode-json (axllm:jget fixture "response")))))
    (let ((client (axllm:provider :profile "typesafe" :api-key "test-key" :transport transport)))
      (flet ((invoke ()
               (if (equal (axllm:jget fixture "operation") "models")
                   (axllm::typesafe-list-models client)
                   (axllm::typesafe-system-one client (fixture-request fixture)))))
        (if (fixture-expects-failure-p fixture)
            (run-expected-failure fixture "system-one" wire #'invoke)
            (let ((out (invoke)))
              (assert-json-equal out (axllm:jget fixture "expected_output") "system-one")
              (check-transport-request fixture wire)
              (values :semantic out)))))))

(defun run-session-state (fixture)
  (loop for item across (axllm:jget fixture "validation_cases" #())
        for index from 0
        do (let ((valid (handler-case
                            (progn (axllm::chat-session-validate-arguments
                                    (axllm:jget item "schema") (axllm:jget item "arguments"))
                                   'yason:true)
                          (axllm:ax-error () 'yason:false))))
             (assert-json-equal valid (axllm:jget item "valid") (format nil "argument validity case ~d" index))
             (when (gethash "errors" item)
               (assert-json-equal
                (axllm::chat-session-argument-errors (axllm:jget item "schema") (axllm:jget item "arguments"))
                (axllm:jget item "errors") "argument errors"))))
  (let ((session (axllm::chat-session-state (axllm:jget fixture "model")
                                           (axllm:jget fixture "path") (axllm:jget fixture "max_steps"))))
    (loop for item across (axllm:jget fixture "cases")
          do (assert-json-equal (axllm::chat-session-transition session (axllm:jget item "event"))
                                (axllm:jget item "expected_action") "session transition"))
    (assert-json-equal (axllm::chat-session-unresolved session) (axllm:jget fixture "expected_pending") "pending")
    (assert-json-equal (axllm:jget (axllm::chat-session-value session) "steps")
                      (axllm:jget fixture "expected_steps") "steps")
    (values :semantic session)))

(defun run-session-events (fixture)
  (let ((decoder (axllm::responses-session-decoder (axllm:jget fixture "model"))))
    (loop for item across (axllm:jget fixture "cases")
          do (let ((events nil) (failure nil))
               (handler-case (setf events (axllm::responses-session-event decoder (axllm:jget item "event")))
                 (axllm:ax-error (c) (setf failure c)))
               (when (gethash "expected_active_id" item)
                 (assert-json-equal (axllm:jget (axllm::responses-session-cursor decoder) "active_id")
                                   (axllm:jget item "expected_active_id") "active response"))
               (if (gethash "expected_exception" item)
                   (unless (and failure (search (gethash "expected_exception" item) (princ-to-string failure)))
                     (fail "expected session error ~a, got ~a" (gethash "expected_exception" item) failure))
                   (progn
                     (when failure (error failure))
                     (assert-json-equal (map 'vector (lambda (event) (axllm:jget event "type")) events)
                                       (axllm:jget item "expected_types") "event types")
                     (dolist (key '("call" "response_id" "status" "required_call_ids" "error"))
                       (let ((expected-key (concatenate 'string "expected_" key)))
                         (when (gethash expected-key item)
                           (assert-json-equal (axllm:jget (aref events 0) key)
                                             (gethash expected-key item) key))))))))
    (values :semantic decoder)))

(defun run-context-cache (fixture)
  (let ((fn (cdr (assoc (axllm:jget fixture "operation")
                       (list (cons "rejection" #'axllm::ai-context-cache-rejection)
                             (cons "expiry" #'axllm::ai-context-cache-expiry)
                             (cons "plan" #'axllm::ai-context-cache-plan)
                             (cons "recovery" #'axllm::ai-context-cache-recovery)
                             (cons "gemini_ops" #'axllm::ai-gemini-cache-ops)) :test #'equal))))
    (unless fn (fail "Unknown context-cache operation"))
    (loop for item across (axllm:jget fixture "cases" (vector fixture))
          do (assert-json-equal (apply fn (coerce (axllm:jget item "args") 'list))
                                (axllm:jget item "expected") "context cache"))
    (values :semantic t)))

(defun run-error-request (fixture)
  (loop for item across (axllm:jget fixture "cases") do
    (cond ((equal (axllm:jget fixture "operation") "view")
           (assert-json-equal (axllm/core::ai-error-request (axllm:jget item "call") (axllm:jget item "options"))
                              (axllm:jget item "expected") "error request view"))
          ((equal (axllm:jget fixture "operation") "normalize")
           (check-error item (axllm/core::openai-normalize-error (axllm:jget item "status")
                                (axllm:jget item "body") (axllm:jget item "call") (axllm:jget item "options"))))
          (t (fail "Unknown error request operation"))))
  (values :semantic t))

(defun run-credential-wrapper (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let* ((client (build-client fixture transport))
           (router (axllm::multiservice-router (list (axllm:object "key" "wrapped" "service" client)))))
      (axllm:ax-chat router (fixture-request fixture))
      (check-transport-request fixture wire)
      (values :semantic router))))

(defun run-multiservice-router (fixture)
  (let* ((services (fixture-services fixture))
         (entries (map 'vector
                       (lambda (item)
                         (let ((service (aref services (axllm:jget item "service_index" 0))))
                           (if (equal (axllm:jget item "kind") "key")
                               (axllm/core::core-map-merge item (axllm:object "service" service)) service)))
                       (axllm:jget fixture "router_entries"))))
    (handler-case
        (let ((router (axllm::multiservice-router entries)) (outputs (axllm:object)))
          (loop for op across (axllm:jget fixture "operations" #())
                for name = (axllm:jget op "name") do
            (if (equal name "set_options") (setf (axllm:ax-options router) (axllm:jget op "options"))
                (let ((fn (cdr (assoc name (list (cons "chat" #'axllm:ax-chat) (cons "embed" #'axllm:ax-embed)
                                                  (cons "transcribe" #'axllm:ax-transcribe) (cons "speak" #'axllm:ax-speak)) :test #'equal))))
                  (unless fn (fail "Unknown router operation ~a" name))
                  (setf (gethash name outputs) (funcall fn router (axllm:jget op "request") (axllm:jget op "options" (axllm:object)))))))
          (when (fixture-expects-failure-p fixture) (fail "Router did not reject fixture"))
          (let ((actual (axllm:object "outputs" outputs "lastChat" (axllm::ax-last-chat-model router)
                                      "lastEmbed" (axllm::ax-last-embed-model router) "lastConfig" (axllm::ax-last-model-config router)
                                      "metrics" (axllm:ax-metrics router) "options" (axllm:ax-options router)
                                      "serviceCalls" (coerce (loop for service across services
                                                                  when (plusp (length (service-calls service))) collect (service-calls service)) 'vector))))
            (when (gethash "modelList" (axllm:jget fixture "expected_output"))
              (setf (gethash "modelList" actual) (axllm::ax-model-list router)))
            (check-output fixture actual)
            (values :semantic actual)))
      (fixture-failed (c) (error c))
      (error (c) (unless (fixture-expects-failure-p fixture) (error c))
        (check-error fixture c) (values :validation-error c)))))

(defun run-verbose (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let ((client (build-client fixture transport)) (logs (axllm::%new-array)))
      (loop for call across (axllm:jget fixture "calls") do
        (let* ((entries nil)
               (axllm::*provider-verbose-sink* (lambda (text) (push text entries)))
               (request (axllm:jget call "request")) (options (axllm:jget call "options")))
          (if (axllm/core::core-true-p (axllm:jget (axllm:jget request "model_config") "stream"))
              (axllm::%collect-stream-chunks (axllm:ax-stream client request options))
              (axllm:ax-chat client request options))
          (vector-push-extend
           (map 'vector (lambda (text)
                          (when (search (axllm:jget fixture "api_key" "test-key") text) (fail "Verbose output leaked API key"))
                          (cl-ppcre:regex-replace " Headers: \\{[\\s\\S]*?\\n\\} \\nBody:" text
                                                 (format nil " Headers: {{HEADERS}} ~%Body:")))
                (nreverse entries)) logs)))
      (assert-json-equal logs (axllm:jget fixture "expected_verbose_logs") "verbose logs")
      (check-transport-request fixture wire)
      (values :semantic logs))))

(defun run-usage-observer (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let ((client (build-client fixture transport)) (failures 0) (events nil)
          (axllm::*telemetry-globals* (axllm::globals-snapshot)))
      (flet ((invoke () (axllm:ax-chat client (fixture-request fixture) (axllm:jget fixture "call_options"))))
        (axllm:set-global "onUsage" (lambda (event) (declare (ignore event)) (incf failures) (error "observer failed")))
        (invoke)
        (assert-json-equal failures 1 "failing observer calls")
        (axllm:set-global "onUsage" (lambda (event) (push event events)))
        (invoke)
        (assert-json-equal (length events) 1 "observer calls")
        (json-subset-p (first events) (axllm:jget fixture "expected_event_subset") "usage event")
        (axllm:set-global "onUsage" :null)
        (invoke)
        (assert-json-equal (length events) 1 "cleared observer calls")
        (assert-json-equal (wire-count wire) 3 "observer transport calls")
        (values :semantic events)))))

(defun run-runtime-hooks (fixture)
  (multiple-value-bind (transport wire) (scripted-wire (fixture-responses fixture))
    (let ((client (build-client fixture transport)) (calls nil)
          (axllm::*telemetry-globals* (axllm::globals-snapshot)))
      (labels ((limiter (name)
                 (lambda (next info)
                   (json-subset-p info (axllm:object "operation" "chat" "streaming" axllm:false) "limiter info")
                   (unless (and (axllm::%present-string (axllm:jget info "model"))
                                (axllm::%present-string (axllm:jget info "provider"))) (fail "Missing limiter model/provider"))
                   (push name calls) (funcall next)))
               (invoke (&optional options) (axllm:ax-chat client (fixture-request fixture) options)))
        (axllm:set-global "rateLimiter" (limiter "global")) (invoke)
        (setf (gethash "rateLimiter" (axllm:ax-options client)) (limiter "service")) (invoke)
        (invoke (axllm:object "rateLimiter" (limiter "call")))
        (remhash "rateLimiter" (axllm:ax-options client))
        (axllm:set-global "tracer" (lambda (&rest args) (declare (ignore args)) (error "tracer failed")))
        (axllm:set-global "meter" (lambda (&rest args) (declare (ignore args)) (error "meter failed")))
        (invoke)
        (handler-case
            (progn (invoke (axllm:object "rateLimiter" (lambda (next info) (declare (ignore next info)) (error "limited"))))
                   (fail "Limiter rejection did not propagate"))
          (fixture-failed (c) (error c))
          (error (c) (unless (search "limited" (princ-to-string c)) (error c))))
        (dolist (key '("rateLimiter" "tracer" "meter")) (axllm:set-global key :null))
        (invoke)
        (let* ((ready (sb-thread:make-semaphore)) (release (sb-thread:make-semaphore))
               (threads (loop for label in '("thread-a" "thread-b") collect
                          (let ((hook (limiter label)))
                            (sb-thread:make-thread
                             (lambda ()
                               (let ((opts (axllm:options-with-runtime-hook-frame (axllm:object)
                                             (axllm:make-runtime-hook-frame :globals (axllm:object "rateLimiter" hook)))))
                                 (sb-thread:signal-semaphore ready)
                                 (sb-thread:wait-on-semaphore release :timeout 5)
                                 (eq hook (axllm:jget (axllm::%provider-hook-context client opts) "rateLimiter")))))))))
          (dotimes (i 2) (unless (sb-thread:wait-on-semaphore ready :timeout 5) (fail "Hook worker did not start")))
          (sb-thread:signal-semaphore release 2)
          (dolist (thread threads) (unless (sb-thread:join-thread thread) (fail "Hook frame leaked across threads"))))
        (assert-json-equal (coerce (nreverse calls) 'vector) (axllm:jget fixture "expected_limiter_order") "limiter order")
        (check-transport-request fixture wire)
        (values :semantic calls)))))

(defun run-custom-labels (fixture)
  ;; Match the TS extractor's customPart: built-in generator labels such as
  ;; success, signature and ai_service do not have the provider's ax. prefix.
  (labels ((run-labels (labelled)
             (let* ((copy (axllm:parse-json (axllm:encode-json fixture)))
                    (service-options (fixture-service-options copy))
                    (records nil)
                    (axllm::*telemetry-globals* (axllm::globals-snapshot)))
               (unless labelled (remhash "customLabels" service-options))
               (axllm::%set-key copy "service_options" service-options)
               (multiple-value-bind (transport wire) (scripted-wire (fixture-responses copy))
                 (let ((client (build-client copy transport)))
                   (flet ((instrument (name &optional options)
                            (declare (ignore options))
                            (flet ((record-value (value &optional attributes)
                                     (declare (ignore value))
                                     (push (cons name (axllm:parse-json (axllm:encode-json attributes))) records)))
                              (axllm:object "add" #'record-value "record" #'record-value))))
                     (axllm:set-global "meter" (axllm:object "createCounter" #'instrument
                                                            "createHistogram" #'instrument "createGauge" #'instrument)))
                   (axllm:set-global "customLabels" :null)
                   (let ((chat (axllm:jget copy "chat")))
                     (axllm:ax-chat client (axllm:jget chat "request")
                                    (if labelled (axllm:object "customLabels" (axllm:jget chat "custom_labels"))
                                        (axllm:object))))
                   (let ((chat-records (reverse records)))
                     (setf records nil)
                     (let* ((spec (axllm:jget copy "forward"))
                            (options (axllm:object "stream" axllm:false))
                            (gen (axllm:ax (axllm:jget spec "signature")
                                           :options (if labelled
                                                        (axllm:object "customLabels" (axllm:jget spec "constructor_custom_labels"))
                                                        (axllm:object)))))
                       (when labelled
                         (axllm::%set-key options "customLabels" (axllm:jget spec "call_custom_labels")))
                       (axllm:forward gen client (axllm:jget spec "input") options))
                     (assert-json-equal (wire-count wire) 2 "label transport calls")
                     (list chat-records (reverse records)))))))
           (check-labels (records baseline expected)
             (dolist (name (axllm::%object-keys expected))
               (let ((entry (assoc name records :test #'equal))
                     (base (assoc name baseline :test #'equal))
                     (custom (axllm:object)))
                 (unless (and entry base) (fail "No ~a metric in labelled or baseline run" name))
                 (dolist (key (axllm::%object-keys (cdr entry)))
                   (unless (nth-value 1 (gethash key (cdr base)))
                     (axllm::%set-key custom key (gethash key (cdr entry)))))
                 (assert-json-equal custom (gethash name expected) name)))))
    (let ((labelled (run-labels t)) (baseline (run-labels nil)))
      (check-labels (first labelled) (first baseline) (axllm:jget fixture "expected_chat_custom_labels"))
      (check-labels (second labelled) (second baseline) (axllm:jget fixture "expected_forward_custom_labels"))
      (values :semantic (second labelled)))))

(defun run-cancellation (fixture)
  (let* ((reason (axllm:jget fixture "reason"))
         (request (fixture-request fixture))
         (stage "provider preflight")
         (token (axllm::cancellation-token)))
    (axllm::cancel token reason)
    (labels ((expect-aborted (thunk limit)
               (let ((start (get-internal-real-time)))
                 (handler-case (progn (funcall thunk) (fail "Cancellation was ignored at ~a" stage))
                   (axllm:provider-error (c)
                     (unless (and (eq (axllm:provider-error-kind c) :aborted)
                                  (not (axllm::provider-error-retryable-p c))
                                  (search reason (princ-to-string c)))
                       (fail "Wrong cancellation error: ~a" c))))
                 (when (> (* 1000 (/ (- (get-internal-real-time) start) internal-time-units-per-second)) limit)
                   (fail "Cancellation exceeded ~a ms" limit)))))
      (multiple-value-bind (transport wire) (scripted-wire (list (cons 200 (axllm:encode-json (axllm:jget (axllm:jget fixture "success_response") "json")))))
        (let ((client (build-client fixture transport)) (options (axllm:object "cancellation" token "infraRetries" 2)))
          (expect-aborted (lambda () (axllm:ax-chat client request options)) (axllm:jget fixture "max_elapsed_ms"))
          (let ((flow (axllm:flow)))
            (axllm:flow-execute flow "answer" (axllm:ax "question:string -> answer:string"))
            (dolist (program (list (axllm:ax "question:string -> answer:string")
                                   (axllm:agent "question:string -> answer:string" :options (axllm:object "actorMode" "completion")) flow))
              (setf stage (format nil "~a preflight" (type-of program)))
              (expect-aborted (lambda () (axllm:forward program client (axllm:object "question" "cancel") options))
                              (axllm:jget fixture "program_max_elapsed_ms"))))
          (assert-json-equal (wire-count wire) 0 "cancelled preflight requests")))
      (multiple-value-bind (transport wire)
          (scripted-wire (list (cons 200 (axllm:jget (axllm:jget fixture "retry_response") "body"))))
        (setf stage "stream retry backoff")
        (let* ((backoff-token (axllm::cancellation-token)) (timer nil)
               (stream-transport (streaming-wire transport))
               (client (build-client fixture transport :streaming-transport
                        (lambda (url headers body)
                          (setf timer (sb-thread:make-thread (lambda () (sleep 0.01) (axllm::cancel backoff-token reason))))
                          (funcall stream-transport url headers body))))
               (axllm::*provider-retry-sleep* (lambda (ms cancellation)
                                              (axllm::cancellation-wait cancellation (/ ms 1000))
                                              (axllm::throw-if-cancelled cancellation))))
          (unwind-protect
               (expect-aborted
                (lambda () (axllm::%collect-stream-chunks
                            (axllm:ax-stream client request
                             (axllm/core::core-map-merge (axllm:jget fixture "retry_options") (axllm:object "cancellation" backoff-token)))))
                (axllm:jget fixture "max_elapsed_ms"))
            (when timer (sb-thread:join-thread timer)))
          (assert-json-equal (wire-count wire) 1 "cancelled backoff requests")))
      (multiple-value-bind (transport wire)
          (scripted-wire (list (cons 200 (axllm:jget (axllm:jget fixture "stream_response") "body"))))
        (setf stage "stream next after cancellation")
        (let* ((stream-token (axllm::cancellation-token)) (client (build-client fixture transport))
               (handle (axllm:ax-stream client request (axllm:object "cancellation" stream-token))))
          (unwind-protect
               (progn (when (eq (axllm:ax-stream-next handle) :null) (fail "Stream ended before cancellation"))
                      (axllm::cancel stream-token reason)
                      (expect-aborted (lambda () (axllm:ax-stream-next handle)) (axllm:jget fixture "max_elapsed_ms")))
            (axllm:ax-stream-close handle))
          (assert-json-equal (wire-count wire) 1 "cancelled stream requests")))
      (values :semantic t))))

(defparameter +fixture-runners+
  (list (cons "ai_chat" #'run-ai-chat)
        (cons "ai_error_request" #'run-error-request)
        (cons "ai_credential_wrapper" #'run-credential-wrapper)
        (cons "ai_multiservice_router" #'run-multiservice-router)
        (cons "ai_verbose" #'run-verbose)
        (cons "ai_usage_observer" #'run-usage-observer)
        (cons "ai_runtime_hooks" #'run-runtime-hooks)
        (cons "ai_custom_labels" #'run-custom-labels)
        (cons "ai_cancellation" #'run-cancellation)
        (cons "ai_balancer" #'run-balancer)
        (cons "ai_typesafe_native" #'run-typesafe-native)
        (cons "ai_session_state" #'run-session-state)
        (cons "ai_session_events" #'run-session-events)
        (cons "ai_context_cache" #'run-context-cache)
        (cons "ai_error" #'run-ai-error)
        (cons "ai_embed" #'run-ai-embed)
        (cons "ai_transcribe" #'run-ai-transcribe)
        (cons "ai_speak" #'run-ai-speak)
        (cons "ai_stream" #'run-ai-stream)
        (cons "ai_realtime" #'run-ai-realtime)
        (cons "ai_model_catalog_runtime" #'run-model-catalog-runtime)
        (cons "ai_model_catalog_audit" #'run-model-catalog-audit)
        (cons "ai_provider_registry" #'run-provider-registry)
        (cons "ai_unsupported" #'run-ai-unsupported)
        (cons "ai_provider_router" #'run-provider-router)
        (cons "ai_provider_descriptor" #'run-provider-descriptor)
        (cons "ai_provider_features" #'run-provider-features))
  "The fixture kinds this runner executes.  A kind absent here is reported as
not claimed rather than passed.")

;;; ------------------------------------------------------------------
;;; The run
;;; ------------------------------------------------------------------

(defun run-ai-conformance-tests (&key verbose (limit nil))
  "Run every axai fixture this port claims.

Returns (values passed failed unclaimed).  UNCLAIMED is the third value on
purpose: a gate that watched only FAILED would pass a run in which most of
the surface was never executed, which is exactly the shape of a false green."
  (let ((passed 0) (failed 0) (unclaimed 0)
        (coverage (make-hash-table :test 'equal))
        (failures '())
        (seen 0))
    (dolist (entry (load-fixtures))
      (when (and limit (>= seen limit)) (return))
      (incf seen)
      (let* ((file (car entry))
             (fixture (cdr entry))
             (name (or (axllm::%present-string (axllm:jget fixture "name"))
                       (pathname-name file)))
             (kind (or (axllm::%present-string (axllm:jget fixture "kind")) "unknown"))
             (provider (or (axllm::%present-string (axllm:jget fixture "provider")) ""))
             (runner (cdr (assoc kind +fixture-runners+ :test #'string=)))
             (bucket (or (gethash kind coverage)
                         (setf (gethash kind coverage)
                               (axllm:object "kind" kind "semantic" 0 "validation-error" 0
                                             "transport-boundary" 0
                                             "explicitly-not-claimed" 0 "failed" 0)))))
        (flet ((bump (key) (incf (gethash key bucket))))
          (if (null runner)
              (progn (incf unclaimed) (bump "explicitly-not-claimed"))
              (handler-case
                  (multiple-value-bind (classification)
                      (with-fixture-environment (fixture)
                        (let* ((delays '()) (warnings '()) (*credential-requests* nil)
                               (axllm/core::*ai-warnings-shown* (make-hash-table :test 'equal))
                               (axllm/core::*ai-warning-sink* (lambda (text) (push text warnings)))
                               (axllm::*provider-retry-sleep* (lambda (ms cancellation)
                                                               (axllm:throw-if-cancelled cancellation)
                                                               (push ms delays)))
                               (axllm::*provider-retry-now* (lambda () (axllm:jget fixture "retry_now_ms" 1800000000000)))
                               (axllm::*provider-retry-random* (lambda () (axllm:jget fixture "retry_random" 0.5d0))))
                          (multiple-value-prog1 (funcall runner fixture)
                            (when (gethash "expected_credential_requests" fixture)
                              (assert-json-equal (coerce (nreverse *credential-requests*) 'vector)
                                                 (gethash "expected_credential_requests" fixture) "credential callbacks"))
                            (when (gethash "expected_warnings" fixture)
                              (assert-json-equal (coerce (nreverse warnings) 'vector) (gethash "expected_warnings" fixture) "warnings"))
                            (when (gethash "expected_request_after" fixture)
                              (assert-json-equal (fixture-request fixture) (gethash "expected_request_after" fixture) "request after"))
                            (when (gethash "expected_retry_delays_ms" fixture)
                              (assert-json-equal (coerce (nreverse delays) 'vector)
                                                 (gethash "expected_retry_delays_ms" fixture) "retry delays")))))
                    (incf passed)
                    (bump (string-downcase (symbol-name classification)))
                    ;; The receipt records only fixtures that actually passed,
                    ;; keyed by their filename on disk, so the parent can
                    ;; reconcile what ran against the full inventory. It is a
                    ;; no-op until a full gate enables it, and it is deliberately
                    ;; unreachable for a failure or an unclaimed kind.
                    (axllm/conformance:record-result "axai" file classification)
                    (when verbose (format t "ok   ~a~%" name)))
                (error (condition)
                  (incf failed)
                  (bump "failed")
                  (push (list name kind provider (princ-to-string condition)) failures)
                  (when verbose (format t "FAIL ~a: ~a~%" name condition))))))))
    (format t "~&axai: ~a passed, ~a failed, ~a not claimed (of ~a fixtures)~%"
            passed failed unclaimed seen)
    (when failures
      (format t "~&First failures:~%")
      (let* ((ordered (nreverse failures))
             (shown (subseq ordered 0 (min 25 (length ordered)))))
        (dolist (failure shown)
          (format t "  ~a [~a ~a]~%    ~a~%"
                  (first failure) (third failure) (second failure)
                  (let ((text (fourth failure)))
                    (subseq text 0 (min 300 (length text))))))))
    (write-coverage-report coverage)
    (values passed failed unclaimed)))

(defun write-coverage-report (coverage)
  "Write conformance-coverage.json beside this file.

Every kind the runner saw is listed with how each of its fixtures was
exercised, so a reader can tell a semantic pass from an unclaimed kind without
reading the runner."
  (let ((report (axllm:object))
        (kinds (sort (loop for key being the hash-keys of coverage collect key) #'string<)))
    (let ((entries (axllm::%new-array)))
      (dolist (kind kinds)
        (vector-push-extend (gethash kind coverage) entries))
      (setf (gethash "surface" report) "axai"
            (gethash "target" report) "lisp"
            (gethash "kinds" report) entries))
    (let ((path (merge-pathnames "tests/conformance-coverage.json"
                                 (asdf:system-source-directory "axllm"))))
      (handler-case
          (with-open-file (out path :direction :output :if-exists :supersede
                                    :if-does-not-exist :create
                                    :external-format :utf-8)
            (write-string (axllm:encode-json report) out)
            (terpri out))
        (error (condition)
          (format t "~&could not write ~a: ~a~%" path condition))))
    report))

(defun run-ai-conformance-tests-or-die ()
  "Run the axai fixtures and fail unless every one of them was executed.

Both numbers have to be zero.  A fixture kind with no runner arm is a gap in
this port, not a fixture to skip: counting it as anything other than a failure
of the gate would let the suite report success while a claimed surface went
untested."
  (multiple-value-bind (passed failed unclaimed) (run-ai-conformance-tests)
    (when (zerop passed)
      (error "Ax axai conformance executed no fixtures; the fixture directory ~
or its path is wrong."))
    (unless (zerop failed)
      (error "Ax axai conformance failed: ~a fixture(s) did not match." failed))
    (unless (zerop unclaimed)
      (error "Ax axai conformance is incomplete: ~a fixture(s) have no runner ~
arm. See tests/conformance-coverage.json for the kinds." unclaimed))
    t))
