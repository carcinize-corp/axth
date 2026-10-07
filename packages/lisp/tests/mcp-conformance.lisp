;;;; mcp-conformance.lisp --- the shared ir/conformance/axmcp fixtures.
;;;;
;;;; These run the same 44 fixtures every other Ax port runs, against each
;;;; fixture's own recorded expectations. Nothing is asserted against this
;;;; implementation's output.
;;;;
;;;; Two rules this file keeps, because they are what makes a conformance
;;;; run mean anything:
;;;;
;;;;   Dispatch is explicit. Every operation has its own arm. An operation
;;;;   with no arm is a failure naming it, never a silent pass, so adding a
;;;;   fixture kind nobody implemented cannot look green.
;;;;
;;;;   Assertions are semantic. Each arm checks outputs, recorded requests,
;;;;   request headers, client state or the expected error, not merely that
;;;;   a call returned without signalling. There is no broad "expectation
;;;;   helper" that passes when a key merely exists.
;;;;
;;;; RUN-MCP-CONFORMANCE-TESTS returns (values passed failed coverage), and
;;;; COVERAGE classifies every fixture as semantic, validation-error,
;;;; transport-boundary or explicitly-not-claimed. An explicitly-not-claimed
;;;; fixture must name what the Lisp subset does not have; it is the only
;;;; way a fixture counts as handled without executing an implementation
;;;; path, and the report says so out loud.

(in-package #:axllm)

(export '(run-mcp-conformance-tests run-mcp-conformance-tests-or-die
          run-mcp-harness-selftests mcp-conformance-directory))

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(define-condition mcp-fixture-failure (error)
  ((detail :initarg :detail :reader mcp-fixture-failure-detail))
  (:report (lambda (condition stream)
             (write-string (mcp-fixture-failure-detail condition) stream))))

(defun %fixture-fail (format-control &rest arguments)
  (error 'mcp-fixture-failure :detail (apply #'format nil format-control arguments)))

(defun mcp-conformance-directory (&optional (suite "axmcp"))
  "Where the shared AxIR fixtures live."
  (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
    (merge-pathnames (concatenate 'string suite "/")
                     (if (and override (plusp (length override)))
                         (uiop:ensure-directory-pathname override)
                         (asdf:system-relative-pathname "axllm" "../../ir/conformance/")))))

(defun %fixture-files (suite)
  (sort (directory (merge-pathnames "*.json" (mcp-conformance-directory suite)))
        #'string< :key #'namestring))

(defun %read-fixture (path)
  (parse-json (uiop:read-file-string path)))

(defun %show (value)
  (if (stringp value) (format nil "~s" value) (encode-json value)))

(defun %same (left right) (axllm/core::core-value-equal left right))

(defun %assert-equal (actual expected label)
  (unless (%same actual expected)
    (%fixture-fail "~a mismatch~%    expected: ~a~%    actual:   ~a"
                   label (%show expected) (%show actual)))
  t)

(defun %assert-true (value label)
  (unless (axllm/core::core-true-p value)
    (%fixture-fail "~a was not true (got ~a)" label (%show value)))
  t)

(defun %assert-subset (actual expected label)
  "Every member EXPECTED records must be present in ACTUAL and match.

A subset check is what the fixtures are written against: they pin the fields
that carry meaning and stay silent about the rest. It is still a real check,
because a missing key, a wrong value or a short array all fail."
  (cond ((hash-table-p expected)
         (unless (hash-table-p actual)
           (%fixture-fail "~a: expected an object, got ~a" label (%show actual)))
         (dolist (key (%object-keys expected))
           (unless (%mcp-present-key-p actual key)
             (%fixture-fail "~a: missing key ~s" label key))
           (%assert-subset (gethash key actual) (gethash key expected)
                           (format nil "~a.~a" label key))))
        ((%array-p expected)
         (unless (%array-p actual)
           (%fixture-fail "~a: expected an array, got ~a" label (%show actual)))
         (unless (>= (length actual) (length expected))
           (%fixture-fail "~a: expected at least ~a items, got ~a"
                          label (length expected) (length actual)))
         (loop for index from 0 below (length expected)
               do (%assert-subset (aref actual index) (aref expected index)
                                  (format nil "~a[~a]" label index))))
        (t (%assert-equal actual expected label)))
  t)

(defun %methods (requests)
  (let ((out (%new-array)))
    (loop for request across requests
          do (vector-push-extend (jget request "method") out))
    out))

(defun %assert-requests (requests fixture)
  (let ((expected (%event-array (jget fixture "expected_requests"))))
    (when (< (length requests) (length expected))
      (%fixture-fail "expected at least ~a requests, got ~a (~a)"
                     (length expected) (length requests) (%show (%methods requests))))
    (loop for index from 0 below (length expected)
          do (%assert-subset (aref requests index) (aref expected index)
                             (format nil "request ~a" index))))
  t)

(defun %assert-catalog-names (catalog expected label)
  (unless (eq expected :null)
    (let ((names (%new-array)))
      (loop for item across catalog
            do (vector-push-extend (%mcp-text (jget item "name")) names))
      (%assert-equal names expected label)))
  t)

(defun %fixture-client (fixture &key responses options)
  "A client over a scripted transport. Returns (values client transport)."
  (let* ((transport (make-mcp-scripted-transport
                     (%event-array (or responses
                                       (let ((value (jget fixture "responses")))
                                         (if (eq value :null)
                                             (jget fixture "transport_responses")
                                             value))))))
         (client (apply #'make-mcp-client transport
                        (loop for key in (%object-keys (%mcp-object-or-empty
                                                        (or options
                                                            (jget fixture "client_options"))))
                              append (list key (gethash key (%mcp-object-or-empty
                                                             (or options
                                                                 (jget fixture "client_options")))))))))
    (values client transport)))

;;; ------------------------------------------------------------------
;;; Operation arms
;;; ------------------------------------------------------------------

(defmacro %expect-error (label fragment &body body)
  "Run BODY, require a failure, and tie it to FRAGMENT."
  (let ((condition (gensym)) (fragment-value (gensym)))
    `(let ((,fragment-value ,fragment))
       (handler-case (progn ,@body
                            (%fixture-fail "~a was accepted; expected a failure containing ~s"
                                           ,label ,fragment-value))
         (mcp-fixture-failure (,condition) (error ,condition))
         (error (,condition)
           (let ((text (princ-to-string ,condition)))
             (unless (or (zerop (length ,fragment-value)) (search ,fragment-value text))
               (%fixture-fail "~a failed with ~s, expected it to contain ~s"
                              ,label text ,fragment-value))
             ,condition))))))


(defun %run-ssrf (fixture)
  (mcp-validate-endpoint (%mcp-text (jget fixture "endpoint" "https://127.0.0.1/mcp"))
                         (jget fixture "ssrfProtection"))
  (when (stringp (jget fixture "expected_error_contains"))
    (%fixture-fail "expected SSRF validation to reject the endpoint"))
  :validation-error)

(defun %run-stdio-framing (fixture)
  (let* ((message (jget fixture "message"))
         (encoded (mcp-stdio-encode message))
         (expected (jget fixture "expected_line")))
    (unless (eq expected :null)
      (%assert-equal encoded expected "stdio line"))
    ;; Round-tripping is the real contract: the frame a server reads must
    ;; decode back to the message we meant to send.
    (%assert-subset (mcp-stdio-decode encoded) message "stdio decoded")
    (unless (char= (char encoded (1- (length encoded))) #\Newline)
      (%fixture-fail "stdio frame is not newline terminated")))
  :semantic)

(defun %run-oauth-discovery (fixture)
  (%assert-subset (axllm/core::mcp-oauth-parse-www-authenticate
                   (%mcp-text (jget fixture "www_authenticate" "")))
                  (%mcp-object-or-empty (jget fixture "expected_parse"))
                  "OAuth WWW-Authenticate parsing")
  (%assert-subset (axllm/core::mcp-oauth-discovery-endpoints
                   (%mcp-text (jget fixture "requested_url" ""))
                   (%mcp-text (jget fixture "issuer" ""))
                   (%mcp-text (jget fixture "resource_metadata_url" "")))
                  (%mcp-object-or-empty (jget fixture "expected_endpoints"))
                  "OAuth discovery endpoints")
  (loop for case across (%event-array (jget fixture "coverage_cases"))
        do (%assert-subset (axllm/core::mcp-oauth-validate-resource-coverage
                            (%mcp-text (jget case "requested_url" ""))
                            (%mcp-object-or-empty (jget case "metadata")))
                           (%mcp-object-or-empty (jget case "expected"))
                           "OAuth resource coverage"))
  :semantic)

(defun %run-oauth-as-metadata (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (%assert-subset (axllm/core::mcp-oauth-validate-as-metadata
                            (%mcp-object-or-empty (jget case "metadata"))
                            (%mcp-text (jget case "expected_issuer" ""))
                            (json-boolean (axllm/core::core-true-p
                                           (jget case "require_authorization")))
                            (%mcp-text (jget case "client_auth_method" "none")))
                           (%mcp-object-or-empty (jget case "expected"))
                           "OAuth AS metadata"))
  :semantic)

(defun %core-args (case key count &optional (default ""))
  "CASE's recorded positional arguments, padded to COUNT."
  (let ((args (%event-array (jget case key))))
    (loop for index from 0 below count
          collect (if (< index (length args)) (aref args index) default))))

(defun %run-oauth-token (fixture)
  (let ((authorization (%mcp-object-or-empty (jget fixture "authorization"))))
    (destructuring-bind (client-id redirect-uri scopes resource state challenge)
        (%core-args authorization "args" 6)
      (%assert-subset (axllm/core::mcp-oauth-authorization-request-params
                       client-id redirect-uri (%event-array scopes) resource state challenge)
                      (%mcp-object-or-empty (jget authorization "expected"))
                      "OAuth authorization params")))
  (loop for case across (%event-array (jget fixture "grant_cases"))
        do (destructuring-bind (grant-type client-id client-secret auth-method resource
                                scopes code redirect-uri verifier refresh-token)
               (%core-args case "args" 10)
             (%assert-subset (axllm/core::mcp-oauth-grant-body
                              grant-type client-id client-secret auth-method resource
                              (%event-array scopes) code redirect-uri verifier refresh-token)
                             (%mcp-object-or-empty (jget case "expected"))
                             "OAuth grant body")))
  (loop for case across (%event-array (jget fixture "token_cases"))
        do (%assert-subset (axllm/core::mcp-oauth-parse-token-response
                            (%mcp-object-or-empty (jget case "response"))
                            (jget case "now_ms" 0)
                            (%mcp-text (jget case "previous_refresh_token" ""))
                            (%mcp-text (jget case "issuer" "")))
                           (%mcp-object-or-empty (jget case "expected"))
                           "OAuth token response"))
  (loop for case across (%event-array (jget fixture "plan_cases"))
        do (%assert-subset (axllm/core::mcp-oauth-plan-ensure-token
                            (jget case "token")
                            (jget case "now_ms" 0)
                            (json-boolean (axllm/core::core-true-p (jget case "force_refresh")))
                            (%mcp-text (jget case "grant_type" "authorization_code"))
                            (json-boolean (axllm/core::core-true-p (jget case "has_on_auth_code"))))
                           (%mcp-object-or-empty (jget case "expected"))
                           "OAuth token plan"))
  :semantic)

(defun %run-oauth-issuer (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (%assert-subset (axllm/core::mcp-oauth-validate-issuer
                            (%mcp-object-or-empty (jget case "response"))
                            (%mcp-text (jget case "expected_issuer" ""))
                            (json-boolean (axllm/core::core-true-p (jget case "require_iss"))))
                           (%mcp-object-or-empty (jget case "expected"))
                           "OAuth issuer validation"))
  :semantic)

(defun %run-oauth (fixture)
  "PKCE plus the endpoint-keyed cached-token path through a real transport."
  (%assert-equal (mcp-pkce-challenge (%mcp-text (jget fixture "verifier" "test-verifier")))
                 (jget fixture "expected_challenge") "PKCE challenge")
  (let* ((endpoint (%mcp-text (jget fixture "endpoint" "https://example.com/mcp")))
         (store (object endpoint (%mcp-object-or-empty (jget fixture "stored_token"))))
         (transport (make-mcp-streamable-http-transport
                     endpoint :oauth (mcp-oauth-options :token-store store))))
    ;; No network: a cached, unexpired token must satisfy the plan on its own.
    (%assert-true (json-boolean (%mcp-apply-oauth transport)) "cached OAuth token applied")
    (%assert-equal (jget (mcp-http-headers transport) "Authorization")
                   (jget fixture "expected_authorization") "cached OAuth Authorization"))
  ;; A fresh verifier must never repeat, or PKCE is decorative.
  (let ((first (mcp-pkce-verifier)) (second (mcp-pkce-verifier)))
    (when (string= first second) (%fixture-fail "PKCE verifier repeated"))
    (when (< (length first) 43) (%fixture-fail "PKCE verifier is too short: ~a" (length first))))
  :semantic)

(defun %run-discover (fixture)
  (let ((constants (axllm/core::mcp-protocol-constants))
        (version (%mcp-text (jget fixture "protocol_version" "2026-07-28"))))
    (unless (find version (%event-array (jget constants "supportedProtocolVersions"))
                  :test #'%same)
      (%fixture-fail "missing supported MCP protocol version ~a" version))
    (%assert-subset (axllm/core::mcp-jsonrpc-request
                     (%mcp-text (jget fixture "request_id" "discover-1"))
                     "server/discover"
                     (%mcp-object-or-empty (jget fixture "params")))
                    (%mcp-object-or-empty (jget fixture "expected_request"))
                    "discover request"))
  :semantic)

(defun %run-modern-headers (fixture)
  (let ((headers (axllm/core::mcp-modern-request-headers
                  (%mcp-text (jget fixture "method" "server/discover"))
                  (%mcp-text (jget fixture "resource_name" ""))
                  (%mcp-text (jget fixture "protocol_version" "")))))
    (%assert-subset headers (%mcp-object-or-empty (jget fixture "expected_headers")) "modern headers")
    (loop for name across (%event-array (jget fixture "forbidden_headers"))
          do (when (%mcp-present-key-p headers (%mcp-text name))
               (%fixture-fail "modern headers contain forbidden ~a" (%mcp-text name)))))
  :semantic)

(defun %run-era-classification (fixture)
  (%assert-subset (axllm/core::mcp-classify-discovery-result (jget fixture "discovery_result"))
                  (%mcp-object-or-empty (jget fixture "expected_classification"))
                  "discovery classification")
  (loop for invalid across (%event-array (jget fixture "invalid_discovery_results"))
        do (let ((result (axllm/core::mcp-classify-discovery-result invalid)))
             (unless (json-false-p (jget result "valid"))
               (%fixture-fail "invalid discovery result classified as valid: ~a" (%show invalid)))))
  (loop for case across (%event-array (jget fixture "era_cases"))
        do (%assert-subset (axllm/core::mcp-resolve-known-era
                            (%mcp-text (jget case "configured" "auto"))
                            (jget case "hint") (jget case "cached") (jget case "stored"))
                           (%mcp-object-or-empty (jget case "expected")) "era resolution"))
  (let ((case (%mcp-object-or-empty (jget fixture "capability_case"))))
    (%assert-subset (axllm/core::mcp-client-capabilities
                     (json-boolean (axllm/core::core-true-p (jget case "has_roots")))
                     (json-boolean (axllm/core::core-true-p (jget case "has_sampling")))
                     (json-boolean (axllm/core::core-true-p (jget case "has_elicitation")))
                     (%mcp-text (jget case "era" "legacy"))
                     (json-boolean (axllm/core::core-true-p (jget case "tasks_extension"))))
                    (%mcp-object-or-empty (jget case "expected")) "client capabilities"))
  (loop for case across (%event-array (jget fixture "request_name_cases"))
        do (%assert-equal (axllm/core::mcp-request-name
                           (%mcp-text (jget case "method" ""))
                           (%mcp-object-or-empty (jget case "params")))
                          (jget case "expected" "") "request name"))
  :semantic)

(defun %run-mutual-version (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (%assert-equal (axllm/core::mcp-select-mutual-version
                           (jget case "error_data")
                           (%event-array (jget case "client_versions")))
                          (jget case "expected_version" "") "mutual version"))
  :semantic)

(defun %run-request-meta (fixture)
  (%assert-subset (axllm/core::mcp-build-request-meta
                   (jget fixture "existing")
                   (%mcp-text (jget fixture "protocol_version" "2026-07-28"))
                   (%mcp-object-or-empty (jget fixture "client_capabilities"))
                   (%mcp-object-or-empty (jget fixture "client_info"))
                   (jget fixture "log_level")
                   (jget fixture "traceparent")
                   (jget fixture "tracestate"))
                  (%mcp-object-or-empty (jget fixture "expected_meta")) "request meta")
  :semantic)

(defun %run-extension-negotiation (fixture)
  (%assert-equal (axllm/core::mcp-negotiate-extensions
                  (%mcp-object-or-empty (jget fixture "client_extensions"))
                  (%mcp-object-or-empty (jget fixture "server_extensions")))
                 (%mcp-object-or-empty (jget fixture "expected_extensions"))
                 "extension negotiation")
  :semantic)

(defun %run-param-headers (fixture)
  (let ((bindings (axllm/core::mcp-param-header-bindings
                   (%mcp-object-or-empty (jget fixture "input_schema")))))
    (%assert-equal bindings (%event-array (jget fixture "expected_bindings"))
                   "parameter header bindings")
    (%assert-equal (axllm/core::mcp-param-header-values
                    bindings (%mcp-object-or-empty (jget fixture "arguments")))
                   (%mcp-object-or-empty (jget fixture "expected_values"))
                   "parameter header values")
    (loop for invalid across (%event-array (jget fixture "invalid_schemas"))
          do (%expect-error (format nil "invalid parameter header schema ~a"
                                    (%show (jget invalid "schema")))
                            (%mcp-text (jget invalid "expected_error_contains" ""))
               (axllm/core::mcp-param-header-bindings (%mcp-object-or-empty (jget invalid "schema")))))
    (loop for invalid across (%event-array (jget fixture "invalid_values"))
          do (%expect-error "invalid parameter header value"
                            (%mcp-text (jget invalid "expected_error_contains" ""))
               (axllm/core::mcp-param-header-values
                bindings (%mcp-object-or-empty (jget invalid "arguments"))))))
  :validation-error)

(defun %run-header-value (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (%assert-equal (axllm/core::mcp-header-value-plan (%mcp-text (jget case "value" "")))
                          (%mcp-object-or-empty (jget case "expected_plan")) "header value plan"))
  :semantic)

(defun %run-cache-fold (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (let ((actual (axllm/core::mcp-fold-cache-info
                          (%event-array (jget case "pages")) (jget case "fetched_at" 0))))
             (%assert-subset actual (%mcp-object-or-empty (jget case "expected")) "cache info")
             (loop for field across (%event-array (jget case "forbidden_fields"))
                   do (when (%mcp-present-key-p actual (%mcp-text field))
                        (%fixture-fail "cache info contains forbidden field ~a" (%mcp-text field))))
             (%assert-equal (axllm/core::mcp-cache-freshness actual (jget case "now" 0))
                            (jget case "expected_fresh") "cache freshness")))
  :semantic)

(defun %run-tasks-v2-violations (fixture)
  (loop for case across (%event-array (jget fixture "validation_cases"))
        do (%assert-equal (axllm/core::mcp-validate-modern-task (jget case "task"))
                          (jget case "expected_valid") "modern task validation"))
  (loop for case across (%event-array (jget fixture "terminal_cases"))
        do (%assert-subset (axllm/core::mcp-task-terminal-outcome
                            (%mcp-object-or-empty (jget case "task")))
                           (%mcp-object-or-empty (jget case "expected")) "task terminal outcome"))
  (loop for scenario across (%event-array (jget fixture "scenarios"))
        do (multiple-value-bind (client transport)
               (%fixture-client fixture :responses (jget scenario "responses")
                                        :options (jget scenario "client_options"))
             (declare (ignore transport))
             (mcp-init client)
             (%expect-error "Tasks v2 protocol violation"
                            (%mcp-text (jget scenario "expected_error" ""))
               (mcp-call-tool client "slow" (object)))))
  :validation-error)

(defun %run-mrtr-violations (fixture)
  (loop for case across (%event-array (jget fixture "plan_cases"))
        do (%assert-subset (axllm/core::mcp-mrtr-plan-round
                            (%mcp-object-or-empty (jget case "result"))
                            (%mcp-text (jget case "era" "legacy"))
                            (%mcp-text (jget case "method" "tools/call"))
                            (jget case "round" 0) (jget case "max_rounds"))
                           (%mcp-object-or-empty (jget case "expected")) "MRTR round plan"))
  (loop for case across (%event-array (jget fixture "fulfill_cases"))
        do (%assert-subset (axllm/core::mcp-mrtr-plan-fulfillment
                            (%mcp-object-or-empty (jget case "input_requests"))
                            (jget case "roots")
                            (json-boolean (axllm/core::core-true-p (jget case "has_elicitation")))
                            (json-boolean (axllm/core::core-true-p (jget case "has_sampling"))))
                           (%mcp-object-or-empty (jget case "expected")) "MRTR fulfillment plan"))
  (loop for case across (%event-array (jget fixture "next_params_cases"))
        do (%assert-equal (axllm/core::mcp-mrtr-next-params
                           (%mcp-object-or-empty (jget case "base_params"))
                           (jget case "input_responses") (jget case "request_state"))
                          (%mcp-object-or-empty (jget case "expected")) "MRTR next params"))
  :validation-error)

(defun %run-http-session-headers (fixture)
  (let ((transport (apply #'make-mcp-streamable-http-transport
                          (%mcp-text (jget fixture "endpoint" "https://example.com/mcp"))
                          (loop for key in (%object-keys (%mcp-object-or-empty
                                                          (jget fixture "transport_options")))
                                append (list key (gethash key (%mcp-object-or-empty
                                                               (jget fixture "transport_options"))))))))
    (setf (mcp-http-session-id transport) (%mcp-text (jget fixture "session_id" "session-1")))
    (mcp-transport-set-protocol-version
     transport (%mcp-text (jget fixture "protocol_version" (mcp-protocol-version))))
    (%assert-subset (mcp-http-build-headers transport :base (object "Accept" "application/json"))
                    (%mcp-object-or-empty (jget fixture "expected_headers")) "headers"))
  :transport-boundary)

(defun %run-modern-transport-headers (fixture)
  (let ((transport (make-mcp-streamable-http-transport
                    (%mcp-text (jget fixture "endpoint" "https://example.com/mcp")))))
    (setf (mcp-http-session-id transport) (%mcp-text (jget fixture "session_id" "legacy-session")))
    (mcp-transport-set-era transport (%mcp-text (jget fixture "era" "modern")))
    (mcp-transport-set-protocol-version
     transport (%mcp-text (jget fixture "protocol_version" "2026-07-28")))
    (let ((headers (mcp-http-build-headers
                    transport :base (object "Accept" "application/json")
                              :method (%mcp-text (jget fixture "method" ""))
                              :params (%mcp-object-or-empty (jget fixture "params"))
                              :extra-headers (%mcp-object-or-empty (jget fixture "extra_headers")))))
      (%assert-subset headers (%mcp-object-or-empty (jget fixture "expected_headers"))
                      "modern headers")
      (loop for name across (%event-array (jget fixture "forbidden_headers"))
            do (when (%mcp-present-key-p headers (%mcp-text name))
                 (%fixture-fail "forbidden modern header present: ~a" (%mcp-text name)))))
    (%assert-equal (mcp-transport-era-cache-key transport)
                   (jget fixture "expected_era_cache_key") "era cache key")
    ;; Modern MCP must refuse the legacy GET stream outright.
    (%expect-error "modern HTTP GET listening"
                   (%mcp-text (jget fixture "expected_listen_error_contains" ""))
      (mcp-transport-start-listening transport)))
  :transport-boundary)

(defun %run-inheritance-plan (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (let ((expected-error (jget case "expected_error")))
             (if (stringp expected-error)
                 (handler-case
                     (progn (axllm/core::mcp-inheritance-plan
                             (%event-array (jget fixture "mcp"))
                             (%event-array (jget fixture "ucp"))
                             (jget case "inheritance"))
                            (%fixture-fail "inheritance ~a was accepted; expected ~s"
                                           (%show (jget case "inheritance")) expected-error))
                   (mcp-fixture-failure (condition) (error condition))
                   (error (condition)
                     (%assert-equal (princ-to-string condition) expected-error
                                    "inheritance plan error")))
                 (%assert-equal (axllm/core::mcp-inheritance-plan
                                 (%event-array (jget fixture "mcp"))
                                 (%event-array (jget fixture "ucp"))
                                 (jget case "inheritance"))
                                (jget case "expected") "inheritance plan"))))
  :semantic)

(defun %run-inheritance-context (fixture)
  "Derive a child context per case and check what each client actually saw."
  (loop for case across (%event-array (jget fixture "cases"))
        do (let ((clients '()) (transports (object)) (results (%new-array)) (selected (%new-array))
                 (error-text :null))
             (loop for spec across (%event-array (jget fixture "clients"))
                   do (let ((transport (make-mcp-scripted-transport
                                        (%event-array (jget spec "responses")))))
                        (%set-key transports (%mcp-text (jget spec "namespace")) transport)
                        (push (make-mcp-client transport
                                               :namespace (%mcp-text (jget spec "namespace"))
                                               :era "modern")
                              clients)))
             (setf clients (nreverse clients))
             (let ((context (make-execution-context :mcp clients)))
               (handler-case
                   (let ((child (execution-context-derive context (jget case "inheritance"))))
                     (dolist (client (execution-context-mcp child))
                       (vector-push-extend (mcp-namespace client) selected))
                     (dolist (spec (execution-context-native-tools child))
                       (vector-push-extend (native-tool-call spec (object "query" "scope-probe"))
                                           results)))
                 (error (condition) (setf error-text (princ-to-string condition))))
               (%assert-equal error-text (jget case "expected_error") "inheritance error")
               (when (eq error-text :null)
                 (%assert-equal selected (%event-array (jget case "expected_namespaces"))
                                "inherited namespaces"))
               (%assert-equal results (%event-array (jget case "expected_results"))
                              "child tool results")
               (dolist (namespace (%object-keys transports))
                 (let ((transport (gethash namespace transports)))
                   (%assert-equal (%methods (mcp-scripted-requests transport))
                                  (%event-array (jget (%mcp-object-or-empty
                                                       (jget case "expected_methods"))
                                                      namespace))
                                  (format nil "~a child methods" namespace))
                   (loop for request across (mcp-scripted-requests transport)
                         do (when (equal (jget request "method") "tools/call")
                              (%assert-equal (jget (%mcp-object-or-empty (jget request "params"))
                                                   "arguments")
                                             (object "query" "scope-probe")
                                             "child tool arguments")))))
               ;; The parent keeps every client a restricted child gave up.
               (let ((parent-results (%new-array)))
                 (dolist (spec (execution-context-native-tools context))
                   (vector-push-extend (native-tool-call spec (object "query" "parent-probe"))
                                       parent-results))
                 (%assert-equal parent-results
                                (%event-array (jget case "expected_parent_results"))
                                "parent tool results"))
               (dolist (namespace (%object-keys transports))
                 (let ((transport (gethash namespace transports))
                       (calls (%new-array)))
                   (%assert-equal (%methods (mcp-scripted-requests transport))
                                  (%event-array (jget (%mcp-object-or-empty
                                                       (jget case "expected_parent_methods"))
                                                      namespace))
                                  (format nil "~a parent methods" namespace))
                   (loop for request across (mcp-scripted-requests transport)
                         do (when (equal (jget request "method") "tools/call")
                              (let ((params (%mcp-object-or-empty (jget request "params"))))
                                (vector-push-extend (object "name" (jget params "name")
                                                            "arguments" (jget params "arguments"))
                                                    calls))))
                   (%assert-equal calls
                                  (%event-array (jget (%mcp-object-or-empty (jget case "expected_calls"))
                                                      namespace))
                                  (format nil "~a tool calls" namespace)))))))
  :semantic)

(defun %run-execution-context-ucp (fixture)
  "The runnable half of the UCP execution-context fixture.

The agent-callable and policy-flag expectations need AxAgent, which the Lisp
subset does not have; they are reported as not claimed rather than skipped
silently. Everything else here runs: namespaces, native tools, runtime
modules, a real UCP call through a binding, and the continuation state."
  (let* ((transport (make-mcp-scripted-transport (%event-array (jget fixture "responses"))))
         (mcp (apply #'make-mcp-client transport
                     (loop for key in (%object-keys (%mcp-object-or-empty
                                                     (jget fixture "client_options")))
                           append (list key (gethash key (%mcp-object-or-empty
                                                          (jget fixture "client_options")))))))
         (ucp (apply #'make-ucp-client (%mcp-object-or-empty (jget fixture "ucp_profile"))
                     (lambda (operation payload options)
                       (declare (ignore operation payload options))
                       (%mcp-json-clone (%mcp-object-or-empty (jget fixture "ucp_response"))))
                     (loop for key in (%object-keys (%mcp-object-or-empty (jget fixture "ucp_options")))
                           append (list (intern (string-upcase
                                                 (cl-ppcre:regex-replace-all
                                                  "([a-z0-9])([A-Z])" key "\\1-\\2"))
                                                :keyword)
                                        (gethash key (%mcp-object-or-empty
                                                      (jget fixture "ucp_options")))))))
         (context (execution-context-initialize (make-execution-context :mcp mcp :ucp ucp)))
         (names (%new-array)))
    (dolist (client (execution-context-mcp context)) (vector-push-extend (mcp-namespace client) names))
    (dolist (client (execution-context-ucp context)) (vector-push-extend (ucp-namespace client) names))
    (%assert-equal names (%event-array (jget fixture "expected_namespaces")) "context namespaces")
    (let ((tool-names (mapcar (lambda (spec) (%mcp-text (native-tool-name spec)))
                              (execution-context-native-tools context))))
      (loop for expected across (%event-array (jget fixture "expected_native_tools"))
            do (unless (member (%mcp-text expected) tool-names :test #'string=)
                 (%fixture-fail "missing native context tool ~a" (%mcp-text expected)))))
    (let ((modules (execution-context-runtime-modules context)))
      (loop for expected across (%event-array (jget fixture "expected_runtime_modules"))
            do (let ((module (find (%mcp-text (jget expected "name")) modules
                                   :key (lambda (m) (%mcp-text (jget m "name")))
                                   :test #'string=)))
                 (unless module
                   (%fixture-fail "missing runtime module ~a" (%mcp-text (jget expected "name"))))
                 (let ((functions (mapcar (lambda (spec) (%mcp-text (native-tool-name spec)))
                                          (coerce (jget module "functions") 'list))))
                   (loop for wanted across (%event-array (jget expected "functions"))
                         do (unless (member (%mcp-text wanted) functions :test #'string=)
                              (%fixture-fail "missing runtime callable ~a.~a"
                                             (%mcp-text (jget expected "name"))
                                             (%mcp-text wanted))))))))
    (let* ((call (%mcp-object-or-empty (jget fixture "call_ucp")))
           (outcome (ucp-call ucp (%mcp-text (jget call "operation" "catalog.search"))
                              (%mcp-object-or-empty (jget call "payload"))
                              :idempotency-key "fixture-key")))
      (%assert-subset outcome (%mcp-object-or-empty (jget fixture "expected_ucp_outcome"))
                      "UCP outcome")
      (%assert-equal (jget outcome "idempotencyKey") "fixture-key" "UCP idempotency key"))
    (let ((state (execution-context-continuation-state context)))
      (%assert-equal (jget state "namespaces") names "continuation namespaces")
      (unless (plusp (length (%mcp-text (jget state "catalogFingerprint"))))
        (%fixture-fail "execution context continuation state has no catalog fingerprint"))
      ;; The fingerprint must be a function of the namespaces, not a nonce.
      (%assert-equal (jget (execution-context-continuation-state context) "catalogFingerprint")
                     (jget state "catalogFingerprint") "catalog fingerprint is stable")))
  :semantic)

(defun %run-tool-authorization (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (let* ((transport (make-mcp-scripted-transport
                              (%event-array (jget fixture "responses"))))
                  (observed (%new-array))
                  (client nil)
                  (result :null)
                  (error-text :null))
             (setf client
                   (apply #'make-mcp-client transport
                          (append
                           (loop for key in (%object-keys (%mcp-object-or-empty
                                                           (jget fixture "client_options")))
                                 append (list key (gethash key (%mcp-object-or-empty
                                                                 (jget fixture "client_options")))))
                           (list "authorizeToolCall"
                                 (lambda (call)
                                   (unless (eq (jget call "client") client)
                                     (%fixture-fail "authorization context lost client identity"))
                                   (let ((copy (object)))
                                     (dolist (key (%object-keys call))
                                       (unless (string= key "client")
                                         (%set-key copy key (gethash key call))))
                                     (vector-push-extend copy observed))
                                   (jget case "decision"))))))
             (mcp-init client)
             (handler-case (setf result (mcp-call-tool client (%mcp-text (jget case "name"))
                                                       (object "query" "REF-42")))
               (error (condition) (setf error-text (princ-to-string condition))))
             (%assert-equal error-text (jget case "expected_error") "authorization error")
             (when (eq error-text :null)
               (%assert-equal result (jget case "expected_result") "authorized tool result"))
             (%assert-equal (length observed) (jget case "expected_authorization_calls")
                            "authorization call count")
             (when (plusp (length observed))
               (%assert-equal (aref observed 0) (jget case "expected_context")
                              "authorization context"))
             (let ((sent (remove-if-not (lambda (request)
                                          (equal (jget request "method") "tools/call"))
                                        (coerce (mcp-scripted-requests transport) 'list))))
               (%assert-equal (length sent) (jget case "expected_tool_requests")
                              "tools/call request count")
               (dolist (request sent)
                 (let ((params (%mcp-object-or-empty (jget request "params"))))
                   (%assert-equal (jget params "name") (jget case "name") "authorized tool name")
                   (%assert-equal (jget params "arguments") (object "query" "REF-42")
                                  "authorized tool arguments"))))))
  :semantic)

(defun %run-tool-task-handling (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (let* ((transport (make-mcp-scripted-transport (%event-array (jget case "responses"))))
                  (options (axllm/core::core-map-merge
                            (%mcp-object-or-empty (jget fixture "client_options"))
                            (object "era" (jget case "era"))))
                  (client (apply #'make-mcp-client transport
                                 (loop for key in (%object-keys options)
                                       append (list key (gethash key options)))))
                  (api (%mcp-text (jget case "api")))
                  (result :null) (error-text :null))
             (mcp-init client)
             (let ((before (length (mcp-scripted-requests transport))))
               (handler-case
                   (setf result
                         (cond ((string= api "call_tool")
                                (mcp-call-tool client (%mcp-text (jget fixture "tool"))
                                               (jget fixture "arguments")))
                               ((string= api "call_tool_expose")
                                (mcp-call-tool client (%mcp-text (jget fixture "tool"))
                                               (jget fixture "arguments")
                                               :task-handling "expose"))
                               ((string= api "call_tool_outcome")
                                (mcp-call-tool-outcome client (%mcp-text (jget fixture "tool"))
                                                       (jget fixture "arguments")))
                               (t (%fixture-fail "unknown task-handling api ~a" api))))
                 (mcp-fixture-failure (condition) (error condition))
                 (mcp-error (condition) (setf error-text (princ-to-string condition))))
               (%assert-equal error-text (jget case "expected_error")
                              (format nil "~a error" (%mcp-text (jget case "name"))))
               (when (eq error-text :null)
                 (%assert-equal result (jget case "expected")
                                (format nil "~a result" (%mcp-text (jget case "name")))))
               (%assert-equal (%methods (subseq (mcp-scripted-requests transport) before))
                              (%event-array (jget case "expected_methods"))
                              (format nil "~a methods" (%mcp-text (jget case "name")))))))
  :semantic)

(defun %elicitation-recording-client (fixture)
  "A client whose elicitation handler records what the server asked for."
  (let* ((calls (%new-array))
         (transport (make-mcp-scripted-transport
                     (%event-array (let ((value (jget fixture "responses")))
                                     (if (eq value :null)
                                         (jget fixture "transport_responses")
                                         value)))))
         (options (axllm/core::core-map-merge
                   (%mcp-object-or-empty (jget fixture "client_options")) (object))))
    (%set-key options "elicitation"
              (lambda (params context)
                (when (axllm/core::core-true-p (jget params "fail"))
                  (%mcp-fail "fixture handler failed"))
                (vector-push-extend (object "params" params "context" context) calls)
                (%mcp-json-clone (%mcp-object-or-empty (jget fixture "elicitation_result")))))
    (values (apply #'make-mcp-client transport
                   (loop for key in (%object-keys options)
                         append (list key (gethash key options))))
            transport calls)))

(defun %run-mrtr-elicitation (fixture)
  (multiple-value-bind (client transport calls) (%elicitation-recording-client fixture)
    (mcp-init client)
    (let ((result (mcp-call-tool client "work" (object "value" 1))))
      (%assert-subset result (%mcp-object-or-empty (jget fixture "expected_result"))
                      "MRTR elicitation result"))
    (%assert-equal (length calls) 1 "MRTR elicitation handler count")
    (%assert-subset (jget (aref calls 0) "params")
                    (%mcp-object-or-empty (jget fixture "expected_elicitation_params"))
                    "MRTR elicitation params")
    (%assert-subset (jget (aref calls 0) "context")
                    (%mcp-object-or-empty (jget fixture "expected_context"))
                    "MRTR elicitation context")
    (let ((tool-calls (remove-if-not (lambda (request) (equal (jget request "method") "tools/call"))
                                     (coerce (mcp-scripted-requests transport) 'list))))
      (loop for expected across (%event-array (jget fixture "expected_call_params"))
            for index from 0
            do (%assert-subset (jget (nth index tool-calls) "params") expected
                               (format nil "MRTR call params ~a" index))))
    ;; The advertised capabilities must match the handlers actually installed.
    (let ((capabilities (%mcp-object-or-empty
                         (jget (%mcp-object-or-empty
                                (jget (%mcp-object-or-empty
                                       (jget (aref (mcp-scripted-requests transport) 0) "params"))
                                      "_meta"))
                               "io.modelcontextprotocol/clientCapabilities"))))
      (unless (%mcp-present-key-p capabilities "elicitation")
        (%fixture-fail "a client with an elicitation handler did not advertise it"))
      (when (%mcp-present-key-p capabilities "sampling")
        (%fixture-fail "a client with no sampling handler advertised sampling"))))
  ;; A truthy sampling option that is not a handler must be refused.
  (%expect-error "truthy sampling option" "sampling is not supported"
    (mcp-init (make-mcp-client (make-mcp-scripted-transport) :era "modern" :sampling true)))
  :semantic)

(defun %run-mrtr-roots (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (%assert-subset (mcp-call-tool client "work" (object "value" 1))
                    (%mcp-object-or-empty (jget fixture "expected_call_result")) "MRTR tool result")
    (%assert-subset (mcp-get-prompt client "ask" (object))
                    (%mcp-object-or-empty (jget fixture "expected_prompt_result"))
                    "MRTR prompt result")
    (%assert-subset (mcp-read-resource client "file:///resource")
                    (%mcp-object-or-empty (jget fixture "expected_resource_result"))
                    "MRTR resource result")
    (%assert-equal (%methods (mcp-scripted-requests transport))
                   (%event-array (jget fixture "expected_methods")) "MRTR request methods")
    (let ((tool-calls (remove-if-not (lambda (request) (equal (jget request "method") "tools/call"))
                                     (coerce (mcp-scripted-requests transport) 'list))))
      (let ((ids (mapcar (lambda (request) (%mcp-text (jget request "id"))) tool-calls)))
        (unless (= (length ids) (length (remove-duplicates ids :test #'string=)))
          (%fixture-fail "MRTR rounds reused a request id: ~a" ids)))
      (loop for expected across (%event-array (jget fixture "expected_tool_call_params"))
            for index from 0
            do (let ((params (%mcp-object-or-empty (jget (nth index tool-calls) "params"))))
                 (%assert-subset params expected (format nil "MRTR tool params ~a" index))
                 (let ((expected-responses (jget expected "inputResponses")))
                   (if (eq expected-responses :null)
                       (when (or (%mcp-present-key-p params "inputResponses")
                                 (%mcp-present-key-p params "requestState"))
                         (%fixture-fail "the initial MRTR request carried round state"))
                       ;; Stale responses from an earlier round must not ride along.
                       (%assert-equal (coerce (%object-keys (%mcp-object-or-empty
                                                             (jget params "inputResponses")))
                                              'vector)
                                      (coerce (%object-keys (%mcp-object-or-empty expected-responses))
                                              'vector)
                                      (format nil "MRTR round ~a input response keys" index))))
                 (when (and (not (%mcp-present-key-p expected "requestState"))
                            (%mcp-present-key-p params "requestState"))
                   (%fixture-fail "MRTR request retained a stale requestState"))))))
  :semantic)

(defun %run-tasks-v2-modern (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (%assert-subset (mcp-call-tool client "slow" (object))
                    (%mcp-object-or-empty (jget fixture "expected_call_result")) "task call result")
    (mcp-provide-task-input client "task-1" (object))
    (mcp-cancel-task client "task-1")
    (%expect-error "tasks/list on a modern server"
                   (%mcp-text (jget fixture "expected_list_error" ""))
      (mcp-list-tasks client))
    (%expect-error "tasks/result on a modern server"
                   (%mcp-text (jget fixture "expected_result_error" ""))
      (mcp-get-task-result client "task-1"))
    (%assert-equal (%methods (mcp-scripted-requests transport))
                   (%event-array (jget fixture "expected_methods")) "task request methods"))
  :semantic)

(defun %run-tasks-v2-input-required (fixture)
  (multiple-value-bind (client transport calls) (%elicitation-recording-client fixture)
    (mcp-init client)
    (%assert-subset (mcp-call-tool client "slow" (object))
                    (%mcp-object-or-empty (jget fixture "expected_result"))
                    "task input-required result")
    (%assert-equal (length calls) 1 "task elicitation handler count")
    (%assert-subset (jget (aref calls 0) "params")
                    (%mcp-object-or-empty (jget fixture "expected_elicitation_params"))
                    "task elicitation params")
    (%assert-subset (jget (aref calls 0) "context")
                    (%mcp-object-or-empty (jget fixture "expected_context"))
                    "task elicitation context")
    (let ((updates (remove-if-not (lambda (request) (equal (jget request "method") "tasks/update"))
                                  (coerce (mcp-scripted-requests transport) 'list))))
      (unless updates (%fixture-fail "no tasks/update was sent for an input-required task"))
      (%assert-subset (%mcp-object-or-empty (jget (first updates) "params"))
                      (%mcp-object-or-empty (jget fixture "expected_update_params"))
                      "task update params"))
    (%assert-equal (%methods (mcp-scripted-requests transport))
                   (%event-array (jget fixture "expected_methods"))
                   "task input-required methods"))
  :semantic)

(defun %run-server-requests-legacy (fixture)
  (multiple-value-bind (client transport calls) (%elicitation-recording-client fixture)
    (mcp-init client)
    (loop for request across (%event-array (jget fixture "server_requests"))
          do (mcp-scripted-emit transport request))
    (loop for expected across (%event-array (jget fixture "expected_responses"))
          for index from 0
          do (%assert-subset (aref (mcp-scripted-sent-responses transport) index) expected
                             (format nil "server response ~a" index)))
    (%assert-equal (length calls) 1 "legacy elicitation handler count")
    (%assert-subset (jget (aref calls 0) "params")
                    (%mcp-object-or-empty (jget fixture "expected_elicitation_params"))
                    "legacy elicitation params")
    (%assert-subset (jget (aref calls 0) "context")
                    (%mcp-object-or-empty (jget fixture "expected_context"))
                    "legacy elicitation context")
    (let ((initialize (find "initialize" (coerce (mcp-scripted-requests transport) 'list)
                            :key (lambda (request) (%mcp-text (jget request "method")))
                            :test #'string=)))
      (unless initialize (%fixture-fail "no initialize request was sent"))
      (%assert-subset (%mcp-object-or-empty
                       (jget (%mcp-object-or-empty (jget initialize "params")) "capabilities"))
                      (%mcp-object-or-empty (jget fixture "expected_legacy_capabilities"))
                      "legacy client capabilities")))
  :semantic)

(defun %run-server-requests-sampling (fixture)
  "Legacy inbound sampling: the handler answers a well-formed request, a
malformed one never reaches it, and a client with no handler neither
advertises sampling nor accepts it."
  (let* ((calls (%new-array))
         (transport (make-mcp-scripted-transport (%event-array (jget fixture "responses"))))
         (client (make-mcp-client transport
                                  "era" (jget (%mcp-object-or-empty
                                               (jget fixture "client_options")) "era")
                                  "sampling"
                                  (lambda (params context)
                                    (vector-push-extend (object "params" params
                                                                "context" context)
                                                        calls)
                                    (%mcp-json-clone
                                     (%mcp-object-or-empty (jget fixture "sampling_result")))))))
    (mcp-init client)
    (loop for request across (%event-array (jget fixture "server_requests"))
          do (mcp-scripted-emit transport request))
    (loop for expected across (%event-array (jget fixture "expected_responses"))
          for index from 0
          do (%assert-subset (aref (mcp-scripted-sent-responses transport) index) expected
                             (format nil "sampling response ~a" index)))
    (%assert-equal (length calls) (jget fixture "expected_handler_calls")
                   "sampling handler call count")
    (%assert-subset (jget (aref calls 0) "params")
                    (%mcp-object-or-empty (jget fixture "expected_handler_params"))
                    "sampling handler params")
    (%assert-subset (jget (aref calls 0) "context")
                    (%mcp-object-or-empty (jget fixture "expected_context"))
                    "sampling handler context")
    (let ((initialize (find "initialize" (coerce (mcp-scripted-requests transport) 'list)
                            :key (lambda (r) (%mcp-text (jget r "method"))) :test #'string=)))
      (%assert-subset (%mcp-object-or-empty
                       (jget (%mcp-object-or-empty (jget initialize "params")) "capabilities"))
                      (%mcp-object-or-empty (jget fixture "expected_capabilities"))
                      "advertised sampling capability")))
  (let* ((without (%mcp-object-or-empty (jget fixture "without_handler")))
         (transport (make-mcp-scripted-transport (%event-array (jget fixture "responses"))))
         (client (make-mcp-client transport "era" "legacy")))
    (mcp-init client)
    (mcp-scripted-emit transport (jget without "server_request"))
    (%assert-subset (aref (mcp-scripted-sent-responses transport) 0)
                    (%mcp-object-or-empty (jget without "expected_response"))
                    "sampling response without a handler")
    (let* ((initialize (find "initialize" (coerce (mcp-scripted-requests transport) 'list)
                             :key (lambda (r) (%mcp-text (jget r "method"))) :test #'string=))
           (capabilities (%mcp-object-or-empty
                          (jget (%mcp-object-or-empty (jget initialize "params"))
                                "capabilities"))))
      (loop for forbidden across (%event-array (jget without "forbidden_capabilities"))
            do (when (%mcp-present-key-p capabilities (%mcp-text forbidden))
                 (%fixture-fail "a client with no sampling handler advertised ~a"
                                (%mcp-text forbidden))))))
  ;; The three-argument contract the other generated ports call is unchanged.
  (loop for case across (%event-array (jget fixture "legacy_plan_cases"))
        do (%assert-subset (axllm/core::mcp-server-request-plan
                            (jget case "request") :null false)
                           (%mcp-object-or-empty (jget case "expected"))
                           "three-argument server request plan"))
  :semantic)

(defun %app-bridge-client (fixture)
  "An initialized client with the fixture's App-bearing tool catalog."
  (let* ((transport (make-mcp-scripted-transport (%event-array (jget fixture "responses"))))
         (options (%mcp-object-or-empty (jget fixture "client_options")))
         (client (apply #'make-mcp-client transport
                        (loop for key in (%object-keys options)
                              append (list key (gethash key options))))))
    (mcp-init client)
    (values client transport)))

(defun %app-bridge-with-read (fixture content)
  "A client whose next resources/read returns CONTENT."
  (multiple-value-bind (client transport) (%app-bridge-client fixture)
    (setf (%scripted-responses transport)
          (list (object "method" "resources/read"
                        "result" (object "contents" (vector content)))))
    (values client transport)))

(defun %run-app-bridge (fixture)
  "MCP Apps: resource policy, the visibility gate, and frame dispatch."
  (multiple-value-bind (client transport) (%app-bridge-client fixture)
    (declare (ignore transport))
    ;; Core decides which tools a frame may reach.
    (loop for case across (%event-array (jget fixture "visibility_cases"))
          do (let ((tool (find (%mcp-text (jget case "tool")) (mcp-client-tools client)
                               :key (lambda (item) (%mcp-text (jget item "name")))
                               :test #'string=)))
               (unless tool (%fixture-fail "no tool ~a in the catalog" (jget case "tool")))
               (%assert-equal (json-boolean (mcp-app-tool-visible-to
                                             tool (%mcp-text (jget case "principal"))))
                              (jget case "expected")
                              (format nil "~a visible to ~a"
                                      (%mcp-text (jget case "tool"))
                                      (%mcp-text (jget case "principal"))))))
    (let ((tool (find (%mcp-text (jget fixture "tool")) (mcp-client-tools client)
                      :key (lambda (item) (%mcp-text (jget item "name"))) :test #'string=)))
      (%assert-subset (mcp-app-tool-meta tool)
                      (%mcp-object-or-empty (jget fixture "expected_tool_meta"))
                      "App tool meta")))
  ;; A valid resource yields the sandbox payload, CSP and permission policy.
  (let ((read (%mcp-object-or-empty (jget fixture "resource_read"))))
    (multiple-value-bind (client transport)
        (%app-bridge-with-read fixture
                               (aref (%event-array
                                      (jget (%mcp-object-or-empty (jget read "result"))
                                            "contents"))
                                     0))
      (declare (ignore transport))
      (let ((bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool")))))
        (%assert-subset (mcp-app-bridge-load-resource bridge)
                        (%mcp-object-or-empty (jget fixture "expected_resource"))
                        "App resource"))))
  ;; Every refusal: MIME type, non-HTML body, and unsafe CSP sources.
  (loop for invalid across (%event-array (jget fixture "invalid_resources"))
        do (multiple-value-bind (client transport)
               (%app-bridge-with-read fixture (jget invalid "content"))
             (declare (ignore transport))
             (let ((bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool")))))
               (%expect-error (format nil "invalid App resource: ~a"
                                      (%mcp-text (jget invalid "note")))
                              (%mcp-text (jget invalid "expected_error_contains"))
                 (mcp-app-bridge-load-resource bridge)))))
  ;; A base64 blob body decodes to the same HTML a text body would carry.
  (let ((blob (%mcp-object-or-empty (jget fixture "blob_resource"))))
    (multiple-value-bind (client transport)
        (%app-bridge-with-read fixture (jget blob "content"))
      (declare (ignore transport))
      (let ((bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool")))))
        (%assert-equal (jget (mcp-app-bridge-load-resource bridge) "html")
                       (jget blob "expected_html") "App blob resource HTML"))))
  ;; Nothing works before the frame initializes.
  (loop for case across (%event-array (jget fixture "pre_initialize_cases"))
        do (multiple-value-bind (client transport) (%app-bridge-client fixture)
             (declare (ignore transport))
             (let ((bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool")))))
               (let ((expected (jget case "expected_response")))
                 (if (hash-table-p expected)
                     (%assert-subset (mcp-app-bridge-handle-view-message
                                      bridge (jget case "message"))
                                     expected "pre-initialize response")
                     (%expect-error "pre-initialize notification"
                                    (%mcp-text (jget case "expected_error_contains"))
                       (mcp-app-bridge-handle-view-message bridge (jget case "message"))))))))
  ;; The dispatch cases, each on its own bridge so state cannot leak.
  (loop for case across (%event-array (jget fixture "request_cases"))
        do (let ((opened (%new-array)) (updates (%new-array)))
             (multiple-value-bind (client transport) (%app-bridge-client fixture)
               (let ((response (jget case "response")))
                 (when (hash-table-p response)
                   (setf (%scripted-responses transport) (list response))))
               (let ((bridge (make-mcp-app-bridge
                              client (%mcp-text (jget fixture "tool"))
                              "sendToView" (lambda (message) (declare (ignore message)) nil)
                              "openLink" (lambda (url) (vector-push-extend url opened))
                              "updateModelContext"
                              (lambda (update) (vector-push-extend update updates))
                              "requestDisplayMode" (lambda (mode) (declare (ignore mode)) "inline"))))
                 ;; ui/initialize is the one request allowed before the
                 ;; frame has sent its initialized notification.
                 (%assert-subset (mcp-app-bridge-handle-view-message
                                  bridge (jget fixture "initialize_message"))
                                 (%mcp-object-or-empty
                                  (jget fixture "expected_initialize_response"))
                                 "ui/initialize response")
                 (mcp-app-bridge-handle-view-message
                  bridge (object "jsonrpc" "2.0" "method" "ui/notifications/initialized"))
                 (%assert-subset (mcp-app-bridge-handle-view-message
                                  bridge (jget case "message"))
                                 (%mcp-object-or-empty (jget case "expected_response"))
                                 (format nil "App request: ~a" (%mcp-text (jget case "note"))))
                 (let ((expected-calls (jget case "expected_tool_requests")))
                   (unless (eq expected-calls :null)
                     (%assert-equal (count "tools/call"
                                           (coerce (mcp-scripted-requests transport) 'list)
                                           :key (lambda (r) (%mcp-text (jget r "method")))
                                           :test #'string=)
                                    expected-calls "App tools/call wire count")))
                 (let ((expected-request (jget case "expected_tool_request")))
                   (when (hash-table-p expected-request)
                     (let ((sent (find "tools/call"
                                       (coerce (mcp-scripted-requests transport) 'list)
                                       :key (lambda (r) (%mcp-text (jget r "method")))
                                       :test #'string=)))
                       (unless sent (%fixture-fail "no tools/call reached the transport"))
                       (%assert-subset (%mcp-object-or-empty (jget sent "params"))
                                       expected-request "App tool call params"))))
                 (let ((expected-opens (jget case "expected_open_links")))
                   (unless (eq expected-opens :null)
                     (%assert-equal (length opened) expected-opens "App open-link count")))
                 (let ((expected-url (jget case "expected_opened_url")))
                   (unless (eq expected-url :null)
                     (%assert-equal (aref opened 0) expected-url "App opened URL")))
                 (let ((expected-update (jget case "expected_model_context_update")))
                   (when (hash-table-p expected-update)
                     (%assert-equal (length updates) 1 "App model-context update count")
                     ;; Untrusted and the source are Core's stamp, not the frame's.
                     (%assert-subset (aref updates 0) expected-update
                                     "App model-context update")))))))
  ;; A host that installed no callback must report the feature disabled.
  (loop for case across (%event-array (jget fixture "disabled_cases"))
        do (multiple-value-bind (client transport) (%app-bridge-client fixture)
             (declare (ignore transport))
             (let ((bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool")))))
               (mcp-app-bridge-handle-view-message
                bridge (object "jsonrpc" "2.0" "method" "ui/notifications/initialized"))
               (%assert-subset (mcp-app-bridge-handle-view-message
                                bridge (jget case "message"))
                               (%mcp-object-or-empty (jget case "expected_response"))
                               (format nil "disabled: ~a" (%mcp-text (jget case "note")))))))
  ;; Notifications: reserved sandbox names, sizes, and a rejected bad size.
  (multiple-value-bind (client transport) (%app-bridge-client fixture)
    (declare (ignore transport))
    (let* ((sizes (%new-array))
           (bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool"))
                                        "sizeChanged"
                                        (lambda (size) (vector-push-extend size sizes)))))
      (mcp-app-bridge-handle-view-message
       bridge (object "jsonrpc" "2.0" "method" "ui/notifications/initialized"))
      (let ((reserved (%mcp-object-or-empty (jget fixture "reserved_notification"))))
        (%expect-error "reserved sandbox notification"
                       (%mcp-text (jget reserved "expected_error_contains"))
          (mcp-app-bridge-handle-view-message bridge (jget reserved "message"))))
      (let ((sized (%mcp-object-or-empty (jget fixture "size_notification"))))
        (mcp-app-bridge-handle-view-message bridge (jget sized "message"))
        (%assert-equal (length sizes) 1 "App size-changed count")
        (%assert-subset (aref sizes 0) (%mcp-object-or-empty (jget sized "expected_size"))
                        "App size-changed payload"))
      (let ((invalid (%mcp-object-or-empty (jget fixture "invalid_size_notification"))))
        (mcp-app-bridge-handle-view-message bridge (jget invalid "message"))
        ;; A non-numeric width must not reach the host callback at all.
        (%assert-equal (length sizes) (+ 1 (jget invalid "expected_sizes"))
                       "App invalid size-changed was ignored"))))
  ;; The host authorization hook runs before any effect.
  (let ((denied (%mcp-object-or-empty (jget fixture "authorize_denied"))))
    (multiple-value-bind (client transport) (%app-bridge-client fixture)
      (let ((bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool"))
                                         "authorize" (lambda (action)
                                                       (declare (ignore action))
                                                       false))))
        (mcp-app-bridge-handle-view-message
         bridge (object "jsonrpc" "2.0" "method" "ui/notifications/initialized"))
        (%assert-subset (mcp-app-bridge-handle-view-message bridge (jget denied "message"))
                        (%mcp-object-or-empty (jget denied "expected_response"))
                        "App authorization denial")
        (%assert-equal (count "tools/call" (coerce (mcp-scripted-requests transport) 'list)
                              :key (lambda (r) (%mcp-text (jget r "method"))) :test #'string=)
                       (jget denied "expected_tool_requests")
                       "a denied App call reached the wire"))))
  ;; Outbound notifications and teardown.
  (multiple-value-bind (client transport) (%app-bridge-client fixture)
    (declare (ignore transport))
    (let* ((sent (%new-array))
           (bridge (make-mcp-app-bridge client (%mcp-text (jget fixture "tool"))
                                        "sendToView"
                                        (lambda (message) (vector-push-extend message sent)))))
      ;; Before initialization a notification must not be sent at all.
      (%expect-error "notify before initialization" "not initialized"
        (mcp-app-bridge-notify-tool-input bridge (object "item" "sku-2")))
      (mcp-app-bridge-handle-view-message
       bridge (object "jsonrpc" "2.0" "method" "ui/notifications/initialized"))
      (mcp-app-bridge-notify-tool-input bridge (object "item" "sku-2"))
      (mcp-app-bridge-notify-tool-result bridge (object "structuredContent"
                                                        (object "picked" "sku-2")))
      (loop for expected across (%event-array (jget fixture "expected_notifications"))
            for index from 0
            do (%assert-subset (aref sent index) expected
                               (format nil "App notification ~a" index)))
      (mcp-app-bridge-teardown bridge (%mcp-text (jget fixture "teardown_reason")))
      (%assert-subset (aref sent (1- (length sent)))
                      (%mcp-object-or-empty (jget fixture "expected_teardown"))
                      "App teardown")
      ;; Teardown requires a fresh initialization.
      (when (mcp-app-bridge-initialized-p bridge)
        (%fixture-fail "teardown left the App bridge initialized"))))
  :semantic)

(defun %run-subscriptions-listen (fixture)
  (loop for case across (%event-array (jget fixture "semantic_cases"))
        do (%assert-equal (axllm/core::mcp-listen-interests
                           (%event-array (jget case "subscribed_uris"))
                           (%mcp-object-or-empty (jget case "filters"))
                           (jget case "task_ids"))
                          (%mcp-object-or-empty (jget case "expected")) "listen interests"))
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (let ((delivered (%new-array)))
      (mcp-add-notification-listener client (lambda (message)
                                              (vector-push-extend message delivered)))
      (mcp-start-listening client)
      (%assert-equal (length (mcp-scripted-request-streams transport)) 1
                     "initial subscriptions/listen stream count")
      (let ((first-stream (aref (mcp-scripted-request-streams transport) 0)))
        (%assert-subset (%mcp-object-or-empty
                         (jget (%mcp-object-or-empty (jget first-stream "params")) "notifications"))
                        (%mcp-object-or-empty (jget fixture "expected_first_notifications"))
                        "initial listen interests")
        (mcp-acquire-resource-subscription client (%mcp-text (jget fixture "uri" "")) "fixture")
        (%assert-equal (length (mcp-scripted-request-streams transport))
                       (jget fixture "expected_stream_count")
                       "stream count after an interest change")
        (let ((second (aref (mcp-scripted-request-streams transport)
                            (1- (length (mcp-scripted-request-streams transport))))))
          (when (%same (jget first-stream "id") (jget second "id"))
            (%fixture-fail "the restarted subscriptions/listen reused its request id"))
          (%assert-subset (%mcp-object-or-empty
                           (jget (%mcp-object-or-empty (jget second "params")) "notifications"))
                          (%mcp-object-or-empty (jget fixture "expected_second_notifications"))
                          "updated listen interests")
          (flet ((updates ()
                   (count "notifications/resources/updated" (coerce delivered 'list)
                          :key (lambda (item) (%mcp-text (jget item "method")))
                          :test #'string=)))
            (let* ((before (updates))
                   (notification (%mcp-json-clone (%mcp-object-or-empty
                                               (jget fixture "delivered_notification")))))
              (unless (%mcp-present-key-p notification "params")
                (%set-key notification "params" (object)))
              ;; A notification tagged with somebody else's subscription id
              ;; must not be delivered to this client's listeners.
              (%set-key (jget notification "params") "_meta"
                        (object "io.modelcontextprotocol/subscriptionId" "other"))
              (mcp-scripted-emit transport notification)
              (unless (= (updates) before)
                (%fixture-fail "a cross-subscription notification was delivered"))
              (%set-key (jget notification "params") "_meta"
                        (object "io.modelcontextprotocol/subscriptionId" (jget second "id")))
              (mcp-scripted-emit transport notification)
              (unless (= (updates) (1+ before))
                (%fixture-fail "the active subscription's notification was not delivered"))
              (let ((last (find "notifications/resources/updated" (reverse (coerce delivered 'list))
                                :key (lambda (item) (%mcp-text (jget item "method")))
                                :test #'string=)))
                (when (%mcp-present-key-p (%mcp-object-or-empty (jget last "params")) "_meta")
                  (%fixture-fail "the delivered notification still carried _meta")))))))
      (let ((methods (coerce (%methods (mcp-scripted-requests transport)) 'list)))
        (loop for forbidden across (%event-array (jget fixture "expected_forbidden_methods"))
              do (when (member (%mcp-text forbidden) methods
                               :key #'%mcp-text :test #'string=)
                   (%fixture-fail "a modern subscription emitted the legacy method ~a"
                                  (%mcp-text forbidden)))))))
  :semantic)

(defun %run-task-listen-restart (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (let ((delivered (%new-array)))
      (mcp-add-notification-listener client (lambda (message)
                                              (vector-push-extend message delivered)))
      (mcp-start-listening client)
      (%assert-equal (length (mcp-scripted-request-streams transport)) 1
                     "initial subscriptions/listen stream count")
      (when (%mcp-present-key-p (%mcp-object-or-empty
                             (jget (%mcp-object-or-empty
                                    (jget (aref (mcp-scripted-request-streams transport) 0)
                                          "params"))
                                   "notifications"))
                            "taskIds")
        (%fixture-fail "the listener asked for task updates before any task existed"))
      (let ((outcome (mcp-call-tool-outcome client (%mcp-text (jget fixture "tool"))
                                            (%mcp-object-or-empty (jget fixture "arguments")))))
        (%assert-equal (jget outcome "kind") "task" "tool outcome kind"))
      (%assert-equal (length (mcp-scripted-request-streams transport)) 2
                     "stream count after a task was recorded")
      (let ((second (aref (mcp-scripted-request-streams transport) 1)))
        (%assert-subset (%mcp-object-or-empty
                         (jget (%mcp-object-or-empty (jget second "params")) "notifications"))
                        (jget fixture "expected_second_notifications") "task listen interests")
        (let ((notification (%mcp-json-clone (jget fixture "task_notification"))))
          (unless (%mcp-present-key-p notification "params")
            (%set-key notification "params" (object)))
          (%set-key (jget notification "params") "_meta"
                    (object "io.modelcontextprotocol/subscriptionId" (jget second "id")))
          (mcp-scripted-emit transport notification)
          (%assert-equal (count "notifications/tasks" (coerce delivered 'list)
                                :key (lambda (item) (%mcp-text (jget item "method")))
                                :test #'string=)
                         1 "task notification delivery count")
          ;; A task we already knew about must not restart the listener again.
          (%assert-equal (length (mcp-scripted-request-streams transport)) 2
                         "stream count after a known task")))))
  :semantic)

(defun %run-read-cache (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (let ((catalog-methods '("resources/list" "resources/templates/list")))
      (mcp-refresh client :force nil)
      (%assert-equal (count-if (lambda (request)
                                 (member (%mcp-text (jget request "method")) catalog-methods
                                         :test #'string=))
                               (coerce (mcp-scripted-requests transport) 'list))
                     (jget fixture "expected_catalog_requests_after_fresh_refresh")
                     "catalog requests after a non-forced refresh"))
    (let ((uri (%mcp-text (jget fixture "uri" ""))))
      (%assert-subset (mcp-read-resource client uri)
                      (%mcp-object-or-empty (jget fixture "expected_first")) "first resource read")
      (%assert-subset (mcp-read-resource client uri)
                      (%mcp-object-or-empty (jget fixture "expected_first")) "cached resource read")
      ;; An update notification must invalidate the cached read.
      (mcp-scripted-emit transport (%mcp-object-or-empty (jget fixture "notification")))
      (%assert-subset (mcp-read-resource client uri)
                      (%mcp-object-or-empty (jget fixture "expected_after_update"))
                      "resource read after update")
      (%assert-equal (count "resources/read" (coerce (mcp-scripted-requests transport) 'list)
                            :key (lambda (request) (%mcp-text (jget request "method")))
                            :test #'string=)
                     (jget fixture "expected_read_requests") "resources/read request count")))
  :semantic)

(defun %run-client-discovery (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (let ((call (jget fixture "call_tool")))
      (when (hash-table-p call)
        (%assert-subset (mcp-call-tool client (%mcp-text (jget call "name"))
                                       (%mcp-object-or-empty (jget call "arguments")))
                        (%mcp-object-or-empty (jget fixture "expected_call_result")) "tool result")))
    (%assert-equal (mcp-get-era client) (jget fixture "expected_era") "classified era")
    (let ((expected-version (jget fixture "expected_protocol_version")))
      (unless (eq expected-version :null)
        (%assert-equal (mcp-negotiated-protocol-version client) expected-version
                       "negotiated protocol version")))
    (%assert-catalog-names (mcp-client-tools client) (jget fixture "expected_tool_names")
                           "tool names")
    (let ((expected-info (jget fixture "expected_server_info")))
      (when (hash-table-p expected-info)
        (%assert-subset (%mcp-object-or-empty (mcp-server-info client)) expected-info "server info")))
    (%assert-requests (mcp-scripted-requests transport) fixture)
    (loop for subset across (%event-array (jget fixture "expected_request_headers"))
          for index from 0
          do (%assert-subset (aref (mcp-scripted-request-headers transport) index) subset
                             (format nil "request headers ~a" index)))
    (let ((emitted (append (coerce (%methods (mcp-scripted-requests transport)) 'list)
                           (coerce (%methods (mcp-scripted-notifications transport)) 'list))))
      (loop for forbidden across (%event-array (jget fixture "forbidden_methods"))
            do (when (member (%mcp-text forbidden) emitted :key #'%mcp-text :test #'string=)
                 (%fixture-fail "forbidden method emitted: ~a" (%mcp-text forbidden)))))
    (let ((expected-notifications (jget fixture "expected_notification_methods")))
      (unless (eq expected-notifications :null)
        (%assert-equal (%methods (mcp-scripted-notifications transport)) expected-notifications
                       "notification methods"))))
  :semantic)

(defun %run-protocol-negotiation (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (declare (ignore transport))
    (let ((expected-error (jget fixture "expected_error_contains")))
      (if (stringp expected-error)
          (%expect-error "protocol negotiation" expected-error (mcp-init client))
          (progn (mcp-init client)
                 (%assert-equal (mcp-negotiated-protocol-version client)
                                (jget fixture "expected_protocol_version")
                                "negotiated protocol version")))))
  :validation-error)

(defun %run-initialize (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (%assert-equal (mcp-negotiated-protocol-version client)
                   (jget fixture "expected_protocol_version") "negotiated protocol version")
    (%assert-requests (mcp-scripted-requests transport) fixture))
  :semantic)

(defun %run-ping (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (mcp-ping client)
    (%assert-requests (mcp-scripted-requests transport) fixture))
  :semantic)

(defun %run-tools (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (let* ((tools (mcp-native-tools client))
           (schemas (%mcp-object-or-empty (jget fixture "expected_schemas"))))
      ;; The server owns its schemas; a native tool must not rewrite one.
      (dolist (spec tools)
        (let ((name (%mcp-text (native-tool-name spec))))
          (when (%mcp-present-key-p schemas name)
            (%assert-equal (native-tool-parameters spec) (gethash name schemas)
                           (format nil "native tool ~a schema" name)))))
      (let ((expected-names (jget fixture "expected_function_names")))
        (unless (eq expected-names :null)
          (%assert-equal (coerce (mapcar #'native-tool-name tools) 'vector) expected-names
                         "function names")))
      (let ((call (jget fixture "call_function")))
        (when (hash-table-p call)
          (let ((spec (find (%mcp-text (jget call "name")) tools
                            :key (lambda (item) (%mcp-text (native-tool-name item)))
                            :test #'string=)))
            (unless spec (%fixture-fail "no native tool named ~a" (%mcp-text (jget call "name"))))
            (%assert-subset (native-tool-call spec (%mcp-object-or-empty (jget call "arguments")))
                            (%mcp-object-or-empty (jget fixture "expected_call_result"))
                            "tool result")))))
    (%assert-requests (mcp-scripted-requests transport) fixture))
  :semantic)

(defun %run-prompts-resources (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (declare (ignore transport))
    (mcp-init client)
    (%assert-catalog-names (mcp-client-prompts client) (jget fixture "expected_prompt_names")
                           "prompt names")
    (%assert-catalog-names (mcp-client-resources client) (jget fixture "expected_resource_names")
                           "resource names")
    (%assert-catalog-names (mcp-client-resource-templates client)
                           (jget fixture "expected_resource_template_names")
                           "resource template names"))
  :semantic)

(defun %run-roots-notifications (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (mcp-scripted-emit transport (object "jsonrpc" "2.0" "id" "server-1" "method" "roots/list"))
    (let ((expected (jget fixture "expected_roots_response")))
      (when (hash-table-p expected)
        (%assert-subset (aref (mcp-scripted-sent-responses transport) 0) expected
                        "roots response"))))
  :semantic)

(defun %run-cancellation (fixture)
  (multiple-value-bind (client transport) (%fixture-client fixture)
    (mcp-init client)
    (mcp-cancel-request client (jget fixture "request_id" "1") (jget fixture "reason" "cancelled"))
    (%assert-subset (aref (mcp-scripted-notifications transport)
                          (1- (length (mcp-scripted-notifications transport))))
                    (%mcp-object-or-empty (jget fixture "expected_notification"))
                    "cancel notification"))
  :semantic)

(defun %run-inheritance-agent-context (fixture)
  "An agent over a derived execution context reaches exactly the inherited
clients, through its own runtime callables.

This is the live-context test: the agent is built with the DERIVED context,
so a namespace the child did not inherit must not be reachable as
mcp.<namespace>.tools.<tool>, and the agent's own inline function must stay
reachable either way. Every protocol call is checked at the transport, so a
callable that silently returned without touching the wire would fail.

The arm first probes whether an agent can be built in this image at all. The
agent's actor stage declares a `code' output field, and the Lisp signature
subset does not yet support that field type, so construction fails before
any MCP work happens. That blocker belongs to the signature/agent surface,
not to MCP, so it is reported as an unclaimed fixture naming the exact
missing piece. The gate still fails on it: an unclaimed fixture is an
incomplete claim. Every other failure stays a hard failure."
  (handler-case (agent "question:string -> answer:string"
                       :options (%mcp-object-or-empty (jget fixture "agent_options")))
    (error (condition)
      (let ((text (princ-to-string condition)))
        (when (search "Unsupported output field type" text)
          (return-from %run-inheritance-agent-context
            (values :explicitly-not-claimed
                    (format nil "agent construction needs a signature field type this subset lacks: ~a"
                            text)))))))
  (loop for case across (%event-array (jget fixture "cases"))
        do (let ((clients '()) (transports (object)) (results (%new-array))
                 (selected (%new-array)) (error-text :null))
             (loop for spec across (%event-array (jget fixture "clients"))
                   do (let ((transport (make-mcp-scripted-transport
                                        (%event-array (jget spec "responses")))))
                        (%set-key transports (%mcp-text (jget spec "namespace")) transport)
                        (push (make-mcp-client transport
                                               "namespace" (%mcp-text (jget spec "namespace"))
                                               "era" "modern")
                              clients)))
             (setf clients (nreverse clients))
             (let ((context (make-execution-context :mcp clients)))
               (handler-case
                   (let* ((child (execution-context-derive context (jget case "inheritance")))
                          (options (axllm/core::core-map-merge
                                    (%mcp-object-or-empty (jget fixture "agent_options"))
                                    (object))))
                     (dolist (client (execution-context-mcp child))
                       (vector-push-extend (mcp-namespace client) selected))
                     (%set-key options "executionContext" child)
                     (let ((program (agent "question:string -> answer:string"
                                           :options options)))
                       ;; The agent's own inline function is unaffected by MCP
                       ;; inheritance and must still answer.
                       (%assert-equal (agent-invoke-callable program "tools.local_echo"
                                                             :args (object))
                                      (jget fixture "expected_local_result")
                                      "agent local callable result")
                       (dolist (client (execution-context-mcp child))
                         (dolist (spec (mcp-native-tools client))
                           (let ((result (agent-invoke-callable
                                          program
                                          (format nil "mcp.~a.tools.~a"
                                                  (mcp-namespace client)
                                                  (%mcp-text (native-tool-name spec)))
                                          :args (object "query" "scope-probe"))))
                             (%assert-equal (jget result "status") "ok"
                                            "agent MCP callable status")
                             (vector-push-extend (jget result "value") results))))))
                 (error (condition) (setf error-text (princ-to-string condition))))
               (%assert-equal error-text (jget case "expected_error") "agent inheritance error")
               (when (eq error-text :null)
                 (%assert-equal selected (%event-array (jget case "expected_namespaces"))
                                "agent inherited namespaces"))
               (%assert-equal results (%event-array (jget case "expected_results"))
                              "agent callable results")
               (dolist (namespace (%object-keys transports))
                 (let ((transport (gethash namespace transports)))
                   (%assert-equal (%methods (mcp-scripted-requests transport))
                                  (%event-array (jget (%mcp-object-or-empty
                                                       (jget case "expected_methods"))
                                                      namespace))
                                  (format nil "~a agent methods" namespace))
                   (loop for request across (mcp-scripted-requests transport)
                         do (when (equal (jget request "method") "tools/call")
                              (%assert-equal (jget (%mcp-object-or-empty (jget request "params"))
                                                   "arguments")
                                             (object "query" "scope-probe")
                                             "agent tool arguments")))))
               ;; The parent context keeps every client the child gave up.
               (let ((parent-results (%new-array)))
                 (dolist (spec (execution-context-native-tools context))
                   (vector-push-extend (native-tool-call spec (object "query" "parent-probe"))
                                       parent-results))
                 (%assert-equal parent-results
                                (%event-array (jget case "expected_parent_results"))
                                "agent parent tool results")))))
  :semantic)

;;; ------------------------------------------------------------------
;;; Dispatch
;;; ------------------------------------------------------------------

(defparameter +mcp-conformance-operations+
  '(("app_bridge" . %run-app-bridge)
    ("cache_fold" . %run-cache-fold)
    ("cancellation" . %run-cancellation)
    ("client_discovery" . %run-client-discovery)
    ("discover" . %run-discover)
    ("era_classification" . %run-era-classification)
    ("execution_context_ucp" . %run-execution-context-ucp)
    ("extension_negotiation" . %run-extension-negotiation)
    ("header_value" . %run-header-value)
    ("http_session_headers" . %run-http-session-headers)
    ("inheritance_agent_context" . %run-inheritance-agent-context)
    ("inheritance_context" . %run-inheritance-context)
    ("inheritance_plan" . %run-inheritance-plan)
    ("initialize" . %run-initialize)
    ("modern_headers" . %run-modern-headers)
    ("modern_transport_headers" . %run-modern-transport-headers)
    ("mrtr_elicitation" . %run-mrtr-elicitation)
    ("mrtr_roots" . %run-mrtr-roots)
    ("mrtr_violations" . %run-mrtr-violations)
    ("mutual_version" . %run-mutual-version)
    ("oauth" . %run-oauth)
    ("oauth_as_metadata" . %run-oauth-as-metadata)
    ("oauth_discovery" . %run-oauth-discovery)
    ("oauth_issuer" . %run-oauth-issuer)
    ("oauth_token" . %run-oauth-token)
    ("param_headers" . %run-param-headers)
    ("ping" . %run-ping)
    ("prompts_resources" . %run-prompts-resources)
    ("protocol_negotiation" . %run-protocol-negotiation)
    ("read_cache" . %run-read-cache)
    ("request_meta" . %run-request-meta)
    ("roots_notifications" . %run-roots-notifications)
    ("server_requests_legacy" . %run-server-requests-legacy)
    ("server_requests_sampling" . %run-server-requests-sampling)
    ("ssrf" . %run-ssrf)
    ("stdio_framing" . %run-stdio-framing)
    ("subscriptions_listen" . %run-subscriptions-listen)
    ("task_listen_restart" . %run-task-listen-restart)
    ("tasks_v2_input_required" . %run-tasks-v2-input-required)
    ("tasks_v2_modern" . %run-tasks-v2-modern)
    ("tasks_v2_violations" . %run-tasks-v2-violations)
    ("tool_authorization" . %run-tool-authorization)
    ("tool_task_handling" . %run-tool-task-handling)
    ("tools" . %run-tools))
  "Every axmcp operation and the arm that runs it.

There is no catch-all. A fixture whose operation is absent fails naming the
operation, so a new fixture kind cannot pass by default.")

(defun %mcp-fixture-error-matches-p (condition expected-error)
  "Whether CONDITION is the implementation error EXPECTED-ERROR describes.

A MCP-FIXTURE-FAILURE never matches, whatever its text. It is this harness's
own assertion failure, and %EXPECT-ERROR quotes the fragment it was looking
for into both of its messages -- \"X was accepted; expected a failure
containing \\\"frag\\\"\" and \"X failed with \\\"other\\\", expected it to contain
\\\"frag\\\"\". Matching those on text reported a fixture as a passing
validation-error when the implementation had returned successfully, or had
raised a completely unrelated error. That is the precise false green this
runner exists to prevent, so the exclusion is by condition type rather than
by trying to recognise the wording."
  (and (stringp expected-error)
       (not (typep condition 'mcp-fixture-failure))
       (search expected-error (princ-to-string condition))))

(defun %run-mcp-fixture (fixture)
  "Run one fixture. Returns (values classification note)."
  (let* ((operation (%mcp-text (jget fixture "operation" "initialize")))
         (arm (cdr (assoc operation +mcp-conformance-operations+ :test #'string=)))
         (expected-error (jget fixture "expected_error_contains")))
    (unless arm
      (%fixture-fail "unsupported MCP conformance operation ~a" operation))
    (handler-case
        (multiple-value-bind (classification note) (funcall arm fixture)
          ;; An arm that returns normally while the fixture records an
          ;; expected error has not proved the fixture. Arms that handle the
          ;; expectation themselves say so by classifying it
          ;; :VALIDATION-ERROR; anything else returning normally here means
          ;; the implementation accepted input the fixture says it must
          ;; reject, which is a failure and not a pass under another name.
          (when (and (stringp expected-error)
                     (not (member classification
                                  '(:validation-error :explicitly-not-claimed))))
            (%fixture-fail
             "the operation succeeded (classified ~a) but the fixture requires a failure containing ~s"
             classification expected-error))
          (values classification note))
      (error (condition)
        (if (%mcp-fixture-error-matches-p condition expected-error)
            (values :validation-error nil)
            (error condition))))))

(defparameter +mcp-harness-selftests+
  '(("a fixture whose implementation raises the expected error passes"
     "right-error" :validation-error)
    ("a fixture whose implementation raises nothing must NOT pass"
     "no-error" :failed)
    ("a fixture whose implementation raises an unrelated error must NOT pass"
     "wrong-error" :failed)
    ("a fixture whose assertion fails inside %expect-error must NOT pass"
     "assertion-failure" :failed))
  "Negative tests for the harness itself: (label behaviour expected-outcome).")

(defun %mcp-harness-selftest-arm (fixture)
  "A controllable arm, used only by the harness self-tests."
  (let ((behaviour (%mcp-text (jget fixture "behaviour"))))
    (cond ((string= behaviour "right-error")
           (%mcp-fail "the server rejected the protocol version"))
          ((string= behaviour "wrong-error")
           (%mcp-fail "something completely unrelated went wrong"))
          ((string= behaviour "no-error")
           ;; An implementation that accepted what the fixture says it must
           ;; reject, classified as an ordinary semantic pass.
           :semantic)
          ((string= behaviour "assertion-failure")
           ;; Exactly what %EXPECT-ERROR raises when the implementation did
           ;; not fail: a harness assertion quoting the expected fragment.
           (%expect-error "a call that should have failed"
                          "the server rejected the protocol version"
             :this-value-is-returned-successfully))
          (t :validation-error))))

(defun run-mcp-harness-selftests (&key (stream *standard-output*))
  "Check that the fixture runner fails the fixtures it must fail.

The runner classifies a fixture by catching its arm's error and matching the
fixture's expected_error_contains against the message. That shape can report
a false green, because the harness's own assertion failures quote the
fragment they were looking for. These tests drive the real dispatch path
with a controlled arm and assert the outcome, so a regression in
%MCP-FIXTURE-ERROR-MATCHES-P fails here rather than silently turning
negative fixtures green.

Returns (values passed failed)."
  (let ((passed 0) (failed 0))
    (dolist (entry +mcp-harness-selftests+)
      (destructuring-bind (label behaviour expected) entry
        (let* ((+mcp-conformance-operations+
                 (cons (cons "harness-selftest" #'%mcp-harness-selftest-arm)
                       +mcp-conformance-operations+))
               (fixture (object "operation" "harness-selftest"
                                "behaviour" behaviour
                                "expected_error_contains"
                                "the server rejected the protocol version"))
               (outcome (handler-case (values (%run-mcp-fixture fixture))
                          (error () :failed))))
          (if (eq outcome expected)
              (incf passed)
              (progn (incf failed)
                     (format stream "~&  FAIL harness: ~a~%    expected ~a, got ~a~%"
                             label expected outcome))))))
    (format stream "~&mcp harness selftests: ~a passed, ~a failed~%" passed failed)
    (values passed failed)))

(defun run-mcp-conformance-tests (&key (stream *standard-output*))
  "Run every ir/conformance/axmcp fixture.

Returns (values passed failed coverage unclaimed).

PASSED counts only fixtures that executed an implementation path and
matched their recorded expectation. A fixture classified
:EXPLICITLY-NOT-CLAIMED is NOT passed: it executed nothing, so counting it
as a pass would be the exact dishonesty this runner exists to prevent. It
is counted in UNCLAIMED instead, and RUN-MCP-CONFORMANCE-TESTS-OR-DIE
treats a non-zero UNCLAIMED as a failed gate.

COVERAGE is a JSON object from fixture file name to its classification, so a
caller can write conformance-coverage.json without re-deriving what each
fixture proved."
  (let ((files (%fixture-files "axmcp"))
        (passed 0) (failed 0) (unclaimed 0)
        (coverage (object)))
    (when (null files)
      (format stream "~&No axmcp fixtures found under ~a~%" (mcp-conformance-directory "axmcp"))
      (return-from run-mcp-conformance-tests (values 0 1 coverage 0)))
    (dolist (path files)
      (let* ((fixture (%read-fixture path))
             (name (%mcp-text (jget fixture "name" (pathname-name path)))))
        (handler-case
            (multiple-value-bind (classification note) (%run-mcp-fixture fixture)
              (if (eq classification :explicitly-not-claimed)
                  (progn (incf unclaimed)
                         (format stream "~&  NOT CLAIMED ~a (~a): ~a~%"
                                 name (pathname-name path) note))
                  (progn
                    ;; The receipt records the fixture's DISK name, and only
                    ;; after a dispatch that actually succeeded. It is inside
                    ;; this branch rather than after the HANDLER-CASE so that
                    ;; a failure or an unclaimed fixture can never contribute
                    ;; one, and a no-op until the full gate enables it.
                    (axllm/conformance:record-result "axmcp" path classification)
                    (incf passed)))
              (%set-key coverage (pathname-name path)
                        (object "operation" (jget fixture "operation" "initialize")
                                "classification" (string-downcase (symbol-name classification))
                                "note" (or note :null))))
          (error (condition)
            (incf failed)
            (%set-key coverage (pathname-name path)
                      (object "operation" (jget fixture "operation" "initialize")
                              "classification" "failed"
                              "note" (princ-to-string condition)))
            (format stream "~&  FAIL ~a (~a)~%    ~a~%"
                    name (pathname-name path) condition)))))
    (format stream "~&axmcp: ~a passed, ~a failed, ~a not claimed (~a fixtures)~%"
            passed failed unclaimed (length files))
    (let ((not-claimed (loop for key in (%object-keys coverage)
                             when (equal (jget (gethash key coverage) "classification")
                                         "explicitly-not-claimed")
                               collect key)))
      (when not-claimed
        (format stream "~&axmcp: not claimed, so the suite is INCOMPLETE: ~{~a~^, ~}~%"
                not-claimed)))
    (values passed failed coverage unclaimed)))

(defun run-mcp-conformance-tests-or-die ()
  "Run the axmcp suite and signal unless every fixture was actually proved.

An unclaimed fixture fails this gate. A suite that skips a fixture is an
incomplete claim, not a pass, and the caller is a release gate.

The harness self-tests run first and fail this gate too. A runner that can
report a false green makes every number below it meaningless, so it is
checked before the numbers are believed."
  (multiple-value-bind (selftests-passed selftests-failed) (run-mcp-harness-selftests)
    (declare (ignore selftests-passed))
    (unless (zerop selftests-failed)
      (error "~a MCP harness selftest(s) failed; the axmcp numbers cannot be trusted."
             selftests-failed)))
  (multiple-value-bind (passed failed coverage unclaimed) (run-mcp-conformance-tests)
    (declare (ignore passed coverage))
    (unless (zerop failed)
      (error "~a axmcp conformance fixture(s) failed." failed))
    (unless (zerop unclaimed)
      (error "~a axmcp conformance fixture(s) executed no implementation path; the axmcp claim is incomplete."
             unclaimed))
    t))
