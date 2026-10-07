;;;; UCP schema parity and adversarial native-boundary tests.
;;;; No shared fixture mutations, public network, credentials or JS bridge.

(in-package #:axllm)

(defun %ucps-test-error (thunk message &optional (kind 'ucp-schema-error))
  (let ((condition (handler-case (progn (funcall thunk) nil)
                     (ucp-schema-error (e) e))))
    (assert condition () "Expected ~a, got success" message)
    (assert (typep condition kind) () "Expected ~a, got ~a" kind condition)
    (assert (search message (princ-to-string condition)) ()
            "Expected diagnostic ~s, got ~a" message condition)
    condition))

(defun %ucps-test-validator (schema &rest options)
  (apply #'make-ucp-schema-validator
         :fetch (lambda (url request)
                  (declare (ignore url))
                  (assert (equal (jget request "redirect") "manual"))
                  (assert (plusp (jget request "maxBytes")))
                  (values (encode-json schema) 200 nil "OK"))
         options))

(defun %ucps-test-valid (schema value &rest options)
  (ucp-schema-validate (apply #'%ucps-test-validator schema options)
                       value "https://schemas.example/root.json"))

(defun %ucps-test-invalid (schema value message)
  (%ucps-test-error (lambda () (%ucps-test-valid schema value)) message
                    'ucp-schema-validation-error))

(defun %ucps-test-loopback (responses consumer)
  "Serve exactly RESPONSES with a native loopback socket; always close it."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (worker nil) (failure nil) (requests nil))
    (unwind-protect
        (progn
          (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
          (sb-bsd-sockets:socket-listen listener 4)
          (let ((port (nth-value 1 (sb-bsd-sockets:socket-name listener))))
            (setf worker
                  (sb-thread:make-thread
                   (lambda ()
                     (handler-case
                         (sb-ext:with-timeout 5
                           (dolist (response responses)
                             (let* ((socket (sb-bsd-sockets:socket-accept listener))
                                    (stream (sb-bsd-sockets:socket-make-stream
                                             socket :input t :output t
                                             :element-type 'character :external-format :utf-8
                                             :buffering :full)))
                               (unwind-protect
                                   (progn
                                     (push (read-line stream) requests)
                                     (loop for line = (read-line stream nil "")
                                           until (zerop (length (string-trim '(#\Return) line))))
                                     (write-string response stream)
                                     (finish-output stream))
                                 (close stream :abort t)))))
                       (error (e) (setf failure e))))
                   :name "ucp-schema-test-http"))
            (funcall consumer (format nil "http://127.0.0.1:~d/root.json" port))
            (sb-thread:join-thread worker)
            (when failure (error failure))
            (assert (= (length responses) (length requests)))
            (nreverse requests)))
      (ignore-errors (sb-bsd-sockets:socket-close listener))
      (when (and worker (sb-thread:thread-alive-p worker))
        (sb-thread:join-thread worker :timeout 6 :default nil)))))

(defun %ucps-test-response (body &optional (status "200 OK") headers)
  (format nil "HTTP/1.1 ~a~c~cContent-Length: ~d~c~cConnection: close~c~c~a~c~c~a"
          status #\Return #\Newline (length (%mcp-utf8 body)) #\Return #\Newline
          #\Return #\Newline (or headers "") #\Return #\Newline body))

(defun run-ucp-schema-tests (&key (stream *standard-output*))
  "Return passed and failed counts; each check performs semantic assertions."
  (let ((passed 0) (failed 0))
    (flet ((check (name thunk)
             (handler-case (progn (funcall thunk) (incf passed))
               (error (e) (incf failed) (format stream "FAIL UCP schema ~a: ~a~%" name e)))))
      (check "TS remote checkout example, cache and diagnostics"
             (lambda ()
               (let* ((calls nil)
                      (root (parse-json "{\"type\":\"object\",\"required\":[\"ucp\",\"id\"],\"properties\":{\"ucp\":{\"type\":\"object\",\"required\":[\"version\"],\"properties\":{\"version\":{\"const\":\"2026-04-08\"}}},\"id\":{\"type\":\"string\",\"minLength\":1},\"discounts\":{\"$ref\":\"./discount.json#/$defs/discounts\"}}}"))
                      (other (parse-json "{\"$defs\":{\"discounts\":{\"type\":\"object\",\"required\":[\"codes\"],\"properties\":{\"codes\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"uniqueItems\":true}}}}}"))
                      (validator (make-ucp-schema-validator
                                  :fetch (lambda (url options)
                                           (assert (equal (jget options "redirect") "manual"))
                                           (assert (equal (jget (jget options "headers") "Accept")
                                                          "application/schema+json, application/json"))
                                           (push url calls)
                                           (values (encode-json
                                                    (cond ((equal url "https://schemas.example/checkout.json") root)
                                                          ((equal url "https://schemas.example/discount.json") other)
                                                          (t (error "Unexpected URL ~a" url)))) 200))))
                      (value (object "ucp" (object "version" "2026-04-08") "id" "checkout-1"
                                     "discounts" (object "codes" #("SAVE10")))))
                 (assert (funcall (ucp-schema-validation-callback validator) value
                                  "https://SCHEMAS.example:443/checkout.json"))
                 (%set-key (jget value "discounts") "codes" #("SAVE10" "SAVE10"))
                 (let ((e (%ucps-test-error
                           (lambda () (ucp-schema-validate validator value "https://schemas.example/checkout.json"))
                           "array items must be unique" 'ucp-schema-validation-error)))
                   (assert (equal (ucp-schema-error-instance-path e) "/discounts/codes"))
                   (assert (equal (ucp-schema-error-schema-path e) "#/$defs/discounts/properties/codes")))
                 (assert (= 2 (length calls)))
                 (ucp-schema-clear-cache validator)
                 (%set-key (jget value "discounts") "codes" #("SAVE20"))
                 (assert (ucp-schema-validate validator value "https://schemas.example/checkout.json"))
                 (assert (= 4 (length calls))))))
      (check "recursive document terminates by value, unused bad ref stays lazy"
             (lambda ()
               (assert (%ucps-test-valid
                        (object "type" "object" "$defs" (object "unused" (object "$ref" "https://localhost/"))
                                "properties" (object "child" (object "$ref" "#")))
                        (object "child" (object "child" (object)))))))
      (check "cross-document cycle stops at depth; cached documents fetched once"
             (lambda ()
               (let* ((calls 0)
                      (validator (make-ucp-schema-validator
                                  :max-depth 5 :fetch
                                  (lambda (url options)
                                    (declare (ignore options)) (incf calls)
                                    (values (encode-json (object "$ref" (if (search "root" url) "b.json" "root.json"))) 200)))))
                 (%ucps-test-error (lambda () (ucp-schema-validate validator 1 "https://schemas.example/root.json"))
                                   "validation depth exceeded")
                 (assert (= calls 2)))))
      (check "ref budget cannot be swallowed by anyOf"
             (lambda ()
               (%ucps-test-error
                (lambda () (%ucps-test-valid (object "anyOf" (vector true (object "$ref" "#")))
                                             1 :max-refs 2))
                "reference limit exceeded")))
      ;; Test ref and document budgets separately, with a low bound so a depth
      ;; limit cannot accidentally be the oracle for a reference-budget check.
      (check "reference limit"
             (lambda ()
               (%ucps-test-error
                (lambda () (%ucps-test-valid (object "$ref" "#") 1 :max-refs 2)) "reference limit exceeded")))
      (check "document limit, including cached successes"
             (lambda ()
               (let ((calls 0))
                 (%ucps-test-error
                  (lambda ()
                    (ucp-schema-validate
                     (make-ucp-schema-validator :max-documents 1
                      :fetch (lambda (url options) (declare (ignore url options)) (incf calls)
                               (values "{\"$ref\":\"other.json\"}" 200))) 1 "https://schemas.example/root.json"))
                  "document limit exceeded")
                 (assert (= calls 1)))))
      (dolist (case '(("#/missing" "Unresolved UCP schema reference")
                      ("#anchor" "Unsupported UCP schema anchor")
                      ("#/$defs/bad" "is not a schema")))
        (check (first case)
               (lambda ()
                 (%ucps-test-error
                  (lambda () (%ucps-test-valid (object "$defs" (object "bad" 3) "$ref" (first case)) 1))
                  (second case)))))
      (check "pointer escapes, array pointer and false target"
             (lambda ()
               (assert (%ucps-test-valid (object "$defs" (object "a/b~c" (object "const" 9))
                                                "$ref" "#/$defs/a~1b~0c") 9))
               (assert (%ucps-test-valid (object "$defs" (vector (object "const" 4)) "$ref" "#/$defs/0") 4))
               (%ucps-test-invalid (object "$defs" (object "no" false) "$ref" "#/$defs/no") 1 "boolean schema rejects")))
      (check "ref siblings still validate"
             (lambda () (%ucps-test-invalid (object "$defs" (object "yes" true) "$ref" "#/$defs/yes" "const" 2)
                                             1 "value does not match const")))
      (dolist (keyword '("anyOf" "oneOf" "not" "if"))
        (check (format nil "~a propagates broken reference" keyword)
               (lambda ()
                 (%ucps-test-error
                  (lambda () (%ucps-test-valid
                              (object keyword (if (member keyword '("anyOf" "oneOf") :test #'equal)
                                                  (vector true (object "$ref" "#/absent"))
                                                  (object "$ref" "#/absent"))) 1))
                  "Unresolved UCP schema reference"))))
      (dolist (url '("http://public.example/a" "https://localhost/a" "https://x.localhost./a"
                     "https://127.1/a" "https://0x7f000001/a" "https://0177.0.0.1/a"
                     "https://10.1/a" "https://0300.0250.0001.0001/a" "https://169.254.169.254/a"
                     "https://100.64.0.1/a" "https://192.0.0.1/a" "https://198.18.0.1/a"
                     "https://224.0.0.1/a" "https://[::]/a" "https://[::1]/a"
                     "https://[0:0:0:0:0:0:0:1]/a" "https://[fc00::1]/a" "https://[fe80::1]/a"
                     "https://[::ffff:7f00:1]/a" "https://[::ffff:127.0.0.1]/a"))
        (check url
               (lambda ()
                 (let ((called nil))
                   (%ucps-test-error (lambda () (ucp-schema-validate
                                                (make-ucp-schema-validator :fetch
                                                 (lambda (u o) (declare (ignore u o)) (setf called t) (values "{}" 200)))
                                                1 url)) "Blocked")
                   (assert (not called))))))
      (check "exceptions are explicit and allowedHosts does not disable HTTPS"
             (lambda ()
               (dolist (case (list (list "http://localhost/x" (object "allowHTTP" true "allowLoopback" true))
                                  (list "https://10.0.0.1/x" (object "allowPrivateNetwork" true))
                                  (list "https://localhost/x" (object "allowedHosts" #("LOCALHOST.")))
                                  (list "http://localhost/x" (object "disabled" true))))
                 (assert (ucp-schema-validate (%ucps-test-validator (object) :ssrf-protection (second case))
                                              1 (first case))))
               (%ucps-test-error
                (lambda () (ucp-schema-validate (%ucps-test-validator (object) :ssrf-protection
                                                (object "allowedHosts" #("localhost"))) 1 "http://localhost/x"))
                "expected https")))
      (check "cross-document host policy runs before fetching target"
             (lambda ()
               (let ((calls 0))
                 (%ucps-test-error
                  (lambda () (ucp-schema-validate
                              (make-ucp-schema-validator :fetch
                               (lambda (url options) (declare (ignore url options)) (incf calls)
                                 (values "{\"$ref\":\"https://169.254.169.254/meta\"}" 200)))
                              1 "https://schemas.example/root.json")) "Blocked private")
                 (assert (= calls 1)))))
      (check "redirect gate, relative resolution and custom host restriction"
             (lambda ()
               (let ((calls nil) (contexts nil))
                 (let ((validator
                         (make-ucp-schema-validator
                          :ssrf-protection (object "validateURL"
                                                   (lambda (url context)
                                                     (push context contexts)
                                                     (unless (equal (puri:uri-host (puri:parse-uri url)) "schemas.example")
                                                       (error "custom host restriction"))))
                          :fetch (lambda (url options)
                                   (declare (ignore options)) (push url calls)
                                   (if (= (length calls) 1) (values "" 302 '((:location . "./final.json")))
                                       (values "{}" 200))))))
                   (assert (ucp-schema-validate validator 1 "https://schemas.example/root.json"))
                   (assert (equal calls '("https://schemas.example/final.json" "https://schemas.example/root.json")))
                   (assert (equal contexts '("redirect" "mcp-endpoint")))
                   (%ucps-test-error (lambda () (ucp-schema-validate validator 1 "https://other.example/x"))
                                     "custom host restriction")))))
      (dolist (case '(("https://localhost/" "Blocked loopback") (nil "missing Location") ("./root.json" "too many hops")))
        (check (format nil "redirect ~a" (second case))
               (lambda ()
                 (let ((calls 0))
                   (%ucps-test-error
                    (lambda () (ucp-schema-validate
                                (make-ucp-schema-validator :fetch
                                 (lambda (url options) (declare (ignore url options)) (incf calls)
                                   (values "" 302 (when (first case) (list (cons :location (first case)))))))
                                1 "https://schemas.example/root.json")) (second case))
                   (assert (= calls (if (equal (second case) "too many hops") 6 1)))))))
      (check "failed responses and invalid JSON never enter cache"
             (lambda ()
               (let* ((calls 0) (validator (make-ucp-schema-validator :fetch
                                          (lambda (url options) (declare (ignore url options))
                                            (incf calls)
                                            (case calls (1 (values "" 503 nil "Unavailable"))
                                                        (2 (values "{" 200)) (t (values "{}" 200)))))))
                 (%ucps-test-error (lambda () (ucp-schema-validate validator 1 "https://schemas.example/"))
                                   "fetch failed: 503 Unavailable")
                 (%ucps-test-error (lambda () (ucp-schema-validate validator 1 "https://schemas.example/")) "Invalid JSON")
                 (assert (ucp-schema-validate validator 1 "https://schemas.example/"))
                 (assert (ucp-schema-validate validator 1 "https://schemas.example/"))
                 (assert (= calls 3)))))
      (check "canonical cache keys, query distinction, stale entries and clearing"
             (lambda ()
               (let* ((calls 0) (version 1)
                      (validator (make-ucp-schema-validator :fetch
                                  (lambda (url options) (declare (ignore url options)) (incf calls)
                                    (values (encode-json (object "const" version)) 200)))))
                 (assert (ucp-schema-validate validator 1 "https://SCHEMAS.example:443/a/../root.json?q=1"))
                 (setf version 2)
                 (assert (ucp-schema-validate validator 1 "https://schemas.example/a/%2e%2e/root.json?q=1"))
                 (%ucps-test-error (lambda () (ucp-schema-validate validator 2 "https://schemas.example/root.json?q=1")) "const")
                 (assert (= calls 1))
                 (assert (ucp-schema-validate validator 2 "https://schemas.example/root.json?q=2"))
                 (assert (= calls 2))
                 (ucp-schema-clear-cache validator)
                 (assert (ucp-schema-validate validator 2 "https://schemas.example/root.json?q=1"))
                 (assert (= calls 3)))))
      (check "concurrent calls share one successful fetch"
             (lambda ()
               (let* ((calls 0)
                      (validator (make-ucp-schema-validator :fetch
                                  (lambda (url options) (declare (ignore url options))
                                    (incf calls) (sleep 0.02) (values "{}" 200))))
                      (threads (loop repeat 4 collect
                                     (sb-thread:make-thread
                                      (lambda ()
                                        (handler-case
                                            (ucp-schema-validate validator 1 "https://schemas.example/root.json")
                                          (error (e) e)))))))
                 (dolist (thread threads) (assert (eq t (sb-thread:join-thread thread))))
                 (assert (= calls 1)))))
      (check "zero document budget never fetches"
             (lambda ()
               (%ucps-test-error (lambda () (%ucps-test-valid (object) 1 :max-documents 0))
                                 "document limit exceeded")))
      (dolist (body '("true" "false" "[]" "null"))
        (check (format nil "root ~a rejected" body)
               (lambda ()
                 (%ucps-test-error
                  (lambda () (ucp-schema-validate (make-ucp-schema-validator :fetch
                                                  (lambda (u o) (declare (ignore u o)) (values body 200)))
                                                  1 "https://schemas.example/")) "not a JSON object"))))
      (check "bytes not characters, exact boundary, stream cleanup"
             (lambda ()
               (let* ((text "{\"title\":\"é\"}") (bytes (%mcp-utf8 text)) (size (length bytes)))
                 (assert (%ucps-test-valid (parse-json text) 1 :max-bytes size))
                 (%ucps-test-error (lambda () (%ucps-test-valid (parse-json text) 1 :max-bytes (1- size)))
                                   "byte limit exceeded")
                 (let* ((body (flexi-streams:make-in-memory-input-stream bytes))
                        (validator (make-ucp-schema-validator :max-bytes 5 :fetch
                                    (lambda (url options) (declare (ignore url options)) (values body 200)))))
                   (%ucps-test-error (lambda () (ucp-schema-validate validator 1 "https://schemas.example/"))
                                     "byte limit exceeded")
                   (assert (not (open-stream-p body)))))))
      (check "stream stops after limit plus one byte"
             (lambda ()
               (let ((body (flexi-streams:make-in-memory-input-stream (%mcp-utf8 (make-string 100 :initial-element #\Space)))))
                 (unwind-protect
                     (progn
                       (%ucps-test-error (lambda () (%ucps-body-text body 5)) "byte limit exceeded")
                       (assert (= (file-position body) 6)))
                   (close body)))))
      (check "cyclic host value fails before fetch"
             (lambda ()
               (let ((value (object)))
                 (%set-key value "self" value)
                 (%ucps-test-error (lambda () (%ucps-test-valid (object) value)) "Cyclic UCP instance"))))
      (check "JSON depth preflight and quote escapes"
             (lambda ()
               (%ucps-test-error (lambda () (%ucps-test-valid (object "$defs" (object "a" (object))) 1 :max-json-depth 2))
                                 "JSON depth exceeded")
               (assert (%ucps-test-valid (object "title" "[[[{\\\"") 1 :max-json-depth 1))))
      (check "depth and work boundary, including additionalProperties"
             (lambda ()
               (assert (%ucps-test-valid (object "items" true) #(1) :max-depth 1))
               (%ucps-test-error (lambda () (%ucps-test-valid (object "items" true) #(1) :max-depth 0)) "depth exceeded")
               (%ucps-test-error (lambda () (%ucps-test-valid (object "additionalProperties" (object))
                                                             (object "a" 1) :max-depth 0)) "depth exceeded")
               (%ucps-test-error (lambda () (%ucps-test-valid (object "allOf" (vector true true true))
                                                             1 :max-nodes 2)) "work limit exceeded")))
      (check "timeout and lock released after timeout"
             (lambda ()
               (let* ((slow t) (validator (make-ucp-schema-validator :timeout 0.02 :fetch
                                          (lambda (u o) (declare (ignore u o))
                                            (when slow (sleep 1)) (values "{}" 200)))))
                 (%ucps-test-error (lambda () (ucp-schema-validate validator 1 "https://schemas.example/")) "timeout exceeded")
                 (setf slow nil)
                 (assert (ucp-schema-validate validator 1 "https://schemas.example/")))))
      (check "compositions, conditionals, booleans and keyword paths"
             (lambda ()
               (assert (%ucps-test-valid (object "anyOf" (vector false (object "type" "number"))) 2))
               (%ucps-test-invalid (object "oneOf" (vector true true)) 2 "oneOf matched 2 schemas")
               (%ucps-test-invalid (object "anyOf" #()) 2 "anyOf matched 0 schemas")
               (%ucps-test-invalid (object "not" (object "type" "number")) 2 "forbidden not")
               (assert (%ucps-test-valid (object "if" (object "type" "number") "then" (object "minimum" 1) "else" false) 2))
               (%ucps-test-invalid (object "if" false "else" false) 2 "boolean schema rejects")
               (let ((e (%ucps-test-invalid (object "allOf" (vector (object "const" 3))) 2 "const")))
                 (assert (equal (ucp-schema-error-instance-path e) "/"))
                 (assert (equal (ucp-schema-error-schema-path e) "#/allOf~10")))))
      (check "operational failures precede scalar constraints and escape branches"
             (lambda ()
               (%ucps-test-error
                (lambda () (%ucps-test-valid (object "minLength" 10 "allOf" (vector (object "$ref" "#/absent"))) "x"))
                "Unresolved UCP schema reference")
               (let ((e (%ucps-test-error
                         (lambda () (%ucps-test-valid (object "anyOf" (vector true (object "pattern" "("))) "x"))
                         "Invalid regular expression")))
                 (assert (not (typep e 'ucp-schema-validation-error))))))
      (check "scalar and collection constraints"
             (lambda ()
               (dolist (case (list (list (object "type" "integer") 1.5 "expected type")
                                  (list (object "type" "") 1 "expected type")
                                  (list (object "type" false) 1 "expected type")
                                  (list (object "enum" #(1 2)) 3 "not in enum")
                                  (list (object "const" :null) false "const")
                                  (list (object "minLength" 2) "a" "shorter")
                                  (list (object "maxLength" 1) (string (code-char #x1f600)) "longer")
                                  (list (object "pattern" "^[A-Z]+$") "a" "does not match")
                                  (list (object "minimum" 2) 1 "below")
                                  (list (object "maximum" 2) 3 "above")
                                  (list (object "exclusiveMinimum" 2) 2 "must exceed")
                                  (list (object "exclusiveMaximum" 2) 2 "must be below")
                                  (list (object "minItems" 1) #() "at least")
                                  (list (object "maxItems" 0) #(1) "at most")
                                  (list (object "items" false) #(1) "boolean schema")
                                  (list (object "format" "uri") "relative" "not a URI")
                                  (list (object "format" "date-time") "not a date" "date-time")))
                 (apply #'%ucps-test-invalid case))
               (assert (%ucps-test-valid (object "type" #("null" "boolean")) false))
               (assert (%ucps-test-valid (object "format" "uri") "urn:example:x"))
               (assert (%ucps-test-valid (object "format" "date-time") "2026-04-08T12:30:00Z"))
               (assert (%ucps-test-valid (object "format" "date-time") "Wed, 08 Apr 2026 12:30:00 GMT"))
               (assert (%ucps-test-valid (object "format" "date-time") "2026-04-08"))))
      (check "object properties, overlapping patterns, extras and escaping"
             (lambda ()
               (let ((schema (object "properties" (object "a/b~c" (object "type" "integer"))
                                     "patternProperties" (object "^a" (object "minimum" 2))
                                     "additionalProperties" false)))
                 (assert (%ucps-test-valid schema (object "a/b~c" 2 "apple" 3)))
                 (%ucps-test-invalid schema (object "a/b~c" 1) "below")
                 (%ucps-test-invalid schema (object "z" 1) "additional property z")
                 (let ((e (%ucps-test-invalid schema (object "a/b~c" "bad") "expected type")))
                   (assert (equal (ucp-schema-error-instance-path e) "/a~1b~0c"))
                   (assert (equal (ucp-schema-error-schema-path e) "#/properties/a~1b~0c"))))
               (%ucps-test-invalid (object "required" #("id")) (object) "missing required property id")
               (%ucps-test-invalid (object "additionalProperties" (object "type" "string")) (object "x" 2) "expected type")))
      (check "default streamed HTTP resolves cross-document refs on loopback"
             (lambda ()
               (let ((requests
                       (%ucps-test-loopback
                        (list (%ucps-test-response "{\"$ref\":\"./other.json#/$defs/x\"}")
                              (%ucps-test-response "{\"$defs\":{\"x\":{\"const\":7}}}"))
                        (lambda (url)
                          (let ((validator (make-ucp-schema-validator
                                            :ssrf-protection (object "allowHTTP" true "allowLoopback" true))))
                            (assert (ucp-schema-validate validator 7 url))
                            (%ucps-test-error (lambda () (ucp-schema-validate validator 8 url)) "const"
                                              'ucp-schema-validation-error))))))
                 (assert (search "GET /root.json " (first requests)))
                 (assert (search "GET /other.json " (second requests))))))
      (check "default HTTP rejects oversized Content-Length before buffering"
             (lambda ()
               (%ucps-test-loopback
                (list (%ucps-test-response (make-string 64 :initial-element #\Space)))
                (lambda (url)
                  (%ucps-test-error
                   (lambda () (ucp-schema-validate
                               (make-ucp-schema-validator :max-bytes 8
                                :ssrf-protection (object "allowHTTP" true "allowLoopback" true)) 1 url))
                   "byte limit exceeded"))))))
    (format stream "UCP schema: ~d passed, ~d failed~%" passed failed)
    (values passed failed)))

(defun run-ucp-schema-tests-or-die ()
  (multiple-value-bind (passed failed) (run-ucp-schema-tests)
    (unless (and (plusp passed) (zerop failed)) (error "UCP schema tests failed"))
    t))
