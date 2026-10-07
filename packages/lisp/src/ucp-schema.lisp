;;;; ucp-schema.lisp --- native, bounded UCP schema validation.
;;;;
;;;; Mirrors src/ax/ucp/schema.ts, not the full JSON Schema specification.
;;;; Core supplies raw-schema type checks, UTF-16 and ECMAScript patterns.
;;;; References and traversal stay here because Core's raw tool validator
;;;; rejects external references and has different composition semantics.
;;;; Unlike the TS fetch().json() boundary, the native boundary also limits
;;;; response bytes, JSON nesting, reference visits, work and elapsed time.

(in-package #:axllm)

(define-condition ucp-schema-error (ax-error) ())

(define-condition ucp-schema-validation-error (ucp-schema-error)
  ((instance-path :initarg :instance-path :reader ucp-schema-error-instance-path)
   (schema-path :initarg :schema-path :reader ucp-schema-error-schema-path)))

(defun %ucps-error (control &rest arguments)
  (error 'ucp-schema-error :message (apply #'format nil control arguments)))

(defun %ucps-fail (instance-path schema-path control &rest arguments)
  (error 'ucp-schema-validation-error
         :instance-path instance-path :schema-path schema-path
         :message (format nil "UCP schema validation failed at ~a: ~a"
                          (if (equal instance-path "") "/" instance-path)
                          (apply #'format nil control arguments))))

(defclass ucp-schema-validator ()
  ((fetcher :initarg :fetch :reader %ucps-fetcher)
   (guard :initarg :ssrf-protection :reader %ucps-guard)
   (max-documents :initarg :max-documents :reader %ucps-max-documents)
   (max-depth :initarg :max-depth :reader %ucps-max-depth)
   (max-refs :initarg :max-refs :reader %ucps-max-refs)
   (max-bytes :initarg :max-bytes :reader %ucps-max-bytes)
   (max-json-depth :initarg :max-json-depth :reader %ucps-max-json-depth)
   (max-nodes :initarg :max-nodes :reader %ucps-max-nodes)
   (timeout :initarg :timeout :reader %ucps-timeout)
   (documents :initform (make-hash-table :test 'equal) :reader %ucps-documents)
   (lock :initform (sb-thread:make-mutex :name "UCP schema cache") :reader %ucps-lock)))

(defun make-ucp-schema-validator (&key fetch ssrf-protection (max-documents 64)
                                     (max-depth 128) (max-refs 1024)
                                     (max-bytes 1048576) (max-json-depth 256)
                                     (max-nodes 100000) (timeout 30))
  "Make a synchronous UCP validator. Successful document loads are cached
until UCP-SCHEMA-CLEAR-CACHE; failed loads are not cached. Calls are serialized.

FETCH, if supplied, receives (URL OPTIONS) and returns (values BODY STATUS
HEADERS REASON). BODY is UTF-8 octets, a string, or an owned binary stream;
HEADERS is a string/keyword-keyed alist. Streams are always closed. OPTIONS
contains headers, redirect=manual, maxBytes and timeout. A custom fetch must
not follow redirects or buffer unbounded data. The default uses verified TLS,
streamed Drakma, a byte cap, and an elapsed-time deadline.

SSRF-PROTECTION is an Ax object with the TypeScript keys disabled, allowHTTP,
allowLoopback, allowPrivateNetwork, allowedHosts and validateURL. Allowed hosts
bypass host classification, NOT HTTPS. validateURL receives (URL CONTEXT), with
CONTEXT mcp-endpoint or redirect, and must signal to refuse. As in TypeScript,
DNS pinning is application policy, not implicit hostname resolution here."
  (dolist (limit (list max-documents max-refs max-bytes max-json-depth max-nodes))
    (unless (and (integerp limit) (<= 0 limit))
      (%ucps-error "UCP schema limits must be nonnegative integers")))
  (unless (and (integerp max-depth) (<= 0 max-depth)
               (realp timeout) (plusp timeout))
    (%ucps-error "UCP schema depth must be nonnegative and timeout positive"))
  (when (and fetch (not (functionp fetch)))
    (%ucps-error "UCP schema fetch must be a function"))
  (make-instance 'ucp-schema-validator :fetch fetch :ssrf-protection ssrf-protection
                 :max-documents max-documents :max-depth max-depth :max-refs max-refs
                 :max-bytes max-bytes :max-json-depth max-json-depth
                 :max-nodes max-nodes :timeout timeout))

(defun ucp-schema-clear-cache (validator)
  (sb-thread:with-mutex ((%ucps-lock validator))
    (clrhash (%ucps-documents validator)))
  (values))

(defun %ucps-ipv4 (host)
  "WHATWG IPv4 number forms (including short, octal and hexadecimal hosts)."
  (let* ((parts (uiop:split-string (string-right-trim "." host) :separator "."))
         (last (car (last parts))))
    (labels ((number-part (part)
               (cond ((and (> (length part) 2) (string-equal part "0x" :end1 2))
                      (parse-integer part :start 2 :radix 16))
                     ((and (> (length part) 1) (char= (char part 0) #\0))
                      (parse-integer part :start 1 :radix 8))
                     (t (parse-integer part :radix 10)))))
      (when (and last (plusp (length last))
                 (or (every #'digit-char-p last)
                     (ignore-errors (number-part last))))
        (handler-case
            (let ((numbers (mapcar #'number-part parts)))
              (unless (and (<= 1 (length numbers) 4)
                           (every (lambda (n) (<= 0 n 255)) (butlast numbers))
                           (<= 0 (car (last numbers)))
                           (< (car (last numbers)) (expt 256 (- 5 (length numbers)))))
                (%ucps-error "Invalid UCP schema IPv4 host"))
              (let ((value (car (last numbers))))
                (loop for n in (butlast numbers) for shift from 24 downto 8 by 8
                      do (incf value (ash n shift)))
                (loop for shift from 24 downto 0 by 8 collect (ldb (byte 8 shift) value))))
          (error () (%ucps-error "Invalid UCP schema IPv4 host")))))))

(defun %ucps-ipv4-class (bytes)
  (destructuring-bind (a b c d) bytes
    (declare (ignore c d))
    (cond ((= a 127) :loopback)
          ((or (= a 10) (and (= a 172) (<= 16 b 31))
               (and (= a 192) (= b 168))
               (and (= a 169) (= b 254)) (= a 0) (>= a 224)
               (and (= a 100) (<= 64 b 127))
               (and (= a 192) (= b 0)) (and (= a 198) (member b '(18 19))))
           :private)
          (t :public))))

(defun %ucps-host-class (host)
  (let ((host (string-downcase (string-right-trim "." (string-trim "[]" host)))))
    (cond ((or (equal host "localhost") (uiop:string-suffix-p host ".localhost"))
           :loopback)
          ((find #\: host)
           (let* ((bytes (sb-bsd-sockets:make-inet6-address host))
                  (value (reduce (lambda (a b) (+ (ash a 8) b)) bytes :initial-value 0)))
             (cond ((= value 1) :loopback)
                   ((= (ash value -32) #xffff)
                    (%ucps-ipv4-class (coerce (subseq bytes 12) 'list)))
                   ((or (zerop value) (= (ash value -120) #xff)
                        (= (ash value -121) #x7e) (= (ash value -118) #x3fa)) :private)
                   (t :public))))
          ((%ucps-ipv4 host) (%ucps-ipv4-class (%ucps-ipv4 host)))
          (t :public))))

(defun %ucps-uri-input (text)
  ;; PURI rejects dotted IPv4 tails inside IPv6 literals. Let SBCL's native
  ;; address parser expand those before PURI parses the surrounding URI.
  (multiple-value-bind (begin end)
      (cl-ppcre:scan "^(?:[A-Za-z][A-Za-z0-9+.-]*:)?//\\[[0-9a-fA-F:.]+\\]" text)
    (let ((start (and begin (position #\[ text :start begin :end end))))
      (if (and start (find #\. text :start start :end end))
          (let ((bytes (sb-bsd-sockets:make-inet6-address (subseq text (1+ start) (1- end)))))
            (concatenate 'string (subseq text 0 start) "["
                         (format nil "~(~{~x~^:~}~)"
                                 (loop for i from 0 below 16 by 2
                                       collect (+ (ash (aref bytes i) 8) (aref bytes (1+ i)))))
                         "]" (subseq text end)))
          text))))

(defun %ucps-normal-path (path)
  ;; WHATWG removes both literal and percent-encoded dot segments. Preserve
  ;; empty segments: /a//b and /a/b are different cache keys and resources.
  (let ((out nil) (parts (uiop:split-string (or path "/") :separator "/")))
    (loop for tail on parts for part = (car tail) do
      (cond ((member part '("." "%2e") :test #'string-equal)
             (unless (cdr tail) (push "" out)))
            ((member part '(".." ".%2e" "%2e." "%2e%2e") :test #'string-equal)
             (when (cdr out) (pop out))
             (unless (cdr tail) (push "" out)))
            (t (push part out))))
    (format nil "~{~a~^/~}" (nreverse out))))

(defun %ucps-url (text &optional base)
  "PURI resolution plus host canonicalization; ambiguous URL syntax fails closed."
  (handler-case
      (let* ((input (%ucps-uri-input text))
             (uri (if base (puri:merge-uris input base) (puri:parse-uri input)))
             (host (puri:uri-host uri)))
        (unless (and (puri:uri-scheme uri) host (plusp (length host)))
          (%ucps-error "UCP schema URL must be absolute with a host"))
        (when (or (find #\% host) (find #\\ text)
                  (some (lambda (c) (< (char-code c) 33)) text))
          (%ucps-error "Ambiguous UCP schema URL"))
        (setf (puri:uri-host uri) (string-downcase host))
        (unless (find #\: host)
          (let ((ip (%ucps-ipv4 host)))
            (when ip (setf (puri:uri-host uri) (format nil "~{~d~^.~}" ip)))))
        (setf (puri:uri-path uri) (%ucps-normal-path (puri:uri-path uri)))
        (puri:render-uri uri nil))
    (ucp-schema-error (e) (error e))
    (error () (%ucps-error "Invalid UCP schema URL: ~a" text))))

(defun %ucps-check-url (validator url context)
  (let* ((guard (%ucps-guard validator))
         (uri (puri:parse-uri url))
         (host (string-downcase (string-right-trim "." (puri:uri-host uri)))))
    (unless (axllm/core::core-true-p (jget guard "disabled"))
      (unless (or (eq (puri:uri-scheme uri) :https)
                  (and (eq (puri:uri-scheme uri) :http)
                       (axllm/core::core-true-p (jget guard "allowHTTP"))))
        (%ucps-error "Blocked unsafe MCP URL for ~a: expected https URL" context))
      (unless (find host (let ((hosts (jget guard "allowedHosts")))
                           (if (axllm/core::core-array-p hosts) hosts #()))
                    :test #'equal :key (lambda (h) (string-downcase
                                                    (string-right-trim "." (string-trim "[]" h)))))
        (case (%ucps-host-class host)
          (:loopback
           (unless (axllm/core::core-true-p (jget guard "allowLoopback"))
             (%ucps-error "Blocked loopback MCP URL for ~a: ~a" context url)))
          (:private
           (unless (axllm/core::core-true-p (jget guard "allowPrivateNetwork"))
             (%ucps-error "Blocked private or reserved MCP URL for ~a: ~a" context url)))))
      (let ((check (jget guard "validateURL")))
        (when (functionp check) (funcall check url context))))
    url))

(defun %ucps-default-fetch (url options)
  (multiple-value-bind (body status headers uri stream close reason)
      (drakma:http-request url :method :get :redirect nil :want-stream t
                           :force-binary t :decode-content t :verify :required
                           :connection-timeout (jget options "timeout")
                           :additional-headers '(("Accept" . "application/schema+json, application/json")))
    (declare (ignore uri stream close))
    (values body status headers reason)))

(defun %ucps-header (headers name)
  (cdr (find name headers :key (lambda (entry) (string (car entry))) :test #'string-equal)))

(defun %ucps-body-text (body limit)
  (let ((bytes
          (cond ((stringp body)
                 ;; Check characters first, so a huge string is not copied just to reject it.
                 (when (> (length body) limit) (%ucps-error "UCP schema byte limit exceeded"))
                 (%mcp-utf8 body))
                ((streamp body)
                 (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0)))
                   (loop for byte = (read-byte body nil nil) while byte
                         do (when (= (length out) limit)
                              (%ucps-error "UCP schema byte limit exceeded"))
                            (vector-push-extend byte out))
                   out))
                ((typep body '(vector (unsigned-byte 8))) body)
                (t (%ucps-error "UCP schema fetch body must be UTF-8 text or a binary stream")))))
    (when (> (length bytes) limit) (%ucps-error "UCP schema byte limit exceeded"))
    (%mcp-from-utf8 bytes)))

(defun %ucps-check-json-depth (text limit)
  ;; Lexical preflight BEFORE the recursive JSON reader, including unused $defs.
  (let ((depth 0) (quoted nil) (escaped nil))
    (loop for c across text do
      (cond (escaped (setf escaped nil))
            (quoted (cond ((char= c #\\) (setf escaped t))
                          ((char= c #\") (setf quoted nil))))
            ((char= c #\") (setf quoted t))
            ((find c "[{")
             (when (> (incf depth) limit) (%ucps-error "UCP schema JSON depth exceeded")))
            ((find c "]}") (decf depth))))))

(defun %ucps-load-document (validator url)
  (let* ((canonical (%ucps-url url))
         (documents (%ucps-documents validator)))
    (or (gethash canonical documents)
        (progn
          (when (>= (hash-table-count documents) (%ucps-max-documents validator))
            (%ucps-error "UCP schema document limit exceeded"))
          (let ((current (%ucps-check-url validator canonical "mcp-endpoint")))
            (loop for redirects from 0 to 5 do
              (multiple-value-bind (body status headers reason)
                  (funcall (or (%ucps-fetcher validator) #'%ucps-default-fetch)
                           current
                           (object "headers" (object "Accept" "application/schema+json, application/json")
                                   "redirect" "manual" "maxBytes" (%ucps-max-bytes validator)
                                   "timeout" (%ucps-timeout validator)))
                (unwind-protect
                    (cond
                      ((member status '(301 302 303 307 308))
                       (let ((location (%ucps-header headers "Location")))
                         (unless location (%ucps-error "Blocked MCP redirect: missing Location header"))
                         (when (= redirects 5) (%ucps-error "Blocked MCP redirect: too many hops"))
                         (setf current (%ucps-check-url validator (%ucps-url location current) "redirect"))))
                      (t
                       (unless (and (integerp status) (<= 200 status 299))
                         (%ucps-error "UCP schema fetch failed: ~a ~a" status (or reason "")))
                       (let ((content-length (%ucps-header headers "Content-Length")))
                         (when (and content-length
                                    (> (parse-integer content-length) (%ucps-max-bytes validator)))
                           (%ucps-error "UCP schema byte limit exceeded")))
                       (let* ((text (%ucps-body-text body (%ucps-max-bytes validator)))
                              (schema (progn (%ucps-check-json-depth text (%ucps-max-json-depth validator))
                                             (parse-json text))))
                         (unless (hash-table-p schema)
                           (%ucps-error "UCP schema ~a is not a JSON object" canonical))
                         (setf (gethash canonical documents) schema)
                         (return-from %ucps-load-document schema))))
                  (when (streamp body) (close body :abort t))))))))))

(defun %ucps-escape (token)
  (axllm/core::core-string-replace
   (axllm/core::core-string-replace token "~" "~0") "/" "~1"))

(defun %ucps-reference (validator reference url document)
  (let* ((resolved (puri:parse-uri (%ucps-url reference url)))
         (fragment (puri:uri-fragment resolved))
         (document-url (puri:render-uri (puri:copy-uri resolved :fragment nil) nil))
         (root (if (equal document-url url) document (%ucps-load-document validator document-url)))
         (schema root))
    (when (and fragment (plusp (length fragment)))
      (unless (char= (char fragment 0) #\/)
        (%ucps-error "Unsupported UCP schema anchor #~a" fragment))
      (dolist (token (uiop:split-string (subseq fragment 1) :separator "/"))
        (let* ((key (axllm/core::core-string-replace
                     (axllm/core::core-string-replace token "~1" "/") "~0" "~"))
               (index (and (axllm/core::core-array-p schema) (%array-index-key key))))
          (setf schema
                (cond ((and (hash-table-p schema) (nth-value 1 (gethash key schema)))
                       (gethash key schema))
                      ((and index (< index (length schema))) (aref schema index))
                      (t (%ucps-error "Unresolved UCP schema reference ~a" reference)))))))
    (unless (or (hash-table-p schema) (eq schema true) (eq schema false))
      (%ucps-error "UCP schema reference ~a is not a schema" reference))
    (values schema document-url root (if fragment (concatenate 'string "#" fragment) "#"))))

(defun %ucps-same (a b)
  ;; TS intentionally uses JSON.stringify, so object insertion order matters.
  (string= (encode-json a) (encode-json b)))

(defun %ucps-leaf (value schema ip sp)
  (flet ((fail (control &rest arguments) (apply #'%ucps-fail ip sp control arguments)))
    (when (and (nth-value 1 (gethash "const" schema))
               (not (%ucps-same value (jget schema "const"))))
      (fail "value does not match const"))
    (let ((enum (jget schema "enum")))
      (when (and (axllm/core::core-array-p enum) (not (find value enum :test #'%ucps-same)))
        (fail "value is not in enum")))
    (when (nth-value 1 (gethash "type" schema))
      (let ((type (jget schema "type")))
        (when (or (and (axllm/core::core-array-p type) (zerop (length type)))
                  (not (or (stringp type) (axllm/core::core-array-p type)))
                  (equal type "")
                  (plusp (length (axllm/core::chat-session-tool-argument-errors
                                  (object "type" type) value))))
          (fail "expected type ~a" (encode-json type)))))))

(defun %ucps-date-valid-p (value)
  ;; Date.parse accepts ISO dates and RFC 2822 HTTP dates as well as full
  ;; timestamps. Reuse native parsers, not the unrelated Ax datetime field
  ;; parser (which also accepts named IANA zones). Engine-specific legacy
  ;; Date.parse spellings outside these grammars are not portable.
  (or (ignore-errors (local-time:parse-timestring value :allow-missing-time-part t))
      (and (cl-ppcre:scan "^[A-Za-z]{3},? " value)
           (let ((drakma:*ignore-unparseable-cookie-dates-p* nil))
             (ignore-errors (drakma:parse-cookie-date value))))))

(defun %ucps-constraints (value schema ip sp)
  (flet ((fail (control &rest arguments) (apply #'%ucps-fail ip sp control arguments)))
    (when (stringp value)
      (let ((size (length (axllm/core::core-string-utf16-units value)))
            (minimum (jget schema "minLength")) (maximum (jget schema "maxLength"))
            (pattern (jget schema "pattern")) (format (jget schema "format")))
        (when (and (numberp minimum) (< size minimum)) (fail "string is shorter than ~a" minimum))
        (when (and (numberp maximum) (> size maximum)) (fail "string is longer than ~a" maximum))
        (when (and (stringp pattern)
                   (not (axllm/core::core-true-p (axllm/core::regex-test pattern value))))
          (fail "string does not match ~a" pattern))
        (when (equal format "uri")
          (unless (handler-case
                      (let ((uri (puri:parse-uri value)))
                        (and (puri:uri-scheme uri)
                             (or (not (member (puri:uri-scheme uri) '(:http :https :ftp)))
                                 (puri:uri-host uri))))
                    (error () nil))
            (fail "string is not a URI")))
        (when (equal format "date-time")
          (unless (%ucps-date-valid-p value)
            (fail "string is not an RFC 3339 date-time")))))
    (when (numberp value)
      (dolist (rule '(("minimum" < "number is below ~a")
                      ("maximum" > "number is above ~a")
                      ("exclusiveMinimum" <= "number must exceed ~a")
                      ("exclusiveMaximum" >= "number must be below ~a")))
        (let ((bound (jget schema (first rule))))
          (when (and (numberp bound) (funcall (second rule) value bound))
            (fail (third rule) bound)))))
    (when (axllm/core::core-array-p value)
      (let ((minimum (jget schema "minItems")) (maximum (jget schema "maxItems")))
        (when (and (numberp minimum) (< (length value) minimum))
          (fail "requires at least ~a items" minimum))
        (when (and (numberp maximum) (> (length value) maximum))
          (fail "allows at most ~a items" maximum)))
      (when (eq (jget schema "uniqueItems") true)
        (let ((seen (make-hash-table :test 'equal)))
          (loop for item across value for key = (encode-json item) do
            (when (gethash key seen) (fail "array items must be unique"))
            (setf (gethash key seen) t)))))))

(defun %ucps-validate (validator value root url)
  (let ((refs 0) (nodes 0))
    (labels
        ((visit (value schema document url ip sp depth)
           (when (> depth (%ucps-max-depth validator))
             (%ucps-error "UCP schema validation depth exceeded"))
           (when (> (incf nodes) (%ucps-max-nodes validator))
             (%ucps-error "UCP schema validation work limit exceeded"))
           (when (eq schema true) (return-from visit nil))
           (when (eq schema false) (%ucps-fail ip sp "boolean schema rejects value"))
           (unless (hash-table-p schema) (%ucps-error "UCP schema is not a schema at ~a" sp))
           (labels ((child (item sub token schema-token)
                      (visit item sub document url
                             (format nil "~a/~a" ip (%ucps-escape token))
                             (format nil "~a/~a" sp (%ucps-escape schema-token)) (1+ depth)))
                    (matches (sub keyword)
                      (handler-case (progn (child value sub "" keyword) t)
                        (ucp-schema-validation-error () nil))))
             (let ((ref (jget schema "$ref")))
               (when (stringp ref)
                 (when (> (incf refs) (%ucps-max-refs validator))
                   (%ucps-error "UCP schema reference limit exceeded"))
                 (multiple-value-bind (target target-url target-document target-path)
                     (%ucps-reference validator ref url document)
                   (visit value target target-document target-url ip target-path (1+ depth)))))
             (%ucps-leaf value schema ip sp)
             (dolist (keyword '("allOf" "anyOf" "oneOf"))
               (let ((branches (jget schema keyword)) (count 0))
                 (when (axllm/core::core-array-p branches)
                   (loop for branch across branches for index from 0
                         for token = (format nil "~a/~d" keyword index) do
                     (if (equal keyword "allOf") (child value branch "" token)
                         (when (matches branch token) (incf count))))
                   (when (or (and (equal keyword "anyOf") (zerop count))
                             (and (equal keyword "oneOf") (/= count 1)))
                     (%ucps-fail ip sp "~a matched ~d schemas" keyword count)))))
             (when (and (nth-value 1 (gethash "not" schema)) (matches (jget schema "not") "not"))
               (%ucps-fail ip sp "value matches forbidden not schema"))
             (when (nth-value 1 (gethash "if" schema))
               (let ((branch (if (matches (jget schema "if") "if") "then" "else")))
                 (when (nth-value 1 (gethash branch schema)) (child value (jget schema branch) "" branch))))
             (%ucps-constraints value schema ip sp)
             (when (and (axllm/core::core-array-p value) (nth-value 1 (gethash "items" schema)))
               (loop for item across value for i from 0 do
                 (child item (jget schema "items") (write-to-string i) "items")))
             (when (hash-table-p value)
               (let ((required (jget schema "required"))
                     (properties (jget schema "properties")) (patterns (jget schema "patternProperties"))
                     (additional (jget schema "additionalProperties")))
                 (when (axllm/core::core-array-p required)
                   (loop for key across required do
                     (when (and (stringp key) (not (nth-value 1 (gethash key value))))
                       (%ucps-fail ip sp "missing required property ~a" key))))
                 (dolist (key (%object-keys value))
                   (let ((item (gethash key value))
                         (known (and (hash-table-p properties) (nth-value 1 (gethash key properties))))
                         (matched nil))
                     (when known
                       (visit item (gethash key properties) document url
                              (format nil "~a/~a" ip (%ucps-escape key))
                              (format nil "~a/properties/~a" sp (%ucps-escape key)) (1+ depth)))
                     (when (hash-table-p patterns)
                       (dolist (pattern (%object-keys patterns))
                         (when (axllm/core::core-true-p (axllm/core::regex-test pattern key))
                           (setf matched t)
                           (visit item (gethash pattern patterns) document url
                                  (format nil "~a/~a" ip (%ucps-escape key))
                                  (format nil "~a/patternProperties/~a" sp (%ucps-escape pattern)) (1+ depth)))))
                     (unless (or known matched)
                       (when (eq additional false) (%ucps-fail ip sp "additional property ~a is not allowed" key))
                       (when (hash-table-p additional)
                         ;; TS retains the parent diagnostic path here. Increment
                         ;; depth anyway: its missing increment must not defeat bounds.
                         (visit item additional document url ip sp (1+ depth)))))))))))
      (visit value root root url "" "#" 0))))

(defun %ucps-check-value (validator value)
  ;; Bound JSON serialization too (enum/const/uniqueItems), and reject host cycles.
  (let ((active (make-hash-table :test 'eq)) (nodes 0))
    (labels ((walk (item depth)
               (when (> (incf nodes) (%ucps-max-nodes validator))
                 (%ucps-error "UCP schema validation work limit exceeded"))
               (when (> depth (%ucps-max-json-depth validator))
                 (%ucps-error "UCP instance JSON depth exceeded"))
               (when (or (hash-table-p item) (axllm/core::core-array-p item))
                 (when (gethash item active) (%ucps-error "Cyclic UCP instance value"))
                 (setf (gethash item active) t)
                 (if (hash-table-p item)
                     (maphash (lambda (key v) (declare (ignore key)) (walk v (1+ depth))) item)
                     (loop for v across item do (walk v (1+ depth))))
                 (remhash item active))))
      (walk value 0))))

(defun ucp-schema-validate (validator value schema-url)
  "Validate VALUE against SCHEMA-URL. Return T or signal UCP-SCHEMA-ERROR.
Only UCP-SCHEMA-VALIDATION-ERROR denotes a nonmatching value; transport,
reference, policy and resource errors must not be swallowed by anyOf/not/if."
  (handler-case
      (sb-ext:with-timeout (%ucps-timeout validator)
        (sb-thread:with-mutex ((%ucps-lock validator))
          (%ucps-check-value validator value)
          (let* ((url (%ucps-url schema-url)) (root (%ucps-load-document validator url)))
            (%ucps-validate validator value root url)
            t)))
    (ucp-schema-error (e) (error e))
    (sb-ext:timeout () (%ucps-error "UCP schema validation timeout exceeded"))
    (error (e) (%ucps-error "UCP schema validation failed: ~a" e))))

(defun ucp-schema-validation-callback (validator)
  "Adapt VALIDATOR to a client's (VALUE SCHEMA-URL) validation callback."
  (lambda (value schema-url) (ucp-schema-validate validator value schema-url)))
