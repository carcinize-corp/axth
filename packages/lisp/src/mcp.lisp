;;;; mcp.lisp --- the native MCP and UCP boundaries.
;;;;
;;;; MCP is a live protocol here, not a function-conversion utility. Ax's
;;;; protocol semantics live in Core (ir/axcore/mcp.axir, emitted into
;;;; src/core.lisp): era classification, request metadata, header plans and
;;;; parameter bindings, extension negotiation, MRTR rounds, Tasks v2
;;;; validation and terminal outcomes, subscription selection/ownership and
;;;; listen interests, catalog cache folding, JSON-RPC shaping, error
;;;; normalization, tool authorization context, inheritance plans, and the
;;;; whole OAuth middle tier.
;;;;
;;;; This file owns only what a portable IR cannot: the transport protocol
;;;; and its session, task and subscription lifecycle, request-id
;;;; allocation, the live catalogs and read cache, inbound dispatch and
;;;; listener registration, host handler invocation, cancellation, token
;;;; stores, and the SSRF gate. No era rule, header rule, task rule,
;;;; subscription rule or OAuth rule is decided here.
;;;;
;;;; SAMPLING, in both directions, is implemented and Core-owned:
;;;;
;;;;   Modern MRTR sampling goes through Core's mcp_mrtr_plan_fulfillment,
;;;;   which takes has_sampling and marks a sampling/createMessage round
;;;;   pending for the host handler to answer.
;;;;
;;;;   Legacy inbound sampling/createMessage goes through Core's
;;;;   mcp_server_request_plan_full, whose has_sampling branch validates the
;;;;   request shape and returns action "sampling" for the host handler.
;;;;   The three-argument mcp_server_request_plan still exists and still
;;;;   answers -32601 for sampling, because the other generated ports call
;;;;   it; this client calls the four-argument form.
;;;;
;;;; Either way a truthy :SAMPLING option that is not a handler function is
;;;; rejected at initialization, so the client never advertises a capability
;;;; it cannot answer.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Conditions and constants
;;; ------------------------------------------------------------------

(define-condition mcp-error (ax-error)
  ((code :initarg :code :initform :null :reader mcp-error-code)
   (data :initarg :data :initform :null :reader mcp-error-data))
  (:documentation
   "An MCP protocol or transport failure. CODE and DATA carry the JSON-RPC
error fields when the failure came from the wire, so era renegotiation and
catalog-revalidation decisions can be made on the code rather than on text."))

(defun %mcp-fail (format-control &rest arguments)
  (error 'mcp-error :message (apply #'format nil format-control arguments)))

(defun %mcp-protocol-constants ()
  (axllm/core::mcp-protocol-constants))

(defun mcp-protocol-version ()
  "The MCP protocol version this client offers a legacy server."
  (jget (%mcp-protocol-constants) "protocolVersion"))

(defun mcp-modern-protocol-version ()
  "The stateless modern MCP protocol version."
  (jget (%mcp-protocol-constants) "modernProtocolVersion"))

(defun mcp-supported-protocol-versions ()
  "Every MCP protocol version this client accepts, newest first."
  (jget (%mcp-protocol-constants) "supportedProtocolVersions"))

(defparameter +mcp-client-info+
  (object "name" "AxMCPClient" "title" "Ax MCP Client" "version" "1.0.0"))

;;; ------------------------------------------------------------------
;;; Small JSON and text helpers
;;; ------------------------------------------------------------------

(defun %mcp-json-clone (value)
  "A deep copy of VALUE through JSON, as the other ports clone snapshots."
  (if (or (stringp value) (realp value) (keywordp value) (symbolp value))
      value
      (parse-json (encode-json value))))

(defun %mcp-text (value)
  (cond ((stringp value) value)
        ((eq value :null) "")
        ((null value) "")
        (t (axllm/core::core-js-text value))))

(defun %mcp-present-key-p (object key)
  (and (hash-table-p object) (nth-value 1 (gethash key object))))

(defun %mcp-capability-present-p (value)
  "Whether a capability entry claims support: present and not false."
  (and (not (eq value :null)) (not (json-false-p value))))

(defun %mcp-object-or-empty (value)
  (if (hash-table-p value) value (object)))

(defun %mcp-string-vector (values)
  (let ((out (%new-array)))
    (map nil (lambda (value) (vector-push-extend (%mcp-text value) out)) values)
    out))

(defun %mcp-sorted-strings (values)
  (sort (map 'list #'%mcp-text values) #'string<))

(defun %mcp-random-octets (count)
  "COUNT cryptographically random bytes.

/dev/urandom is the source; there is no in-image fallback, because a PKCE
verifier built from a predictable generator is worse than a clear failure."
  (with-open-file (stream "/dev/urandom" :element-type '(unsigned-byte 8)
                                         :if-does-not-exist nil)
    (unless stream
      (%mcp-fail "MCP needs /dev/urandom for PKCE and request identifiers"))
    (let ((out (make-array count :element-type '(unsigned-byte 8))))
      (unless (= count (read-sequence out stream))
        (%mcp-fail "MCP could not read ~a random bytes" count))
      out)))

(defun %mcp-uuid ()
  "A random (version 4) UUID string."
  (let ((bytes (%mcp-random-octets 16)))
    (setf (aref bytes 6) (logior #x40 (logand #x0f (aref bytes 6)))
          (aref bytes 8) (logior #x80 (logand #x3f (aref bytes 8))))
    (format nil "~(~2,'0x~2,'0x~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~)"
            (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
            (aref bytes 4) (aref bytes 5) (aref bytes 6) (aref bytes 7)
            (aref bytes 8) (aref bytes 9) (aref bytes 10) (aref bytes 11)
            (aref bytes 12) (aref bytes 13) (aref bytes 14) (aref bytes 15))))

(defun %mcp-sha256 (octets)
  "SHA-256 of OCTETS, from OpenSSL through CFFI.

PKCE S256 is protocol-mandatory, so the digest comes from the library
already linked into this image by cl+ssl rather than from a hand-written
implementation in this file."
  (let ((count (length octets)))
    (cffi:with-foreign-objects ((input :unsigned-char (max 1 count))
                                (digest :unsigned-char 64)
                                (size :unsigned-int))
      (dotimes (index count)
        (setf (cffi:mem-aref input :unsigned-char index) (aref octets index)))
      (let ((algorithm (cffi:foreign-funcall "EVP_sha256" :pointer)))
        (when (cffi:null-pointer-p algorithm)
          (%mcp-fail "OpenSSL has no SHA-256; MCP PKCE cannot be computed"))
        (unless (= 1 (cffi:foreign-funcall "EVP_Digest"
                                           :pointer input :unsigned-long count
                                           :pointer digest :pointer size
                                           :pointer algorithm
                                           :pointer (cffi:null-pointer)
                                           :int))
          (%mcp-fail "OpenSSL SHA-256 failed")))
      (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
        (dotimes (index 32)
          (setf (aref out index) (cffi:mem-aref digest :unsigned-char index)))
        out))))

(defun %mcp-utf8 (text)
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun %mcp-from-utf8 (octets)
  (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                           :external-format :utf-8))

(defun %mcp-base64 (octets)
  (cl-base64:usb8-array-to-base64-string (coerce octets '(vector (unsigned-byte 8)))))

(defun %mcp-base64url (octets)
  "Base64url without padding, as OAuth and PKCE require."
  (let ((text (%mcp-base64 octets)))
    (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ text)))))

(defun %mcp-hex (octets)
  (string-downcase (with-output-to-string (stream)
                     (map nil (lambda (byte) (format stream "~2,'0x" byte)) octets))))

;;; ------------------------------------------------------------------
;;; Cancellation context
;;; ------------------------------------------------------------------

(defun %mcp-check-context (context)
  "Signal when CONTEXT's cancellation token has been cancelled.

Called before a request is built, after it is sent and around each blocking
wait, so a cancelled program stops at the next boundary instead of after the
whole round trip."
  (when (hash-table-p context)
    (let ((token (jget context "cancellation")))
      (when (typep token 'cancellation-token)
        (cancellation-token-throw-if-cancelled token))))
  nil)

;;; ------------------------------------------------------------------
;;; Transport protocol
;;; ------------------------------------------------------------------
;;;
;;; A transport moves JSON-RPC messages and owns its own connection,
;;; session and listening lifecycle. Everything else about MCP is the
;;; client's. An application-owned binding only has to subclass
;;; MCP-TRANSPORT and define MCP-TRANSPORT-SEND and
;;; MCP-TRANSPORT-SEND-NOTIFICATION; the defaults below cover the rest.

(defclass mcp-transport ()
  ((message-handler :initform nil :accessor %transport-message-handler)
   (request-handler :initform nil :accessor %transport-request-handler)
   (lifecycle-handler :initform nil :accessor %transport-lifecycle-handler)
   (protocol-version :initform :null :accessor mcp-transport-protocol-version))
  (:documentation "Base class for every MCP transport."))

(defgeneric mcp-transport-send (transport message)
  (:documentation "Send a JSON-RPC request and return its response object."))

(defgeneric mcp-transport-send-notification (transport message)
  (:documentation "Send a JSON-RPC notification; no response is expected."))

(defgeneric mcp-transport-send-with-headers (transport message headers)
  (:documentation "Send MESSAGE with per-request HEADERS when the transport has them.")
  (:method ((transport mcp-transport) message headers)
    (declare (ignore headers))
    (mcp-transport-send transport message)))

(defgeneric mcp-transport-send-with-context (transport message headers context)
  (:documentation
   "Send MESSAGE under a cancellation CONTEXT. The default checks the token
on both sides of the call; a transport that can abort an in-flight request
overrides this to do so.")
  (:method ((transport mcp-transport) message headers context)
    (%mcp-check-context context)
    (let ((result (mcp-transport-send-with-headers transport message headers)))
      (%mcp-check-context context)
      result)))

(defgeneric mcp-transport-send-response (transport message)
  (:documentation "Answer a server-initiated request.")
  (:method ((transport mcp-transport) message)
    (mcp-transport-send-notification transport message)))

(defgeneric mcp-transport-set-message-handler (transport handler)
  (:method ((transport mcp-transport) handler)
    (setf (%transport-message-handler transport) handler)))

(defgeneric mcp-transport-set-request-handler (transport handler)
  (:method ((transport mcp-transport) handler)
    (setf (%transport-request-handler transport) handler)))

(defgeneric mcp-transport-set-lifecycle-handler (transport handler)
  (:method ((transport mcp-transport) handler)
    (setf (%transport-lifecycle-handler transport) handler)))

(defgeneric mcp-transport-set-protocol-version (transport version)
  (:method ((transport mcp-transport) version)
    (setf (mcp-transport-protocol-version transport) version)))

(defgeneric mcp-transport-set-era (transport era)
  (:documentation "Tell the transport which wire model the client classified.")
  (:method ((transport mcp-transport) era) (declare (ignore era)) nil))

(defgeneric mcp-transport-era-hint (transport)
  (:documentation "A known era for this binding, or :NULL to let the client probe.")
  (:method ((transport mcp-transport)) :null))

(defgeneric mcp-transport-era-cache-key (transport)
  (:documentation "A stable key for remembering this endpoint's era, or :NULL.")
  (:method ((transport mcp-transport)) :null))

(defgeneric mcp-transport-connect (transport)
  (:method ((transport mcp-transport)) nil))

(defgeneric mcp-transport-start-listening (transport)
  (:documentation "Begin a legacy server-to-client stream.")
  (:method ((transport mcp-transport)) nil))

(defgeneric mcp-transport-open-request-stream (transport message)
  (:documentation "Begin a modern long-running request stream.")
  (:method ((transport mcp-transport) message)
    (declare (ignore message))
    (%mcp-fail "Request streams are only available for modern MCP")))

(defgeneric mcp-transport-close-request-stream (transport)
  (:method ((transport mcp-transport)) nil))

(defgeneric mcp-transport-send-batch (transport messages &key context)
  (:documentation
   "Send a JSON-RPC batch and return the responses in order.

Not every binding has one: batching is negotiated and only MCP 2025-03-26
allows it, so the default refuses rather than silently sending the messages
one at a time, which would look like success while violating the batch
semantics the caller asked for.")
  (:method ((transport mcp-transport) messages &key context)
    (declare (ignore messages context))
    (%mcp-fail "This MCP transport does not support batching")))

(defgeneric mcp-transport-take-request-metadata (transport id)
  (:documentation
   "Per-request metadata the transport accumulated for ID, or :NULL.

Taking it is destructive by name and by intent: the metadata belongs to one
completed request and must not be reported again for the next one.")
  (:method ((transport mcp-transport) id) (declare (ignore id)) :null))

(defgeneric mcp-transport-terminate-session (transport)
  (:documentation
   "End the transport's session, if it has one. A no-op where it does not.")
  (:method ((transport mcp-transport)) nil))

(defgeneric mcp-transport-close (transport)
  (:method ((transport mcp-transport)) nil))

(defun mcp-transport-dispatch-inbound (transport message)
  "Route one inbound message: a server request to the request handler and
its reply back out, anything else to the message handler."
  (let ((request-handler (%transport-request-handler transport)))
    (if (and request-handler
             (%mcp-present-key-p message "id")
             (%mcp-present-key-p message "method"))
        (mcp-transport-send-response transport (funcall request-handler message))
        (let ((handler (%transport-message-handler transport)))
          (when handler (funcall handler message)))))
  nil)

(defun %mcp-transport-lifecycle (transport state)
  (let ((handler (%transport-lifecycle-handler transport)))
    (when handler (funcall handler state)))
  nil)

;;; ------------------------------------------------------------------
;;; Scripted transport
;;; ------------------------------------------------------------------

(defclass mcp-scripted-transport (mcp-transport)
  ((responses :initarg :responses :accessor %scripted-responses)
   (requests :initform (%new-array) :reader mcp-scripted-requests)
   (notifications :initform (%new-array) :reader mcp-scripted-notifications)
   (sent-responses :initform (%new-array) :reader mcp-scripted-sent-responses)
   (request-headers :initform (%new-array) :reader mcp-scripted-request-headers)
   (request-streams :initform (%new-array) :reader mcp-scripted-request-streams)
   (era :initform :null :accessor mcp-scripted-era)
   (session-id :initform :null :accessor mcp-scripted-session-id))
  (:documentation
   "A deterministic transport that answers from a script and records what it
was asked. The protocol-level counterpart of a replay fixture: no sockets,
no processes, no time."))

(defun make-mcp-scripted-transport (&optional responses)
  "A scripted transport.

RESPONSES is a sequence of objects, each optionally keyed by \"method\" and
carrying \"result\", \"error\" or \"headers\". The first entry whose method
matches (or which has no method) answers a request and is consumed."
  (make-instance 'mcp-scripted-transport
                 :responses (coerce (if responses (coerce responses 'list) '()) 'list)))

(defmethod mcp-transport-send ((transport mcp-scripted-transport) message)
  (vector-push-extend (%mcp-json-clone message) (mcp-scripted-requests transport))
  (let* ((method (jget message "method"))
         (match (find-if (lambda (response)
                           (axllm/core::core-value-equal (jget response "method" method) method))
                         (%scripted-responses transport)))
         (raw (or match (object "result" (object)))))
    (when match
      (setf (%scripted-responses transport)
            (remove match (%scripted-responses transport) :count 1 :test #'eq)))
    (let ((headers (let ((value (jget raw "headers")))
                     (if (hash-table-p value) value (%mcp-object-or-empty (jget raw "responseHeaders"))))))
      (let ((session (jget headers "MCP-Session-Id")))
        (when (stringp session) (setf (mcp-scripted-session-id transport) session))))
    (if (%mcp-present-key-p raw "error")
        (object "jsonrpc" "2.0" "id" (jget message "id") "error" (jget raw "error"))
        (object "jsonrpc" "2.0" "id" (jget message "id")
                "result" (jget raw "result" (object))))))

(defmethod mcp-transport-send-with-headers ((transport mcp-scripted-transport) message headers)
  (vector-push-extend (%mcp-object-or-empty headers) (mcp-scripted-request-headers transport))
  (mcp-transport-send transport message))

(defmethod mcp-transport-send-notification ((transport mcp-scripted-transport) message)
  (vector-push-extend (%mcp-json-clone message) (mcp-scripted-notifications transport))
  nil)

(defmethod mcp-transport-send-response ((transport mcp-scripted-transport) message)
  (vector-push-extend (%mcp-json-clone message) (mcp-scripted-sent-responses transport))
  nil)

(defmethod mcp-transport-set-era ((transport mcp-scripted-transport) era)
  (setf (mcp-scripted-era transport) era))

(defun mcp-scripted-clients (spec &key (era "modern"))
  "Build a live MCP client per entry in SPEC. Returns (values clients transports).

SPEC is the shape the shared agent and MCP fixtures record their clients in:
an array of objects carrying \"namespace\" and \"responses\". Each entry becomes a
scripted transport and a client over it, initialized lazily like any other
client, so an attached agent or execution context exercises the real
protocol path rather than a stub.

TRANSPORTS is a namespace-keyed object, so a caller can assert on exactly
what each server was asked. This exists so a test in another file does not
have to know how a client is wired; it is the supported way to obtain
deterministic protocol clients."
  (let ((clients '())
        (transports (object)))
    (loop for entry across (%event-array spec)
          do (let* ((namespace (%mcp-text (jget entry "namespace")))
                    (transport (make-mcp-scripted-transport
                                (%event-array (jget entry "responses"))))
                    (client (make-mcp-client transport
                                             "namespace" namespace
                                             "era" (let ((configured (jget entry "era")))
                                                     (if (stringp configured) configured era)))))
               (%set-key transports namespace transport)
               (push client clients)))
    (values (nreverse clients) transports)))

(defun mcp-scripted-tool-calls (transport)
  "The tools/call requests TRANSPORT saw, as {name, arguments} objects.

This is what a fixture asserting \"which tools did this server actually get
asked for\" needs, without reaching into the transport's recorded requests."
  (let ((out (%new-array)))
    (loop for request across (mcp-scripted-requests transport)
          do (when (equal (jget request "method") "tools/call")
               (let ((params (%mcp-object-or-empty (jget request "params"))))
                 (vector-push-extend (object "name" (jget params "name")
                                             "arguments" (jget params "arguments"))
                                     out))))
    out))

(defun mcp-scripted-methods (transport)
  "The method names TRANSPORT saw, in order."
  (let ((out (%new-array)))
    (loop for request across (mcp-scripted-requests transport)
          do (vector-push-extend (jget request "method") out))
    out))

(defun mcp-scripted-emit (transport message)
  "Deliver MESSAGE as if the server had sent it."
  (mcp-transport-dispatch-inbound transport message))

(defmethod mcp-transport-open-request-stream ((transport mcp-scripted-transport) message)
  (unless (equal (mcp-scripted-era transport) "modern")
    (%mcp-fail "Request streams are only available for modern MCP"))
  (let ((request (%mcp-json-clone message)))
    (vector-push-extend request (mcp-scripted-request-streams transport))
    ;; A modern server acknowledges the listen request with the interests it
    ;; accepted; the client keys later notifications on that subscription id.
    (mcp-scripted-emit
     transport
     (object "jsonrpc" "2.0"
             "method" "notifications/subscriptions/acknowledged"
             "params" (object "notifications"
                              (%mcp-object-or-empty (jget (%mcp-object-or-empty (jget request "params"))
                                                      "notifications"))
                              "_meta"
                              (object "io.modelcontextprotocol/subscriptionId"
                                      (jget request "id")))))))

;;; ------------------------------------------------------------------
;;; Token sets, OAuth options and token stores
;;; ------------------------------------------------------------------

(defun mcp-token-set (&key access-token refresh-token expires-at issuer token-type scope)
  "An OAuth token set as the JSON object Core's OAuth helpers read."
  (let ((out (object "accessToken" (or access-token ""))))
    (when refresh-token (%set-key out "refreshToken" refresh-token))
    (when expires-at (%set-key out "expiresAt" expires-at))
    (when issuer (%set-key out "issuer" issuer))
    (when token-type (%set-key out "tokenType" token-type))
    (when scope (%set-key out "scope" scope))
    out))

(defun mcp-oauth-options (&key client-id client-secret redirect-uri scopes
                               on-auth-code token-store ssrf-protection
                               require-iss grant-type resource
                               authorization-server-metadata)
  "Transport OAuth configuration.

TOKEN-STORE is an endpoint-keyed JSON object, or an object carrying
getToken/setToken/clearToken functions. ON-AUTH-CODE receives an
authorization URL and returns an object with code, state and iss; it owns
whatever browser or headless interaction the host needs.

Only the portable middle tier is here: RFC 9728 discovery, RFC 8414/OIDC
authorization-server metadata, PKCE S256, RFC 8707 resource binding,
refresh, client credentials and RFC 9207 iss. The client authentication
methods are none and client_secret_post."
  (let ((out (object)))
    (when client-id (%set-key out "clientId" client-id))
    (when client-secret (%set-key out "clientSecret" client-secret))
    (when redirect-uri (%set-key out "redirectUri" redirect-uri))
    (when scopes (%set-key out "scopes" (%mcp-string-vector scopes)))
    (when on-auth-code (%set-key out "onAuthCode" on-auth-code))
    (when token-store (%set-key out "tokenStore" token-store))
    (when ssrf-protection (%set-key out "ssrfProtection" ssrf-protection))
    (%set-key out "requireIss" (json-boolean require-iss))
    (when grant-type (%set-key out "grantType" grant-type))
    (when resource (%set-key out "resource" resource))
    (when authorization-server-metadata
      (%set-key out "authorizationServerMetadata" authorization-server-metadata))
    out))

(defun %mcp-token-store-get (store key)
  (when (hash-table-p store)
    (let ((getter (jget store "getToken")))
      (if (functionp getter)
          (let ((token (funcall getter key)))
            (and (hash-table-p token) token))
          (let ((token (jget store key)))
            (and (hash-table-p token) token))))))

(defun %mcp-token-store-set (store key token)
  (when (hash-table-p store)
    (let ((setter (jget store "setToken")))
      (if (functionp setter)
          (funcall setter key token)
          (%set-key store key token))))
  nil)

(defun %mcp-token-store-clear (store key)
  (when (hash-table-p store)
    (let ((clearer (jget store "clearToken")))
      (if (functionp clearer)
          (funcall clearer key)
          (remhash key store))))
  nil)

;;; ------------------------------------------------------------------
;;; SSRF gate and endpoint validation
;;; ------------------------------------------------------------------

(defun %mcp-parse-ipv4 (host)
  "HOST's four octets, or NIL when it is not a dotted-quad address."
  (let ((parts (cl-ppcre:split "\\." host)))
    (when (= 4 (length parts))
      (let ((octets '()))
        (dolist (part parts)
          (unless (and (plusp (length part)) (every #'digit-char-p part))
            (return-from %mcp-parse-ipv4 nil))
          (let ((value (parse-integer part)))
            (unless (<= 0 value 255) (return-from %mcp-parse-ipv4 nil))
            (push value octets)))
        (nreverse octets)))))

(defun %mcp-ipv4-class (octets)
  "The classes OCTETS belongs to, as keywords."
  (destructuring-bind (a b c d) octets
    (declare (ignore c d))
    (let ((classes '()))
      (when (= a 127) (push :loopback classes))
      (when (or (= a 10)
                (and (= a 172) (<= 16 b 31))
                (and (= a 192) (= b 168)))
        (push :private classes))
      (when (and (= a 169) (= b 254)) (push :link-local classes))
      (when (<= 224 a 239) (push :multicast classes))
      (when (= a 0) (push :unspecified classes))
      (when (>= a 240) (push :reserved classes))
      (when (and (= a 100) (<= 64 b 127)) (push :reserved classes))
      classes)))

(defun %mcp-ipv6-class (host)
  "The classes an IPv6 literal HOST belongs to, or NIL when it is not one."
  (let ((text (string-downcase (string-trim "[]" host))))
    (unless (find #\: text) (return-from %mcp-ipv6-class nil))
    (let ((classes '(:ipv6)))
      (cond ((string= text "::1") (push :loopback classes))
            ((string= text "::") (push :unspecified classes))
            ((and (>= (length text) 2)
                  (member (subseq text 0 2) '("fc" "fd") :test #'string=))
             (push :private classes))
            ((and (>= (length text) 4)
                  (member (subseq text 0 4) '("fe80" "fe90" "fea0" "feb0") :test #'string=))
             (push :link-local classes))
            ((and (>= (length text) 2) (string= (subseq text 0 2) "ff"))
             (push :multicast classes)))
      classes)))

(defun %mcp-ssrf-option (options &rest names)
  (dolist (name names)
    (let ((value (jget (%mcp-object-or-empty options) name)))
      (unless (eq value :null) (return-from %mcp-ssrf-option value))))
  :null)

(defun mcp-validate-endpoint (endpoint &optional options)
  "ENDPOINT, checked against the SSRF policy, or an error.

The gate is closed by default: only http and https, https required, and a
loopback, private, link-local, multicast, unspecified or reserved address
rejected. OPTIONS may relax exactly three things for controlled local
development: requireHttps, allowLocalhost and allowPrivateNetworks. An
unresolvable or hostless URL is rejected rather than attempted."
  (unless (stringp endpoint)
    (%mcp-fail "MCP endpoint must be a string"))
  (let* ((uri (handler-case (puri:parse-uri endpoint)
                (error () (%mcp-fail "MCP endpoint is not a valid URL"))))
         (scheme (string-downcase (string (or (puri:uri-scheme uri) ""))))
         (host (or (puri:uri-host uri) ""))
         (require-https (let ((value (%mcp-ssrf-option options "requireHttps" "require_https")))
                          (if (eq value :null) t (axllm/core::core-true-p value))))
         (allow-localhost (axllm/core::core-true-p
                           (%mcp-ssrf-option options "allowLocalhost" "allow_localhost")))
         (allow-private (axllm/core::core-true-p
                         (%mcp-ssrf-option options "allowPrivateNetworks" "allow_private_networks"))))
    (unless (member scheme '("http" "https") :test #'string=)
      (%mcp-fail "MCP endpoint must use http or https"))
    (when (and require-https (not (string= scheme "https")))
      (%mcp-fail "MCP endpoint must use https"))
    (when (zerop (length host))
      (%mcp-fail "MCP endpoint must include a host"))
    (when (and (member (string-downcase host) '("localhost" "localhost.localdomain")
                       :test #'string=)
               (not allow-localhost))
      (%mcp-fail "MCP endpoint host is local"))
    (let ((classes (or (%mcp-ipv6-class host)
                       (let ((octets (%mcp-parse-ipv4 host)))
                         (and octets (or (%mcp-ipv4-class octets) '(:public)))))))
      (when classes
        (when (or (and (member :loopback classes) (not allow-localhost))
                  (and (member :private classes) (not allow-private))
                  (member :link-local classes)
                  (member :multicast classes)
                  (member :unspecified classes)
                  (member :reserved classes))
          (%mcp-fail "MCP endpoint host is not allowed by SSRF protection"))))
    endpoint))

;;; ------------------------------------------------------------------
;;; PKCE, framing and header encoding
;;; ------------------------------------------------------------------

(defun mcp-pkce-verifier ()
  "A fresh high-entropy PKCE code verifier."
  (%mcp-base64url (%mcp-random-octets 32)))

(defun mcp-pkce-challenge (verifier)
  "VERIFIER's S256 code challenge."
  (%mcp-base64url (%mcp-sha256 (%mcp-utf8 verifier))))

(defun mcp-stdio-encode (message)
  "MESSAGE as one newline-delimited JSON-RPC frame."
  (concatenate 'string (encode-json message) (string #\Newline)))

(defun mcp-stdio-decode (line)
  "One newline-delimited JSON-RPC frame as a message object."
  (parse-json (string-trim '(#\Space #\Tab #\Return #\Newline) line)))

(defun %mcp-encode-header-value (value)
  "VALUE as an HTTP header value, base64-wrapped when Core says it must be.

Core owns the decision, so every port wraps the same values the same way."
  (if (equal (%mcp-text (jget (axllm/core::mcp-header-value-plan value) "mode")) "plain")
      value
      (format nil "=?base64?~a?=" (%mcp-base64 (%mcp-utf8 value)))))

(defun %mcp-parse-sse-messages (text)
  "The JSON-RPC messages in the data: frames of an SSE body."
  (let ((out (%new-array)))
    (dolist (raw (cl-ppcre:split "\\n" (substitute #\Newline #\Return text)))
      (let ((line (string-trim '(#\Space #\Tab) raw)))
        (when (and (>= (length line) 5) (string= "data:" (subseq line 0 5)))
          (let ((data (string-trim '(#\Space #\Tab) (subseq line 5))))
            (when (and (plusp (length data)) (not (string= data "[DONE]")))
              (vector-push-extend (parse-json data) out))))))
    out))

;;; ------------------------------------------------------------------
;;; Tool conversion helpers
;;; ------------------------------------------------------------------

(defun %mcp-override-name (name options)
  (loop for override across (%event-array (jget options "functionOverrides"))
        when (equal (jget override "name") name)
          do (let ((updated (jget (%mcp-object-or-empty (jget override "updates")) "name")))
               (when (stringp updated) (return updated)))
        finally (return name)))

(defun %mcp-override-description (item options)
  (let* ((name (%mcp-text (jget item "name")))
         (description (let ((value (jget item "description")))
                        (if (stringp value)
                            value
                            (let ((title (jget item "title")))
                              (if (stringp title) title name))))))
    (loop for override across (%event-array (jget options "functionOverrides"))
          when (equal (%mcp-text (jget override "name")) name)
            do (let ((updated (jget (%mcp-object-or-empty (jget override "updates")) "description")))
                 (when (stringp updated) (return updated)))
          finally (return description))))

(defun %mcp-safe-tool-name (value)
  (let ((text (map 'string (lambda (char) (if (alphanumericp char) char #\_))
                   (%mcp-text value))))
    (let ((trimmed (string-trim "_" text)))
      (if (plusp (length trimmed)) trimmed "item"))))

(defun %mcp-content-to-value (content)
  "A tool result's content as the value a lossy function adapter returns."
  (let ((texts (loop for item across (%event-array content)
                     when (equal (jget item "type") "text")
                       collect (%mcp-text (jget item "text")))))
    (if texts
        (object "content" (format nil "~{~a~^~%~}" texts))
        (object "content" (%event-array content)))))

(defun %mcp-tool-schema (tool)
  (let ((schema (jget tool "inputSchema")))
    (if (hash-table-p schema)
        schema
        (object "type" "object" "properties" (object)))))

;;; A native tool keeps the server's name, description and JSON Schema
;;; exactly as the server published them. AXLLM:TOOL deliberately validates
;;; its schema fail-closed against the keyword subset it can enforce, which
;;; would reject or alter a legitimate MCP schema ($defs, $ref,
;;; additionalProperties, numeric bounds). A protocol catalog is not ours to
;;; rewrite, so native tools carry the raw schema and leave argument
;;; validation to the server that published it.

(defun native-tool (&key name description parameters handler protocol)
  "A native protocol tool: the server's own name, description and schema.

HANDLER is called with (arguments context); CONTEXT may be :NULL. The
handler is stored under a keyword key, so it never reaches JSON output."
  (let ((spec (object "name" name
                      "description" (or description "")
                      "parameters" (or parameters (object "type" "object"
                                                          "properties" (object))))))
    (setf (gethash :handler spec) handler)
    (when protocol (setf (gethash "protocol" spec) protocol))
    spec))

(defun native-tool-name (spec) (jget spec "name"))

(defun native-tool-description (spec) (jget spec "description"))

(defun native-tool-parameters (spec)
  "SPEC's JSON Schema, exactly as the server published it."
  (jget spec "parameters"))

(defun native-tool-handler (spec) (gethash :handler spec))

(defun native-tool-call (spec &optional arguments (context :null))
  "Invoke SPEC with ARGUMENTS under CONTEXT."
  (let ((handler (native-tool-handler spec)))
    (unless (functionp handler)
      (%mcp-fail "native tool ~a has no handler" (%mcp-text (jget spec "name"))))
    (funcall handler (%mcp-object-or-empty arguments) context)))

;;; ------------------------------------------------------------------
;;; Client
;;; ------------------------------------------------------------------

(defvar *mcp-era-cache* (make-hash-table :test #'equal :synchronized t)
  "Endpoint key to classified era, for this image.

Classification costs a round trip, so it is remembered. An :ERA-STORE
option persists the same decision across images.")

(defclass mcp-client ()
  ((transport :initarg :transport :reader mcp-client-transport)
   (options :initarg :options :reader mcp-client-options)
   (server-capabilities :initform (object) :accessor mcp-server-capabilities)
   (server-info :initform :null :accessor mcp-server-info)
   (server-instructions :initform :null :accessor mcp-server-instructions)
   (protocol-version :initform :null :accessor mcp-negotiated-protocol-version)
   (era :initform :null :accessor %client-era)
   (discover-result :initform :null :accessor mcp-discover-result)
   (extensions :initform (object) :accessor mcp-negotiated-extensions)
   (tools :initform (%new-array) :accessor mcp-client-tools)
   (prompts :initform (%new-array) :accessor mcp-client-prompts)
   (resources :initform (%new-array) :accessor mcp-client-resources)
   (resource-templates :initform (%new-array) :accessor mcp-client-resource-templates)
   (catalog-cache :initform (object) :accessor %client-catalog-cache)
   (read-cache :initform (make-hash-table :test #'equal) :reader %client-read-cache)
   (catalog-revision :initform 0 :accessor mcp-catalog-revision)
   (subscription-owners :initform (make-hash-table :test #'equal)
                        :reader %client-subscription-owners)
   (active-subscription :initform :null :accessor %client-active-subscription)
   (subscription-ready :initform (make-cancellation-token) :accessor %client-subscription-ready)
   (restart-lock :initform (sb-thread:make-mutex :name "ax-mcp-listen") :reader %client-restart-lock)
   (next-id :initform 1 :accessor %client-next-id)
   (id-lock :initform (sb-thread:make-mutex :name "ax-mcp-request-id") :reader %client-id-lock)
   (notification-listeners :initform '() :accessor %client-notification-listeners)
   (lifecycle-listeners :initform '() :accessor %client-lifecycle-listeners)
   (tasks :initform (make-hash-table :test #'equal) :reader %client-tasks)
   (initialized :initform nil :accessor %client-initialized))
  (:documentation
   "A live MCP client: one protocol session, its catalogs, its subscriptions
and its tasks. Pass the client itself through Ax rather than converting it
to functions; MCP-TO-FUNCTION is a lossy adapter for old applications."))

(defun make-mcp-client (transport &rest options)
  "An MCP client over TRANSPORT.

Useful options:

  :namespace          a stable, unique name for this server
  :era                \"auto\" (default), \"legacy\" or \"modern\"
  :era-store          a JSON object that persists the classified era
  :read-cache         honour server cache metadata on resources/read
  :roots              a JSON array answering roots/list
  :elicitation        a handler (params context) for elicitation/create
  :sampling           a handler (params context) for modern MRTR sampling
  :authorize-tool-call  a predicate run before a tool call reaches the wire
  :log-level          a logging level sent in modern request metadata
  :client-info        overrides for the advertised client identity

The client is not initialized until MCP-INIT, so constructing one performs
no IO.

OPTIONS is a plain &REST list of alternating keys and values, not a &KEY
lambda list: a key may be a :KEBAB-CASE keyword or the camelCase string the
other Ax ports use, and both normalize to one options table."
  (let ((table (object)))
    (loop for (key value) on options by #'cddr
          do (%set-key table (%mcp-option-name key) value))
    (let ((client (make-instance 'mcp-client :transport transport :options table)))
      (mcp-transport-set-message-handler
       transport (lambda (message) (%mcp-handle-inbound client message)))
      (mcp-transport-set-request-handler
       transport (lambda (message) (%mcp-handle-server-request client message)))
      (mcp-transport-set-lifecycle-handler
       transport (lambda (state) (mcp-emit-lifecycle client state)))
      client)))

(defun %mcp-option-name (key)
  "A :KEBAB-CASE keyword as the camelCase option name Core and the other
ports use, so one options table serves both."
  (if (stringp key)
      key
      (let ((parts (cl-ppcre:split "-" (string-downcase (symbol-name key)))))
        (apply #'concatenate 'string (first parts)
               (mapcar (lambda (part)
                         (if (plusp (length part))
                             (concatenate 'string (string (char-upcase (char part 0)))
                                          (subseq part 1))
                             part))
                       (rest parts))))))

(defun %mcp-option (client name &optional (fallback :null))
  (jget (mcp-client-options client) name fallback))

(defun %mcp-handler-option (client name)
  (let ((value (%mcp-option client name)))
    (and (functionp value) value)))

(defun mcp-namespace (client)
  "CLIENT's stable namespace."
  (let ((configured (%mcp-option client "namespace")))
    (if (and (stringp configured) (plusp (length configured)))
        configured
        (let ((name (jget (%mcp-object-or-empty (mcp-server-info client)) "name")))
          (if (and (stringp name) (plusp (length name))) name "mcp")))))

(defun mcp-get-era (client)
  "The era CLIENT classified, or :NULL before initialization."
  (%client-era client))

;;; --- initialization and era classification ------------------------

(defun mcp-init (client)
  "Initialize CLIENT once: classify the era, negotiate, and load catalogs."
  (when (%client-initialized client) (return-from mcp-init client))
  (let ((sampling (%mcp-option client "sampling")))
    (when (and (not (eq sampling :null)) (not (functionp sampling)))
      ;; A truthy flag is not a handler. Advertising sampling without one
      ;; would make the client lie about what it can answer.
      (%mcp-fail "MCP sampling is not supported without a host handler function")))
  (mcp-transport-connect (mcp-client-transport client))
  (let* ((configured (let ((value (%mcp-option client "era"))) (if (stringp value) value "auto")))
         (key (mcp-transport-era-cache-key (mcp-client-transport client)))
         (era-store (%mcp-option client "eraStore"))
         (stored (if (hash-table-p era-store) (jget era-store (%mcp-text key)) :null))
         (resolution (axllm/core::mcp-resolve-known-era
                      configured
                      (mcp-transport-era-hint (mcp-client-transport client))
                      (gethash (%mcp-text key) *mcp-era-cache* :null)
                      stored))
         (resolved (%mcp-text (jget resolution "era" "modern"))))
    (if (axllm/core::core-true-p (jget resolution "probe"))
        (%mcp-init-probing client)
        (progn
          (if (string= resolved "legacy")
              (%mcp-initialize-legacy client)
              (progn (%mcp-apply-era client "modern")
                     (%mcp-apply-discovery client (%mcp-request-discovery client))
                     (mcp-refresh client)))
          (%mcp-remember-era client resolved)
          (setf (%client-initialized client) t)
          (when (string= resolved "legacy")
            (mcp-transport-start-listening (mcp-client-transport client))))))
  client)

(defun %mcp-init-probing (client)
  "Probe for a modern endpoint, falling back to a legacy session.

A -32022 version error is the server telling us its protocol list, not that
it is legacy, so it propagates instead of triggering a fallback."
  (%mcp-apply-era client "modern")
  (handler-case
      (%mcp-apply-discovery client (%mcp-request-discovery client))
    (mcp-error (condition)
      (when (eql (mcp-error-code condition) -32022) (error condition))
      (return-from %mcp-init-probing (%mcp-fall-back-to-legacy client)))
    (error () (return-from %mcp-init-probing (%mcp-fall-back-to-legacy client))))
  (%mcp-remember-era client "modern")
  (mcp-refresh client)
  (setf (%client-initialized client) t))

(defun %mcp-fall-back-to-legacy (client)
  (%mcp-initialize-legacy client)
  (%mcp-remember-era client "legacy")
  (setf (%client-initialized client) t)
  (mcp-transport-start-listening (mcp-client-transport client)))

(defun %mcp-initialize-legacy (client)
  (%mcp-apply-era client "legacy")
  (let* ((version (let ((value (%mcp-option client "protocolVersion")))
                    (if (stringp value) value (mcp-protocol-version))))
         (result (%mcp-request client "initialize"
                           (object "protocolVersion" version
                                   "capabilities" (%mcp-client-capabilities client)
                                   "clientInfo" (axllm/core::core-map-merge
                                                 +mcp-client-info+
                                                 (%mcp-object-or-empty (%mcp-option client "clientInfo"))))))
         (supported (let ((value (%mcp-option client "supportedProtocolVersions")))
                      (if (%array-p value) value (mcp-supported-protocol-versions))))
         (negotiated (jget result "protocolVersion")))
    (unless (find negotiated supported :test #'axllm/core::core-value-equal)
      (%mcp-fail "Unsupported MCP protocol version ~a" (%mcp-text negotiated)))
    (setf (mcp-negotiated-protocol-version client) negotiated)
    (mcp-transport-set-protocol-version (mcp-client-transport client) negotiated)
    (setf (mcp-server-capabilities client) (%mcp-object-or-empty (jget result "capabilities"))
          (mcp-server-info client) (%mcp-object-or-empty (jget result "serverInfo"))
          (mcp-server-instructions client) (jget result "instructions"))
    (%mcp-negotiate-extensions client)
    (mcp-notify client "notifications/initialized")
    (mcp-refresh client)))

(defun %mcp-apply-era (client era)
  (setf (%client-era client) era)
  (mcp-transport-set-era (mcp-client-transport client) era)
  (if (string= era "modern")
      (progn (setf (mcp-negotiated-protocol-version client) (mcp-modern-protocol-version))
             (mcp-transport-set-protocol-version (mcp-client-transport client)
                                                 (mcp-modern-protocol-version)))
      (setf (mcp-negotiated-protocol-version client) :null)))

(defun %mcp-remember-era (client era)
  (let ((key (mcp-transport-era-cache-key (mcp-client-transport client))))
    (when (and (stringp key) (plusp (length key)))
      (setf (gethash key *mcp-era-cache*) era)
      (let ((store (%mcp-option client "eraStore")))
        (when (hash-table-p store) (%set-key store key era)))))
  nil)

(defun %mcp-request-discovery (client)
  (%mcp-request client "server/discover" (object)))

(defun %mcp-apply-discovery (client result)
  (let ((classified (axllm/core::mcp-classify-discovery-result result)))
    (unless (axllm/core::core-true-p (jget classified "valid"))
      (%mcp-fail "Invalid MCP server/discover result"))
    (setf (mcp-discover-result client) (%mcp-json-clone result)
          (mcp-server-capabilities client)
          (%mcp-json-clone (%mcp-object-or-empty (jget classified "capabilities")))
          (mcp-server-instructions client) (jget result "instructions"))
    (let ((info (jget classified "serverInfo")))
      (when (hash-table-p info) (setf (mcp-server-info client) info)))
    (%mcp-negotiate-extensions client)))

(defun %mcp-negotiate-extensions (client)
  (setf (mcp-negotiated-extensions client)
        (axllm/core::mcp-negotiate-extensions
         (%mcp-object-or-empty (jget (%mcp-client-capabilities client) "extensions"))
         (%mcp-object-or-empty (jget (mcp-server-capabilities client) "extensions")))))

(defun %mcp-client-capabilities (client)
  "What this client honestly claims it can answer.

Core derives the shapes; this only drops a capability whose host handler is
absent, because advertising one without a handler is a protocol lie."
  (let* ((capabilities (axllm/core::core-map-merge
                        (object) (%mcp-object-or-empty (%mcp-option client "capabilities"))))
         (elicitation (%mcp-handler-option client "elicitation"))
         (sampling (%mcp-handler-option client "sampling"))
         (derived (axllm/core::mcp-client-capabilities
                   (axllm/core::core-true-p (%mcp-option client "roots"))
                   (json-boolean sampling)
                   (json-boolean elicitation)
                   (let ((era (%client-era client))) (if (stringp era) era "legacy"))
                   (json-boolean (not (json-false-p (%mcp-option client "tasksExtension")))))))
    (dolist (key (%object-keys derived))
      (unless (%mcp-present-key-p capabilities key)
        (%set-key capabilities key (gethash key derived))))
    (unless sampling (remhash "sampling" capabilities))
    (unless elicitation (remhash "elicitation" capabilities))
    capabilities))

(defun mcp-discover (client)
  "A modern server's discovery result. Legacy endpoints have none."
  (mcp-init client)
  (unless (equal (%client-era client) "modern")
    (%mcp-fail "server/discover is only available for modern MCP"))
  (let ((result (%mcp-request-discovery client)))
    (%mcp-apply-discovery client result)
    (%mcp-json-clone result)))

(defun mcp-close (client)
  "Release CLIENT's protocol state and close its transport."
  (setf (%client-active-subscription client) :null)
  (mcp-transport-close-request-stream (mcp-client-transport client))
  (clrhash (%client-subscription-owners client))
  (setf (%client-catalog-cache client) (object))
  (clrhash (%client-read-cache client))
  (setf (%client-initialized client) nil)
  (mcp-transport-close (mcp-client-transport client))
  nil)

;;; --- catalogs -----------------------------------------------------

(defun %mcp-capability (client name)
  (%mcp-capability-present-p (jget (mcp-server-capabilities client) name)))

(defun %mcp-catalog-cache-fresh-p (client name)
  (axllm/core::core-true-p
   (axllm/core::mcp-cache-freshness (jget (%client-catalog-cache client) name) (%mcp-now-ms))))

(defun %mcp-now-ms ()
  (round (* 1000 (/ (get-internal-real-time) internal-time-units-per-second))))

(defun mcp-refresh (client &key (force t))
  "Reload the catalogs CLIENT's server advertises.

A tool whose x-mcp-header annotation Core rejects is excluded rather than
offered with a binding nobody can honour; the exclusion is logged when a
:LOGGER option is present. The catalog revision moves whenever a list is
reloaded, so native model steps rebuild their tool definitions."
  (let ((changed nil))
    (when (and (%mcp-capability client "tools")
               (or force (not (%mcp-catalog-cache-fresh-p client "tools"))))
      (let ((raw (%mcp-collect-catalog client "tools/list" "tools"))
            (kept (%new-array)))
        (loop for tool across raw
              do (handler-case
                     (progn (axllm/core::mcp-param-header-bindings (%mcp-tool-schema tool))
                            (vector-push-extend tool kept))
                   (error ()
                     (let ((logger (%mcp-handler-option client "logger")))
                       (when logger
                         (funcall logger
                                  (format nil "Warning: excluded MCP tool ~a: invalid x-mcp-header annotation"
                                          (%mcp-text (jget tool "name")))))))))
        (setf (mcp-client-tools client) kept changed t)))
    (when (and (%mcp-capability client "prompts")
               (or force (not (%mcp-catalog-cache-fresh-p client "prompts"))))
      (setf (mcp-client-prompts client) (%mcp-collect-catalog client "prompts/list" "prompts")
            changed t))
    (when (%mcp-capability client "resources")
      (when (or force (not (%mcp-catalog-cache-fresh-p client "resources")))
        (setf (mcp-client-resources client) (%mcp-collect-catalog client "resources/list" "resources")
              changed t))
      (when (or force (not (%mcp-catalog-cache-fresh-p client "resourceTemplates")))
        (setf (mcp-client-resource-templates client)
              (%mcp-collect-catalog client "resources/templates/list" "resourceTemplates")
              changed t)))
    (when changed (incf (mcp-catalog-revision client))))
  nil)

(defparameter +catalog-cache-names+
  '(("tools/list" . "tools") ("prompts/list" . "prompts")
    ("resources/list" . "resources") ("resources/templates/list" . "resourceTemplates")))

(defun %mcp-collect-catalog (client method field &key context)
  "Every page of one catalog, with bounded pagination.

A repeated cursor is a server fault and stops the walk; so does exceeding
:MAX-PAGINATION-PAGES. Neither silently truncates the catalog."
  (let ((values (%new-array))
        (pages (%new-array))
        (cursor :null)
        (seen '())
        (max-pages (let ((value (%mcp-option client "maxPaginationPages")))
                     (if (realp value) (round value) 1000))))
    (loop repeat max-pages
          do (let ((result (%mcp-request client method
                                     (if (eq cursor :null) (object) (object "cursor" cursor))
                                     :context context)))
        (vector-push-extend result pages)
        (loop for item across (%event-array (jget result field))
              do (vector-push-extend (%mcp-json-clone item) values))
        (setf cursor (jget result "nextCursor"))
        (when (or (eq cursor :null) (equal cursor ""))
          (let ((name (cdr (assoc method +catalog-cache-names+ :test #'string=))))
            (when name
              (%set-key (%client-catalog-cache client) name
                        (axllm/core::mcp-fold-cache-info pages (%mcp-now-ms)))))
          (return-from %mcp-collect-catalog values))
               (when (member (%mcp-text cursor) seen :test #'string=)
                 (%mcp-fail "MCP ~a repeated pagination cursor ~a" method (%mcp-text cursor)))
               (push (%mcp-text cursor) seen)))
    (%mcp-fail "MCP ~a exceeded ~a pagination pages" method max-pages)))

(defun mcp-inspect-catalog (client &key refresh)
  "A cloned snapshot of everything CLIENT's server owns.

An endpoint is only an address: the server names the tools, prompts,
resources, URI templates and subscriptions. Mutating the snapshot cannot
change the live client. URI templates are discoverable and never expanded
automatically."
  (mcp-init client)
  (when refresh (mcp-refresh client))
  (%mcp-json-clone
   (object "namespace" (mcp-namespace client)
           "protocolVersion" (mcp-negotiated-protocol-version client)
           "revision" (mcp-catalog-revision client)
           "serverInfo" (mcp-server-info client)
           "serverCapabilities" (mcp-server-capabilities client)
           "tools" (mcp-client-tools client)
           "prompts" (mcp-client-prompts client)
           "resources" (mcp-client-resources client)
           "resourceTemplates" (mcp-client-resource-templates client)
           "subscriptions" (coerce (%mcp-subscribed-uris client) 'vector))))

(defun %mcp-subscribed-uris (client)
  (%mcp-sorted-strings (loop for uri being the hash-keys of (%client-subscription-owners client)
                         collect uri)))

;;; --- raw protocol operations --------------------------------------

(defun mcp-ping (client) (%mcp-request client "ping" (object)))

(defun mcp-list-tools (client &optional cursor)
  (%mcp-request client "tools/list" (if cursor (object "cursor" cursor) (object))))

(defun mcp-list-prompts (client &optional cursor)
  (%mcp-request client "prompts/list" (if cursor (object "cursor" cursor) (object))))

(defun mcp-get-prompt (client name &optional arguments)
  (%mcp-request-with-input-rounds client "prompts/get"
                              (object "name" name "arguments" (%mcp-object-or-empty arguments))))

(defun mcp-list-resources (client &optional cursor)
  (%mcp-request client "resources/list" (if cursor (object "cursor" cursor) (object))))

(defun mcp-list-resource-templates (client &optional cursor)
  (%mcp-request client "resources/templates/list" (if cursor (object "cursor" cursor) (object))))

(defun mcp-read-resource (client uri)
  "Read URI, honouring server cache metadata when :READ-CACHE is set.

A resources/updated notification for URI drops its cached entry, so an
updated resource is re-read rather than served stale."
  (let ((cache-enabled (and (equal (%client-era client) "modern")
                            (axllm/core::core-true-p (%mcp-option client "readCache")))))
    (when cache-enabled
      (let ((cached (gethash uri (%client-read-cache client))))
        (if (and cached (axllm/core::core-true-p
                         (axllm/core::mcp-cache-freshness (jget cached "cache") (%mcp-now-ms))))
            (return-from mcp-read-resource (%mcp-json-clone (jget cached "result")))
            (remhash uri (%client-read-cache client)))))
    (let ((result (%mcp-request-with-input-rounds client "resources/read" (object "uri" uri))))
      (when cache-enabled
        (let ((cache (axllm/core::mcp-fold-cache-info (vector result) (%mcp-now-ms))))
          (when (axllm/core::core-true-p (axllm/core::mcp-cache-freshness cache (%mcp-now-ms)))
            (setf (gethash uri (%client-read-cache client))
                  (object "result" (%mcp-json-clone result) "cache" cache)))))
      result)))

(defun mcp-request (client method &optional params)
  "Send any negotiated MCP METHOD without converting it to a function."
  (%mcp-request client method (%mcp-object-or-empty params)))

(defun mcp-notify (client method &optional params)
  "Send a notification. Modern MCP has no session notifications, so the
three legacy-only ones are dropped rather than sent to a stateless server."
  (when (and (equal (%client-era client) "modern")
             (member method '("notifications/initialized" "notifications/roots/list_changed"
                              "notifications/cancelled")
                     :test #'string=))
    (return-from mcp-notify nil))
  (let ((message (object "jsonrpc" "2.0" "method" method)))
    (when params (%set-key message "params" params))
    (mcp-transport-send-notification (mcp-client-transport client) message))
  nil)

(defun mcp-cancel-request (client request-id &optional reason)
  "Tell the server to abandon REQUEST-ID."
  (let ((params (object "requestId" request-id)))
    (when (and reason (plusp (length (%mcp-text reason))))
      (%set-key params "reason" reason))
    (mcp-notify client "notifications/cancelled" params)))

;;; --- tools --------------------------------------------------------

(defun mcp-call-tool (client name arguments &key context (task-handling "await"))
  "Call tool NAME.

When a modern server answers with a task, TASK-HANDLING \"await\" (the
default) polls it to its terminal result so an Ax tool binding still returns
a tool result, and \"expose\" returns the task handle instead."
  (unless (member task-handling '("await" "expose") :test #'string=)
    (%mcp-fail "task-handling must be \"await\" or \"expose\", not ~s" task-handling))
  (let ((outcome (%mcp-tool-call-outcome client name arguments :context context)))
    (if (equal (%mcp-text (jget outcome "kind")) "complete")
        (jget outcome "result")
        (let ((task (jget outcome "task")))
          (if (string= task-handling "expose")
              task
              (%mcp-await-modern-task client (%mcp-text (jget task "taskId")) :context context))))))

(defun mcp-call-tool-outcome (client name arguments &key context)
  "Call tool NAME without waiting on a task.

Returns {kind: \"complete\", result} or {kind: \"task\", task}. A legacy
server's task-shaped result is a complete result, as Core classifies it."
  (%mcp-tool-call-outcome client name arguments :context context))

(defun %mcp-tool-call-outcome (client name arguments &key context)
  (%mcp-check-context context)
  (let ((args (%mcp-object-or-empty arguments)))
    (%mcp-authorize-tool-call client name args)
    (let* ((headers (%mcp-tool-call-headers client name args))
           (result
             (handler-case
                 (%mcp-request-with-input-rounds client "tools/call"
                                             (object "name" name "arguments" args)
                                             :extra-headers headers :context context)
               (mcp-error (condition)
                 ;; -32020 is a modern server saying our tool snapshot is
                 ;; stale: reload the catalog and retry once with fresh
                 ;; parameter-header bindings.
                 (unless (and (equal (%client-era client) "modern")
                              (eql (mcp-error-code condition) -32020))
                   (error condition))
                 (%mcp-reload-tools client :context context)
                 (%mcp-request-with-input-rounds client "tools/call"
                                             (object "name" name "arguments" args)
                                             :extra-headers (%mcp-tool-call-headers client name args)
                                             :context context))))
           (outcome (axllm/core::mcp-tool-call-outcome result (%mcp-has-tasks-capability-p client))))
      (when (equal (%mcp-text (jget outcome "kind")) "violation")
        (%mcp-fail "~a" (%mcp-text (jget outcome "message"))))
      (when (equal (%mcp-text (jget outcome "kind")) "task")
        (%mcp-record-task client (jget outcome "task")))
      outcome)))

(defun %mcp-authorize-tool-call (client name arguments)
  "Run the host authorization hook before the call reaches the wire."
  (let ((authorize (or (%mcp-handler-option client "authorizeToolCall")
                       (%mcp-handler-option client "authorize_tool_call"))))
    (when authorize
      (let ((context (axllm/core::mcp-tool-authorization-context
                      (mcp-client-tools client) (mcp-namespace client) name arguments)))
        (%set-key context "client" client)
        (axllm/core::mcp-tool-authorization-result name (funcall authorize context)))))
  nil)

(defun %mcp-tool-call-headers (client name arguments)
  (if (equal (%client-era client) "modern")
      (let ((tool (find name (mcp-client-tools client)
                        :key (lambda (item) (%mcp-text (jget item "name")))
                        :test #'string=)))
        (if tool
            (axllm/core::mcp-param-header-values
             (axllm/core::mcp-param-header-bindings (%mcp-tool-schema tool)) arguments)
            (object)))
      (object)))

(defun %mcp-reload-tools (client &key context)
  "Reload the tool catalog after a modern server rejected a stale snapshot."
  (let ((raw (%mcp-collect-catalog client "tools/list" "tools" :context context))
        (kept (%new-array)))
    (loop for item across raw
          do (handler-case
                 (progn (axllm/core::mcp-param-header-bindings (%mcp-tool-schema item))
                        (vector-push-extend item kept))
               (error () nil)))
    (setf (mcp-client-tools client) kept)))

(defun mcp-native-tools (client)
  "CLIENT's tools as native Ax tools.

Names, descriptions and schemas are the server's, unchanged. Each handler
calls the live client, so client identity, raw results and cancellation
survive into Ax execution."
  (let ((out '()))
    (loop for item across (mcp-client-tools client)
          do (let ((original (%mcp-text (jget item "name"))))
               (push (native-tool
                      :name (%mcp-override-name original (mcp-client-options client))
                      :description (%mcp-override-description item (mcp-client-options client))
                      :parameters (%mcp-tool-schema item)
                      :protocol (object "kind" "mcp" "namespace" (mcp-namespace client)
                                        "name" original)
                      :handler (lambda (arguments context)
                                 (mcp-call-tool client original arguments :context context)))
                     out)))
    (nreverse out)))

(defun mcp-to-function (client)
  "CLIENT's whole catalog flattened into tools.

This is the lossy compatibility adapter: tool results become text or
structured content, prompts and resources become pseudo-tools, and the
protocol session is no longer visible. Use MCP-NATIVE-TOOLS, or pass the
client itself, for native integration."
  (let ((out '()))
    (loop for item across (mcp-client-tools client)
          do (push (%mcp-tool-to-function client item) out))
    (loop for prompt across (mcp-client-prompts client)
          do (push (%mcp-prompt-to-function client prompt) out))
    (loop for resource across (mcp-client-resources client)
          do (push (%mcp-resource-to-function client resource) out))
    (loop for template across (mcp-client-resource-templates client)
          do (push (%mcp-resource-template-to-function client template) out))
    (nreverse out)))

(defun %mcp-tool-to-function (client item)
  (let ((original (%mcp-text (jget item "name"))))
    (native-tool
     :name (%mcp-override-name original (mcp-client-options client))
     :description (%mcp-override-description item (mcp-client-options client))
     :parameters (%mcp-tool-schema item)
     :handler (lambda (arguments context)
                (let ((result (mcp-call-tool client original arguments :context context)))
                  (if (%mcp-present-key-p result "structuredContent")
                      (jget result "structuredContent")
                      (%mcp-content-to-value (jget result "content"))))))))

(defun %mcp-prompt-to-function (client item)
  (let ((original (%mcp-text (jget item "name")))
        (properties (object)))
    (loop for argument across (%event-array (jget item "arguments"))
          do (%set-key properties (%mcp-text (jget argument "name" "arg"))
                       (object "type" "string"
                               "description" (%mcp-text (jget argument "description" "")))))
    (native-tool
     :name (%mcp-override-name (concatenate 'string "prompt_" original) (mcp-client-options client))
     :description (%mcp-override-description item (mcp-client-options client))
     :parameters (object "type" "object" "properties" properties)
     :handler (lambda (arguments context)
                (declare (ignore context))
                (mcp-get-prompt client original arguments)))))

(defun %mcp-resource-to-function (client item)
  (let ((uri (%mcp-text (jget item "uri"))))
    (native-tool
     :name (%mcp-override-name
            (concatenate 'string "resource_"
                         (%mcp-safe-tool-name (let ((name (jget item "name")))
                                            (if (stringp name) name uri))))
            (mcp-client-options client))
     :description (%mcp-override-description item (mcp-client-options client))
     :parameters (object "type" "object" "properties" (object))
     :handler (lambda (arguments context)
                (declare (ignore arguments context))
                (mcp-read-resource client uri)))))

(defun %mcp-resource-template-to-function (client item)
  (native-tool
   :name (%mcp-override-name
          (concatenate 'string "resource_template_"
                       (%mcp-safe-tool-name (jget item "name" "template")))
          (mcp-client-options client))
   :description (%mcp-override-description item (mcp-client-options client))
   :parameters (object "type" "object" "properties" (object "uri" (object "type" "string")))
   :handler (lambda (arguments context)
              (declare (ignore context))
              (mcp-read-resource client (%mcp-text (jget arguments "uri"))))))

;;; --- tasks --------------------------------------------------------

(defun %mcp-has-tasks-capability-p (client)
  (if (equal (%client-era client) "modern")
      (%mcp-present-key-p (mcp-negotiated-extensions client) "io.modelcontextprotocol/tasks")
      (%mcp-capability client "tasks")))

(defun %mcp-record-task (client task)
  "Remember TASK's latest snapshot, and ask a running listener for a new
task's updates."
  (let ((task-id (and (hash-table-p task) (jget task "taskId"))))
    (when (and (stringp task-id) (plusp (length task-id)))
      (let ((new (not (nth-value 1 (gethash task-id (%client-tasks client))))))
        (setf (gethash task-id (%client-tasks client)) (%mcp-json-clone task))
        (when (and new (equal (%client-era client) "modern"))
          (%mcp-restart-modern-listener client)))))
  nil)

(defun %mcp-listen-task-ids (client)
  (let ((listening (or (%client-notification-listeners client)
                       (%mcp-handler-option client "onNotification"))))
    (if (and listening (%mcp-has-tasks-capability-p client))
        (coerce (%mcp-sorted-strings (loop for id being the hash-keys of (%client-tasks client)
                                       collect id))
                'vector)
        (%new-array))))

(defun mcp-get-task (client task-id &key context)
  (unless (%mcp-has-tasks-capability-p client) (%mcp-fail "Tasks are not supported"))
  (let ((result (%mcp-request client "tasks/get" (object "taskId" task-id) :context context)))
    (when (and (equal (%client-era client) "modern")
               (not (axllm/core::core-true-p (axllm/core::mcp-validate-modern-task result))))
      (%mcp-fail "MCP protocol violation: invalid tasks/get result"))
    (%mcp-record-task client result)
    result))

(defun mcp-cancel-task (client task-id)
  (unless (%mcp-has-tasks-capability-p client) (%mcp-fail "Tasks are not supported"))
  (let ((result (%mcp-request client "tasks/cancel" (object "taskId" task-id))))
    (if (equal (%client-era client) "modern") (object) result)))

(defun mcp-list-tasks (client &optional cursor)
  "Legacy task-draft listing. Modern results live in tasks/get."
  (when (equal (%client-era client) "modern")
    (%mcp-fail "tasks/list is only available for legacy MCP tasks"))
  (unless (%mcp-has-tasks-capability-p client) (%mcp-fail "Tasks are not supported"))
  (%mcp-request client "tasks/list" (if cursor (object "cursor" cursor) (object))))

(defun mcp-get-task-result (client task-id)
  "Legacy task-draft result fetch."
  (when (equal (%client-era client) "modern")
    (%mcp-fail "tasks/result is only available for legacy MCP tasks; modern results are embedded in tasks/get"))
  (unless (%mcp-has-tasks-capability-p client) (%mcp-fail "Tasks are not supported"))
  (%mcp-request client "tasks/result" (object "taskId" task-id)))

(defun mcp-provide-task-input (client task-id input-responses &key context)
  "Answer a modern task's input_required state."
  (unless (and (equal (%client-era client) "modern") (%mcp-has-tasks-capability-p client))
    (%mcp-fail "tasks/update is only available for modern MCP Tasks v2"))
  (%mcp-request client "tasks/update"
            (object "taskId" task-id "inputResponses" (%mcp-object-or-empty input-responses))
            :context context)
  nil)

(defun %mcp-await-modern-task (client task-id &key context)
  "Poll TASK-ID to a terminal outcome, answering input_required on the way."
  (let ((max-polls (let ((value (%mcp-option client "maxTaskPolls")))
                     (if (realp value) (round value) 1000))))
    (loop repeat max-polls
          do (let* ((task (mcp-get-task client task-id :context context))
             (outcome (axllm/core::mcp-task-terminal-outcome task))
             (kind (%mcp-text (jget outcome "kind"))))
        (cond ((string= kind "result") (return-from %mcp-await-modern-task
                                         (%mcp-json-clone (jget outcome "result"))))
              ((string= kind "protocol_error")
               (error 'mcp-error
                      :message (%mcp-text (jget outcome "message" "MCP task failed"))
                      :code (let ((code (jget outcome "code"))) (if (realp code) (round code) :null))
                      :data (jget outcome "data")))
              ((member kind '("violation" "failure" "cancelled") :test #'string=)
               (%mcp-fail "~a" (%mcp-text (jget outcome "message" "MCP task failed"))))
                   ((string= kind "input_required")
                    (mcp-provide-task-input client task-id
                                            (%mcp-fulfill-input-requests
                                             client (jget outcome "inputRequests"))
                                            :context context)))))
    (%mcp-fail "MCP task ~a exceeded ~a polls" task-id max-polls)))

(defun %mcp-fulfill-input-requests (client requests)
  "Answer one round of server input requests through the host's handlers.

Core decides which requests this client may answer; the handlers below are
the only place a host policy actually runs."
  (let* ((elicitation (%mcp-handler-option client "elicitation"))
         (sampling (%mcp-handler-option client "sampling"))
         (fulfillment (axllm/core::mcp-mrtr-plan-fulfillment
                       requests
                       (%mcp-option client "roots")
                       (json-boolean elicitation)
                       (json-boolean sampling))))
    (unless (axllm/core::core-true-p (jget fulfillment "ok"))
      (%mcp-fail "~a" (%mcp-text (jget fulfillment "message" "MCP protocol violation"))))
    (let ((responses (axllm/core::core-map-merge
                      (object) (%mcp-object-or-empty (jget fulfillment "responses"))))
          (pending (%mcp-object-or-empty (jget fulfillment "pending")))
          (context (object "client" client "namespace" (mcp-namespace client))))
      (dolist (key (%object-keys pending))
        (let* ((request (gethash key pending))
               (method (%mcp-text (jget request "method")))
               (handler (cond ((string= method "elicitation/create") elicitation)
                              ((string= method "sampling/createMessage") sampling))))
          (unless handler
            (%mcp-fail "MCP protocol violation: unsupported pending input request method ~a"
                       method))
          (%set-key responses key
                    (funcall handler (%mcp-object-or-empty (jget request "params")) context))))
      responses)))

;;; --- subscriptions and listening ----------------------------------

(defun %mcp-assert-resource-subscriptions (client)
  (let ((resources (jget (mcp-server-capabilities client) "resources")))
    (unless (and (hash-table-p resources)
                 (axllm/core::core-true-p (jget resources "subscribe")))
      (%mcp-fail "Resource subscriptions are not supported"))))

(defun mcp-subscribe-resource (client uri)
  "Subscribe to URI as the manual owner."
  (mcp-acquire-resource-subscription client uri "manual"))

(defun mcp-unsubscribe-resource (client uri)
  (mcp-release-resource-subscription client uri "manual"))

(defun mcp-acquire-resource-subscription (client uri owner)
  "Take a logical share of URI's subscription for OWNER.

Only the first owner sends resources/subscribe, so one source closing can
never break another's subscription. Core owns the ownership transition."
  (%mcp-assert-resource-subscriptions client)
  (let* ((current (%mcp-sorted-strings (gethash uri (%client-subscription-owners client) '())))
         (transition (axllm/core::mcp-resource-subscription-ownership
                      (coerce current 'vector) owner "acquire")))
    (setf (gethash uri (%client-subscription-owners client))
          (coerce (%event-array (jget transition "owners")) 'list))
    (if (equal (%client-era client) "modern")
        (progn
          (when (and (axllm/core::core-true-p (jget transition "changed"))
                     (not (eq (%client-active-subscription client) :null)))
            (%mcp-restart-modern-listener client))
          (object))
        (if (equal (%mcp-text (jget transition "wireAction")) "subscribe")
            (%mcp-request client "resources/subscribe" (object "uri" uri))
            (object)))))

(defun mcp-release-resource-subscription (client uri owner)
  "Give up OWNER's share of URI. Only the last release unsubscribes."
  (%mcp-assert-resource-subscriptions client)
  (let* ((current (%mcp-sorted-strings (gethash uri (%client-subscription-owners client) '())))
         (transition (axllm/core::mcp-resource-subscription-ownership
                      (coerce current 'vector) owner "release"))
         (owners (coerce (%event-array (jget transition "owners")) 'list)))
    (if owners
        (setf (gethash uri (%client-subscription-owners client)) owners)
        (remhash uri (%client-subscription-owners client)))
    (if (equal (%client-era client) "modern")
        (progn
          (when (and (axllm/core::core-true-p (jget transition "changed"))
                     (not (eq (%client-active-subscription client) :null)))
            (%mcp-restart-modern-listener client))
          (object))
        (if (equal (%mcp-text (jget transition "wireAction")) "unsubscribe")
            (%mcp-request client "resources/unsubscribe" (object "uri" uri))
            (object)))))

(defun mcp-restore-resource-subscriptions (client)
  "Re-establish the current logical subscriptions after a reconnect."
  (if (equal (%client-era client) "modern")
      (unless (eq (%client-active-subscription client) :null)
        (%mcp-restart-modern-listener client))
      (dolist (uri (%mcp-subscribed-uris client))
        (%mcp-request client "resources/subscribe" (object "uri" uri))))
  nil)

(defun mcp-start-listening (client)
  "Begin receiving server-to-client messages.

Legacy clients resume a GET/SSE stream. Modern clients place their catalog
interests, concrete resource URIs and known task ids in a long-running
subscriptions/listen POST; an interest change restarts that stream with a
fresh request id. Nonblocking in both eras."
  (mcp-init client)
  (unless (equal (%client-era client) "modern")
    (mcp-transport-start-listening (mcp-client-transport client))
    (return-from mcp-start-listening nil))
  (mcp-transport-close-request-stream (mcp-client-transport client))
  (let ((subscription-id (%mcp-uuid))
        (ready (make-cancellation-token)))
    (setf (%client-active-subscription client) subscription-id
          (%client-subscription-ready client) ready)
    (let ((params (object "notifications"
                          (axllm/core::mcp-listen-interests
                           (coerce (%mcp-subscribed-uris client) 'vector)
                           (%mcp-object-or-empty (%mcp-option client "subscriptionFilters"))
                           (%mcp-listen-task-ids client)))))
      (%set-key params "_meta" (%mcp-request-meta client (object)))
      (mcp-transport-open-request-stream
       (mcp-client-transport client)
       (object "jsonrpc" "2.0" "id" subscription-id
               "method" "subscriptions/listen" "params" params)))
    (let ((timeout (let ((value (%mcp-option client "listenAckTimeout")))
                     (if (realp value) value 2))))
      (unless (cancellation-token-wait ready timeout)
        (%mcp-fail "subscriptions/listen acknowledgement timed out"))))
  nil)

(defun %mcp-restart-modern-listener (client)
  "Restart the modern listen stream for a changed interest set.

A restart from inside the listening thread would deadlock on that thread's
own stream, so it is handed to a fresh thread."
  (when (and (equal (%client-era client) "modern")
             (not (eq (%client-active-subscription client) :null)))
    (flet ((restart ()
             (sb-thread:with-mutex ((%client-restart-lock client))
               (when (and (equal (%client-era client) "modern")
                          (not (eq (%client-active-subscription client) :null)))
                 (mcp-start-listening client)))))
      (if (eq sb-thread:*current-thread* (%mcp-transport-listen-thread (mcp-client-transport client)))
          (sb-thread:make-thread #'restart :name "ax-mcp-listen-restart")
          (restart))))
  nil)

(defgeneric %mcp-transport-listen-thread (transport)
  (:documentation "The thread a transport reads its stream on, or NIL.")
  (:method ((transport mcp-transport)) nil))

;;; --- listeners and inbound dispatch -------------------------------

(defun mcp-add-notification-listener (client listener)
  "Register LISTENER for every delivered notification; returns a remover.

A listener enqueues or observes. It must not invoke a model: remote
notification content is untrusted."
  (push listener (%client-notification-listeners client))
  (lambda ()
    (setf (%client-notification-listeners client)
          (remove listener (%client-notification-listeners client) :count 1 :test #'eq))
    nil))

(defun mcp-add-lifecycle-listener (client listener)
  "Register LISTENER for connected/disconnected/reconnected; returns a remover."
  (push listener (%client-lifecycle-listeners client))
  (lambda ()
    (setf (%client-lifecycle-listeners client)
          (remove listener (%client-lifecycle-listeners client) :count 1 :test #'eq))
    nil))

(defun mcp-emit-lifecycle (client state)
  "Record a transport lifecycle STATE and tell every listener."
  (cond ((and (equal (%client-era client) "modern") (equal state "disconnected")
              (not (eq (%client-active-subscription client) :null)))
         (%mcp-restart-modern-listener client))
        ((equal state "reconnected")
         (mcp-restore-resource-subscriptions client)))
  (dolist (listener (reverse (%client-lifecycle-listeners client)))
    (funcall listener state))
  nil)

(defun %mcp-handle-inbound (client message)
  "Deliver one inbound notification, after Core's subscription filter."
  (let ((delivered message))
    (when (equal (%client-era client) "modern")
      (let ((filtered (axllm/core::mcp-notification-subscription-filter
                       message (%client-active-subscription client))))
        (unless (axllm/core::core-true-p (jget filtered "deliver"))
          (return-from %mcp-handle-inbound nil))
        (setf delivered (%mcp-object-or-empty (jget filtered "message")))
        (when (axllm/core::core-true-p (jget filtered "acknowledged"))
          (cancellation-token-cancel (%client-subscription-ready client) "acknowledged"))))
    (let ((method (%mcp-text (jget delivered "method"))))
      (when (member method '("notifications/tools/list_changed"
                             "notifications/prompts/list_changed"
                             "notifications/resources/list_changed")
                    :test #'string=)
        (mcp-refresh client))
      (when (string= method "notifications/resources/updated")
        (let ((uri (jget (%mcp-object-or-empty (jget delivered "params")) "uri")))
          (when (stringp uri) (remhash uri (%client-read-cache client)))))
      (when (member method '("notifications/tasks" "notifications/tasks/status")
                    :test #'string=)
        (let* ((params (%mcp-object-or-empty (jget delivered "params")))
               (task (jget params "task")))
          (%mcp-record-task client (if (hash-table-p task) task params)))))
    (let ((callback (%mcp-handler-option client "onNotification")))
      (when callback (funcall callback delivered)))
    (dolist (listener (reverse (%client-notification-listeners client)))
      (funcall listener delivered)))
  nil)

(defun %mcp-handle-server-request (client message)
  "Answer one server-initiated request, as Core plans it.

Core decides whether to answer directly (ping, roots, or an unsupported
method) or to hand the request to a host handler. This function only runs
the handler Core named and shapes the reply; a handler that signals becomes
an internal-error response rather than taking the connection down."
  (let* ((elicitation (%mcp-handler-option client "elicitation"))
         (sampling (%mcp-handler-option client "sampling"))
         (plan (axllm/core::mcp-server-request-plan-full
                message (%mcp-option client "roots")
                (json-boolean elicitation) (json-boolean sampling)))
         (action (%mcp-text (jget plan "action")))
         (handler (cond ((string= action "elicitation") elicitation)
                        ((string= action "sampling") sampling))))
    (if (null handler)
        (%mcp-object-or-empty (jget plan "response"))
        (handler-case
            (object "jsonrpc" "2.0" "id" (jget plan "id")
                    "result" (funcall handler
                                      (%mcp-object-or-empty (jget plan "params"))
                                      (object "client" client
                                              "namespace" (mcp-namespace client))))
          (error (condition)
            (object "jsonrpc" "2.0" "id" (jget plan "id")
                    "error" (object "code" -32603 "message" (princ-to-string condition))))))))

;;; --- requests -----------------------------------------------------

(defun %mcp-request-meta (client existing)
  (axllm/core::mcp-build-request-meta
   existing
   (let ((version (mcp-negotiated-protocol-version client)))
     (if (stringp version) version (mcp-modern-protocol-version)))
   (%mcp-client-capabilities client)
   (axllm/core::core-map-merge +mcp-client-info+
                               (%mcp-object-or-empty (%mcp-option client "clientInfo")))
   (%mcp-option client "logLevel")
   :null :null))

(defun %mcp-request-with-input-rounds (client method base-params &key extra-headers context)
  "Send METHOD, fulfilling any multi-round-trip input requests.

Core bounds the loop and shapes each round's parameters, including the
byte-exact requestState a modern server expects back."
  (let ((params (%mcp-json-clone base-params))
        (max-rounds (%mcp-option client "maxInputRounds"))
        (round 0))
    (loop
      (let* ((result (%mcp-request client method params :extra-headers extra-headers
                                                    :context context))
             (plan (axllm/core::mcp-mrtr-plan-round
                    result (let ((era (%client-era client))) (if (stringp era) era "legacy"))
                    method round max-rounds))
             (action (%mcp-text (jget plan "action"))))
        (when (string= action "complete") (return result))
        (when (string= action "violation")
          (%mcp-fail "~a" (%mcp-text (jget plan "message" "MCP protocol violation"))))
        (let ((input-responses :null)
              (requests (jget plan "inputRequests")))
          (when (and (axllm/core::core-true-p (jget plan "hasInputRequests"))
                     (hash-table-p requests))
            (setf input-responses (%mcp-fulfill-input-requests client requests)))
          (setf params (axllm/core::mcp-mrtr-next-params
                        base-params input-responses
                        (if (axllm/core::core-true-p (jget plan "hasRequestState"))
                            (jget plan "requestState")
                            :null))
                round (1+ round)))))))

(defun %mcp-next-request-id (client)
  (sb-thread:with-mutex ((%client-id-lock client))
    (prog1 (format nil "~a" (%client-next-id client))
      (incf (%client-next-id client)))))

(defun %mcp-request (client method params &key extra-headers context
                                           (allow-version-retry t))
  "Send one JSON-RPC request and return its result object.

Modern requests carry Core's request metadata. A -32022 version error is
renegotiated once against the versions Core selects; any other error becomes
an MCP-ERROR carrying the wire code and data."
  (%mcp-check-context context)
  (let* ((request-id (%mcp-next-request-id client))
         (message (object "jsonrpc" "2.0" "id" request-id "method" method))
         (request-params (axllm/core::core-map-merge (object) (%mcp-object-or-empty params))))
    (when (equal (%client-era client) "modern")
      (%set-key request-params "_meta"
                (%mcp-request-meta client (%mcp-object-or-empty (jget request-params "_meta")))))
    (when params (%set-key message "params" request-params))
    (let ((response (mcp-transport-send-with-context
                     (mcp-client-transport client) message
                     (%mcp-object-or-empty extra-headers) context)))
      (%mcp-check-context context)
      (when (%mcp-present-key-p response "error")
        (let* ((normalized (axllm/core::mcp-normalize-error response))
               (code (jget normalized "code"))
               (data (jget normalized "data")))
          (when (and (equal (%client-era client) "modern") allow-version-retry
                     (eql (and (realp code) (round code)) -32022))
            (let ((version (%mcp-text (axllm/core::mcp-select-mutual-version
                                       (%mcp-object-or-empty data)
                                       (let ((value (%mcp-option client "supportedProtocolVersions")))
                                         (if (%array-p value)
                                             value
                                             (mcp-supported-protocol-versions)))))))
              (when (plusp (length version))
                (setf (mcp-negotiated-protocol-version client) version)
                (mcp-transport-set-protocol-version (mcp-client-transport client) version)
                (return-from %mcp-request
                  (%mcp-request client method params :extra-headers extra-headers
                                                 :context context
                                                 :allow-version-retry nil)))))
          (error 'mcp-error
                 :message (%mcp-text (jget normalized "message" "MCP JSON-RPC error"))
                 :code (if (realp code) (round code) :null)
                 :data data)))
      (let ((result (%mcp-object-or-empty (jget response "result"))))
        (when (equal (%client-era client) "modern")
          (%mcp-capture-server-info client result))
        result))))

(defun %mcp-capture-server-info (client result)
  "Keep a modern server's identity when it names itself in a result."
  (let* ((classified (axllm/core::mcp-classify-discovery-result result))
         (info (jget classified "serverInfo")))
    (unless (hash-table-p info)
      (let ((candidate (jget (%mcp-object-or-empty (jget result "_meta"))
                             "io.modelcontextprotocol/serverInfo")))
        (when (and (hash-table-p candidate)
                   (stringp (jget candidate "name"))
                   (stringp (jget candidate "version")))
          (setf info candidate))))
    (when (hash-table-p info) (setf (mcp-server-info client) info)))
  nil)

;;; ------------------------------------------------------------------
;;; MCP event source
;;; ------------------------------------------------------------------

(defclass mcp-event-source ()
  ((client :initarg :client :reader mcp-event-source-client)
   (namespace :initarg :namespace :reader mcp-event-source-namespace)
   (identity-scope :initarg :identity-scope :reader mcp-event-source-identity-scope)
   (trust :initarg :trust :reader mcp-event-source-trust)
   (policy :initarg :policy :reader mcp-event-source-policy)
   (subscriptions :initform '() :accessor mcp-event-source-subscriptions)
   (owner :initform nil :reader mcp-event-source-owner)
   (errors :initform '() :accessor mcp-event-source-errors)
   (publish :initform nil :accessor %source-publish)
   (remove-notification :initform nil :accessor %source-remove-notification)
   (remove-lifecycle :initform nil :accessor %source-remove-lifecycle))
  (:documentation
   "An MCP notification adapter for the event runtime.

The source only publishes into the inbox; explicit routes decide whether to
observe, invalidate, wake or resume. Identity must come from the host's
authentication state, because a bare MCP session is anonymous."))

(defun make-mcp-event-source (client &key namespace (identity-scope "anonymous")
                                          (trust "untrusted")
                                          (resource-subscriptions :none)
                                          subscriptions)
  "An event source over CLIENT.

RESOURCE-SUBSCRIPTIONS is :NONE (the default, subscribing to nothing), :ALL,
or a selector function called with (resource catalog). SUBSCRIPTIONS is an
explicit list of application-constructed concrete URIs. URI templates are
never expanded automatically."
  (when (and subscriptions (not (eq resource-subscriptions :none)))
    (%mcp-fail "Specify either :resource-subscriptions or :subscriptions, not both"))
  (let ((source (make-instance 'mcp-event-source
                               :client client
                               :namespace (or namespace (mcp-namespace client))
                               :identity-scope identity-scope :trust trust
                               :policy (or subscriptions resource-subscriptions))))
    (setf (slot-value source 'owner) (format nil "event-source:~a" (%mcp-uuid)))
    source))

(defmethod event-source-id ((source mcp-event-source))
  (mcp-event-source-namespace source))

(defmethod event-source-start ((source mcp-event-source) publish)
  (let ((client (mcp-event-source-client source)))
    (mcp-init client)
    (setf (%source-publish source) publish
          (%source-remove-notification source)
          (mcp-add-notification-listener
           client (lambda (message) (%mcp-source-on-notification source message)))
          (%source-remove-lifecycle source)
          (mcp-add-lifecycle-listener
           client (lambda (state)
                    (when (equal state "reconnected") (%mcp-source-reconcile source)))))
    (when (equal (mcp-get-era client) "modern")
      (mcp-start-listening client))
    (%mcp-source-reconcile source))
  source)

(defun %mcp-source-on-notification (source message)
  (let ((publish (%source-publish source)))
    (when (and publish (%mcp-present-key-p message "method"))
      (when (equal (jget message "method") "notifications/resources/list_changed")
        (%mcp-source-reconcile source))
      (let* ((normalized (event-normalize-mcp (mcp-event-source-namespace source)
                                              (%mcp-text (jget message "method"))
                                              (%mcp-object-or-empty (jget message "params"))))
             (raw (jget normalized "correlation"))
             (correlation (cond ((hash-table-p raw) (vector raw))
                                ((%array-p raw) raw)
                                (t (%new-array))))
             (data (jget normalized "data"))
             (subject (if (hash-table-p data)
                          (let ((uri (jget data "uri")))
                            (if (stringp uri)
                                uri
                                (jget (%mcp-object-or-empty (jget data "task")) "taskId")))
                          :null)))
        (funcall publish
                 (make-event-envelope
                  (format nil "mcp:~a:~a" (mcp-event-source-namespace source) (%mcp-uuid))
                  (%mcp-text (jget normalized "source"))
                  (%mcp-text (jget normalized "type"))
                  :data data :subject subject :correlation correlation)
                 :identity-scope (mcp-event-source-identity-scope source)
                 :trust (mcp-event-source-trust source)))))
  nil)

(defun mcp-event-source-reconnect (source)
  "Restore SOURCE's logical subscriptions and re-diff its selection."
  (mcp-restore-resource-subscriptions (mcp-event-source-client source))
  (%mcp-source-reconcile source))

(defun %mcp-source-selected-uris (source catalog)
  "The concrete URIs SOURCE's policy selects, as Core selects them."
  (let* ((policy (mcp-event-source-policy source))
         (resources (%event-array (jget catalog "resources")))
         (mode "none")
         (explicit (%new-array))
         (candidates (%new-array)))
    (cond ((or (eq policy :none) (null policy)))
          ((eq policy :all) (setf mode "all" candidates resources))
          ((functionp policy)
           (setf mode "selector")
           (loop for resource across resources
                 when (funcall policy resource catalog)
                   do (vector-push-extend resource candidates)))
          ((listp policy)
           (setf mode "explicit" explicit (%mcp-string-vector policy)))
          (t (%mcp-fail "Invalid MCP resource subscription policy")))
    (%mcp-sorted-strings (axllm/core::mcp-resource-subscription-selection
                      candidates mode explicit))))

(defun %mcp-source-reconcile (source)
  "Diff the desired subscription set against the current one.

A selector that fails keeps the prior selection and records the error, and a
partial wire failure keeps whatever transitions did succeed, so the next
change or reconnect retries only the incomplete work."
  (let* ((client (mcp-event-source-client source))
         (catalog (mcp-inspect-catalog client))
         (policy (mcp-event-source-policy source)))
    (unless (or (eq policy :none) (null policy))
      (let ((capability (jget (%mcp-object-or-empty (jget catalog "serverCapabilities")) "resources")))
        (unless (and (hash-table-p capability)
                     (axllm/core::core-true-p (jget capability "subscribe")))
          (%mcp-fail "MCP server ~a does not advertise resource subscriptions"
                     (%mcp-text (jget catalog "namespace"))))))
    (let ((desired (handler-case (%mcp-source-selected-uris source catalog)
                     (error (condition)
                       (push condition (mcp-event-source-errors source))
                       (return-from %mcp-source-reconcile nil)))))
      (let ((plan (axllm/core::mcp-resource-subscription-plan
                   (coerce desired 'vector)
                   (coerce (mcp-event-source-subscriptions source) 'vector))))
        (loop for uri across (%event-array (jget plan "removals"))
              do (handler-case
                     (progn (mcp-release-resource-subscription client (%mcp-text uri)
                                                               (mcp-event-source-owner source))
                            (setf (mcp-event-source-subscriptions source)
                                  (remove (%mcp-text uri)
                                          (mcp-event-source-subscriptions source)
                                          :test #'string=)))
                   (error (condition) (push condition (mcp-event-source-errors source)))))
        (loop for uri across (%event-array (jget plan "additions"))
              do (handler-case
                     (progn (mcp-acquire-resource-subscription client (%mcp-text uri)
                                                               (mcp-event-source-owner source))
                            (setf (mcp-event-source-subscriptions source)
                                  (sort (cons (%mcp-text uri)
                                              (mcp-event-source-subscriptions source))
                                        #'string<)))
                   (error (condition) (push condition (mcp-event-source-errors source))))))))
  nil)

(defmethod event-source-close ((source mcp-event-source))
  (dolist (uri (mcp-event-source-subscriptions source))
    (ignore-errors (mcp-release-resource-subscription (mcp-event-source-client source) uri
                                                      (mcp-event-source-owner source))))
  (setf (mcp-event-source-subscriptions source) '())
  (when (%source-remove-notification source) (funcall (%source-remove-notification source)))
  (when (%source-remove-lifecycle source) (funcall (%source-remove-lifecycle source)))
  (setf (%source-remove-notification source) nil
        (%source-remove-lifecycle source) nil
        (%source-publish source) nil)
  nil)

;;; ------------------------------------------------------------------
;;; UCP
;;; ------------------------------------------------------------------

(defparameter +ucp-operations+
  '("catalog.search" "catalog.lookup" "catalog.product"
    "cart.create" "cart.get" "cart.update" "cart.cancel"
    "checkout.create" "checkout.get" "checkout.update" "checkout.complete"
    "checkout.cancel" "fulfillment.quote" "discounts.apply" "payments.create"
    "payments.confirm" "orders.get" "identity.link" "attribution.record"
    "handoff.create")
  "The UCP operations a binding may be asked for. An operation outside this
list is rejected before any host call.")

(defgeneric ucp-binding-call (binding operation payload options)
  (:documentation
   "Perform one UCP OPERATION. The binding owns the REST or MCP transport;
OPTIONS carries the negotiated version and the idempotency key."))

(defclass function-ucp-binding ()
  ((call :initarg :call :reader %ucp-binding-function)))

(defmethod ucp-binding-call ((binding function-ucp-binding) operation payload options)
  (funcall (%ucp-binding-function binding) operation payload options))

(defmethod ucp-binding-call ((binding function) operation payload options)
  (funcall binding operation payload options))

;; UCP-SCHEMA loads after this file because its transport uses our UTF-8
;; boundary. Declare the two forward calls for warning-free serial builds.
(declaim (ftype function make-ucp-schema-validator ucp-schema-validation-callback))

(defclass ucp-client ()
  ((profile :initarg :profile :reader ucp-client-profile)
   (binding :initarg :binding :reader ucp-client-binding)
   (options :initarg :options :reader ucp-client-options)
   (version :initarg :version :reader ucp-client-version)
   (services :initarg :services :reader ucp-client-services)
   (schema-validator :initarg :schema-validator :reader %ucp-client-schema-validator)
   (capabilities :initarg :capabilities :reader ucp-client-capabilities))
  (:documentation "A UCP commerce client over a host-owned binding."))

(defun make-ucp-client (profile binding &key namespace version supported-versions
                                             requested-services (schema-validation t)
                                             fetch ssrf-protection mcp-options)
  "A UCP client for PROFILE.

Core describes the profile; the version allowlist and the required-service
list are host policy, so an unsupported version or a missing service is
refused here, before any operation reaches the binding.

SCHEMA-VALIDATION defaults to enabled. NIL or JSON false disables it; an
existing validator or a (VALUE SCHEMA-URL) callback can be supplied instead.
A callback signals on failure. An object accepts fetch and ssrfProtection;
these override FETCH/SSRF-PROTECTION and MCP-OPTIONS transport defaults.
Only schemas declared by the operation's capability (or its extensions) are
validated, against the raw response, before Core wraps the outcome."
  (let* ((profile (%mcp-object-or-empty profile))
         (supported (%mcp-string-vector (or supported-versions '("2026-04-08"))))
         (wanted (%mcp-string-vector (or requested-services '())))
         (descriptor (axllm/core::ucp-negotiate-profile profile supported wanted))
         (negotiated (let ((value (jget descriptor "version")))
                       (if (stringp value)
                           value
                           (let ((configured (or version "2026-04-08")))
                             (if (stringp configured) configured "2026-04-08")))))
         (services (%mcp-object-or-empty (jget descriptor "services"))))
    (unless (find negotiated supported :test #'axllm/core::core-value-equal)
      (%mcp-fail "Unsupported UCP version ~a" negotiated))
    (loop for service across wanted
          do (unless (%mcp-present-key-p services service)
               (%mcp-fail "UCP profile ~a does not offer the ~a service"
                          (%mcp-text (jget profile "name" "ucp")) service)))
    (make-instance 'ucp-client
                   :profile profile
                   :binding binding
                   :options (object "namespace" (or namespace :null))
                   :schema-validator
                   (%ucp-make-validator schema-validation fetch ssrf-protection mcp-options)
                   :version negotiated
                   :services services
                   :capabilities (%mcp-object-or-empty (jget descriptor "capabilities")))))

(defun %ucp-make-validator (validation fetch ssrf-protection mcp-options)
  (cond ((or (null validation) (eq validation false)) nil)
        ((functionp validation) validation)
        ((typep validation (find-class 'ucp-schema-validator))
         (ucp-schema-validation-callback validation))
        (t
         (let* ((config (%mcp-object-or-empty validation))
                (mcp (%mcp-object-or-empty mcp-options))
                (mtls (%mcp-object-or-empty (jget mcp "mtls"))))
           (ucp-schema-validation-callback
            (make-ucp-schema-validator
             :fetch (or (%present (jget config "fetch")) fetch
                        (%present (jget mtls "fetch")) (%present (jget mcp "fetch")))
             :ssrf-protection (or (%present (jget config "ssrfProtection")) ssrf-protection
                                  (%present (jget mcp "ssrfProtection")))))))))

(defun %ucp-operation-capability (operation)
  ;; Native operation spelling is Core's dotted spelling. Do not infer schemas
  ;; for additional host operations that the reference has no mapping for.
  (cond ((equal operation "catalog.search") "dev.ucp.shopping.catalog.search")
        ((member operation '("catalog.lookup" "catalog.product") :test #'equal)
         "dev.ucp.shopping.catalog.lookup")
        ((member operation '("cart.create" "cart.get" "cart.update" "cart.cancel") :test #'equal)
         "dev.ucp.shopping.cart")
        ((member operation '("checkout.create" "checkout.get" "checkout.update"
                             "checkout.complete" "checkout.cancel") :test #'equal)
         "dev.ucp.shopping.checkout")
        ((equal operation "orders.get") "dev.ucp.shopping.order")))

(defun %ucp-validate-outcome (client operation value)
  (let ((validate (%ucp-client-schema-validator client))
        (root (%ucp-operation-capability operation)) (seen '()))
    (when (and validate root)
      (maphash
       (lambda (name declarations)
         (when (and (%array-p declarations)
                    (or (equal name root)
                        (some (lambda (declaration)
                                (let ((parents (jget declaration "extends")))
                                  (or (equal parents root)
                                      (and (%array-p parents) (find root parents :test #'equal)))))
                              declarations)))
           (loop for declaration across declarations
                 for schema = (jget declaration "schema")
                 when (and (stringp schema) (not (member schema seen :test #'equal)))
                   do (push schema seen) (funcall validate value schema))))
       (ucp-client-capabilities client))))
  value)

(defun ucp-namespace (client)
  (let ((configured (jget (ucp-client-options client) "namespace")))
    (if (stringp configured)
        configured
        (let ((name (jget (ucp-client-profile client) "name")))
          (if (stringp name) name "ucp")))))

(defun ucp-call (client operation payload &key idempotency-key)
  "Perform OPERATION. Core normalizes the binding's answer."
  (unless (member operation +ucp-operations+ :test #'string=)
    (%mcp-fail "Unsupported UCP operation ~a" operation))
  (let* ((options (object "version" (ucp-client-version client)
                          "idempotencyKey" (or idempotency-key (%mcp-uuid))))
         (value (ucp-binding-call (ucp-client-binding client) operation
                                  (%mcp-object-or-empty payload) options)))
    (unless (hash-table-p value)
      (%mcp-fail "UCP binding must return an object"))
    (%ucp-validate-outcome client operation value)
    (let ((outcome (axllm/core::ucp-normalize-outcome operation value)))
      (%set-key outcome "idempotencyKey" (jget options "idempotencyKey"))
      outcome)))

(defun ucp-native-tools (client)
  "CLIENT's operations as namespaced native tools."
  (mapcar (lambda (operation)
            (native-tool
             :name (format nil "~a_~a" (ucp-namespace client) (substitute #\_ #\. operation))
             :description (format nil "UCP ~a operation" operation)
             :parameters (object "type" "object" "properties" (object))
             :protocol (object "kind" "ucp" "namespace" (ucp-namespace client)
                               "name" operation "meta" (object "version" (ucp-client-version client)))
             :handler (lambda (arguments context)
                        (declare (ignore context))
                        (ucp-call client operation arguments))))
          +ucp-operations+))

(defun ucp-runtime-tools (client)
  "CLIENT's operations under their bare UCP names, for a runtime module.

The dotted operation name is the runtime callable's name: a program reaches
checkout.create as ucp.<namespace>.checkout.create. Only the provider-native
tool names in UCP-NATIVE-TOOLS are flattened, because those must satisfy a
provider's function-name grammar."
  (mapcar (lambda (operation)
            (native-tool
             :name operation
             :description (format nil "UCP ~a operation" operation)
             :parameters (object "type" "object" "properties" (object))
             :handler (lambda (arguments context)
                        (declare (ignore context))
                        (ucp-call client operation arguments))))
          +ucp-operations+))

;;; ------------------------------------------------------------------
;;; Execution context
;;; ------------------------------------------------------------------

(defclass execution-context ()
  ((mcp :initarg :mcp :reader execution-context-mcp)
   (ucp :initarg :ucp :reader execution-context-ucp)
   (initialized :initarg :initialized :reader %context-initialized))
  (:documentation
   "The live, inheritable protocol clients an Ax program runs with.

Clients are shared, not copied: a derived child context initializes each
client at most once and keeps the parent's clients usable."))

(defun make-execution-context (&key mcp ucp)
  "An execution context over MCP and UCP clients.

A namespace collision is an error here, before any program runs, because
two servers answering to one name make a tool call ambiguous."
  (let* ((mcp (if (listp mcp) mcp (list mcp)))
         (ucp (if (listp ucp) ucp (list ucp)))
         (mcp (remove nil mcp))
         (ucp (remove nil ucp))
         (namespaces (append (mapcar #'mcp-namespace mcp) (mapcar #'ucp-namespace ucp))))
    (unless (= (length namespaces) (length (remove-duplicates namespaces :test #'string=)))
      (%mcp-fail "MCP/UCP namespace collision"))
    (make-instance 'execution-context :mcp mcp :ucp ucp
                                      :initialized (make-hash-table :test #'eq))))

(defun execution-context-initialize (context)
  "Initialize each attached MCP client once."
  (dolist (client (execution-context-mcp context))
    (unless (gethash client (%context-initialized context))
      (mcp-init client)
      (setf (gethash client (%context-initialized context)) t)))
  context)

(defun execution-context-native-tools (context)
  "Every attached client's native tools, with collisions rejected."
  (execution-context-initialize context)
  (let ((tools (append (loop for client in (execution-context-mcp context)
                             append (mcp-native-tools client))
                       (loop for client in (execution-context-ucp context)
                             append (ucp-native-tools client)))))
    (let ((names (mapcar (lambda (spec) (%mcp-text (jget spec "name"))) tools)))
      (unless (= (length names) (length (remove-duplicates names :test #'string=)))
        (%mcp-fail "MCP/UCP tool collision")))
    tools))

(defun execution-context-runtime-modules (context)
  "The runtime modules a program exposes: mcp.<namespace>.tools and
ucp.<namespace>. Neither protocol is handed to a model as bare functions."
  (append (loop for client in (execution-context-mcp context)
                collect (object "name" (format nil "mcp.~a.tools" (mcp-namespace client))
                                "functions" (coerce (mcp-native-tools client) 'vector)
                                "client" client))
          (loop for client in (execution-context-ucp context)
                collect (object "name" (format nil "ucp.~a" (ucp-namespace client))
                                "functions" (coerce (ucp-runtime-tools client) 'vector)
                                "client" client))))

(defun execution-context-derive (context &optional (inheritance "all"))
  "A child context under INHERITANCE: \"all\", \"none\" or a namespace list.

Core owns the plan, so an unknown or repeated namespace is an error in every
port. Restricting a child never removes the parent's clients."
  (let* ((mcp-names (mapcar #'mcp-namespace (execution-context-mcp context)))
         (ucp-names (mapcar #'ucp-namespace (execution-context-ucp context)))
         (plan (axllm/core::mcp-inheritance-plan
                (coerce mcp-names 'vector) (coerce ucp-names 'vector)
                (if (listp inheritance) (coerce inheritance 'vector) inheritance)))
         (child (make-instance
                 'execution-context
                 :mcp (loop for name across (%event-array (jget plan "mcp"))
                            collect (find (%mcp-text name) (execution-context-mcp context)
                                          :key #'mcp-namespace :test #'string=))
                 :ucp (loop for name across (%event-array (jget plan "ucp"))
                            collect (find (%mcp-text name) (execution-context-ucp context)
                                          :key #'ucp-namespace :test #'string=))
                 :initialized (%context-initialized context))))
    child))

(defun execution-context-continuation-state (context)
  "A resumable description of CONTEXT: namespaces and a catalog fingerprint.

No tokens and no catalog contents are serialized; the fingerprint only tells
a resumed program whether the protocol surface it was built against changed."
  (let ((namespaces (append (mapcar #'mcp-namespace (execution-context-mcp context))
                            (mapcar #'ucp-namespace (execution-context-ucp context)))))
    (object "namespaces" (coerce namespaces 'vector)
            "tasks" (%new-array)
            "subscriptions" (%new-array)
            "catalogFingerprint"
            (%mcp-hex (%mcp-sha256 (%mcp-utf8 (encode-json (coerce (sort (copy-list namespaces) #'string<)
                                                       'vector))))))))

(defun execution-context-descriptor (context &optional (inheritance "all"))
  "CONTEXT as Core describes it: namespaces, inheritance, native, lossyAdapter."
  (axllm/core::mcp-execution-context-descriptor
   (coerce (append (mapcar #'mcp-namespace (execution-context-mcp context))
                   (mapcar #'ucp-namespace (execution-context-ucp context)))
           'vector)
   (if (listp inheritance) (coerce inheritance 'vector) inheritance)))

(defun resolve-execution-context (options &optional parent)
  "The execution context OPTIONS asks for, or PARENT's.

OPTIONS is a JSON object which may carry executionContext, mcp or ucp."
  (let* ((options (%mcp-object-or-empty options))
         (explicit (let ((value (jget options "executionContext")))
                     (if (typep value 'execution-context)
                         value
                         (let ((alternative (jget options "mcpExecutionContext")))
                           (and (typep alternative 'execution-context) alternative))))))
    (cond (explicit explicit)
          ((or (%mcp-present-key-p options "mcp") (%mcp-present-key-p options "ucp"))
           (make-execution-context
            :mcp (let ((value (jget options "mcp")))
                   (cond ((eq value :null) '()) ((listp value) value) (t (list value))))
            :ucp (let ((value (jget options "ucp")))
                   (cond ((eq value :null) '()) ((listp value) value) (t (list value))))))
          (parent parent)
          (t (let ((inherited (jget options "inheritedExecutionContext")))
               (and (typep inherited 'execution-context) inherited))))))

;;; ------------------------------------------------------------------
;;; Core host object protocol
;;; ------------------------------------------------------------------

(%event-define-host-reader mcp-client
  ("namespace" mcp-namespace) ("era" mcp-get-era)
  ("protocolVersion" mcp-negotiated-protocol-version)
  ("serverInfo" mcp-server-info) ("serverCapabilities" mcp-server-capabilities)
  ("tools" mcp-client-tools) ("prompts" mcp-client-prompts)
  ("resources" mcp-client-resources)
  ("resourceTemplates" mcp-client-resource-templates)
  ("revision" mcp-catalog-revision)
  ("extensions" mcp-negotiated-extensions))

(%event-define-host-reader ucp-client
  ("namespace" ucp-namespace) ("version" ucp-client-version)
  ("services" ucp-client-services) ("capabilities" ucp-client-capabilities))

(defmethod axllm/core::core-host-call ((target mcp-client) method args)
  (let ((arguments (%event-array args)))
    (cond ((equal method "callTool")
           (mcp-call-tool target (%mcp-text (jget arguments 0))
                          (%mcp-object-or-empty (jget arguments 1))))
          ((equal method "readResource") (mcp-read-resource target (%mcp-text (jget arguments 0))))
          ((equal method "getPrompt")
           (mcp-get-prompt target (%mcp-text (jget arguments 0))
                           (%mcp-object-or-empty (jget arguments 1))))
          ((equal method "inspectCatalog") (mcp-inspect-catalog target))
          ((equal method "ping") (mcp-ping target))
          (t (%mcp-fail "MCP client has no method ~a" method)))))

(defmethod axllm/core::core-host-call ((target ucp-client) method args)
  (if (member method +ucp-operations+ :test #'equal)
      (ucp-call target method (%mcp-object-or-empty (jget (%event-array args) 0)))
      (%mcp-fail "UCP client has no operation ~a" method)))

(export '(mcp-transport-send-batch mcp-transport-take-request-metadata
          mcp-transport-terminate-session
          mcp-error mcp-error-code mcp-error-data
          mcp-protocol-version mcp-modern-protocol-version mcp-supported-protocol-versions
          mcp-transport mcp-transport-send mcp-transport-send-notification
          mcp-transport-send-with-headers mcp-transport-send-with-context
          mcp-transport-send-response mcp-transport-set-message-handler
          mcp-transport-set-request-handler mcp-transport-set-lifecycle-handler
          mcp-transport-set-protocol-version mcp-transport-protocol-version
          mcp-transport-set-era mcp-transport-era-hint mcp-transport-era-cache-key
          mcp-transport-connect mcp-transport-start-listening
          mcp-transport-open-request-stream mcp-transport-close-request-stream
          mcp-transport-close mcp-transport-dispatch-inbound
          mcp-scripted-transport make-mcp-scripted-transport mcp-scripted-emit
          mcp-scripted-requests mcp-scripted-notifications mcp-scripted-sent-responses
          mcp-scripted-request-headers mcp-scripted-request-streams
          mcp-scripted-era mcp-scripted-session-id
          mcp-scripted-clients mcp-scripted-tool-calls mcp-scripted-methods
          mcp-token-set mcp-oauth-options mcp-validate-endpoint
          mcp-pkce-verifier mcp-pkce-challenge mcp-stdio-encode mcp-stdio-decode
          mcp-client make-mcp-client mcp-init mcp-close mcp-discover mcp-get-era
          mcp-refresh mcp-inspect-catalog mcp-namespace mcp-catalog-revision
          mcp-negotiated-protocol-version mcp-negotiated-extensions
          mcp-server-capabilities mcp-server-info mcp-server-instructions
          mcp-discover-result mcp-client-tools mcp-client-prompts
          mcp-client-resources mcp-client-resource-templates mcp-client-transport
          mcp-client-options
          mcp-ping mcp-list-tools mcp-call-tool mcp-call-tool-outcome
          mcp-list-prompts mcp-get-prompt mcp-list-resources mcp-read-resource
          mcp-list-resource-templates mcp-request mcp-notify mcp-cancel-request
          mcp-subscribe-resource mcp-unsubscribe-resource
          mcp-acquire-resource-subscription mcp-release-resource-subscription
          mcp-restore-resource-subscriptions mcp-start-listening
          mcp-get-task mcp-cancel-task mcp-list-tasks mcp-get-task-result
          mcp-provide-task-input
          native-tool native-tool-name native-tool-description
          native-tool-parameters native-tool-handler native-tool-call
          mcp-native-tools mcp-to-function
          mcp-add-notification-listener mcp-add-lifecycle-listener mcp-emit-lifecycle
          mcp-event-source make-mcp-event-source mcp-event-source-reconnect
          mcp-event-source-subscriptions mcp-event-source-errors
          mcp-event-source-namespace mcp-event-source-client
          ucp-binding-call ucp-client make-ucp-client ucp-namespace ucp-call
          ucp-native-tools ucp-runtime-tools ucp-client-version
          execution-context make-execution-context execution-context-initialize
          execution-context-native-tools execution-context-runtime-modules
          execution-context-derive execution-context-continuation-state
          execution-context-descriptor execution-context-mcp execution-context-ucp
          resolve-execution-context))
