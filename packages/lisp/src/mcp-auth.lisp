;;;; mcp-auth.lisp --- transport authentication, DPoP and JWT verification.
;;;;
;;;; These are the credential-shaped boundaries around an MCP transport:
;;;; simple authentication strategies, RFC 9449 DPoP proofs and an
;;;; OAuth/OIDC JWT verifier over a JWKS. None of them is Core policy; Core
;;;; owns the OAuth middle tier (discovery, grant bodies, token parsing,
;;;; issuer checks) and this file owns the cryptography and the HTTP-facing
;;;; shapes Core cannot express.
;;;;
;;;; No cryptography is written here. Hashing, HMAC, ECDSA, Ed25519 and RSA
;;;; all come from Ironclad; base64 from cl-base64. A verifier that cannot
;;;; reach a real primitive fails closed with the algorithm named, because a
;;;; signature check that quietly degrades is worse than one that refuses.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Secrets
;;; ------------------------------------------------------------------

(defun %mcp-resolve-secret (provider)
  "PROVIDER as a string: either the string itself or a thunk returning one.

A thunk is the supported shape, so a caller can read a rotating credential
at request time without this file ever storing it."
  (let ((value (if (functionp provider) (funcall provider) provider)))
    (unless (stringp value)
      (%mcp-fail "MCP authentication secret must resolve to a string"))
    value))

;;; ------------------------------------------------------------------
;;; Authentication strategies
;;; ------------------------------------------------------------------
;;;
;;; A strategy is a function of one request object returning an object with
;;; optional "headers" and "query" members. MCP-APPLY-AUTHENTICATION folds a
;;; list of them over an outgoing request, in order, so a bearer token and
;;; an HMAC signature compose without either knowing about the other.

(defun mcp-bearer-authentication (token &optional (token-type "Bearer"))
  "Send TOKEN as an Authorization header."
  (lambda (request)
    (declare (ignore request))
    (object "headers" (object "Authorization"
                              (format nil "~a ~a" token-type (%mcp-resolve-secret token))))))

(defun mcp-basic-authentication (username password)
  "Send USERNAME and PASSWORD as HTTP Basic credentials."
  (lambda (request)
    (declare (ignore request))
    (object "headers"
            (object "Authorization"
                    (format nil "Basic ~a"
                            (%mcp-base64 (%mcp-utf8 (format nil "~a:~a"
                                                    (%mcp-resolve-secret username)
                                                    (%mcp-resolve-secret password)))))))))

(defun mcp-api-key-authentication (key &key (name "X-API-Key") (in :header) (prefix ""))
  "Send KEY as a header or query parameter named NAME."
  (lambda (request)
    (declare (ignore request))
    (let ((value (concatenate 'string prefix (%mcp-resolve-secret key))))
      (if (eq in :query)
          (object "query" (object name value))
          (object "headers" (object name value))))))

(defun mcp-hmac-authentication (&key key-id secret (signature-header "X-Signature")
                                     (timestamp-header "X-Timestamp")
                                     (nonce-header "X-Nonce")
                                     now nonce)
  "Sign each request with HMAC-SHA256 over a canonical string.

The canonical string is method, path with query, the body's SHA-256 hex
digest, the timestamp and the nonce, newline-joined. The timestamp and nonce
travel in their own headers so a server can reject a stale or replayed
request; NOW and NONCE are injectable for deterministic tests."
  (lambda (request)
    (let* ((timestamp (format nil "~a" (if now (funcall now) (%mcp-now-ms))))
           (nonce-value (if nonce (funcall nonce) (%mcp-uuid)))
           (uri (puri:parse-uri (%mcp-text (jget request "url"))))
           (path (format nil "~a~a"
                         (or (puri:uri-path uri) "/")
                         (let ((query (puri:uri-query uri)))
                           (if query (format nil "?~a" query) ""))))
           (body (let ((value (jget request "body"))) (if (stringp value) value "")))
           (canonical (format nil "~a~c~a~c~a~c~a~c~a"
                              (string-upcase (%mcp-text (jget request "method"))) #\Newline
                              path #\Newline
                              (%mcp-hex (%mcp-sha256 (%mcp-utf8 body))) #\Newline
                              timestamp #\Newline
                              nonce-value)))
      (object "headers"
              (object signature-header
                      (format nil "keyId=~a,algorithm=hmac-sha256,signature=~a"
                              key-id (%mcp-hex (%mcp-hmac-sha256 (%mcp-utf8 (%mcp-resolve-secret secret))
                                                         (%mcp-utf8 canonical))))
                      timestamp-header timestamp
                      nonce-header nonce-value)))))

(defun %mcp-hmac-sha256 (key message)
  (let ((hmac (ironclad:make-hmac (coerce key '(vector (unsigned-byte 8))) :sha256)))
    (ironclad:update-hmac hmac (coerce message '(vector (unsigned-byte 8))))
    (ironclad:hmac-digest hmac)))

(defun mcp-apply-authentication (url headers method body authentication)
  "Fold AUTHENTICATION over one outgoing request.

Returns (values url headers). AUTHENTICATION is one strategy or a list of
them; each sees the headers the previous ones produced, and query
parameters are merged into URL."
  ;; An unconfigured option reads as :NULL, not NIL, so both mean "no
  ;; authentication". Testing only for NIL let :NULL through as a one-element
  ;; strategy list and the request died on (funcall :null ...) -- a request
  ;; with no authentication at all was the one case the scripted fixtures
  ;; never exercise, because they always configure a strategy.
  (if (or (null authentication) (eq authentication :null))
      (values url headers)
      (let ((strategies (remove :null
                                (if (listp authentication)
                                    authentication
                                    (list authentication))))
            (result-headers (axllm/core::core-map-merge (object) (%mcp-object-or-empty headers)))
            (query '()))
        (dolist (strategy strategies)
          (let ((result (%mcp-object-or-empty
                         (funcall strategy
                                  (object "url" url
                                          "method" (or method "GET")
                                          "headers" result-headers
                                          "body" (or body :null))))))
            (let ((produced (%mcp-object-or-empty (jget result "headers"))))
              (dolist (key (%object-keys produced))
                (%set-key result-headers key (gethash key produced))))
            (let ((produced (%mcp-object-or-empty (jget result "query"))))
              (dolist (key (%object-keys produced))
                (setf query (append (remove key query :key #'car :test #'string=)
                                    (list (cons key (%mcp-text (gethash key produced))))))))))
        (values (if query (%mcp-merge-query url query) url) result-headers))))

(defun %mcp-url-encode (text)
  "TEXT percent-encoded for a query value, as RFC 3986 unreserved allows."
  (with-output-to-string (stream)
    (loop for byte across (%mcp-utf8 text)
          for char = (code-char byte)
          do (if (or (alphanumericp char) (find char "-_.~"))
                 (write-char char stream)
                 (format stream "%~2,'0X" byte)))))

(defun %mcp-merge-query (url pairs)
  (let* ((separator (if (find #\? url) "&" "?")))
    (format nil "~a~a~{~a~^&~}" url separator
            (mapcar (lambda (pair)
                      (format nil "~a=~a" (%mcp-url-encode (car pair)) (%mcp-url-encode (cdr pair))))
                    pairs))))

(defun %mcp-form-encode (body)
  "BODY as application/x-www-form-urlencoded."
  (format nil "~{~a~^&~}"
          (mapcar (lambda (key)
                    (format nil "~a=~a" (%mcp-url-encode key)
                            (%mcp-url-encode (%mcp-text (gethash key body)))))
                  (%object-keys body))))

;;; ------------------------------------------------------------------
;;; Bounded JSON over HTTP
;;; ------------------------------------------------------------------
;;;
;;; JWKS documents, protected-resource metadata, authorization-server
;;; metadata and token endpoints are all small JSON documents fetched from a
;;; URL a server told us about, which is exactly the SSRF-sensitive shape.
;;; Every one of them goes through MCP-VALIDATE-ENDPOINT first and through
;;; Drakma with redirects disabled, so a 302 can never replay an
;;; Authorization header to another host.

(defparameter +mcp-metadata-byte-limit+ (* 1024 1024)
  "The largest discovery or JWKS document this client will parse.")

(defun %mcp-http-json (url &key ssrf-protection (method :get) content content-type
                            headers (timeout 30))
  (let ((checked (mcp-validate-endpoint url ssrf-protection)))
    (handler-case
        (multiple-value-bind (body status)
            (drakma:http-request checked
                                 :method method
                                 :additional-headers
                                 (append '(("Accept" . "application/json"))
                                         (loop for key in (%object-keys (%mcp-object-or-empty headers))
                                               collect (cons key (%mcp-text
                                                                  (gethash key
                                                                           (%mcp-object-or-empty headers))))))
                                 :content content
                                 :content-type content-type
                                 :external-format-out :utf-8
                                 :external-format-in :utf-8
                                 :connection-timeout timeout
                                 :verify :required
                                 :redirect nil
                                 :force-binary nil
                                 :want-stream nil)
          (let ((text (if (stringp body) body (%mcp-from-utf8 body))))
            (when (> (length text) +mcp-metadata-byte-limit+)
              (%mcp-fail "~a returned more than ~a bytes" checked +mcp-metadata-byte-limit+))
            (unless (and (integerp status) (<= 200 status 299))
              (%mcp-fail "~a request failed: HTTP ~a" checked status))
            (parse-json text)))
      (mcp-error (condition) (error condition))
      (ax-error (condition) (error condition))
      (error (condition)
        (%mcp-fail "~a request failed: ~a" checked
                   (substitute #\Space #\Newline (princ-to-string condition)))))))

(defun %mcp-http-json-get (url &optional ssrf-protection)
  (%mcp-http-json url :ssrf-protection ssrf-protection))

(defun %mcp-http-form-post (url body &optional ssrf-protection)
  (%mcp-http-json url :ssrf-protection ssrf-protection :method :post
                  :content (%mcp-form-encode (%mcp-object-or-empty body))
                  :content-type "application/x-www-form-urlencoded"))

;;; ------------------------------------------------------------------
;;; Base64url and JWK helpers
;;; ------------------------------------------------------------------

(defun %mcp-base64url-decode (text)
  "TEXT as octets, accepting unpadded base64url."
  (let* ((normalized (substitute #\/ #\_ (substitute #\+ #\- text)))
         (padding (mod (- 4 (mod (length normalized) 4)) 4))
         (padded (concatenate 'string normalized (make-string padding :initial-element #\=))))
    (handler-case (cl-base64:base64-string-to-usb8-array padded)
      (error () (%mcp-fail "invalid base64url encoding")))))

(defun %mcp-jwk-integer (jwk key)
  (let ((value (jget jwk key)))
    (unless (stringp value) (%mcp-fail "JWK is missing ~a" key))
    (ironclad:octets-to-integer (%mcp-base64url-decode value))))

(defun %mcp-jwk-public-key (jwk algorithm)
  "JWK as an Ironclad public key for ALGORITHM, or an error naming why not.

Only the key families the algorithms below actually need are built; an
unknown curve or key type fails rather than falling through to a weaker
check."
  (let ((kty (%mcp-text (jget jwk "kty"))))
    (cond ((and (string= kty "RSA")
                (member algorithm '("RS256" "RS384" "RS512" "PS256" "PS384" "PS512")
                        :test #'string=))
           (ironclad:make-public-key :rsa :n (%mcp-jwk-integer jwk "n")
                                          :e (%mcp-jwk-integer jwk "e")))
          ((and (string= kty "EC") (member algorithm '("ES256" "ES384") :test #'string=))
           (let* ((curve (if (string= algorithm "ES256") :secp256r1 :secp384r1))
                  (size (if (string= algorithm "ES256") 32 48))
                  (x (%mcp-base64url-decode (%mcp-text (jget jwk "x"))))
                  (y (%mcp-base64url-decode (%mcp-text (jget jwk "y")))))
             (unless (and (= (length x) size) (= (length y) size))
               (%mcp-fail "JWK EC coordinates do not match ~a" algorithm))
             (ironclad:make-public-key
              curve :y (concatenate '(vector (unsigned-byte 8)) #(4) x y))))
          ((and (string= kty "OKP") (string= algorithm "EdDSA"))
           (unless (string= (%mcp-text (jget jwk "crv")) "Ed25519")
             (%mcp-fail "only Ed25519 is supported for EdDSA"))
           (ironclad:make-public-key
            :ed25519 :y (%mcp-base64url-decode (%mcp-text (jget jwk "x")))))
          (t (%mcp-fail "JWK key type ~a cannot verify ~a" kty algorithm)))))

(defparameter +jwt-digest+
  '(("RS256" . :sha256) ("RS384" . :sha384) ("RS512" . :sha512)
    ("PS256" . :sha256) ("PS384" . :sha384) ("PS512" . :sha512)
    ("ES256" . :sha256) ("ES384" . :sha384)
    ("EdDSA" . nil)))

(defparameter +jwt-default-algorithms+
  '("RS256" "RS384" "RS512" "PS256" "PS384" "PS512" "ES256" "ES384" "EdDSA")
  "The JWT algorithms this verifier will consider.

ES512 and the \"none\" algorithm are absent on purpose: P-521 is not in the
curve set, and an unsigned token must never verify.")

(defun %mcp-jwt-verify-signature (algorithm jwk signing-input signature)
  "Whether SIGNATURE over SIGNING-INPUT verifies under JWK."
  (let ((key (%mcp-jwk-public-key jwk algorithm))
        (digest (cdr (assoc algorithm +jwt-digest+ :test #'string=))))
    (handler-case
        (cond ((string= algorithm "EdDSA")
               (ironclad:verify-signature key signing-input signature))
              ((char= (char algorithm 0) #\E)
               ;; A JWS ECDSA signature is raw r||s; Ironclad wants the same
               ;; fixed-width pair, so the length is checked rather than
               ;; guessed.
               (let ((size (if (string= algorithm "ES256") 64 96)))
                 (and (= (length signature) size)
                      (ironclad:verify-signature key (ironclad:digest-sequence digest signing-input)
                                                 signature))))
              ((string= (subseq algorithm 0 2) "PS")
               (ironclad:verify-signature key (ironclad:digest-sequence digest signing-input)
                                          signature :pss t :digest digest))
              (t (ironclad:verify-signature key (ironclad:digest-sequence digest signing-input)
                                            signature :pkcs1-v1.5 t :digest digest)))
      (error () nil))))

;;; ------------------------------------------------------------------
;;; JWT verifier
;;; ------------------------------------------------------------------

(defclass mcp-jwt-verifier ()
  ((allowed-algorithms :initarg :allowed-algorithms :reader %jwt-allowed)
   (clock-tolerance :initarg :clock-tolerance :reader %jwt-tolerance)
   (now :initarg :now :reader %jwt-now)
   (fetcher :initarg :fetcher :reader %jwt-fetcher)
   (ssrf-protection :initarg :ssrf-protection :reader %jwt-ssrf)
   (jwks-cache :initform (make-hash-table :test #'equal) :reader %jwt-cache))
  (:documentation
   "An OAuth/OIDC JWT verifier over a JWKS endpoint.

The JWKS is cached by URI. MCP-JWT-CLEAR-CACHE drops one or all entries,
which is what a host calls after a signing-key rotation so the next verify
re-fetches instead of rejecting a freshly rotated kid."))

(defun make-mcp-jwt-verifier (&key allowed-algorithms (clock-tolerance-seconds 60)
                                   now fetcher ssrf-protection)
  "A JWT verifier.

FETCHER, when given, replaces the HTTP GET used to load a JWKS; it receives
the URI and returns a JSON object. It exists for tests and for hosts with
their own bounded fetch, not to bypass the SSRF gate, which still runs on
the default path."
  (make-instance 'mcp-jwt-verifier
                 :allowed-algorithms (or allowed-algorithms +jwt-default-algorithms+)
                 :clock-tolerance clock-tolerance-seconds
                 :now now :fetcher fetcher :ssrf-protection ssrf-protection))

(defun mcp-jwt-clear-cache (verifier &optional jwks-uri)
  (if jwks-uri
      (remhash jwks-uri (%jwt-cache verifier))
      (clrhash (%jwt-cache verifier)))
  nil)

(defun %mcp-jwt-decode-segment (text label)
  (handler-case (parse-json (%mcp-from-utf8 (%mcp-base64url-decode text)))
    (error () (%mcp-fail "OAuth JWT has an invalid ~a" label))))

(defun %mcp-jwt-jwks (verifier uri)
  (or (gethash uri (%jwt-cache verifier))
      (let* ((document (if (%jwt-fetcher verifier)
                           (funcall (%jwt-fetcher verifier) uri)
                           (%mcp-http-json-get uri (%jwt-ssrf verifier))))
             (keys (jget (%mcp-object-or-empty document) "keys")))
        (unless (and (%array-p keys) (plusp (length keys)))
          (%mcp-fail "OAuth JWKS response contains no keys"))
        (setf (gethash uri (%jwt-cache verifier)) keys))))

(defun mcp-jwt-verify (verifier token &key issuer audience nonce jwks-uri)
  "Verify TOKEN and return its header and claims.

Signature first, then claims: issuer, audience (with the azp rule for a
multi-audience token), exp, iat, nbf and nonce. Every failure is an error;
there is no partially verified result. A kid that is absent from the cached
JWKS triggers one refresh, so a rotated key verifies without a restart."
  (let ((parts (cl-ppcre:split "\\." token)))
    (unless (and (= 3 (length parts)) (every (lambda (part) (plusp (length part))) parts))
      (%mcp-fail "OAuth JWT must contain three encoded segments"))
    (destructuring-bind (header-text claims-text signature-text) parts
      (let* ((header (%mcp-jwt-decode-segment header-text "header"))
             (claims (%mcp-jwt-decode-segment claims-text "claims"))
             (algorithm (jget header "alg")))
        (unless (and (stringp algorithm)
                     (member algorithm (%jwt-allowed verifier) :test #'string=))
          (%mcp-fail "OAuth JWT uses disallowed algorithm ~a" (%mcp-text algorithm)))
        (let* ((signing-input (%mcp-utf8 (format nil "~a.~a" header-text claims-text)))
               (signature (%mcp-base64url-decode signature-text))
               (kid (jget header "kid")))
          (flet ((candidates (keys)
                   (loop for jwk across (%event-array keys)
                         when (and (or (not (stringp kid)) (equal (jget jwk "kid") kid))
                                   (let ((alg (jget jwk "alg")))
                                     (or (not (stringp alg)) (string= alg algorithm)))
                                   (let ((use (jget jwk "use")))
                                     (or (not (stringp use)) (string= use "sig"))))
                           collect jwk)))
            (let* ((keys (%mcp-jwt-jwks verifier jwks-uri))
                   (eligible (candidates keys)))
              (when (null eligible)
                ;; A rotated signing key is the common cause; refresh once.
                (mcp-jwt-clear-cache verifier jwks-uri)
                (setf eligible (candidates (%mcp-jwt-jwks verifier jwks-uri))))
              (when (null eligible)
                (%mcp-fail "OAuth JWT signing key ~a not found"
                           (if (stringp kid) kid "<none>")))
              (unless (some (lambda (jwk)
                              (%mcp-jwt-verify-signature algorithm jwk signing-input signature))
                            eligible)
                (%mcp-fail "OAuth JWT signature verification failed")))))
        (%mcp-jwt-validate-claims verifier claims issuer audience nonce)
        (values header claims)))))

(defun %mcp-jwt-audience-list (value)
  (cond ((stringp value) (list value))
        ((%array-p value)
         (let ((items (coerce value 'list)))
           (if (every #'stringp items) items '())))
        ((listp value) (remove-if-not #'stringp value))
        (t '())))

(defun %mcp-jwt-validate-claims (verifier claims issuer audience nonce)
  (let* ((now (floor (/ (if (%jwt-now verifier) (funcall (%jwt-now verifier)) (%mcp-now-ms)) 1000)))
         (tolerance (%jwt-tolerance verifier))
         (actual (%mcp-jwt-audience-list (jget claims "aud")))
         (expected (%mcp-jwt-audience-list (if (listp audience) (or audience '()) audience))))
    (unless (equal (jget claims "iss") issuer)
      (%mcp-fail "OAuth JWT issuer mismatch"))
    (unless (some (lambda (one) (member one actual :test #'string=)) expected)
      (%mcp-fail "OAuth JWT audience mismatch"))
    (when (> (length actual) 1)
      (let ((azp (jget claims "azp")))
        (unless (stringp azp)
          (%mcp-fail "OAuth ID token with multiple audiences is missing azp"))
        (unless (member azp expected :test #'string=)
          (%mcp-fail "OAuth JWT authorized-party mismatch"))))
    (let ((exp (jget claims "exp")))
      (unless (and (realp exp) (<= now (+ exp tolerance)))
        (%mcp-fail "OAuth JWT is expired or missing exp")))
    (let ((iat (jget claims "iat")))
      (unless (and (realp iat) (<= iat (+ now tolerance)))
        (%mcp-fail "OAuth JWT has an invalid or missing iat")))
    (let ((nbf (jget claims "nbf")))
      (when (and (realp nbf) (> nbf (+ now tolerance)))
        (%mcp-fail "OAuth JWT is not active yet")))
    (when (and nonce (not (equal (jget claims "nonce") nonce)))
      (%mcp-fail "OAuth ID token nonce mismatch")))
  nil)

;;; ------------------------------------------------------------------
;;; DPoP
;;; ------------------------------------------------------------------

(defclass mcp-dpop-factory ()
  ((private-key :initarg :private-key :accessor %dpop-private-key)
   (public-jwk :initarg :public-jwk :accessor %dpop-public-jwk)
   (create-proof :initarg :create-proof :reader %dpop-create-proof)
   (now :initarg :now :reader %dpop-now)
   (jti :initarg :jti :reader %dpop-jti)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-dpop") :reader %dpop-lock))
  (:documentation
   "An RFC 9449 DPoP proof factory.

One ES256 key pair per factory, generated on first use and reused, so every
proof a transport sends is bound to the same public JWK the authorization
server saw. A host that keeps its key in a device or an HSM supplies
:CREATE-PROOF instead and this file never sees a private key."))

(defun make-mcp-dpop-factory (&key private-key public-jwk create-proof now jti)
  (make-instance 'mcp-dpop-factory :private-key private-key :public-jwk public-jwk
                                   :create-proof create-proof :now now :jti jti))

(defun %mcp-dpop-keys (factory)
  "FACTORY's key pair, generating one once if the host supplied none."
  (sb-thread:with-mutex ((%dpop-lock factory))
    (unless (and (%dpop-private-key factory) (%dpop-public-jwk factory))
      (multiple-value-bind (public private)
          (ironclad:generate-key-pair :secp256r1)
        (let* ((point (ironclad:destructure-public-key public))
               (y (getf point :y)))
          ;; An uncompressed SEC1 point is 0x04 || X || Y for P-256.
          (unless (and (= (length y) 65) (= (aref y 0) 4))
            (%mcp-fail "generated DPoP key is not an uncompressed P-256 point"))
          (setf (%dpop-private-key factory) private
                (%dpop-public-jwk factory)
                (object "kty" "EC" "crv" "P-256"
                        "x" (%mcp-base64url (subseq y 1 33))
                        "y" (%mcp-base64url (subseq y 33 65)))))))
    (values (%dpop-private-key factory) (%dpop-public-jwk factory))))

(defun mcp-dpop-proof (factory &key url method access-token nonce)
  "A DPoP proof JWT binding METHOD and URL to FACTORY's key.

htu is the origin and path only, as RFC 9449 requires; a query string or
fragment is excluded. ath is the access token's SHA-256 thumbprint, present
only when a token is being presented."
  (when (%dpop-create-proof factory)
    (return-from mcp-dpop-proof
      (funcall (%dpop-create-proof factory)
               (object "url" url "method" method
                       "accessToken" (or access-token :null)
                       "nonce" (or nonce :null)))))
  (multiple-value-bind (private public-jwk) (%mcp-dpop-keys factory)
    (let* ((uri (puri:parse-uri url))
           (origin (format nil "~(~a~)://~a~@[:~a~]"
                           (puri:uri-scheme uri) (puri:uri-host uri)
                           (let ((port (puri:uri-port uri)))
                             (and port
                                  (not (member port '(80 443)))
                                  port))))
           (header (object "typ" "dpop+jwt" "alg" "ES256" "jwk" public-jwk))
           (payload (object "jti" (if (%dpop-jti factory) (funcall (%dpop-jti factory)) (%mcp-uuid))
                            "htm" (string-upcase (%mcp-text method))
                            "htu" (format nil "~a~a" origin (or (puri:uri-path uri) "/"))
                            "iat" (floor (/ (if (%dpop-now factory)
                                                (funcall (%dpop-now factory))
                                                (%mcp-now-ms))
                                            1000)))))
      (when nonce (%set-key payload "nonce" nonce))
      (when access-token
        (%set-key payload "ath" (%mcp-base64url (%mcp-sha256 (%mcp-utf8 access-token)))))
      (let* ((signing-input (format nil "~a.~a"
                                    (%mcp-base64url (%mcp-utf8 (encode-json header)))
                                    (%mcp-base64url (%mcp-utf8 (encode-json payload)))))
             (signature (ironclad:sign-message
                         private (ironclad:digest-sequence :sha256 (%mcp-utf8 signing-input)))))
        (format nil "~a.~a" signing-input (%mcp-base64url signature))))))

(export '(mcp-bearer-authentication mcp-basic-authentication
          mcp-api-key-authentication mcp-hmac-authentication
          mcp-apply-authentication
          mcp-jwt-verifier make-mcp-jwt-verifier mcp-jwt-verify mcp-jwt-clear-cache
          mcp-dpop-factory make-mcp-dpop-factory mcp-dpop-proof))
