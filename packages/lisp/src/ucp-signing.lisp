;;;; ucp-signing.lisp --- UCP HTTP Message Signatures, the native half.
;;;;
;;;; The portable half lives in Core, added to ir/axcore/mcp.axir after a
;;;; direct comparison with src/ax/ucp/signing.ts: UCP-SIGNATURE-COMPONENTS,
;;;; UCP-SIGNATURE-PARAMS, UCP-SIGNATURE-BASE, UCP-SIGNATURE-HEADERS and
;;;; UCP-VERIFY-SIGNATURE-POLICY. Nothing in this file re-derives any of
;;;; that; a second canonicalization is exactly the bug that makes two ports
;;;; disagree about a base a verifier has to rebuild byte for byte.
;;;;
;;;; What is native here, and why each part has to be:
;;;;
;;;;   Crypto. Ironclad, for SHA-256 and HMAC. No primitive is hand-rolled
;;;;   and no signature algorithm is implemented here.
;;;;
;;;;   The clock. Signature expiry is a real-time decision, so the caller
;;;;   can inject one; Core is handed a number and never reads a clock.
;;;;
;;;;   URL parsing. @authority, @path and @query come from puri, because
;;;;   Core has no URL model and should not grow one for this.
;;;;
;;;;   Key material and its refresh. A JWKS fetch is I/O, and the replay
;;;;   set is per-process state. Core decides WHETHER a refresh or a replay
;;;;   rejection is called for; this file performs it.
;;;;
;;;; The signing algorithm itself is a function the caller supplies, which
;;;; is how the reference does it too (options.sign). UCP-HMAC-SIGNER is
;;;; provided because it is testable without key distribution; it is not a
;;;; recommendation for production, and the docstring says so.

(in-package #:axllm)

(define-condition ucp-signature-error (error)
  ((code :initarg :code :initform "signature_invalid" :reader ucp-signature-error-code)
   (message :initarg :message :reader ucp-signature-error-message))
  (:report (lambda (condition stream)
             (format stream "~a (~a)"
                     (ucp-signature-error-message condition)
                     (ucp-signature-error-code condition))))
  (:documentation
   "A UCP signing or verification failure.

CODE is one of the reference's AxUCPHTTPMessageSignatureErrorCode values, so
a caller can branch on the same string every other port reports."))

(defun %ucp-fail (code format-control &rest arguments)
  (error 'ucp-signature-error :code code
                              :message (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; Digests
;;; ------------------------------------------------------------------

(defun ucp-content-digest (body)
  "BODY's Content-Digest header value, as RFC 9530 sha-256.

The structured-field byte-sequence form wraps base64 in colons; that is the
reference's `sha-256=:...:` exactly, and a verifier compares the string."
  (format nil "sha-256=:~a:"
          (cl-base64:usb8-array-to-base64-string
           (ironclad:digest-sequence :sha256 (%mcp-utf8 body)))))

(defun ucp-verify-content-digest (body header)
  "Check HEADER against BODY, or signal digest_mismatch."
  (unless (stringp header)
    (%ucp-fail "digest_mismatch" "UCP response has a body but no Content-Digest"))
  (let ((expected (ucp-content-digest body)))
    ;; Compared as the whole structured-field value, so a header naming a
    ;; different algorithm fails rather than being silently accepted.
    (unless (string= expected header)
      (%ucp-fail "digest_mismatch" "UCP response body does not match its Content-Digest"))
    t))

;;; ------------------------------------------------------------------
;;; Signers
;;; ------------------------------------------------------------------

(defun ucp-hmac-signer (secret)
  "A signer over HMAC-SHA256 with SECRET, for tests and symmetric deployments.

Returns a function of (signature-base context) answering the raw signature
bytes, which is the shape UCP-SIGN-REQUEST expects and the same shape the
reference's options.sign has. This is deliberately not an ECDSA default:
asymmetric keys need distribution this file has no business inventing, so a
caller doing ES256 passes its own signer and keeps its key handling."
  (let ((key (if (stringp secret) (%mcp-utf8 secret) secret)))
    (lambda (signature-base context)
      (declare (ignore context))
      (let ((mac (ironclad:make-mac :hmac key :sha256)))
        (ironclad:update-mac mac signature-base)
        (ironclad:produce-mac mac)))))

;;; ------------------------------------------------------------------
;;; Signing
;;; ------------------------------------------------------------------

(defun %ucp-header (headers name)
  "HEADERS' value for NAME, matched case-insensitively, or :NULL."
  (let ((wanted (string-downcase name)))
    (dolist (key (%object-keys headers) :null)
      (when (string= (string-downcase key) wanted)
        (return (gethash key headers))))))

(defun %ucp-default-port-p (scheme port)
  "Whether PORT is SCHEME's default, and so suppressed from the authority.

Default-port suppression is per scheme, which is the whole point: 80 is
default for http and ws but is a NON-default port on https, and 443 is the
reverse. A scheme-blind check against both numbers signs
\"example.com\" for https://example.com:80, while the request is sent to
:80 and the verifier rebuilds \"example.com:80\" -- the signature then
fails to verify for a reason nothing reports."
  (let ((scheme (string-downcase (string (or scheme "")))))
    (or (and (member scheme '("http" "ws") :test #'string=) (eql port 80))
        (and (member scheme '("https" "wss") :test #'string=) (eql port 443)))))

(defun %ucp-authority (uri)
  "URI's @authority component, as the reference's URL.host produces it."
  (let ((host (or (puri:uri-host uri) ""))
        (port (puri:uri-port uri))
        (scheme (puri:uri-scheme uri)))
    (if (and port (not (%ucp-default-port-p scheme port)))
        (format nil "~a:~a" host port)
        host)))

(defun %ucp-component-values (uri method headers)
  "The derived and header component values Core's signature base needs."
  (let ((values (object)))
    (%set-key values "@method" (string-upcase method))
    (%set-key values "@authority" (%ucp-authority uri))
    (%set-key values "@path" (or (puri:uri-path uri) "/"))
    (let ((query (puri:uri-query uri)))
      (when query (%set-key values "@query" (format nil "?~a" query))))
    (dolist (key (%object-keys headers))
      (%set-key values (string-downcase key) (gethash key headers)))
    values))

(defmacro %with-ucp-core-errors (&body body)
  "Run BODY, reporting a Core raise as a UCP-SIGNATURE-ERROR.

Core signals AX-ERROR from core.raise, which is right for Core but wrong at
this boundary: a caller handling UCP-SIGNATURE-ERROR would miss it, and the
code it needs to branch on would be absent. The only Core raise reachable
from here is the missing-component guard in UCP-SIGNATURE-BASE, which is a
malformed signing request."
  `(handler-case (progn ,@body)
     (ucp-signature-error (condition) (error condition))
     (ax-error (condition)
       (%ucp-fail "signature_invalid" "~a" (ax-error-message condition)))))

(defun ucp-sign-request (url method headers body
                         &key key-id algorithm (label "sig1") components
                              created nonce sign)
  "Sign one UCP request. Returns the headers to send, including the body digest.

URL, METHOD, HEADERS and BODY describe the request; BODY may be NIL. SIGN is
a function of (signature-base-octets context) returning the signature bytes,
as UCP-HMAC-SIGNER produces. CREATED and NONCE are thunks, so the caller owns
the clock and the nonce source.

Every policy decision here is Core's: which components are covered, the
@signature-params string, the signature base and the two header shapes. This
function parses the URL, hashes the body, calls SIGN and merges headers."
  (unless (functionp sign)
    (%ucp-fail "signature_invalid" "UCP signing needs a sign function"))
  (unless (stringp key-id)
    (%ucp-fail "key_not_found" "UCP signing needs a key id"))
  (let* ((outgoing (axllm/core::core-map-merge (object) (%mcp-object-or-empty headers)))
         (uri (handler-case (puri:parse-uri url)
                (error () (%ucp-fail "signature_invalid" "UCP signing needs a valid URL")))))
    (when body
      (unless (stringp (%ucp-header outgoing "content-type"))
        (%set-key outgoing "Content-Type" "application/json"))
      (%set-key outgoing "Content-Digest" (ucp-content-digest body)))
    (%with-ucp-core-errors
     (let* ((selected
             (if components
                 (coerce components 'vector)
                 (axllm/core::ucp-signature-components
                  (json-boolean (and (puri:uri-query uri) t))
                  (json-boolean (stringp (%ucp-header outgoing "ucp-agent")))
                  (json-boolean (stringp (%ucp-header outgoing "idempotency-key")))
                  (json-boolean (and body t)))))
           (created-value (if created
                              (funcall created)
                              ;; Unix seconds, which is what the reference's
                              ;; Date.now()/1000 floor produces.
                              (- (floor (get-universal-time))
                                 (encode-universal-time 0 0 0 1 1 1970 0))))
           (params (axllm/core::ucp-signature-params
                    selected created-value key-id
                    (if algorithm algorithm :null)
                    (if nonce (funcall nonce) :null)))
           (values (%ucp-component-values uri method outgoing))
           (base (axllm/core::ucp-signature-base selected values params))
           (signature (funcall sign (%mcp-utf8 base) (object "signatureInput" params))))
      (axllm/core::core-map-merge
       outgoing
       (axllm/core::ucp-signature-headers
        label params
        (cl-base64:usb8-array-to-base64-string
         (coerce signature '(vector (unsigned-byte 8))))))))))

(defun ucp-request-signature-base (url method headers body
                                   &key key-id algorithm components created nonce)
  "The exact signature base UCP-SIGN-REQUEST would sign, without signing it.

Exposed because a signature base is the thing two implementations must agree
on, so it has to be assertable on its own rather than only through a
signature that could match for the wrong reason."
  (let* ((outgoing (axllm/core::core-map-merge (object) (%mcp-object-or-empty headers)))
         (uri (puri:parse-uri url)))
    (when body
      (unless (stringp (%ucp-header outgoing "content-type"))
        (%set-key outgoing "Content-Type" "application/json"))
      (%set-key outgoing "Content-Digest" (ucp-content-digest body)))
    (%with-ucp-core-errors
     (let* ((selected (if components
                         (coerce components 'vector)
                         (axllm/core::ucp-signature-components
                          (json-boolean (and (puri:uri-query uri) t))
                          (json-boolean (stringp (%ucp-header outgoing "ucp-agent")))
                          (json-boolean (stringp (%ucp-header outgoing "idempotency-key")))
                          (json-boolean (and body t)))))
           (params (axllm/core::ucp-signature-params
                    selected (if created (funcall created) 0) key-id
                    (if algorithm algorithm :null)
                    (if nonce (funcall nonce) :null))))
      (values (axllm/core::ucp-signature-base
               selected (%ucp-component-values uri method outgoing) params)
              params)))))

;;; ------------------------------------------------------------------
;;; Verification
;;; ------------------------------------------------------------------

(defun %ucp-parse-signature-input (header)
  "Parse a Signature-Input header into what Core's policy op reads.

Returns an object with label, components, created, expires, keyId, algorithm
and nonce. Only the structure is parsed here; every judgement about it is
Core's."
  (let* ((equals (position #\= header))
         (label (and equals (string-trim " " (subseq header 0 equals))))
         (rest (if equals (subseq header (1+ equals)) header))
         (close (position #\) rest))
         (inside (if close (subseq rest 1 close) ""))
         (params (if close (subseq rest (1+ close)) ""))
         (components (%new-array))
         (out (object)))
    (dolist (piece (coerce (axllm/core::core-string-split inside " ") 'list))
      (let ((trimmed (string-trim '(#\Space #\") piece)))
        (when (plusp (length trimmed))
          (vector-push-extend trimmed components))))
    (%set-key out "label" (or label "sig1"))
    (%set-key out "components" components)
    (%set-key out "created" :null)
    (%set-key out "expires" :null)
    (%set-key out "keyId" :null)
    (%set-key out "algorithm" :null)
    (%set-key out "nonce" :null)
    (dolist (piece (coerce (axllm/core::core-string-split params ";") 'list))
      (let ((split (position #\= piece)))
        (when split
          (let ((name (string-trim " " (subseq piece 0 split)))
                (value (string-trim '(#\Space #\") (subseq piece (1+ split)))))
            (cond ((string= name "created")
                   (%set-key out "created" (or (parse-integer value :junk-allowed t) :null)))
                  ((string= name "expires")
                   (%set-key out "expires" (or (parse-integer value :junk-allowed t) :null)))
                  ((string= name "keyid") (%set-key out "keyId" value))
                  ((string= name "alg") (%set-key out "algorithm" value))
                  ((string= name "nonce") (%set-key out "nonce" value)))))))
    out))

(defclass ucp-verifier ()
  ((options :initarg :options :reader ucp-verifier-options)
   (seen :initform (make-hash-table :test #'equal) :reader %ucp-verifier-seen)
   (lock :initform (sb-thread:make-mutex :name "ax-ucp-verifier") :reader %ucp-verifier-lock))
  (:documentation
   "Verifies UCP response signatures, holding the replay set for this process."))

(defun make-ucp-verifier (&key required max-age-seconds (clock-tolerance-seconds 60)
                               replay-protection now)
  "A verifier. NOW is a thunk of Unix seconds, so the clock is the caller's."
  (make-instance 'ucp-verifier
                 :options (object "required" (json-boolean required)
                                  "maxAgeSeconds" (or max-age-seconds :null)
                                  "clockToleranceSeconds" clock-tolerance-seconds
                                  "replayProtection" (json-boolean replay-protection)
                                  "now" (or now :null))))

(defun %ucp-verifier-now (verifier)
  (let ((now (jget (ucp-verifier-options verifier) "now")))
    (if (functionp now)
        (funcall now)
        (- (floor (get-universal-time))
           (encode-universal-time 0 0 0 1 1 1970 0)))))

(defun ucp-verify-response (verifier &key headers body signing-keys refresh-signing-keys
                                          verify)
  "Verify one UCP response. True when accepted, or signals UCP-SIGNATURE-ERROR.

Core decides presence, expiry, clock skew, a future created time, maximum
age, mandatory @status coverage, mandatory body digest coverage and replay;
this function parses the headers, checks the digest, finds the key, refreshes
the key set once when the key id is unknown, and calls VERIFY for the crypto.

VERIFY receives (signature-base-octets signature-octets key) and answers
whether the signature is good. Keeping it injected is what lets this file
avoid implementing a signature algorithm."
  (let* ((input-header (%ucp-header (%mcp-object-or-empty headers) "signature-input"))
         (signature-header (%ucp-header (%mcp-object-or-empty headers) "signature"))
         (present (and (stringp input-header) (stringp signature-header)))
         (parsed (if present (%ucp-parse-signature-input input-header) (object)))
         (has-body (and (stringp body) (plusp (length body))))
         (nonce (jget parsed "nonce" :null))
         ;; Scoped by key id, as the reference's replay set is. A nonce is
         ;; only unique within the issuer that minted it, so two distinct
         ;; keys legitimately using the same nonce value must not collide --
         ;; a bare nonce key would have let one signer lock out another.
         ;; Falls back to the signature value when there is no nonce, which
         ;; is also what the reference does.
         (replay-key (let ((scope (%mcp-text (jget parsed "keyId" ""))))
                       (cond ((stringp nonce)
                              (format nil "~a~cnonce:~a" scope #\Newline nonce))
                             (present
                              (format nil "~a~csig:~a" scope #\Newline signature-header))
                             (t nil))))
         ;; Read only to let Core reject an already-known replay early. It is
         ;; NOT what decides acceptance: see the atomic claim below.
         (seen (and replay-key
                    (sb-thread:with-mutex ((%ucp-verifier-lock verifier))
                      (gethash replay-key (%ucp-verifier-seen verifier)))))
         (policy-input (axllm/core::core-map-merge
                        (object "present" (json-boolean present)
                                "hasBody" (json-boolean has-body)
                                "seen" (json-boolean seen))
                        parsed))
         (verdict (axllm/core::ucp-verify-signature-policy
                   policy-input (%ucp-verifier-now verifier)
                   (ucp-verifier-options verifier))))
    (unless (axllm/core::core-true-p (jget verdict "ok"))
      (%ucp-fail (%mcp-text (jget verdict "code")) "~a" (%mcp-text (jget verdict "message"))))
    ;; Core said an absent signature is acceptable here, so there is nothing
    ;; to verify and nothing to record.
    (when (equal (%mcp-text (jget verdict "code")) "absent")
      (return-from ucp-verify-response t))
    ;; The two headers are keyed by label, and a Signature under a
    ;; different label is not the signature for this Signature-Input. Taking
    ;; it anyway would verify a base that was never signed by that value,
    ;; and on a multi-signature response it would pair the wrong two.
    (let ((input-label (%ucp-signature-label input-header))
          (signature-label (%ucp-signature-label signature-header)))
      (unless (and input-label signature-label (string= input-label signature-label))
        (%ucp-fail "signature_invalid"
                   "UCP Signature label ~s does not match Signature-Input label ~s"
                   signature-label input-label)))
    (when has-body
      (ucp-verify-content-digest body (%ucp-header (%mcp-object-or-empty headers)
                                                   "content-digest")))
    (let* ((key-id (%mcp-text (jget parsed "keyId")))
           (keys (%mcp-object-or-empty (object)))
           (key (%ucp-find-key signing-keys key-id)))
      (declare (ignore keys))
      (when (and (null key) (functionp refresh-signing-keys))
        ;; One refresh, not a retry loop: an unknown key id is the signal
        ;; that the JWKS rotated, and a second miss is a real failure.
        (setf key (%ucp-find-key (funcall refresh-signing-keys) key-id)))
      (unless key
        (%ucp-fail "key_not_found" "UCP signing key ~a was not found" key-id))
      (unless (functionp verify)
        (%ucp-fail "signature_invalid" "UCP verification needs a verify function"))
      (let* ((components (jget parsed "components"))
             (base (axllm/core::ucp-signature-base
                    components
                    (%ucp-verification-values headers body)
                    (%ucp-params-of input-header)))
             (signature (%ucp-decode-signature signature-header)))
        (unless (funcall verify (%mcp-utf8 base) signature key)
          (%ucp-fail "signature_invalid" "UCP response signature did not verify"))
        ;; Acceptance is this claim, not the earlier read. Reading "seen"
        ;; before the crypto and writing it afterwards under a second lock
        ;; leaves a window in which two concurrent identical valid requests
        ;; both read NIL, both verify, and both are accepted -- which is
        ;; exactly the replay the option exists to prevent. One
        ;; test-and-set under one acquisition makes exactly one of them the
        ;; winner. The crypto is pure, so the loser having computed it too
        ;; costs nothing but is not allowed to count.
        (when (and replay-key
                   (axllm/core::core-true-p
                    (jget (ucp-verifier-options verifier) "replayProtection")))
          (unless (%ucp-claim-replay-key verifier replay-key)
            (%ucp-fail "signature_replayed" "UCP response signature was replayed")))
        t))))

(defun %ucp-claim-replay-key (verifier key)
  "Claim KEY for this verification. True only for the first claimer.

One mutex acquisition covering both the test and the set, so two threads
cannot both conclude the key was unseen."
  (sb-thread:with-mutex ((%ucp-verifier-lock verifier))
    (if (gethash key (%ucp-verifier-seen verifier))
        nil
        (progn (setf (gethash key (%ucp-verifier-seen verifier)) t) t))))

(defun %ucp-find-key (keys key-id)
  "The JWK in KEYS whose kid is KEY-ID, or NIL."
  (when (and keys (stringp key-id))
    (loop for key across (if (%array-p keys) keys (coerce keys 'vector))
          when (equal (%mcp-text (jget key "kid")) key-id)
            do (return key))))

(defun %ucp-params-of (input-header)
  "The @signature-params value inside a Signature-Input header."
  (let ((equals (position #\= input-header)))
    (if equals (subseq input-header (1+ equals)) input-header)))

(defun %ucp-signature-label (header)
  "The label a Signature or Signature-Input header is keyed by."
  (let ((equals (position #\= header)))
    (if equals (string-trim " " (subseq header 0 equals)) nil)))

(defun %ucp-decode-signature (header)
  "The raw signature bytes from a Signature header's byte-sequence form."
  (let* ((equals (position #\= header))
         (rest (if equals (subseq header (1+ equals)) header))
         (trimmed (string-trim ": " rest)))
    (handler-case (cl-base64:base64-string-to-usb8-array trimmed)
      (error () (%ucp-fail "signature_invalid" "UCP signature is not valid base64")))))

(defun %ucp-verification-values (headers body)
  "The component values a verifier rebuilds the base from.

@status and the derived request components are supplied by the caller in
HEADERS under their component names, because a response's covered components
include things no header carries."
  (declare (ignore body))
  (let ((values (object)))
    (dolist (key (%object-keys (%mcp-object-or-empty headers)))
      (%set-key values (string-downcase key) (gethash key (%mcp-object-or-empty headers))))
    values))


;;; ------------------------------------------------------------------
;;; ES256 over a JWK
;;; ------------------------------------------------------------------
;;;
;;; The one algorithm this file implements end to end, because ES256 is what
;;; the reference's allowedAlgorithms admits and a caller should not have to
;;; assemble a P-256 point to use it. Even so the primitives are ironclad's:
;;; SHA-256 and secp256r1 ECDSA. Nothing here does field arithmetic.
;;;
;;; JWS ES256 is raw r || s, 64 bytes, not a DER SEQUENCE. Ironclad's
;;; secp256r1 signatures are the same raw concatenation, so no re-encoding
;;; is needed -- but a DER signature from another stack would be silently
;;; the wrong length, which is why the length is checked rather than assumed.

(defun %ucp-base64url-decode (text)
  "Decode base64url without padding, as a JWK member is encoded."
  (unless (stringp text)
    (%ucp-fail "key_not_found" "UCP JWK member is not a string"))
  (let* ((padded (concatenate 'string
                              (substitute #\/ #\_ (substitute #\+ #\- text))
                              (make-string (mod (- 4 (mod (length text) 4)) 4)
                                           :initial-element #\=))))
    (handler-case (cl-base64:base64-string-to-usb8-array padded)
      (error () (%ucp-fail "key_not_found" "UCP JWK member is not valid base64url")))))

(defun ucp-jwk-public-key (jwk)
  "An ironclad P-256 public key from JWK, or a signalled key_not_found.

Only EC P-256 is accepted. A JWK naming another curve or key type is
refused rather than coerced, because quietly treating a P-384 key as P-256
would fail verification for a reason nothing reports."
  (let ((kty (%mcp-text (jget jwk "kty" "")))
        (crv (%mcp-text (jget jwk "crv" ""))))
    (unless (string= kty "EC")
      (%ucp-fail "algorithm_unsupported" "UCP ES256 needs an EC JWK, not ~a" kty))
    (unless (string= crv "P-256")
      (%ucp-fail "algorithm_unsupported" "UCP ES256 needs curve P-256, not ~a" crv))
    (let ((x (%ucp-base64url-decode (jget jwk "x")))
          (y (%ucp-base64url-decode (jget jwk "y"))))
      (unless (and (= (length x) 32) (= (length y) 32))
        (%ucp-fail "key_not_found"
                   "UCP P-256 coordinates must be 32 bytes each, got ~a and ~a"
                   (length x) (length y)))
      ;; The uncompressed point form ironclad's :y expects: 0x04 || X || Y.
      (let ((point (make-array 65 :element-type '(unsigned-byte 8))))
        (setf (aref point 0) 4)
        (replace point x :start1 1)
        (replace point y :start1 33)
        (handler-case (ironclad:make-public-key :secp256r1 :y point)
          (error () (%ucp-fail "key_not_found" "UCP P-256 point is not on the curve")))))))

(defun ucp-es256-verifier ()
  "A verify function for UCP-VERIFY-RESPONSE that checks ES256 over a JWK.

Answers NIL for a bad signature rather than signalling, because the caller
reports signature_invalid; it signals only when the KEY itself cannot be
used, which is a different failure with a different code."
  (lambda (signature-base signature key)
    (let ((public (ucp-jwk-public-key key)))
      (unless (= (length signature) 64)
        ;; A DER-encoded ECDSA signature lands here. Saying so is more
        ;; useful than a bare verification failure.
        (%ucp-fail "signature_invalid"
                   "UCP ES256 signature must be 64 raw bytes of r||s, got ~a"
                   (length signature)))
      (handler-case
          (ironclad:verify-signature
           public (ironclad:digest-sequence :sha256 signature-base) signature)
        (error () nil)))))

(defun ucp-es256-signer (private-key)
  "A signer over ES256 with an ironclad secp256r1 PRIVATE-KEY.

Provided so the signing and verifying halves can be exercised against each
other with a real key pair. Key generation and storage stay the caller's."
  (lambda (signature-base context)
    (declare (ignore context))
    (ironclad:sign-message private-key
                           (ironclad:digest-sequence :sha256 signature-base))))

(defun ucp-jwk-from-public-key (public-key &key kid (algorithm "ES256"))
  "PUBLIC-KEY as an EC P-256 JWK, for publishing or for a test's key set."
  (let* ((parts (ironclad:destructure-public-key public-key))
         (point (getf parts :y)))
    (unless (and point (= (length point) 65) (eql (aref point 0) 4))
      (%ucp-fail "key_not_found" "UCP JWK export needs an uncompressed P-256 point"))
    (flet ((encode (octets)
             (string-right-trim
              "=" (substitute #\_ #\/ (substitute #\- #\+
                                                  (cl-base64:usb8-array-to-base64-string
                                                   octets))))))
      (object "kty" "EC" "crv" "P-256"
              "x" (encode (subseq point 1 33))
              "y" (encode (subseq point 33 65))
              "alg" algorithm
              "kid" (or kid "key-1")))))

(export '(ucp-signature-error ucp-signature-error-code ucp-signature-error-message
          ucp-content-digest ucp-verify-content-digest ucp-hmac-signer
          ucp-sign-request ucp-request-signature-base
          ucp-verifier make-ucp-verifier ucp-verify-response
          ucp-jwk-public-key ucp-es256-verifier ucp-es256-signer
          ucp-jwk-from-public-key))
