;;;; ucp-signing-conformance.lisp --- UCP HTTP Message Signatures.
;;;;
;;;; Vectors here are derived by hand from src/ax/ucp/signing.ts and written
;;;; out literally, not captured from this implementation's output. A test
;;;; whose expectation came from the code under test reproduces that code's
;;;; bugs, and a signature base is precisely the artefact two independent
;;;; implementations must agree on byte for byte, so it is the one place
;;;; that matters most.
;;;;
;;;; The portable half is Core's (UCP-SIGNATURE-COMPONENTS, -PARAMS, -BASE,
;;;; -HEADERS, UCP-VERIFY-SIGNATURE-POLICY). These checks exercise it
;;;; through the native entry points, so a Core regression and a native
;;;; regression both surface here.

(in-package #:axllm)

(export '(run-ucp-signing-tests run-ucp-signing-tests-or-die))

(define-condition ucp-check-failure (error)
  ((detail :initarg :detail :reader ucp-check-failure-detail))
  (:report (lambda (condition stream)
             (write-string (ucp-check-failure-detail condition) stream))))

(defun %ucp-check-fail (format-control &rest arguments)
  (error 'ucp-check-failure :detail (apply #'format nil format-control arguments)))

(defun %ucp-same (actual expected label)
  (unless (equal actual expected)
    (%ucp-check-fail "~a mismatch~%    expected: ~s~%    actual:   ~s" label expected actual))
  t)

(defun %ucp-true (value label)
  (unless value (%ucp-check-fail "~a was not true" label))
  t)

(defmacro %ucp-code (expected label &body body)
  "Assert BODY signals UCP-SIGNATURE-ERROR with code EXPECTED."
  `(let ((code (handler-case (progn ,@body :%no-error)
                 (ucp-signature-error (condition) (ucp-signature-error-code condition)))))
     (when (eq code :%no-error)
       (%ucp-check-fail "~a was accepted; expected ~s" ,label ,expected))
     (%ucp-same code ,expected ,label)))

;;; The reference's own worked example, written out by hand.
(defparameter +ucp-vector-url+ "https://api.example.com/v1/items?page=2")
(defparameter +ucp-vector-base+
  (format nil "~{~a~^~c~}"
          (list "\"@method\": GET" #\Newline
                "\"@authority\": api.example.com" #\Newline
                "\"@path\": /v1/items" #\Newline
                "\"@query\": ?page=2" #\Newline
                "\"@signature-params\": (\"@method\" \"@authority\" \"@path\" \"@query\");created=1700000000;keyid=\"k1\""))
  "Hand-derived from axSignUCPRequest: @method, @authority, @path always;
@query because url.search is non-empty; no ucp-agent, no idempotency-key and
no body components; params as ({list});created=N;keyid=\"K\".")

(defun %ucp-at (seconds) (lambda () seconds))

(defun run-ucp-signing-tests (&key (stream *standard-output*))
  "Check UCP signing and verification. Returns (values passed failed)."
  (let ((passed 0) (failed 0))
    (flet ((check (label thunk)
             (handler-case (progn (funcall thunk) (incf passed))
               (error (condition)
                 (incf failed)
                 (format stream "~&  FAIL ~a~%    ~a~%" label condition)))))

      (check "the signature base matches the hand-derived reference vector"
             (lambda ()
               (%ucp-same (ucp-request-signature-base
                           +ucp-vector-url+ "GET" (object) nil
                           :key-id "k1" :created (%ucp-at 1700000000))
                          +ucp-vector-base+ "signature base")
               ;; Method case is normalised, as the reference uppercases it.
               (%ucp-same (ucp-request-signature-base
                           +ucp-vector-url+ "get" (object) nil
                           :key-id "k1" :created (%ucp-at 1700000000))
                          +ucp-vector-base+ "signature base from a lowercase method")))

      (check "a default port is suppressed per scheme, not per number"
             (lambda ()
               ;; The review case. URL.host suppresses only the scheme's own
               ;; default, so :80 is non-default on https and :443 is
               ;; non-default on http. A scheme-blind check signs an
               ;; authority the verifier will not rebuild.
               (flet ((authority (url)
                        (let* ((base (ucp-request-signature-base
                                      url "GET" (object) nil
                                      :key-id "k1" :created (%ucp-at 1700000000)))
                               (start (search "\"@authority\": " base))
                               (from (+ start (length "\"@authority\": ")))
                               (end (position #\Newline base :start from)))
                          (subseq base from end))))
                 (%ucp-same (authority "https://api.example.com/v1") "api.example.com"
                            "https with no port")
                 (%ucp-same (authority "https://api.example.com:443/v1") "api.example.com"
                            "https on its own default port")
                 (%ucp-same (authority "https://api.example.com:80/v1") "api.example.com:80"
                            "https on port 80 is NOT default")
                 (%ucp-same (authority "http://api.example.com:80/v1") "api.example.com"
                            "http on its own default port")
                 (%ucp-same (authority "http://api.example.com:443/v1") "api.example.com:443"
                            "http on port 443 is NOT default")
                 (%ucp-same (authority "https://api.example.com:8443/v1") "api.example.com:8443"
                            "a non-default port is kept"))))

      (check "component selection follows the reference's conditions"
             (lambda ()
               (flet ((covered (base name)
                        (and (search (format nil "\"~a\": " name) base) t)))
                 ;; No body: no digest, no content-type.
                 (let ((base (ucp-request-signature-base
                              "https://api.example.com/v1" "POST" (object) nil
                              :key-id "k1" :created (%ucp-at 1700000000))))
                   (%ucp-true (not (covered base "content-digest")) "bodyless digest absent")
                   (%ucp-true (not (covered base "@query")) "queryless @query absent"))
                 ;; A body adds both, and the digest is RFC 9530.
                 (let ((base (ucp-request-signature-base
                              "https://api.example.com/v1" "POST" (object) "{\"a\":1}"
                              :key-id "k1" :created (%ucp-at 1700000000))))
                   (%ucp-true (covered base "content-digest") "body digest covered")
                   (%ucp-true (covered base "content-type") "body content-type covered")
                   (%ucp-true (search "sha-256=:" base) "digest is sha-256"))
                 ;; These two are covered only when the header is present.
                 (let ((base (ucp-request-signature-base
                              "https://api.example.com/v1" "GET"
                              (object "UCP-Agent" "agent/1" "Idempotency-Key" "idem-1") nil
                              :key-id "k1" :created (%ucp-at 1700000000))))
                   (%ucp-true (covered base "ucp-agent") "ucp-agent covered when present")
                   (%ucp-true (covered base "idempotency-key")
                              "idempotency-key covered when present")))))

      (check "the content digest is the published SHA-256 of the body"
             (lambda ()
               ;; Independent vector: SHA-256 of {"a":1} base64-encoded,
               ;; wrapped in the structured-field byte-sequence colons.
               (%ucp-same (ucp-content-digest "{\"a\":1}")
                          "sha-256=:AVq9f1zFei3ZS3WQ8ErYCEJzkF7jPsXOvq5iJ2qX+GI=:"
                          "content digest")
               (ucp-verify-content-digest "{\"a\":1}" (ucp-content-digest "{\"a\":1}"))
               ;; A body that does not match its header is a digest_mismatch,
               ;; not a signature failure, so the caller can tell them apart.
               (%ucp-code "digest_mismatch" "a tampered body"
                 (ucp-verify-content-digest "{\"a\":2}" (ucp-content-digest "{\"a\":1}")))
               (%ucp-code "digest_mismatch" "a body with no digest header"
                 (ucp-verify-content-digest "{\"a\":1}" :null))))

      (check "a signed request carries both headers and signs exactly the base"
             (lambda ()
               (let* ((signer (ucp-hmac-signer "secret"))
                      (headers (ucp-sign-request
                                +ucp-vector-url+ "GET" (object) nil
                                :key-id "k1" :created (%ucp-at 1700000000) :sign signer))
                      (mac (ironclad:make-mac :hmac (%mcp-utf8 "secret") :sha256)))
                 (%ucp-same (jget headers "Signature-Input")
                            "sig1=(\"@method\" \"@authority\" \"@path\" \"@query\");created=1700000000;keyid=\"k1\""
                            "Signature-Input")
                 ;; The signature is the MAC of the hand-derived base, computed
                 ;; here rather than taken from the signer, so a change to the
                 ;; base cannot pass by also changing the signature.
                 (ironclad:update-mac mac (%mcp-utf8 +ucp-vector-base+))
                 (%ucp-same (jget headers "Signature")
                            (format nil "sig1=:~a:"
                                    (cl-base64:usb8-array-to-base64-string
                                     (ironclad:produce-mac mac)))
                            "Signature"))))

      (check "alg, nonce and a quoted key id appear in the documented order"
             (lambda ()
               (multiple-value-bind (base params)
                   (ucp-request-signature-base
                    "https://api.example.com/v1" "GET" (object) nil
                    :key-id "key\"with\"quotes" :algorithm "ES256"
                    :created (%ucp-at 1700000000) :nonce (lambda () "n-1"))
                 (declare (ignore base))
                 (%ucp-same params
                            "(\"@method\" \"@authority\" \"@path\");created=1700000000;keyid=\"key\\\"with\\\"quotes\";alg=\"ES256\";nonce=\"n-1\""
                            "params with alg and nonce"))))

      (check "signing refuses to proceed without a key id or a signer"
             (lambda ()
               (%ucp-code "key_not_found" "signing with no key id"
                 (ucp-sign-request "https://api.example.com/v1" "GET" (object) nil
                                   :sign (ucp-hmac-signer "s")))
               (%ucp-code "signature_invalid" "signing with no signer"
                 (ucp-sign-request "https://api.example.com/v1" "GET" (object) nil
                                   :key-id "k1"))
               ;; An explicitly requested component with no value must fail
               ;; rather than be signed as empty, because the verifier would
               ;; rebuild a different base.
               (%ucp-code "signature_invalid" "a component with no value"
                 (ucp-request-signature-base
                  "https://api.example.com/v1" "GET" (object) nil
                  :key-id "k1" :created (%ucp-at 1700000000)
                  :components '("@method" "request-id")))))

      (check "the verifier's pre-crypto policy returns the reference's codes"
             (lambda ()
               (let ((verifier (make-ucp-verifier :required t :now (%ucp-at 1700000000))))
                 (%ucp-code "signature_missing" "a required but absent signature"
                   (ucp-verify-response verifier :headers (object)))
                 (%ucp-code "signature_expired" "an expired signature"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1600000000;expires=1600000100;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:")))
                 (%ucp-code "signature_invalid" "a created time in the future"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1900000000;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:")))
                 (%ucp-code "signature_invalid" "a signature not covering @status"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@method\");created=1700000000;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:"))))
               ;; An absent signature is fine when it is not required, and
               ;; must not be reported as verified either.
               (let ((verifier (make-ucp-verifier :now (%ucp-at 1700000000))))
                 (%ucp-true (ucp-verify-response verifier :headers (object))
                            "an optional absent signature"))
               ;; Expiry is inside the clock tolerance, so it is accepted up
               ;; to the boundary and refused past it.
               (let ((verifier (make-ucp-verifier :required t :clock-tolerance-seconds 30
                                                  :now (%ucp-at 1700000000))))
                 (%ucp-code "signature_expired" "expiry beyond the tolerance"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1699999000;expires=1699999960;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:")))
                 ;; 1699999980 + 30 = 1700000010 >= now, so still inside.
                 (%ucp-code "key_not_found" "expiry inside the tolerance reaches key lookup"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1699999000;expires=1699999980;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:"))))))

      (check "maxAgeSeconds rejects an old or undated signature"
             (lambda ()
               (let ((verifier (make-ucp-verifier :required t :max-age-seconds 60
                                                  :clock-tolerance-seconds 0
                                                  :now (%ucp-at 1700000000))))
                 (%ucp-code "signature_expired" "a signature older than maxAge"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1699999000;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:")))
                 (%ucp-code "signature_expired" "a signature with no created under maxAge"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input" "sig1=(\"@status\");keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:")))
                 ;; Inside maxAge, so it gets as far as the key lookup.
                 (%ucp-code "key_not_found" "a fresh signature reaches key lookup"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1699999970;keyid=\"k1\""
                                     "Signature" "sig1=:AAAA:"))))))

      (check "an unknown key id triggers exactly one JWKS refresh"
             (lambda ()
               (let* ((verifier (make-ucp-verifier :required t :now (%ucp-at 1700000000)))
                      (refreshes 0)
                      (headers (object "Signature-Input"
                                       "sig1=(\"@status\");created=1700000000;keyid=\"k2\""
                                       "Signature" "sig1=:AAAA:"
                                       "@status" "200")))
                 ;; The rotated key arrives only on refresh, and the refresh
                 ;; happens because the key id was unknown, not on every call.
                 (%ucp-true (ucp-verify-response
                             verifier :headers headers
                             :signing-keys (vector (object "kid" "k1"))
                             :refresh-signing-keys (lambda ()
                                                     (incf refreshes)
                                                     (vector (object "kid" "k2")))
                             :verify (lambda (base signature key)
                                       (declare (ignore base signature key))
                                       t))
                            "a rotated key verified after one refresh")
                 (%ucp-same refreshes 1 "refresh count")
                 ;; A key present up front must not trigger a refresh at all.
                 (%ucp-true (ucp-verify-response
                             verifier
                             :headers (object "Signature-Input"
                                              "sig1=(\"@status\");created=1700000000;keyid=\"k1\""
                                              "Signature" "sig1=:BBBB:"
                                              "@status" "200")
                             :signing-keys (vector (object "kid" "k1"))
                             :refresh-signing-keys (lambda () (incf refreshes) (%new-array))
                             :verify (lambda (base signature key)
                                       (declare (ignore base signature key))
                                       t))
                            "a known key verified")
                 (%ucp-same refreshes 1 "refresh count after a known key")
                 ;; Still unknown after the refresh is a real failure.
                 (%ucp-code "key_not_found" "a key absent even after refresh"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1700000000;keyid=\"k9\""
                                     "Signature" "sig1=:CCCC:"
                                     "@status" "200")
                    :signing-keys (vector (object "kid" "k1"))
                    :refresh-signing-keys (lambda () (vector (object "kid" "k2")))
                    :verify (lambda (base signature key)
                              (declare (ignore base signature key))
                              t))))))

      (check "a tampered signature is rejected and not recorded as seen"
             (lambda ()
               (let ((verifier (make-ucp-verifier :required t :replay-protection t
                                                  :now (%ucp-at 1700000000)))
                     (headers (object "Signature-Input"
                                      "sig1=(\"@status\");created=1700000000;keyid=\"k1\";nonce=\"n-1\""
                                      "Signature" "sig1=:AAAA:"
                                      "@status" "200")))
                 (%ucp-code "signature_invalid" "a signature the crypto rejects"
                   (ucp-verify-response verifier :headers headers
                                        :signing-keys (vector (object "kid" "k1"))
                                        :verify (lambda (base signature key)
                                                  (declare (ignore base signature key))
                                                  nil)))
                 ;; A rejected nonce must stay unclaimed: otherwise one bad
                 ;; signature would lock out the legitimate retry.
                 (%ucp-true (ucp-verify-response
                             verifier :headers headers
                             :signing-keys (vector (object "kid" "k1"))
                             :verify (lambda (base signature key)
                                       (declare (ignore base signature key))
                                       t))
                            "the legitimate retry after a rejected signature"))))

      (check "ES256 over a real P-256 key pair verifies, and a tamper does not"
             (lambda ()
               ;; The crypto half, end to end against a generated key pair:
               ;; sign with ironclad secp256r1, publish the public half as a
               ;; JWK, and verify through the same path a server response
               ;; would take. Primitives are ironclad's; what is proven here
               ;; is that this file assembles them correctly.
               (multiple-value-bind (private public) (ironclad:generate-key-pair :secp256r1)
                 (let* ((jwk (ucp-jwk-from-public-key public :kid "es256-1"))
                        (verifier (make-ucp-verifier :required t :now (%ucp-at 1700000000)))
                        (params "(\"@status\");created=1700000000;keyid=\"es256-1\";alg=\"ES256\"")
                        (base (format nil "\"@status\": 200~c\"@signature-params\": ~a"
                                      #\Newline params))
                        (signature (funcall (ucp-es256-signer private) (%mcp-utf8 base) nil)))
                   ;; A real ES256 signature is raw r||s, 64 bytes.
                   (%ucp-same (length signature) 64 "ES256 signature length")
                   (flet ((headers (sig)
                            (object "Signature-Input" (format nil "sig1=~a" params)
                                    "Signature"
                                    (format nil "sig1=:~a:"
                                            (cl-base64:usb8-array-to-base64-string sig))
                                    "@status" "200")))
                     (%ucp-true (ucp-verify-response
                                 verifier :headers (headers signature)
                                 :signing-keys (vector jwk)
                                 :verify (ucp-es256-verifier))
                                "a genuine ES256 signature")
                     ;; One flipped bit must fail, and as signature_invalid.
                     (let ((tampered (copy-seq signature)))
                       (setf (aref tampered 0) (logxor (aref tampered 0) 1))
                       (%ucp-code "signature_invalid" "a tampered ES256 signature"
                         (ucp-verify-response
                          (make-ucp-verifier :required t :now (%ucp-at 1700000000))
                          :headers (headers tampered)
                          :signing-keys (vector jwk)
                          :verify (ucp-es256-verifier))))
                     ;; A signature that is genuine but over a DIFFERENT base
                     ;; must fail: this is the check that would pass if the
                     ;; verifier rebuilt the base loosely.
                     (let ((other (funcall (ucp-es256-signer private)
                                           (%mcp-utf8 "\"@status\": 500") nil)))
                       (%ucp-code "signature_invalid" "a signature over another base"
                         (ucp-verify-response
                          (make-ucp-verifier :required t :now (%ucp-at 1700000000))
                          :headers (headers other)
                          :signing-keys (vector jwk)
                          :verify (ucp-es256-verifier))))
                     ;; A DER signature from another stack is the wrong
                     ;; length and is named as such.
                     (%ucp-code "signature_invalid" "a DER-encoded signature"
                       (ucp-verify-response
                        (make-ucp-verifier :required t :now (%ucp-at 1700000000))
                        :headers (headers (subseq signature 0 40))
                        :signing-keys (vector jwk)
                        :verify (ucp-es256-verifier))))))))

      (check "a JWK that is not EC P-256 is refused rather than coerced"
             (lambda ()
               (%ucp-code "algorithm_unsupported" "an RSA JWK"
                 (ucp-jwk-public-key (object "kty" "RSA" "n" "AQAB" "e" "AQAB")))
               (%ucp-code "algorithm_unsupported" "a P-384 JWK"
                 (ucp-jwk-public-key (object "kty" "EC" "crv" "P-384" "x" "AA" "y" "AA")))
               (%ucp-code "key_not_found" "coordinates of the wrong length"
                 (ucp-jwk-public-key (object "kty" "EC" "crv" "P-256" "x" "AA" "y" "AA")))
               (%ucp-code "key_not_found" "a coordinate that is not base64url"
                 (ucp-jwk-public-key (object "kty" "EC" "crv" "P-256"
                                             "x" "!!!!" "y" "!!!!")))
               ;; A valid key round-trips through the JWK form.
               (multiple-value-bind (private public)
                   (ironclad:generate-key-pair :secp256r1)
                 (declare (ignore private))
                 (%ucp-true (ucp-jwk-public-key
                             (ucp-jwk-from-public-key public :kid "rt"))
                            "a round-tripped P-256 JWK"))))

      (check "a replay nonce is scoped by key id"
             (lambda ()
               ;; The review case. A nonce is unique only within the issuer
               ;; that minted it, so two distinct keys using the same nonce
               ;; value are both legitimate; a bare-nonce replay key would
               ;; let the first signer lock out the second.
               (let ((verifier (make-ucp-verifier :required t :replay-protection t
                                                  :now (%ucp-at 1700000000)))
                     (keys (vector (object "kid" "k1") (object "kid" "k2"))))
                 (flet ((attempt (key-id)
                          (ucp-verify-response
                           verifier
                           :headers (object "Signature-Input"
                                            (format nil "sig1=(\"@status\");created=1700000000;keyid=\"~a\";nonce=\"shared\"" key-id)
                                            "Signature" "sig1=:AAAA:"
                                            "@status" "200")
                           :signing-keys keys
                           :verify (lambda (base signature key)
                                     (declare (ignore base signature key))
                                     t))))
                   (%ucp-true (attempt "k1") "the first key's nonce")
                   ;; Same nonce, different key: must be accepted.
                   (%ucp-true (attempt "k2") "a second key sharing that nonce")
                   ;; And each key's own nonce is still single-use.
                   (%ucp-code "signature_replayed" "the first key reusing its nonce"
                     (attempt "k1"))
                   (%ucp-code "signature_replayed" "the second key reusing its nonce"
                     (attempt "k2"))))))

      (check "a Signature label must match its Signature-Input label"
             (lambda ()
               ;; Pairing a Signature under one label with a Signature-Input
               ;; under another verifies a base that value never signed, and
               ;; on a multi-signature response it pairs the wrong two.
               (let ((verifier (make-ucp-verifier :required t :now (%ucp-at 1700000000))))
                 (%ucp-code "signature_invalid" "mismatched labels"
                   (ucp-verify-response
                    verifier
                    :headers (object "Signature-Input"
                                     "sig1=(\"@status\");created=1700000000;keyid=\"k1\""
                                     "Signature" "sig2=:AAAA:"
                                     "@status" "200")
                    :signing-keys (vector (object "kid" "k1"))
                    :verify (lambda (base signature key)
                              (declare (ignore base signature key))
                              t)))
                 ;; The same pair under a matching non-default label works.
                 (%ucp-true (ucp-verify-response
                             verifier
                             :headers (object "Signature-Input"
                                              "ucp1=(\"@status\");created=1700000000;keyid=\"k1\""
                                              "Signature" "ucp1=:AAAA:"
                                              "@status" "200")
                             :signing-keys (vector (object "kid" "k1"))
                             :verify (lambda (base signature key)
                                       (declare (ignore base signature key))
                                       t))
                            "matching non-default labels"))))

      (check "replay acceptance is atomic under concurrent identical requests"
             (lambda ()
               ;; The review case. The old shape read "seen" before the
               ;; crypto and wrote it afterwards under a second lock, so two
               ;; threads could both read NIL, both verify and both be
               ;; accepted. Exactly one must win.
               (let* ((verifier (make-ucp-verifier :required t :replay-protection t
                                                   :now (%ucp-at 1700000000)))
                      (headers (object "Signature-Input"
                                       "sig1=(\"@status\");created=1700000000;keyid=\"k1\";nonce=\"race-1\""
                                       "Signature" "sig1=:AAAA:"
                                       "@status" "200"))
                      (gate (sb-thread:make-semaphore))
                      (accepted 0) (replayed 0) (other 0)
                      (lock (sb-thread:make-mutex :name "ax-ucp-test-tally"))
                      (workers '()))
                 (loop repeat 8
                   do (push (sb-thread:make-thread
                          (lambda ()
                            (sb-thread:wait-on-semaphore gate)
                            (let ((outcome
                                    (handler-case
                                        (progn
                                          (ucp-verify-response
                                           verifier :headers headers
                                           :signing-keys (vector (object "kid" "k1"))
                                           :verify (lambda (base signature key)
                                                     (declare (ignore base signature key))
                                                     ;; Widen the window the
                                                     ;; old code left open.
                                                     (sleep 0.01)
                                                     t))
                                          :accepted)
                                      (ucp-signature-error (condition)
                                        (ucp-signature-error-code condition)))))
                              (sb-thread:with-mutex (lock)
                                (cond ((eq outcome :accepted) (incf accepted))
                                      ((equal outcome "signature_replayed") (incf replayed))
                                      (t (incf other))))))
                          :name "ax-ucp-replay-race")
                         workers))
                 (sb-thread:signal-semaphore gate 8)
                 (dolist (worker workers)
                   (sb-thread:join-thread worker :timeout 10 :default nil))
                 (%ucp-same accepted 1 "exactly one concurrent request was accepted")
                 (%ucp-same replayed 7 "the rest were reported as replays")
                 (%ucp-same other 0 "no request failed for another reason")))))

    (format stream "~&ucp signing: ~a passed, ~a failed~%" passed failed)
    (format stream "~&ucp signing: the portable half -- component selection, @signature-params, the signature base and the two header shapes -- is Core's, exercised here through the native entry points. Signature-base and digest expectations are hand-derived from src/ax/ucp/signing.ts, not captured from this implementation. Crypto is ironclad; the signing and verifying algorithms are injected, so what is proven is the message construction, the pre-crypto policy, key refresh and replay atomicity, and ES256 over a JWK end to end against a generated P-256 key pair, including a flipped bit, a genuine signature over a different base, and a DER-length signature. Not claimed: any algorithm other than ES256, and PEM or certificate key formats.~%")
    (values passed failed)))

(defun run-ucp-signing-tests-or-die ()
  (multiple-value-bind (passed failed) (run-ucp-signing-tests)
    (declare (ignore passed))
    (when (plusp failed)
      (error "UCP signing checks failed: ~a" failed))
    t))
