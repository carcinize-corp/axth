;;;; provider.lisp --- the Core-driven provider client.
;;;;
;;;; Reference: tools/axir/internal/axir/templates/python/pyAI.py's
;;;; ProviderOperationClient, and ir/axcore/provider.md.
;;;;
;;;; Every provider decision lives in Core and is read from the generated
;;;; functions, never restated here:
;;;;
;;;;   provider-resolve-descriptor            base URL, auth scheme, headers
;;;;   provider-resolve-operation-descriptor  path, method, body kind
;;;;   provider-chat-operation-path           the model-dependent path
;;;;   provider-build-chat-request            the whole request body
;;;;   provider-normalize-chat-response       the normalized response
;;;;   provider-normalize-stream-delta        one streamed chunk
;;;;   provider-build/normalize-embed         embeddings
;;;;   provider-build/normalize-transcribe    audio in
;;;;   provider-build/normalize-speak         audio out
;;;;   openai-normalize-error                 HTTP status and error bodies
;;;;   provider-resolve-features              capabilities for a model
;;;;   provider-estimate-cost                 cost
;;;;   provider-validate-chat-request         request validation
;;;;
;;;; That is why this client covers every profile in
;;;; ir/axcore/data/provider-descriptors.json rather than a hand-written pair:
;;;; the native layer contributes the HTTP call, the credential rules, the
;;;; multipart encoder and the binary-body handling, and nothing else. Adding a
;;;; provider is a Core change, not a change here.
;;;;
;;;; Credential rules, which are native and deliberately strict:
;;;; certificate verification is required, redirects are refused so a
;;;; credential is never replayed to another host, and no condition carries a
;;;; key, a header or a provider response body.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Profiles
;;; ------------------------------------------------------------------

(defun provider-profiles ()
  "Every provider profile Core knows, sorted.

Read from Core's own registry under \"supportedProfileIds\", so it cannot
drift from what Core can actually describe.  The registry object itself also
carries its version and a profile map, which are not profile names."
  (let* ((registry (axllm/core::provider-profile-registry))
         (ids (%present (jget registry "supportedProfileIds"))))
    (sort (coerce (if (and ids (vectorp ids) (not (stringp ids))) ids (vector)) 'list)
          #'string<)))

(defun supported-ai-models (&optional model-type)
  "Ax's model catalogue, optionally narrowed to MODEL-TYPE.

This is the model accessor, and it is deliberately not `provider-profiles':
that function lists the provider profiles Core describes, which are vendors and
endpoints, not models.  Counting profile names as models would overstate what
the catalogue knows.

MODEL-TYPE narrows the catalogue the way the reference does, for example
\"code\", \"audio\" or \"embeddings\".  The catalogue itself is Core's."
  (let ((options (%new-object)))
    (when (and model-type (not (%blankp model-type)))
      (%set-key options "type" (%trim model-type)))
    (axllm/core::provider-model-catalog options)))

(defun model-catalog-summary ()
  "A summary of Ax's model catalogue, as Core reports it."
  (axllm/core::provider-model-catalog-summary))

(defun model-info (profile model)
  "What the catalogue knows about MODEL under PROFILE, or :NULL.

This is the entry the expensive-model gate reads, so a caller can see in
advance whether a model will need confirmation."
  (axllm/core::provider-find-model-info
   (or (provider-profile-known-p profile) (%trim (or profile "")))
   (%trim (or model ""))
   :null))

(defun provider-profile-known-p (profile)
  (let ((normalized (axllm/core::provider-normalize-profile (%trim (or profile "")))))
    (and (stringp normalized)
         (plusp (hash-table-count (axllm/core::provider-descriptor normalized)))
         normalized)))

(defparameter +openai-env-profiles+
  '("openai" "openai-responses" "openai-compatible")
  "The profiles that may read OPENAI_BASE_URL and OPENAI_API_KEY.

Another provider's key never goes to OpenAI's host, and OpenAI's key never
goes to another provider, so this list is an allowlist rather than a default.")

(defparameter +profile-key-env+
  '(("anthropic" . "ANTHROPIC_API_KEY")
    ("google-gemini" . "GOOGLE_APIKEY")
    ("typesafe" . "TYPESAFE_APIKEY")
    ("groq" . "GROQ_APIKEY")
    ("deepseek" . "DEEPSEEK_APIKEY")
    ("deepseek-responses" . "DEEPSEEK_APIKEY")
    ("mistral" . "MISTRAL_APIKEY")
    ("cohere" . "COHERE_APIKEY")
    ("together" . "TOGETHER_APIKEY")
    ("grok" . "GROK_APIKEY"))
  "The environment variable each profile's key comes from, where it has one.")

(defun %profile-env-key (profile)
  (let ((entry (assoc profile +profile-key-env+ :test #'string=)))
    (cond (entry (let ((value (uiop:getenv (cdr entry))))
                   (and (not (%blankp value)) (%trim value))))
          ((member profile +openai-env-profiles+ :test #'string=)
           (let ((value (uiop:getenv "OPENAI_API_KEY")))
             (and (not (%blankp value)) (%trim value))))
          (t nil))))

;;; ------------------------------------------------------------------
;;; The client
;;; ------------------------------------------------------------------

(defclass provider-client ()
  ((profile :initarg :profile :reader provider-profile)
   (name :initarg :name :reader provider-name)
   (model :initarg :model :accessor provider-model)
   (embed-model :initarg :embed-model :accessor provider-embed-model)
   (api-key :initarg :api-key :reader %provider-api-key)
   (base-url :initarg :base-url :reader provider-base-url)
   (base-url-override :initarg :base-url-override :reader %provider-base-url-override)
   (api-version :initarg :api-version :reader provider-api-version)
   (descriptor :initarg :descriptor :reader provider-descriptor-of)
   (options :initarg :options :accessor %provider-options)
   (model-config :initarg :model-config :accessor provider-model-config)
   (timeout :initarg :timeout :reader provider-timeout)
   (transport :initarg :transport :reader provider-transport)
   (streaming-transport :initarg :streaming-transport :reader provider-streaming-transport)
   (credential-provider :initarg :credential-provider :reader provider-credential-provider)
   (last-chat-model :initform :null :accessor provider-last-chat-model)
   (last-embed-model :initform :null :accessor provider-last-embed-model)
   (last-model-config :initform :null :accessor provider-last-model-config)
   (context-cache-entries :initform (make-hash-table :test 'equal) :reader provider-cache-entries)
   ;; Values a credential provider supplied, kept only so redaction can remove
   ;; them. When a callback supplies the credential the API key slot is NIL, so
   ;; redacting on the key alone would scrub nothing at all.
   (supplied-credentials :initform '() :accessor %provider-supplied-credentials)
   (metrics :initform (default-metrics) :reader %provider-metrics))
  (:documentation
   "A provider client for any profile Core describes.

PROFILE names the profile, such as \"openai\", \"openai-responses\",
\"google-gemini\", \"anthropic\", \"meta\", \"azure-openai\", \"vertex-ai\",
\"groq\", \"deepseek\" or \"typesafe\"; `provider-profiles' lists them all.
Nothing about the profile's request or response shape is written here: the
client asks Core."))

(defmethod print-object ((client provider-client) stream)
  ;; Never print the API key.
  (print-unreadable-object (client stream :type t)
    (format stream "~a model=~a" (provider-profile client) (provider-model client))))

(defun provider (&key profile name model embed-model api-key base-url api-version
                      options model-config transport streaming-transport timeout
                      credential-provider)
  "Create a provider client for PROFILE.

MODEL is optional: when it is omitted the profile's own default model is used,
from Core's descriptor, so no stale default is written here.  A profile with no
default model refuses rather than inventing one.  API-KEY falls back to the profile's
environment variable, and only to OPENAI_API_KEY for OpenAI's own profiles.
TRANSPORT, when given, replaces the HTTP call with a function of
\(url headers json-body) returning (values body status).  The default performs
a real bounded HTTPS POST with certificate verification required and redirects
refused, so a credential is never sent to an unverified host or replayed to
another one."
  (let* ((requested (or profile "openai"))
         (resolved (or (provider-profile-known-p requested)
                       (provider-fail
                        :config
                        (format nil "Unknown provider profile '~a'. Known profiles: ~{~a~^, ~}."
                                requested (provider-profiles)))))
         (service-options (let ((merged (%new-object)))
                            (when (hash-table-p options)
                              (dolist (key (%object-keys options))
                                (%set-key merged key (gethash key options))))
                            (when api-version (%set-key merged "api_version" api-version))
                            merged))
         (descriptor (axllm/core::provider-resolve-descriptor resolved service-options))
         (timeout (cond ((null timeout) 60)
                        ((and (realp timeout) (plusp timeout)) timeout)
                        (t (provider-fail :config "provider: :timeout must be positive."
                                          :provider resolved))))
         (defaults (or (%present-object (jget descriptor "defaults") "defaults" resolved)
                       (%new-object)))
         (model (or (and (not (%blankp model)) (%trim model))
                    (%present-string (jget defaults "model"))
                    (provider-fail
                     :config
                     (format nil "provider: :model is required for profile '~a'; it has no ~
default model." resolved)
                     :provider resolved)))
         (embed-model (or (and (not (%blankp embed-model)) (%trim embed-model))
                          (%present-string (jget defaults "embedModel"))))
         (reads-openai-env (member resolved +openai-env-profiles+ :test #'string=))
         (base (or (and (not (%blankp base-url)) (%strip-trailing-slash (%trim base-url)))
                   (and reads-openai-env
                        (let ((value (uiop:getenv "OPENAI_BASE_URL")))
                          (and (not (%blankp value))
                               (%strip-trailing-slash (%trim value)))))
                   (%present-string (jget descriptor "baseUrl"))
                   (%present-string (jget descriptor "baseURL"))))
         (key (if (%blankp api-key) (%profile-env-key resolved) (%trim api-key))))
    (when (and transport (not (functionp transport)))
      (provider-fail :config "provider: :transport must be a function." :provider resolved))
    (when (and credential-provider (not (functionp credential-provider)))
      (provider-fail :config "provider: :credential-provider must be a function."
                     :provider resolved))
    ;; Core decides whether the profile needs a credential at all; a local
    ;; model served over loopback does not.
    (when (and (json-true-p (jget descriptor "authRequired"))
               (null key)
               (null credential-provider)
               (null transport))
      (provider-fail :auth (axllm/core::provider-missing-api-key-message resolved)
                     :provider resolved))
    (make-instance 'provider-client
                   :profile resolved
                   :name (or (and (not (%blankp name)) (%trim name)) resolved)
                   :model model
                   :embed-model embed-model
                   :api-key key
                   :base-url (%strip-trailing-slash (or base ""))
                   :base-url-override (and (not (%blankp base-url))
                                           (%strip-trailing-slash (%trim base-url)))
                   :api-version (or (%present-string (jget descriptor "apiVersion")) api-version)
                   :descriptor descriptor
                   :options service-options
                   :model-config (or model-config (%new-object))
                   :timeout timeout
                   :transport transport
                   :streaming-transport streaming-transport
                   :credential-provider credential-provider)))

;;; ------------------------------------------------------------------
;;; The public factory
;;;
;;; `ai' is the primary way to build a client and now builds the Core-driven
;;; one, so every profile Core describes is reachable from the public API
;;; rather than only through `provider'. Its lambda list is unchanged, so
;;; existing callers keep working:
;;;
;;;   NAME        the profile, under any alias Core knows ("claude" and
;;;               "gemini" resolve the same way they do everywhere else)
;;;   MODEL       optional; the profile's default is used when omitted, and a
;;;               profile with no default refuses rather than inventing one
;;;   API-KEY     falls back to the profile's own environment variable
;;;   BASE-URL    overrides the descriptor's
;;;   TRANSPORT   a function of (url headers json-body) -> (values body status)
;;;   TIMEOUT     bounds a single request, in seconds
;;;   MAX-TOKENS  the token cap, carried as model_config maxTokens so Core maps
;;;               it to each provider's own spelling instead of this file
;;;               knowing that max_completion_tokens and max_tokens differ
;;; ------------------------------------------------------------------

(defun ai (&key name model api-key base-url transport timeout max-tokens)
  "Create a provider client for any profile Core describes.

NAME is a profile such as \"openai\", \"anthropic\", \"google-gemini\",
\"openai-responses\" or \"groq\", under any alias Core knows;
`provider-profiles' lists them all.  MODEL is optional: when it is omitted the
profile's own default model is used, from Core's descriptor, so no stale default
is written here; a profile with no default refuses rather than inventing one.
MAX-TOKENS becomes model_config maxTokens, which Core maps to the selected
provider's own spelling.

Every provider decision -- the request body, the response shape, the base URL,
the auth scheme, the capabilities -- comes from Core, so this factory gains a
provider when Core does."
  (when (and max-tokens (not (and (integerp max-tokens) (plusp max-tokens))))
    (provider-fail :config "ai: :max-tokens must be a positive integer."))
  (let ((model-config (%new-object)))
    (when max-tokens (%set-key model-config "maxTokens" max-tokens))
    (provider :profile (cond ((null name) "openai")
                             ((stringp name) name)
                             ((symbolp name) (string-downcase (symbol-name name)))
                             (t (provider-fail
                                 :config "ai: :name must be a string or symbol.")))
              :model model
              :api-key api-key
              :base-url base-url
              :transport transport
              :timeout timeout
              :model-config model-config)))

;;; ------------------------------------------------------------------
;;; Request addressing, all from Core's descriptor
;;; ------------------------------------------------------------------

(defun %provider-operation (client operation)
  "CLIENT's descriptor for OPERATION, from its own operations map or Core's."
  (let ((operations (%present-object (jget (provider-descriptor-of client) "operations")
                                     "operations" (provider-profile client))))
    (or (and operations (%present-object (jget operations operation) operation
                                         (provider-profile client)))
        (axllm/core::provider-operation-descriptor (provider-profile client) operation))))

(defun %url-encode-component (text)
  (axllm/core::core-url-encode-component (or text "")))

(defun %provider-operation-path (client operation &optional model)
  "The path for OPERATION, with the model substituted and the API version
appended, exactly as Core composes it."
  (let* ((descriptor (%provider-operation client operation))
         (declared (or (%present-string (jget descriptor "path"))
                       (concatenate 'string "/" operation)))
         (path (axllm/core::provider-chat-operation-path
                (provider-profile client)
                (or model (provider-model client))
                operation declared)))
    (when model
      (setf path (%replace-all path "{model}" (%url-encode-component model))))
    (let ((version (provider-api-version client)))
      (when (and version (not (%blankp version)))
        (setf path (concatenate 'string path
                                (if (find #\? path) "&" "?")
                                "api-version=" (%url-encode-component version)))))
    path))

(defun %provider-operation-method (client operation)
  (let ((method (%present-string (jget (%provider-operation client operation) "method"))))
    (string-upcase (or method "POST"))))

(defun %provider-call-base-url (client options)
  "The base URL for this call.

A call's options can move a provider's base URL -- a Vertex beta selects
v1beta1 -- and Core resolves that; an explicit base URL still wins."
  (axllm/core::provider-require-api-url
   (provider-profile client)
   (if (%provider-base-url-override client)
       (axllm/core::core-map-merge (or options (object)) (object "base_url" (%provider-base-url-override client)))
       (or options (%provider-options client))))
  (let* ((descriptor-base (%strip-trailing-slash
                           (or (%present-string (jget (provider-descriptor-of client) "baseUrl"))
                               "")))
         (current (provider-base-url client)))
    (if (or (%provider-base-url-override client)
            (null options)
            (not (equal current descriptor-base)))
        current
        (let ((resolved (%strip-trailing-slash
                         (or (%present-string
                              (jget (axllm/core::provider-resolve-descriptor
                                     (provider-profile client) options)
                                    "baseUrl"))
                             ""))))
          (if (%blankp resolved) current resolved)))))

(defun %provider-headers (client &key (content-type "application/json"))
  "CLIENT's request headers, as an alist.

The auth scheme is the descriptor's, so a profile that authenticates with
x-api-key or an api-key header is not special-cased here."
  (let* ((descriptor (provider-descriptor-of client))
         (auth (%present-string (jget descriptor "auth")))
         (key (%provider-api-key client))
         (headers (list (cons "content-type" content-type))))
    (when (and (json-true-p (jget descriptor "authRequired")) (null key)
               (null (provider-credential-provider client)))
      (provider-fail :auth (axllm/core::provider-missing-api-key-message (provider-profile client))))
    (cond ((equal auth "bearer")
           (push (cons "Authorization" (concatenate 'string "Bearer " (or key ""))) headers))
          ((member auth '("anthropic_key" "x-api-key") :test #'equal)
           (push (cons "x-api-key" (or key "")) headers))
          ((equal auth "api_key_header")
           (push (cons (or (%present-string (jget descriptor "apiKeyHeader")) "api-key")
                       (or key ""))
                 headers)))
    (let ((extra (%present-object (jget descriptor "headers") "headers"
                                  (provider-profile client))))
      (when extra
        (dolist (name (%object-keys extra))
          (push (cons name (axllm/core::core-js-text (gethash name extra))) headers))))
    (nreverse headers)))

(defun %provider-credential-headers (client headers operation method url)
  "HEADERS with any credential the credential provider supplies.

The callback receives the profile, operation, method and URL, and must answer
a header object; anything else is a configuration failure rather than a
silently unauthenticated request."
  (let ((callback (provider-credential-provider client)))
    (if (null callback)
        headers
        (let ((fresh (funcall callback
                              (object "profile" (provider-profile client)
                                      "operation" operation
                                      "method" method
                                      "url" url))))
          (unless (hash-table-p fresh)
            (provider-fail :auth "credential-provider must return a header object."
                           :provider (provider-profile client)))
          (let ((merged (copy-alist headers)))
            (dolist (name (%object-keys fresh))
              (let ((value (axllm/core::core-js-text (gethash name fresh))))
                ;; Remember it for redaction. A bearer header carries the
                ;; credential inside a longer string, so the bare token is
                ;; recorded as well as the whole value.
                (dolist (candidate (list value (%bearer-token value)))
                  (when (and candidate (> (length candidate) 8)
                             (not (member candidate (%provider-supplied-credentials client)
                                          :test #'equal)))
                    (push candidate (%provider-supplied-credentials client))))
                (let ((existing (assoc name merged :test #'string-equal)))
                  (if existing
                      (setf (cdr existing) value)
                      (setf merged (append merged (list (cons name value))))))))
            merged)))))

;;; ------------------------------------------------------------------
;;; The HTTP call
;;; ------------------------------------------------------------------

(defun %provider-raise-status (client status body request options)
  "Raise the condition Core builds for a non-2xx response.

Core owns the mapping from a status and a provider error body to an error
class, a code and whether it is retryable, so this never invents one."
  (let ((error-object (axllm/core::openai-normalize-error
                       status
                       (or body :null)
                       (or request :null)
                       (or options :null))))
    (if (typep error-object 'condition)
        (error error-object)
        (provider-fail :status
                       (format nil "Provider '~a' returned HTTP ~a. The response body is not ~
included in this condition." (provider-name client) status)
                       :provider (provider-name client) :status status))))

(defvar *provider-retry-sleep* (lambda (milliseconds cancellation)
                               (if cancellation
                                   (progn (cancellation-wait cancellation (/ milliseconds 1000))
                                          (throw-if-cancelled cancellation))
                                   (sleep (/ milliseconds 1000)))))
(defvar *provider-retry-now* (lambda () (* 1000 (- (get-universal-time) 2208988800))))
(defvar *provider-retry-random* (lambda () (random 1d0)))
(defvar *provider-response-headers* nil)
(defvar *provider-verbose-sink* (lambda (text) (write-line text)))

(defun %provider-verbose-request (options url method headers payload)
  (when (core-truthy-p (jget options "verbose"))
    (let ((map (object)))
      (dolist (entry headers) (%set-key map (car entry) (cdr entry)))
      (funcall *provider-verbose-sink* (axllm/core::ai-verbose-request-log url method map payload)))))

(defun %http-header (headers name)
  (if (hash-table-p headers)
      (loop for key being the hash-keys of headers
            when (string-equal name key) return (gethash key headers))
      (cdr (assoc name headers :test (lambda (a b) (string-equal a (string b)))))))

(defun %provider-retrying (options thunk)
  (let ((config (axllm/core::resolve-stream-retry (or options (object))))
        (*provider-response-headers* nil))
    (loop for attempt from 0
          do (handler-case (return (multiple-value-call #'values (funcall thunk)))
               (provider-error (condition)
                 (let ((delay (axllm/core::request-retry-delay
                               config attempt
                               (object "status" (or (provider-error-status condition) :null)
                                       "network" (json-boolean (eq (provider-error-kind condition) :network))
                                       "retry_after" (or (%http-header *provider-response-headers* "retry-after") :null))
                               (funcall *provider-retry-now*) (funcall *provider-retry-random*))))
                   (when (eq delay :null) (error condition))
                   (funcall *provider-retry-sleep* delay (%call-cancellation options))))))))

(defun %provider-request-json (client operation payload &rest arguments &key options &allow-other-keys)
  (if (member operation '("transcribe" "speak") :test #'equal)
      (apply #'%provider-request-once client operation payload arguments)
      (%provider-retrying options (lambda () (apply #'%provider-request-once client operation payload arguments)))))

(defun %provider-request-once (client operation payload
                               &key model (method nil) (options nil) (body-kind "json") path binary-response accept)
  "Send PAYLOAD for OPERATION and answer the parsed response body.

A non-2xx answer becomes the condition Core builds for that status."
  (let* ((cancellation (%call-cancellation (%present options)))
         (method (or method (%provider-operation-method client operation)))
         (path (if (%blankp path) (%provider-operation-path client operation model) path))
         (url (if (or (search "https://" path) (search "http://" path)) path
                  (concatenate 'string (%provider-call-base-url client options) path)))
         (content-type (if (equal body-kind "multipart")
                           "multipart/form-data"
                           "application/json"))
         (headers (%provider-credential-headers
                   client (%provider-headers client :content-type content-type)
                   operation method url))
         (body (encode-json payload))
         (*provider-http-method* method)
         (transport (or (provider-transport client)
                        (make-default-transport (provider-timeout client) :method method
                                                :body-kind body-kind :binary-response binary-response))))
    (when accept (push (cons "Accept" accept) headers))
    (throw-if-cancelled cancellation (provider-name client))
    (%provider-verbose-request options url method headers payload)
    (multiple-value-bind (text status response-headers)
        (handler-case (funcall transport url headers body)
          (provider-error (condition) (error condition))
          (error (condition)
            (provider-fail :transport
                           (format nil "Transport failure: ~a"
                                   (substitute #\Space #\Newline (princ-to-string condition)))
                           :provider (provider-name client))))
      (throw-if-cancelled cancellation (provider-name client))
      (setf *provider-response-headers* response-headers)
      (unless (stringp text)
        (provider-fail :transport "Transport must return the response body as a string."
                       :provider (provider-name client)))
      (unless (or (null status) (integerp status))
        (provider-fail :transport "Transport must return the HTTP status as an integer."
                       :provider (provider-name client)))
      (let* ((status (or status 200))
             (parsed (handler-case (parse-json text) (error () :null))))
        (unless (<= 200 status 299)
          (%provider-raise-status client status parsed (object "url" url "json" payload) options))
        (when (core-truthy-p (jget options "verbose"))
          (funcall *provider-verbose-sink* (axllm/core::ai-verbose-response-log status parsed)))
        (if (and (or binary-response accept) (eq parsed :null))
            (values text (%http-header response-headers "content-type"))
            (values parsed (%http-header response-headers "content-type")))))))

;;; ------------------------------------------------------------------
;;; The service protocol
;;; ------------------------------------------------------------------

(defun %bearer-token (value)
  "The token inside an Authorization value, or NIL.

A credential provider usually answers \"Bearer <token>\", and a provider that
echoes the credential back echoes the token, not the whole header value."
  (when (stringp value)
    (let ((space (position #\Space value)))
      (when (and space (< (1+ space) (length value)))
        (subseq value (1+ space))))))

(defun %provider-secrets (client)
  "Every string that must never escape CLIENT, longest first.

Longest first matters: a bearer value contains its token, so scrubbing the
longer string first leaves no fragment of the shorter one behind."
  (sort (remove-duplicates
         (remove-if (lambda (secret) (or (null secret) (<= (length secret) 3)))
                    (cons (%provider-api-key client)
                          (copy-list (%provider-supplied-credentials client))))
         :test #'equal)
        #'> :key #'length))

(defun %scrub (value secrets)
  "VALUE with every secret removed, through strings nested in objects and arrays.

The printed message is not the only place a credential reaches a caller: a
provider that echoes the key back puts it in the response body, which the
condition carries in a slot. Scrubbing only the message would leave the
credential one accessor away."
  (cond
    ((stringp value)
     (let ((out value))
       (dolist (secret secrets out) (setf out (%replace-all out secret "[redacted]")))))
    ((hash-table-p value)
     (let ((copy (%new-object)))
       (dolist (key (%object-keys value))
         (%set-key copy key (%scrub (gethash key value) secrets)))
       copy))
    ((and (vectorp value) (not (stringp value)))
     (let ((copy (%new-array)))
       (map nil (lambda (item) (vector-push-extend (%scrub item secrets) copy)) value)
       copy))
    (t value)))

(defun %call-redacting (client thunk)
  "Run THUNK with CLIENT's credentials scrubbed from any provider failure.

The guard belongs here, at the service boundary that owns the credential, not
in `chat'.  `chat' is only one entry point: the generator calls `ax-chat'
directly, and a streamed or embedding failure never passes through `chat' at
all, so a guard there leaves every other public path exposed.

The secrets are read inside the handler rather than captured up front: a
credential provider supplies its value during the request, so a snapshot taken
before THUNK runs would be empty in exactly the case where the callback is the
only source of the credential.

Provider error text is preserved except for the credential, because a 5xx with
no detail is undiagnosable; redaction is not suppression."
  (handler-bind
      ((provider-error
         (lambda (condition)
           (let ((secrets (%provider-secrets client)))
             (when secrets
               (let* ((text (princ-to-string condition))
                      (clean (%scrub text secrets))
                      (body (provider-error-response-body condition))
                      (clean-body (%scrub body secrets))
                      (request (provider-error-request condition))
                      (clean-request (%scrub request secrets)))
                 ;; Re-raise whenever a secret is reachable, through the message
                 ;; or through the body slot a caller can read.
                 (unless (and (equal text clean)
                              (equal (encode-json body) (encode-json clean-body))
                              (equal (encode-json request) (encode-json clean-request)))
                   (error 'provider-error
                          :kind (provider-error-kind condition)
                          :provider (provider-error-provider condition)
                          :status (provider-error-status condition)
                          :code (provider-error-code condition)
                          :retryable (provider-error-retryable-p condition)
                          :response-body clean-body
                          :request clean-request
                          :message clean))))))))
    (funcall thunk)))

(defmacro redacting ((client) &body body)
  "BODY with CLIENT's credential scrubbed from any provider failure it raises."
  `(%call-redacting ,client (lambda () ,@body)))

(defmethod ax-service-name ((client provider-client)) (provider-name client))
(defmethod ax-id ((client provider-client)) (provider-profile client))

;;; The legacy accessor names, so a caller written against the native client --
;;; including the generator, which records (ai-name client) and (ai-model
;;; client) in its chat log and traces -- works unchanged against a Core-driven
;;; client. These are the same two questions under the older names, not a
;;; second stack.
(defmethod ai-name ((client provider-client)) (provider-name client))
(defmethod ai-model ((client provider-client)) (provider-model client))
(defmethod ai-base-url ((client provider-client)) (provider-base-url client))
(defmethod ai-timeout ((client provider-client)) (provider-timeout client))
(defmethod ai-transport ((client provider-client)) (provider-transport client))
(defmethod ai-streaming-transport ((client provider-client))
  (provider-streaming-transport client))
(defmethod ai-max-tokens ((client provider-client))
  "The token cap this client carries, from its model config.

Core owns the per-provider spelling on the wire; what the client holds is the
one portable value."
  (let ((value (%present (jget (provider-model-config client) "maxTokens"))))
    (if (integerp value) value :null)))
(defmethod ai-descriptor ((client provider-client)) (provider-descriptor-of client))
(defmethod ai-custom-transport-p ((client provider-client))
  (and (provider-transport client) t))
(defmethod ax-credential ((client provider-client))
  "The client's credential, for `chat''s redaction guard only.  It is never
written into a condition, a log line or a request this file builds."
  (%provider-api-key client))
(defmethod ax-metrics ((client provider-client)) (%provider-metrics client))
(defmethod ax-options ((client provider-client)) (%provider-options client))

(defmethod (setf ax-options) (options (client provider-client))
  (setf (%provider-options client) (or options (%new-object))))

(defmethod ax-features ((client provider-client) &optional model)
  (axllm/core::provider-resolve-features (provider-profile client)
                                         (or (%present model) (provider-model client))
                                         (%provider-options client)))

(defmethod ax-estimated-cost ((client provider-client) &optional model-usage)
  (let ((overrides (or (%present (jget (%provider-options client) "modelInfo"))
                       (%present (jget (%provider-options client) "model_info"))
                       :null)))
    (axllm/core::provider-estimate-cost (or model-usage (%new-object)) overrides)))

(defun %provider-merged-options (client options)
  (let ((merged (axllm/core::core-map-merge (%provider-options client)
                  (axllm/core::provider-normalize-call-options (or options :null)))))
    (%set-key merged "usageContext"
              (axllm/core::merge-usage-context (jget (%provider-options client) "usageContext")
                                               (jget options "usageContext")))
    (%set-key merged "customLabels"
              (merge-custom-labels (jget (%provider-options client) "customLabels") (jget options "customLabels")))
    merged))

(defun %provider-resolve-call (client request options &optional embed)
  (axllm/core::resolve-model-key
   (%provider-options client) request (or options :null)
   (if embed (or (provider-embed-model client) :null) (provider-model client))
   (json-boolean embed)))

(defun %provider-prepare-request (client request options)
  "REQUEST with the model and the merged model config Core expects.

Core merges the sampling configuration: the client's settings, then the
request's, then the call's, with the provider's own defaults underneath."
  (let ((prepared (%new-object)))
    (when (hash-table-p request)
      (dolist (key (%object-keys request))
        (%set-key prepared key (gethash key request))))
    (%set-key prepared "model"
              (or (%present-string (jget prepared "model")) (provider-model client)))
    ;; The expensive-model gate runs here, after the model is resolved and
    ;; before anything is merged, built, recorded or sent, which is where the
    ;; reference puts it. An expensive model has to be confirmed by the call's
    ;; useExpensiveModel option; refusing afterwards would already have billed
    ;; the request and recorded it in the run's usage and traces.
    (axllm/core::provider-require-expensive-model-confirmation
     (provider-profile client)
     (or (%present-string (jget prepared "model")) "")
     (or (%provider-options client) (%new-object))
     (or (%present options) (%new-object)))
    (%set-key prepared "model_config"
              (axllm/core::merge-model-config (provider-model-config client)
                                              (or (%present (jget prepared "model_config"))
                                                  (%present (jget prepared "modelConfig"))
                                                  :null)
                                              (%provider-merged-options client options)))
    prepared))

(defun %wire-tool-call-type-problem (raw)
  "The first raw tool call whose declared type is not \"function\", or NIL.

Core normalizes a tool call by setting type \"function\" unconditionally, which
is right for the shapes it knows but means a call the provider declared as
something else -- an OpenAI `custom' tool call, say -- would be rewritten into a
function call and run.  The raw response is the only place that type still
exists, so it is checked here, before Core sees it.

Covers the Chat Completions shape (choices[].message.tool_calls[]).  Other
dialects carry their call kind differently and are not inspected here; this is
a guard against a known rewrite, not a claim to validate every wire shape."
  (let ((choices (%present (jget raw "choices"))))
    (when (and choices (vectorp choices) (not (stringp choices)))
      (loop for choice across choices
            do (let* ((message (and (hash-table-p choice)
                                    (%present-object (jget choice "message") "message" nil)))
                      (calls (and message (%present (jget message "tool_calls")))))
                 (when (and calls (vectorp calls) (not (stringp calls)))
                   (loop for call across calls
                         for index from 0
                         do (when (and (hash-table-p call)
                                       (%ai-object-has-key call "type"))
                              (let ((type (gethash "type" call)))
                                (unless (or (eq type :null) (equal type "function"))
                                  (return-from %wire-tool-call-type-problem
                                    (format nil "Function call at index ~a in result 0 must have ~
type 'function', received: ~a"
                                            index (%ai-json-received call "type")))))))))))
    nil))

(defun %validate-normalized-calls (response provider)
  "Shape-check every call in a Core-normalized response before it escapes.

The reference's own rules, applied to Core's nested call shape.  Without this
the guard would exist only on the legacy path, and a malformed or
non-function call would reach the tool loop and run a handler the caller never
authorized -- which is the defect this port was asked to close."
  (let ((results (%present (jget response "results"))))
    (when (and results (vectorp results) (not (stringp results)))
      (loop for result across results
            do (let ((calls (and (hash-table-p result)
                                 (%present (jget result "function_calls")))))
                 (when (and calls (vectorp calls) (not (stringp calls)))
                   (loop for call across calls
                         for index from 0
                         do (%ai-check-tool-call-shape call :response index
                                                       :provider provider)))))))
  response)

(defun %provider-canonical-request (request)
  "Accept the public camelCase spelling at the native/Core boundary."
  (let ((out (axllm/core::core-map-merge request (object))))
    (dolist (pair '(("chatPrompt" . "chat_prompt")
                    ("modelConfig" . "model_config")
                    ("responseFormat" . "response_format")
                    ("functionCall" . "function_call")))
      (when (and (hash-table-p request) (nth-value 1 (gethash (car pair) request)))
        (%set-key out (cdr pair) (gethash (car pair) request))))
    out))

(defun %provider-hook-context (client options)
  (let* ((frame (get-runtime-hook-frame options))
         (globals (if (typep frame 'runtime-hook-frame) (runtime-hook-frame-globals frame) (globals-snapshot))))
    (%telemetry-merge globals (%provider-options client) options)))

(defun %provider-metric (meter name method value labels)
  (when (%present meter)
    (handler-case
        (let ((instrument (telemetry-call meter (if (equal method "add") "createCounter" "createHistogram") name (object))))
          (telemetry-call instrument method value labels))
      (error () nil))))

(defmethod ax-chat :around ((client provider-client) request &optional options)
  (let* ((request (%provider-canonical-request request))
         (context (%provider-hook-context client options))
         (merged (%provider-merged-options client options))
         (limiter (%present (jget context "rateLimiter")))
         (meter (jget context "meter"))
         (span (start-span-fail-open (jget context "tracer") "ax_llm_chat"))
         (started (get-internal-real-time))
         (labels (merge-custom-labels (jget context "customLabels") (jget merged "customLabels"))))
    (unwind-protect
         (let* ((invoke (lambda () (call-next-method client request options)))
                (response (if limiter
                              (funcall limiter invoke (object "operation" "chat" "provider" (provider-name client)
                                                              "model" (jget request "model" (provider-model client)) "streaming" false))
                              (funcall invoke)))
                (observer (%present (jget context "onUsage")))
                (event (axllm/core::build-usage-event "chat" response merged false)))
           (when (and observer (%present event))
             (handler-case (funcall observer event) (error () nil)))
           response)
      (%provider-metric meter "ax_llm_requests_total" "add" 1 labels)
      (%provider-metric meter "ax_llm_request_duration_ms" "record"
                        (* 1000d0 (/ (- (get-internal-real-time) started) internal-time-units-per-second))
                        (%telemetry-labels (object) labels))
      (span-call span "end"))))

(defmethod ax-stream :around ((client provider-client) request &optional options)
  (call-next-method client (%provider-canonical-request request) options))

(defun %provider-context-cache-chat (client request payload model options)
  "Execute Core's cache plan through the same authenticated retry boundary."
  (let ((cfg (jget options "contextCache" (jget options "context_cache"))))
    (unless (and (equal (provider-profile client) "google-gemini")
                 (core-truthy-p cfg)
                 (core-truthy-p (jget (jget (jget (provider-descriptor-of client) "features") "caching") "supported")))
      (return-from %provider-context-cache-chat nil))
    (unless (hash-table-p cfg) (setf cfg (object)))
    (let ((explicit (%present-string (jget cfg "name" (jget cfg "cacheName" (jget cfg "cache_name"))))))
      (when explicit
        (return-from %provider-context-cache-chat
          (%provider-request-json client "chat" (axllm/core::core-map-merge payload (object "cachedContent" explicit))
                                  :model model :options options))))
    (let* ((body (object)) (count 0) (seen 0)
           (contents (jget payload "contents" #()))
           (registry (%present (jget cfg "registry")))
           (namespace (jget cfg "namespace" "default")))
      (dolist (key '("systemInstruction" "tools" "toolConfig"))
        (when (gethash key payload) (%set-key body key (gethash key payload))))
      (loop for prompt across (jget request "chat_prompt" #())
            unless (equal (jget prompt "role") "system") do
              (incf seen) (when (json-true-p (jget prompt "cache")) (setf count seen)))
      (when (plusp count) (%set-key body "contents" (subseq contents 0 count)))
      (unless (or (gethash "systemInstruction" body) (gethash "contents" body))
        (return-from %provider-context-cache-chat nil))
      (let* ((encoded (axllm/core::core-json-stable-stringify body))
             (key (format nil "~a:~a:~a" (provider-profile client) model
                          (ironclad:byte-array-to-hex-string
                           (ironclad:digest-sequence :sha256 (sb-ext:string-to-octets encoded :external-format :utf-8)))))
             (existing (if registry (telemetry-call registry "get" namespace key)
                           (gethash key (provider-cache-entries client) :null)))
             (plan (axllm/core::ai-context-cache-plan true true "" existing (funcall *provider-retry-now*)
                      (* 1000 (jget cfg "refreshWindowSeconds" (jget cfg "refresh_window_seconds" 300)))
                      (json-boolean (>= (ceiling (length encoded) 4) (jget cfg "minTokens" (jget cfg "min_tokens" 2048))))))
             (action (jget plan "action")) (name (jget plan "cacheName" "")))
        (labels ((save (entry)
                   (if registry (telemetry-call registry "set" namespace key entry)
                       (setf (gethash key (provider-cache-entries client)) entry)))
                 (cache-op (operation)
                   (let* ((ops (axllm/core::ai-gemini-cache-ops name
                                (jget cfg "ttlSeconds" (jget cfg "ttl_seconds" 3600))
                                (or (%provider-api-key client) "") model body options))
                          (op (jget ops operation))
                          (base (%present-string (jget op "base_url")))
                          (response (%provider-request-json client "chat" (jget op "request")
                                     :path (if base (concatenate 'string base (jget op "path")) (jget op "path"))
                                     :method (jget op "method") :options options))
                          (expiry (jget response "expireTime" (jget response "expire_time")))
                          (millis (if (numberp expiry) expiry
                                      (handler-case (* 1000 (local-time:timestamp-to-unix (local-time:parse-timestring expiry)))
                                        (error () 0))))
                          (future (axllm/core::ai-context-cache-expiry millis (funcall *provider-retry-now*))))
                     (when (equal operation "create") (setf name (jget response "name" "")))
                     (unless (and (plusp (length name)) (plusp future))
                       (provider-fail :response "Gemini cache response omitted name or future expireTime"))
                     (save (object "cacheName" name "expiresAt" future)))))
          (when (equal action "none") (return-from %provider-context-cache-chat nil))
          (handler-case
              (cond ((equal action "create") (cache-op "create"))
                    ((equal action "refresh")
                     (handler-case (cache-op "update")
                       (provider-error (c)
                         (when (eq (provider-error-kind c) :aborted) (error c))
                         (cache-op "create")))))
            (provider-error (c)
              (when (eq (provider-error-kind c) :aborted) (error c))
              (return-from %provider-context-cache-chat nil)))
          (let ((cached (axllm/core::core-map-merge payload (object "cachedContent" name))))
            (dolist (field '("systemInstruction" "tools" "toolConfig")) (remhash field cached))
            (%set-key cached "contents" (subseq contents count))
            (handler-case (%provider-request-json client "chat" cached :model model :options options)
              (provider-error (c)
                (unless (core-truthy-p (axllm/core::ai-context-cache-rejection
                                       (or (provider-error-status c) 0) (provider-error-response-body c)))
                  (error c))
                (let* ((current (if registry (telemetry-call registry "get" namespace key)
                                    (gethash key (provider-cache-entries client) :null)))
                       (recovery (axllm/core::ai-context-cache-recovery current name (json-boolean registry))))
                  (when (core-truthy-p (jget recovery "invalidated"))
                    (if registry (save (jget recovery "externalEntry"))
                        (remhash key (provider-cache-entries client)))))
                nil))))))))

(defmethod ax-chat ((client provider-client) request &optional options)
  "One chat turn, with every provider decision taken by Core."
  (redacting (client)
    (axllm/core::validate-chat-request request)
    (let* ((resolved (%provider-resolve-call client request options))
           (request (jget resolved "request"))
           (options (jget resolved "options"))
           (prepared (%provider-prepare-request client request options))
           (options (%provider-merged-options client options))
           (model (%present-string (jget prepared "model"))))
      (axllm/core::provider-should-use-realtime (provider-profile client) model prepared options)
      (axllm/core::provider-validate-chat-request (provider-profile client) prepared
                                                  (or options :null))
      (let ((payload (axllm/core::provider-build-chat-request
                      (provider-profile client) prepared (or options :null))))
        (setf (provider-last-chat-model client) model
              (provider-last-model-config client) (jget prepared "model_config"))
        ;; A request that asked to stream is folded here, so a caller using chat
        ;; on a streaming request still gets one response.
        (if (core-truthy-p (jget payload "stream"))
            (axllm/core::fold-chat-response-stream
             (%collect-stream-chunks (ax-stream client request options)))
            (let ((raw (or (%provider-context-cache-chat client prepared payload model options)
                           (%provider-request-json client "chat" payload :model model :options options))))
              ;; The declared call kind is checked on the raw response, because
              ;; Core sets it to "function" and the original is gone afterwards.
              (let ((problem (%wire-tool-call-type-problem raw)))
                (when problem
                  (provider-fail :response problem :provider (provider-name client))))
              (%validate-normalized-calls
               (axllm/core::provider-normalize-chat-response
                (provider-profile client) raw (provider-name client) model
                (if (equal (provider-profile client) "typesafe")
                    (axllm/core::typesafe-response-context payload (or options :null))
                    payload))
               (provider-name client))))))))

(defun core-truthy-p (value)
  "VALUE's truthiness by Core's rule, as a Lisp boolean."
  (axllm/core::core-true-p value))

(defun %collect-stream-chunks (handle)
  "Every chunk HANDLE answers, as a Core list, closing it afterwards."
  (let ((out (%new-array)))
    (unwind-protect
         (loop for chunk = (ax-stream-next handle)
               until (eq chunk :null)
               do (vector-push-extend chunk out))
      (ax-stream-close handle))
    out))

(defmethod ax-complete ((client provider-client) request)
  (axllm/core::chat-response-to-completion (ax-chat client request)))

(defmethod ax-embed ((client provider-client) request &optional options)
  (redacting (client)
    (let* ((resolved (%provider-resolve-call client request options t))
           (request (jget resolved "request"))
           (options (%provider-merged-options client (jget resolved "options")))
           (model (or (%present-string (jget request "embed_model"))
                      (%present-string (jget request "embedModel"))
                      (%present-string (jget request "model"))
                      (provider-embed-model client)
                      (provider-model client)))
           (prepared (%new-object)))
      (when (hash-table-p request)
        (dolist (key (%object-keys request))
          (%set-key prepared key (gethash key request))))
      (%set-key prepared "embed_model" model)
      (let* ((payload (axllm/core::provider-build-embed-request
                       (provider-profile client) prepared (or options :null)))
             (raw (%provider-request-json client "embed" payload
                                          :model model :options options
                                          :path (%present-string
                                                 (axllm/core::provider-embed-url
                                                  (provider-profile client) model
                                                  (if (%provider-base-url-override client)
                                                      (axllm/core::core-map-merge options (object "base_url" (%provider-base-url-override client)))
                                                      options))))))
        (setf (provider-last-embed-model client) model)
        (axllm/core::provider-normalize-embed-response
         (provider-profile client) raw (provider-name client) model)))))

(defmethod ax-transcribe ((client provider-client) request &optional options)
  (redacting (client)
    (let* ((options (%provider-merged-options client options))
           (model (or (%present-string (jget request "model"))
                      (%present-string (jget (%provider-operation client "transcribe") "defaultModel"))
                      (provider-model client)))
           (payload (axllm/core::provider-build-transcribe-request
                     (provider-profile client) request))
           (kind (or (%present-string (jget (%provider-operation client "transcribe") "body"))
                     "json"))
           (path (%provider-operation-path client "transcribe" model))
           (query (%present (jget payload "query")))
           (event-stream (and (equal (provider-profile client) "meta")
                              (or (%present (jget request "partialMode" (jget request "partial_mode")))
                                  (core-truthy-p (jget request "emitAudioProgress" (jget request "emit_audio_progress"))))))
           (path (if query
                     (concatenate 'string path "?"
                                  (%string-join "&" (mapcar (lambda (key) (format nil "~a=~a" (%url-encode-component key)
                                                                                 (%url-encode-component (axllm/core::core-js-text (gethash key query)))))
                                                           (%object-keys query)))) path))
           (payload (progn (remhash "query" payload) payload))
           (raw (%provider-request-json client "transcribe" payload
                                        :model model :options options :body-kind kind :path path
                                        :accept (when event-stream "text/event-stream"))))
      (axllm/core::provider-normalize-transcribe-response
       (provider-profile client) (if (and event-stream (stringp raw))
                                    (object "events" (coerce (sse-events raw) 'vector)) raw) request))))

(defmethod ax-speak ((client provider-client) request &optional options)
  (redacting (client)
    (let* ((options (%provider-merged-options client options))
           (model (or (%present-string (jget request "model"))
                      (%present-string (jget (%provider-operation client "speak") "defaultModel"))
                      (provider-model client)))
           (payload (axllm/core::provider-build-speak-request
                     (provider-profile client) request)))
      (multiple-value-bind (raw content-type)
          (%provider-request-json client "speak" payload :model model :options options
                                  :binary-response (equal (jget (%provider-operation client "speak") "response") "binary"))
        (axllm/core::provider-normalize-speak-response
         (provider-profile client) raw request
         (or content-type (%present-string (jget request "content_type")) :null))))))

(defmethod ax-stream ((client provider-client) request &optional options)
  "Stream a chat turn.

Each event is handed to Core's `provider-normalize-stream-delta' with the
stream's own state object, so chunk semantics stay Core's."
  (redacting (client)
    (axllm/core::validate-chat-request request)
    (let* ((resolved (%provider-resolve-call client request options))
           (request (jget resolved "request"))
           (options (jget resolved "options"))
           (prepared (%provider-prepare-request client request options))
           (options (%provider-merged-options client options))
           (model (%present-string (jget prepared "model"))))
      (axllm/core::provider-validate-chat-request (provider-profile client) prepared
                                                  (or options :null))
      (let* ((payload (axllm/core::provider-build-chat-request
                       (provider-profile client) prepared (or options :null)))
             (cancellation (%call-cancellation options))
             (url (concatenate 'string (%provider-call-base-url client options)
                               (%provider-operation-path client "stream_chat" model)))
             (headers (%provider-headers client))
             (transport (or (provider-streaming-transport client)
                            (make-default-streaming-transport (provider-timeout client)))))
        (%set-key payload "stream" *json-true*)
        (setf (provider-last-chat-model client) model)
        (throw-if-cancelled cancellation (provider-name client))
        ;; The subscription is registered before the transport call, not after.
        ;; A streaming transport returns once the response headers arrive, so a
        ;; server that stalls before sending them would otherwise leave the run
        ;; with no way to interrupt: a subscription that can only fire after the
        ;; call returns cannot abort the wait it exists for.  The cell holds
        ;; whatever is closeable at the time cancellation arrives, and a cancel
        ;; that lands first is remembered so the connection is closed as soon as
        ;; it exists rather than being read from.
        (let ((closeable (list nil))
              (cancelled-early (list nil))
              (unsubscribe nil))
          (when cancellation
            (setf unsubscribe
                  (cancellation-subscribe
                   cancellation
                   (lambda ()
                     (let ((target (first closeable)))
                       (if target
                           (handler-case (funcall target) (error () nil))
                           (setf (first cancelled-early) t)))))))
          ;; The subscription has to outlive this call: the handle it closes is
          ;; still open when ax-stream returns, so tearing it down here would
          ;; leave a later cancel with nothing to close. It is released when the
          ;; handle closes, and on a failure before the handle exists.
          (handler-bind ((error (lambda (condition)
                                  (declare (ignore condition))
                                  (when unsubscribe (funcall unsubscribe)))))
            (%provider-open-stream client transport url headers payload options model
                                   cancellation closeable cancelled-early
                                   (lambda () (when unsubscribe (funcall unsubscribe))))))))))

(defun %provider-open-cancellable (client transport url headers payload cancellation release)
  "Call TRANSPORT on its own thread, abandoning it if the run is cancelled.

Answers (values read-chunk status closer).  Raises the aborted failure instead
when the run stops first, after unwinding the worker so the transport's cleanup
runs and the connection is released rather than left waiting."
  (if (null cancellation)
      (funcall transport url headers (encode-json payload))
      (let* ((done (sb-thread:make-semaphore :name "ax-stream-open"))
             (opened (list nil))
             (failure (list nil))
             (body (encode-json payload))
             (worker (sb-thread:make-thread
                      (lambda ()
                        (unwind-protect
                             (handler-case
                                 (multiple-value-bind (read-chunk status closer)
                                     (funcall transport url headers body)
                                   (setf (first opened) (list read-chunk status closer)))
                               (error (condition) (setf (first failure) condition)))
                          (sb-thread:signal-semaphore done)))
                      :name "ax-stream-open")))
        (loop
          (when (sb-thread:wait-on-semaphore done :timeout 0.02) (return))
          (when (cancelled-p cancellation)
            ;; Unwinding the worker runs the transport's own cleanup, which is
            ;; what actually releases a connection stuck awaiting headers.
            (when (sb-thread:thread-alive-p worker)
              (handler-case (sb-thread:terminate-thread worker) (error () nil)))
            (handler-case (sb-thread:join-thread worker :default nil :timeout 1)
              (error () nil))
            ;; A response that arrived in the same instant is closed, not used.
            (let ((late (first opened)))
              (when (and late (third late))
                (handler-case (funcall (third late)) (error () nil))))
            (when release (funcall release))
            (throw-if-cancelled cancellation (provider-name client))))
        (when (first failure)
          (when release (funcall release))
          (error (first failure)))
        (let ((result (first opened)))
          (if result
              (values (first result) (second result) (third result))
              (values (lambda () nil) 200 nil))))))

(defun %provider-open-stream (client transport url headers payload options model
                              cancellation closeable cancelled-early release &optional (start-attempt 0))
  "Open the streamed response and wrap it, with cancellation already armed.

The transport call runs on its own thread and this function waits for whichever
comes first, the response or the cancellation.  That is the point: a transport
has no obligation to know about cancellation, so a synchronous call blocks until
the server answers and no subscription can interrupt it.  A server that
withholds its response headers is exactly that case, and a test whose transport
waits on the token itself proves nothing, because the double supplies the
behaviour under test.  On cancellation the worker is unwound, which runs the
transport's own cleanup and closes the socket, and nothing partial is returned."
  (multiple-value-bind (read-chunk status closer)
      (%provider-retrying
       options
       (lambda ()
         (multiple-value-bind (reader status closer response-headers)
             (let ((fresh (%provider-credential-headers client (%provider-headers client)
                                                        "stream_chat" "POST" url)))
               (%provider-verbose-request options url "POST" fresh payload)
               (%provider-open-cancellable client transport url fresh payload cancellation nil))
           (setf *provider-response-headers* response-headers)
           (unless (<= 200 (or status 200) 299)
             (let ((text (unwind-protect
                             (with-output-to-string (out)
                               (loop for chunk = (funcall reader) while chunk
                                     do (write-string (%decode-response-body chunk) out)))
                           (when closer (funcall closer)))))
               (%provider-raise-status client status
                                       (handler-case (parse-json text) (error () :null))
                                       (object "url" url "json" payload) options)))
           (when (core-truthy-p (jget options "verbose"))
             (funcall *provider-verbose-sink* (axllm/core::ai-verbose-stream-log status)))
           (values reader status closer))))
    (declare (ignore status))
    (let ()
      (let* ((events (sse-stream-handle read-chunk :closer closer
                                                   :cancellation cancellation))
             (state (%new-object))
             (first-event t)
             (replacement nil))
        ;; Hand the live stream to the subscription that is already armed,
        ;; and honour a cancel that arrived while the headers were awaited.
        (setf (first closeable) (lambda () (ax-stream-close events)))
        (when (first cancelled-early)
          (ax-stream-close events)
          (throw-if-cancelled cancellation (provider-name client)))
        ;; The raw events are normalized one at a time, so a caller folding
        ;; them and a caller reading them incrementally agree.
        (make-ax-stream-handle
         ;; The read closure runs after this function has returned, so the
         ;; dynamic guard around the open is long gone. A failure while reading
         ;; the body is exactly where a provider echoes a credential back, so
         ;; the closure carries its own guard rather than relying on the open's.
         (lambda ()
           (redacting (client)
             ;; The cancellation subscriber closes the raw handle immediately.
             ;; Check the token before reading that now-closed handle, otherwise
             ;; its EOF would erase the aborted outcome.
             (throw-if-cancelled cancellation (provider-name client))
             (loop
               (when replacement (return (ax-stream-next replacement)))
               (let ((raw (ax-stream-next events)))
                 (when (eq raw :null) (return :null))
                 (when first-event
                   (setf first-event nil)
                   (let ((transient (axllm/core::provider-classify-stream-error-status (provider-profile client) raw))
                         (config (axllm/core::resolve-stream-retry options)))
                     (when (and (%present transient)
                                (< start-attempt (jget config "max_retries"))
                                (core-truthy-p (axllm/core::retry-status-listed config transient)))
                       (ax-stream-close events)
                       (funcall *provider-retry-sleep*
                                (min (jget config "max_delay_ms")
                                     (* (jget config "initial_delay_ms")
                                        (expt (jget config "backoff_factor") start-attempt))) cancellation)
                       (setf replacement (%provider-open-stream client transport url headers payload options model
                                                                 cancellation closeable cancelled-early release (1+ start-attempt)))
                       (return (ax-stream-next replacement)))))
                 (let ((chunk (axllm/core::provider-normalize-stream-delta
                               (provider-profile client) raw state
                               (provider-name client) model payload)))
                   ;; Core answers nothing for an event that carries no delta,
                   ;; such as a keep-alive or a role-only first chunk.
                   (unless (eq chunk :null) (return chunk)))))))
         :closer (lambda ()
                   (ax-stream-close events)
                   (when replacement (ax-stream-close replacement))
                   (when release (funcall release))))))))

;;; Native System One keeps the typed probabilities, not the chat adapter's
;;; boolean/enum projection. Snapshot before any credential/transport callback.
(defun typesafe-system-one (client request &optional options)
  (redacting (client)
    (let ((payload (parse-json (encode-json request))))
      (unless (gethash "model" payload)
        (setf (gethash "model" payload) (provider-model client)))
      (axllm/core::typesafe-validate-request payload)
      (axllm/core::typesafe-decode-response
       (%provider-request-json client "chat" payload
                               :options (%provider-merged-options client options))
       (jget payload "questions")))))

(defun typesafe-list-models (client &optional options)
  (redacting (client)
    (axllm/core::typesafe-decode-models
     (%provider-request-json client "models" :null :method "GET"
                             :path "/v1/models"
                             :options (%provider-merged-options client options)))))

(defun ai-context-cache-plan (configured supported explicit-name existing now refresh-window-ms create-eligible)
  (axllm/core::ai-context-cache-plan configured supported explicit-name existing now refresh-window-ms create-eligible))

(defun ai-context-cache-rejection (status body)
  (axllm/core::ai-context-cache-rejection status body))

(defun ai-context-cache-expiry (expiry now)
  (axllm/core::ai-context-cache-expiry expiry now))

(defun ai-context-cache-recovery (entry name external-registry)
  (axllm/core::ai-context-cache-recovery entry name external-registry))

(defun ai-gemini-cache-ops (name ttl api-key model body &optional (options (object)))
  (axllm/core::ai-gemini-cache-ops name ttl api-key model body options))

;;; Service composition. Selection and request transformations stay in Core;
;;; these objects retain the selected native service and delegate execution.
(defgeneric ax-model-list (service)
  (:method ((service t)) :null)
  (:method ((service provider-client)) (jget (%provider-options service) "models")))
(defgeneric ax-last-chat-model (service)
  (:method ((service t)) :null)
  (:method ((service provider-client)) (provider-last-chat-model service)))
(defgeneric ax-last-embed-model (service)
  (:method ((service t)) :null)
  (:method ((service provider-client)) (provider-last-embed-model service)))
(defgeneric ax-last-model-config (service)
  (:method ((service t)) :null)
  (:method ((service provider-client)) (provider-last-model-config service)))
(defgeneric ax-validate-request (service request)
  (:method ((service t) request) (declare (ignore request)) t)
  (:method ((service provider-client) request)
    (axllm/core::provider-validate-chat-request
     (provider-profile service) (%provider-prepare-request service request nil) (%provider-options service))))

(defun %service-accepts-request (service request)
  (handler-case (progn (ax-validate-request service request) t)
    (ax-error () nil)))

(defclass routing-service ()
  ((services :initarg :services :reader routing-services)
   (current :initarg :current :accessor routing-current)))
(defmethod ax-service-name ((service routing-service)) (ax-service-name (routing-current service)))
(defmethod ax-id ((service routing-service)) (ax-id (routing-current service)))
(defmethod ax-features ((service routing-service) &optional model)
  (ax-features (routing-current service) model))
(defmethod ax-metrics ((service routing-service)) (ax-metrics (routing-current service)))
(defmethod ai-name ((service routing-service)) (ax-service-name service))
(defmethod ai-model ((service routing-service)) (ai-model (routing-current service)))
(defmethod ax-last-chat-model ((service routing-service)) (ax-last-chat-model (routing-current service)))
(defmethod ax-last-embed-model ((service routing-service)) (ax-last-embed-model (routing-current service)))
(defmethod ax-last-model-config ((service routing-service)) (ax-last-model-config (routing-current service)))
(defmethod ax-options ((service routing-service)) (ax-options (routing-current service)))
(defmethod (setf ax-options) (options (service routing-service))
  (dolist (child (routing-services service)) (setf (ax-options child) options))
  options)
(defmethod ax-estimated-cost ((service routing-service) &optional usage)
  (ax-estimated-cost (routing-current service) usage))
(defmethod ax-model-list ((service routing-service))
  (loop for child in (routing-services service)
        for models = (ax-model-list child) when (%present models) return models finally (return :null)))
(defmethod ax-complete ((service routing-service) request)
  (axllm/core::chat-response-to-completion (ax-chat service request)))
(defmethod ax-transcribe ((service routing-service) request &optional options)
  (ax-transcribe (routing-current service) request options))
(defmethod ax-speak ((service routing-service) request &optional options)
  (ax-speak (routing-current service) request options))

(defclass provider-router (routing-service)
  ((routing :initarg :routing :reader router-routing)
   (processing :initarg :processing :reader router-processing)))
(defun provider-router (primary alternatives &key (routing (object)) (processing (object)))
  (let ((services (remove nil (cons primary (%as-list alternatives)))))
    (unless services (provider-fail :config "No AI services provided."))
    (make-instance 'provider-router :services services :current (first services)
                   :routing (jget routing "capability" routing) :processing processing)))
(defun %router-records (router &optional request)
  (map 'vector (lambda (service)
                 (object "name" (ax-service-name service) "id" (ax-id service)
                         "features" (ax-features service (%present-string (jget request "model")))
                         "requestCompatible" (json-boolean (or (null request) (%service-accepts-request service request)))))
       (routing-services router)))
(defun provider-routing-recommendation (router request)
  (axllm/core::provider-route-recommendation (%router-records router request) request (router-routing router)))
(defun provider-routing-validation (router request)
  (axllm/core::provider-route-validation (%router-records router request) request
                                        (router-processing router) (router-routing router)))
(defun provider-routing-stats (router)
  (axllm/core::provider-routing-stats (%router-records router)))
(defun %router-select (router request)
  (let* ((rec (provider-routing-recommendation router request))
         (name (jget rec "providerName"))
         (service (find name (routing-services router) :key #'ax-service-name :test #'equal)))
    (unless service (provider-fail :unsupported "No provider selected"))
    (setf (routing-current router) service)
    service))
(defun %router-prepare (router service request)
  (let* ((features (ax-features service (%present-string (jget request "model"))))
         (processing (axllm/core::core-map-merge (router-processing router) (object)))
         (extractor (%present (jget processing "fileToText" (jget processing "file_to_text")))))
    (when (functionp extractor)
      (let ((texts (object)))
        (loop for task across (axllm/core::provider-route-file-extractions features request)
              do (%set-key texts (jget task "slot") (funcall extractor (jget task "data") (jget task "mime_type"))))
        (%set-key processing "file_texts" texts)))
    (axllm/core::provider-route-preprocess-request features request processing)))
(defmethod ax-chat ((router provider-router) request &optional options)
  (let ((service (%router-select router request)))
    (ax-chat service (%router-prepare router service request) options)))
(defmethod ax-stream ((router provider-router) request &optional options)
  (let ((service (%router-select router request)))
    (ax-stream service (%router-prepare router service request) options)))
(defmethod ax-embed ((router provider-router) request &optional options)
  (ax-embed (%router-select router request) request options))

(defclass multiservice-router (routing-service)
  ((entries :initarg :entries :reader multiservice-entries)))

(defun multiservice-router (items)
  "Route named model entries without losing the underlying service's hooks."
  (let ((entries (object)) (services nil))
    (labels ((add (key entry service)
               (when (nth-value 1 (gethash key entries))
                 (provider-fail :config (format nil "Duplicate model key: ~a" key)))
               (%set-key entry "service" service)
               (%set-key entries key entry)
               (pushnew service services)))
      (loop for item in (%as-list items) for index from 0 do
        (if (hash-table-p item)
            (let* ((entry (copy-runtime-options item)) (model (%present (jget entry "model"))))
              (when model (%set-key entry "explicit_model" true))
              (add (jget entry "key") entry (jget entry "service")))
            (let ((models (%present (ax-model-list item))))
              (unless (and models (plusp (length models)))
                (provider-fail :config (format nil "Service ~a '~a' has no model list." index (ax-service-name item))))
              (loop for entry across models do
                (let* ((key (jget entry "key")) (existing (gethash key entries)))
                  (when existing
                    (provider-fail :config (format nil "Service ~a '~a' has duplicate model key: ~a as service ~a"
                                                   index (ax-service-name item) key (ax-service-name (jget existing "service"))))))
                (unless (or (%present (jget entry "model")) (%present (jget entry "embedModel"))
                            (%present (jget entry "embed_model")))
                  (provider-fail :config "Model list entry is missing a model or embedModel property."))
                (add (jget entry "key") (copy-runtime-options entry) item)))))
      (unless services (provider-fail :config "No AI services provided."))
      (make-instance 'multiservice-router :entries entries :services (nreverse services)
                     :current (jget (gethash (first (%object-keys entries)) entries) "service")))))

(defmethod ax-model-list ((router multiservice-router))
  (let ((out (%new-array)))
    (dolist (key (%object-keys (multiservice-entries router)) out)
      (let ((entry (gethash key (multiservice-entries router))))
        (unless (core-truthy-p (jget entry "isInternal" (jget entry "is_internal")))
          (let ((model-key (if (%present (jget entry "model")) "model" "embedModel")))
            (unless (%present (jget entry model-key))
              (provider-fail :config (format nil "Service ~a has no model or embedModel" key)))
            (vector-push-extend (object "key" key "description" (jget entry "description" "")
                                        model-key (jget entry model-key)) out)))))))

(defun %multiservice-invoke (router request options operation)
  (let* ((embed (eq operation :embed))
         (audio (member operation '(:transcribe :speak)))
         (key (%present (if embed (jget request "embedModel" (jget request "embed_model")) (jget request "model"))))
         (entry (if (and audio (null key))
                    (gethash (first (%object-keys (multiservice-entries router))) (multiservice-entries router))
                    (gethash key (multiservice-entries router)))))
    (unless (or key audio)
      (provider-fail :config (if embed "Embed model key must be specified for multi-service"
                                "Model key must be specified for multi-service")))
    (unless entry (provider-fail :config (format nil "No service found for model key: ~a" key)))
    (let ((service (jget entry "service")) (forwarded request) (opts options))
      (setf (routing-current router) service)
      (cond (embed (setf forwarded (axllm/core::router-embed-request request (jget entry "model")
                                     (jget entry "embedModel" (jget entry "embed_model")))))
            ((and (not audio) (or (core-truthy-p (jget entry "explicit_model"))
                                 (null (%present (jget entry "model")))))
             (let ((resolved (axllm/core::provider-session-route entry request (or options (object)))))
               (setf forwarded (jget resolved "request") opts (jget resolved "options")))))
      (funcall (ecase operation (:chat #'ax-chat) (:stream #'ax-stream) (:embed #'ax-embed)
                      (:transcribe #'ax-transcribe) (:speak #'ax-speak)) service forwarded opts))))

(defmethod ax-chat ((router multiservice-router) request &optional options)
  (%multiservice-invoke router request options :chat))
(defmethod ax-stream ((router multiservice-router) request &optional options)
  (%multiservice-invoke router request options :stream))
(defmethod ax-embed ((router multiservice-router) request &optional options)
  (%multiservice-invoke router request options :embed))
(defmethod ax-transcribe ((router multiservice-router) request &optional options)
  (%multiservice-invoke router request options :transcribe))
(defmethod ax-speak ((router multiservice-router) request &optional options)
  (%multiservice-invoke router request options :speak))

(defclass balancer-stats-store ()
  ((entries :initform (make-hash-table :test 'equal) :reader balancer-store-entries)
   (lock :initform (sb-thread:make-mutex :name "balancer-stats") :reader balancer-store-lock)))
(defun balancer-stats-store () (make-instance 'balancer-stats-store))
(defgeneric balancer-store-get (store key))
(defgeneric balancer-store-observe (store key observation))
(defmethod balancer-store-get ((store balancer-stats-store) key)
  (sb-thread:with-mutex ((balancer-store-lock store))
    (parse-json (encode-json (gethash (encode-json key) (balancer-store-entries store) :null)))))
(defmethod balancer-store-observe ((store balancer-stats-store) key observation)
  (sb-thread:with-mutex ((balancer-store-lock store))
    (setf (gethash (encode-json key) (balancer-store-entries store))
          (axllm/core::provider-balancer-observe-route
           (gethash (encode-json key) (balancer-store-entries store) :null) observation))))
(defmethod balancer-store-get ((store hash-table) key) (telemetry-call store "get" key))
(defmethod balancer-store-observe ((store hash-table) key observation) (telemetry-call store "observe" key observation))
(defun balancer-route-stats () (axllm/core::provider-balancer-route-stats))
(defun balancer-observe-route (stats observation) (axllm/core::provider-balancer-observe-route stats observation))
(defun balancer-sample-health (stats deadline) (axllm/core::provider-balancer-sample-health stats deadline))
(defun balancer-adaptive-score (cost bad-cost failure late)
  (axllm/core::provider-balancer-adaptive-score cost bad-cost failure late))

(defclass balancer (routing-service)
  ((policy :initarg :policy :reader balancer-policy)
   (strategy :initarg :strategy :reader balancer-strategy)
   (failures :initform (make-hash-table :test 'equal) :reader balancer-failures)
   (stats :initarg :stats :reader balancer-stats)))

(defmethod ax-metrics ((service balancer))
  (let ((out (default-metrics)))
    (dolist (kind '("chat" "embed") out)
      (let ((errors (jget (jget out "errors") kind))
            (latency (jget (jget out "latency") kind)) (sum 0) (count 0))
        (dolist (child (routing-services service))
          (let* ((metrics (ax-metrics child)) (err (jget (jget metrics "errors") kind))
                 (lat (jget (jget metrics "latency") kind)) (n (length (jget lat "samples" #()))))
            (dolist (key '("count" "total")) (incf (gethash key errors) (jget err key 0)))
            (incf sum (* n (jget lat "mean" 0))) (incf count n)
            (dolist (key '("p95" "p99")) (setf (gethash key latency) (max (gethash key latency) (jget lat key 0))))))
        (when (plusp (gethash "total" errors))
          (setf (gethash "rate" errors) (/ (gethash "count" errors) (gethash "total" errors))))
        (when (plusp count) (setf (gethash "mean" latency) (/ sum count)))))))

(defmethod ax-features ((service balancer) &optional model)
  ;; Same union as the native reference's _merge_service_features. Requiring
  ;; structured output is the exception: every route must require it.
  (let ((out (object "functions" false "streaming" false "thinking" false
                     "asyncTools" false "nativeSteering" false "reasoningUpdates" false
                     "multiTurn" false "structuredOutputs" false))
        (required t) (modes-known t))
    (labels ((merge-features (target source)
               (dolist (key (%object-keys source))
                 (let ((value (gethash key source)) (old (jget target key)))
                   (cond ((hash-table-p value)
                          (unless (hash-table-p old) (setf old (object)) (%set-key target key old))
                          (merge-features old value))
                         ((and (vectorp value) (not (stringp value)))
                          (%set-key target key (coerce (remove-duplicates
                            (append (if (%array-p old) (coerce old 'list)) (coerce value 'list))
                            :test #'equal :from-end t) 'vector)))
                         ((member value (list true false))
                          (%set-key target key (json-boolean (or (core-truthy-p old) (eq value true)))))
                         ((not (equal value "none")) (%set-key target key value))
                         ((eq old :null) (%set-key target key value)))))))
      (dolist (child (routing-services service))
        (let ((raw (ax-features child model)))
          (setf required (and required (core-truthy-p (jget raw "requiresStructuredOutput" (jget raw "requires_structured_output"))))
                modes-known (and modes-known (%present (jget raw "structuredOutputModes" (jget raw "structured_output_modes")))))
          (merge-features out raw)))
      (remhash "requiresStructuredOutput" out) (remhash "requires_structured_output" out)
      (when required (%set-key out "requiresStructuredOutput" true))
      (unless modes-known (remhash "structuredOutputModes" out) (remhash "structured_output_modes" out))
      out)))

(defun %balancer-event (balancer event)
  (let ((callback (%present (jget (balancer-strategy balancer) "onRoutingEvent"))))
    (when callback (handler-case (funcall callback event) (error () nil)))))
(defun %balancer-stats-key (balancer service request)
  (let* ((strategy (balancer-strategy balancer))
         (route-key (%present (jget strategy "routeKey")))
         (slice (%present (jget strategy "slice")))
         (slice-value (if slice (funcall slice (object "model" (jget request "model"))) "default")))
    (when (%blankp slice-value) (provider-fail :config "Adaptive slice must be non-empty."))
    (object "namespace" (jget strategy "namespace" "default") "slice" slice-value
            "logicalModel" (jget request "model" "default")
            "routeKey" (if route-key (funcall route-key service (position service (routing-services balancer))) (ax-id service)))))
(defun %balancer-observe (balancer service request observation)
  (let ((key (%balancer-stats-key balancer service request)))
    (handler-case (balancer-store-observe (balancer-stats balancer) key observation)
      (error () (%balancer-event balancer (object "type" "store-error" "operation" "observe" "key" key))))
    (%balancer-event balancer (axllm/core::core-map-merge key
                               (axllm/core::core-map-merge observation (object "type" "observation"))))))

(defun balancer (services &optional (options (object)))
  (let* ((services (%as-list services))
         (policy (axllm/core::provider-balancer-retry-policy options))
         (strategy (%present (jget options "strategy"))))
    (unless services (provider-fail :config "No AI services provided."))
    (when (hash-table-p strategy)
      (axllm/core::provider-balancer-adaptive-policy strategy)
      (when (%blankp (jget strategy "namespace" "default"))
        (provider-fail :config "Adaptive namespace must be non-empty."))
      (when (and (%present (jget strategy "statsStore")) (not (%present (jget strategy "routeKey"))))
        (provider-fail :config "Adaptive routeKey is required when statsStore is supplied."))
      (let ((seen (%new-array)))
        (loop for service in services for index from 0
              for callback = (%present (jget strategy "routeKey"))
              do (vector-push-extend (axllm/core::provider-balancer-validate-route-key
                                     (if callback (funcall callback service index) (ax-id service)) seen) seen))))
    (unless (equal (jget policy "strategy") "input_order")
      (setf services (stable-sort services #'< :key (lambda (service)
                                                    (axllm/core::provider-balancer-metric-score (ax-metrics service))))))
    (make-instance 'balancer :services services :current (first services) :policy policy :strategy strategy
                   :stats (or (%present (jget strategy "statsStore")) (balancer-stats-store)))))

(defun %balancer-candidates (balancer request)
  (let ((candidates (remove-if-not
                     (lambda (service)
                       (and (%service-accepts-request service request)
                            (core-truthy-p (axllm/core::provider-balancer-candidate-allowed
                                            (ax-features service (%present-string (jget request "model"))) request))))
                     (routing-services balancer))))
    (unless candidates (provider-fail :unsupported "No services available that support required capabilities."))
    (if (hash-table-p (balancer-strategy balancer))
        (let ((ranked (%new-array)) (strategy (balancer-strategy balancer)))
          (loop for service in candidates for order from 0
                for key = (%balancer-stats-key balancer service request)
                for stats = (handler-case (balancer-store-get (balancer-stats balancer) key)
                              (error () (%balancer-event balancer (object "type" "store-error" "operation" "get" "key" key)) :null))
                for health = (axllm/core::provider-balancer-sample-health stats (jget strategy "deadlineMs"))
                for cost = (ax-estimated-cost service)
                do (vector-push-extend
                    (object "routeKey" (ax-id service) "order" order
                            "score" (axllm/core::provider-balancer-adaptive-score cost (jget strategy "badOutcomeCost")
                                                                                 (jget health "failureProbability") (jget health "deadlineMissProbability"))) ranked))
          (%balancer-event balancer (object "type" "ranked" "candidates" ranked))
          (map 'list (lambda (entry) (find (jget entry "routeKey") candidates :key #'ax-id :test #'equal))
               (axllm/core::provider-balancer-rank-candidates ranked)))
        candidates)))

(defun %balancer-invoke (balancer request options operation)
  (let* ((chat (member operation '(:chat :stream)))
         (candidates (if chat (%balancer-candidates balancer request) (routing-services balancer)))
         (adaptive (and chat (hash-table-p (balancer-strategy balancer))))
         (last-error nil)
         (started (get-internal-real-time)))
    (dolist (service candidates)
      (setf (routing-current balancer) service)
      (when adaptive (%balancer-event balancer (object "type" "selected" "routeKey" (ax-id service))))
      (unless (and (not adaptive) (gethash (ax-id service) (balancer-failures balancer)))
        (handler-case
            (let ((response (ecase operation
                              (:chat (ax-chat service request options))
                              (:embed (ax-embed service request options))
                              (:stream (let ((handle (ax-stream service request options)))
                                         (handler-case
                                             (let ((first (ax-stream-next handle)) (sent nil))
                                               (make-ax-stream-handle
                                                (lambda () (if sent (ax-stream-next handle) (progn (setf sent t) first)))
                                                :closer (lambda () (ax-stream-close handle))))
                                           (error (c) (ax-stream-close handle) (error c))))))))
              (remhash (ax-id service) (balancer-failures balancer))
              (when adaptive
                (%balancer-observe balancer service request
                                   (object "outcome" "success" "latencyMs"
                                           (max 1 (* 1000 (/ (- (get-internal-real-time) started) internal-time-units-per-second))))))
              (return-from %balancer-invoke response))
          (provider-error (c)
            (unless (or (provider-error-retryable-p c)
                        (member (provider-error-kind c) '(:network :timeout :stream))) (error c))
            (setf last-error c (gethash (ax-id service) (balancer-failures balancer)) t)
            (when adaptive
              (%balancer-observe balancer service request (object "outcome" "failure"))
              (%balancer-event balancer (object "type" "fallback" "routeKey" (ax-id service))))))))
    (when (and adaptive last-error) (error last-error))
    (provider-fail :response (format nil "All ~aservices exhausted (tried ~a service(s))"
                                     (if chat "candidate " "") (length candidates)))))
(defmethod ax-chat ((service balancer) request &optional options)
  (%balancer-invoke service request options :chat))
(defmethod ax-stream ((service balancer) request &optional options)
  (%balancer-invoke service request options :stream))
(defmethod ax-embed ((service balancer) request &optional options)
  (%balancer-invoke service request options :embed))
