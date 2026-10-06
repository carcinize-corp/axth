;;;; ai.lisp --- native provider layer for the experimental Common Lisp Ax port.
;;;;
;;;; Scope: synchronous, non-streaming chat against OpenAI-compatible Chat
;;;; Completions and Anthropic Messages.  This is an explicit experimental
;;;; subset of AxIR `axai`, not parity: no streaming, embeddings, audio,
;;;; routers, balancers, thinking budgets, or provider presets.
;;;;
;;;; Depends on the foundation layer (package, json, signature) for
;;;; `object', `jget', `parse-json', `encode-json' and the `ax-error'
;;;; condition.  Depends on Drakma only for the default HTTP transport.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Conditions
;;; ------------------------------------------------------------------

(define-condition provider-error (ax-error)
  ((kind :initarg :kind :initform :provider :reader provider-error-kind)
   (provider :initarg :provider :initform nil :reader provider-error-provider)
   (status :initarg :status :initform nil :reader provider-error-status))
  (:documentation
   "A typed provider failure.  KIND is one of :config, :auth, :transport,
:http, :response, :refusal or :truncated.  Messages are redacted: API keys
and request headers are never included."))

(defparameter *json-true* 'yason:true
  "The value the foundation's `parse-json' produces for JSON true.")

(defparameter *json-false* 'yason:false
  "The value the foundation's `parse-json' produces for JSON false.")

(defun json-true-p (value) (eq value *json-true*))

(defun json-false-p (value) (eq value *json-false*))

(defun json-boolean-p (value)
  (or (json-true-p value) (json-false-p value)))

(defun json-boolean (generalized)
  "Convert a Lisp generalized boolean to the JSON representation."
  (if generalized *json-true* *json-false*))

(defun %present (value)
  "Treat an explicit JSON null as an absent optional field."
  (if (eq value :null) nil value))

(defun %present-string (value)
  (let ((value (%present value)))
    (and (stringp value) value)))

(defun %present-array (value name provider)
  "VALUE as a vector, or nil when absent/null.  A wrong shape is a typed
response error rather than a raw type error later on."
  (let ((value (%present value)))
    (cond ((null value) nil)
          ((and (vectorp value) (not (stringp value))) value)
          ((consp value) (coerce value 'vector))
          (t (provider-fail :response
                            (format nil "Provider response field \"~a\" was not an array." name)
                            :provider provider)))))

(defun %present-object (value name provider)
  (let ((value (%present value)))
    (cond ((null value) nil)
          ((hash-table-p value) value)
          (t (provider-fail :response
                            (format nil "Provider response field \"~a\" was not an object." name)
                            :provider provider)))))

(defun provider-fail (kind message &key provider status)
  "Signal a `provider-error'.  MESSAGE must already be redacted."
  (error 'provider-error
         :kind kind
         :message message
         :provider provider
         :status status))

;;; ------------------------------------------------------------------
;;; Small string helpers (no reader, no eval, ever)
;;; ------------------------------------------------------------------

(defun %blankp (value)
  (or (null value)
      (not (stringp value))
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun %trim (value)
  (if (stringp value)
      (string-trim '(#\Space #\Tab #\Newline #\Return) value)
      value))

(defun %replace-all (text needle replacement)
  (if (or (not (stringp text)) (zerop (length needle)))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for hit = (search needle text :start2 start)
              while hit
              do (write-string text out :start start :end hit)
                 (write-string replacement out)
                 (setf start (+ hit (length needle)))
              finally (write-string text out :start start)))))

(defun %redact (text secret)
  "Remove SECRET from TEXT.  Used on every provider/transport message so a
condition can never carry an API key."
  (if (and (stringp text) (stringp secret) (> (length secret) 3))
      (%replace-all text secret "[redacted]")
      text))

(defun %string-join (separator strings)
  (with-output-to-string (out)
    (loop for rest on strings
          do (write-string (car rest) out)
             (when (cdr rest) (write-string separator out)))))

(defun %as-list (sequence)
  (cond ((null sequence) nil)
        ((listp sequence) (copy-list sequence))
        ((vectorp sequence) (coerce sequence 'list))
        (t (provider-fail :config "Expected a list or vector of messages."))))

;;; ------------------------------------------------------------------
;;; Provider descriptors
;;; ------------------------------------------------------------------

(defstruct (provider-descriptor (:conc-name pd-))
  name
  base-url
  path
  key-env
  auth-header
  auth-prefix
  extra-headers)

(defparameter *provider-descriptors*
  (list
   (make-provider-descriptor
    :name "openai"
    :base-url "https://api.openai.com/v1"
    :path "/chat/completions"
    :key-env "OPENAI_API_KEY"
    :auth-header "Authorization"
    :auth-prefix "Bearer "
    :extra-headers nil)
   (make-provider-descriptor
    :name "anthropic"
    :base-url "https://api.anthropic.com/v1"
    :path "/messages"
    :key-env "ANTHROPIC_API_KEY"
    :auth-header "x-api-key"
    :auth-prefix ""
    :extra-headers '(("anthropic-version" . "2023-06-01"))))
  "Provider mapping descriptors for the surfaces this subset claims.
`openai' also covers OpenAI-compatible Chat Completions endpoints.")

(defun find-provider-descriptor (name)
  (find name *provider-descriptors* :key #'pd-name :test #'string=))

(defun %provider-name (name)
  (let ((text (cond ((null name) "openai")
                    ((stringp name) name)
                    ((symbolp name) (string-downcase (symbol-name name)))
                    (t (provider-fail :config "ai: :name must be a string or symbol.")))))
    (let ((normalized (string-downcase (%trim text))))
      (cond ((string= normalized "openai-compatible") "openai")
            (t normalized)))))

;;; ------------------------------------------------------------------
;;; Client
;;; ------------------------------------------------------------------

(defclass ai-client ()
  ((name :initarg :name :reader ai-name)
   (model :initarg :model :reader ai-model)
   (api-key :initarg :api-key :reader ai-api-key)
   (base-url :initarg :base-url :reader ai-base-url)
   (transport :initarg :transport :reader ai-transport)
   (timeout :initarg :timeout :reader ai-timeout)
   (max-tokens :initarg :max-tokens :reader ai-max-tokens)
   (max-tokens-explicit-p :initarg :max-tokens-explicit-p :reader ai-max-tokens-explicit-p)
   (descriptor :initarg :descriptor :reader ai-descriptor)
   (custom-transport-p :initarg :custom-transport-p :reader ai-custom-transport-p)))

(defmethod print-object ((client ai-client) stream)
  ;; Never print the API key.
  (print-unreadable-object (client stream :type t)
    (format stream "~a model=~a" (ai-name client) (ai-model client))))

(defun %strip-trailing-slash (url)
  (if (and (stringp url) (> (length url) 1) (char= (char url (1- (length url))) #\/))
      (subseq url 0 (1- (length url)))
      url))

(defun ai (&key name model api-key base-url transport timeout max-tokens)
  "Create a provider client.

NAME is \"openai\" (OpenAI-compatible Chat Completions) or \"anthropic\"
\(Messages).  MODEL is required: there is no stale default.  API-KEY falls
back to the provider's environment variable.  TRANSPORT, when supplied, is a
function of (url headers json-body) returning (values response-json-string
status-integer); the default transport performs a real bounded HTTP POST.
TIMEOUT bounds a single request in seconds.  MAX-TOKENS is required by the
Anthropic Messages API and defaults to 4096 there; for OpenAI it is omitted
unless you pass it, in which case it is sent as `max_completion_tokens'."
  (let* ((provider (%provider-name name))
         (descriptor (or (find-provider-descriptor provider)
                         (provider-fail
                          :config
                          (format nil "Unknown provider '~a'. Supported: ~{~a~^, ~}."
                                  provider (mapcar #'pd-name *provider-descriptors*))))))
    (when (%blankp model)
      (provider-fail :config
                     (format nil "ai: :model is required for provider '~a'; ~
pass an explicit model name." provider)
                     :provider provider))
    (when (and transport (not (functionp transport)))
      (provider-fail :config "ai: :transport must be a function of (url headers json-body)."
                     :provider provider))
    (let* ((timeout (cond ((null timeout) 60)
                          ((and (realp timeout) (plusp timeout)) timeout)
                          (t (provider-fail :config "ai: :timeout must be a positive number."
                                            :provider provider))))
           (max-tokens-supplied max-tokens)
           (max-tokens (cond ((null max-tokens) 4096)
                             ((and (integerp max-tokens) (plusp max-tokens)) max-tokens)
                             (t (provider-fail :config "ai: :max-tokens must be a positive integer."
                                               :provider provider))))
           (key (if (%blankp api-key)
                    (let ((from-env (uiop:getenv (pd-key-env descriptor))))
                      (if (%blankp from-env) nil (%trim from-env)))
                    (%trim api-key))))
      (when (and (null key) (null transport))
        (provider-fail :auth
                       (format nil "Missing API key for provider '~a'. Set ~a or pass :api-key."
                               provider (pd-key-env descriptor))
                       :provider provider))
      (make-instance 'ai-client
                     :name provider
                     :model (%trim model)
                     :api-key key
                     :base-url (%strip-trailing-slash
                                (if (%blankp base-url) (pd-base-url descriptor) (%trim base-url)))
                     :transport (or transport (make-default-transport timeout))
                     :custom-transport-p (and transport t)
                     :timeout timeout
                     :max-tokens max-tokens
                     :max-tokens-explicit-p (and max-tokens-supplied t)
                     :descriptor descriptor))))

(defun ai-request-url (client)
  (concatenate 'string (ai-base-url client) (pd-path (ai-descriptor client))))

(defun ai-request-headers (client)
  "Request headers as an alist of (name . value) strings."
  (let* ((descriptor (ai-descriptor client))
         (key (ai-api-key client))
         (headers (list (cons "content-type" "application/json"))))
    (dolist (extra (pd-extra-headers descriptor))
      (push (cons (car extra) (cdr extra)) headers))
    (when key
      (push (cons (pd-auth-header descriptor)
                  (concatenate 'string (pd-auth-prefix descriptor) key))
            headers))
    (nreverse headers)))

;;; ------------------------------------------------------------------
;;; Default HTTP transport
;;; ------------------------------------------------------------------

(defun %decode-response-body (body)
  "Decode a Drakma response body.  `application/json' is not a Drakma text
content type, so the body arrives as octets; it must be decoded as UTF-8 or
every non-ASCII character is corrupted."
  (cond ((stringp body) body)
        ((null body) "")
        ((and (vectorp body)
              (every (lambda (byte) (typep byte '(unsigned-byte 8))) body))
         (handler-case
             (sb-ext:octets-to-string (coerce body '(vector (unsigned-byte 8)))
                                      :external-format :utf-8)
           (error ()
             (provider-fail :response "Provider response body was not valid UTF-8."))))
        (t (provider-fail :transport "Unexpected HTTP response body representation."))))

(defun make-default-transport (timeout)
  "Return the default bounded HTTP transport closure."
  (lambda (url headers json-body)
    (handler-case
        (sb-ext:with-timeout timeout
          (multiple-value-bind (body status)
              (drakma:http-request url
                                   :method :post
                                   :additional-headers
                                   (remove "content-type" headers
                                           :key #'car :test #'string-equal)
                                   :content-type "application/json"
                                   :content json-body
                                   :external-format-out :utf-8
                                   :external-format-in :utf-8
                                   :connection-timeout timeout
                                   ;; Never follow redirects: Drakma would
                                   ;; replay the Authorization/x-api-key
                                   ;; header to the redirect target.
                                   :redirect nil
                                   :force-binary nil
                                   :want-stream nil)
            (values (%decode-response-body body) status)))
      (sb-ext:timeout ()
        (provider-fail :transport
                       (format nil "HTTP request timed out after ~a second(s)." timeout)))
      (provider-error (condition) (error condition))
      (error (condition)
        ;; Drakma/usocket conditions can mention the URL but never the headers.
        (provider-fail :transport
                       (format nil "HTTP transport failure: ~a"
                               (substitute #\Space #\Newline (princ-to-string condition))))))))

;;; ------------------------------------------------------------------
;;; Normalized message helpers
;;; ------------------------------------------------------------------

(defparameter +message-roles+ '("system" "user" "assistant" "tool"))

(defun message (role content &key tool-calls tool-call-id)
  "Build a normalized message object: string keys, vectors for arrays."
  (let ((msg (object "role" role "content" (or content ""))))
    (when tool-calls
      (setf (gethash "toolCalls" msg)
            (if (vectorp tool-calls) tool-calls (coerce tool-calls 'vector))))
    (when tool-call-id
      (setf (gethash "toolCallId" msg) tool-call-id))
    msg))

(defun %message-role (msg)
  (let ((role (%present (jget msg "role"))))
    (unless (and (stringp role) (member role +message-roles+ :test #'string=))
      (provider-fail :config
                     (format nil "Message role must be one of ~{~a~^, ~}."
                             +message-roles+)))
    role))

(defun %message-content-string (msg)
  (let ((content (%present (jget msg "content" ""))))
    (cond ((stringp content) content)
          ((eq content :null) "")
          ((null content) "")
          (t (provider-fail :config "Message \"content\" must be a string.")))))

(defun %message-tool-calls (msg)
  (let ((calls (%present (jget msg "toolCalls"))))
    (cond ((null calls) nil)
          ((vectorp calls) (coerce calls 'list))
          ((listp calls) calls)
          (t (provider-fail :config "Message \"toolCalls\" must be an array.")))))

(defun %tool-call-fields (call)
  (let ((id (%present (jget call "id")))
        (name (%present (jget call "name")))
        (arguments (or (%present (jget call "arguments")) "{}")))
    (when (%blankp id)
      (provider-fail :config "Tool call is missing \"id\"."))
    (when (%blankp name)
      (provider-fail :config "Tool call is missing \"name\"."))
    (values id name (if (stringp arguments) arguments (encode-json arguments)))))

;;; ------------------------------------------------------------------
;;; Request mapping: OpenAI-compatible Chat Completions
;;; ------------------------------------------------------------------

(defun %openai-messages (messages)
  (let ((out '()))
    (dolist (msg messages)
      (let ((role (%message-role msg))
            (content (%message-content-string msg)))
        (cond
          ((string= role "tool")
           (let ((id (%present (jget msg "toolCallId"))))
             (when (%blankp id)
               (provider-fail :config "A \"tool\" message requires \"toolCallId\"."))
             (push (object "role" "tool" "tool_call_id" id "content" content) out)))
          ((string= role "assistant")
           (let ((calls (%message-tool-calls msg))
                 (entry (object "role" "assistant")))
             (setf (gethash "content" entry)
                   (if (and calls (zerop (length content))) :null content))
             (when calls
               (setf (gethash "tool_calls" entry)
                     (map 'vector
                          (lambda (call)
                            (multiple-value-bind (id name arguments) (%tool-call-fields call)
                              (object "id" id
                                      "type" "function"
                                      "function" (object "name" name
                                                         "arguments" arguments))))
                          (coerce calls 'vector))))
             (push entry out)))
          (t (push (object "role" role "content" content) out)))))
    (coerce (nreverse out) 'vector)))

(defun %openai-tools (tools)
  (map 'vector
       (lambda (spec)
         (object "type" "function"
                 "function" (object "name" (jget spec "name")
                                    "description" (or (jget spec "description") "")
                                    "parameters" (jget spec "parameters"))))
       (coerce tools 'vector)))

(defun %openai-request-body (client messages tools tool-choice)
  (let ((body (object "model" (ai-model client)
                      "messages" (%openai-messages messages))))
    (when (ai-max-tokens-explicit-p client)
      (setf (gethash "max_completion_tokens" body) (ai-max-tokens client)))
    (when tools
      (setf (gethash "tools" body) (%openai-tools tools))
      (setf (gethash "tool_choice" body)
            (ecase tool-choice (:auto "auto") (:none "none"))))
    body))

;;; ------------------------------------------------------------------
;;; Request mapping: Anthropic Messages
;;; ------------------------------------------------------------------

(defun %anthropic-request-body (client messages tools tool-choice)
  (let ((system '())
        (out '()))
    (dolist (msg messages)
      (let ((role (%message-role msg))
            (content (%message-content-string msg)))
        (cond
          ((string= role "system")
           (push content system))
          ((string= role "tool")
           (let ((id (%present (jget msg "toolCallId"))))
             (when (%blankp id)
               (provider-fail :config "A \"tool\" message requires \"toolCallId\"."))
             (let ((block* (object "type" "tool_result"
                                   "tool_use_id" id
                                   "content" content))
                   (previous (first out)))
               ;; Merge consecutive tool results into a single user turn, as
               ;; the Messages API expects.
               (if (and previous
                        (string= (jget previous "role") "user")
                        (eq (gethash "axToolResultTurn" previous) t))
                   (setf (gethash "content" previous)
                         (concatenate 'vector (jget previous "content") (vector block*)))
                   (let ((turn (object "role" "user" "content" (vector block*))))
                     (setf (gethash "axToolResultTurn" turn) t)
                     (push turn out))))))
          ((string= role "assistant")
           (let* ((calls (%message-tool-calls msg))
                  (blocks '()))
             (unless (zerop (length content))
               (push (object "type" "text" "text" content) blocks))
             (dolist (call calls)
               (multiple-value-bind (id name arguments) (%tool-call-fields call)
                 (push (object "type" "tool_use"
                               "id" id
                               "name" name
                               "input" (parse-json arguments))
                       blocks)))
             ;; The Messages API rejects an assistant turn with empty content,
             ;; which a blank model reply in a correction history would produce.
             (unless blocks
               (push (object "type" "text" "text" "(the assistant returned no content)")
                     blocks))
             (push (object "role" "assistant"
                           "content" (coerce (nreverse blocks) 'vector))
                   out)))
          (t
           (push (object "role" "user"
                         "content" (vector (object "type" "text" "text" content)))
                 out)))))
    (let ((turns (nreverse out)))
      ;; Drop the internal merge marker before encoding.
      (dolist (turn turns) (remhash "axToolResultTurn" turn))
      (let ((body (object "model" (ai-model client)
                          "max_tokens" (ai-max-tokens client)
                          "messages" (coerce turns 'vector))))
        (when system
          (setf (gethash "system" body) (%string-join (string #\Newline) (nreverse system))))
        (when tools
          (setf (gethash "tools" body)
                (map 'vector
                     (lambda (spec)
                       (object "name" (jget spec "name")
                               "description" (or (jget spec "description") "")
                               "input_schema" (jget spec "parameters")))
                     (coerce tools 'vector)))
          (setf (gethash "tool_choice" body)
                (object "type" (ecase tool-choice (:auto "auto") (:none "none")))))
        body))))

;;; ------------------------------------------------------------------
;;; Response normalization
;;; ------------------------------------------------------------------

(defun usage-object (prompt completion &optional total)
  (object "promptTokens" (or prompt 0)
          "completionTokens" (or completion 0)
          "totalTokens" (or total (+ (or prompt 0) (or completion 0)))))

(defun %integer-or-zero (value)
  (if (integerp value) value 0))

(defun %openai-normalize (payload provider)
  (let* ((choices (%present-array (jget payload "choices") "choices" provider))
         (choice (if (and choices (plusp (length choices)))
                     (aref choices 0)
                     (provider-fail :response
                                    "Provider response is missing a \"choices\" entry."
                                    :provider provider)))
         (msg (or (%present-object (jget choice "message") "message" provider)
                  (provider-fail :response "Provider choice is missing \"message\"."
                                 :provider provider)))
         (refusal (%present (jget msg "refusal")))
         (finish (%present (jget choice "finish_reason")))
         (content (let ((raw (%present (jget msg "content"))))
                    (cond ((stringp raw) raw)
                          ((null raw) "")
                          (t (provider-fail :response "Unexpected \"content\" shape."
                                            :provider provider)))))
         (tool-calls (%present-array (jget msg "tool_calls") "tool_calls" provider))
         (usage (%present-object (jget payload "usage") "usage" provider)))
    (when (and (stringp refusal) (not (%blankp refusal)))
      ;; The refusal text itself is provider-controlled content and is not
      ;; copied into the condition.
      (provider-fail :refusal
                     (format nil "Provider '~a' refused the request; the refusal text is not ~
included in this condition." provider)
                     :provider provider))
    (let ((normalized-calls
            (if (and tool-calls (plusp (length tool-calls)))
                (map 'vector
                     (lambda (call)
                       (unless (hash-table-p call)
                         (provider-fail :response "A \"tool_calls\" entry was not an object."
                                        :provider provider))
                       (let* ((fn (%present-object (jget call "function") "function" provider))
                              (arguments (and fn (jget fn "arguments"))))
                         (unless (stringp arguments)
                           (provider-fail :response "Tool arguments must be a JSON-encoded string."
                                          :provider provider))
                         (object "id" (or (%present-string (jget call "id")) "")
                                 "name" (or (and fn (%present-string (jget fn "name"))) "")
                                 "arguments" arguments)))
                     tool-calls)
                (vector))))
      (object "content" content
              "toolCalls" normalized-calls
              "finishReason" (if (stringp finish) finish "")
              "usage" (usage-object (%integer-or-zero (and usage (%present (jget usage "prompt_tokens"))))
                                    (%integer-or-zero (and usage (%present (jget usage "completion_tokens"))))
                                    (and usage (let ((total (%present (jget usage "total_tokens"))))
                                                 (and (integerp total) total))))))))

(defun %anthropic-normalize (payload provider)
  (let ((blocks (%present-array (jget payload "content") "content" provider))
        (stop (%present (jget payload "stop_reason")))
        (usage (%present-object (jget payload "usage") "usage" provider))
        (texts '())
        (calls '()))
    (unless blocks
      (provider-fail :response "Provider response is missing a \"content\" array."
                     :provider provider))
    (map nil
         (lambda (block*)
           (unless (hash-table-p block*)
             (provider-fail :response "A \"content\" block was not an object."
                            :provider provider))
           (let ((type (%present-string (jget block* "type"))))
             (cond ((equal type "text")
                    (let ((text (%present-string (jget block* "text"))))
                      (when text (push text texts))))
                   ((equal type "tool_use")
                    (let ((input (jget block* "input")))
                      (unless (hash-table-p input)
                        (provider-fail :response "Tool input must be a JSON object."
                                       :provider provider))
                      (push (object "id" (or (%present-string (jget block* "id")) "")
                                    "name" (or (%present-string (jget block* "name")) "")
                                    "arguments" (encode-json input))
                            calls)))
                   (t nil))))
         blocks)
    (object "content" (%string-join (string #\Newline) (nreverse texts))
            "toolCalls" (coerce (nreverse calls) 'vector)
            "finishReason" (if (stringp stop) stop "")
            "usage" (usage-object (%integer-or-zero (and usage (%present (jget usage "input_tokens"))))
                                  (%integer-or-zero (and usage (%present (jget usage "output_tokens"))))))))

(defparameter +refusal-finish-reasons+
  '(("openai" . ("content_filter" "refusal"))
    ("anthropic" . ("refusal")))
  "Finish/stop reasons that mean the provider declined to answer.")

(defparameter +truncation-finish-reasons+
  '(("openai" . ("length" "model_context_window_exceeded"))
    ("anthropic" . ("max_tokens" "model_context_window_exceeded")))
  "Finish/stop reasons that mean the answer is incomplete.")

(defun %finish-reason-in (table provider finish-reason)
  (and (stringp finish-reason)
       (member finish-reason (cdr (assoc provider table :test #'string=)) :test #'string=)
       t))

(defun %refusal-finish-p (provider finish-reason)
  (%finish-reason-in +refusal-finish-reasons+ provider finish-reason))

(defun %truncated-p (provider finish-reason)
  (%finish-reason-in +truncation-finish-reasons+ provider finish-reason))

;;; ------------------------------------------------------------------
;;; chat
;;; ------------------------------------------------------------------

(defun %resolve-tool-choice (tool-choice provider)
  (case tool-choice
    ((nil :auto) :auto)
    (:none :none)
    (t (provider-fail :config "chat: :tool-choice must be :auto or :none."
                      :provider provider))))

(defun %chat-1 (client messages tools tool-choice)
  (let* ((provider (ai-name client))
         (message-list (%as-list messages))
         (tool-list (%as-list tools))
         (choice (%resolve-tool-choice tool-choice provider)))
    (when (null message-list)
      (provider-fail :config "chat: at least one message is required." :provider provider))
    (dolist (spec tool-list)
      (when (%blankp (jget spec "name"))
        (provider-fail :config "chat: every tool requires a \"name\"." :provider provider)))
    (let* ((body-object (if (string= provider "anthropic")
                            (%anthropic-request-body client message-list tool-list choice)
                            (%openai-request-body client message-list tool-list choice)))
           (json-body (encode-json body-object))
           (url (ai-request-url client))
           (headers (ai-request-headers client)))
      (multiple-value-bind (response-text status)
          (handler-case (funcall (ai-transport client) url headers json-body)
            (provider-error (condition) (error condition))
            (error (condition)
              ;; A transport condition comes from the local HTTP stack, not the
              ;; provider body, but it is still redacted by `chat'.
              (provider-fail :transport
                             (format nil "Transport failure: ~a"
                                     (substitute #\Space #\Newline (princ-to-string condition)))
                             :provider provider)))
        (unless (stringp response-text)
          (provider-fail :transport "Transport must return the response body as a string."
                         :provider provider))
        (unless (or (null status) (integerp status))
          (provider-fail :transport
                         "Transport must return the HTTP status as an integer."
                         :provider provider))
        (let ((status (or status 200))
              (payload (handler-case (parse-json response-text)
                         (error () nil))))
          ;; Provider error bodies are never copied into a condition: the kind
          ;; and the HTTP status carry the diagnosis instead.
          (when (= status 401)
            (provider-fail :auth
                           (format nil "Authentication rejected by provider '~a' (HTTP 401). ~
The response body is not included in this condition."
                                   provider)
                           :provider provider :status status))
          (unless (<= 200 status 299)
            (provider-fail :http
                           (format nil "Provider '~a' returned HTTP ~a. ~
The response body is not included in this condition."
                                   provider status)
                           :provider provider :status status))
          (unless (hash-table-p payload)
            (provider-fail :response
                           "Provider response was not a JSON object."
                           :provider provider :status status))
          (let* ((result (if (string= provider "anthropic")
                             (%anthropic-normalize payload provider)
                             (%openai-normalize payload provider)))
                 (finish (jget result "finishReason")))
            (when (%refusal-finish-p provider finish)
              (provider-fail :refusal
                             (format nil "Provider '~a' declined the request (finish reason '~a')."
                                     provider finish)
                             :provider provider :status status))
            (when (%truncated-p provider finish)
              (provider-fail :truncated
                             (format nil "Provider '~a' truncated the response (finish reason ~
'~a'); increase :max-tokens or shorten the request."
                                     provider finish)
                             :provider provider :status status))
            result))))))

(defun chat (client messages &key tools tool-choice)
  "Send MESSAGES to CLIENT synchronously and return a normalized object with
\"content\", \"toolCalls\", \"usage\" and \"finishReason\".

MESSAGES is a list or vector of normalized message objects.  TOOLS is a list
or vector of tool specs (see `tool').  TOOL-CHOICE is :auto (the default) or
:none; :none keeps the tool definitions in the request, which the providers
require while the history still contains tool calls and results, while
forbidding a new call.  No request is ever retried automatically, and no
condition signalled from here carries an API key or a provider response body."
  (check-type client ai-client)
  (let ((secret (ai-api-key client)))
    (handler-bind
        ((provider-error
           (lambda (condition)
             ;; Last line of defence: whatever path raised this, the key
             ;; cannot appear in the message a caller sees or logs.
             ;; Read the message through the condition's report so this does
             ;; not depend on the foundation's slot reader name.
             (let* ((text (princ-to-string condition))
                    (clean (%redact text secret)))
               (unless (equal text clean)
                 (error 'provider-error
                        :kind (provider-error-kind condition)
                        :provider (provider-error-provider condition)
                        :status (provider-error-status condition)
                        :message clean))))))
      (%chat-1 client messages tools tool-choice))))
