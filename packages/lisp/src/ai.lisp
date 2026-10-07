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
   (status :initarg :status :initform nil :reader provider-error-status)
   ;; The reference's AxAIService*Error carry a provider error code, the
   ;; response body and the failing request alongside the status, and say
   ;; whether the failure is worth retrying.  Core's error constructors pass
   ;; all of them, so they are slots rather than text baked into the message.
   (code :initarg :code :initform nil :reader provider-error-code)
   (response-body :initarg :response-body :initform nil
                  :reader provider-error-response-body)
   (request :initarg :request :initform nil :reader provider-error-request)
   (retryable :initarg :retryable :initform nil :reader provider-error-retryable-p)
   (cause :initarg :cause :initform nil :reader provider-error-cause))
  (:documentation
   "A typed provider failure.

KIND is one of :config, :auth, :transport, :http, :response, :refusal or
:truncated -- the kinds this port has always signalled -- plus the kinds
Core's own error constructors produce: :status, :timeout, :stream,
:unsupported, :network and :aborted.

Messages are redacted: API keys and request headers are never included.
RESPONSE-BODY and REQUEST are available to a caller that deliberately asks
for them, and are never written into the message."))

(define-condition ax-generate-error (ax-error)
  ((cause :initarg :cause :initform nil :reader ax-generate-error-cause))
  (:documentation
   "Generation failed.  CAUSE is the original failure.

This is the reference's own AxGenerateError, the condition Core's
`intrinsic.exception.generate' produces.  It is deliberately not this port's
`generation-error': Core wraps a failure without claiming the generator's own
problem taxonomy."))

;;; ------------------------------------------------------------------
;;; Failure classification
;;;
;;; Core asks four questions about a condition, through
;;; intrinsic.exception.is_{aborted,infrastructure,refusal,validation}, and
;;; decides whether to retry, correct or give up from the answers.  The
;;; reference's answers are by error class (pyGen's _core_exception_is_*,
;;; from TS's AxGen retry policy); this port answers by kind, so the kinds it
;;; signalled before Core existed classify correctly too.
;;; ------------------------------------------------------------------

(defparameter +aborted-error-kinds+ '(:aborted)
  "Kinds that mean the caller cancelled the run.")

(defparameter +infrastructure-error-kinds+ '(:transport :network :timeout :stream)
  "Kinds the reference retries without changing the request.  A status
failure joins them only for a 5xx, which `provider-error-infrastructure-p'
checks separately.")

(defparameter +refusal-error-kinds+ '(:refusal)
  "Kinds that mean the model declined; the reference retries these inside
its validation loop rather than failing the run.")

(defun provider-error-aborted-p (condition)
  (and (typep condition 'provider-error)
       (member (provider-error-kind condition) +aborted-error-kinds+)
       t))

(defun provider-error-infrastructure-p (condition)
  "Whether CONDITION is a failure the reference retries unchanged.

A 5xx is infrastructure; a 4xx is the request's own fault and is not retried,
which is why the status is checked rather than only the kind."
  (and (typep condition 'provider-error)
       (let ((kind (provider-error-kind condition))
             (status (provider-error-status condition)))
         (cond ((member kind +infrastructure-error-kinds+) t)
               ((member kind '(:status :http))
                (and (integerp status) (<= 500 status 599)))
               (t nil)))))

(defun provider-error-refusal-p (condition)
  (and (typep condition 'provider-error)
       (member (provider-error-kind condition) +refusal-error-kinds+)
       t))

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

(defun provider-fail (kind message &key provider status code response-body request
                                        retryable cause)
  "Signal a `provider-error'.  MESSAGE must already be redacted."
  (error (make-provider-error kind message
                              :provider provider :status status :code code
                              :response-body response-body :request request
                              :retryable retryable :cause cause)))

(defun make-provider-error (kind message &key provider status code response-body request
                                              retryable cause)
  "A `provider-error' condition, not signalled.

Core's error constructors build a condition and raise it separately, so the
two have to be separable here as well."
  (make-condition 'provider-error
                  :kind kind
                  :message (if (stringp message) message (princ-to-string message))
                  :provider provider
                  :status status
                  :code code
                  :response-body response-body
                  :request request
                  :retryable retryable
                  :cause cause))

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

(defun %strip-trailing-slash (url)
  (if (and (stringp url) (> (length url) 1) (char= (char url (1- (length url))) #\/))
      (subseq url 0 (1- (length url)))
      url))

;;; `ai' lives in provider.lisp, immediately after the Core-driven client it
;;; builds. It cannot live here: it calls `provider', which is defined there,
;;; and a forward reference across files is a compile warning this package
;;; treats as an error.

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

(defvar *provider-http-method* "POST")

(defun %multipart-body (payload boundary)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((emit (text) (map nil (lambda (b) (vector-push-extend b bytes))
                              (sb-ext:string-to-octets text :external-format :utf-8)))
             (safe (text)
               (when (find-if (lambda (ch) (find ch '(#\Return #\Newline #\"))) text)
                 (provider-fail :config "Unsafe multipart field or filename."))
               text))
      (dolist (key (%object-keys payload))
        (let* ((value (gethash key payload))
               (file (and (hash-table-p value) (%present-string (jget value "data")))))
          (emit (format nil "--~a~c~cContent-Disposition: form-data; name=~s" boundary #\Return #\Newline (safe key)))
          (if file
              (progn
                (emit (format nil "; filename=~s~c~cContent-Type: ~a~c~c~c~c"
                              (safe (jget value "filename" "audio.bin")) #\Return #\Newline
                              (safe (jget value "mimeType" (jget value "mime_type" "application/octet-stream")))
                              #\Return #\Newline #\Return #\Newline))
                (map nil (lambda (b) (vector-push-extend b bytes)) (cl-base64:base64-string-to-usb8-array file)))
              (emit (format nil "~c~c~c~c~a" #\Return #\Newline #\Return #\Newline
                            (if (stringp value) value (encode-json value)))))
          (emit (format nil "~c~c" #\Return #\Newline))))
      (emit (format nil "--~a--~c~c" boundary #\Return #\Newline)))
    bytes))

(defun make-default-transport (timeout &key (method "POST") (body-kind "json") binary-response)
  "Return the default bounded HTTP transport closure."
  (lambda (url headers json-body)
    (handler-case
        (sb-ext:with-timeout timeout
          (let* ((multipart (equal body-kind "multipart"))
                 (boundary (format nil "ax-~36r-~36r" (get-universal-time) (random most-positive-fixnum)))
                 (content-type (if multipart (format nil "multipart/form-data; boundary=~a" boundary) "application/json"))
                 (content (unless (member method '("GET" "HEAD") :test #'equal)
                            (if multipart (%multipart-body (parse-json json-body) boundary) json-body))))
          (multiple-value-bind (body status response-headers)
              (drakma:http-request url
                                   :method (intern method :keyword)
                                   :additional-headers
                                   (remove "content-type" headers
                                           :key #'car :test #'string-equal)
                                   :content-type content-type
                                   :content content
                                   :external-format-out :utf-8
                                   :external-format-in :utf-8
                                   :connection-timeout timeout
                                   ;; Drakma otherwise disables certificate
                                   ;; verification even for HTTPS endpoints.
                                   :verify :required
                                   ;; Never follow redirects: Drakma would
                                   ;; replay the Authorization/x-api-key
                                   ;; header to the redirect target.
                                   :redirect nil
                                   :force-binary binary-response
                                   :want-stream nil)
            (let ((type (cdr (assoc :content-type response-headers))))
              (values (if (and binary-response (<= 200 status 299)
                               (not (and type (search "json" type))))
                          (cl-base64:usb8-array-to-base64-string body)
                          (%decode-response-body body)) status response-headers)))))
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

;;; Tool-call shape checks.
;;;
;;; The reference rejects a call whose declared type is not "function"
;;; (`axValidateChatRequestMessage' in src/ax/ai/validate.ts for a request
;;; message, `validateChatResponseFunctionCalls' for a result, ported as
;;; `ax.ai.semantic @chat_result_function_call_problems' in ir/axcore/ai.axir).
;;; Core states the rule for both call shapes: a call carrying "type" or
;;; "function" is the reference's nested shape and its "type" must be exactly
;;; "function"; a flat {id, name, arguments} call -- this port's shape -- has
;;; neither key and is checked for its id, name and arguments only.
;;;
;;; Dropping the type is not safe: a provider or a caller can present a
;;; non-function call (an OpenAI `custom' tool call, say), and rewriting it to
;;; "function" would run a handler the caller never authorized.

(defun %ai-object-has-key (object key)
  (and (hash-table-p object) (nth-value 1 (gethash key object))))

(defun %ai-json-received (object key)
  "The reference's `received:' text for OBJECT's KEY: the JSON value, or
\"undefined\" when the key is absent.  Composite values differ from the
reference only in indentation."
  (if (%ai-object-has-key object key)
      (encode-json (gethash key object))
      "undefined"))

(defun %ai-tool-call-shape-problem (call &optional (index 0) (result-index 0))
  "The first reference problem with CALL, or NIL when its shape is usable.

Returns (values message kind).  KIND is :unnamed when the reference treats
the call as one without a usable name, which a port's default corrects and
asks again, and :call when the reference's default runs the call as given;
that is Core's `chat_result_function_call_problems' classification.

MESSAGE is Core's own wording, including the call and result indices, so a
caller doing the full-batch preflight can report it verbatim."
  (macrolet ((problem (kind text &rest arguments)
               `(return-from %ai-tool-call-shape-problem
                  (values (format nil ,(concatenate 'string
                                                    "Function call at index ~a in result ~a "
                                                    text)
                                  index result-index ,@arguments)
                          ,kind))))
    (when (or (null call) (eq call :null))
      (problem :unnamed "cannot be null or undefined, received: null"))
    (unless (hash-table-p call)
      (problem :unnamed "must be an object, received: ~a" (encode-json call)))
    (let* ((nested (or (%ai-object-has-key call "function") (%ai-object-has-key call "type")))
           (id (gethash "id" call)))
      ;; The id first, as the reference checks it first.
      (unless (and (stringp id) (plusp (length (%trim id))))
        (problem :call "must have a non-empty string id, received: ~a"
                 (%ai-json-received call "id")))
      (let ((fn (gethash "function" call)))
        (when nested
          (unless (equal (gethash "type" call) "function")
            (problem :call "must have type 'function', received: ~a"
                     (%ai-json-received call "type")))
          ;; The reference's !functionCall.function: absent, null, false, 0 or
          ;; "" fails; any other value goes on to the name.
          (when (or (not (%ai-object-has-key call "function"))
                    (eq fn :null)
                    (null fn)
                    (json-false-p fn)
                    (eql fn 0)
                    (equal fn ""))
            (problem :unnamed "must have a function object, received: ~a"
                     (%ai-json-received call "function"))))
        (let* ((holder (if nested fn call))
               (name (and (hash-table-p holder) (gethash "name" holder))))
          (unless (and (stringp name) (plusp (length (%trim name))))
            (problem :unnamed "must have a non-empty function name, received: ~a"
                     (if (hash-table-p holder)
                         (%ai-json-received holder "name")
                         "undefined")))
          ;; The arguments, when given: a string or an object, as the
          ;; reference's typeof check on params.  Null and arrays are objects
          ;; in JavaScript and pass here, to be rejected by argument
          ;; validation; a number or a boolean fails now.
          (let* ((params-key (if nested "params" "arguments"))
                 (present (and (hash-table-p holder) (%ai-object-has-key holder params-key)))
                 (params (and present (gethash params-key holder))))
            (when (and present
                       (not (stringp params))
                       (not (hash-table-p params))
                       (not (and (vectorp params) (not (stringp params))))
                       (not (eq params :null)))
              (return-from %ai-tool-call-shape-problem
                (values (format nil "Function call params at index ~a in result ~a must be a ~
string or object, received: ~a"
                                index result-index (%ai-json-received holder params-key))
                        :call))))
          nil)))))

(defun tool-call-problems (calls &optional (result-index 0))
  "The reference's per-call problems for CALLS, a list or vector of tool
calls, as a vector of objects with \"index\", \"kind\" and \"message\".  Empty
when every call's shape is usable.

KIND is \"unnamed\" for a failure a port's default corrects and asks again,
and \"call\" for one it runs as given; MESSAGE is Core's own wording.  This is
the whole-batch classifier: a caller running several calls must preflight
them all here, so a malformed second call cannot leave the first one's side
effect behind.  `chat' separately refuses to let a call with an unusable
shape escape a normalized response at all."
  (let ((out '())
        (index 0))
    (map nil
         (lambda (call)
           (multiple-value-bind (message kind)
               (%ai-tool-call-shape-problem call index result-index)
             (when message
               (push (object "index" index
                             "kind" (string-downcase (symbol-name kind))
                             "message" message)
                     out)))
           (incf index))
         (if (and (vectorp calls) (not (stringp calls))) calls (coerce (or calls '()) 'vector)))
    (coerce (nreverse out) 'vector)))

(defun %ai-check-tool-call-shape (call kind index &key provider status)
  "Signal a `provider-error' of KIND when CALL's shape is unusable."
  (multiple-value-bind (message problem-kind) (%ai-tool-call-shape-problem call index)
    (declare (ignore problem-kind))
    (when message
      (provider-fail kind message :provider provider :status status))))

(defun %tool-call-fields (call &optional (index 0) provider)
  "CALL's id, name and JSON-encoded arguments, after the reference's shape
checks.  Accepts this port's flat shape and the reference's nested
{id, type, function:{name, params}} shape, which is what a Core-normalized
response carries."
  (%ai-check-tool-call-shape call :config index :provider provider)
  (let* ((nested (or (%ai-object-has-key call "function") (%ai-object-has-key call "type")))
         (holder (if nested (gethash "function" call) call))
         (id (gethash "id" call))
         (name (gethash "name" holder))
         (raw (gethash (if nested "params" "arguments") holder)))
    (let ((arguments (cond ((stringp raw) raw)
                           ((or (null raw) (eq raw :null)) "{}")
                           (t (encode-json raw)))))
      (values id name arguments))))

(defun usage-object (prompt completion &optional total)
  "A token-usage object in this port's public shape.

Kept here because it is the shape every public surface reports usage in --
the generator, the refiner, the synthesiser and the flow all build it -- even
though the hand-written provider mapping that first needed it is gone."
  (object "promptTokens" (or prompt 0)
          "completionTokens" (or completion 0)
          "totalTokens" (or total (+ (or prompt 0) (or completion 0)))))

(defun %integer-or-zero (value)
  (if (integerp value) value 0))

;;; ------------------------------------------------------------------
;;; chat
;;; ------------------------------------------------------------------

(defun %resolve-tool-choice (tool-choice provider)
  (case tool-choice
    ((nil :auto) :auto)
    (:none :none)
    (t (provider-fail :config "chat: :tool-choice must be :auto or :none."
                      :provider provider))))

(defun %native-messages-to-core (messages)
  "This port's message objects as Core's chat_prompt entries."
  (let ((out (%new-array)))
    (map nil
         (lambda (message)
           (let* ((role (%present (jget message "role")))
                  (content (%present (jget message "content")))
                  (calls (%present (jget message "toolCalls")))
                  (call-id (%present (jget message "toolCallId")))
                  (entry (%new-object)))
             (cond
               ((equal role "tool")
                (%set-key entry "role" "function")
                (%set-key entry "functionId" (or call-id ""))
                (%set-key entry "result" (if (stringp content) content "")))
               (t
                (%set-key entry "role" (or role "user"))
                (%set-key entry "content" (if (stringp content) content ""))
                (when (and calls (plusp (length calls)))
                  ;; Core's chat_prompt carries the reference's nested shape.
                  (let ((nested (%new-array)))
                    (map nil
                         (lambda (call)
                           (multiple-value-bind (id name arguments) (%tool-call-fields call)
                             (let ((item (%new-object))
                                   (fn (%new-object)))
                               (%set-key fn "name" name)
                               (%set-key fn "params" arguments)
                               (%set-key item "id" id)
                               (%set-key item "type" "function")
                               (%set-key item "function" fn)
                               (vector-push-extend item nested))))
                         calls)
                    (%set-key entry "functionCalls" nested)))))
             (vector-push-extend entry out)))
         (if (and (vectorp messages) (not (stringp messages)))
             messages
             (coerce (or messages (list)) 'vector)))
    out))

(defun %completion-to-native (completion)
  "Core's flat completion object as this port's normalized chat response.

Core's completion already uses the flat call shape (id, name, params), which
is this port's own; only the key names and the usage totals differ."
  (let* ((content (let ((value (%present (jget completion "content"))))
                    (if (stringp value) value "")))
         (calls (%present (jget completion "function_calls")))
         (usage (%present-object (jget completion "usage") "usage" nil))
         (tokens (and usage (or (%present-object (jget usage "tokens") "tokens" nil) usage)))
         (normalized (%new-array)))
    (when calls
      (map nil
           (lambda (call)
             (let ((params (%present (jget call "params")))
                   (entry (%new-object)))
               (%set-key entry "id" (or (%present-string (jget call "id")) :null))
               (%set-key entry "name" (or (%present-string (jget call "name")) :null))
               (%set-key entry "arguments"
                         (cond ((stringp params) params)
                               ((or (null params) (eq params :null)) "{}")
                               (t (encode-json params))))
               (%ai-check-tool-call-shape entry :response (length normalized))
               (vector-push-extend entry normalized)))
           calls))
    (flet ((count-of (&rest names)
             (or (and tokens
                      (loop for name in names
                            for candidate = (%present (jget tokens name))
                            when (integerp candidate) return candidate))
                 0)))
      (object "content" content
              "toolCalls" normalized
              "finishReason" (or (%present-string (jget completion "finish_reason")) "")
              "usage" (usage-object (count-of "promptTokens" "prompt_tokens")
                                    (count-of "completionTokens" "completion_tokens")
                                    (and tokens
                                         (let ((value (or (%present (jget tokens "totalTokens"))
                                                          (%present (jget tokens "total_tokens")))))
                                           (and (integerp value) value))))))))

(defun chat-response-to-native (response)
  "Core's chat response object as this port's normalized response.

Answers an object with \"content\", \"toolCalls\", \"usage\" and
\"finishReason\" -- the shape `chat' has always returned and the shape the
generator, refiner, synthesiser and flow all record.

`ax-chat' answers Core's shape for every service, because that is the shape
Core's own operations consume.  A caller that wants the port's shape converts
here rather than reading Core's keys inline: one mapping, in one place, so a
change to Core's response shape does not have to be chased through every
caller.  The conversion goes through Core's own `chat-response-to-completion',
so folding, usage totals and the flat call shape are Core's decisions.

IT COLLAPSES A MULTI-RESULT RESPONSE INTO ONE COMPLETION.  That is right for a
caller that wants a single answer, which is most of them, and wrong for a
caller that needs the samples: a multi-sample response loses every candidate
but the first, so a result picker has nothing to choose between and a failure
report quotes one candidate instead of all of them.  A caller that samples must
read Core's response as it arrives, through the \"results\" array, rather than
convert here first."
  (%completion-to-native (axllm/core::chat-response-to-completion response)))

(defun %service-chat (service messages tools tool-choice model)
  "One chat turn against any service, in this port's normalized shape.

Core's own request and completion objects are the bridge, so there is one
provider stack rather than a legacy path and a Core path that can drift."
  (let ((request (%new-object)))
    (%set-key request "chat_prompt" (%native-messages-to-core messages))
    (let ((functions (%as-list tools)))
      (when functions
        (%set-key request "functions" (coerce functions 'vector))
        (%set-key request "function_call"
                  (ecase (%resolve-tool-choice tool-choice (ax-service-name service))
                    (:auto "auto") (:none "none")))))
    (when (and model (not (%blankp model)))
      (%set-key request "model" (%trim model)))
    (%completion-to-native (ax-complete service request))))

;;; A note on the two directions, because getting them backwards is what caused
;;; a silent empty answer once already:
;;;   ax-chat   answers Core's shape   (results, model_usage)
;;;   chat      answers this port's    (content, toolCalls, usage, finishReason)
;;; chat-response-to-native converts the first into the second, and
;;; Nothing else should be translating between them.

(defgeneric ax-credential (service)
  (:documentation
   "SERVICE's credential, for redaction only.

`chat' needs it to guarantee that no condition it lets out can carry an API
key, whatever path raised it.  A service with no credential answers NIL.")
  (:method ((service t)) nil))

(defun chat (client messages &key tools tool-choice model)
  "Send MESSAGES to CLIENT synchronously and return a normalized object with
\"content\", \"toolCalls\", \"usage\" and \"finishReason\".

CLIENT is any Ax service: a provider client from `ai' or `provider', a run
boundary, a router, a balancer or a mock.  MESSAGES is a list or vector of
normalized message objects.  TOOLS is a list or vector of tool specs (see
`tool').  TOOL-CHOICE is :auto (the default) or :none; :none keeps the tool
definitions in the request, which the providers require while the history
still contains tool calls and results, while forbidding a new call.  MODEL
overrides the client's model for this call only; the client is unchanged.

No request is ever retried automatically, and no condition signalled from here
carries an API key or a provider response body."
  (when (and model (%blankp model))
    ;; Checked first and for every service: a blank override is a caller
    ;; mistake, and quietly falling back to the client's own model would answer
    ;; from a different model than the caller asked for.
    (provider-fail :config "chat: :model must be a non-empty string when given."))
  (let ((secret (ax-credential client)))
    (handler-bind
        ((provider-error
           (lambda (condition)
             ;; Last line of defence, kept from the first version of this
             ;; function: whatever path raised this, the credential cannot
             ;; appear in the message a caller sees or logs.
             (let* ((text (princ-to-string condition))
                    (clean (%redact text secret)))
               (unless (equal text clean)
                 (error 'provider-error
                        :kind (provider-error-kind condition)
                        :provider (provider-error-provider condition)
                        :status (provider-error-status condition)
                        :code (provider-error-code condition)
                        :retryable (provider-error-retryable-p condition)
                        :message clean))))))
      ;; One stack: every client is a service, so there is no second path.
      (%service-chat client messages tools tool-choice model))))

;;; ------------------------------------------------------------------
;;; Cancellation
;;;
;;; Reference: pyAI.py's AxCancellationToken.  A token is cooperative: it
;;; records that the caller asked to stop and wakes anything waiting, and it
;;; never claims an external action was undone.
;;; ------------------------------------------------------------------

(defclass cancellation-token ()
  ((lock :initform (sb-thread:make-mutex :name "ax-cancellation") :reader %token-lock)
   (gate :initform (sb-thread:make-waitqueue) :reader %token-gate)
   (cancelled :initform nil :accessor %token-cancelled)
   (reason :initform nil :accessor %token-reason)
   (subscribers :initform '() :accessor %token-subscribers))
  (:documentation
   "A cooperative cancellation signal.

CANCEL records the request and wakes every waiter exactly once; a second
CANCEL is a no-op and answers NIL.  WAIT returns as soon as the token is
cancelled, so a backoff does not outlive the run that asked to stop."))

(defun cancellation-token ()
  "A fresh, uncancelled `cancellation-token'."
  (make-instance 'cancellation-token))

(defun cancelled-p (token)
  (and token (sb-thread:with-mutex ((%token-lock token)) (%token-cancelled token))))

(defun cancellation-reason (token)
  (and token (sb-thread:with-mutex ((%token-lock token)) (%token-reason token))))

(defun cancel (token &optional (reason "cancelled"))
  "Ask the run to stop.  Returns T the first time and NIL afterwards."
  (let ((listeners nil))
    (sb-thread:with-mutex ((%token-lock token))
      (when (%token-cancelled token)
        (return-from cancel nil))
      (setf (%token-cancelled token) t
            (%token-reason token) reason
            listeners (reverse (%token-subscribers token)))
      ;; The listeners are spent: each runs exactly once, so keeping them would
      ;; leak them for the token's lifetime and make the subscription count
      ;; report work that can never happen again.
      (setf (%token-subscribers token) '())
      (sb-thread:condition-broadcast (%token-gate token)))
    ;; Outside the lock: a listener must not be able to deadlock the token it
    ;; is reacting to.
    (dolist (listener listeners)
      (handler-case (funcall listener) (error () nil)))
    t))

(defun cancellation-wait (token &optional timeout)
  "Block until TOKEN is cancelled or TIMEOUT seconds pass.  Answers whether
the token is cancelled.  A NIL TOKEN waits out the timeout, so a caller does
not need two code paths."
  (if (null token)
      (progn (when (and timeout (plusp timeout)) (sleep timeout)) nil)
      (sb-thread:with-mutex ((%token-lock token))
        (loop
          (when (%token-cancelled token) (return t))
          (unless (sb-thread:condition-wait (%token-gate token) (%token-lock token)
                                            :timeout timeout)
            (return (%token-cancelled token)))))))

(defun throw-if-cancelled (token &optional provider)
  "Signal an :aborted `provider-error' when TOKEN is cancelled."
  (when (cancelled-p token)
    (provider-fail :aborted
                   (format nil "Run aborted: ~a" (or (cancellation-reason token) "cancelled"))
                   :provider provider))
  token)

(defun cancellation-subscribe (token listener)
  "Call LISTENER once when TOKEN is cancelled, or now when it already is.
Returns a function that removes the subscription."
  (let ((already nil))
    (sb-thread:with-mutex ((%token-lock token))
      (if (%token-cancelled token)
          (setf already t)
          (push listener (%token-subscribers token))))
    (when already (handler-case (funcall listener) (error () nil)))
    (lambda ()
      (sb-thread:with-mutex ((%token-lock token))
        (setf (%token-subscribers token) (remove listener (%token-subscribers token))))
      nil)))

(defun cancellation-subscription-count (token)
  (sb-thread:with-mutex ((%token-lock token)) (length (%token-subscribers token))))

(defun %call-cancellation (options)
  "The cancellation token in OPTIONS, under any of the names the reference
accepts.  Signals when the value is not a token: silently ignoring it would
make a cancelled run look like a running one."
  (when (hash-table-p options)
    (let ((value (or (%present (jget options "cancellation"))
                     (%present (jget options "cancellationToken"))
                     (%present (jget options "cancellation_token")))))
      (cond ((null value) nil)
            ((typep value 'cancellation-token) value)
            (t (provider-fail :config
                              "cancellation must be a cancellation-token."))))))

;;; ------------------------------------------------------------------
;;; Token-usage rate limiter
;;;
;;; Reference: src/ax/util/rate-limit.ts AxRateLimiterTokenUsage.  A leaky
;;; bucket of MAX-TOKENS refilling at REFILL-RATE tokens a second.
;;;
;;; The oversized-request rule is the part a callback hook cannot express: a
;;; single request may legitimately ask for more than the bucket can ever
;;; hold, and waiting for that many tokens would never finish, because a
;;; refill caps the bucket at MAX-TOKENS.  So the limiter waits for a full
;;; bucket, then lets the balance go negative and repays the borrowed
;;; capacity out of later refills, which keeps the average rate honest.
;;; ------------------------------------------------------------------

(defclass rate-limiter-token-usage ()
  ((max-tokens :initarg :max-tokens :reader rate-limiter-max-tokens)
   (refill-rate :initarg :refill-rate :reader rate-limiter-refill-rate)
   (current :initarg :current :accessor %limiter-current)
   (last-refill :initarg :last-refill :accessor %limiter-last-refill)
   (debug :initarg :debug :initform nil :reader %limiter-debug)
   (lock :initform (sb-thread:make-mutex :name "ax-rate-limiter") :reader %limiter-lock))
  (:documentation
   "A token-usage rate limiter.  ACQUIRE blocks until the bucket can pay for
the request, and repays an oversized request's borrowed capacity from later
refills instead of deadlocking on it."))

(defun rate-limiter-token-usage (max-tokens refill-rate &key debug)
  "A limiter of MAX-TOKENS refilling at REFILL-RATE tokens a second."
  (unless (and (realp max-tokens) (plusp max-tokens))
    (provider-fail :config "rate-limiter-token-usage: max-tokens must be positive."))
  (unless (and (realp refill-rate) (plusp refill-rate))
    (provider-fail :config "rate-limiter-token-usage: refill-rate must be positive."))
  (make-instance 'rate-limiter-token-usage
                 :max-tokens max-tokens
                 :refill-rate refill-rate
                 :current max-tokens
                 :debug debug
                 :last-refill (%monotonic-seconds)))

(defun %monotonic-seconds ()
  (/ (float (get-internal-real-time) 1.0d0) internal-time-units-per-second))

(defun %limiter-refill (limiter now)
  "Add the tokens LIMITER earned since its last refill, capped at the bucket."
  (let* ((elapsed (max 0 (- now (%limiter-last-refill limiter))))
         (earned (* elapsed (rate-limiter-refill-rate limiter))))
    (setf (%limiter-current limiter)
          (min (rate-limiter-max-tokens limiter) (+ (%limiter-current limiter) earned))
          (%limiter-last-refill limiter) now)
    (%limiter-current limiter)))

(defun rate-limiter-available (limiter)
  "LIMITER's balance right now, after accounting for refills.  Negative while
an oversized request's borrowed capacity is still being repaid."
  (sb-thread:with-mutex ((%limiter-lock limiter))
    (%limiter-refill limiter (%monotonic-seconds))))

(defun rate-limiter-acquire (limiter tokens &key cancellation)
  "Block until LIMITER can pay for TOKENS, then charge them.

A request bigger than the bucket waits for a full bucket and then borrows:
the balance goes negative and later refills repay it, so the average rate
still holds.  Returns the seconds spent waiting."
  (let ((tokens (max 0 (or tokens 0)))
        (started (%monotonic-seconds)))
    (loop
      (throw-if-cancelled cancellation)
      (let ((wait nil))
        (sb-thread:with-mutex ((%limiter-lock limiter))
          (let* ((now (%monotonic-seconds))
                 (available (%limiter-refill limiter now))
                 ;; Never wait for more than the bucket can ever hold.
                 (required (min tokens (rate-limiter-max-tokens limiter))))
            (if (>= available required)
                (progn (decf (%limiter-current limiter) tokens)
                       (return (- (%monotonic-seconds) started)))
                (setf wait (/ (- required available)
                              (rate-limiter-refill-rate limiter))))))
        (when (%limiter-debug limiter)
          (format *error-output* "~&Rate limiter: waiting ~,3fs~%" wait))
        ;; A cancelled run stops waiting immediately rather than sleeping out
        ;; the whole backoff.
        (cancellation-wait cancellation (max 0.001 (min wait 0.1)))))))

;;; ------------------------------------------------------------------
;;; The native service protocol
;;;
;;; Every Ax service object -- a provider client, a router, a balancer, a
;;; run-control boundary, a mock -- answers the same generic functions, so
;;; Gen, Flow and Agent dispatch on the object instead of checking its class.
;;;
;;; OPTIONS is always the call's option object (a JSON object) or NIL, and
;;; REQUEST is always Core's chat request object.  A method must not mutate
;;; either.
;;; ------------------------------------------------------------------

(defgeneric ax-service-name (service)
  (:documentation "SERVICE's provider name, as a string."))

(defgeneric ax-id (service)
  (:documentation "SERVICE's stable identity, as a string."))

(defgeneric ax-features (service &optional model)
  (:documentation
   "SERVICE's capability object for MODEL, as Core's feature map.  A key the
service does not claim is absent rather than false, because Core applies its
own default for an unknown client only when the key is missing."))

(defgeneric ax-chat (service request &optional options)
  (:documentation
   "One chat turn.  Returns Core's normalized chat response object."))

(defgeneric ax-stream (service request &optional options)
  (:documentation
   "A chat turn as a stream handle: pass it to `ax-stream-next' until that
answers NIL, then `ax-stream-close'.  A service without native streaming
answers a handle over its single response."))

(defgeneric ax-stream-next (handle)
  (:documentation
   "HANDLE's next chunk, or :NULL once the stream is exhausted.  :NULL is
Core's none, so a generated loop ends on it."))

(defgeneric ax-stream-close (handle)
  (:documentation
   "Release HANDLE.  Closing an abandoned stream is best effort: a transport
that already failed must not turn cleanup into a second failure."))

(defgeneric ax-embed (service request &optional options)
  (:documentation "Embeddings.  Returns Core's normalized embed response."))

(defgeneric ax-transcribe (service request &optional options)
  (:documentation "Audio transcription."))

(defgeneric ax-speak (service request &optional options)
  (:documentation "Speech synthesis."))

(defgeneric ax-complete (service request)
  (:documentation
   "One turn in Core's completion shape, for a service that has no chat."))

(defgeneric ax-metrics (service)
  (:documentation "SERVICE's latency and error metrics object."))

(defgeneric ax-options (service)
  (:documentation "SERVICE's option object."))

(defgeneric (setf ax-options) (options service)
  (:documentation "Replace SERVICE's option object."))

(defgeneric ax-estimated-cost (service &optional model-usage)
  (:documentation "SERVICE's estimated cost for MODEL-USAGE."))

(defgeneric ax-owned-worker-factory (service)
  (:documentation
   "A function of no arguments returning a service this run owns, or NIL when
SERVICE may be shared across workers as it is."))


(defmethod ax-owned-worker-factory ((service t)) nil)
(defmethod ax-estimated-cost ((service t) &optional model-usage)
  (declare (ignore model-usage))
  0)
(defmethod ax-metrics ((service t)) (default-metrics))
(defmethod ax-options ((service t)) (object))
(defmethod (setf ax-options) (options (service t))
  (declare (ignore service))
  options)

(defun default-metrics ()
  "A fresh metrics object in the reference's shape."
  (flet ((latency () (object "mean" 0 "p95" 0 "p99" 0 "samples" (%new-array)))
         (errors () (object "count" 0 "rate" 0 "total" 0)))
    (object "latency" (object "chat" (latency) "embed" (latency))
            "errors" (object "chat" (errors) "embed" (errors)))))

(defun %core-message-to-native (message)
  "One of Core's chat_prompt entries as this port's message object."
  (let* ((role (%present (jget message "role")))
         (content (%present (jget message "content")))
         (calls (or (%present (jget message "functionCalls"))
                    (%present (jget message "function_calls")))))
    (cond
      ((equal role "function")
       (message "tool" (or (%present (jget message "result")) "")
                :tool-call-id (or (%present (jget message "functionId"))
                                  (%present (jget message "function_id")))))
      ((equal role "assistant")
       (message "assistant" (if (stringp content) content "")
                :tool-calls (and calls (coerce calls 'vector))))
      (t (message (or role "user") (if (stringp content) content ""))))))

;;; ------------------------------------------------------------------
;;; Core host boundaries: ai.* , exception.* , retry.*
;;;
;;; Names derive from Core's intrinsic table exactly as the other targets'
;;; do.  Arities are the ones `lisp-core --verify-runtime' reports, so a Core
;;; body that widens its call fails loudly instead of silently.
;;; ------------------------------------------------------------------

(in-package #:axllm/core)

(defun core-ai-error-response (message &optional (response-body :null))
  (axllm::make-provider-error :response (core-js-text message)
                              :response-body (axllm::%present response-body)))

(defun core-ai-error-refusal (message &optional (response-body :null))
  (axllm::make-provider-error :refusal (core-js-text message)
                              :response-body (axllm::%present response-body)))

(defun core-ai-error-stream (message &optional (response-body :null) (retryable 'yason:true))
  (axllm::make-provider-error :stream (core-js-text message)
                              :response-body (axllm::%present response-body)
                              :retryable (core-true-p retryable)))

(defun core-ai-error-unsupported (message)
  (axllm::make-provider-error :unsupported (core-js-text message)))

(defun core-ai-error-auth (message &optional (status :null) (code :null)
                                     (response-body :null) (request :null))
  (axllm::make-provider-error :auth (core-js-text message)
                              :status (axllm::%present status)
                              :code (axllm::%present code)
                              :response-body (axllm::%present response-body)
                              :request (axllm::%present request)))

(defun core-ai-error-timeout (message &optional (status :null) (code :null)
                                        (response-body :null) (request :null)
                                        (retryable 'yason:true))
  (axllm::make-provider-error :timeout (core-js-text message)
                              :status (axllm::%present status)
                              :code (axllm::%present code)
                              :response-body (axllm::%present response-body)
                              :request (axllm::%present request)
                              :retryable (core-true-p retryable)))

(defun core-ai-error-status (message &optional (status :null) (code :null)
                                       (response-body :null) (request :null)
                                       (retryable 'yason:false))
  (axllm::make-provider-error :status (core-js-text message)
                              :status (axllm::%present status)
                              :code (axllm::%present code)
                              :response-body (axllm::%present response-body)
                              :request (axllm::%present request)
                              :retryable (core-true-p retryable)))

(defvar *ai-warnings-shown* (make-hash-table :test 'equal :synchronized t)
  "Keys already warned about, so a setting Ax could not send is reported once
per image rather than once per request.")

(defvar *ai-warning-sink* nil
  "When set, a function of one string that receives warnings instead of
*ERROR-OUTPUT*.  The conformance runner collects them this way.")

(defun core-ai-warn-once (key message)
  "Report MESSAGE once for KEY, as the reference's console.warn does."
  (let ((key (core-js-text key)))
    (unless (gethash key *ai-warnings-shown*)
      (setf (gethash key *ai-warnings-shown*) t)
      (let ((text (core-js-text message)))
        (if *ai-warning-sink*
            (funcall *ai-warning-sink* text)
            (format *error-output* "~&Ax: ~a~%" text)))))
  :null)

(defun core-ai-capture-warnings (sink)
  "Send later one-time warnings to SINK and forget the keys already shown."
  (clrhash *ai-warnings-shown*)
  (setf *ai-warning-sink* sink))

(defun core-ai-client-features (client model)
  "CLIENT's capability map for MODEL.

Core applies its own compatibility default for a client that does not report
features, so an unknown object answers with an empty map rather than a
fabricated capability set."
  (let ((model (axllm::%present model)))
    (if (null client)
        (core-new-map)
        (handler-case (or (axllm::ax-features client model) (core-new-map))
          (error () (core-new-map))))))

(defun core-ai-complete-once (client request options)
  "One model turn in Core's completion shape.

A streamed request folds its chunks into a single response first, as the
reference's streamed forward does, so a caller that asked for streaming and a
caller that did not get the same completion."
  (let* ((config (core-get request "model_config" (core-new-map)))
         (streaming (core-true-p (core-get config "stream" 'yason:false))))
    (if streaming
        (let ((handle (core-ai-stream-open client request options))
              (chunks (core-new-list)))
          (unwind-protect
               (loop for chunk = (core-ai-stream-next handle)
                     until (or (null chunk) (eq chunk :null))
                     do (core-append chunks chunk))
            (core-ai-stream-close handle))
          (chat-response-to-completion (fold-chat-response-stream chunks)))
        (chat-response-to-completion (axllm::ax-chat client request options)))))

;;; core-exception-message belongs to core-runtime.lisp: its definition there
;;; already handles an Ax error, a plain condition, a string and a message map
;;; with a fallback, and is covered by that file's own regression tests.  This
;;; file deliberately does not define a second, narrower one.

(defun core-exception-rewrap (condition message)
  "The same failure with a new message, keeping its class so a handler that
caught it before still catches it, and keeping the original as its cause."
  (let ((text (core-js-text message)))
    (if (typep condition 'axllm::provider-error)
        (axllm::make-provider-error (axllm::provider-error-kind condition) text
                                    :provider (axllm::provider-error-provider condition)
                                    :status (axllm::provider-error-status condition)
                                    :code (axllm::provider-error-code condition)
                                    :response-body (axllm::provider-error-response-body condition)
                                    :request (axllm::provider-error-request condition)
                                    :retryable (axllm::provider-error-retryable-p condition)
                                    :cause condition)
        (make-condition 'axllm::ax-generate-error :message text :cause condition))))

(defun core-exception-generate (condition message)
  (make-condition 'axllm::ax-generate-error
                  :message (core-js-text message)
                  :cause condition))

(defun core-exception-is-aborted (condition)
  (core-bool (axllm::provider-error-aborted-p condition)))

(defun core-exception-is-infrastructure (condition)
  (core-bool (axllm::provider-error-infrastructure-p condition)))

(defun core-exception-is-refusal (condition)
  (core-bool (axllm::provider-error-refusal-p condition)))

(defun core-exception-is-validation (condition)
  (core-bool (typep condition 'axllm::validation-error)))

(defun core-retry-sleep (attempt &optional client options)
  "Wait before retrying ATTEMPT, as the reference's backoff does: a quarter
second per attempt, capped at one second.  A cancelled run stops waiting at
once and fails rather than sleeping out the backoff."
  (declare (ignore client))
  (let* ((attempt (if (realp attempt) attempt 0))
         (delay (min (* 0.25d0 (+ attempt 1)) 1.0d0))
         (token (axllm::%call-cancellation (axllm::%present options))))
    (axllm::cancellation-wait token delay)
    (axllm::throw-if-cancelled token))
  :null)

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Stream handles
;;;
;;; Reference: pyGen.py's _CoreChatStream, a pull handle whose next() answers
;;; the next chunk or nothing.  A service that cannot stream still answers a
;;; handle, over its one response, so a streamed forward has a single shape
;;; to read.
;;; ------------------------------------------------------------------

(defclass ax-stream-handle ()
  ((next :initarg :next :reader %handle-next)
   (closer :initarg :closer :initform nil :reader %handle-closer)
   (closed :initform nil :accessor %handle-closed))
  (:documentation
   "A pull handle over a chat stream.  NEXT is a function of no arguments
answering the next chunk or :NULL; CLOSER, when given, releases the
underlying transport."))

(defun make-ax-stream-handle (next &key closer)
  (make-instance 'ax-stream-handle :next next :closer closer))

(defun ax-stream-handle-over (chunks)
  "A handle that answers each of CHUNKS in turn.  Used for a service whose
answer is already complete."
  (let ((rest (coerce chunks 'list)))
    (make-ax-stream-handle (lambda () (if rest (pop rest) :null)))))

(defmethod ax-stream-next ((handle ax-stream-handle))
  (if (%handle-closed handle)
      :null
      (let ((chunk (funcall (%handle-next handle))))
        (if (null chunk) :null chunk))))

(defmethod ax-stream-close ((handle ax-stream-handle))
  (unless (%handle-closed handle)
    (setf (%handle-closed handle) t)
    (let ((closer (%handle-closer handle)))
      (when closer (handler-case (funcall closer) (error () nil)))))
  :null)

(defmethod ax-stream ((service t) request &optional options)
  "The default stream: one chunk, the service's single chat response.

A service that streams natively overrides this; one that does not must still
answer a handle, because Core reads every streamed turn the same way."
  (ax-stream-handle-over (list (ax-chat service request options))))

;;; ------------------------------------------------------------------
;;; Core host boundaries: ai.stream.* and stream.*
;;; ------------------------------------------------------------------

(in-package #:axllm/core)

(defun core-ai-stream-open (client request options)
  (axllm::ax-stream client request (axllm::%present options)))

(defun core-ai-stream-next (handle)
  (axllm::ax-stream-next handle))

(defun core-ai-stream-close (handle)
  (axllm::ax-stream-close handle))

(defun core-stream-event-content-parts (event)
  "EVENT's content strings, in order.

Reference: pyGen.py's _core_stream_event_content_parts.  A terminal event has
no content; a normalized response answers one string per result; anything
else answers its first content-bearing field."
  (let ((out (core-new-list)))
    (cond
      ((stringp event) (core-append out event))
      ((not (core-object-p event)) out)
      (t
       (let* ((nested (core-get event "data" :null))
              (data (if (core-object-p nested) nested event))
              (kind (core-get data "type" :null)))
         (unless (or (equal kind "done") (equal kind "message_stop"))
           (let ((results (core-get data "results" :null)))
             (if (core-true-p results)
                 (dolist (result (core-elements results))
                   (let ((content (core-get result "content" :null)))
                     (core-append out (if (stringp content) content ""))))
                 (let ((value (or (axllm::%present-string (core-get data "delta" :null))
                                  (axllm::%present-string (core-get data "content_delta" :null))
                                  (axllm::%present-string (core-get data "contentDelta" :null))
                                  (axllm::%present-string (core-get data "text" :null))
                                  (axllm::%present-string (core-get data "content" :null)))))
                   (core-append out (or value "")))))))))
    out))

(in-package #:axllm)
