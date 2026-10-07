;;;; provider.lisp --- tests for the native provider / generation / tool layer.
;;;;
;;;; Entry point for the repository runner:
;;;;
;;;;   (axllm:run-provider-tests)            ; => (values passed failed)
;;;;   (axllm:run-provider-tests-or-die)     ; exits non-zero on failure
;;;;
;;;; Coverage is by injected scripted transport (exact request bodies and
;;;; normalized outputs for both providers) plus real Drakma requests against a
;;;; loopback HTTP server (success, bounded timeout, and no credential
;;;; forwarding on redirect).  No network egress and no provider credentials
;;;; are required.

(in-package #:axllm)

(export '(run-provider-tests run-provider-tests-or-die))

(defun %join-lines (lines &key (trailing t))
  "LINES joined with newlines, with a trailing newline unless TRAILING is nil."
  (with-output-to-string (out)
    (loop for line in lines
          for first = t then nil
          do (unless first (terpri out))
             (write-string line out))
    (when trailing (terpri out))))

(defparameter +test-openai-model+ "gpt-5.4-mini"
  "The OpenAI-compatible model these tests name.

Deliberately a Chat Completions model.  A gpt-6 family model selects the
Responses dialect, where the request carries \"input\" rather than \"messages\", so
a case that reads the prompt structure would be asserting the dialect rather than
the behaviour it is about.  A case that wants the Responses dialect names
+responses-dialect-model+ and says so.")

(defparameter +responses-dialect-model+ "gpt-6-luna"
  "A model whose profile selects the Responses dialect.

Named so a case that is about that dialect says which one it means, instead of
depending on whichever model the suite happens to default to.")

(defparameter +test-anthropic-model+ "claude-fable-5-1")

(define-condition test-failure (error)
  ((text :initarg :text :reader test-failure-text))
  (:report (lambda (c s) (write-string (test-failure-text c) s))))

(defvar *tests* '())

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *tests* (append (remove ',name *tests* :key #'car) (list (cons ',name #',name))))
     ',name))

(defun expect (ok description)
  (unless ok (error 'test-failure :text description))
  t)

(defun expect-equal (actual expected description)
  (expect (equal actual expected)
          (format nil "~a (expected ~s, got ~s)" description expected actual)))

(defun expect-contains (haystack needle description)
  (expect (and (stringp haystack) (search needle haystack))
          (format nil "~a (~s not found in ~s)" description needle haystack)))

(defun expect-not-contains (haystack needle description)
  (expect (not (and (stringp haystack) (search needle haystack)))
          (format nil "~a (~s unexpectedly present)" description needle)))

(defmacro expect-error (type kind-reader kind &body body)
  "Run BODY, require a TYPE condition whose KIND-READER returns KIND, and
return the condition."
  (let ((c (gensym)))
    `(handler-case (progn ,@body
                          (error 'test-failure
                                 :text (format nil "expected a ~a of kind ~a, nothing signalled"
                                               ',type ',kind)))
       (,type (,c)
         (expect (eq (,kind-reader ,c) ,kind)
                 (format nil "expected ~a kind ~a, got ~a: ~a"
                         ',type ,kind (,kind-reader ,c) ,c))
         ,c))))

;;; ------------------------------------------------------------------
;;; Scripted transport
;;; ------------------------------------------------------------------

(defstruct (script (:conc-name script-)) queue (calls '()))

(defun make-scripted-transport (responses)
  "RESPONSES is a list of body strings or (status . body) conses.  Returns
\(values transport script)."
  (let ((script (make-script :queue (copy-list responses))))
    (values (lambda (url headers json-body)
              (push (list url headers json-body) (script-calls script))
              (let ((next (if (script-queue script)
                              (pop (script-queue script))
                              (error "scripted transport exhausted"))))
                (if (consp next)
                    (values (cdr next) (car next))
                    (values next 200))))
            script)))

(defun script-call-count (script) (length (script-calls script)))

(defun script-request (script n)
  "Parsed request body of the Nth (0-based) call."
  (let ((calls (reverse (script-calls script))))
    (expect (< n (length calls)) (format nil "expected at least ~a transport call(s)" (1+ n)))
    (parse-json (third (nth n calls)))))

(defun script-request-text (script n)
  (third (nth n (reverse (script-calls script)))))

(defun script-headers (script n)
  (second (nth n (reverse (script-calls script)))))

(defun absent (value)
  "True when a JSON field is missing.  Tolerates a foundation whose `jget'
returns :null rather than NIL for an absent key."
  (or (null value) (eq value :null)))

(defun header-value (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; ------------------------------------------------------------------
;;; Canned provider payloads
;;; ------------------------------------------------------------------

(defun openai-text-response (content &key (finish "stop") (prompt 11) (completion 7))
  (encode-json
   (object "choices" (vector (object "index" 0
                                     "finish_reason" finish
                                     "message" (object "role" "assistant" "content" content)))
           "usage" (object "prompt_tokens" prompt "completion_tokens" completion
                           "total_tokens" (+ prompt completion)))))

(defun openai-tool-response (calls &key (content ""))
  (encode-json
   (object "choices" (vector (object "index" 0
                                     "finish_reason" "tool_calls"
                                     "message" (object "role" "assistant"
                                                       "content" (if (string= content "") :null content)
                                                       "tool_calls" (coerce calls 'vector))))
           "usage" (object "prompt_tokens" 5 "completion_tokens" 3 "total_tokens" 8))))

(defun openai-tool-call (id name arguments-json)
  (object "id" id "type" "function"
          "function" (object "name" name "arguments" arguments-json)))

(defun anthropic-text-response (content &key (stop "end_turn"))
  (encode-json
   (object "id" "msg_1" "role" "assistant" "stop_reason" stop
           "content" (vector (object "type" "text" "text" content))
           "usage" (object "input_tokens" 13 "output_tokens" 4))))

(defun scripted-client (provider responses &key (api-key "sk-test-dummy-key") model)
  (multiple-value-bind (transport script) (make-scripted-transport responses)
    (values (ai :name provider
                :model (or model (if (string= provider "anthropic")
                                     +test-anthropic-model+
                                     +test-openai-model+))
                :api-key api-key
                :transport transport)
            script)))

;;; ------------------------------------------------------------------
;;; Signatures used by the tests
;;; ------------------------------------------------------------------

(defparameter +jiti-signature+
  (concatenate 'string
               "observation:string -> action:class \"develop, execute, resume, abort\", "
               "source?:string, preview?:boolean, restartId?:string, arguments?:string"))

;;; ------------------------------------------------------------------
;;; Client construction
;;; ------------------------------------------------------------------

(deftest test-a-client-names-its-provider-and-model
  ;; The factory resolves a provider name through Core's profile table, so an
  ;; unknown name is refused there rather than by a hand-written allowlist.
  (expect (handler-case (progn (ai :name "mystery" :model "m" :api-key "sk-x") nil)
            (error () t))
          "an unknown provider name is refused")
  ;; A model is no longer required at construction: a profile supplies its own
  ;; default.  What matters to a caller is that the request names one, so that is
  ;; what this asserts instead of pretending construction refuses.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (forward (ax "q:string -> answer:string") client (object "q" "go"))
    (let ((model (jget (script-request script 0) "model")))
      (expect (and (stringp model) (not (%blankp model)))
              (format nil "the request names a model, got ~s" model))))
  ;; An explicit model wins over the profile's default.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok"))
                       :model "gpt-5.4-mini")
    (forward (ax "q:string -> answer:string") client (object "q" "go"))
    (expect-equal (jget (script-request script 0) "model") "gpt-5.4-mini"
                  "an explicit model is the one sent")))

(deftest test-missing-auth-rejected-for-real-requests
  (let ((saved (uiop:getenv "OPENAI_API_KEY")))
    (unwind-protect
         (progn
           (ignore-errors (funcall (find-symbol "SETENV" "SB-POSIX") "OPENAI_API_KEY" "" 1))
           (if (%blankp (uiop:getenv "OPENAI_API_KEY"))
               (expect-error provider-error provider-error-kind :auth
                 (ai :name "openai" :model +test-openai-model+))
               (expect t "skipped: OPENAI_API_KEY could not be cleared in this image")))
      (when saved
        (ignore-errors (funcall (find-symbol "SETENV" "SB-POSIX") "OPENAI_API_KEY" saved 1))))))

(deftest test-dummy-key-with-custom-transport-works
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")) :api-key "dummy")
    (let ((result (chat client (vector (message "user" "hi")))))
      (expect-equal (jget result "content") "Answer: ok" "custom transport response content")
      (expect-equal (script-call-count script) 1 "exactly one request, no retries"))))

(deftest test-api-key-env-fallback
  (let ((saved (uiop:getenv "OPENAI_API_KEY"))
        (setenv (find-symbol "SETENV" "SB-POSIX")))
    (unwind-protect
         (progn
           (funcall setenv "OPENAI_API_KEY" "sk-env-fallback-value" 1)
           (multiple-value-bind (transport script)
               (make-scripted-transport (list (openai-text-response "Answer: ok")))
             (let ((client (ai :name "openai" :model +test-openai-model+ :transport transport)))
               (chat client (vector (message "user" "hi")))
               (expect-equal (header-value (script-headers script 0) "Authorization")
                             "Bearer sk-env-fallback-value"
                             "API key taken from OPENAI_API_KEY"))))
      (funcall setenv "OPENAI_API_KEY" (or saved "") 1))))

;;; ------------------------------------------------------------------
;;; Request mapping
;;; ------------------------------------------------------------------

;;; The per-provider request body and headers moved to the provider's own suite
;;; with the ai() factory flip: the body is Core's now, and asserting its shape
;;; here would be this suite testing the provider's mapping rather than the
;;; generator's behaviour.  What this suite keeps asserting about a request is
;;; what the generator put in it, over Core's request object.

(deftest test-openai-normalization
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-tool-response
                              (list (openai-tool-call "call_7" "get_weather" "{\"city\":\"Oslo\"}"))
                              :content "working")))
    (declare (ignore script))
    (let* ((result (chat client (vector (message "user" "hi"))))
           (call (aref (jget result "toolCalls") 0)))
      (expect-equal (jget result "content") "working" "assistant content normalized")
      (expect-equal (jget call "id") "call_7" "normalized tool call id")
      (expect-equal (jget call "name") "get_weather" "normalized tool call name")
      (expect-equal (jget call "arguments") "{\"city\":\"Oslo\"}" "normalized arguments string")
      (expect-equal (jget (jget result "usage") "promptTokens") 5 "normalized prompt tokens")
      (expect-equal (jget (jget result "usage") "completionTokens") 3 "normalized completion tokens")
      (expect-equal (jget (jget result "usage") "totalTokens") 8 "normalized total tokens"))))

(deftest test-anthropic-normalization
  (multiple-value-bind (client script)
      (scripted-client "anthropic"
                       (list (encode-json
                              (object "stop_reason" "tool_use"
                                      "content" (vector (object "type" "text" "text" "thinking out loud")
                                                        (object "type" "tool_use" "id" "toolu_9"
                                                                "name" "get_weather"
                                                                "input" (object "city" "Oslo")))
                                      "usage" (object "input_tokens" 21 "output_tokens" 6)))))
    (declare (ignore script))
    (let* ((result (chat client (vector (message "user" "hi"))))
           (call (aref (jget result "toolCalls") 0)))
      (expect-equal (jget result "content") "thinking out loud" "text blocks concatenated")
      (expect-equal (jget call "id") "toolu_9" "normalized tool_use id")
      (expect-equal (jget call "arguments") "{\"city\":\"Oslo\"}"
                    "tool_use input re-encoded as an arguments string")
      (expect-equal (jget (jget result "usage") "promptTokens") 21 "input_tokens -> promptTokens")
      (expect-equal (jget (jget result "usage") "completionTokens") 6
                    "output_tokens -> completionTokens")
      (expect-equal (jget (jget result "usage") "totalTokens") 27 "total tokens derived"))))

;;; ------------------------------------------------------------------
;;; Typed provider errors
;;; ------------------------------------------------------------------

(deftest test-http-error-is-typed-and-redacted
  (let ((secret "sk-super-secret-value-123"))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (cons 500 (encode-json
                                          (object "error" (object "message"
                                                                  (format nil "boom for key ~a" secret))))))
                         :api-key secret)
      (let ((condition (expect-error provider-error provider-error-kind :status
                         (ax-chat client (object "chat_prompt" (vector (message "user" "hi")))
                                  (object "retry" (object "maxRetries" 0))))))
        (expect-equal (provider-error-status condition) 500 "status is carried on the condition")
        (let ((text (princ-to-string condition)))
          ;; The credential must never appear.  The provider's own message does
          ;; now reach the condition, which is a deliberate change: a 500 with no
          ;; detail is undiagnosable, and the redaction is of the key, not of the
          ;; provider's explanation.
          (expect-not-contains text secret "the key never appears in the condition")
          (expect-contains text "redacted"
                           "the credential is replaced rather than simply dropped")))
      (expect-equal (script-call-count script) 1
                    "an explicit zero request retry budget makes one HTTP attempt"))))

(deftest test-auth-error-from-status-401
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (cons 401 (encode-json
                                        (object "error" (object "message" "invalid api key"))))))
    (expect-error provider-error provider-error-kind :auth
      (chat client (vector (message "user" "hi"))))
    (expect-equal (script-call-count script) 1 "a 401 is not retried automatically")))

(deftest test-refusal-is-typed
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (encode-json
                              (object "choices"
                                      (vector (object "finish_reason" "stop"
                                                      "message" (object "role" "assistant"
                                                                        "content" :null
                                                                        "refusal" "I cannot help with that")))))))
    (declare (ignore script))
    (expect-error provider-error provider-error-kind :refusal
      (chat client (vector (message "user" "hi"))))))

(deftest test-a-truncated-completion-fails-the-run
  ;; A completion cut off at the token cap cannot be trusted, so it ends the run
  ;; rather than being returned as a short answer.  The decision moved above the
  ;; transport with the factory flip: Core classifies the finish reason and the
  ;; generator refuses, which is where the reference puts it.
  (dolist (provider '("openai" "anthropic"))
    (multiple-value-bind (client script)
        (if (string= provider "anthropic")
            (scripted-client "anthropic"
                             (list (encode-json
                                    (object "stop_reason" "max_tokens"
                                            "content" (vector (object "type" "text"
                                                                      "text" "Answer: cut"))
                                            "usage" (object "input_tokens" 1
                                                            "output_tokens" 1)))))
            (scripted-client "openai"
                             (list (openai-text-response "Answer: cut" :finish "length"))))
      (declare (ignore script))
      (let ((failure (handler-case
                         (progn (forward (ax "q:string -> answer:string" :max-retries 0)
                                         client (object "q" "go"))
                                nil)
                       (error (condition) condition))))
        (expect failure (format nil "~a: a truncated completion fails the run" provider))
        (expect-contains (ax-error-message-text failure)
                         "Max tokens reached before completion"
                         (format nil "~a: and says why" provider))))))

(deftest test-malformed-response-body-is-typed
  (multiple-value-bind (client script)
      (scripted-client "openai" (list "<html>gateway error</html>"))
    (declare (ignore script))
    (expect-error provider-error provider-error-kind :response
      (chat client (vector (message "user" "hi")))))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (encode-json (object "id" "x"))))
    (declare (ignore script))
    (expect-error provider-error provider-error-kind :response
      (chat client (vector (message "user" "hi"))))))

;;; ------------------------------------------------------------------
;;; Structured generation
;;; ------------------------------------------------------------------

(deftest test-generation-renders-signature-and-omits-optional-fields
  ;; Core renders the prompt, so the labels, the wire keys and the optional
  ;; wording are the ones every port sends.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Action: execute")))
    (let* ((gen (ax "observation:string -> action:class \"develop, execute\", source?:string"))
           (outputs (forward gen client (object "observation" "disk full"))))
      (expect-equal (jget outputs "action") "execute" "the class value is parsed")
      (expect-equal (nth-value 1 (gethash "source" outputs)) nil
                    "an optional field the model omitted stays absent")
      (let ((system (jget (aref (jget (script-request script 0) "messages") 0) "content"))
            (user (jget (aref (jget (script-request script 0) "messages") 1) "content")))
        (expect-contains system "Action (wire key: `action`)"
                         "an output field is named with its wire key")
        (expect-contains system "(This classification class field must be included)"
                         "a required field says so")
        (expect-contains system "Allowed values: develop, execute"
                         "a class field lists its options")
        (expect-contains system "(Only include this string field if its value is available)"
                         "an optional field says so")
        (expect-equal user (format nil "Observation: disk full~c" #\Newline)
                      "the user turn carries the input values")))))

(deftest test-generation-parses-all-typed-values
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "N: 7~cB: true~cC: a~cJ: {\"x\":1}~cArr: [\"p\",\"q\"]"
                                      #\Newline #\Newline #\Newline #\Newline))))
    (declare (ignore script))
    (let* ((gen (ax "q:string -> n:number, b:boolean, c:class \"a, b\", j:json, arr:string[]"))
           (outputs (forward gen client (object "q" "go"))))
      (expect-equal (jget outputs "n") 7 "a number")
      (expect (json-true-p (jget outputs "b")) "a JSON boolean, not a Lisp one")
      (expect-equal (jget outputs "c") "a" "a classification value")
      (expect-equal (jget (jget outputs "j") "x") 1 "a JSON object")
      (expect-equal (coerce (jget outputs "arr") 'list) '("p" "q") "a string array"))))

(deftest test-generation-parses-json-null-false-and-empty-arrays
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "Payload: {\"a\": null}~cFlags: false~cItems: []~cScores: [1, 2.5]~cTags: []"
                                      #\Newline #\Newline #\Newline #\Newline))))
    (declare (ignore script))
    (let* ((gen (ax (concatenate 'string
                                 "q:string -> payload:json, flags:boolean, items:json[], "
                                 "scores:number[], tags:string[]")))
           (outputs (forward gen client (object "q" "go"))))
      (expect (eq (jget (jget outputs "payload") "a") :null)
              "a JSON null inside a parsed payload stays :null")
      (expect (json-false-p (jget outputs "flags")) "false becomes JSON false, not nil")
      (expect (and (vectorp (jget outputs "items")) (zerop (length (jget outputs "items"))))
              "empty JSON array becomes an empty vector")
      (let ((scores (jget outputs "scores")))
        (expect (and (vectorp scores) (= (length scores) 2)
                     (= (aref scores 0) 1) (integerp (aref scores 0))
                     (< (abs (- (aref scores 1) 2.5)) 1/1000))
                (format nil "number array parsed to typed numbers, got ~s" scores)))
      (expect (and (vectorp (jget outputs "tags")) (zerop (length (jget outputs "tags"))))
              "empty string array becomes an empty vector")
      (expect-equal (hash-table-count outputs) 5 "all five fields present, including the empty ones"))))

(deftest test-generation-history-boundary-keeps-core-arrays-extensible
  ;; Both forward paths append tool/correction turns to the cache boundary's
  ;; return value. Neither a fixed-size input nor disabled caching may turn
  ;; that history into a fixed-size vector.
  (dolist (options (list (object) (object "contextCache" false)
                         (object "contextCache" true)
                         (object "contextCache" (object "cacheBreakpoint" "system"))))
    (let* ((gen (ax "question:string -> answer:string"))
           (source (vector (object "role" "system" "content" "system")
                           (object "role" "user" "content" "question")))
           (history (axllm/core::core-axgen-apply-context-cache gen source options)))
      (expect (and (adjustable-array-p history) (array-has-fill-pointer-p history))
              "the native boundary returns an extensible Core history")
      (axllm/core::append-tool-call-messages-impl
       history (object "content" "calling")
       (vector (object "id" "call_1" "name" "lookup" "arguments" (object))))
      (expect-equal (length history) 3 "Core appends to the returned history in place")
      (expect-equal (jget (aref history 2) "role") "assistant" "Core owns the appended turn")
      (expect-equal (length source) 2 "the original history is not extended")
      (expect (not (nth-value 1 (gethash "cache" (aref source 0))))
              "cache marking does not mutate the source message")
      (expect-equal (jget (aref history 0) "cache")
                    (if (json-true-p (jget options "contextCache")) true
                        (if (hash-table-p (jget options "contextCache")) true :null))
                    "enabled caching still marks the stable message"))))

(deftest test-generation-corrects-invalid-output-without-tools
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response "Action: deploy")
                             (openai-text-response (format nil "Action: execute~cPreview: true" #\Newline))))
    (let* ((weather (tool :name "get_weather"
                          :parameters (object "type" "object"
                                              "properties" (object "city" (object "type" "string")))
                          :handler (lambda (args) (declare (ignore args)) "12C")))
           (gen (ax +jiti-signature+ :tools (list weather) :max-retries 1))
           (outputs (forward gen client (object "observation" "ready"))))
      (expect-equal (jget outputs "action") "execute" "corrected output is used")
      (expect-equal (script-call-count script) 2 "exactly one correction turn")
      (let* ((first-body (script-request script 0))
             (second-body (script-request script 1))
             (messages (jget second-body "messages")))
        (expect (jget first-body "tools") "tools are offered on the working turn")
        (expect-equal (jget first-body "tool_choice") "auto" "the working turn allows tool calls")
        (expect (jget second-body "tools")
                "tool definitions are retained on the correction turn, as the API requires")
        (expect-equal (jget second-body "tool_choice") "auto"
                      "the correction turn still declares the tools, which both providers require")
        (expect-equal (length messages) 4 "history is preserved and appended to")
        (expect-equal (jget (aref messages 1) "content")
                      (jget (aref (jget first-body "messages") 1) "content")
                      "the caller's rendered input message is preserved verbatim")
        (expect-equal (jget (aref messages 2) "content") "Action: deploy"
                      "the rejected assistant message stays in history")
        (expect-contains (jget (aref (jget (aref messages 3) "content") 0) "text") "Invalid class"
                         "the correction turn names the validation problem")))))

(deftest test-generation-fails-after-correction-budget
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response "Action: deploy")
                             (openai-text-response "Action: deploy")
                             (openai-text-response "Action: deploy")))
    (let* ((gen (ax +jiti-signature+ :max-retries 2))
           (condition (expect-error generation-error generation-error-kind :validation
                        (forward gen client (object "observation" "ready")))))
      (expect (generation-error-problems condition) "validation problems are carried")
      (expect-equal (script-call-count script) 3 "initial turn plus exactly two corrections"))))

(defun tool-message-error-text (content)
  "The reason inside a failed tool result's JSON payload."
  (let ((payload (parse-json content)))
    (expect (hash-table-p payload) "a failed tool result is a JSON object")
    (jget payload "error")))

(defun tool-message-error (message)
  "The reason a tool MESSAGE reports back to the model."
  (tool-message-error-text (jget message "content")))

(defun make-recording-weather-tool ()
  (let ((seen '()))
    (values (tool :name "get_weather"
                  :description "Look up the weather for a city"
                  ;; additionalProperties false, so an argument the tool does not
                  ;; declare is refused.  Without it JSON Schema allows extras,
                  ;; which is what the reference does.
                  :parameters (object "type" "object"
                                      "properties" (object "city" (object "type" "string")
                                                           "days" (object "type" "integer"))
                                      "required" (vector "city")
                                      "additionalProperties" *json-false*)
                  :handler (lambda (args)
                             (push args seen)
                             (format nil "~a: 12C" (jget args "city"))))
            (lambda () (reverse seen)))))

(deftest test-tool-call-and-result-roundtrip
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_a" "get_weather"
                                                        "{\"city\":\"Oslo\",\"days\":2}")))
                               (openai-text-response "Answer: 12C in Oslo")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list weather)))
             (outputs (forward gen client (object "question" "weather in Oslo?"))))
        (expect-equal (jget outputs "answer") "12C in Oslo" "final output after the tool round")
        (expect-equal (length (funcall invocations)) 1 "handler invoked exactly once")
        (let ((log (program-chat-log gen)))
          (expect-equal (length (jget (aref log 0) "messages")) 2
                        "the first logged request does not acquire later tool turns")
          (expect-equal (length (jget (aref log 1) "messages")) 4
                        "the second logged request includes the tool round"))
        (expect-equal
         (length (jget (first (memory-history (generator-memory gen))) "messages")) 2
         "the remembered request does not alias the mutable Core history")
        (let ((args (first (funcall invocations))))
          (expect (hash-table-p args) "handler receives a hash table")
          (expect-equal (hash-table-test args) 'equal "handler hash table uses string keys")
          (expect-equal (jget args "city") "Oslo" "string argument passed through")
          (expect-equal (jget args "days") 2 "integer argument passed through"))
        (let ((messages (jget (script-request script 1) "messages")))
          (expect-equal (length messages) 4 "system, user, assistant tool call, tool result")
          (expect-equal (jget (aref messages 3) "role") "tool" "tool result appended to history")
          (expect-equal (jget (aref messages 3) "tool_call_id") "call_a" "tool result id matches")
          (expect-equal (jget (aref messages 3) "content") "Oslo: 12C" "handler result forwarded"))))))

(deftest test-malformed-tool-arguments-do-not-invoke-the-handler
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_b" "get_weather" "{\"city\": ")))
                               (openai-text-response "Answer: recovered")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list weather)))
             (outputs (forward gen client (object "question" "weather?"))))
        (expect-equal (jget outputs "answer") "recovered" "run continues after malformed arguments")
        (expect-equal (length (funcall invocations)) 0 "handler was never invoked")
        (let ((messages (jget (script-request script 1) "messages")))
          (expect-equal (tool-message-error (aref messages 3))
                        "Invalid JSON at character 8"
                        "Core's JSON parser error reaches the model without invoking the handler"))))))

(deftest test-unknown-and-invalid-tool-arguments-are-rejected
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_c" "get_weather"
                                                        "{\"city\":\"Oslo\",\"extra\":1}")))
                               (openai-tool-response
                                (list (openai-tool-call "call_d" "get_weather"
                                                        "{\"days\":\"two\"}")))
                               (openai-text-response "Answer: done")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list weather) :max-steps 3))
             (outputs (forward gen client (object "question" "weather?"))))
        (expect-equal (jget outputs "answer") "done" "run completes")
        (expect-equal (length (funcall invocations)) 0 "handler never invoked with bad arguments")
        (expect-contains (tool-message-error (aref (jget (script-request script 1) "messages") 3))
                         "Unexpected property: extra"
                         "an undeclared argument is rejected when the schema forbids extras")
        (let ((content (tool-message-error (aref (jget (script-request script 2) "messages") 5))))
          (expect-contains content "argument \"city\": Required argument is missing"
                           "a missing required argument names the argument")
          (expect-contains content "Expected integer, received string"
                           "a wrong argument type is rejected"))))))

(deftest test-an-unknown-tool-name-is-corrected-not-fatal
  ;; ir/conformance/axgen/unknown-tool-call-correction.json: a tool name the
  ;; model invented is something it can fix, so it is told the real names and
  ;; the run continues.  Aborting would throw away the work already done.
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_e" "launch_missiles" "{}")))
                               (openai-text-response "Answer: No such tool is registered.")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list weather)))
             (outputs (forward gen client (object "question" "?"))))
        (expect-equal (jget outputs "answer") "No such tool is registered."
                      "the run completes after the correction")
        (expect-equal (length (funcall invocations)) 0 "no handler ran")
        (expect-equal (script-call-count script) 2 "the model got a second turn")
        (let ((content (jget (aref (jget (script-request script 1) "messages") 3) "content")))
          (expect-contains (tool-message-error-text content) "Function not found: launch_missiles"
                           "the model is told which name failed")
          (expect-contains (tool-message-error-text content) "Available functions: get_weather"
                           "the model is told the names that exist")
          (expect-contains (tool-message-error-text content) "Call one of these exact function names"
                           "the model is told what to do about it"))
        (let ((traces (generator-function-call-traces gen)))
          (expect-equal (jget (aref traces 0) "status") "error"
                        "the failed call is traced as an error")
          (expect-equal (jget (aref traces 0) "name") "launch_missiles"
                        "the trace keeps the name the model used"))))))

(deftest test-a-tool-name-is-matched-exactly-before-it-is-normalized
  ;; src/ax/dsp/functions.ts resolves the exact name first and only then folds
  ;; punctuation and case.  With both get_weather and getWeather registered, the
  ;; name the model wrote must win over the one that merely folds to it.
  (let* ((log (list :log))
         (snake (make-effect-tool "get_weather" log))
         (camel (tool :name "getWeather" :description "camel"
                      :parameters (object "type" "object"
                                          "properties" (object "key" (object "type" "string"))
                                          "required" (vector "key"))
                      :handler (lambda (args) (push (cons "getWeather" (jget args "key")) (cdr log))
                                 "camel result")))
         (processor (make-function-processor (list snake camel))))
    (expect-equal (jget (function-processor-resolve processor "getWeather") "name") "getWeather"
                  "an exact camelCase name resolves to that tool")
    (expect-equal (jget (function-processor-resolve processor "get_weather") "name") "get_weather"
                  "an exact snake_case name resolves to that tool")
    ;; Both fold to "getweather", so a name that matches neither exactly is
    ;; ambiguous and resolves to nothing rather than to an arbitrary one.
    (expect-equal (function-processor-resolve processor "Get-Weather") nil
                  "an ambiguous normalized name resolves to nothing"))
  (let* ((log (list :log))
         (only (make-effect-tool "get_weather" log))
         (processor (make-function-processor (list only))))
    (expect-equal (jget (function-processor-resolve processor "getWeather") "name") "get_weather"
                  "an unambiguous normalized name still resolves")
    (expect-equal (jget (function-processor-resolve processor "Get Weather") "name") "get_weather"
                  "punctuation and case are folded away")))

(deftest test-a-failing-tool-handler-is-reported-not-fatal
  ;; ir/conformance/axgen/tool-handler-exception-correction.json: the handler
  ;; ran and failed, so the model is told why and answers around it.  The call
  ;; is still recorded as attempted.
  (let ((search (tool :name "search"
                      :description "Search docs"
                      :parameters (object "type" "object"
                                          "properties" (object "query" (object "type" "string"))
                                          "required" (vector "query"))
                      :handler (lambda (args) (declare (ignore args))
                                 (error "backend unavailable")))))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_1" "search" "{\"query\":\"docs\"}")))
                               (openai-text-response
                                "Answer: The search backend is unavailable.")))
      (let* ((gen (ax "query:string -> answer:string" :tools (list search)))
             (outputs (forward gen client (object "query" "docs"))))
        (expect-equal (jget outputs "answer") "The search backend is unavailable."
                      "the run completes after the handler failed")
        (expect-equal (script-call-count script) 2 "the model got a second turn")
        (expect-contains (tool-message-error (aref (jget (script-request script 1) "messages") 3))
                         "backend unavailable"
                         "the handler's own reason reaches the model")
        (let ((traces (generator-function-call-traces gen)))
          (expect-equal (jget (aref traces 0) "status") "error" "the call is traced as an error")
          (expect-equal (jget (aref traces 0) "name") "search" "the attempted call is recorded"))))))

(defun tool-call-failure-message (thunk)
  "Run THUNK and return the `function-call-error' message it signals."
  (handler-case (progn (funcall thunk)
                       (error 'test-failure :text "expected a function-call-error, nothing signalled"))
    (function-call-error (condition) (ax-error-message condition))))

(deftest test-a-tool-result-and-its-raw-value-are-both-available
  ;; executeWithDetails keeps the parsed arguments and the handler's own value
  ;; beside the text the model sees, so a caller can use the value without
  ;; re-parsing the formatted text.
  (let* ((lookup (tool :name "lookup"
                       :description "Look up a key"
                       :parameters (object "type" "object"
                                           "properties" (object "key" (object "type" "string"))
                                           "required" (vector "key"))
                       :handler (lambda (args) (object "found" (jget args "key") "count" 2))))
         (processor (make-function-processor (list lookup))))
    (multiple-value-bind (text raw arguments)
        (execute-function-with-details
         processor (object "id" "call_1" "name" "lookup" "arguments" "{\"key\":\"a\"}"))
      (expect-equal (jget raw "found") "a" "the raw handler value is returned as it is")
      (expect-equal (jget raw "count") 2 "the raw value keeps its types")
      (expect-equal (jget arguments "key") "a" "the parsed arguments are returned")
      (expect-equal text
                    (format nil "{~c  \"found\": \"a\",~c  \"count\": 2~c}"
                            #\Newline #\Newline #\Newline)
                    "the formatted text is the pretty JSON the model sees"))
    (expect-equal (execute-function
                   processor (object "id" "call_2" "name" "lookup" "arguments" "{\"key\":\"b\"}"))
                  (format nil "{~c  \"found\": \"b\",~c  \"count\": 2~c}"
                          #\Newline #\Newline #\Newline)
                  "execute returns only the formatted text")
    ;; A provider that already decoded the arguments is accepted as it is.
    (multiple-value-bind (text raw arguments)
        (execute-function-with-details
         processor (object "id" "call_3" "name" "lookup" "arguments" (object "key" "c")))
      (declare (ignore text raw))
      (expect-equal (jget arguments "key") "c" "a decoded argument object is used directly"))
    ;; A tool with no parameters is called with no arguments, not refused.
    (let* ((ping (tool :name "ping" :description "Ping"
                       :handler (lambda (args) (declare (ignore args)) "pong")))
           (bare (make-function-processor (list ping))))
      (expect-equal (execute-function bare (object "id" "call_4" "name" "ping")) "pong"
                    "an absent arguments value means no arguments"))
    ;; Every failure is a function-call-error, so a caller can tell a model
    ;; mistake from a definition mistake.
    (expect-equal (tool-call-failure-message
                   (lambda () (execute-function processor (object "id" "call_5" "name" "nope"))))
                  "Function not found: nope. Available functions: lookup. Call one of these exact function names."
                  "an unknown name names the available tools")
    (let ((empty (make-function-processor '())))
      (expect-equal (tool-call-failure-message
                     (lambda () (execute-function empty (object "id" "call_6" "name" "nope"))))
                    "Function not found: nope. Available functions: (none). Call one of these exact function names."
                    "with no tools registered the list reads (none)"))
    (expect-equal (tool-call-failure-message
                   (lambda () (execute-function
                               processor (object "id" "call_7" "name" "lookup"
                                                 "arguments" "{\"key\": "))))
                  "Invalid function arguments: {\"key\": "
                  "unparseable arguments name the text that failed")))

(deftest test-duplicate-tool-call-ids-are-rejected
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (declare (ignore invocations))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_dup" "get_weather" "{\"city\":\"Oslo\"}")
                                      (openai-tool-call "call_dup" "get_weather" "{\"city\":\"Bergen\"}")))))
      (declare (ignore script))
      (let ((gen (ax "question:string -> answer:string" :tools (list weather))))
        (expect-error generation-error generation-error-kind :tool
          (forward gen client (object "question" "?")))))))

(deftest test-tool-loop-is-bounded
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (let ((counter 0))
      (let* ((transport (lambda (url headers body)
                          (declare (ignore url headers body))
                          (incf counter)
                          (values (openai-tool-response
                                   (list (openai-tool-call (format nil "call_~a" counter)
                                                           "get_weather" "{\"city\":\"Oslo\"}")))
                                  200)))
             (client (ai :name "openai" :model +test-openai-model+
                         :api-key "dummy" :transport transport))
             (gen (ax "question:string -> answer:string" :tools (list weather)
                                                         :max-steps 2 :max-retries 0)))
        (expect-error generation-error generation-error-kind :steps
          (forward gen client (object "question" "?")))
        (expect (<= counter 3) (format nil "provider calls bounded, saw ~a" counter))
        (expect-equal (length (funcall invocations)) 2 "handler ran once per permitted step")))))

(deftest test-duplicate-tool-names-are-rejected
  (let ((a (tool :name "same" :handler (lambda (args) (declare (ignore args)) "1")))
        (b (tool :name "same" :handler (lambda (args) (declare (ignore args)) "2"))))
    (handler-case (progn (ax "q:string -> a:string" :tools (list a b))
                         (error 'test-failure :text "expected duplicate tool names to be rejected"))
      (tool-error (c) (expect-contains (princ-to-string c) "Duplicate tool name"
                                       "duplicate tool names rejected")))))

(deftest test-non-object-tool-arguments-never-reach-the-handler
  ;; The original defect: a tool call whose params are a JSON array, a number or
  ;; null must not be coerced into an empty object, because a tool whose
  ;; arguments are all optional would then run with none of them.  Core
  ;; normalizes the wire; what must hold is that the handler either receives the
  ;; arguments it was given or does not run at all.
  (dolist (params '("[1,2]" "7" "null" "\"text\"" "true"))
    (let ((ran '()))
      (let ((spec (tool :name "lookup"
                        :description "Look up a key"
                        :parameters (object "type" "object"
                                            "properties" (object "key" (object "type" "string"))
                                            "required" (vector "key"))
                        :handler (lambda (args) (push args ran) "ran"))))
        (multiple-value-bind (client script)
            (scripted-client "openai"
                             (list (openai-tool-response
                                    (list (openai-tool-call "call_x" "lookup" params)))
                                   (openai-text-response "Answer: recovered")
                                   (openai-text-response "Answer: recovered")
                                   (openai-text-response "Answer: recovered")
                                   (openai-text-response "Answer: recovered")))
          (declare (ignore script))
          (handler-case (forward (ax "q:string -> answer:string" :tools (list spec))
                                 client (object "q" "go"))
            (error () nil))
          ;; Either the handler never ran, or it ran with the required argument
          ;; actually present.  What it must never see is an empty object.
          (dolist (seen ran)
            (expect (and (hash-table-p seen) (nth-value 1 (gethash "key" seen)))
                    (format nil "params ~a must not reach the handler as empty arguments, saw ~a"
                            params (encode-json seen)))))))))

(deftest test-the-full-core-field-language-is-supported
  ;; The subset used to refuse every field modifier and type it could not
  ;; enforce itself.  Extraction is Core's now, so the modifiers and types the
  ;; other ports support are built and parsed here too, rather than rejected.
  (dolist (signature '("q:string -> answer:string \"maxLength:10\""
                       "q:string -> code:code"
                       "q:string -> when:date"
                       "q:string -> at:datetime"
                       "q:string -> score:number"
                       "q:string -> tags:string[]"
                       "q:string -> mode:class \"fast, slow\""
                       "q:string -> payload:json"))
    (expect (ax signature) (format nil "~a builds" signature)))
  ;; A code field delivers the body as written, including text that is not
  ;; prose, and a fenced body loses its fence.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              "Code: final('Answer', {answer: 'Paris'})")))
    (declare (ignore script))
    (let* ((gen (ax "q:string -> code:code"))
           (outputs (forward gen client (object "q" "go"))))
      (expect-equal (jget outputs "code") "final('Answer', {answer: 'Paris'})"
                    "a code field keeps the source as the model wrote it")))
  ;; ir/conformance/axgen/date-forward-parse-dates.json carries no options and
  ;; expects parsed dates, so parsing is the default; the sibling fixture passes
  ;; parse_dates false to keep the text the model wrote.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "When: 2024-05-09~cScore: 42" #\Newline))))
    (declare (ignore script))
    (let* ((gen (ax "q:string -> when:date, score:number"))
           (outputs (forward gen client (object "q" "go"))))
      (expect-equal (jget outputs "when") "2024-05-09T00:00:00.000Z"
                    "a date field is parsed by default")
      (expect-equal (jget outputs "score") 42 "a number field parses")))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "When: 2024-05-09")))
    (declare (ignore script))
    (let* ((gen (ax "q:string -> when:date"))
           (outputs (forward gen client (object "q" "go") (object "parse_dates" false))))
      (expect-equal (jget outputs "when") "2024-05-09"
                    "parse_dates false keeps the text the model wrote")))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "When: 2024-05-09")))
    (declare (ignore script))
    (let* ((gen (ax "q:string -> when:date" :options (object "parseDates" false)))
           (outputs (forward gen client (object "q" "go"))))
      (expect-equal (jget outputs "when") "2024-05-09"
                    "and so does the generator's own option")))
  ;; The exhausted-correction wording is Core's, so a cross-port failure report
  ;; reads the same and names the model's last output.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "N: nope")))
    (declare (ignore script))
    (let ((gen (ax "q:string -> n:number" :max-retries 0)))
      (let ((condition (expect-error generation-error generation-error-kind :validation
                        (forward gen client (object "q" "go")))))
        (let ((text (ax-error-message condition)))
          (expect (%prefix-p "Generate failed: Unable to fix validation error: " text)
                  (format nil "the failure is worded as Core words it, got ~s" text))
          (expect-contains text "LLM Output:" "and quotes the model's own output")
          (expect-contains text "N: nope" "including what it actually wrote"))))))

;;; ------------------------------------------------------------------
;;; Caller input type validation (before any provider request)
;;; ------------------------------------------------------------------

(defparameter +typed-input-signature+
  (concatenate 'string
               "count:number, enabled:boolean, mode:string, "
               "tags:string[], blob:json -> answer:string"))

(defun %typed-inputs (&rest overrides)
  (let ((inputs (object "count" 3
                        "enabled" *json-true*
                        "mode" "fast"
                        "tags" (vector "a" "b")
                        "blob" (object "k" "v"))))
    (loop for (key value) on overrides by #'cddr do (setf (gethash key inputs) value))
    inputs))

(deftest test-valid-typed-inputs-are-accepted-and-rendered
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (let* ((gen (ax +typed-input-signature+))
           (outputs (forward gen client (%typed-inputs))))
      (expect-equal (jget outputs "answer") "ok" "valid inputs complete the run")
      (let ((user (jget (aref (jget (script-request script 0) "messages") 1) "content")))
        ;; Core renders each value under its title, one blank line apart, with an
        ;; array or object as pretty JSON.
        (expect-equal
         user
         (%join-lines '("Count: 3" "" "Enabled: true" "" "Mode: fast" ""
                        "Tags: [" "  \"a\"," "  \"b\"" "]" ""
                        "Blob: {" "  \"k\": \"v\"" "}"))
         "the user turn is exactly what Core renders")))))

(deftest test-invalid-typed-inputs-are-rejected-before-any-request
  ;; Core decides what an input value may be, and it is stricter about absence
  ;; and shape than about scalar spelling: a null or an unrenderable value is
  ;; refused, while a scalar it can render is rendered.  Either way no provider
  ;; request is made for a value it refuses.
  (dolist (override (list (list "count" :null)
                          (list "mode" :null)
                          (list "tags" :null)
                          (list "blob" (lambda () nil))))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response "Answer: ok")))
      (let ((gen (ax +typed-input-signature+)))
        (expect (handler-case (progn (forward gen client (apply #'%typed-inputs override)) nil)
                  (ax-error () t))
                (format nil "invalid input ~a is refused" (first override)))
        (expect-equal (script-call-count script) 0
                      (format nil "no provider request is made for invalid input ~a"
                              (first override)))))))

(deftest test-an-undeclared-input-key-is-dropped-not-refused
  ;; A program running as a node in a flow or an agent stage is handed the whole
  ;; shared state, so keys it did not declare are another step's business.  The
  ;; other ports drop them; refusing them made every flow and agent stage fail.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (let* ((gen (ax "question:string -> answer:string"))
           (outputs (forward gen client (object "question" "go"
                                                "actionLog" (vector)
                                                "contextPressure" 3
                                                "leftResult" "other step's"))))
      (expect-equal (jget outputs "answer") "ok" "the run completes")
      (let ((user (jget (aref (jget (script-request script 0) "messages") 1) "content")))
        (expect-contains user "go" "the declared input is rendered")
        (expect-not-contains user "other step's" "an undeclared value is not rendered")
        (expect-not-contains user "contextPressure" "an undeclared key is not rendered"))))
  ;; A missing required input is still a failure: dropping what was not declared
  ;; is not the same as inventing what was not supplied.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (let ((gen (ax "question:string -> answer:string")))
      (expect (handler-case (progn (forward gen client (object "other" "x")) nil)
                (ax-error () t))
              "a missing required input is refused")
      (expect-equal (script-call-count script) 0 "no request for a missing required input"))))

(deftest test-core-signature-restrictions-precede-provider-requests
  ;; Core forbids class inputs and image outputs; a permissive test stub did
  ;; not. Exercise these through the real public boundary, not a stub parser.
  (multiple-value-bind (client script) (scripted-client "openai" nil)
    (dolist (signature '("mode:class \"fast, slow\" -> answer:string"
                         "question:string -> pic:image"))
      (expect (handler-case (progn (forward (ax signature) client (object)) nil)
                (signature-error () t))
              "Core rejects invalid field positions")
      (expect-equal (script-call-count script) 0 "no request for invalid signatures"))))

;;; ------------------------------------------------------------------
;;; Fail-closed schema validation
;;; ------------------------------------------------------------------

(deftest test-a-schema-constraint-is-either-enforced-or-refused
  ;; The rule has not changed: a constraint this port cannot check is refused
  ;; rather than ignored.  What changed is which constraints it can check.
  ;; Argument validation is Core's now, the same code the reference runs, so the
  ;; length, range and pattern keywords are enforced instead of rejected.
  (handler-case
      (progn (tool :name "t"
                   :parameters (object "type" "object"
                                       "properties" (object "x" (object "anyOf" (vector)))
                                       "required" (vector "x"))
                   :handler (lambda (args) (declare (ignore args)) ""))
             (error 'test-failure :text "expected anyOf to be rejected"))
    (tool-error (c) (expect-contains (princ-to-string c) "unsupported schema keyword"
                                     "a keyword nothing enforces is reported, not ignored")))
  (let ((problems (validate-schema-support (object "type" "array") "schema")))
    (expect problems "a non-object top-level schema is rejected"))
  ;; Every keyword below is accepted at definition time AND enforced before a
  ;; handler runs.  Accepting without enforcing would be the real failure, so
  ;; each one is driven with an argument that violates it.
  (dolist (case (list (list (object "type" "string" "minLength" 3) "ab" "too short")
                      (list (object "type" "string" "maxLength" 2) "abc" "too long")
                      (list (object "type" "string" "pattern" "^a") "b" "pattern")
                      (list (object "type" "number" "minimum" 5) 1 "minimum")
                      (list (object "type" "number" "maximum" 5) 9 "maximum")
                      (list (object "type" "array" "items" (object "type" "string")
                                    "minItems" 2)
                            (vector "a") "Too few items")))
    (destructuring-bind (field-schema bad-value needle) case
      (expect-equal (validate-schema-support
                     (object "type" "object" "properties" (object "x" field-schema))
                     "schema")
                    '()
                    (format nil "~a is a keyword this port enforces" needle))
      (let ((ran nil))
        (let ((spec (tool :name "t"
                          :parameters (object "type" "object"
                                              "properties" (object "x" field-schema)
                                              "required" (vector "x"))
                          :handler (lambda (args) (declare (ignore args))
                                     (setf ran t) "ran"))))
          (multiple-value-bind (result problems) (invoke-tool spec (object "x" bad-value))
            (declare (ignore result))
            (expect problems
                    (format nil "a value violating ~a is rejected" needle))
            (expect (not ran)
                    (format nil "and the handler never ran for ~a" needle)))))))
  ;; The four edges the hand-written validator got wrong, each checked against
  ;; the reference's own rule.
  (let ((spec (tool :name "t"
                    :parameters (object "type" "object"
                                        "properties" (object "n" (object "type" "integer"))
                                        "required" (vector "n"))
                    :handler (lambda (args) (declare (ignore args)) "ran"))))
    ;; An integer is a number with an integral value, so 1.0 is one.
    (expect-equal (nth-value 1 (invoke-tool spec (object "n" 1.0d0))) '()
                  "a JSON 1.0 satisfies an integer argument")
    (expect-equal (nth-value 1 (invoke-tool spec (object "n" 1))) '()
                  "and so does 1")
    (expect (nth-value 1 (invoke-tool spec (object "n" "1")))
            "a string does not")
    ;; An extra property is allowed unless the schema forbids it, which is what
    ;; JSON Schema means by an omitted additionalProperties.
    (expect-equal (nth-value 1 (invoke-tool spec (object "n" 1 "extra" 5))) '()
                  "an extra argument is allowed when the schema does not forbid it"))
  (let ((closed (tool :name "t"
                      :parameters (object "type" "object"
                                          "properties" (object "n" (object "type" "integer"))
                                          "additionalProperties" *json-false*)
                      :handler (lambda (args) (declare (ignore args)) "ran"))))
    (expect (nth-value 1 (invoke-tool closed (object "n" 1 "extra" 5)))
            "and refused when it does"))
  (let ((enumerated (tool :name "t"
                          :parameters (object "type" "object"
                                              "properties"
                                              (object "e" (object "type" "number"
                                                                  "enum" (vector 1 2)))
                                              "required" (vector "e"))
                          :handler (lambda (args) (declare (ignore args)) "ran"))))
    ;; Enum membership compares values, not Lisp objects, so 1.0 matches 1.
    (expect-equal (nth-value 1 (invoke-tool enumerated (object "e" 1.0d0))) '()
                  "1.0 matches an enum of 1")
    (expect (nth-value 1 (invoke-tool enumerated (object "e" 3)))
            "a value outside the enum is refused"))
  ;; A non-finite number is not a JSON number, so no handler may see one.
  (let ((ran nil))
    (let ((spec (tool :name "t"
                      :parameters (object "type" "object"
                                          "properties" (object "n" (object "type" "number"))
                                          "required" (vector "n"))
                      :handler (lambda (args) (declare (ignore args)) (setf ran t) "ran"))))
      (dolist (value (list sb-ext:double-float-positive-infinity
                           sb-ext:double-float-negative-infinity))
        (expect (nth-value 1 (invoke-tool spec (object "n" value)))
                (format nil "~a is refused as a number argument" value)))
      (expect (not ran) "and no handler ran for a non-finite number"))))

(deftest test-tool-argument-validation-details
  (let ((spec (tool :name "t"
                    :parameters (object "type" "object"
                                        "properties"
                                        (object "mode" (object "type" "string"
                                                               "enum" (vector "fast" "slow"))
                                                "tags" (object "type" "array"
                                                               "items" (object "type" "string"))
                                                "flag" (object "type" "boolean"))
                                        "required" (vector "mode"))
                    :handler (lambda (args) (jget args "mode")))))
    (expect (null (validate-tool-arguments
                   spec (object "mode" "fast" "tags" (vector "a") "flag" *json-true*)))
            "valid arguments produce no problems")
    ;; The wording is Core's, the same text the reference produces, and each
    ;; problem names the argument it is about.
    (let ((problem (first (validate-tool-arguments spec (object "mode" "medium")))))
      (expect-contains problem "Value is not in the allowed enum" "enum violation reported")
      (expect-contains problem "\"mode\"" "and names the argument"))
    (expect-contains (first (validate-tool-arguments spec (object "mode" "fast"
                                                                  "tags" (vector 1))))
                     "tags[0]" "array element problems are located")
    (expect-contains (first (validate-tool-arguments spec (object "mode" "fast" "flag" "yes")))
                     "Expected boolean, received string"
                     "booleans must be real JSON booleans")
    (multiple-value-bind (result problems) (invoke-tool spec (object "mode" "medium"))
      (expect (null result) "handler result is nil when validation fails")
      (expect problems "problems returned when validation fails"))
    (multiple-value-bind (result problems) (invoke-tool spec (object "mode" "fast"))
      (expect (null problems) "no problems for valid arguments")
      (expect-equal result "fast" "handler invoked and result stringified"))))

;;; ------------------------------------------------------------------
;;; Real Drakma default transport against a loopback server
;;; ------------------------------------------------------------------

(defun %octets-to-latin1 (bytes)
  (sb-ext:octets-to-string (coerce bytes '(vector (unsigned-byte 8)))
                           :external-format :latin-1))

(defun %read-byte-line (stream)
  "Read one CRLF-terminated header line from a binary STREAM."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
        (saw-any nil))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (cond ((null byte) (return (if saw-any (%octets-to-latin1 bytes) nil)))
              ((= byte 10) (return (%octets-to-latin1 bytes)))
              (t (setf saw-any t)
                 (unless (= byte 13) (vector-push-extend byte bytes))))))))

(defun %read-http-request (stream)
  "Read one HTTP request from a binary STREAM.  Returns an object with
\"requestLine\", \"headers\" (alist) and \"body\" (decoded as UTF-8).  The body
is read as Content-Length octets, not characters, so a multi-byte body is not
truncated."
  (let ((lines '()))
    (loop for line = (%read-byte-line stream)
          while (and line (not (string= line "")))
          do (push line lines))
    (let* ((lines (nreverse lines))
           (request-line (or (first lines) ""))
           (headers (mapcar (lambda (line)
                              (let ((colon (position #\: line)))
                                (if colon
                                    (cons (string-trim " " (subseq line 0 colon))
                                          (string-trim " " (subseq line (1+ colon))))
                                    (cons line ""))))
                            (rest lines)))
           (length-header (cdr (assoc "content-length" headers :test #'string-equal)))
           (count (if length-header (parse-integer length-header :junk-allowed t) 0))
           (body (if (and count (plusp count))
                     (let ((buffer (make-array count :element-type '(unsigned-byte 8))))
                       (read-sequence buffer stream)
                       (sb-ext:octets-to-string buffer :external-format :utf-8))
                     "")))
      (object "requestLine" request-line "headers" headers "body" body))))

(defun %write-http-response (stream status body &key (extra-headers nil))
  "Write an HTTP response on a binary STREAM.  Content-Length counts UTF-8
octets, which is what an HTTP client reads."
  (let* ((body-octets (sb-ext:string-to-octets (or body "") :external-format :utf-8))
         (header (with-output-to-string (out)
                   (format out "HTTP/1.1 ~a ~a~c~c" status
                           (case status
                             (200 "OK") (302 "Found") (500 "Internal Server Error")
                             (t "Status"))
                           #\Return #\Newline)
                   (format out "Content-Type: application/json~c~c" #\Return #\Newline)
                   (format out "Content-Length: ~a~c~c" (length body-octets) #\Return #\Newline)
                   (dolist (pair extra-headers)
                     (format out "~a: ~a~c~c" (car pair) (cdr pair) #\Return #\Newline))
                   (format out "Connection: close~c~c~c~c"
                           #\Return #\Newline #\Return #\Newline))))
    (write-sequence (sb-ext:string-to-octets header :external-format :latin-1) stream)
    (write-sequence body-octets stream)
    (force-output stream)))

(defstruct (loopback (:conc-name loopback-)) port socket thread (requests '()) lock)

(defun start-loopback-server (responder &key certificate key)
  "Start a one-thread loopback HTTP server.  RESPONDER receives the parsed
request object and a binary stream. Optional CERTIFICATE and KEY enable TLS."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 5)
    (multiple-value-bind (address port) (sb-bsd-sockets:socket-name socket)
      (declare (ignore address))
      (let ((server (make-loopback :port port :socket socket :lock (sb-thread:make-mutex))))
        (setf (loopback-thread server)
              (sb-thread:make-thread
               (lambda ()
                 (handler-case
                     (loop
                       (let ((client (sb-bsd-sockets:socket-accept socket)))
                         (unwind-protect
                              (handler-case
                                  (let* ((stream (sb-bsd-sockets:socket-make-stream
                                                  client :input t :output t
                                                         :element-type '(unsigned-byte 8)))
                                         (stream (if certificate
                                                     (cl+ssl:make-ssl-server-stream
                                                      stream :certificate (namestring certificate)
                                                      :key (namestring key))
                                                     stream))
                                         (request (%read-http-request stream)))
                                    (sb-thread:with-mutex ((loopback-lock server))
                                      (push request (loopback-requests server)))
                                    (unwind-protect (funcall responder request stream)
                                      (close stream)))
                                (error () nil))
                           (ignore-errors (sb-bsd-sockets:socket-close client)))))
                   (error () nil)))
               :name "ax-loopback-http"))
        server))))

(defun stop-loopback-server (server)
  (ignore-errors (sb-thread:terminate-thread (loopback-thread server)))
  (ignore-errors (sb-bsd-sockets:socket-close (loopback-socket server)))
  (ignore-errors (sb-thread:join-thread (loopback-thread server) :default nil))
  nil)

(defun loopback-request-count (server)
  (sb-thread:with-mutex ((loopback-lock server))
    (length (loopback-requests server))))

(defun loopback-url (server) (format nil "http://127.0.0.1:~a" (loopback-port server)))

(deftest test-default-drakma-transport-against-loopback
  (let ((server (start-loopback-server
                 (lambda (request stream)
                   (declare (ignore request))
                   (%write-http-response stream 200 (openai-text-response "Answer: loopback"))))))
    (unwind-protect
         (let* ((client (ai :name "openai" :model +test-openai-model+
                            :api-key "sk-loopback-key"
                            :base-url (concatenate 'string (loopback-url server) "/v1")))
                (result (chat client (vector (message "user" "hi")))))
           (expect-equal (jget result "content") "Answer: loopback"
                         "real HTTP round trip through the default Drakma transport")
           (let* ((request (first (loopback-requests server)))
                  (headers (jget request "headers"))
                  (body (parse-json (jget request "body"))))
             (expect-contains (jget request "requestLine") "POST /v1/chat/completions"
                              "default transport POSTs to the chat completions path")
             (expect-equal (header-value headers "Authorization") "Bearer sk-loopback-key"
                           "auth header reached the server")
             (expect-equal (jget body "model") +test-openai-model+
                           "explicit model reached the server")))
      (stop-loopback-server server))))

(deftest test-default-transport-rejects-untrusted-tls-before-sending-credentials
  (uiop:with-temporary-file (:pathname certificate :type "pem")
    (uiop:with-temporary-file (:pathname key :type "pem")
      (uiop:run-program (list "openssl" "req" "-x509" "-newkey" "rsa:2048"
                              "-nodes" "-keyout" (namestring key)
                              "-out" (namestring certificate) "-days" "1"
                              "-subj" "/CN=localhost" "-addext" "subjectAltName=IP:127.0.0.1")
                        :output nil :error-output nil)
      (let ((server (start-loopback-server
                     (lambda (request stream)
                       (declare (ignore request))
                       (%write-http-response stream 200 (openai-text-response "insecure")))
                     :certificate certificate :key key)))
        (unwind-protect
             (let ((client (ai :name "openai" :model +test-openai-model+
                               :api-key "tls-test-key" :timeout 3
                               :base-url (format nil "https://127.0.0.1:~d/v1" (loopback-port server)))))
               (let ((condition (expect-error provider-error provider-error-kind :transport
                                  (chat client (vector (message "user" "hi"))))))
                 (expect-not-contains (princ-to-string condition) "tls-test-key"
                                     "TLS condition never exposes the key"))
               (expect-equal (loopback-request-count server) 0
                             "TLS validation fails before any HTTP headers reach the server"))
          (stop-loopback-server server))))))

(deftest test-default-transport-timeout-is-bounded
  (let ((server (start-loopback-server
                 (lambda (request stream)
                   (declare (ignore request))
                   (sleep 5)
                   (ignore-errors
                    (%write-http-response stream 200 (openai-text-response "Answer: late")))))))
    (unwind-protect
         (let ((client (ai :name "openai" :model +test-openai-model+
                           :api-key "sk-loopback-key"
                           :timeout 1
                           :base-url (concatenate 'string (loopback-url server) "/v1")))
               (started (get-internal-real-time)))
           (expect-contains
            (princ-to-string
             (expect-error provider-error provider-error-kind :transport
               (chat client (vector (message "user" "hi")))))
            "timed out" "a slow provider is bounded by :timeout")
           (expect (< (/ (- (get-internal-real-time) started) internal-time-units-per-second) 4)
                   "the bounded request returned well before the server replied"))
      (stop-loopback-server server))))

(deftest test-default-transport-does-not-forward-credentials-on-redirect
  (let* ((sink (start-loopback-server
                (lambda (request stream)
                  (declare (ignore request))
                  (%write-http-response stream 200 (openai-text-response "Answer: leaked")))))
         (redirector (start-loopback-server
                      (lambda (request stream)
                        (declare (ignore request))
                        (%write-http-response
                         stream 302 "{}"
                         :extra-headers (list (cons "Location"
                                                    (format nil "~a/v1/chat/completions"
                                                            (loopback-url sink)))))))))
    (unwind-protect
         (let ((client (ai :name "openai" :model +test-openai-model+
                           :api-key "sk-redirect-secret"
                           :base-url (concatenate 'string (loopback-url redirector) "/v1"))))
           (let ((condition (expect-error provider-error provider-error-kind :status
                              (chat client (vector (message "user" "hi"))))))
             (expect-equal (provider-error-status condition) 302
                           "the redirect is surfaced as a typed HTTP error, not followed")
             (expect-not-contains (princ-to-string condition) "sk-redirect-secret"
                                  "the key is not echoed in the error"))
           (expect-equal (loopback-request-count sink) 0
                         "the redirect target received no request, so no credential was forwarded")
           (expect-equal (loopback-request-count redirector) 1 "exactly one request, no retry"))
      (stop-loopback-server redirector)
      (stop-loopback-server sink))))

(deftest test-default-transport-round-trips-utf8
  ;; application/json is not a Drakma text content type, so the body arrives
  ;; as octets.  Decoding them one byte per character corrupts every
  ;; non-ASCII character, in both directions.
  (let ((server (start-loopback-server
                 (lambda (request stream)
                   (declare (ignore request))
                   (%write-http-response
                    stream 200
                    (openai-text-response "Answer: São Paulo is 12°C — mild"))))))
    (unwind-protect
         (let* ((client (ai :name "openai" :model +test-openai-model+
                            :api-key "sk-loopback-key"
                            :base-url (concatenate 'string (loopback-url server) "/v1")))
                (result (chat client (vector (message "user" "Tempo em São Paulo? 12°C?")))))
           (expect-equal (jget result "content") "Answer: São Paulo is 12°C — mild"
                         "a non-ASCII response body survives the round trip")
           (let ((body (parse-json (jget (first (loopback-requests server)) "body"))))
             (expect-equal (jget (aref (jget body "messages") 0) "content")
                           "Tempo em São Paulo? 12°C?"
                           "a non-ASCII request body reaches the server intact")))
      (stop-loopback-server server))))

(deftest test-correction-after-tool-round-retains-tools-for-both-providers
  (dolist (provider '("openai" "anthropic"))
    (multiple-value-bind (weather invocations) (make-recording-weather-tool)
      (multiple-value-bind (client script)
          (if (string= provider "anthropic")
              (scripted-client
               "anthropic"
               (list (encode-json
                      (object "stop_reason" "tool_use"
                              "content" (vector (object "type" "tool_use" "id" "toolu_1"
                                                        "name" "get_weather"
                                                        "input" (object "city" "Oslo")))
                              "usage" (object "input_tokens" 5 "output_tokens" 2)))
                     (anthropic-text-response "Reason: none given")
                     (anthropic-text-response "Answer: 12C in Oslo")))
              (scripted-client
               "openai"
               (list (openai-tool-response
                      (list (openai-tool-call "call_1" "get_weather" "{\"city\":\"Oslo\"}")))
                     (openai-text-response "Reason: none given")
                     (openai-text-response "Answer: 12C in Oslo"))))
        ;; A reply that labels only the optional field leaves the required one
        ;; missing, so the turn is rejected and corrected.
        (let* ((gen (ax "question:string -> answer:string, reason?:string"
                        :tools (list weather) :max-retries 1))
               (outputs (forward gen client (object "question" "weather in Oslo?"))))
          (expect-equal (jget outputs "answer") "12C in Oslo"
                        (format nil "~a: corrected output after a tool round" provider))
          (expect-equal (script-call-count script) 3
                        (format nil "~a: tool round, rejected turn, correction" provider))
          (expect-equal (length (funcall invocations)) 1
                        (format nil "~a: the tool handler is not replayed during correction"
                                provider))
          (let ((correction (script-request script 2)))
            (expect (jget correction "tools")
                    (format nil "~a: tool definitions retained while the history holds tool use"
                            provider))
            ;; The scripted transport sees the wire, and both providers spell a
            ;; forbidden call as "none" there now that Core builds the body.
            ;; A correction turn no longer forbids a call on the wire: Core allows
            ;; one, and the correction prompt is what asks the model to answer
            ;; instead.  What must survive is that the tools are still declared,
            ;; which both providers require while the history holds tool use.
            (expect (jget correction "tools")
                    (format nil "~a: the tools are still declared" provider))))))))

;;; Whether a provider body may carry an empty assistant turn, and which
;;; malformed response shapes are refused, are decisions in the body Core builds
;;; and the response Core normalizes.  They moved to the provider's own suite
;;; with the factory flip; this suite asserts what the generator does with a
;;; normalized response, not how one is produced.

(deftest test-a-refused-or-truncated-completion-does-not-become-an-answer
  ;; Neither a refusal nor a truncation may be returned as output.  Truncation is
  ;; Core's classification and ends the run; a refusal reaches the generator as a
  ;; completion with no usable fields, so it fails validation rather than being
  ;; handed back.  Either way the caller gets an error, not half an answer.
  (dolist (finish '("length" "content_filter"))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response "" :finish finish)))
      (declare (ignore script))
      (expect (handler-case
                  (progn (forward (ax "q:string -> answer:string" :max-retries 0)
                                  client (object "q" "go"))
                         nil)
                (error () t))
              (format nil "a ~a completion does not become an answer" finish)))))

(deftest test-explicit-json-null-optional-fields-are-absent
  ;; Wire-level nulls, not omitted keys.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (encode-json
                              (object "choices"
                                      (vector (object "finish_reason" :null
                                                      "message" (object "role" "assistant"
                                                                        "content" "Answer: ok"
                                                                        "refusal" :null
                                                                        "tool_calls" :null)))
                                      "usage" :null))))
    (declare (ignore script))
    (let ((result (chat client (vector (message "user" "hi")))))
      (expect-equal (jget result "content") "Answer: ok" "content read past the null siblings")
      (expect-equal (length (jget result "toolCalls")) 0 "null tool_calls normalizes to empty")
      (expect-equal (jget result "finishReason") "" "null finish_reason normalizes to empty")
      (expect-equal (jget (jget result "usage") "totalTokens") 0 "null usage normalizes to zeros")))
  (multiple-value-bind (client script)
      (scripted-client "anthropic"
                       (list (encode-json
                              (object "stop_reason" :null
                                      "content" (vector (object "type" "text" "text" "Answer: ok"))
                                      "usage" :null))))
    (declare (ignore script))
    (let ((result (chat client (vector (message "user" "hi")))))
      (expect-equal (jget result "content") "Answer: ok" "anthropic content with null siblings")
      (expect-equal (jget (jget result "usage") "promptTokens") 0 "null usage normalizes to zeros"))))

(deftest test-a-provider-failure-reaches-the-caller-as-a-typed-error
  ;; Whatever a service does wrong, a caller must get a typed Ax error rather
  ;; than a raw Lisp condition leaking from a parser or a transport.
  (dolist (payload (list "not json at all" "[]" "{\"choices\":7}"))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list payload))
      (declare (ignore script))
      (let ((failure (handler-case (progn (chat client (vector (message "user" "hi"))) nil)
                       (ax-error (condition) condition)
                       (error (condition) condition))))
        (expect (typep failure 'ax-error)
                (format nil "~s produces a typed Ax error, got ~a" payload (type-of failure)))))))

(deftest test-non-integer-transport-status-is-rejected
  (let ((client (ai :name "openai" :model +test-openai-model+ :api-key "dummy"
                    :transport (lambda (url headers body)
                                 (declare (ignore url headers body))
                                 (values (openai-text-response "Answer: ok") "200")))))
    (expect-error provider-error provider-error-kind :transport
      (chat client (vector (message "user" "hi")))))
  ;; A transport that returns only a body is still accepted as 200.
  (let ((client (ai :name "openai" :model +test-openai-model+ :api-key "dummy"
                    :transport (lambda (url headers body)
                                 (declare (ignore url headers body))
                                 (openai-text-response "Answer: ok")))))
    (expect-equal (jget (chat client (vector (message "user" "hi"))) "content") "Answer: ok"
                  "a single-value transport defaults to status 200")))

(deftest test-number-parsing-follows-core
  ;; Core coerces a number the way the other ports do, so "+5" is five rather
  ;; than a validation failure, and text that is not a number still fails.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "N: +5")))
    (declare (ignore script))
    (expect-equal (jget (forward (ax "q:string -> n:number") client (object "q" "go")) "n") 5
                  "a leading plus is accepted, as in every other port"))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "N: nope")))
    (declare (ignore script))
    (let ((gen (ax "q:string -> n:number" :max-retries 0)))
      (let ((condition (expect-error generation-error generation-error-kind :validation
                        (forward gen client (object "q" "go")))))
        (expect (find-if (lambda (p) (search "Invalid number" p))
                         (generation-error-problems condition))
                "text that is not a number is a validation failure")))))

(deftest test-label-matching-follows-core
  ;; A line that merely looks like a label is part of the previous field's text,
  ;; because Core only opens a field for a label the signature declares.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (%join-lines '("Answer: see below" "Arguments: x is a list")
                                           :trailing nil))))
    (declare (ignore script))
    (expect-equal (jget (forward (ax "q:string -> answer:string") client (object "q" "go"))
                        "answer")
                  (%join-lines '("see below" "Arguments: x is a list") :trailing nil)
                  "an undeclared label stays inside the field it follows"))
  ;; Unlabelled text fills a lone required field rather than failing.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "no labelled fields here")))
    (declare (ignore script))
    (expect-equal (jget (forward (ax "q:string -> answer:string") client (object "q" "go"))
                        "answer")
                  "no labelled fields here"
                  "a single required field takes the whole reply when nothing is labelled")))

(deftest test-tool-names-reject-trailing-newlines
  (dolist (bad (list (format nil "deploy~c" #\Newline)
                     (format nil "deploy~cx" #\Newline)
                     "deploy ship" "0deploy" "deploy!" ""))
    (handler-case
        (progn (tool :name bad :handler (lambda (args) (declare (ignore args)) ""))
               (error 'test-failure
                      :text (format nil "expected tool name ~s to be rejected" bad)))
      (tool-error () t)))
  (expect (jget (tool :name "deploy-v2_1" :handler (lambda (args) (declare (ignore args)) ""))
                "name")
          "a valid identifier is still accepted"))

(deftest test-the-token-cap-is-one-option-mapped-per-provider
  ;; :max-tokens is a single option on the client; Core maps it to each
  ;; provider's own spelling.  Asserting both spellings keeps this a test of that
  ;; mapping rather than of one dialect.
  (multiple-value-bind (transport script)
      (make-scripted-transport (list (openai-text-response "Answer: ok")))
    (let ((client (ai :name "openai" :model +test-openai-model+ :api-key "sk-test-dummy-key"
                      :max-tokens 4096 :transport transport)))
      (expect-equal (ai-max-tokens client) 4096 "the client reports the cap it was built with")
      (forward (ax "q:string -> answer:string") client (object "q" "go"))
      (expect-equal (jget (script-request script 0) "max_completion_tokens") 4096
                    "openai gets max_completion_tokens")))
  (multiple-value-bind (transport script)
      (make-scripted-transport (list (anthropic-text-response "Answer: ok")))
    (let ((client (ai :name "anthropic" :model +test-anthropic-model+ :api-key "sk-test-dummy-key"
                      :max-tokens 4096 :transport transport)))
      (forward (ax "q:string -> answer:string") client (object "q" "go"))
      (expect-equal (jget (script-request script 0) "max_tokens") 4096
                    "anthropic gets max_tokens, which it requires")))
  ;; Without the option each profile keeps its own default rather than this
  ;; suite's guess at one.
  (multiple-value-bind (client script)
      (scripted-client "anthropic" (list (anthropic-text-response "Answer: ok")))
    (forward (ax "q:string -> answer:string") client (object "q" "go"))
    (let ((cap (jget (script-request script 0) "max_tokens")))
      (expect (and (integerp cap) (plusp cap))
              (format nil "anthropic always gets a cap, got ~s" cap)))))

(deftest test-no-provider-error-path-reflects-the-credential
  (let ((secret "sk-reflect-me-0123456789"))
    (flet ((check (label responses)
             (multiple-value-bind (client script) (scripted-client "openai" responses
                                                                   :api-key secret)
               (declare (ignore script))
               (let ((condition (handler-case (progn (chat client (vector (message "user" "hi")))
                                                     nil)
                                  (provider-error (c) c))))
                 (expect condition (format nil "~a signals a provider-error" label))
                 (expect-not-contains (princ-to-string condition) secret
                                      (format nil "~a does not reflect the credential" label))))))
      ;; The key echoed inside an error body.
      (check "http error body"
             (list (cons 500 (encode-json
                              (object "error" (object "message"
                                                      (format nil "bad key ~a" secret)))))))
      (check "auth error body"
             (list (cons 401 (encode-json
                              (object "error" (object "message"
                                                      (format nil "bad key ~a" secret)))))))
      ;; The key echoed inside a refusal message.
      (check "refusal text"
             (list (encode-json
                    (object "choices"
                            (vector (object "finish_reason" "stop"
                                            "message"
                                            (object "role" "assistant" "content" :null
                                                    "refusal"
                                                    (format nil "I will not use ~a" secret))))))))
      ;; A content_filter finish no longer signals at the transport boundary; the
      ;; generator refuses it, which is covered by
      ;; test-a-refused-or-truncated-completion-does-not-become-an-answer.
      )
    ;; The key echoed by a raising transport, which bypasses the normalizer.
    (let ((client (ai :name "openai" :model +test-openai-model+ :api-key secret
                      :transport (lambda (url headers body)
                                   (declare (ignore url headers body))
                                   (error "upstream rejected key ~a" secret)))))
      (let ((condition (handler-case (progn (chat client (vector (message "user" "hi"))) nil)
                         (provider-error (c) c))))
        (expect condition "a raising transport yields a provider-error")
        (expect-equal (provider-error-kind condition) :transport "classified as a transport failure")
        (expect-contains (princ-to-string condition) "[redacted]"
                         "the transport condition text is redacted")
        (expect-not-contains (princ-to-string condition) secret
                             "a raising transport cannot reflect the credential")))
    ;; And the client never prints its key.
    (let ((client (ai :name "openai" :model +test-openai-model+ :api-key secret
                      :transport (lambda (url headers body)
                                   (declare (ignore url headers body))
                                   (openai-text-response "Answer: ok")))))
      (expect-not-contains (princ-to-string client) secret
                           "printing the client does not expose the key"))))

;;; ------------------------------------------------------------------
;;; Regressions: a malformed batch must not leave a side effect behind
;;; ------------------------------------------------------------------

(defun make-effect-tool (name log)
  "A tool whose only job is to record that its handler ran."
  (tool :name name
        :description (format nil "Record a call to ~a" name)
        :parameters (object "type" "object"
                            "properties" (object "key" (object "type" "string"))
                            "required" (vector "key"))
        :handler (lambda (args)
                   (push (cons name (jget args "key")) (cdr log))
                   "recorded")))

(deftest test-a-malformed-later-tool-call-runs-no-earlier-handler
  ;; The reproduced failure: a batch whose first call is valid and whose second
  ;; is missing its id used to invoke the first handler before the batch was
  ;; rejected.  A tool call is a side effect; rejecting the batch afterwards
  ;; cannot undo it, so nothing may run until the whole batch is known good.
  ;;
  ;; Each case asserts the boundary that owns it.  An unusable wire shape is the
  ;; provider's to refuse, and it refuses the whole response rather than letting
  ;; a half-usable batch through; a reused call id is the generator's, because
  ;; that is the layer which keys a tool result by its id.  Both must leave no
  ;; handler effect, one request, and a failed trace.
  (dolist (case (list
                 ;; The reported case: a second call with no id at all.
                 (list 'provider-error :response
                       (object "type" "function"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" "" "type" "function"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" "  " "type" "function"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" :null "type" "function"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" 7 "type" "function"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" "call_b" "type" "function"
                               "function" (object "name" "" "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" "call_b" "type" "function"
                               "function" (object "name" "  " "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response
                       (object "id" "call_b" "type" "tool"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))
                 (list 'provider-error :response :null)
                 ;; A reused id would make two results indistinguishable, and
                 ;; only the generator knows it already answered that id.
                 (list 'generation-error :tool
                       (object "id" "call_a" "type" "function"
                               "function" (object "name" "effect_b"
                                                  "arguments" "{\"key\":\"b\"}")))))
    (destructuring-bind (condition-type kind broken) case
      ;; Core now checks the normalized batch before memory/tools, and wraps
      ;; that validation in AxGenerateError. Duplicate ids remain a native
      ;; generation tool error. Assert the new boundary, not just any error.
      (when (eq condition-type 'provider-error)
        (setf condition-type 'generation-error kind :generation))
      (let* ((log (list :log))
             (tool-a (make-effect-tool "effect_a" log))
             (tool-b (make-effect-tool "effect_b" log)))
        (multiple-value-bind (client script)
            (scripted-client "openai"
                             (list (encode-json
                                    (object "choices"
                                            (vector (object "index" 0
                                                            "finish_reason" "tool_calls"
                                                            "message"
                                                            (object "role" "assistant"
                                                                    "content" ""
                                                                    "tool_calls"
                                                                    (vector
                                                                     (object "id" "call_a"
                                                                             "type" "function"
                                                                             "function"
                                                                             (object "name" "effect_a"
                                                                                     "arguments" "{\"key\":\"a\"}"))
                                                                     broken))))))))
          (let ((gen (ax "question:string -> answer:string" :tools (list tool-a tool-b)))
                (described (if (eq broken :null) "null" (encode-json broken))))
            (let ((signalled
                    (handler-case (progn (forward gen client (object "question" "go")) nil)
                      (ax-error (c) c))))
              (expect (typep signalled condition-type)
                      (format nil "a second call of ~a is refused by ~a, got ~a"
                              described condition-type signalled))
              (when (eq kind :generation)
                (expect (typep (ax-generate-error-cause signalled) 'ax-error)
                        "the Core batch validation failure survives as the cause"))
              (expect-equal (funcall (if (eq condition-type 'provider-error)
                                         #'provider-error-kind
                                         #'generation-error-kind)
                                     signalled)
                            kind
                            (format nil "~a is reported as kind ~a" described kind)))
            (expect-equal (cdr log) '()
                          (format nil "no handler ran for a batch whose second call is ~a"
                                  described))
            (expect-equal (script-call-count script) 1
                          "the batch is rejected without a second provider request")
            (expect-equal (length (program-traces gen)) 1 "the rejected run is still traced")
            (expect-equal (jget (aref (program-traces gen) 0) "status") "error"
                          "the trace records the failure")))))))

(deftest test-a-tool-call-must-be-answerable-before-it-is-run
  ;; This layer checks only what it needs to answer a call: an id to key the
  ;; result by and a name to resolve.  The wire shape, including the call kind
  ;; and the exact cross-port message text, belongs to the provider layer
  ;; (ir/axcore/ai.axir), which rejects a malformed response before gen sees it.
  (expect-equal (%tool-batch-problem :null 0)
                "Function call at index 0 cannot be null or undefined."
                "a null call")
  (expect-equal (%tool-batch-problem (object "name" "lookup") 0)
                "Function call at index 0 must have a non-empty string id."
                "an absent id")
  (expect-equal (%tool-batch-problem (object "id" :null "name" "lookup") 1)
                "Function call at index 1 must have a non-empty string id."
                "an explicit null id keeps its index")
  (expect-equal (%tool-batch-problem (object "id" 7 "name" "lookup") 0)
                "Function call at index 0 must have a non-empty string id."
                "a non-string id")
  (expect-equal (%tool-batch-problem (object "id" "  " "name" "lookup") 0)
                "Function call at index 0 must have a non-empty string id."
                "a blank id")
  (expect-equal (%tool-batch-problem (object "id" "call_1" "name" "  ") 0)
                "Function call at index 0 must have a non-empty function name."
                "a blank name")
  (expect-equal (%tool-batch-problem (object "id" "call_1" "name" "lookup") 0) nil
                "a call with an id and a name is answerable"))

(deftest test-a-well-formed-batch-still-runs-every-handler
  ;; The guard above must not be a blanket refusal: two valid calls in one batch
  ;; both run, in order.
  (let* ((log (list :log))
         (tool-a (make-effect-tool "effect_a" log))
         (tool-b (make-effect-tool "effect_b" log)))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_a" "effect_a" "{\"key\":\"a\"}")
                                      (openai-tool-call "call_b" "effect_b" "{\"key\":\"b\"}")))
                               (openai-text-response "Answer: both ran")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list tool-a tool-b)))
             (outputs (forward gen client (object "question" "go"))))
        (expect-equal (jget outputs "answer") "both ran" "the run completes")
        (expect-equal (reverse (cdr log)) '(("effect_a" . "a") ("effect_b" . "b"))
                      "both handlers ran, in batch order")
        (expect-equal (script-call-count script) 2 "one tool round and one answer")
        (let ((traces (program-traces gen))
              (calls (generator-function-call-traces gen)))
          (expect-equal (length calls) 2 "both calls are traced")
          (expect-equal (jget (aref calls 0) "name") "effect_a" "first trace names the first tool")
          (expect-equal (jget (aref calls 0) "status") "ok" "a successful call is traced as ok")
          (expect-equal (jget (aref calls 0) "result") "recorded" "the trace keeps the result text")
          (expect-equal (jget (aref traces 0) "status") "ok" "the run trace is ok"))))))

(deftest test-an-invalid-argument-in-a-batch-still-corrects-per-call
  ;; Argument validation is per call, not per batch: a bad argument object is a
  ;; correctable mistake the model is told about, while its sibling still runs.
  (let* ((log (list :log))
         (tool-a (make-effect-tool "effect_a" log))
         (tool-b (make-effect-tool "effect_b" log)))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_a" "effect_a" "{\"key\":\"a\"}")
                                      (openai-tool-call "call_b" "effect_b" "{\"key\":5}")))
                               (openai-text-response "Answer: partial")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list tool-a tool-b)))
             (outputs (forward gen client (object "question" "go"))))
        (expect-equal (jget outputs "answer") "partial" "the run continues")
        (expect-equal (cdr log) '(("effect_a" . "a")) "only the valid call reached a handler")
        (let ((messages (jget (script-request script 1) "messages")))
          (expect-contains (tool-message-error (aref messages 4))
                           "Expected string, received number"
                           "the invalid call is reported back to the model"))))))

;;; ------------------------------------------------------------------
;;; Regressions: absent textual values and tool result text
;;; ------------------------------------------------------------------

(deftest test-an-optional-field-written-as-null-is-omitted
  ;; ir/conformance/axgen/optional-null.json: "Middle Name: null" for an
  ;; optional field is the model saying there is no middle name, not the
  ;; four-character string "null".
  (dolist (text '("null" "NULL" " null " "undefined" "Null"))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-text-response
                                (format nil "Display Name: Ada~cMiddle Name: ~a" #\Newline text))))
      (let* ((gen (ax "name:string -> displayName:string, middleName?:string"))
             (outputs (forward gen client (object "name" "Ada"))))
        (expect-equal (jget outputs "displayName") "Ada" "the required field is parsed")
        (expect-equal (nth-value 1 (gethash "middleName" outputs)) nil
                      (format nil "the optional field is absent, not the text ~s" text))
        (expect-equal (hash-table-count outputs) 1 "exactly one output field")
        (expect-equal (script-call-count script) 1 "no correction turn was needed")))))

(deftest test-a-required-field-written-as-null-is-a-validation-failure
  ;; The same text in a required field is a missing value, so the model is
  ;; corrected rather than handed the word "null" as the answer.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response "Answer: null")
                             (openai-text-response "Answer: real")))
    (let* ((gen (ax "question:string -> answer:string"))
           (outputs (forward gen client (object "question" "go"))))
      (expect-equal (jget outputs "answer") "real" "the correction turn produced a value")
      (expect-equal (script-call-count script) 2 "one correction turn")
      (expect-contains (encode-json (jget (script-request script 1) "messages"))
                       "Do not use null, undefined, or leave it blank"
                       "the correction names the problem")))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: undefined")))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string" :max-retries 0)))
      (let ((condition (expect-error generation-error generation-error-kind :validation
                        (forward gen client (object "question" "go")))))
        (expect (find-if (lambda (problem) (search "Required field is missing: 'Answer'" problem))
                         (generation-error-problems condition))
                "the exhausted failure names the missing field")))))

(deftest test-a-nothing-result-is-reported-to-the-model-as-done
  ;; ir/conformance/axgen/function-result-format-default.json: a handler that
  ;; returns no value succeeded.  Telling the model "null" invites it to report
  ;; a failure that did not happen.
  (expect-equal (tool-result-text nil) "done" "a Lisp nil result reads as done")
  (expect-equal (tool-result-text :null) "done" "a JSON null result reads as done")
  (expect-equal (tool-result-text "") "done" "an empty string result reads as done")
  (expect-equal (tool-result-text "plain text") "plain text" "a string passes through")
  (expect-equal (tool-result-text 42) "42" "a number renders as JSON")
  (expect-equal (tool-result-text (object "a" (vector 2) "b" 1 "c" :null))
                (format nil "{~c  \"a\": [~c    2~c  ],~c  \"b\": 1,~c  \"c\": null~c}"
                        #\Newline #\Newline #\Newline #\Newline #\Newline #\Newline)
                "an object renders as two-space indented JSON in key order")
  (expect-equal (tool-result-text (object)) "{}" "an empty object stays compact")
  (expect-equal (tool-result-text (vector)) "[]" "an empty array stays compact")
  (let ((quiet (tool :name "quiet"
                     :description "Does the work and says nothing"
                     :handler (lambda (args) (declare (ignore args)) nil))))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_q" "quiet" "{}")))
                               (openai-text-response "Answer: finished")))
      (let* ((gen (ax "question:string -> answer:string" :tools (list quiet)))
             (outputs (forward gen client (object "question" "go"))))
        (expect-equal (jget outputs "answer") "finished" "the run completes")
        (let* ((messages (jget (script-request script 1) "messages"))
               (tool-message (find-if (lambda (m) (equal (jget m "role") "tool")) messages)))
          (expect-equal (jget tool-message "content") "done"
                        "the model is told the tool is done, not that the result was null"))))))

;;; ------------------------------------------------------------------
;;; Program contract
;;; ------------------------------------------------------------------

(deftest test-generator-reports-its-optimizable-components
  ;; ir/conformance/axprogram/axgen-component-contract.json
  (let* ((gen (ax "question:string -> answer:string" :id "qa" :description "Answer questions."))
         (components (program-optimizable-components gen)))
    (expect-equal (map 'list (lambda (c) (jget c "id")) components)
                  '("qa::description" "qa::instruction")
                  "a described generator exposes its description and instruction")
    (expect-equal (jget (aref components 0) "current") "Answer questions."
                  "the description component carries the current text")
    (expect-equal (jget (aref components 0) "owner") "qa" "components name their owner"))
  (let ((bare (ax "question:string -> answer:string")))
    (expect-equal (map 'list (lambda (c) (jget c "id")) (program-optimizable-components bare))
                  '("root::instruction")
                  "an undescribed generator exposes only its instruction, under the default id"))
  (let* ((lookup (tool :name "lookup" :description "Look up a key"
                       :handler (lambda (args) (declare (ignore args)) "x")))
         (gen (ax "question:string -> answer:string" :id "qa" :tools (list lookup))))
    (expect-equal (map 'list (lambda (c) (jget c "id")) (program-optimizable-components gen))
                  '("qa::instruction" "qa::fn:lookup:desc" "qa::fn:lookup:name")
                  "each tool contributes a description and a name component")))

(deftest test-optimized-components-are-applied-and-checked
  (let* ((lookup (tool :name "lookup" :description "Look up a key"
                       :handler (lambda (args) (declare (ignore args)) "x")))
         (gen (ax "question:string -> answer:string"
                  :id "qa" :description "Old." :tools (list lookup))))
    (program-apply-optimized-components
     gen (object "qa::description" "New description."
                 "qa::instruction" "Be brief."
                 "qa::fn:lookup:desc" "Resolve one key."
                 "qa::fn:lookup:name" "resolve_key"
                 "other::instruction" "ignored"))
    (expect-equal (generator-description gen) "New description." "the description is replaced")
    (expect-equal (generator-instruction gen) "Be brief." "the instruction is replaced")
    (expect-equal (jget lookup "name") "resolve_key" "the tool is renamed")
    (expect-equal (jget lookup "description") "Resolve one key." "the tool description is replaced")
    (expect (gethash "resolve_key" (generator-tool-index gen))
            "the tool index follows the rename")
    (expect (not (gethash "lookup" (generator-tool-index gen)))
            "the old tool name is gone from the index")
    (let ((system (jget (aref (render-prompt (generator-signature gen) (object "question" "q")
                                             :options (%prompt-options gen (object)))
                              0)
                        "content")))
      (expect-contains system "Be brief." "the instruction reaches the rendered prompt")))
  (let* ((lookup (tool :name "lookup" :handler (lambda (args) (declare (ignore args)) "x")))
         (gen (ax "question:string -> answer:string" :id "qa" :tools (list lookup))))
    (expect-error generation-error generation-error-kind :config
      (program-apply-optimized-components gen (object "qa::fn:lookup:name" "Not Snake Case")))
    (expect-equal (jget lookup "name") "lookup" "a rejected rename leaves the tool alone"))
  (let* ((a (tool :name "lookup_a" :handler (lambda (args) (declare (ignore args)) "a")))
         (b (tool :name "lookup_b" :handler (lambda (args) (declare (ignore args)) "b")))
         (gen (ax "question:string -> answer:string" :id "qa" :tools (list a b))))
    (expect-error generation-error generation-error-kind :config
      (program-apply-optimized-components gen (object "qa::fn:lookup_a:name" "lookup_b")))
    (expect-equal (jget a "name") "lookup_a" "a colliding rename is refused before it is applied")
    (expect-equal (jget b "name") "lookup_b" "the other tool is untouched")))

(deftest test-program-records-usage-chat-log-and-traces
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response "Answer: one" :prompt 3 :completion 4)
                             (openai-text-response "Answer: two" :prompt 5 :completion 6)))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string")))
      (forward gen client (object "question" "a"))
      (forward gen client (object "question" "b"))
      (let ((usage (program-usage gen)))
        (expect-equal (length usage) 1 "one entry per provider and model")
        ;; A usage entry is exactly the three token counts: a subset assertion
        ;; over a usage list compares the list by value in every port, so an
        ;; extra key would make the comparison fail.
        (expect-equal (sort (%object-keys (aref usage 0)) #'string<)
                      '("completion_tokens" "prompt_tokens" "total_tokens")
                      "an entry carries exactly the three snake_case token counts")
        (expect-equal (jget (aref usage 0) "prompt_tokens") 8 "prompt tokens accumulate")
        (expect-equal (jget (aref usage 0) "completion_tokens") 10 "completion tokens accumulate")
        (expect-equal (jget (aref usage 0) "total_tokens") 18 "total tokens accumulate"))
      (let ((by-model (program-usage-by-model gen)))
        (expect-equal (jget (aref by-model 0) "ai") "openai"
                      "the provider is reported beside the counts, not inside them")
        (expect-equal (jget (aref by-model 0) "model") +test-openai-model+ "and so is the model")
        (expect-equal (jget (aref by-model 0) "total_tokens") 18 "with the same totals"))
      (let ((log (program-chat-log gen)))
        (expect-equal (length log) 2 "one chat-log entry per provider turn")
        ;; The content is lifted beside the response, so a reader of the log does
        ;; not have to know which shape the service answered in.
        (expect-equal (jget (aref log 0) "content") "Answer: one"
                      "the entry keeps the content")
        (expect (jget (aref log 0) "response") "and the response it came from")
        (expect-equal (jget (jget (aref log 0) "response") "content") "Answer: one"
                      "the recorded completion exposes response.content to flows")
        (expect-equal (jget (jget (aref log 0) "usage") "total_tokens") 7
                      "the entry keeps that turn's usage, in the one spelling"))
      (let ((traces (program-traces gen)))
        (expect-equal (length traces) 2 "one trace per run")
        (expect-equal (jget (jget (aref traces 0) "input") "question") "a" "the trace keeps the input")
        (expect-equal (jget (jget (aref traces 0) "output") "answer") "one"
                      "the trace keeps the output")
        (expect-equal (jget (aref traces 0) "status") "ok" "a completed run traces as ok"))
      (let ((history (memory-history (generator-memory gen))))
        (expect-equal (mapcar (lambda (entry) (jget entry "role")) history)
                      '("request" "assistant" "request" "assistant")
                      "memory keeps the request and response of each turn")))))

(deftest test-memory-skips-an-empty-response
  ;; ir/conformance/axgen/empty-response-memory-skip.json: a blank completion is
  ;; the provider failing to answer, so remembering it would replay the blank.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response (format nil " ~c " #\Newline))
                             (openai-text-response "Answer: recovered")))
    (declare (ignore script))
    (let* ((gen (ax "question:string -> answer:string"))
           (outputs (forward gen client (object "question" "blank?"))))
      (expect-equal (jget outputs "answer") "recovered" "the correction turn recovered")
      (let ((history (memory-history (generator-memory gen))))
        (expect-equal (count "assistant" history
                             :test #'equal :key (lambda (e) (jget e "role")))
                      1 "only the meaningful response is remembered")
        (expect-equal (%response-content (jget (first (last history)) "response"))
                      "Answer: recovered"
                      "the remembered response is the one that answered")))))

(deftest test-forward-options-override-the-call-budgets
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: null")))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string" :max-retries 2)))
      ;; maxRetries 0 for this call alone: the generator still allows two.
      (expect-error generation-error generation-error-kind :validation
        (forward gen client (object "question" "go") (object "maxRetries" 0)))))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: null")))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string" :max-retries 2)))
      (expect-error generation-error generation-error-kind :validation
        (forward gen client (object "question" "go") (object "max_retries" 0)))))
  (let ((lookup (tool :name "lookup"
                      :description "Look up"
                      :handler (lambda (args) (declare (ignore args)) "x"))))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_a" "lookup" "{}")))))
      (declare (ignore script))
      (let ((gen (ax "question:string -> answer:string" :tools (list lookup) :max-steps 5)))
        (expect-error generation-error generation-error-kind :steps
          (forward gen client (object "question" "go") (object "maxSteps" 0))))))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string")))
      (expect-error generation-error generation-error-kind :config
        (forward gen client (object "question" "go") (object "maxRetries" -1)))
      (expect-error generation-error generation-error-kind :config
        (forward gen client (object "question" "go") (object "maxSteps" "many")))
      (expect-error generation-error generation-error-kind :config
        (forward gen client (object "question" "go") 7)))))

;;; ------------------------------------------------------------------
;;; Memory tags
;;; ------------------------------------------------------------------

(deftest test-memory-rewind-drops-the-tagged-item-and-everything-after
  ;; src/ax/mem/memory.ts rewindToTag removes the FIRST item carrying the tag
  ;; and every item after it.  Keeping the last one instead would leave the
  ;; failed turn's earlier messages in the conversation on a retry.
  (let ((memory (make-instance 'memory)))
    (memory-add-request memory (vector (message "user" "one")))
    (memory-add-tag memory "attempt")
    (memory-add-response memory (object "content" "first"))
    (memory-add-request memory (vector (message "user" "two")))
    (memory-add-tag memory "attempt")
    (memory-add-response memory (object "content" "second"))
    (expect-equal (length (memory-history memory)) 4 "four items before the rewind")
    (let ((removed (memory-rewind-to-tag memory "attempt")))
      (expect-equal (length removed) 4
                    "the first tagged item and all three after it are removed")
      (expect-equal (jget (aref removed 0) "role") "request" "the removed run starts at the tag")
      (expect-equal (memory-history memory) '() "nothing is left before the first tag")))
  (let ((memory (make-instance 'memory)))
    (memory-add-request memory (vector (message "user" "keep")))
    (memory-add-response memory (object "content" "keep"))
    (memory-add-request memory (vector (message "user" "drop")))
    (memory-add-tag memory "retry")
    (memory-add-response memory (object "content" "drop"))
    (let ((removed (memory-rewind-to-tag memory "retry")))
      (expect-equal (length removed) 2 "only the tagged turn onwards is removed")
      (expect-equal (mapcar (lambda (e) (jget e "role")) (memory-history memory))
                    '("request" "assistant")
                    "the untagged prefix survives"))
    ;; A tag this memory has seen but has already rewound past removes nothing.
    (expect-equal (length (memory-rewind-to-tag memory "retry")) 0
                  "a seen tag that is gone rewinds nothing")
    ;; A tag it has never seen is a programming error, not a silent no-op.
    (expect-error generation-error generation-error-kind :config
      (memory-rewind-to-tag memory "never-applied")))
  (let ((memory (make-instance 'memory)))
    ;; A tag applied twice to the same item is recorded once.
    (memory-add-request memory (vector (message "user" "one")))
    (memory-add-tag memory "same")
    (memory-add-tag memory "same")
    (expect-equal (length (jget (first (memory-history memory)) "tags")) 1
                  "a repeated tag is not duplicated")
    (expect-equal (length (memory-rewind-to-tag memory "same")) 1 "the item is removed once"))
  (let ((memory (make-instance 'memory)))
    ;; Tagging an empty memory records nothing, and the tag stays unseen.
    (memory-add-tag memory "early")
    (expect-error generation-error generation-error-kind :config
      (memory-rewind-to-tag memory "early")))
  (let ((a (make-instance 'memory))
        (b (make-instance 'memory)))
    ;; Two memories do not share tag state.
    (memory-add-request a (vector (message "user" "a")))
    (memory-add-tag a "shared")
    (expect-equal (length (memory-rewind-to-tag a "shared")) 1 "the owner can rewind")
    (expect-error generation-error generation-error-kind :config
      (memory-rewind-to-tag b "shared"))))

(deftest test-memory-remove-by-tag-removes-every-tagged-item-in-order
  (let ((memory (make-instance 'memory)))
    (memory-add-request memory (vector (message "user" "one")))
    (memory-add-tag memory "mark")
    (memory-add-response memory (object "content" "between"))
    (memory-add-request memory (vector (message "user" "two")))
    (memory-add-tag memory "mark")
    (let ((removed (memory-remove-by-tag memory "mark")))
      (expect-equal (length removed) 2 "both tagged items are removed")
      (expect-equal (map 'list (lambda (e) (jget e "role")) removed) '("request" "request")
                    "the removed items are the tagged entries, oldest first")
      (expect-equal (mapcar (lambda (e) (jget e "role")) (memory-history memory))
                    '("assistant")
                    "the untagged item between them survives")))
  (let ((memory (make-instance 'memory)))
    (expect-equal (length (memory-remove-by-tag memory "absent")) 0
                  "removing an absent tag removes nothing")))

(deftest test-memory-keeps-sessions-and-sample-indices-apart
  (let ((memory (make-instance 'memory)))
    (memory-add-request memory (vector (message "user" "a")) :session-id "s1" :index 0)
    (memory-add-response memory (object "content" "a") :session-id "s1" :index 0)
    (memory-add-request memory (vector (message "user" "b")) :session-id "s2" :index 1)
    (expect-equal (length (memory-history memory)) 3 "every item is kept")
    (expect-equal (length (memory-history memory :session-id "s1")) 2 "one session's items")
    (expect-equal (length (memory-history memory :session-id "s2")) 1 "the other session's items")
    (expect-equal (length (memory-history memory :index 1)) 1 "one sample index's items")
    (expect-equal (jget (first (memory-history memory :session-id "s2")) "session_id") "s2"
                  "the session id is stored on the item")
    (let ((history (memory-history memory)))
      (setf (gethash "role" (first history)) "tampered")
      (expect-equal (jget (first (memory-history memory)) "role") "tampered"
                    "history shares the stored entries"))
    (let ((snapshot (memory-history memory)))
      (memory-add-response memory (object "content" "later"))
      (expect-equal (length snapshot) 3 "an earlier snapshot is not extended by a later write"))))

(defclass recording-control () ((sink :initarg :sink :reader recording-control-sink)))

(defmethod axllm/core::core-host-call ((control recording-control) method args)
  "A run control that records the events a program emits on it."
  (if (equal method "_emit")
      (progn (map nil (recording-control-sink control) args) axllm:true)
      (call-next-method)))

(defun make-recording-control (sink)
  (make-instance 'recording-control :sink sink))

;;; ------------------------------------------------------------------
;;; Program hooks used by flows, agents and optimizers
;;; ------------------------------------------------------------------

(deftest test-a-program-answers-the-core-boundaries
  (let* ((lookup (tool :name "lookup" :description "Look up a key"
                       :handler (lambda (args) (declare (ignore args)) "x")))
         (gen (ax "question:string -> answer:string"
                  :id "qa" :description "Answer questions." :tools (list lookup))))
    (expect-equal (axllm/core::core-program-signature gen)
                  (signature-string (generator-signature gen))
                  "Core reads a program's signature as text")
    (expect-equal (map 'list (lambda (c) (jget c "id"))
                       (axllm/core::core-program-components gen))
                  '("qa::description" "qa::instruction" "qa::fn:lookup:desc" "qa::fn:lookup:name")
                  "Core reads the same components as the public hook")
    (axllm/core::core-program-apply-components gen (object "qa::instruction" "Be brief."))
    (expect-equal (generator-instruction gen) "Be brief."
                  "Core applies components through the public hook")
    ;; A program with no signature answers with empty text rather than failing,
    ;; because a flow asks every node whether it declares one.
    (expect-equal (axllm/core::core-program-signature "not a program") ""
                  "a value that is not a program declares no signature")
    (expect-equal (program-signature "not a program") :null
                  "the public hook says so as :null")
    ;; Host-object reads and calls Core makes on a program.
    (expect-equal (axllm/core::core-host-get gen "program_id") "qa" "the program id")
    (expect-equal (axllm/core::core-host-get gen "instruction") "Be brief." "the instruction")
    (expect-equal (axllm/core::core-host-get gen "nothing-like-this" "fallback") "fallback"
                  "an unknown key answers with the fallback rather than failing")
    (expect-equal (axllm/core::core-host-call gen "signature" (vector))
                  (generator-signature gen)
                  "a signature call")
    ;; A generator offers no owned worker, which is what makes a caller run it
    ;; serially instead of handing it to another thread.
    (expect-equal (axllm/core::core-host-call gen "owned_worker_factory" (vector)) :null
                  "no owned worker factory")
    (expect (handler-case (progn (axllm/core::core-host-call gen "no_such_method" (vector)) nil)
              (ax-error () t))
            "an unknown method is an error, not a placeholder success"))
  ;; The sampling hook is false until one forward can really produce several
  ;; candidates, so an optimizer samples serially instead of scoring one
  ;; candidate n times.
  (expect (program-native-sample-capable-p (ax "q:string -> a:string"))
          "a generator can produce several candidates from one call")
  (expect-equal (program-native-sample-capable-p "anything") nil
                "nothing else claims that by default")
  ;; Streaming is declared with no default method, so a program that cannot be
  ;; driven by a prefix refuses rather than silently falling back to forward.
  ;; Tests run inside AXLLM, but applications must not need private symbols.
  (dolist (name '("PROGRAM-STREAMING-FORWARD" "PROGRAM-SIGNATURE"
                  "PROGRAM-TOOLS" "PROGRAM-SET-TOOLS" "PROGRAM-FUNCTION-CALL-TRACES"
                  "PROGRAM-SET-FUNCTION-CALL-TRACES" "PROGRAM-CLEAR-FUNCTION-CALL-TRACES"
                  "FUNCTION-PROCESSOR" "MAKE-FUNCTION-PROCESSOR" "FUNCTION-PROCESSOR-RESOLVE"
                  "EXECUTE-FUNCTION" "EXECUTE-FUNCTION-WITH-DETAILS" "FUNCTION-CALL-ERROR"))
    (expect-equal (nth-value 1 (find-symbol name :axllm)) :external
                  (format nil "~A is part of the public API" name)))
  (expect (typep #'program-streaming-forward 'generic-function)
          "program-streaming-forward is a generic function")
  (expect-equal (find-method #'program-streaming-forward nil
                             (list (find-class t) (find-class t) (find-class t)) nil)
                nil
                "it has no default method"))

(deftest test-tools-and-traces-can-be-lent-and-restored
  ;; An agent stage lends a program extra tools for one call and must be able to
  ;; put it back exactly as it was, without reaching into its slots.
  (let* ((base (tool :name "base" :description "Base"
                     :handler (lambda (args) (declare (ignore args)) "base")))
         (extra (tool :name "extra" :description "Extra"
                      :handler (lambda (args) (declare (ignore args)) "extra")))
         (gen (ax "question:string -> answer:string" :tools (list base))))
    (let ((before (program-tools gen))
          (traces-before (program-function-call-traces gen)))
      (expect-equal (map 'list (lambda (spec) (jget spec "name")) before) '("base")
                    "the program reports its own tools")
      (program-set-tools gen (vector base extra))
      (expect-equal (map 'list (lambda (spec) (jget spec "name")) (program-tools gen))
                    '("base" "extra")
                    "a lent tool is visible")
      (expect (function-processor-resolve (generator-function-processor gen) "extra")
              "a lent tool resolves by name")
      (program-set-function-call-traces gen (vector (object "name" "extra" "status" "ok")))
      (expect-equal (length (program-function-call-traces gen)) 1 "a record can be installed")
      ;; Restore.
      (program-set-tools gen before)
      (program-set-function-call-traces gen traces-before)
      (expect-equal (map 'list (lambda (spec) (jget spec "name")) (program-tools gen)) '("base")
                    "the lent tool is gone again")
      (expect (not (function-processor-resolve (generator-function-processor gen) "extra"))
              "and no longer resolves")
      (expect-equal (length (program-function-call-traces gen)) 0 "the records are restored")
      (program-clear-function-call-traces gen)
      (expect-equal (length (program-function-call-traces gen)) 0 "clearing is idempotent"))))

(deftest test-forward-reports-its-run-control-lifecycle-at-its-own-path
  (let ((events '()))
    (let ((control (make-recording-control (lambda (event) (push event events)))))
      (multiple-value-bind (client script)
          (scripted-client "openai" (list (openai-text-response "Answer: ok")))
        (declare (ignore script))
        (forward (ax "question:string -> answer:string") client (object "question" "go")
                 (object "control" control "executionPath" "root/outer")))
      (expect-equal (mapcar (lambda (e) (jget e "type")) (reverse events))
                    '("started" "completed")
                    "a completed run reports started then completed")
      (expect-equal (jget (first (reverse events)) "path") "root/outer"
                    "at the path the caller gave it"))
    (setf events '())
    (let ((control (make-recording-control (lambda (event) (push event events)))))
      (multiple-value-bind (client script)
          (scripted-client "openai" (list (openai-text-response "Answer: null")))
        (declare (ignore script))
        (handler-case
            (forward (ax "question:string -> answer:string" :max-retries 0) client
                     (object "question" "go")
                     (object "control" control "executionPath" "root/node"))
          (ax-error () nil)))
      (expect-equal (mapcar (lambda (e) (jget e "type")) (reverse events))
                    '("started" "failed")
                    "a failed run reports started then failed"))
    ;; Without a control there is nothing to report to, and the run is unaffected.
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response "Answer: ok")))
      (declare (ignore script))
      (expect-equal (jget (forward (ax "question:string -> answer:string") client
                                   (object "question" "go"))
                          "answer")
                    "ok" "a run with no control still completes")))
  ;; A run that carries a run control is being watched or steered, so it must not
  ;; touch the cache at all: the caller asked for this run, not a remembered one.
  ;; ir/conformance/axflow/flow-cache-control-skips-cache pins zero cache reads
  ;; and two real requests.
  (let ((events '())
        (store (make-hash-table :test #'equal))
        (gets 0))
    (let ((control (make-recording-control (lambda (event) (push event events))))
          (cache (lambda (key &optional (value nil value-p))
                   (if value-p
                       (setf (gethash key store) value)
                       (progn (incf gets) (gethash key store))))))
      (multiple-value-bind (client script)
          (scripted-client "openai"
                           (list (openai-text-response "Answer: Paris")
                                 (openai-text-response "Answer: Paris again")))
        (let ((gen (ax "question:string -> answer:string")))
          (expect-equal (jget (forward gen client (object "question" "go")
                                       (object "cachingFunction" cache "control" control))
                              "answer")
                        "Paris" "the first run answers")
          (expect-equal (jget (forward gen client (object "question" "go")
                                       (object "cachingFunction" cache "control" control))
                              "answer")
                        "Paris again"
                        "the second run runs again rather than returning the first answer")
          (expect-equal gets 0 "a run under a control never reads the cache")
          (expect-equal (hash-table-count store) 0 "nor writes it")
          (expect-equal (script-call-count script) 2 "both runs reached the provider")
          (expect-equal (mapcar (lambda (e) (jget e "type")) (reverse events))
                        '("started" "completed" "started" "completed")
                        "and both reported their lifecycle")))))
  ;; Without a control, a cache hit reports no lifecycle, because nothing ran.
  (let ((store (make-hash-table :test #'equal))
        (events '()))
    (let ((cache (lambda (key &optional (value nil value-p))
                   (if value-p (setf (gethash key store) value) (gethash key store)))))
      (multiple-value-bind (client script)
          (scripted-client "openai" (list (openai-text-response "Answer: ok")))
        (let ((gen (ax "question:string -> answer:string")))
          (forward gen client (object "question" "go") (object "cachingFunction" cache))
          (setf events '())
          (let ((outputs (forward gen client (object "question" "go")
                                  (object "cachingFunction" cache))))
            (expect-equal (jget outputs "answer") "ok" "the second run is served from the cache")
            (expect-equal (script-call-count script) 1 "and makes no provider request")
            (expect-equal events '() "and reports no lifecycle, because nothing ran")))))))

(deftest test-forward-reads-and-writes-the-caching-function
  (let* ((store (make-hash-table :test #'equal))
         (reads 0)
         (writes 0)
         (cache (lambda (key &optional (value nil value-p))
                  (cond (value-p (incf writes) (setf (gethash key store) value))
                        (t (incf reads) (gethash key store))))))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-text-response "Answer: first")
                               (openai-text-response "Answer: second")))
      (let ((gen (ax "question:string -> answer:string")))
        (expect-equal (jget (forward gen client (object "question" "a")
                                     (object "cachingFunction" cache))
                            "answer")
                      "first" "a miss runs the program")
        (expect-equal reads 1 "the miss read the cache once")
        (expect-equal writes 1 "and stored its output")
        (expect-equal (jget (forward gen client (object "question" "a")
                                     (object "cachingFunction" cache))
                            "answer")
                      "first" "a hit returns the stored output")
        (expect-equal reads 2 "the hit read the cache")
        (expect-equal writes 1 "and stored nothing new")
        (expect-equal (script-call-count script) 1 "the hit made no provider request")
        ;; A different input is a different key, so it misses.
        (expect-equal (jget (forward gen client (object "question" "b")
                                     (object "cachingFunction" cache))
                            "answer")
                      "second" "a different input misses")
        (expect-equal (script-call-count script) 2 "and runs the program")))
    ;; The key does not depend on the order the input keys were written in.
    (let ((gen (ax "a:string, b:string -> answer:string")))
      (expect-equal (%forward-cache-key gen (object "a" "1" "b" "2"))
                    (%forward-cache-key gen (object "b" "2" "a" "1"))
                    "two spellings of the same input share one cache key"))
    ;; The process-wide caching function is the fallback, and a per-call one wins.
    (let ((global-store (make-hash-table :test #'equal)))
      (set-global "cachingFunction"
                  (lambda (key &optional (value nil value-p))
                    (if value-p (setf (gethash key global-store) value) (gethash key global-store))))
      (unwind-protect
           (multiple-value-bind (client script)
               (scripted-client "openai" (list (openai-text-response "Answer: global")))
             (declare (ignore script))
             (forward (ax "question:string -> answer:string") client (object "question" "g"))
             (expect-equal (hash-table-count global-store) 1
                           "a run with no per-call cache uses the process-wide one"))
        (set-global "cachingFunction" :null)))))

(deftest test-fresh-memory-isolates-one-attempt
  ;; An optimizer runs the same program several times and reads back one
  ;; attempt's conversation.  With a fresh memory each attempt starts empty,
  ;; while the program's own chat log, usage and traces still accumulate: those
  ;; are the program's history, not the attempt's.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response "Answer: one" :prompt 1 :completion 1)
                             (openai-text-response "Answer: two" :prompt 1 :completion 1)))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string")))
      (forward gen client (object "question" "a") (object "freshMemory" true))
      (expect-equal (length (memory-history (generator-memory gen))) 0
                    "a fresh-memory attempt records nothing in the program's memory")
      (forward gen client (object "question" "b") (object "freshMemory" true))
      (expect-equal (length (program-chat-log gen)) 2
                    "the program's chat log still grows across attempts")
      (expect-equal (length (program-traces gen)) 2 "and so do its traces")
      (expect-equal (jget (aref (program-usage gen) 0) "total_tokens") 4
                    "and so does its usage")))
  ;; Without the option the program's own memory records the turns.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: kept")))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string")))
      (forward gen client (object "question" "a"))
      (expect-equal (mapcar (lambda (e) (jget e "role")) (memory-history (generator-memory gen)))
                    '("request" "assistant")
                    "the default records into the program's memory"))))

(deftest test-program-usage-entries-are-exactly-the-token-counts
  ;; ir/conformance/axflow/simple-forward-returns and
  ;; ir/conformance/axagent/trace-max-step-error both pin a usage entry as the
  ;; three snake_case counts and nothing else, because a subset assertion
  ;; descends an object but compares a list by value.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok" :prompt 3 :completion 4)))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string")))
      (forward gen client (object "question" "go"))
      (let ((entry (aref (program-usage gen) 0)))
        (expect-equal (sort (%object-keys entry) #'string<)
                      '("completion_tokens" "prompt_tokens" "total_tokens")
                      "exactly three keys, no ai, no model, no camelCase")
        (expect-equal (jget entry "prompt_tokens") 3 "prompt tokens")
        (expect-equal (jget entry "completion_tokens") 4 "completion tokens")
        (expect-equal (jget entry "total_tokens") 7 "total tokens"))
      (let ((entry (aref (program-usage-by-model gen) 0)))
        (expect-equal (jget entry "ai") "openai" "the provider is available separately")
        (expect-equal (jget entry "model") +test-openai-model+ "and the model")
        (expect-equal (jget entry "prompt_tokens") 3 "beside the same counts")))))

(deftest test-a-forward-model-option-is-passed-to-the-provider
  ;; A caller that asks to run this node on another model must not be told it
  ;; ran on the client's, so the option is passed through to the provider
  ;; request rather than dropped.
  ;; The option travels into the request Core builds, which is what the service
  ;; is handed.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (declare (ignore script))
    (let* ((gen (ax "q:string -> answer:string"))
           (request (%chat-request gen (list (object "role" "user" "content" "hi"))
                                   :auto (object "model" "gpt-flow-fixture")
                                   (%output-selection gen client (object)) 0)))
      (expect-equal (jget request "model") "gpt-flow-fixture"
                    "a model option becomes the request's model")))
  ;; End to end once the provider's chat takes the keyword.  Until then this
  ;; records the gap instead of pretending the model was honoured.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (handler-case
        (progn
          (forward (ax "question:string -> answer:string") client (object "question" "go")
                   (object "model" "gpt-flow-fixture"))
          (expect-equal (jget (script-request script 0) "model") "gpt-flow-fixture"
                        "the call's model reaches the request body"))
      (error (condition)
        (expect (search "MODEL" (string-upcase (princ-to-string condition)))
                (format nil "a provider chat without :model fails loudly, not silently: ~a"
                        condition)))))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (forward (ax "question:string -> answer:string") client (object "question" "go"))
    (expect-equal (jget (script-request script 0) "model") +test-openai-model+
                  "without the option the client's model is used"))
  ;; A run sent to another model must be recorded against that model: a usage
  ;; report or a chat log that names the client's default would disagree with the
  ;; request that produced it.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok" :prompt 1 :completion 2)))
    (let ((gen (ax "question:string -> answer:string")))
      (forward gen client (object "question" "go") (object "model" "gpt-flow-fixture"))
      (expect-equal (jget (script-request script 0) "model") "gpt-flow-fixture"
                    "the request names the override")
      (expect-equal (jget (aref (program-usage-by-model gen) 0) "model") "gpt-flow-fixture"
                    "and the usage is attributed to it, not to the client default")
      (expect-equal (jget (aref (program-chat-log gen) 0) "model") "gpt-flow-fixture"
                    "and so is the chat log")))
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok" :prompt 1 :completion 2)))
    (declare (ignore script))
    (let ((gen (ax "question:string -> answer:string")))
      (forward gen client (object "question" "go"))
      (expect-equal (jget (aref (program-usage-by-model gen) 0) "model") +test-openai-model+
                    "without an override the client's model is what is recorded"))))

;;; ------------------------------------------------------------------
;;; Streaming
;;; ------------------------------------------------------------------

(defclass scripted-stream-service ()
  ((chunks :initarg :chunks :accessor scripted-stream-chunks)
   (requests :initform '() :accessor scripted-stream-requests)
   (reads :initform 0 :accessor scripted-stream-reads)
   (closed :initform 0 :accessor scripted-stream-closed)
   ;; When true, a read is refused unless the sink has run since the last chunk
   ;; was handed over.  That is what distinguishes streaming from replay: a
   ;; buffered implementation drains the source first and would fail here.
   (lockstep :initarg :lockstep :initform nil :reader scripted-stream-lockstep)
   (owed :initform nil :accessor scripted-stream-owed)))

(defmethod ax-stream ((service scripted-stream-service) request &optional options)
  (declare (ignore options))
  (push request (scripted-stream-requests service))
  (let ((remaining (copy-list (scripted-stream-chunks service))))
    (make-ax-stream-handle
     (lambda ()
       (when (and (scripted-stream-lockstep service) (scripted-stream-owed service))
         (error "read ahead of the sink: chunk ~a was requested before the previous chunk's delta was delivered"
                (1+ (scripted-stream-reads service))))
       (if remaining
           (let ((content (pop remaining)))
             (incf (scripted-stream-reads service))
             (setf (scripted-stream-owed service) t)
             ;; Core's chat response shape, which is what ax-stream answers for
             ;; every service: a double that spoke this port's native shape
             ;; instead would pass while the real contract was unmet.
             (let* ((result (object "index" 0 "content" content))
                    (chunk (object "results" (vector result))))
               ;; A provider reports usage once the turn is over, so it rides the
               ;; last chunk.
               (when (null remaining)
                 (setf (gethash "model_usage" chunk)
                       (object "ai" "scripted" "model" "scripted-model"
                               "tokens" (object "prompt_tokens" 1 "completion_tokens" 2
                                                "total_tokens" 3)))
                 (setf (gethash "finish_reason" result) "stop"))
               chunk))
           :null))
     :closer (lambda () (incf (scripted-stream-closed service)) :null))))

(defmethod ax-service-name ((service scripted-stream-service)) "scripted")

(defmethod ax-options ((service scripted-stream-service))
  (object "model" "scripted-model"))

(defun %stream-sink-that (service action)
  "A sink that settles SERVICE's debt and then runs ACTION on the envelope."
  (lambda (envelope)
    (setf (scripted-stream-owed service) nil)
    (funcall action envelope)))

(deftest test-streaming-forward-emits-each-delta-before-reading-further
  ;; The discriminating case: the source refuses a second read until the sink has
  ;; been given the first chunk's delta.  A buffered implementation, which drains
  ;; the stream and replays the deltas afterwards, fails on the second read.
  (let* ((service (make-instance 'scripted-stream-service
                                 :lockstep t
                                 :chunks (list "Answer: Par" "is is the " "capital.")))
         (deltas '())
         (gen (ax "question:string -> answer:string")))
    (multiple-value-bind (outputs usage)
        (program-streaming-forward
         gen service (object "question" "capital?")
         (object "sink" (%stream-sink-that service (lambda (e) (push e deltas)))))
      (expect-equal (jget outputs "answer") "Paris is the capital."
                    "the finished value is the whole answer")
      (expect (hash-table-p usage) "usage is returned as the second value")
      (expect-equal (scripted-stream-reads service) 3 "every chunk was read")
      (expect-equal (scripted-stream-closed service) 1 "the handle was closed once")
      (let ((seen (reverse deltas)))
        (expect-equal (length seen) 3 "one delta per chunk, delivered in lockstep")
        ;; A delta's version is the attempt it belongs to, not a delta counter:
        ;; one uninterrupted turn is version 0 throughout, and only a correction
        ;; turn moves it on, so a consumer knows when to replace what it drew.
        (expect-equal (mapcar (lambda (e) (jget e "version")) seen) '(0 0 0)
                      "one attempt means one version")
        (let ((joined (with-output-to-string (out)
                        (dolist (envelope seen)
                          (let ((delta (jget envelope "delta")))
                            (expect (hash-table-p delta) "a delta is an object")
                            (dolist (key (%object-keys delta))
                              (expect-equal key "answer" "a delta names a declared field")
                              (write-string (jget delta key) out)))))))
          (expect-equal joined "Paris is the capital."
                        "the deltas concatenate to the final value")))
      (expect-equal (jget (aref (program-chat-log gen) 0) "model") "scripted-model"
                    "a non-ai-client service is recorded by its own model")
      (expect-equal (jget (aref (program-usage-by-model gen) 0) "ai") "scripted"
                    "and by its own service name"))))

(deftest test-a-streaming-sink-that-refuses-stops-the-upstream-read
  ;; A consumer that cannot go on must stop the work, not merely be skipped: the
  ;; failure has to reach the loop before the next chunk is requested.
  (let* ((service (make-instance 'scripted-stream-service
                                 :chunks (list "Answer: one" " two" " three")))
         (gen (ax "question:string -> answer:string")))
    (expect (handler-case
                (progn (program-streaming-forward
                        gen service (object "question" "q")
                        (object "sink" (lambda (envelope)
                                         (declare (ignore envelope))
                                         (error "the consumer stopped reading"))))
                       nil)
              (error () t))
            "a sink that raises stops the run")
    (expect-equal (scripted-stream-reads service) 1
                  "no chunk was read after the sink refused")
    (expect-equal (scripted-stream-closed service) 1
                  "and the handle was still closed")
    (expect-equal (jget (aref (program-traces gen) 0) "status") "error"
                  "the abandoned run is traced as an error")))

(deftest test-streaming-forward-without-a-sink-and-with-a-bad-reply
  ;; No sink is a legitimate call: the caller wants the result, not the pieces.
  (let* ((service (make-instance 'scripted-stream-service :chunks (list "Answer: done")))
         (gen (ax "question:string -> answer:string")))
    (expect-equal (jget (program-streaming-forward gen service (object "question" "q")) "answer")
                  "done" "a streamed run with no sink still returns the result")
    (expect-equal (scripted-stream-closed service) 1 "and closes the handle"))
  ;; A streamed reply that does not satisfy the signature fails, rather than
  ;; handing back a partial object.
  (let* ((service (make-instance 'scripted-stream-service :chunks (list "Answer: null")))
         (gen (ax "question:string -> answer:string, score:number")))
    (expect-error generation-error generation-error-kind :validation
      (program-streaming-forward gen service (object "question" "q")))
    (expect-equal (jget (aref (program-traces gen) 0) "status") "error"
                  "the failed streamed run is traced as an error")))

(defclass scripted-chat-service ()
  ((replies :initarg :replies :accessor scripted-chat-replies)
   (requests :initform '() :accessor scripted-chat-requests)))

(defmethod ax-chat ((service scripted-chat-service) request &optional options)
  (declare (ignore options))
  (push request (scripted-chat-requests service))
  (let ((reply (pop (scripted-chat-replies service))))
    (unless reply (error "scripted chat service exhausted"))
    reply))

(defmethod ax-service-name ((service scripted-chat-service)) "router")

(defmethod ax-options ((service scripted-chat-service)) (object "model" "routed-model"))

(defun %sampled-reply (count)
  "One Core chat response carrying COUNT candidates."
  (object "results"
          (coerce (loop for index from 0 below count
                        collect (object "index" index
                                        "content" (format nil "Answer: candidate ~a" index)
                                        "function_calls" (%new-array)
                                        "finish_reason" "stop"))
                  'vector)
          "model_usage" (object "ai" "router" "model" "routed-model"
                                "tokens" (object "prompt_tokens" 1 "completion_tokens" 1
                                                 "total_tokens" 2))))

(deftest test-a-provider-that-ignores-the-sample-count-is-refused
  ;; The discriminating case for native best-of-n.  A dialect without an `n'
  ;; parameter -- the OpenAI Responses API is the real one -- is sent a request
  ;; with no count and answers a single candidate.  An optimizer that asked for
  ;; three would then score one and report it as the best of three, which is a
  ;; wrong answer that nothing surfaces.  So the run must fail here, naming the
  ;; counts, rather than succeed quietly.
  (let* ((service (make-instance 'scripted-chat-service :replies (list (%sampled-reply 1))))
         (gen (ax "question:string -> answer:string")))
    (handler-case
        (progn (forward gen service (object "question" "q") (object "sampleCount" 3))
               (expect nil "a run that asked for 3 candidates and got 1 must not succeed"))
      (ax-error (condition)
        (let ((text (ax-error-message-text condition)))
          (expect (search "received 1" text)
                  "the failure says how many candidates came back")
          (expect (search "3" text) "and how many were asked for"))))))

(deftest test-a-provider-that-honours-the-sample-count-is-not-refused
  ;; The other side of the boundary: the same request against a dialect that does
  ;; carry the count must go through untouched, so the check cannot be satisfied
  ;; by refusing native sampling everywhere.
  (let* ((service (make-instance 'scripted-chat-service :replies (list (%sampled-reply 3))))
         (gen (ax "question:string -> answer:string")))
    (expect-equal (jget (forward gen service (object "question" "q") (object "sampleCount" 3))
                        "answer")
                  "candidate 0"
                  "three candidates asked for and three received runs normally")))

(deftest test-forward-drives-any-service-not-only-the-built-in-client
  ;; A generator must run against anything that implements the service protocol:
  ;; a router, a balancer or a test double, none of which is an ai-client.  The
  ;; request it hands over is Core's own, so the two sides share one shape.
  ;; The double answers in Core's shape, because that is ax-chat's contract for
  ;; every service, not a convenience of the built-in client.
  (let* ((service (make-instance 'scripted-chat-service
                                 :replies (list (object "results"
                                                        (vector (object "index" 0
                                                                        "content" "Answer: routed"
                                                                        "function_calls" (%new-array)
                                                                        "finish_reason" "stop"))
                                                        "model_usage"
                                                        (object "ai" "router"
                                                                "model" "routed-model"
                                                                "tokens"
                                                                (object "prompt_tokens" 2
                                                                        "completion_tokens" 3
                                                                        "total_tokens" 5))))))
         (gen (ax "question:string -> answer:string")))
    (multiple-value-bind (outputs usage) (forward gen service (object "question" "q"))
      (expect-equal (jget outputs "answer") "routed" "the run completes against the service")
      (expect-equal (jget usage "totalTokens") 5 "its usage is returned"))
    (let ((request (first (scripted-chat-requests service))))
      (expect (jget request "chat_prompt") "the service is handed Core's chat_prompt")
      (expect-equal (jget request "function_call") "auto" "and Core's function_call")
      (expect-equal (length (jget request "chat_prompt")) 2 "a system and a user turn"))
    (expect-equal (jget (aref (program-usage-by-model gen) 0) "ai") "router"
                  "usage names the service, not a provider class")
    (expect-equal (jget (aref (program-usage-by-model gen) 0) "model") "routed-model"
                  "and the model the service reports")))

(deftest test-a-streaming-method-exists-only-where-streaming-is-implemented
  ;; A generator streams.  Nothing else inherits that, so a program which cannot
  ;; be driven by a prefix still has no applicable method.
  (expect (compute-applicable-methods #'program-streaming-forward
                                      (list (ax "q:string -> a:string") nil (object) (object)))
          "a generator has an applicable streaming method")
  (expect-equal (find-method #'program-streaming-forward nil
                             (list (find-class t) (find-class t) (find-class t)) nil)
                nil
                "and there is still no default method"))

(deftest test-the-selected-output-rung-reaches-the-request
  ;; A structured run that asks for a shape in the prompt and does not ask the
  ;; provider to enforce it has quietly degraded to a textual one, and nothing
  ;; downstream notices.  Core builds the request, so this asserts through the
  ;; real path: a service that advertises native support gets a json_schema
  ;; response format and the rung recorded in the request metadata, and a service
  ;; that advertises none gets neither.
  (let ((gen (ax "q:string -> answer:string" :options (object "force_structured" true))))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response "{\"answer\":\"x\"}")))
      (declare (ignore script))
      (let* ((selection (%output-selection gen client (object)))
             (request (%chat-request gen (list (object "role" "user" "content" "hi"))
                                     :auto (object) selection 0)))
        (expect-equal (%present (jget selection "rung")) "native"
                      "a service advertising native support selects the native rung")
        (let* ((format (jget request "response_format"))
               (wrapper (jget format "schema")))
          (expect-equal (jget format "type") "json_schema" "the request asks for a schema")
          (expect-equal (jget wrapper "name") "output" "named output")
          (expect (json-true-p (jget wrapper "strict"))
                  "and strict, so the model cannot answer off-shape")
          (expect (nth-value 1 (gethash "answer" (jget (jget wrapper "schema") "properties")))
                  "carrying the signature's own fields"))
        ;; The rung is also recorded on the request, which is how a provider or a
        ;; trace can tell which contract a turn was sent under.
        (expect-equal (jget (jget (jget request "provider_metadata") "ax")
                            "structured_output_rung")
                      "native"
                      "and the request records which rung it was sent under"))))
  ;; A service that advertises no structured output gets no response format, and
  ;; the run falls back to the textual contract rather than asking for a shape
  ;; the provider cannot honour.
  (let ((gen (ax "q:string -> answer:string")))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response "Answer: x")))
      (declare (ignore script))
      (let* ((selection (%output-selection gen client (object)))
             (request (%chat-request gen (list (object "role" "user" "content" "hi"))
                                     :auto (object) selection 0)))
        (expect-equal (%present (jget selection "rung")) nil
                      "a simple signature selects no rung")
        (expect-equal (nth-value 1 (gethash "response_format" request)) nil
                      "so the request asks for no response format")
        (expect-equal (nth-value 1 (gethash "provider_metadata" request)) nil
                      "and records no rung")))))

(defclass rung-service ()
  ((modes :initarg :modes :reader rung-service-modes)
   (native :initarg :native :initform *json-false* :reader rung-service-native)))

(defmethod ax-features ((service rung-service) &optional model)
  (declare (ignore model))
  (object "functions" true
          "structured_outputs" (rung-service-native service)
          "structured_output_modes" (rung-service-modes service)))

(defmethod ax-service-name ((service rung-service)) "rung")

(defmethod ax-options ((service rung-service)) (object "model" "m"))

(deftest test-the-prompt-describes-the-contract-the-rung-asked-for
  ;; A structured run whose prompt still asks for labelled lines tells the model
  ;; one thing and the provider another, and the model follows the prompt.  Core
  ;; derives the render options from the rung, so the prompt and the request
  ;; describe the same contract.
  (let ((gen (ax "question:string -> answer:string" :options (object "force_structured" true))))
    ;; json_object: the prompt asks for one JSON object keyed by the wire keys.
    (let* ((service (make-instance 'rung-service :modes (vector "json_object")))
           (selection (%output-selection gen service (object)))
           (system (jget (first (%prompt-messages gen (object "question" "q") (object) selection))
                         "content")))
      (expect-equal (%present (jget selection "rung")) "json_object"
                    "a service offering only json_object selects it")
      (expect-contains system "Return one valid JSON object matching <output_fields>"
                       "the prompt asks for one JSON object")
      (expect-contains system "do not invent, rename, or wrap them"
                       "and tells the model to use the wire keys as they are"))
    ;; No rung: the prompt asks for the labelled lines the textual contract reads.
    (let* ((service (make-instance 'rung-service :modes (%new-array)))
           (plain (ax "question:string -> answer:string"))
           (selection (%output-selection plain service (object)))
           (system (jget (first (%prompt-messages plain (object "question" "q")
                                                  (object) selection))
                         "content")))
      (expect-equal (%present (jget selection "rung")) nil "a simple signature selects no rung")
      (expect-contains system "field name: value"
                       "the prompt asks for the labelled lines the textual contract reads")
      (expect-not-contains system "do not invent, rename, or wrap them"
                           "and does not carry the structured guidance"))))

;;; Generation instrumentation exercises the public forward boundary, not the
;;; recording helpers in isolation. TS includes built-in labels alongside the
;;; custom labels; both sets are checked here.
(defun %generation-test-meter (record &key throw-create throw-record)
  (flet ((instrument (name &optional options)
           (declare (ignore options))
           (when throw-create (error "meter factory failed"))
           (let ((callback (lambda (value labels)
                             (funcall record name value labels)
                             (when throw-record (error "meter recording failed")))))
             (object "record" callback "add" callback))))
    (object "createHistogram" #'instrument "createCounter" #'instrument "createGauge" #'instrument)))

(deftest test-generation-duration-and-label-precedence
  (let* ((*telemetry-globals* (globals-snapshot))
         (records nil)
         (meter (%generation-test-meter (lambda (name value labels) (push (list name value labels) records))))
         (constructor (object "customLabels" (object "team" "constructor" "tier" "constructor")))
         (call (object "customLabels" (object "tier" "call")))
         (client (provider :name "openai" :model +test-openai-model+ :api-key "test-only"
                           :options (object "customLabels" (object "region" "eu" "team" "service"))
                           :transport (lambda (url headers body)
                                        (declare (ignore url headers body))
                                        (sleep 0.02)
                                        (values (openai-text-response "Answer: ok") 200))))
         (gen (ax "question:string -> answer:string" :options constructor)))
    (set-global "meter" meter)
    (set-global "customLabels" (object "tenant" "global" "team" "global"))
    (expect-equal (jget (forward gen client (object "question" "status") call) "answer") "ok" "metered forward")
    (let ((durations (remove-if-not (lambda (record) (equal (first record) "ax_gen_generation_duration_ms")) records)))
      (expect-equal (length durations) 1 "one generation duration per forward")
      (expect (>= (second (first durations)) 15d0) "duration includes actual provider latency")
      (expect (equalp
               (third (first durations))
               (object "success" "true" "signature" "unknown_signature" "ai_service" "openai"
                       "tenant" "global" "region" "eu" "team" "constructor" "tier" "call"))
              "TS generation labels include base labels and global/service/constructor/call precedence"))
    (dolist (name '("ax_gen_generation_requests_total" "ax_llm_request_duration_ms" "ax_llm_requests_total"))
      (let ((record (assoc name records :test #'equal)))
        (expect record (format nil "~a was recorded" name))
        (expect-equal (jget (third record) "team") "constructor" "constructor labels reach downstream chat")
        (expect-equal (jget (third record) "tier") "call" "call labels override constructor")))
    (expect-equal (jget (jget constructor "customLabels") "tier") "constructor" "constructor options remain unchanged")
    (expect (absent (jget (jget call "customLabels") "team")) "call options remain unchanged")))

(deftest test-generation-meter-fail-open-and-error-duration
  (dolist (factory-failure '(nil t))
    (let* ((records nil)
           (meter (%generation-test-meter
                   (lambda (name value labels) (push (list name value labels) records))
                   :throw-create factory-failure :throw-record t))
           (gen (ax "question:string -> answer:string" :options (object "meter" meter "validationRetries" 0))))
      (multiple-value-bind (client script) (scripted-client "openai" (list (openai-text-response "Answer: ok")))
        (expect-equal (jget (forward gen client (object "question" "q")) "answer") "ok" "broken meter cannot fail a result")
        (expect-equal (script-call-count script) 1 "broken meter cannot replay a request"))
      (setf records nil)
      (multiple-value-bind (client script) (scripted-client "openai" (list (openai-text-response "Answer: null")))
        (let ((failure (handler-case (progn (forward gen client (object "question" "q")) nil)
                         (error (condition) condition))))
          (expect (typep failure 'ax-generate-error) "original generation error survives meter failure")
          (expect-not-contains (princ-to-string failure) "meter" "meter failure does not replace generation failure")
          (expect-equal (script-call-count script) 1 "failed generation is not replayed"))
        (unless factory-failure
          (let ((record (assoc "ax_gen_generation_duration_ms" records :test #'equal)))
            (expect record "error exit attempts duration recording")
            (expect (>= (second record) 0) "error duration is measured")
            (expect-equal (jget (third record) "success") "false" "error duration is labelled failed")))))))

(deftest test-generation-hook-frame-meter-and-cache-bypass
  (let* ((*telemetry-globals* (globals-snapshot))
         (records nil)
         (meter (%generation-test-meter (lambda (name value labels)
                                         (declare (ignore value labels)) (push name records))))
         (frame (make-runtime-hook-frame :globals (object "meter" meter) :resolved true))
         (options (options-with-runtime-hook-frame (object) frame)))
    (set-global "meter" :null)
    (multiple-value-bind (client script) (scripted-client "openai" (list (openai-text-response "Answer: ok")))
      (declare (ignore script))
      (forward (ax "question:string -> answer:string") client (object "question" "q") options))
    (expect (member "ax_gen_generation_duration_ms" records :test #'equal) "forward retains the native hook frame")
    (setf records nil)
    (set-global "meter" meter)
    (multiple-value-bind (client script) (scripted-client "openai" nil)
      (let ((gen (ax "question:string -> answer:string")))
        (expect-equal
         (jget (forward gen client (object "question" "q")
                        (object "cachingFunction" (lambda (key &optional value)
                                                    (declare (ignore key value)) (object "answer" "cached")))) "answer")
         "cached" "cache output is returned")
        (expect-equal (script-call-count script) 0 "cache hit makes no provider call")
        (expect-equal records nil "cache hit does not record generation metrics")))))

(deftest test-ai-custom-labels-fixture-against-unlabelled-baseline
  ;; The TS extractor's customPart subtracts keys observed in an unlabelled
  ;; run. Built-in AxGen labels do not have the provider's "ax." prefix.
  (let ((fixture (parse-json (uiop:read-file-string
                             (merge-pathnames "axai/ai-custom-labels-merge.json"
                                              (axllm/tests::conformance-directory))))))
    (labels ((run-labels (labelled)
               (let* ((*telemetry-globals* (globals-snapshot))
                      (records nil) (calls 0)
                      (replies (coerce (jget fixture "transport_responses") 'list))
                      (meter (%generation-test-meter (lambda (name value labels)
                                                      (declare (ignore value)) (push (cons name labels) records))))
                      (client (provider :name (jget fixture "provider") :model (jget fixture "model")
                                        :api-key "test-only"
                                        :options (if labelled (jget fixture "service_options") (object))
                                        :transport (lambda (url headers body)
                                                     (declare (ignore url headers body))
                                                     (incf calls)
                                                     (let ((reply (pop replies)))
                                                       (unless reply (error "label fixture exhausted"))
                                                       (values (encode-json (jget reply "json")) (jget reply "status"))))))
                      (chat (jget fixture "chat"))
                      (spec (jget fixture "forward")))
                 (set-global "meter" meter)
                 (set-global "customLabels" :null)
                 (ax-chat client (jget chat "request")
                          (if labelled (object "customLabels" (jget chat "custom_labels")) (object)))
                 (let ((chat-records (reverse records)))
                   (setf records nil)
                   (forward (ax (jget spec "signature")
                                :options (if labelled (object "customLabels" (jget spec "constructor_custom_labels")) (object)))
                            client (jget spec "input")
                            (if labelled (object "customLabels" (jget spec "call_custom_labels")) (object)))
                   (expect-equal calls 2 "labels fixture performs both real provider operations")
                   (list chat-records (reverse records)))))
             (compare-custom (records baseline expected)
               (dolist (name (%object-keys expected))
                 (let ((record (assoc name records :test #'equal))
                       (base (assoc name baseline :test #'equal))
                       (custom (object)))
                   (expect (and record base) (format nil "both label runs record ~a" name))
                   (dolist (key (%object-keys (cdr record)))
                     (unless (nth-value 1 (gethash key (cdr base)))
                       (%set-key custom key (gethash key (cdr record)))))
                   (expect (equalp custom (jget expected name)) (format nil "~a matches TS customPart" name))))))
      (let ((labelled (run-labels t)) (baseline (run-labels nil)))
        (compare-custom (first labelled) (first baseline) (jget fixture "expected_chat_custom_labels"))
        (compare-custom (second labelled) (second baseline) (jget fixture "expected_forward_custom_labels"))))))

(deftest test-streaming-consumer-close-is-normal-and-lazy
  (let* ((service (make-instance 'scripted-stream-service :chunks (list "Answer: one" " two" " three")))
         (gen (ax "question:string -> answer:string"))
         (events nil)
         (control (make-run-control :listener (lambda (event) (push (jget event "type") events)))))
    (program-streaming-forward gen service (object "question" "q")
                               (object "control" control "sink" (lambda (delta) (declare (ignore delta)) false)))
    (expect-equal (scripted-stream-reads service) 1 "consumer close stops before the next provider read")
    (expect-equal (scripted-stream-closed service) 1 "consumer close releases the provider handle exactly once")
    (expect-equal (reverse events) '("started" "aborted") "consumer close is not a failed generation")))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun run-provider-tests (&key (stream *standard-output*))
  "Run the provider/generation/tool tests.  Returns (values passed failed)."
  (let ((passed 0)
        (failed 0))
    (dolist (entry *tests*)
      (handler-case
          (progn (funcall (cdr entry))
                 (incf passed)
                 (format stream "ok   ~a~%" (car entry)))
        (error (condition)
          (incf failed)
          (format stream "FAIL ~a~%     ~a~%" (car entry) condition))))
    (format stream "~&~a passed, ~a failed~%" passed failed)
    (finish-output stream)
    (values passed failed)))

(defun run-provider-tests-or-die ()
  (multiple-value-bind (passed failed) (run-provider-tests)
    (declare (ignore passed))
    (if (zerop failed)
        (sb-ext:exit :code 0)
        (sb-ext:exit :code 1))))
