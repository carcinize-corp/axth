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

(defparameter +test-openai-model+ "gpt-6-luna")
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

(deftest test-model-is-required
  (expect-error provider-error provider-error-kind :config
    (ai :name "openai" :api-key "sk-x"))
  (expect-error provider-error provider-error-kind :config
    (ai :name "openai" :model "   " :api-key "sk-x")))

(deftest test-unknown-provider-rejected
  (expect-error provider-error provider-error-kind :config
    (ai :name "ollama" :model "x" :api-key "sk-x")))

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

(deftest test-openai-request-body-and-headers
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: hello"))
                       :api-key "sk-openai-header-key")
    (let ((weather (tool :name "get_weather"
                         :description "Look up weather"
                         :parameters (object "type" "object"
                                             "properties" (object "city" (object "type" "string"))
                                             "required" (vector "city"))
                         :handler (lambda (args) (jget args "city")))))
      (chat client
            (vector (message "system" "be terse")
                    (message "user" "weather in Oslo?")
                    (message "assistant" ""
                             :tool-calls (vector (object "id" "call_1" "name" "get_weather"
                                                         "arguments" "{\"city\":\"Oslo\"}")))
                    (message "tool" "12C" :tool-call-id "call_1"))
            :tools (list (tool-request-spec weather)))
      (let* ((body (script-request script 0))
             (messages (jget body "messages"))
             (tools (jget body "tools")))
        (expect-equal (jget body "model") +test-openai-model+ "model is sent explicitly")
        (expect-equal (length messages) 4 "all four messages are mapped")
        (expect-equal (jget (aref messages 0) "role") "system" "system message stays a message")
        (let ((assistant (aref messages 2)))
          (expect-equal (jget assistant "content") :null "assistant tool-call content is null")
          (let ((call (aref (jget assistant "tool_calls") 0)))
            (expect-equal (jget call "type") "function" "openai tool call type")
            (expect-equal (jget (jget call "function") "name") "get_weather" "tool call name")
            (expect-equal (jget (jget call "function") "arguments") "{\"city\":\"Oslo\"}"
                          "tool call arguments stay a JSON string")))
        (let ((tool-msg (aref messages 3)))
          (expect-equal (jget tool-msg "role") "tool" "tool result role")
          (expect-equal (jget tool-msg "tool_call_id") "call_1" "tool result id"))
        (expect-equal (jget (jget (aref tools 0) "function") "name") "get_weather"
                      "tool declared under function.name")
        (expect (hash-table-p (jget (jget (aref tools 0) "function") "parameters"))
                "tool parameters sent as a JSON schema object")
        (expect-equal (header-value (script-headers script 0) "Authorization")
                      "Bearer sk-openai-header-key" "bearer auth header")))))

(deftest test-anthropic-request-body-and-headers
  (multiple-value-bind (client script)
      (scripted-client "anthropic" (list (anthropic-text-response "Answer: hei"))
                       :api-key "sk-ant-header-key")
    (let ((weather (tool :name "get_weather"
                         :parameters (object "type" "object"
                                             "properties" (object "city" (object "type" "string"))
                                             "required" (vector "city"))
                         :handler (lambda (args) (jget args "city")))))
      (chat client
            (vector (message "system" "be terse")
                    (message "system" "answer in Norwegian")
                    (message "user" "weather in Oslo?")
                    (message "assistant" "checking"
                             :tool-calls (vector (object "id" "toolu_1" "name" "get_weather"
                                                         "arguments" "{\"city\":\"Oslo\"}")))
                    (message "tool" "12C" :tool-call-id "toolu_1")
                    (message "tool" "clear" :tool-call-id "toolu_2"))
            :tools (list (tool-request-spec weather)))
      (let* ((body (script-request script 0))
             (messages (jget body "messages")))
        (expect-equal (jget body "model") +test-anthropic-model+ "model is sent explicitly")
        (expect-equal (jget body "system") (format nil "be terse~canswer in Norwegian" #\Newline)
                      "system messages hoisted to the top-level system field")
        (expect (integerp (jget body "max_tokens")) "max_tokens is required and present")
        (expect-equal (length messages) 3 "user, assistant, merged tool-result turn")
        (let ((assistant (aref messages 1)))
          (expect-equal (jget assistant "role") "assistant" "assistant turn")
          (expect-equal (jget (aref (jget assistant "content") 0) "type") "text" "text block first")
          (let ((use (aref (jget assistant "content") 1)))
            (expect-equal (jget use "type") "tool_use" "tool_use block")
            (expect-equal (jget (jget use "input") "city") "Oslo"
                          "tool_use input is a parsed JSON object")))
        (let ((results (jget (aref messages 2) "content")))
          (expect-equal (jget (aref messages 2) "role") "user" "tool results ride on a user turn")
          (expect-equal (length results) 2 "consecutive tool results merged into one turn")
          (expect-equal (jget (aref results 0) "tool_use_id") "toolu_1" "first tool_result id")
          (expect-equal (jget (aref results 1) "tool_use_id") "toolu_2" "second tool_result id"))
        (expect (hash-table-p (jget (aref (jget body "tools") 0) "input_schema"))
                "anthropic tools use input_schema")
        (expect-equal (header-value (script-headers script 0) "x-api-key") "sk-ant-header-key"
                      "x-api-key header")
        (expect-equal (header-value (script-headers script 0) "anthropic-version") "2023-06-01"
                      "anthropic-version header")
        (expect-not-contains (script-request-text script 0) "axToolResultTurn"
                             "internal merge marker never reaches the wire")))))

;;; ------------------------------------------------------------------
;;; Response normalization
;;; ------------------------------------------------------------------

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
      (let ((condition (expect-error provider-error provider-error-kind :http
                         (chat client (vector (message "user" "hi"))))))
        (expect-equal (provider-error-status condition) 500 "status is carried on the condition")
        (let ((text (princ-to-string condition)))
          (expect-not-contains text secret "the key never appears in the condition")
          (expect-not-contains text "boom for key" "the provider response body is not propagated")
          (expect-contains text "500" "the status remains available for diagnosis")))
      (expect-equal (script-call-count script) 1 "a 500 is not retried automatically"))))

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

(deftest test-truncation-is-typed-for-both-providers
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: trunc" :finish "length")))
    (declare (ignore script))
    (expect-error provider-error provider-error-kind :truncated
      (chat client (vector (message "user" "hi")))))
  (multiple-value-bind (client script)
      (scripted-client "anthropic" (list (anthropic-text-response "Answer: trunc" :stop "max_tokens")))
    (declare (ignore script))
    (expect-error provider-error provider-error-kind :truncated
      (chat client (vector (message "user" "hi"))))))

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
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Action: develop")))
    (let* ((gen (ax +jiti-signature+ :description "Pick the next Jiti action."))
           (outputs (forward gen client (object "observation" "tests are red"))))
      (expect-equal (jget outputs "action") "develop" "class output parsed to a canonical option")
      (expect-equal (hash-table-count outputs) 1 "optional fields are omitted, not required")
      (let* ((body (script-request script 0))
             (messages (jget body "messages"))
             (system (jget (aref messages 0) "content"))
             (user (jget (aref messages 1) "content")))
        (expect-contains system "<output_fields>" "signature output fields are rendered")
        (expect-contains system "- Action:" "output field label is rendered")
        (expect-contains system "\"develop\"" "class options are rendered")
        (expect-contains system "Pick the next Jiti action." "description is rendered")
        (expect-contains system ", optional" "optional fields are marked optional")
        (expect-contains user "Observation: tests are red" "inputs rendered with title labels")
        (expect (absent (jget body "tools")) "no tools key when the generator has no tools")))))

(deftest test-generation-parses-all-typed-values
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "Action: EXECUTE~cPreview: true~cRestart Id: r-9~cArguments: --fast"
                                      #\Newline #\Newline #\Newline))))
    (declare (ignore script))
    (let* ((gen (ax +jiti-signature+))
           (outputs (forward gen client (object "observation" "ready"))))
      (expect-equal (jget outputs "action") "execute" "enum match is case-insensitive, value canonical")
      (expect (json-true-p (jget outputs "preview")) "boolean parsed to JSON true")
      (expect-equal (jget outputs "restartId") "r-9" "camelCase field matched via its title")
      (expect-equal (jget outputs "arguments") "--fast" "string field parsed"))))

(deftest test-generation-parses-json-null-false-and-empty-arrays
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "Payload: null~cFlags: false~cItems: []~cScores: [1, 2.5]~cTags: []"
                                      #\Newline #\Newline #\Newline #\Newline))))
    (declare (ignore script))
    (let* ((gen (ax (concatenate 'string
                                 "q:string -> payload:json, flags:boolean, items:json[], "
                                 "scores:number[], tags:string[]")))
           (outputs (forward gen client (object "q" "go"))))
      (expect (eq (jget outputs "payload") :null) "JSON null becomes :null, not a missing field")
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
        (expect-equal (jget second-body "tool_choice") "none"
                      "the correction turn forbids a new tool call instead of dropping the tools")
        (expect-equal (length messages) 4 "history is preserved and appended to")
        (expect-equal (jget (aref messages 1) "content")
                      (jget (aref (jget first-body "messages") 1) "content")
                      "the caller's rendered input message is preserved verbatim")
        (expect-equal (jget (aref messages 2) "content") "Action: deploy"
                      "the rejected assistant message stays in history")
        (expect-contains (jget (aref messages 3) "content") "not allowed"
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

(deftest test-generation-rejects-missing-and-unknown-inputs
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Action: develop")))
    (declare (ignore script))
    (let ((gen (ax +jiti-signature+)))
      (expect-error generation-error generation-error-kind :config
        (forward gen client (object)))
      (expect-error generation-error generation-error-kind :config
        (forward gen client (object "observation" "x" "oops" "y"))))))

(deftest test-generation-fails-closed-on-unsupported-field-type
  (expect-error generation-error generation-error-kind :unsupported
    (ax "q:string -> when:date"))
  (expect-error generation-error generation-error-kind :unsupported
    (ax "pic:image -> answer:string")))

;;; ------------------------------------------------------------------
;;; Tool loop
;;; ------------------------------------------------------------------

(defun make-recording-weather-tool ()
  (let ((seen '()))
    (values (tool :name "get_weather"
                  :description "Look up the weather for a city"
                  :parameters (object "type" "object"
                                      "properties" (object "city" (object "type" "string")
                                                           "days" (object "type" "integer"))
                                      "required" (vector "city"))
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
          (expect-contains (jget (aref messages 3) "content") "not valid JSON"
                           "the malformed-argument error is fed back as the tool result"))))))

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
        (expect-contains (jget (aref (jget (script-request script 1) "messages") 3) "content")
                         "unknown argument \"extra\"" "unknown argument rejected by name")
        (let ((content (jget (aref (jget (script-request script 2) "messages") 5) "content")))
          (expect-contains content "missing required argument \"city\"" "missing required argument")
          (expect-contains content "must be of type \"integer\"" "wrong argument type rejected"))))))

(deftest test-unknown-tool-name-is-rejected
  (multiple-value-bind (weather invocations) (make-recording-weather-tool)
    (declare (ignore invocations))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-tool-call "call_e" "launch_missiles" "{}")))))
      (declare (ignore script))
      (let ((gen (ax "question:string -> answer:string" :tools (list weather))))
        (expect-error generation-error generation-error-kind :tool
          (forward gen client (object "question" "?")))))))

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

(deftest test-malformed-wire-tool-input-never-becomes-empty-arguments
  (dolist (provider '("openai" "anthropic"))
    (dolist (bad (list :missing :null 42 *json-false* (vector)))
      (let* ((runs 0)
             (spec (tool :name "deploy" :handler (lambda (args)
                                                  (declare (ignore args))
                                                  (incf runs))))
             (call (if (equal provider "openai")
                       (openai-tool-call "call_bad" "deploy" bad)
                       (object "type" "tool_use" "id" "call_bad" "name" "deploy" "input" bad))))
        (when (eq bad :missing)
          (if (equal provider "openai")
              (remhash "arguments" (jget call "function"))
              (remhash "input" call)))
        (multiple-value-bind (client script)
            (scripted-client provider
                             (list (if (equal provider "openai")
                                       (openai-tool-response (list call))
                                       (encode-json (object "stop_reason" "tool_use"
                                                            "content" (vector call))))))
          (expect-error provider-error provider-error-kind :response
            (forward (ax "q:string -> a:string" :tools (list spec)) client (object "q" "go")))
          (expect-equal runs 0 "invalid wire arguments never run a zero-required-argument tool")
          (expect-equal (script-call-count script) 1 "invalid wire response is not retried"))))))

(deftest test-non-object-tool-arguments-never-reach-the-handler
  ;; A tool with no required arguments is the dangerous case: coercing a JSON
  ;; array, null or number into an empty object would let it execute.
  (let ((runs 0))
    (let ((no-arg-tool (tool :name "deploy"
                             :description "Side-effecting, takes no arguments"
                             :parameters (object "type" "object"
                                                 "properties" (object)
                                                 "required" (vector))
                             :handler (lambda (args)
                                        (declare (ignore args))
                                        (incf runs)
                                        "deployed"))))
      ;; Each of these is valid JSON but is not a JSON object.
      (dolist (payload '("[]" "null" "42" "\"go\"" "true"))
        (setf runs 0)
        (multiple-value-bind (client script)
            (scripted-client "openai"
                             (list (openai-tool-response
                                    (list (openai-tool-call "call_x" "deploy" payload)))
                                   (openai-text-response "Answer: refused")))
          (let* ((gen (ax "question:string -> answer:string" :tools (list no-arg-tool)))
                 (outputs (forward gen client (object "question" "?"))))
            (expect-equal runs 0
                          (format nil "handler must not run for non-object arguments ~a" payload))
            (expect-equal (jget outputs "answer") "refused" "the run continues after rejection")
            (expect-contains (jget (aref (jget (script-request script 1) "messages") 3) "content")
                             "must be a JSON object"
                             (format nil "rejection reported for arguments ~a" payload)))))
      ;; The symmetric accepted case: an actual empty object does run.
      (setf runs 0)
      (multiple-value-bind (client script)
          (scripted-client "openai"
                           (list (openai-tool-response
                                  (list (openai-tool-call "call_y" "deploy" "{}")))
                                 (openai-text-response "Answer: ok")))
        (declare (ignore script))
        (forward (ax "question:string -> answer:string" :tools (list no-arg-tool))
                 client (object "question" "?"))
        (expect-equal runs 1 "an empty JSON object is accepted and runs the handler"))
      ;; And the validator itself rejects the same payloads directly.
      (dolist (value (list (vector) :null 42 "go" *json-true*))
        (expect (validate-tool-arguments no-arg-tool value)
                (format nil "validate-tool-arguments rejects ~s" value))
        (multiple-value-bind (result problems) (invoke-tool no-arg-tool value)
          (expect (null result) "no result for non-object arguments")
          (expect problems "problems reported for non-object arguments")))
      (expect-equal runs 1 "direct validator calls never invoked the handler"))))

;;; ------------------------------------------------------------------
;;; Fail-closed signature field modifiers
;;; ------------------------------------------------------------------

(defun %test-field (&key (type-name "string") (array nil) type-extra field-extra)
  (let ((type (object "name" type-name "isArray" (json-boolean array))))
    (loop for (key value) on type-extra by #'cddr do (setf (gethash key type) value))
    (let ((field (object "name" "q" "title" "Q" "type" type
                         "isOptional" *json-false* "isInternal" *json-false*
                         "isCached" *json-false*)))
      (loop for (key value) on field-extra by #'cddr do (setf (gethash key field) value))
      field)))

(deftest test-unsupported-field-modifiers-fail-closed
  ;; Accepted baseline: a plain field, a described field, and a class field.
  (%check-field-supported (%test-field) :output)
  (%check-field-supported (%test-field :field-extra '("description" "a question")) :input)
  (%check-field-supported (%test-field :type-name "class"
                                       :type-extra (list "options" (vector "a" "b")))
                          :output)
  ;; Rejected: every type modifier this subset cannot enforce.
  (dolist (modifier '(("minLength" 3) ("maxLength" 10) ("minimum" 0) ("maximum" 5)
                      ("pattern" "^a") ("patternDescription" "starts with a")
                      ("format" "email") ("language" "python")))
    (expect-error generation-error generation-error-kind :unsupported
      (%check-field-supported (%test-field :type-extra modifier) :output)))
  ;; Rejected: a nested object shape we would otherwise flatten to a string.
  (expect-error generation-error generation-error-kind :unsupported
    (%check-field-supported (%test-field :type-extra (list "fields" (vector))) :output))
  ;; Rejected: an unknown field-level attribute.
  (expect-error generation-error generation-error-kind :unsupported
    (%check-field-supported (%test-field :field-extra '("isRequired" "yes")) :output))
  ;; Rejected: options on a non-class type, and a class with non-string options.
  (expect-error generation-error generation-error-kind :unsupported
    (%check-field-supported (%test-field :type-extra (list "options" (vector "a"))) :output))
  (expect-error generation-error generation-error-kind :unsupported
    (%check-field-supported (%test-field :type-name "class"
                                         :type-extra (list "options" (vector 1 2)))
                            :output))
  ;; Rejected: a non-boolean isArray flag.
  (expect-error generation-error generation-error-kind :unsupported
    (%check-field-supported (%test-field :type-extra '("isArray" "true")) :output)))

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
        (expect-contains user "Count: 3" "numbers rendered without quotes")
        (expect-contains user "Enabled: true" "booleans rendered as JSON booleans")
        (expect-contains user "Mode: fast" "string value rendered")
        (expect-contains user "Tags: [\"a\",\"b\"]" "arrays rendered as JSON")
        (expect-contains user "Blob: {\"k\":\"v\"}" "json values rendered as JSON")))))

(deftest test-invalid-typed-inputs-are-rejected-before-any-request
  ;; Each pair is the invalid value just outside the boundary; the valid value
  ;; just inside the boundary is covered by the test above.
  (dolist (override (list (list "count" "3")              ; string, not number
                          (list "count" *json-true*)      ; boolean, not number
                          (list "count" :null)            ; null, not number
                          (list "enabled" "true")         ; string, not boolean
                          (list "enabled" 1)              ; number, not boolean
                          (list "mode" *json-false*)      ; not a string
                          (list "tags" "a")               ; scalar, not an array
                          (list "tags" (vector "a" 2))    ; non-string element
                          (list "blob" (lambda () nil)))) ; not JSON-compatible
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response "Answer: ok")))
      (let ((gen (ax +typed-input-signature+)))
        (expect-error generation-error generation-error-kind :config
          (forward gen client (apply #'%typed-inputs override)))
        (expect-equal (script-call-count script) 0
                      (format nil "no provider request is made for invalid input ~a"
                              (first override)))))))

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

(deftest test-unsupported-schema-constraints-fail-closed
  (handler-case
      (progn (tool :name "t"
                   :parameters (object "type" "object"
                                       "properties" (object "x" (object "anyOf" (vector)))
                                       "required" (vector "x"))
                   :handler (lambda (args) (declare (ignore args)) ""))
             (error 'test-failure :text "expected anyOf to be rejected"))
    (tool-error (c) (expect-contains (princ-to-string c) "unsupported schema keyword"
                                     "anyOf is reported, not ignored")))
  (let ((problems (validate-schema-support
                   (object "type" "object"
                           "properties" (object "x" (object "type" "string" "pattern" "^a"))
                           "required" (vector "x"))
                   "schema")))
    (expect problems "an unimplemented string constraint is reported")
    (expect-contains (first problems) "pattern" "the offending keyword is named"))
  (let ((problems (validate-schema-support (object "type" "array") "schema")))
    (expect problems "a non-object top-level schema is rejected")))

(deftest test-schema-support-check-is-recursive-and-type-specific
  (flet ((problems (properties &rest extra)
           (let ((schema (object "type" "object" "properties" properties)))
             (loop for (key value) on extra by #'cddr do (setf (gethash key schema) value))
             (validate-schema-support schema "schema")))
         (named (list needle description)
           (expect (some (lambda (p) (search needle p)) list)
                   (format nil "~a (problems: ~s)" description list))))
    ;; Accepted baseline: a deeply nested but fully supported schema.
    (expect (null (problems (object "outer"
                                    (object "type" "object"
                                            "properties"
                                            (object "items" (object "type" "array"
                                                                    "items" (object "type" "string"
                                                                                    "enum" (vector "a"))))
                                            "required" (vector "items")
                                            "additionalProperties" *json-false*))))
            "a nested supported schema produces no problems")
    ;; A nested unsupported keyword must be reported, not ignored.
    (named (problems (object "outer" (object "type" "object"
                                             "properties" (object "x" (object "type" "string"
                                                                              "minLength" 2)))))
           "minLength" "a constraint two levels deep is reported")
    ;; additionalProperties as a schema (not a boolean) describes properties we
    ;; never check, at any depth.
    (named (problems (object "outer" (object "type" "object"
                                             "additionalProperties"
                                             (object "type" "string"))))
           "additionalProperties" "a schema-valued nested additionalProperties is rejected")
    ;; "required" with the wrong type, or naming an undeclared property.
    (named (problems (object "x" (object "type" "string")) "required" "x")
           "must be an array" "a string \"required\" is rejected")
    (named (problems (object "x" (object "type" "string")) "required" (vector 1))
           "only strings" "a non-string entry in \"required\" is rejected")
    (named (problems (object "x" (object "type" "string")) "required" (vector "y"))
           "undeclared property" "\"required\" naming an undeclared property is rejected")
    (named (problems (object "outer" (object "type" "object"
                                             "properties" (object "x" (object "type" "string"))
                                             "required" (object))))
           "must be an array" "a nested object-valued \"required\" is rejected")
    ;; Keywords legal on another type must not be accepted here.
    (named (problems (object "x" (object "type" "string" "items" (object "type" "string"))))
           "items" "\"items\" on a string type is rejected")
    (named (problems (object "x" (object "type" "array"
                                         "items" (object "type" "string")
                                         "properties" (object))))
           "properties" "\"properties\" on an array type is rejected")
    (named (problems (object "x" (object "type" "array"
                                         "items" (object "type" "string")
                                         "additionalProperties" *json-false*)))
           "additionalProperties" "\"additionalProperties\" on an array type is rejected")
    (named (problems (object "x" (object "type" "object" "enum" (vector (object)))))
           "enum" "\"enum\" on an object type is rejected")
    ;; Composite enum values cannot be compared structurally, so they are refused.
    (named (problems (object "x" (object "type" "string" "enum" (vector (vector "a")))))
           "primitives" "a composite enum value is refused rather than mismatched")
    (named (problems (object "x" (object "type" "string" "enum" (vector))))
           "must not be empty" "an empty enum is rejected")
    ;; Primitive enum values of every supported kind stay acceptable.
    (expect (null (problems (object "x" (object "type" "number" "enum" (vector 1 2.5))
                                    "y" (object "type" "boolean" "enum" (vector *json-true*))
                                    "z" (object "type" "null" "enum" (vector :null)))))
            "primitive enums of each kind are accepted")
    ;; A tool whose nested schema is unenforceable is rejected before any call.
    (let ((runs 0))
      (handler-case
          (progn (tool :name "nested"
                       :parameters (object "type" "object"
                                           "properties"
                                           (object "outer"
                                                   (object "type" "object"
                                                           "properties"
                                                           (object "x" (object "type" "string"
                                                                               "pattern" "^a")))))
                       :handler (lambda (args) (declare (ignore args)) (incf runs) ""))
                 (error 'test-failure :text "expected a nested pattern to be rejected"))
        (tool-error (c) (expect-contains (princ-to-string c) "pattern"
                                         "the nested offending keyword is named")))
      (expect-equal runs 0 "the handler was never reachable"))))

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
    (expect-contains (first (validate-tool-arguments spec (object "mode" "medium")))
                     "must be one of" "enum violation reported")
    (expect-contains (first (validate-tool-arguments spec (object "mode" "fast"
                                                                  "tags" (vector 1))))
                     "tags\"[0]" "array element problems are located")
    (expect-contains (first (validate-tool-arguments spec (object "mode" "fast" "flag" "yes")))
                     "must be of type \"boolean\"" "booleans must be real JSON booleans")
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
           (let ((condition (expect-error provider-error provider-error-kind :http
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
                     (anthropic-text-response "")
                     (anthropic-text-response "Answer: 12C in Oslo")))
              (scripted-client
               "openai"
               (list (openai-tool-response
                      (list (openai-tool-call "call_1" "get_weather" "{\"city\":\"Oslo\"}")))
                     (openai-text-response "no labelled fields here")
                     (openai-text-response "Answer: 12C in Oslo"))))
        (let* ((gen (ax "question:string -> answer:string"
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
            (if (string= provider "anthropic")
                (expect-equal (jget (jget correction "tool_choice") "type") "none"
                              "anthropic tool_choice is the object {\"type\":\"none\"}")
                (expect-equal (jget correction "tool_choice") "none"
                              "openai tool_choice is the string \"none\""))))))))

(deftest test-anthropic-assistant-turn-is-never-empty
  ;; A blank assistant reply kept in a correction history would otherwise
  ;; serialize as an empty content array, which the Messages API rejects.
  (multiple-value-bind (client script)
      (scripted-client "anthropic"
                       (list (anthropic-text-response "")
                             (anthropic-text-response "Answer: recovered")))
    (let* ((gen (ax "question:string -> answer:string" :max-retries 1))
           (outputs (forward gen client (object "question" "?"))))
      (expect-equal (jget outputs "answer") "recovered" "the run recovers from a blank reply")
      (let* ((messages (jget (script-request script 1) "messages"))
             (assistant (find-if (lambda (m) (equal (jget m "role") "assistant")) messages)))
        (expect assistant "the blank assistant turn is still present in the history")
        (expect (plusp (length (jget assistant "content")))
                "the assistant turn carries at least one content block")
        (expect-equal (jget (aref (jget assistant "content") 0) "type") "text"
                      "the placeholder block is a text block")))))

(deftest test-refusal-and-context-window-finish-reasons
  (dolist (case* (list (list "openai" "content_filter" :refusal)
                       (list "openai" "refusal" :refusal)
                       (list "openai" "model_context_window_exceeded" :truncated)
                       (list "anthropic" "refusal" :refusal)
                       (list "anthropic" "model_context_window_exceeded" :truncated)))
    (destructuring-bind (provider reason kind) case*
      (multiple-value-bind (client script)
          (scripted-client provider
                           (list (if (string= provider "anthropic")
                                     (anthropic-text-response "partial" :stop reason)
                                     (openai-text-response "partial" :finish reason))))
        (declare (ignore script))
        (let ((condition (handler-case (progn (chat client (vector (message "user" "hi")))
                                              nil)
                           (provider-error (c) c))))
          (expect condition (format nil "~a/~a signals a provider-error" provider reason))
          (expect-equal (provider-error-kind condition) kind
                        (format nil "~a/~a maps to ~a" provider reason kind)))))))

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

(deftest test-malformed-response-shapes-are-typed-not-raw-errors
  (dolist (payload (list (object "choices" 7)
                         (object "choices" (vector 7))
                         (object "choices" (vector (object "message" "hello")))
                         (object "choices" (vector (object "message" (object "content" 7))))
                         (object "choices" (vector (object "message"
                                                           (object "content" "x"
                                                                   "tool_calls" 5))))
                         (object "choices" (vector (object "message"
                                                           (object "content" "x"
                                                                   "tool_calls" (vector 5)))))
                         (object "choices" (vector (object "message" (object "content" "x")))
                                 "usage" 3)))
    (multiple-value-bind (client script) (scripted-client "openai" (list (encode-json payload)))
      (declare (ignore script))
      (expect-error provider-error provider-error-kind :response
        (chat client (vector (message "user" "hi"))))))
  (dolist (payload (list (object "content" 7)
                         (object "content" (vector 7))
                         (object "content" (vector (object "type" "text" "text" "x"))
                                 "usage" "lots")))
    (multiple-value-bind (client script) (scripted-client "anthropic" (list (encode-json payload)))
      (declare (ignore script))
      (expect-error provider-error provider-error-kind :response
        (chat client (vector (message "user" "hi")))))))

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

(deftest test-number-parsing-uses-the-json-grammar
  ;; Each of these is rejected by the JSON number grammar or unrepresentable,
  ;; and must become a bounded correction rather than an escaping parser error.
  (dolist (bad '("+5" "007" "1e999" "1." ".5" "5,0" "0x10" "1e" "- 5"))
    (multiple-value-bind (client script)
        (scripted-client "openai"
                         (list (openai-text-response (format nil "Count: ~a" bad))
                               (openai-text-response "Count: 5")))
      (let* ((gen (ax "q:string -> count:number" :max-retries 1))
             (outputs (forward gen client (object "q" "how many?"))))
        (expect-equal (jget outputs "count") 5
                      (format nil "~a is corrected to a valid number" bad))
        (expect-equal (script-call-count script) 2
                      (format nil "~a costs exactly one correction turn" bad))
        (expect-contains (jget (aref (jget (script-request script 1) "messages") 3) "content")
                         "Count"
                         (format nil "the correction names the offending field for ~a" bad)))))
  ;; Valid JSON numbers on the other side of the boundary.
  (dolist (pair '(("0" 0) ("-5" -5) ("10" 10) ("1e2" 100.0)))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response (format nil "Count: ~a" (first pair)))))
      (declare (ignore script))
      (let ((outputs (forward (ax "q:string -> count:number") client (object "q" "?"))))
        (expect (< (abs (- (jget outputs "count") (second pair))) 1/1000)
                (format nil "~a parses to ~a" (first pair) (second pair)))))))

(deftest test-label-matching-ignores-quoted-and-parenthesized-prose
  ;; A multi-line Source value containing a Lisp docstring must survive: the
  ;; line `  "Arguments: x is a list"' is content, not a new field.
  (let* ((source (format nil "(defun f (x)~c  \"Arguments: x is a list\"~c  (length x))"
                         #\Newline #\Newline))
         (content (format nil "Action: develop~cSource: ~a~cPreview: true"
                          #\Newline source #\Newline)))
    (multiple-value-bind (client script)
        (scripted-client "openai" (list (openai-text-response content)))
      (declare (ignore script))
      (let ((outputs (forward (ax +jiti-signature+) client (object "observation" "x"))))
        (expect-equal (jget outputs "action") "develop" "the first field still parses")
        (expect-equal (jget outputs "source") source
                      "the whole multi-line source, docstring included, is preserved")
        (expect (json-true-p (jget outputs "preview")) "the field after it still parses")
        (expect (absent (jget outputs "arguments"))
                "the docstring did not populate the Arguments field"))))
  ;; Markdown emphasis and list markers are still accepted as labels.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "- **Action**: execute~c  Preview : false"
                                      #\Newline))))
    (declare (ignore script))
    (let ((outputs (forward (ax +jiti-signature+) client (object "observation" "x"))))
      (expect-equal (jget outputs "action") "execute" "a bulleted, emphasized label is recognized")
      (expect (json-false-p (jget outputs "preview")) "a padded label is recognized")))
  ;; A parenthesized or semicolon-bearing prefix is prose, not a label.
  (multiple-value-bind (client script)
      (scripted-client "openai"
                       (list (openai-text-response
                              (format nil "Action: develop~cSource: line one~cnote (Preview: yes); see above~cmore"
                                      #\Newline #\Newline #\Newline))))
    (declare (ignore script))
    (let ((outputs (forward (ax +jiti-signature+) client (object "observation" "x"))))
      (expect-equal (jget outputs "source")
                    (format nil "line one~cnote (Preview: yes); see above~cmore" #\Newline #\Newline)
                    "parenthesized prose stays inside the Source value")
      (expect (absent (jget outputs "preview")) "no Preview field was invented from prose"))))

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

(deftest test-max-tokens-is-provider-correct
  ;; Anthropic always needs max_tokens; OpenAI gets it only when asked, and as
  ;; max_completion_tokens.
  (multiple-value-bind (client script)
      (scripted-client "openai" (list (openai-text-response "Answer: ok")))
    (chat client (vector (message "user" "hi")))
    (expect (absent (jget (script-request script 0) "max_completion_tokens"))
            "no token cap is invented for OpenAI")
    (expect (absent (jget (script-request script 0) "max_tokens"))
            "the Anthropic field name is never sent to OpenAI"))
  (multiple-value-bind (transport script)
      (make-scripted-transport (list (openai-text-response "Answer: ok")))
    (let ((client (ai :name "openai" :model +test-openai-model+ :api-key "dummy"
                      :transport transport :max-tokens 256)))
      (chat client (vector (message "user" "hi")))
      (expect-equal (jget (script-request script 0) "max_completion_tokens") 256
                    "an explicit cap is sent as max_completion_tokens")))
  (multiple-value-bind (client script)
      (scripted-client "anthropic" (list (anthropic-text-response "Answer: ok")))
    (chat client (vector (message "user" "hi")))
    (expect-equal (jget (script-request script 0) "max_tokens") 4096
                  "Anthropic still gets its required max_tokens")))

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
      (check "content_filter finish reason"
             (list (openai-text-response "x" :finish "content_filter"))))
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
