;;;; mcp-chat.lisp --- native service/tool integration, not fixture replay.
(in-package #:axllm)

(export '(run-mcp-chat-tests run-mcp-chat-tests-or-die))

(defclass mcp-chat-test-service ()
  ((callback :initarg :callback :reader %mcp-chat-test-callback)
   (requests :initform nil :accessor %mcp-chat-test-requests)))

(defmethod ax-chat ((service mcp-chat-test-service) request &optional options)
  (%mcp-runtime-equal (jget options "stream") false "chat forces non-streaming")
  (push request (%mcp-chat-test-requests service))
  (funcall (%mcp-chat-test-callback service) request (length (%mcp-chat-test-requests service))))

(defclass mcp-chat-test-stream () ((closed :initform nil :accessor %mcp-chat-test-closed)))
(defmethod ax-stream-close ((stream mcp-chat-test-stream)) (setf (%mcp-chat-test-closed stream) t))

(defun %mcp-chat-test-call (name &optional (params (object)) (id "call-1"))
  (object "id" id "type" "function" "function" (object "name" name "params" params)))

(defun %mcp-chat-test-reply (&rest calls)
  (object "results" (vector (if calls (object "function_calls" (coerce calls 'vector))
                               (object "content" "Finished")))))

(defun %mcp-chat-test-wire-reply (message)
  (encode-json (object "id" "reply" "object" "chat.completion" "model" "gpt-6-luna"
                       "choices" (vector (object "index" 0 "message" message "finish_reason" "stop"))
                       "usage" (object "prompt_tokens" 2 "completion_tokens" 3 "total_tokens" 5))))

(defun %mcp-chat-test-provider-child ()
  ;; Real provider request mapping and normalization plus a real MCP pipe.
  (%with-mcp-runtime-stdio (client)
    (let* ((requests nil) (ucp-calls nil) (inline-args nil) (formatted nil)
           (merchant (make-ucp-client
                      (object "version" "2026-04-08")
                      (lambda (op args opts)
                        (push (list op args opts) ucp-calls)
                        (object "cart" "cart-7")) :namespace "merchant"))
           (service
             ;; Pin the dialect via the compatible profile, not an old model.
             (ai :name "openai-compatible" :model "gpt-6-luna" :api-key "test-only"
                 :base-url "https://provider.example/v1"
                 :transport
                 (lambda (url headers body)
                   (declare (ignore url headers))
                   (push (parse-json body) requests)
                   (values
                    (%mcp-chat-test-wire-reply
                     (if (= 1 (length requests))
                         (object "role" "assistant" "content" "Using tools"
                                 "tool_calls"
                                 (vector
                                  (object "id" "echo-1" "type" "function"
                                          "function" (object "name" "echo" "arguments" "{\"text\":\"hello there\"}"))
                                  (object "id" "cart-1" "type" "function"
                                          "function" (object "name" "merchant_cart_create" "arguments" "{\"quantity\":2}"))
                                  (object "id" "inline-1" "type" "function"
                                          "function" (object "name" "LocalLookup" "arguments" "{\"key\":\"x\"}"))))
                         (object "role" "assistant" "content" "All three completed"))) 200))))
           (inline (tool :name "local_lookup" :parameters (object "type" "object" "properties"
                                                                  (object "key" (object "type" "string")))
                         :handler (lambda (args) (setf inline-args args) (object "local" 8))))
           (request (object "chat_prompt" (vector (object "role" "system" "content" "Keep me")
                                                  (object "role" "user" "content" "Run the tools"))
                            "functions" (vector (object "name" "declaration_only" "description" "Not a binding"
                                                         "parameters" (object "type" "object" "properties" (object))))))
           (result (mcp-chat service request
                             (object "mcp" client "ucp" merchant "functions" (vector inline)
                                     "functionResultFormatter"
                                     (lambda (raw) (push raw formatted) "formatted result"))))
           (messages (jget result "messages"))
           (functions (remove-if-not (lambda (m) (equal (jget m "role") "function"))
                                     (coerce messages 'list))))
      (%mcp-runtime-equal (jget (aref (jget (jget result "response") "results") 0) "content")
                          "All three completed" "provider final response")
      (%mcp-runtime-equal (length requests) 2 "two actual provider turns")
      (%mcp-runtime-equal (length (jget request "chat_prompt")) 2 "input history not mutated")
      (%mcp-runtime-equal (jget inline-args "key") "x" "normalized inline dispatch with parsed args")
      (%mcp-runtime-equal (first (first ucp-calls)) "cart.create" "namespaced UCP dispatch")
      (%mcp-runtime-equal (jget (second (first ucp-calls)) "quantity") 2 "UCP arguments")
      (%mcp-runtime-equal (length formatted) 3 "formatter runs for each result")
      (%mcp-runtime-equal (jget (jget (jget (first functions) "protocolResult") "protocol") "namespace")
                          "fixture" "MCP result namespace")
      (%mcp-runtime-equal (jget (aref (jget (jget (jget (first functions) "protocolResult") "value") "content") 0) "text")
                          "hello there" "raw child reply, not formatted text")
      (%mcp-runtime-equal (jget (jget (jget (second functions) "protocolResult") "protocol") "kind")
                          "ucp" "UCP protocol provenance")
      (%mcp-runtime-equal (jget (jget (jget (jget (second functions) "protocolResult") "value") "value") "cart")
                          "cart-7" "UCP Core outcome retained")
      (%mcp-runtime-equal (jget (third functions) "protocolResult") :null "inline has no invented protocol")
      (let* ((second-request (first requests)) (wire-messages (jget second-request "messages"))
             (tools (jget second-request "tools")))
        (%mcp-runtime-equal (length wire-messages) 6 "provider receives retained assistant and function history")
        (%mcp-runtime-equal (jget (aref wire-messages 3) "tool_call_id") "echo-1" "function ID on wire")
        (%mcp-runtime-equal (jget (aref wire-messages 3) "content") "formatted result" "formatted text on wire")
        (%mcp-runtime-true (find "declaration_only" tools
                                 :key (lambda (s) (jget (jget s "function") "name")) :test #'equal)
                           "request tool declarations preserved")
        (%mcp-runtime-true (not (search "test-only" (encode-json second-request))) "no auth in history"))
      ;; Explicit close is the caller's responsibility; the child is really reaped.
      (let* ((transport (mcp-client-transport client)) (process (slot-value transport 'process)))
        (mcp-close client)
        (%mcp-runtime-true (not (uiop:process-alive-p process)) "chat child reaped after close")))))

(defun %mcp-chat-test-context-catalog ()
  (let* ((transport
           (make-mcp-scripted-transport
            (vector
             (object "method" "initialize" "result"
                     (object "protocolVersion" "2025-11-25" "capabilities"
                             (object "tools" (object) "prompts" (object) "resources" (object))))
             (object "method" "tools/list" "result" (object "tools" (vector (object "name" "old" "inputSchema" (object "type" "object")))))
             (object "method" "tools/list" "result" (object "tools" (vector (object "name" "fresh" "inputSchema" (object "type" "object")))))
             (object "method" "prompts/get" "result"
                     (object "messages" (vector (object "role" "user" "content" (object "type" "image" "mimeType" "image/png" "data" "aW1n"))
                                                (object "role" "assistant" "content" (object "type" "text" "text" "Remote assistant")))))
             (object "method" "resources/read" "result" (object "contents" (vector (object "uri" "test://data" "text" "Resource text")
                                                                                    (object "uri" "test://file" "blob" "Ymlu"))))
             (object "method" "tools/call" "result" (object "structuredContent" (object "fresh" true))))))
         (client (make-mcp-client transport :namespace "catalog" :era "legacy"))
         (service (make-instance
                   'mcp-chat-test-service
                   :callback
                   (lambda (request step)
                     (cond
                       ((= step 1)
                        (%mcp-runtime-equal (map 'vector (lambda (m) (jget m "role")) (jget request "chat_prompt"))
                                            (vector "system" "system" "user" "assistant" "user" "user" "system")
                                            "only leading system prefix stays before context")
                        (object "results" (vector (object "name" "thinking" "thought" "Keep thought"
                                                          "thought_blocks" (vector (object "type" "thinking" "signature" "signed")))
                                                   (object "function_calls" (vector (%mcp-chat-test-call "refresh"))))))
                       ((= step 2)
                        (%mcp-runtime-true (find "fresh" (jget request "functions") :key #'native-tool-name :test #'equal)
                                           "new catalogue offered on next model turn")
                        (%mcp-runtime-true (not (find "old" (jget request "functions") :key #'native-tool-name :test #'equal))
                                           "removed tool disappears")
                        (%mcp-chat-test-reply (%mcp-chat-test-call "fresh" "" "fresh-1")))
                       (t (%mcp-chat-test-reply)))))))
    (unwind-protect
         (let* ((result (mcp-chat
                         service
                         (object "chatPrompt" (vector (object "role" "system" "content" "S1") (object "role" "system" "content" "S2")
                                                       (object "role" "user" "content" "Question") (object "role" "system" "content" "Later")))
                         (object "mcp" client "mcpContext"
                                 (vector (object "client" "catalog" "prompt" (object "name" "guide" "arguments" (object "topic" "stock")))
                                         (object "client" client "resource" (object "uri" "test://data")))
                                 "functions" (vector (tool :name "refresh" :handler (lambda (args) (declare (ignore args)) (mcp-refresh client) "refreshed"))))))
                (messages (jget result "messages"))
                (parts (jget (aref messages 2) "content")))
           (%mcp-runtime-contains (jget (aref parts 0) "text") "trust=\"untrusted\"" "untrusted context delimiter")
           (%mcp-runtime-equal (jget (aref parts 1) "image") "aW1n" "prompt image retained")
           (%mcp-runtime-contains (jget (aref messages 3) "content") "Remote assistant" "assistant context rendered")
           (%mcp-runtime-equal (jget (aref (jget (aref messages 4) "content") 2) "mimeType")
                               "application/octet-stream" "resource blob retained as file")
           (%mcp-runtime-equal (jget (aref messages 7) "thought") "Keep thought" "thought-only result retained")
           (%mcp-runtime-equal (jget (aref (jget (aref messages 7) "thoughtBlocks") 0) "signature")
                               "signed" "opaque signed thought block retained")
           (let ((call (find "tools/call" (mcp-scripted-requests transport) :key (lambda (r) (jget r "method")) :test #'equal)))
             (%mcp-runtime-equal (jget (jget call "params") "name") "fresh" "new binding dispatches on wire"))
           (%mcp-runtime-equal (length (%mcp-chat-test-requests service)) 3 "bounded three-turn catalogue run"))
      (mcp-close client))))

(defun %mcp-chat-test-failures ()
  (let ((count 0)
        (request (object "chat_prompt" (vector (object "role" "user" "content" "Run"))
                         "functions" (vector (object "name" "missing" "description" "Declaration only"
                                                      "parameters" (object "type" "object" "properties" (object)))))))
    (flet ((service (callback) (make-instance 'mcp-chat-test-service :callback callback)))
      (let ((ai (service (lambda (req step) (declare (ignore req step)) (%mcp-chat-test-reply (%mcp-chat-test-call "again"))))))
        (%mcp-runtime-contains
         (princ-to-string (%mcp-runtime-fails (mcp-error) "bounded model steps"
                            (mcp-chat ai request (object "maxSteps" 2 "functions"
                                                         (vector (tool :name "again" :handler (lambda (args) (declare (ignore args)) (incf count))))))))
         "exceeded 2" "step limit reported")
        (%mcp-runtime-equal count 2 "exact bounded tool dispatch count")
        (%mcp-runtime-equal (length (%mcp-chat-test-requests ai)) 2 "exact bounded model count"))
      (%mcp-runtime-fails (mcp-error) "request declaration is not an executable binding"
        (mcp-chat (service (lambda (req step) (declare (ignore req step)) (%mcp-chat-test-reply (%mcp-chat-test-call "missing")))) request))
      (let ((before count))
        (%mcp-runtime-contains
         (princ-to-string
          (%mcp-runtime-fails (ax-error) "invalid JSON fails before dispatch"
            (mcp-chat (service (lambda (req step) (declare (ignore req step)) (%mcp-chat-test-reply (%mcp-chat-test-call "again" "{"))))
                      request (object "functions" (vector (tool :name "again" :handler (lambda (args) (declare (ignore args)) (incf count))))))))
         "Invalid JSON" "failure is from argument parsing")
        (%mcp-runtime-equal count before "bad arguments never reach handler"))
      (let ((stream (make-instance 'mcp-chat-test-stream)))
        (%mcp-runtime-fails (mcp-error) "streaming response refused"
          (mcp-chat (service (lambda (req step) (declare (ignore req step)) stream)) request))
        (%mcp-runtime-true (%mcp-chat-test-closed stream) "rejected stream closed"))
      (%with-mcp-runtime-stdio (client)
        (%mcp-runtime-fails (error) "tool failure leaves caller-owned client usable"
          (mcp-chat (service (lambda (req step) (declare (ignore req step)) (%mcp-chat-test-reply (%mcp-chat-test-call "echo" "{\"text\":\"before failure\"}"))))
                    request (object "mcp" client "functionResultFormatter" (lambda (v) (declare (ignore v)) (error "formatter failed")))))
        (%mcp-runtime-equal (jget (aref (jget (mcp-call-tool client "echo" (object "text" "after failure")) "content") 0) "text")
                            "after failure" "client still usable after chat failure")))))

(defun %mcp-chat-test-protocol-error ()
  (let* ((transport
           (make-mcp-scripted-transport
            (vector (object "method" "initialize" "result"
                            (object "protocolVersion" "2025-11-25" "capabilities" (object "tools" (object))))
                    (object "method" "tools/list" "result"
                            (object "tools" (vector (object "name" "lookup" "inputSchema" (object "type" "object")))))
                    (object "method" "tools/call" "error" (object "code" -32001 "message" "backend refused"))
                    (object "method" "tools/call" "result" (object "content" (vector (object "type" "text" "text" "recovered")))))))
         (client (make-mcp-client transport :namespace "broken" :era "legacy"))
         (service (make-instance 'mcp-chat-test-service
                                 :callback (lambda (request step)
                                             (declare (ignore request step))
                                             (%mcp-chat-test-reply (%mcp-chat-test-call "lookup")))))
         (request (object "chat_prompt" (vector (object "role" "user" "content" "Query")))))
    (unwind-protect
         (progn
           (let ((condition (%mcp-runtime-fails (mcp-error) "protocol error reaches caller"
                              (mcp-chat service request (object "mcp" client)))))
             (%mcp-runtime-equal (mcp-error-code condition) -32001 "wire error code retained"))
           (%mcp-runtime-equal (length (%mcp-chat-test-requests service)) 1 "no model turn after tool failure")
           (%mcp-runtime-equal (jget (aref (jget (mcp-call-tool client "lookup" (object)) "content") 0) "text")
                               "recovered" "wire failure did not close borrowed client")
           (%mcp-runtime-equal (count "tools/call" (mcp-scripted-requests transport)
                                      :key (lambda (r) (jget r "method")) :test #'equal)
                               2 "actual failing dispatch and recovery reached transport")
           ;; An exact name must beat an earlier normalized match.
           (let* ((wrong 0) (right 0)
                  (saved-formatter (get-global "functionResultFormatter"))
                  (ai (make-instance 'mcp-chat-test-service
                                     :callback (lambda (req step)
                                                 (declare (ignore req))
                                                 (if (= step 1) (%mcp-chat-test-reply (%mcp-chat-test-call "getThing"))
                                                     (%mcp-chat-test-reply))))))
             (unwind-protect
                  (progn
                    (set-global "functionResultFormatter" (lambda (raw) (format nil "global:~a" raw)))
                    (let* ((result
                             (mcp-chat ai request
                                       (object "functions"
                                               (vector (tool :name "get_thing" :handler (lambda (args) (declare (ignore args)) (incf wrong)))
                                                       (tool :name "getThing" :handler (lambda (args) (declare (ignore args)) (incf right)))))))
                           (function (find "function" (jget result "messages") :key (lambda (m) (jget m "role")) :test #'equal)))
                      (%mcp-runtime-equal (jget function "result") "global:1" "global formatter is the default")))
               (set-global "functionResultFormatter" saved-formatter))
             (%mcp-runtime-equal wrong 0 "normalized match loses to exact match")
             (%mcp-runtime-equal right 1 "exact handler invoked")))
      (mcp-close client))))

(defun run-mcp-chat-tests (&key (stream *standard-output*))
  (let ((passed 0) (failed 0))
    (dolist (test '(%mcp-chat-test-provider-child %mcp-chat-test-context-catalog
                   %mcp-chat-test-failures %mcp-chat-test-protocol-error))
      (handler-case (progn (funcall test) (incf passed))
        (error (condition) (incf failed) (format stream "~&FAIL ~a: ~a~%" test condition))))
    (format stream "~&mcp chat: ~a passed, ~a failed~%" passed failed)
    (values passed failed)))

(defun run-mcp-chat-tests-or-die ()
  (multiple-value-bind (passed failed) (run-mcp-chat-tests)
    (declare (ignore passed))
    (unless (zerop failed) (error "MCP chat checks failed: ~a" failed))
    t))
