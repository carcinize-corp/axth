;;;; mcp-chat.lisp --- synchronous native MCP/UCP chat orchestration.
;;;; Provider mapping, tool specifications and call conversion remain Core-owned.

(in-package #:axllm)

(defun %mcp-chat-copy (value)
  (let ((out (object)))
    (when (hash-table-p value)
      (maphash (lambda (key item) (setf (gethash key out) item)) value))
    out))

(defun %mcp-chat-sequence (value)
  (cond ((%array-p value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %mcp-chat-resource-part (resource)
  (if (%mcp-present-key-p resource "text")
      (object "type" "text" "text" (jget resource "text"))
      (object "type" "file" "data" (jget resource "blob")
              "filename" (jget resource "uri")
              "mimeType" (jget resource "mimeType" "application/octet-stream"))))

(defun %mcp-chat-content-part (content)
  (let ((kind (jget content "type")) (mime (jget content "mimeType")))
    (cond ((equal kind "text") (object "type" "text" "text" (jget content "text")))
          ((equal kind "image")
           (object "type" "image" "image" (jget content "data") "mimeType" mime
                   "altText" (format nil "[MCP image from remote server: ~a]" mime)))
          ((equal kind "audio")
           (object "type" "audio" "data" (jget content "data") "mimeType" mime
                   "transcription" (format nil "[MCP audio from remote server: ~a]" mime)))
          ((equal kind "resource_link")
           (object "type" "url" "url" (jget content "uri")
                   "title" (jget content "name" (jget content "title"))
                   "description" (jget content "description")))
          (t (%mcp-chat-resource-part (jget content "resource"))))))

(defun %mcp-chat-content-text (content)
  (let ((kind (jget content "type")))
    (cond ((equal kind "text") (jget content "text"))
          ((equal kind "image") (format nil "[Image: ~a]" (jget content "mimeType")))
          ((equal kind "audio") (format nil "[Audio: ~a]" (jget content "mimeType")))
          ((equal kind "resource_link") (format nil "[Resource: ~a]" (jget content "uri")))
          (t (let ((resource (jget content "resource")))
               (if (%mcp-present-key-p resource "text") (jget resource "text")
                   (format nil "[Binary resource: ~a]" (jget resource "uri"))))))))

(defun execution-context-resolve-context-prompt (context &optional requests)
  "Resolve MCP prompt/resource REQUESTS into untrusted chat messages.
Each request names client (a namespace or client instance) and prompt or
resource, as in the MCP execution API. Clients remain owned by the caller."
  (execution-context-initialize context)
  (let ((out (%new-array)))
    (dolist (request (%mcp-chat-sequence requests))
      (let* ((designator (jget request "client"))
             (client (if (typep designator 'mcp-client) designator
                         (find designator (execution-context-mcp context)
                               :key #'mcp-namespace :test #'equal)))
             (prompt (jget request "prompt"))
             (resource (jget request "resource")))
        (unless client (%mcp-fail "Unknown MCP client namespace: ~a" designator))
        (if (hash-table-p prompt)
            (let* ((name (jget prompt "name"))
                   (result (mcp-get-prompt client name (jget prompt "arguments")))
                   (header (format nil "<mcp_context source=~s kind=~s name=~s trust=~s>"
                                   (mcp-namespace client) "prompt" name "untrusted")))
              (loop for message across (%event-array (jget result "messages"))
                    for content = (jget message "content") do
                (vector-push-extend
                 (if (equal (jget message "role") "assistant")
                     (object "role" "assistant" "content"
                             (format nil "~a~%~a~%</mcp_context>" header (%mcp-chat-content-text content)))
                     (object "role" "user" "content"
                             (vector (object "type" "text" "text" header)
                                     (%mcp-chat-content-part content)
                                     (object "type" "text" "text" "</mcp_context>")))) out)))
            (let* ((uri (jget resource "uri"))
                   (result (mcp-read-resource client uri))
                   (parts (%new-array)))
              (vector-push-extend
               (object "type" "text" "text"
                       (format nil "<mcp_context source=~s kind=~s uri=~s trust=~s>"
                               (mcp-namespace client) "resource" uri "untrusted")) parts)
              (loop for content across (%event-array (jget result "contents"))
                    do (vector-push-extend (%mcp-chat-resource-part content) parts))
              (vector-push-extend (object "type" "text" "text" "</mcp_context>") parts)
              (vector-push-extend (object "role" "user" "content" parts) out)))))
    out))

(defun %mcp-chat-find-binding (bindings name)
  ;; Exact match first, then the reference's first ASCII-normalized match.
  (labels ((normalized (value)
             (string-downcase (cl-ppcre:regex-replace-all "[^a-zA-Z0-9]" value ""))))
    (or (find name bindings :key #'native-tool-name :test #'equal)
        (and (stringp name)
             (find (normalized name) bindings
                   :key (lambda (spec) (normalized (native-tool-name spec))) :test #'equal)))))

(defun %mcp-chat-calls (result)
  (mapcar (lambda (call)
            (if (hash-table-p (jget call "function")) call
                (axllm/core::completion-call-to-chat-impl call)))
          (%mcp-chat-sequence (jget result "function_calls" (jget result "functionCalls")))))

(defun %mcp-chat-format-result (value)
  (let ((formatter (axllm/core::core-axgen-function-result-formatter)))
    (funcall (if (functionp formatter) formatter #'tool-result-text) value)))

(defun mcp-chat (service request &optional (options (object)))
  "Run a bounded non-streaming native tool loop. Return {response,messages}.
REQUEST uses AX-CHAT's Core shape (chat_prompt); chatPrompt is also accepted.
OPTIONS accepts functions, mcp/ucp/executionContext, mcpContext, maxSteps
(default 10), cancellation, and functionResultFormatter (default the global
formatter, falling back to TOOL-RESULT-TEXT).
Inline tools use TOOL's one-argument handler; native tools receive execution
context and cancellation. Borrowed clients and SERVICE are never closed here."
  (let* ((context (resolve-execution-context options))
         (inline (%mcp-chat-sequence (jget options "functions")))
         (bindings nil) (revision :initial)
         (messages (%new-array))
         (original (%mcp-chat-sequence (jget request "chat_prompt" (jget request "chatPrompt"))))
         (split (or (position-if-not (lambda (m) (equal (jget m "role") "system")) original)
                    (length original)))
         (max-steps (jget options "maxSteps" 10))
         (formatter (jget options "functionResultFormatter" #'%mcp-chat-format-result))
         (call-context (%mcp-chat-copy options)))
    (unless (and (integerp max-steps) (<= 0 max-steps))
      (%mcp-fail "MCP chat maxSteps must be a nonnegative integer"))
    (unless (functionp formatter) (%mcp-fail "MCP chat formatter must be a function"))
    (setf (gethash "ai" call-context) service
          (gethash "_mcpExecutionContext" call-context) context)
    (when context (execution-context-initialize context))
    (dolist (message (subseq original 0 split)) (vector-push-extend (%mcp-json-clone message) messages))
    (when context
      (loop for message across (execution-context-resolve-context-prompt context (jget options "mcpContext"))
            do (vector-push-extend message messages)))
    (dolist (message (subseq original split)) (vector-push-extend (%mcp-json-clone message) messages))
    (dotimes (step max-steps)
      (declare (ignorable step))
      (%mcp-check-context call-context)
      (let ((next (and context (mapcar #'mcp-catalog-revision (execution-context-mcp context)))))
        (unless (equal next revision)
          (setf bindings (append inline (when context (execution-context-native-tools context)))
                revision next)))
      (let ((turn (%mcp-chat-copy request)) (turn-options (%mcp-chat-copy options)))
        (remhash "chatPrompt" turn)
        (setf (gethash "chat_prompt" turn) (%mcp-json-clone messages)
              (gethash "functions" turn)
              (coerce (append (%mcp-chat-sequence (jget request "functions"))
                              (mapcar #'axllm/core::tool-spec-impl bindings)) 'vector)
              (gethash "stream" turn-options) false)
        (let* ((response (ax-chat service turn turn-options))
               (results (and (hash-table-p response) (jget response "results")))
               (calls nil))
          (unless (%array-p results)
            (ignore-errors (ax-stream-close response))
            (%mcp-fail "MCP high-level chat requires a non-streaming response with results"))
          (loop for result across results for result-calls = (%mcp-chat-calls result) do
            (setf calls (append calls result-calls))
            (when (or (%mcp-present-key-p result "content") result-calls
                      (%present (jget result "thought"))
                      (%mcp-chat-sequence (jget result "thought_blocks" (jget result "thoughtBlocks"))))
              (let ((message (object "role" "assistant")))
                (dolist (key '("content" "name" "thought"))
                  (when (%mcp-present-key-p result key) (setf (gethash key message) (jget result key))))
                (when result-calls (setf (gethash "functionCalls" message) (coerce result-calls 'vector)))
                (let ((blocks (jget result "thought_blocks" (jget result "thoughtBlocks"))))
                  (unless (eq blocks :null) (setf (gethash "thoughtBlocks" message) blocks)))
                (vector-push-extend message messages))))
          (unless calls (return-from mcp-chat (object "response" response "messages" messages)))
          (dolist (call calls)
            (%mcp-check-context call-context)
            (let* ((function (jget call "function")) (name (jget function "name"))
                   (binding (%mcp-chat-find-binding bindings name))
                   (params (jget function "params"))
                   (args (cond ((stringp params) (parse-json (if (equal params "") "{}" params)))
                               ((eq params :null) (object)) (t params))))
              (unless binding (%mcp-fail "MCP chat tool not found: ~a" name))
              (let* ((raw (if (native-tool-handler binding) (native-tool-call binding args call-context)
                              (funcall (tool-handler binding) args)))
                     (message (object "role" "function" "functionId" (jget call "id")
                                      "result" (funcall formatter raw)))
                     (protocol (jget binding "protocol")))
                (unless (eq protocol :null)
                  (setf (gethash "protocolResult" message) (object "protocol" protocol "value" raw)))
                (vector-push-extend message messages)))))))
    (%mcp-fail "MCP high-level chat exceeded ~a model steps" max-steps)))
