;;;; ax-example:start
;;;; title: Common Lisp MCP Tool Call
;;;; group: mcp
;;;; description: Calls one MCP tool by name and prints the content blocks it returned.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY, AX_MCP_COMMAND, AX_MCP_TOOL
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; A tool result is content blocks, not a string: text, resources and
;;;; structured output arrive side by side, so the caller decides what to read.

(defpackage #:ax-example/mcp-tool-call
  (:use #:cl))

(in-package #:ax-example/mcp-tool-call)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(defun mcp-command ()
  (or (uiop:getenv "AX_MCP_COMMAND")
      (error "Set AX_MCP_COMMAND to an MCP server command.")))

(defun tool-name ()
  (or (uiop:getenv "AX_MCP_TOOL") "echo"))

(let* ((argv (uiop:split-string (mcp-command) :separator " "))
       (transport (ax:make-mcp-stdio-transport (first argv) :arguments (rest argv)))
       (mcp (ax:make-mcp-client transport "name" "ax-lisp-example" "version" "0.1.0")))
  (unwind-protect
       (let ((result (ax:mcp-call-tool mcp (tool-name)
                                       (ax:object "message" "hello from Common Lisp"))))
         (format t "~&tool       : ~a~%" (tool-name))
         (format t "~&isError    : ~a~%" (ax:jget result "isError"))
         (loop for block across (ax:jget result "content")
               do (format t "~&  ~a: ~a~%" (ax:jget block "type") (ax:jget block "text"))))
    (ax:mcp-close mcp)))
