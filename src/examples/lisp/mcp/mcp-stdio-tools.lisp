;;;; ax-example:start
;;;; title: Common Lisp MCP Tools Over Stdio
;;;; group: mcp
;;;; description: Connects to an MCP server over stdio and lists the tools it advertises.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY, AX_MCP_COMMAND
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; An MCP client is a transport plus options. The stdio transport launches
;;;; the server as a child process; nothing in the command is re-parsed by a
;;;; shell.

(defpackage #:ax-example/mcp-stdio-tools
  (:use #:cl))

(in-package #:ax-example/mcp-stdio-tools)

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
      (error "Set AX_MCP_COMMAND to an MCP server command, for example \"npx -y @modelcontextprotocol/server-everything\".")))

(let* ((argv (uiop:split-string (mcp-command) :separator " "))
       (transport (ax:make-mcp-stdio-transport (first argv) :arguments (rest argv)))
       (mcp (ax:make-mcp-client transport "name" "ax-lisp-example" "version" "0.1.0")))
  (unwind-protect
       (let ((tools (ax:mcp-list-tools mcp)))
         (format t "~&tools      : ~d~%" (length (ax:jget tools "tools")))
         (loop for tool across (ax:jget tools "tools")
               do (format t "~&  ~a~30t~a~%"
                          (ax:jget tool "name")
                          (ax:jget tool "description"))))
    (ax:mcp-close mcp)))
