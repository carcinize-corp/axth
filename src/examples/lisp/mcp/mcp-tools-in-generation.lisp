;;;; ax-example:start
;;;; title: Common Lisp MCP Tools Inside A Generator
;;;; group: mcp
;;;; description: Wraps MCP tools as native Ax tools so a typed generator can call them during a run.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY, AX_MCP_COMMAND
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; An MCP tool and an Ax tool are the same idea with different wire formats:
;;;; a request spec plus something that answers. Bridging them is a handler
;;;; that forwards to the MCP client, so the generator's tool loop is unchanged.

(defpackage #:ax-example/mcp-tools-in-generation
  (:use #:cl))

(in-package #:ax-example/mcp-tools-in-generation)

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

(defun mcp-tool-as-ax-tool (mcp descriptor)
  "DESCRIPTOR, one MCP tool, as a native Ax tool over the same client."
  (let ((name (ax:jget descriptor "name")))
    (ax:tool :name name
             :description (let ((text (ax:jget descriptor "description")))
                            (if (stringp text) text name))
             ;; The server's own schema is already the shape Ax validates against.
             :parameters (ax:jget descriptor "inputSchema")
             :handler (lambda (arguments)
                        (let ((result (ax:mcp-call-tool mcp name arguments)))
                          ;; tool-result-text flattens the content blocks a
                          ;; model can read.
                          (ax:tool-result-text result))))))

(let* ((argv (uiop:split-string (mcp-command) :separator " "))
       (transport (ax:make-mcp-stdio-transport (first argv) :arguments (rest argv)))
       (mcp (ax:make-mcp-client transport "name" "ax-lisp-example" "version" "0.1.0")))
  (unwind-protect
       (let* ((descriptors (ax:jget (ax:mcp-list-tools mcp) "tools"))
              (tools (loop for descriptor across descriptors
                           collect (mcp-tool-as-ax-tool mcp descriptor)))
              (program (ax:ax "question:string -> answer:string"
                              :tools tools
                              :description "Use the available tools when they help.")))
         (format t "~&bridged    : ~d tool(s)~%" (length tools))
         (multiple-value-bind (output usage)
             (ax:forward program (client)
                         (ax:object "question" "Use a tool to echo the word handshake, then report it."))
           (declare (ignore usage))
           (format t "~&answer     : ~a~%" (ax:jget output "answer")))
         (format t "~&tool calls : ~a~%"
                 (ax:encode-json (ax:generator-function-call-traces program))))
    (ax:mcp-close mcp)))
