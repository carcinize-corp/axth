;;;; mcp-transport-stdio.lisp --- MCP over a local child process.
;;;;
;;;; Newline-delimited JSON-RPC on the child's stdin and stdout. The
;;;; transport owns the process: it starts it, serializes access to the pipe
;;;; so two threads cannot interleave frames, routes any message that is not
;;;; the response it is waiting for to the inbound handler, and terminates
;;;; the child on close.

(in-package #:axllm)

(defclass mcp-stdio-transport (mcp-transport)
  ((process :initarg :process :reader mcp-stdio-process)
   (command :initarg :command :reader mcp-stdio-command)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-stdio") :reader %stdio-lock)
   (closed :initform nil :accessor %stdio-closed))
  (:documentation "MCP over a local process's standard input and output."))

(defun make-mcp-stdio-transport (command &key arguments environment directory)
  "Start COMMAND and speak MCP to it over newline-delimited JSON-RPC.

ENVIRONMENT is an alist of extra variables for the child. The child is this
transport's to own: MCP-TRANSPORT-CLOSE terminates it."
  (let ((process (uiop:launch-program (cons command (mapcar #'princ-to-string
                                                            (or arguments '())))
                                      :input :stream :output :stream
                                      :error-output :interactive
                                      :directory directory
                                      :environment
                                      (when environment
                                        (append (mapcar (lambda (pair)
                                                          (format nil "~a=~a" (car pair) (cdr pair)))
                                                        environment)
                                                (sb-ext:posix-environ))))))
    (make-instance 'mcp-stdio-transport :process process
                                        :command (cons command (or arguments '())))))

(defun %mcp-stdio-streams (transport)
  (let ((process (mcp-stdio-process transport)))
    (unless (and process (not (%stdio-closed transport))
                 (uiop:process-alive-p process))
      (%mcp-fail "MCP stdio process is not connected"))
    (values (uiop:process-info-input process) (uiop:process-info-output process))))

(defmethod mcp-transport-send ((transport mcp-stdio-transport) message)
  (multiple-value-bind (input output) (%mcp-stdio-streams transport)
    (sb-thread:with-mutex ((%stdio-lock transport))
      (write-string (mcp-stdio-encode message) input)
      (finish-output input)
      (loop
        (let ((line (read-line output nil nil)))
          (unless line (%mcp-fail "MCP stdio process closed"))
          (when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
            (let ((parsed (mcp-stdio-decode line)))
              (if (axllm/core::core-value-equal (jget parsed "id") (jget message "id"))
                  (return parsed)
                  ;; A notification or a server request arrived on the same
                  ;; pipe; deliver it and keep waiting for our own response.
                  (mcp-transport-dispatch-inbound transport parsed)))))))))

(defmethod mcp-transport-send-notification ((transport mcp-stdio-transport) message)
  (multiple-value-bind (input output) (%mcp-stdio-streams transport)
    (declare (ignore output))
    (sb-thread:with-mutex ((%stdio-lock transport))
      (write-string (mcp-stdio-encode message) input)
      (finish-output input)))
  nil)

(defmethod mcp-transport-era-hint ((transport mcp-stdio-transport))
  "A stdio server is a local process with a session, never a stateless
modern HTTP endpoint, so there is nothing to probe for."
  "legacy")

(defmethod mcp-transport-close ((transport mcp-stdio-transport))
  (setf (%stdio-closed transport) t)
  (let ((process (mcp-stdio-process transport)))
    (when process
      (ignore-errors (close (uiop:process-info-input process)))
      (ignore-errors (uiop:terminate-process process))
      (ignore-errors (uiop:wait-process process))))
  nil)

(export '(mcp-stdio-transport make-mcp-stdio-transport mcp-stdio-process))
