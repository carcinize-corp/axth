;;;; ax-example:start
;;;; title: Common Lisp Agent With A JavaScript Actor Runtime
;;;; group: short-agents
;;;; description: Runs an agent whose actor writes code in a worker runtime, then reports its action log and usage.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; An Ax agent is three stages over one signature. The actor stage writes
;;;; code and the runtime executes it, so the model decides what to do while
;;;; the host keeps the execution boundary.

(defpackage #:ax-example/agent-openai
  (:use #:cl))

(in-package #:ax-example/agent-openai)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(defun runtime ()
  "A JavaScript actor runtime, run as a worker process.

The repository ships the worker the other ports use; the example runner
exports AXIR_AXJS_RUNTIME_SERVER and AXIR_REPO_ROOT, as it does for the
Java, Go and Rust agent examples."
  (let ((server (uiop:getenv "AXIR_AXJS_RUNTIME_SERVER"))
        (root (uiop:getenv "AXIR_REPO_ROOT")))
    (unless (and server root)
      (error "AXIR_AXJS_RUNTIME_SERVER and AXIR_REPO_ROOT are required; run this through `npm run example -- lisp <path>`."))
    (ax:make-process-runtime (list "node" "--import=tsx" server) :cwd root)))

(let ((assistant (ax:agent "question:string -> answer:string"
                           :options (ax:object "runtime" (ax:object "language" "JavaScript"))))
      (engine (runtime)))
  (unwind-protect
       (let ((output (ax:agent-forward assistant (client)
                                       (ax:object "question" "What is 17 * 23? Show the product only.")
                                       :options (ax:object "runtime" engine))))
         (format t "~&answer     : ~a~%" (ax:jget output "answer"))
         ;; What the actor actually did, step by step.
         (format t "~&actions    : ~d~%" (length (ax:agent-action-log assistant)))
         (format t "~&usage      : ~a~%" (ax:encode-json (ax:agent-usage assistant))))
    (ax:runtime-shutdown engine)))
