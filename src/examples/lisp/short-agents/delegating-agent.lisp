;;;; ax-example:start
;;;; title: Common Lisp Agent Delegation
;;;; group: short-agents
;;;; description: Gives a parent agent a namespaced child agent and shows the callable inventory the actor can reach.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; A child agent is a namespaced callable the parent's actor may invoke.
;;;; The inventory is what the actor is told it can call, so it is the right
;;;; thing to inspect when a delegation does not happen.

(defpackage #:ax-example/delegating-agent
  (:use #:cl))

(in-package #:ax-example/delegating-agent)

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

(let* ((researcher (ax:agent "topic:string -> notes:string"))
       (writer (ax:agent "question:string -> answer:string"
                         :options (ax:object "runtime" (ax:object "language" "JavaScript"))))
       (engine (runtime)))
  ;; namespace and name together form the callable the actor sees.
  (ax:agent-add-child writer "research" "notes" researcher)
  (format t "~&callables  : ~a~%" (ax:encode-json (ax:agent-callable-inventory writer)))
  (unwind-protect
       (let ((output (ax:agent-forward
                      writer (client)
                      (ax:object "question" "Summarise why Common Lisp has a condition system, in one sentence.")
                      :options (ax:object "runtime" engine))))
         (format t "~&answer     : ~a~%" (ax:jget output "answer"))
         (format t "~&actions    : ~d~%" (length (ax:agent-action-log writer))))
    (ax:runtime-shutdown engine)))
