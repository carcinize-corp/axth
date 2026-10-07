;;;; ax-example:start
;;;; title: Common Lisp Context Pressure Metrics
;;;; group: long-agents
;;;; description: Observes an agent's context events with a metrics collector and reports the pressure summary.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; A long-running agent's real constraint is context. The collector observes
;;;; the context events a run emits and answers with a summary, so pressure is
;;;; measured rather than guessed at from output length.

(defpackage #:ax-example/context-metrics
  (:use #:cl))

(in-package #:ax-example/context-metrics)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(defun runtime ()
  (let ((server (uiop:getenv "AXIR_AXJS_RUNTIME_SERVER"))
        (root (uiop:getenv "AXIR_REPO_ROOT")))
    (unless (and server root)
      (error "AXIR_AXJS_RUNTIME_SERVER and AXIR_REPO_ROOT are required; run this through `npm run example -- lisp <path>`."))
    (ax:make-process-runtime (list "node" "--import=tsx" server) :cwd root)))

(let* ((collector (ax:make-context-metrics-collector))
       (assistant (ax:agent "question:string -> answer:string"
                            :options (ax:object "runtime" (ax:object "language" "JavaScript"))))
       (engine (runtime)))
  (unwind-protect
       (let ((output (ax:agent-forward
                      assistant (client)
                      (ax:object "question" "List three uses for a condition system, briefly.")
                      ;; The handler receives each context event as it happens.
                      :options (ax:object "runtime" engine
                                          "contextMetricsHandler"
                                          (ax:context-metrics-handler collector)))))
         (format t "~&answer     : ~a~%" (ax:jget output "answer"))
         ;; Feeding the run's usage in lets the summary report token pressure.
         (format t "~&pressure   : ~a~%"
                 (ax:encode-json (ax:context-metrics-summary collector (ax:agent-usage assistant)))))
    (ax:runtime-shutdown engine)))
