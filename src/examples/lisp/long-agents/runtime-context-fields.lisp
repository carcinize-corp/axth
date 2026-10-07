;;;; ax-example:start
;;;; title: Common Lisp Large Input Kept In The Runtime
;;;; group: long-agents
;;;; description: Declares a bulk input as a context field so the agent filters it in code instead of putting it in the prompt.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; There is no separate "RLM" API: this is an ordinary agent with three
;;;; options. contextFields names the inputs that stay in the code runtime
;;;; rather than the prompt, contextPolicy says how aggressively to summarise
;;;; what does reach the model, and maxRuntimeChars caps how much the actor may
;;;; pull across in one turn. The actor filters the bulk input in code and only
;;;; the evidence it extracts is ever seen by the model.

(defpackage #:ax-example/runtime-context-fields
  (:use #:cl))

(in-package #:ax-example/runtime-context-fields)

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

(defun log-dump ()
  "A synthetic export far too large to put in a prompt."
  (let ((events (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (i 1800)
      (vector-push-extend
       (ax:object "ts" (+ 1760000000 (* i 7))
                  "level" (cond ((zerop (mod i 97)) "ERROR")
                                ((zerop (mod i 13)) "WARN")
                                (t "INFO"))
                  "service" (if (zerop (mod i 3)) "search-api" "catalog-cron")
                  "statusCode" (if (zerop (mod i 97)) 429 200)
                  "message" (if (zerop (mod i 97))
                                "rate limited: downstream catalog unavailable"
                                "request served"))
       events))
    events))

(let* ((logs (log-dump))
       (forensics
         (ax:agent "task:string, logs:json \"Raw export; keep this out of the prompt\" -> findings:json[] \"Each: issue, count, evidence\", overallHealth:string"
                   :options (ax:object
                             ;; logs never enters the prompt: it is bound in the
                             ;; runtime and the actor queries it in code.
                             "contextFields" (vector "logs")
                             "contextPolicy" (ax:object "preset" "lean" "budget" "balanced")
                             "maxRuntimeChars" 12000
                             "runtime" (ax:object "language" "JavaScript"))))
       (engine (runtime)))
  (format t "~&events     : ~a (kept out of the prompt)~%" (length logs))
  (unwind-protect
       (let ((report (ax:agent-forward
                      forensics (client)
                      (ax:object "logs" logs
                                 "task" "Find the repeated errors and throttles, with an occurrence count and concrete log evidence for each.")
                      :options (ax:object "runtime" engine "maxActorSteps" 40))))
         (format t "~&health     : ~a~%" (ax:jget report "overallHealth"))
         (format t "~&findings   : ~a~%" (ax:encode-json (ax:jget report "findings")))
         (format t "~&usage      : ~a~%" (ax:encode-json (ax:agent-usage forensics))))
    (ax:runtime-shutdown engine)))
