;;;; ax-example:start
;;;; title: Common Lisp Context Map Reused Across Queries
;;;; group: long-agents
;;;; description: Builds a small persistent orientation with contextMap and reuses it over several questions about one large index.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 40
;;;; ax-example:end

;;;; Peek is the contextMap option on an ordinary agent, not a separate API.
;;;; The map is a small persistent orientation over a context field: it is built
;;;; on the first query and reused for the rest, so each later question pays for
;;;; the map rather than re-reading the whole index. maxChars caps it,
;;;; infiniteEvolve and evolveSteps say whether and how often it may be revised.

(defpackage #:ax-example/context-map-reuse
  (:use #:cl))

(in-package #:ax-example/context-map-reuse)

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

(defun module-index ()
  "A PATH / IMPORTS / WRITES record per module, too large for a prompt."
  (with-output-to-string (out)
    (dotimes (i 400)
      (format out "PATH: packages/~a/module~3,'0d.ts~%IMPORTS: ~a~%WRITES: ~a~%~%"
              (if (zerop (mod i 4)) "api/routes" "clients/acquirer")
              i
              (if (zerop (mod i 5)) "packages/clients/acquirer" "packages/core/util")
              (if (zerop (mod i 7)) "orders" "none")))))

(let* ((index (module-index))
       (analyst
         (ax:agent "context:string, question:string -> answer:string, paths:string[] \"Exact PATH values from the index\""
                   :options (ax:object
                             "contextFields" (vector "context")
                             "contextPolicy" (ax:object "preset" "adaptive" "budget" "balanced")
                             "contextOptions"
                             (ax:object "description"
                                        "The context is a module index of PATH / IMPORTS / WRITES records. Answer by filtering those records in code, never by guessing. Return exact PATH values verbatim.")
                             ;; The persistent orientation: small, built once,
                             ;; reused for every later question.
                             "contextMap" (ax:object "maxChars" 1800
                                                     "infiniteEvolve" ax:false
                                                     "evolveSteps" 1)
                             "runtime" (ax:object "language" "JavaScript"))))
       (engine (runtime)))
  (format t "~&index      : ~a characters (kept out of the prompt)~%" (length index))
  (unwind-protect
       (dolist (question '("Which modules import packages/clients/acquirer? Give exact PATH values."
                           "Which modules write to the orders table?"
                           "How many modules are under packages/api/routes?"))
         (let ((result (ax:agent-forward
                        analyst (client)
                        (ax:object "context" index "question" question)
                        :options (ax:object "runtime" engine "maxActorSteps" 24))))
           (format t "~&~%Q: ~a~%A: ~a~%paths: ~a~%"
                   question
                   (ax:jget result "answer")
                   (ax:encode-json (ax:jget result "paths")))))
    (ax:runtime-shutdown engine))
  (format t "~&~%The context map was built on the first query and reused for the rest.~%"))
