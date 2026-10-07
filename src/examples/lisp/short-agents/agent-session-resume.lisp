;;;; ax-example:start
;;;; title: Common Lisp Agent Pause And Resume
;;;; group: short-agents
;;;; description: Exports an agent's runtime session after one run, restores it into a second run, and replays the trace.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; An agent's runtime session is state a caller can carry: export it to
;;;; pause, restore it to resume. The trace is separate and replayable, so a
;;;; finished run can be examined without the provider.

(defpackage #:ax-example/agent-session-resume
  (:use #:cl))

(in-package #:ax-example/agent-session-resume)

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
       (let ((first (ax:agent-forward assistant (client)
                                      (ax:object "question" "Remember the number 41. Reply with it.")
                                      :options (ax:object "runtime" engine))))
         (format t "~&first      : ~a~%" (ax:jget first "answer"))
         ;; Pause: the session's globals, as a JSON snapshot.
         (let ((snapshot (ax:agent-export-session-state assistant))
               (trace (ax:agent-trace assistant)))
           (format t "~&snapshot   : ~a~%" (ax:encode-json snapshot))
           ;; Resume: the same state, back in the session.
           (ax:agent-restore-session-state assistant snapshot)
           (let ((second (ax:agent-forward assistant (client)
                                           (ax:object "question" "Add one to the number you were given.")
                                           :options (ax:object "runtime" engine))))
             (format t "~&second     : ~a~%" (ax:jget second "answer")))
           ;; Replay needs no provider: it reads the recorded events.
           (format t "~&replayed   : ~a~%"
                   (ax:encode-json (ax:agent-replay-trace assistant trace)))))
    (ax:runtime-shutdown engine)))
