;;;; ax-example:start
;;;; title: Common Lisp Capstone Research Desk
;;;; group: long-agents
;;;; description: Composes a flow of typed nodes with an agent node, an optimizer pass, and the rendered graph.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; The capstone: one program built from the other units. A flow plans the
;;;; graph, a node is itself an agent with a code runtime, an optimizer rewrites
;;;; one component against a metric, and the whole thing renders as a document.

(defpackage #:ax-example/capstone-research-desk
  (:use #:cl))

(in-package #:ax-example/capstone-research-desk)

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

(defparameter +tasks+
  (vector
   (ax:object "input" (ax:object "brief" "Should we cache provider responses?")
              "expectedOutput" (ax:object "verdict" "yes"))
   (ax:object "input" (ax:object "brief" "Should we log full prompts in production?")
              "expectedOutput" (ax:object "verdict" "no"))))

(defun verdict-metric (prediction task)
  (let ((expected (ax:jget (ax:jget task "expectedOutput") "verdict"))
        (actual (ax:jget (ax:jget prediction "output") "verdict")))
    (if (and (stringp actual) (stringp expected) (string-equal actual expected)) 1 0)))

(let* ((shared (client))
       (engine (runtime))
       ;; One node is an agent: it writes code in the runtime to do its work.
       (analyst (ax:agent "brief:string -> findings:string"
                          :options (ax:object "runtime" (ax:object "language" "JavaScript"))))
       (desk (ax:flow (ax:object "id" "desk.flow"))))
  (unwind-protect
       (progn
         (ax:flow-execute desk "analyst" analyst
                          (ax:object "reads" (vector "brief") "writes" (vector "analystResult")))
         (ax:flow-execute desk "risks" (ax:ax "brief:string -> risks:string")
                          (ax:object "reads" (vector "brief") "writes" (vector "risksResult")))
         (ax:flow-execute desk "decide"
                          (ax:ax "findings:string, risks:string -> verdict:class \"yes, no\", because:string"
                                 :id "decide"
                                 :instruction "Decide from the findings and risks.")
                          (ax:object "reads" (vector "analystResult" "risksResult")
                                     "writes" (vector "decideResult")))
         (ax:flow-returns desk (ax:object "verdict" "decideResult.verdict"
                                          "because" "decideResult.because"))
         (format t "~&graph:~%~a~%" (ax:flow-mermaid desk))
         (format t "~&components : ~d~%" (length (ax:flow-components desk)))

         (multiple-value-bind (output usage)
             (ax:forward desk shared
                         (ax:object "brief" "Should we cache provider responses?")
                         (ax:object "runtime" engine))
           (declare (ignore usage))
           (format t "~&verdict    : ~a~%" (ax:jget output "verdict"))
           (format t "~&because    : ~a~%" (ax:jget output "because")))

         ;; The flow is a program, so the optimizer treats it like any other.
         (let ((artifact (ax:optimize-program
                          desk +tasks+
                          :engine (ax:make-bootstrap-few-shot)
                          :client shared
                          :options (ax:object "metric" #'verdict-metric
                                              "maxMetricCalls" 4
                                              "runtime" engine))))
           (format t "~&kind       : ~a~%" (ax:program-kind desk))
           (format t "~&changed    : ~a~%" (ax:encode-json (ax:jget artifact "changedComponents")))))
    (ax:runtime-shutdown engine)))
