;;;; ax-example:start
;;;; title: Common Lisp Parallel Flow And Mermaid Output
;;;; group: flows
;;;; description: Runs two independent nodes in one parallel group, then renders the same graph as a Mermaid flowchart.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; Two nodes that read the same field and write different ones share a
;;;; parallel group. Core plans the group; the host dispatches it. The same
;;;; graph renders as Mermaid, and a rendered document parses back.

(defpackage #:ax-example/parallel-flow-mermaid
  (:use #:cl))

(in-package #:ax-example/parallel-flow-mermaid)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let ((review (ax:flow (ax:object "id" "review.flow"))))
  (ax:flow-execute review "risks" (ax:ax "proposal:string -> risks:string")
                   (ax:object "reads" (vector "proposal") "writes" (vector "risksResult")))
  (ax:flow-execute review "benefits" (ax:ax "proposal:string -> benefits:string")
                   (ax:object "reads" (vector "proposal") "writes" (vector "benefitsResult")))
  (ax:flow-execute review "verdict"
                   (ax:ax "risks:string, benefits:string -> verdict:string")
                   (ax:object "reads" (vector "risksResult" "benefitsResult")
                              "writes" (vector "verdictResult")))
  (ax:flow-returns review (ax:object "verdict" "verdictResult.verdict"))

  (let ((plan (ax:flow-plan review)))
    (format t "~&groups     : ~a~%" (ax:jget plan "parallelGroups"))
    (format t "~&parallelism: ~a~%" (ax:jget plan "maxParallelism")))
  ;; The graph as a document, which Ax can also read back.
  (format t "~&mermaid:~%~a~%" (ax:flow-mermaid review))
  ;; The parts an optimizer may rewrite, including the graph plan itself.
  (format t "~&components : ~d~%" (length (ax:flow-components review)))

  (multiple-value-bind (output usage)
      (ax:forward review (client)
                  (ax:object "proposal" "Replace the nightly batch job with a streaming pipeline."))
    (declare (ignore usage))
    (format t "~&verdict    : ~a~%" (ax:jget output "verdict"))))
