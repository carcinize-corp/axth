;;;; ax-example:start
;;;; title: Common Lisp Flow Branching And Map Nodes
;;;; group: flows
;;;; description: Classifies an input, branches on the class, and reshapes the state with native map and derive nodes.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; map and derive nodes run Lisp functions rather than prompts. A callback
;;;; is handed a clone of the state, so it can mutate what it receives without
;;;; reaching the run.

(defpackage #:ax-example/branching-flow
  (:use #:cl))

(in-package #:ax-example/branching-flow)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let ((triage (ax:flow (ax:object "id" "triage.flow"))))
  (ax:flow-execute triage "classify"
                   (ax:ax "report:string -> urgent:class \"yes, no\"")
                   (ax:object "reads" (vector "report") "writes" (vector "classifyResult" "urgent")))
  ;; A data predicate reads a node's result without a callback: nodeName plus
  ;; the field, compared against value.
  (ax:flow-branch triage "route"
                  (ax:object "nodeName" "classify" "field" "urgent" "value" "yes")
                  (vector
                   (ax:object "when" ax:true
                              "steps" (vector (ax:flow-step
                                               "map" "page"
                                               (lambda (state)
                                                 (ax:object "route" "page-on-call"
                                                            "report" (ax:jget state "report"))))))
                   (ax:object "when" ax:false
                              "steps" (vector (ax:flow-step
                                               "map" "queue"
                                               (lambda (state)
                                                 (ax:object "route" "backlog"
                                                            "report" (ax:jget state "report"))))))))
  (ax:flow-derive triage "shout"
                  (lambda (state)
                    (ax:object "__derived" (string-upcase (ax:jget state "__item"))))
                  (ax:object "reads" (vector "route")))
  (ax:flow-returns triage (ax:object "urgent" "urgent" "route" "route" "shout" "shout"))
  (multiple-value-bind (output usage)
      (ax:forward triage (client)
                  (ax:object "report" "Payments have been failing for every customer for ten minutes."))
    (declare (ignore usage))
    (format t "~&urgent     : ~a~%" (ax:jget output "urgent"))
    (format t "~&route      : ~a~%" (ax:jget output "route"))
    (format t "~&shout      : ~a~%" (ax:jget output "shout"))))
