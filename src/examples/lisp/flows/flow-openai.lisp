;;;; ax-example:start
;;;; title: Common Lisp Flow Of Two Program Nodes
;;;; group: flows
;;;; description: Chains two typed generators into a flow and projects the final state through returns.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; A flow is a graph of nodes over one shared state. Core plans the graph
;;;; from each node's signature, so declaring reads and writes by hand is
;;;; optional: here the second node reads what the first one wrote.

(defpackage #:ax-example/flow-openai
  (:use #:cl))

(in-package #:ax-example/flow-openai)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let ((pipeline (ax:flow (ax:object "id" "outline.flow"))))
  (ax:flow-execute pipeline "outline" (ax:ax "topic:string -> outline:string"))
  (ax:flow-execute pipeline "polish" (ax:ax "outline:string -> answer:string"))
  ;; returns projects the final state: output key -> dotted state path.
  (ax:flow-returns pipeline (ax:object "answer" "answer" "outline" "outlineResult.outline"))
  (format t "~&plan       : ~a~%" (ax:encode-json (ax:flow-plan pipeline)))
  (multiple-value-bind (output usage) (ax:forward pipeline (client)
                                                  (ax:object "topic" "Why condition handlers beat exceptions"))
    (declare (ignore usage))
    (format t "~&outline    : ~a~%" (ax:jget output "outline"))
    (format t "~&answer     : ~a~%" (ax:jget output "answer"))
    (format t "~&usage      : ~a~%" (ax:encode-json (ax:flow-usage pipeline)))))
