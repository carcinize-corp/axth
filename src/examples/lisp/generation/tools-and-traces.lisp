;;;; ax-example:start
;;;; title: Common Lisp Tool Calls And Traces
;;;; group: generation
;;;; description: Gives a generator a native tool, then reads back the tool calls and the chat log it recorded.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end
;;;;
;;;; A tool is a JSON request spec plus a Lisp handler. Ax validates the
;;;; model's arguments against the spec before the handler sees them, so a
;;;; handler never has to defend itself against a malformed call.

(defpackage #:ax-example/tools-and-traces
  (:use #:cl))

(in-package #:ax-example/tools-and-traces)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defparameter +stock+
  (ax:object "widget" 42 "sprocket" 7)
  "A tiny inventory the tool answers from, so the example needs no network.")

(defun lookup-stock (arguments)
  "Answer with the stock level for ARGUMENTS' item, as the tool's handler."
  (let* ((item (ax:jget arguments "item"))
         (count (ax:jget +stock+ item)))
    (if (eq count :null)
        (format nil "~a is not a stocked item" item)
        (format nil "~a: ~d in stock" item count))))

(let* ((client (ax:ai :name "openai"
                      :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
                      :api-key (api-key)))
       (stock-tool (ax:tool :name "lookup_stock"
                            :description "Current stock level for one inventory item."
                            :parameters (ax:object
                                         "type" "object"
                                         "properties" (ax:object
                                                       "item" (ax:object
                                                               "type" "string"
                                                               "description" "The item to look up."))
                                         "required" (vector "item"))
                            :handler #'lookup-stock))
       (program (ax:ax "question:string -> answer:string"
                       :tools (list stock-tool)
                       :description "Answer inventory questions using the lookup_stock tool.")))
  (multiple-value-bind (output usage) (ax:forward program client
                                                  (ax:object "question" "How many sprockets are left?"))
    (declare (ignore usage))
    (format t "~&answer     : ~a~%" (ax:jget output "answer")))
  ;; Both records are the program's own, so a caller can audit a run without
  ;; wrapping the provider.
  (format t "~&tool calls : ~a~%" (ax:encode-json (ax:generator-function-call-traces program)))
  (format t "~&turns      : ~d~%" (length (ax:program-chat-log program))))
