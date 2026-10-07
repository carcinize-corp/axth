;;;; ax-example:start
;;;; title: Common Lisp Fluent Signatures And Schemas
;;;; group: signatures
;;;; description: Builds one signature with the fluent field API, renders it, derives its JSON Schema, and runs it.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; A signature is the program's type. It can be written as text or built
;;;; field by field; both produce the same record, so the schema a provider
;;;; sees is derived from the same place the prompt is.

(defpackage #:ax-example/fluent-signatures
  (:use #:cl))

(in-package #:ax-example/fluent-signatures)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let* ((signature
         (ax:signature-from-spec
          (ax:s :description "Extract a structured contact from free text."
                :inputs (ax:object
                         "note" (ax:f "string" :description "The raw note to read"))
                :outputs (ax:object
                          "name" (ax:f "string")
                          "email" (ax:f "string" :email ax:true)
                          "priority" (ax:f "class" :options (vector "high" "normal" "low"))
                          "topics" (ax:f "string" :array ax:true
                                                  :description "Each subject mentioned")))))
       (program (ax:ax signature)))
  ;; The same signature, as text and as a schema.
  (format t "~&signature  : ~a~%" (ax:signature-string signature))
  (format t "~&inputs     : ~d field(s)~%" (length (ax:signature-fields signature :side :input)))
  (format t "~&out schema : ~a~%"
          (ax:encode-json (ax:json-schema signature :side :output :title "Contact")))
  (multiple-value-bind (output usage)
      (ax:forward program (client)
                  (ax:object "note" "Ada Lovelace (ada@example.com) needs the billing export today."))
    (declare (ignore usage))
    (format t "~&name       : ~a~%" (ax:jget output "name"))
    (format t "~&email      : ~a~%" (ax:jget output "email"))
    (format t "~&priority   : ~a~%" (ax:jget output "priority"))
    (format t "~&topics     : ~a~%" (ax:encode-json (ax:jget output "topics")))))
