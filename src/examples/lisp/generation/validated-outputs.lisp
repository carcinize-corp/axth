;;;; ax-example:start
;;;; title: Common Lisp Validated Output Recovery
;;;; group: generation
;;;; description: Builds a constrained signature, shows its JSON Schema, and reports the individual problems when validation fails.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end
;;;;
;;;; Ax validates a model's output against the signature and spends bounded
;;;; correction turns on the problems it found. When it runs out, the condition
;;;; carries the problem list rather than one flattened message, so a caller
;;;; can decide per problem instead of re-reading prose.

(defpackage #:ax-example/validated-outputs
  (:use #:cl))

(in-package #:ax-example/validated-outputs)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defparameter +signature+
  (concatenate 'string
               "report:string \"The text to triage\" "
               "-> severity:class \"blocker, major, minor\", "
               "summary:string, "
               "tags:string[]")
  "A signature with a closed class field and an array field: both are
constraints Ax can check, so both can be corrected.")

(let* ((client (ax:ai :name "openai"
                      :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
                      :api-key (api-key)))
       (signature (ax:parse-signature +signature+))
       ;; Two correction turns: enough to recover a mislabelled class, few
       ;; enough that a model that cannot answer fails quickly.
       (program (ax:ax +signature+ :max-retries 2)))
  (format t "~&signature  : ~a~%" (ax:signature-string signature))
  ;; The same schema Ax sends to a structured-output provider.
  (format t "~&out schema : ~a~%"
          (ax:encode-json (ax:json-schema signature :side :output :title "Triage")))
  (handler-case
      (multiple-value-bind (output usage)
          (ax:forward program client
                      (ax:object "report" "The checkout page returns 500 for every card payment."))
        (declare (ignore usage))
        (format t "~&severity   : ~a~%" (ax:jget output "severity"))
        (format t "~&summary    : ~a~%" (ax:jget output "summary"))
        (format t "~&tags       : ~a~%" (ax:encode-json (ax:jget output "tags"))))
    (ax:generation-error (condition)
      (format t "~&generation failed (~a)~%" (ax:generation-error-kind condition))
      (dolist (problem (ax:generation-error-problems condition))
        (format t "~&  - ~a~%" problem))
      ;; A failed run is still a run: the chat log shows what was attempted.
      (format t "~&turns      : ~d~%" (length (ax:program-chat-log program))))))
