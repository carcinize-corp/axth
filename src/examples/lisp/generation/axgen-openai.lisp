;;;; ax-example:start
;;;; title: Common Lisp Typed Generation
;;;; group: generation
;;;; description: Runs one typed signature against OpenAI and prints the parsed output and token usage.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end
;;;;
;;;; The smallest complete Ax program in Common Lisp: a client, a signature,
;;;; and one call. FORWARD returns the parsed outputs and this call's usage as
;;;; two values, so nothing has to be fished back out of a log.

(defpackage #:ax-example/axgen-openai
  (:use #:cl))

(in-package #:ax-example/axgen-openai)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun model ()
  (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini"))

(let ((client (ax:ai :name "openai" :model (model) :api-key (api-key)))
      (program (ax:ax "question:string -> answer:string, confidence:class \"high, medium, low\"")))
  (multiple-value-bind (output usage)
      (ax:forward program client (ax:object "question" "What is the capital of France?"))
    (format t "~&answer     : ~a~%" (ax:jget output "answer"))
    (format t "~&confidence : ~a~%" (ax:jget output "confidence"))
    ;; A usage object is JSON like every other Ax value, so it prints with the
    ;; same encoder the rest of the port uses.
    (format t "~&usage      : ~a~%" (ax:encode-json usage))))
