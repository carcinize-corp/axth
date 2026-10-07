;;;; ax-example:start
;;;; title: Common Lisp Event Notifications
;;;; group: providers
;;;; description: Wakes a typed program from an event envelope and writes its output to a sink through the event runtime.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; Notifications arrive as events, not calls. A route matches an envelope, a
;;;; target invokes a program, and a sink receives the output; the runtime owns
;;;; ordering, retries and dead letters.

(defpackage #:ax-example/event-notifications
  (:use #:cl))

(in-package #:ax-example/event-notifications)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let* ((shared (client))
       (triage (ax:ax "subject:string, body:string -> reply:string"
                      :instruction "Draft a one-sentence reply."))
       (delivered '())
       (sink (ax:make-event-sink
              :id "outbox"
              :write (lambda (output context)
                       (push (ax:jget output "reply") delivered)
                       (format t "~&sink       : run ~a~%" (ax:jget context "runId")))))
       (target (ax:event-target
                "triage"
                ;; A declarative mapping needs a signature, so the envelope is
                ;; validated into the program's inputs instead of trusted.
                :signature "subject:string, body:string -> reply:string"
                :input (ax:event-input-plan
                        :fields (ax:object "subject" (ax:event-path-data "subject")
                                           "body" (ax:event-path-data "body")))
                :sinks (list sink)
                :retry-safety "safe"
                :invoke (lambda (input context)
                          (declare (ignore context))
                          (ax:forward triage shared input))))
       (runtime (ax:make-event-runtime
                 (list (ax:event-route "inbound"
                                       :action "wake"
                                       :types (list "mail.received")
                                       :target target))
                 :targets (list target))))
  (unwind-protect
       (progn
         (ax:event-runtime-start runtime)
         (ax:event-runtime-publish
          runtime
          (ax:make-event-envelope "evt-1" "/mail" "mail.received"
                                  :data (ax:object "subject" "Invoice query"
                                                   "body" "Which PO covers invoice 4471?")))
         (format t "~&dispatched : ~d delivery(s)~%" (ax:event-runtime-run-due runtime))
         (format t "~&replies    : ~a~%" (ax:encode-json (coerce (reverse delivered) 'vector)))
         (format t "~&dead       : ~d~%" (length (ax:event-runtime-list-dead-letters runtime))))
    (ax:event-runtime-close runtime)))
