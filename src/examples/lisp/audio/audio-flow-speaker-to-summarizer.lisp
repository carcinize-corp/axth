;;;; ax-example:start
;;;; title: Common Lisp Audio Flow From Speaker To Summarizer
;;;; group: audio
;;;; description: Passes a rendered audio field from one flow node into a second node that consumes it.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; Audio is an ordinary field type, so it travels through flow state like any
;;;; other. The first node writes speech:audio, the second declares speech:audio
;;;; as an input, and Core's planner connects them from the signatures alone.
;;;; renderAudio applies to the whole run, so the speaker's field is synthesised
;;;; before the summarizer reads it.

(defpackage #:ax-example/audio-flow-speaker-to-summarizer
  (:use #:cl))

(in-package #:ax-example/audio-flow-speaker-to-summarizer)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let ((pipeline (ax:flow (ax:object "id" "narration.flow" "autoParallel" ax:false))))
  (ax:flow-execute pipeline "speaker" (ax:ax "question:string -> speech:audio"))
  (ax:flow-execute pipeline "summarizer" (ax:ax "speech:audio -> summary:string"))
  (ax:flow-returns pipeline (ax:object "summary" "summary"))
  (format t "~&plan      : ~a~%" (ax:encode-json (ax:flow-plan pipeline)))
  (format t "~&mermaid   :~%~a~%" (ax:flow-mermaid pipeline))

  (multiple-value-bind (output usage)
      (ax:forward pipeline (client)
                  (ax:object "question" "Say hello to the release channel")
                  (ax:object "renderAudio" ax:true))
    (format t "~&summary   : ~a~%" (ax:jget output "summary"))
    (format t "~&usage     : ~a~%" (ax:encode-json usage))))
