;;;; ax-example:start
;;;; title: Common Lisp Speech Synthesis And Transcription
;;;; group: audio
;;;; description: Synthesises speech with ax-speak, then reads the same audio back with ax-transcribe.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; The two audio boundaries are plain generics on the service: ax-speak takes
;;;; a request object and answers Core's normalized speak response, ax-transcribe
;;;; does the reverse. Nothing here goes through a generator, so this is the
;;;; smallest thing that proves the audio path end to end.

(defpackage #:ax-example/speak-and-transcribe
  (:use #:cl))

(in-package #:ax-example/speak-and-transcribe)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client (model)
  (ax:ai :name "openai" :model model :api-key (api-key)))

(let* ((line "Condition handlers can resume; exceptions cannot.")
       (speaker (client (or (uiop:getenv "AX_OPENAI_SPEAK_MODEL") "gpt-4o-mini-tts")))
       ;; A speak request is text plus the wire format you want back. Core
       ;; builds the provider-specific payload from it.
       (spoken (ax:ax-speak speaker (ax:object "text" line
                                               "voice" "alloy"
                                               "format" "mp3"))))
  (format t "~&spoken format : ~a~%" (ax:jget spoken "format"))
  (format t "~&spoken mime   : ~a~%" (ax:jget spoken "mimeType"))
  (let ((data (ax:jget spoken "data" :null)))
    (format t "~&spoken bytes  : ~a (base64 characters)~%"
            (if (stringp data) (length data) data)))

  ;; Transcription takes the audio back. The request carries the audio object
  ;; the speak call produced, so no file handling is needed to show the round
  ;; trip.
  (let* ((listener (client (or (uiop:getenv "AX_OPENAI_TRANSCRIBE_MODEL")
                               "gpt-4o-mini-transcribe")))
         (heard (ax:ax-transcribe listener
                                  (ax:object "audio" (ax:object "data" (ax:jget spoken "data")
                                                                "format" (ax:jget spoken "format"))))))
    (format t "~&said          : ~a~%" line)
    (format t "~&heard         : ~a~%" (ax:jget heard "text"))
    (format t "~&language      : ~a~%" (ax:jget heard "language" :null))))
