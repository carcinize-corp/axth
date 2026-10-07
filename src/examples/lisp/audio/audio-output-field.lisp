;;;; ax-example:start
;;;; title: Common Lisp Audio Output Field
;;;; group: audio
;;;; description: Declares an audio output field on a signature and renders it to speech during forward.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; An audio output field is part of the signature, not a separate call. The
;;;; model produces the text; the renderAudio option tells the generator to
;;;; synthesise it through the service's ax-speak. Without renderAudio the same
;;;; program returns the field unrendered, which is the honest default: a
;;;; silent artifact must not look like speech that was produced.

(defpackage #:ax-example/audio-output-field
  (:use #:cl))

(in-package #:ax-example/audio-output-field)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(let ((announcer (ax:ax "headline:string -> speech:audio"))
      (service (client)))
  (format t "~&signature  : ~a~%" (ax:encode-json (ax:program-signature announcer)))

  ;; Unrendered: the field is produced but no speech is synthesised.
  (multiple-value-bind (quiet usage)
      (ax:forward announcer service (ax:object "headline" "The build is green"))
    (declare (ignore usage))
    (format t "~&unrendered : ~a~%" (ax:encode-json (ax:jget quiet "speech"))))

  ;; Rendered: the generator calls ax-speak for the audio field.
  (multiple-value-bind (loud usage)
      (ax:forward announcer service
                  (ax:object "headline" "The build is green")
                  (ax:object "renderAudio" ax:true))
    (let ((speech (ax:jget loud "speech")))
      (format t "~&transcript : ~a~%" (ax:jget speech "transcript" :null))
      (format t "~&format     : ~a~%" (ax:jget speech "format" :null))
      (format t "~&usage      : ~a~%" (ax:encode-json usage)))))
