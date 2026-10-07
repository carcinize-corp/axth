;;;; ax-example:start
;;;; title: Common Lisp Bootstrap Few-Shot Optimization
;;;; group: optimization
;;;; description: Mines demonstrations from a labelled set with BootstrapFewShot and applies the artifact to the program.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: beginner
;;;; order: 10
;;;; ax-example:end

;;;; An optimizer measures a program on a dataset, proposes a change, and
;;;; returns a normalized artifact. The driver applies it by default, and the
;;;; artifact says exactly what changed.

(defpackage #:ax-example/bootstrap-few-shot
  (:use #:cl))

(in-package #:ax-example/bootstrap-few-shot)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(defparameter +tasks+
  (vector
   (ax:object "input" (ax:object "question" "2 + 2") "expectedOutput" (ax:object "answer" "4"))
   (ax:object "input" (ax:object "question" "7 * 6") "expectedOutput" (ax:object "answer" "42"))
   (ax:object "input" (ax:object "question" "90 / 9") "expectedOutput" (ax:object "answer" "10"))
   (ax:object "input" (ax:object "question" "12 - 5") "expectedOutput" (ax:object "answer" "7")))
  "A labelled set. Each task is an input and the output it should produce.")

(defun exact-answer (prediction task)
  "Score one rollout: 1 when the answer matches the label, else 0."
  (let ((expected (ax:jget (ax:jget task "expectedOutput") "answer"))
        (actual (ax:jget (ax:jget prediction "output") "answer")))
    (if (and (stringp actual) (stringp expected)
             (string= (string-trim " ." actual) expected))
        1 0)))

(let* ((program (ax:ax "question:string -> answer:string"
                       :id "qa"
                       :instruction "Answer with the number alone."))
       (artifact (ax:optimize-program
                  program +tasks+
                  :engine (ax:make-bootstrap-few-shot)
                  :client (client)
                  :options (ax:object "metric" #'exact-answer
                                      "maxMetricCalls" 8))))
  (format t "~&artifact   : ~a~%" (ax:jget artifact "artifactVersion"))
  (format t "~&changed    : ~a~%" (ax:encode-json (ax:jget artifact "changedComponents")))
  (format t "~&components : ~a~%" (ax:encode-json (ax:program-optimizable-components program))))
