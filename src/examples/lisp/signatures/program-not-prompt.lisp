;;;; ax-example:start
;;;; title: Common Lisp Programs Instead Of Prompts
;;;; group: signatures
;;;; description: Measures a program on a labelled set, lets an optimizer rewrite its instruction, and measures again.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; The DSPy idea in one file: the unit of work is a program with a signature
;;;; and a metric, not a prompt string. The instruction is a component the
;;;; optimizer may rewrite, and the measurement is what decides whether it did
;;;; better.

(defpackage #:ax-example/program-not-prompt
  (:use #:cl))

(in-package #:ax-example/program-not-prompt)

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
   (ax:object "input" (ax:object "sentence" "The flight was delayed four hours and nobody told us.")
              "expectedOutput" (ax:object "tone" "negative"))
   (ax:object "input" (ax:object "sentence" "Crew were kind and we landed early.")
              "expectedOutput" (ax:object "tone" "positive"))
   (ax:object "input" (ax:object "sentence" "We departed on time.")
              "expectedOutput" (ax:object "tone" "neutral"))))

(defun tone-metric (prediction task)
  (let ((expected (ax:jget (ax:jget task "expectedOutput") "tone"))
        (actual (ax:jget (ax:jget prediction "output") "tone")))
    (if (and (stringp actual) (stringp expected) (string-equal actual expected)) 1 0)))

(defun score (program shared)
  "Measure PROGRAM on every task and answer with the mean score."
  (let ((total 0))
    (loop for task across +tasks+
          do (multiple-value-bind (output usage)
                 (ax:forward program shared (ax:jget task "input"))
               (declare (ignore usage))
               (incf total (tone-metric (ax:object "output" output) task))))
    (/ (float total) (length +tasks+))))

(let* ((shared (client))
       (program (ax:ax "sentence:string -> tone:class \"positive, neutral, negative\""
                       :id "tone"
                       :instruction "Label the tone.")))
  (format t "~&before     : ~a~%" (ax:program-optimizable-components program))
  (format t "~&score      : ~,2f~%" (score program shared))
  (let ((artifact (ax:optimize-program
                   program +tasks+
                   :engine (ax:make-gepa :reflection (ax:make-ai-reflection-callback shared) :seed 3)
                   :client shared
                   :options (ax:object "target" "tone::instruction"
                                       "metric" #'tone-metric
                                       "maxMetricCalls" 12
                                       "numTrials" 1
                                       "minibatchSize" 2))))
    (format t "~&rewritten  : ~a~%" (ax:jget (ax:jget artifact "componentMap") "tone::instruction"))
    (format t "~&score      : ~,2f~%" (score program shared))))
