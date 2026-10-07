;;;; ax-example:start
;;;; title: Common Lisp GEPA Instruction Search
;;;; group: optimization
;;;; description: Searches one program component with GEPA using a provider-backed reflection callback.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: intermediate
;;;; order: 20
;;;; ax-example:end

;;;; GEPA proposes a replacement for a chosen component, measures it, and keeps
;;;; what scored better. The proposals come from a model, through the
;;;; reflection callback, so the search itself is provider-backed.

(defpackage #:ax-example/gepa-instructions
  (:use #:cl))

(in-package #:ax-example/gepa-instructions)

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
   (ax:object "input" (ax:object "review" "Arrived broken and support never replied.")
              "expectedOutput" (ax:object "sentiment" "negative"))
   (ax:object "input" (ax:object "review" "Works exactly as described, shipped early.")
              "expectedOutput" (ax:object "sentiment" "positive"))
   (ax:object "input" (ax:object "review" "It is fine. Does the job.")
              "expectedOutput" (ax:object "sentiment" "neutral"))))

(defun sentiment-metric (prediction task)
  (let ((expected (ax:jget (ax:jget task "expectedOutput") "sentiment"))
        (actual (ax:jget (ax:jget prediction "output") "sentiment")))
    (if (and (stringp actual) (stringp expected) (string-equal actual expected)) 1 0)))

(let* ((shared (client))
       (program (ax:ax "review:string -> sentiment:class \"positive, neutral, negative\""
                       :id "sentiment"
                       :instruction "Classify the review."))
       (artifact (ax:optimize-program
                  program +tasks+
                  ;; The reflection callback asks the model for a better
                  ;; component; make-ai-reflection-callback wraps a client.
                  :engine (ax:make-gepa :reflection (ax:make-ai-reflection-callback shared)
                                        :seed 7)
                  :client shared
                  :options (ax:object "target" "sentiment::instruction"
                                      "metric" #'sentiment-metric
                                      "maxMetricCalls" 12
                                      "numTrials" 1
                                      "minibatchSize" 2))))
  (format t "~&instruction: ~a~%" (ax:jget (ax:jget artifact "componentMap") "sentiment::instruction"))
  (format t "~&changed    : ~a~%" (ax:encode-json (ax:jget artifact "changedComponents")))
  ;; The artifact serialises, so a tuned program can be rebuilt later.
  (format t "~&artifact   : ~a~%" (ax:optimized-program-json artifact)))
