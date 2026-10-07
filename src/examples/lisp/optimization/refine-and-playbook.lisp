;;;; ax-example:start
;;;; title: Common Lisp Refinement And An Evolving Playbook
;;;; group: optimization
;;;; description: Samples candidates with a reward threshold, then evolves an ACE playbook from the same labelled set.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; Two different ways to spend more compute for a better answer: refine
;;;; samples a program until a reward threshold is met, and a playbook
;;;; accumulates durable rules from measured runs.

(defpackage #:ax-example/refine-and-playbook
  (:use #:cl))

(in-package #:ax-example/refine-and-playbook)

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
   (ax:object "input" (ax:object "question" "Name the largest ocean.")
              "expectedOutput" (ax:object "answer" "Pacific Ocean"))
   (ax:object "input" (ax:object "question" "Name the longest river in Africa.")
              "expectedOutput" (ax:object "answer" "Nile"))))

(defun contains-expected (prediction task)
  (let ((expected (ax:jget (ax:jget task "expectedOutput") "answer"))
        (actual (ax:jget (ax:jget prediction "output") "answer")))
    (if (and (stringp actual) (stringp expected) (search expected actual :test #'char-equal))
        1 0)))

(let* ((shared (client))
       (program (ax:ax "question:string -> answer:string" :id "qa")))

  ;; Refinement: sample until the reward clears the threshold, then stop.
  (let ((refined (ax:refine program
                            :rounds 2
                            :samples-per-round 2
                            :threshold 1
                            :reward-fn (lambda (output)
                                         (if (and (stringp (ax:jget output "answer"))
                                                  (plusp (length (ax:jget output "answer"))))
                                             1 0)))))
    (multiple-value-bind (output usage) (ax:forward refined shared
                                                    (ax:object "question" "Name the largest ocean."))
      (declare (ignore usage))
      (format t "~&refined    : ~a~%" (ax:jget output "answer")))
    (format t "~&attempts   : ~d~%" (length (ax:program-attempts refined))))

  ;; A playbook: rules the optimizer keeps, rendered into the prompt.
  (let ((playbook (ax:make-playbook :program program
                                    :teacher shared
                                    :metric #'contains-expected)))
    (ax:playbook-evolve playbook +tasks+ :metric #'contains-expected)
    (format t "~&playbook   : ~a~%" (ax:playbook-json playbook))))
