;;;; evaluate.lisp --- test-prompt evaluation over labelled examples.
;;;;
;;;; Port of src/ax/dsp/evaluate.ts (AxTestPrompt). A test prompt runs a
;;;; program over every example with one retry allowed, scores each result
;;;; with a metric function, and averages the scores. An example whose
;;;; forward or metric fails is reported and contributes zero rather than
;;;; aborting the run.
;;;;
;;;; The TypeScript `run` discards the average, so RUN-TEST-PROMPT returns
;;;; no values: the observable results are the forward calls, the warnings,
;;;; and the debug line. Load after src/refine.lisp, which creates the
;;;; frozen program generic functions when gen.lisp has not yet done so.

(in-package #:axllm)

(defclass test-prompt ()
  ((client :initarg :client :reader test-prompt-client)
   (program :initarg :program :reader test-prompt-program)
   (examples :initarg :examples :reader test-prompt-examples)
   (debug :initarg :debug :initform nil :reader test-prompt-debug))
  (:documentation "A program, a client, and the examples to score it on."))

(defmethod print-object ((test test-prompt) stream)
  (print-unreadable-object (test stream :type t)
    (format stream "~a example(s)" (length (test-prompt-examples test)))))

(defun test-prompt (&key client program examples debug)
  "A test prompt scoring PROGRAM on EXAMPLES against CLIENT.

EXAMPLES is a non-empty list or JSON array of JSON objects; each one holds
both the input fields the program needs and whatever the metric function
compares against. DEBUG prints the summed and average score after the run,
standing in for the TypeScript ai.getOptions().debug flag.

Signals AX-ERROR when EXAMPLES is empty, as the TypeScript constructor
does."
  (let ((list (cond ((null examples) '())
                    ((%array-p examples) (coerce examples 'list))
                    ((listp examples) examples)
                    (t (error 'ax-error
                              :message (format nil "test-prompt: :examples must be a list or JSON array, got ~S"
                                               examples))))))
    (when (null list)
      (error 'ax-error :message "No examples found"))
    (make-instance 'test-prompt
                   :client client
                   :program program
                   :examples list
                   :debug debug)))

(defun run-test-prompt (test metric-fn)
  "Score TEST's program on every example with METRIC-FN.

METRIC-FN is called with one JSON object holding prediction and example,
and must return a number. Every example is forwarded with maxRetries 1. An
example that signals, in the program or in the metric, is warned about and
scores zero; the run continues. Returns no values, like the TypeScript."
  (unless (functionp metric-fn)
    (error 'ax-error :message "run-test-prompt: metric function must be a function"))
  (let ((total (length (test-prompt-examples test)))
        (sum 0)
        (index -1))
    (dolist (example (test-prompt-examples test))
      (incf index)
      (handler-case
          (multiple-value-bind (prediction usage)
              (forward (test-prompt-program test) (test-prompt-client test) example
                       (object "maxRetries" 1))
            (declare (ignore usage))
            (let ((score (funcall metric-fn (object "prediction" prediction "example" example))))
              (unless (realp score)
                (error 'ax-error
                       :message (format nil "run-test-prompt: metric function must return a number, got ~S"
                                        score)))
              (incf sum score)))
        (error (condition)
          ;; Keep going: this example scores zero, exactly as in the
          ;; TypeScript, where the catch leaves sumOfScores untouched.
          (warn "Program evaluation failed for example ~a: ~a" index condition))))
    (when (test-prompt-debug test)
      (format t "~%Performance:  ~a / ~a Average Score:  ~a~%~%"
              sum total (if (plusp total) (coerce (/ sum total) 'double-float) 0)))
    (values)))
