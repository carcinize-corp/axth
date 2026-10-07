;;;; refine.lisp --- tests for reward-scored selection and evaluation.
;;;;
;;;; Entry point for the repository runner:
;;;;
;;;;   (axllm:run-refine-tests)          ; => (values passed failed)
;;;;
;;;; Covers src/refine.lisp and src/evaluate.lisp. Every program here is a
;;;; scripted one: it implements the frozen program interface, records the
;;;; client, inputs and forward options it was given, and returns canned
;;;; outputs, usage or errors. No provider is contacted and no credential
;;;; is needed.
;;;;
;;;; The assertions are derived from the TypeScript behavior in
;;;; src/ax/dsp/refine.ts and src/ax/dsp/evaluate.ts, and are deliberately
;;;; asymmetric: each one fails for an implementation that merely has the
;;;; right functions. Among others, a threshold hit must win over a
;;;; strictly better reward, a failed native batch must discard its own
;;;; scored samples, trace slices must hold only one attempt's traces, and
;;;; instruction components must come back unchanged even when the run
;;;; unwinds.

(in-package #:axllm)

(export '(run-refine-tests))

;;; ------------------------------------------------------------------
;;; Harness (tests/synth.lisp reuses these; load this file first)
;;; ------------------------------------------------------------------

(define-condition refine-test-failure (error)
  ((text :initarg :text :reader refine-test-failure-text))
  (:report (lambda (condition stream)
             (write-string (refine-test-failure-text condition) stream))))

(defvar *refine-tests* '())

(defmacro define-refine-test (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *refine-tests*
           (append (remove ',name *refine-tests* :key #'car) (list (cons ',name #',name))))
     ',name))

(defun refine-expect (ok description)
  (unless ok (error 'refine-test-failure :text description))
  t)

(defun refine-expect-equal (actual expected description)
  (refine-expect (equal actual expected)
                 (format nil "~a (expected ~s, got ~s)" description expected actual)))

(defun refine-expect-contains (haystack needle description)
  (refine-expect (and (stringp haystack) (search needle haystack))
                 (format nil "~a (~s not found in ~s)" description needle haystack)))

(defmacro refine-expect-error (type &body body)
  "Run BODY, require a TYPE condition, and return it."
  (let ((condition (gensym)))
    `(handler-case (progn ,@body
                          (error 'refine-test-failure
                                 :text (format nil "expected a ~a, nothing signalled" ',type)))
       (,type (,condition) ,condition))))

(defmacro without-test-warnings (&body body)
  "Run BODY with warnings muffled; several paths warn by design."
  `(handler-bind ((warning #'muffle-warning)) ,@body))

(defun run-refine-test-list (tests label)
  (let ((passed 0) (failed 0))
    (dolist (entry tests)
      (handler-case (progn (funcall (cdr entry)) (incf passed)
                           (format t "ok   ~a~%" (car entry)))
        (error (condition)
          (incf failed)
          (format t "FAIL ~a: ~a~%" (car entry) condition))))
    (format t "~a: ~a passed, ~a failed~%" label passed failed)
    (values passed failed)))

(defun run-refine-tests ()
  (run-refine-test-list *refine-tests* "refine/evaluate"))

;;; ------------------------------------------------------------------
;;; Scripted programs
;;; ------------------------------------------------------------------

(defclass scripted-program ()
  ((script :initarg :script :accessor scripted-script
           :documentation "Remaining entries; see SCRIPTED-PROGRAM's docstring.")
   (calls :initform '() :accessor scripted-calls)
   (native :initarg :native :initform nil :reader scripted-native-p)
   (components :initarg :components :initform '() :accessor scripted-components)
   (instruction :initarg :instruction :initform nil :accessor scripted-instruction)
   (traces :initform (%new-array) :reader scripted-traces)
   (chat-log :initform (%new-array) :reader scripted-chat-log)
   (usage :initarg :usage :initform (%new-array) :accessor scripted-reported-usage))
  (:documentation
   "A program that replays a script.

Each script entry is a plist:

  (:outputs OBJECT :usage OBJECT)    succeed with OBJECT
  (:samples (OBJECT ...))            call the forward options result
                                     picker with these samples and return
                                     the one it chooses
  (:error STRING)                    signal AX-ERROR
  (:from-component KEY)              succeed with an answer derived from
                                     the current value of component KEY

Every call appends one trace and one chat log entry, so a trace slice that
leaks another attempt's traces is visible."))

(defmethod program-native-sample-capable-p ((program scripted-program))
  (scripted-native-p program))

(defmethod program-traces ((program scripted-program)) (scripted-traces program))
(defmethod program-chat-log ((program scripted-program)) (scripted-chat-log program))
(defmethod program-usage ((program scripted-program)) (scripted-reported-usage program))

(defmethod program-set-instruction ((program scripted-program) text)
  (setf (scripted-instruction program) text))

(defmethod program-optimizable-components ((program scripted-program))
  ;; Component shape as src/gen.lisp produces it: identified by "id", with
  ;; "kind" naming what it is.
  (let ((out (%new-array)))
    (loop for (id . current) in (scripted-components program)
          do (vector-push-extend
              (object "id" id "owner" "root" "kind" "instruction" "current" current) out))
    ;; Kinds other than instruction must be left alone by refine.
    (vector-push-extend (object "id" "root::description" "owner" "root"
                                "kind" "description" "current" "A description.")
                        out)
    out))

(defmethod program-apply-optimized-components ((program scripted-program) component-map)
  (dolist (key (%object-keys component-map))
    (let ((entry (assoc key (scripted-components program) :test #'equal)))
      (if entry
          (setf (cdr entry) (gethash key component-map))
          (setf (scripted-components program)
                (append (scripted-components program)
                        (list (cons key (gethash key component-map)))))))))

(defun scripted-component (program key)
  (cdr (assoc key (scripted-components program) :test #'equal)))

(defun scripted-call-count (program) (length (scripted-calls program)))

(defun scripted-call (program index)
  (nth index (reverse (scripted-calls program))))

(defun scripted-call-options (program index) (third (scripted-call program index)))

(defmethod forward ((program scripted-program) client inputs &optional options)
  (push (list client inputs options) (scripted-calls program))
  (vector-push-extend (object "call" (scripted-call-count program))
                      (scripted-traces program))
  (vector-push-extend (object "role" "assistant" "call" (scripted-call-count program))
                      (scripted-chat-log program))
  (let ((entry (pop (scripted-script program))))
    (unless entry (error 'ax-error :message "scripted program exhausted"))
    (let ((samples (getf entry :samples)))
      (cond
        ((getf entry :error) (error 'ax-error :message (getf entry :error)))
        (samples
         (let ((picker (jget options "resultPicker" nil))
               (results (%new-array)))
           (refine-expect (functionp picker) "native strategy must pass a result picker")
           (loop for sample in samples
                 for index from 0
                 do (vector-push-extend (object "index" index "sample" sample) results))
           (let ((chosen (funcall picker (object "type" "fields" "results" results))))
             (refine-expect (integerp chosen) "result picker must return an index")
             (when (getf entry :then-error)
               ;; The picker already scored every sample; the call fails
               ;; afterwards, as a provider or parse failure would.
               (error 'ax-error :message (getf entry :then-error)))
             (values (nth chosen samples)
                     (or (getf entry :usage) (usage-object 1 1))))))
        ((getf entry :from-component)
         (values (object "answer" (or (scripted-component program (getf entry :from-component)) ""))
                 (or (getf entry :usage) (usage-object 1 1))))
        (t (values (getf entry :outputs) (or (getf entry :usage) (usage-object 1 1))))))))

(defun answer-length-reward (args)
  (length (jget (jget args "prediction") "answer")))

;;; ------------------------------------------------------------------
;;; best-of-n: native samples
;;; ------------------------------------------------------------------

(define-refine-test test-native-samples-scores-every-sample-and-picks-the-best
  (let* ((program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :samples (list (object "answer" "short")
                                                                    (object "answer" "much better answer")
                                                                    (object "answer" "ok"))
                                                     :usage (usage-object 11 7)))))
         (picked (best-of-n program :n 3 :reward-fn #'answer-length-reward)))
    (multiple-value-bind (result usage) (forward picked (object "name" "client")
                                                 (object "question" "pick one"))
      (refine-expect-equal (jget result "answer") "much better answer"
                           "the highest-reward sample is returned")
      (refine-expect-equal (jget usage "promptTokens") 11 "the selected attempt keeps its usage")
      (refine-expect-equal (scripted-call-count program) 1
                           "native sampling uses exactly one forward")
      (refine-expect-equal (jget (scripted-call-options program 0) "sampleCount") 3
                           "n is passed as the sample count")
      (let ((attempts (program-attempts picked)))
        (refine-expect-equal (length attempts) 3 "every sample is recorded as an attempt")
        (refine-expect-equal (map 'list #'attempt-reward attempts) '(5 18 2)
                             "each sample keeps its own reward")
        (refine-expect-equal (map 'list #'attempt-number attempts) '(1 2 3)
                             "attempt numbers count from one across the batch")
        (refine-expect (every (lambda (a) (eq (attempt-strategy a) :native-samples)) attempts)
                       "every attempt records the native strategy")
        ;; Only the selected sample carries the realized prediction, traces
        ;; and usage; the others keep their scored sample and nothing else.
        (let ((selected (find 1 attempts :key #'attempt-sample-index)))
          (refine-expect-equal (length (attempt-usage selected)) 1
                               "the selected attempt carries the call usage")
          (refine-expect-equal (length (attempt-traces selected)) 1
                               "the selected attempt carries this call's trace"))
        (refine-expect-equal (length (attempt-usage (aref attempts 0))) 0
                             "an unselected sample reports no usage of its own")))))

(define-refine-test test-threshold-hit-beats-a-strictly-better-reward
  ;; The TypeScript picks the FIRST sample at or above the threshold even
  ;; when a later sample scores higher. Picking the maximum fails here.
  (let* ((program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :samples (list (object "answer" "tiny")
                                                                    (object "answer" "good enough")
                                                                    (object "answer" "the longest answer here"))
                                                     :usage (usage-object 9 1)))))
         (picked (best-of-n program :n 3 :threshold 5 :reward-fn #'answer-length-reward)))
    (let ((result (forward picked (object "name" "client") (object "question" "q")))
          (attempts (program-attempts picked)))
      (refine-expect-equal (jget result "answer") "good enough"
                           "the first sample meeting the threshold wins")
      (refine-expect-equal (map 'list #'attempt-met-threshold attempts)
                           '(nil t t)
                           "threshold flags follow the reward, not the selection")
      ;; The picker must also tell the provider to realize the first
      ;; threshold sample, so that sample is the one holding the call's
      ;; usage and traces, not the highest scoring one.
      (refine-expect-equal (length (attempt-usage (find 1 attempts :key #'attempt-sample-index)))
                           1 "the realized sample is the first threshold sample")
      (refine-expect-equal (length (attempt-usage (find 2 attempts :key #'attempt-sample-index)))
                           0 "the highest scoring sample was not realized"))))

(define-refine-test test-native-model-config-and-session-are-merged-not-replaced
  (let* ((program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :samples (list (object "answer" "a"))))))
         (picked (best-of-n program :n 1
                                    :model-config (object "temperature" 0.2d0 "topP" 0.5d0)
                                    :reward-fn #'answer-length-reward)))
    (forward picked (object "name" "client") (object "question" "q")
             (object "maxRetries" 4 "modelConfig" (object "topP" 0.9d0)))
    (let* ((options (scripted-call-options program 0))
           (config (jget options "modelConfig")))
      (refine-expect-equal (jget options "maxRetries") 4 "caller options are preserved")
      (refine-expect-equal (jget config "temperature") 0.2d0
                           "the wrapper's model config wins over the default temperature")
      (refine-expect-equal (jget config "topP") 0.9d0
                           "the caller's model config wins over the wrapper's")
      (refine-expect (json-false-p (jget config "stream"))
                     "native sampling disables streaming")
      (refine-expect-contains (jget options "sessionId") "ax-refine-native"
                              "the native session id is tagged")
      (refine-expect (json-true-p (jget options "freshMemory"))
                     "a native batch asks for its own conversation memory"))))

(define-refine-test test-native-strategy-requires-a-capable-program
  (let ((picked (best-of-n (make-instance 'scripted-program :native nil :script '())
                           :n 2 :strategy :native-samples
                           :reward-fn #'answer-length-reward)))
    (refine-expect-contains
     (ax-error-message (refine-expect-error refine-error
                         (forward picked (object "name" "client") (object "question" "q"))))
     "native sampling" "an incapable program rejects the native strategy")))

(define-refine-test test-a-failed-native-batch-discards-its-scored-samples
  ;; The TypeScript returns only the failure attempt; samples scored inside
  ;; the picker before the error are dropped.
  (let* ((scored 0)
         (program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :error "provider exploded"))))
         (picked (best-of-n program :n 3 :fail-count 1
                                    :reward-fn (lambda (args)
                                                 (incf scored)
                                                 (answer-length-reward args)))))
    (let ((condition (refine-expect-error refine-error
                       (forward picked (object "name" "client") (object "question" "q")))))
      (refine-expect-equal scored 0 "no sample was scored")
      (refine-expect-equal (length (refine-error-attempts condition)) 1
                           "the failure is reported as one attempt")
      (refine-expect-contains (ax-error-message condition) "no successful candidates"
                              "best-of-n reports that nothing scored"))))

(define-refine-test test-samples-scored-before-a-late-failure-are-discarded
  ;; The picker scores all three samples and the call then fails. The
  ;; TypeScript keeps none of them: the batch is the single failure
  ;; attempt, so a later attempt is numbered 2, not 5.
  (let* ((scored 0)
         (program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :samples (list (object "answer" "a")
                                                                    (object "answer" "bb")
                                                                    (object "answer" "ccc"))
                                                     :then-error "stream broke"))))
         (picked (best-of-n program :n 3 :fail-count 2
                                    :reward-fn (lambda (args)
                                                 (incf scored)
                                                 (answer-length-reward args)))))
    (let ((condition (refine-expect-error refine-error
                       (forward picked (object "name" "client") (object "question" "q")))))
      (refine-expect-equal scored 3 "every sample was scored before the failure")
      (refine-expect-equal (length (refine-error-attempts condition)) 1
                           "the scored samples are discarded with the failed call")
      (let ((attempt (first (refine-error-attempts condition))))
        (refine-expect-equal (attempt-number attempt) 1
                             "the failure is attempt one, not attempt four")
        (refine-expect (attempt-error attempt) "the attempt carries the failure")
        (refine-expect (null (attempt-prediction attempt))
                       "a discarded batch leaves no prediction")))))

(define-refine-test test-native-failure-budget-stops-the-run
  (let* ((program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :error "boom"))))
         (picked (best-of-n program :n 2 :fail-count 0 :reward-fn #'answer-length-reward)))
    (refine-expect-contains
     (ax-error-message (refine-expect-error refine-error
                         (forward picked (object "name" "client") (object "question" "q"))))
     "Native sample attempt failed" "a zero failure budget surfaces the native failure")))

;;; ------------------------------------------------------------------
;;; best-of-n: serial
;;; ------------------------------------------------------------------

(define-refine-test test-serial-stops-at-the-threshold-and-numbers-attempts
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :outputs (object "answer" "excellent"))
                                               (list :outputs (object "answer" "never used")))))
         (picked (best-of-n program :n 3 :threshold 1
                                    :reward-fn (lambda (args)
                                                 (if (equal (jget (jget args "prediction") "answer")
                                                            "excellent")
                                                     1 0)))))
    (let ((result (forward picked (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (jget result "answer") "excellent" "the threshold candidate is returned")
      (refine-expect-equal (scripted-call-count program) 2
                           "the serial batch stops as soon as the threshold is met")
      (let ((attempts (program-attempts picked)))
        (refine-expect-equal (length attempts) 2 "only the attempts that ran are recorded")
        (refine-expect-equal (map 'list #'attempt-sample-index attempts) '(0 1)
                             "sample indexes follow the serial order")
        (refine-expect (every (lambda (a) (eq (attempt-strategy a) :serial)) attempts)
                       "every attempt records the serial strategy")))))

(define-refine-test test-a-non-capable-program-defaults-to-serial
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "one"))
                                               (list :outputs (object "answer" "two")))))
         (picked (best-of-n program :n 2 :reward-fn #'answer-length-reward)))
    (forward picked (object "name" "client") (object "question" "q"))
    (refine-expect-equal (scripted-call-count program) 2
                         "auto strategy runs one forward per candidate")
    (refine-expect (null (jget (scripted-call-options program 0) "sampleCount" nil))
                   "the serial strategy never asks for native samples")
    (refine-expect-contains (jget (scripted-call-options program 0) "sessionId")
                            "ax-refine-serial" "the serial session id is tagged")
    ;; Each candidate is a separate run, so each asks for a fresh memory and
    ;; its own session, the way the TypeScript hands every attempt a new
    ;; AxMemory.
    (refine-expect (json-true-p (jget (scripted-call-options program 0) "freshMemory"))
                   "each serial attempt asks for its own conversation memory")
    (refine-expect (not (equal (jget (scripted-call-options program 0) "sessionId")
                               (jget (scripted-call-options program 1) "sessionId")))
                   "two attempts never share a session id")))

(define-refine-test test-serial-trace-and-chat-slices-hold-one-attempt-each
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "a"))
                                               (list :outputs (object "answer" "bb")))))
         (seen '())
         (picked (best-of-n program :n 2
                                    :reward-fn (lambda (args)
                                                 (push (list (jget args "attempt")
                                                             (jget args "round")
                                                             (jget args "sampleIndex")
                                                             (length (jget args "traces"))
                                                             (length (jget args "chatLog")))
                                                       seen)
                                                 (answer-length-reward args)))))
    (forward picked (object "name" "client") (object "question" "q"))
    (refine-expect-equal (reverse seen) '((1 1 0 1 1) (2 1 1 1 1))
                         "each attempt sees only the traces and chat entries it produced")
    (let ((attempts (program-attempts picked)))
      (refine-expect-equal (jget (aref (attempt-traces (aref attempts 1)) 0) "call") 2
                           "the second attempt's trace slice starts after the first"))))

(define-refine-test test-serial-failures-count-against-the-budget
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :error "first failed")
                                               (list :outputs (object "answer" "recovered")))))
         (picked (best-of-n program :n 2 :reward-fn #'answer-length-reward)))
    (let ((result (forward picked (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (jget result "answer") "recovered"
                           "a failure inside the budget does not end the run")
      (let ((attempts (program-attempts picked)))
        (refine-expect-equal (length attempts) 2 "the failed attempt is recorded")
        (refine-expect (attempt-error (aref attempts 0)) "the first attempt carries its error")
        (refine-expect (null (attempt-reward (aref attempts 0)))
                       "a failed attempt has no reward")
        (refine-expect-equal (attempt-number (aref attempts 1)) 2
                             "attempt numbering counts failures too"))))
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :error "one") (list :error "two"))))
         (picked (best-of-n program :n 2 :fail-count 1 :reward-fn #'answer-length-reward))
         (condition (refine-expect-error refine-error
                      (forward picked (object "name" "client") (object "question" "q")))))
    (refine-expect-contains (ax-error-message condition) "after 2 failures"
                            "the second failure exceeds a budget of one")
    (refine-expect-equal (length (refine-error-attempts condition)) 2
                         "the error carries every attempt")))

(define-refine-test test-a-signalling-reward-function-is-an-attempt-failure
  ;; In the TypeScript the reward call sits inside the serial try block, so
  ;; a throwing reward function consumes the failure budget instead of
  ;; escaping immediately.
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "a"))
                                               (list :outputs (object "answer" "bb")))))
         (calls 0)
         (picked (best-of-n program :n 2
                                    :reward-fn (lambda (args)
                                                 (incf calls)
                                                 (if (= calls 1)
                                                     (error 'ax-error :message "reward broke")
                                                     (answer-length-reward args))))))
    (let ((result (forward picked (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (jget result "answer") "bb"
                           "the run continues after a failed reward")
      (refine-expect-equal (length (program-attempts picked)) 2
                           "the failed reward is recorded as a failed attempt"))))

(define-refine-test test-wrapper-usage-merges-every-attempt
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "a")
                                                     :usage (usage-object 10 2))
                                               (list :outputs (object "answer" "bb")
                                                     :usage (usage-object 5 3)))))
         (picked (best-of-n program :n 2 :reward-fn #'answer-length-reward)))
    (multiple-value-bind (result usage) (forward picked (object "name" "client")
                                                 (object "question" "q"))
      (refine-expect-equal (jget result "answer") "bb" "the longer answer wins")
      (refine-expect-equal (jget usage "promptTokens") 5
                           "forward reports the selected attempt's usage")
      (let ((merged (program-usage picked)))
        (refine-expect-equal (length merged) 1 "one ai/model pair merges to one entry")
        (refine-expect-equal (jget (aref merged 0) "promptTokens") 15
                             "prompt tokens are summed across attempts")
        (refine-expect-equal (jget (aref merged 0) "totalTokens") 20
                             "total tokens are summed across attempts")))))

(define-refine-test test-wrapper-exposes-only-the-selected-attempt-context
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "a"))
                                               (list :outputs (object "answer" "bb")))))
         (picked (best-of-n program :n 2 :reward-fn #'answer-length-reward)))
    (refine-expect-equal (length (program-traces picked)) 0
                         "there is no selected context before a run")
    (forward picked (object "name" "client") (object "question" "q"))
    (refine-expect-equal (length (program-traces picked)) 1
                         "traces come from the selected attempt only")
    (refine-expect-equal (jget (aref (program-chat-log picked) 0) "call") 2
                         "the chat log is the selected attempt's")))

(define-refine-test test-counts-and-reward-functions-are-validated
  (let ((program (make-instance 'scripted-program :native nil :script '())))
    (refine-expect-contains
     (ax-error-message (refine-expect-error refine-error
                         (best-of-n program :n 0 :reward-fn #'answer-length-reward)))
     "n must be a positive number" "n must be at least one")
    (refine-expect-error refine-error (best-of-n program :n 2 :reward-fn nil))
    (refine-expect-error refine-error
      (refine program :rounds 2 :samples-per-round 0 :reward-fn #'answer-length-reward))
    (refine-expect-equal (best-of-n-count (best-of-n program :n 3.7d0
                                                             :reward-fn #'answer-length-reward))
                         3 "a fractional count is floored")
    (let ((picked (best-of-n program :n 2 :strategy :sideways
                                     :reward-fn #'answer-length-reward)))
      (refine-expect-contains
       (ax-error-message (refine-expect-error refine-error
                           (forward picked (object "name" "client") (object "question" "q"))))
       "unknown strategy" "an unknown strategy is rejected at run time"))))

(define-refine-test test-a-reward-that-is-not-a-number-is-reported
  (let* ((program (make-instance 'scripted-program
                                 :native t
                                 :script (list (list :samples (list (object "answer" "a"))))))
         (picked (best-of-n program :n 1 :fail-count 0
                                    :reward-fn (lambda (args) (declare (ignore args)) "high"))))
    (refine-expect-error refine-error
      (forward picked (object "name" "client") (object "question" "q")))))

(define-refine-test test-program-streaming-forward-is-refused
  ;; One public streaming protocol: a wrapper refuses the same generic a
  ;; streaming program would implement, rather than scoring a prefix.
  (let ((picked (best-of-n (make-instance 'scripted-program :native t :script '())
                           :n 2 :reward-fn #'answer-length-reward)))
    (refine-expect (typep (fdefinition 'program-streaming-forward) 'generic-function)
                   "program-streaming-forward is the shared streaming generic")
    (refine-expect-contains
     (ax-error-message (refine-expect-error refine-error
                         (program-streaming-forward picked (object "name" "client")
                                                    (object "question" "q"))))
     "do not support program-streaming-forward"
     "a wrapper refuses to stream rather than scoring a prefix")))

;;; ------------------------------------------------------------------
;;; refine
;;; ------------------------------------------------------------------

(defun advice-feedback-factory (advice &key (calls (list 0)))
  "A feedback generator factory returning ADVICE, counting its calls."
  (lambda (signature)
    (refine-expect-contains signature "instructionComponents"
                            "the feedback signature keeps its component input")
    (incf (first calls))
    (make-instance 'scripted-program
                   :native nil
                   :script (list (list :outputs (object "summary" "do better"
                                                        "advice" advice))))))

(define-refine-test test-refine-applies-advice-then-restores-the-components
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Answer the question."))
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :from-component "root::instruction"))))
         (calls (list 0))
         (*refine-feedback-generator-factory*
           (advice-feedback-factory (object "root::instruction" "  say improved  "
                                            "root::description" "ignored"
                                            "unknown::key" "ignored")
                                    :calls calls))
         (improved (refine program :rounds 2 :samples-per-round 1 :threshold 1
                                   :reward-fn (lambda (args)
                                                (if (search "say improved"
                                                            (jget (jget args "prediction") "answer"))
                                                    1 0)))))
    (let ((result (forward improved (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (first calls) 1 "advice is requested once between two rounds")
      (refine-expect-contains (jget result "answer") "say improved"
                              "the second round sees the advised instruction")
      (refine-expect-equal (scripted-component program "root::instruction")
                           "Answer the question."
                           "the original instruction component is restored")
      (refine-expect (null (scripted-component program "unknown::key"))
                     "advice for an id the program does not own is ignored")
      (refine-expect (null (scripted-component program "root::description"))
                     "advice for a component of another kind is ignored")
      (let ((attempts (program-attempts improved)))
        (refine-expect-equal (length attempts) 2 "both rounds are recorded")
        (refine-expect-equal (map 'list #'attempt-round attempts) '(1 2)
                             "attempts record their round")
        (refine-expect (attempt-advice-applied (aref attempts 0))
                       "the first round records that advice was applied")
        (refine-expect-equal (jget (attempt-advice (aref attempts 0)) "root::instruction")
                             "  say improved  "
                             "the raw advice is kept on the attempt")
        (refine-expect (null (attempt-advice-applied (aref attempts 1)))
                       "no advice is requested after the final round")))))

(define-refine-test test-refine-advice-text-is-appended-to-the-original
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :from-component "root::instruction"))))
         (*refine-feedback-generator-factory*
           (advice-feedback-factory (object "root::instruction" "Be terse.")))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :reward-fn #'answer-length-reward)))
    (let ((result (forward improved (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (jget result "answer")
                           (format nil "Base.~%~%Refinement advice from previous attempt:~%Be terse.")
                           "advice is appended under a fixed heading"))))

(define-refine-test test-refine-ignores-blank-advice-and-non-string-values
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :from-component "root::instruction"))))
         (*refine-feedback-generator-factory*
           (advice-feedback-factory (object "root::instruction" "   ")))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :reward-fn #'answer-length-reward)))
    (let ((result (forward improved (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (jget result "answer") "Base."
                           "whitespace-only advice changes nothing")
      (refine-expect (null (attempt-advice-applied (aref (program-attempts improved) 0)))
                     "nothing applied means adviceApplied stays false")))
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :from-component "root::instruction"))))
         (*refine-feedback-generator-factory*
           (advice-feedback-factory (object "root::instruction" 42)))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :reward-fn #'answer-length-reward)))
    (forward improved (object "name" "client") (object "question" "q"))
    (refine-expect-equal (scripted-component program "root::instruction") "Base."
                         "a non-string advice value is dropped")))

(define-refine-test test-refine-without-instruction-components-asks-for-no-advice
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components '()
                                 :script (list (list :outputs (object "answer" "a"))
                                               (list :outputs (object "answer" "bb")))))
         (calls (list 0))
         (*refine-feedback-generator-factory* (advice-feedback-factory (%new-object) :calls calls))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :reward-fn #'answer-length-reward)))
    (let ((result (forward improved (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (first calls) 0 "no components means no feedback request")
      (refine-expect-equal (jget result "answer") "bb"
                           "the best round still wins across rounds"))))

(define-refine-test test-refine-keeps-the-best-round-and-restores-on-unwind
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "medium"))
                                               (list :error "round two failed"))))
         (*refine-feedback-generator-factory*
           (advice-feedback-factory (object "root::instruction" "Try harder.")))
         (improved (refine program :rounds 2 :samples-per-round 1 :fail-count 0
                                   :reward-fn #'answer-length-reward)))
    (refine-expect-error refine-error
      (forward improved (object "name" "client") (object "question" "q")))
    (refine-expect-equal (scripted-component program "root::instruction") "Base."
                         "components are restored even when the run unwinds")))

(define-refine-test test-refine-threshold-ends-the-run-before-later-rounds
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "good")))))
         (calls (list 0))
         (*refine-feedback-generator-factory*
           (advice-feedback-factory (object "root::instruction" "unused") :calls calls))
         (improved (refine program :rounds 3 :samples-per-round 1 :threshold 1
                                   :reward-fn (lambda (args) (declare (ignore args)) 1))))
    (let ((result (forward improved (object "name" "client") (object "question" "q"))))
      (refine-expect-equal (jget result "answer") "good" "the threshold candidate is returned")
      (refine-expect-equal (scripted-call-count program) 1 "later rounds do not run")
      (refine-expect-equal (first calls) 0 "a threshold hit skips the feedback call")
      (refine-expect-equal (scripted-component program "root::instruction") "Base."
                           "an early return still restores the components"))))

(define-refine-test test-refine-feedback-uses-its-own-client-and-model-config
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :from-component "root::instruction"))))
         (feedback-program nil)
         (feedback-client (object "name" "feedback-client"))
         (*refine-feedback-generator-factory*
           (lambda (signature)
             (declare (ignore signature))
             (setf feedback-program
                   (make-instance 'scripted-program
                                  :native nil
                                  :script (list (list :outputs
                                                      (object "summary" "s"
                                                              "advice" (object "root::instruction"
                                                                               "Be terse."))))))))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :feedback-client feedback-client
                                   :feedback-model-config (object "temperature" 0.1d0)
                                   :reward-description "length of the answer"
                                   :program-description "a question answerer"
                                   :reward-fn #'answer-length-reward)))
    (forward improved (object "name" "gen-client") (object "question" "q"))
    (refine-expect feedback-program "a feedback generator was built")
    (let* ((call (scripted-call feedback-program 0))
           (inputs (second call))
           (options (third call)))
      (refine-expect-equal (jget (first call) "name") "feedback-client"
                           "the feedback client is used for advice")
      (refine-expect-equal (jget (jget options "modelConfig") "temperature") 0.1d0
                           "the feedback model config is passed through")
      (refine-expect-equal (jget inputs "programDescription") "a question answerer"
                           "the program description is passed through")
      (refine-expect-equal (jget inputs "rewardDescription") "length of the answer"
                           "the reward description is passed through")
      (refine-expect-equal (jget inputs "rewardThreshold") "not specified"
                           "an absent threshold is reported as not specified")
      (refine-expect-equal (jget inputs "rewardValue") 3
                           "the scored reward of the round's best attempt is reported")
      (refine-expect-contains (jget inputs "instructionComponents") "root::instruction"
                              "the component keys are listed for the feedback model")
      (refine-expect-contains (jget inputs "failedPrediction") "bad"
                              "the failed prediction is included")
      (refine-expect-contains (jget inputs "attemptSummaries") "\"reward\":3"
                              "earlier attempts are summarized as JSON")
      (refine-expect-equal (scripted-instruction feedback-program)
                           +refine-feedback-instruction+
                           "the feedback generator gets the advice instruction"))))

(define-refine-test test-a-program-without-a-signature-is-still-described
  ;; PROGRAM-SIGNATURE answers :NULL for a program that declares none, so
  ;; the feedback prompt must fall back to the printed program instead of
  ;; handing :NULL to SIGNATURE-STRING.
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :components (list (cons "root::instruction" "Base."))
                                 :script (list (list :outputs (object "answer" "bad"))
                                               (list :from-component "root::instruction"))))
         (feedback nil)
         (*refine-feedback-generator-factory*
           (lambda (signature)
             (declare (ignore signature))
             (setf feedback
                   (make-instance 'scripted-program
                                  :native nil
                                  :script (list (list :outputs
                                                      (object "summary" "s"
                                                              "advice"
                                                              (object "root::instruction" "Be terse."))))))))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :reward-fn #'answer-length-reward)))
    (refine-expect (eq (program-signature program) :null)
                   "the scripted program declares no signature")
    (forward improved (object "name" "client") (object "question" "q"))
    (refine-expect feedback "advice was still requested")
    (let ((description (jget (second (scripted-call feedback 0)) "programDescription")))
      (refine-expect (and (stringp description) (plusp (length description)))
                     "the program is described by some text")
      (refine-expect-contains description "SCRIPTED-PROGRAM"
                              "the description falls back to the printed program"))))

(define-refine-test test-refine-rejects-an-exhausted-run-with-no-candidate
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :error "a") (list :error "b"))))
         (improved (refine program :rounds 2 :samples-per-round 1
                                   :reward-fn #'answer-length-reward))
         (condition (refine-expect-error refine-error
                      (forward improved (object "name" "client") (object "question" "q")))))
    (refine-expect-contains (ax-error-message condition) "no successful candidates"
                            "refine reports that no candidate scored")
    (refine-expect-equal (length (refine-error-attempts condition)) 2
                         "both failures are reported")))

;;; ------------------------------------------------------------------
;;; evaluate
;;; ------------------------------------------------------------------

(define-refine-test test-test-prompt-requires-examples
  (refine-expect-contains
   (ax-error-message (refine-expect-error ax-error
                       (test-prompt :client (object "name" "client")
                                    :program (make-instance 'scripted-program :script '())
                                    :examples '())))
   "No examples found" "an empty example set is refused"))

(define-refine-test test-test-prompt-scores-every-example-with-one-retry
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "yes"))
                                               (list :outputs (object "answer" "no")))))
         (seen '())
         (test (test-prompt :client (object "name" "client")
                            :program program
                            :examples (list (object "question" "a" "answer" "yes")
                                            (object "question" "b" "answer" "yes")))))
    (refine-expect-equal
     (multiple-value-list
      (run-test-prompt test (lambda (args)
                              (push (jget (jget args "example") "question") seen)
                              (if (equal (jget (jget args "prediction") "answer")
                                         (jget (jget args "example") "answer"))
                                  1 0))))
     '() "run-test-prompt returns no values, like the TypeScript")
    (refine-expect-equal (reverse seen) '("a" "b") "the metric sees every example in order")
    (refine-expect-equal (jget (scripted-call-options program 0) "maxRetries") 1
                         "each example is forwarded with one retry")
    (refine-expect-equal (jget (second (scripted-call program 1)) "question") "b"
                         "the example itself is the forward input")))

(define-refine-test test-test-prompt-scores-a-failing-example-as-zero
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "yes"))
                                               (list :error "provider down")
                                               (list :outputs (object "answer" "yes")))))
         (test (test-prompt :client (object "name" "client")
                            :program program
                            :examples (list (object "question" "a")
                                            (object "question" "b")
                                            (object "question" "c"))
                            :debug t))
         (output (without-test-warnings
                   (with-output-to-string (*standard-output*)
                     (run-test-prompt test (lambda (args) (declare (ignore args)) 1))))))
    (refine-expect-equal (scripted-call-count program) 3
                         "a failing example does not stop the run")
    (refine-expect-contains output "Performance:  2 / 3"
                            "the failed example contributes zero to the sum")
    (refine-expect-contains output "Average Score:  0.6666"
                            "the average divides by every example, including the failure")))

(define-refine-test test-test-prompt-reports-a-failing-metric-without-stopping
  (let* ((program (make-instance 'scripted-program
                                 :native nil
                                 :script (list (list :outputs (object "answer" "a"))
                                               (list :outputs (object "answer" "b")))))
         (test (test-prompt :client (object "name" "client")
                            :program program
                            :examples (list (object "question" "a") (object "question" "b"))
                            :debug t))
         (warnings 0)
         (output (handler-bind ((warning (lambda (condition)
                                           (declare (ignore condition))
                                           (incf warnings)
                                           (muffle-warning))))
                   (with-output-to-string (*standard-output*)
                     (run-test-prompt test (lambda (args)
                                             (if (equal (jget (jget args "example") "question") "a")
                                                 (error 'ax-error :message "metric broke")
                                                 1)))))))
    (refine-expect-equal warnings 1 "the failing example is reported once")
    (refine-expect-contains output "Performance:  1 / 2"
                            "only the scoring example contributes")))

;;; ------------------------------------------------------------------
;;; Against the real generator
;;; ------------------------------------------------------------------
;;;
;;; These tests wrap programs built by AX and run them through the real
;;; FORWARD in src/gen.lisp. Only the HTTP transport is scripted, so the
;;; prompts, output parsing, correction turns, traces, chat log and usage
;;; are the generator's own. No credential and no network are used.

(defstruct (client-script (:conc-name cs-))
  "Scripted transport state: replies to hand out, and requests recorded."
  (replies '())
  (requests '()))

(defun refine-openai-body (content &key (prompt 5) (completion 2) responses)
  "A provider reply carrying CONTENT, in the dialect the request actually used.

RESPONSES selects the Responses envelope over Chat Completions.  The
scripted client answers whichever API the client really called, because a
model moving between the two is a provider detail and these tests are about
refine's own accounting: answering in the wrong dialect makes every reply
unparseable and turns one provider change into a dozen unrelated failures.

The token counts are identical either way, so the usage assertions here
read the same numbers whichever envelope was sent."
  (encode-json
   (if responses
       (object "output" (vector (object "type" "message" "role" "assistant"
                                        "content" (vector (object "type" "output_text"
                                                                  "text" content))))
               "usage" (object "input_tokens" prompt "output_tokens" completion
                               "total_tokens" (+ prompt completion)))
       (object "choices" (vector (object "index" 0 "finish_reason" "stop"
                                         "message" (object "role" "assistant" "content" content)))
               "usage" (object "prompt_tokens" prompt "completion_tokens" completion
                               "total_tokens" (+ prompt completion))))))

(defun refine-responses-url-p (url)
  "Whether URL is the Responses endpoint rather than Chat Completions."
  (and (stringp url) (search "/responses" url) t))

(defun refine-scripted-client (replies)
  "A real AI-CLIENT whose transport replies with REPLIES, in order.

Each reply is assistant content, or a function of the request body
returning it, so a reply can depend on the prompt the generator built.
Returns (values client script)."
  (let ((script (make-client-script :replies (copy-list replies))))
    (values (ai :name "openai" :model "gpt-6-luna" :api-key "test-key"
                :transport (lambda (url headers json-body)
                             (declare (ignore headers))
                             (push json-body (cs-requests script))
                             (let ((next (if (cs-replies script)
                                             (pop (cs-replies script))
                                             (error 'ax-error
                                                    :message "scripted client exhausted"))))
                               (values (refine-openai-body
                                        (if (functionp next) (funcall next json-body) next)
                                        :responses (refine-responses-url-p url))
                                       200))))
            script)))

(defun cs-request-count (script) (length (cs-requests script)))
(defun cs-request (script index) (nth index (reverse (cs-requests script))))

(define-refine-test test-real-generator-is-sampled-serially
  ;; Asking for n candidates serially must really run n complete forwards;
  ;; scoring one candidate n times would make best-of-n a no-op that still
  ;; looks like it worked.
  ;;
  ;; This asserts the serial strategy by name rather than relying on the
  ;; generator lacking native sampling.  That absence is a fact about today's
  ;; generator, not a guarantee of refine's, and when native sampling lands
  ;; a test that assumed it would start failing for a reason that has
  ;; nothing to do with what it is checking.
  (multiple-value-bind (client script)
      (refine-scripted-client '("Answer: a" "Answer: bbbb"))
    (let* ((gen (ax "question:string -> answer:string"))
           (best (forward (best-of-n gen :n 2 :strategy :serial
                                         :reward-fn #'answer-length-reward)
                          client (object "question" "q"))))
      (refine-expect-equal (cs-request-count script) 2
                           "two candidates means two real forwards")
      (refine-expect-equal (jget best "answer") "bbbb"
                           "and the better-rewarded candidate is the one returned")))
  ;; The native strategy must fail closed while the capability is absent,
  ;; rather than quietly degrade to one sample.  Guarded by the capability
  ;; itself so that landing native sampling turns this into a live path
  ;; instead of a false failure.
  (let ((gen (ax "question:string -> answer:string")))
    (when (null (program-native-sample-capable-p gen))
      (refine-expect-contains
       (ax-error-message
        (refine-expect-error refine-error
          (forward (best-of-n gen :n 2 :strategy :native-samples
                                  :reward-fn #'answer-length-reward)
                   (object "name" "unused") (object "question" "q"))))
       "native sampling" "the native strategy fails closed while it is unsupported"))))

(define-refine-test test-best-of-n-runs-the-real-generator
  ;; Every assertion below is a serial-batch contract: one request per
  ;; candidate, the threshold stopping the batch early, and one trace, chat
  ;; turn and usage entry sliced off per attempt.  The strategy is named for
  ;; that reason.  Left to negotiate, this now picks the native path, which
  ;; asks once and has no per-candidate requests to count.
  (multiple-value-bind (client script)
      (refine-scripted-client '("Answer: bad" "Answer: excellent" "Answer: never requested"))
    (let* ((gen (ax "question:string -> answer:string"))
           (picked (best-of-n gen :n 3 :threshold 1 :strategy :serial
                                  :reward-fn (lambda (args)
                                               (if (equal (jget (jget args "prediction") "answer")
                                                          "excellent")
                                                   1 0)))))
      (multiple-value-bind (out usage) (forward picked client (object "question" "pick one"))
        (refine-expect-equal (jget out "answer") "excellent"
                             "the threshold candidate is returned from the real generator")
        (refine-expect-equal (cs-request-count script) 2
                             "the serial batch stops at the threshold, one request per candidate")
        (refine-expect-equal (jget usage "promptTokens") 5
                             "the selected attempt reports the real call usage")
        (let ((attempts (program-attempts picked)))
          (refine-expect-equal (length attempts) 2 "both real attempts are recorded")
          (refine-expect-equal (length (program-traces gen)) 2
                               "the generator accumulated a trace per run")
          (loop for attempt across attempts
                for index from 0
                do (refine-expect-equal (length (attempt-traces attempt)) 1
                                        "each attempt slices off exactly its own trace")
                   (refine-expect-equal (jget (aref (attempt-traces attempt) 0) "status") "ok"
                                        "the sliced trace is the generator's own run record")
                   (refine-expect-equal (length (attempt-chat-log attempt)) 1
                                        "each attempt slices off exactly its own chat turn")
                   (refine-expect-equal (jget (aref (attempt-chat-log attempt) 0) "model")
                                        "gpt-6-luna" "the chat turn is the real provider turn")
                   (refine-expect-equal (jget (first (attempt-usage attempt)) "totalTokens") 7
                                        (format nil "attempt ~a carries its real usage" index)))
          (let ((merged (program-usage picked)))
            (refine-expect-equal (jget (aref merged 0) "promptTokens") 10
                                 "the wrapper's usage sums both real calls")))))))

(define-refine-test test-refine-runs-the-real-generator-and-restores-its-instruction
  ;; Port of the TypeScript refine test: the feedback model's advice must
  ;; reach the generator's prompt for the second round, and the generator's
  ;; instruction component must be back to its original text afterwards.
  (let ((reply (lambda (body)
                 (if (search "say improved" body) "Answer: improved" "Answer: bad"))))
    (multiple-value-bind (gen-client gen-script) (refine-scripted-client (list reply reply))
      (multiple-value-bind (feedback-client feedback-script)
          (refine-scripted-client
           (list (format nil "Summary: tell it to improve~%Advice: {\"root::instruction\": \"say improved\"}")))
        (let* ((gen (ax "question:string -> answer:string"
                        :instruction "Answer the question."))
               (improved (refine gen :rounds 2 :samples-per-round 1 :threshold 1
                                     :feedback-client feedback-client
                                     :reward-fn (lambda (args)
                                                  (if (equal (jget (jget args "prediction") "answer")
                                                             "improved")
                                                      1 0)))))
          (let ((result (forward improved gen-client (object "question" "test"))))
            (refine-expect-equal (jget result "answer") "improved"
                                 "the advised round produces the better answer")
            (refine-expect-equal (cs-request-count gen-script) 2 "one generator call per round")
            (refine-expect-equal (cs-request-count feedback-script) 1
                                 "advice is requested once, from the feedback client")
            (refine-expect-contains (cs-request feedback-script 0) "root::instruction"
                                    "the feedback prompt names the component id to advise")
            ;; With no PROGRAM-SIGNATURE hook, the program description falls
            ;; back to the printed program, which carries its signature.
            (refine-expect-contains (cs-request feedback-script 0)
                                    "question:string -> answer:string"
                                    "the feedback prompt describes the program being refined")
            (refine-expect (null (search "say improved" (cs-request gen-script 0)))
                           "the first round runs the original instruction")
            (refine-expect-contains (cs-request gen-script 1) "say improved"
                                    "the second round prompt carries the advice")
            (let ((components (program-optimizable-components gen)))
              (refine-expect-equal (jget (find "root::instruction" components
                                               :key (lambda (c) (jget c "id")) :test #'equal)
                                         "current")
                                   "Answer the question."
                                   "the generator's instruction component is restored"))
            (refine-expect (attempt-advice-applied (aref (program-attempts improved) 0))
                           "the first round records that advice was applied")))))))

(define-refine-test test-test-prompt-passes-one-retry-to-the-real-generator
  ;; The generator allows two corrections by default; evaluation must send
  ;; maxRetries 1, so the second example fails after a single correction
  ;; instead of consuming a third reply and scoring. Two output fields are
  ;; needed to make unlabelled prose invalid: with a single output field,
  ;; Core's text contract accepts a bare response as that field's value.
  (multiple-value-bind (client script)
      (refine-scripted-client (list (format nil "Answer: yes~%Score: 1")
                                    "I would rather not"
                                    "Still refusing"
                                    (format nil "Answer: yes~%Score: 2")))
    (let* ((gen (ax "question:string -> answer:string, score:number"))
           (test (test-prompt :client client
                              :program gen
                              :examples (list (object "question" "a") (object "question" "b"))
                              :debug t))
           (output (without-test-warnings
                     (with-output-to-string (*standard-output*)
                       (run-test-prompt test (lambda (args) (declare (ignore args)) 1))))))
      (refine-expect-equal (cs-request-count script) 3
                           "one request for the first example, two for the second, whose single correction is its last")
      (refine-expect-contains output "Performance:  1 / 2"
                              "the example that exhausted its single retry scores zero")
      (refine-expect-equal (length (program-traces gen)) 2
                           "the generator recorded both runs, including the failed one")
      (refine-expect-equal (jget (aref (program-traces gen) 1) "status") "error"
                           "the failed example is recorded as an error run"))))

(define-refine-test test-each-attempt-gets-a-fresh-memory-from-the-real-generator
  ;; The generator honors "freshMemory", so an attempt's conversation is its
  ;; own: the generator's memory is left untouched while its chat log,
  ;; usage and traces still accumulate, which is what every attempt's
  ;; slices are cut from.
  ;; Serial by name: fresh memory is a property of a per-attempt forward, and
  ;; the native path makes one call with no separate attempts to isolate.
  (multiple-value-bind (client script)
      (refine-scripted-client '("Answer: a" "Answer: bb"))
    (let* ((gen (ax "question:string -> answer:string"))
           (picked (best-of-n gen :n 2 :strategy :serial
                                  :reward-fn #'answer-length-reward)))
      (forward picked client (object "question" "q"))
      (refine-expect-equal (cs-request-count script) 2 "both attempts ran")
      (refine-expect-equal (length (memory-history (generator-memory gen))) 0
                           "no attempt recorded its turns in the shared memory")
      (refine-expect-equal (length (program-chat-log gen)) 2
                           "the program's own chat log still accumulates across attempts")
      (refine-expect-equal (length (program-traces gen)) 2
                           "the program's own traces still accumulate across attempts")
      ;; Without the option the same two calls share the generator's memory,
      ;; which is the state refine must not leave behind.
      (multiple-value-bind (plain-client plain-script) (refine-scripted-client '("Answer: a"))
        (declare (ignore plain-script))
        (forward gen plain-client (object "question" "q"))
        (refine-expect (plusp (length (memory-history (generator-memory gen))))
                       "a plain forward does record into the shared memory")))))

(define-refine-test test-a-wrapper-is-still-the-program-it-wraps
  ;; An agent lends a program tools and reads back which ones ran; wrapping
  ;; it in best-of-n must not hide any of that.
  (let* ((probe (tool :name "probe"
                      :description "A probe."
                      :parameters (object "type" "object"
                                          "properties" (object "x" (object "type" "string")))
                      :handler (lambda (args) (declare (ignore args)) "ok")))
         (gen (ax "question:string -> answer:string"))
         (picked (best-of-n gen :n 1 :reward-fn #'answer-length-reward)))
    (refine-expect-equal (signature-string (program-signature picked))
                         "question:string -> answer:string"
                         "the wrapper reports the wrapped program's signature")
    (refine-expect-equal (length (program-tools picked)) 0 "it starts with no tools")
    (program-set-tools picked (list probe))
    (refine-expect-equal (jget (aref (program-tools gen) 0) "name") "probe"
                         "setting tools through the wrapper reaches the real program")
    (program-set-function-call-traces picked (vector (object "name" "probe" "status" "ok")))
    (refine-expect-equal (length (program-function-call-traces picked)) 1
                         "recorded tool calls are readable through the wrapper")
    (program-clear-function-call-traces picked)
    (refine-expect-equal (length (program-function-call-traces gen)) 0
                         "clearing through the wrapper clears the real program")
    (refine-expect-equal (program-native-sample-capable-p picked) nil
                         "a wrapper does not claim to sample natively")))

(defun refine-multi-sample-client (contents)
  "(values CLIENT SCRIPT): a client answering one request with many samples.

Pinned to a Chat Completions model, because that dialect is the one that
carries a sample count on the request and returns several choices for it.
The reply holds every CONTENTS entry at once, which is what a provider
really does when asked for n samples, so a generator that quietly asked for
one would come back with one and be caught here."
  (let ((script (make-client-script :replies nil)))
    (values (ai :name "openai" :model "gpt-5.4-mini" :api-key "test-key"
                :transport (lambda (url headers json-body)
                             (declare (ignore url headers))
                             (push json-body (cs-requests script))
                             (values (encode-json
                                      (object "choices"
                                              (coerce
                                               (loop for content in contents
                                                     for index from 0
                                                     collect (object "index" index
                                                                     "finish_reason" "stop"
                                                                     "message"
                                                                     (object "role" "assistant"
                                                                             "content" content)))
                                               'vector)
                                              "usage" (object "prompt_tokens" 5
                                                              "completion_tokens" 2
                                                              "total_tokens" 7)))
                                     200)))
            script)))

(define-refine-test test-native-sampling-asks-once-and-picks-the-best-sample
  ;; The generator negotiates native sampling now, so best-of-n defaults to
  ;; it.  The whole risk of that path is the one the serial test's comment
  ;; named: claiming the capability and then scoring a single candidate.
  ;; This pins all three things that stop it -- the count really reaches the
  ;; provider, one request serves every candidate, and the sample that wins
  ;; is the best-rewarded one rather than the first returned.
  (multiple-value-bind (client script)
      (refine-multi-sample-client '("Answer: one" "Answer: twotwo" "Answer: threethree"))
    (let* ((gen (ax "question:string -> answer:string")))
      (refine-expect (program-native-sample-capable-p gen)
                     "the generator reports native sampling")
      (let ((out (forward (best-of-n gen :n 3 :strategy :native-samples
                                         :reward-fn #'answer-length-reward)
                          client (object "question" "q"))))
        (refine-expect-equal (cs-request-count script) 1
                             "every candidate came from one provider call")
        (refine-expect-equal (jget (parse-json (cs-request script 0)) "n") 3
                             "and the request really asked for three samples")
        (refine-expect-equal (jget out "answer") "threethree"
                             "the best-rewarded sample wins, not the first returned")))))
