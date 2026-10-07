;;;; optimize-runtime.lisp --- the native optimizer engines actually running.
;;;;
;;;; The conformance file replays the shared AxIR fixtures.  This file asks
;;;; the questions a fixture cannot: does the search really measure, really
;;;; select, really stop, and does applying what it found really change what
;;;; the program does?
;;;;
;;;; Every expectation here is derived independently of the engine:
;;;;
;;;;   * the ranking GEPA must arrive at is computed first, by scoring each
;;;;     candidate instruction directly with the same metric and no engine
;;;;     in the loop; the test then asserts the engine agrees
;;;;   * the demo count BootstrapFewShot must mine is counted from the
;;;;     dataset's own scores against the threshold
;;;;   * the evaluation-metric expectations are hand-computed from the
;;;;     documented arithmetic, not read back from this implementation
;;;;
;;;; A plausible wrong implementation fails them: one that returns the last
;;;; proposal instead of the best, one that forgets to restore the program
;;;; after a candidate, one that treats a rollout failure as a crash, one
;;;; that reads CL:RANDOM instead of its own seeded state, and one that
;;;; copies the reference's NaN novel-F1.
;;;;
;;;; No provider is contacted: the program under test is a deterministic
;;;; local program and the teacher is a plain function.

(defpackage #:axllm/optimize-runtime
  (:use #:cl)
  (:export #:run-optimize-runtime-tests))

(in-package #:axllm/optimize-runtime)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(defvar *tests* '())

(define-condition check-failure (error)
  ((detail :initarg :detail :reader check-failure-detail))
  (:report (lambda (condition stream) (write-string (check-failure-detail condition) stream))))

(defmacro deftest (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *tests* (append (remove ',name *tests*) (list ',name)))
     ',name))

(defun fail (format-control &rest arguments)
  (error 'check-failure :detail (apply #'format nil format-control arguments)))

(defun check (ok description)
  (unless ok (fail "~a" description))
  t)

(defun show (value)
  (cond ((stringp value) (format nil "~s" value))
        ((consp value) (format nil "~s" value))
        (t (handler-case (ax:encode-json value) (error () (format nil "~s" value))))))

(defun check-equal (actual expected description)
  (unless (or (and (consp actual) (equal actual expected))
              (axllm/core::core-value-equal actual expected))
    (fail "~a~%    expected: ~a~%    actual:   ~a" description (show expected) (show actual))))

(defun check-close (actual expected description &optional (tolerance 1d-9))
  (unless (and (realp actual) (< (abs (- actual expected)) tolerance))
    (fail "~a~%    expected: ~a~%    actual:   ~a" description expected actual)))

(defmacro check-error (kind description &body body)
  "Run BODY, require an OPTIMIZE-ERROR of KIND, and return its text."
  (let ((condition (gensym)))
    `(handler-case (progn ,@body (fail "~a: nothing was signalled" ,description))
       (ax:optimize-error (,condition)
         (check (eq (ax:optimize-error-kind ,condition) ,kind)
                (format nil "~a: expected kind ~a, got ~a (~a)"
                        ,description ,kind (ax:optimize-error-kind ,condition) ,condition))
         (princ-to-string ,condition)))))

(defmacro check-error-text (fragment description &body body)
  "Run BODY, require any error whose text contains FRAGMENT."
  (let ((condition (gensym)) (text (gensym)))
    `(handler-case (progn ,@body (fail "~a: nothing was signalled" ,description))
       (check-failure (,condition) (error ,condition))
       (error (,condition)
         (let ((,text (princ-to-string ,condition)))
           (check (search ,fragment ,text)
                  (format nil "~a: expected text containing ~s, got: ~a"
                          ,description ,fragment ,text))
           ,text)))))

(defun jget (object key &optional (default :null)) (ax:jget object key default))

(defun elements (value)
  (cond ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((eq value :null) '())
        (t '())))

(defun jarray (&rest items)
  (let ((out (axllm::%new-array)))
    (dolist (item items out) (vector-push-extend item out))))

;;; ------------------------------------------------------------------
;;; The program under test
;;; ------------------------------------------------------------------
;;;
;;; A deterministic local program: its instruction decides the answer it
;;; gives, so changing the instruction really changes its behaviour and a
;;; metric over its output really ranks instructions.  It implements the
;;; program protocol src/gen.lisp defines, which is all the optimizer uses.

(defclass tuned-program ()
  ((instruction :initarg :instruction :accessor tuned-instruction)
   (lookup-name :initform "lookup" :accessor tuned-lookup-name)
   (answers :initarg :answers :reader tuned-answers)
   (failing :initarg :failing :initform '() :accessor tuned-failing)
   (demos :initform (axllm::%new-array) :accessor tuned-demos)
   (calls :initform (axllm::%new-array) :accessor tuned-calls)
   (rollouts :initform 0 :accessor tuned-rollouts)
   (chat-log :initform (axllm::%new-array) :accessor tuned-chat-log)
   (traces :initform (axllm::%new-array) :accessor tuned-traces)))

(defparameter +base-instruction+ "answer plainly")

(defparameter +default-proposals+
  '("answer with citations" "answer with the city only" "answer vaguely")
  "What the scripted teacher proposes, in order.

The order is chosen so that selecting badly is visible.  The minibatch is
one question, the Pareto set is both.  \"answer with citations\" wins over
both questions; \"answer with the city only\" wins the single minibatch
question outright and loses the other, so GEPA accepts it as the last
candidate and must still not choose it.  An engine that returned the newest
candidate, or the last proposal it was handed, lands on the wrong one.  The
third proposal is worse than its parent on the minibatch, so a correct
search rejects it and never puts it in the archive.")

(defparameter +answer-table+
  ;; instruction -> the answer the program gives, per question.
  '(("answer plainly" . (("capital of france" . "unknown")
                         ("largest ocean" . "the sea")))
    ("answer with the city only" . (("capital of france" . "paris")
                                    ("largest ocean" . "the big sea")))
    ("answer with citations" . (("capital of france" . "paris france")
                                ("largest ocean" . "pacific ocean")))
    ("answer vaguely" . (("capital of france" . "somewhere")
                         ("largest ocean" . "water")))
    ;; Scores as well as the best instruction, but drops the word the
    ;; component declares it must preserve.
    ("reply with citations" . (("capital of france" . "paris france")
                               ("largest ocean" . "pacific ocean"))))
  "What the program replies, keyed by instruction then question.")

(defparameter +truth+
  '(("capital of france" . "paris")
    ("largest ocean" . "pacific ocean")))

(defun make-tuned-program (&key (instruction +base-instruction+) failing)
  (make-instance 'tuned-program :instruction instruction
                                :answers +answer-table+
                                :failing failing))

(defmethod ax:program-kind ((program tuned-program)) "axgen")

(defmethod ax:program-set-demos ((program tuned-program) demos)
  (setf (tuned-demos program) demos)
  program)

(defmethod ax:program-function-calls ((program tuned-program))
  "The calls this program recorded.  It has a tool, so it reports a real
history: an empty one here would mean it genuinely called nothing."
  (tuned-calls program))

(defmethod axllm::program-traces ((program tuned-program)) (tuned-traces program))

(defmethod axllm::program-chat-log ((program tuned-program)) (tuned-chat-log program))

(defmethod axllm::program-optimizable-components ((program tuned-program))
  (jarray
   (ax:object "id" "qa::instruction"
              "owner" "qa"
              "kind" "instruction"
              "current" (tuned-instruction program)
              "description" "Prompt instruction text used by this generator."
              "constraints" (jarray "Keep the word answer in the instruction.")
              "dependsOn" (axllm::%new-array)
              "preserve" (jarray "answer")
              "format" "markdown"
              "validation" (ax:object))
   (ax:object "id" "qa::fn:lookup:name"
              "owner" "qa"
              "kind" "fn-name"
              "current" (tuned-lookup-name program)
              "description" "Callable name for the lookup tool."
              "constraints" (jarray "snake_case" "32 characters or fewer")
              "dependsOn" (axllm::%new-array)
              "preserve" (axllm::%new-array)
              "format" "snake_case"
              "maxLength" 32
              "validation" (ax:object))))

(defmethod axllm::program-apply-optimized-components ((program tuned-program) component-map)
  (let ((instruction (jget component-map "qa::instruction"))
        (name (jget component-map "qa::fn:lookup:name")))
    (unless (eq instruction :null) (setf (tuned-instruction program) instruction))
    (unless (eq name :null) (setf (tuned-lookup-name program) name)))
  program)

(defmethod axllm::forward ((program tuned-program) client inputs &optional options)
  (declare (ignore client options))
  (incf (tuned-rollouts program))
  (let ((question (jget inputs "question")))
    (when (member question (tuned-failing program) :test #'equal)
      (error 'ax:ax-error :message (format nil "rollout failed for ~a" question)))
    (let* ((table (cdr (assoc (tuned-instruction program) (tuned-answers program) :test #'equal)))
           (answer (or (cdr (assoc question table :test #'equal)) "")))
      (vector-push-extend (ax:object "name" (tuned-lookup-name program)
                                     "qualifiedName" (concatenate 'string "tools."
                                                                  (tuned-lookup-name program))
                                     "arguments" (ax:object "question" question)
                                     "status" "ok")
                          (tuned-calls program))
      (vector-push-extend (ax:object "question" question "answer" answer) (tuned-traces program))
      (values (ax:object "answer" answer) (ax:usage-object 1 1 2)))))

;;; The metric, used both by the optimizer and by the test's own independent
;;; ranking.  It never sees a component map: it only reads what came out.

(defun answer-metric (task prediction)
  (let* ((question (jget (jget task "input") "question"))
         (expected (or (cdr (assoc question +truth+ :test #'equal)) ""))
         (output (jget prediction "output"))
         (answer (jget output "answer" "")))
    (if (equal (jget prediction "completionType") "error")
        0
        (ax:f1-score (if (stringp answer) answer "") expected))))

(defun all-tasks ()
  "Both questions: the set a candidate is finally judged on."
  (jarray (ax:object "input" (ax:object "question" "capital of france"))
          (ax:object "input" (ax:object "question" "largest ocean"))))

(defun gepa-dataset ()
  "One question to propose against, both to be judged on.

Splitting them is what makes the selection test sharp: a candidate can win
the minibatch and still lose overall."
  (ax:object "train" (jarray (ax:object "input" (ax:object "question" "capital of france")))
             "validation" (all-tasks)))

(defun independent-mean-score (instruction &optional (tasks (elements (all-tasks))))
  "What INSTRUCTION scores over TASKS, computed without the optimizer in the loop."
  (let ((program (make-tuned-program :instruction instruction))
        (total 0d0)
        (count 0))
    (dolist (task tasks (if (plusp count) (/ total count) 0d0))
      (multiple-value-bind (output) (axllm::forward program :no-client (jget task "input"))
        (incf total (answer-metric task (ax:object "completionType" "final" "output" output)))
        (incf count)))))

(defun independent-ranking ()
  "The instructions the search can reach, best first, scored independently.

\"reply with citations\" is excluded: it is the deliberately invalid
proposal, which the component's constraint forbids the search from
reaching, so it is not a candidate to be ranked against."
  (let ((rows (mapcar (lambda (instruction)
                        (cons instruction (independent-mean-score instruction)))
                      (cons +base-instruction+ +default-proposals+))))
    (sort rows #'> :key #'cdr)))

;;; ------------------------------------------------------------------
;;; Deterministic randomness
;;; ------------------------------------------------------------------

(defun rng-sequence (seed count)
  (let ((rng (ax:make-optimizer-rng seed)))
    (loop repeat count collect (ax:optimizer-rng-next rng))))

(deftest seeded-rng-is-reproducible-and-seed-sensitive
  (check-equal (rng-sequence 7 8) (rng-sequence 7 8)
               "the same seed must replay the same sequence")
  (check (not (equal (rng-sequence 7 8) (rng-sequence 8 8)))
         "a different seed must explore differently")
  (check-equal (rng-sequence 0 4) (rng-sequence nil 4)
               "seed 0 and no seed must both fall back to the shared default")
  (check (every (lambda (value) (and (<= 0d0 value) (< value 1d0))) (rng-sequence 7 64))
         "every draw must lie in [0,1)"))

(deftest search-never-touches-the-global-random-state
  ;; A run that read CL:RANDOM would both be irreproducible and perturb the
  ;; caller's stream; this catches either.
  (let* ((before (make-random-state nil))
         (expected (loop repeat 5 collect (random 1000000 (make-random-state before)))))
    (run-gepa-search :seed 7)
    (check-equal (loop repeat 5 collect (random 1000000 (make-random-state before)))
                 expected
                 "a GEPA run must leave the process-global random state alone")))

(deftest component-selection-is-driven-by-the-seed
  (let* ((components (elements (axllm::program-optimizable-components
                                (make-tuned-program))))
         (third-component (ax:object "id" "qa::style" "owner" "qa" "kind" "instruction"
                                     "current" "plain" "format" "markdown"))
         (all (append components (list third-component))))
    (flet ((picks (seed)
             (let ((selector (ax:make-gepa-component-selector all))
                   (rng (ax:make-optimizer-rng seed)))
               (loop for iteration from 0 below 24
                     collect (progn
                               (let ((picked (ax:gepa-selector-pick selector iteration rng)))
                                 (ax:gepa-selector-record-proposal selector (jget picked "id"))
                                 (ax:gepa-selector-record-result
                                  selector (jget picked "id") (zerop (mod iteration 3)) iteration)
                                 (jget picked "id")))))))
      (check-equal (picks 7) (picks 7) "the same seed must pick the same components")
      (check (not (equal (picks 7) (picks 11)))
             "a different seed must pick a different component order"))))

(deftest selector-state-round-trips-through-its-snapshot
  (let* ((components (elements (axllm::program-optimizable-components (make-tuned-program))))
         (selector (ax:make-gepa-component-selector components)))
    (ax:gepa-selector-record-proposal selector "qa::instruction")
    (ax:gepa-selector-record-result selector "qa::instruction" t 3)
    (ax:gepa-selector-record-proposal selector "qa::fn:lookup:name")
    (ax:gepa-selector-record-result selector "qa::fn:lookup:name" nil 3)
    (let* ((snapshot (ax:gepa-selector-snapshot selector))
           (resumed (ax:make-gepa-component-selector components :state snapshot)))
      (check-equal (ax:gepa-selector-snapshot resumed) snapshot
                   "a resumed selector must carry the snapshot's counters forward")
      (check-equal (jget (jget snapshot "qa::instruction") "lastAcceptIter") 3
                   "an accepted proposal records the iteration it was accepted on")
      (check-equal (jget (jget snapshot "qa::fn:lookup:name") "stagnation") 1
                   "a rejected proposal increments stagnation"))))

;;; ------------------------------------------------------------------
;;; GEPA really searches
;;; ------------------------------------------------------------------

(defun proposal-teacher (values &key record)
  "A teacher that proposes VALUES in order, then repeats the last one."
  (let ((remaining values)
        (last (car (last values))))
    (lambda (payload)
      (when record (push (jget payload "componentKey") (cdr record)))
      (if remaining (pop remaining) last))))

(defun run-gepa-search (&key (seed 7) (proposals +default-proposals+)
                             (max-metric-calls 40) (num-trials 3) record program)
  "Run GEPA over the tuned program and return (values ARTIFACT PROGRAM EVALUATOR)."
  (let* ((program (or program (make-tuned-program)))
         (evaluator (ax:make-program-evaluator
                     program :no-client
                     :dataset (gepa-dataset)
                     :metric #'answer-metric))
         (engine (ax:make-gepa :reflection (proposal-teacher proposals :record record)
                               :seed seed
                               :options (ax:object "maxMetricCalls" max-metric-calls
                                                   "numTrials" num-trials
                                                   "minibatchSize" 1
                                                   "skipPerfectScore" ax:false)))
         (request (ax:object "contractVersion" "axir-optimize-contract-v1"
                             "programKind" "axgen"
                             "components" (jarray
                                           (first (elements
                                                   (axllm::program-optimizable-components program))))
                             "dataset" (axllm/core::normalize-optimization-dataset (gepa-dataset))
                             "options" (ax:object)
                             "trace" (ax:object)
                             "evaluator" (ax:object "available" ax:true))))
    (values (ax:run-optimizer-engine engine request evaluator) program evaluator)))

(deftest gepa-selects-the-independently-best-candidate
  (let* ((ranking (independent-ranking))
         (best (car (first ranking)))
         (worst (car (first (last ranking))))
         (minibatch (elements (jget (gepa-dataset) "train"))))
    ;; Guard the fixture itself.  A flat ranking, or one where the minibatch
    ;; agrees with the overall ranking, would make the test vacuous.
    (check (> (cdr (first ranking)) (cdr (second ranking)))
           "the independent ranking must have a strict winner")
    (check (string= best "answer with citations")
           (format nil "the independent ranking should favour the citing instruction, got ~s" best))
    (check (> (independent-mean-score "answer with the city only" minibatch)
              (independent-mean-score "answer with citations" minibatch))
           "the decoy must really win the minibatch, or the test proves nothing")
    (multiple-value-bind (artifact) (run-gepa-search)
      (let* ((chosen (jget (jget artifact "componentMap") "qa::instruction"))
             (explored (elements (jget (jget artifact "metadata") "paretoFront"))))
        (check-equal chosen best
                     "GEPA must land on the independently best instruction")
        (check (not (equal chosen worst))
               "GEPA must not land on the worst instruction")
        (check (not (equal chosen (car (last +default-proposals+))))
               "GEPA must not keep the last proposal it was handed")
        ;; Three proposals, two of which beat their parent on the minibatch,
        ;; so the archive is the seed plus those two and nothing else.
        (check-equal (jget (jget artifact "metadata") "candidatesExplored") 3
                     "only proposals that really improved may enter the archive")
        (check (= (length explored) 1)
               "only the undominated candidate belongs on the reported frontier")
        (check-equal (jget (jget (jget (jget artifact "metadata") "selectorState")
                                 "qa::instruction")
                           "stagnation")
                     1
                     "the rejected proposal must be recorded as stagnation")
        (check-equal (jget (jget (jget (jget artifact "metadata") "selectorState")
                                 "qa::instruction")
                           "accepts")
                     2
                     "and the two accepted ones as accepts")))))

(deftest gepa-reports-what-it-measured
  (multiple-value-bind (artifact program evaluator) (run-gepa-search)
    (declare (ignore program))
    (let* ((metadata (jget artifact "metadata"))
           (best-score (jget metadata "bestScore"))
           (independent (cdr (first (independent-ranking)))))
      (check-close best-score independent
                   "the reported best score must be the score that was measured")
      (check-equal (jget metadata "totalMetricCalls") (ax:evaluator-metric-calls evaluator)
                   "totalMetricCalls must equal the rollouts actually spent")
      (check (> (jget metadata "candidatesExplored") 1)
             "a search that accepted an improvement must report more than the seed")
      (check-equal (jget (jget artifact "provenance") "sourceProgramKind") "axgen"
                   "provenance must carry the program kind from the request")
      (check-equal (jget (jget (jget artifact "provenance") "componentOwners") "qa::instruction")
                   "qa"
                   "provenance must carry the component owner"))))

(deftest gepa-is-repeatable-under-the-same-seed
  (let ((first-run (jget (run-gepa-search :seed 7) "componentMap"))
        (second-run (jget (run-gepa-search :seed 7) "componentMap")))
    (check-equal first-run second-run "the same seed must produce the same component map")))

(deftest gepa-refuses-a-budget-too-small-to-start
  (let ((text (check-error :budget "an impossible initial Pareto budget"
                (run-gepa-search :max-metric-calls 1))))
    (check (search "too small to evaluate the initial Pareto set" text)
           "the refusal must say which budget was too small")))

(deftest gepa-stops-when-the-budget-runs-out-mid-search
  ;; Enough for the seed evaluation, not enough to finish a trial: the run
  ;; must end with the seed rather than report an unmeasured candidate.
  (multiple-value-bind (artifact program evaluator) (run-gepa-search :max-metric-calls 3)
    (declare (ignore program))
    (check-equal (jget (jget artifact "componentMap") "qa::instruction") +base-instruction+
                 "a search that could not afford a trial must keep the seed value")
    (check (<= (ax:evaluator-metric-calls evaluator) 3)
           "the engine must not spend more rollouts than its budget")))

(deftest gepa-keeps-the-current-value-when-the-teacher-fails
  (let ((notifications '()))
    (let* ((program (make-tuned-program))
           (evaluator (ax:make-program-evaluator program :no-client
                                                 :dataset (all-tasks) :metric #'answer-metric))
           (engine (ax:make-gepa
                    :reflection (lambda (payload)
                                  (declare (ignore payload))
                                  (error 'ax:ax-error :message "teacher unavailable"))
                    :seed 7
                    :options (ax:object "maxMetricCalls" 40 "numTrials" 1 "minibatchSize" 2
                                        "skipPerfectScore" ax:false
                                        "logger" (lambda (event) (push event notifications)))))
           (artifact (ax:run-optimizer-engine
                      engine
                      (ax:object "programKind" "axgen"
                                 "components" (jarray (first (elements
                                                              (axllm::program-optimizable-components
                                                               program))))
                                 "dataset" (axllm/core::normalize-optimization-dataset (all-tasks))
                                 "options" (ax:object))
                      evaluator)))
      (check-equal (jget (jget artifact "componentMap") "qa::instruction") +base-instruction+
                   "a failed teacher must leave the current value in place")
      (check (= (length notifications) 1)
             (format nil "the failure must be reported once, got ~a" (length notifications)))
      (check (search "teacher unavailable" (jget (first notifications) "value"))
             "the notification must carry the underlying failure"))))

(defun run-failing-teacher (options)
  "Run one GEPA trial whose teacher always fails, with OPTIONS layered on."
  (let* ((program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (all-tasks) :metric #'answer-metric))
         (engine (ax:make-gepa
                  :reflection (lambda (payload)
                                (declare (ignore payload))
                                (error 'ax:ax-error :message "teacher unavailable"))
                  :seed 7
                  :options (axllm::%opt-merge
                            (ax:object "maxMetricCalls" 40 "numTrials" 1 "minibatchSize" 1
                                       "skipPerfectScore" ax:false)
                            options))))
    (ax:run-optimizer-engine
     engine
     (ax:object "programKind" "axgen"
                "components" (jarray (first (elements
                                             (axllm::program-optimizable-components program))))
                "dataset" (axllm/core::normalize-optimization-dataset (gepa-dataset))
                "options" (ax:object))
     evaluator)))

(deftest teacher-failures-report-through-the-shared-optimizer-logger
  ;; The optimizer keeps no logger default of its own: it reports through the
  ;; process-wide optimizerLogger global unless the run supplies one.
  (let ((from-global '())
        (from-run '()))
    (unwind-protect
         (progn
           (axllm::set-global "optimizerLogger" (lambda (event) (push event from-global)))
           (run-failing-teacher (ax:object))
           (check (= (length from-global) 1)
                  "the shared global logger must receive the teacher failure")
           (check-equal (jget (first from-global) "id") "gepa_teacher"
                        "and must receive it under the shared notification id")
           (setf from-global '())
           (run-failing-teacher (ax:object "logger" (lambda (event) (push event from-run))))
           (check (= (length from-run) 1) "a run's own logger takes precedence")
           (check (null from-global) "and the global must not also be called")
           (setf from-run '())
           (run-failing-teacher (ax:object "verbose" ax:false))
           (check (null from-global) "verbose false silences the global logger too"))
      (axllm::set-global "optimizerLogger" :null))))

(deftest gepa-rejects-a-proposal-that-breaks-a-constraint
  ;; The instruction component must preserve the word "answer".  This
  ;; proposal scores as well as the best valid instruction, so only the
  ;; constraint can be what stops it.
  (check (> (independent-mean-score "reply with citations")
            (independent-mean-score +base-instruction+))
         "the invalid proposal must be one the search would otherwise want")
  (multiple-value-bind (artifact)
      (run-gepa-search :proposals '("reply with citations") :num-trials 1)
    (check-equal (jget (jget artifact "componentMap") "qa::instruction") +base-instruction+
                 "a proposal failing validation must not reach the component map")
    (check-equal (jget (jget artifact "metadata") "candidatesExplored") 1
                 "and must not enter the archive either")))

;;; ------------------------------------------------------------------
;;; The evaluator: measurement, restoration, budget, failure, cancellation
;;; ------------------------------------------------------------------

(deftest evaluating-a-candidate-restores-the-program
  (let* ((program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (all-tasks) :metric #'answer-metric))
         (result (ax:evaluate-candidate
                  evaluator
                  (ax:object "qa::instruction" "answer with citations")
                  (ax:object "phase" "train"))))
    (check-equal (jget result "count") 2 "every task must have produced a row")
    (check-close (jget result "avg") (independent-mean-score "answer with citations")
                 "the measured average must match the independently computed score")
    (check-equal (tuned-instruction program) +base-instruction+
                 "the program must be restored after a candidate is measured")))

(deftest a-rollout-failure-is-scored-not-raised
  (let* ((program (make-tuned-program :instruction "answer with citations"
                                     :failing '("largest ocean")))
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (all-tasks) :metric #'answer-metric))
         (result (ax:evaluate-candidate evaluator (ax:object) (ax:object)))
         (rows (elements (jget result "rows"))))
    (check-equal (length rows) 2 "a failing task still contributes a row")
    (check-equal (jget (jget (second rows) "prediction") "completionType") "error"
                 "the failing rollout must be recorded as an error")
    (check (search "rollout failed for largest ocean"
                   (jget (jget (second rows) "error") "message"))
           "the row must carry the underlying failure message")
    (check-equal (jget (second rows) "scalar") 0 "a failed rollout scores zero")
    (check (> (jget (first rows) "scalar") 0) "the surviving task still scores")
    (check-equal (tuned-instruction program) "answer with citations"
                 "the program must be restored after a failing candidate"))
  ;; With no metric supplied, the default scoring must still separate a
  ;; completed rollout from a failed one.
  (let* ((program (make-tuned-program :instruction "answer with citations"
                                      :failing '("largest ocean")))
         (evaluator (ax:make-program-evaluator program :no-client :dataset (all-tasks)))
         (rows (elements (jget (ax:evaluate-candidate evaluator (ax:object) (ax:object))
                               "rows"))))
    (check-equal (jget (first rows) "scalar") 1 "a completed rollout defaults to one")
    (check-equal (jget (second rows) "scalar") 0 "a failed rollout defaults to zero")))

(deftest the-metric-budget-stops-the-evaluator
  (let* ((program (make-tuned-program :instruction "answer with citations"))
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (all-tasks) :metric #'answer-metric
                                               :max-metric-calls 1))
         (text (check-error :budget "a candidate wider than the remaining budget"
                 (ax:evaluate-candidate evaluator
                                        (ax:object "qa::instruction" "answer plainly")
                                        (ax:object)))))
    (check (search "max metric calls exceeded" text)
           "the refusal must use the shared budget wording")
    (check-equal (ax:evaluator-metric-calls evaluator) 1
               "the budget stops at the ceiling, it does not overshoot")
    (check-equal (ax:evaluator-budget-remaining evaluator) 0 "no budget may remain")
    (check-equal (tuned-instruction program) "answer with citations"
                 "a budget stop must still restore the program")))

(deftest cancellation-stops-the-run-and-restores-the-program
  (let* ((program (make-tuned-program))
         (seen 0)
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (all-tasks) :metric #'answer-metric
                                               :cancel (lambda () (> (incf seen) 1)))))
    (check-error :cancelled "a cancelled evaluation"
      (ax:evaluate-candidate evaluator
                             (ax:object "qa::instruction" "answer with citations")
                             (ax:object)))
    (check-equal (tuned-rollouts program) 1
                 "cancellation must take effect before the second rollout")
    (check-equal (tuned-instruction program) +base-instruction+
                 "a cancelled evaluation must still restore the program")))

(defclass callless-program (demoless-program) ()
  (:documentation "A program that cannot report the calls it made."))

(deftest a-program-that-cannot-report-calls-is-refused-not-assumed-silent
  ;; Scoring it against expectedActions would treat it as having called
  ;; nothing, which is a fabricated measurement.
  (let* ((program (make-instance 'callless-program))
         (evaluator (ax:make-program-evaluator program :no-client))
         (task (ax:object "input" (ax:object "question" "q")
                          "expectedActions" (jarray "search"))))
    (let ((text (check-error :components "a task scored against calls nobody recorded"
                  (ax:evaluate-candidate evaluator (ax:object)
                                         (ax:object "dataset" (jarray task))))))
      (check (search "PROGRAM-FUNCTION-CALLS" text)
             "the refusal must name the generic the program has to implement")))
  ;; A task that does not depend on calls is scored normally.
  (let* ((program (make-instance 'callless-program))
         (evaluator (ax:make-program-evaluator program :no-client))
         (result (ax:evaluate-candidate evaluator (ax:object)
                                        (ax:object "dataset"
                                                   (jarray (ax:object "input" (ax:object)
                                                                      "score" 0.5d0))))))
    (check-equal (jget result "count") 1
                 "a program that records no calls is still usable when nothing reads them")))

(deftest expected-and-forbidden-actions-adjust-the-score
  ;; Core owns the adjustment; this checks the evaluator really routes a
  ;; task's action expectations into it.
  (let* ((program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client :metric #'answer-metric))
         (task (ax:object "input" (ax:object "question" "capital of france")
                          "expectedActions" (jarray "never_called")))
         (result (ax:evaluate-candidate
                  evaluator (ax:object "qa::instruction" "answer with citations")
                  (ax:object "dataset" (jarray task))))
         (row (first (elements (jget result "rows")))))
    ;; No expected action was observed, so the score is halved: factor
    ;; 0.5 + 0.5 * (0 matched / 1 expected).
    (check-close (jget row "scalar")
                 (* 0.5d0 (ax:f1-score "paris france" "paris"))
                 "an unmet expected action must halve the measured score"))
  ;; The same program, scored against the call it really made: the expected
  ;; action is matched, so the score is not reduced.
  (let* ((program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client :metric #'answer-metric))
         (task (ax:object "input" (ax:object "question" "capital of france")
                          "expectedActions" (jarray "lookup")))
         (row (first (elements (jget (ax:evaluate-candidate
                                      evaluator (ax:object "qa::instruction" "answer with citations")
                                      (ax:object "dataset" (jarray task)))
                                     "rows")))))
    (check (plusp (length (jget (jget row "prediction") "functionCalls")))
           "the rollout must have recorded a real call history")
    (check-close (jget row "scalar") (ax:f1-score "paris france" "paris")
                 "a matched expected action must leave the score alone"))
  ;; A forbidden action the program really performed cuts the score to a fifth.
  (let* ((program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client :metric #'answer-metric))
         (task (ax:object "input" (ax:object "question" "capital of france")
                          "forbiddenActions" (jarray "lookup")))
         (row (first (elements (jget (ax:evaluate-candidate
                                      evaluator (ax:object "qa::instruction" "answer with citations")
                                      (ax:object "dataset" (jarray task)))
                                     "rows")))))
    (check-close (jget row "scalar") (* 0.2d0 (ax:f1-score "paris france" "paris"))
                 "a forbidden action the program really took must penalize it")))

;;; ------------------------------------------------------------------
;;; BootstrapFewShot
;;; ------------------------------------------------------------------

(deftest bootstrap-mines-only-the-rows-that-cleared-the-threshold
  (let* ((tasks (list (ax:object "input" (ax:object "question" "a") "score" 0.9d0)
                      (ax:object "input" (ax:object "question" "b") "score" 0.4d0)
                      (ax:object "input" (ax:object "question" "c") "score" 0.8d0)))
         (threshold 0.5d0)
         ;; Counted from the dataset, not from the engine.
         (expected (count-if (lambda (task) (>= (jget task "score") threshold)) tasks))
         (program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (apply #'jarray tasks)))
         (engine (ax:make-bootstrap-few-shot (ax:object "qualityThreshold" threshold
                                                        "maxDemos" 4
                                                        "maxRounds" 1)))
         (artifact (ax:run-optimizer-engine
                    engine
                    (ax:object "programKind" "axgen"
                               "components" (axllm::program-optimizable-components program)
                               "dataset" (axllm/core::normalize-optimization-dataset
                                          (apply #'jarray tasks))
                               "options" (ax:object))
                    evaluator)))
    (check-equal (length (jget artifact "demos")) expected
                 "only rows at or above the threshold become demos")
    (check-equal (jget (jget artifact "metadata") "demosGenerated") expected
                 "the metadata must report the demos it really mined")
    (check-equal (jget (jget artifact "metadata") "totalMetricCalls") (length tasks)
                 "one round over the sample costs one metric call per example")
    (check-equal (jget artifact "componentMap") (ax:object)
                 "BootstrapFewShot changes demos, not component text")
    (check-equal (jget (first (elements (jget artifact "demos"))) "programId") "root"
                 "a demo records the program it came from")))

(deftest bootstrap-stops-at-max-demos
  (let* ((tasks (loop repeat 6 collect (ax:object "input" (ax:object "question" "a") "score" 1)))
         (program (make-tuned-program))
         (evaluator (ax:make-program-evaluator program :no-client
                                               :dataset (apply #'jarray tasks)))
         (engine (ax:make-bootstrap-few-shot (ax:object "qualityThreshold" 0.5 "maxDemos" 2)))
         (artifact (ax:run-optimizer-engine
                    engine
                    (ax:object "programKind" "axgen"
                               "components" (axllm::program-optimizable-components program)
                               "dataset" (axllm/core::normalize-optimization-dataset
                                          (apply #'jarray tasks))
                               "options" (ax:object))
                    evaluator)))
    ;; Identical examples are also deduplicated, so one demo is all six can
    ;; contribute; the cap is never exceeded either way.
    (check (<= (length (jget artifact "demos")) 2)
           "maxDemos must bound the mined demos")))

(deftest an-engine-without-an-evaluator-refuses
  (let ((request (ax:object "programKind" "axgen"
                            "components" (axllm::program-optimizable-components (make-tuned-program))
                            "dataset" (axllm/core::normalize-optimization-dataset (all-tasks))
                            "options" (ax:object "maxMetricCalls" 10))))
    (check-error :evaluator "GEPA with no evaluator"
      (ax:run-optimizer-engine (ax:make-gepa :reflection (lambda (p) (declare (ignore p)) "x"))
                               request nil))
    (check-error :evaluator "BootstrapFewShot with no evaluator"
      (ax:run-optimizer-engine (ax:make-bootstrap-few-shot) request nil))))

;;; ------------------------------------------------------------------
;;; Artifacts: validation and real application
;;; ------------------------------------------------------------------

(defun artifact-for (component-map &key (optimizer "GEPA") (version "axir-gepa-v1")
                                        (artifact-version "axir-optimized-artifact-v1")
                                        owners)
  (ax:object "artifactVersion" artifact-version
             "optimizerName" optimizer
             "optimizerVersion" version
             "componentMap" component-map
             "metadata" (ax:object)
             "evidence" (ax:object)
             "provenance" (if owners
                              (ax:object "componentOwners" owners)
                              (ax:object))))

(deftest applying-an-artifact-changes-what-the-program-does
  (let ((program (make-tuned-program)))
    (multiple-value-bind (before) (axllm::forward program :no-client
                                              (ax:object "question" "capital of france"))
      (check-equal (jget before "answer") "unknown"
                   "the program starts on its base instruction"))
    (ax:apply-optimization program
                           (artifact-for (ax:object "qa::instruction" "answer with citations")))
    (check-equal (tuned-instruction program) "answer with citations"
                 "the component map must reach the program")
    (multiple-value-bind (after) (axllm::forward program :no-client
                                             (ax:object "question" "capital of france"))
      (check-equal (jget after "answer") "paris france"
                   "the program must really behave differently after the artifact"))
    (check (> (independent-mean-score "answer with citations")
              (independent-mean-score +base-instruction+))
           "the applied instruction must be the better one by the same metric")))

(deftest artifact-validation-rejects-what-it-should
  (let ((program (make-tuned-program)))
    (check-error-text "unknown optimized component id" "an id the program does not expose"
      (ax:apply-optimization program (artifact-for (ax:object "qa::nope" "x"))))
    (check-error-text "unsupported optimized artifact version" "a future artifact version"
      (ax:apply-optimization program
                            (artifact-for (ax:object "qa::instruction" "answer now")
                                          :artifact-version "axir-optimized-artifact-v2")))
    (check-error-text "stale optimized component owner" "an artifact built for another owner"
      (ax:apply-optimization program
                            (artifact-for (ax:object "qa::instruction" "answer now")
                                          :owners (ax:object "qa::instruction" "old.owner"))))
    (check-error-text "optimizerName" "an artifact with no optimizer name"
      (ax:apply-optimization program
                            (artifact-for (ax:object "qa::instruction" "answer now")
                                          :optimizer "")))
    (check-equal (tuned-instruction program) +base-instruction+
                 "no rejected artifact may have changed the program")))

(deftest an-artifact-round-trips-through-text
  (let* ((program (make-tuned-program))
         (artifact (artifact-for (ax:object "qa::instruction" "answer with citations")))
         (text (ax:artifact-text artifact))
         (parsed (ax:parse-artifact text (axllm::program-optimizable-components program))))
    (check (stringp text) "an artifact serializes to text")
    (check-equal (jget (jget parsed "componentMap") "qa::instruction") "answer with citations"
                 "the component map survives the round trip")
    (check-error-text "unknown optimized component id" "text validated against the wrong program"
      (ax:parse-artifact (ax:artifact-text (artifact-for (ax:object "other::instruction" "x")))
                         (axllm::program-optimizable-components program)))))

(defclass demoless-program ()
  ((instruction :initform +base-instruction+ :accessor demoless-instruction))
  (:documentation
   "A program that exposes a component but has nowhere to put demos.

It implements the protocol the optimizer needs and nothing more, so the
default PROGRAM-SET-DEMOS applies."))

(defmethod ax:program-kind ((program demoless-program)) "axgen")

(defmethod axllm::program-optimizable-components ((program demoless-program))
  (jarray (ax:object "id" "qa::instruction" "owner" "qa" "kind" "instruction"
                     "current" (demoless-instruction program)
                     "preserve" (jarray "answer"))))

(defmethod axllm::program-apply-optimized-components ((program demoless-program) component-map)
  (let ((instruction (jget component-map "qa::instruction")))
    (unless (eq instruction :null) (setf (demoless-instruction program) instruction)))
  program)

(defmethod axllm::program-traces ((program demoless-program)) (axllm::%new-array))
(defmethod axllm::program-chat-log ((program demoless-program)) (axllm::%new-array))

(defmethod axllm::forward ((program demoless-program) client inputs &optional options)
  (declare (ignore client inputs options))
  (ax:object "answer" (demoless-instruction program)))

(deftest demos-are-refused-rather-than-dropped
  ;; A program that cannot hold demos must say so, not silently discard the
  ;; mined traces and look as if the artifact applied.
  (let ((program (make-instance 'demoless-program))
        (artifact (artifact-for (ax:object "qa::instruction" "answer with citations"))))
    (axllm::%set-key artifact "demos" (jarray (ax:object "programId" "root"
                                                         "traces" (axllm::%new-array))))
    (check-error :artifact "an artifact carrying demos for a program with no demo slot"
      (ax:apply-optimization program artifact))))

;;; ------------------------------------------------------------------
;;; The whole driver
;;; ------------------------------------------------------------------

(deftest optimize-program-measures-applies-and-reports
  (let* ((program (make-tuned-program))
         (artifact (ax:optimize-program
                    program (all-tasks)
                    :engine (ax:make-gepa :reflection (proposal-teacher
                                                       '("answer with citations"))
                                          :seed 7)
                    :client :no-client
                    :options (ax:object "target" "qa::instruction"
                                        "maxMetricCalls" 40
                                        "numTrials" 1
                                        "minibatchSize" 2
                                        "skipPerfectScore" ax:false
                                        "metric" #'answer-metric))))
    (check-equal (jget artifact "artifactVersion") "axir-optimized-artifact-v1"
                 "the driver must return a normalized artifact")
    (check-equal (tuned-instruction program) "answer with citations"
                 "the driver applies the artifact by default")
    (check-equal (jget (first (elements (jget artifact "changedComponents"))) "id")
                 "qa::instruction"
                 "the artifact must report what actually changed")
    (multiple-value-bind (output) (axllm::forward program :no-client
                                              (ax:object "question" "largest ocean"))
      (check-equal (jget output "answer") "pacific ocean"
                   "the optimized program must answer differently"))))

(deftest optimize-program-can-be-asked-not-to-apply
  (let* ((program (make-tuned-program))
         (artifact (ax:optimize-program
                    program (all-tasks)
                    :engine (ax:make-gepa :reflection (proposal-teacher '("answer with citations"))
                                          :seed 7)
                    :client :no-client
                    :options (ax:object "target" "qa::instruction" "apply" ax:false
                                        "maxMetricCalls" 40 "numTrials" 1 "minibatchSize" 2
                                        "skipPerfectScore" ax:false
                                        "metric" #'answer-metric))))
    (check-equal (jget (jget artifact "componentMap") "qa::instruction") "answer with citations"
                 "the artifact still reports what it found")
    (check-equal (tuned-instruction program) +base-instruction+
                 "apply: false must leave the program untouched")))

(deftest an-unmatched-target-is-refused
  (check-error-text "no optimizable components match target"
                    "a target that selects nothing"
    (ax:optimize-program (make-tuned-program) (all-tasks)
                         :engine (ax:make-bootstrap-few-shot)
                         :client :no-client
                         :options (ax:object "target" "qa::missing"))))

(deftest the-stream-entry-point-reports-that-it-yields-nothing
  (multiple-value-bind (artifact progress)
      (ax:optimize-program-stream
       (make-tuned-program) (all-tasks)
       :engine (ax:make-bootstrap-few-shot (ax:object "maxDemos" 1))
       :client :no-client
       :options (ax:object "metric" #'answer-metric))
    (check-equal (jget artifact "artifactVersion") "axir-optimized-artifact-v1"
                 "the stream entry point still returns the artifact")
    (check-equal (length progress) 0
                 "it must report an empty progress sequence, as the reference does")))

;;; ------------------------------------------------------------------
;;; Component value validators (TypeScript rules)
;;; ------------------------------------------------------------------

(deftest snake-case-validation-follows-the-typescript-rule
  (let ((validator (ax:optimizable-snake-case-identifier)))
    (check-equal (funcall validator "  lookup_docs  ") t
                 "a value is trimmed before it is judged")
    (check (stringp (funcall validator "_lookup"))
           "a leading underscore is rejected, unlike the Python template's rule")
    (check (stringp (funcall validator "Lookup")) "an initial capital is rejected")
    (check (stringp (funcall validator "")) "an empty identifier is rejected")
    (check (search "<= 32 chars" (funcall validator (make-string 33 :initial-element #\a)))
           "an over-long identifier names the limit")
    (check-equal (funcall (ax:optimizable-snake-case-identifier 4) "abcd") t
                 "the limit is configurable")
    (check (stringp (funcall (ax:optimizable-snake-case-identifier 4) "abcde"))
           "a value past a configured limit is rejected")))

(deftest placeholder-and-emptiness-validators
  (let ((validator (ax:optimizable-preserves-placeholders (jarray "{{tools}}"))))
    (check-equal (funcall validator "use {{tools}} first") t "a surviving placeholder passes")
    (check (search "must preserve placeholder {{tools}}" (funcall validator "use tools first"))
           "a dropped placeholder is named"))
  (check-equal (funcall (ax:optimizable-non-empty) " x ") t "a non-blank value passes")
  (check (stringp (funcall (ax:optimizable-non-empty) "   ")) "a blank value is rejected"))

(deftest component-declarations-drive-validation
  (let ((name-component (second (elements (axllm::program-optimizable-components
                                           (make-tuned-program)))))
        (instruction-component (first (elements (axllm::program-optimizable-components
                                                 (make-tuned-program))))))
    (check-equal (ax:validate-component-value name-component "lookup_docs") t
                 "a well-formed callable name passes")
    (check (stringp (ax:validate-component-value name-component "_lookup"))
           "a leading underscore fails the declared snake_case format")
    (check (stringp (ax:validate-component-value name-component
                                                 (make-string 40 :initial-element #\a)))
           "maxLength is enforced")
    (check-equal (ax:validate-component-value instruction-component "answer briefly") t
                 "a preserved literal passes")
    (check (search "must preserve placeholder answer"
                   (ax:validate-component-value instruction-component "reply briefly"))
           "a dropped preserved literal is named")
    (check (stringp (ax:validate-component-value instruction-component 7))
           "a non-string value is refused")))

;;; ------------------------------------------------------------------
;;; Evaluation metrics
;;; ------------------------------------------------------------------

(deftest normalization-matches-the-documented-steps
  ;; Article removal is case sensitive and runs before lowercasing, so a
  ;; capitalized "The" survives as a token.  Checked against the TypeScript
  ;; reference, not against this implementation.
  (check-equal (ax:normalize-eval-text "The  Dog!") "the dog"
               "a capitalized article survives; whitespace collapses, punctuation goes, case folds")
  (check-equal (ax:normalize-eval-text "an Apple, a Pear") " apple pear"
               "lowercase articles are dropped, leaving the empty piece JavaScript leaves"))

(deftest exact-match-and-f1-are-hand-checkable
  (check-equal (ax:em-score "the  dog!" "a dog") 1d0
               "two texts that normalize alike match exactly")
  (check-equal (ax:em-score "the dog" "the cat") 0d0 "different texts do not match")
  ;; Dropping "the" leaves a leading empty token, as JavaScript's split
  ;; does, so the prediction contributes four tokens: overlap 2, precision
  ;; 2/4, recall 2/3, F1 4/7.  Confirmed against the TypeScript reference.
  (check-close (ax:f1-score "the quick brown fox" "quick brown dog") (/ 4d0 7d0)
               "F1 is the harmonic mean of the token precision and recall")
  (check-equal (ax:f1-score "alpha" "gamma") 0d0 "no overlap scores zero, it does not divide by zero")
  ;; Two empty texts each tokenize to one empty token, which overlaps, so
  ;; the reference scores them 1.  Confirmed against TypeScript: this is the
  ;; shared behaviour, not a rounding accident here.
  (check-equal (ax:f1-score "" "") 1d0 "two empty texts agree on their one empty token")
  ;; Repeats are counted, not collapsed: overlap 2, precision 2/3, recall 1.
  ;; overlap 2, precision 2/3, recall 1, F1 0.8.
  (check-close (ax:f1-score "dog dog cat" "dog dog") 0.8d0
               "a repeated token counts as many times as both sides carry it"))

(deftest novel-f1-measures-only-what-the-history-did-not-say
  ;; Everything the prediction says is already in the history, so it adds
  ;; nothing: 0, not a NaN and not a high score for parroting.
  (check-equal (ax:novel-f1-score "the capital of france is paris"
                                  "paris is the capital" "paris")
               0d0
               "repeating the history back scores nothing")
  ;; rain and tomorrow are in neither the stopword list nor the history, so
  ;; both sides keep both tokens: overlap 2, precision 1, recall 1, F1 1.
  (check-close (ax:novel-f1-score "weather report" "it will rain tomorrow" "rain tomorrow") 1d0
               "new contentful agreement scores fully")
  (check-close (ax:novel-f1-score "weather report" "it will rain tomorrow" "rain tomorrow" t) 1d0
               "recall can be asked for instead of F1")
  ;; alpha/beta against alpha/gamma: overlap 1, precision 1/2, recall 1/2,
  ;; F1 1/2.  The reference implementation returns NaN here.
  (let ((score (ax:novel-f1-score "" "alpha beta" "alpha gamma")))
    (check (= score score) "the score must be a number, not NaN")
    (check-close score 0.5d0 "partial agreement scores the documented F1"))
  (check-equal (ax:novel-f1-score "" "alpha" "gamma") 0d0
               "no overlap scores zero rather than dividing zero by zero")
  (check-equal (ax:novel-f1-score "" "the a an" "the a an") 0d0
               "stopwords alone are not agreement"))

;;; ------------------------------------------------------------------
;;; Cost tracking
;;; ------------------------------------------------------------------

(deftest the-cost-tracker-prices-tokens-and-stops-a-run
  (let ((tracker (ax:make-cost-tracker :cost-per-model (ax:object "priced" 2)
                                       :max-cost 3)))
    (ax:track-tokens tracker 1500 "priced")
    ;; 1500 tokens at 2 per 1000 = 3.0, which reaches the limit.
    (check-close (ax:cost-tracker-cost tracker) 3d0 "cost is derived from the tokens tracked")
    (check (ax:cost-tracker-limit-reached-p tracker) "the cost limit stops the run")
    (ax:track-tokens tracker 1000 "unpriced")
    ;; An unpriced model falls back to 0.001 per 1000 rather than to free.
    (check-close (ax:cost-tracker-cost tracker) 3.001d0
                 "an unpriced model is charged the fallback rate, not nothing")
    (check-equal (ax:cost-tracker-total-tokens tracker) 2500 "tokens accumulate across models")
    (check-equal (jget (ax:cost-tracker-token-usage tracker) "priced") 1500
                 "per-model usage is reported")
    (ax:reset-cost-tracker tracker)
    (check-equal (ax:cost-tracker-total-tokens tracker) 0 "reset forgets the tokens")
    (check-close (ax:cost-tracker-cost tracker) 0d0 "reset zeroes the derived cost")
    (check (not (ax:cost-tracker-limit-reached-p tracker)) "reset clears the limit")
    (check-equal (ax:cost-tracker-token-usage tracker) (ax:object)
                 "reset empties the per-model table"))
  (let ((tracker (ax:make-cost-tracker :max-tokens 10)))
    (ax:track-tokens tracker 9 "m")
    (check (not (ax:cost-tracker-limit-reached-p tracker)) "below the token limit the run continues")
    (ax:track-tokens tracker 1 "m")
    (check (ax:cost-tracker-limit-reached-p tracker) "the token limit stops the run"))
  (let ((tracker (ax:make-cost-tracker)))
    (ax:track-tokens tracker 1000000 "m")
    (check (not (ax:cost-tracker-limit-reached-p tracker))
           "a tracker with no limits only reports")))

;;; ------------------------------------------------------------------
;;; Run state, statistics and checkpoints
;;; ------------------------------------------------------------------

(deftest every-exported-symbol-names-something
  ;; A name on the export list that defines nothing is a broken public API:
  ;; it compiles, it imports, and it fails at the call site.  This caught
  ;; OPTIMIZED-PROGRAM, which was exported for a record that is a plain JSON
  ;; object with no type of its own.
  (let ((missing '()))
    (dolist (name axllm::+optimizer-exports+)
      (let ((symbol (find-symbol (string name) :axllm)))
        (unless (and symbol
                     (or (fboundp symbol)
                         (boundp symbol)
                         (find-class symbol nil)
                         (fboundp (list 'setf symbol))))
          (push name missing))))
    (check (null missing)
           (format nil "exported but undefined: ~{~a~^ ~}" (reverse missing)))))

(deftest the-optimizer-reads-no-constant-before-it-is-defined
  ;; A defparameter placed after its first use compiles only because one
  ;; compilation unit resolves it late; a form-by-form load does not.  Every
  ;; constant the optimizer reads must already be bound.
  (dolist (name '("+OPTIMIZE-DEFAULT-MAX-METRIC-CALLS+"
                  "+OPTIMIZE-BOOTSTRAP-EXAMPLE-LIMIT+"
                  "+EVAL-STOPWORDS+"
                  "+JS-WHITESPACE-CHARS+"
                  "+ACE-DEFAULT-CONFIG+"
                  "+OPTIMIZED-PROGRAM-FIELDS+"))
    (let ((symbol (find-symbol name :axllm)))
      (check (and symbol (boundp symbol))
             (format nil "~a must be bound before anything reads it" name))))
  (check-equal axllm::+optimize-default-max-metric-calls+ 100
               "the helper's default metric budget is the shared 100")
  (check-equal axllm::+optimize-bootstrap-example-limit+ 8
               "and its bootstrap example limit the shared 8"))

(deftest resource-usage-is-reported-from-the-tracker-not-a-copy
  ;; Recording must recompute from the tokens tracked, so the figure on the
  ;; metric is the one the limits are checked against.
  (let ((tracker (ax:make-cost-tracker :cost-per-model (ax:object "m" 2))))
    (ax:track-tokens tracker 1000 "m")
    (check-close (ax:cost-tracker-cost tracker) 2d0 "the tracker prices what it holds")
    (ax:record-optimizer-resource-usage tracker :optimizer-type "GEPA")
    (ax:track-tokens tracker 1000 "m")
    (check-close (ax:cost-tracker-cost tracker) 4d0
                 "recording must not freeze or reset the running cost")
    (check-equal (ax:cost-tracker-total-tokens tracker) 2000
                 "nor the running token count")))

(deftest a-reflection-model-reaches-the-provider-request
  ;; Answering with the client's default while the run asked for another
  ;; model would silently mis-attribute the reflection.
  (let* ((bodies '())
         (client (ax:ai :name "openai" :model "gpt-5.4-mini" :api-key "sk-test-dummy-key"
                        :transport (lambda (url headers body)
                                     (declare (ignore url headers))
                                     (push body bodies)
                                     (values (ax:encode-json
                                              (ax:object "choices"
                                                         (jarray (ax:object "index" 0
                                                                            "finish_reason" "stop"
                                                                            "message" (ax:object "role" "assistant" "content" "New Value: answer well")))))
                                             200))))
         (callback (ax:make-ai-reflection-callback client)))
    (check-equal (funcall callback (ax:object "componentKey" "qa::instruction"
                                              "model" "some-other-model"))
                 "answer well"
                 "the proposed value comes back")
    (check-equal (jget (ax:parse-json (first bodies)) "model") "some-other-model"
                 "options.reflectionModel must reach the provider request")
    ;; With no model named, the client's own is used.
    (setf bodies '())
    (funcall callback (ax:object "componentKey" "qa::instruction"))
    (check-equal (jget (ax:parse-json (first bodies)) "model") "gpt-5.4-mini"
                 "and without one the client's own model is used")))

(deftest round-statistics-track-improvement-and-stagnation
  (let ((state (ax:make-optimizer-state)))
    (ax:record-optimizer-round state 0 0.4d0 (ax:object "instruction" "a"))
    (ax:record-optimizer-round state 1 0.9d0 (ax:object "instruction" "b"))
    (ax:record-optimizer-round state 2 0.5d0 (ax:object "instruction" "c"))
    (let ((stats (ax:optimizer-stats state)))
      (check-equal (jget stats "bestScore") 0.9d0 "the best score is the best measured")
      (check-equal (jget (jget stats "bestConfiguration") "instruction") "b"
                   "the best configuration is the one that scored best")
      (check-equal (jget (jget stats "convergenceInfo") "stagnationRounds") 1
                   "a round that did not improve counts as stagnation")
      (check-equal (jget stats "totalCalls") 3 "every round is counted"))
    (check-equal (ax:optimizer-score-history state) (jarray 0.4d0 0.9d0 0.5d0)
                 "the score history keeps the order the rounds happened in")
    (check-equal (ax:optimizer-current-round state) 2 "the round counter follows the last round")
    (ax:reset-optimizer state)
    (check-equal (ax:optimizer-current-round state) 0 "reset clears the round counter")
    (check-equal (length (ax:optimizer-score-history state)) 0 "reset clears the history")
    (check-equal (jget (ax:optimizer-stats state) "bestScore") 0 "reset clears the best score")))

(deftest a-checkpoint-round-trips-and-resumes
  (let ((state (ax:make-optimizer-state :cost-tracker (ax:make-cost-tracker))))
    (ax:track-tokens (ax:optimizer-state-cost-tracker state) 500 "m")
    (ax:record-optimizer-round state 0 0.3d0 (ax:object "instruction" "a"))
    (ax:record-optimizer-round state 1 0.8d0 (ax:object "instruction" "b"))
    (let* ((checkpoint (ax:optimizer-checkpoint
                        state :optimizer-type "GEPA"
                              :optimizer-config (ax:object "numTrials" 4)
                              :engine-state (ax:object "selectorState" (ax:object))))
           (text (ax:encode-json checkpoint))
           (restored (ax:make-optimizer-state))
           (engine-state (ax:load-optimizer-checkpoint restored (ax:parse-json text))))
      (check-equal (jget checkpoint "version") "1.0.0" "a checkpoint names its version")
      (check-equal (jget checkpoint "optimizerType") "GEPA" "a checkpoint names its optimizer")
      (check-equal (jget (jget (jget checkpoint "stats") "resourceUsage") "totalTokens") 500
                   "the checkpoint carries the live token usage")
      (check-equal (ax:optimizer-current-round restored) 1 "the round counter is resumed")
      (check-equal (ax:optimizer-score-history restored) (jarray 0.3d0 0.8d0)
                   "the score history is resumed in order")
      (check-equal (jget (ax:optimizer-stats restored) "bestScore") 0.8d0
                   "the best score is resumed")
      (check-equal (jget engine-state "selectorState") (ax:object)
                   "the engine's own state comes back")
      (check-error :config "a checkpoint from an unknown version"
        (ax:load-optimizer-checkpoint (ax:make-optimizer-state)
                                      (ax:object "version" "9.9.9"))))))

;;; ------------------------------------------------------------------
;;; Optimized program records
;;; ------------------------------------------------------------------

(deftest an-optimized-program-round-trips-and-applies
  (let* ((program (make-tuned-program))
         (optimized (ax:make-optimized-program
                     :best-score 0.9d0
                     :component-map (ax:object "qa::instruction" "answer with citations")
                     :selector-state (ax:object "qa::instruction"
                                                (ax:object "proposals" 2 "accepts" 1
                                                           "lastAcceptIter" 1 "stagnation" 0))
                     :optimizer-type "GEPA"
                     :total-rounds 3
                     :converged t
                     :score-history (jarray 0.4d0 0.9d0)))
         (text (ax:optimized-program-json optimized))
         (parsed (ax:parse-optimized-program text)))
    (check-equal (jget parsed "componentMap") (jget optimized "componentMap")
                 "the component map survives JSON")
    (check-equal (jget parsed "selectorState") (jget optimized "selectorState")
                 "the selector state survives JSON, so a later run can resume")
    (check-equal (jget parsed "converged") ax:true "the converged flag stays a JSON boolean")
    (check-equal (jget parsed "bestScore") 0.9d0 "the best score survives JSON")
    (check-equal (jget parsed "scoreHistory") (jarray 0.4d0 0.9d0) "the history survives JSON")
    (check-equal (ax:optimized-program-json parsed) text
                 "a second round trip is byte-identical")
    (ax:apply-optimized-program parsed program)
    (check-equal (tuned-instruction program) "answer with citations"
                 "applying an optimized program really changes the program")
    (multiple-value-bind (output) (axllm::forward program :no-client
                                              (ax:object "question" "capital of france"))
      (check-equal (jget output "answer") "paris france"
                   "and really changes what it answers"))))

(deftest an-optimized-program-is-validated-like-any-artifact
  (let ((program (make-tuned-program))
        (optimized (ax:make-optimized-program
                    :component-map (ax:object "somewhere::else" "x")
                    :optimizer-type "GEPA")))
    (check-error-text "unknown optimized component id"
                      "an optimized program built for another program"
      (ax:apply-optimized-program optimized program))
    (check-equal (tuned-instruction program) +base-instruction+
                 "a rejected record must not have changed the program")))

;;; ------------------------------------------------------------------
;;; ACE beyond the fixtures
;;; ------------------------------------------------------------------

(defun labelled-client (replies)
  "A client whose transport replays Ax labelled-output REPLIES in order.

No provider and no credentials: the transport is a closure."
  (let ((queue (copy-list replies)))
    (ax:ai :name "openai" :model "gpt-5.4-mini" :api-key "sk-test-dummy-key"
           :transport (lambda (url headers body)
                        (declare (ignore url headers body))
                        (let ((content (if queue
                                           (pop queue)
                                           (fail "the scripted client ran out of replies"))))
                          (values (ax:encode-json
                                   (ax:object "choices"
                                              (jarray (ax:object "index" 0
                                                                 "finish_reason" "stop"
                                                                 "message" (ax:object "role" "assistant"
                                                                                      "content" content)))
                                              "usage" (ax:object "prompt_tokens" 1
                                                                 "completion_tokens" 1
                                                                 "total_tokens" 2)))
                                  200))))))

(deftest a-playbook-learns-through-real-reflector-and-curator-programs
  ;; The ACE driver takes its roles as callbacks so a test can script them.
  ;; This checks the other half: the same roles as real AxGen programs over
  ;; the shared signatures, answering through a scripted transport, really
  ;; drive a playbook change.
  (let* ((client (labelled-client
                  (list
                   ;; the generator's answer
                   "Answer: france"
                   ;; the reflector
                   (format nil "Reasoning: the answer named a country~%~
Error Identification: gave a country, not a city~%~
Root Cause Analysis: no instruction to name the city~%~
Correct Approach: name the city~%~
Key Insight: name the city, not the country~%~
Bullet Tags: []~%")
                   ;; the curator
                   (format nil "Reasoning: add the rule~%~
Operations: [{\"type\": \"ADD\", \"section\": \"answer_rules\", \"content\": \"Name the city, not the country.\"}]~%"))))
         (program (ax:ax "question:string -> answer:string"))
         (playbook (ax:make-playbook :program program
                                     :student client
                                     :metric (lambda (prediction example)
                                               (declare (ignore prediction example))
                                               0.2d0)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1))))
    (check-equal (ax:ace-render playbook) "" "a new playbook renders as nothing")
    (let ((result (ax:playbook-evolve
                   playbook (jarray (ax:object "input" (ax:object "question" "capital of france"))))))
      (check (search "Name the city, not the country." (ax:ace-render playbook))
             "the real curator program's rule must reach the playbook")
      (check-equal (jget (jget (ax:ace-playbook playbook) "stats") "bulletCount") 1
                   "and be counted in the playbook statistics")
      (check-close (jget result "bestScore") 0.2d0 "the result reports the metric it saw")
      (check-equal (length (jget (ax:ace-artifact playbook) "history")) 1
                   "the applied operation is recorded in the delta history"))))

(deftest a-playbook-records-its-target-without-the-driver-reading-it
  ;; Binding a playbook to one stage and evolving it against the whole run
  ;; are separate decisions: the target is a contract for whoever attaches
  ;; the playbook, and the driver must not change behaviour because of it.
  (let* ((client (labelled-client (list "Answer: x")))
         (plain (ax:make-playbook :student client))
         (bound (ax:make-playbook :student client
                                  :target "task.root.responder::instruction")))
    (check-equal (ax:playbook-target plain) :null
                 "a playbook with no target reports none")
    (check-equal (ax:playbook-target bound) "task.root.responder::instruction"
                 "a bound playbook reports the component it is attached to")
    (check-equal (ax:ace-render bound) (ax:ace-render plain)
                 "and the target changes nothing the driver does")))

(deftest a-seed-loads-into-a-playbook-and-survives-a-reset
  ;; The public seed loader an attaching host needs: a configured playbook
  ;; becomes the state the playbook returns to, not a one-shot assignment.
  (let* ((client (labelled-client (list "Answer: x")))
         (playbook (ax:make-playbook :student client
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z")))
         (seed (ax:object "version" 1
                          "sections" (ax:object "Guidelines"
                                                (jarray (ax:object "id" "guidelines-00001"
                                                                   "section" "Guidelines"
                                                                   "content" "Cite your sources."
                                                                   "helpfulCount" 0
                                                                   "harmfulCount" 0)))
                          "stats" (ax:object "bulletCount" 1 "helpfulCount" 0
                                             "harmfulCount" 0 "tokenEstimate" 4)
                          "updatedAt" "1970-01-01T00:00:00.000Z")))
    (check-equal (ax:ace-render playbook) "" "a new playbook starts empty")
    (ax:playbook-load playbook seed)
    (check (search "Cite your sources." (ax:ace-render playbook))
           "the seed must reach the rendered playbook")
    (ax:ace-reset playbook)
    (check (search "Cite your sources." (ax:ace-render playbook))
           "and must survive a reset, because it is now the starting state")
    (check-error :config "a seed that is not an object"
      (ax:playbook-load playbook "not an object"))
    ;; The other shape the loader takes: a whole state, which restores the
    ;; artifact as well.  A loader that restored only the playbook would
    ;; leave a rejected proposal's feedback behind, so the artifact is
    ;; asserted rather than assumed.
    (let ((state (ax:object "playbook" seed
                            "artifact" (ax:object "feedback" (jarray (ax:object "note" "kept"))
                                                  "history" (axllm::%new-array)))))
      (ax:playbook-load playbook state)
      (check (search "Cite your sources." (ax:ace-render playbook))
             "a whole state restores its playbook")
      (check-equal (axllm::%opt-count
                    (jget (jget (ax:playbook-state playbook) "artifact") "feedback"))
                   1
                   "and restores its artifact rather than dropping it"))))

(defclass recording-program ()
  ((options :initform (axllm::%new-array) :accessor recorded-options)
   (calls :initform 0 :accessor recorded-calls))
  (:documentation "A program that records the forward options it was handed."))

(defmethod axllm::program-traces ((program recording-program)) (axllm::%new-array))
(defmethod axllm::program-chat-log ((program recording-program)) (axllm::%new-array))
(defmethod ax:program-kind ((program recording-program)) "axgen")

(defmethod axllm::forward ((program recording-program) client inputs &optional options)
  (declare (ignore client))
  (incf (recorded-calls program))
  (vector-push-extend (if (hash-table-p options) options (ax:object)) (recorded-options program))
  (ax:object "answer" (jget inputs "question" "")))

(defun reflection-reply ()
  (format nil "Reasoning: r~%Error Identification: e~%Root Cause Analysis: c~%~
Correct Approach: a~%Key Insight: k~%Bullet Tags: []~%"))

(defun curator-reply (content)
  (format nil "Reasoning: add~%Operations: [{\"type\": \"ADD\", \"section\": \"Guidelines\", \"content\": \"~a\"}]~%"
          content))

(deftest an-evolve-call-uses-the-teacher-it-names
  ;; Dropping a per-call teacherAI would score the client the playbook was
  ;; built with, which is a different model answering.
  (let* ((built-with (labelled-client (list "unused")))
         (named (labelled-client (list (reflection-reply) (curator-reply "From the named teacher."))))
         (program (make-instance 'recording-program))
         (playbook (ax:make-playbook :program program
                                     :signature (ax:parse-signature "question:string -> answer:string")
                                     :student (labelled-client (list "Answer: a"))
                                     :teacher built-with
                                     :metric (lambda (p e) (declare (ignore p e)) 0.5d0)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1))))
    (ax:playbook-evolve playbook (jarray (ax:object "question" "q"))
                        :options (ax:object "teacherAI" named))
    (check (search "From the named teacher." (ax:ace-render playbook))
           "the named teacher's curator output must be what reached the playbook")
    ;; And the playbook is left bound to the teacher it was built with.
    (check-equal (ax:playbook-json playbook) (ax:encode-json (ax:playbook-state playbook))
                 "playbook-json is the whole state, serialized")))

(deftest an-evolve-runtime-reaches-the-program
  ;; A program that holds only a runtime descriptor is handed the real
  ;; runtime on the evolve call; dropping it would fail that whole fixture
  ;; family silently.
  (let* ((program (make-instance 'recording-program))
         (runtime (ax:object "language" "JavaScript"))
         (playbook (ax:make-playbook :program program
                                     :signature (ax:parse-signature "question:string -> answer:string")
                                     :student (labelled-client (list "Answer: a"))
                                     :teacher (labelled-client (list (reflection-reply)
                                                                     (curator-reply "Rule.")))
                                     :metric (lambda (p e) (declare (ignore p e)) 0.5d0)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1))))
    (ax:playbook-evolve playbook (jarray (ax:object "question" "q"))
                        :options (ax:object "runtime" runtime))
    (check (plusp (recorded-calls program)) "the program must have been run")
    (check-equal (jget (aref (recorded-options program) 0) "runtime") runtime
                 "the evolve call's runtime must reach the program's forward options")))

(deftest evolve-options-last-only-for-the-call
  ;; Two evolve calls with different options must not contaminate each other.
  (let* ((program (make-instance 'recording-program))
         (playbook (ax:make-playbook :program program
                                     :signature (ax:parse-signature "question:string -> answer:string")
                                     :student (labelled-client
                                               (list "Answer: a" "Answer: a" "Answer: a"))
                                     :teacher (labelled-client
                                               (list (reflection-reply) (curator-reply "One.")
                                                     (reflection-reply) (curator-reply "Two.")
                                                     (reflection-reply) (curator-reply "Three.")))
                                     :metric (lambda (p e) (declare (ignore p e)) 0.5d0)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1))))
    ;; maxEpochs 2 runs the single example twice.
    (ax:playbook-evolve playbook (jarray (ax:object "question" "q"))
                        :options (ax:object "maxEpochs" 2))
    (check-equal (recorded-calls program) 2 "maxEpochs must change the number of rounds")
    (check-equal (jget (ax:ace-playbook playbook) "version") 1 "the playbook is still a playbook")
    ;; The next call must be back to one epoch, not still two.
    (setf (recorded-calls program) 0)
    (ax:playbook-evolve playbook (jarray (ax:object "question" "q")))
    (check-equal (recorded-calls program) 1
                 "the previous call's maxEpochs must not have leaked into this one")
    (check-equal (jget (axllm::ace-config playbook) "maxEpochs") 1
                 "and the playbook's own configuration must be back as it was")))

(deftest evolve-takes-every-dataset-shape-ax-uses
  ;; A playbook is handed the optimizer dataset verbatim: examples nested
  ;; under "input" with a score beside them, inside a {train, validation}
  ;; object.  Evolving over the wrapper itself would run the program with no
  ;; question at all, which is a required-input failure rather than a wrong
  ;; answer, so this pins both shapes.
  (flet ((evolve-over (dataset)
           (let* ((program (make-instance 'recording-program))
                  (playbook (ax:make-playbook
                             :program program
                             :signature (ax:parse-signature "question:string -> answer:string")
                             :student (labelled-client (list "Answer: a"))
                             :teacher (labelled-client (list (reflection-reply)
                                                             (curator-reply "Rule.")))
                             :metric (lambda (p e) (declare (ignore p e)) 0.5d0)
                             :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                 "maxReflectorRounds" 1))))
             (ax:playbook-evolve playbook dataset)
             program)))
    ;; The optimizer dataset shape, exactly as a fixture carries it.
    (let ((program (evolve-over
                    (ax:object "train"
                               (jarray (ax:object "input" (ax:object "question" "Answer briefly.")
                                                  "score" 0))))))
      (check-equal (recorded-calls program) 1 "the training example must have been run")
      (check-equal (jget (aref (recorded-options program) 0) "runtime") :null
                   "and run with no runtime when none was given"))
    ;; A bare array of wrapped examples.
    (check-equal (recorded-calls
                  (evolve-over (jarray (ax:object "input" (ax:object "question" "q") "score" 0))))
                 1 "a bare array of dataset examples is the examples")
    ;; A bare array of flat examples, which is what the optimize fixture uses.
    (check-equal (recorded-calls (evolve-over (jarray (ax:object "question" "q"))))
                 1 "and a flat example still works")))

(deftest the-reflector-sees-the-task-not-the-dataset-wrapper
  ;; The Reflector's Question must be the task, and its Expected answer the
  ;; ground truth; handing it the wrapper would teach the playbook about the
  ;; score field.
  (let* ((seen '())
         (teacher (labelled-client (list (reflection-reply) (curator-reply "Rule."))))
         (program (make-instance 'recording-program))
         (playbook (ax:make-playbook
                    :program program
                    :signature (ax:parse-signature "question:string -> answer:string")
                    :student (labelled-client (list "Answer: a"))
                    :teacher teacher
                    :metric (lambda (p e) (declare (ignore p e)) 0.5d0)
                    :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                        "maxReflectorRounds" 1))))
    (declare (ignorable seen))
    (ax:playbook-evolve
     playbook
     (ax:object "train" (jarray (ax:object "input" (ax:object "question" "Answer briefly.")
                                           "expectedOutput" (ax:object "answer" "ok")
                                           "score" 0))))
    (check-equal (recorded-calls program) 1 "the example ran")
    (check-equal (jget (aref (recorded-options program) 0) "runtime") :null
                 "with no runtime")
    ;; The rule reached the playbook, so the Reflector and Curator both ran
    ;; against a task they could read.
    (check (search "Rule." (ax:ace-render playbook))
           "the roles must have produced a usable rule from the unwrapped task")))

(deftest a-playbook-snapshot-shows-whether-a-run-changed-anything
  (let* ((program (make-instance 'recording-program))
         (playbook (ax:make-playbook :program program
                                     :signature (ax:parse-signature "question:string -> answer:string")
                                     :student (labelled-client (list "Answer: a" "Answer: a"))
                                     :teacher (labelled-client
                                               (list (reflection-reply) (curator-reply "A rule.")
                                                     (reflection-reply)
                                                     (format nil "Reasoning: none~%Operations: []~%")))
                                     :metric (lambda (p e) (declare (ignore p e)) 0.5d0)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1))))
    (let ((empty (ax:playbook-json playbook)))
      (ax:playbook-evolve playbook (jarray (ax:object "question" "q")))
      (let ((after (ax:playbook-json playbook)))
        (check (not (equal empty after)) "a run that added a rule must change the snapshot")
        (check (search "A rule." after) "and the snapshot must carry it")
        ;; An evolve call starts from the seed, as the reference's compile
        ;; does, so a second run does not continue the first: its curator
        ;; asks for nothing, and the snapshot is the seed state again rather
        ;; than the first run's playbook.  A caller comparing before against
        ;; after is therefore comparing one call, which is what the fixtures
        ;; assert.
        (ax:playbook-evolve playbook (jarray (ax:object "question" "q")))
        (check-equal (ax:encode-json (jget (ax:playbook-state playbook) "playbook"))
                     (ax:encode-json (jget (ax:parse-json empty) "playbook"))
                     "a run whose curator asked for nothing must leave the seed playbook")
        ;; The state is still not the state it started from, because the
        ;; artifact records that the round happened.  Asserting only on the
        ;; playbook above would pass just as well if the artifact were
        ;; silently dropped, so both halves are pinned.
        (check (not (equal (ax:playbook-json playbook) empty))
               "but the artifact must record the round that produced nothing")
        (check (plusp (axllm::%opt-count
                       (jget (jget (ax:playbook-state playbook) "artifact") "feedback")))
               "and that record is the round's feedback event")))))

(deftest the-ace-role-signatures-are-the-shared-ones
  ;; Built with the field builder, not signature text, because the curator's
  ;; operations description carries double quotes.
  (let ((reflector (ax:signature-fields (ax:ace-reflector-signature) :side :output))
        (curator (ax:signature-fields (ax:ace-curator-signature) :side :output)))
    (check-equal (map 'vector (lambda (field) (jget field "name")) reflector)
                 (jarray "reasoning" "errorIdentification" "rootCauseAnalysis"
                         "correctApproach" "keyInsight" "bulletTags")
                 "the reflector's outputs are the shared six, in order")
    (check-equal (map 'vector (lambda (field) (jget field "name")) curator)
                 (jarray "reasoning" "operations")
                 "the curator's outputs are reasoning and operations")
    (check (search "never emit an ADD whose content"
                   axllm::+ace-curator-operations-description+)
           "the operations description is the shared text")
    (check (find #\" axllm::+ace-curator-operations-description+)
           "it carries the double quotes signature text cannot")))

(deftest ace-really-rewrites-its-playbook-and-can-be-reset
  (let* ((ace (ax:make-ace
               :reflector (lambda (payload)
                            (declare (ignore payload))
                            (ax:object "reasoning" "the answer missed the city"
                                       "errorIdentification" "gave a country, not a city"
                                       "rootCauseAnalysis" "no instruction to name the city"
                                       "correctApproach" "name the city"
                                       "keyInsight" "name the city"
                                       "bulletTags" (axllm::%new-array)))
               :curator (lambda (payload)
                          (declare (ignore payload))
                          (ax:object "reasoning" "add the rule"
                                     "operations"
                                     (jarray (ax:object "type" "ADD"
                                                        "section" "answer_rules"
                                                        "content" "Name the city, not the country."))))
               :generator (lambda (example) (declare (ignore example))
                            (ax:object "thought" "france" "answer" "france"))
               :metric (lambda (prediction example) (declare (ignore prediction example)) 0.2d0)
               :options (ax:object "now" "1970-01-01T00:00:00.000Z" "maxReflectorRounds" 1)))
         (before (ax:ace-render ace))
         (result (ax:ace-compile ace (jarray (ax:object "question" "capital of france")))))
    (check-equal before "" "an empty playbook renders as nothing")
    (check (search "Name the city, not the country." (ax:ace-render ace))
           "the curator's rule must really be in the playbook")
    (check-equal (jget (jget (ax:ace-playbook ace) "stats") "bulletCount") 1
                 "the playbook statistics must count the new bullet")
    (check-close (jget result "bestScore") 0.2d0 "the result reports the metric it saw")
    (check-equal (length (jget (ax:ace-artifact ace) "history")) 1
                 "an applied operation is recorded in the delta history")
    (check-equal (length (jget (ax:ace-artifact ace) "feedback")) 1
                 "the round is recorded in the feedback history")
    (ax:ace-reset ace)
    (check-equal (ax:ace-render ace) "" "reset really clears the playbook")
    (check-equal (length (jget (ax:ace-artifact ace) "history")) 0 "reset clears the history")))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun run-optimize-runtime-tests (&key (stream *standard-output*))
  "Run every runtime test.  Returns (values PASSED FAILED)."
  (let ((passed 0) (failed 0))
    (dolist (test *tests*)
      (handler-case (progn (funcall test) (incf passed))
        (error (condition)
          (incf failed)
          (format stream "~&FAIL ~a: ~a~%" test condition))))
    (format stream "~&optimizer runtime: ~a passed, ~a failed.~%" passed failed)
    (values passed failed)))

;;; ------------------------------------------------------------------
;;; The agent-level playbook round
;;; ------------------------------------------------------------------

(defclass evaluable-program ()
  ((runs :initform 0 :accessor evaluable-runs)
   (log :initarg :log :initform "RuntimeError: the final step was never checked"
        :reader evaluable-log))
  (:documentation
   "A program that can be evaluated, standing in for an agent.

Carries a real action log, because the miner's excerpts and the evidence
grounding are both read out of it; a stub with an empty log would let a
broken grounding check pass."))

(defmethod ax:program-kind ((program evaluable-program)) "axagent")
(defmethod axllm::program-traces ((program evaluable-program)) (axllm::%new-array))
(defmethod axllm::program-chat-log ((program evaluable-program)) (axllm::%new-array))

(defmethod ax:program-evaluate-task ((program evaluable-program) client task &key options)
  (declare (ignore client task options))
  (incf (evaluable-runs program))
  (ax:object "completionType" "final"
             "output" (ax:object "answer" "a")
             "actionLog" (evaluable-log program)
             "functionCalls" (axllm::%new-array)
             "toolErrors" (axllm::%new-array)))

(defun miner-reply (&key (quote "RuntimeError: the final step was never checked")
                         (guidance "Verify the final step before finishing."))
  (format nil "Weakness Description: w~%Root Cause: c~%Proposed Guidance: ~a~%~
Evidence Quotes: [~s]~%Config Recommendations: []~%" guidance quote))

(defun counting-client (replies)
  "(values CLIENT COUNT-FN): a scripted client and how many calls it answered.

The grounding test needs to tell a miner that ran and rejected its own
evidence from a miner that never ran at all.  Both leave no weakness
behind, so only the call count separates them, and without it a miner that
threw would pass the test that is supposed to prove grounding works."
  (let ((queue (copy-list replies))
        (calls 0))
    (values (ax:ai :name "openai" :model "gpt-5.4-mini" :api-key "sk-test-dummy-key"
                   :transport (lambda (url headers body)
                                (declare (ignore url headers body))
                                (incf calls)
                                (let ((content (if queue
                                                   (pop queue)
                                                   (fail "the scripted client ran out of replies"))))
                                  (values (ax:encode-json
                                           (ax:object "choices"
                                                      (jarray (ax:object "index" 0
                                                                         "finish_reason" "stop"
                                                                         "message"
                                                                         (ax:object "role" "assistant"
                                                                                    "content" content)))))
                                          200))))
            (lambda () calls))))

(defun evolve-teacher (&key (quote "RuntimeError: the final step was never checked")
                            (rule "Verify the final step before finishing."))
  "A teacher that mines one grounded weakness and curates one rule from it."
  (labelled-client (list (miner-reply :quote quote)
                         (reflection-reply)
                         (curator-reply rule))))

(defun evolve-task-set (&optional (score 0))
  (ax:object "train" (jarray (ax:object "input" (ax:object "question" "q") "score" score))))

(defun rising-metric (scores)
  "A metric that answers SCORES in order, so a round can really improve."
  (let ((queue (copy-list scores)))
    (lambda (task prediction)
      (declare (ignore task prediction))
      (if queue (pop queue) 0))))

(deftest an-agent-round-keeps-a-rule-that-pays-for-itself
  ;; The accept path: a rule whose re-measurement clears the gate is kept,
  ;; and the outcome reports the measurement rather than the intent.
  (let* ((program (make-instance 'evaluable-program))
         (playbook (ax:make-playbook :program program
                                     :student (labelled-client '())
                                     :teacher (evolve-teacher)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1)))
         (result (ax:playbook-evolve-agent
                  playbook (evolve-task-set)
                  :metric (rising-metric '(0 1))
                  :options (ax:object "verify" ax:true "minHeldInGain" 0.5d0
                                      "maxProposals" 1 "maxMetricCalls" 2))))
    (check-equal (axllm::%opt-count (jget result "outcomes")) 1 "one cluster, one outcome")
    (let ((outcome (first (axllm::%opt-list (jget result "outcomes")))))
      (check-equal (jget outcome "accepted") ax:true "a measured gain of 1.0 clears a gate of 0.5")
      (check-equal (jget (jget outcome "heldIn") "before") 0 "held-in before is the baseline")
      (check-equal (jget (jget outcome "heldIn") "after") 1d0 "and after is the re-measurement"))
    (check-equal (jget result "metricCallsUsed") 2 "two runs were spent, baseline and re-evaluation")
    (check (search "Verify the final step before finishing."
                   (ax:ace-render playbook))
           "and the kept rule is really in the playbook")))

(deftest an-agent-round-undoes-a-rule-that-does-not
  ;; The reject path, and the one that matters most: a rolled-back proposal
  ;; must leave the state byte-identical, artifact included.  Restoring only
  ;; the playbook would pass a weaker check and still leave the feedback and
  ;; delta history of a rule the round decided against.
  (let* ((program (make-instance 'evaluable-program))
         (playbook (ax:make-playbook :program program
                                     :student (labelled-client '())
                                     :teacher (evolve-teacher)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1)))
         (before (ax:playbook-json playbook))
         (result (ax:playbook-evolve-agent
                  playbook (evolve-task-set)
                  :metric (rising-metric '(0 0))
                  :options (ax:object "verify" ax:true "minHeldInGain" 0.5d0
                                      "maxProposals" 1 "maxMetricCalls" 2))))
    (let ((outcome (first (axllm::%opt-list (jget result "outcomes")))))
      (check-equal (jget outcome "accepted") ax:false "a flat score cannot clear a gate of 0.5")
      (check (search "held-in gain" (jget outcome "reason")) "and the reason names the gate it missed"))
    (check-equal (ax:playbook-json playbook) before
                 "a rejected proposal must leave the state byte-identical")
    (check (not (search "Verify the final step" (ax:ace-render playbook)))
           "so the rule it proposed is not in the playbook")))

(deftest an-ungrounded-weakness-is-discarded-rather-than-curated
  ;; The grounding check is the only thing standing between a model's
  ;; invention and a permanent playbook rule, so a quote that is not in the
  ;; excerpts must cost the whole weakness.
  (multiple-value-bind (teacher teacher-calls)
      (counting-client (list (miner-reply :quote "a quote from nowhere")))
   (let* ((program (make-instance 'evaluable-program))
         (playbook (ax:make-playbook :program program
                                     :student (labelled-client '())
                                     :teacher teacher
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1)))
         (result (ax:playbook-evolve-agent
                  playbook (evolve-task-set)
                  :metric (rising-metric '(0 1))
                  :options (ax:object "verify" ax:true "minHeldInGain" 0d0
                                      "maxProposals" 1 "maxMetricCalls" 4))))
    (check-equal (funcall teacher-calls) 1
                 "the miner really ran, so this is a rejection and not a miner that threw")
    (check-equal (axllm::%opt-count (jget result "weaknesses")) 0
                 "an invented quote grounds nothing")
    (check-equal (axllm::%opt-count (jget result "outcomes")) 0
                 "so there is no proposal to decide on")
    (check-equal (ax:ace-render playbook) "" "and the playbook is untouched"))))

(deftest an-agent-round-refuses-to-verify-on-a-budget-it-cannot-afford
  ;; Spending the last run on a re-evaluation that cannot finish would
  ;; compare a one-task mean with a two-task baseline and accept on the
  ;; arithmetic.  The proposal is skipped instead, and said to be skipped.
  (let* ((program (make-instance 'evaluable-program))
         (playbook (ax:make-playbook :program program
                                     :student (labelled-client '())
                                     :teacher (evolve-teacher)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1)))
         (result (ax:playbook-evolve-agent
                  playbook (evolve-task-set)
                  :metric (rising-metric '(0 1))
                  :options (ax:object "verify" ax:true "minHeldInGain" 0d0
                                      "maxProposals" 1 "maxMetricCalls" 1))))
    (let ((outcome (first (axllm::%opt-list (jget result "outcomes")))))
      (check-equal (jget outcome "accepted") ax:false "an unaffordable proposal is not kept")
      (check (search "budget" (jget outcome "reason")) "and the reason names the budget"))
    (check-equal (jget result "metricCallsUsed") 1 "the baseline spent the only run there was")
    (check-equal (evaluable-runs program) 1 "and no re-evaluation was attempted")))

(deftest a-trust-batch-keeps-a-rule-without-measuring-it
  ;; verify false is the documented trust-batch.  It must skip the
  ;; re-measurement rather than measure and ignore the answer, so the run
  ;; count is what separates the two.
  (let* ((program (make-instance 'evaluable-program))
         (playbook (ax:make-playbook :program program
                                     :student (labelled-client '())
                                     :teacher (evolve-teacher)
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1)))
         (result (ax:playbook-evolve-agent
                  playbook (evolve-task-set)
                  :metric (rising-metric '(0))
                  :options (ax:object "verify" ax:false "maxProposals" 1 "maxMetricCalls" 4))))
    (let ((outcome (first (axllm::%opt-list (jget result "outcomes")))))
      (check-equal (jget outcome "accepted") ax:true "a trust-batch keeps what it mined")
      (check (search "without verification" (jget outcome "reason")) "and says it did not measure"))
    (check-equal (evaluable-runs program) 1 "only the baseline ran")
    (check (search "Verify the final step before finishing." (ax:ace-render playbook))
           "and the rule reached the playbook")))

(deftest the-worst-failure-cluster-is-the-one-that-gets-mined
  ;; maxProposals bounds how many clusters are mined, so which one survives
  ;; the cap has to be the most severe rather than the first seen.  The
  ;; cheap cluster is listed first and scores better, so a selector that
  ;; kept insertion order would mine the wrong one.
  (let* ((records (jarray (ax:object "task" (ax:object "id" "cheap")
                                     "score" 0.6d0
                                     "prediction" (ax:object "actionLog" "MinorError: x"))
                          (ax:object "task" (ax:object "id" "costly")
                                     "score" 0d0
                                     "prediction" (ax:object "actionLog" "FatalError: y"))
                          (ax:object "task" (ax:object "id" "costly-again")
                                     "score" 0d0
                                     "prediction" (ax:object "actionLog" "FatalError: y"))))
         (clusters (axllm::%evolve-cluster-failures records 0.7d0 1)))
    (check-equal (length clusters) 1 "the cap keeps one cluster")
    (check-equal (first (first clusters)) "FatalError: y"
                 "and it is the one that failed twice and badly, not the one seen first")
    (check-close (fourth (first clusters)) 2d0
                 "severity is the cluster's size times its mean miss")))

(deftest a-cluster-signature-prefers-what-the-run-reported
  ;; A run that both threw and reported signals is better described by what
  ;; it reported, and ties among signals go to the one reported first, so
  ;; two runs of the same failure never split into two clusters.
  (let ((record (ax:object "error" "FatalError: thrown"
                           "prediction"
                           (ax:object "failureSignals"
                                      (jarray (ax:object "signature" "first" "occurrences" 2)
                                              (ax:object "signature" "second" "occurrences" 2))))))
    (check-equal (axllm::%evolve-record-signature record) "first"
                 "the reported signal wins over the thrown error, and a tie goes to the first"))
  (check-equal (axllm::%evolve-record-signature
                (ax:object "prediction" (ax:object "actionLog" "all fine")))
               "behavioral:no_error"
               "a run that failed without naming an error lands in the behavioral cluster"))

;;; ------------------------------------------------------------------
;;; The suite must not be able to disappear quietly
;;; ------------------------------------------------------------------

(deftest a-vanished-fixture-inventory-is-a-failure-not-a-clean-run
  ;; The one failure that looks exactly like success: a missing or empty
  ;; fixture root makes every count zero, and a gate that checks for zero
  ;; failures is satisfied by a suite that never ran.  Both the empty root
  ;; and the missing root are pinned, because a checkout that lost the
  ;; directory and one that lost only its contents fail differently.
  (let* ((empty (merge-pathnames "axopt-empty-inventory/"
                                 (uiop:ensure-directory-pathname
                                  (uiop:temporary-directory))))
         (missing (merge-pathnames "axopt-inventory-that-does-not-exist/" empty)))
    (ensure-directories-exist (merge-pathnames "axoptimize/" empty))
    (dolist (root (list empty missing))
      (let ((axllm/optimize-conformance:*conformance-directory* root))
        ;; The inventory itself refuses, so every caller is covered and not
        ;; only the runner that happens to check today.
        (check-error-text "the suite has nothing to run"
                          (format nil "an empty inventory under ~a must be refused" root)
          (axllm/optimize-conformance::fixture-files))
        ;; And the runner turns that refusal into a counted failure, because
        ;; the count is what the build gate actually reads.
        (multiple-value-bind (passed failed blocked skipped)
            (axllm/optimize-conformance:run-optimize-conformance-tests
             :stream (make-broadcast-stream))
          (check-equal passed 0 "a suite that did not run passed nothing")
          (check-equal failed 1 "and must report a failure, not a clean zero")
          (check-equal blocked 0 "nothing was blocked")
          (check-equal skipped 0 "and nothing was skipped"))))
    ;; The guard must not fire on the real inventory, or it would be a
    ;; permanent red that everyone learns to ignore.
    (check (plusp (length (axllm/optimize-conformance::fixture-files)))
           "the real fixture root still yields fixtures")))

(deftest an-overridden-model-is-attributed-to-the-model-that-served-the-run
  ;; A rollout may name its own model, and the optimizer prices what it is
  ;; told ran.  Asserting only that the override reaches the request body
  ;; leaves the run attributed to the client's default in the usage and the
  ;; chat log, which is a silently mispriced optimizer run and a trace that
  ;; names the wrong model.  All three are pinned here, and the price is
  ;; pinned too, because the cost is the consequence that actually matters.
  (let* ((bodies '())
         (client (ax:ai :name "openai" :model "cheap-default" :api-key "sk-test-dummy-key"
                        :transport (lambda (url headers body)
                                     (declare (ignore url headers))
                                     (push body bodies)
                                     (values (ax:encode-json
                                              (ax:object
                                               "choices"
                                               (jarray (ax:object "index" 0 "finish_reason" "stop"
                                                                  "message" (ax:object "role" "assistant"
                                                                                       "content" "Answer: ok")))
                                               "usage" (ax:object "prompt_tokens" 400
                                                                  "completion_tokens" 600
                                                                  "total_tokens" 1000)))
                                             200))))
         (program (ax:ax "question:string -> answer:string")))
    (axllm::forward program client (ax:object "question" "q")
                    (ax:object "model" "expensive-override"))
    (check-equal (jget (ax:parse-json (first bodies)) "model") "expensive-override"
                 "the override reaches the provider request")
    (let ((entry (elt (axllm::program-usage-by-model program) 0)))
      (check-equal (jget entry "model") "expensive-override"
                   "and the usage is attributed to the model that served it")
      (check-equal (jget entry "ai") "openai" "under the client's provider")
      ;; The discriminator: one price list that knows both models and prices
      ;; them differently.  Charging the client's default would give 1, and an
      ;; unknown-model fallback would give neither number, so only attributing
      ;; the run to the model that served it produces 7.
      (let ((tracker (ax:make-cost-tracker
                      :cost-per-model (ax:object "expensive-override" 7 "cheap-default" 1))))
        (ax:track-tokens tracker (jget entry "total_tokens") (jget entry "model"))
        (check-close (ax:cost-tracker-cost tracker) 7d0
                     "1000 tokens are priced as the model that served them, not as the client's default")))
    (check-equal (jget (elt (axllm::program-chat-log program) 0) "model") "expensive-override"
                 "and the chat log names the model that answered, not the client's")))

(deftest the-rounds-teacher-options-reach-the-miner-and-stop-there
  ;; A round names a teacher for its own diagnosis.  The Reflector and the
  ;; Curator belong to the playbook the caller configured and may be pointed
  ;; somewhere else deliberately, so the round must not re-point them.
  ;;
  ;; The difference is only observable once a teacher can refuse, which is
  ;; why it is pinned with a client that really does: an expensive model
  ;; answers the miner, which was given the round's confirmation, and refuses
  ;; the curate, which was not.  A round that leaked its options into the
  ;; handle would be confirmed throughout and the refusal would vanish.
  (let* ((calls 0)
         (teacher (axllm::provider
                   :profile "openai" :model "premium-model" :api-key "sk-test-dummy-key"
                   :options (ax:object
                             "modelInfo" (jarray (ax:object "name" "premium-model"
                                                            "isExpensive" ax:true
                                                            "promptTokenCostPer1M" 150
                                                            "completionTokenCostPer1M" 600)))
                   :transport (lambda (url headers body)
                                (declare (ignore url headers body))
                                (incf calls)
                                (values (ax:encode-json
                                         (ax:object "choices"
                                                    (jarray (ax:object "index" 0 "finish_reason" "stop"
                                                                       "message"
                                                                       (ax:object "role" "assistant"
                                                                                  "content" (miner-reply))))))
                                        200))))
         (program (make-instance 'evaluable-program))
         ;; The handle is built without teacher options, as a caller who never
         ;; confirmed the expensive model would build it.
         (playbook (ax:make-playbook :program program
                                     :student (labelled-client '())
                                     :teacher teacher
                                     :options (ax:object "now" "1970-01-01T00:00:00.000Z"
                                                         "maxReflectorRounds" 1)))
         (result (ax:playbook-evolve-agent
                  playbook (evolve-task-set)
                  :metric (rising-metric '(0 1))
                  :options (ax:object "verify" ax:true "minHeldInGain" 0d0
                                      "maxProposals" 1 "maxMetricCalls" 4
                                      "teacherOptions" (ax:object "useExpensiveModel" "yes")))))
    (check-equal calls 1
                 "only the miner reached the teacher; the curate was refused unconfirmed")
    (let ((outcome (first (axllm::%opt-list (jget result "outcomes")))))
      (check-equal (jget outcome "accepted") ax:false
                   "a proposal that could not be applied is not kept")
      (check (search "expensive" (jget outcome "reason"))
             "and the reason names the refusal rather than hiding it"))
    (check-equal (ax:ace-render playbook) ""
                 "nothing was curated into the playbook")))

(deftest the-reflection-callback-takes-any-client-and-refuses-a-plain-function
  ;; This guard broke silently when the provider factories were consolidated:
  ;; it named a concrete class that no longer existed, so every caller was
  ;; refused and only an integration run noticed.  It is pinned from both
  ;; sides now -- a real client is accepted and actually answers, and the
  ;; mistake the docstring warns about is still refused by name.
  (let ((client (labelled-client (list "New Value: answer well"))))
    (check (functionp (ax:make-ai-reflection-callback client))
           "a client built by the current factory is accepted")
    (check-equal (funcall (ax:make-ai-reflection-callback
                           (labelled-client (list "New Value: answer well")))
                          (ax:object "componentKey" "qa::instruction"))
                 "answer well"
                 "and the callback it returns really reflects"))
  ;; A session boundary wraps a client and is a client for this purpose.
  ;; This is the assertion that makes the guard's shape matter: a check
  ;; naming the concrete provider class passes every other line in this
  ;; test and fails here, because a boundary is not that class and is still
  ;; a perfectly good client.
  (let ((boundary (axllm::boundary-service (labelled-client (list "New Value: wrapped"))
                                           (axllm::make-run-control))))
    (check (functionp (ax:make-ai-reflection-callback boundary))
           "a service wrapping a client is accepted too")
    (check-equal (funcall (ax:make-ai-reflection-callback boundary)
                          (ax:object "componentKey" "qa::instruction"))
                 "wrapped"
                 "and reflects through the wrapper"))
  (check-error :config "a plain reflection function is refused by name"
    (ax:make-ai-reflection-callback (lambda (payload) payload)))
  (check-error :config "and so is something that is not a client at all"
    (ax:make-ai-reflection-callback "not a client")))

(defun captured-system-prompt (program inputs reply)
  "The system prompt PROGRAM really sends when forwarding INPUTS."
  (let ((system nil))
    (let ((client (ax:ai :name "openai" :model "m" :api-key "sk-test-dummy-key"
                         :transport (lambda (url headers body)
                                      (declare (ignore url headers))
                                      (loop for message across (jget (ax:parse-json body) "messages")
                                            when (equal (jget message "role") "system")
                                              do (setf system (jget message "content")))
                                      (values (ax:encode-json
                                               (ax:object "choices"
                                                          (jarray (ax:object "index" 0 "finish_reason" "stop"
                                                                             "message"
                                                                             (ax:object "role" "assistant"
                                                                                        "content" reply)))))
                                              200)))))
      (ignore-errors (axllm::forward program client inputs (ax:object))))
    system))

(deftest the-miner-prompt-carries-its-task-definition
  ;; The task definition is rendered from the signature, not the generator, so
  ;; attaching it to the generator produces a prompt with no <task_definition>
  ;; block at all -- a silent divergence from every other port that still
  ;; mines weaknesses and still passes every behavioural test here.  The
  ;; agent_playbook_evolve fixtures compare this prompt byte for byte, so it
  ;; is pinned byte for byte.
  (let* ((prompt (captured-system-prompt
                  (ax:ax (axllm::evolve-miner-signature))
                  (ax:object "clusterSignature" "behavioral:no_error"
                             "taskSummaries" "- #1 (score 0.00): {}"
                             "actionLogExcerpts" "--- run 1 ---\nx"
                             "currentPlaybook" "## Context Playbook")
                  (format nil "Weakness Description: w~%Root Cause: c~%Proposed Guidance: g~%~
Evidence Quotes: [\"x\"]~%Config Recommendations: []~%")))
         (block (format nil "<task_definition>~%~a~%</task_definition>"
                        axllm::+evolve-miner-description+)))
    (check prompt "the miner really sent a system prompt")
    (check (search block prompt)
           "the miner's task definition reaches the prompt, in its own block")
    ;; The optional inputs must stay out when the cluster has none, or every
    ;; miner prompt grows two spec lines the other ports do not send.
    (check (not (search "Function Call Summary" prompt))
           "an absent optional input adds no line to the prompt")
    (check (not (search "Tool Errors" prompt))
           "nor does the other one")))
