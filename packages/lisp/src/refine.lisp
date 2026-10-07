;;;; refine.lisp --- reward-scored best-of-n and iterative refinement.
;;;;
;;;; Port of src/ax/dsp/refine.ts. A refine wrapper is itself a program: it
;;;; implements the frozen program interface (FORWARD plus the PROGRAM-*
;;;; accessors) by running the wrapped program several times, scoring every
;;;; complete candidate with a reward function, and returning the best one.
;;;;
;;;; Two strategies, exactly as in the TypeScript:
;;;;
;;;;   native-samples  one FORWARD with a sampleCount and a result picker,
;;;;                   so the provider produces the candidates. Requires a
;;;;                   program whose PROGRAM-NATIVE-SAMPLE-CAPABLE-P is
;;;;                   true; the generator in src/gen.lisp is not, so this
;;;;                   strategy must be asked for and then fails closed.
;;;;   serial          one complete FORWARD per candidate, for any program.
;;;;
;;;; REFINE adds rounds: between rounds it asks a feedback generator for
;;;; advice keyed by the ids of the wrapped program's optimizable
;;;; instruction components, appends that advice, and always restores the
;;;; original components before returning or unwinding.
;;;;
;;;; Nothing here streams: a reward needs a complete candidate, so
;;;; PROGRAM-STREAMING-FORWARD on a wrapper signals REFINE-ERROR instead
;;;; of silently scoring a prefix.
;;;;
;;;; Every attempt is a separate run, so each FORWARD asks for a fresh
;;;; conversation memory with the "freshMemory" option and carries its own
;;;; "sessionId", the way the TypeScript hands each candidate a new
;;;; AxMemory. The program's chat log, usage and traces still accumulate
;;;; across attempts, which is what lets each attempt slice out its own.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; The program interface
;;; ------------------------------------------------------------------
;;;
;;; src/gen.lisp owns FORWARD, PROGRAM-STREAMING-FORWARD and the program
;;; hooks this file uses: PROGRAM-SIGNATURE, PROGRAM-TRACES,
;;; PROGRAM-CHAT-LOG, PROGRAM-USAGE, PROGRAM-SET-INSTRUCTION,
;;; PROGRAM-OPTIMIZABLE-COMPONENTS, PROGRAM-APPLY-OPTIMIZED-COMPONENTS,
;;; PROGRAM-NATIVE-SAMPLE-CAPABLE-P, PROGRAM-TOOLS, PROGRAM-SET-TOOLS and
;;; the function-call trace hooks. This file declares only PROGRAM-ATTEMPTS,
;;; which is its own, and specializes the rest for its wrappers.
;;;
;;; A wrapper is deliberately not native-sample capable: it inherits the
;;; false default, so wrapping a sampling program does not make the wrapper
;;; itself claim to produce several candidates from one call.

(defgeneric program-attempts (program)
  (:documentation
   "The scored ATTEMPTs of PROGRAM's last run, in order, as a vector."))

;;; ------------------------------------------------------------------
;;; Errors and attempts
;;; ------------------------------------------------------------------

(define-condition refine-error (ax-error)
  ((attempts :initarg :attempts :initform '() :reader refine-error-attempts))
  (:documentation
   "A refinement that could not produce a scored candidate.

REFINE-ERROR-ATTEMPTS holds every ATTEMPT recorded before the failure,
including the failed ones, so a caller can see the rewards and the
underlying errors."))

(defstruct (attempt (:conc-name attempt-) (:copier nil))
  "One scored candidate.

NUMBER counts attempts across the whole run from 1, ROUND is the refine
round (1 for best-of-n), and SAMPLE-INDEX is the candidate's index inside
its batch. PREDICTION and REWARD are absent on a failed attempt, which
carries ERROR instead. TRACES, CHAT-LOG and USAGE are the slices the
attempt itself produced."
  (number 0 :type integer)
  (round 1 :type integer)
  (sample-index 0 :type integer)
  (strategy :serial)
  (input nil)
  (prediction nil)
  (reward nil)
  (met-threshold nil)
  (traces nil)
  (chat-log nil)
  (usage '())
  (error nil)
  (advice nil)
  (advice-applied nil))

(defun attempt-json (attempt)
  "ATTEMPT as a JSON object, for a feedback prompt or a log."
  (let ((out (object "attempt" (attempt-number attempt)
                     "round" (attempt-round attempt)
                     "sampleIndex" (attempt-sample-index attempt)
                     "strategy" (string-downcase (symbol-name (attempt-strategy attempt)))
                     "metThreshold" (if (attempt-met-threshold attempt) 'yason:true 'yason:false))))
    (%set-key out "reward" (or (attempt-reward attempt) :null))
    (%set-key out "prediction" (or (attempt-prediction attempt) :null))
    (when (attempt-error attempt)
      (%set-key out "error" (princ-to-string (attempt-error attempt))))
    out))

;;; ------------------------------------------------------------------
;;; Small helpers
;;; ------------------------------------------------------------------

(defvar *refine-session-counter* 0)

(defun %refine-next-session-id (prefix)
  (format nil "~a-~a-~a" prefix
          (* 1000 (- (get-universal-time) (encode-universal-time 0 0 0 1 1 1970 0)))
          (incf *refine-session-counter*)))

(defun %refine-count (value name)
  "VALUE as a positive whole count, as normalizeCount does."
  (unless (and (realp value) (>= value 1))
    (error 'refine-error :message (format nil "~a must be a positive number" name)))
  (floor value))

(defun %refine-stringify (value)
  "VALUE as JSON text, falling back to its printed form.

Forward options can hold Lisp functions, which are not JSON values; the
TypeScript safeStringify has the same fallback."
  (handler-case (encode-json value)
    (error () (princ-to-string value))))

(defun %refine-object-copy (source)
  (let ((out (%new-object)))
    (when (hash-table-p source)
      (dolist (key (%object-keys source))
        (%set-key out key (gethash key source))))
    out))

(defun %refine-options (base &rest key-values)
  "BASE forward options with KEY-VALUES set, leaving BASE untouched."
  (let ((out (%refine-object-copy base)))
    (loop for (key value) on key-values by #'cddr
          do (%set-key out key value))
    out))

(defun %refine-model-config (base override)
  "mergeModelConfig: temperature 1, then BASE, then OVERRIDE."
  (let ((out (object "temperature" 1)))
    (dolist (source (list base override))
      (when (hash-table-p source)
        (dolist (key (%object-keys source))
          (%set-key out key (gethash key source)))))
    out))

(defun %refine-slice (value start)
  "VALUE, a JSON array, from index START, as a fresh JSON array."
  (let ((out (%new-array)))
    (when (%array-p value)
      (loop for index from (min start (length value)) below (length value)
            do (vector-push-extend (aref value index) out)))
    out))

(defun %refine-usage-list (usage)
  "USAGE as a list of usage objects.

Accepts a single usage object, a JSON array of them, or an agent usage
object with actor and responder arrays."
  (cond ((null usage) '())
        ((eq usage :null) '())
        ((%array-p usage) (coerce usage 'list))
        ((hash-table-p usage)
         (let ((actor (jget usage "actor"))
               (responder (jget usage "responder")))
           (if (or (%array-p actor) (%array-p responder))
               (append (and (%array-p actor) (coerce actor 'list))
                       (and (%array-p responder) (coerce responder 'list)))
               (list usage))))
        (t '())))

(defparameter +usage-token-keys+
  '("promptTokens" "completionTokens" "totalTokens" "thoughtsTokens"
    "reasoningTokens" "cacheCreationTokens" "cacheReadTokens"))

(defun %refine-merge-usage (usages)
  "USAGES merged per ai/model pair, summing token counts.

Mirrors mergeProgramUsage: the first usage of a pair fixes the non-token
fields and key order, later ones only add tokens."
  (let ((order '())
        (table (make-hash-table :test 'equal)))
    (dolist (usage usages)
      (when (hash-table-p usage)
        (let* ((key (format nil "~a:~a" (%refine-stringify (jget usage "ai"))
                            (%refine-stringify (jget usage "model"))))
               (merged (gethash key table)))
          (cond ((null merged)
                 (setf (gethash key table) (%refine-object-copy usage))
                 (push key order))
                (t
                 (dolist (token-key +usage-token-keys+)
                   (let ((left (jget merged token-key))
                         (right (jget usage token-key)))
                     (when (or (realp left) (realp right))
                       (%set-key merged token-key
                                 (+ (if (realp left) left 0)
                                    (if (realp right) right 0)))))))))))
    (let ((out (%new-array)))
      (dolist (key (nreverse order))
        (vector-push-extend (gethash key table) out))
      out)))

(defun %refine-trim (text)
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed) text)
      ""))

;;; ------------------------------------------------------------------
;;; Base wrapper
;;; ------------------------------------------------------------------

(defclass refine-base ()
  ((program :initarg :program :reader refine-program)
   (reward-fn :initarg :reward-fn :reader refine-reward-fn)
   (model-config :initarg :model-config :initform nil :reader refine-model-config)
   (threshold :initarg :threshold :initform nil :reader refine-threshold)
   (fail-count :initarg :fail-count :reader refine-fail-count)
   (strategy :initarg :strategy :initform nil :reader refine-strategy)
   (on-attempt :initarg :on-attempt :initform nil :reader refine-on-attempt)
   (attempts :initform (%new-array) :accessor refine-attempts)
   (selected :initform nil :accessor refine-selected))
  (:documentation "Shared state of a reward-scored wrapper."))

(defmethod program-attempts ((wrapper refine-base))
  "Every attempt of the last run, in order, as a JSON array of ATTEMPTs."
  (refine-attempts wrapper))

(defmethod program-traces ((wrapper refine-base))
  (let ((selected (refine-selected wrapper)))
    (if selected (attempt-traces selected) (%new-array))))

(defmethod program-chat-log ((wrapper refine-base))
  (let ((selected (refine-selected wrapper)))
    (if selected (attempt-chat-log selected) (%new-array))))

(defmethod program-usage ((wrapper refine-base))
  (%refine-merge-usage
   (loop for attempt across (refine-attempts wrapper)
         append (attempt-usage attempt))))

(defmethod program-set-instruction ((wrapper refine-base) text)
  (program-set-instruction (refine-program wrapper) text))

(defmethod program-optimizable-components ((wrapper refine-base))
  (program-optimizable-components (refine-program wrapper)))

(defmethod program-apply-optimized-components ((wrapper refine-base) component-map)
  (program-apply-optimized-components (refine-program wrapper) component-map))

;;; The rest of the program surface belongs to the wrapped program: a
;;; wrapper is still the same program, scored, so an agent that lends it
;;; tools or reads back which ones ran must reach the real one.

(defmethod program-signature ((wrapper refine-base))
  (program-signature (refine-program wrapper)))

(defmethod program-tools ((wrapper refine-base))
  (program-tools (refine-program wrapper)))

(defmethod program-set-tools ((wrapper refine-base) tools)
  (program-set-tools (refine-program wrapper) tools)
  wrapper)

(defmethod program-function-call-traces ((wrapper refine-base))
  (program-function-call-traces (refine-program wrapper)))

(defmethod program-clear-function-call-traces ((wrapper refine-base))
  (program-clear-function-call-traces (refine-program wrapper))
  wrapper)

(defmethod program-set-function-call-traces ((wrapper refine-base) records)
  (program-set-function-call-traces (refine-program wrapper) records)
  wrapper)

(defmethod program-streaming-forward ((wrapper refine-base) client inputs &optional options)
  (declare (ignore client inputs options))
  (error 'refine-error
         :message "best-of-n/refine wrappers do not support program-streaming-forward; use forward so complete candidates can be scored."
         :attempts (coerce (refine-attempts wrapper) 'list)))

(defun %refine-reset-run (wrapper)
  (setf (refine-attempts wrapper) (%new-array)
        (refine-selected wrapper) nil))

(defun %refine-emit (wrapper attempt)
  (vector-push-extend attempt (refine-attempts wrapper))
  (let ((hook (refine-on-attempt wrapper)))
    (when hook (funcall hook attempt)))
  attempt)

(defun %refine-attempt-count (wrapper &optional (extra 0))
  (+ (length (refine-attempts wrapper)) extra))

(defun %refine-failure-count (wrapper)
  (count-if #'attempt-error (refine-attempts wrapper)))

(defun %refine-resolve-strategy (wrapper)
  (let ((strategy (refine-strategy wrapper))
        (program (refine-program wrapper)))
    (case strategy
      (:serial :serial)
      (:native-samples
       (unless (program-native-sample-capable-p program)
         (error 'refine-error
                :message "strategy :native-samples requires a program that supports native sampling"))
       :native-samples)
      ((nil :auto)
       (if (program-native-sample-capable-p program) :native-samples :serial))
      (t (error 'refine-error
                :message (format nil "unknown strategy ~S; use :auto, :native-samples or :serial"
                                 strategy))))))

(defun %refine-reward (wrapper input prediction number round sample-index traces chat-log)
  (let ((value (funcall (refine-reward-fn wrapper)
                        (object "input" input
                                "prediction" prediction
                                "attempt" number
                                "round" round
                                "sampleIndex" sample-index
                                "traces" traces
                                "chatLog" chat-log))))
    (unless (realp value)
      (error 'refine-error
             :message (format nil "reward function must return a number, got ~S" value)))
    value))

(defun %refine-met-threshold (threshold reward)
  (and threshold (realp reward) (>= reward threshold)))

(defun %refine-select-best (attempts)
  "The scored attempt with the highest reward, or NIL."
  (let ((best nil))
    (map nil (lambda (attempt)
               (when (and (attempt-prediction attempt) (realp (attempt-reward attempt))
                          (or (null best) (> (attempt-reward attempt) (attempt-reward best))))
                 (setf best attempt)))
         attempts)
    best))

(defun %refine-first-threshold (attempts)
  (find-if (lambda (attempt)
             (and (attempt-prediction attempt) (attempt-met-threshold attempt)))
           attempts))

;;; ------------------------------------------------------------------
;;; Candidate batches
;;; ------------------------------------------------------------------

(defun %refine-run-batch (wrapper client input options count round strategy)
  (if (eq strategy :native-samples)
      (%refine-run-native-batch wrapper client input options count round)
      (%refine-run-serial-batch wrapper client input options count round)))

(defun %refine-run-native-batch (wrapper client input options count round)
  "One FORWARD producing COUNT samples, scored inside the result picker."
  (let* ((program (refine-program wrapper))
         (threshold (refine-threshold wrapper))
         (batch '())
         (trace-start (length (program-traces program)))
         (chat-start (length (program-chat-log program)))
         (selected-index 0)
         (best-reward nil)
         (first-threshold-index (and threshold -1))
         (picker
           (lambda (data)
            (block picked
             ;; Only a fields result carries scorable candidates; anything
             ;; else keeps the provider's own first choice.
             (unless (equal (jget data "type") "fields")
               (return-from picked 0))
             (let ((results (jget data "results")))
               (when (%array-p results)
                 (loop for item across results
                       for sample-index = (jget item "index")
                       for prediction = (jget item "sample")
                       for number = (%refine-attempt-count wrapper (1+ (length batch)))
                       do (let* ((reward (%refine-reward wrapper input prediction number round
                                                         sample-index (%new-array) (%new-array)))
                                 (attempt (make-attempt :number number
                                                        :round round
                                                        :sample-index sample-index
                                                        :strategy :native-samples
                                                        :input input
                                                        :prediction prediction
                                                        :reward reward
                                                        :traces (%new-array)
                                                        :chat-log (%new-array)
                                                        :usage '()
                                                        :met-threshold (%refine-met-threshold threshold reward))))
                            (when (or (null best-reward) (> reward best-reward))
                              (setf best-reward reward
                                    selected-index sample-index))
                            (when (and first-threshold-index
                                       (minusp first-threshold-index)
                                       (attempt-met-threshold attempt))
                              (setf first-threshold-index sample-index))
                            (setf batch (append batch (list attempt)))))))
             (when (and first-threshold-index (not (minusp first-threshold-index)))
               (setf selected-index first-threshold-index))
             selected-index))))
    (multiple-value-bind (result usage failure)
        (handler-case
            (multiple-value-bind (outputs usage)
                (forward program client input
                         (%refine-options options
                                          "sessionId" (%refine-next-session-id "ax-refine-native")
                                          "freshMemory" 'yason:true
                                          "sampleCount" count
                                          "resultPicker" picker
                                          "modelConfig"
                                          (let ((config (%refine-model-config
                                                         (refine-model-config wrapper)
                                                         (jget options "modelConfig" nil))))
                                            (%set-key config "stream" 'yason:false)
                                            config)))
              (values outputs usage nil))
          (error (condition) (values nil nil condition)))
      (when failure
        ;; A failed native batch discards its scored samples, exactly as the
        ;; TypeScript does, and counts once against the failure budget.
        (let* ((failures (1+ (%refine-failure-count wrapper)))
               (attempt (make-attempt :number (%refine-attempt-count wrapper 1)
                                      :round round
                                      :sample-index 0
                                      :strategy :native-samples
                                      :input input
                                      :traces (%refine-slice (program-traces program) trace-start)
                                      :chat-log (%refine-slice (program-chat-log program) chat-start)
                                      :usage (%refine-usage-list (program-usage program))
                                      :error failure)))
          (when (> failures (refine-fail-count wrapper))
            (error 'refine-error
                   :message "Native sample attempt failed"
                   :attempts (append (coerce (refine-attempts wrapper) 'list) (list attempt))))
          (return-from %refine-run-native-batch (list attempt))))
      (let ((traces (%refine-slice (program-traces program) trace-start))
            (chat-log (%refine-slice (program-chat-log program) chat-start))
            (usage-list (%refine-usage-list usage)))
        (when (null batch)
          ;; The picker never ran: the single result is the only candidate.
          (let* ((number (%refine-attempt-count wrapper 1))
                 (reward (%refine-reward wrapper input result number round 0 traces chat-log)))
            (return-from %refine-run-native-batch
              (list (make-attempt :number number
                                  :round round
                                  :sample-index 0
                                  :strategy :native-samples
                                  :input input
                                  :prediction result
                                  :reward reward
                                  :traces traces
                                  :chat-log chat-log
                                  :usage usage-list
                                  :met-threshold (%refine-met-threshold threshold reward))))))
        (let ((selected (or (find selected-index batch :key #'attempt-sample-index :test #'equal)
                            (first batch))))
          (when selected
            (setf (attempt-prediction selected) result
                  (attempt-traces selected) traces
                  (attempt-chat-log selected) chat-log
                  (attempt-usage selected) usage-list)))
        batch))))

(defun %refine-run-serial-batch (wrapper client input options count round)
  "COUNT complete FORWARD calls, stopping as soon as one meets the threshold."
  (let* ((program (refine-program wrapper))
         (threshold (refine-threshold wrapper))
         (batch '())
         (failures (%refine-failure-count wrapper)))
    (dotimes (sample-index count)
      (let ((number (%refine-attempt-count wrapper (1+ (length batch))))
            (trace-start (length (program-traces program)))
            (chat-start (length (program-chat-log program))))
        ;; The reward is scored inside the same guard as the FORWARD: a
        ;; reward function that signals is an attempt failure, not a
        ;; failure of the whole run.
        (multiple-value-bind (attempt failure)
            (handler-case
                (multiple-value-bind (prediction usage)
                    (forward program client input
                             (%refine-options options
                                              "sessionId" (%refine-next-session-id "ax-refine-serial")
                                              "freshMemory" 'yason:true
                                              "modelConfig" (%refine-model-config
                                                             (refine-model-config wrapper)
                                                             (jget options "modelConfig" nil))))
                  (let* ((traces (%refine-slice (program-traces program) trace-start))
                         (chat-log (%refine-slice (program-chat-log program) chat-start))
                         (reward (%refine-reward wrapper input prediction number round
                                                 sample-index traces chat-log)))
                    (values (make-attempt :number number
                                          :round round
                                          :sample-index sample-index
                                          :strategy :serial
                                          :input input
                                          :prediction prediction
                                          :reward reward
                                          :traces traces
                                          :chat-log chat-log
                                          :usage (%refine-usage-list usage)
                                          :met-threshold (%refine-met-threshold threshold reward))
                            nil)))
              (error (condition) (values nil condition)))
          (cond
            (failure
             (incf failures)
             (let ((failed (make-attempt :number number
                                         :round round
                                         :sample-index sample-index
                                         :strategy :serial
                                         :input input
                                         :traces (%refine-slice (program-traces program) trace-start)
                                         :chat-log (%refine-slice (program-chat-log program) chat-start)
                                         :usage (%refine-usage-list (program-usage program))
                                         :error failure)))
               (setf batch (append batch (list failed)))
               (when (> failures (refine-fail-count wrapper))
                 (error 'refine-error
                        :message (format nil "Refine attempt failed after ~a failures" failures)
                        :attempts (append (coerce (refine-attempts wrapper) 'list) batch)))))
            (t
             (setf batch (append batch (list attempt)))
             (when (attempt-met-threshold attempt)
               (return)))))))
    batch))

;;; ------------------------------------------------------------------
;;; best-of-n
;;; ------------------------------------------------------------------

(defclass best-of-n-program (refine-base)
  ((n :initarg :n :reader best-of-n-count))
  (:documentation "Scores N candidates once and returns the best."))

(defmethod print-object ((wrapper best-of-n-program) stream)
  (print-unreadable-object (wrapper stream :type t)
    (format stream "n=~a" (best-of-n-count wrapper))))

(defun best-of-n (program &key n reward-fn threshold fail-count model-config strategy on-attempt)
  "A program that scores N candidates of PROGRAM and returns the best.

REWARD-FN is called with one JSON object holding input, prediction,
attempt, round, sampleIndex, traces and chatLog, and must return a number.
With THRESHOLD, the first candidate whose reward reaches it wins and the
serial strategy stops early. FAIL-COUNT bounds failed attempts and
defaults to N. MODEL-CONFIG is merged under the caller's forward options.
STRATEGY is :auto, :native-samples or :serial. ON-ATTEMPT, when given, is
called with every ATTEMPT as it is recorded."
  (unless (functionp reward-fn)
    (error 'refine-error :message "best-of-n: :reward-fn must be a function"))
  (let ((count (%refine-count n "n")))
    (make-instance 'best-of-n-program
                   :program program
                   :n count
                   :reward-fn reward-fn
                   :threshold threshold
                   :fail-count (if fail-count (floor fail-count) count)
                   :model-config model-config
                   :strategy strategy
                   :on-attempt on-attempt)))

(defmethod forward ((wrapper best-of-n-program) client inputs &optional options)
  (%refine-reset-run wrapper)
  (let ((batch (%refine-run-batch wrapper client inputs options
                                  (best-of-n-count wrapper) 1
                                  (%refine-resolve-strategy wrapper))))
    (dolist (attempt batch) (%refine-emit wrapper attempt))
    (let ((selected (or (%refine-first-threshold (refine-attempts wrapper))
                        (%refine-select-best (refine-attempts wrapper)))))
      (unless (and selected (attempt-prediction selected))
        (error 'refine-error
               :message "best-of-n produced no successful candidates"
               :attempts (coerce (refine-attempts wrapper) 'list)))
      (setf (refine-selected wrapper) selected)
      (values (attempt-prediction selected)
              (let ((merged (%refine-merge-usage (attempt-usage selected))))
                (if (plusp (length merged)) (aref merged 0) (usage-object 0 0 0)))))))

;;; ------------------------------------------------------------------
;;; refine
;;; ------------------------------------------------------------------

(defparameter +refine-feedback-signature+
  "programDescription:string, programInput:string, failedPrediction:string, rewardValue:number, rewardThreshold:string, attemptSummaries:string, instructionComponents:string, rewardDescription:string, traceSummary:string, chatSummary:string -> summary:string, advice:json")

(defparameter +refine-feedback-instruction+
  "Generate concrete, actionable advice for the listed instruction component ids. Return advice as a JSON object whose keys exactly match component ids and whose values are short instructions for the next attempt.")

(defvar *refine-feedback-generator-factory*
  (lambda (signature) (ax signature))
  "How REFINE builds its feedback generator from a signature string.

The default calls AX. A test binds this to supply a scripted program
instead of a provider-backed generator.")

(defclass refine-program (refine-base)
  ((rounds :initarg :rounds :reader refine-rounds)
   (samples-per-round :initarg :samples-per-round :reader refine-samples-per-round)
   (feedback-client :initarg :feedback-client :initform nil :reader refine-feedback-client)
   (feedback-model-config :initarg :feedback-model-config :initform nil
                          :reader refine-feedback-model-config)
   (reward-description :initarg :reward-description :initform nil :reader refine-reward-description)
   (program-description :initarg :program-description :initform nil :reader refine-program-description))
  (:documentation "Scores candidates over several advice-guided rounds."))

(defmethod print-object ((wrapper refine-program) stream)
  (print-unreadable-object (wrapper stream :type t)
    (format stream "rounds=~a samples=~a" (refine-rounds wrapper)
            (refine-samples-per-round wrapper))))

(defun refine (program &key rounds (samples-per-round 1) reward-fn threshold fail-count
                            model-config strategy feedback-client feedback-model-config
                            reward-description program-description on-attempt)
  "A program that refines PROGRAM over ROUNDS reward-scored rounds.

Each round scores SAMPLES-PER-ROUND candidates. A candidate that reaches
THRESHOLD ends the run. Otherwise the round's best candidate is sent to a
feedback generator, which returns advice per optimizable instruction
component id; the advice is appended to those components for the next
round and the originals are always restored before returning.

FEEDBACK-CLIENT defaults to the client passed to FORWARD. FAIL-COUNT
defaults to ROUNDS times SAMPLES-PER-ROUND. The remaining keywords match
BEST-OF-N."
  (unless (functionp reward-fn)
    (error 'refine-error :message "refine: :reward-fn must be a function"))
  (let ((round-count (%refine-count rounds "rounds"))
        (sample-count (%refine-count samples-per-round "samples-per-round")))
    (make-instance 'refine-program
                   :program program
                   :rounds round-count
                   :samples-per-round sample-count
                   :reward-fn reward-fn
                   :threshold threshold
                   :fail-count (if fail-count (floor fail-count) (* round-count sample-count))
                   :model-config model-config
                   :strategy strategy
                   :feedback-client feedback-client
                   :feedback-model-config feedback-model-config
                   :reward-description reward-description
                   :program-description program-description
                   :on-attempt on-attempt)))

(defun %refine-capture-components (program)
  "PROGRAM's optimizable instruction components as an ordered alist.

A component is identified by its \"id\", the key PROGRAM-APPLY-OPTIMIZED-
COMPONENTS expects, and only the instruction kind is refined: a
description, a tool name or a tool description is not prompt advice."
  (let ((components (program-optimizable-components program))
        (out '()))
    (when (%array-p components)
      (loop for component across components
            for id = (jget component "id")
            when (and (equal (jget component "kind") "instruction") (stringp id))
              do (push (cons id (let ((current (jget component "current")))
                                  (if (stringp current) current "")))
                       out)))
    (nreverse out)))

(defun %refine-apply-advice (program advice originals)
  "Append ADVICE to ORIGINALS and apply it. True when anything was applied.

ADVICE is read by component id and only its string values are used: the
program rejects a non-string component value, and advice for an id the
program does not own is ignored."
  (let ((updates (%new-object))
        (applied nil))
    (loop for (key . current) in originals
          for value = (%refine-trim (jget advice key))
          unless (zerop (length value))
            do (setf applied t)
               (%set-key updates key
                         (%refine-trim
                          (format nil "~a~%~%Refinement advice from previous attempt:~%~a"
                                  current value))))
    (when applied
      (program-apply-optimized-components program updates))
    applied))

(defun %refine-restore-components (program originals)
  (when originals
    (let ((updates (%new-object)))
      (loop for (key . current) in originals
            do (%set-key updates key current))
      (program-apply-optimized-components program updates))))

(defun %refine-program-description (wrapper)
  "What the feedback model is told the refined program does.

PROGRAM-SIGNATURE when the program has one; a program that answers :NULL
is described by its printed form instead."
  (or (refine-program-description wrapper)
      (let* ((program (refine-program wrapper))
             (signature (program-signature program)))
        (if (eq signature :null)
            (princ-to-string program)
            (signature-string signature)))))

(defun %refine-generate-advice (wrapper client input attempt originals)
  "Advice per instruction component key for the next round."
  (if (null originals)
      (%new-object)
      (let* ((feedback (funcall *refine-feedback-generator-factory* +refine-feedback-signature+))
             (threshold (refine-threshold wrapper))
             (reward-text (or (refine-reward-description wrapper)
                              (let ((text (princ-to-string (refine-reward-fn wrapper))))
                                (subseq text 0 (min 2000 (length text))))))
             (components (%new-array)))
        (program-set-instruction feedback +refine-feedback-instruction+)
        (loop for (id . current) in originals
              do (vector-push-extend (object "id" id "current" current) components))
        (let* ((options (if (refine-feedback-model-config wrapper)
                            (object "modelConfig" (refine-feedback-model-config wrapper))
                            (%new-object)))
               (outputs (forward feedback
                                 (or (refine-feedback-client wrapper) client)
                                 (object "programDescription" (%refine-program-description wrapper)
                                         "programInput" (%refine-stringify input)
                                         "failedPrediction" (%refine-stringify (attempt-prediction attempt))
                                         "rewardValue" (or (attempt-reward attempt) 0)
                                         "rewardThreshold" (if threshold
                                                               (princ-to-string threshold)
                                                               "not specified")
                                         "attemptSummaries"
                                         (%refine-stringify
                                          (let ((summaries (%new-array)))
                                            (loop for recorded across (refine-attempts wrapper)
                                                  do (vector-push-extend (attempt-json recorded) summaries))
                                            summaries))
                                         "instructionComponents" (%refine-stringify components)
                                         "rewardDescription" reward-text
                                         "traceSummary" (%refine-stringify (attempt-traces attempt))
                                         "chatSummary" (%refine-stringify (attempt-chat-log attempt)))
                                 options))
               (advice (and (hash-table-p outputs) (jget outputs "advice")))
               (out (%new-object)))
          (when (hash-table-p advice)
            (dolist (key (%object-keys advice))
              (let ((value (gethash key advice)))
                (when (stringp value) (%set-key out key value)))))
          out))))

(defmethod forward ((wrapper refine-program) client inputs &optional options)
  (%refine-reset-run wrapper)
  (let* ((program (refine-program wrapper))
         (originals (%refine-capture-components program))
         (best nil))
    (unwind-protect
         (progn
           (loop for round from 1 to (refine-rounds wrapper)
                 do (let ((batch (%refine-run-batch wrapper client inputs options
                                                    (refine-samples-per-round wrapper) round
                                                    (%refine-resolve-strategy wrapper))))
                      (dolist (attempt batch) (%refine-emit wrapper attempt))
                      (let ((hit (%refine-first-threshold batch)))
                        (when (and hit (attempt-prediction hit))
                          (setf (refine-selected wrapper) hit)
                          (return-from forward
                            (values (attempt-prediction hit)
                                    (let ((merged (%refine-merge-usage (attempt-usage hit))))
                                      (if (plusp (length merged))
                                          (aref merged 0)
                                          (usage-object 0 0 0)))))))
                      (let ((round-best (%refine-select-best batch)))
                        (when (and round-best
                                   (or (null best)
                                       (> (attempt-reward round-best)
                                          (or (attempt-reward best) 0))))
                          (setf best round-best))
                        (when (and (< round (refine-rounds wrapper))
                                   round-best
                                   (attempt-prediction round-best))
                          (let ((advice (%refine-generate-advice wrapper client inputs
                                                                 round-best originals)))
                            (setf (attempt-advice round-best) advice
                                  (attempt-advice-applied round-best)
                                  (%refine-apply-advice program advice originals)))))))
           (unless (and best (attempt-prediction best))
             (error 'refine-error
                    :message "refine produced no successful candidates"
                    :attempts (coerce (refine-attempts wrapper) 'list)))
           (setf (refine-selected wrapper) best)
           (values (attempt-prediction best)
                   (let ((merged (%refine-merge-usage (attempt-usage best))))
                     (if (plusp (length merged)) (aref merged 0) (usage-object 0 0 0)))))
      (%refine-restore-components program originals))))
