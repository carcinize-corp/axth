;;;; telemetry.lisp --- native public observability facade.
;;;; Load after package/json/core-runtime; no provider or generated Core dependency.
;;;; Sources: dsp/globals.ts, dsp/metrics.ts, dsp/optimizer.ts (metrics),
;;;; ai/metrics.ts (label merge), util/telemetry.ts, trace/trace.ts.
;;;;
;;;; Integration contract (runtime workers own intrinsic instrumentation):
;;;; - Read GLOBALS-SNAPSHOT at operation entry; do not JSON-clone native hooks.
;;;; - Carry MAKE-RUNTIME-HOOK-FRAME via OPTIONS-WITH-RUNTIME-HOOK-FRAME and
;;;;   COPY-RUNTIME-OPTIONS. RESOLVED distinguishes an intentionally empty hook set.
;;;; - Use TELEMETRY-CALL for bound callbacks in string-keyed method tables,
;;;;   dispatcher functions (method &rest arguments), and native host objects.
;;;;   Native host objects use CORE-HOST-GET (target key fallback), returning a
;;;;   bound function. That generic belongs to AXLLM/CORE, not this facade.
;;;; - Safe spans/instruments are opaque dispatcher functions, not JSON objects.
;;;; - Active-span operations are synchronous; callers own span ending, as in TS.
;;;;   Use UNWIND-PROTECT and SPAN-CALL "end". No scheduler/async ABI is invented.
;;;; - Config enabledCategories/samplingRate are stored, not enforced by these
;;;;   recording helpers (same as the authoritative source). Runtimes decide when
;;;;   to collect; ENABLED is also a runtime gate, not a helper-side filter.
;;;; - No definitions here replace runtime workers' intrinsic hook internals.

(in-package #:axllm)

(defparameter *telemetry-native-fields*
  '("tracer" "meter" "rateLimiter" "logger" "optimizerLogger" "abortSignal"
    "onUsage" "cachingFunction" "functionResultFormatter"))

(defun %telemetry-copy (value &optional native-options-p)
  "Copy data containers; preserve native hooks and non-string metadata by identity."
  (cond ((hash-table-p value)
         (let ((out (object)))
           (dolist (key (%object-keys value))
             (%set-key out (copy-seq key)
                       (if (and native-options-p
                                (member key *telemetry-native-fields* :test #'equal))
                           (gethash key value)
                           (%telemetry-copy (gethash key value)))))
           (maphash (lambda (key item)
                      (unless (stringp key) (setf (gethash key out) item))) value)
           out))
        ((stringp value) (copy-seq value))
        ((vectorp value) (map 'vector #'%telemetry-copy value))
        (t value)))

(defun %telemetry-merge (&rest sources)
  (let ((out (object)))
    (dolist (source sources out)
      (when (hash-table-p source)
        (dolist (key (%object-keys source)) (%set-key out key (gethash key source)))))))

(defun %telemetry-present-p (value)
  (not (or (null value) (eq value :null))))

(defun %telemetry-true-p (value)
  (and (%telemetry-present-p value) (not (eq value false))))

(defun %telemetry-boolean-text (value)
  (if (%telemetry-true-p value) "true" "false"))

(defun %telemetry-pretty-json (value)
  "Use the native ordered JSON writer, with JSON.stringify's two-space layout."
  (with-output-to-string (stream)
    (labels ((newline-at (depth)
               (terpri stream)
               (dotimes (i (* 2 depth)) (write-char #\Space stream)))
             (emit (item depth)
               (cond
                 ((hash-table-p item)
                  (let ((keys (%object-keys item)))
                    (write-char #\{ stream)
                    (loop for key in keys for first = t then nil do
                      (unless first (write-char #\, stream))
                      (newline-at (1+ depth))
                      (%write-json-string key stream)
                      (write-string ": " stream)
                      (emit (gethash key item) (1+ depth)))
                    (when keys (newline-at depth))
                    (write-char #\} stream)))
                 ((%array-p item)
                  (write-char #\[ stream)
                  (loop for entry across item for first = t then nil do
                    (unless first (write-char #\, stream))
                    (newline-at (1+ depth))
                    (emit entry (1+ depth)))
                  (when (plusp (length item)) (newline-at depth))
                  (write-char #\] stream))
                 (t (%write-json item stream)))))
      (emit value 0))))

(defun %telemetry-default-formatter (value)
  (cond ((stringp value) value)
        ((not (%telemetry-present-p value)) "")
        (t (%telemetry-pretty-json value))))

(defun %telemetry-default-globals ()
  (object "signatureStrict" true "tracer" :null "meter" :null
          "rateLimiter" :null "logger" :null "optimizerLogger" :null
          "debug" :null "abortSignal" :null "customLabels" :null
          "onUsage" :null "cachingFunction" :null
          "functionResultFormatter" #'%telemetry-default-formatter))

(defvar *telemetry-globals* (%telemetry-default-globals))

(defun globals-snapshot ()
  "An isolated native snapshot. JSON containers are copied; hooks retain identity.
This is runtime context, not a serializable program export. Put it in a hook frame
when carrying it in serializable options. Use :NULL to clear optional globals."
  (%telemetry-copy *telemetry-globals* t))

(defun get-global (name &optional (default :null))
  (let ((value (jget *telemetry-globals* name default)))
    (if (member name *telemetry-native-fields* :test #'equal)
        value (%telemetry-copy value))))

(defun set-global (name value)
  "Set a source-compatible camelCase global name; reject accidental misspellings."
  (unless (nth-value 1 (gethash name *telemetry-globals*))
    (error 'ax-error :message (format nil "Unknown Ax global: ~S" name)))
  (%set-key *telemetry-globals* name
            (if (member name *telemetry-native-fields* :test #'equal)
                value (%telemetry-copy value)))
  value)

(defun update-globals (config)
  "Atomically validate names before applying a partial native configuration."
  (dolist (key (%object-keys config))
    (unless (nth-value 1 (gethash key *telemetry-globals*))
      (error 'ax-error :message (format nil "Unknown Ax global: ~S" key))))
  (dolist (key (%object-keys config)) (set-global key (gethash key config)))
  (globals-snapshot))

(defun reset-globals ()
  (setf *telemetry-globals* (%telemetry-default-globals))
  (globals-snapshot))

;;; A symbol key matches TS symbol metadata: JSON/cache/export see string keys only.
(defvar +runtime-hook-frame-key+ (make-symbol "ax.runtimeHookFrame"))

(defstruct (runtime-hook-frame
            (:constructor %make-runtime-hook-frame (globals resolved)))
  (globals (object) :read-only t)
  (resolved false :read-only t))

(defun make-runtime-hook-frame (&key (globals (globals-snapshot)) (resolved false))
  (%make-runtime-hook-frame (%telemetry-copy globals t)
                            (if (%telemetry-true-p resolved) true false)))

(defun get-runtime-hook-frame (options)
  (if (hash-table-p options) (gethash +runtime-hook-frame-key+ options :null) :null))

(defun copy-runtime-options (options)
  "Copy option data without serializing native context; preserve frame identity."
  (if (hash-table-p options) (%telemetry-copy options t) (object)))

(defun options-with-runtime-hook-frame (options frame)
  (let ((out (copy-runtime-options options)))
    (setf (gethash +runtime-hook-frame-key+ out) frame)
    out))

;;; Native protocol. No generic definitions: the Core runtime owns those.
(defun telemetry-call (target method &rest arguments)
  "Call a camelCase bound method. Hash callbacks receive only ARGUMENTS;
dispatcher functions receive METHOD then ARGUMENTS. Host methods are bound
callbacks obtained through AXLLM/CORE::CORE-HOST-GET. Errors are not hidden here;
safe wrappers and recording helpers are the fail-open boundaries."
  (cond ((functionp target) (apply target method arguments))
        ((hash-table-p target)
         (let ((callback (jget target method)))
           (if (functionp callback) (apply callback arguments)
               (error 'ax-error :message (format nil "Missing telemetry method ~A" method)))))
        (t
         (let ((getter (find-symbol "CORE-HOST-GET" :axllm/core)))
           (if (and getter (fboundp getter))
               (let ((callback (funcall getter target method :null)))
                 (if (functionp callback) (apply callback arguments)
                     (error 'ax-error :message (format nil "Missing host telemetry method ~A" method))))
               (error 'ax-error :message "Native telemetry object needs CORE-HOST-GET or a callback adapter"))))))

(defun fail-open-span (span)
  "Wrap SPAN once per operation. END is attempted at most once, even if it fails.
ISRECORDING failure returns AX:FALSE, other method failures return :NULL."
  (let ((ended nil))
    (lambda (method &rest arguments)
      (handler-case
          (if (and (equal method "end") ended) :null
              (progn
                (when (equal method "end") (setf ended t))
                (let ((value (apply #'telemetry-call span method arguments)))
                  (if (equal method "isRecording")
                      (if (%telemetry-true-p value) true false) value))))
        (error () (if (equal method "isRecording") false :null))))))

(defun span-call (span method &rest arguments)
  (if (%telemetry-present-p span) (apply #'telemetry-call span method arguments)
      (if (equal method "isRecording") false :null)))

(defun start-span-fail-open (tracer name &optional (options :null) (parent-context :null))
  (when (%telemetry-present-p tracer)
    (handler-case
        (return-from start-span-fail-open
          (fail-open-span (telemetry-call tracer "startSpan" name options parent-context)))
      (error () nil)))
  :null)

(defun start-active-span-fail-open (tracer name options parent-context operation)
  "Call OPERATION with a safe span or :NULL, EXACTLY ONCE. Preserve multiple values
and the original operation error even if a tracer swallows/replaces it. Tracer
failure before invocation falls back; failure after completion returns the result.
Repeated tracer callback invocations return the saved result without rerunning work.
Nonlocal Lisp exits propagate naturally. No span is ended on the caller's behalf."
  (let ((invoked nil) (results nil) (operation-error nil))
    (labels ((invoke (span)
               (unless invoked
                 (setf invoked t)
                 (handler-case (setf results (multiple-value-list (funcall operation span)))
                   (error (condition) (setf operation-error condition))))
               (when operation-error (error operation-error))
               (values-list results)))
      (when (%telemetry-present-p tracer)
        (handler-case
            (telemetry-call tracer "startActiveSpan" name options parent-context
                            (lambda (span) (invoke (fail-open-span span))))
          (error () nil)))
      (unless invoked (invoke :null))
      (when operation-error (error operation-error))
      (values-list results))))

;;; Configuration snapshots intentionally deep-copy arrays and labels (unlike
;;; the TS shallow getters), so callers cannot mutate global config accidentally.
(defun default-metrics-config ()
  (%telemetry-copy
   (object "enabled" true
           "enabledCategories" (vector "generation" "streaming" "functions" "errors" "performance")
           "maxLabelLength" 100 "samplingRate" 1.0d0)))

(defun default-optimizer-metrics-config ()
  (%telemetry-copy
   (object "enabled" true
           "enabledCategories" (vector "optimization" "convergence" "resource_usage"
                                       "teacher_student" "checkpointing" "pareto")
           "maxLabelLength" 100 "samplingRate" 1.0d0)))

(defvar *telemetry-metrics-config* (default-metrics-config))
(defvar *telemetry-optimizer-metrics-config* (default-optimizer-metrics-config))

(defun get-metrics-config () (%telemetry-copy *telemetry-metrics-config*))
(defun get-optimizer-metrics-config () (%telemetry-copy *telemetry-optimizer-metrics-config*))
(defun update-metrics-config (config)
  (setf *telemetry-metrics-config*
        (%telemetry-copy (%telemetry-merge *telemetry-metrics-config* config)))
  (get-metrics-config))
(defun update-optimizer-metrics-config (config)
  (setf *telemetry-optimizer-metrics-config*
        (%telemetry-copy (%telemetry-merge *telemetry-optimizer-metrics-config* config)))
  (get-optimizer-metrics-config))

(defun merge-custom-labels (&rest sources)
  "Later label sources override earlier sources; no source is modified."
  (%telemetry-copy (apply #'%telemetry-merge sources)))

(defun %telemetry-label-text (value)
  (cond ((stringp value) value)
        ((eq value true) "true") ((eq value false) "false")
        ((numberp value) (encode-json value))
        ((hash-table-p value) "[object Object]")
        ((vectorp value)
         (format nil "~{~A~^,~}"
                 (loop for item across value collect
                       (if (%telemetry-present-p item) (%telemetry-label-text item) ""))))
        (t (princ-to-string value))))

(defun %telemetry-labels (base custom &optional optimizer-p)
  (let ((out (object))
        (limit (jget (if optimizer-p *telemetry-optimizer-metrics-config*
                        *telemetry-metrics-config*) "maxLabelLength")))
    (let ((labels (%telemetry-merge base custom)))
      (dolist (key (%object-keys labels))
        (let ((value (gethash key labels)))
          (when (%telemetry-present-p value)
            (let ((text (%telemetry-label-text value)))
              (%set-key out key (subseq text 0 (min (length text) (max 0 (truncate limit))))))))))
    out))

(defun %telemetry-optional-label (key text)
  (if (and (stringp text) (plusp (length text))) (object key text) (object)))

(defun %telemetry-gen-labels (signature custom &rest pairs)
  (%telemetry-labels
   (%telemetry-merge (apply #'object pairs)
                     (%telemetry-optional-label "signature" signature)) custom))

(defun %telemetry-opt-labels (optimizer custom &rest pairs)
  (%telemetry-labels (%telemetry-merge (apply #'object pairs)
                                     (object "optimizer_type" optimizer)) custom t))

;;; Each entry is (public field, meter method, metric name, description, unit).
(defparameter *telemetry-gen-specs*
  '(("generationLatencyHistogram" "createHistogram" "ax_gen_generation_duration_ms" "End-to-end duration of AxGen generation requests" "ms")
    ("generationRequestsCounter" "createCounter" "ax_gen_generation_requests_total" "Total number of AxGen generation requests")
    ("generationErrorsCounter" "createCounter" "ax_gen_generation_errors_total" "Total number of failed AxGen generations")
    ("multiStepGenerationsCounter" "createCounter" "ax_gen_multistep_generations_total" "Total number of generations that required multiple steps")
    ("stepsPerGenerationHistogram" "createHistogram" "ax_gen_steps_per_generation" "Number of steps taken per generation")
    ("maxStepsReachedCounter" "createCounter" "ax_gen_max_steps_reached_total" "Total number of generations that hit max steps limit")
    ("validationErrorsCounter" "createCounter" "ax_gen_validation_errors_total" "Total number of validation errors encountered")
    ("errorCorrectionAttemptsHistogram" "createHistogram" "ax_gen_error_correction_attempts" "Number of error correction attempts per generation")
    ("errorCorrectionSuccessCounter" "createCounter" "ax_gen_error_correction_success_total" "Total number of successful error corrections")
    ("errorCorrectionFailureCounter" "createCounter" "ax_gen_error_correction_failure_total" "Total number of failed error corrections")
    ("maxRetriesReachedCounter" "createCounter" "ax_gen_max_retries_reached_total" "Total number of generations that hit max retries limit")
    ("functionsEnabledGenerationsCounter" "createCounter" "ax_gen_functions_enabled_generations_total" "Total number of generations with functions enabled")
    ("functionCallStepsCounter" "createCounter" "ax_gen_function_call_steps_total" "Total number of steps that included function calls")
    ("functionsExecutedPerGenerationHistogram" "createHistogram" "ax_gen_functions_executed_per_generation" "Number of unique functions executed per generation")
    ("functionErrorCorrectionCounter" "createCounter" "ax_gen_function_error_correction_total" "Total number of function-related error corrections")
    ("fieldProcessorsExecutedCounter" "createCounter" "ax_gen_field_processors_executed_total" "Total number of field processors executed")
    ("streamingFieldProcessorsExecutedCounter" "createCounter" "ax_gen_streaming_field_processors_executed_total" "Total number of streaming field processors executed")
    ("streamingGenerationsCounter" "createCounter" "ax_gen_streaming_generations_total" "Total number of streaming generations")
    ("streamingDeltasEmittedCounter" "createCounter" "ax_gen_streaming_deltas_emitted_total" "Total number of streaming deltas emitted")
    ("streamingFinalizationLatencyHistogram" "createHistogram" "ax_gen_streaming_finalization_duration_ms" "Duration of streaming response finalization" "ms")
    ("samplesGeneratedHistogram" "createHistogram" "ax_gen_samples_generated" "Number of samples generated per request")
    ("resultPickerUsageCounter" "createCounter" "ax_gen_result_picker_usage_total" "Total number of times result picker was used")
    ("resultPickerLatencyHistogram" "createHistogram" "ax_gen_result_picker_duration_ms" "Duration of result picker execution" "ms")
    ("inputFieldsGauge" "createGauge" "ax_gen_input_fields" "Number of input fields in signature")
    ("outputFieldsGauge" "createGauge" "ax_gen_output_fields" "Number of output fields in signature")
    ("examplesUsedGauge" "createGauge" "ax_gen_examples_used" "Number of examples used in generation")
    ("demosUsedGauge" "createGauge" "ax_gen_demos_used" "Number of demos used in generation")
    ("promptRenderLatencyHistogram" "createHistogram" "ax_gen_prompt_render_duration_ms" "Duration of prompt template rendering" "ms")
    ("extractionLatencyHistogram" "createHistogram" "ax_gen_extraction_duration_ms" "Duration of value extraction from responses" "ms")
    ("stateCreationLatencyHistogram" "createHistogram" "ax_gen_state_creation_duration_ms" "Duration of state creation for multiple samples" "ms")
    ("memoryUpdateLatencyHistogram" "createHistogram" "ax_gen_memory_update_duration_ms" "Duration of memory updates during generation" "ms")))

(defparameter *telemetry-opt-specs*
  '(("optimizationLatencyHistogram" "createHistogram" "ax_optimizer_optimization_duration_ms" "End-to-end duration of optimization runs" "ms")
    ("optimizationRequestsCounter" "createCounter" "ax_optimizer_optimization_requests_total" "Total number of optimization requests")
    ("optimizationErrorsCounter" "createCounter" "ax_optimizer_optimization_errors_total" "Total number of failed optimizations")
    ("convergenceRoundsHistogram" "createHistogram" "ax_optimizer_convergence_rounds" "Number of rounds until convergence")
    ("convergenceScoreGauge" "createGauge" "ax_optimizer_convergence_score" "Current best score during optimization")
    ("convergenceImprovementGauge" "createGauge" "ax_optimizer_convergence_improvement" "Improvement in score from baseline")
    ("stagnationRoundsGauge" "createGauge" "ax_optimizer_stagnation_rounds" "Number of rounds without improvement")
    ("earlyStoppingCounter" "createCounter" "ax_optimizer_early_stopping_total" "Total number of early stopping events")
    ("tokenUsageCounter" "createCounter" "ax_optimizer_token_usage_total" "Total tokens used during optimization")
    ("costUsageCounter" "createCounter" "ax_optimizer_cost_usage_total" "Total cost incurred during optimization" "$")
    ("memoryUsageGauge" "createGauge" "ax_optimizer_memory_usage_bytes" "Peak memory usage during optimization" "By")
    ("optimizationDurationHistogram" "createHistogram" "ax_optimizer_duration_ms" "Duration of optimization runs" "ms")
    ("teacherStudentUsageCounter" "createCounter" "ax_optimizer_teacher_student_usage_total" "Total number of teacher-student interactions")
    ("teacherStudentLatencyHistogram" "createHistogram" "ax_optimizer_teacher_student_latency_ms" "Latency of teacher-student interactions" "ms")
    ("teacherStudentScoreImprovementGauge" "createGauge" "ax_optimizer_teacher_student_score_improvement" "Score improvement from teacher-student interactions")
    ("checkpointSaveCounter" "createCounter" "ax_optimizer_checkpoint_save_total" "Total number of checkpoint saves")
    ("checkpointLoadCounter" "createCounter" "ax_optimizer_checkpoint_load_total" "Total number of checkpoint loads")
    ("checkpointSaveLatencyHistogram" "createHistogram" "ax_optimizer_checkpoint_save_latency_ms" "Latency of checkpoint save operations" "ms")
    ("checkpointLoadLatencyHistogram" "createHistogram" "ax_optimizer_checkpoint_load_latency_ms" "Latency of checkpoint load operations" "ms")
    ("paretoOptimizationsCounter" "createCounter" "ax_optimizer_pareto_optimizations_total" "Total number of Pareto optimizations")
    ("paretoFrontSizeHistogram" "createHistogram" "ax_optimizer_pareto_front_size" "Size of Pareto frontier")
    ("paretoHypervolumeGauge" "createGauge" "ax_optimizer_pareto_hypervolume" "Hypervolume of Pareto frontier")
    ("paretoSolutionsGeneratedHistogram" "createHistogram" "ax_optimizer_pareto_solutions_generated" "Number of solutions generated for Pareto optimization")
    ("programInputFieldsGauge" "createGauge" "ax_optimizer_program_input_fields" "Number of input fields in optimized program")
    ("programOutputFieldsGauge" "createGauge" "ax_optimizer_program_output_fields" "Number of output fields in optimized program")
    ("examplesCountGauge" "createGauge" "ax_optimizer_examples_count" "Number of training examples used")
    ("validationSetSizeGauge" "createGauge" "ax_optimizer_validation_set_size" "Size of validation set used")
    ("evaluationLatencyHistogram" "createHistogram" "ax_optimizer_evaluation_latency_ms" "Latency of program evaluations" "ms")
    ("demoGenerationLatencyHistogram" "createHistogram" "ax_optimizer_demo_generation_latency_ms" "Latency of demo generation" "ms")
    ("metricComputationLatencyHistogram" "createHistogram" "ax_optimizer_metric_computation_latency_ms" "Latency of metric computation" "ms")
    ("optimizerTypeGauge" "createGauge" "ax_optimizer_type" "Type of optimizer being used")
    ("targetScoreGauge" "createGauge" "ax_optimizer_target_score" "Target score for optimization")
    ("maxRoundsGauge" "createGauge" "ax_optimizer_max_rounds" "Maximum rounds for optimization")))

(defun %telemetry-safe-instrument (instrument)
  (lambda (method &rest arguments)
    (handler-case (apply #'telemetry-call instrument method arguments)
      (error () :null))))

(defun %telemetry-create-instruments (meter specs)
  (let ((out (object)))
    (dolist (spec specs out)
      (destructuring-bind (field method name description &optional unit) spec
        (let ((options (object "description" description)))
          (when unit (%set-key options "unit" unit))
          (%set-key out field (%telemetry-safe-instrument
                               (telemetry-call meter method name options))))))))

(defun create-gen-metrics-instruments (meter)
  (%telemetry-create-instruments meter *telemetry-gen-specs*))
(defun create-optimizer-metrics-instruments (meter)
  (%telemetry-create-instruments meter *telemetry-opt-specs*))

(defvar *telemetry-gen-by-meter* (make-hash-table :test 'eq :weakness :key))
(defvar *telemetry-opt-by-meter* (make-hash-table :test 'eq :weakness :key))
(defvar *telemetry-last-optimizer-instruments* :null)

(defun get-or-create-gen-metrics-instruments (&optional (meter :null))
  (let ((active (if (%telemetry-present-p meter) meter (get-global "meter"))))
    (if (%telemetry-present-p active)
        (or (gethash active *telemetry-gen-by-meter*)
            (handler-case
                (setf (gethash active *telemetry-gen-by-meter*)
                      (create-gen-metrics-instruments active))
              (error () :null))) :null)))

(defun get-or-create-optimizer-metrics-instruments (&optional (meter :null))
  "Explicit meters have isolated caches; no meter returns the last optimizer set,
matching the source fallback (not the generation factory's global-meter fallback)."
  (if (%telemetry-present-p meter)
      (handler-case
          (setf *telemetry-last-optimizer-instruments*
                (or (gethash meter *telemetry-opt-by-meter*)
                    (setf (gethash meter *telemetry-opt-by-meter*)
                          (create-optimizer-metrics-instruments meter))))
        (error () :null))
      *telemetry-last-optimizer-instruments*))

(defun reset-gen-metrics-instruments () (clrhash *telemetry-gen-by-meter*) :null)
(defun reset-optimizer-metrics-instruments ()
  (clrhash *telemetry-opt-by-meter*)
  (setf *telemetry-last-optimizer-instruments* :null))

(defun check-metrics-health ()
  (let ((issues (%new-array)) (meter (get-global "meter")))
    (cond ((not (%telemetry-present-p meter))
           (vector-push-extend "Global meter not initialized" issues))
          ((not (gethash meter *telemetry-gen-by-meter*))
           (vector-push-extend "Metrics instruments not created despite available meter" issues)))
    (object "healthy" (if (zerop (length issues)) true false) "issues" issues)))

(defun record-metric (instruments field value labels &optional (method "record"))
  "Low-level fail-open recording for a factory field. Counters use METHOD \"add\".
LABELS are already sanitized by the named helpers; low-level callers supply them."
  (handler-case
      (let ((instrument (jget instruments field)))
        (when (%telemetry-present-p instrument)
          (telemetry-call instrument method value labels)))
    (error () nil))
  :null)

;;; All label construction is also inside the fail-open boundary.
(defmacro %with-telemetry-labels ((name expression) &body body)
  `(handler-case (let ((,name ,expression)) ,@body :null) (error () :null)))

(defun record-generation-metric (instruments duration success
                                 &key signature-name ai-service model custom-labels)
  (%with-telemetry-labels
      (labels (%telemetry-labels
               (%telemetry-merge (object "success" (%telemetry-boolean-text success))
                                 (%telemetry-optional-label "signature" signature-name)
                                 (%telemetry-optional-label "ai_service" ai-service)
                                 (%telemetry-optional-label "model" model)) custom-labels))
    (record-metric instruments "generationLatencyHistogram" duration labels)
    (record-metric instruments "generationRequestsCounter" 1 labels "add")
    (unless (%telemetry-true-p success)
      (record-metric instruments "generationErrorsCounter" 1 labels "add"))))

(defun record-multi-step-metric (instruments steps-used max-steps &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels))
    (when (> steps-used 1) (record-metric instruments "multiStepGenerationsCounter" 1 labels "add"))
    (record-metric instruments "stepsPerGenerationHistogram" steps-used labels)
    (when (>= steps-used max-steps) (record-metric instruments "maxStepsReachedCounter" 1 labels "add"))))

(defun record-validation-error-metric (instruments error-type &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels "error_type" error-type))
    (when (equal error-type "validation") (record-metric instruments "validationErrorsCounter" 1 labels "add"))))

(defun record-refusal-error-metric (instruments &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels "error_type" "refusal"))
    (record-metric instruments "validationErrorsCounter" 1 labels "add")))

(defun record-error-correction-metric (instruments attempts success max-retries &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels "success" (%telemetry-boolean-text success)))
    (record-metric instruments "errorCorrectionAttemptsHistogram" attempts labels)
    (record-metric instruments (if (%telemetry-true-p success) "errorCorrectionSuccessCounter"
                                  "errorCorrectionFailureCounter") 1 labels "add")
    (when (and (not (%telemetry-true-p success)) (>= attempts max-retries))
      (record-metric instruments "maxRetriesReachedCounter" 1 labels "add"))))

(defun record-function-calling-metric (instruments functions-enabled functions-executed had-function-calls
                                      &key (function-error-correction false) signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels
                                  "functions_enabled" (%telemetry-boolean-text functions-enabled)
                                  "had_function_calls" (%telemetry-boolean-text had-function-calls)))
    (when (%telemetry-true-p functions-enabled) (record-metric instruments "functionsEnabledGenerationsCounter" 1 labels "add"))
    (when (%telemetry-true-p had-function-calls) (record-metric instruments "functionCallStepsCounter" 1 labels "add"))
    (when (> functions-executed 0) (record-metric instruments "functionsExecutedPerGenerationHistogram" functions-executed labels))
    (when (%telemetry-true-p function-error-correction) (record-metric instruments "functionErrorCorrectionCounter" 1 labels "add"))))

(defun record-field-processing-metric (instruments field-processors-executed streaming-field-processors-executed
                                      &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels))
    (when (> field-processors-executed 0) (record-metric instruments "fieldProcessorsExecutedCounter" field-processors-executed labels "add"))
    (when (> streaming-field-processors-executed 0)
      (record-metric instruments "streamingFieldProcessorsExecutedCounter" streaming-field-processors-executed labels "add"))))

(defun record-streaming-metric (instruments is-streaming deltas-emitted &key finalization-duration signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels "is_streaming" (%telemetry-boolean-text is-streaming)))
    (when (%telemetry-true-p is-streaming) (record-metric instruments "streamingGenerationsCounter" 1 labels "add"))
    (when (> deltas-emitted 0) (record-metric instruments "streamingDeltasEmittedCounter" deltas-emitted labels "add"))
    (when (and (%telemetry-present-p finalization-duration) (not (zerop finalization-duration)))
      (record-metric instruments "streamingFinalizationLatencyHistogram" finalization-duration labels))))

(defun record-samples-metric (instruments samples-count result-picker-used &key result-picker-latency signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels "result_picker_used" (%telemetry-boolean-text result-picker-used)))
    (record-metric instruments "samplesGeneratedHistogram" samples-count labels)
    (when (%telemetry-true-p result-picker-used) (record-metric instruments "resultPickerUsageCounter" 1 labels "add"))
    (when (and (%telemetry-present-p result-picker-latency) (not (zerop result-picker-latency)))
      (record-metric instruments "resultPickerLatencyHistogram" result-picker-latency labels))))

(defun record-signature-complexity-metrics (instruments input-fields output-fields examples-count demos-count &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels))
    (loop for field in '("inputFieldsGauge" "outputFieldsGauge" "examplesUsedGauge" "demosUsedGauge")
          for value in (list input-fields output-fields examples-count demos-count)
          do (record-metric instruments field value labels))))

(defun record-performance-metric (instruments metric-type duration &key signature-name custom-labels)
  (%with-telemetry-labels (labels (%telemetry-gen-labels signature-name custom-labels "metric_type" metric-type))
    (let ((field (cdr (assoc metric-type '(("prompt_render" . "promptRenderLatencyHistogram")
                                          ("extraction" . "extractionLatencyHistogram")
                                          ("state_creation" . "stateCreationLatencyHistogram")
                                          ("memory_update" . "memoryUpdateLatencyHistogram")) :test #'equal))))
      (when field (record-metric instruments field duration labels)))))

(defun record-optimization-metric (instruments duration success optimizer-type &key program-signature custom-labels)
  (%with-telemetry-labels
      (labels (%telemetry-labels
               (%telemetry-merge (object "success" (%telemetry-boolean-text success)
                                         "optimizer_type" optimizer-type)
                                 (%telemetry-optional-label "program_signature" program-signature))
               custom-labels t))
    (record-metric instruments "optimizationLatencyHistogram" duration labels)
    (record-metric instruments "optimizationRequestsCounter" 1 labels "add")
    (unless (%telemetry-true-p success) (record-metric instruments "optimizationErrorsCounter" 1 labels "add"))))

(defun record-convergence-metric (instruments rounds current-score improvement stagnation-rounds optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (loop for field in '("convergenceRoundsHistogram" "convergenceScoreGauge" "convergenceImprovementGauge" "stagnationRoundsGauge")
          for value in (list rounds current-score improvement stagnation-rounds)
          do (record-metric instruments field value labels))))

(defun record-early-stopping-metric (instruments reason optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels "reason" reason))
    (record-metric instruments "earlyStoppingCounter" 1 labels "add")))

(defun record-resource-usage-metric (instruments tokens-used cost-incurred optimizer-type &key memory-usage custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (record-metric instruments "tokenUsageCounter" tokens-used labels "add")
    (record-metric instruments "costUsageCounter" cost-incurred labels "add")
    (when (%telemetry-present-p memory-usage) (record-metric instruments "memoryUsageGauge" memory-usage labels))))

(defun record-optimization-duration-metric (instruments duration optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (record-metric instruments "optimizationDurationHistogram" duration labels)))

(defun record-teacher-student-metric (instruments latency score-improvement optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (record-metric instruments "teacherStudentUsageCounter" 1 labels "add")
    (record-metric instruments "teacherStudentLatencyHistogram" latency labels)
    (record-metric instruments "teacherStudentScoreImprovementGauge" score-improvement labels)))

(defun record-checkpoint-metric (instruments operation latency success optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels "operation" operation "success" (%telemetry-boolean-text success)))
    (record-metric instruments (if (equal operation "save") "checkpointSaveCounter" "checkpointLoadCounter") 1 labels "add")
    (record-metric instruments (if (equal operation "save") "checkpointSaveLatencyHistogram" "checkpointLoadLatencyHistogram") latency labels)))

(defun record-pareto-metric (instruments front-size solutions-generated optimizer-type &key hypervolume custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (record-metric instruments "paretoOptimizationsCounter" 1 labels "add")
    (record-metric instruments "paretoFrontSizeHistogram" front-size labels)
    (when (%telemetry-present-p hypervolume) (record-metric instruments "paretoHypervolumeGauge" hypervolume labels))
    (record-metric instruments "paretoSolutionsGeneratedHistogram" solutions-generated labels)))

(defun record-program-complexity-metric (instruments input-fields output-fields examples-count validation-set-size optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (loop for field in '("programInputFieldsGauge" "programOutputFieldsGauge" "examplesCountGauge" "validationSetSizeGauge")
          for value in (list input-fields output-fields examples-count validation-set-size)
          do (record-metric instruments field value labels))))

(defun record-optimizer-performance-metric (instruments metric-type duration optimizer-type &key custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels "metric_type" metric-type))
    (let ((field (cdr (assoc metric-type '(("evaluation" . "evaluationLatencyHistogram")
                                          ("demo_generation" . "demoGenerationLatencyHistogram")
                                          ("metric_computation" . "metricComputationLatencyHistogram")) :test #'equal))))
      (when field (record-metric instruments field duration labels)))))

(defun record-optimizer-configuration-metric (instruments optimizer-type &key target-score max-rounds custom-labels)
  (%with-telemetry-labels (labels (%telemetry-opt-labels optimizer-type custom-labels))
    (record-metric instruments "optimizerTypeGauge" 1 labels)
    (when (%telemetry-present-p target-score) (record-metric instruments "targetScoreGauge" target-score labels))
    (when (%telemetry-present-p max-rounds) (record-metric instruments "maxRoundsGauge" max-rounds labels))))

;;; Use functions rather than mutable constant hash tables: each call is isolated.
(defun span-attributes ()
  (%telemetry-copy (object
   "LLM_SYSTEM" "gen_ai.system" "LLM_OPERATION_NAME" "gen_ai.operation.name"
   "LLM_REQUEST_MODEL" "gen_ai.request.model" "LLM_REQUEST_MAX_TOKENS" "gen_ai.request.max_tokens"
   "LLM_REQUEST_TEMPERATURE" "gen_ai.request.temperature" "LLM_REQUEST_TOP_K" "gen_ai.request.top_k"
   "LLM_REQUEST_FREQUENCY_PENALTY" "gen_ai.request.frequency_penalty"
   "LLM_REQUEST_PRESENCE_PENALTY" "gen_ai.request.presence_penalty"
   "LLM_REQUEST_STOP_SEQUENCES" "gen_ai.request.stop_sequences"
   "LLM_REQUEST_LLM_IS_STREAMING" "gen_ai.request.llm_is_streaming"
   "LLM_REQUEST_TOP_P" "gen_ai.request.top_p" "LLM_RESPONSE_ID" "gen_ai.response.id"
   "LLM_RESPONSE_MODEL" "gen_ai.response.model" "LLM_CONVERSATION_ID" "gen_ai.conversation.id"
   "LLM_USAGE_INPUT_TOKENS" "gen_ai.usage.input_tokens" "LLM_USAGE_OUTPUT_TOKENS" "gen_ai.usage.output_tokens"
   "LLM_USAGE_TOTAL_TOKENS" "gen_ai.usage.total_tokens" "LLM_USAGE_THOUGHTS_TOKENS" "gen_ai.usage.thoughts_tokens"
   "AX_SESSION_ID" "ax.session.id" "AX_PROVIDER_REQUEST_ID" "ax.provider.request_id"
   "AX_PROVIDER_SESSION_ID" "ax.provider.session_id")))

(defun span-events ()
  (%telemetry-copy
   (object "GEN_AI_USER_MESSAGE" "gen_ai.user.message" "GEN_AI_SYSTEM_MESSAGE" "gen_ai.system.message"
          "GEN_AI_ASSISTANT_MESSAGE" "gen_ai.assistant.message" "GEN_AI_TOOL_MESSAGE" "gen_ai.tool.message"
           "GEN_AI_CHOICE" "gen_ai.choice" "GEN_AI_USAGE" "gen_ai.usage")))

;;; Kept here until the parent integrates package.lisp / ASDF. This is the exact
;;; public export list; exporting here also makes direct source loading useful.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(globals-snapshot get-global set-global update-globals reset-globals
            runtime-hook-frame make-runtime-hook-frame runtime-hook-frame-globals
            runtime-hook-frame-resolved get-runtime-hook-frame copy-runtime-options
            options-with-runtime-hook-frame +runtime-hook-frame-key+
            telemetry-call fail-open-span span-call start-span-fail-open start-active-span-fail-open
            default-metrics-config default-optimizer-metrics-config
            get-metrics-config update-metrics-config get-optimizer-metrics-config update-optimizer-metrics-config
            merge-custom-labels create-gen-metrics-instruments create-optimizer-metrics-instruments
            get-or-create-gen-metrics-instruments get-or-create-optimizer-metrics-instruments
            reset-gen-metrics-instruments reset-optimizer-metrics-instruments check-metrics-health
            record-metric record-generation-metric record-multi-step-metric record-validation-error-metric
            record-refusal-error-metric record-error-correction-metric record-function-calling-metric
            record-field-processing-metric record-streaming-metric record-samples-metric
            record-signature-complexity-metrics record-performance-metric
            record-optimization-metric record-convergence-metric record-early-stopping-metric
            record-resource-usage-metric record-optimization-duration-metric record-teacher-student-metric
            record-checkpoint-metric record-pareto-metric record-program-complexity-metric
            record-optimizer-performance-metric record-optimizer-configuration-metric
            span-attributes span-events)))
