;;;; optimize-conformance.lisp --- ir/conformance/axoptimize against the
;;;; native optimizer engines.
;;;;
;;;; These tests read the shared AxIR fixtures directly, the same files every
;;;; other Ax port runs, and compare against each fixture's recorded
;;;; expectation.  Nothing is asserted against this implementation's own
;;;; output.
;;;;
;;;; Coverage is declared, not implied.  Every fixture in the suite is
;;;; classified before anything runs:
;;;;
;;;;   semantic             an operation this file executes through the real
;;;;                        engine or evaluator and checks against the
;;;;                        fixture's expectation
;;;;   explicitly-not-claimed
;;;;                        an operation this file does not run, with the
;;;;                        reason recorded next to it
;;;;
;;;; A fixture whose operation is in neither list is a failure naming the
;;;; operation, so a new fixture kind cannot quietly start passing.  There is
;;;; no catch-all arm: an unknown operation never counts as a pass.
;;;;
;;;; No provider is contacted.  Teachers are scripted functions, and the
;;;; candidate evaluator is the fixture's own score table.

(defpackage #:axllm/optimize-conformance
  (:use #:cl)
  (:export #:run-optimize-conformance-tests #:coverage-report
           #:*conformance-directory* #:empty-fixture-inventory))

(in-package #:axllm/optimize-conformance)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(define-condition fixture-failure (error)
  ((detail :initarg :detail :reader fixture-failure-detail))
  (:report (lambda (condition stream)
             (write-string (fixture-failure-detail condition) stream))))

(define-condition fixture-blocked (error)
  ((detail :initarg :detail :reader fixture-blocked-detail))
  (:report (lambda (condition stream)
             (write-string (fixture-blocked-detail condition) stream)))
  (:documentation
   "A claimed fixture whose upstream surface does not exist yet.

Reported separately from a pass and from a failure: the runner is written
and will exercise the fixture the moment the surface lands, and until then
neither a green tick nor a red cross would be true."))

(defun fail (format-control &rest arguments)
  (error 'fixture-failure :detail (apply #'format nil format-control arguments)))

(defun blocked (detail)
  (error 'fixture-blocked :detail detail))

(define-condition empty-fixture-inventory (error)
  ((directory :initarg :directory :reader empty-fixture-inventory-directory))
  (:report (lambda (condition stream)
             (format stream "no axoptimize fixtures under ~a: the suite has nothing to run, ~
which is a broken checkout or a wrong AXIR_CONFORMANCE_DIR rather than a pass"
                     (empty-fixture-inventory-directory condition))))
  (:documentation
   "Signalled when the fixture root holds no fixtures.

An empty inventory is the one failure that looks exactly like success: every
count is zero, nothing is reported, and a gate that checks for zero failures
is satisfied by a suite that did not run.  It is refused here, at the single
place the inventory comes from, so no caller can be the one that forgets."))

(defvar *conformance-directory* nil
  "Fixture root override for a caller that must point the runner elsewhere.

Takes precedence over AXIR_CONFORMANCE_DIR.  A special variable rather than
an argument so a test can rebind it around the whole runner without
mutating the process environment that every other test shares.")

(defun conformance-directory ()
  (or *conformance-directory*
      (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
        (if (and override (plusp (length override)))
            (uiop:ensure-directory-pathname override)
            (asdf:system-relative-pathname "axllm" "../../ir/conformance/")))))

(defun fixture-files ()
  "Every axoptimize fixture, in a stable order.

Refuses an empty inventory rather than returning one: see
EMPTY-FIXTURE-INVENTORY."
  (let ((files (sort (directory (merge-pathnames "axoptimize/*.json" (conformance-directory)))
                     #'string< :key #'namestring)))
    (unless files
      (error 'empty-fixture-inventory :directory (conformance-directory)))
    files))

(defun read-fixture (path)
  (ax:parse-json (uiop:read-file-string path)))

(defun jget (object key &optional (default :null)) (ax:jget object key default))

(defun show (value) (if (stringp value) (format nil "~s" value) (ax:encode-json value)))

(defun same-value-p (left right) (axllm/core::core-value-equal left right))

(defun present (value) (if (eq value :null) nil value))

(defun elements (value)
  (cond ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((eq value :null) '())
        ((listp value) value)
        (t (list value))))

(defun assert-equal (actual expected label)
  (unless (same-value-p actual expected)
    (fail "~a mismatch~%    expected: ~a~%    actual:   ~a" label (show expected) (show actual))))

(defun assert-list-subset (actual expected label)
  "Every item EXPECTED names must appear in ACTUAL, in order.

The match is a subsequence, not index by index: a fixture lists the entries
it cares about in the order it expects them, and the program may carry more
in between.  Index-by-index comparison is what made
agent-stage-instruction-apply look broken when the program was right."
  (unless (and (vectorp actual) (not (stringp actual)))
    (fail "~a: expected an array, got ~a" label (show actual)))
  (let ((cursor 0))
    (dolist (want (elements expected))
      (let ((matched nil))
        (loop for index from cursor below (length actual)
              do (handler-case
                     (progn (assert-subset (aref actual index) want
                                           (format nil "~a[~a]" label index))
                            (setf cursor (1+ index) matched t))
                   (fixture-failure () nil))
              until matched)
        (unless matched
          (fail "~a is missing the expected entry ~a~%    actual: ~a"
                label (show want) (show actual)))))))

(defun assert-subset (actual expected label)
  "Every key EXPECTED carries must match ACTUAL; ACTUAL may carry more.

Objects recurse, arrays compare element-wise with the same rule, and
anything else must be equal."
  (cond
    ((hash-table-p expected)
     (unless (hash-table-p actual)
       (fail "~a: expected an object, got ~a" label (show actual)))
     (dolist (key (axllm::%object-keys expected))
       (assert-subset (jget actual key) (gethash key expected)
                      (format nil "~a.~a" label key))))
    ((and (vectorp expected) (not (stringp expected)))
     (unless (and (vectorp actual) (not (stringp actual)))
       (fail "~a: expected an array, got ~a" label (show actual)))
     (unless (>= (length actual) (length expected))
       (fail "~a: expected at least ~a element(s), got ~a"
             label (length expected) (length actual)))
     (loop for index from 0 below (length expected)
           do (assert-subset (aref actual index) (aref expected index)
                             (format nil "~a[~a]" label index))))
    (t (assert-equal actual expected label))))

;;; ------------------------------------------------------------------
;;; Declared coverage
;;; ------------------------------------------------------------------

(defparameter +semantic-operations+
  '("gepa" "bootstrap" "ace-compile" "ace-online-update" "dataset" "score" "evidence"
    "components" "filter" "artifact" "apply" "evaluate" "engine" "helper" "judge_payload"
    "playbook-empty" "playbook-render" "playbook-stats" "playbook-dedupe"
    "playbook-feedback" "playbook-apply-ops" "eval" "playbook-evolve" "verification")
  "Operations this file runs through the real optimizer surface.

gepa, bootstrap, ace-compile and ace-online-update drive the native engines.
dataset, score and evidence drive the evaluator's dataset normalization, its
scoring path and the evidence batch an engine sends its teacher.")

(defparameter +not-claimed-operations+
  '()
  "Operations this file does not run, each with the reason.

Empty: every fixture kind in the suite is claimed and has a runner, so no
fixture is scored as a pass without an implementation path behind it.")

;;; ------------------------------------------------------------------
;;; The fixture's score table as a candidate evaluator
;;; ------------------------------------------------------------------
;;;
;;; A fixture decides a candidate's score from the text that candidate puts
;;; in one component, so an engine's selection is checked against a ranking
;;; the fixture states and the engine never sees.

(defclass scripted-evaluator ()
  ((fixture :initarg :fixture :reader scripted-fixture)
   (evaluations :initform '() :accessor scripted-evaluations)
   (options-seen :initform '() :accessor scripted-options-seen)))

(defun scored-component-id (fixture)
  (or (present (jget fixture "score_component_id"))
      (let ((components (elements (jget fixture "components"))))
        (and components (present (jget (first components) "id"))))))

(defun scripted-raw-score (fixture component-value index)
  "The score the fixture assigns the COMPONENT-VALUE at task INDEX."
  (let ((table (jget fixture "gepa_scores")))
    (if (hash-table-p table)
        (let ((entry (let ((direct (jget table component-value :missing)))
                       (if (eq direct :missing) (jget table "*" 0) direct))))
          (if (and (vectorp entry) (not (stringp entry)) (plusp (length entry)))
              (aref entry (min index (1- (length entry))))
              entry))
        :from-task)))

(defmethod ax:evaluate-candidate ((evaluator scripted-evaluator) candidate-map options)
  (let* ((fixture (scripted-fixture evaluator))
         (recorded (ax:object)))
    (dolist (key (axllm::%object-keys options))
      (unless (string= key "dataset")
        (axllm::%set-key recorded key (gethash key options))))
    (push recorded (scripted-options-seen evaluator))
    (let* ((dataset (let ((given (jget options "dataset")))
                      (if (eq given :null) (jget fixture "dataset") given)))
           (normalized (axllm/core::normalize-optimization-dataset dataset))
           (component-id (scored-component-id fixture))
           (component-value (let ((given (jget candidate-map component-id :missing)))
                              (if (eq given :missing)
                                  (jget fixture "base_component_value" "")
                                  given)))
           (rows (ax:object))
           (row-vector (axllm::%new-array)))
      (declare (ignore rows))
      (loop for task in (elements (jget normalized "train"))
            for index from 0
            do (let* ((raw (let ((scripted (scripted-raw-score fixture component-value index)))
                             (if (eq scripted :from-task)
                                 (let ((metric (jget task "metric_score" :missing)))
                                   (if (eq metric :missing)
                                       (let ((scores (jget task "scores" :missing)))
                                         (if (eq scores :missing) (jget task "score" 0) scores))
                                       metric))
                                 scripted)))
                      (scores (axllm/core::normalize-optimization-metric-scores raw))
                      (scalar (axllm/core::scalarize-optimization-scores
                               scores (let ((given (jget fixture "score_options")))
                                        (if (hash-table-p given) given (ax:object)))))
                      (prediction (ax:object "completionType" "final"
                                             "output" (ax:object "componentValue" component-value)
                                             "finalOutput" (ax:object "componentValue" component-value)
                                             "functionCalls" (axllm::%new-array)
                                             "actionLog" (axllm::%new-array)
                                             "usage" (ax:object)
                                             "trace" (ax:object "componentValue" component-value))))
                 (vector-push-extend
                  (axllm/core::build-optimization-eval-row
                   task prediction scores scalar (jget prediction "trace") :null)
                  row-vector)))
      (let ((result (axllm/core::build-optimization-eval-result
                     row-vector
                     (if (hash-table-p candidate-map) candidate-map (ax:object))
                     (let ((phase (present (jget options "phase")))) (or phase "train")))))
        (push result (scripted-evaluations evaluator))
        result))))

(defun build-request (fixture)
  "The optimizer request a GEPA or BootstrapFewShot fixture describes."
  (ax:object "contractVersion" "axir-optimize-contract-v1"
             "programKind" (let ((kind (present (jget fixture "program")))) (or kind "axgen"))
             "components" (jget fixture "components" (axllm::%new-array))
             "dataset" (axllm/core::normalize-optimization-dataset
                        (jget fixture "dataset" (axllm::%new-array)))
             "options" (let ((given (jget fixture "optimize_options")))
                         (if (hash-table-p given) given (ax:object)))
             "trace" (ax:object)
             "evaluator" (ax:object "available" ax:true
                                    "contractVersion" "axir-optimizer-evaluator-v1")))

;;; ------------------------------------------------------------------
;;; The scripted teacher
;;; ------------------------------------------------------------------

(defun expensive-model-refusal (fixture options)
  "The message a provider returns when an expensive teacher is not confirmed.

The gate itself belongs to the provider layer, not the optimizer; it is
reproduced here because the fixture asserts what GEPA does when a teacher
call fails, and this is the failure it scripts."
  (let* ((client (jget fixture "reflection_client"))
         (model (present (jget client "model")))
         (info (elements (jget (jget client "options") "modelInfo")))
         (expensive (find-if (lambda (entry)
                               (and (equal (present (jget entry "name")) model)
                                    (ax:json-true-p (jget entry "isExpensive"))))
                             info))
         (confirmed (equal (present (jget (jget options "teacherOptions") "useExpensiveModel"))
                           "yes")))
    (when (and expensive (not confirmed))
      (format nil "Model ~a is marked as expensive and requires explicit confirmation. Set useExpensiveModel: \"yes\" to proceed."
              model))))

(defun make-scripted-reflection (fixture requests)
  "A GEPA reflection callback answering from the fixture's scripted replies.

Replies are read exactly as MAKE-AI-REFLECTION-CALLBACK reads a provider
reply, so the \"New Value:\" and fenced-block rules are the ones under test."
  (let ((queue (elements (jget fixture "reflection_responses"))))
    (lambda (payload)
      (let ((refusal (expensive-model-refusal
                      fixture
                      (ax:object "teacherOptions" (jget payload "teacherOptions")))))
        (when refusal (error 'ax:ax-error :message refusal)))
      (push payload (cdr requests))
      (let ((response (if queue (pop queue) (error 'ax:ax-error :message "scripted teacher exhausted"))))
        (axllm::%gepa-extract-text
         (present (jget (aref (jget response "results") 0) "content")))))))


;;; ------------------------------------------------------------------
;;; Building the program a fixture describes
;;; ------------------------------------------------------------------
;;;
;;; The real AxGen, AxAgent and AxFlow, built from the fixture's own
;;; signature, options, tools and steps.  Nothing is simulated: a component
;;; inventory, a target filter and an applied artifact all run against the
;;; program the other ports build from the same fixture.

(defun tool-parameters (spec)
  "A fixture's shorthand arg map as the JSON Schema object Ax tools take."
  (let ((args (jget spec "args")))
    (if (not (hash-table-p args))
        (ax:object "type" "object" "properties" (ax:object) "required" (axllm::%new-array))
        (let ((properties (ax:object))
              (required (axllm::%new-array)))
          (dolist (name (axllm::%object-keys args))
            (let* ((declared (gethash name args))
                   (type (if (hash-table-p declared)
                             (let ((given (present (jget declared "type")))) (or given "string"))
                             "string")))
              (axllm::%set-key properties name (ax:object "type" type))
              (vector-push-extend name required)))
          (ax:object "type" "object" "properties" properties "required" required)))))

(defun build-tools (specs)
  "The fixture's tool specs as Ax tools.

The result the fixture declares is what the handler returns, so a tool that
is actually called answers the fixture rather than an empty string."
  (let ((out '()))
    (dolist (spec (elements specs) (nreverse out))
      (let ((result (jget spec "result")))
        (push (ax:tool :name (present (jget spec "name"))
                       :description (let ((text (present (jget spec "description")))) (or text ""))
                       :parameters (tool-parameters spec)
                       :handler (lambda (arguments)
                                  (declare (ignore arguments))
                                  (if (eq result :null) "" result)))
              out)))))

(defun build-flow-program (fixture)
  "The flow a fixture's steps describe, nested flows included."
  (let* ((id (let ((given (present (jget fixture "program_id")))) (or given "root.flow")))
         (built (ax:flow (ax:object "id" id))))
    (dolist (step (elements (jget fixture "steps")))
      (let* ((name (present (jget step "name")))
             (options (let ((given (jget step "options")))
                        (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
             (node (if (equal (present (jget step "program")) "flow")
                       ;; A nested flow is named from its own step, not by
                       ;; inheriting the parent's program_id: the Python
                       ;; conformance runner builds one with
                       ;; {"id": step.get("program_id", f"root.{name}")}.
                       ;; Inheriting instead gave the inner and outer flow
                       ;; the same id, which is what made their graph
                       ;; components look like a collision in flow.
                       (build-flow-program
                        (let ((nested (axllm::%opt-clone step)))
                          (unless (present (jget nested "program_id"))
                            (axllm::%set-key nested "program_id"
                                             (format nil "root.~a" name)))
                          nested))
                       (ax:ax (let ((signature (present (jget step "signature"))))
                                (or signature "question:string -> answer:string"))
                              :id (present (jget options "id"))
                              :instruction (present (jget options "instruction"))))))
        (ax:flow-execute built name node options)))
    (let ((returns (jget fixture "returns")))
      (when (hash-table-p returns) (ax:flow-returns built returns)))
    built))

(defun build-program (fixture)
  "The program this fixture is about."
  (let* ((kind (let ((given (present (jget fixture "program")))) (or given "agent")))
         (signature (let ((given (present (jget fixture "signature"))))
                      (or given "question:string -> answer:string")))
         (options (let ((given (jget fixture "options")))
                    (if (hash-table-p given) (axllm::%opt-clone given) (ax:object)))))
    (cond ((string= kind "axgen")
           (ax:ax signature
                  :tools (build-tools (jget fixture "tools"))
                  :id (present (jget options "id"))
                  :instruction (present (jget options "instruction"))))
          ((string= kind "flow") (build-flow-program fixture))
          (t (ax:agent signature :options options)))))

(defun fixture-program-kind (fixture)
  (let ((given (present (jget fixture "program")))) (or given "agent")))

;;; A client whose transport replays the fixture's scripted provider
;;; responses.  No network, no credentials, no paid provider.

(defstruct (script (:conc-name script-)) queue (requests '()))

(defun scripted-body (response)
  (let* ((content (if (hash-table-p response)
                      (let ((given (present (jget response "content")))) (or given ""))
                      (format nil "~a" response)))
         (usage (and (hash-table-p response) (jget response "usage"))))
    (ax:encode-json
     (ax:object "choices" (axllm::%opt-array
                           (list (ax:object "index" 0
                                            "finish_reason" "stop"
                                            "message" (ax:object "role" "assistant"
                                                                 "content" content))))
                "usage" (if (hash-table-p usage)
                            usage
                            (ax:object "prompt_tokens" 1 "completion_tokens" 1 "total_tokens" 2))))))

(defun scripted-client (responses)
  "(values CLIENT SCRIPT) replaying RESPONSES in order."
  (let ((script (make-script :queue (mapcar #'scripted-body (elements responses)))))
    (values (ax:ai :name "openai" :model "gpt-5.4-mini" :api-key "sk-test-dummy-key"
                   :transport (lambda (url headers body)
                                (declare (ignore url headers))
                                (push body (script-requests script))
                                (if (script-queue script)
                                    (values (pop (script-queue script)) 200)
                                    (fail "the fixture sent more provider requests than it scripted"))))
            script)))

(defun component-ids (components)
  (axllm::%opt-array (mapcar (lambda (component) (jget component "id")) (elements components))))

(defun fixture-artifact (fixture)
  "The artifact a fixture describes, built by Core's own constructor."
  (axllm/core::optimized-artifact
   "fixture" "1"
   (let ((given (jget fixture "component_map"))) (if (hash-table-p given) given (ax:object)))
   (let ((given (jget fixture "metadata"))) (if (hash-table-p given) given (ax:object)))))

;;; ------------------------------------------------------------------
;;; Operation runners
;;; ------------------------------------------------------------------

(defun run-gepa (fixture)
  (let* ((requests (cons :requests '()))
         (notifications '())
         (options (let ((given (jget fixture "gepa_options")))
                    (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
         (engine (progn
                   (when (axllm::%opt-key-present-p fixture "expected_notifications")
                     (axllm::%set-key options "logger"
                                      (lambda (event) (push event notifications))))
                   (ax:make-gepa :reflection (make-scripted-reflection fixture requests)
                                 :options options)))
         (evaluator (make-instance 'scripted-evaluator :fixture fixture))
         (artifact (ax:run-optimizer-engine engine (build-request fixture) evaluator)))
    (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
      (assert-subset artifact (jget fixture "expected_artifact_subset") "GEPA artifact"))
    (when (axllm::%opt-key-present-p fixture "expected_gepa_evaluations_subset")
      (assert-list-subset (axllm::%opt-array (reverse (scripted-evaluations evaluator)))
                     (jget fixture "expected_gepa_evaluations_subset") "GEPA evaluations"))
    (when (axllm::%opt-key-present-p fixture "expected_reflection_request_count")
      (assert-equal (length (cdr requests))
                    (jget fixture "expected_reflection_request_count")
                    "GEPA reflection request count"))
    (when (axllm::%opt-key-present-p fixture "expected_notifications")
      (assert-notifications (reverse notifications) (jget fixture "expected_notifications")))))

(defun assert-notifications (actual expected)
  (let ((expected (elements expected)))
    (unless (= (length actual) (length expected))
      (fail "notification count mismatch~%    expected: ~a~%    actual:   ~a"
            (length expected) (show (axllm::%opt-array actual))))
    (loop for event in actual
          for want in expected
          do (dolist (key (axllm::%object-keys want))
               (if (string= key "value_contains")
                   (dolist (fragment (elements (gethash key want)))
                     (unless (search fragment (format nil "~a" (present (jget event "value"))))
                       (fail "notification value is missing ~s~%    actual: ~a"
                             fragment (show (jget event "value")))))
                   (assert-equal (jget event key) (gethash key want)
                                 (format nil "notification ~a" key)))))))

(defun run-bootstrap (fixture)
  (let* ((engine (ax:make-bootstrap-few-shot
                  (let ((given (jget fixture "optimize_options")))
                    (if (hash-table-p given) given (ax:object)))))
         (evaluator (make-instance 'scripted-evaluator :fixture fixture))
         (artifact (ax:run-optimizer-engine engine (build-request fixture) evaluator)))
    (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
      (assert-subset artifact (jget fixture "expected_artifact_subset") "BootstrapFewShot artifact"))
    (when (axllm::%opt-key-present-p fixture "expected_demo_count")
      (assert-equal (length (jget artifact "demos")) (jget fixture "expected_demo_count")
                    "BootstrapFewShot demo count"))
    (when (axllm::%opt-key-present-p fixture "expected_gepa_evaluations_subset")
      (assert-list-subset (axllm::%opt-array (reverse (scripted-evaluations evaluator)))
                     (jget fixture "expected_gepa_evaluations_subset")
                     "BootstrapFewShot evaluations"))
    (when (axllm::%opt-key-present-p fixture "expected_evaluate_options_subset")
      (assert-list-subset (axllm::%opt-array (reverse (scripted-options-seen evaluator)))
                     (jget fixture "expected_evaluate_options_subset")
                     "BootstrapFewShot evaluate options"))))

(defun queue-popper (items)
  (let ((queue (elements items)))
    (lambda (&rest ignored)
      (declare (ignore ignored))
      (if queue (axllm::%opt-clone (pop queue)) :null))))

(defun run-ace (fixture operation)
  (let* ((reflections (queue-popper (jget fixture "reflection_responses")))
         (curators (queue-popper (jget fixture "curator_responses")))
         (predictions (queue-popper (jget fixture "generator_predictions")))
         (scores (elements (jget fixture "metric_scores")))
         (metric (lambda (prediction example)
                   (declare (ignore prediction example))
                   (if scores (pop scores) 0)))
         (options (let ((given (jget fixture "ace_options")))
                    (if (hash-table-p given) (axllm::%opt-clone given) (ax:object)))))
    (axllm::%set-key options "now"
                     (let ((now (present (jget fixture "now"))))
                       (or now "1970-01-01T00:00:00.000Z")))
    (when (present (jget fixture "initial_playbook"))
      (axllm::%set-key options "initialPlaybook" (jget fixture "initial_playbook")))
    (let ((ace (ax:make-ace :reflector (lambda (payload)
                                         (let ((value (funcall reflections payload)))
                                           (if (eq value :null) nil value)))
                            :curator (lambda (payload)
                                       (let ((value (funcall curators payload)))
                                         (if (eq value :null) nil value)))
                            :generator (lambda (example)
                                         (let ((value (funcall predictions example)))
                                           (if (eq value :null) (ax:object) value)))
                            :metric metric
                            :options options)))
      (if (string= operation "ace-compile")
          (let ((result (ax:ace-compile ace (jget fixture "examples" (axllm::%new-array)))))
            (when (axllm::%opt-key-present-p fixture "expected_playbook")
              (assert-equal (ax:ace-playbook ace) (jget fixture "expected_playbook")
                            "ace compile playbook"))
            (when (axllm::%opt-key-present-p fixture "expected_artifact")
              (assert-equal (ax:ace-artifact ace) (jget fixture "expected_artifact")
                            "ace compile artifact"))
            (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
              (assert-subset (ax:ace-artifact ace) (jget fixture "expected_artifact_subset")
                             "ace compile artifact"))
            (when (axllm::%opt-key-present-p fixture "expected_result_subset")
              (assert-subset result (jget fixture "expected_result_subset") "ace compile result")))
          (let* ((update (let ((given (jget fixture "update")))
                           (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
                 (curator-result (progn
                                   (unless (axllm::%opt-key-present-p update "prediction")
                                     (axllm::%set-key update "prediction"
                                                      (funcall predictions (jget update "example"))))
                                   (ax:ace-apply-online-update ace update))))
            (when (axllm::%opt-key-present-p fixture "expected_playbook")
              (assert-equal (ax:ace-playbook ace) (jget fixture "expected_playbook")
                            "ace online playbook"))
            (when (axllm::%opt-key-present-p fixture "expected_artifact")
              (assert-equal (ax:ace-artifact ace) (jget fixture "expected_artifact")
                            "ace online artifact"))
            (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
              (assert-subset (ax:ace-artifact ace) (jget fixture "expected_artifact_subset")
                             "ace online artifact"))
            (when (axllm::%opt-key-present-p fixture "expected_curator")
              (assert-equal curator-result (jget fixture "expected_curator")
                            "ace online curator")))))))

(defun run-dataset (fixture)
  (assert-equal (axllm/core::normalize-optimization-dataset
                 (jget fixture "dataset" (axllm::%new-array)))
                (jget fixture "expected_dataset")
                "normalized dataset"))

(defun run-score (fixture)
  (let* ((scores (axllm/core::normalize-optimization-metric-scores (jget fixture "metric_score")))
         (options (let ((given (jget fixture "score_options")))
                    (if (hash-table-p given) given (ax:object))))
         (scalar (axllm/core::scalarize-optimization-scores scores options))
         (prediction (let ((given (jget fixture "prediction")))
                       (if (hash-table-p given)
                           given
                           (ax:object "functionCalls" (axllm::%new-array)))))
         (task (let ((given (jget fixture "task"))) (if (hash-table-p given) given (ax:object))))
         (adjusted (axllm/core::adjust-optimization-score-for-actions scalar task prediction)))
    (when (axllm::%opt-key-present-p fixture "expected_scores")
      (assert-equal scores (jget fixture "expected_scores") "metric scores"))
    (when (axllm::%opt-key-present-p fixture "expected_scalar")
      (assert-equal adjusted (jget fixture "expected_scalar") "metric scalar"))))

(defun run-evidence (fixture)
  (let* ((components (jget fixture "components" (axllm::%new-array)))
         (eval-result (let ((given (jget fixture "eval_result")))
                        (if (hash-table-p given) given (ax:object))))
         (evidence (ax:optimizer-evidence-batch eval-result components)))
    (when (axllm::%opt-key-present-p fixture "expected_evidence_subset")
      (assert-subset evidence (jget fixture "expected_evidence_subset") "optimizer evidence"))))


(defun run-components (fixture)
  (let ((components (axllm::program-optimizable-components (build-program fixture))))
    (when (axllm::%opt-key-present-p fixture "expected_components_subset")
      (assert-list-subset components (jget fixture "expected_components_subset")
                    "optimizable components"))
    (when (axllm::%opt-key-present-p fixture "expected_component_ids")
      (assert-equal (component-ids components) (jget fixture "expected_component_ids")
                    "component ids"))))

(defun run-filter (fixture)
  (let* ((components (axllm::program-optimizable-components (build-program fixture)))
         (target (let ((given (present (jget fixture "target")))) (or given "all")))
         (filtered (axllm/core::filter-optimization-components components target)))
    (assert-equal (component-ids filtered)
                  (jget fixture "expected_component_ids" (axllm::%new-array))
                  "filtered component ids")))

(defun run-artifact (fixture)
  (let* ((program (build-program fixture))
         (components (axllm::program-optimizable-components program))
         (validated (axllm/core::validate-optimized-artifact (fixture-artifact fixture) components))
         (decoded (axllm/core::deserialize-optimized-artifact
                   (axllm/core::serialize-optimized-artifact validated) components)))
    (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
      (assert-subset decoded (jget fixture "expected_artifact_subset") "optimized artifact"))))

(defun run-apply (fixture)
  (let* ((program (build-program fixture))
         (before (axllm::%opt-clone (axllm::program-optimizable-components program)))
         (artifact (axllm/core::validate-optimized-artifact (fixture-artifact fixture) before))
         (payload (if (ax:json-true-p (jget fixture "serialized_artifact"))
                      (axllm/core::serialize-optimized-artifact artifact)
                      artifact)))
    (ax:apply-optimization program payload)
    (let ((after (axllm::program-optimizable-components program)))
      (when (axllm::%opt-key-present-p fixture "expected_components_subset")
        (assert-list-subset after (jget fixture "expected_components_subset") "optimized components"))
      (when (axllm::%opt-key-present-p fixture "expected_changed_components")
        (assert-equal (axllm/core::optimization-changed-components
                       before
                       (let ((given (jget fixture "component_map")))
                         (if (hash-table-p given) given (ax:object))))
                      (jget fixture "expected_changed_components")
                      "changed components")))))

(defun fixture-evaluator (fixture program client)
  "The candidate evaluator a fixture's program and scripted client give."
  (ax:make-program-evaluator
   program client
   :dataset (jget fixture "dataset" (axllm::%new-array))
   :options (let ((given (jget fixture "eval_options")))
              (if (hash-table-p given) (axllm::%opt-clone given) (ax:object)))
   :max-metric-calls (let ((limit (jget (jget fixture "eval_options") "maxMetricCalls")))
                       (when (realp limit) (floor limit)))))

(defun run-evaluate (fixture)
  (let* ((program (build-program fixture))
         (client (scripted-client (jget fixture "responses")))
         (evaluator (fixture-evaluator fixture program client))
         (before (axllm::%opt-clone (axllm::program-optimizable-components program)))
         (result (ax:evaluate-candidate
                  evaluator
                  (let ((given (jget fixture "candidate_map")))
                    (if (hash-table-p given) given (ax:object)))
                  (let ((given (jget fixture "eval_options")))
                    (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))))
    (declare (ignorable before))
    (when (axllm::%opt-key-present-p fixture "expected_evaluation_subset")
      (assert-subset result (jget fixture "expected_evaluation_subset") "optimization evaluation"))
    (when (axllm::%opt-key-present-p fixture "expected_evaluation_rows_subset")
      (assert-list-subset (jget result "rows") (jget fixture "expected_evaluation_rows_subset")
                     "optimization evaluation rows"))
    (when (axllm::%opt-key-present-p fixture "expected_components_subset_after")
      (assert-list-subset (axllm::program-optimizable-components program)
                          (jget fixture "expected_components_subset_after")
                     "post-eval components"))))

;;; The fixture's scripted optimizer engine: it returns the artifact the
;;; fixture names, and when the fixture asks, really drives the evaluator
;;; first so the rollouts and the transcript are measured, not invented.

(defclass scripted-engine (ax:optimizer-engine)
  ((response :initarg :response :reader scripted-response)
   (requests :initform '() :accessor scripted-requests)
   (evaluations :initform '() :accessor scripted-engine-evaluations)
   (transcripts :initform '() :accessor scripted-transcripts)))

(defmethod ax:optimizer-engine-name ((engine scripted-engine)) "scripted")
(defmethod ax:optimizer-engine-version ((engine scripted-engine)) "1")

(defun scripted-engine-step (engine evaluator request step)
  "Evaluate one scripted candidate and record what came back."
  (let* ((candidate (let ((given (jget step "component_map")))
                      (if (hash-table-p given) given
                          (let ((camel (jget step "componentMap")))
                            (if (hash-table-p camel) camel (ax:object))))))
         (options (let ((given (jget step "options")))
                    (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
         (result (ax:evaluate-candidate evaluator candidate options))
         (evidence (ax:optimizer-evidence-batch
                    result (jget request "components" (axllm::%new-array)))))
    (push (axllm::%opt-clone result) (scripted-engine-evaluations engine))
    (push (ax:object "candidateMap" (axllm::%opt-clone candidate)
                     "options" options
                     "result" (axllm::%opt-clone result)
                     "evidence" evidence)
          (scripted-transcripts engine))
    (values candidate result)))

(defmethod ax:run-optimizer-engine ((engine scripted-engine) request evaluator)
  (push (axllm::%opt-clone request) (scripted-requests engine))
  (let ((response (scripted-response engine)))
    ;; A reference engine really measures each candidate and keeps the best
    ;; it saw.  The comparison is strict, so the first of equally good
    ;; candidates wins, which is what the fixture's name asserts.
    (when (and evaluator (axllm::%opt-key-present-p response "referenceCandidates"))
      (let ((best-map (ax:object))
            (best-score nil))
        (dolist (step (elements (jget response "referenceCandidates")))
          (multiple-value-bind (candidate result)
              (scripted-engine-step engine evaluator request step)
            (let ((score (let ((avg (jget result "avg"))) (if (realp avg) avg 0))))
              (when (or (null best-score) (> score best-score))
                (setf best-score score
                      best-map (axllm::%opt-clone candidate))))))
        (return-from ax:run-optimizer-engine
          (ax:object "componentMap" best-map
                     "metadata" (ax:object "referenceEngine" ax:true
                                           "evaluations"
                                           (axllm::%opt-array
                                            (reverse (scripted-transcripts engine))))))))
    (when evaluator
      (dolist (step (elements (jget response "evaluate")))
        (scripted-engine-step engine evaluator request step)))
    (axllm::%opt-clone response)))

(defun run-engine (fixture)
  (let* ((program (build-program fixture))
         (engine (make-instance 'scripted-engine
                                :response (let ((given (jget fixture "engine_response")))
                                            (if (hash-table-p given) given (ax:object)))))
         (options (let ((given (jget fixture "optimize_options")))
                   (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
         (client (when (ax:json-true-p (jget fixture "engine_uses_evaluator"))
                   (scripted-client (jget fixture "responses"))))
         (artifact (ax:optimize-program program (jget fixture "dataset" (axllm::%new-array))
                                        :engine engine :client client :options options)))
    (when (axllm::%opt-key-present-p fixture "expected_engine_request_subset")
      (unless (scripted-requests engine) (fail "the optimizer engine was not called"))
      (assert-subset (first (last (scripted-requests engine)))
                     (jget fixture "expected_engine_request_subset") "optimizer engine request"))
    (when (axllm::%opt-key-present-p fixture "expected_engine_evaluations_subset")
      (assert-list-subset (axllm::%opt-array (reverse (scripted-engine-evaluations engine)))
                     (jget fixture "expected_engine_evaluations_subset")
                     "optimizer engine evaluations"))
    (when (axllm::%opt-key-present-p fixture "expected_engine_transcripts_subset")
      (assert-list-subset (axllm::%opt-array (reverse (scripted-transcripts engine)))
                     (jget fixture "expected_engine_transcripts_subset")
                     "optimizer engine transcripts"))
    (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
      (assert-subset artifact (jget fixture "expected_artifact_subset") "optimizer artifact"))
    (when (axllm::%opt-key-present-p fixture "expected_components_subset")
      (assert-list-subset (axllm::program-optimizable-components program)
                          (jget fixture "expected_components_subset") "optimized components"))))

(defun run-helper (fixture)
  (let* ((program (build-program fixture))
         (client (scripted-client (jget fixture "responses")))
         (options (let ((given (jget fixture "optimize_options")))
                    (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
         (artifact (ax:optimize-program program (jget fixture "dataset" (axllm::%new-array))
                                        :client client :options options)))
    (when (axllm::%opt-key-present-p fixture "expected_artifact_subset")
      (assert-subset artifact (jget fixture "expected_artifact_subset") "optimize helper artifact"))
    (when (axllm::%opt-key-present-p fixture "expected_demo_count")
      (assert-equal (length (jget artifact "demos" (axllm::%new-array)))
                    (jget fixture "expected_demo_count") "optimize helper demo count"))
    (when (axllm::%opt-key-present-p fixture "expected_components_subset")
      (assert-list-subset (axllm::program-optimizable-components program)
                          (jget fixture "expected_components_subset") "post-helper components"))))

(defun run-judge-payload (fixture)
  (let ((payload (axllm/core::build-optimization-judge-payload
                  (let ((given (jget fixture "task"))) (if (hash-table-p given) given (ax:object)))
                  (let ((given (jget fixture "prediction")))
                    (if (hash-table-p given) given (ax:object)))
                  (let ((given (present (jget fixture "criteria")))) (or given "")))))
    (when (axllm::%opt-key-present-p fixture "expected_judge_payload_subset")
      (assert-subset payload (jget fixture "expected_judge_payload_subset") "judge payload"))))

(defun fixture-now (fixture)
  (let ((given (present (jget fixture "now")))) (or given "")))

(defun run-playbook (fixture operation)
  "The Core ACE playbook ops the ACE driver is built on, run directly."
  (let ((playbook (axllm::%opt-clone
                   (let ((given (jget fixture "playbook")))
                     (if (hash-table-p given) given (ax:object))))))
    (cond
      ((string= operation "playbook-empty")
       (assert-equal (axllm/core::ace-empty-playbook (jget fixture "description")
                                                     (fixture-now fixture))
                     (jget fixture "expected_playbook") "ace empty playbook"))
      ((string= operation "playbook-render")
       (assert-equal (axllm/core::ace-render-playbook playbook)
                     (jget fixture "expected_render") "ace rendered playbook"))
      ((string= operation "playbook-stats")
       (assert-equal (axllm/core::ace-recompute-playbook-stats playbook)
                     (jget fixture "expected_playbook") "ace recomputed stats"))
      ((string= operation "playbook-dedupe")
       (assert-equal (axllm/core::ace-dedupe-playbook playbook)
                     (jget fixture "expected_playbook") "ace deduped playbook"))
      ((string= operation "playbook-feedback")
       (assert-equal (axllm/core::ace-update-bullet-feedback
                      playbook
                      (let ((given (present (jget fixture "bullet_id")))) (or given ""))
                      (let ((given (present (jget fixture "tag")))) (or given ""))
                      (fixture-now fixture))
                     (jget fixture "expected_playbook") "ace bullet feedback"))
      ((string= operation "playbook-apply-ops")
       (assert-equal (axllm/core::ace-apply-curator-operations
                      playbook
                      (jget fixture "operations" (axllm::%new-array))
                      (let ((given (jget fixture "apply_options")))
                        (if (hash-table-p given) given (ax:object)))
                      (fixture-now fixture))
                     (jget fixture "expected_result") "ace applied operations"))
      (t (fail "unhandled playbook operation ~s" operation)))))


;;; ------------------------------------------------------------------
;;; Agent evaluation predictions
;;; ------------------------------------------------------------------
;;;
;;; An agent's eval prediction is built by the agent, not by the optimizer:
;;; it marks a run, runs once, and asks Core for the prediction.  The
;;; optimizer owns the fixture runner, the agent owns the method, so this
;;; resolves the agent's surface at run time instead of duplicating it.
;;;
;;; The scripted code runtime is the agent worker's own
;;; AXLLM/TESTS-AGENT:MAKE-SCRIPTED-RUNTIME, resolved the same way: writing
;;; a second scripted runtime here would be a second thing to keep correct.

(defparameter +agent-eval-method-name+ "AGENT-EVALUATE-OPTIMIZATION-TASK"
  "The agent's eval surface, agreed with the agent owner:

  (agent-evaluate-optimization-task agent client task &key options)

It marks the state, runs the agent once, turns however the run ended into a
completion record, and returns Core's prediction.  One name, resolved at run
time rather than at compile time, because the agent owns the method and this
file is loaded before it in the test order.")

(defun agent-eval-function ()
  "The agent's eval method, or NIL while it does not exist yet."
  (let ((symbol (find-symbol +agent-eval-method-name+ :axllm)))
    (and symbol (fboundp symbol) (fdefinition symbol))))

(defun scripted-runtime-for (fixture)
  "The agent worker's scripted runtime, carrying this fixture's script."
  (let* ((package (find-package :axllm/tests-agent))
         (maker (and package (find-symbol "MAKE-SCRIPTED-RUNTIME" package))))
    (unless (and maker (fboundp maker))
      (blocked "the agent worker's AXLLM/TESTS-AGENT:MAKE-SCRIPTED-RUNTIME is not available"))
    (funcall maker
             :script (elements (jget fixture "runtime_script"))
             :language (let ((given (present (jget fixture "runtime_language"))))
                         (or given "JavaScript")))))

(defun build-eval-agent (fixture client)
  "The agent an eval fixture describes, with its scripted runtime installed."
  (let ((options (let ((given (jget fixture "options")))
                   (if (hash-table-p given) (axllm::%opt-clone given) (ax:object)))))
    (when (present (jget fixture "description"))
      (axllm::%set-key options "description" (jget fixture "description")))
    (when (present (jget fixture "runtime_script"))
      (axllm::%set-key options "runtime" (scripted-runtime-for fixture)))
    ;; A playbook without its own student learns through the fixture's
    ;; client, as the agent fixtures do.
    (let ((playbook (jget options "playbook")))
      (when (and (hash-table-p playbook) (eq (jget playbook "studentAI") :null))
        (axllm::%set-key playbook "studentAI" client)))
    (ax:agent (let ((given (present (jget fixture "signature"))))
                (or given "question:string -> answer:string"))
              :options options)))

(defun run-eval (fixture)
  (let ((evaluate (agent-eval-function)))
    (unless evaluate
      (blocked (format nil "the agent has no ~a yet" +agent-eval-method-name+)))
    (multiple-value-bind (client script) (scripted-client (jget fixture "responses"))
      (let* ((program (build-eval-agent fixture client))
             (task (let ((given (jget fixture "task")))
                     (if (hash-table-p given)
                         given
                         (ax:object "input" (let ((input (jget fixture "input")))
                                              (if (hash-table-p input) input (ax:object)))))))
             (options (let ((given (jget fixture "eval_options")))
                        (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
             (prediction (funcall evaluate program client task :options options)))
        (when (axllm::%opt-key-present-p fixture "expected_prediction_subset")
          (assert-subset prediction (jget fixture "expected_prediction_subset") "eval prediction"))
        ;; Fields the fixture pins exactly: a list must match in full, so a
        ;; prediction that reports no calls cannot pass a fixture that names
        ;; the calls the run really made.
        (let ((exact (jget fixture "expected_prediction_fields")))
          (when (hash-table-p exact)
            (dolist (key (axllm::%object-keys exact))
              (assert-equal (jget prediction key) (gethash key exact)
                            (format nil "eval prediction ~a" key)))))
        (when (axllm::%opt-key-present-p fixture "expected_request_count")
          (assert-equal (length (script-requests script))
                        (jget fixture "expected_request_count")
                        "eval provider request count"))))))


;;; ------------------------------------------------------------------
;;; Playbook evolution
;;; ------------------------------------------------------------------
;;;
;;; The playbook surface driven end to end: a real student program answers
;;; each example, real Reflector and Curator programs run against a scripted
;;; teacher, and Core's playbook ops apply whatever the Curator asked for.

(defun run-playbook-evolve (fixture)
  (multiple-value-bind (student) (scripted-client (jget fixture "responses"))
    (multiple-value-bind (teacher teacher-script) (scripted-client (jget fixture "teacher_responses"))
      (let* ((scores (elements (jget fixture "metric_scores")))
             (metric (lambda (prediction example)
                       (declare (ignore prediction example))
                       (if scores (pop scores) 0)))
             (options (let ((given (jget fixture "playbook_options")))
                        (if (hash-table-p given) (axllm::%opt-clone given) (ax:object))))
             (signature (ax:parse-signature
                         (let ((given (present (jget fixture "signature"))))
                           (or given "question:string -> answer:string"))))
             (program (ax:ax signature)))
        (axllm::%set-key options "now"
                         (let ((now (present (jget fixture "now"))))
                           (or now "1970-01-01T00:00:00.000Z")))
        (let* ((playbook (ax:make-playbook :program program
                                           :signature signature
                                           :student student
                                           :teacher teacher
                                           :metric metric
                                           :options options))
               (result (ax:playbook-evolve playbook (jget fixture "examples" (axllm::%new-array))
                                           :metric metric)))
          (declare (ignorable result))
          (when (axllm::%opt-key-present-p fixture "expected_playbook")
            (assert-equal (ax:ace-playbook playbook) (jget fixture "expected_playbook")
                          "playbook evolve playbook"))
          ;; Every fragment the fixture names must appear in some teacher
          ;; request: that is how the Playbook field's {markdown, structured}
          ;; shape and the task/ground-truth split are pinned.
          (when (axllm::%opt-key-present-p fixture "expected_teacher_request_contains")
            (let ((text (teacher-request-text teacher-script)))
              (dolist (fragment (elements (jget fixture "expected_teacher_request_contains")))
                (unless (search fragment text)
                  (fail "the teacher requests are missing ~a" (show fragment))))))
          ;; The rendered system prompts, in call order, byte for byte.
          (when (axllm::%opt-key-present-p fixture "expected_teacher_system_prompts")
            (let ((prompts (teacher-system-prompts teacher-script)))
              (assert-equal prompts (jget fixture "expected_teacher_system_prompts")
                            "teacher system prompts"))))))))

(defun teacher-request-text (script)
  "Every message body the teacher was sent, decoded and joined.

The fragments a fixture pins are JSON the message carries, so they have to
be matched against the decoded content rather than the request body, where
every quote is escaped."
  (with-output-to-string (out)
    (dolist (body (reverse (script-requests script)))
      (let ((parsed (handler-case (ax:parse-json body) (error () :null))))
        (dolist (message (elements (jget parsed "messages")))
          (let ((content (present (jget message "content"))))
            (when (stringp content)
              (write-string content out)
              (terpri out))))))))

(defun teacher-system-prompts (script)
  "Each system prompt the teacher was sent, in call order."
  (let ((out (axllm::%new-array)))
    (dolist (body (reverse (script-requests script)) out)
      (let ((parsed (handler-case (ax:parse-json body) (error () :null))))
        (dolist (message (elements (jget parsed "messages")))
          (when (equal (present (jget message "role")) "system")
            (vector-push-extend (jget message "content") out)))))))


;;; ------------------------------------------------------------------
;;; Whole-package instrument summary
;;; ------------------------------------------------------------------
;;;
;;; The verification fixture asks one question of the whole package: does
;;; every Core instrument a generated target is built on actually run?  It
;;; reaches well outside the optimizer, into prompts, providers, audio,
;;; policy, flow, MCP and generation, and it is claimed here because the
;;; optimizer suite is where the emitter reports coverage.  Every value is
;;; computed by calling the real Core function; nothing is transcribed from
;;; the fixture.

(defun %verify-first (value &optional (default :null))
  (let ((items (elements value)))
    (if items (first items) default)))

(defun verification-summary ()
  "The instrument summary, every field produced by running Core."
  (let* ((core (find-package :axllm/core))
         (call (lambda (name &rest arguments)
                 (let ((symbol (find-symbol name core)))
                   (unless (and symbol (fboundp symbol))
                     (blocked (format nil "axllm/core::~(~a~) is not defined" name)))
                   (apply (fdefinition symbol) arguments)))))
    (let* ((prompt-vars (funcall call "COLLECT-TEMPLATE-VARIABLE-NAMES"
                                 "Hello {{name}} and {{count}}" "verification"))
           (chat-payload (funcall call "BUILD-CHAT-REQUEST" :null
                                  (ax:object "model" "gpt-fixture"
                                             "chat_prompt" (axllm::%opt-array
                                                            (list (ax:object "role" "user"
                                                                             "content" "hello")))
                                             "model_config" (ax:object))
                                  (ax:object)))
           (chat-response (funcall call "NORMALIZE-CHAT-RESPONSE"
                                   (ax:object "id" "chat-1" "model" "gpt-fixture"
                                              "choices" (axllm::%opt-array
                                                         (list (ax:object
                                                                "index" 0
                                                                "message" (ax:object "content" "hello")
                                                                "finish_reason" "stop")))
                                              "usage" (ax:object "prompt_tokens" 1
                                                                 "completion_tokens" 2
                                                                 "total_tokens" 3))))
           (embed-payload (funcall call "BUILD-EMBED-REQUEST" :null
                                   (ax:object "embedModel" "embed-fixture"
                                              "texts" (axllm::%opt-array (list "hello")))
                                   (ax:object)))
           (embed-response (funcall call "NORMALIZE-EMBED-RESPONSE"
                                    (ax:object "id" "embed-1" "model" "embed-fixture"
                                               "data" (axllm::%opt-array
                                                       (list (ax:object "embedding"
                                                                        (axllm::%opt-array
                                                                         (list 0.1d0 0.2d0)))))
                                               "usage" (ax:object "prompt_tokens" 1
                                                                  "total_tokens" 1))))
           (stream-response (funcall call "NORMALIZE-STREAM-DELTA"
                                     (ax:object "id" "stream-1" "model" "gpt-fixture"
                                                "choices" (axllm::%opt-array
                                                           (list (ax:object
                                                                  "index" 0
                                                                  "delta" (ax:object "content" "delta")))))
                                     (ax:object)))
           (tool-call (funcall call "OPENAI-TOOL-CALL-TO-PROVIDER-IMPL"
                               (ax:object "id" "call-1"
                                          "function" (ax:object "name" "lookup"
                                                                "params" (ax:object "term" "ax")))))
           (profile (funcall call "PROVIDER-RESOLVE-PROFILE" "openai"))
           (gemini-transcript (funcall call "GEMINI-NORMALIZE-TRANSCRIBE-RESPONSE"
                                       (ax:object "candidates"
                                                  (axllm::%opt-array
                                                   (list (ax:object "content"
                                                                    (ax:object "parts"
                                                                               (axllm::%opt-array
                                                                                (list (ax:object "text" "transcript"))))))))))
           (gemini-speech (funcall call "GEMINI-NORMALIZE-SPEAK-RESPONSE"
                                   (ax:object "candidates"
                                              (axllm::%opt-array
                                               (list (ax:object "content"
                                                                (ax:object "parts"
                                                                           (axllm::%opt-array
                                                                            (list (ax:object "inlineData"
                                                                                             (ax:object "data" "audio-bytes")))))))))
                                   (ax:object "format" "wav")))
           (grok-transcribe (funcall call "GROK-BUILD-TRANSCRIBE-REQUEST"
                                     (ax:object "audio" "audio-bytes" "language" "en"
                                                "prompt" "names")))
           (grok-speak (funcall call "GROK-BUILD-SPEAK-REQUEST"
                                (ax:object "text" "speak"
                                           "voice" (ax:object "id" "eve")
                                           "format" "pcm16" "sampleRate" 16000)))
           (registry (ax:object "flags" (ax:object "skillsMode" ax:true)
                                "protocol_actions" (axllm::%opt-array
                                                    (list (ax:object "id" "respond")))
                                "runtime_globals" (axllm::%opt-array
                                                   (list (ax:object "id" "runtime")))
                                "actor_primitives" (axllm::%opt-array
                                                    (list (ax:object "id" "speak"
                                                                     "effect" "fixture guidance"
                                                                     "stages" (axllm::%opt-array
                                                                               (list "actor"))
                                                                     "availability_condition" "always")))))
           (guidance (progn (funcall call "VALIDATE-POLICY-RESERVED-NAMES" registry "fixtureCallable")
                            (funcall call "RENDER-ACTOR-PRIMITIVE-GUIDANCE" registry "actor")))
           (policy-state (ax:object))
           (policy-result (progn (funcall call "RECORD-POLICY-EVENT" policy-state "respond"
                                          (ax:object "ok" ax:true))
                                 (funcall call "NORMALIZE-POLICY-ACTION-RESULT" "respond"
                                          (ax:object "ok" ax:true))))
           (descriptor (funcall call "PROGRAM-DESCRIPTOR" "fixture" "core"
                                (ax:object "source" "verification")))
           (merged (funcall call "FLOW-MERGE-PARALLEL-RESULTS"
                            (ax:object "base" "keep") (ax:object "answer" "ok")))
           (gen-marker (ax:object))
           (gen-examples (progn (funcall call "SET-EXAMPLES" gen-marker
                                         (axllm::%opt-array
                                          (list (ax:object "input" (ax:object "question" "q")
                                                           "output" (ax:object "answer" "a")))))
                                (funcall call "SET-DEMOS" gen-marker
                                         (axllm::%opt-array
                                          (list (ax:object "traces" (axllm::%new-array)))))
                                gen-marker))
           (constants (funcall call "MCP-PROTOCOL-CONSTANTS"))
           (request (funcall call "MCP-JSONRPC-REQUEST" "1" "ping" (ax:object "ok" ax:true)))
           (notification (funcall call "MCP-JSONRPC-NOTIFICATION" "progress" (ax:object "pct" 1)))
           (mcp-error (funcall call "MCP-NORMALIZE-ERROR"
                               (ax:object "jsonrpc" "2.0" "id" "1"
                                          "error" (ax:object "code" -32000 "message" "nope")))))
      (funcall call "GEMINI-BUILD-TRANSCRIBE-REQUEST"
               (ax:object "audio" (ax:object "data" "audio-bytes" "mimeType" "audio/wav")))
      (funcall call "GEMINI-BUILD-SPEAK-REQUEST"
               (ax:object "text" "speak" "voice" "Kore" "format" "wav"))
      (ax:object
       "promptVars" (axllm::%opt-array (sort (mapcar (lambda (name) (format nil "~a" name))
                                                     (elements prompt-vars))
                                             #'string<))
       "chatModel" (jget chat-payload "model")
       "chatContent" (jget (%verify-first (jget chat-response "results")) "content")
       "embedModel" (jget embed-payload "model")
       "embedCount" (length (jget embed-response "embeddings" (axllm::%new-array)))
       "streamContent" (jget (%verify-first (jget stream-response "results")) "content")
       "toolName" (jget (jget tool-call "function") "name")
       "profileId" (jget profile "id")
       "geminiText" (jget gemini-transcript "text")
       ;; The normalized speak response carries the audio at its top level,
       ;; not under an "audio" wrapper.
       "geminiAudio" (jget gemini-speech "data")
       ;; The codec is the speak request's; the transcribe request is what
       ;; carries the format flag.
       "grokCodec" (jget (jget grok-speak "output_format") "codec")
       "grokFormat" (jget grok-transcribe "format")
       "policyActions" (length (jget registry "protocol_actions"))
       "runtimeGlobals" (length (jget registry "runtime_globals"))
       "qualityScore" (funcall call "MAP-OPTIMIZATION-JUDGE-QUALITY-TO-SCORE" "good")
       "policyTrace" (length (jget policy-state "policy_trace" (axllm::%new-array)))
       "policyEffectOnly" (jget policy-result "effect_only" ax:false)
       "guidance" guidance
       "programKind" (jget descriptor "kind")
       "flowAnswer" (jget merged "answer")
       "mcpVersion" (jget constants "protocolVersion")
       "mcpRequest" (jget request "method")
       "mcpNotification" (jget notification "method")
       "mcpError" (jget mcp-error "code")
       "genExamples" (length (jget gen-examples "examples" (axllm::%new-array)))
       "genDemos" (length (jget gen-examples "demos" (axllm::%new-array)))))))

(defun run-verification (fixture)
  (assert-subset (verification-summary) (jget fixture "expected_output")
                 "verification instruments"))

(defun run-operation (fixture operation)
  "Run one claimed operation.  There is no default arm on purpose."
  (cond ((string= operation "gepa") (run-gepa fixture))
        ((string= operation "components") (run-components fixture))
        ((string= operation "filter") (run-filter fixture))
        ((string= operation "artifact") (run-artifact fixture))
        ((string= operation "apply") (run-apply fixture))
        ((string= operation "evaluate") (run-evaluate fixture))
        ((string= operation "engine") (run-engine fixture))
        ((string= operation "helper") (run-helper fixture))
        ((string= operation "judge_payload") (run-judge-payload fixture))
        ((string= operation "eval") (run-eval fixture))
        ((string= operation "playbook-evolve") (run-playbook-evolve fixture))
        ((string= operation "verification") (run-verification fixture))
        ((and (> (length operation) 9) (string= "playbook-" operation :end2 9))
         (run-playbook fixture operation))
        ((string= operation "bootstrap") (run-bootstrap fixture))
        ((string= operation "ace-compile") (run-ace fixture "ace-compile"))
        ((string= operation "ace-online-update") (run-ace fixture "ace-online-update"))
        ((string= operation "dataset") (run-dataset fixture))
        ((string= operation "score") (run-score fixture))
        ((string= operation "evidence") (run-evidence fixture))
        (t (fail "operation ~s is declared semantic but has no runner" operation))))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun fixture-operation (fixture)
  (let ((operation (present (jget fixture "operation"))))
    (or operation "components")))

(defun run-fixture (fixture)
  "Run FIXTURE, honouring expected_error_contains the way every port does."
  (let ((expected (present (jget fixture "expected_error_contains")))
        (operation (fixture-operation fixture)))
    (handler-case (progn (run-operation fixture operation)
                         (when expected
                           (fail "expected an error containing ~s, the run succeeded" expected)))
      (fixture-blocked (condition) (error condition))
      (fixture-failure (condition) (error condition))
      (error (condition)
        (let ((text (princ-to-string condition)))
          (unless (and expected (search expected text))
            (fail "~a" (if expected
                           (format nil "expected an error containing ~s, got: ~a" expected text)
                           text))))))))

(defun coverage-report ()
  "Each fixture with how this file covers it, for the run's summary."
  (let ((rows '()))
    (dolist (path (fixture-files) (nreverse rows))
      (let* ((fixture (read-fixture path))
             (operation (fixture-operation fixture))
             (reason (cdr (assoc operation +not-claimed-operations+ :test #'string=))))
        (push (list (pathname-name path)
                    operation
                    (cond ((member operation +semantic-operations+ :test #'string=) :semantic)
                          (reason :explicitly-not-claimed)
                          (t :unclassified))
                    reason)
              rows)))))

(defun run-optimize-conformance-tests (&key (stream *standard-output*))
  "Run every claimed axoptimize fixture.

Returns (values PASSED FAILED SKIPPED BLOCKED).  BLOCKED counts claimed
fixtures whose upstream surface is not available yet; the runner for each
exists and the reason is printed."
  (let ((passed 0) (failed 0) (skipped 0) (blocked 0) (blockers '()) (unclassified '())
        (inventory '()))
    ;; Report an empty inventory as a failure rather than letting it abort the
    ;; run: the count is what the build gate reads, and a suite that vanished
    ;; has to be visible there and not only in a backtrace.
    (handler-case (setf inventory (coverage-report))
      (empty-fixture-inventory (condition)
        (format stream "~&FAIL axoptimize inventory: ~a~%" condition)
        (format stream "~&axoptimize: 0 passed, 1 failed, 0 blocked upstream, ~
0 explicitly not claimed.~%")
        (return-from run-optimize-conformance-tests (values 0 1 0 0))))
    (dolist (row inventory)
      (destructuring-bind (name operation classification reason) row
        (declare (ignore reason))
        (case classification
          (:semantic
           (handler-case
               (let ((path (merge-pathnames (format nil "axoptimize/~a.json" name)
                                            (conformance-directory))))
                 (let ((fixture (read-fixture path)))
                   (run-fixture fixture)
                   (incf passed)
                   ;; Only a fixture that ran to completion is recorded, and
                   ;; under the name on disk rather than its title, so the
                   ;; receipt can be reconciled against the inventory.  A
                   ;; fixture that pins a rejection says so itself through
                   ;; expected_error_contains; classifying from the fixture
                   ;; rather than from a list here means the two cannot drift.
                   (axllm/conformance:record-result
                    "axoptimize" path
                    (if (ax:jget fixture "expected_error_contains" nil)
                        :validation-error
                        :semantic))))
             (fixture-blocked (condition)
               (incf blocked)
               (pushnew (princ-to-string condition) blockers :test #'equal)
               (format stream "~&BLOCKED ~a (~a): ~a~%" name operation condition))
             (error (condition)
               (incf failed)
               (format stream "~&FAIL ~a (~a): ~a~%" name operation condition))))
          (:explicitly-not-claimed (incf skipped))
          (t (push (list name operation) unclassified)))))
    (when unclassified
      (incf failed (length unclassified))
      (dolist (row unclassified)
        (format stream "~&FAIL ~a: operation ~s is in neither the semantic nor the ~
not-claimed list; classify it before it can pass.~%" (first row) (second row))))
    (format stream "~&axoptimize: ~a passed, ~a failed, ~a blocked upstream, ~a explicitly not claimed.~%"
            passed failed blocked skipped)
    (dolist (reason (reverse blockers))
      (format stream "~&  blocked on: ~a~%" reason))
    (values passed failed blocked skipped)))
