;;;; agent-conformance.lisp --- the shared AxAgent fixtures, run natively.
;;;;
;;;; Entry point for the repository runner:
;;;;
;;;;   (axllm/tests-agent:run-agent-conformance)
;;;;     ; => (values passed failed blocked)
;;;;
;;;; The fixtures in ir/conformance/axagent and ir/conformance/axagent-real are
;;;; the same files every Ax port runs. This file reads them from disk, so a
;;;; fixture changed in Core is a changed expectation here too, and runs each
;;;; one against this package's own implementation.
;;;;
;;;; Dispatch is explicit for every fixture kind. There is no catch-all arm:
;;;; a kind this port does not run yet is reported as BLOCKED with the reason,
;;;; and blocked fixtures are never counted as passes. A kind nobody listed at
;;;; all is a failure, so a new fixture kind cannot slip through unnoticed.
;;;;
;;;; Set AXIR_CONFORMANCE_DIR to run against a different ir/conformance tree.

(in-package #:axllm/tests-agent)

;;; ------------------------------------------------------------------
;;; Fixture files
;;; ------------------------------------------------------------------

(defun %conformance-directory ()
  (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
    (if (and override (plusp (length override)))
        (uiop:ensure-directory-pathname override)
        (asdf:system-relative-pathname "axllm" "../../ir/conformance/"))))

(defparameter +conformance-suites+ '("axagent/" "axagent-real/")
  "The fixture suites a conformance run must cover.

Both are required. A suite that is missing or empty is a broken checkout
or a mistyped AXIR_CONFORMANCE_DIR, not a suite with nothing to say, and
skipping it would let a run over a single file report the same clean
result as a run over the whole inventory.")

(defun %fixture-files ()
  "Every axagent and axagent-real fixture, in a stable order.

A missing or empty suite stops the run rather than shrinking it: the
counts this returns are the evidence the release gate reads, and a gate
cannot tell a passing run from an almost-empty one by the numbers alone."
  (let ((root (%conformance-directory))
        (files '()))
    (dolist (suite +conformance-suites+)
      (let* ((directory (merge-pathnames suite root))
             (found (and (probe-file directory)
                         (sort (directory (merge-pathnames "*.json" directory))
                               #'string< :key #'namestring))))
        (unless found
          (error 'test-failure
                 :text (format nil "conformance suite ~a is missing or empty under ~a; ~
a run that skipped it would report a clean result over a fraction of the inventory"
                               suite (namestring root))))
        (setf files (append files found))))
    files))

(defun %fixture-suite (path)
  "Which suite PATH belongs to, from the directory it was read out of.

Taken from the path rather than the fixture's own fields, because the
report is keyed on what is actually on disk: a fixture that named a suite
it does not live in would be reported under the wrong one."
  (car (last (pathname-directory path))))

(defun %report-pass (path)
  "Tell the full gate that PATH ran and checked out completely.

A no-op unless the gate is running, and keyed on the real filename rather
than the fixture's title, because the gate reconciles what ran against the
inventory on disk."
  (uiop:symbol-call :axllm/conformance :record-result
                    (%fixture-suite path) path :semantic))

(defun %read-fixture (path)
  (let ((fixture (ax:parse-json (uiop:read-file-string path))))
    ;; A top-level `<key>_json' string holds `<key>' in TypeScript's key
    ;; order, because the canonical fixture sort rewrites the order of a
    ;; fixture's own objects. Expand it exactly as the other ports do.
    (dolist (key (ax::%object-keys fixture))
      (let ((value (gethash key fixture)))
        (when (and (stringp value)
                   (> (length key) 5)
                   (string= "_json" key :start2 (- (length key) 5)))
          (let ((target (subseq key 0 (- (length key) 5))))
            (unless (nth-value 1 (gethash target fixture))
              (ax::%set-key fixture target (ax:parse-json value)))))))
    fixture))

;;; ------------------------------------------------------------------
;;; Fixture kinds
;;; ------------------------------------------------------------------
;;;
;;; Every kind the two suites contain is listed. A kind maps either to a
;;; runner, or to the reason this port cannot run it yet. The reasons are
;;; specific: each one names the surface that has to land first, so this table
;;; is a work list rather than an excuse.

(defparameter +fixture-runners+
  '(("agent_runtime_protocol" . run-runtime-protocol-fixture)
    ("agent_runtime_adapter" . run-runtime-adapter-fixture)
    ("agent_prompt" . run-agent-prompt-fixture)
    ("agent_runtime_policy" . run-agent-runtime-policy-fixture)
    ("agent_runtime_session" . run-agent-runtime-session-fixture)
    ("agent_playbook_coverage" . run-agent-playbook-coverage-fixture)
    ("agent_forward" . run-agent-forward-fixture)
    ("agent_streaming_forward" . run-agent-forward-fixture)
    ;; Same pipeline, a real engine behind it: the fixture names the engine
    ;; and the runner builds a worker process instead of a script.
    ("agent_runtime_real" . run-agent-forward-fixture)
    ("agent_playbook_evolve" . run-agent-playbook-evolve-fixture))
  "Fixture kinds this port runs natively.")

(defparameter +fixture-blocked+ nil
  "Fixture kinds this port does not run yet, and what each one waits for.")

;;; ------------------------------------------------------------------
;;; agent_runtime_protocol
;;; ------------------------------------------------------------------

(defun %protocol-fixture-runtime (fixture)
  (let ((mode (let ((value (ax:jget fixture "mode" "normal")))
                (if (eq value :null) "normal" value))))
    (make-protocol-runtime :mode mode :timeout 20)))

(defun %fixture-object (fixture key)
  (let ((value (ax:jget fixture key)))
    (if (ax::%object-p value) value (ax:object))))

(defun %fixture-expect (fixture key actual label)
  (let ((expected (ax:jget fixture key)))
    (unless (eq expected :null)
      (assert-json-subset actual expected label))))

(defun run-runtime-protocol-fixture (fixture)
  "Drive the runtime protocol client through one fixture's operation.

The fixture's `expected_error_contains' is an expectation, not an escape:
an operation that must fail has to fail with that text, and one that
succeeds when the fixture says it should not is a failure."
  (let* ((operation (let ((value (ax:jget fixture "operation" "roundtrip")))
                      (if (eq value :null) "roundtrip" value)))
         (expected-error (ax:jget fixture "expected_error_contains"))
         (runtime (%protocol-fixture-runtime fixture))
         (session nil)
         (signalled nil))
    (unwind-protect
         (handler-case
             (cond
               ((string= operation "roundtrip")
                (%fixture-expect fixture "expected_capabilities_subset"
                                 (ax::%protocol-result
                                  (ax::%protocol-request runtime "capabilities" nil nil))
                                 "protocol capabilities")
                (setf session (ax::runtime-create-session
                               runtime
                               (%fixture-object fixture "create_globals")
                               (%fixture-object fixture "create_options")))
                (%fixture-expect fixture "expected_execute_subset"
                                 (ax::session-execute session
                                                      (ax:jget fixture "execute_code" "final()")
                                                      (%fixture-object fixture "execute_options"))
                                 "protocol execute")
                (%fixture-expect fixture "expected_inspect_subset"
                                 (ax::session-inspect-globals session (ax:object))
                                 "protocol inspect")
                (%fixture-expect fixture "expected_snapshot_subset"
                                 (ax::session-snapshot-globals session (ax:object))
                                 "protocol snapshot")
                (%fixture-expect fixture "expected_patch_subset"
                                 (ax::session-patch-globals session
                                                            (%fixture-object fixture "patch_globals")
                                                            (ax:object))
                                 "protocol patch")
                (%fixture-expect fixture "expected_close_subset"
                                 (ax::session-close session)
                                 "protocol close"))

               ((string= operation "execute_error")
                (setf session (ax::runtime-create-session
                               runtime
                               (%fixture-object fixture "create_globals")
                               (%fixture-object fixture "create_options")))
                (%fixture-expect fixture "expected_execute_subset"
                                 (ax::session-execute session
                                                      (ax:jget fixture "execute_code" "timeout()")
                                                      (%fixture-object fixture "execute_options"))
                                 "protocol execute error"))

               ((string= operation "unknown_op")
                (ax::%protocol-request runtime "unknown_op" nil nil)
                (error 'test-failure :text "expected an unknown protocol op to fail"))

               ((string= operation "capabilities_error")
                (ax::%protocol-request runtime "capabilities" nil nil)
                (error 'test-failure :text "expected the capabilities request to fail"))

               ((string= operation "unavailable")
                (setf session (ax::runtime-create-session
                               runtime
                               (%fixture-object fixture "create_globals")
                               (%fixture-object fixture "create_options")))
                (let ((method (let ((value (ax:jget fixture "method" "inspect_globals")))
                                (if (eq value :null) "inspect_globals" value))))
                  (cond ((string= method "inspect_globals")
                         (ax::session-inspect-globals session (ax:object)))
                        ((string= method "snapshot_globals")
                         (ax::session-snapshot-globals session (ax:object)))
                        ((string= method "patch_globals")
                         (ax::session-patch-globals session (ax:object) (ax:object)))
                        (t (error 'test-failure
                                  :text (format nil "unknown unavailable method ~s" method)))))
                (error 'test-failure :text "expected the unavailable method to fail"))

               ((string= operation "session_mismatch")
                (setf session (ax::runtime-create-session
                               runtime
                               (%fixture-object fixture "create_globals")
                               (%fixture-object fixture "create_options")))
                (ax::%protocol-request runtime "execute" "s1"
                                       (ax:object "code" (ax:jget fixture "execute_code" "final()")
                                                  "options" (ax:object)))
                (error 'test-failure :text "expected the session mismatch to fail"))

               (t (error 'test-failure
                         :text (format nil "unknown runtime protocol operation ~s" operation))))
           (test-failure (condition) (error condition))
           (error (condition)
             (setf signalled (princ-to-string condition))
             (if (and (stringp expected-error) (search expected-error signalled))
                 nil
                 (error condition))))
      (ignore-errors (when session (ax::session-close session)))
      (ignore-errors (ax::runtime-shutdown runtime)))
    (when (and (stringp expected-error) (null signalled))
      (error 'test-failure
             :text (format nil "expected a failure containing ~s, the operation succeeded"
                           expected-error)))
    t))

;;; ------------------------------------------------------------------
;;; agent_runtime_adapter
;;; ------------------------------------------------------------------

(defun %adapter-arg (spec index &optional (fallback :null))
  (let ((args (ax:jget spec "args")))
    (if (and (ax::%array-p args) (< index (length args))) (aref args index) fallback)))

(defun %adapter-kwarg (spec name)
  (let ((kwargs (ax:jget spec "kwargs")))
    (if (ax::%object-p kwargs) (ax:jget kwargs name) :null)))

(defun %adapter-helper-call (spec)
  "One runtime adapter helper call, as the fixture names it."
  (let* ((name (core::core-js-text (ax:jget spec "name" "")))
         (args (let ((value (ax:jget spec "args")))
                 (if (ax::%array-p value) (coerce value 'list) '()))))
    (cond
      ((string= name "result") (ax::envelope-result (%adapter-arg spec 0)))
      ((string= name "error")
       (ax::envelope-error (%adapter-arg spec 0 "")
                           (let ((category (%adapter-arg spec 1)))
                             (if (eq category :null)
                                 (let ((keyword (%adapter-kwarg spec "category")))
                                   (if (eq keyword :null) "runtime" keyword))
                                 category))))
      ((string= name "session_closed")
       (ax::envelope-session-closed (%adapter-arg spec 0 "session closed")))
      ((string= name "timeout")
       (ax::envelope-timeout (%adapter-arg spec 0 "execution timed out")))
      ((string= name "final") (apply #'ax::envelope-final args))
      ((string= name "ask_clarification") (apply #'ax::envelope-ask-clarification args))
      ((string= name "discover") (ax::envelope-discover (%adapter-arg spec 0 (ax:object))))
      ((string= name "recall") (ax::envelope-recall (%adapter-arg spec 0 (ax::%new-array))))
      ((string= name "used")
       (ax::envelope-used (%adapter-arg spec 0 (ax:object))
                          :reason (let ((reason (%adapter-kwarg spec "reason")))
                                    (unless (eq reason :null) reason))
                          :stage (let ((stage (%adapter-kwarg spec "stage")))
                                   (unless (eq stage :null) stage))))
      ((string= name "status")
       (ax::envelope-status (%adapter-arg spec 0 "success")
                            (let ((message (%adapter-arg spec 1)))
                              (if (eq message :null) "" message))))
      ((string= name "guide_agent")
       (ax::envelope-guide-agent (%adapter-arg spec 0 "")
                                 (let ((trigger (%adapter-arg spec 1)))
                                   (unless (eq trigger :null) trigger))))
      (t (error 'test-failure :text (format nil "unknown runtime adapter helper ~s" name))))))

(defun run-runtime-adapter-fixture (fixture)
  "Check the runtime adapter's capabilities and envelope helpers.

A fixture that also asks for `run_session' needs Core's emitted step
normalization, so that half is reported as blocked rather than skipped
silently: the helper calls still run and still have to match."
  (let ((capabilities (ax:jget fixture "capabilities")))
    (when (ax::%object-p capabilities)
      (let ((actual (ax::runtime-capabilities
                     :inspect (let ((value (ax:jget capabilities "inspect")))
                                (or (eq value :null) (ax:json-true-p value)))
                     :snapshot (let ((value (ax:jget capabilities "snapshot")))
                                 (or (eq value :null) (ax:json-true-p value)))
                     :patch (let ((value (ax:jget capabilities "patch")))
                              (or (eq value :null) (ax:json-true-p value)))
                     :abort (ax:json-true-p (ax:jget capabilities "abort"))
                     :language (let ((value (ax:jget capabilities "language")))
                                 (if (eq value :null) "JavaScript" value))
                     :usage-instructions (let ((value (ax:jget capabilities "usage_instructions")))
                                           (if (eq value :null) "" value)))))
        (%fixture-expect fixture "expected_capabilities" actual "runtime capabilities"))))
  (let ((normalizing 0))
    (loop for spec across (let ((calls (ax:jget fixture "helper_calls")))
                            (if (ax::%array-p calls) calls (ax::%new-array)))
          do (let ((actual (%adapter-helper-call spec))
                   (label (format nil "runtime helper ~a" (ax:jget spec "name"))))
               (let ((expected (ax:jget spec "expected")))
                 (unless (eq expected :null)
                   (assert-json-equal actual expected label)))
               (let ((expected (ax:jget spec "expected_subset")))
                 (unless (eq expected :null)
                   (assert-json-subset actual expected label)))
               ;; An envelope only becomes a step result through Core, so the
               ;; normalized expectation is checked against Core's own
               ;; normalizer rather than restated here.
               (when (ax:json-true-p (ax:jget spec "normalize"))
                 (incf normalizing)
                 (let ((expected (ax:jget spec "expected_normalized_subset")))
                   (unless (eq expected :null)
                     (assert-json-subset
                      (core::normalize-agent-runtime-step-result
                       actual
                       (let ((code (ax:jget spec "code" "<adapter>")))
                         (if (eq code :null) "<adapter>" code)))
                      expected
                      (format nil "runtime helper normalized ~a" (ax:jget spec "name"))))))))
    ;; A fixture that also asks for run_session drives a whole agent session
    ;; from one scripted envelope, which is the session runner's job.
    (let ((run-session (ax:jget fixture "run_session")))
      (unless (eq run-session :null)
        (let ((session-fixture
                (ax:object "kind" "agent_runtime_session"
                           "name" (ax:jget fixture "name" "runtime-adapter")
                           "signature" (ax:jget fixture "signature"
                                                 "question:string -> answer:string")
                           "operation" "test"
                           "code" "adapter()"
                           "context_values" (let ((values (ax:jget fixture "context_values")))
                                              (if (ax::%object-p values)
                                                  values
                                                  (ax:object "question" "adapter")))
                           "runtime_script"
                           (vector (ax:object "expected_code" "adapter()"
                                              "result" (%adapter-helper-call run-session))))))
          (dolist (key '("expected_result_subset" "expected_action_log_subset"
                         "expected_trace_event_kinds" "expected_closed_session_count"))
            (let ((value (ax:jget fixture key)))
              (unless (eq value :null)
                (ax::%set-key session-fixture key value))))
          (run-agent-runtime-session-fixture session-fixture)))))
  t)

(define-condition partial-fixture (condition)
  ((reason :initarg :reason :reader partial-fixture-reason))
  (:documentation
   "Signalled by a fixture runner that checked part of a fixture and could
not check the rest. The checked part still has to pass; the rest is
reported so a partial run is never counted as a complete one."))


;;; ------------------------------------------------------------------
;;; agent_prompt
;;; ------------------------------------------------------------------

(defun run-agent-prompt-fixture (fixture)
  "Build a real agent and check that Core rendered the actor prompts.

This is the gate that catches a hollow agent: an agent whose stage
descriptions were never rendered has empty or absent description keys, so
an empty description is a failure here rather than a vacuous pass."
  (let* ((agent (ax::agent (ax:jget fixture "signature" "question:string -> answer:string")
                           :options (%fixture-object fixture "options")))
         (expects (%fixture-object fixture "expected_description_contains")))
    (dolist (field (ax::%object-keys expects))
      (unless (string= field "__order")
        (let ((description (ax::%state-get agent field "")))
          (unless (and (stringp description)
                       (plusp (length (string-trim '(#\Space #\Tab #\Newline) description))))
            (error 'test-failure
                   :text (format nil "agent stage description ~a is empty; the actor prompt was not rendered into agent state"
                                 field)))
          (loop for needle across (let ((needles (gethash field expects)))
                                    (if (ax::%array-p needles) needles (ax::%new-array)))
                do (expect-contains description (core::core-js-text needle)
                                    (format nil "agent stage description ~a carries its expected text"
                                            field))))))
    t))

;;; ------------------------------------------------------------------
;;; agent_runtime_policy
;;; ------------------------------------------------------------------

(defun %policy-step (fixture agent key thunk)
  "Run one optional fixture step, when the fixture asks for it."
  (unless (eq (ax:jget fixture key) :null)
    (funcall thunk agent (ax:jget fixture key))))

(defun %expect-list-subset (fixture key actual label)
  (let ((expected (ax:jget fixture key)))
    (unless (eq expected :null)
      (assert-json-list-subset actual expected label))))

(defun run-agent-runtime-policy-fixture (fixture)
  "Drive the agent's policy, discovery, delegation and state surface.

Everything checked here comes out of Core through the public accessors, so
a fixture failing means either Core or this file's wiring is wrong -- not
that the expectation was skipped."
  (let ((expected-error (ax:jget fixture "expected_error_contains"))
        (signalled nil)
        (agent nil))
    (handler-case
        (progn
          (setf agent (ax::agent (ax:jget fixture "signature" "question:string -> answer:string")
                                 :options (%fixture-object fixture "options")))
          (%policy-step fixture agent "set_signature"
                        (lambda (agent value) (ax::agent-set-signature agent value)))
          (%policy-step fixture agent "discover"
                        (lambda (agent value)
                          (%fixture-expect fixture "expected_discover_result"
                                           (ax::agent-discover agent value) "discover result")))
          (%policy-step fixture agent "recall"
                        (lambda (agent value)
                          (%fixture-expect fixture "expected_recall_result"
                                           (ax::agent-recall agent value) "recall result")))
          (%policy-step fixture agent "used"
                        (lambda (agent value)
                          (let ((stage (ax:jget value "stage" "executor"))
                                (reason (ax:jget value "reason" "")))
                            (%fixture-expect fixture "expected_used_result"
                                             (ax::agent-used agent (ax:jget value "id")
                                                             :reason (if (eq reason :null) "" reason)
                                                             :stage (if (eq stage :null) "executor" stage))
                                             "used result"))))
          (%policy-step fixture agent "invoke_callable"
                        (lambda (agent value)
                          (let ((name (let ((qualified (ax:jget value "qualified_name")))
                                        (if (eq qualified :null) (ax:jget value "name") qualified))))
                            (%fixture-expect fixture "expected_callable_result_subset"
                                             (ax::agent-invoke-callable
                                              agent name
                                              :args (let ((args (ax:jget value "args")))
                                                      (if (ax::%object-p args) args (ax:object))))
                                             "callable result"))))
          (%policy-step fixture agent "replay_trace_input"
                        (lambda (agent value)
                          (%fixture-expect fixture "expected_replay_result_subset"
                                           (ax::agent-replay-trace
                                            agent value
                                            :fixtures (%fixture-object fixture "replay_fixtures"))
                                           "agent replay")))
          (%policy-step fixture agent "restore_runtime_state"
                        (lambda (agent value) (ax::agent-restore-runtime-state agent value)))
          (unless (eq (ax:jget fixture "context_operation") :null)
            (let ((result (core::agent-context-fixture-result (ax::agent-core-state agent) fixture)))
              (%fixture-expect fixture "expected_context_result_subset" result "agent context result")
              (let ((expected (ax:jget fixture "expected_context_result")))
                (unless (eq expected :null)
                  (assert-json-equal result expected "agent context result")))
              (let ((expected (ax:jget fixture "expected_context_events_subset")))
                (unless (eq expected :null)
                  (assert-json-list-subset
                   (ax:jget (%object-or-empty (ax:jget result "exported")) "context_events" (ax::%new-array))
                   expected "agent context events")))))
          (unless (eq (ax:jget fixture "final_payload") :null)
            (assert-json-equal (core::normalize-agent-final-payload (ax:jget fixture "final_payload"))
                               (ax:jget fixture "expected_final_payload") "final payload"))
          (unless (eq (ax:jget fixture "clarification_payload") :null)
            (assert-json-equal (core::normalize-agent-clarification-payload
                                (ax:jget fixture "clarification_payload"))
                               (ax:jget fixture "expected_clarification_payload")
                               "clarification payload")))
      (test-failure (condition) (error condition))
      (error (condition)
        (setf signalled (princ-to-string condition))
        (unless (and (stringp expected-error) (search expected-error signalled))
          (error condition))))
    (when (stringp expected-error)
      (unless signalled
        (error 'test-failure
               :text (format nil "expected a failure containing ~s, the fixture succeeded" expected-error)))
      (return-from run-agent-runtime-policy-fixture t))

    ;; Native background-agent selection is Core's, and the fixture pins the
    ;; exact qualified names it chooses.
    (loop for case across (let ((cases (ax:jget fixture "native_cases")))
                            (if (ax::%array-p cases) cases (ax::%new-array)))
          do (let* ((selected (core::agent-native-callables
                               (ax::agent-core-state agent)
                               (%object-or-empty (ax:jget case "features"))
                               (%object-or-empty (ax:jget case "options"))))
                    (names (ax::%new-array)))
               (loop for item across (if (ax::%array-p selected) selected (ax::%new-array))
                     do (vector-push-extend (ax:jget item "qualified_name") names))
               (assert-json-equal names (ax:jget case "expected") "native agent selection")))

    (%fixture-expect fixture "expected_runtime_contract_subset"
                     (ax::agent-runtime-contract agent) "runtime contract")
    (%fixture-expect fixture "expected_policy_subset" (ax::agent-policy agent) "agent policy")
    (%fixture-expect fixture "expected_policy_registry_subset"
                     (ax::agent-policy-registry agent) "policy registry")
    (%fixture-expect fixture "expected_state_subset" (ax::agent-state agent) "agent state")
    (let ((registry (ax::agent-policy-registry agent)))
      (%expect-list-subset fixture "expected_actor_primitives_subset"
                           (ax:jget registry "actor_primitives" (ax::%new-array)) "actor primitives")
      (%expect-list-subset fixture "expected_protocol_actions_subset"
                           (ax:jget registry "protocol_actions" (ax::%new-array)) "protocol actions")
      (%expect-list-subset fixture "expected_runtime_globals_subset"
                           (ax:jget registry "runtime_globals" (ax::%new-array)) "runtime globals")
      (%expect-list-subset fixture "expected_host_boundaries_subset"
                           (ax:jget registry "host_boundaries" (ax::%new-array)) "host boundaries"))
    (%expect-list-subset fixture "expected_callable_inventory_subset"
                         (ax::agent-callable-inventory agent) "callable inventory")
    (%expect-list-subset fixture "expected_discovery_catalog_subset"
                         (ax::agent-discovery-catalog agent) "discovery catalog")
    (let ((state (ax::agent-export-runtime-state agent)))
      (dolist (pair '(("expected_discovered_tool_docs_subset" "discovered_tool_docs" "discovered tools")
                      ("expected_loaded_skill_docs_subset" "loaded_skill_docs" "loaded skills")
                      ("expected_loaded_memories_subset" "loaded_memories" "loaded memories")
                      ("expected_used_memories_subset" "used_memories" "used memories")
                      ("expected_used_skills_subset" "used_skills" "used skills")
                      ("expected_guidance_log_subset" "guidance_log" "guidance log")
                      ("expected_function_call_traces_subset" "function_call_traces" "function call traces")
                      ("expected_policy_trace_subset" "policy_trace" "policy trace")
                      ("expected_action_log_subset" "action_log" "action log")))
        (%expect-list-subset fixture (first pair)
                             (ax:jget state (second pair) (ax::%new-array)) (third pair)))
      (%fixture-expect fixture "expected_exported_state_subset" state "exported runtime state"))
    (%fixture-expect fixture "expected_optimizer_metadata_subset"
                     (ax::agent-optimizer-metadata agent) "optimizer metadata")
    (assert-agent-trace agent fixture)
    t))

(defun %object-or-empty (value)
  (if (ax::%object-p value) value (ax:object)))

(defun assert-agent-trace (agent fixture)
  "The trace, usage and chat-log expectations every agent fixture shares."
  (%fixture-expect fixture "expected_usage_subset" (ax::agent-usage agent) "agent usage")
  (let ((expected (ax:jget fixture "expected_chat_log_length")))
    (unless (eq expected :null)
      (expect-equal (length (ax::agent-chat-log agent)) expected "agent chat log length")))
  (let ((trace (ax::agent-trace agent)))
    (%fixture-expect fixture "expected_trace_subset" trace "agent trace")
    (let ((expected (ax:jget fixture "expected_trace_event_kinds")))
      (unless (eq expected :null)
        (let ((kinds (ax::%new-array)))
          (loop for event across (let ((events (ax:jget trace "events")))
                                   (if (ax::%array-p events) events (ax::%new-array)))
                do (vector-push-extend (ax:jget event "kind") kinds))
          (assert-json-equal kinds expected "agent trace event kinds"))))
    (when (ax:json-true-p (ax:jget fixture "replay_trace"))
      (let ((replay-fixtures (core::core-map-merge (%fixture-object fixture "replay_fixtures")
                                                   (ax:object))))
        (let ((kinds (ax:jget fixture "expected_trace_event_kinds")))
          (unless (or (eq kinds :null)
                      (nth-value 1 (gethash "expected_event_kinds" replay-fixtures)))
            (ax::%set-key replay-fixtures "expected_event_kinds" kinds)))
        (let ((output (ax:jget fixture "expected_output")))
          (unless (or (eq output :null)
                      (nth-value 1 (gethash "expected_output" replay-fixtures)))
            (ax::%set-key replay-fixtures "expected_output" output)))
        (let ((replayed (ax::agent-replay-trace agent trace :fixtures replay-fixtures))
              (expected (ax:jget fixture "expected_replay_result_subset")))
          (if (eq expected :null)
              (assert-json-subset replayed (ax:object "ok" ax:true "status" "replayed")
                                  "agent replay")
              (assert-json-subset replayed expected "agent replay")))))))

;;; ------------------------------------------------------------------
;;; agent_runtime_session
;;; ------------------------------------------------------------------

(defun run-agent-runtime-session-fixture (fixture)
  "Drive one agent runtime session against the scripted runtime.

The scripted runtime is what makes this deterministic: each step pins the
code it expects, patches the session globals and answers a fixed envelope,
so what is being checked is Core's session handling and this file's
session wiring, not a language engine."
  (let* ((agent (ax::agent (ax:jget fixture "signature" "question:string -> answer:string")
                           :options (%fixture-object fixture "options")))
         (runtime (make-scripted-runtime
                   :script (let ((script (ax:jget fixture "runtime_script")))
                             (if (ax::%array-p script) (coerce script 'list) '()))
                   :capabilities (%fixture-object fixture "runtime_capabilities")))
         (operation (let ((value (ax:jget fixture "operation" "test")))
                      (if (eq value :null) "test" value)))
         (context-values (let ((values (ax:jget fixture "context_values")))
                           (if (ax::%object-p values)
                               values
                               (%fixture-object fixture "input"))))
         (expected-error (ax:jget fixture "expected_error_contains"))
         (signalled nil)
         (result :null))
    (handler-case
        (cond
          ((string= operation "test")
           (setf result (ax::agent-test agent runtime (ax:jget fixture "code" "")
                                        :context-values context-values
                                        :options (%fixture-object fixture "runtime_options"))))
          ((string= operation "reserved")
           (setf result (ax::agent-test agent runtime (ax:jget fixture "code" "")
                                        :context-values context-values
                                        :options (ax:object))))
          ((string= operation "steps")
           (loop for step across (let ((steps (ax:jget fixture "steps")))
                                   (if (ax::%array-p steps) steps (ax::%new-array)))
                 do (progn
                      (unless (eq (ax:jget step "restore_session_state") :null)
                        (ax::agent-restore-session-state agent
                                                         (ax:jget step "restore_session_state")))
                      (setf result (ax::agent-execute-actor-step
                                    agent runtime (ax:jget step "code" "")
                                    :values (let ((values (ax:jget step "values")))
                                              (if (ax::%object-p values) values context-values))
                                    :options (%object-or-empty (ax:jget step "options"))))
                      (when (ax:json-true-p (ax:jget step "inspect"))
                        (ax::agent-inspect-runtime agent))
                      (when (ax:json-true-p (ax:jget step "export_session_state"))
                        (ax::agent-export-session-state agent))))
           (when (ax:json-true-p (ax:jget fixture "close_runtime_session"))
             (ax::agent-close-runtime-session agent)))
          (t (error 'test-failure
                    :text (format nil "unknown agent runtime session operation ~s" operation))))
      (test-failure (condition) (error condition))
      (error (condition)
        (setf signalled (princ-to-string condition))
        (unless (and (stringp expected-error) (search expected-error signalled))
          (error condition))
        (setf result :null)))
    (when (stringp expected-error)
      (unless signalled
        (error 'test-failure
               :text (format nil "expected a failure containing ~s, the fixture succeeded"
                             expected-error))))
    (%fixture-expect fixture "expected_result_subset" result "runtime result")
    (let ((expected (ax:jget fixture "expected_result")))
      (unless (eq expected :null)
        (assert-json-equal result expected "runtime result")))
    (let ((exported (ax::agent-export-runtime-state agent)))
      (%fixture-expect fixture "expected_exported_state_subset" exported "runtime state")
      (%expect-list-subset fixture "expected_action_log_subset"
                           (ax:jget exported "action_log" (ax::%new-array)) "action log")
      (%expect-list-subset fixture "expected_status_log_subset"
                           (ax:jget exported "status_log" (ax::%new-array)) "status log")
      (let ((expected (ax:jget fixture "expected_runtime_inspection")))
        (unless (eq expected :null)
          (assert-json-equal (ax:jget exported "runtime_inspection") expected "runtime inspection")))
      (let ((expected (ax:jget fixture "expected_runtime_inspection_contains")))
        (unless (eq expected :null)
          (expect-contains (core::core-js-text (ax:jget exported "runtime_inspection"))
                           (core::core-js-text expected) "runtime inspection")))
      (let ((absent (ax:jget fixture "expected_absent_runtime_session_globals")))
        (unless (eq absent :null)
          (let ((globals (ax:jget (%object-or-empty (ax:jget exported "runtime_session_state"))
                                  "globals")))
            (loop for key across (if (ax::%array-p absent) absent (ax::%new-array))
                  do (expect (not (and (ax::%object-p globals)
                                       (nth-value 1 (gethash (core::core-js-text key) globals))))
                             (format nil "runtime session globals must not carry ~a" key)))))))
    (let ((expected (ax:jget fixture "expected_session_count")))
      (unless (eq expected :null)
        (expect-equal (length (scripted-runtime-sessions runtime)) expected "session count")))
    (let ((expected (ax:jget fixture "expected_closed_session_count")))
      (unless (eq expected :null)
        (expect-equal (count-if #'scripted-session-closed (scripted-runtime-sessions runtime))
                      expected "closed session count")))
    (let ((expected (ax:jget fixture "expected_executed")))
      (unless (eq expected :null)
        (assert-json-equal (scripted-runtime-executed runtime) expected "executed code")))
    (let ((expected (ax:jget fixture "expected_create_globals_subset")))
      (unless (eq expected :null)
        (expect (plusp (length (scripted-runtime-create-requests runtime)))
                "at least one runtime create_session request")
        (assert-json-subset
         (ax:jget (aref (scripted-runtime-create-requests runtime)
                        (1- (length (scripted-runtime-create-requests runtime))))
                  "globals")
         expected "runtime create globals")))
    (let ((expected (ax:jget fixture "expected_create_options_subset")))
      (unless (eq expected :null)
        (expect (plusp (length (scripted-runtime-create-requests runtime)))
                "at least one runtime create_session request")
        (assert-json-subset
         (ax:jget (aref (scripted-runtime-create-requests runtime)
                        (1- (length (scripted-runtime-create-requests runtime))))
                  "options")
         expected "runtime create options")))
    (let ((expected (ax:jget fixture "expected_execute_options_subset")))
      (unless (eq expected :null)
        (expect (plusp (length (scripted-runtime-execute-options runtime)))
                "at least one runtime execute request")
        (assert-json-subset (aref (scripted-runtime-execute-options runtime)
                                  (1- (length (scripted-runtime-execute-options runtime))))
                            expected "runtime execute options")))
    (assert-agent-trace agent fixture)
    t))

;;; ------------------------------------------------------------------
;;; agent_playbook_coverage
;;; ------------------------------------------------------------------

(defun run-agent-playbook-coverage-fixture (fixture)
  "Which failure signatures a playbook snapshot already covers."
  (loop for case across (let ((cases (ax:jget fixture "cases")))
                          (if (ax::%array-p cases) cases (ax::%new-array)))
        do (assert-json-equal
            (core::agent-collect-covered-failure-signatures
             (%object-or-empty (ax:jget case "snapshot")))
            (let ((expected (ax:jget case "expected_covered")))
              (if (ax::%array-p expected) expected (ax::%new-array)))
            (format nil "playbook coverage ~a" (ax:jget case "name"))))
  t)


;;; ------------------------------------------------------------------
;;; agent_forward
;;; ------------------------------------------------------------------
;;;
;;; A whole agent run with no provider. The fixture supplies the model's
;;; answers at the Ax level, as {"content": ...}; this port's stage boundary
;;; is an AI client, so the answers are handed back through an injected
;;; transport that renders each one as the provider payload a real request
;;; would have returned. Nothing reaches the network, and the requests the
;;; agent actually sent stay readable so a fixture can assert on them.

(defparameter +forward-unsupported-keys+
  '(("stream_events" . "the agent runner expects per-response stream chunks"))
  "Fixture keys the forward runner cannot honour, and why.

A fixture carrying one of these is reported as partial with the reason
rather than run with the feature quietly dropped: a pass that ignored the
thing the fixture was written for would be worse than no pass at all.")

(defun %forward-unsupported (fixture)
  (loop for (key . reason) in +forward-unsupported-keys+
        when (not (eq (ax:jget fixture key) :null))
          collect (format nil "~a: ~a" key reason)))

(defstruct (scripted-ai (:conc-name scripted-ai-))
  (responses '()) (requests (ax::%new-array))
  on-request (transcript (ax::%new-array))
  (speak-responses '()) (speak-requests (ax::%new-array))
  (transcribe-responses '()) (transcribe-requests (ax::%new-array))
  ;; Core's request object, recorded beside the wire body. Some of what a
  ;; fixture pins never reaches the wire -- provider_metadata, which carries
  ;; the structured output rung, is Core's to read and the provider consumes
  ;; it rather than sending it -- so a runner that only watched the HTTP body
  ;; could not see it at all.
  (core-requests (ax::%new-array)))

(defclass scripted-ai-client (ax::provider-client)
  ((advertised :initarg :advertised :initform :null :reader %advertised-features)
   (script :initarg :script :initform nil :reader %client-script))
  (:documentation
   "A real provider client that reports the capabilities a fixture names.

A fixture's \"features\" key is the model's side of the output contract:
Core picks the text contract or a structured rung from it, so a scripted
client that reported the live provider's capabilities would be answering
a different question from the one the fixture asked. Only the capability
report is overridden -- the chat, stream and parse path stay the native
client's, because the fixture is checking what that path does under those
capabilities."))

(defmethod ax::ax-features ((service scripted-ai-client) &optional model)
  (declare (ignore model))
  (let ((advertised (%advertised-features service)))
    (if (ax::%object-p advertised)
        advertised
        (call-next-method))))

(defun %agent-request-stage (request)
  (let ((system (with-output-to-string (out)
                  (loop for message across (ax:jget request "chat_prompt" #())
                        when (equal (ax:jget message "role") "system")
                          do (write-string (ax:jget message "content" "") out)))))
    (cond ((search "You (`distiller`)" system) "distiller")
          ((search "You (`executor`)" system) "executor")
          ((or (search "`Generator answer`" system) (search "`Question context`" system)) "playbook")
          ((search "Your task is to generate new fields: `Completion`" system) "runtime_less")
          ((or (search "context-map Distiller" system) (search "context-map Cartographer" system)) "context_map")
          (t "responder"))))

(defun %record-agent-request (script request)
  (vector-push-extend (ax:encode-json request) (scripted-ai-core-requests script))
  (vector-push-extend (concatenate 'string "request:" (%agent-request-stage request))
                      (scripted-ai-transcript script))
  (when (scripted-ai-on-request script)
    (funcall (scripted-ai-on-request script) (length (scripted-ai-core-requests script)))))

(defmethod ax::ax-chat ((service scripted-ai-client) request &optional options)
  "Record Core's request object, then answer through the real provider path."
  (declare (ignore options))
  (let ((script (%client-script service)))
    (when script
      (%record-agent-request script request)))
  (call-next-method))

(defclass agent-fixture-stream ()
  ((chunks :initarg :chunks :accessor %agent-stream-chunks)
   (closed :initform nil :accessor %agent-stream-closed)))

(defmethod ax::ax-stream ((service scripted-ai-client) request &optional options)
  "Inject model chunks at the AI boundary; Core still parses every delta."
  (declare (ignore options))
  (let* ((script (%client-script service))
         (response (pop (scripted-ai-responses script))))
    (%record-agent-request script request)
    (vector-push-extend (ax:encode-json request) (scripted-ai-requests script))
    (unless response (error 'test-failure :text "scripted agent stream exhausted"))
    (let ((chunks (ax:jget response "stream")))
      (make-instance 'agent-fixture-stream
                     :chunks (if (ax::%array-p chunks) (coerce chunks 'list)
                                 (list (ax:object "results" (vector response))))))))

(defmethod ax::ax-stream-next ((stream agent-fixture-stream))
  (expect (not (%agent-stream-closed stream)) "agent stream is still open")
  (or (pop (%agent-stream-chunks stream)) :null))

(defmethod ax::ax-stream-close ((stream agent-fixture-stream))
  (setf (%agent-stream-closed stream) t))

(defmethod ax::ax-speak ((service scripted-ai-client) request &optional options)
  (declare (ignore options))
  (let ((script (%client-script service)))
    (vector-push-extend request (scripted-ai-speak-requests script))
    (or (pop (scripted-ai-speak-responses script))
        (error 'test-failure :text "scripted speech responses exhausted"))))

(defmethod ax::ax-transcribe ((service scripted-ai-client) request &optional options)
  (declare (ignore options))
  (let ((script (%client-script service)))
    (vector-push-extend request (scripted-ai-transcribe-requests script))
    (or (pop (scripted-ai-transcribe-responses script))
        (error 'test-failure :text "scripted transcription responses exhausted"))))

(defun %scripted-ai-client (responses &optional (features :null))
  "An AI client whose transport answers from RESPONSES.

Each fixture response is an Ax-level answer. It is rendered into the
provider payload shape the client parses, so the whole request, parse and
validation path runs exactly as it would against a real provider.

FEATURES, when the fixture names them, are the capabilities the client
reports. The client is built by `ai' exactly as production builds one and
then changed to the reporting subclass, so nothing about the request or
response path is a test construction -- only the answer to \"what can this
model do\", which is the question the fixture is setting up."
  (let* ((script (make-scripted-ai :responses (if (ax::%array-p responses)
                                                  (coerce responses 'list)
                                                  '())))
         (client
           ;; The profile is pinned rather than inferred from the model,
           ;; because the model catalog decides which dialect a model speaks
           ;; and a scripted answer has to be in that dialect. Naming the
           ;; profile keeps the two from drifting apart as the catalog moves.
           (ax::provider :profile "openai-responses" :model "gpt-6-luna"
                         :api-key "conformance-key"
            :transport
            (lambda (url headers json-body &rest rest)
              (declare (ignore url headers rest))
              (vector-push-extend json-body (scripted-ai-requests script))
              (let ((next (if (scripted-ai-responses script)
                              (pop (scripted-ai-responses script))
                              (error 'test-failure
                                     :text "scripted AI client exhausted; the fixture gave fewer responses than the agent asked for"))))
                (values (ax:encode-json (%provider-payload next :responses)) 200))))))
    ;; Always the recording subclass, so Core's request object is captured
    ;; whether or not the fixture names features.
    (change-class client 'scripted-ai-client
                  :advertised (if (ax::%object-p features) features :null)
                  :script script)
    (values client script)))

(defun %scripted-provider-client (spec responses)
  "The teacher client a fixture's teacher_client SPEC describes.

Built through `provider' rather than `ai' because the spec's options carry
modelInfo, and that is what tells the cost guard a model is expensive. An
`ai' client has nowhere to put it, so the guard never fires and a fixture
written to watch an expensive teacher be refused instead watches it answer
every call."
  (let ((script (make-scripted-ai :responses (if (ax::%array-p responses)
                                                 (coerce responses 'list)
                                                 '()))))
    (values
     (ax::provider :profile "openai"
                   :model (let ((model (ax:jget spec "model")))
                            (if (eq model :null) "gpt-6-luna" (core::core-js-text model)))
                   :api-key "conformance-key"
                   :options (%fixture-object spec "options")
                   ;; A provider transport takes the method as well.
                   :transport
                   (lambda (url headers json-body &rest rest)
                     (declare (ignore url headers rest))
                     (vector-push-extend json-body (scripted-ai-requests script))
                     (let ((next (if (scripted-ai-responses script)
                                     (pop (scripted-ai-responses script))
                                     (error 'test-failure
                                            :text "scripted teacher client exhausted; the fixture gave fewer responses than the run asked for"))))
                       (values (ax:encode-json (%provider-payload next)) 200))))
     script)))

(defun %provider-payload (response &optional (dialect :chat))
  "One fixture response as the wire payload DIALECT expects.

A profile's dialect decides the envelope, and the two are not
interchangeable: a Responses profile handed a chat-completions body reads
no content at all and the run continues with an empty answer rather than
an error. The scripted client therefore answers in the dialect of the
profile it was built for, which is what makes the real parse path run."
  (let* ((content (let ((value (ax:jget response "content")))
                    (if (eq value :null) "" (core::core-js-text value))))
         (usage (ax:jget response "usage"))
         (prompt (let ((value (if (ax::%object-p usage)
                                 (ax:jget usage "promptTokens" (ax:jget usage "prompt_tokens" 0)) 0)))
                   (if (realp value) value 0)))
         (completion (let ((value (if (ax::%object-p usage)
                                     (ax:jget usage "completionTokens" (ax:jget usage "completion_tokens" 0)) 0)))
                       (if (realp value) value 0))))
    (ecase dialect
      (:responses
       (ax:object "id" "resp-conformance"
                  "model" "gpt-6-luna"
                  "output" (vector (ax:object "type" "message"
                                              "id" "msg-conformance"
                                              "content" (vector (ax:object "type" "output_text"
                                                                           "text" content))))
                  "usage" (ax:object "input_tokens" prompt
                                     "output_tokens" completion
                                     "total_tokens" (+ prompt completion))))
      (:chat
       (ax:object "choices" (vector (ax:object "index" 0
                                               "finish_reason" "stop"
                                               "message" (ax:object "role" "assistant"
                                                                    "content" content)))
                  "usage" (ax:object "prompt_tokens" prompt
                                     "completion_tokens" completion
                                     "total_tokens" (+ prompt completion)))))))

(defun %playbook-student (options client)
  "OPTIONS with CLIENT as the playbook's student, when it names none.

A playbook needs a client for its Reflector and Curator. A fixture does not
carry one, because the scripted client is the runner's, so the runner
supplies it exactly as the other ports' runners do."
  (let ((playbook (ax:jget options "playbook")))
    (when (and (ax::%object-p playbook)
               (eq (ax:jget playbook "studentAI") :null)
               (eq (ax:jget playbook "student_ai") :null))
      (ax::%set-key playbook "studentAI" client)))
  options)

(defparameter +semantic-observer-labels+
  '(("onLoadedSkills" "loaded_skills" t)
    ("onLoadedMemories" "loaded_memories" t)
    ("onUsedSkills" "constructor.used_skills" nil)
    ("onUsedMemories" "constructor.used_memories" nil))
  "The constructor observers a semantic-parity fixture records, and whether
each one throws.

Two of them throw on purpose: a fixture proves that a caller watching the
run cannot fail it, so an observer that signals has to be part of the
recorded run rather than the end of it.")

(defparameter +semantic-forward-observer-labels+
  '(("onUsedSkills" . "forward.used_skills")
    ("onUsedMemories" . "forward.used_memories"))
  "The per-run observers, which win over the constructor's and are labelled
apart so the transcript shows which one answered.")

(defun %record-observer (transcript label throws)
  "An observer that appends to TRANSCRIPT under LABEL."
  (lambda (payload)
    (vector-push-extend (ax:object "callback" label
                                   "payload" (if (ax::%array-p payload) payload (ax::%new-array)))
                        transcript)
    (when throws
      (error 'test-failure :text "semantic parity observer failure"))))

(defun %install-semantic-observers (options transcript table)
  "Replace each observer OPTIONS already names with a recording one.

Only keys the fixture set are replaced: adding an observer a fixture did
not ask for would change which callbacks the run makes."
  (when (ax::%object-p options)
    (dolist (entry table)
      (destructuring-bind (key label &optional throws)
          (if (consp (cdr entry)) entry (list (car entry) (cdr entry) nil))
        (when (nth-value 1 (gethash key options))
          (ax::%set-key options key (%record-observer transcript label throws))))))
  options)

(defun %attach-scripted-mcp (options spec)
  "Attach the fixture's scripted MCP servers to OPTIONS.

A fixture groups its servers by owner, and two owners may publish the same
namespace -- a child agent with its own inventory server alongside the
parent's is the whole point of the inheritance fixtures. So each owner gets
its own set of clients, and only the parent's are attached here; a
child-owned server is reached through the child's own configuration.
Building one context from all of them would be a namespace collision, which
is what the MCP layer correctly refuses.

Returns the transports keyed exactly as expected_mcp_calls names them,
\"<owner>/<namespace>\", so a fixture can assert what each server was really
asked. These are real clients over scripted transports, so era
classification, catalog pagination and the rest of the protocol path run;
they are lazy, so attaching them costs nothing until a tool is reached."
  (unless (ax::%array-p spec)
    (return-from %attach-scripted-mcp nil))
  (let ((owners '())
        (transports (ax:object)))
    ;; Group in the order the fixture lists them, so a server's transport is
    ;; recorded under the owner that published it.
    (loop for entry across spec
          do (let* ((owner (let ((given (ax:jget entry "owner" "parent")))
                             (if (eq given :null) "parent" (core::core-js-text given))))
                    (group (assoc owner owners :test #'string=)))
               (if group
                   (vector-push-extend entry (cdr group))
                   (let ((fresh (ax::%new-array)))
                     (vector-push-extend entry fresh)
                     (setf owners (append owners (list (cons owner fresh))))))))
    (let ((by-owner '()))
      (dolist (group owners)
        (multiple-value-bind (clients owner-transports) (ax::mcp-scripted-clients (cdr group))
          (if (string= (car group) "parent")
              (ax::%set-key options "mcp" clients)
              (setf by-owner (append by-owner (list (cons (car group) clients)))))
          (dolist (namespace (ax::%object-keys owner-transports))
            (ax::%set-key transports (format nil "~a/~a" (car group) namespace)
                          (gethash namespace owner-transports)))))
      (values transports by-owner))))

(defvar *fixture-child-runtimes* nil)

(defun %production-javascript-runtime ()
  "The shipped TypeScript runtime adapter, with its real host callback bridge."
  (let* ((root (uiop:ensure-directory-pathname
                (or (uiop:getenv "AXIR_REPO_ROOT")
                    (truename (merge-pathnames "../../" (%conformance-directory))))))
         (adapter (or (uiop:getenv "AXIR_AXJS_RUNTIME_SERVER")
                      (namestring (merge-pathnames "tools/axir/adapters/axjs-runtime-server.ts" root))))
         (loader (merge-pathnames "node_modules/tsx/dist/loader.mjs" root)))
    (ax::make-process-runtime
     (list "node" (concatenate 'string "--import=" (namestring loader)) adapter)
     :language "JavaScript" :timeout 30)))

(defun %attach-scripted-children (agent fixture child-clients)
  "Build each child agent the fixture declares and expose it to AGENT.

A child is a whole agent in its own right, so it gets its own scripted
runtime from its own runtime_script. Sharing the parent's would be wrong
in a way that looks like a sequencing bug: the child's first step would
consume the slot the parent's next step expects, and the parent would be
blamed for executing the child's code.

CHILD-CLIENTS maps an owner name to the MCP clients that owner published,
so a child-owned server is reached through the child's own configuration
rather than the parent's -- two owners may publish the same namespace, and
building one context from both is the collision the MCP layer refuses."
  (let ((children (ax:jget fixture "child_agents")))
    (unless (ax::%array-p children)
      (return-from %attach-scripted-children nil))
    (loop for entry across children
          do (let* ((namespace (core::core-js-text (ax:jget entry "namespace")))
                    (name (core::core-js-text (ax:jget entry "name")))
                    (qualified (format nil "~a.~a" namespace name))
                    (options (core::core-map-merge (%fixture-object entry "options")
                                                   (ax:object)))
                    (steps (ax:jget entry "runtime_script"))
                    (owned (assoc qualified child-clients :test #'string=)))
               (when (equal (ax:jget entry "runtime_engine") "javascript")
                 (let ((runtime (%production-javascript-runtime)))
                   (push runtime *fixture-child-runtimes*)
                   (ax::%set-key options "runtime" runtime)))
               (when (ax::%array-p steps)
                 (ax::%set-key options "runtime"
                               (make-scripted-runtime
                                :script (coerce steps 'list)
                                :capabilities (%fixture-object entry "runtime_capabilities"))))
               (when owned
                 (ax::%set-key options "mcp" (cdr owned)))
               (ax::agent-add-child
                agent namespace name
                (ax::agent (ax:jget entry "signature" "question:string -> answer:string")
                           :options options))))))

(defun %assert-mcp-calls (fixture transports)
  "Each server was asked exactly what the fixture says it was asked."
  (let ((expected (ax:jget fixture "expected_mcp_calls")))
    (when (and (ax::%object-p expected) transports)
      (dolist (key (ax::%object-keys expected))
        (let ((transport (ax:jget transports key)))
          (expect (not (eq transport :null))
                  (format nil "the fixture names a scripted MCP server that exists: ~a" key))
          (assert-json-equal (ax::mcp-scripted-tool-calls transport)
                             (gethash key expected)
                             (format nil "MCP tool calls for ~a" key)))))))

(defun %fixture-runtime (fixture)
  "The code runtime one fixture's run executes against.

A fixture naming a runtime_engine wants a real engine: the model writes
code and an actual interpreter runs it, which is the only way to check
that the agent reads what the code really did rather than what a script
said it would. Everything else gets the scripted runtime, where the point
is the exact code the agent chose to run."
  (let ((engine (ax:jget fixture "runtime_engine"))
        (steps (ax:jget fixture "runtime_script")))
    (cond
      ((and (stringp engine) (string-equal engine "javascript"))
       (%production-javascript-runtime))
      ((stringp engine)
       (error 'test-failure
              :text (format nil "fixture names runtime_engine ~s, which this port has no worker for"
                            engine)))
      ((ax::%array-p steps)
       (make-scripted-runtime
        :script (coerce steps 'list)
        :capabilities (%fixture-object fixture "runtime_capabilities")))
      (t nil))))

(defun %shutdown-engine (runtime real-engine)
  "Stop RUNTIME when it is a live worker process. Safe to call twice."
  (dolist (child *fixture-child-runtimes*)
    (ax::runtime-shutdown child))
  (when real-engine
    (ignore-errors (ax::runtime-shutdown runtime))))

(defun %install-run-observers (fixture options script calls)
  (loop for name across (ax:jget fixture "observers" #())
        do (let* ((label name)
                  (callback
                    (lambda (payload)
                      (vector-push-extend label (scripted-ai-transcript script))
                      (vector-push-extend
                       (ax:object "callback" label "payload"
                                  (if (equal label "playbook_update")
                                      (ax:object "status" (ax:jget payload "status")) payload))
                       calls))))
             (cond
               ((equal label "citations")
                (let ((config (%fixture-object options "citations")))
                  (ax::%set-key config "onCitations" callback)
                  (ax::%set-key options "citations" config)))
               ((equal label "playbook_update")
                (let ((config (%fixture-object options "playbook")))
                  (ax::%set-key config "onUpdate" callback)
                  (ax::%set-key options "playbook" config)))
               ((equal label "used_memories") (ax::%set-key options "onUsedMemories" callback))
               ((equal label "used_skills") (ax::%set-key options "onUsedSkills" callback))
               (t (error 'test-failure :text (format nil "unknown agent observer ~a" label)))))))

(defun %assert-run-projections (fixture agent script deltas calls)
  (dolist (entry (list (cons "expected_deltas" deltas)
                      (cons "expected_observer_calls" calls)
                      (cons "expected_speak_requests" (scripted-ai-speak-requests script))
                      (cons "expected_transcript" (scripted-ai-transcript script))))
    (let ((expected (ax:jget fixture (car entry))))
      (unless (eq expected :null)
        (assert-json-equal (cdr entry) expected (car entry)))))
  (let* ((requests (map 'vector #'ax:parse-json (scripted-ai-core-requests script)))
         (roles (map 'vector (lambda (request)
                               (map 'vector (lambda (message) (ax:jget message "role"))
                                    (ax:jget request "chat_prompt" #()))) requests)))
    (let ((expected (ax:jget fixture "expected_request_roles")))
      (unless (eq expected :null) (assert-json-equal roles expected "request roles")))
    (loop for first across (ax:jget fixture "expected_stage_first_requests" #())
          do (let* ((index (ax:jget first "index"))
                    (request (and (< index (length requests)) (aref requests index))))
               (expect request "stage first request exists")
               ;; Report the first differing text, not pages of common prompt
               ;; prefix. The full structural comparison still follows.
               (loop for actual across (ax:jget request "chat_prompt" #())
                     for expected across (ax:jget first "messages")
                     for message-index from 0
                     do (let ((a (ax:jget actual "content")) (e (ax:jget expected "content")))
                          (when (and (stringp a) (stringp e) (not (string= a e)))
                            (let* ((offset (mismatch a e)) (start (max 0 (- offset 60))))
                              (error 'test-failure :text
                                     (format nil "request ~D (~A), message ~D differs at character ~D~%expected: ~S~%actual: ~S"
                                             index (ax:jget first "stage") message-index offset
                                             (subseq e start (min (length e) (+ offset 180)))
                                             (subseq a start (min (length a) (+ offset 180)))))))))
               (assert-json-equal
                (map 'vector (lambda (message)
                               (ax:object "role" (ax:jget message "role")
                                          "content" (ax:jget message "content")))
                     (ax:jget request "chat_prompt" #()))
                (ax:jget first "messages") "stage first request"))))
  (let ((expected (ax:jget fixture "expected_chat_log_shape")))
    (unless (eq expected :null)
      (assert-json-equal
       (map 'vector (lambda (entry) (ax:object "name" (ax:jget entry "name")
                                              "stage" (ax:jget entry "stage")))
            (ax::agent-chat-log agent)) expected "chat log shape"))))

(defun %run-forwards (agent client fixture run-options)
  "Run FIXTURE's forward call, or its sequence of them, and return the output.

A fixture with forward_runs is making a claim about what one agent carries
between calls -- a runtime passed per run, an error turn carried or
dropped, a signature changed mid-life -- so the runs share one agent and
their outputs are collected in order. Running only the first would check
the opposite of what the fixture is for.

A run's own forward_options win over the fixture's, and a run marked
without_runtime drops the runtime rather than inheriting it, which is how
a fixture asks what happens when the engine is absent for one call."
  (let ((runs (ax:jget fixture "forward_runs")))
    (unless (ax::%array-p runs)
      (return-from %run-forwards
        (ax::agent-forward agent client (%fixture-object fixture "input")
                           :options run-options)))
    (let ((outputs (ax::%new-array)))
      (loop for run across runs
            do (let ((options (core::core-map-merge run-options
                                                    (%fixture-object run "forward_options"))))
                 (when (ax:json-true-p (ax:jget run "without_runtime"))
                   (remhash "runtime" options))
                 (let ((signature (ax:jget run "set_signature")))
                   (unless (eq signature :null)
                     (ax::agent-set-signature agent signature)))
                 (vector-push-extend
                  (ax::agent-forward agent client (%fixture-object run "input")
                                     :options options)
                  outputs)))
      outputs)))

(defun run-agent-forward-fixture (fixture)
  "Run one whole agent forward against the scripted client."
  (let ((unsupported (%forward-unsupported fixture)))
    (when unsupported
      (signal 'partial-fixture :reason (format nil "~{~a~^; ~}" unsupported))
      (return-from run-agent-forward-fixture t)))
  (multiple-value-bind (client script)
      (%scripted-ai-client (ax:jget fixture "responses") (ax:jget fixture "features"))
    (setf (scripted-ai-speak-responses script) (coerce (ax:jget fixture "speak_responses" #()) 'list)
          (scripted-ai-transcribe-responses script) (coerce (ax:jget fixture "transcribe_responses" #()) 'list))
    (let* ((*fixture-child-runtimes* nil)
           (transcript (ax::%new-array))
           (calls (ax::%new-array))
           (deltas (ax::%new-array))
           (streaming (equal (ax:jget fixture "kind") "agent_streaming_forward"))
           (observers-enabled (not (eq (ax:jget fixture "expected_observer_transcript") :null)))
           (runtime (%fixture-runtime fixture))
           (real-engine (and runtime (typep runtime 'ax::process-runtime)))
           (run-options (core::core-map-merge (%fixture-object fixture "forward_options")
                                              (ax:object)))
           (expected-error (ax:jget fixture "expected_error_contains"))
           (signalled nil)
           (agent nil)
           (control nil)
           (mcp-transports nil)
           (output :null))
      (when runtime (ax::%set-key run-options "runtime" runtime))
      ;; A control fixture pins the run's whole lifecycle, so the control is a
      ;; real one and the events are compared against what it actually heard.
      (when (ax:json-true-p (ax:jget fixture "control"))
        (setf control (ax::make-run-control))
        (ax::%set-key run-options "control" control))
      (let ((steer (ax:jget fixture "control_steer")))
        (when (hash-table-p steer)
          (expect control "steering requires a run control")
          (setf (scripted-ai-on-request script)
                (lambda (index)
                  (when (= index (ax:jget steer "during_request"))
                    (ax::run-control-steer control (ax:jget steer "text")))))))
      ;; Construction is inside the guard: a configuration fixture expects
      ;; agent() itself to refuse, and that refusal is the result under test.
      (handler-case
          (progn
            (let ((agent-options (%playbook-student (%fixture-object fixture "options") client)))
              ;; An owned runtime contributes its actual usage instructions
              ;; while Core constructs stage prompts. Metadata-only configs
              ;; cannot stand in for a concrete engine here.
              (when (and real-engine (not (ax:json-true-p (ax:jget fixture "runtime_on_forward"))))
                (ax::%set-key agent-options "runtime" runtime))
              (%install-run-observers fixture agent-options script calls)
              (when observers-enabled
                (%install-semantic-observers agent-options transcript
                                             +semantic-observer-labels+))
              (multiple-value-bind (transports child-clients)
                  (%attach-scripted-mcp agent-options (ax:jget fixture "mcp_clients"))
                (setf mcp-transports transports)
                (when observers-enabled
                  (%install-semantic-observers run-options transcript
                                               +semantic-forward-observer-labels+))
                (setf agent (ax::agent (ax:jget fixture "signature"
                                                "question:string -> answer:string")
                                       :options agent-options))
                (%attach-scripted-children agent fixture child-clients)
                ;; A fixture that reshapes the agent after construction does it
                ;; here, before the run: these compose into the stage prompts,
                ;; so applying them later would not reach the requests the
                ;; fixture pins.
                (let ((instruction (ax:jget fixture "set_instruction")))
                  (unless (eq instruction :null)
                    (ax::agent-set-instruction agent (core::core-js-text instruction))))
                (let ((addendum (ax:jget fixture "add_actor_instruction")))
                  (unless (eq addendum :null)
                    (ax::agent-add-actor-instruction agent (core::core-js-text addendum))))
                ;; A restored agent carries the checkpoint the fixture hands it
                ;; into the run, which is the whole point of these cases: the
                ;; pending summary is what makes the summarizer turn happen at
                ;; all, so restoring after the run would check nothing.
                (let ((snapshot (ax:jget fixture "restore_runtime_state")))
                  (when (ax::%object-p snapshot)
                    (ax::agent-restore-runtime-state agent snapshot)))))
            (setf output
                  (if streaming
                      (catch 'stop-agent-stream
                        (ax::agent-streaming-forward
                         agent client (%fixture-object fixture "input")
                         (lambda (delta)
                           (vector-push-extend (ax:parse-json (ax:encode-json delta)) deltas)
                           (when (eql (length deltas) (ax:jget fixture "stop_after_deltas"))
                             (throw 'stop-agent-stream :null)))
                         :options run-options))
                      (%run-forwards agent client fixture run-options))))
        (test-failure (condition)
          (%shutdown-engine runtime real-engine)
          (error condition))
        (error (condition)
          (%shutdown-engine runtime real-engine)
          (setf signalled (princ-to-string condition))
          (when (typep condition 'ax::agent-clarification-error)
            (%fixture-expect fixture "expected_clarification"
                             (ax::agent-clarification condition) "clarification"))
          (unless (and (stringp expected-error) (search expected-error signalled))
            (error condition))))
      ;; A real engine is a live process, so it is stopped as soon as the run
      ;; that needed it is over, on every path out. The assertions below read
      ;; what the run recorded, not the engine.
      (%shutdown-engine runtime real-engine)
      (when (stringp expected-error)
        (unless signalled
          (error 'test-failure
                 :text (format nil "expected a failure containing ~s, the run succeeded"
                               expected-error))))

      (%assert-run-projections fixture agent script deltas calls)
      (when (integerp (ax:jget fixture "stop_after_deltas"))
        (expect (ax:json-false-p (ax:jget (ax::agent-core-state agent) "forward_active"))
                "early stop releases the active-run guard")
        (expect (eq :null (ax:jget (ax::agent-core-state agent) "active_client"))
                "early stop drops the run client"))
      (when (nth-value 1 (gethash "transcribe_responses" fixture))
        (expect-equal (length (scripted-ai-transcribe-requests script))
                      (length (ax:jget fixture "transcribe_responses")) "transcription request count"))
      (let ((expected (ax:jget fixture "expected_output")))
        (unless (eq expected :null)
          (assert-json-equal output expected "agent output")))
      (%fixture-expect fixture "expected_output_subset" output "agent output")
      (let ((expected (ax:jget fixture "expected_request_count")))
        (unless (eq expected :null)
          (expect-equal (length (scripted-ai-requests script)) expected "model request count")))
      (let ((expected (ax:jget fixture "expected_request_contains")))
        (unless (eq expected :null)
          ;; Both views of the same calls: the wire body and Core's request
          ;; object. A fixture pins things that live in only one of them --
          ;; the rendered prompt reaches the wire, provider_metadata does not.
          (let ((all (with-output-to-string (out)
                       (loop for request across (scripted-ai-requests script)
                             do (write-string request out) (terpri out))
                       (loop for request across (scripted-ai-core-requests script)
                             do (write-string request out) (terpri out)))))
            (loop for needle across (if (ax::%array-p expected) expected (ax::%new-array))
                  do (expect-contains all (core::core-js-text needle)
                                      "the model requests carry their expected text")))))
      (let ((expected (ax:jget fixture "expected_control_events")))
        (unless (or (eq expected :null) (null control))
          (assert-json-equal
           (map 'vector (lambda (event) (ax:object "type" (ax:jget event "type")
                                                  "path" (ax:jget event "path")))
                (ax::run-control-events control)) expected
                             "run control lifecycle events")))
      (when (null agent) (return-from run-agent-forward-fixture t))
      (%expect-list-subset fixture "expected_chat_log_subset" (ax::agent-chat-log agent)
                           "agent chat log")
      (%expect-list-subset fixture "expected_action_log_subset" (ax::agent-action-log agent)
                           "agent action log")
      (%expect-list-subset fixture "expected_function_call_traces_subset"
                           (ax:jget (ax::agent-core-state agent) "function_call_traces" #())
                           "agent callable traces")
      (let ((expected (ax:jget fixture "expected_executed")))
        (unless (eq expected :null)
          (expect (typep runtime 'scripted-runtime) "executed-code assertion needs a recorded runtime")
          (assert-json-equal (scripted-runtime-executed runtime) expected "executed code")))
      (let ((state (ax::agent-export-runtime-state agent)))
        (%fixture-expect fixture "expected_exported_state_subset" state "exported runtime state"))
      (let ((expected (ax:jget fixture "expected_observer_transcript")))
        (unless (eq expected :null)
          (assert-json-equal transcript expected "agent observer transcript")))
      (%assert-mcp-calls fixture mcp-transports)
      (assert-agent-trace agent fixture)
      t)))


;;; ------------------------------------------------------------------
;;; agent_playbook_evolve
;;; ------------------------------------------------------------------
;;;
;;; One evolve run per case, each on a fresh agent, playbook, client and
;;; runtime, because an evolve call resets to the seed: two calls on one
;;; playbook would both start from the seed rather than accumulate, so a
;;; before-and-after comparison only means anything within a single call.

(defun %evolve-script (responses)
  "RESPONSES as the script one case plays.

Several responses play in order; a single one repeats, because a case with
one scripted answer means every request gets it."
  (let ((items (if (ax::%array-p responses) responses (ax::%new-array)))
        (out (ax::%new-array)))
    (if (> (length items) 1)
        (loop for item across items do (vector-push-extend item out))
        (let ((only (if (plusp (length items)) (aref items 0) (ax:object))))
          (dotimes (index 32) (declare (ignorable index))
            (vector-push-extend only out))))
    out))

(defun %teacher-messages (script role)
  "Each request's message of ROLE, in call order."
  (let ((out (ax::%new-array)))
    (loop for body across (scripted-ai-requests script)
          do (let ((messages (ax:jget (jparse body) "messages")))
               (loop for message across (if (ax::%array-p messages) messages (ax::%new-array))
                     do (when (equal (ax:jget message "role") role)
                          (vector-push-extend (ax:jget message "content") out)))))
    out))

(defun run-agent-playbook-evolve-fixture (fixture)
  "Evolve a playbook once per case and check what the run produced.

The teacher assertions are byte-exact on the prompts the Reflector and
Curator were actually sent, so a reshaped teacher signature shows up here
rather than passing quietly."
  (let* ((language (let ((given (ax:jget fixture "runtime_language" "Python")))
                     (if (eq given :null) "Python" given)))
         (runtime-on-evolve (ax:json-true-p (ax:jget fixture "runtime_on_evolve")))
         (teacher-spec (ax:jget fixture "teacher_client")))
    (loop for case across (let ((cases (ax:jget fixture "cases")))
                            (if (ax::%array-p cases) cases (ax::%new-array)))
          do (multiple-value-bind (client script)
                 (%scripted-ai-client (%evolve-script (ax:jget fixture "responses")))
               (multiple-value-bind (teacher teacher-script)
                   (if (eq teacher-spec :null)
                       (values client script)
                       (%scripted-provider-client
                        teacher-spec
                        (%evolve-script (let ((responses (ax:jget fixture "teacher_responses")))
                                          (if (ax::%array-p responses)
                                              responses
                                              (ax:jget fixture "responses"))))))
                 (let* ((runtime (make-scripted-runtime
                                  :script (let ((steps (ax:jget fixture "runtime_script")))
                                            (if (ax::%array-p steps) (coerce steps 'list) '()))
                                  :language language))
                        (agent-options (core::core-map-merge (%fixture-object fixture "options")
                                                             (ax:object))))
                   ;; runtime_on_evolve: the agent holds only a descriptor and
                   ;; the real runtime arrives on the evolve call.
                   (ax::%set-key agent-options "runtime"
                                 (if runtime-on-evolve (ax:object "language" language) runtime))
                   (let* ((agent (ax::agent (ax:jget fixture "signature"
                                                     "question:string -> answer:string")
                                            :options agent-options))
                          (playbook-options
                            (core::core-map-merge
                             (ax:object "target" "responder" "maxEpochs" 1)
                             (%object-or-empty (ax:jget case "playbook_options"))))
                          (handle (ax::agent-playbook agent :options playbook-options
                                                            :client client :teacher teacher))
                          (seed (ax:jget fixture "seed")))
                     ;; The fixture's seed is a whole snapshot, {playbook,
                     ;; artifact}, and the loader takes it as it stands. Wrapping
                     ;; it would bury the snapshot a level down and restore a
                     ;; playbook with no feedback history behind it.
                     (when (ax::%object-p seed)
                       (ax::playbook-load handle seed))
                     (let* ((before (ax::playbook-json handle))
                            (evolve-options (core::core-map-merge
                                             (%object-or-empty (ax:jget case "options"))
                                             (ax:object))))
                       (unless (eq teacher-spec :null)
                         (ax::%set-key evolve-options "teacherAI" teacher))
                       (when runtime-on-evolve
                         (ax::%set-key evolve-options "runtime" runtime))
                       ;; The agent round, not ACE compile: a task carries only
                       ;; what the agent did with it, so the measured round is a
                       ;; different surface from evolving over labelled answers.
                       (let* ((actual (ax::playbook-evolve-agent
                                       handle
                                       (%object-or-empty (ax:jget fixture "dataset"))
                                       :options evolve-options))
                              (outcomes (%core-array (ax:jget actual "outcomes")))
                              (expected (%object-or-empty (ax:jget case "expected")))
                              (label (core::core-js-text (ax:jget case "name" "case"))))
                         (let ((count (ax:jget expected "outcome_count")))
                           (unless (eq count :null)
                             (expect-equal (length outcomes) count
                                           (format nil "playbook evolve ~a outcome count" label))))
                         (let ((count (ax:jget case "expected_teacher_request_count")))
                           (unless (eq count :null)
                             ;; Name the roles that actually ran: a bare count
                             ;; says the teacher was asked too often but not
                             ;; which stage asked, and the first line of each
                             ;; system prompt identifies the role.
                             (expect-equal (length (scripted-ai-requests teacher-script)) count
                                           (format nil "playbook evolve ~a teacher request count (roles asked: ~{~s~^, ~})"
                                                   label
                                                   (map 'list (lambda (prompt)
                                                                (let* ((text (core::core-js-text prompt))
                                                                       ;; Every teacher prompt opens with the
                                                                       ;; same identity header, so the role is
                                                                       ;; whatever follows it.
                                                                       (start (let ((break (position #\Newline text)))
                                                                                (if break (1+ break) 0)))
                                                                       (rest (string-trim '(#\Space #\Newline)
                                                                                          (subseq text start))))
                                                                  (subseq rest 0 (min 80 (length rest)))))
                                                        (%teacher-messages teacher-script "system"))))))
                         (let ((prompts (ax:jget case "expected_teacher_system_prompts")))
                           (unless (eq prompts :null)
                             (assert-json-equal (%teacher-messages teacher-script "system") prompts
                                                (format nil "playbook evolve ~a teacher system prompts"
                                                        label))))
                         (let ((messages (ax:jget case "expected_teacher_user_messages")))
                           (unless (eq messages :null)
                             (assert-json-equal (%teacher-messages teacher-script "user") messages
                                                (format nil "playbook evolve ~a teacher user messages"
                                                        label))))
                         (if (zerop (length outcomes))
                             (unless (eql (ax:jget expected "outcome_count") 0)
                               (error 'test-failure
                                      :text (format nil "playbook evolve ~a produced no outcome: ~a"
                                                    label (ax:encode-json actual))))
                             (let ((outcome (aref outcomes 0)))
                               (let ((accepted (ax:jget expected "accepted")))
                                 (unless (eq accepted :null)
                                   (assert-json-equal (ax:jget outcome "accepted") accepted
                                                      (format nil "playbook evolve ~a accepted" label))))
                               (let ((calls (ax:jget expected "metricCallsUsed")))
                                 (unless (eq calls :null)
                                   (assert-json-equal (ax:jget actual "metricCallsUsed") calls
                                                      (format nil "playbook evolve ~a metric calls"
                                                              label))))
                               (let ((held (ax:jget expected "heldIn")))
                                 (unless (eq held :null)
                                   (assert-json-subset (%object-or-empty (ax:jget outcome "heldIn"))
                                                       held
                                                       (format nil "playbook evolve ~a held-in" label))))
                               (let ((reason (ax:jget expected "reason_contains")))
                                 (unless (eq reason :null)
                                   (expect-contains (core::core-js-text (ax:jget outcome "reason" ""))
                                                    (core::core-js-text reason)
                                                    (format nil "playbook evolve ~a reason" label))))
                               (when (ax:json-true-p (ax:jget case "expected_rollback"))
                                 (expect-equal (ax::playbook-json handle) before
                                               (format nil "playbook evolve ~a byte-exact rollback"
                                                       label)))
                               (let ((state (ax:jget case "expected_exported_state_subset")))
                                 (unless (eq state :null)
                                   ;; The snapshot, not the bare playbook: a
                                   ;; rollback that restored the rules but kept
                                   ;; the rejected rule's feedback would look
                                   ;; byte-exact against the playbook alone.
                                   (assert-json-subset (ax::playbook-state handle) state
                                                       (format nil "playbook evolve ~a exported state"
                                                               label))))))))))))))
  t)

(defun %core-array (value)
  (if (ax::%array-p value) value (ax::%new-array)))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun run-agent-conformance (&key (verbose nil))
  "Run the shared AxAgent fixtures.

Returns (values passed failed blocked partial).

A fixture whose kind this port runs must pass. A fixture whose kind is
listed as blocked is counted as blocked, never as a pass. A fixture that
ran but could not be checked in full is counted as partial, also never as
a pass. A fixture whose kind appears in neither table is a failure, so a
new kind cannot be ignored by omission.

All four counts are returned on every path, including the early exit when
no fixtures were found, so a gate can reject an incomplete run without
having to tell an empty result from a clean one."
  (let ((passed 0) (failed 0) (blocked 0) (partial 0)
        (blocked-kinds (make-hash-table :test #'equal))
        (files (%fixture-files)))
    (when (null files)
      (format t "~&agent conformance: no fixtures found under ~a~%" (%conformance-directory))
      ;; One failure, so an empty tree can never read as a clean run.
      (return-from run-agent-conformance (values 0 1 0 0)))
    (dolist (path files)
      (let* ((fixture (%read-fixture path))
             (kind (core::core-js-text (ax:jget fixture "kind" "")))
             (name (core::core-js-text (ax:jget fixture "name" (pathname-name path))))
             (runner (cdr (assoc kind +fixture-runners+ :test #'string=)))
             (blocker (cdr (assoc kind +fixture-blocked+ :test #'string=))))
        (cond
          (runner
           (let ((partial-reason nil))
             (handler-case
                 (handler-bind ((partial-fixture
                                  (lambda (condition)
                                    (setf partial-reason (partial-fixture-reason condition)))))
                   (funcall runner fixture)
                   (if partial-reason
                       (progn (incf partial)
                              (format t "~&part ~a (~a): ~a~%" name kind partial-reason))
                       (progn (incf passed)
                              ;; Only a fixture that ran and checked out in
                              ;; full is reported. A partial, a blocked kind
                              ;; and a failure all reach here by other
                              ;; branches, and reporting one of those would
                              ;; tell the gate a fixture was covered when its
                              ;; own reason says it was not.
                              (%report-pass path)
                              (when verbose (format t "~&ok   ~a (~a)~%" name kind)))))
               (error (condition)
                 (incf failed)
                 (let ((detail (princ-to-string condition)))
                   (format t "~&FAIL ~a (~a)~%     ~a~a~%" name kind
                           (subseq detail 0 (min 1600 (length detail)))
                           (if (> (length detail) 1600) " [detail truncated]" "")))))))
          (blocker
           (incf blocked)
           (incf (gethash kind blocked-kinds 0)))
          (t
           (incf failed)
           (format t "~&FAIL ~a: fixture kind ~s is in neither the runner nor the blocked table~%"
                   name kind)))))
    (format t "~&agent conformance: ~a passed, ~a partial, ~a failed, ~a blocked on other workers~%"
            passed partial failed blocked)
    (let ((kinds (sort (loop for kind being the hash-keys of blocked-kinds collect kind) #'string<)))
      (dolist (kind kinds)
        (format t "~&     blocked ~a x~a: ~a~%"
                kind (gethash kind blocked-kinds)
                (cdr (assoc kind +fixture-blocked+ :test #'string=)))))
    (values passed failed blocked partial)))
