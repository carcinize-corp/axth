;;;; event-conformance.lisp --- the shared ir/conformance/axevent fixtures,
;;;; plus the native lifecycle behaviour those fixtures imply.
;;;;
;;;; WHAT THIS DOES AND DOES NOT ESTABLISH, stated before any number:
;;;;
;;;; The seven axevent fixtures cover Core's routing, retry, continuation
;;;; matching, MCP normalization and input mapping, plus one `lifecycle'
;;;; fixture whose recorded data is small. Passing them establishes the
;;;; Core-owned `axevent.single-worker' contract: volatile storage, no
;;;; worker threads, no implicit wake. It establishes NOTHING about the
;;;; TypeScript runtime's persistent multi-worker contract: no leader
;;;; election, no cross-process lease, no durable queue and no store
;;;; conformance marker is exercised by these fixtures. Seven fixtures are
;;;; not parity with that runtime and this file does not claim it.
;;;;
;;;; The `lifecycle' fixture records only one envelope, one route and a
;;;; cancellation budget, so running it alone would prove almost nothing.
;;;; The native lifecycle checks below therefore go past the recorded data
;;;; into the transitions the single-worker contract actually promises:
;;;; cancellation promptness and subscription cleanup on both clocks,
;;;; delayed retry with backoff, strict per-instance ordering while a retry
;;;; waits, debounce coalescing to the latest value, queue backpressure,
;;;; program state capture and restore, cooperative cancellation discarding
;;;; output, isolated sink redrive, continuation resume, signature-aware
;;;; mapping with dead-lettering, and MCP source composition with logical
;;;; resubscription. Each uses a manual clock, so none of them sleeps.

(in-package #:axllm)

(export '(run-event-conformance-tests run-event-conformance-tests-or-die))

;;; ------------------------------------------------------------------
;;; Fixture operations
;;; ------------------------------------------------------------------

(defun %run-event-routing (fixture)
  (%assert-equal (axllm/core::event-route-commands
                  (jget fixture "event")
                  (%event-array (jget fixture "routes"))
                  (%mcp-text (jget fixture "identity_scope"))
                  (%mcp-text (jget fixture "trust")))
                 (jget fixture "expected") "event routing")
  :semantic)

(defun %run-event-retry (fixture)
  (loop for case across (%event-array (jget fixture "cases"))
        do (%assert-equal (axllm/core::event-retry-transition
                           (json-boolean (axllm/core::core-true-p
                                          (jget case "invocation_started")))
                           (%mcp-text (jget case "retry_safety"))
                           (jget case "attempt") (jget case "max_attempts"))
                          (jget case "expected") "event retry"))
  :semantic)

(defun %run-event-continuation (fixture)
  (let* ((key (%mcp-object-or-empty (jget fixture "correlation")))
         (actual (axllm/core::event-continuation-match
                  (%event-array (jget fixture "continuations"))
                  (%mcp-text (jget fixture "identity_scope"))
                  (%mcp-text (jget key "kind"))
                  (%mcp-text (jget key "value"))
                  (jget fixture "now"))))
    (%assert-equal (if (eq actual :null) :null (jget actual "id"))
                   (jget fixture "expected_id") "event continuation"))
  :semantic)

(defun %run-event-mcp-normalization (fixture)
  (%assert-equal (event-normalize-mcp (%mcp-text (jget fixture "namespace"))
                                      (%mcp-text (jget fixture "method"))
                                      (jget fixture "params"))
                 (jget fixture "expected") "event MCP normalization")
  :semantic)

(defun %run-event-mapping (fixture)
  (%assert-equal (axllm/core::event-map-input
                  (jget fixture "ingress") (jget fixture "plan")
                  (%event-array (jget fixture "signature_fields")) :null)
                 (jget fixture "expected") "event input mapping")
  :semantic)

;;; ------------------------------------------------------------------
;;; Lifecycle
;;; ------------------------------------------------------------------

(defun %event-envelope-for (id type data &optional correlation)
  (make-event-envelope id "test://axevent" type :data data
                                                :correlation (or correlation (%new-array))))

(defun %run-event-lifecycle (fixture)
  (%lifecycle-recorded-dispatch fixture)
  (%lifecycle-cancellation fixture)
  (%lifecycle-retry-and-ordering)
  (%lifecycle-debounce-and-capacity)
  (%lifecycle-state-and-cancel)
  (%lifecycle-sink-redrive)
  (%lifecycle-continuation)
  (%lifecycle-mapping)
  (%lifecycle-mcp-source)
  :semantic)

(defun %lifecycle-recorded-dispatch (fixture)
  "The fixture's own envelope, route and expected output."
  (let* ((route (%mcp-object-or-empty (jget fixture "route")))
         (source (make-push-event-source "fixture"))
         (target (event-target (%mcp-text (jget route "targetId"))
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (object "handled" (jget value "message")))))
         (runtime (make-event-runtime
                   (list (event-route (%mcp-text (jget route "id"))
                                      :action (%mcp-text (jget route "action"))
                                      :types (coerce (%event-array
                                                      (jget (%mcp-object-or-empty (jget route "match"))
                                                            "types"))
                                                     'list)
                                      :target (%mcp-text (jget route "targetId"))))
                   :targets (list target) :sources (list source))))
    (event-runtime-start runtime)
    (let* ((envelope (%event-envelope-of (jget fixture "event")))
           (receipt (push-event-source-publish
                     source envelope
                     :identity-scope (%mcp-text (jget fixture "identity_scope"))
                     :trust (%mcp-text (jget fixture "trust"))))
           (run (event-runtime-get-run
                 runtime (format nil "run:~a:~a:1" (%mcp-text (jget route "id"))
                                 (%mcp-text (jget (jget fixture "event") "id"))))))
      (%assert-equal (jget receipt "accepted") true "publish receipt accepted")
      (%assert-equal (jget receipt "durability") "volatile" "publish receipt durability")
      (unless run (%fixture-fail "no run was recorded for the lifecycle envelope"))
      (%assert-equal (event-run-output run) (jget fixture "expected_output")
                     "inline dispatch output")
      (%assert-equal (event-run-status run) "succeeded" "inline dispatch status")
      ;; The descriptor is the contract this package claims, in Core's words.
      (%assert-equal (jget (event-runtime-descriptor runtime) "coordination")
                     "single-worker" "runtime coordination")
      (%assert-equal (jget (event-runtime-descriptor runtime) "durability")
                     "volatile" "runtime durability")
      (%assert-equal (jget (event-runtime-descriptor runtime) "implicitWake")
                     false "runtime implicit wake"))
    (event-runtime-close runtime)))

(defun %lifecycle-cancellation (fixture)
  "Cancellation is one-shot, prompt on both clocks, and leaves nothing behind."
  (let* ((spec (%mcp-object-or-empty (jget fixture "cancellation")))
         (reason (%mcp-text (jget spec "reason")))
         (seconds (/ (jget spec "sleep_ms") 1000d0))
         (budget (jget spec "max_elapsed_ms"))
         (token (make-cancellation-token))
         (removed 0))
    (let ((remove (cancellation-token-subscribe token (lambda () (incf removed)))))
      (funcall remove))
    (unless (cancellation-token-cancel token reason)
      (%fixture-fail "the first cancellation did not report success"))
    (when (cancellation-token-cancel token "ignored")
      (%fixture-fail "a second cancellation reported success"))
    (%assert-equal (cancellation-token-reason token) reason "cancellation keeps its first reason")
    (unless (zerop removed)
      (%fixture-fail "a removed cancellation subscriber was still called"))
    (dolist (clock (list (make-system-event-clock) (make-manual-event-clock)))
      (let* ((sleep-token (make-cancellation-token))
             (result '())
             (started (get-internal-real-time))
             (worker (sb-thread:make-thread
                      (lambda () (push (event-clock-sleep clock seconds sleep-token) result))
                      :name "ax-event-sleep")))
        (if (typep clock 'manual-event-clock)
            (manual-clock-wait-for-sleepers clock)
            (sleep 0.02))
        (cancellation-token-cancel sleep-token reason)
        (sb-thread:join-thread worker :default nil :timeout 2)
        (let ((elapsed (* 1000 (/ (- (get-internal-real-time) started)
                                  internal-time-units-per-second))))
          (when (sb-thread:thread-alive-p worker)
            (%fixture-fail "~a: a cancelled sleep did not return" (type-of clock)))
          ;; EVENT-CLOCK-SLEEP answers with a Lisp boolean, not a JSON one.
          (unless (equal result '(nil))
            (%fixture-fail "~a: a cancelled sleep returned ~s, expected (NIL)"
                           (type-of clock) result))
          ;; The sleep was 30 seconds; returning inside the budget is the proof
          ;; that cancellation woke it rather than the clock expiring.
          (when (> elapsed budget)
            (%fixture-fail "~a: cancellation took ~,1fms, budget is ~ams"
                           (type-of clock) elapsed budget))
          (%assert-equal (cancellation-token-subscription-count sleep-token) 0
                         (format nil "~a cancelled sleep cleanup" (type-of clock))))))
    ;; A sleep that completes normally must also deregister its waker.
    (let* ((clock (make-manual-event-clock))
           (token (make-cancellation-token))
           (result '())
           (worker (sb-thread:make-thread
                    (lambda () (push (event-clock-sleep clock 0.001 token) result)))))
      (manual-clock-wait-for-sleepers clock)
      (manual-clock-advance clock 1)
      (sb-thread:join-thread worker :default nil :timeout 2)
      (unless (equal result '(t))
        (%fixture-fail "a completed manual sleep returned ~s, expected (T)" result))
      (%assert-equal (cancellation-token-subscription-count token) 0
                     "manual clock successful sleep cleanup"))))

(defun %lifecycle-retry-and-ordering ()
  "A failed idempotent invocation retries after a backoff, and strict
ordering holds the next delivery for that instance until it does."
  (let* ((calls 0)
         (target (event-target "retry-target"
                               :invoke (lambda (value context)
                                         (declare (ignore value context))
                                         (incf calls)
                                         (when (= calls 1) (error "retry once"))
                                         (object "attempt" calls))
                               :retry-safety "idempotent"))
         (clock (make-manual-event-clock 1000))
         (runtime (make-event-runtime
                   (list (event-route "retry-route" :action "wake" :types '("event.retry")
                                                    :target "retry-target"))
                   :targets (list target) :clock clock :retry-backoff-ms 500)))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "retry-1" "event.retry" (object)))
    (let ((run (event-runtime-get-run runtime "run:retry-route:retry-1:1")))
      (%assert-equal (vector (event-run-attempt run) (event-run-status run)
                             (event-runtime-next-due-at runtime))
                     (vector 1 "queued" 1500) "delayed retry state")
      ;; The host, not the runtime, decides when to come back.
      (manual-clock-advance clock 500)
      (event-runtime-run-due runtime)
      (%assert-equal (vector (event-run-attempt run) (event-run-status run))
                     (vector 2 "succeeded") "retry dispatch")
      (%assert-equal (event-run-output run) (object "attempt" 2) "retry output")))
  (let* ((seen (%new-array))
         (target (event-target "strict-target"
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (vector-push-extend (jget value "name") seen)
                                         (when (and (equal (jget value "name") "first")
                                                    (= 1 (count "first" (coerce seen 'list)
                                                                :test #'equal)))
                                           (error "retry first"))
                                         value)
                               :retry-safety "idempotent"))
         (clock (make-manual-event-clock 1000))
         (runtime (make-event-runtime
                   (list (event-route "strict-route" :action "wake" :types '("event.strict")
                                                     :target "strict-target"))
                   :targets (list target) :clock clock :retry-backoff-ms 500)))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "strict-1" "event.strict"
                                                        (object "name" "first")))
    (event-runtime-publish runtime (%event-envelope-for "strict-2" "event.strict"
                                                        (object "name" "second")))
    ;; "second" must wait: it shares the instance whose retry is pending.
    (%assert-equal seen (vector "first") "strict ordering while a retry waits")
    (manual-clock-advance clock 500)
    (event-runtime-run-due runtime)
    (%assert-equal seen (vector "first" "first" "second") "strict retry release ordering")))

(defun %lifecycle-debounce-and-capacity ()
  "Debounce coalesces to the latest value, and a full inbox times out."
  (let* ((values (%new-array))
         (target (event-target "debounce-target"
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (vector-push-extend value values)
                                         value)))
         (clock (make-manual-event-clock 2000))
         (runtime (make-event-runtime
                   (list (event-route "debounce-route" :action "wake"
                                                       :types '("event.debounce")
                                                       :target "debounce-target"
                                                       :debounce-ms 250))
                   :targets (list target) :clock clock)))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "debounce-1" "event.debounce"
                                                        (object "revision" 1)))
    (event-runtime-publish runtime (%event-envelope-for "debounce-2" "event.debounce"
                                                        (object "revision" 2)))
    (%assert-equal (vector (length values) (event-runtime-next-due-at runtime))
                   (vector 0 2250) "debounce scheduling")
    (manual-clock-advance clock 250)
    (event-runtime-run-due runtime)
    ;; Revision 1 is gone, not queued behind revision 2.
    (%assert-equal values (vector (object "revision" 2)) "latest-value coalescing"))
  (let* ((clock (make-manual-event-clock 3000))
         (target (event-target "capacity-target"
                               :invoke (lambda (value context)
                                         (declare (ignore context)) value)))
         (runtime (make-event-runtime
                   (list (event-route "capacity-route" :action "wake"
                                                       :types '("event.capacity")
                                                       :target "capacity-target"
                                                       :debounce-ms 1000
                                                       :instance-key (event-path-data "key")))
                   :targets (list target) :clock clock
                   :max-pending 1 :publish-timeout-ms 100))
         (errors (%new-array)))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "capacity-1" "event.capacity"
                                                        (object "key" "one")))
    (let ((worker (sb-thread:make-thread
                   (lambda ()
                     (handler-case
                         (event-runtime-publish runtime
                                                (%event-envelope-for "capacity-2" "event.capacity"
                                                                     (object "key" "two")))
                       (error (condition) (vector-push-extend condition errors)))))))
      (manual-clock-wait-for-sleepers clock)
      (manual-clock-advance clock 100)
      (sb-thread:join-thread worker :default nil :timeout 2)
      (%assert-equal (length errors) 1 "backpressure error count")
      (unless (search "Backpressure" (princ-to-string (aref errors 0)))
        (%fixture-fail "a full inbox did not report backpressure: ~a" (aref errors 0))))))

(defun %lifecycle-state-and-cancel ()
  "Program state survives between runs, and a cancelled run keeps no output."
  (let* ((state 0)
         (target (event-target "state-target"
                               :invoke (lambda (value context)
                                         (declare (ignore value context))
                                         (incf state)
                                         (object "state" state))
                               :retry-safety "idempotent"
                               :capture-state (lambda () state)
                               :restore-state (lambda (value) (setf state value))))
         (runtime (make-event-runtime
                   (list (event-route "state-route" :action "wake" :types '("event.state")
                                                    :target "state-target"))
                   :targets (list target))))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "state-1" "event.state" (object)))
    ;; Clobber the live variable: only a restore can produce 2 on the next run.
    (setf state 0)
    (event-runtime-publish runtime (%event-envelope-for "state-2" "event.state" (object)))
    (%assert-equal (event-run-output (event-runtime-get-run runtime "run:state-route:state-2:2"))
                   (object "state" 2) "program state restore"))
  (let* ((target (event-target "cancel-target"
                               :invoke (lambda (value context)
                                         (declare (ignore value))
                                         (cancellation-token-cancel (jget context "cancellation")
                                                                    "fixture")
                                         (object "should" "not persist"))))
         (runtime (make-event-runtime
                   (list (event-route "cancel-route" :action "wake" :types '("event.cancel")
                                                     :target "cancel-target"))
                   :targets (list target))))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "cancel-1" "event.cancel" (object)))
    (let ((run (event-runtime-get-run runtime "run:cancel-route:cancel-1:1")))
      (%assert-equal (event-run-status run) "cancelled" "cooperative cancellation status")
      (%assert-equal (event-run-output run) :null "a cancelled run persisted output"))))

(defun %lifecycle-sink-redrive ()
  "A sink failure dead-letters the sink alone; redrive must not re-invoke."
  (let* ((sink-calls 0)
         (model-calls 0)
         (sink (make-event-sink
                :id "fixture-sink"
                :write (lambda (output context)
                         (incf sink-calls)
                         (when (= sink-calls 1) (error "sink once"))
                         (unless (%same (event-run-output (jget context "run")) output)
                           (error "output was not persisted before the sink ran")))))
         (target (event-target "sink-target"
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (incf model-calls) value)
                               :sinks (list sink)
                               :retry-safety "idempotent"))
         (runtime (make-event-runtime
                   (list (event-route "sink-route" :action "wake" :types '("event.sink")
                                                   :target "sink-target"))
                   :targets (list target))))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "sink-1" "event.sink" (object "ok" true)))
    (let ((dead (first (event-runtime-list-dead-letters runtime))))
      (unless dead (%fixture-fail "a failing sink did not dead-letter"))
      (%assert-equal (event-dead-letter-sink-id dead) "fixture-sink" "sink dead letter id")
      (event-runtime-redrive runtime (event-dead-letter-id dead)))
    (%assert-equal (vector model-calls sink-calls
                           (length (event-runtime-list-dead-letters runtime)))
                   (vector 1 2 0) "sink-only redrive")))

(defun %lifecycle-continuation ()
  "A declared wait registers a continuation that a resume route matches."
  (let* ((calls 0)
         (target (event-target "continuation-target"
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (incf calls) value)
                               :wait-for (list (list "job" (event-path-data "job")))))
         (runtime (make-event-runtime
                   (list (event-route "continuation-wake" :action "wake"
                                                          :types '("event.continuation.start")
                                                          :target "continuation-target")
                         (event-route "continuation-resume" :action "resume"
                                                            :types '("event.continuation.done")))
                   :targets (list target))))
    (event-runtime-start runtime)
    (event-runtime-publish runtime
                           (%event-envelope-for "continuation-1" "event.continuation.start"
                                                (object "job" "job-1"))
                           :identity-scope "tenant:test" :trust "authenticated")
    (event-runtime-publish runtime
                           (%event-envelope-for "continuation-2" "event.continuation.done"
                                                (object "job" "job-1")
                                                (vector (object "kind" "job" "value" "job-1")))
                           :identity-scope "tenant:test" :trust "authenticated")
    (%assert-equal calls 2 "continuation resume invoked the owning target")
    ;; Another tenant's correlation key must not resume this continuation.
    (event-runtime-publish runtime
                           (%event-envelope-for "continuation-3" "event.continuation.done"
                                                (object "job" "job-1")
                                                (vector (object "kind" "job" "value" "job-1")))
                           :identity-scope "tenant:other" :trust "authenticated")
    (%assert-equal calls 2 "a foreign identity scope resumed a continuation")
    (unless (find "continuation_not_found" (event-runtime-list-dead-letters runtime)
                  :key #'event-dead-letter-reason :test #'equal)
      (%fixture-fail "an unmatched resume did not dead-letter"))))

(defun %lifecycle-mapping ()
  "Declarative and callback mappings both normalize to the signature."
  (let* ((inputs (%new-array))
         (signature (parse-signature "url:string, revision:number -> ok:boolean"))
         (target (event-target "mapped-target"
                               :signature signature
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (vector-push-extend value inputs) value)
                               :wake-input (event-input-plan
                                            :fields (list (cons "url" (event-path-data "uri"))
                                                          (cons "revision"
                                                                (event-path-data "revision"))))))
         (runtime (make-event-runtime
                   (list (event-route "mapped-route" :action "wake" :types '("event.mapped")
                                                     :target "mapped-target"
                                                     :instance-key (event-path-data "uri")))
                   :targets (list target))))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "mapped-1" "event.mapped"
                                                        (object "uri" "demo://one" "revision" 2)))
    ;; A revision of the wrong type must dead-letter before invocation.
    (event-runtime-publish runtime (%event-envelope-for "mapped-2" "event.mapped"
                                                        (object "uri" "demo://two"
                                                                "revision" "bad")))
    (%assert-equal inputs (vector (object "url" "demo://one" "revision" 2))
                   "signature-aware event mapping")
    (%assert-equal (length (event-runtime-list-dead-letters runtime)) 1
                   "invalid mapped input dead letter count"))
  (let* ((inputs (%new-array))
         (signature (parse-signature "url:string -> ok:boolean"))
         (target (event-target "callback-target"
                               :signature signature
                               :invoke (lambda (value context)
                                         (declare (ignore context))
                                         (vector-push-extend value inputs) value)
                               :map-input (lambda (event continuation)
                                            (declare (ignore continuation))
                                            (object "url" (jget (event-envelope-data event) "uri")
                                                    "secret" "drop-me"))))
         (runtime (make-event-runtime
                   (list (event-route "callback-route" :action "wake" :types '("event.callback")
                                                       :target "callback-target"))
                   :targets (list target))))
    (event-runtime-start runtime)
    (event-runtime-publish runtime (%event-envelope-for "callback-1" "event.callback"
                                                        (object "uri" "demo://callback")))
    ;; The callback is an escape hatch, not a validation bypass: "secret" is
    ;; not a signature input and must not reach the program.
    (%assert-equal inputs (vector (object "url" "demo://callback"))
                   "callback signature normalization")))

(defun %lifecycle-mcp-source ()
  "An MCP source composes with the runtime and owns its subscriptions."
  (let* ((transport (make-mcp-scripted-transport
                     (vector (object "method" "initialize"
                                     "result" (object "protocolVersion" "2025-11-25"
                                                      "capabilities"
                                                      (object "resources"
                                                              (object "subscribe" true)))))))
         (client (make-mcp-client transport :namespace "inventory" :era "legacy"))
         (lifecycle-calls 0)
         (invocations 0))
    (mcp-add-lifecycle-listener client (lambda (state) (declare (ignore state))
                                         (incf lifecycle-calls)))
    (let* ((target (event-target "mcp-target"
                                 :invoke (lambda (value context)
                                           (declare (ignore context))
                                           (incf invocations) value)))
           (source (make-mcp-event-source client :namespace "inventory"
                                                 :identity-scope "tenant:test"
                                                 :trust "authenticated"
                                                 :subscriptions '("demo://inventory")))
           (runtime (make-event-runtime
                     (list (event-route "mcp-wake" :action "wake"
                                                   :types '("mcp.resource.updated")
                                                   :target "mcp-target"
                                                   :authenticated t))
                     :targets (list target) :sources (list source))))
      (event-runtime-start runtime)
      (mcp-scripted-emit transport (object "jsonrpc" "2.0"
                                           "method" "notifications/resources/updated"
                                           "params" (object "uri" "demo://inventory")))
      (mcp-emit-lifecycle client "reconnected")
      (event-runtime-close runtime)
      ;; After close the source is detached: a further notification is inert.
      (mcp-scripted-emit transport (object "jsonrpc" "2.0"
                                           "method" "notifications/resources/updated"
                                           "params" (object "uri" "demo://inventory")))
      (%assert-equal (vector invocations lifecycle-calls
                             (count "resources/subscribe"
                                    (coerce (mcp-scripted-requests transport) 'list)
                                    :key (lambda (r) (%mcp-text (jget r "method")))
                                    :test #'string=))
                     (vector 1 1 2)
                     "MCP listener composition and logical resubscription")))
  ;; Ownership and selection are Core's; check the transitions a source relies on.
  (let ((ownership (axllm/core::mcp-resource-subscription-ownership
                    (%new-array) "source-a" "acquire")))
    (setf ownership (axllm/core::mcp-resource-subscription-ownership
                     (jget ownership "owners") "source-b" "acquire"))
    (setf ownership (axllm/core::mcp-resource-subscription-ownership
                     (jget ownership "owners") "source-a" "release"))
    (%assert-equal ownership (object "owners" (vector "source-b")
                                     "wireAction" "none" "changed" true)
                   "subscription ownership transition"))
  (let ((resources (vector (object "uri" "demo://b") (object "uri" "demo://a")
                           (object "uri" "demo://b") (object "uri" ""))))
    (%assert-equal (vector (axllm/core::mcp-resource-subscription-selection resources "all"
                                                                            (%new-array))
                           (axllm/core::mcp-resource-subscription-selection
                            (%new-array) "explicit" (vector "demo://x" "demo://y" "demo://x"))
                           (axllm/core::mcp-resource-subscription-selection
                            (vector (aref resources 1)) "selector" (%new-array)))
                   (vector (vector "demo://b" "demo://a")
                           (vector "demo://x" "demo://y")
                           (vector "demo://a"))
                   "subscription selection modes")))

;;; ------------------------------------------------------------------
;;; Dispatch
;;; ------------------------------------------------------------------

(defparameter +event-conformance-operations+
  '(("routing" . %run-event-routing)
    ("retry" . %run-event-retry)
    ("continuation" . %run-event-continuation)
    ("mcp_normalization" . %run-event-mcp-normalization)
    ("mapping" . %run-event-mapping)
    ("lifecycle" . %run-event-lifecycle))
  "Every axevent operation and the arm that runs it. No catch-all.")

(defun run-event-conformance-tests (&key (stream *standard-output*))
  "Run every ir/conformance/axevent fixture.

Returns (values passed failed coverage unclaimed). An unclaimed fixture is
not counted as passed."
  (let ((files (%fixture-files "axevent"))
        (passed 0) (failed 0) (unclaimed 0)
        (coverage (object)))
    (when (null files)
      (format stream "~&No axevent fixtures found under ~a~%"
              (mcp-conformance-directory "axevent"))
      (return-from run-event-conformance-tests (values 0 1 coverage 0)))
    (dolist (path files)
      (let* ((fixture (%read-fixture path))
             (name (%mcp-text (jget fixture "name" (pathname-name path))))
             (operation (%mcp-text (jget fixture "operation" "")))
             (arm (cdr (assoc operation +event-conformance-operations+ :test #'string=))))
        (handler-case
            (progn
              (unless arm
                (%fixture-fail "unsupported event conformance operation ~a" operation))
              (let ((classification (funcall arm fixture)))
                (if (eq classification :explicitly-not-claimed)
                    (progn (incf unclaimed)
                           (format stream "~&  NOT CLAIMED ~a (~a)~%" name (pathname-name path)))
                    (progn
                      ;; Disk name, successful dispatch only; see the same
                      ;; call in mcp-conformance.lisp.
                      (axllm/conformance:record-result "axevent" path classification)
                      (incf passed)))
                (%set-key coverage (pathname-name path)
                          (object "operation" operation
                                  "classification" (string-downcase (symbol-name classification))
                                  "note" :null))))
          (error (condition)
            (incf failed)
            (%set-key coverage (pathname-name path)
                      (object "operation" operation "classification" "failed"
                              "note" (princ-to-string condition)))
            (format stream "~&  FAIL ~a (~a)~%    ~a~%" name (pathname-name path) condition)))))
    (format stream "~&axevent: ~a passed, ~a failed, ~a not claimed (~a fixtures)~%"
            passed failed unclaimed (length files))
    (format stream "~&axevent: contract is axevent.single-worker (volatile, no worker threads, no implicit wake); persistent multi-worker parity is NOT claimed from these fixtures~%")
    (values passed failed coverage unclaimed)))

(defun run-event-conformance-tests-or-die ()
  "Run the axevent suite and signal unless every fixture was actually proved."
  (multiple-value-bind (passed failed coverage unclaimed) (run-event-conformance-tests)
    (declare (ignore passed coverage))
    (unless (zerop failed)
      (error "~a axevent conformance fixture(s) failed." failed))
    (unless (zerop unclaimed)
      (error "~a axevent conformance fixture(s) executed no implementation path; the axevent claim is incomplete."
             unclaimed))
    t))
