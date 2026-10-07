;;;; event-store-conformance.lisp --- what a store must prove before it may
;;;; claim multi-worker coordination.
;;;;
;;;; Core refuses a multi-worker claim without the
;;;; axevent.store-conformance.v1 marker. This suite is what earns that
;;;; marker, so it is deliberately adversarial: each check is one a plausible
;;;; wrong implementation fails.
;;;;
;;;; Everything here is local: one temporary SQLite file per run, two
;;;; connections to it to stand in for two workers, and a manual clock so
;;;; lease expiry is decided rather than waited for. No network, no fixtures
;;;; outside this file, and the database is deleted afterwards.
;;;;
;;;; WHAT THIS SUITE DOES NOT ESTABLISH: it proves the store is persistent
;;;; and that its lease coordination is correct under two workers. It does
;;;; not prove AxEventRuntime dispatches from this store, because the inline
;;;; runtime still reads and mutates in-memory delivery records. Until a
;;;; store interface is extracted in event.lisp, a green run here is a
;;;; store-level claim only, and RUN-EVENT-STORE-CONFORMANCE says so.

(in-package #:axllm)

(export '(run-event-store-conformance run-event-store-conformance-or-die))

(defun %event-store-temp-path (&optional (tag "store"))
  (merge-pathnames (format nil "ax-event-~a-~a.sqlite" tag (%mcp-uuid))
                   (uiop:temporary-directory)))

(defun %assert-flag (value expected label)
  "Compare Lisp generalized booleans.

%ASSERT-EQUAL is for JSON values and cannot render a Lisp T, so a failed
boolean assertion printed \"encode-json: T is not a JSON value\" instead of
the mismatch. Store predicates answer T or NIL, so they need this."
  (let ((got (and value t)) (want (and expected t)))
    (unless (eq got want)
      (%fixture-fail "~a mismatch~%    expected: ~a~%    actual:   ~a"
                     label (if want "true" "false") (if got "true" "false"))))
  t)

(defmacro %with-event-store ((store &rest options) path &body body)
  `(let ((,store (make-sqlite-event-store ,path ,@options)))
     (unwind-protect (progn ,@body) (event-store-close ,store))))

(defun %store-envelope (id type data)
  (make-event-envelope id "test://axevent" type :data data))

(defun %store-command (route-id event-id &key (action "wake") (target "t")
                                              (instance-key "instance-1"))
  (object "routeId" route-id "action" action "targetId" target
          "instanceKey" instance-key
          "idempotencyKey" (format nil "~a:~a" route-id event-id)))

(defun run-event-store-conformance (&key (stream *standard-output*))
  "Prove the SQLite store is persistent and coordinates two workers.

Returns (values passed failed)."
  (let ((passed 0) (failed 0)
        (path (%event-store-temp-path)))
    (flet ((check (name thunk)
             (handler-case (progn (funcall thunk) (incf passed))
               (error (condition)
                 (incf failed)
                 (format stream "~&  FAIL ~a~%    ~a~%" name condition)))))
      (unwind-protect
           (progn
             (check "Core accepts the store's claim only as presented"
                    (lambda ()
                      (%with-event-store (store) path
                        (let ((capability (event-store-capability store)))
                          (%assert-equal (jget capability "coordination") "multi-worker"
                                         "store coordination")
                          (%assert-equal (jget capability "durability") "persistent"
                                         "store durability")
                          (%assert-equal (jget capability "conformant") true
                                         "store conformance flag"))
                        ;; The same store without the marker must be refused.
                        (let* ((descriptor (event-store-descriptor store))
                               (stripped (axllm/core::core-map-merge descriptor (object))))
                          (remhash "conformanceMarker" stripped)
                          (let ((verdict (axllm/core::event-store-capability stripped)))
                            (%assert-equal (jget verdict "ok") false
                                           "unmarked multi-worker claim")
                            (%assert-equal (jget verdict "coordination") "single-worker"
                                           "unmarked claim downgraded"))))))

             (check "a volatile store may not claim multi-worker"
                    (lambda ()
                      (let ((verdict (axllm/core::event-store-capability
                                      (object "durability" "volatile"
                                              "coordination" "multi-worker"
                                              "conformanceMarker"
                                              +event-store-conformance-marker+
                                              "leaseMs" 1000))))
                        (%assert-equal (jget verdict "ok") false "volatile multi-worker claim")
                        (%assert-equal (jget verdict "coordination") "single-worker"
                                       "volatile claim downgraded"))))

             (check "a multi-worker claim needs a positive lease"
                    (lambda ()
                      (let ((verdict (axllm/core::event-store-capability
                                      (object "durability" "persistent"
                                              "coordination" "multi-worker"
                                              "conformanceMarker"
                                              +event-store-conformance-marker+
                                              "leaseMs" 0))))
                        (%assert-equal (jget verdict "ok") false "zero-lease claim"))))

             (check "deliveries survive closing and reopening the database"
                    (lambda ()
                      (let ((envelope (%store-envelope "persist-1" "event.persist"
                                                       (object "n" 1))))
                        (%with-event-store (store) path
                          (event-store-enqueue store envelope
                                               (list (%store-command "r1" "persist-1")))
                          (event-store-set-ingress store "r1:persist-1" "tenant:a" "authenticated"))
                        ;; A new connection to the same file must see it.
                        (%with-event-store (store) path
                          (let ((delivery (event-store-delivery store "r1:persist-1")))
                            (when (eq delivery :null)
                              (%fixture-fail "the delivery did not survive a reopen"))
                            (%assert-equal (jget delivery "identityScope") "tenant:a"
                                           "persisted identity scope")
                            (%assert-equal (jget delivery "trust") "authenticated"
                                           "persisted trust")
                            (%assert-equal (jget (jget delivery "event") "id") "persist-1"
                                           "persisted envelope")
                            (%assert-equal (jget delivery "status") "queued"
                                           "persisted status"))))))

             (check "a duplicate publication does not create a second delivery"
                    (lambda ()
                      (let ((envelope (%store-envelope "dup-1" "event.dup" (object))))
                        (%with-event-store (store) path
                          (let ((first (event-store-enqueue
                                        store envelope (list (%store-command "r2" "dup-1"))))
                                (second (event-store-enqueue
                                         store envelope (list (%store-command "r2" "dup-1")))))
                            (%assert-equal (length first) 1 "first publication admitted")
                            (%assert-equal (length second) 0 "duplicate publication admitted"))))))

             (check "a due delivery is visible and an undue one is not"
                    (lambda ()
                      (let ((clock (make-manual-event-clock 1000)))
                        (%with-event-store (store :clock clock) path
                          (event-store-enqueue store (%store-envelope "due-1" "event.due" (object))
                                               (list (%store-command "r3" "due-1"))
                                               :available-at 1500)
                          (let ((due (event-store-due-deliveries store)))
                            (when (find "r3:due-1" (coerce due 'list)
                                        :key (lambda (d) (%mcp-text (jget d "id")))
                                        :test #'string=)
                              (%fixture-fail "a delivery became due before its time")))
                          (manual-clock-advance clock 500)
                          (let ((due (event-store-due-deliveries store)))
                            (unless (find "r3:due-1" (coerce due 'list)
                                          :key (lambda (d) (%mcp-text (jget d "id")))
                                          :test #'string=)
                              (%fixture-fail "a due delivery was not returned")))))))

             (check "two workers cannot hold one instance lease at once"
                    (lambda ()
                      (let ((clock (make-manual-event-clock 5000)))
                        (%with-event-store (first :worker "worker-a" :lease-ms 1000 :clock clock)
                            path
                          (%with-event-store (second :worker "worker-b" :lease-ms 1000
                                                     :clock clock)
                              path
                            (let ((a (event-store-acquire-lease first "instance-1")))
                              (%assert-equal (jget a "action") "claim" "first lease action")
                              (%assert-equal (jget a "granted") true "first lease granted")
                              (%assert-equal (jget a "owner") "worker-a" "first lease owner"))
                            ;; The second worker must be refused while the lease is live.
                            (let ((b (event-store-acquire-lease second "instance-1")))
                              (%assert-equal (jget b "action") "deny" "second lease action")
                              (%assert-equal (jget b "granted") false "second lease granted")
                              (%assert-equal (jget b "owner") "worker-a" "denied lease owner"))
                            ;; The holder renews rather than re-claiming.
                            (let ((again (event-store-acquire-lease first "instance-1")))
                              (%assert-equal (jget again "action") "renew" "renewal action"))
                            ;; Once it expires the other worker may steal it.
                            (manual-clock-advance clock 2000)
                            (let ((stolen (event-store-acquire-lease second "instance-1")))
                              (%assert-equal (jget stolen "action") "steal" "steal action")
                              (%assert-equal (jget stolen "granted") true "steal granted")
                              (%assert-equal (jget stolen "previousOwner") "worker-a"
                                             "stolen from"))
                            ;; A release by a worker that no longer owns it is a no-op.
                            (when (event-store-release-lease first "instance-1")
                              (%fixture-fail "a non-owner released another worker's lease"))
                            (%assert-equal (jget (event-store-lease-holder second "instance-1")
                                                 "owner")
                                           "worker-b" "lease holder after a failed release")
                            (unless (event-store-release-lease second "instance-1")
                              (%fixture-fail "the owner could not release its own lease"))
                            (%assert-equal (event-store-lease-holder second "instance-1") :null
                                           "lease after release"))))))

             (check "a lease is visible to a separate connection immediately"
                    (lambda ()
                      (let ((clock (make-manual-event-clock 9000)))
                        (%with-event-store (writer :worker "worker-w" :lease-ms 5000 :clock clock)
                            path
                          (event-store-acquire-lease writer "instance-cross")
                          (%with-event-store (reader :worker "worker-r" :clock clock) path
                            (%assert-equal (jget (event-store-lease-holder reader
                                                                           "instance-cross")
                                                 "owner")
                                           "worker-w" "cross-connection lease visibility"))))))

             (check "runs, dead letters, continuations and state persist"
                    (lambda ()
                      (let ((run (make-instance 'event-run :id "run:x:1" :delivery-id "r4:x"
                                                           :route-id "r4" :target-id "t"
                                                           :instance-key "instance-1")))
                        (setf (event-run-status run) "succeeded"
                              (event-run-attempt run) 2
                              (event-run-output run) (object "ok" true))
                        (%with-event-store (store) path
                          (event-store-put-run store run)
                          (event-store-put-dead-letter
                           store (make-instance 'event-dead-letter :id "dead:1"
                                                                    :delivery-id "r4:x"
                                                                    :reason "sink once"
                                                                    :run-id "run:x:1"
                                                                    :sink-id "fixture-sink"))
                          (event-store-put-continuation
                           store (make-instance 'event-continuation
                                                :id "continuation:t:1" :target-id "t"
                                                :instance-key "instance-1"
                                                :identity-scope "tenant:a"
                                                :correlation (vector (object "kind" "job"
                                                                             "value" "job-1"))))
                          (event-store-put-program-state store "t\\ntenant:a\\ninstance-1"
                                                         (object "counter" 7)))
                        (%with-event-store (store) path
                          (let ((stored (event-store-run store "run:x:1")))
                            (%assert-equal (jget stored "status") "succeeded" "persisted run status")
                            (%assert-equal (jget stored "attempt") 2 "persisted run attempt")
                            (%assert-equal (jget stored "output") (object "ok" true)
                                           "persisted run output"))
                          (%assert-equal (event-store-dead-letter-ids store) (vector "dead:1")
                                         "persisted dead letters")
                          (let ((open (event-store-open-continuations store)))
                            (%assert-equal (length open) 1 "persisted open continuations")
                            (%assert-equal (jget (aref open 0) "identityScope") "tenant:a"
                                           "persisted continuation scope")
                            ;; Core must be able to match the persisted shape.
                            (let ((match (axllm/core::event-continuation-match
                                          open "tenant:a" "job" "job-1" 0)))
                              (when (eq match :null)
                                (%fixture-fail "Core could not match a persisted continuation"))))
                          (event-store-complete-continuation store "continuation:t:1")
                          (%assert-equal (length (event-store-open-continuations store)) 0
                                         "completed continuation still open")
                          (%assert-equal (event-store-program-state store
                                                                     "t\\ntenant:a\\ninstance-1")
                                         (object "counter" 7) "persisted program state")))))

             (check "AxEventRuntime dispatches from the persistent store"
                    (lambda ()
                      (let ((outputs (%new-array))
                            (runtime-path (%event-store-temp-path)))
                        (unwind-protect
                             (progn
                               (%with-event-store (store :worker "runtime-a") runtime-path
                                 (let* ((target (event-target
                                                 "durable-target"
                                                 :invoke (lambda (value context)
                                                           (declare (ignore context))
                                                           (vector-push-extend value outputs)
                                                           (object "handled"
                                                                   (jget value "message")))))
                                        (runtime (make-event-runtime
                                                  (list (event-route "durable-route"
                                                                     :action "wake"
                                                                     :types '("event.durable")
                                                                     :target "durable-target"))
                                                  :targets (list target) :store store)))
                                   (event-runtime-start runtime)
                                   (event-runtime-publish
                                    runtime
                                    (%store-envelope "durable-1" "event.durable"
                                                     (object "message" "persisted"))
                                    :identity-scope "tenant:a" :trust "authenticated")
                                   ;; The program ran, from a store on disk.
                                   (%assert-equal outputs
                                                  (vector (object "message" "persisted"))
                                                  "persistent-store dispatch input")
                                   (let ((run (event-runtime-get-run
                                               runtime "run:durable-route:durable-1:1")))
                                     (unless run
                                       (%fixture-fail "no run was recorded in the persistent store"))
                                     (%assert-equal (event-run-status run) "succeeded"
                                                    "persistent-store run status")
                                     (%assert-equal (event-run-output run)
                                                    (object "handled" "persisted")
                                                    "persistent-store run output"))
                                   ;; The store claims persistent/multi-worker and
                                   ;; carries the conformance marker, so Core's
                                   ;; verdict in the runtime descriptor has to
                                   ;; follow the store rather than the default.
                                   (%assert-equal (jget (event-runtime-descriptor runtime)
                                                        "coordination")
                                                  "multi-worker"
                                                  "descriptor coordination from the store")
                                   (%assert-equal (jget (event-runtime-descriptor runtime)
                                                        "durability")
                                                  "persistent"
                                                  "descriptor durability from the store")
                                   (%assert-equal (jget (event-runtime-descriptor runtime)
                                                        "storeConformant")
                                                  true
                                                  "descriptor store conformance")
                                   (event-runtime-close runtime)))
                               ;; A fresh process would see exactly this: the run,
                               ;; its output and the delivery's terminal status, with
                               ;; no in-memory state at all.
                               (%with-event-store (store :worker "runtime-b") runtime-path
                                 (let ((run (event-store-run store
                                                             "run:durable-route:durable-1:1")))
                                   (when (eq run :null)
                                     (%fixture-fail "the run did not survive the restart"))
                                   (%assert-equal (jget run "status") "succeeded"
                                                  "restarted run status")
                                   (%assert-equal (jget run "output")
                                                  (object "handled" "persisted")
                                                  "restarted run output"))
                                 (let ((delivery (event-store-delivery
                                                  store "durable-route:durable-1")))
                                   (%assert-equal (jget delivery "status") "succeeded"
                                                  "restarted delivery status")
                                   (%assert-equal (jget delivery "identityScope") "tenant:a"
                                                  "restarted delivery identity")
                                   (%assert-equal (jget delivery "runId")
                                                  "run:durable-route:durable-1:1"
                                                  "restarted delivery run id"))
                                 ;; Republishing the same event is a duplicate, so a
                                 ;; restarted worker cannot re-run completed work.
                                 (let* ((target (event-target
                                                 "durable-target"
                                                 :invoke (lambda (value context)
                                                           (declare (ignore context))
                                                           (vector-push-extend value outputs)
                                                           value)))
                                        (runtime (make-event-runtime
                                                  (list (event-route "durable-route"
                                                                     :action "wake"
                                                                     :types '("event.durable")
                                                                     :target "durable-target"))
                                                  :targets (list target) :store store)))
                                   (event-runtime-start runtime)
                                   (let ((receipt (event-runtime-publish
                                                   runtime
                                                   (%store-envelope "durable-1" "event.durable"
                                                                    (object "message" "persisted"))
                                                   :identity-scope "tenant:a"
                                                   :trust "authenticated")))
                                     (%assert-equal (jget receipt "duplicate") true
                                                    "a restarted worker saw a duplicate"))
                                   (%assert-equal (length outputs) 1
                                                  "a restarted worker re-ran completed work")
                                   (event-runtime-close runtime))))
                          (ignore-errors (delete-file runtime-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults runtime-path
                                             :name (concatenate 'string
                                                                (pathname-name runtime-path)
                                                                suffix)))))))))

             (check "two runtimes on one file never enter an instance together"
                    (lambda ()
                      ;; The multi-worker RUNTIME claim is mutual exclusion per
                      ;; instance, not queue ownership: both workers poll the same
                      ;; file, so either may pick up any delivery. What must never
                      ;; happen is two workers inside one instance at once, and no
                      ;; delivery may be lost or run twice to achieve that.
                      (let* ((concurrent-path (%event-store-temp-path "concurrent"))
                             (depth 0) (max-depth 0) (entries (%new-array))
                             (denied-status :null)
                             (b-receipt :null))
                        (unwind-protect
                             (%with-event-store (store-a :worker "worker-a") concurrent-path
                               (%with-event-store (store-b :worker "worker-b") concurrent-path
                                 (let* ((route (event-route
                                                "concurrent-route"
                                                :action "wake"
                                                :types '("event.concurrent")
                                                :target "concurrent-target"
                                                :instance-key (event-path-data "conversationId")))
                                        (runtime-b nil)
                                        (enter
                                          (lambda (worker value)
                                            (incf depth)
                                            (setf max-depth (max max-depth depth))
                                            (vector-push-extend
                                             (object "worker" worker
                                                     "who" (jget value "who"))
                                             entries)))
                                        (b-target
                                          (event-target
                                           "concurrent-target"
                                           :invoke (lambda (value context)
                                                     (declare (ignore context))
                                                     (funcall enter "worker-b" value)
                                                     (unwind-protect (object "ran" "b")
                                                       (decf depth)))))
                                        (a-target
                                          (event-target
                                           "concurrent-target"
                                           :invoke
                                           (lambda (value context)
                                             (declare (ignore context))
                                             (funcall enter "worker-a" value)
                                             (unwind-protect
                                                  (progn
                                                    ;; Still inside A's instance. B
                                                    ;; publishes the same instance key
                                                    ;; through its own handle on the
                                                    ;; same file and must be shut out.
                                                    (when (string= (%event-text
                                                                    (jget value "who")) "a")
                                                      (setf b-receipt
                                                            (event-runtime-publish
                                                             runtime-b
                                                             (%store-envelope
                                                              "instance-b" "event.concurrent"
                                                              (object "conversationId" "c-1"
                                                                      "who" "b"))
                                                             :identity-scope "tenant:a"
                                                             :trust "authenticated"))
                                                      (setf denied-status
                                                            (jget (event-store-delivery
                                                                   store-b
                                                                   "concurrent-route:instance-b")
                                                                  "status")))
                                                    (object "ran" "a"))
                                               (decf depth))))))
                                   (setf runtime-b (make-event-runtime
                                                    (list route) :targets (list b-target)
                                                    :store store-b))
                                   (let ((runtime-a (make-event-runtime
                                                     (list route) :targets (list a-target)
                                                     :store store-a)))
                                     (event-runtime-start runtime-a)
                                     (event-runtime-start runtime-b)
                                     (event-runtime-publish
                                      runtime-a
                                      (%store-envelope "instance-a" "event.concurrent"
                                                       (object "conversationId" "c-1"
                                                               "who" "a"))
                                      :identity-scope "tenant:a" :trust "authenticated")
                                     ;; The invariant. Strict ordering contributes
                                     ;; here too, because eligibility is read from
                                     ;; the shared file; the lease is what closes
                                     ;; the check-then-write window strict ordering
                                     ;; cannot, which the next check exercises.
                                     (%assert-equal max-depth 1
                                                    "two workers were inside one instance")
                                     (when (eq b-receipt :null)
                                       (%fixture-fail "worker b never published"))
                                     (%assert-equal (jget b-receipt "duplicate") false
                                                    "worker b publish was not a duplicate")
                                     ;; Shut out, but kept: queued, not failed, not dropped.
                                     (%assert-equal denied-status "queued"
                                                    "delivery status under a held lease")
                                     (%assert-equal (length (event-store-dead-letters store-b)) 0
                                                    "a denied lease dead-lettered work")
                                     ;; Both events ran exactly once, in order, once the
                                     ;; lease was free. Whichever worker drains is fine;
                                     ;; running twice or never is not.
                                     (%assert-equal (length entries) 2
                                                    "total dispatches for the instance")
                                     (%assert-equal (jget (aref entries 0) "who") "a"
                                                    "first dispatch")
                                     (%assert-equal (jget (aref entries 1) "who") "b"
                                                    "second dispatch")
                                     (%assert-equal (jget (event-store-delivery
                                                           store-a
                                                           "concurrent-route:instance-b")
                                                          "status")
                                                    "succeeded"
                                                    "the shut-out delivery still completed")
                                     (%assert-equal (event-store-lease-holder
                                                     store-a
                                                     (format nil "concurrent-target~cc-1"
                                                             #\Newline))
                                                    :null
                                                    "a lease outlived its dispatch")
                                     (event-runtime-close runtime-a)
                                     (event-runtime-close runtime-b)))))
                          (ignore-errors (delete-file concurrent-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults concurrent-path
                                             :name (concatenate 'string
                                                                (pathname-name concurrent-path)
                                                                suffix)))))))))

             (check "a lease held by another worker blocks dispatch and then frees it"
                    (lambda ()
                      ;; Strict ordering denies a second delivery by reading
                      ;; committed rows, which leaves a window: two workers can
                      ;; both read a queued row before either writes "running".
                      ;; The dispatch lease is the only thing that closes it, so
                      ;; this check puts the lease in a foreign worker's hands and
                      ;; asserts the runtime refuses to enter the instance.
                      (let* ((lease-path (%event-store-temp-path "lease-block"))
                             (inputs (%new-array))
                             (lease-key (format nil "lease-target~cc-9" #\Newline))
                             ;; A manual clock, because a denied lease defers the
                             ;; delivery by the retry backoff and the check has to
                             ;; step over that window deliberately.
                             (clock (make-manual-event-clock)))
                        (unwind-protect
                             (%with-event-store (store :worker "worker-a"
                                                       :lease-ms 600000
                                                       :clock clock) lease-path
                               (%with-event-store (foreign :worker "worker-foreign"
                                                           :lease-ms 600000
                                                           :clock clock) lease-path
                                 ;; Another worker is inside the instance already.
                                 (%assert-equal (jget (event-store-acquire-lease
                                                       foreign lease-key) "granted")
                                                true
                                                "the foreign worker took the lease")
                                 (let ((runtime (make-event-runtime
                                                 (list (event-route
                                                        "lease-route" :action "wake"
                                                        :types '("event.lease")
                                                        :target "lease-target"
                                                        :instance-key (event-path-data
                                                                       "conversationId")))
                                                 :targets
                                                 (list (event-target
                                                        "lease-target"
                                                        :invoke (lambda (value context)
                                                                  (declare (ignore context))
                                                                  (vector-push-extend value inputs)
                                                                  (object "ran" true))))
                                                 :store store :clock clock
                                                 :retry-backoff-ms 5000)))
                                   (event-runtime-start runtime)
                                   (event-runtime-publish
                                    runtime
                                    (%store-envelope "lease-1" "event.lease"
                                                     (object "conversationId" "c-9"
                                                             "n" 1))
                                    :identity-scope "tenant:a" :trust "authenticated")
                                   (%assert-equal (length inputs) 0
                                                  "dispatched into an instance another worker held")
                                   (let ((delivery (event-store-delivery
                                                    store "lease-route:lease-1")))
                                     (when (eq delivery :null)
                                       (%fixture-fail "the blocked delivery was dropped"))
                                     (%assert-equal (jget delivery "status") "queued"
                                                    "blocked delivery status")
                                     (%assert-equal (jget delivery "attempt") 0
                                                    "a blocked delivery burned an attempt")
                                     ;; Deferred, not due: a drain loop would
                                     ;; otherwise spin on work it cannot take.
                                     (%assert-equal (jget delivery "availableAt") 5000
                                                    "a blocked delivery stayed due"))
                                   (%assert-equal (event-runtime-next-due-at runtime) 5000
                                                  "next due time while blocked")
                                   (%assert-equal (event-runtime-run-due runtime) 0
                                                  "a blocked delivery ran before its retry")
                                   (%assert-equal (length (event-store-dead-letters store)) 0
                                                  "a blocked delivery was dead-lettered")
                                   (%assert-equal (length (event-store-runs store)) 0
                                                  "a blocked delivery recorded a run")
                                   ;; The holder leaves; the same delivery now runs.
                                   (%assert-flag (event-store-release-lease foreign lease-key)
                                                  t "the foreign worker released the lease")
                                   (manual-clock-advance clock 5000)
                                   (%assert-equal (event-runtime-run-due runtime) 1
                                                  "due count after the lease was freed")
                                   (%assert-equal (length inputs) 1
                                                  "dispatch count after the lease was freed")
                                   (%assert-equal (jget (aref inputs 0) "n") 1
                                                  "dispatch input after the lease was freed")
                                   (%assert-equal (jget (event-store-delivery
                                                         store "lease-route:lease-1")
                                                        "status")
                                                  "succeeded" "delivery status after dispatch")
                                   (event-runtime-close runtime))))
                          (ignore-errors (delete-file lease-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults lease-path
                                             :name (concatenate 'string
                                                                (pathname-name lease-path)
                                                                suffix)))))))))

             (check "a lease stolen mid-dispatch fences the old holder's write-back"
                    (lambda ()
                      ;; Core's lease transition STEALS an expired lease, so a
                      ;; target that outlives leaseMs leaves two workers free to
                      ;; run one instance. Mutual exclusion cannot be claimed by
                      ;; listing that as untested: what must be true is that the
                      ;; worker whose lease expired can no longer write a result,
                      ;; because the thief's result is the real one.
                      (let* ((fence-path (%event-store-temp-path "fence"))
                             (clock (make-manual-event-clock))
                             (inputs (%new-array))
                             (stolen :null)
                             (lease-key (format nil "fence-target~cc-7" #\Newline)))
                        (unwind-protect
                             (%with-event-store (store :worker "worker-a" :lease-ms 1000
                                                       :clock clock) fence-path
                               (%with-event-store (thief :worker "worker-thief" :lease-ms 1000
                                                         :clock clock) fence-path
                                 (let ((runtime
                                         (make-event-runtime
                                          (list (event-route
                                                 "fence-route" :action "wake"
                                                 :types '("event.fence")
                                                 :target "fence-target"
                                                 :instance-key (event-path-data
                                                                "conversationId")))
                                          :targets
                                          (list (event-target
                                                 "fence-target"
                                                 :invoke
                                                 (lambda (value context)
                                                   (declare (ignore context))
                                                   (vector-push-extend value inputs)
                                                   ;; The target outlives the lease,
                                                   ;; and another worker takes it.
                                                   (manual-clock-advance clock 5000)
                                                   (setf stolen
                                                         (event-store-acquire-lease
                                                          thief lease-key))
                                                   (object "ran" "a"))))
                                          :store store :clock clock)))
                                   (event-runtime-start runtime)
                                   (event-runtime-publish
                                    runtime
                                    (%store-envelope "fence-1" "event.fence"
                                                     (object "conversationId" "c-7"))
                                    :identity-scope "tenant:a" :trust "authenticated")
                                   (%assert-equal (length inputs) 1 "the target ran once")
                                   ;; Core stole it, and the generation moved.
                                   (%assert-equal (jget stolen "action") "steal"
                                                  "the expired lease was stolen")
                                   (%assert-equal (jget stolen "granted") true
                                                  "the thief was granted the lease")
                                   (%assert-true (> (jget stolen "generation" 0) 1)
                                                 "a steal advanced the lease generation")
                                   ;; The fence refuses the old holder by generation.
                                   (%assert-flag (event-store-fence-ok-p store lease-key 1)
                                                  nil "the fenced-out worker still passed")
                                   (%assert-flag (event-store-fence-ok-p
                                                   thief lease-key
                                                   (jget stolen "generation"))
                                                  t "the new holder failed its own fence")
                                   ;; And the write-back did not land: no success
                                   ;; was recorded behind the thief's back.
                                   (let ((delivery (event-store-delivery
                                                    store "fence-route:fence-1")))
                                     ;; Untouched: the thief owns this row now.
                                     (%assert-equal (jget delivery "status") "running"
                                                    "a fenced-out worker wrote the delivery row"))
                                   (%assert-equal (length (event-store-runs store)) 1
                                                  "run count")
                                   (%assert-equal (jget (event-store-run
                                                         store "run:fence-route:fence-1:1")
                                                        "status")
                                                  "queued"
                                                  "a fenced-out worker saved its run")
                                   (let ((dead (event-store-dead-letters store)))
                                     (%assert-equal (length dead) 1 "dead letter count")
                                     (%assert-equal (event-dead-letter-reason (first dead))
                                                    "lease_lost"
                                                    "the fenced-out dispatch was not reported"))
                                   (event-runtime-close runtime))))
                          (ignore-errors (delete-file fence-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults fence-path
                                             :name (concatenate 'string
                                                                (pathname-name fence-path)
                                                                suffix)))))))))

             (check "a worker that lost the queued race cannot write its stale row"
                    (lambda ()
                      ;; Two connections, and the losing worker holds a delivery
                      ;; struct it read while the row was still queued. If the
                      ;; queued-to-running move were a read then a write, this
                      ;; worker would overwrite the winner's finished row and run
                      ;; the program a second time.
                      (let* ((race-path (%event-store-temp-path "race"))
                             (inputs (%new-array)))
                        (unwind-protect
                             (%with-event-store (winner :worker "worker-win") race-path
                               (%with-event-store (loser :worker "worker-lose") race-path
                                 (let ((runtime
                                         (make-event-runtime
                                          (list (event-route
                                                 "race-route" :action "wake"
                                                 :types '("event.race")
                                                 :target "race-target"
                                                 :instance-key (event-path-data
                                                                "conversationId")))
                                          :targets
                                          (list (event-target
                                                 "race-target"
                                                 :invoke (lambda (value context)
                                                           (declare (ignore context))
                                                           (vector-push-extend value inputs)
                                                           (object "ran" true))))
                                          :store winner)))
                                   (event-runtime-start runtime)
                                   ;; Enqueue, then the loser reads the queued row.
                                   (event-store-enqueue
                                    winner
                                    (%store-envelope "race-1" "event.race"
                                                     (object "conversationId" "c-3"))
                                    (list (%store-command "race-route" "race-1"
                                                          :target "race-target"
                                                          :instance-key "c-3")))
                                   (let ((stale (find "race-route:race-1"
                                                      (event-store-deliveries loser)
                                                      :key #'%event-delivery-id
                                                      :test #'equal)))
                                     (when (null stale)
                                       (%fixture-fail "the loser never saw the queued row"))
                                     (%assert-equal (%event-delivery-status stale) "queued"
                                                    "the loser's copy was queued")
                                     ;; The winner takes and finishes it.
                                     (%assert-equal (event-runtime-run-due runtime) 1
                                                    "the winner dispatched")
                                     (%assert-equal (length inputs) 1 "the winner ran once")
                                     (%assert-equal (jget (event-store-delivery
                                                           winner "race-route:race-1")
                                                          "status")
                                                    "succeeded" "the winner's status")
                                     ;; Now the loser tries, holding its stale copy.
                                     (%assert-flag (event-store-begin-delivery loser stale)
                                                    nil
                                                    "the loser won an already-taken delivery")
                                     (%assert-equal (jget (event-store-delivery
                                                           loser "race-route:race-1")
                                                          "status")
                                                    "succeeded"
                                                    "the loser overwrote the finished row")
                                     ;; Even its deferral must not resurrect the row
                                     ;; as queued work for someone else to run again.
                                     (event-store-requeue-delivery loser stale 999)
                                     (%assert-equal (jget (event-store-delivery
                                                           winner "race-route:race-1")
                                                          "status")
                                                    "succeeded"
                                                    "a loser's requeue revived finished work")
                                     (%assert-equal (event-runtime-run-due runtime) 0
                                                    "the revived row was dispatched again")
                                     (%assert-equal (length inputs) 1
                                                    "the program ran twice"))
                                   (event-runtime-close runtime))))
                          (ignore-errors (delete-file race-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults race-path
                                             :name (concatenate 'string
                                                                (pathname-name race-path)
                                                                suffix)))))))))

             (check "a fenced-out dispatch writes no state and no continuation either"
                    (lambda ()
                      ;; The fence is only real if EVERY persistent write of a
                      ;; dispatch is behind it. Captured state and declared
                      ;; continuations used to be written the moment the program
                      ;; produced them, which left them outside the boundary: a
                      ;; worker whose lease had been stolen still left its state
                      ;; and its continuations in the database. This check steals
                      ;; the lease mid-run and then asserts that nothing at all
                      ;; from that dispatch landed.
                      (let* ((part-path (%event-store-temp-path "partial"))
                             (clock (make-manual-event-clock))
                             (lease-key (format nil "partial-target~cc-5" #\Newline)))
                        (unwind-protect
                             (%with-event-store (store :worker "worker-a" :lease-ms 1000
                                                       :clock clock) part-path
                               (%with-event-store (thief :worker "worker-thief" :lease-ms 1000
                                                         :clock clock) part-path
                                 (let ((runtime
                                         (make-event-runtime
                                          (list (event-route
                                                 "partial-route" :action "wake"
                                                 :types '("event.partial")
                                                 :target "partial-target"
                                                 :instance-key (event-path-data
                                                                "conversationId")))
                                          :targets
                                          (list (event-target
                                                 "partial-target"
                                                 :capture-state (lambda ()
                                                                  (object "seen" "a"))
                                                 :wait-for (list (list "correlation"
                                                                       (event-path-data "waitKey")))
                                                 :invoke
                                                 (lambda (value context)
                                                   (declare (ignore value context))
                                                   ;; The lease expires and another
                                                   ;; worker takes it, all before
                                                   ;; this dispatch commits.
                                                   (manual-clock-advance clock 5000)
                                                   (event-store-acquire-lease thief lease-key)
                                                   (object "ran" "a"))))
                                          :store store :clock clock)))
                                   (event-runtime-start runtime)
                                   (event-runtime-publish
                                    runtime
                                    (%store-envelope "partial-1" "event.partial"
                                                     (object "conversationId" "c-5"
                                                             "waitKey" "w-1"))
                                    :identity-scope "tenant:a" :trust "authenticated")
                                   ;; Nothing from the fenced-out dispatch survives.
                                   (%assert-equal (event-store-captured-state
                                                   store
                                                   (format nil "partial-target~ctenant:a~cc-5"
                                                           #\Newline #\Newline))
                                                  :null
                                                  "a fenced-out worker saved captured state")
                                   (%assert-equal (length (event-store-continuations store)) 0
                                                  "a fenced-out worker saved a continuation")
                                   (%assert-equal (jget (event-store-run
                                                         store
                                                         "run:partial-route:partial-1:1")
                                                        "status")
                                                  "queued"
                                                  "a fenced-out worker saved its run")
                                   (%assert-equal (jget (event-store-delivery
                                                         store "partial-route:partial-1")
                                                        "status")
                                                  "running"
                                                  "a fenced-out worker wrote the delivery row")
                                   (%assert-equal (event-dead-letter-reason
                                                   (first (event-store-dead-letters store)))
                                                  "lease_lost"
                                                  "the fenced-out dispatch was not reported")
                                   (event-runtime-close runtime))))
                          (ignore-errors (delete-file part-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults part-path
                                             :name (concatenate 'string
                                                                (pathname-name part-path)
                                                                suffix)))))))))

             (check "a steal cannot land between the fence check and the commit"
                    (lambda ()
                      ;; The previous design checked the lease with one SELECT
                      ;; and wrote afterwards, so a steal could land in between
                      ;; and the stale holder still overwrote the thief. The
                      ;; repair is that the check and the writes are one
                      ;; transaction. This forces the interleaving from a real
                      ;; second thread: the thief tries to steal from its own
                      ;; connection while the commit is open, and the assertion
                      ;; is that the commit is all-or-nothing against it.
                      (let* ((tx-path (%event-store-temp-path "tx"))
                             (started (sb-thread:make-semaphore))
                             (thief-done (sb-thread:make-semaphore))
                             (thief-result :null))
                        (unwind-protect
                             (%with-event-store (store :worker "worker-a"
                                                       :lease-ms 600000) tx-path
                               (%with-event-store (thief :worker "worker-thief"
                                                         :lease-ms 600000) tx-path
                                 (let ((lease-key (format nil "tx-target~cc-1" #\Newline)))
                                   (%assert-equal (jget (event-store-acquire-lease
                                                         store lease-key) "granted")
                                                  true "worker a took the lease")
                                   (let ((thief-thread
                                           (sb-thread:make-thread
                                            (lambda ()
                                              (sb-thread:wait-on-semaphore started)
                                              ;; Its own connection, racing the
                                              ;; open commit transaction.
                                              (setf thief-result
                                                    (handler-case
                                                        (event-store-acquire-lease
                                                         thief lease-key)
                                                      (error (e)
                                                        (object "error"
                                                                (princ-to-string e)))))
                                              (sb-thread:signal-semaphore thief-done))
                                            :name "ax-test-thief")))
                                     ;; A fenced commit that writes two rows while
                                     ;; the thief is trying to steal.
                                     (%assert-equal
                                      (event-store-commit-fenced
                                       store lease-key 1
                                       (lambda ()
                                         ;; The property, stated where it can
                                         ;; actually be observed: these writes run
                                         ;; inside the very transaction the lease
                                         ;; check ran in. A commit boundary that
                                         ;; checked and then wrote separately
                                         ;; would not be in one here, which is the
                                         ;; window a steal used to fit through.
                                         (%assert-true
                                          (eq (%event-sqlite-transaction-thread store)
                                              sb-thread:*current-thread*)
                                          "the fenced writes ran inside the check's transaction")
                                         (sb-thread:signal-semaphore started)
                                         (event-store-save-captured-state
                                          store "tx-state" (object "half" 1))
                                         (sleep 0.05)
                                         (event-store-save-captured-state
                                          store "tx-state-2" (object "half" 2))))
                                      t "the fenced commit was refused")
                                     (sb-thread:wait-on-semaphore thief-done)
                                     (sb-thread:join-thread thief-thread :default nil))
                                   ;; Both writes are present: the transaction was
                                   ;; not torn in half by the steal attempt.
                                   (%assert-equal (event-store-captured-state store "tx-state")
                                                  (object "half" 1) "first fenced write")
                                   (%assert-equal (event-store-captured-state store "tx-state-2")
                                                  (object "half" 2) "second fenced write")
                                   ;; The thief's steal, whenever it landed, cannot
                                   ;; have landed inside the commit: a successful
                                   ;; steal must have advanced the generation, and
                                   ;; worker a's old generation must now be refused.
                                   (when (and (not (eq thief-result :null))
                                              (axllm/core::core-true-p
                                               (jget thief-result "granted")))
                                     (%assert-true (> (jget thief-result "generation" 0) 1)
                                                   "a steal reused the fenced generation")
                                     (%assert-flag (event-store-fence-ok-p store lease-key 1)
                                                    nil
                                                    "worker a still passed after the steal")
                                     (%assert-equal
                                      (event-store-commit-fenced
                                       store lease-key 1
                                       (lambda ()
                                         (event-store-save-captured-state
                                          store "tx-state" (object "overwritten" true))))
                                      nil "a stale holder committed after the steal")
                                     (%assert-equal (event-store-captured-state store "tx-state")
                                                    (object "half" 1)
                                                    "a stale holder overwrote the thief")))))
                          (ignore-errors (delete-file tx-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults tx-path
                                             :name (concatenate 'string
                                                                (pathname-name tx-path)
                                                                suffix)))))))))

             (check "a released and reacquired lease does not revive a stale generation"
                    (lambda ()
                      ;; ABA. Release is owner-only, so the obvious implementation
                      ;; deletes the row -- and then the next claim starts the
                      ;; generation again at 1, so a worker still holding
                      ;; generation 1 from a previous dispatch passes a fence it
                      ;; must fail. Worst case is the SAME worker id, where the
                      ;; owner check cannot tell the two apart either.
                      (let ((aba-path (%event-store-temp-path "aba"))
                            (lease-key "aba-key"))
                        (unwind-protect
                             (%with-event-store (store :worker "worker-a"
                                                       :lease-ms 600000) aba-path
                               (let* ((first-lease (event-store-acquire-lease store lease-key))
                                      (first-generation (jget first-lease "generation")))
                                 (%assert-equal (jget first-lease "granted") true
                                                "first claim")
                                 (%assert-flag (event-store-fence-ok-p
                                                 store lease-key first-generation)
                                                t "the holder failed its own fence")
                                 (%assert-flag (event-store-release-lease store lease-key)
                                                t "release")
                                 (%assert-equal (event-store-lease-holder store lease-key)
                                                :null
                                                "a released lease still reports a holder")
                                 ;; Same worker id, same lease key, fresh claim.
                                 (let* ((second-lease (event-store-acquire-lease store lease-key))
                                        (second-generation (jget second-lease "generation")))
                                   (%assert-equal (jget second-lease "granted") true
                                                  "reacquire")
                                   (%assert-equal (jget second-lease "action") "claim"
                                                  "a released lease was treated as held")
                                   (%assert-true (> second-generation first-generation)
                                                 "the generation restarted after release")
                                   ;; The stale generation must now be worthless,
                                   ;; even to the worker that originally held it.
                                   (%assert-flag (event-store-fence-ok-p
                                                   store lease-key first-generation)
                                                  nil
                                                  "a stale generation still passed the fence")
                                   (%assert-equal
                                    (event-store-commit-fenced
                                     store lease-key first-generation
                                     (lambda () (event-store-save-captured-state
                                                 store "aba-state" (object "stale" true))))
                                    nil "a stale generation committed")
                                   (%assert-equal (event-store-captured-state store "aba-state")
                                                  :null "a stale generation wrote state")
                                   ;; And the current generation still works.
                                   (%assert-equal
                                    (event-store-commit-fenced
                                     store lease-key second-generation
                                     (lambda () (event-store-save-captured-state
                                                 store "aba-state" (object "fresh" true))))
                                    t "the current generation was refused")
                                   (%assert-equal (event-store-captured-state store "aba-state")
                                                  (object "fresh" true)
                                                  "the current generation's write"))))
                          (ignore-errors (delete-file aba-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults aba-path
                                             :name (concatenate 'string
                                                                (pathname-name aba-path)
                                                                suffix)))))))))

             (check "a stale dispatch cleanup leaves the current generation held"
                    (lambda ()
                      ;; The other half of ABA, and not the same thing as
                      ;; rejecting a stale commit. A fenced-out dispatch still
                      ;; runs its unwind cleanup, and releasing by owner alone is
                      ;; wrong for precisely the worker most likely to do it: the
                      ;; same worker id can legitimately hold a LATER generation
                      ;; of that lease by then, so an owner-only release unlocks
                      ;; an instance this worker is currently inside. The commit
                      ;; boundary stops a stale worker from writing; this stops it
                      ;; from unlocking.
                      (let ((cleanup-path (%event-store-temp-path "cleanup"))
                            (clock (make-manual-event-clock))
                            (lease-key "cleanup-key"))
                        (unwind-protect
                             (%with-event-store (store :worker "worker-a" :lease-ms 1000
                                                       :clock clock) cleanup-path
                               (%with-event-store (thief :worker "worker-thief" :lease-ms 1000
                                                         :clock clock) cleanup-path
                                 ;; A takes generation 1, as an in-flight dispatch.
                                 (let ((first-generation
                                         (jget (event-store-acquire-lease store lease-key)
                                               "generation")))
                                   ;; It expires, a thief steals it and leaves.
                                   (manual-clock-advance clock 5000)
                                   (%assert-equal (jget (event-store-acquire-lease
                                                         thief lease-key) "action")
                                                  "steal" "the thief stole the lease")
                                   (%assert-flag (event-store-release-lease thief lease-key)
                                                  t "the thief released")
                                   ;; A reclaims it for a NEW dispatch: same worker
                                   ;; id, later generation.
                                   (let* ((second (event-store-acquire-lease store lease-key))
                                          (second-generation (jget second "generation")))
                                     (%assert-equal (jget second "action") "claim"
                                                    "the reclaim was not a fresh claim")
                                     (%assert-true (> second-generation first-generation)
                                                   "the reclaim did not advance the generation")
                                     ;; Now the OLD dispatch's cleanup runs.
                                     (%assert-flag (event-store-release-instance
                                                     store lease-key first-generation)
                                                    nil
                                                    "a stale cleanup reported a release")
                                     ;; The new dispatch still holds its instance.
                                     (%assert-equal (jget (event-store-lease-holder
                                                           store lease-key) "owner")
                                                    "worker-a"
                                                    "a stale cleanup unlocked the instance")
                                     (%assert-flag (event-store-fence-ok-p
                                                     store lease-key second-generation)
                                                    t
                                                    "the current generation lost its fence")
                                     ;; And it can still commit, which is the
                                     ;; consequence that actually matters.
                                     (%assert-equal
                                      (event-store-commit-fenced
                                       store lease-key second-generation
                                       (lambda () (event-store-save-captured-state
                                                   store "cleanup-state" (object "live" true))))
                                      t "the live dispatch was refused its commit")
                                     (%assert-equal (event-store-captured-state
                                                     store "cleanup-state")
                                                    (object "live" true)
                                                    "the live dispatch's write")
                                     ;; The real holder's own release still works.
                                     (%assert-flag (event-store-release-instance
                                                     store lease-key second-generation)
                                                    t
                                                    "the current holder could not release")
                                     (%assert-equal (event-store-lease-holder store lease-key)
                                                    :null
                                                    "the lease survived its holder's release")))))
                          (ignore-errors (delete-file cleanup-path))
                          (dolist (suffix '("-wal" "-shm"))
                            (ignore-errors
                             (delete-file
                              (make-pathname :defaults cleanup-path
                                             :name (concatenate 'string
                                                                (pathname-name cleanup-path)
                                                                suffix)))))))))

             (check "a delivery status change is durable"
                    (lambda ()
                      (%with-event-store (store) path
                        (event-store-enqueue store (%store-envelope "st-1" "event.st" (object))
                                             (list (%store-command "r5" "st-1")))
                        (event-store-set-delivery-status store "r5:st-1" "running"
                                                         :attempt 1 :run-id "run:st:1"))
                      (%with-event-store (store) path
                        (let ((delivery (event-store-delivery store "r5:st-1")))
                          (%assert-equal (jget delivery "status") "running" "durable status")
                          (%assert-equal (jget delivery "attempt") 1 "durable attempt")
                          (%assert-equal (jget delivery "runId") "run:st:1" "durable run id"))))))
        (ignore-errors (delete-file path))
        (dolist (suffix '("-wal" "-shm"))
          (ignore-errors
           (delete-file (make-pathname :defaults path
                                       :name (concatenate 'string (pathname-name path) suffix)))))))
    (format stream "~&axevent store: ~a passed, ~a failed~%" passed failed)
    (format stream "~&axevent store: proven here: persistence across a reopen; Core-judged lease coordination between two workers; AxEventRuntime dispatching from the persistent store without re-running completed work after a restart; per-instance mutual exclusion, where a lease held by another worker defers the delivery by its retry backoff, unattempted and not dead-lettered, until the holder releases; a compare-and-set queued-to-running move, so a worker holding a delivery it read while queued can neither take it nor revive it after the winner finished; one atomic commit boundary per dispatch, where the lease owner-and-generation check and every persistent write of that dispatch -- captured state, declared continuations, the run and the delivery row -- share a single BEGIN IMMEDIATE transaction, so a fenced-out worker leaves nothing at all behind and is recorded as lease_lost; and a monotonic generation per lease key, so a released and reacquired lease, including by the same worker id, cannot revive a stale generation -- which the dispatch release honours too, so a fenced-out dispatch's cleanup cannot unlock a later generation of that lease its own worker id is currently holding. Four mechanisms contribute and none is redundant: strict ordering denies a second delivery by reading committed rows; the compare-and-set denies a second worker the queued row; the lease denies a second worker the instance; the generation denies a stale holder both its write-back and its release, which matters because Core steals an expired lease and because a stale cleanup that unlocked a live instance would undo every other mechanism. What follows is at-least-once delivery with no partial and no overwriting write, NOT exactly-once: external sink effects are outside the boundary and have already happened by commit time, and there is no lease renewal, so a target that outlives leaseMs loses its instance and the thief may run that instance again. Also not claimed: OS-level process crash recovery, and contention under real thread preemption rather than the deterministic interleavings these checks force -- the one threaded check here proves the commit is not torn by a concurrent steal and that a stale holder is refused afterwards, but a window that no longer exists cannot be observed from outside, so that property is pinned white-box by asserting the writes run inside the check's own transaction~%")
    (values passed failed)))

(defun run-event-store-conformance-or-die ()
  (multiple-value-bind (passed failed) (run-event-store-conformance)
    (declare (ignore passed))
    (unless (zerop failed)
      (error "~a event store conformance check(s) failed." failed))
    t))
