;;;; event-store-sqlite.lisp --- a persistent, lease-coordinated event store.
;;;;
;;;; The in-memory store in event.lisp is volatile and single-worker by
;;;; construction. This one is persistent and can be shared by cooperating
;;;; processes on one local disk, which is the only configuration SQLite is
;;;; safe in: never on a network filesystem.
;;;;
;;;; Core owns coordination, not this file. EVENT-STORE-CAPABILITY decides
;;;; whether a descriptor may claim multi-worker at all, and refuses the
;;;; claim without the axevent.store-conformance.v1 marker, persistent
;;;; durability and a positive lease. EVENT-LEASE-TRANSITION decides claim,
;;;; renew, steal or deny for every lease attempt. What is native here is
;;;; SQL, the WAL and busy-timeout configuration, and the transaction
;;;; boundaries that make a lease decision atomic against another worker.
;;;;
;;;; HONEST SCOPE, because this is the easiest place in the package to
;;;; overclaim. This store provides persistence and correct multi-worker
;;;; lease coordination, and EVENT-SQLITE-STORE-CONFORMANCE proves both,
;;;; including across a reopen and against a second connection. It is NOT
;;;; yet wired underneath AxEventRuntime's inline dispatch: that runtime
;;;; reads and mutates in-memory delivery records directly, so making it
;;;; drive this store needs a store interface extracted in event.lisp
;;;; first. Until that lands, MAKE-EVENT-RUNTIME on this store would still
;;;; dispatch from memory, so EVENT-RUNTIME-DESCRIPTOR reports what is
;;;; actually true rather than what the store could support.

(in-package #:axllm)

(defparameter +event-store-conformance-marker+ "axevent.store-conformance.v1"
  "The marker Core requires before a store may claim multi-worker.

A store that does not pass EVENT-SQLITE-STORE-CONFORMANCE must not present
it. Core refuses the multi-worker claim without it, which is the whole
point: a capability claim has to be earned, not asserted.")

(defclass sqlite-event-store ()
  ((path :initarg :path :reader event-store-path)
   (handle :initarg :handle :reader %event-sqlite-handle)
   (worker :initarg :worker :reader event-store-worker)
   (lease-ms :initarg :lease-ms :reader event-store-lease-ms)
   (clock :initarg :clock :reader event-store-clock)
   (lock :initform (sb-thread:make-mutex :name "ax-event-sqlite") :reader %event-sqlite-lock)
   ;; The thread currently inside this connection's transaction, so a nested
   ;; store operation joins that transaction instead of opening a second one
   ;; SQLite would refuse, or blocking on a mutex it already holds.
   (transaction-thread :initform nil :accessor %event-sqlite-transaction-thread))
  (:documentation
   "A persistent event store on one local SQLite database.

Several processes may share one PATH. Each takes a distinct WORKER id and
holds a lease per instance key while it dispatches that instance, so strict
per-instance ordering survives across processes."))

(defun make-sqlite-event-store (path &key worker (lease-ms 30000) clock)
  "Open or create the event store at PATH.

WORKER defaults to a fresh id, so two stores in one image never share a
lease by accident. WAL and a busy timeout are set because without them a
second worker sees SQLITE_BUSY instead of waiting."
  (let* ((handle (sqlite:connect (namestring path)))
         (store (make-instance 'sqlite-event-store
                               :path (namestring path) :handle handle
                               :worker (or worker (format nil "worker:~a" (%mcp-uuid)))
                               :lease-ms lease-ms
                               :clock (or clock (make-system-event-clock)))))
    ;; WAL lets a reader and a writer proceed together; the busy timeout
    ;; turns a concurrent writer into a wait instead of an error.
    (sqlite:execute-non-query handle "pragma journal_mode = wal")
    (sqlite:execute-non-query handle "pragma busy_timeout = 5000")
    (sqlite:execute-non-query handle "pragma synchronous = full")
    (dolist (statement
             '("create table if not exists deliveries (
                  id text primary key, route_id text not null, event_id text not null,
                  target_id text, instance_key text not null, idempotency_key text not null,
                  action text not null, envelope text not null, status text not null,
                  available_at real not null, sequence integer not null,
                  size integer not null, attempt integer not null,
                  identity_scope text not null, trust text not null, run_id text)"
               "create index if not exists deliveries_due
                  on deliveries (status, available_at, sequence)"
               "create table if not exists runs (
                  id text primary key, delivery_id text not null, route_id text not null,
                  target_id text, instance_key text not null, status text not null,
                  attempt integer not null, output text, error text, sequence integer not null)"
               "create table if not exists dead_letters (
                  id text primary key, delivery_id text not null, reason text not null,
                  run_id text, sink_id text, sequence integer not null)"
               "create table if not exists continuations (
                  id text primary key, target_id text not null, instance_key text not null,
                  identity_scope text not null, correlation text not null,
                  metadata text, completed integer not null, expires_at real,
                  sequence integer not null)"
               "create table if not exists program_state (
                  state_key text primary key, value text not null)"
               "create table if not exists leases (
                  lease_key text primary key, owner text not null, expires_at real not null,
                  generation integer not null default 0)"
               "create table if not exists sequences (name text primary key, value integer not null)"))
      (sqlite:execute-non-query handle statement))
    ;; A store file written before fencing existed has no generation column,
    ;; and a lease with no generation cannot fence anything. Add it rather
    ;; than silently running unfenced against an old file.
    (unless (member "generation"
                    (sqlite:execute-to-list handle "pragma table_info(leases)")
                    :test #'equal :key #'second)
      (sqlite:execute-non-query
       handle "alter table leases add column generation integer not null default 0"))
    store))

(defun %event-sqlite-in-transaction (store thunk)
  "Call THUNK with STORE's handle inside exactly one immediate transaction.

BEGIN IMMEDIATE takes SQLite's RESERVED lock, so from the moment this
transaction opens no other connection can write this database until it
commits. That is what makes a check and the writes that depend on it
atomic: a lease verified inside the transaction cannot be stolen before
the transaction's own writes land.

A nested call joins the open transaction rather than starting another,
because SQLite has no nested BEGIN and the mutex is not recursive. Joining
is what lets the dispatch commit boundary be assembled from the ordinary
single-row store operations instead of one hand-written mega-statement."
  (if (eq (%event-sqlite-transaction-thread store) sb-thread:*current-thread*)
      (funcall thunk (%event-sqlite-handle store))
      (sb-thread:with-mutex ((%event-sqlite-lock store))
        (let ((handle (%event-sqlite-handle store)))
          (sqlite:execute-non-query handle "begin immediate")
          (setf (%event-sqlite-transaction-thread store) sb-thread:*current-thread*)
          (let ((committed nil))
            (unwind-protect
                 (multiple-value-prog1 (funcall thunk handle)
                   (sqlite:execute-non-query handle "commit")
                   (setf committed t))
              (setf (%event-sqlite-transaction-thread store) nil)
              (unless committed
                (ignore-errors (sqlite:execute-non-query handle "rollback")))))))))

(defmacro %with-event-sqlite ((handle store) &body body)
  "Run BODY in STORE's transaction, opening one if none is open."
  `(%event-sqlite-in-transaction ,store (lambda (,handle) ,@body)))

(defun event-store-close (store)
  (sb-thread:with-mutex ((%event-sqlite-lock store))
    (ignore-errors (sqlite:disconnect (%event-sqlite-handle store))))
  nil)

(defun %event-sqlite-next-sequence (handle name)
  (sqlite:execute-non-query
   handle "insert into sequences (name, value) values (?, 0) on conflict(name) do nothing" name)
  (sqlite:execute-non-query
   handle "update sequences set value = value + 1 where name = ?" name)
  (sqlite:execute-single handle "select value from sequences where name = ?" name))

;;; ------------------------------------------------------------------
;;; Capability
;;; ------------------------------------------------------------------

(defun event-store-descriptor (store)
  "STORE's own claim, as Core reads it.

The marker is presented because EVENT-SQLITE-STORE-CONFORMANCE passes for
this implementation. Core still validates the rest of the claim."
  (object "durability" "persistent"
          "coordination" "multi-worker"
          "conformanceMarker" +event-store-conformance-marker+
          ;; Disclosed rather than implied: dispatch is fenced, so two
          ;; workers never both write one run, but nothing renews a lease
          ;; while an arbitrary target runs. A target that outlives leaseMs
          ;; loses its instance to a thief, which makes this at-least-once.
          "fencing" true
          "leaseRenewal" false
          "leaseMs" (event-store-lease-ms store)
          "path" (event-store-path store)
          "worker" (event-store-worker store)))

(defun event-store-capability (store)
  "Core's verdict on STORE's claim: coordination, durability and why."
  (axllm/core::event-store-capability (event-store-descriptor store)))

;;; ------------------------------------------------------------------
;;; Leases
;;; ------------------------------------------------------------------

(defun event-store-fencing-p (store)
  "Whether STORE fences a stolen lease out of its own write-back.

True for this implementation; the predicate exists so the runtime claim
follows a capability rather than the class."
  (declare (ignore store))
  t)

(defun event-store-acquire-lease (store lease-key)
  "Try to hold LEASE-KEY for this worker. Returns Core's transition.

Core decides claim, renew, steal or deny; this only reads the current row
and writes the one Core granted, inside a single immediate transaction so
two workers cannot both be granted.

The returned transition carries a \"generation\" the caller must keep. A
steal increments it, so a worker whose lease expired mid-dispatch holds a
stale generation and every fenced write it attempts afterwards is refused.
Without that, an expired lease would let two workers write the same run."
  (%with-event-sqlite (handle store)
    (let* ((row (sqlite:execute-to-list
                 handle "select owner, expires_at, generation from leases where lease_key = ?"
                 lease-key))
           (held-owner (and row (first (first row))))
           ;; A released lease keeps its row with an empty owner. Core's
           ;; transition only understands held or absent, so an empty owner
           ;; is reported as absent while the generation is still read from
           ;; the row it left behind.
           (current (if (and held-owner (plusp (length held-owner)))
                        (object "owner" held-owner
                                "expiresAt" (second (first row)))
                        :null))
           (held-generation (if row (or (third (first row)) 0) 0))
           (transition (axllm/core::event-lease-transition
                        (event-clock-now (event-store-clock store))
                        current
                        (event-store-worker store)
                        (event-store-lease-ms store))))
      (when (axllm/core::core-true-p (jget transition "granted"))
        ;; Core names the transition; the generation follows from it. Only a
        ;; steal fences the previous holder, and a claim onto an absent row
        ;; starts a fresh generation for the same reason.
        (let ((generation (if (equal (%mcp-text (jget transition "action")) "renew")
                              held-generation
                              (1+ held-generation))))
          (sqlite:execute-non-query
           handle "insert into leases (lease_key, owner, expires_at, generation)
                   values (?, ?, ?, ?)
                   on conflict(lease_key) do update set owner = excluded.owner,
                   expires_at = excluded.expires_at, generation = excluded.generation"
           lease-key (%mcp-text (jget transition "owner")) (jget transition "expiresAt")
           generation)
          (%set-key transition "generation" generation)))
      transition)))

(defun event-store-fence-ok-p (store lease-key generation)
  "Whether this worker still holds LEASE-KEY at GENERATION."
  (%with-event-sqlite (handle store)
    (plusp (sqlite:execute-single
            handle
            "select count(*) from leases
             where lease_key = ? and owner = ? and generation = ?"
            lease-key (event-store-worker store) generation))))

(defun event-store-release-lease (store lease-key)
  "Give up LEASE-KEY, but only if this worker still holds it.

The row is kept with an empty owner rather than deleted. Deleting it would
restart the generation at 1 on the next claim, so a worker still holding a
stale generation 1 -- the same worker id after a release and reacquire, or
any worker after a full cycle -- would pass a fence it must fail. The
generation is monotonic per lease key for exactly that reason."
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "update leases set owner = '', expires_at = 0
             where lease_key = ? and owner = ?"
     lease-key (event-store-worker store))
    (plusp (sqlite:execute-single handle "select changes()"))))

(defun event-store-lease-holder (store lease-key)
  "Who holds LEASE-KEY, or :NULL.

A released lease keeps its row so the generation stays monotonic, so an
empty owner means unheld and is reported as :NULL rather than as a holder
whose name happens to be the empty string."
  (%with-event-sqlite (handle store)
    (let ((row (sqlite:execute-to-list handle
                                       "select owner, expires_at from leases where lease_key = ?"
                                       lease-key)))
      (if (and row (plusp (length (first (first row)))))
          (object "owner" (first (first row)) "expiresAt" (second (first row)))
          :null))))

;;; ------------------------------------------------------------------
;;; Deliveries
;;; ------------------------------------------------------------------

(defun %event-sqlite-delivery-struct (row)
  "One row as the delivery struct the runtime expects."
  (destructuring-bind (id route-id event-id target-id instance-key idempotency-key
                       action envelope status available-at sequence attempt
                       identity-scope trust run-id)
      row
    (declare (ignore event-id))
    (make-event-delivery
     :id id
     :envelope (%event-envelope-of (parse-json envelope))
     :command (object "routeId" route-id "action" action
                      "targetId" (or target-id :null)
                      "instanceKey" instance-key
                      "idempotencyKey" idempotency-key)
     :status status :available-at available-at :sequence sequence
     :size (length (%mcp-utf8 envelope)) :attempt attempt
     :identity-scope identity-scope :trust trust :run-id run-id)))

(defparameter +event-sqlite-delivery-columns+
  "id, route_id, event_id, target_id, instance_key, idempotency_key, action,
   envelope, status, available_at, sequence, attempt, identity_scope, trust, run_id")


(defmethod event-store-enqueue ((store sqlite-event-store) envelope commands
                                &key (available-at nil))
  "Admit one delivery per command, idempotently on routeId:eventId.

A duplicate publication is a no-op rather than a second delivery, which is
what makes an at-least-once source safe to retry across processes. Returns
the admitted delivery records, the same shape the in-memory store returns,
so the runtime can stamp the verified ingress identity on them."
  (let ((encoded (encode-json (event-envelope-object envelope)))
        (admitted '()))
    (%with-event-sqlite (handle store)
      (dolist (command commands)
        (let* ((key (format nil "~a:~a" (%mcp-text (jget command "routeId"))
                            (event-envelope-id envelope)))
               (existing (sqlite:execute-single
                          handle "select count(*) from deliveries where id = ?" key)))
          (when (zerop existing)
            (let ((sequence (%event-sqlite-next-sequence handle "delivery")))
              (sqlite:execute-non-query
               handle "insert into deliveries (id, route_id, event_id, target_id, instance_key,
                       idempotency_key, action, envelope, status, available_at, sequence, size,
                       attempt, identity_scope, trust, run_id)
                       values (?, ?, ?, ?, ?, ?, ?, ?, 'queued', ?, ?, ?, 0, ?, ?, null)"
               key (%mcp-text (jget command "routeId")) (event-envelope-id envelope)
               (let ((target (jget command "targetId"))) (if (eq target :null) nil (%mcp-text target)))
               (%mcp-text (jget command "instanceKey"))
               (%mcp-text (jget command "idempotencyKey"))
               (%mcp-text (jget command "action"))
               encoded
               (or available-at (event-clock-now (event-store-clock store)))
               sequence (length (%mcp-utf8 encoded))
               "anonymous" "untrusted")
              (push key admitted))))))
    ;; Re-read each admitted row so the caller gets the same delivery shape
    ;; the in-memory store hands back, rather than a bare id.
    (let ((records '()))
      (dolist (key (nreverse admitted))
        (%with-event-sqlite (handle store)
          (let ((row (sqlite:execute-to-list
                      handle
                      (format nil "select ~a from deliveries where id = ?"
                              +event-sqlite-delivery-columns+)
                      key)))
            (when row (push (%event-sqlite-delivery-struct (first row)) records)))))
      (nreverse records))))

(defun event-store-set-ingress (store delivery-id identity-scope trust)
  "Record the verified ingress identity on an admitted delivery."
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "update deliveries set identity_scope = ?, trust = ? where id = ?"
     identity-scope trust delivery-id))
  nil)

(defun event-store-delivery (store delivery-id)
  "One delivery as a JSON object, or :NULL."
  (%with-event-sqlite (handle store)
    (let ((row (sqlite:execute-to-list
                handle
                "select id, route_id, event_id, target_id, instance_key, idempotency_key,
                        action, envelope, status, available_at, sequence, attempt,
                        identity_scope, trust, run_id from deliveries where id = ?"
                delivery-id)))
      (if row (%event-sqlite-delivery-object (first row)) :null))))

(defun %event-sqlite-delivery-object (row)
  (destructuring-bind (id route-id event-id target-id instance-key idempotency-key
                       action envelope status available-at sequence attempt
                       identity-scope trust run-id)
      row
    (object "id" id "routeId" route-id "eventId" event-id
            "targetId" (or target-id :null) "instanceKey" instance-key
            "idempotencyKey" idempotency-key "action" action
            "event" (parse-json envelope) "status" status
            "availableAt" available-at "sequence" sequence "attempt" attempt
            "identityScope" identity-scope "trust" trust
            "runId" (or run-id :null))))

(defun event-store-due-deliveries (store &key (limit 100))
  "Queued deliveries whose time has come, oldest first, as Core judges due."
  (let ((now (event-clock-now (event-store-clock store)))
        (out (%new-array)))
    (%with-event-sqlite (handle store)
      (dolist (row (sqlite:execute-to-list
                    handle
                    "select id, route_id, event_id, target_id, instance_key, idempotency_key,
                            action, envelope, status, available_at, sequence, attempt,
                            identity_scope, trust, run_id
                     from deliveries where status = 'queued'
                     order by available_at asc, sequence asc limit ?" limit))
        (let ((delivery (%event-sqlite-delivery-object row)))
          (when (axllm/core::core-true-p
                 (axllm/core::event-delivery-due (%mcp-text (jget delivery "status"))
                                                 (jget delivery "availableAt") now))
            (vector-push-extend delivery out)))))
    out))

(defun event-store-set-delivery-status (store delivery-id status &key available-at attempt run-id)
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query handle "update deliveries set status = ? where id = ?"
                              status delivery-id)
    (when available-at
      (sqlite:execute-non-query handle "update deliveries set available_at = ? where id = ?"
                                available-at delivery-id))
    (when attempt
      (sqlite:execute-non-query handle "update deliveries set attempt = ? where id = ?"
                                attempt delivery-id))
    (when run-id
      (sqlite:execute-non-query handle "update deliveries set run_id = ? where id = ?"
                                run-id delivery-id)))
  nil)

(defun event-store-pending-count (store)
  (%with-event-sqlite (handle store)
    (sqlite:execute-single handle
                           "select count(*) from deliveries where status = 'queued'")))

;;; ------------------------------------------------------------------
;;; Runs, dead letters, continuations and program state
;;; ------------------------------------------------------------------

(defun event-store-put-run (store run)
  (%with-event-sqlite (handle store)
    (let ((sequence (%event-sqlite-next-sequence handle "run")))
      (sqlite:execute-non-query
       handle "insert into runs (id, delivery_id, route_id, target_id, instance_key, status,
                                 attempt, output, error, sequence)
               values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
               on conflict(id) do update set status = excluded.status,
                 attempt = excluded.attempt, output = excluded.output, error = excluded.error"
       (event-run-id run) (event-run-delivery-id run) (event-run-route-id run)
       (let ((target (event-run-target-id run))) (if (eq target :null) nil (%mcp-text target)))
       (%mcp-text (event-run-instance-key run)) (event-run-status run)
       (event-run-attempt run)
       (encode-json (event-run-output run))
       (let ((text (event-run-error run))) (if (eq text :null) nil (%mcp-text text)))
       sequence)))
  run)

(defun event-store-run (store run-id)
  "One persisted run as a JSON object, or :NULL."
  (%with-event-sqlite (handle store)
    (let ((row (sqlite:execute-to-list
                handle
                "select id, delivery_id, route_id, target_id, instance_key, status, attempt,
                        output, error from runs where id = ?" run-id)))
      (if row
          (destructuring-bind (id delivery-id route-id target-id instance-key status attempt
                               output error)
              (first row)
            (object "id" id "deliveryId" delivery-id "routeId" route-id
                    "targetId" (or target-id :null) "instanceKey" instance-key
                    "status" status "attempt" attempt
                    "output" (if output (parse-json output) :null)
                    "error" (or error :null)))
          :null))))

(defun event-store-put-dead-letter (store dead-letter)
  (%with-event-sqlite (handle store)
    (let ((sequence (%event-sqlite-next-sequence handle "dead")))
      (sqlite:execute-non-query
       handle "insert into dead_letters (id, delivery_id, reason, run_id, sink_id, sequence)
               values (?, ?, ?, ?, ?, ?) on conflict(id) do nothing"
       (event-dead-letter-id dead-letter) (event-dead-letter-delivery-id dead-letter)
       (event-dead-letter-reason dead-letter)
       (let ((run (event-dead-letter-run-id dead-letter))
             ) (if (eq run :null) nil (%mcp-text run)))
       (let ((sink (event-dead-letter-sink-id dead-letter)))
         (if (eq sink :null) nil (%mcp-text sink)))
       sequence)))
  dead-letter)

(defun event-store-dead-letter-ids (store)
  (let ((out (%new-array)))
    (%with-event-sqlite (handle store)
      (dolist (row (sqlite:execute-to-list
                    handle
                    "select id from dead_letters order by sequence asc"))
        (vector-push-extend (first row) out)))
    out))

(defun event-store-remove-dead-letter (store dead-letter-id)
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query handle "delete from dead_letters where id = ?" dead-letter-id))
  nil)

(defun event-store-put-continuation (store continuation)
  (%with-event-sqlite (handle store)
    (let ((sequence (%event-sqlite-next-sequence handle "continuation")))
      (sqlite:execute-non-query
       handle "insert into continuations (id, target_id, instance_key, identity_scope,
                                         correlation, metadata, completed, expires_at, sequence)
               values (?, ?, ?, ?, ?, ?, ?, ?, ?)
               on conflict(id) do update set completed = excluded.completed"
       (event-continuation-id continuation) (event-continuation-target-id continuation)
       (%mcp-text (event-continuation-instance-key continuation))
       (event-continuation-identity-scope continuation)
       (encode-json (event-continuation-correlation continuation))
       (encode-json (or (event-continuation-metadata continuation) (object)))
       (if (event-continuation-completed-p continuation) 1 0)
       (let ((expires (event-continuation-expires-at continuation)))
         (if (eq expires :null) nil expires))
       sequence)))
  continuation)

(defun event-store-open-continuations (store)
  "Every continuation that is not completed, as JSON objects Core can match."
  (let ((out (%new-array)))
    (%with-event-sqlite (handle store)
      (dolist (row (sqlite:execute-to-list
                    handle
                    "select id, target_id, instance_key, identity_scope, correlation, metadata,
                            completed, expires_at from continuations where completed = 0
                     order by sequence asc"))
        (destructuring-bind (id target-id instance-key identity-scope correlation metadata
                             completed expires-at)
            row
          (vector-push-extend
           (object "id" id "targetId" target-id "instanceKey" instance-key
                   "identityScope" identity-scope
                   "correlation" (parse-json correlation)
                   "metadata" (if metadata (parse-json metadata) (object))
                   "completed" (json-boolean (plusp completed))
                   "expiresAt" (or expires-at :null))
           out))))
    out))

(defun event-store-complete-continuation (store continuation-id)
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query handle "update continuations set completed = 1 where id = ?"
                              continuation-id))
  nil)

(defun event-store-put-program-state (store state-key value)
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "insert into program_state (state_key, value) values (?, ?)
             on conflict(state_key) do update set value = excluded.value"
     state-key (encode-json value)))
  nil)

(defun event-store-program-state (store state-key)
  "The persisted program state at STATE-KEY, or :NULL."
  (%with-event-sqlite (handle store)
    (let ((row (sqlite:execute-to-list handle
                                       "select value from program_state where state_key = ?"
                                       state-key)))
      (if row (parse-json (first (first row))) :null))))

(export '(+event-store-conformance-marker+
          sqlite-event-store make-sqlite-event-store event-store-close
          event-store-path event-store-worker event-store-lease-ms
          event-store-descriptor event-store-capability
          event-store-acquire-lease event-store-release-lease event-store-lease-holder
          event-store-fence-ok-p event-store-fencing-p
          event-store-set-ingress event-store-delivery event-store-due-deliveries
          event-store-set-delivery-status event-store-pending-count
          event-store-put-run event-store-run
          event-store-put-dead-letter event-store-dead-letter-ids
          event-store-remove-dead-letter
          event-store-put-continuation event-store-open-continuations
          event-store-complete-continuation
          event-store-put-program-state event-store-program-state))

;;; ------------------------------------------------------------------
;;; The runtime store interface
;;; ------------------------------------------------------------------
;;;
;;; These are the generics event.lisp's runtime dispatches through. With
;;; them in place AxEventRuntime runs on this store, so a delivery, its run,
;;; its dead letters, its continuations and its captured program state all
;;; survive the process. The runtime still mutates a delivery struct in
;;; place, so every mutation is followed by EVENT-STORE-COMMIT-DELIVERY,
;;; which is where the row is written back.

(defmethod event-store-deliveries ((store sqlite-event-store))
  (%with-event-sqlite (handle store)
    (mapcar #'%event-sqlite-delivery-struct
            (sqlite:execute-to-list
             handle
             (format nil "select ~a from deliveries order by sequence asc"
                     +event-sqlite-delivery-columns+)))))

(defmethod event-store-begin-delivery ((store sqlite-event-store) delivery)
  ;; A conditional update, so two workers that both read the row as queued
  ;; cannot both proceed: SQLite applies one and reports zero changes to the
  ;; other. The struct is only mutated when this worker actually won, which
  ;; is what keeps a losing worker from writing its stale copy back later.
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "update deliveries set status = 'running' where id = ? and status = 'queued'"
     (%event-delivery-id delivery))
    (when (plusp (sqlite:execute-single handle "select changes()"))
      (setf (%event-delivery-status delivery) "running"
            (%event-delivery-size delivery) 0)
      t)))

(defmethod event-store-commit-fenced ((store sqlite-event-store) lease-key generation thunk)
  ;; The whole point of this method is that the check and the writes are one
  ;; transaction. BEGIN IMMEDIATE is taken first, so by the time the lease
  ;; row is read no other connection can write this database; the thunk's
  ;; writes then join the same transaction and land or roll back together.
  ;; Checking the lease with a separate SELECT and writing afterwards -- what
  ;; this replaced -- left a window where a steal landed in between and the
  ;; stale holder still overwrote the thief's result.
  (%with-event-sqlite (handle store)
    (if (plusp (sqlite:execute-single
                handle
                "select count(*) from leases
                 where lease_key = ? and owner = ? and generation = ?"
                lease-key (event-store-worker store) generation))
        (progn (funcall thunk) t)
        nil)))

(defmethod event-store-fence ((store sqlite-event-store) lease-key generation)
  (event-store-fence-ok-p store lease-key generation))

(defmethod event-store-claim-instance ((store sqlite-event-store) lease-key)
  ;; The runtime's per-instance dispatch lease is exactly the lease table:
  ;; two runtimes on one file contend for this row, and Core decides.
  (event-store-acquire-lease store lease-key))

(defmethod event-store-release-instance ((store sqlite-event-store) lease-key generation)
  ;; Owner AND generation. See the generic's docstring: a fenced-out dispatch
  ;; whose worker id now holds a later generation would otherwise release the
  ;; lease it is currently holding for a different dispatch.
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "update leases set owner = '', expires_at = 0
             where lease_key = ? and owner = ? and generation = ?"
     lease-key (event-store-worker store) generation)
    (plusp (sqlite:execute-single handle "select changes()"))))

(defmethod event-store-coordination-descriptor ((store sqlite-event-store))
  ;; Deliberately NOT EVENT-STORE-DESCRIPTOR. That descriptor is the store's
  ;; claim about its own lease table, which is true. The runtime descriptor
  ;; is a claim about dispatch, and dispatch is only safe for more than one
  ;; worker once a stolen lease can no longer write back -- see
  ;; EVENT-STORE-FENCE-OK-P and the fencing checks in
  ;; tests/event-store-conformance.lisp. Until a store reports fencing, the
  ;; runtime claim stays at the volatile default, because an overstated
  ;; coordination value is worse than a missing one.
  (if (event-store-fencing-p store)
      (event-store-descriptor store)
      :null))

(defmethod event-store-has-delivery ((store sqlite-event-store) delivery-id)
  (%with-event-sqlite (handle store)
    (plusp (sqlite:execute-single handle
                                  "select count(*) from deliveries where id = ?" delivery-id))))

(defmethod event-store-commit-delivery ((store sqlite-event-store) delivery)
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "update deliveries set status = ?, available_at = ?, attempt = ?,
             identity_scope = ?, trust = ?, run_id = ? where id = ?"
     (%event-delivery-status delivery) (%event-delivery-available-at delivery)
     (%event-delivery-attempt delivery) (%event-delivery-identity-scope delivery)
     (%event-delivery-trust delivery) (%event-delivery-run-id delivery)
     (%event-delivery-id delivery)))
  nil)

(defmethod event-store-release-delivery ((store sqlite-event-store) delivery)
  ;; Queue bytes are a capacity notion the persistent store does not bound;
  ;; the row stays, so there is nothing to release beyond the struct's own
  ;; accounting.
  (setf (%event-delivery-size delivery) 0)
  nil)

(defmethod event-store-requeue-delivery ((store sqlite-event-store) delivery available-at)
  ;; Only a row this worker is actually running may be put back. A worker
  ;; holding a delivery struct it read while the row was queued would
  ;; otherwise revive finished work: the requeue would write "queued" over
  ;; another worker's "succeeded" and the program would run a second time.
  (%with-event-sqlite (handle store)
    (sqlite:execute-non-query
     handle "update deliveries set status = 'queued', available_at = ?, attempt = ?
             where id = ? and status = 'running'"
     available-at (%event-delivery-attempt delivery) (%event-delivery-id delivery))
    (when (plusp (sqlite:execute-single handle "select changes()"))
      (setf (%event-delivery-status delivery) "queued"
            (%event-delivery-available-at delivery) available-at)
      t)))

(defmethod event-store-runs ((store sqlite-event-store))
  (%with-event-sqlite (handle store)
    (mapcar
     (lambda (row)
       (destructuring-bind (id delivery-id route-id target-id instance-key status attempt
                            output error)
           row
         (let ((run (make-instance 'event-run :id id :delivery-id delivery-id
                                              :route-id route-id
                                              :target-id (or target-id :null)
                                              :instance-key instance-key)))
           (setf (event-run-status run) status
                 (event-run-attempt run) attempt
                 (event-run-output run) (if output (parse-json output) :null)
                 (event-run-error run) (or error :null))
           run)))
     (sqlite:execute-to-list
      handle
      "select id, delivery_id, route_id, target_id, instance_key, status, attempt,
              output, error from runs order by sequence asc"))))

(defmethod event-store-save-run ((store sqlite-event-store) run)
  (event-store-put-run store run))

(defmethod event-store-dead-letters ((store sqlite-event-store))
  (%with-event-sqlite (handle store)
    (mapcar
     (lambda (row)
       (destructuring-bind (id delivery-id reason run-id sink-id) row
         (make-instance 'event-dead-letter :id id :delivery-id delivery-id :reason reason
                                           :run-id (or run-id :null)
                                           :sink-id (or sink-id :null))))
     (sqlite:execute-to-list
      handle
      "select id, delivery_id, reason, run_id, sink_id from dead_letters
       order by sequence asc"))))

(defmethod event-store-save-dead-letter ((store sqlite-event-store) dead-letter)
  (event-store-put-dead-letter store dead-letter))

(defmethod event-store-forget-dead-letter ((store sqlite-event-store) dead-letter-id)
  (event-store-remove-dead-letter store dead-letter-id))

(defmethod event-store-continuations ((store sqlite-event-store))
  (%with-event-sqlite (handle store)
    (mapcar
     (lambda (row)
       (destructuring-bind (id target-id instance-key identity-scope correlation metadata
                            completed expires-at)
           row
         (let ((continuation (make-instance 'event-continuation
                                            :id id :target-id target-id
                                            :instance-key instance-key
                                            :identity-scope identity-scope
                                            :correlation (parse-json correlation)
                                            :metadata (and metadata (parse-json metadata))
                                            :expires-at (or expires-at :null))))
           (setf (event-continuation-completed-p continuation) (plusp completed))
           continuation)))
     (sqlite:execute-to-list
      handle
      "select id, target_id, instance_key, identity_scope, correlation, metadata,
              completed, expires_at from continuations order by sequence asc"))))

(defmethod event-store-save-continuation ((store sqlite-event-store) continuation)
  (event-store-put-continuation store continuation))

(defmethod event-store-captured-state ((store sqlite-event-store) state-key)
  (event-store-program-state store state-key))

(defmethod event-store-save-captured-state ((store sqlite-event-store) state-key value)
  (event-store-put-program-state store state-key value))
