;;;; event.lisp --- the native event-runtime boundaries.
;;;;
;;;; Ax's event semantics live in Core (ir/axcore/event.axir, emitted into
;;;; src/core.lisp). This file owns only what a portable IR cannot express:
;;;; the clock, the store, the inbox's mutable delivery records, source and
;;;; sink boundaries, cooperative cancellation, program state capture, and
;;;; the inline dispatch loop that moves a delivery between the states Core
;;;; names.
;;;;
;;;; Every routing, retry, debounce, capacity, ordering, path-resolution,
;;;; input-mapping and continuation-matching decision is a Core call. When
;;;; you are tempted to add an `if' about event policy here, add it to Core
;;;; instead; the only `if's below are about threads, time and storage.
;;;;
;;;; CONTRACT, stated plainly so nobody reads more into it than is here:
;;;;
;;;;   durability     volatile       the store is in-memory and per-process
;;;;   coordination   single-worker  no worker threads are created
;;;;   implicitWake   false          a route must say wake or resume
;;;;
;;;; This is the same `axevent.single-worker' contract the other generated
;;;; Ax packages expose, and it is NOT the TypeScript runtime's persistent
;;;; multi-worker contract. PUBLISH drains whatever is due at
;;;; (EVENT-CLOCK-NOW clock) on the calling thread. A host schedules later
;;;; work itself: ask EVENT-RUNTIME-NEXT-DUE-AT when to come back, then
;;;; call EVENT-RUNTIME-RUN-DUE. There is no timer thread, no leader
;;;; election, no cross-process lease, and no durable queue.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Conditions
;;; ------------------------------------------------------------------

(define-condition event-error (ax-error) ()
  (:documentation "An event runtime failure."))

(define-condition event-input-error (event-error) ()
  (:documentation
   "An event could not be turned into a program input: an unsafe path
segment, a mapping that produced no value for a required signature input, or
a mapped value that does not satisfy its field's type."))

(define-condition event-backpressure-error (event-error) ()
  (:documentation "The inbox was full for longer than the publish timeout."))

(defun %event-fail (type format-control &rest arguments)
  (error type :message (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; Cancellation
;;; ------------------------------------------------------------------
;;;
;;; The token itself is ai.lisp's CANCELLATION-TOKEN, and there is exactly
;;; one of them in the image. An earlier version of this file defined its
;;; own class of the same name, which silently replaced the AI layer's and
;;; took its accessor methods with it; a provider run then failed with "no
;;; applicable method for %TOKEN-CANCELLED". Two independent tokens would
;;; have been worse than the collision: an MCP request cancelled by the
;;; caller's AI token has to actually stop, so the token has to be shared.
;;;
;;; What remains here is a facade, for two reasons that are not cosmetic:
;;; the event and MCP layers report a cancelled operation as EVENT-ERROR or
;;; MCP-ERROR rather than the AI layer's :ABORTED PROVIDER-ERROR, and a
;;; reason that was never set has to read as JSON :NULL rather than NIL,
;;; because it is serialized into run and delivery records.

(defun make-cancellation-token ()
  "A fresh token. The class is ai.lisp's; this is the event-side spelling."
  (cancellation-token))

(defun cancellation-token-cancelled-p (token)
  (and token (cancelled-p token)))

(defun cancellation-token-reason (token)
  "TOKEN's reason, or :NULL when it has none.

:NULL rather than NIL because this value is serialized into run records,
where an absent reason must encode as JSON null."
  (let ((reason (and token (cancellation-reason token))))
    (if reason reason :null)))

(defun cancellation-token-cancel (token &optional (reason "cancelled"))
  "Cancel TOKEN with REASON. True the first time only."
  (and token (cancel token reason)))

(defun cancellation-token-subscribe (token callback)
  "Call CALLBACK once when TOKEN is cancelled; return a remover."
  (cancellation-subscribe token callback))

(defun cancellation-token-subscription-count (token)
  (cancellation-subscription-count token))

(defun cancellation-token-throw-if-cancelled (token)
  "Signal EVENT-ERROR when TOKEN is cancelled.

Deliberately not the AI layer's THROW-IF-CANCELLED: a cancelled event run
or MCP request is an event/MCP boundary failure, and callers of this file
handle those conditions rather than PROVIDER-ERROR."
  (when (and token (cancelled-p token))
    (%event-fail 'event-error "Operation cancelled: ~a"
                 (let ((reason (cancellation-reason token)))
                   (if (stringp reason) reason "cancelled"))))
  nil)

(defun cancellation-token-wait (token seconds)
  "Wait up to SECONDS for TOKEN. True when it was cancelled, NIL on timeout."
  (and (cancellation-wait token (/ (%event-sleep-milliseconds seconds) 1000)) t))

(defun %event-sleep-milliseconds (seconds)
  "SECONDS as whole milliseconds, exactly as the caller meant them.

RATIONALIZE, not FLOAT, is the right conversion here. A caller writing
0.001 writes a single float whose value is 0.001000000047...; widening that
to a double and multiplying by 1000 gives 1.0000000474..., so a manual
clock advanced by exactly 1 would never reach the target and the sleep
would hang. RATIONALIZE recovers the 1/1000 the caller wrote."
  (let ((value (max 0 seconds)))
    (* 1000 (if (floatp value) (rationalize value) value))))

;;; ------------------------------------------------------------------
;;; Clocks
;;; ------------------------------------------------------------------

(defgeneric event-clock-now (clock)
  (:documentation "CLOCK's current time, in milliseconds."))

(defgeneric event-clock-sleep (clock seconds &optional cancellation)
  (:documentation
   "Sleep SECONDS on CLOCK. True when the sleep completed, NIL when
CANCELLATION cancelled it. The wake subscription is removed either way."))

(defclass system-event-clock () ()
  (:documentation "Wall-clock time and real sleeping."))

(defun make-system-event-clock () (make-instance 'system-event-clock))

(defmethod event-clock-now ((clock system-event-clock))
  (* 1000d0 (/ (get-internal-real-time) internal-time-units-per-second)))

(defmethod event-clock-sleep ((clock system-event-clock) seconds &optional cancellation)
  (cond ((null cancellation) (sleep (/ (%event-sleep-milliseconds seconds) 1000)) t)
        ((cancellation-token-cancelled-p cancellation) nil)
        (t (not (cancellation-token-wait cancellation seconds)))))

(defclass manual-event-clock ()
  ((now :initarg :now :initform 0 :accessor %manual-now)
   (sleepers :initform 0 :accessor %manual-sleepers)
   (lock :initform (sb-thread:make-mutex :name "ax-manual-clock") :reader %manual-lock)
   (gate :initform (sb-thread:make-waitqueue) :reader %manual-gate))
  (:documentation
   "A clock that only moves when MANUAL-CLOCK-ADVANCE is called, so debounce,
retry backoff and continuation expiry are deterministic in tests."))

(defun make-manual-event-clock (&optional (now 0))
  (make-instance 'manual-event-clock :now (float now 1d0)))

(defmethod event-clock-now ((clock manual-event-clock))
  (sb-thread:with-mutex ((%manual-lock clock)) (%manual-now clock)))

(defun manual-clock-advance (clock milliseconds)
  "Move CLOCK forward and wake every sleeper."
  (sb-thread:with-mutex ((%manual-lock clock))
    (incf (%manual-now clock) (float milliseconds 1d0))
    (sb-thread:condition-broadcast (%manual-gate clock)))
  (event-clock-now clock))

(defun manual-clock-wait-for-sleepers (clock &optional (count 1))
  "Block until at least COUNT threads are sleeping on CLOCK."
  (sb-thread:with-mutex ((%manual-lock clock))
    (loop until (>= (%manual-sleepers clock) count)
          do (sb-thread:condition-wait (%manual-gate clock) (%manual-lock clock)))))

(defmethod event-clock-sleep ((clock manual-event-clock) seconds &optional cancellation)
  (let* ((target (+ (event-clock-now clock) (%event-sleep-milliseconds seconds)))
         (wake (lambda ()
                 (sb-thread:with-mutex ((%manual-lock clock))
                   (sb-thread:condition-broadcast (%manual-gate clock)))))
         (remove (if cancellation
                     (cancellation-token-subscribe cancellation wake)
                     (lambda () nil))))
    (unwind-protect
         (sb-thread:with-mutex ((%manual-lock clock))
           (incf (%manual-sleepers clock))
           (sb-thread:condition-broadcast (%manual-gate clock))
           (unwind-protect
                (loop
                  (when (and cancellation (cancellation-token-cancelled-p cancellation))
                    (return nil))
                  (when (>= (%manual-now clock) target) (return t))
                  (sb-thread:condition-wait (%manual-gate clock) (%manual-lock clock)))
             (decf (%manual-sleepers clock))))
      (funcall remove))))

;;; ------------------------------------------------------------------
;;; Envelopes and paths
;;; ------------------------------------------------------------------

(defclass event-envelope ()
  ((id :initarg :id :initform "" :reader event-envelope-id)
   (source :initarg :source :initform "" :reader event-envelope-source)
   (event-type :initarg :type :initform "" :reader event-envelope-type)
   (data :initarg :data :initform :null :reader event-envelope-data)
   (subject :initarg :subject :initform :null :reader event-envelope-subject)
   (specversion :initarg :specversion :initform "1.0" :reader event-envelope-specversion)
   (extensions :initarg :extensions :initform nil :reader event-envelope-extensions)
   (correlation :initarg :correlation :initform nil :reader event-envelope-correlation))
  (:documentation "One CloudEvents-shaped ingress event."))

(defun make-event-envelope (id source type &key (data :null) (subject :null)
                                                (specversion "1.0")
                                                extensions correlation)
  "Build an event envelope.

EXTENSIONS is a JSON object or NIL. CORRELATION is a JSON array of
{kind, value} objects, or NIL; those keys are what a resume route matches
against a registered continuation."
  (make-instance 'event-envelope :id id :source source :type type :data data
                                 :subject subject :specversion specversion
                                 :extensions extensions :correlation correlation))

(defun %event-array (value)
  (cond ((null value) (%new-array))
        ((%array-p value) value)
        ((listp value) (coerce value 'vector))
        (t (%new-array))))

(defun event-envelope-object (envelope)
  "ENVELOPE as the JSON object Core reads.

An absent subject, data, extensions or correlation stays absent rather than
becoming an explicit null, because Core distinguishes the two."
  (let ((out (object "specversion" (event-envelope-specversion envelope)
                     "id" (event-envelope-id envelope)
                     "source" (event-envelope-source envelope)
                     "type" (event-envelope-type envelope))))
    (unless (eq (event-envelope-subject envelope) :null)
      (%set-key out "subject" (event-envelope-subject envelope)))
    (unless (eq (event-envelope-data envelope) :null)
      (%set-key out "data" (event-envelope-data envelope)))
    (let ((extensions (event-envelope-extensions envelope)))
      (when (and (hash-table-p extensions) (plusp (hash-table-count extensions)))
        (%set-key out "extensions" extensions)))
    (let ((correlation (%event-array (event-envelope-correlation envelope))))
      (when (plusp (length correlation))
        (%set-key out "correlation" correlation)))
    out))

(defun %event-envelope-of (value)
  "VALUE as an EVENT-ENVELOPE, accepting the JSON object form too."
  (cond ((typep value 'event-envelope) value)
        ((hash-table-p value)
         (make-event-envelope (%event-text (jget value "id"))
                              (%event-text (jget value "source"))
                              (%event-text (jget value "type"))
                              :data (jget value "data")
                              :subject (jget value "subject")
                              :specversion (let ((v (jget value "specversion")))
                                             (if (stringp v) v "1.0"))
                              :extensions (let ((v (jget value "extensions")))
                                            (and (hash-table-p v) v))
                              :correlation (jget value "correlation")))
        (t (%event-fail 'event-error "publish: ~S is not an event envelope" value))))

(defun %event-text (value)
  (cond ((stringp value) value)
        ((eq value :null) "")
        ((null value) "")
        (t (axllm/core::core-js-text value))))

(defparameter +unsafe-path-segments+ '("__proto__" "constructor" "prototype")
  "Segment names a path may never carry, so a mapping cannot reach a
prototype slot in any Ax port's object model.")

(defclass event-path ()
  ((root :initarg :root :reader event-path-root)
   (segments :initarg :segments :initform '() :reader event-path-segments)
   (correlation-kind :initarg :correlation-kind :initform :null
                     :reader event-path-correlation-kind)
   (value :initarg :value :initform :null :reader event-path-value))
  (:documentation "A segment-safe selector into an ingress event."))

(defun %event-check-path-segments (segments)
  (dolist (segment segments)
    (when (or (and (stringp segment)
                   (or (zerop (length segment))
                       (member segment +unsafe-path-segments+ :test #'string=)))
              (and (integerp segment) (minusp segment))
              (not (or (stringp segment) (integerp segment))))
      (%event-fail 'event-input-error "Unsafe event path segment: ~S" segment)))
  segments)

(defun %event-make-event-path (root segments &key (correlation-kind :null) (value :null))
  (make-instance 'event-path :root root
                             :segments (%event-check-path-segments segments)
                             :correlation-kind correlation-kind
                             :value value))

(defun event-path-data (&rest segments)
  "The event's data, then SEGMENTS."
  (%event-make-event-path "data" segments))

(defun event-path-envelope (&rest segments)
  "The whole envelope, then SEGMENTS."
  (%event-make-event-path "envelope" segments))

(defun event-path-extension (name)
  "One CloudEvents extension."
  (%event-make-event-path "extensions" (list name)))

(defun event-path-identity (&rest segments)
  "The verified ingress identity, then SEGMENTS."
  (%event-make-event-path "identity" segments))

(defun event-path-trust ()
  "The ingress trust level."
  (%event-make-event-path "trust" '()))

(defun event-path-correlation (kind)
  "The value of the correlation key of KIND."
  (%event-make-event-path "correlation" '() :correlation-kind kind))

(defun event-path-continuation (&rest segments)
  "The resumed continuation's metadata, then SEGMENTS."
  (%event-make-event-path "continuation" segments))

(defun event-path-constant (value)
  "A literal VALUE, independent of the event."
  (%event-make-event-path "constant" '() :value value))

(defun event-path-subject ()
  "The envelope's subject."
  (%event-make-event-path "envelope" '("subject")))

(defun event-path-object (path)
  "PATH as the JSON object Core reads."
  (let ((out (object "root" (event-path-root path)
                     "segments" (coerce (event-path-segments path) 'vector))))
    (unless (eq (event-path-correlation-kind path) :null)
      (%set-key out "correlationKind" (event-path-correlation-kind path)))
    (when (string= (event-path-root path) "constant")
      (%set-key out "value" (event-path-value path)))
    out))

;;; ------------------------------------------------------------------
;;; Input plans
;;; ------------------------------------------------------------------

(defclass event-input-plan ()
  ((project :initarg :project :initform :null :reader event-input-plan-project)
   (fields :initarg :fields :initform '() :reader event-input-plan-fields))
  (:documentation
   "A declarative, callback-free mapping from an ingress event to signature
inputs. PROJECT is a same-name projection; FIELDS is an ordered alist of
signature input name to path, and overrides the projection."))

(defun event-input-plan (&key project fields)
  "Build an input plan.

FIELDS is an alist of (name . path). A blank or unsafe name, or a name
mapped twice, is an error rather than a silent last-one-wins."
  (let ((seen '()))
    (dolist (entry fields)
      (let ((name (car entry)))
        (when (or (not (stringp name)) (zerop (length name))
                  (member name +unsafe-path-segments+ :test #'string=))
          (%event-fail 'event-input-error "Unsafe target field: ~S" name))
        (when (member name seen :test #'string=)
          (%event-fail 'event-input-error "Event input field ~a is mapped more than once" name))
        (push name seen)
        (unless (typep (cdr entry) 'event-path)
          (%event-fail 'event-input-error "Event input field ~a needs an event path" name)))))
  (when (and project (not (typep project 'event-path)))
    (%event-fail 'event-input-error "An event input projection needs an event path"))
  (make-instance 'event-input-plan :project (or project :null) :fields fields))

(defun event-input-plan-object (plan)
  "PLAN as the JSON object Core reads."
  (let ((fields (%new-array)))
    (dolist (entry (event-input-plan-fields plan))
      (vector-push-extend (object "field" (car entry)
                                  "path" (event-path-object (cdr entry)))
                          fields))
    (object "project" (let ((project (event-input-plan-project plan)))
                        (if (eq project :null) :null (event-path-object project)))
            "fields" fields)))

(defun %event-plan-of (value)
  (cond ((typep value 'event-input-plan) value)
        ((functionp value) (%event-plan-of (funcall value)))
        (t (%event-fail 'event-input-error "Event input mapping did not produce a plan"))))

;;; ------------------------------------------------------------------
;;; Routes and targets
;;; ------------------------------------------------------------------

(defclass event-target ()
  ((id :initarg :id :reader event-target-id)
   (invoke :initarg :invoke :reader event-target-invoke)
   (map-input :initarg :map-input :initform nil :reader event-target-map-input)
   (sinks :initarg :sinks :initform '() :reader event-target-sinks)
   (retry-safety :initarg :retry-safety :initform "unknown" :reader event-target-retry-safety)
   (wait-for :initarg :wait-for :initform '() :reader event-target-wait-for)
   (capture-state :initarg :capture-state :initform nil :reader event-target-capture-state)
   (restore-state :initarg :restore-state :initform nil :reader event-target-restore-state)
   (signature :initarg :signature :initform nil :reader event-target-signature)
   (input :initarg :input :initform nil :reader event-target-input)
   (wake-input :initarg :wake-input :initform nil :reader event-target-wake-input)
   (resume-input :initarg :resume-input :initform nil :reader event-target-resume-input))
  (:documentation "What a wake or resume route invokes."))

(defun event-target (id &key invoke signature input wake-input resume-input
                             map-input sinks (retry-safety "unknown") wait-for
                             capture-state restore-state)
  "Build an event target.

INVOKE is called with (input context) and returns the program output.
CONTEXT is a JSON object carrying runId, deliveryId, instanceKey,
identityScope, idempotencyKey, cancellation and continuation.

Declarative mappings (INPUT, WAKE-INPUT, RESUME-INPUT) need a SIGNATURE and
exclude MAP-INPUT: a callback is an escape hatch, not a validation bypass.
WAIT-FOR is a list of (kind path &key metadata expires-in-ms) continuation
declarations registered after a successful invocation.

RETRY-SAFETY must be \"idempotent\" before the runtime will retry an
invocation that may already have had an effect."
  (unless (functionp invoke)
    (%event-fail 'event-error "Event target ~a requires an invoker" id))
  (let ((plans (remove nil (list input wake-input resume-input))))
    (when (and plans map-input)
      (%event-fail 'event-error
                   "Event target ~a: declarative mappings and map-input are mutually exclusive" id))
    (when (and plans (null signature))
      (%event-fail 'event-error "Event target ~a: declarative event mappings require a signature" id)))
  (make-instance 'event-target
                 :id id :invoke invoke :signature signature
                 :input (and input (%event-plan-of input))
                 :wake-input (and wake-input (%event-plan-of wake-input))
                 :resume-input (and resume-input (%event-plan-of resume-input))
                 :map-input map-input
                 :sinks (if (listp sinks) sinks (coerce sinks 'list))
                 :retry-safety retry-safety
                 :wait-for wait-for
                 :capture-state capture-state
                 :restore-state restore-state))

(defclass event-route ()
  ((id :initarg :id :reader event-route-id)
   (action :initarg :action :reader event-route-action)
   (match :initarg :match :reader event-route-match)
   (target-id :initarg :target-id :initform :null :reader event-route-target-id)
   (require-authenticated :initarg :require-authenticated :initform nil
                          :reader event-route-require-authenticated-p)
   (ordering :initarg :ordering :initform "strict" :reader event-route-ordering)
   (debounce-ms :initarg :debounce-ms :initform 0 :reader event-route-debounce-ms)
   (instance-key :initarg :instance-key :initform :null :reader event-route-instance-key))
  (:documentation "One explicit decision about one class of events."))

(defparameter +event-actions+ '("observe" "invalidate" "wake" "resume"))

(defun event-route (id &key action types sources authenticated target
                            (ordering "strict") (debounce-ms 0) instance-key)
  "Build an event route.

ACTION must be one of observe, invalidate, wake or resume; only the last two
invoke a program. TARGET is an EVENT-TARGET or a target id, and is required
for wake. TYPES and SOURCES are lists; an omitted list matches everything."
  (unless (member action +event-actions+ :test #'equal)
    (%event-fail 'event-error "Event route ~a needs one of ~{~a~^, ~}" id +event-actions+))
  (let ((match (object)))
    (when types (%set-key match "types" (coerce types 'vector)))
    (when sources (%set-key match "sources" (coerce sources 'vector)))
    (when (and (string= action "wake") (null target))
      (%event-fail 'event-error "Event route ~a wakes nothing" id))
    (make-instance 'event-route
                   :id id :action action :match match
                   :target-id (cond ((null target) :null)
                                    ((typep target 'event-target) (event-target-id target))
                                    (t target))
                   :require-authenticated (and authenticated t)
                   :ordering ordering
                   :debounce-ms debounce-ms
                   :instance-key (or instance-key :null))))

(defun event-route-object (route)
  "ROUTE as the JSON object Core reads."
  (object "id" (event-route-id route)
          "action" (event-route-action route)
          "match" (event-route-match route)
          "targetId" (event-route-target-id route)
          "requireAuthenticated" (json-boolean (event-route-require-authenticated-p route))
          "ordering" (event-route-ordering route)
          "debounceMs" (event-route-debounce-ms route)
          "instanceKey" (let ((key (event-route-instance-key route)))
                          (if (eq key :null) :null (event-path-object key)))))

;;; ------------------------------------------------------------------
;;; Sinks and sources
;;; ------------------------------------------------------------------

(defgeneric event-sink-id (sink)
  (:documentation "SINK's stable id, used for isolated redrive.")
  (:method ((sink t)) "sink"))

(defgeneric event-sink-write (sink output context)
  (:documentation
   "Deliver OUTPUT. CONTEXT carries the stored run and a per-sink
idempotency key. The run, including its output, is already persisted."))

(defclass function-event-sink ()
  ((id :initarg :id :initform "sink" :reader event-sink-id)
   (write-function :initarg :write :reader %sink-write-function)))

(defun make-event-sink (&key (id "sink") write)
  "A sink that calls WRITE with (output context)."
  (unless (functionp write)
    (%event-fail 'event-error "Event sink ~a requires a write function" id))
  (make-instance 'function-event-sink :id id :write write))

(defmethod event-sink-write ((sink function-event-sink) output context)
  (funcall (%sink-write-function sink) output context))

(defgeneric event-source-start (source publish)
  (:documentation
   "Start SOURCE. PUBLISH is called with (envelope &key identity-scope trust)
and only enqueues; a source never invokes a program."))

(defgeneric event-source-close (source)
  (:documentation "Release SOURCE's protocol resources.")
  (:method ((source t)) nil))

(defclass push-event-source ()
  ((id :initarg :id :initform "push" :reader event-source-id)
   (publish :initform nil :accessor %push-publish))
  (:documentation "An application-driven source: the host calls PUBLISH."))

(defun make-push-event-source (&optional (id "push"))
  (make-instance 'push-event-source :id id))

(defmethod event-source-start ((source push-event-source) publish)
  (setf (%push-publish source) publish)
  source)

(defmethod event-source-close ((source push-event-source))
  (setf (%push-publish source) nil))

(defun push-event-source-publish (source envelope &key (identity-scope "anonymous")
                                                       (trust "untrusted"))
  (let ((publish (%push-publish source)))
    (unless publish
      (%event-fail 'event-error "push event source ~a is not started" (event-source-id source)))
    (funcall publish envelope :identity-scope identity-scope :trust trust)))

;;; ------------------------------------------------------------------
;;; Runs, dead letters and continuations
;;; ------------------------------------------------------------------

(defclass event-run ()
  ((id :initarg :id :reader event-run-id)
   (delivery-id :initarg :delivery-id :reader event-run-delivery-id)
   (route-id :initarg :route-id :reader event-run-route-id)
   (target-id :initarg :target-id :reader event-run-target-id)
   (instance-key :initarg :instance-key :reader event-run-instance-key)
   (status :initform "queued" :accessor event-run-status)
   (attempt :initform 0 :accessor event-run-attempt)
   (output :initform :null :accessor event-run-output)
   (error-text :initform :null :accessor event-run-error)
   (continuation-ids :initform '() :accessor event-run-continuation-ids))
  (:documentation "One attempt-tracked invocation of a target."))

(defclass event-dead-letter ()
  ((id :initarg :id :reader event-dead-letter-id)
   (delivery-id :initarg :delivery-id :reader event-dead-letter-delivery-id)
   (reason :initarg :reason :reader event-dead-letter-reason)
   (run-id :initarg :run-id :initform :null :reader event-dead-letter-run-id)
   (sink-id :initarg :sink-id :initform :null :reader event-dead-letter-sink-id))
  (:documentation
   "A delivery that cannot proceed. A sink-id means only that sink failed;
redriving it retries the sink alone and never re-invokes the program."))

(defclass event-continuation ()
  ((id :initarg :id :reader event-continuation-id)
   (target-id :initarg :target-id :reader event-continuation-target-id)
   (instance-key :initarg :instance-key :reader event-continuation-instance-key)
   (identity-scope :initarg :identity-scope :reader event-continuation-identity-scope)
   (correlation :initarg :correlation :reader event-continuation-correlation)
   (metadata :initarg :metadata :initform nil :reader event-continuation-metadata)
   (completed :initform nil :accessor event-continuation-completed-p)
   (expires-at :initarg :expires-at :initform :null :reader event-continuation-expires-at))
  (:documentation "A registered wait that a resume route can match."))

(defun event-continuation-object (continuation)
  "CONTINUATION as the JSON object Core reads."
  (object "id" (event-continuation-id continuation)
          "targetId" (event-continuation-target-id continuation)
          "instanceKey" (event-continuation-instance-key continuation)
          "identityScope" (event-continuation-identity-scope continuation)
          "correlation" (event-continuation-correlation continuation)
          "metadata" (or (event-continuation-metadata continuation) (object))
          "completed" (json-boolean (event-continuation-completed-p continuation))
          "expiresAt" (event-continuation-expires-at continuation)))

;;; ------------------------------------------------------------------
;;; Store
;;; ------------------------------------------------------------------

(defstruct (event-delivery (:conc-name %event-delivery-))
  id envelope command (status "queued") (available-at 0) (sequence 0) (size 0)
  (attempt 0) (identity-scope "anonymous") (trust "untrusted") (run-id nil)
  ;; The instance lease held while this delivery is dispatching, so the
  ;; cleanup releases exactly what it took, and the generation it was held
  ;; at, so a lease stolen mid-dispatch fences this worker's write-back.
  (dispatch-lease nil) (dispatch-generation 0)
  ;; Writes this dispatch intends to make, held until the fenced commit.
  ;; Captured state and declared continuations used to be written the moment
  ;; the program produced them, which put them outside the fence: a worker
  ;; whose lease had been stolen still left its state and continuations
  ;; behind. They are queued here and applied inside the commit instead.
  (pending-writes '()))

(defgeneric event-store-enqueue (store envelope commands &key available-at)
  (:documentation "Admit one delivery per command, honouring capacity."))

(defclass in-memory-event-store ()
  ((deliveries :initform (make-hash-table :test #'equal) :reader %store-deliveries)
   (order :initform (make-array 0 :adjustable t :fill-pointer 0) :reader %store-order)
   (runs :initform (make-hash-table :test #'equal) :reader %store-runs)
   (run-order :initform (make-array 0 :adjustable t :fill-pointer 0) :reader %store-run-order)
   (dead-letters :initform (make-hash-table :test #'equal) :reader %store-dead-letters)
   (dead-order :initform (make-array 0 :adjustable t :fill-pointer 0) :reader %store-dead-order)
   (continuations :initform (make-hash-table :test #'equal) :reader %store-continuations)
   (continuation-order :initform (make-array 0 :adjustable t :fill-pointer 0)
                       :reader %store-continuation-order)
   (program-state :initform (make-hash-table :test #'equal) :reader %store-program-state)
   (max-pending :initarg :max-pending :initform 10000 :reader event-store-max-pending)
   (max-queued-bytes :initarg :max-queued-bytes :initform (* 64 1024 1024)
                     :reader event-store-max-queued-bytes)
   (max-envelope-bytes :initarg :max-envelope-bytes :initform (* 1024 1024)
                       :reader event-store-max-envelope-bytes)
   (publish-timeout-ms :initarg :publish-timeout-ms :initform 5000
                       :reader event-store-publish-timeout-ms)
   (clock :initarg :clock :initform nil :reader event-store-clock)
   (queued-bytes :initform 0 :accessor %store-queued-bytes)
   (sequence :initform 0 :accessor %store-sequence)
   (lock :initform (sb-thread:make-mutex :name "ax-event-store") :reader %store-lock))
  (:documentation
   "The volatile single-worker store. Bounds are the generated-package
defaults: 10,000 pending deliveries, 64 MiB queued, 1 MiB per envelope and a
five-second publication wait."))

(defun make-in-memory-event-store (&key (max-pending 10000)
                                        (max-queued-bytes (* 64 1024 1024))
                                        (max-envelope-bytes (* 1024 1024))
                                        (publish-timeout-ms 5000)
                                        clock)
  (make-instance 'in-memory-event-store
                 :max-pending max-pending :max-queued-bytes max-queued-bytes
                 :max-envelope-bytes max-envelope-bytes
                 :publish-timeout-ms publish-timeout-ms
                 :clock (or clock (make-system-event-clock))))

(defun %event-ordered-values (table order)
  "TABLE's live values in insertion order."
  (let ((out '()))
    (loop for key across order
          do (multiple-value-bind (value found) (gethash key table)
               (when found (push value out))))
    (nreverse out)))

(defun %event-store-put (table order key value)
  (unless (nth-value 1 (gethash key table))
    (vector-push-extend key order))
  (setf (gethash key table) value))

;;; The runtime reaches a store only through the generics below, so a
;;; persistent store can be substituted without touching dispatch. The
;;; in-memory methods are the behaviour this package shipped with. A
;;; persistent store overrides the same set and also implements the
;;; write-through points, because it cannot rely on the runtime having
;;; mutated a struct it still owns.

(defgeneric event-store-deliveries (store)
  (:documentation "Every delivery the store holds, in admission order."))

(defgeneric event-store-runs (store)
  (:documentation "Every run the store holds, in creation order."))

(defgeneric event-store-dead-letters (store)
  (:documentation "Every dead letter the store holds, in creation order."))

(defgeneric event-store-continuations (store)
  (:documentation "Every continuation the store holds, in creation order."))

(defgeneric event-store-claim-instance (store lease-key)
  (:documentation
   "Take the dispatch lease for LEASE-KEY. Returns Core's lease transition.

Strict per-instance ordering is only a real guarantee while one worker at a
time dispatches an instance. A volatile single-worker store has nobody to
coordinate with, so it grants unconditionally; a persistent store asks Core
to judge the lease row another process may already hold.")
  (:method ((store t) lease-key)
    (declare (ignore lease-key))
    (object "action" "claim" "granted" true)))

(defgeneric event-store-release-instance (store lease-key generation)
  (:documentation
   "Give up the dispatch lease for LEASE-KEY, held at GENERATION.

The generation is part of the protocol because releasing by owner alone is
wrong for the one worker most likely to do it. A dispatch that was fenced
out still runs its cleanup, and by then the same worker id may legitimately
hold a LATER generation of the same lease -- after a thief stole it,
released it, and this worker reclaimed it. An owner-only release would
unlock an instance this worker is currently inside. Releasing only the
generation the dispatch actually took leaves the current holder alone.

This is distinct from rejecting a stale commit: the commit boundary stops a
fenced-out worker from WRITING, and this stops it from UNLOCKING.

EVENT-STORE-RELEASE-LEASE stays owner-only on purpose; it is the explicit
administrative release, for an operator clearing a lease rather than a
dispatch ending.")
  (:method ((store t) lease-key generation)
    (declare (ignore lease-key generation))
    nil))

(defgeneric event-store-coordination-descriptor (store)
  (:documentation
   "STORE's own claim about durability and coordination, for Core to judge.

A store that returns :NULL claims nothing, which Core reads as the volatile
single-worker default.")
  (:method ((store t)) :null))

(defgeneric event-store-has-delivery (store delivery-id)
  (:documentation
   "Whether DELIVERY-ID was already admitted. This is what makes a repeated
publication a duplicate rather than a second delivery."))

(defgeneric event-store-begin-delivery (store delivery)
  (:documentation
   "Move DELIVERY from queued to running. True when THIS worker won it.

The queued-to-running transition is the only thing standing between two
workers and the same delivery, so it cannot be a read followed by a write:
a second worker that loaded the row while it was still queued would write
\"running\" over the first worker's row and then run the program again. A
persistent store makes it a conditional update and answers NIL when it did
not apply. A volatile store has no second worker and simply wins.")
  (:method ((store t) delivery)
    (setf (%event-delivery-status delivery) "running")
    (event-store-commit-delivery store delivery)
    (event-store-release-delivery store delivery)
    t))

(defgeneric event-store-commit-fenced (store lease-key generation thunk)
  (:documentation
   "Run THUNK's writes and the lease check for LEASE-KEY as ONE commit.

True when this worker still held LEASE-KEY at GENERATION and THUNK's writes
were applied; NIL when it did not, in which case nothing was written.

This is a commit boundary, not a check followed by writes. A store that
verified the lease and then wrote would leave a window in which another
worker steals the lease and the stale holder still overwrites the thief's
result, so a store claiming fenced dispatch has to make the two atomic. A
volatile store has no second worker and simply runs THUNK.")
  (:method ((store t) lease-key generation thunk)
    (declare (ignore lease-key generation))
    (funcall thunk)
    t))

(defgeneric event-store-fence (store lease-key generation)
  (:documentation
   "Whether this worker still holds LEASE-KEY at GENERATION.

A store with no fencing answers T, which is correct for a single worker.")
  (:method ((store t) lease-key generation)
    (declare (ignore lease-key generation))
    t))

(defgeneric event-store-commit-delivery (store delivery)
  (:documentation
   "Persist DELIVERY's mutable fields after the runtime changed them.

The in-memory store holds the very object the runtime mutated, so this is a
no-op there; a persistent store writes the row back. The runtime calls it
after every status, attempt or run-id change, so no store is left holding a
stale delivery.")
  (:method ((store t) delivery) (declare (ignore delivery)) nil))

(defgeneric event-store-save-run (store run)
  (:documentation "Record RUN, including its output, before any sink runs."))

(defgeneric event-store-save-dead-letter (store dead-letter)
  (:documentation "Record DEAD-LETTER."))

(defgeneric event-store-forget-dead-letter (store dead-letter-id)
  (:documentation "Drop a dead letter that is being redriven."))

(defgeneric event-store-save-continuation (store continuation)
  (:documentation "Record CONTINUATION, or its completion."))

(defgeneric event-store-captured-state (store state-key)
  (:documentation "The captured program state at STATE-KEY, or :NULL."))

(defgeneric event-store-save-captured-state (store state-key value)
  (:documentation "Capture program state at STATE-KEY."))

(defgeneric event-store-release-delivery (store delivery)
  (:documentation "Stop counting DELIVERY's bytes once it leaves the queue."))

(defgeneric event-store-requeue-delivery (store delivery available-at)
  (:documentation "Put DELIVERY back on the queue, due at AVAILABLE-AT."))

(defmethod event-store-deliveries ((store in-memory-event-store))
  (%event-ordered-values (%store-deliveries store) (%store-order store)))

(defmethod event-store-runs ((store in-memory-event-store))
  (%event-ordered-values (%store-runs store) (%store-run-order store)))

(defmethod event-store-dead-letters ((store in-memory-event-store))
  (%event-ordered-values (%store-dead-letters store) (%store-dead-order store)))

(defmethod event-store-continuations ((store in-memory-event-store))
  (%event-ordered-values (%store-continuations store) (%store-continuation-order store)))

(defmethod event-store-has-delivery ((store in-memory-event-store) delivery-id)
  (and (nth-value 1 (gethash delivery-id (%store-deliveries store))) t))

(defmethod event-store-save-run ((store in-memory-event-store) run)
  (%event-store-put (%store-runs store) (%store-run-order store) (event-run-id run) run)
  run)

(defmethod event-store-save-dead-letter ((store in-memory-event-store) dead-letter)
  (%event-store-put (%store-dead-letters store) (%store-dead-order store)
                    (event-dead-letter-id dead-letter) dead-letter)
  dead-letter)

(defmethod event-store-forget-dead-letter ((store in-memory-event-store) dead-letter-id)
  (remhash dead-letter-id (%store-dead-letters store))
  nil)

(defmethod event-store-save-continuation ((store in-memory-event-store) continuation)
  (%event-store-put (%store-continuations store) (%store-continuation-order store)
                    (event-continuation-id continuation) continuation)
  continuation)

(defmethod event-store-captured-state ((store in-memory-event-store) state-key)
  (multiple-value-bind (value found) (gethash state-key (%store-program-state store))
    (if found value :null)))

(defmethod event-store-save-captured-state ((store in-memory-event-store) state-key value)
  (setf (gethash state-key (%store-program-state store)) value))

(defun %event-envelope-bytes (envelope)
  "The envelope's encoded size, as the capacity accounting counts it."
  (length (sb-ext:string-to-octets (encode-json (event-envelope-object envelope))
                                   :external-format :utf-8)))

(defun %event-pending-count (store)
  (count "queued" (event-store-deliveries store)
         :key #'%event-delivery-status :test #'string=))

(defmethod event-store-enqueue ((store in-memory-event-store) envelope commands
                                &key (available-at nil))
  (let* ((size (%event-envelope-bytes envelope))
         (clock (event-store-clock store))
         (fresh (remove-if (lambda (command)
                             (gethash (%event-delivery-key command envelope)
                                      (%store-deliveries store)))
                           commands))
         (deadline (+ (event-clock-now clock) (event-store-publish-timeout-ms store))))
    ;; Capacity is Core's decision; this loop only waits for it or gives up.
    (loop while fresh
          do (let* ((pending (sb-thread:with-mutex ((%store-lock store)) (%event-pending-count store)))
                    (transition (axllm/core::event-capacity-transition
                                 (+ pending (1- (length fresh)))
                                 (%store-queued-bytes store)
                                 size
                                 (event-store-max-pending store)
                                 (event-store-max-queued-bytes store)
                                 (event-store-max-envelope-bytes store))))
               (when (string= (%event-text (jget transition "reason")) "envelope_too_large")
                 (%event-fail 'event-input-error "Event envelope exceeds ~a bytes"
                              (event-store-max-envelope-bytes store)))
               (when (axllm/core::core-true-p (jget transition "accepted")) (return))
               (let ((remaining (- deadline (event-clock-now clock))))
                 (when (<= remaining 0)
                   (%event-fail 'event-backpressure-error
                                "Backpressure: event inbox capacity timed out"))
                 (event-clock-sleep clock (/ (min remaining 50) 1000d0)))))
    ;; Returns the delivery records it admitted, so the caller can stamp the
    ;; verified ingress identity on them without re-reading the store.
    (let ((admitted '()))
      (sb-thread:with-mutex ((%store-lock store))
        (dolist (command fresh)
          (let ((key (%event-delivery-key command envelope)))
            (incf (%store-sequence store))
            (let ((delivery (make-event-delivery
                             :id key :envelope envelope :command command
                             :status "queued"
                             :available-at (or available-at (event-clock-now clock))
                             :sequence (%store-sequence store) :size size)))
              (%event-store-put (%store-deliveries store) (%store-order store) key delivery)
              (push delivery admitted))
            (incf (%store-queued-bytes store) size))))
      (nreverse admitted))))

(defun %event-delivery-key (command envelope)
  (format nil "~a:~a" (%event-text (jget command "routeId")) (event-envelope-id envelope)))

(defmethod event-store-release-delivery ((store in-memory-event-store) delivery)
  (sb-thread:with-mutex ((%store-lock store))
    (setf (%store-queued-bytes store) (max 0 (- (%store-queued-bytes store) (%event-delivery-size delivery)))
          (%event-delivery-size delivery) 0)))

(defmethod event-store-requeue-delivery ((store in-memory-event-store) delivery available-at)
  (let ((size (%event-envelope-bytes (%event-delivery-envelope delivery))))
    (sb-thread:with-mutex ((%store-lock store))
      (setf (%event-delivery-size delivery) size
            (%event-delivery-status delivery) "queued"
            (%event-delivery-available-at delivery) available-at)
      (incf (%store-queued-bytes store) size))))

;;; ------------------------------------------------------------------
;;; Runtime
;;; ------------------------------------------------------------------

(defclass event-runtime ()
  ((routes :initarg :routes :reader event-runtime-routes)
   (targets :initarg :targets :reader %runtime-targets)
   (sources :initarg :sources :reader event-runtime-sources)
   (clock :initarg :clock :reader event-runtime-clock)
   (store :initarg :store :reader event-runtime-store)
   (max-attempts :initarg :max-attempts :reader event-runtime-max-attempts)
   (retry-backoff-ms :initarg :retry-backoff-ms :reader event-runtime-retry-backoff-ms)
   (descriptor :initarg :descriptor :reader event-runtime-descriptor)
   (started :initform nil :reader event-runtime-started-p)
   (closed :initform nil :reader event-runtime-closed-p)
   (active :initform (make-hash-table :test #'equal) :reader %runtime-active))
  (:documentation
   "The inline single-worker event runtime. See this file's header for the
contract: volatile storage, no worker threads, no implicit wake."))

(defun make-event-runtime (routes &key targets sources clock store
                                       (max-attempts 3) (retry-backoff-ms 1000)
                                       (max-pending 10000)
                                       (max-queued-bytes (* 64 1024 1024))
                                       (max-envelope-bytes (* 1024 1024))
                                       (publish-timeout-ms 5000))
  "Build an event runtime over ROUTES.

TARGETS is a list of EVENT-TARGETs, SOURCES a list of sources. The store
defaults to a fresh volatile in-memory store on the same clock."
  (let* ((route-list (if (listp routes) routes (coerce routes 'list)))
         (clock (or clock (make-system-event-clock)))
         (index (make-hash-table :test #'equal))
         (route-objects (%new-array))
         (options (object "maxAttempts" max-attempts
                          "retryBackoffMs" retry-backoff-ms
                          "maxPending" max-pending
                          "maxQueuedBytes" max-queued-bytes
                          "maxEnvelopeBytes" max-envelope-bytes
                          "publishTimeoutMs" publish-timeout-ms)))
    (dolist (target targets)
      (setf (gethash (event-target-id target) index) target))
    (dolist (route route-list)
      (vector-push-extend (event-route-object route) route-objects))
    ;; The descriptor's coordination is the store's to claim and Core's to
    ;; judge, so the store has to exist before the descriptor is computed.
    (let ((resolved-store (or store (make-in-memory-event-store
                                     :max-pending max-pending
                                     :max-queued-bytes max-queued-bytes
                                     :max-envelope-bytes max-envelope-bytes
                                     :publish-timeout-ms publish-timeout-ms
                                     :clock clock))))
      (make-instance 'event-runtime
                     :routes route-list :targets index
                     :sources (if (listp sources) sources (coerce sources 'list))
                     :clock clock
                     :store resolved-store
                     :max-attempts max-attempts
                     :retry-backoff-ms retry-backoff-ms
                     :descriptor
                     (axllm/core::event-runtime-descriptor-full
                      route-objects options
                      (event-store-coordination-descriptor resolved-store))))))

(defun event-runtime-start (runtime)
  "Start every source. Sources only enqueue; nothing is invoked here."
  (when (event-runtime-closed-p runtime)
    (%event-fail 'event-error "event runtime is closed"))
  (unless (event-runtime-started-p runtime)
    (setf (slot-value runtime 'started) t)
    (dolist (source (event-runtime-sources runtime))
      (event-source-start source
                          (lambda (envelope &key (identity-scope "anonymous")
                                                 (trust "untrusted"))
                            (event-runtime-publish runtime envelope
                                                   :identity-scope identity-scope
                                                   :trust trust)))))
  runtime)

(defun %event-runtime-route (runtime id)
  (find id (event-runtime-routes runtime) :key #'event-route-id :test #'equal))

(defun %event-ingress-object (envelope identity-scope trust)
  (object "event" (event-envelope-object envelope)
          "identity" (object "scope" identity-scope)
          "trust" trust
          "correlation" (%event-array (event-envelope-correlation envelope))))

(defun event-runtime-plan (runtime envelope &key (identity-scope "anonymous")
                                                 (trust "untrusted"))
  "The commands ROUTES produce for ENVELOPE, as Core decides them.

Returns a vector of JSON command objects: routeId, action, targetId,
instanceKey and idempotencyKey. A route with an instance key has it resolved
from the ingress event; an absent key is an error, not an anonymous bucket."
  (let* ((event (%event-envelope-of envelope))
         (route-objects (%new-array)))
    (dolist (route (event-runtime-routes runtime))
      (vector-push-extend (event-route-object route) route-objects))
    (let ((commands (axllm/core::event-route-commands
                     (event-envelope-object event) route-objects identity-scope trust))
          (ingress (%event-ingress-object event identity-scope trust)))
      (loop for command across commands
            do (let ((route (%event-runtime-route runtime (%event-text (jget command "routeId")))))
                 (unless (eq (event-route-instance-key route) :null)
                   (let ((resolved (axllm/core::event-resolve-path
                                    ingress
                                    (event-path-object (event-route-instance-key route))
                                    :null)))
                     (when (eq resolved :null)
                       (%event-fail 'event-input-error "Route ~a instance key was not present"
                                    (event-route-id route)))
                     (%set-key command "instanceKey" (%event-text resolved))))))
      commands)))

(defun event-runtime-publish (runtime envelope &key (identity-scope "anonymous")
                                                    (trust "untrusted"))
  "Admit ENVELOPE and drain whatever is due now on this thread.

IDENTITY-SCOPE must come from the host's authenticated adapter state, never
from event data; an unverified event stays anonymous and untrusted. Returns
a receipt object: eventId, accepted, duplicate, durability, deliveryIds."
  (unless (event-runtime-started-p runtime)
    (%event-fail 'event-error "event runtime must be started first"))
  (let* ((event (%event-envelope-of envelope))
         (store (event-runtime-store runtime))
         (clock (event-runtime-clock runtime))
         (commands (event-runtime-plan runtime event :identity-scope identity-scope
                                                     :trust trust))
         (delivery-ids (map 'list (lambda (command) (%event-delivery-key command event)) commands))
         (duplicate (and delivery-ids
                         (every (lambda (id) (event-store-has-delivery store id))
                                delivery-ids)))
         (now (event-clock-now clock)))
    (loop for command across commands
          do (let* ((route (%event-runtime-route runtime (%event-text (jget command "routeId"))))
                    (debounce (event-route-debounce-ms route))
                    (predecessor (and (plusp debounce) (%event-queued-predecessor store command route)))
                    (transition (axllm/core::event-debounce-transition
                                 now debounce (json-boolean predecessor))))
               (when (axllm/core::core-true-p (jget transition "coalescePredecessor"))
                 (setf (%event-delivery-status predecessor) "coalesced")
                 (event-store-release-delivery store predecessor))
               (dolist (delivery (event-store-enqueue store event (list command)
                                                      :available-at (jget transition "availableAt")))
                 (setf (%event-delivery-identity-scope delivery) identity-scope
                       (%event-delivery-trust delivery) trust)
                 ;; The verified ingress identity belongs with the delivery,
                 ;; not just in memory: a restarted worker must see the same
                 ;; scope it was admitted under.
                 (event-store-commit-delivery store delivery))))
    (unless duplicate (event-runtime-run-due runtime))
    (object "eventId" (event-envelope-id event)
            "accepted" true
            "duplicate" (json-boolean duplicate)
            "durability" "volatile"
            "deliveryIds" (coerce delivery-ids 'vector))))

(defun %event-queued-predecessor (store command route)
  "A queued delivery this debounced command replaces, if any."
  (find-if (lambda (delivery)
             (let ((old (%event-delivery-command delivery)))
               (and (string= (%event-delivery-status delivery) "queued")
                    (equal (jget old "routeId") (event-route-id route))
                    (axllm/core::core-value-equal (jget old "targetId") (jget command "targetId"))
                    (axllm/core::core-value-equal (jget old "instanceKey") (jget command "instanceKey")))))
           (event-store-deliveries store)))

(defun event-runtime-next-due-at (runtime)
  "When the host should call EVENT-RUNTIME-RUN-DUE again, or :NULL.

Debounce windows, retry backoff and continuation expiry all surface here.
The runtime never schedules itself."
  (let ((times (loop for delivery in (event-store-deliveries (event-runtime-store runtime))
                     when (string= (%event-delivery-status delivery) "queued")
                       collect (%event-delivery-available-at delivery))))
    (if times (reduce #'min times) :null)))

(defun event-runtime-run-due (runtime)
  "Dispatch every delivery due at the clock's current time. Returns a count."
  (let ((store (event-runtime-store runtime))
        (clock (event-runtime-clock runtime))
        (processed 0))
    (loop
      (let* ((now (event-clock-now clock))
             (deliveries (event-store-deliveries store))
             (due (sort (remove-if-not
                         (lambda (delivery)
                           (and (axllm/core::core-true-p
                                 (axllm/core::event-delivery-due
                                  (%event-delivery-status delivery)
                                  (%event-delivery-available-at delivery) now))
                                (%event-strict-eligible-p runtime delivery deliveries)))
                         deliveries)
                        (lambda (left right)
                          (or (< (%event-delivery-available-at left) (%event-delivery-available-at right))
                              (and (= (%event-delivery-available-at left) (%event-delivery-available-at right))
                                   (< (%event-delivery-sequence left) (%event-delivery-sequence right))))))))
        (unless due (return processed))
        (let ((delivery (first due)))
          ;; Losing the race is not an error: the row is no longer queued, so
          ;; the next pass will not see it as due and the loop still ends.
          (when (event-store-begin-delivery store delivery)
            (%event-dispatch runtime delivery)
            (incf processed)))))))

(defun %event-delivery-descriptor (runtime delivery)
  (let* ((command (%event-delivery-command delivery))
         (route (%event-runtime-route runtime (%event-text (jget command "routeId")))))
    (object "sequence" (%event-delivery-sequence delivery)
            "targetId" (%event-text (jget command "targetId"))
            "instanceKey" (jget command "instanceKey")
            "status" (%event-delivery-status delivery)
            "ordering" (if route (event-route-ordering route) "strict"))))

(defun %event-strict-eligible-p (runtime candidate deliveries)
  (let ((others (%new-array)))
    (dolist (delivery deliveries)
      (vector-push-extend (%event-delivery-descriptor runtime delivery) others))
    (axllm/core::core-true-p
     (axllm/core::event-strict-delivery-eligible
      (%event-delivery-descriptor runtime candidate) others))))

(defun %event-dispatch (runtime delivery)
  (let* ((store (event-runtime-store runtime))
         (clock (event-runtime-clock runtime))
         (command (%event-delivery-command delivery))
         (action (%event-text (jget command "action")))
         (identity-scope (%event-delivery-identity-scope delivery))
         (trust (%event-delivery-trust delivery))
         (event (%event-delivery-envelope delivery))
         (delivery-id (%event-delivery-id delivery))
         (continuation nil)
         (target-id (jget command "targetId")))
    (when (string= action "resume")
      (setf continuation (%event-find-continuation runtime event identity-scope))
      (unless continuation
        (%event-dead-letter runtime delivery-id :null "continuation_not_found")
        (return-from %event-dispatch nil))
      (setf target-id (event-continuation-target-id continuation)))
    (when (member action '("observe" "invalidate") :test #'string=)
      (setf (%event-delivery-status delivery) "succeeded")
      (event-store-commit-delivery store delivery)
      (return-from %event-dispatch nil))
    (let ((target (gethash (%event-text target-id) (%runtime-targets runtime))))
      (unless target
        (%event-dead-letter runtime delivery-id :null
                      (format nil "unknown_target:~a" (%event-text target-id)))
        (return-from %event-dispatch nil))
      ;; One worker at a time per instance. Strict per-instance ordering is
      ;; only a real guarantee while a single worker dispatches an instance,
      ;; so a denied lease puts the delivery back on the queue untouched
      ;; rather than running it twice or dropping it.
      (let* ((lease-key (format nil "~a~c~a" (event-target-id target) #\Newline
                                (%event-text (jget command "instanceKey"))))
             (lease (event-store-claim-instance store lease-key)))
        (unless (axllm/core::core-true-p (jget lease "granted"))
          ;; Another worker is inside this instance. The delivery goes back on
          ;; the queue unattempted, but it must also stop being due now, or a
          ;; drain loop would spin on work it can never take. The host re-polls
          ;; at EVENT-RUNTIME-NEXT-DUE-AT.
          (event-store-requeue-delivery
           store delivery
           (+ (event-clock-now (event-runtime-clock runtime))
              (event-runtime-retry-backoff-ms runtime)))
          (return-from %event-dispatch nil))
        (setf (%event-delivery-dispatch-lease delivery) lease-key
              (%event-delivery-dispatch-generation delivery)
              (jget lease "generation" 0)))
      (let* ((run-id (or (%event-delivery-run-id delivery)
                         (format nil "run:~a:~a" delivery-id
                                 (1+ (length (event-store-runs store))))))
             (run (or (find run-id (event-store-runs store)
                            :key #'event-run-id :test #'equal)
                      (make-instance 'event-run :id run-id :delivery-id delivery-id
                                                :route-id (%event-text (jget command "routeId"))
                                                :target-id (event-target-id target)
                                                :instance-key (jget command "instanceKey"))))
             (token (make-cancellation-token))
             (state-key (format nil "~a~c~a~c~a" (event-target-id target) #\Newline
                                identity-scope #\Newline (jget command "instanceKey"))))
        (event-store-save-run store run)
        (setf (%event-delivery-run-id delivery) run-id)
        (event-store-commit-delivery store delivery)
        (setf (gethash run-id (%runtime-active runtime)) token)
        (unwind-protect
             (let (mapped)
               ;; Mapping failures are an input fault: dead-letter before the
               ;; program is invoked, so an invalid event cannot have an effect.
               (handler-case
                   (setf mapped (%event-map-target-input target event continuation action
                                                   identity-scope trust))
                 (error (condition)
                   (setf (event-run-status run) "failed"
                         (event-run-error run) (format nil "event_input_invalid:~a" condition)
                         (%event-delivery-status delivery) "dead_lettered")
                   (%event-dead-letter runtime delivery-id run-id (event-run-error run))
                   (return-from %event-dispatch nil)))
               (when (event-target-restore-state target)
                 (let ((state (event-store-captured-state store state-key)))
                   (unless (eq state :null)
                     (funcall (event-target-restore-state target) state))))
               (let ((attempt (1+ (%event-delivery-attempt delivery))))
                 (setf (%event-delivery-attempt delivery) attempt
                       (event-run-attempt run) attempt
                       (event-run-status run) "running")
                 (handler-case
                     (let ((output (funcall (event-target-invoke target) mapped
                                            (object "runId" run-id
                                                    "deliveryId" delivery-id
                                                    "instanceKey" (jget command "instanceKey")
                                                    "identityScope" identity-scope
                                                    "idempotencyKey" (jget command "idempotencyKey")
                                                    "cancellation" token
                                                    "continuation" (or continuation :null)))))
                       (if (cancellation-token-cancelled-p token)
                           (setf (event-run-status run) "cancelled"
                                 (%event-delivery-status delivery) "cancelled")
                           (progn
                             (when (event-target-capture-state target)
                               ;; Captured now, written at the commit boundary:
                               ;; the value is the program's, but persisting it
                               ;; is a fenced write like the run itself.
                               (let ((captured (funcall (event-target-capture-state target))))
                                 (%event-defer-write
                                  delivery
                                  (lambda ()
                                    (event-store-save-captured-state
                                     store state-key captured)))))
                             (setf (event-run-output run) output)
                             (let ((registrations (%event-register-declared
                                                   runtime target event command
                                                   identity-scope delivery)))
                               (if registrations
                                   (setf (event-run-continuation-ids run) registrations
                                         (event-run-status run) "waiting_event"
                                         (%event-delivery-status delivery) "waiting_event")
                                   (progn
                                     (setf (event-run-status run) "succeeded"
                                           (%event-delivery-status delivery) "succeeded")
                                     ;; The run, output included, is stored before
                                     ;; any sink runs, so a sink failure can be
                                     ;; redriven without re-invoking the program.
                                     (%event-run-sinks runtime target run delivery-id))))
                             (when continuation
                               (setf (event-continuation-completed-p continuation) t)))))
                   (error (condition)
                     (%event-handle-invocation-failure runtime target run delivery attempt condition
                                                 clock)))))
          ;; Persist the run's final state on every exit path, but only while
          ;; this worker still holds the lease it dispatched under. A lease
          ;; that expired mid-run has been stolen, and the thief may already
          ;; have run this instance, so writing a result now would overwrite
          ;; the real one. The delivery is dead-lettered instead, which says
          ;; what happened rather than quietly producing two answers.
          (let* ((held (%event-delivery-dispatch-lease delivery))
                 (write (lambda ()
                          (%event-apply-pending-writes delivery)
                          (event-store-save-run store run)
                          (event-store-commit-delivery store delivery))))
            ;; One boundary for every persistent write this dispatch makes:
            ;; captured state, declared continuations, the run and the
            ;; delivery row, checked against the lease this dispatch began
            ;; under. A lease stolen mid-run means the thief may already be
            ;; running this instance, so none of it may land; the dispatch is
            ;; recorded as lease_lost and the delivery row is left alone.
            ;; External sink effects are deliberately outside: they have
            ;; already happened and this stays honestly at-least-once.
            (if (and held
                     (not (event-store-commit-fenced
                           store held (%event-delivery-dispatch-generation delivery)
                           write)))
                (progn (setf (%event-delivery-pending-writes delivery) '())
                       (%event-dead-letter runtime delivery-id run-id "lease_lost" :null t))
                (unless held (funcall write)))
            (when held
              (setf (%event-delivery-dispatch-lease delivery) nil)
              (event-store-release-instance
               store held (%event-delivery-dispatch-generation delivery))))
          (remhash run-id (%runtime-active runtime)))))))

(defun %event-handle-invocation-failure (runtime target run delivery attempt condition clock)
  (let* ((transition (axllm/core::event-retry-transition
                      true (event-target-retry-safety target) attempt
                      (event-runtime-max-attempts runtime)))
         (store (event-runtime-store runtime)))
    (if (axllm/core::core-true-p (jget transition "retry"))
        (progn
          (setf (event-run-status run) "queued")
          (event-store-requeue-delivery store delivery
                          (+ (event-clock-now clock)
                             (* (event-runtime-retry-backoff-ms runtime)
                                (expt 2 (1- attempt))))))
        (let ((status (%event-text (jget transition "status"))))
          (setf (event-run-status run) status
                (event-run-error run) (princ-to-string condition)
                (%event-delivery-status delivery) status)
          (%event-dead-letter runtime (%event-delivery-id delivery) (event-run-id run)
                        (princ-to-string condition))))))

(defun %event-run-sinks (runtime target run delivery-id)
  (dolist (sink (event-target-sinks target))
    (handler-case
        (event-sink-write sink (event-run-output run)
                          (object "run" run
                                  "idempotencyKey" (format nil "~a:~a" (event-run-id run)
                                                           (event-sink-id sink))))
      (error (condition)
        (%event-dead-letter runtime delivery-id (event-run-id run)
                      (princ-to-string condition) (event-sink-id sink))))))

(defun %event-map-target-input (target event continuation action identity-scope trust)
  "ENVELOPE as TARGET's program input.

A declarative plan goes through Core's path resolution and input mapping; a
callback is normalized and validated against the signature afterwards, so
neither route can smuggle an unmapped field into a program."
  (let* ((plan (or (if (string= action "resume")
                       (event-target-resume-input target)
                       (event-target-wake-input target))
                   (event-target-input target)))
         (signature (event-target-signature target))
         (mapped
           (if (null plan)
               (if (event-target-map-input target)
                   (funcall (event-target-map-input target) event continuation)
                   (event-envelope-data event))
               (progn
                 (unless signature
                   (%event-fail 'event-input-error
                                "Target ~a requires a signature for declarative input mapping"
                                (event-target-id target)))
                 (let ((result (axllm/core::event-map-input
                                (%event-ingress-object event identity-scope trust)
                                (event-input-plan-object plan)
                                (%event-signature-descriptors signature)
                                (if continuation
                                    (event-continuation-object continuation)
                                    :null))))
                   (unless (axllm/core::core-true-p (jget result "ok"))
                     (%event-fail 'event-input-error "~a" (%event-text (jget result "error"))))
                   (jget result "value"))))))
    (if (null signature)
        mapped
        (let ((normalized (axllm/core::event-normalize-input
                           mapped (%event-signature-descriptors signature))))
          (unless (axllm/core::core-true-p (jget normalized "ok"))
            (%event-fail 'event-input-error "~a" (%event-text (jget normalized "error"))))
          (let ((value (jget normalized "value")))
            (loop for field across (signature-fields signature :side :input)
                  do (let ((name (jget field "name")))
                       (if (eq (jget value name) :null)
                           (unless (json-true-p (jget field "isOptional"))
                             (%event-fail 'event-input-error
                                          "Required signature input ~a was not present" name))
                           (%event-validate-event-input-value field (jget value name)))))
            value)))))

(defun %event-signature-descriptors (signature)
  "SIGNATURE's input fields as the {name, optional} list Core maps against."
  (let ((out (%new-array)))
    (loop for field across (signature-fields signature :side :input)
          do (vector-push-extend (object "name" (jget field "name")
                                         "optional" (jget field "isOptional"))
                                 out))
    out))

(defparameter +event-string-field-types+
  '("string" "url" "date" "datetime" "code" "file" "image" "audio" "class"))

(defparameter +event-structured-field-types+
  '("object" "json" "dateRange" "datetimeRange"))

(defun %event-validate-event-input-value (field value)
  "Reject a mapped value its signature field cannot hold.

This is a type gate, not a constraint check: it exists so an event cannot
reach a program with an input of the wrong shape."
  (let* ((type (jget field "type"))
         (kind (%event-text (jget type "name")))
         (array (json-true-p (jget type "isArray")))
         (name (%event-text (jget field "name"))))
    (when (and array (not (%array-p value)))
      (%event-fail 'event-input-error "Signature input ~a failed validation" name))
    (loop for item across (if array value (vector value))
          do (unless (cond ((member kind +event-string-field-types+ :test #'string=)
                            (stringp item))
                           ((string= kind "number") (realp item))
                           ((string= kind "boolean") (json-boolean-p item))
                           ((member kind +event-structured-field-types+ :test #'string=)
                            (or (hash-table-p item) (%array-p item)))
                           (t nil))
               (%event-fail 'event-input-error "Signature input ~a failed validation" name)))
    value))

(defun %event-defer-continuation (delivery store continuation)
  "Persist CONTINUATION at DELIVERY's commit boundary."
  (%event-defer-write delivery
                      (lambda () (event-store-save-continuation store continuation))))

(defun %event-register-declared (runtime target event command identity-scope delivery)
  "Register TARGET's declared waits. Returns their continuation ids.

The continuation objects are built here but persisted at DELIVERY's fenced
commit boundary, because writing them is as much a fenced write as the run
is: a worker whose lease was stolen must not leave continuations behind."
  (let* ((store (event-runtime-store runtime))
         (clock (event-runtime-clock runtime))
         (reserved (length (event-store-continuations store)))
         (ids '()))
    (dolist (declaration (event-target-wait-for target))
      (destructuring-bind (kind path &key metadata expires-in-ms) declaration
        (let ((value (if (typep path 'event-path)
                         (axllm/core::event-resolve-path
                          (%event-ingress-object event identity-scope "untrusted")
                          (event-path-object path) :null)
                         (funcall path event))))
          (when (eq value :null)
            (%event-fail 'event-input-error "continuation value is missing"))
          (let* ((id (format nil "continuation:~a:~a" (event-target-id target)
                             (incf reserved)))
                 (correlation (%new-array)))
            (vector-push-extend (object "kind" kind "value" (%event-text value)) correlation)
            (%event-defer-continuation
                     delivery store
                     (make-instance 'event-continuation
                                       :id id :target-id (event-target-id target)
                                       :instance-key (jget command "instanceKey")
                                       :identity-scope identity-scope
                                       :correlation correlation
                                       :metadata (if (functionp metadata)
                                                     (funcall metadata event)
                                                     metadata)
                                       :expires-at (if expires-in-ms
                                                       (+ (event-clock-now clock) expires-in-ms)
                                                       :null)))
            (push id ids)))))
    (nreverse ids)))

(defun %event-find-continuation (runtime event identity-scope)
  "The continuation ENVELOPE's correlation keys resume, if the scope owns it.

Core decides whether a key, scope and expiry match; completion is runtime
state and is filtered here."
  (let* ((store (event-runtime-store runtime))
         (now (event-clock-now (event-runtime-clock runtime)))
         (open (remove-if #'event-continuation-completed-p
                          (event-store-continuations store)))
         (candidates (%new-array)))
    (dolist (continuation open)
      (vector-push-extend (event-continuation-object continuation) candidates))
    (loop for key across (%event-array (event-envelope-correlation event))
          do (let ((match (axllm/core::event-continuation-match
                           candidates identity-scope
                           (%event-text (jget key "kind"))
                           (%event-text (jget key "value"))
                           now)))
               (unless (eq match :null)
                 (return (find (%event-text (jget match "id")) open
                               :key #'event-continuation-id :test #'string=)))))))

(defun %event-defer-write (delivery thunk)
  "Queue THUNK to run inside DELIVERY's fenced commit boundary."
  (push thunk (%event-delivery-pending-writes delivery))
  nil)

(defun %event-apply-pending-writes (delivery)
  "Apply DELIVERY's queued writes, in the order they were queued."
  (let ((pending (reverse (%event-delivery-pending-writes delivery))))
    (setf (%event-delivery-pending-writes delivery) '())
    (dolist (thunk pending) (funcall thunk))))

(defun %event-dead-letter (runtime delivery-id run-id reason &optional (sink-id :null)
                                                             keep-delivery)
  "Record a dead letter for DELIVERY-ID.

KEEP-DELIVERY leaves the delivery row untouched. It is set on exactly one
path: a worker whose lease was stolen mid-dispatch. That worker has lost
the right to write this delivery, and the thief may already be running it,
so marking the row dead_lettered would destroy the live state. The dead
letter itself is append-only and says what happened to this worker."
  (let* ((store (event-runtime-store runtime))
         (id (format nil "dead:~a" (1+ (length (event-store-dead-letters store)))))
         (value (make-instance 'event-dead-letter :id id :delivery-id delivery-id
                                                  :reason reason :run-id run-id
                                                  :sink-id sink-id)))
    (event-store-save-dead-letter store value)
    (when (and (eq sink-id :null) (not keep-delivery))
      (let ((delivery (find delivery-id (event-store-deliveries store)
                            :key #'%event-delivery-id :test #'equal)))
        (when delivery
          (setf (%event-delivery-status delivery) "dead_lettered")
          (event-store-commit-delivery store delivery))))
    value))

(defun event-runtime-get-run (runtime run-id)
  "The stored run RUN-ID, or NIL."
  (find run-id (event-store-runs (event-runtime-store runtime))
        :key #'event-run-id :test #'equal))

(defun event-runtime-list-dead-letters (runtime)
  (event-store-dead-letters (event-runtime-store runtime)))

(defun event-runtime-cancel-run (runtime run-id &optional (reason "cancelled"))
  "Ask RUN-ID to stop. True when a run was active to ask."
  (let ((token (gethash run-id (%runtime-active runtime))))
    (when token (cancellation-token-cancel token reason) t)))

(defun event-runtime-redrive (runtime dead-letter-id)
  "Retry a dead letter. A sink dead letter retries only that sink."
  (let* ((store (event-runtime-store runtime))
         (dead (find dead-letter-id (event-store-dead-letters store)
                     :key #'event-dead-letter-id :test #'equal)))
    (unless dead
      (%event-fail 'event-error "unknown dead letter ~a" dead-letter-id))
    (event-store-forget-dead-letter store dead-letter-id)
    (if (eq (event-dead-letter-sink-id dead) :null)
        (let ((delivery (find (event-dead-letter-delivery-id dead)
                              (event-store-deliveries store)
                              :key #'%event-delivery-id :test #'equal)))
          (unless delivery
            (event-store-save-dead-letter store dead)
            (%event-fail 'event-error "redrive state is unavailable"))
          (setf (%event-delivery-attempt delivery) 0)
          (event-store-requeue-delivery store delivery (event-clock-now (event-runtime-clock runtime)))
          (event-runtime-run-due runtime))
        (let* ((run (find (%event-text (event-dead-letter-run-id dead))
                          (event-store-runs store) :key #'event-run-id :test #'equal))
               (target (and run (gethash (event-run-target-id run) (%runtime-targets runtime))))
               (sink (and target (find (event-dead-letter-sink-id dead)
                                       (event-target-sinks target)
                                       :key #'event-sink-id :test #'equal))))
          (unless (and run target sink)
            (event-store-save-dead-letter store dead)
            (%event-fail 'event-error "sink redrive state is unavailable"))
          (handler-case
              (event-sink-write sink (event-run-output run)
                                (object "run" run
                                        "idempotencyKey"
                                        (format nil "~a:~a" (event-run-id run)
                                                (event-dead-letter-sink-id dead))))
            (error (condition)
              (event-store-save-dead-letter store dead)
              (error condition)))))
    nil))

(defun event-runtime-close (runtime)
  "Close every source. Caller-owned protocol clients stay the caller's."
  (dolist (source (event-runtime-sources runtime))
    (event-source-close source))
  (setf (slot-value runtime 'started) nil
        (slot-value runtime 'closed) t)
  nil)

(defun event-normalize-mcp (namespace method params)
  "One MCP notification as an ingress event's source, type, data and
correlation, as Core defines the mapping."
  (axllm/core::event-normalize-mcp namespace method params))

;;; ------------------------------------------------------------------
;;; Core host object protocol
;;; ------------------------------------------------------------------
;;;
;;; Generated Core code and fixture expectations reach into native objects
;;; by key. The generics are owned by the Core runtime layer; these methods
;;; are the event surface's half of that contract.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (spec '((axllm/core::core-host-get (target key &optional fallback))
                  (axllm/core::core-host-set (target key value))
                  (axllm/core::core-host-call (target method args-vector))))
    (unless (fboundp (first spec))
      (ensure-generic-function (first spec) :lambda-list (second spec)))))

(defmacro %event-define-host-reader (class &rest mappings)
  "Expose CLASS's fields to Core under their published JSON names."
  `(defmethod axllm/core::core-host-get ((target ,class) key &optional (fallback :null))
     (cond ,@(mapcar (lambda (mapping)
                       `((equal key ,(first mapping)) (,(second mapping) target)))
                     mappings)
           (t fallback))))

(%event-define-host-reader event-run
  ("id" event-run-id) ("deliveryId" event-run-delivery-id)
  ("routeId" event-run-route-id) ("targetId" event-run-target-id)
  ("instanceKey" event-run-instance-key) ("status" event-run-status)
  ("attempt" event-run-attempt) ("output" event-run-output)
  ("error" event-run-error))

(%event-define-host-reader event-dead-letter
  ("id" event-dead-letter-id) ("deliveryId" event-dead-letter-delivery-id)
  ("reason" event-dead-letter-reason) ("runId" event-dead-letter-run-id)
  ("sinkId" event-dead-letter-sink-id))

(%event-define-host-reader event-envelope
  ("id" event-envelope-id) ("source" event-envelope-source)
  ("type" event-envelope-type) ("data" event-envelope-data)
  ("subject" event-envelope-subject) ("specversion" event-envelope-specversion))

(defmethod axllm/core::core-host-get ((target event-continuation) key &optional (fallback :null))
  (axllm/core::core-get (event-continuation-object target) key fallback))

(defmethod axllm/core::core-host-call ((target cancellation-token) method args)
  (cond ((equal method "cancel")
         (json-boolean (cancellation-token-cancel
                        target (if (plusp (length args)) (aref args 0) "cancelled"))))
        ((equal method "cancelled") (json-boolean (cancellation-token-cancelled-p target)))
        (t (%event-fail 'event-error "cancellation token has no method ~a" method))))

(export '(event-error event-input-error event-backpressure-error
          make-cancellation-token cancellation-token-cancel
          cancellation-token-cancelled-p cancellation-token-reason
          cancellation-token-subscribe cancellation-token-subscription-count
          cancellation-token-throw-if-cancelled cancellation-token-wait
          event-clock-now event-clock-sleep make-system-event-clock
          make-manual-event-clock manual-clock-advance manual-clock-wait-for-sleepers
          make-event-envelope event-envelope-object event-envelope-id
          event-envelope-source event-envelope-type event-envelope-data
          event-envelope-subject event-envelope-correlation event-envelope-extensions
          event-path-data event-path-envelope event-path-extension event-path-identity
          event-path-trust event-path-correlation event-path-continuation
          event-path-constant event-path-subject event-path-object
          event-input-plan event-input-plan-object
          event-route event-route-object event-route-id event-route-action
          event-target event-target-id
          event-sink-id event-sink-write make-event-sink
          event-source-start event-source-close event-source-id
          make-push-event-source push-event-source-publish
          event-run-id event-run-status event-run-attempt event-run-output
          event-run-error event-run-target-id event-run-continuation-ids
          event-dead-letter-id event-dead-letter-reason event-dead-letter-sink-id
          event-dead-letter-run-id event-dead-letter-delivery-id
          event-continuation-id event-continuation-target-id
          event-continuation-metadata event-continuation-completed-p
          make-in-memory-event-store event-store-enqueue
          event-store-deliveries event-store-runs event-store-dead-letters
          event-store-continuations event-store-has-delivery
          event-store-commit-delivery event-store-save-run
          event-store-save-dead-letter event-store-forget-dead-letter
          event-store-save-continuation event-store-captured-state
          event-store-save-captured-state event-store-release-delivery
          event-store-requeue-delivery event-store-begin-delivery event-store-fence event-store-commit-fenced
          event-store-claim-instance
          event-store-release-instance event-store-coordination-descriptor
          make-event-runtime event-runtime-start event-runtime-plan
          event-runtime-publish event-runtime-next-due-at event-runtime-run-due
          event-runtime-get-run event-runtime-list-dead-letters
          event-runtime-cancel-run event-runtime-redrive event-runtime-close
          event-runtime-descriptor event-normalize-mcp))
