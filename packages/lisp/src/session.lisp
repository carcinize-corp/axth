;;;; session.lisp --- the request-boundary service for run controls.
;;;;
;;;; Reference: tools/axir/internal/axir/templates/python/pySession.py
;;;; (_BoundaryClient) and ir/axcore/session.md.
;;;;
;;;; Ownership. The run control itself -- the class, its constructor, its
;;;; abort/steer/event API and its host methods -- belongs to the agent
;;;; runtime (src/agent-runtime.lisp). There is exactly one control
;;;; implementation in this port, and this file does not add a second one. It
;;;; reaches a control only through the host protocol in axllm/core:
;;;; CORE-HOST-GET for "aborted", CORE-HOST-CALL for "take_pending" and
;;;; "pending_count". Any object answering that protocol works here, so the
;;;; provider layer never type-checks a control.
;;;;
;;;; Core owns the policy. Which queued updates reach a path, how a steering
;;;; turn and a thinking-budget change are folded into the next request, and
;;;; what the applied set is, are read from the generated Core functions
;;;; `chat-session-apply-boundary-updates' and `chat-session-target-matches'.
;;;; Nothing here re-decides them.
;;;;
;;;; What this file adds is the boundary itself: an ordinary chat service has
;;;; no native steering channel, so a control reaches it when the next request
;;;; is built. Queued and applied stay distinct, and the applied event says
;;;; which response the change actually took effect on.

(in-package #:axllm)

(defclass chat-session-state ()
  ((value :initarg :value :reader chat-session-value)))

(defun chat-session-state (model path max-steps)
  (make-instance 'chat-session-state
                 :value (axllm/core::chat-session-create-state model path max-steps)))

(defun chat-session-transition (session event)
  (axllm/core::chat-session-transition (chat-session-value session) event))

(defun chat-session-unresolved (session)
  (axllm/core::chat-session-unresolved (chat-session-value session)))

(defun chat-session-validate-arguments (schema arguments)
  (axllm/core::chat-session-validate-required-arguments schema arguments "arguments"))

(defun chat-session-argument-errors (schema arguments)
  (axllm/core::chat-session-tool-argument-errors schema arguments))

(defclass responses-session-decoder ()
  ((model :initarg :model :reader responses-session-model)
   (state :initform (object) :reader responses-session-state)
   (cursor :initform (object) :reader responses-session-cursor)))

(defun responses-session-decoder (model)
  (make-instance 'responses-session-decoder :model model))

(defun responses-session-event (decoder event)
  (axllm/core::openai-responses-transport-cursor (responses-session-cursor decoder) event)
  (axllm/core::openai-responses-session-event event (responses-session-state decoder)
                                             (responses-session-model decoder)))

;;; ------------------------------------------------------------------
;;; Reaching a control through the host protocol
;;; ------------------------------------------------------------------

(defun %control-present-p (control)
  (not (or (null control) (eq control :null))))

(defun %control-aborted-p (control)
  "Whether CONTROL has been asked to stop.

Read with CORE-GET, which is the same path Core's own
`core-run-control-aborted' takes: a JSON object answers from its \"aborted\"
key and a host control object answers through CORE-HOST-GET.  Reading
CORE-HOST-GET directly would silently answer the fallback for a plain object
and let an aborted run keep sending."
  (and (%control-present-p control)
       (axllm/core::core-true-p (axllm/core::core-get control "aborted" 'yason:false))))

(defun %control-take-pending (control)
  "CONTROL's queued updates, removed from its queue, as a vector.

A control that holds nothing answers an empty vector; an object that is not a
control at all answers an empty vector too, because most services have no
control and that is not an error."
  (if (and (%control-present-p control) (plusp (%control-pending-count control)))
      (let ((taken (axllm/core::core-host-call control "take_pending" (vector))))
        (if (and (vectorp taken) (not (stringp taken))) taken (%new-array)))
      (%new-array)))

(defun %control-pending-count (control)
  "How many updates CONTROL is holding, or 0 when it is not a control.

Read with CORE-GET so a plain JSON object answers from its own key and a
non-control answers the fallback instead of failing."
  (if (%control-present-p control)
      (let ((count (axllm/core::core-get control "pending_count" 0)))
        (if (integerp count) count 0))
      0))

(defun %control-requeue (control updates)
  "Give UPDATES back to CONTROL, in order, for the stage they belong to.

The host protocol can only drain a control's whole queue, so a stage that
drains it also picks up updates aimed at another path.  Those are returned
rather than held here: holding them would hide a sibling stage's steering
behind whichever stage happened to run first, and dropping them would cancel
it outright.  Returns the updates that could not be given back, which a
control without a \"steer\" method leaves to the caller."
  (if (or (null updates) (not (%control-present-p control)))
      updates
      (handler-case
          (progn (dolist (update updates)
                   (axllm/core::core-host-call control "steer" (vector update)))
                 '())
        (ax-error () updates))))

(defun %control-emit (control event)
  "Report EVENT to CONTROL when it has one.

Flow reports its lifecycle through CORE-HOST-CALL with \"_emit\"; the agent
runtime's control answers \"emit\".  Both names are tried, newest first, so
this file works against either without owning the control."
  (when (%control-present-p control)
    (handler-case (axllm/core::core-host-call control "emit" (vector event))
      (ax-error ()
        (handler-case (axllm/core::core-host-call control "_emit" (vector event))
          (ax-error () nil)))))
  control)

;;; ------------------------------------------------------------------
;;; Pending control updates, as a service question
;;;
;;; The reference asks the client, not the control: a forward applies a run's
;;; queued updates when a step starts, so the next request boundary must not
;;; apply them again.  Asking the service keeps that polymorphic -- Gen and
;;; Agent never check whether a particular service happens to be a boundary.
;;; ------------------------------------------------------------------

(defgeneric ax-take-control-updates (service)
  (:documentation
   "The run-control updates SERVICE has not applied yet, as a vector, removed
from the queue.  A service with no control answers an empty vector.")
  (:method ((service t)) (%new-array)))

(defgeneric ax-pending-control-count (service)
  (:documentation
   "How many run-control updates SERVICE is still holding.")
  (:method ((service t)) 0))

;;; ------------------------------------------------------------------
;;; The request-boundary service
;;;
;;; Reference: pySession.py's _BoundaryClient.  This is a service object like
;;; any other: Gen, Flow and Agent dispatch the same generics on it and never
;;; check its class.
;;; ------------------------------------------------------------------

(defclass boundary-service ()
  ((inner :initarg :inner :reader boundary-inner)
   (control :initarg :control :reader boundary-control)
   (path :initarg :path :reader boundary-path)
   (options :initarg :options :initform nil :accessor %boundary-options)
   (held :initform '() :accessor %boundary-held)
   (level :initform :null :accessor %boundary-level))
  (:documentation
   "A service that applies a run control's updates at each request boundary.

It wraps another service and forwards every operation to it.  The only thing
it adds is the boundary: before a request goes out, the control's queued
updates for this path are folded into it by Core, and each one is announced as
applied with the timing that actually happened.

An ordinary chat service has no native steering channel, so an update that
arrives after a request was built applies to the next one.  That is what the
applied event's \"next-response\" timing reports; it is not a claim that the
in-flight request carried it."))

(defun boundary-service (inner control &key (path "root") options)
  "Wrap INNER so CONTROL's updates reach it at each request boundary.

CONTROL is any object answering the host protocol -- the agent runtime's run
control, or a JSON object with an \"aborted\" flag."
  (let ((service (make-instance 'boundary-service
                               :inner inner :control control :path path
                               :options options)))
    (%control-emit control (object "type" "started" "path" path))
    service))

(defun %boundary-matches-path-p (service update)
  "Whether UPDATE reaches this boundary's path.

Core decides: `chat-session-target-matches' is the same rule every target
uses, so a node path and the stages under it agree across ports.  An update
with no target reaches every path, which is how a control that does not scope
its updates keeps working."
  (let ((target (%present (jget update "target"))))
    (or (null target)
        (axllm/core::core-true-p
         (axllm/core::chat-session-target-matches target (boundary-path service))))))

(defun %boundary-take (service)
  "The updates this boundary should apply now, marking them applied.

An update aimed at another path is given back to the control, so whichever
stage runs first cannot consume a sibling stage's steering."
  (let* ((control (boundary-control service))
         ;; Anything a control could not take back comes first, so the order
         ;; the caller queued the updates in survives.
         (candidates (append (%boundary-held service)
                             (coerce (%control-take-pending control) 'list)))
         (mine '())
         (theirs '()))
    (dolist (update candidates)
      (if (%boundary-matches-path-p service update)
          (push update mine)
          (push update theirs)))
    (setf (%boundary-held service) (%control-requeue control (nreverse theirs)))
    (let ((mine (nreverse mine)))
      (dolist (update mine)
        (%control-emit control
                       (let ((event (object "type" "applied" "path" (boundary-path service)
                                            "timing" "next-response")))
                         (let ((id (%present (jget update "id"))))
                           (when id (setf (gethash "update_id" event) id)))
                         event)))
      mine)))

(defun %boundary-request-copy (request)
  "A copy of REQUEST that Core may fold updates into.

Core applies a steering turn by appending to the request's own prompt array
and writing the request's own keys, so handing it the caller's request would
leave the steer in the caller's history and send it again on the next turn.
The prompt becomes an extensible array because that is what Core appends to;
the messages themselves are shared, since Core does not rewrite them."
  (let ((copy (%new-object)))
    (when (hash-table-p request)
      (dolist (key (%object-keys request))
        (%set-key copy key (gethash key request))))
    (let ((prompt (gethash "chat_prompt" copy)))
      (when (and (vectorp prompt) (not (stringp prompt)))
        (let ((fresh (%new-array)))
          (map nil (lambda (entry) (vector-push-extend entry fresh)) prompt)
          (%set-key copy "chat_prompt" fresh))))
    (let ((config (gethash "model_config" copy)))
      (when (hash-table-p config)
        (let ((fresh (%new-object)))
          (dolist (key (%object-keys config))
            (%set-key fresh key (gethash key config)))
          (%set-key copy "model_config" fresh))))
    copy))

(defun %boundary-apply (service request)
  "REQUEST with this boundary's pending updates folded in by Core."
  (when (%control-aborted-p (boundary-control service))
    (provider-fail :aborted "Run aborted before the next model request."))
  (let* ((updates (%boundary-take service))
         (applied (axllm/core::chat-session-apply-boundary-updates
                   (%boundary-request-copy request)
                   (coerce updates 'vector)
                   (%boundary-level service))))
    (setf (%boundary-level service) (jget applied "level"))
    (jget applied "request")))

(defmethod ax-take-control-updates ((service boundary-service))
  (coerce (%boundary-take service) 'vector))

(defmethod ax-pending-control-count ((service boundary-service))
  (+ (count-if (lambda (update) (%boundary-matches-path-p service update))
               (%boundary-held service))
     ;; The control's own queue is not drained to count it: a count must not
     ;; consume the updates a later request is going to apply.
     (%control-pending-count (boundary-control service))))

(defmethod ax-service-name ((service boundary-service))
  (ax-service-name (boundary-inner service)))

;;; The generator records the client's name and model in its chat log and its
;;; traces under the older accessor names, so a boundary has to answer them or
;;; wrapping a run's client breaks an ordinary forward.
(defmethod ai-name ((service boundary-service)) (ai-name (boundary-inner service)))
(defmethod ai-model ((service boundary-service)) (ai-model (boundary-inner service)))
(defmethod ai-base-url ((service boundary-service)) (ai-base-url (boundary-inner service)))
(defmethod ai-timeout ((service boundary-service)) (ai-timeout (boundary-inner service)))
(defmethod ai-transport ((service boundary-service)) (ai-transport (boundary-inner service)))
(defmethod ai-streaming-transport ((service boundary-service))
  (ai-streaming-transport (boundary-inner service)))
(defmethod ax-credential ((service boundary-service))
  (ax-credential (boundary-inner service)))

(defmethod ax-id ((service boundary-service))
  (ax-id (boundary-inner service)))

(defmethod ax-features ((service boundary-service) &optional model)
  (ax-features (boundary-inner service) model))

(defmethod ax-metrics ((service boundary-service))
  (ax-metrics (boundary-inner service)))

(defmethod ax-options ((service boundary-service))
  (or (%boundary-options service) (ax-options (boundary-inner service))))

(defmethod (setf ax-options) (options (service boundary-service))
  (setf (%boundary-options service) options))

(defmethod ax-estimated-cost ((service boundary-service) &optional model-usage)
  (ax-estimated-cost (boundary-inner service) model-usage))

(defmethod ax-owned-worker-factory ((service boundary-service))
  ;; A boundary belongs to one run and one path, so it is not shared between
  ;; workers; whether the inner service needs its own worker is its decision.
  (ax-owned-worker-factory (boundary-inner service)))

(defmethod ax-chat ((service boundary-service) request &optional options)
  (ax-chat (boundary-inner service) (%boundary-apply service request)
           (or options (%boundary-options service))))

(defmethod ax-complete ((service boundary-service) request)
  (ax-complete (boundary-inner service) (%boundary-apply service request)))

(defmethod ax-stream ((service boundary-service) request &optional options)
  (ax-stream (boundary-inner service) (%boundary-apply service request)
             (or options (%boundary-options service))))

(defmethod ax-embed ((service boundary-service) request &optional options)
  (ax-embed (boundary-inner service) request (or options (%boundary-options service))))

(defmethod ax-transcribe ((service boundary-service) request &optional options)
  (ax-transcribe (boundary-inner service) request (or options (%boundary-options service))))

(defmethod ax-speak ((service boundary-service) request &optional options)
  (ax-speak (boundary-inner service) request (or options (%boundary-options service))))

(defun boundary-close (service &optional error)
  "Report the run's outcome for this boundary's path."
  (%control-emit (boundary-control service)
                 (if error
                     (object "type" "failed" "path" (boundary-path service)
                             "error" (princ-to-string error))
                     (object "type" "completed" "path" (boundary-path service))))
  service)

;;; A boundary is a host object to Core as well: a generated body handed the
;;; service can read the path it runs at and the control behind it.

(defmethod axllm/core::core-host-get ((target boundary-service) key &optional (fallback :null))
  (cond ((or (equal key "execution_path") (equal key "executionPath"))
         (boundary-path target))
        ((equal key "control") (boundary-control target))
        ((equal key "aborted")
         (axllm/core::core-bool (%control-aborted-p (boundary-control target))))
        ((or (equal key "pending_count") (equal key "pendingCount"))
         (ax-pending-control-count target))
        (t fallback)))

;;; ------------------------------------------------------------------
;;; Core host boundaries: ai.control_take_pending and ai.control_pending_count
;;;
;;; The forward applies a run's queued updates when a step starts, as the
;;; reference does, so the next request boundary must not apply them again.
;;; Both are questions for the service, answered polymorphically.
;;; ------------------------------------------------------------------

(in-package #:axllm/core)

(defun core-ai-control-take-pending (client)
  (axllm::ax-take-control-updates client))

(defun core-ai-control-pending-count (client)
  (axllm::ax-pending-control-count client))

(in-package #:axllm)
