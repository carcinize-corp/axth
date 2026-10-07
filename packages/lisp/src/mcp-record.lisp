;;;; mcp-record.lisp --- record an MCP conversation, then replay it strictly.
;;;;
;;;; This one is deliberately NOT in Core, and the reason is worth stating
;;;; because every other MCP surface went the other way. Reading
;;;; src/ax/mcp/transports/recordingTransport.ts, there is no portable
;;;; algorithm in it: it is a transport wrapper that clones each message,
;;;; keeps them in order, and on replay insists the next request matches the
;;;; next recorded one. Nothing a second port could disagree about, so
;;;; adding Core ops would be ceremony. The MCP policy it wraps is already
;;;; Core's, because a recording transport sits under the same client.
;;;;
;;;; What strict means here, and it is the whole point of the file:
;;;;
;;;;   Replay is positional AND exact. The nth request must be the nth
;;;;   recorded request, compared by method and params rather than by
;;;;   method alone. A replay that matched loosely would let a changed
;;;;   request body pass against an old recording, which is the one failure
;;;;   a recording is supposed to catch.
;;;;
;;;;   A mismatch fails the call. It does not fall through to the live
;;;;   transport, does not skip to the next matching entry, and does not
;;;;   return a stale response, because each of those turns a detected
;;;;   divergence into a silent one.
;;;;
;;;;   Exhaustion is a mismatch too. A replay that runs past the end of its
;;;;   recording is a conversation the recording never saw.
;;;;
;;;; Recorded inbound messages (notifications and server requests) are
;;;; replayed to the handler in order as soon as listening starts, so a
;;;; client that depends on a notification still sees it.

(in-package #:axllm)

(defclass mcp-recording-transport (mcp-transport)
  ((inner :initarg :inner :reader mcp-recording-inner)
   (entries :initform (%new-array) :reader mcp-recording-entries)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-record") :reader %record-lock))
  (:documentation
   "Wraps a transport and records every message that crosses it."))

(defun make-mcp-recording-transport (inner)
  "Record everything crossing INNER. Its entries replay with MAKE-MCP-REPLAY-TRANSPORT."
  (let ((transport (make-instance 'mcp-recording-transport :inner inner)))
    ;; Inbound messages are the inner transport's to deliver; we observe
    ;; them on the way past rather than intercepting, so the client's own
    ;; handler still runs and in the same order.
    (mcp-transport-set-message-handler
     inner
     (lambda (message)
       (%mcp-record transport (object "direction" "inbound" "message" message))
       (mcp-transport-dispatch-inbound transport message)))
    transport))

(defun %mcp-record (transport entry)
  "Record ENTRY, deep-copied so later mutation cannot rewrite it.

A reference would make the recording a view of live objects: a caller that
reuses and mutates one params object across two calls would end up with a
recording claiming it sent the second value both times, and the evidence
would be wrong in the direction that hides the bug. structuredClone is what
the reference does at every push for the same reason."
  (sb-thread:with-mutex ((%record-lock transport))
    (vector-push-extend (axllm/core::core-deep-copy entry)
                        (mcp-recording-entries transport)))
  nil)

(defun mcp-recording-script (transport)
  "TRANSPORT's recording, as the value MAKE-MCP-REPLAY-TRANSPORT takes.

Deep-copied, matching the reference's getRecording. Copying only the vector
would hand out the entry objects themselves, so a caller inspecting or
normalising the returned recording would silently edit the transport's."
  (sb-thread:with-mutex ((%record-lock transport))
    (axllm/core::core-deep-copy (mcp-recording-entries transport))))

(defmethod mcp-transport-era-hint ((transport mcp-recording-transport))
  (mcp-transport-era-hint (mcp-recording-inner transport)))

(defmethod mcp-transport-era-cache-key ((transport mcp-recording-transport))
  (mcp-transport-era-cache-key (mcp-recording-inner transport)))

(defmethod mcp-transport-set-era ((transport mcp-recording-transport) era)
  (mcp-transport-set-era (mcp-recording-inner transport) era))

(defmethod mcp-transport-set-protocol-version ((transport mcp-recording-transport) version)
  (mcp-transport-set-protocol-version (mcp-recording-inner transport) version))

(defmethod mcp-transport-connect ((transport mcp-recording-transport))
  (mcp-transport-connect (mcp-recording-inner transport)))

(defmethod mcp-transport-start-listening ((transport mcp-recording-transport))
  "Forward to the inner transport, or synthesise an abortable handle.

A transport with no server-to-client stream answers :NULL, and a caller
that wants to stop listening then has nothing to call. The reference
synthesises an AbortController-backed handle for exactly that case, so the
caller has one shape to work with either way. The handle here is a
cancellation token, which is the port's existing abortable primitive."
  (let ((handle (mcp-transport-start-listening (mcp-recording-inner transport))))
    (if (and handle (not (eq handle :null)))
        handle
        (let ((done (make-cancellation-token)))
          (object "done" done
                  "close" (lambda ()
                            (cancellation-token-cancel done "MCP recording listener closed")
                            nil))))))

(defmethod mcp-transport-close ((transport mcp-recording-transport))
  (mcp-transport-close (mcp-recording-inner transport)))

(defmethod mcp-transport-terminate-session ((transport mcp-recording-transport))
  (mcp-transport-terminate-session (mcp-recording-inner transport)))

(defmethod mcp-transport-take-request-metadata ((transport mcp-recording-transport) id)
  ;; Forwarded, not recorded. It is per-request bookkeeping the inner
  ;; transport owns, and taking it is destructive, so a recording that
  ;; answered from its own copy would hand out metadata twice.
  (mcp-transport-take-request-metadata (mcp-recording-inner transport) id))

(defmethod mcp-transport-send-batch ((transport mcp-recording-transport) messages
                                     &key context)
  (let ((responses (mcp-transport-send-batch (mcp-recording-inner transport) messages
                                             :context context)))
    ;; One entry per message, pairing each with its own response, as the
    ;; reference does. Recording the batch as a single entry would make the
    ;; recording unreplayable through the ordinary request path.
    (loop for index from 0 below (length messages)
          do (%mcp-record transport
                          (object "direction" "request"
                                  "message" (elt messages index)
                                  "response" (if (< index (length responses))
                                                 (elt responses index)
                                                 :null))))
    responses))

(defmethod mcp-transport-send ((transport mcp-recording-transport) message)
  (let ((response (mcp-transport-send (mcp-recording-inner transport) message)))
    (%mcp-record transport (object "direction" "request"
                                   "message" message
                                   "response" response))
    response))

(defmethod mcp-transport-send-notification ((transport mcp-recording-transport) message)
  (mcp-transport-send-notification (mcp-recording-inner transport) message)
  (%mcp-record transport (object "direction" "notification" "message" message))
  nil)

(defmethod mcp-transport-send-response ((transport mcp-recording-transport) message)
  ;; A reply to a server-initiated request is its own direction in the
  ;; reference's entry union, so it is recorded as one rather than being
  ;; folded into notifications.
  (mcp-transport-send-response (mcp-recording-inner transport) message)
  (%mcp-record transport (object "direction" "response" "message" message))
  nil)

;;; ------------------------------------------------------------------
;;; Replay
;;; ------------------------------------------------------------------

(defclass mcp-replay-transport (mcp-transport)
  ((script :initarg :script :reader mcp-replay-script)
   (cursor :initform 0 :accessor %replay-cursor)
   (era :initarg :era :reader %replay-era)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-replay") :reader %replay-lock))
  (:documentation
   "Answers from a recording, and fails the call on any divergence."))

(defun %mcp-replay-derived-era (script)
  "The era the recording implies, as the reference derives it.

Modern when any recorded request is server/discover or carries the modern
protocol version in its params _meta; legacy otherwise. Derived rather than
asked for, because the recording already contains the answer and a wrong
era would make the client send a request the recording does not have."
  (loop for entry across script
        do (when (equal (%mcp-text (jget entry "direction")) "request")
             (let* ((message (jget entry "message"))
                    (method (%mcp-text (jget message "method")))
                    (params (jget message "params" :null)))
               (when (equal method "server/discover")
                 (return "modern"))
               (when (hash-table-p params)
                 (let ((meta (jget params "_meta" :null)))
                   (when (and (hash-table-p meta)
                              (equal (%mcp-text
                                      (jget meta "io.modelcontextprotocol/protocolVersion"))
                                     (mcp-modern-protocol-version)))
                     (return "modern"))))))
        finally (return "legacy")))

(defun make-mcp-replay-transport (script &key era)
  "Replay SCRIPT, which is a MCP-RECORDING-SCRIPT value.

ERA defaults to the era the recording implies; pass it only to override a
derivation you know to be wrong.

Two deliberate differences from the reference, both documented because the
parity claim should be exact rather than approximate. Params are ALWAYS
compared, where the reference gates that on a strict option: a replay that
accepted a changed argument is not a useful check, so the strict behaviour
is the only behaviour here. And exhaustion is reported as a mismatch
naming the recording, where the reference raises a generic no-entry error."
  (let ((script (if (%array-p script) script (coerce script 'vector))))
    (make-instance 'mcp-replay-transport
                   :script script
                   :era (or era (%mcp-replay-derived-era script)))))

(define-condition mcp-replay-mismatch (mcp-error) ()
  (:documentation
   "The replayed conversation diverged from its recording.

A distinct condition because a divergence is not a server error: the code
under test asked for something the recording never saw, which is a finding
about the code rather than about MCP."))

(defun %mcp-replay-fail (format-control &rest arguments)
  (error 'mcp-replay-mismatch
         :message (apply #'format nil format-control arguments)))

(defmethod mcp-transport-era-hint ((transport mcp-replay-transport))
  (%replay-era transport))

(defmethod mcp-transport-era-cache-key ((transport mcp-replay-transport))
  "replay")

(defmethod mcp-transport-connect ((transport mcp-replay-transport)) nil)
(defmethod mcp-transport-close ((transport mcp-replay-transport)) nil)
(defmethod mcp-transport-terminate-session ((transport mcp-replay-transport)) nil)

(defmethod mcp-transport-take-request-metadata ((transport mcp-replay-transport) id)
  ;; A replay did not retry anything, and saying so is more useful than
  ;; :NULL: a caller reading retryCount gets the truthful zero rather than
  ;; an absence it has to interpret.
  (declare (ignore id))
  (object "retryCount" 0))

(defmethod mcp-transport-send-batch ((transport mcp-replay-transport) messages
                                     &key context)
  ;; Each message replays through the ordinary request path, which is what
  ;; makes a recorded batch replayable at all: the recording holds one entry
  ;; per message, so they are consumed in order like any other requests.
  (declare (ignore context))
  (let ((out (%new-array)))
    (loop for message across (if (%array-p messages) messages (coerce messages 'vector))
          do (vector-push-extend (mcp-transport-send transport message) out))
    out))

(defmethod mcp-transport-start-listening ((transport mcp-replay-transport))
  ;; Recorded inbound messages are delivered in recorded order, so a client
  ;; waiting on a notification sees the same one it saw when recording.
  (loop for entry across (mcp-replay-script transport)
        when (equal (%mcp-text (jget entry "direction")) "inbound")
          ;; Deep-copied: a listener that mutates what it is handed must not
          ;; edit the recording it came from.
          do (mcp-transport-dispatch-inbound
              transport (axllm/core::core-deep-copy (jget entry "message"))))
  nil)

(defun %mcp-replay-next-request (transport method)
  "The next recorded REQUEST entry, or a mismatch naming METHOD.

Only request entries are positional, which is the reference's model: it
filters the recording to requests and indexes those. Notifications and
inbound messages are not caused by a request and must not shift the
position of the next one."
  (sb-thread:with-mutex ((%replay-lock transport))
    (let ((script (mcp-replay-script transport)))
      (loop
        (when (>= (%replay-cursor transport) (length script))
          (%mcp-replay-fail
           "MCP replay ran out of recorded request entries; ~a was not recorded" method))
        (let ((entry (aref script (%replay-cursor transport))))
          (incf (%replay-cursor transport))
          (when (equal (%mcp-text (jget entry "direction")) "request")
            (return entry)))))))

(defun %mcp-replay-check (entry message direction)
  "Insist ENTRY is the recording of MESSAGE."
  (let ((recorded-direction (%mcp-text (jget entry "direction")))
        (recorded (jget entry "message")))
    (unless (equal recorded-direction direction)
      (%mcp-replay-fail "MCP replay expected a recorded ~a, not a ~a"
                        recorded-direction direction))
    (let ((recorded-method (%mcp-text (jget recorded "method")))
          (sent-method (%mcp-text (jget message "method"))))
      (unless (equal recorded-method sent-method)
        (%mcp-replay-fail "MCP replay expected ~a, got ~a" recorded-method sent-method))
      ;; Params as well as method. A recording that only matched the method
      ;; would accept a changed tool argument against an old recording,
      ;; which is exactly the divergence a recording exists to catch.
      (let ((recorded-params (jget recorded "params" :null))
            (sent-params (jget message "params" :null)))
        (unless (axllm/core::core-value-equal recorded-params sent-params)
          (%mcp-replay-fail
           "MCP replay ~a params diverged~%    recorded: ~a~%    sent:     ~a"
           sent-method (encode-json recorded-params) (encode-json sent-params)))))
    entry))

(defmethod mcp-transport-send ((transport mcp-replay-transport) message)
  (let* ((entry (%mcp-replay-next-request transport (%mcp-text (jget message "method"))))
         (checked (%mcp-replay-check entry message "request"))
         (response (jget checked "response" :null)))
    ;; The recorded response carried the recorded request's id; the caller
    ;; is correlating on the id it just sent, so it is rewritten. Nothing
    ;; else about the response is altered.
    ;; Deep-copied, not merged: core-map-merge is shallow, so a caller that
    ;; mutated a nested value in one replayed result would corrupt the
    ;; script and every later replay of it. The reference clones for the
    ;; same reason before overriding the id.
    (if (hash-table-p response)
        (let ((out (axllm/core::core-deep-copy response)))
          (%set-key out "id" (jget message "id" :null))
          out)
        (axllm/core::core-deep-copy response))))

(defmethod mcp-transport-send-notification ((transport mcp-replay-transport) message)
  ;; A no-op, as the reference's sendNotification is. A notification has no
  ;; response to replay, and matching it positionally would make an extra
  ;; or missing notification shift every later request's position -- which
  ;; would report a divergence in the wrong place.
  (declare (ignore message))
  nil)

(defun mcp-replay-exhausted-p (transport)
  "Whether every non-inbound recorded entry has been replayed.

A replay that stops early is also a divergence, but only the caller knows
whether it meant to stop, so this is reported rather than signalled."
  (sb-thread:with-mutex ((%replay-lock transport))
    (let ((remaining 0))
      (loop for index from (%replay-cursor transport)
              below (length (mcp-replay-script transport))
            do (when (equal (%mcp-text (jget (aref (mcp-replay-script transport) index)
                                             "direction"))
                            "request")
                 (incf remaining)))
      (values (zerop remaining) remaining))))

(export '(mcp-recording-transport make-mcp-recording-transport mcp-recording-inner
          mcp-recording-script
          mcp-replay-transport make-mcp-replay-transport mcp-replay-script
          mcp-replay-mismatch mcp-replay-exhausted-p))
