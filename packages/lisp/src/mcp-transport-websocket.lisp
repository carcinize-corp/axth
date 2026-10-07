;;;; mcp-transport-websocket.lisp --- legacy MCP over a WebSocket binding.
;;;;
;;;; The framing, masking, fragmentation, ping/pong and close handshake of
;;;; RFC 6455 are a solved protocol and are NOT reimplemented here. There
;;;; are two ways to get a socket, in this order:
;;;;
;;;;   1. websocket-driver, when its package is loaded in the image. This is
;;;;      the vetted client (fukamachi, BSD-2) and the built-in default.
;;;;      It is detected at run time by package lookup, so this file has no
;;;;      load-time or compile-time dependency on it and the base package
;;;;      stays dependency-light. Debian ships no WebSocket client, so
;;;;      installing it needs Quicklisp:
;;;;
;;;;        (load "~/quicklisp/setup.lisp")
;;;;        ;; Debian's cl-bordeaux-threads 0.8 shadows the v2 API that
;;;;        ;; websocket-driver needs, so install and prefer Quicklisp's.
;;;;        (ql-dist:ensure-installed (ql-dist:find-system "bordeaux-threads"))
;;;;        (dolist (d (directory "~/quicklisp/dists/quicklisp/software/bordeaux-threads-*/"))
;;;;          (push d asdf:*central-registry*))
;;;;        (ql:quickload "websocket-driver-client")
;;;;
;;;;   2. a host :SOCKET-FACTORY, a function of (url protocols) returning an
;;;;      object that answers MCP-WEBSOCKET-SEND, MCP-WEBSOCKET-RECEIVE and
;;;;      MCP-WEBSOCKET-CLOSE. This is how an application binds its own
;;;;      socket, an in-process test double, or a platform client.
;;;;
;;;; With neither available the transport fails closed and names both routes
;;;; rather than pretending to connect.
;;;;
;;;; websocket-driver is event-driven and this transport needs a blocking
;;;; read, so the adapter below owns exactly one thing: a bounded inbox that
;;;; turns :message callbacks into frames MCP-WEBSOCKET-RECEIVE can return,
;;;; and turns a close or error into an end of stream. No protocol logic.
;;;;
;;;; What this file also owns is the request correlation that a naive
;;;; implementation gets wrong: each pending request owns its own slot, so a
;;;; cancelled request can never remove a later request that reuses its id,
;;;; and concurrent duplicate ids are rejected instead of crossing replies.

(in-package #:axllm)

(defgeneric mcp-websocket-send (socket text)
  (:documentation "Send one text frame.")
  (:method ((socket function) text) (funcall socket :send text)))

(defgeneric mcp-websocket-receive (socket)
  (:documentation "Block for the next text frame; NIL once closed.")
  (:method ((socket function) ) (funcall socket :receive)))

(defgeneric mcp-websocket-close (socket)
  (:documentation "Close the socket.")
  (:method ((socket function)) (funcall socket :close)))

(defstruct (%mcp-ws-slot (:conc-name %mcp-ws-slot-))
  (done (make-cancellation-token)) response error)

;;; ------------------------------------------------------------------
;;; Built-in socket: websocket-driver, when it is loaded
;;; ------------------------------------------------------------------

(defclass websocket-driver-socket ()
  ((client :initarg :client :reader %wds-client)
   (inbox :initform '() :accessor %wds-inbox)
   (closed :initform nil :accessor %wds-closed)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-wsd") :reader %wds-lock)
   (gate :initform (sb-thread:make-waitqueue) :reader %wds-gate))
  (:documentation
   "A blocking-read adapter over an event-driven websocket-driver client."))

(defun mcp-websocket-driver-available-p ()
  "Whether websocket-driver is loaded in this image."
  (and (find-package "WEBSOCKET-DRIVER")
       (find-symbol "MAKE-CLIENT" "WEBSOCKET-DRIVER")
       t))

(defun %mcp-wds-call (name &rest arguments)
  (apply (symbol-function (find-symbol name "WEBSOCKET-DRIVER")) arguments))

(defun %mcp-wds-push (socket frame)
  (sb-thread:with-mutex ((%wds-lock socket))
    (setf (%wds-inbox socket) (append (%wds-inbox socket) (list frame)))
    (sb-thread:condition-broadcast (%wds-gate socket))))

(defun make-websocket-driver-socket (url protocols)
  "Connect to URL with websocket-driver and adapt it to a blocking read."
  (unless (mcp-websocket-driver-available-p)
    (%mcp-fail "websocket-driver is not loaded in this image"))
  (let* ((client (if protocols
                     (%mcp-wds-call "MAKE-CLIENT" url :protocols protocols)
                     (%mcp-wds-call "MAKE-CLIENT" url)))
         (socket (make-instance 'websocket-driver-socket :client client)))
    (%mcp-wds-call "ON" :message client (lambda (message) (%mcp-wds-push socket message)))
    (%mcp-wds-call "ON" :close client
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (sb-thread:with-mutex ((%wds-lock socket))
                   (setf (%wds-closed socket) t)
                   (sb-thread:condition-broadcast (%wds-gate socket)))))
    (%mcp-wds-call "ON" :error client
               (lambda (&rest ignored)
                 (declare (ignore ignored))
                 (sb-thread:with-mutex ((%wds-lock socket))
                   (setf (%wds-closed socket) t)
                   (sb-thread:condition-broadcast (%wds-gate socket)))))
    (%mcp-wds-call "START-CONNECTION" client)
    socket))

(defmethod mcp-websocket-send ((socket websocket-driver-socket) text)
  (%mcp-wds-call "SEND" (%wds-client socket) text))

(defmethod mcp-websocket-receive ((socket websocket-driver-socket))
  (sb-thread:with-mutex ((%wds-lock socket))
    (loop
      (let ((inbox (%wds-inbox socket)))
        (when inbox
          (setf (%wds-inbox socket) (rest inbox))
          (return (first inbox))))
      ;; An empty inbox on a closed socket is the end of the stream, which
      ;; the transport reads as a disconnect rather than a stall.
      (when (%wds-closed socket) (return nil))
      (sb-thread:condition-wait (%wds-gate socket) (%wds-lock socket)))))

(defmethod mcp-websocket-close ((socket websocket-driver-socket))
  (sb-thread:with-mutex ((%wds-lock socket))
    (setf (%wds-closed socket) t)
    (sb-thread:condition-broadcast (%wds-gate socket)))
  (ignore-errors (%mcp-wds-call "CLOSE-CONNECTION" (%wds-client socket)))
  nil)

(defclass mcp-websocket-transport (mcp-transport)
  ((url :initarg :url :reader mcp-websocket-url)
   (protocols :initarg :protocols :reader mcp-websocket-protocols)
   (factory :initarg :factory :reader %ws-factory)
   (socket :initform nil :accessor %ws-socket)
   (reader :initform nil :accessor %ws-reader)
   (pending :initform (make-hash-table :test #'equal) :reader %ws-pending)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-websocket") :reader %ws-lock))
  (:documentation "Legacy MCP over a host-supplied WebSocket."))

(defun %mcp-websocket-http-url (url)
  "URL with its ws scheme mapped to the http one, for the SSRF gate.

The gate reasons about http and https; ws and wss are the same two
transports with the same reachability questions, so they are checked as
http and https rather than given a second, weaker policy."
  (cond ((and (>= (length url) 6) (string-equal "wss://" (subseq url 0 6)))
         (concatenate 'string "https://" (subseq url 6)))
        ((and (>= (length url) 5) (string-equal "ws://" (subseq url 0 5)))
         (concatenate 'string "http://" (subseq url 5)))
        (t url)))

(defun make-mcp-websocket-transport (url &key protocols socket-factory ssrf-protection
                                              trust-socket-factory-tls)
  "A WebSocket transport for URL.

SOCKET-FACTORY receives (url protocols) and returns the socket. Omit it to
use websocket-driver when this image has it loaded. PROTOCOLS is a string or
a list of subprotocol names.

URL passes the same SSRF gate as an HTTP endpoint, with ws and wss checked
as http and https, so a WebSocket URL cannot reach a host an HTTP endpoint
would be refused. SSRF-PROTECTION relaxes it the same way.

A wss:// URL is refused on the built-in websocket-driver route. That
library connects with TLS :verify :optional, so it will complete a
handshake against an unverified certificate or a mismatched hostname, and
this transport may carry an Authorization header or a DPoP proof. Failing
closed is the only safe default; a caller that has verified the peer itself
passes its own :socket-factory and sets TRUST-SOCKET-FACTORY-TLS, which
asserts that the factory performs certificate and hostname verification."
  (let ((checked (mcp-validate-endpoint (%mcp-websocket-http-url url) ssrf-protection)))
    (declare (ignore checked))
    (when (and (>= (length url) 6) (string-equal "wss://" (subseq url 0 6)))
      (cond ((and socket-factory trust-socket-factory-tls) nil)
            (socket-factory
             (%mcp-fail "MCP WebSocket over wss:// needs :trust-socket-factory-tls, which asserts that your :socket-factory verifies the certificate and hostname"))
            (t
             (%mcp-fail "MCP WebSocket refuses wss:// on the built-in websocket-driver route, which connects with TLS :verify :optional and would accept an unverified certificate or a mismatched hostname while this transport carries credentials; supply a :socket-factory that verifies the peer and set :trust-socket-factory-tls"))))
    (make-instance 'mcp-websocket-transport
                   :url url
                   :protocols (cond ((null protocols) '())
                                    ((stringp protocols) (list protocols))
                                    (t protocols))
                   :factory socket-factory)))

(defmethod mcp-transport-era-hint ((transport mcp-websocket-transport))
  "A WebSocket binding is a stateful session, never a stateless modern
HTTP endpoint."
  "legacy")

(defmethod mcp-transport-connect ((transport mcp-websocket-transport))
  (sb-thread:with-mutex ((%ws-lock transport))
    (when (%ws-socket transport) (return-from mcp-transport-connect nil))
    (let ((factory (or (%ws-factory transport)
                       (and (mcp-websocket-driver-available-p)
                            #'make-websocket-driver-socket))))
      (unless (functionp factory)
        (%mcp-fail "MCP WebSocket needs websocket-driver loaded in this image or a :socket-factory; see mcp-transport-websocket.lisp for both routes"))
      (let ((socket (funcall factory (mcp-websocket-url transport)
                             (mcp-websocket-protocols transport))))
        (unless socket (%mcp-fail "MCP WebSocket factory returned no socket"))
        (setf (%ws-socket transport) socket
              (%ws-reader transport)
              (sb-thread:make-thread (lambda () (%mcp-ws-read-loop transport socket))
                                     :name "ax-mcp-websocket")))))
  nil)

(defmethod mcp-transport-start-listening ((transport mcp-websocket-transport))
  (mcp-transport-connect transport))

(defun %mcp-ws-read-loop (transport socket)
  (handler-case
      (loop
        (let ((raw (mcp-websocket-receive socket)))
          (when (or (null raw) (and (stringp raw) (zerop (length raw))))
            (%mcp-fail "MCP WebSocket closed"))
          (let* ((parsed (if (stringp raw) (parse-json raw) raw))
                 (batch (%array-p parsed))
                 (messages (if batch parsed (vector parsed))))
            (when (and batch (not (equal (mcp-transport-protocol-version transport) "2025-03-26")))
              (%mcp-fail "JSON-RPC batching is only allowed for MCP 2025-03-26"))
            (loop for message across messages
                  do (let ((slot (sb-thread:with-mutex ((%ws-lock transport))
                                   (unless (eq (%ws-socket transport) socket)
                                     (return-from %mcp-ws-read-loop nil))
                                   (when (and (%mcp-present-key-p message "id")
                                              (not (%mcp-present-key-p message "method")))
                                     (let* ((key (encode-json (jget message "id")))
                                            (found (gethash key (%ws-pending transport))))
                                       (when found
                                         (remhash key (%ws-pending transport))
                                         found))))))
                       (if slot
                           (progn (setf (%mcp-ws-slot-response slot) message)
                                  (cancellation-token-cancel (%mcp-ws-slot-done slot) "response"))
                           ;; A server request or notification: handling it may
                           ;; block on a host handler, so it must not stall the
                           ;; reader that every pending reply depends on.
                           (sb-thread:make-thread
                            (lambda () (%mcp-ws-dispatch transport socket message))
                            :name "ax-mcp-websocket-inbound")))))))
    (error (condition) (%mcp-ws-terminate transport socket condition))))

(defun %mcp-ws-dispatch (transport socket message)
  (sb-thread:with-mutex ((%ws-lock transport))
    (unless (eq (%ws-socket transport) socket) (return-from %mcp-ws-dispatch nil)))
  (let ((request-handler (%transport-request-handler transport)))
    (if (and request-handler (%mcp-present-key-p message "id") (%mcp-present-key-p message "method"))
        (let ((response (funcall request-handler message)))
          (sb-thread:with-mutex ((%ws-lock transport))
            (unless (eq (%ws-socket transport) socket) (return-from %mcp-ws-dispatch nil)))
          ;; A late reply belongs to the connection that asked for it and
          ;; must never reconnect a closed socket.
          (mcp-websocket-send socket (encode-json response)))
        (let ((handler (%transport-message-handler transport)))
          (when handler (funcall handler message)))))
  nil)

(defun %mcp-ws-terminate (transport socket condition)
  (let ((pending '()))
    (sb-thread:with-mutex ((%ws-lock transport))
      (unless (eq (%ws-socket transport) socket) (return-from %mcp-ws-terminate nil))
      (setf (%ws-socket transport) nil)
      (maphash (lambda (key slot) (declare (ignore key)) (push slot pending))
               (%ws-pending transport))
      (clrhash (%ws-pending transport)))
    (dolist (slot pending)
      (setf (%mcp-ws-slot-error slot) condition)
      (cancellation-token-cancel (%mcp-ws-slot-done slot) "terminated")))
  (unwind-protect (ignore-errors (mcp-websocket-close socket))
    (%mcp-transport-lifecycle transport "disconnected"))
  nil)

(defun %mcp-ws-send-requests (transport messages context batch)
  (let ((ids (map 'list #'%mcp-text
                  (axllm/core::mcp-websocket-request-ids
                   (coerce messages 'vector)
                   (%mcp-text (mcp-transport-protocol-version transport))
                   (json-boolean batch)))))
    (%mcp-check-context context)
    ;; Serialize before registering anything: a serialization failure must
    ;; leave no pending slot behind.
    (let ((payload (encode-json (if batch (coerce messages 'vector) (first messages))))
          (slots (mapcar (lambda (id) (declare (ignore id)) (make-%mcp-ws-slot)) ids)))
      (mcp-transport-connect transport)
      (let ((socket (sb-thread:with-mutex ((%ws-lock transport))
                      (when (some (lambda (id) (gethash id (%ws-pending transport))) ids)
                        (%mcp-fail "MCP request ID is already pending"))
                      (let ((socket (%ws-socket transport)))
                        (unless socket (%mcp-fail "MCP WebSocket closed"))
                        (loop for id in ids for slot in slots
                              do (setf (gethash id (%ws-pending transport)) slot))
                        socket))))
        (unwind-protect
             (progn
               (%mcp-check-context context)
               (mcp-websocket-send socket payload)
               (mapcar (lambda (slot)
                         (loop until (cancellation-token-wait (%mcp-ws-slot-done slot) 0.01)
                               do (%mcp-check-context context))
                         (%mcp-check-context context)
                         (when (%mcp-ws-slot-error slot) (error (%mcp-ws-slot-error slot)))
                         (%mcp-ws-slot-response slot))
                       slots))
          (sb-thread:with-mutex ((%ws-lock transport))
            (loop for id in ids for slot in slots
                  do (when (eq (gethash id (%ws-pending transport)) slot)
                       (remhash id (%ws-pending transport))))))))))

(defmethod mcp-transport-send ((transport mcp-websocket-transport) message)
  (first (%mcp-ws-send-requests transport (list message) :null nil)))

(defmethod mcp-transport-send-with-context ((transport mcp-websocket-transport)
                                            message headers context)
  (declare (ignore headers))
  (first (%mcp-ws-send-requests transport (list message) context nil)))

(defun mcp-websocket-send-batch (transport messages &key context)
  "Send a JSON-RPC batch. Negotiated MCP 2025-03-26 only, as Core checks."
  (%mcp-ws-send-requests transport (coerce messages 'list) (or context :null) t))

(defmethod mcp-transport-send-batch ((transport mcp-websocket-transport) messages
                                     &key context)
  ;; Core decides whether the negotiated protocol allows a batch at all; this
  ;; only routes to the binding that can carry one.
  (mcp-websocket-send-batch transport messages :context (or context :null)))

(defmethod mcp-transport-send-notification ((transport mcp-websocket-transport) message)
  (mcp-transport-connect transport)
  (let ((socket (sb-thread:with-mutex ((%ws-lock transport)) (%ws-socket transport))))
    (unless socket (%mcp-fail "MCP WebSocket closed"))
    (mcp-websocket-send socket (encode-json message)))
  nil)

(defmethod mcp-transport-close ((transport mcp-websocket-transport))
  (let ((socket (sb-thread:with-mutex ((%ws-lock transport)) (%ws-socket transport))))
    (when socket
      (%mcp-ws-terminate transport socket (make-condition 'mcp-error :message "MCP WebSocket closed"))))
  nil)

(export '(mcp-websocket-transport make-mcp-websocket-transport
          mcp-websocket-driver-available-p make-websocket-driver-socket
          mcp-websocket-send mcp-websocket-receive mcp-websocket-close
          mcp-websocket-send-batch mcp-websocket-url mcp-websocket-protocols))
