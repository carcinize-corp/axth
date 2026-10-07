;;;; mcp-transport-http.lisp --- MCP over Streamable HTTP.
;;;;
;;;; One endpoint, two wire models. Legacy sessions POST requests, capture
;;;; MCP-Session-Id, and resume a long-lived GET/SSE stream with
;;;; Last-Event-ID. Modern requests are stateless, carry Core's routing and
;;;; protocol headers, and listen through a long-running
;;;; subscriptions/listen POST stream instead of a GET.
;;;;
;;;; Core decides every header: MCP-HEADER-VALUE-PLAN says when a value must
;;;; be base64-wrapped, MCP-REQUEST-NAME says what Mcp-Name carries, and the
;;;; whole OAuth middle tier is Core's. This file owns sockets, threads,
;;;; timeouts, the SSRF gate, cancellation that actually interrupts a
;;;; request, and the credential handling around it.
;;;;
;;;; HTTP comes from Drakma and TLS from cl+ssl; nothing here reimplements
;;;; either. Redirects are disabled on every request, because following one
;;;; would replay an Authorization or DPoP header to a host the server chose.

(in-package #:axllm)

(defclass mcp-streamable-http-transport (mcp-transport)
  ((endpoint :initarg :endpoint :reader mcp-http-endpoint)
   (options :initarg :options :reader mcp-http-options)
   (headers :initarg :headers :accessor mcp-http-headers)
   (session-id :initform :null :accessor mcp-http-session-id)
   (era :initform :null :accessor mcp-http-era)
   (era-cache-key :initarg :era-cache-key :reader %http-era-cache-key)
   (last-headers :initform (object) :accessor mcp-http-last-headers)
   (last-event-id :initform :null :accessor %http-last-event-id)
   (listen-stop :initform nil :accessor %http-listen-stop)
   (listen-thread :initform nil :accessor %http-listen-thread)
   (listen-stream :initform nil :accessor %http-listen-stream)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-http") :reader %http-lock))
  (:documentation
   "MCP over Streamable HTTP, for both the legacy and modern wire models."))

(defun make-mcp-streamable-http-transport (endpoint &rest options)
  "An HTTP transport for ENDPOINT.

Useful options:

  :ssrf-protection   relax requireHttps/allowLocalhost/allowPrivateNetworks
  :headers           a JSON object of static headers
  :authorization     a literal Authorization header value
  :authentication    one strategy, or a list, from mcp-auth.lisp
  :dpop              an MCP-DPOP-FACTORY; its proof is sent per request
  :oauth             MCP-OAUTH-OPTIONS for the portable OAuth middle tier
  :timeout           per-request seconds (default 30)
  :listen-timeout    stream read seconds (default 300)

The endpoint passes the SSRF gate here, at construction, so a bad endpoint
fails before any request is built."
  (let ((table (object)))
    (loop for (key value) on options by #'cddr
          do (%set-key table (%mcp-option-name key) value))
    (let* ((checked (mcp-validate-endpoint endpoint (jget table "ssrfProtection")))
           (uri (puri:parse-uri checked))
           (headers (axllm/core::core-map-merge (object) (%mcp-object-or-empty (jget table "headers")))))
      (let ((authorization (jget table "authorization")))
        (when (stringp authorization) (%set-key headers "Authorization" authorization)))
      (make-instance 'mcp-streamable-http-transport
                     :endpoint checked :options table :headers headers
                     :era-cache-key (format nil "~(~a~)://~a~@[:~a~]"
                                            (puri:uri-scheme uri) (puri:uri-host uri)
                                            (let ((port (puri:uri-port uri)))
                                              (and port (not (member port '(80 443))) port)))))))

(defun %mcp-http-option (transport name &optional (fallback :null))
  (jget (mcp-http-options transport) name fallback))

(defun %mcp-http-number-option (transport name default)
  (let ((value (%mcp-http-option transport name)))
    (if (realp value) value default)))

(defun mcp-http-set-headers (transport headers)
  (setf (mcp-http-headers transport)
        (axllm/core::core-map-merge (object) (%mcp-object-or-empty headers))))

(defun mcp-http-set-authorization (transport authorization)
  (%set-key (mcp-http-headers transport) "Authorization" authorization))

(defmethod mcp-transport-era-cache-key ((transport mcp-streamable-http-transport))
  (%http-era-cache-key transport))

(defmethod mcp-transport-set-era ((transport mcp-streamable-http-transport) era)
  (setf (mcp-http-era transport) era)
  (if (equal era "modern")
      ;; Modern MCP is stateless: a session id from a previous legacy probe
      ;; must not leak into a modern request.
      (setf (mcp-http-session-id transport) :null
            (mcp-transport-protocol-version transport) (mcp-modern-protocol-version))
      (when (equal (mcp-transport-protocol-version transport) (mcp-modern-protocol-version))
        (setf (mcp-transport-protocol-version transport) :null)))
  nil)

(defun mcp-http-build-headers (transport &key base (include-protocol-version t)
                                              method params extra-headers)
  "The headers one request carries, as Core plans them."
  (let ((headers (axllm/core::core-map-merge (mcp-http-headers transport)
                                             (%mcp-object-or-empty base)))
        (modern (equal (mcp-http-era transport) "modern")))
    (let ((extra (%mcp-object-or-empty extra-headers)))
      (dolist (key (%object-keys extra))
        (let ((value (%mcp-text (gethash key extra))))
          (%set-key headers key
                    (if (and modern (>= (length key) 10)
                             (string-equal "mcp-param-" (subseq key 0 10)))
                        (%mcp-encode-header-value value)
                        value)))))
    (when (and (not modern) (stringp (mcp-http-session-id transport)))
      (%set-key headers "MCP-Session-Id" (mcp-http-session-id transport)))
    (when (and (or modern include-protocol-version)
               (stringp (mcp-transport-protocol-version transport)))
      (%set-key headers "MCP-Protocol-Version" (mcp-transport-protocol-version transport)))
    (when (and modern method (plusp (length (%mcp-text method))))
      (%set-key headers "Mcp-Method" (%mcp-text method))
      (let ((name (%mcp-text (axllm/core::mcp-request-name (%mcp-text method)
                                                           (%mcp-object-or-empty params)))))
        (when (plusp (length name))
          (%set-key headers "Mcp-Name" (%mcp-encode-header-value name)))))
    (setf (mcp-http-last-headers transport)
          (axllm/core::core-map-merge (object) headers))
    headers))

(defun %mcp-http-header-alist (transport headers url method body)
  "HEADERS as Drakma's alist, after authentication and DPoP have run.

Returns (values url alist) because an API-key strategy may add a query
parameter."
  (multiple-value-bind (final-url authenticated)
      (mcp-apply-authentication url headers method body (%mcp-http-option transport "authentication"))
    (let ((dpop (%mcp-http-option transport "dpop")))
      (when (typep dpop 'mcp-dpop-factory)
        (let* ((authorization (jget authenticated "Authorization"))
               (token (and (stringp authorization)
                           (let ((space (position #\Space authorization)))
                             (and space (subseq authorization (1+ space)))))))
          (%set-key authenticated "DPoP"
                    (mcp-dpop-proof dpop :url final-url :method (or method "POST")
                                         :access-token token))
          ;; A DPoP-bound access token is presented with the DPoP scheme,
          ;; never as a bearer token.
          (when (and (stringp authorization) token
                     (string-equal "bearer " (subseq authorization 0 (min 7 (length authorization)))))
            (%set-key authenticated "Authorization" (format nil "DPoP ~a" token))))))
    (values final-url
            (loop for key in (%object-keys authenticated)
                  collect (cons key (%mcp-text (gethash key authenticated)))))))

;;; ------------------------------------------------------------------
;;; Requests
;;; ------------------------------------------------------------------

(defmethod mcp-transport-send ((transport mcp-streamable-http-transport) message)
  (mcp-transport-send-with-context transport message (object) :null))

(defmethod mcp-transport-send-with-headers ((transport mcp-streamable-http-transport)
                                            message headers)
  (mcp-transport-send-with-context transport message headers :null))

(defmethod mcp-transport-send-with-context ((transport mcp-streamable-http-transport)
                                            message extra-headers context)
  (%mcp-check-context context)
  (let* ((body (encode-json message))
         (method (%mcp-text (jget message "method")))
         (headers (mcp-http-build-headers
                   transport
                   :base (object "Content-Type" "application/json"
                                 "Accept" "application/json, text/event-stream")
                   :include-protocol-version (not (string= method "initialize"))
                   :method method
                   :params (%mcp-object-or-empty (jget message "params"))
                   :extra-headers extra-headers)))
    (multiple-value-bind (url alist)
        (%mcp-http-header-alist transport headers (mcp-http-endpoint transport) "POST" body)
      (multiple-value-bind (text status response-headers)
          (%mcp-http-post-with-cancellation transport url alist body context)
        (%mcp-check-context context)
        (when (and (eql status 401)
                   (%mcp-apply-oauth transport (cdr (assoc :www-authenticate response-headers))))
          (return-from mcp-transport-send-with-context
            (mcp-transport-send-with-context transport message extra-headers context)))
        (unless (and (integerp status) (<= 200 status 299))
          (%mcp-fail "HTTP error ~a from ~a" status url))
        (%mcp-http-capture-session transport response-headers)
        (if (zerop (length text))
            (object "jsonrpc" "2.0" "id" (jget message "id") "result" (object))
            ;; A spec-compliant server may answer a JSON-RPC POST with an SSE
            ;; stream carrying the response plus interleaved notifications.
            (if (search "text/event-stream"
                        (string-downcase (or (cdr (assoc :content-type response-headers)) "")))
                (%mcp-select-sse-response transport (%mcp-parse-sse-messages text) (jget message "id"))
                (parse-json text)))))))

(defun %mcp-http-post-with-cancellation (transport url alist body context)
  "POST BODY, abandoning the request when CONTEXT is cancelled.

The request runs on its own thread and this one waits for whichever comes
first, the response or the cancellation. That structure is the point: a
server that has not answered yet is the common case for a cancellation, and
the socket Drakma owns is not reachable from here until it returns. An
earlier version subscribed to the token AFTER the call returned, which
could only ever have aborted a request that had already finished -- a
loopback check with a deliberately slow endpoint is what exposed it.

On cancellation the request thread is unwound, which runs Drakma's own
cleanup and closes the socket, and this function signals rather than
returning a partial result. The caller's client stays usable: nothing about
the transport's own state is disturbed by abandoning one request."
  (let* ((timeout (%mcp-http-number-option transport "timeout" 30))
         (token (and (hash-table-p context)
                     (let ((value (jget context "cancellation")))
                       (and (typep value 'cancellation-token) value))))
         (done (sb-thread:make-semaphore))
         (outcome nil)
         (failure nil))
    (flet ((perform ()
             (handler-case
                 (setf outcome
                       (multiple-value-list
                        (drakma:http-request url
                                             :method :post
                                             :additional-headers
                                             (remove "content-type" alist
                                                     :key #'car :test #'string-equal)
                                             :content-type "application/json"
                                             :content body
                                             :external-format-out :utf-8
                                             :external-format-in :utf-8
                                             :connection-timeout timeout
                                             :verify :required
                                             :redirect nil
                                             :force-binary nil
                                             :want-stream nil
                                             :close t)))
               (error (condition) (setf failure condition)))))
      (if (null token)
          ;; No token: no reason to pay for a thread.
          (perform)
          (let ((worker (sb-thread:make-thread #'perform :name "ax-mcp-http-request")))
            (unwind-protect
                 (loop
                   (when (sb-thread:wait-on-semaphore done :timeout 0.02)
                     (return))
                   (unless (sb-thread:thread-alive-p worker) (return))
                   (when (cancellation-token-cancelled-p token)
                     (ignore-errors (sb-thread:terminate-thread worker))
                     (ignore-errors (sb-thread:join-thread worker :timeout 2 :default nil))
                     (%mcp-check-context context)
                     ;; A cancelled token whose own check did not signal still
                     ;; must not look like a successful request.
                     (%mcp-fail "MCP HTTP request was cancelled")))
              (sb-thread:signal-semaphore done)))))
    (cond (failure
           (%mcp-check-context context)
           (if (typep failure 'mcp-error)
               (error failure)
               (%mcp-fail "MCP HTTP transport failure: ~a"
                          (substitute #\Space #\Newline (princ-to-string failure)))))
          ((null outcome)
           (%mcp-check-context context)
           (%mcp-fail "MCP HTTP request produced no response"))
          (t (destructuring-bind (raw status response-headers &rest ignored) outcome
               (declare (ignore ignored))
               (values (if (stringp raw) raw (%mcp-from-utf8 raw)) status response-headers))))))

(defun %mcp-select-sse-response (transport messages request-id)
  "The response whose id matches REQUEST-ID; anything else is inbound."
  (let ((response nil))
    (loop for message across messages
          do (if (and (null response)
                      (hash-table-p message)
                      (axllm/core::core-value-equal (jget message "id") request-id))
                 (setf response message)
                 (mcp-transport-dispatch-inbound transport message)))
    (or response
        (if (plusp (length messages))
            (aref messages (1- (length messages)))
            (object "jsonrpc" "2.0" "id" request-id "result" (object))))))

(defmethod mcp-transport-send-notification ((transport mcp-streamable-http-transport) message)
  (let ((response (mcp-transport-send transport message)))
    (when (%mcp-present-key-p response "error")
      (let ((error-object (%mcp-object-or-empty (jget response "error"))))
        (error 'mcp-error
               :message (%mcp-text (jget error-object "message" "MCP notification failed"))
               :code (let ((code (jget error-object "code")))
                       (if (realp code) (round code) :null))))))
  nil)

(defun %mcp-http-capture-session (transport response-headers)
  (unless (equal (mcp-http-era transport) "modern")
    (let ((session (cdr (assoc :mcp-session-id response-headers))))
      (when (and session (plusp (length session)))
        (setf (mcp-http-session-id transport) session))))
  nil)

(defmethod mcp-transport-terminate-session ((transport mcp-streamable-http-transport))
  (mcp-http-terminate-session transport))

(defun mcp-http-terminate-session (transport)
  "Forget the legacy session id. Modern MCP has no session to terminate."
  (unless (equal (mcp-http-era transport) "modern")
    (setf (mcp-http-session-id transport) :null))
  nil)

;;; ------------------------------------------------------------------
;;; Streams
;;; ------------------------------------------------------------------

(defmethod %mcp-transport-listen-thread ((transport mcp-streamable-http-transport))
  (%http-listen-thread transport))

(defmethod mcp-transport-start-listening ((transport mcp-streamable-http-transport))
  (when (equal (mcp-http-era transport) "modern")
    (%mcp-fail "Modern MCP uses subscriptions/listen via openRequestStream, not HTTP GET"))
  (sb-thread:with-mutex ((%http-lock transport))
    (let ((thread (%http-listen-thread transport)))
      (when (and thread (sb-thread:thread-alive-p thread))
        (return-from mcp-transport-start-listening nil)))
    (let ((stop (make-cancellation-token)))
      (setf (%http-listen-stop transport) stop
            (%http-listen-thread transport)
            (sb-thread:make-thread (lambda () (%mcp-http-listen-loop transport stop))
                                   :name "ax-mcp-sse"))))
  nil)

(defun %mcp-http-listen-loop (transport stop)
  "Supervise a legacy GET/SSE stream, resuming with Last-Event-ID.

A reconnect after a successful connection reports \"reconnected\" so the
client can restore its logical resource subscriptions; a drop reports
\"disconnected\". Neither is reported once the host has asked us to stop."
  (let ((connected-once nil)
        (delay (%mcp-http-number-option transport "reconnectDelay" 0.1)))
    (loop until (cancellation-token-cancelled-p stop)
          do (handler-case
                 (let ((headers (mcp-http-build-headers
                                 transport :base (object "Accept" "text/event-stream"))))
                   (when (stringp (%http-last-event-id transport))
                     (%set-key headers "Last-Event-ID" (%http-last-event-id transport)))
                   (multiple-value-bind (url alist)
                       (%mcp-http-header-alist transport headers (mcp-http-endpoint transport)
                                           "GET" nil)
                     (multiple-value-bind (stream status response-headers)
                         (drakma:http-request url
                                              :method :get
                                              :additional-headers alist
                                              :external-format-in :utf-8
                                              :connection-timeout
                                              (%mcp-http-number-option transport "listenTimeout" 300)
                                              :verify :required
                                              :redirect nil
                                              :want-stream t)
                       (unless (and (integerp status) (<= 200 status 299))
                         (ignore-errors (close stream))
                         (%mcp-fail "MCP listen stream failed: HTTP ~a" status))
                       (setf (%http-listen-stream transport) stream)
                       (%mcp-http-capture-session transport response-headers)
                       (when (and connected-once (not (cancellation-token-cancelled-p stop)))
                         (%mcp-transport-lifecycle transport "reconnected"))
                       (setf connected-once t)
                       (unwind-protect
                            (%mcp-http-read-sse transport stream stop t)
                         (setf (%http-listen-stream transport) nil)
                         (ignore-errors (close stream)))
                       (unless (cancellation-token-cancelled-p stop)
                         (%mcp-transport-lifecycle transport "disconnected")))))
               (error ()
                 (setf (%http-listen-stream transport) nil)
                 (when (and connected-once (not (cancellation-token-cancelled-p stop)))
                   (%mcp-transport-lifecycle transport "disconnected"))))
             (unless (cancellation-token-cancelled-p stop)
               (cancellation-token-wait stop delay)))))

(defun %mcp-http-read-sse (transport stream stop track-event-id)
  "Read SSE frames from STREAM until it ends or STOP is cancelled."
  (let ((data '()) (event-id nil))
    (loop
      (when (cancellation-token-cancelled-p stop) (return))
      (let ((line (handler-case (read-line stream nil nil) (error () nil))))
        (unless line (return))
        (let ((text (string-right-trim '(#\Return) line)))
          (cond ((and track-event-id (>= (length text) 3) (string= "id:" (subseq text 0 3)))
                 (setf event-id (string-trim '(#\Space #\Tab) (subseq text 3))))
                ((and (>= (length text) 5) (string= "data:" (subseq text 0 5)))
                 (push (string-left-trim '(#\Space #\Tab) (subseq text 5)) data))
                ((zerop (length text))
                 (when (and event-id track-event-id)
                   (setf (%http-last-event-id transport) event-id))
                 (when data
                   (let ((payload (format nil "~{~a~^~%~}" (nreverse data))))
                     (handler-case
                         (mcp-transport-dispatch-inbound transport (parse-json payload))
                       ;; A malformed frame must not take the stream down.
                       (error () nil))))
                 (setf data '() event-id nil))))))))

(defmethod mcp-transport-open-request-stream ((transport mcp-streamable-http-transport) message)
  (unless (equal (mcp-http-era transport) "modern")
    (%mcp-fail "Request streams are only available for modern MCP"))
  (mcp-transport-close-request-stream transport)
  (let ((stop (make-cancellation-token))
        (request (%mcp-json-clone message)))
    (setf (%http-listen-stop transport) stop
          (%http-listen-thread transport)
          (sb-thread:make-thread (lambda () (%mcp-http-request-stream transport request stop))
                                 :name "ax-mcp-request-stream")))
  nil)

(defun %mcp-http-request-stream (transport message stop)
  "Run one modern long-lived POST stream to completion."
  (unwind-protect
       (handler-case
           (let* ((body (encode-json message))
                  (headers (mcp-http-build-headers
                            transport
                            :base (object "Content-Type" "application/json"
                                          "Accept" "text/event-stream")
                            :method (%mcp-text (jget message "method"))
                            :params (%mcp-object-or-empty (jget message "params")))))
             (multiple-value-bind (url alist)
                 (%mcp-http-header-alist transport headers (mcp-http-endpoint transport) "POST" body)
               (multiple-value-bind (stream status)
                   (drakma:http-request url
                                        :method :post
                                        :additional-headers
                                        (remove "content-type" alist
                                                :key #'car :test #'string-equal)
                                        :content-type "application/json"
                                        :content body
                                        :external-format-out :utf-8
                                        :external-format-in :utf-8
                                        :connection-timeout
                                        (%mcp-http-number-option transport "listenTimeout" 300)
                                        :verify :required
                                        :redirect nil
                                        :want-stream t)
                 (unless (and (integerp status) (<= 200 status 299))
                   (ignore-errors (close stream))
                   (%mcp-fail "MCP request stream failed: HTTP ~a" status))
                 (setf (%http-listen-stream transport) stream)
                 (unwind-protect
                      ;; A modern stream carries no Last-Event-ID resume:
                      ;; an interest change restarts it with a fresh id.
                      (%mcp-http-read-sse transport stream stop nil)
                   (setf (%http-listen-stream transport) nil)
                   (ignore-errors (close stream))))))
         (error () nil))
    (unless (cancellation-token-cancelled-p stop)
      (%mcp-transport-lifecycle transport "disconnected"))))

(defmethod mcp-transport-close-request-stream ((transport mcp-streamable-http-transport))
  (%mcp-http-stop-stream transport)
  nil)

(defun %mcp-http-stop-stream (transport)
  "Stop whichever stream is running and wait briefly for its thread."
  (let ((stop (%http-listen-stop transport))
        (stream (%http-listen-stream transport))
        (thread (%http-listen-thread transport)))
    (when stop (cancellation-token-cancel stop "closed"))
    ;; Aborting the socket is what actually unblocks a reader parked in
    ;; READ-LINE; cancelling the token alone would wait for the server.
    (when stream (ignore-errors (close stream :abort t)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (ignore-errors
       (sb-thread:join-thread thread :default nil
                                     :timeout (%mcp-http-number-option transport "closeTimeout" 2))))
    (setf (%http-listen-thread transport) nil
          (%http-listen-stream transport) nil
          (%http-listen-stop transport) nil))
  nil)

(defmethod mcp-transport-close ((transport mcp-streamable-http-transport))
  (%mcp-http-stop-stream transport)
  nil)

;;; ------------------------------------------------------------------
;;; OAuth
;;; ------------------------------------------------------------------

(defun %mcp-apply-oauth (transport &optional www-authenticate)
  "Obtain or refresh an access token. True when an Authorization was set.

Every decision here is Core's: MCP-OAUTH-PLAN-ENSURE-TOKEN says whether a
cached token suffices, MCP-OAUTH-DISCOVERY-ENDPOINTS says where to look,
MCP-OAUTH-VALIDATE-RESOURCE-COVERAGE and -VALIDATE-AS-METADATA say whether
what we found is acceptable, MCP-OAUTH-GRANT-BODY builds the exchange and
MCP-OAUTH-PARSE-TOKEN-RESPONSE and -VALIDATE-ISSUER check the answer. This
function fetches, stores and reports."
  (let ((oauth (%mcp-http-option transport "oauth")))
    (unless (hash-table-p oauth) (return-from %mcp-apply-oauth nil))
    (let* ((store (jget oauth "tokenStore"))
           (callback (let ((value (jget oauth "onAuthCode"))) (and (functionp value) value)))
           (scopes (%event-array (jget oauth "scopes")))
           (client-id (let ((value (jget oauth "clientId")))
                        (if (stringp value) value "ax-mcp-client")))
           (client-secret (let ((value (jget oauth "clientSecret")))
                            (if (stringp value) value "")))
           (redirect-uri (let ((value (jget oauth "redirectUri")))
                           (if (stringp value) value "http://localhost:8787/callback")))
           (require-iss (axllm/core::core-true-p (jget oauth "requireIss")))
           (grant-type (let ((value (jget oauth "grantType")))
                         (if (stringp value) value "authorization_code")))
           (ssrf (jget oauth "ssrfProtection"))
           (auth-method (if (plusp (length client-secret)) "client_secret_post" "none"))
           (endpoint (mcp-http-endpoint transport))
           (token (%mcp-token-store-get store endpoint))
           (plan (axllm/core::mcp-oauth-plan-ensure-token
                  (or token :null) (%mcp-now-ms) 'yason:false grant-type
                  (json-boolean callback))))
      (unless (axllm/core::core-true-p (jget plan "ok"))
        (%mcp-fail "~a" (%mcp-text (jget plan "message" "OAuth token planning failed"))))
      (let ((action (%mcp-text (jget plan "action"))))
        (when (string= action "cached")
          (let ((cached (%mcp-object-or-empty (or (jget plan "token") token))))
            (mcp-http-set-authorization
             transport (format nil "Bearer ~a" (%mcp-text (jget cached "accessToken"))))
            (return-from %mcp-apply-oauth t)))
        (multiple-value-bind (as-metadata issuer resource)
            (%mcp-oauth-discover transport oauth www-authenticate ssrf grant-type auth-method)
          (let* ((challenge (axllm/core::mcp-oauth-parse-www-authenticate
                             (or www-authenticate "")))
                 (challenged (%event-array (jget challenge "scopes")))
                 (effective (cond ((plusp (length challenged)) challenged)
                                  ((plusp (length scopes)) scopes)
                                  (t (%event-array (jget as-metadata "scopes_supported")))))
                 (token-endpoint (%mcp-text (jget as-metadata "token_endpoint")))
                 (next nil))
            (flet ((exchange (selected &key (code "") (verifier "") (refresh-token ""))
                     (let ((grant (axllm/core::mcp-oauth-grant-body
                                   selected client-id client-secret auth-method
                                   resource effective code redirect-uri verifier refresh-token)))
                       (unless (axllm/core::core-true-p (jget grant "ok"))
                         (%mcp-fail "~a" (%mcp-text (jget grant "message"
                                                          "OAuth grant planning failed"))))
                       (let* ((response (%mcp-http-form-post token-endpoint (jget grant "body") ssrf))
                              (parsed (axllm/core::mcp-oauth-parse-token-response
                                       response (%mcp-now-ms) refresh-token issuer)))
                         (unless (axllm/core::core-true-p (jget parsed "ok"))
                           (%mcp-fail "~a" (%mcp-text (jget parsed "message"
                                                            "OAuth token response validation failed"))))
                         (jget parsed "token")))))
              (when (string= action "refresh")
                (handler-case
                    (setf next (exchange "refresh_token"
                                         :refresh-token (%mcp-text (jget plan "refreshToken"))))
                  (error ()
                    ;; A refresh token the server no longer honours must be
                    ;; dropped, not retried.
                    (%mcp-token-store-clear store endpoint)
                    (setf action (if (string= grant-type "client_credentials")
                                     "client_credentials"
                                     "authorize")))))
              (cond ((and (null next) (string= action "client_credentials"))
                     (setf next (exchange "client_credentials")))
                    ((and (null next) (string= action "authorize"))
                     (unless callback
                       (%mcp-fail "Authorization required. Provide oauth.onAuthCode to complete the flow"))
                     (let* ((verifier (mcp-pkce-verifier))
                            (challenge-value (mcp-pkce-challenge verifier))
                            (state (mcp-pkce-verifier))
                            (params (axllm/core::mcp-oauth-authorization-request-params
                                     client-id redirect-uri effective resource state
                                     challenge-value))
                            (authorization-endpoint
                              (mcp-validate-endpoint
                               (%mcp-text (jget as-metadata "authorization_endpoint")) ssrf))
                            (auth-url (%mcp-merge-query
                                       authorization-endpoint
                                       (mapcar (lambda (key)
                                                 (cons key (%mcp-text (gethash key params))))
                                               (%object-keys params))))
                            (answer (%mcp-object-or-empty (funcall callback auth-url))))
                       (unless (stringp (jget answer "code"))
                         (return-from %mcp-apply-oauth nil))
                       (%set-key answer "expectedState" state)
                       (let ((validation (axllm/core::mcp-oauth-validate-issuer
                                          answer issuer
                                          (json-boolean (or require-iss
                                                            (axllm/core::core-true-p
                                                             (jget as-metadata "requireIss")))))))
                         (unless (axllm/core::core-true-p (jget validation "ok"))
                           (%mcp-fail "~a" (%mcp-text (jget validation "message"
                                                            "OAuth authorization response validation failed")))))
                       (setf next (exchange "authorization_code"
                                            :code (%mcp-text (jget answer "code"))
                                            :verifier verifier)))))
              (unless next (return-from %mcp-apply-oauth nil))
              (%mcp-token-store-set store endpoint next)
              (mcp-http-set-authorization
               transport (format nil "Bearer ~a" (%mcp-text (jget next "accessToken"))))
              t)))))))

(defun %mcp-oauth-discover (transport oauth www-authenticate ssrf grant-type auth-method)
  "Resolve the authorization server. Returns (values metadata issuer resource)."
  (let ((configured (jget oauth "authorizationServerMetadata"))
        (endpoint (mcp-http-endpoint transport))
        (resource (let ((value (jget oauth "resource"))) (if (stringp value) value "")))
        (issuer ""))
    (when (hash-table-p configured)
      (setf issuer (%mcp-text (jget configured "issuer")))
      (let ((validation (axllm/core::mcp-oauth-validate-as-metadata
                         configured issuer
                         (json-boolean (not (string= grant-type "client_credentials")))
                         auth-method)))
        (unless (axllm/core::core-true-p (jget validation "ok"))
          (%mcp-fail "~a" (%mcp-text (jget validation "message"
                                           "OAuth AS metadata validation failed")))))
      (return-from %mcp-oauth-discover
        (values configured issuer (if (plusp (length resource)) resource endpoint))))
    (let* ((challenge (axllm/core::mcp-oauth-parse-www-authenticate (or www-authenticate "")))
           (discovery (axllm/core::mcp-oauth-discovery-endpoints
                       endpoint "" (%mcp-text (jget challenge "resourceMetadata"))))
           (resource-metadata nil)
           (last-error nil))
      (loop for url across (%event-array (jget discovery "resourceMetadataEndpoints"))
            until resource-metadata
            do (handler-case (setf resource-metadata (%mcp-http-json-get (%mcp-text url) ssrf))
                 (error (condition) (setf last-error condition))))
      (unless resource-metadata
        (%mcp-fail "Failed to resolve protected resource metadata: ~a" last-error))
      (let ((coverage (axllm/core::mcp-oauth-validate-resource-coverage
                       endpoint resource-metadata)))
        (unless (axllm/core::core-true-p (jget coverage "ok"))
          (%mcp-fail "~a" (%mcp-text (jget coverage "message"
                                           "OAuth resource coverage validation failed"))))
        (when (zerop (length resource))
          (setf resource (%mcp-text (jget coverage "resource"))))
        (setf issuer (%mcp-text (jget (%event-array (jget coverage "issuers")) 0))))
      (let ((metadata nil))
        (setf last-error nil)
        (loop for url across (%event-array
                              (jget (axllm/core::mcp-oauth-discovery-endpoints endpoint issuer "")
                                    "authorizationServerMetadataEndpoints"))
              until metadata
              do (handler-case
                     (let* ((candidate (%mcp-http-json-get (%mcp-text url) ssrf))
                            (validation (axllm/core::mcp-oauth-validate-as-metadata
                                         candidate issuer
                                         (json-boolean (not (string= grant-type "client_credentials")))
                                         auth-method)))
                       (unless (axllm/core::core-true-p (jget validation "ok"))
                         (%mcp-fail "~a" (%mcp-text (jget validation "message"
                                                          "OAuth AS metadata validation failed"))))
                       (setf metadata candidate))
                   (error (condition) (setf last-error condition))))
        (unless metadata
          (%mcp-fail "Failed to discover authorization server metadata: ~a" last-error))
        (%set-key oauth "authorizationServerMetadata" metadata)
        (%set-key oauth "resource" (if (plusp (length resource)) resource endpoint))
        (values metadata (if (plusp (length issuer))
                             issuer
                             (%mcp-text (jget metadata "issuer")))
                (if (plusp (length resource)) resource endpoint))))))

(export '(mcp-streamable-http-transport make-mcp-streamable-http-transport
          mcp-http-endpoint mcp-http-headers mcp-http-set-headers
          mcp-http-set-authorization mcp-http-session-id mcp-http-era
          mcp-http-build-headers mcp-http-last-headers mcp-http-terminate-session))
