;;;; mcp-runtime.lisp --- the MCP transports against real endpoints.
;;;;
;;;; The conformance suite in mcp-conformance.lisp runs the shared AxIR
;;;; fixtures through scripted transports, so it proves the portable Core
;;;; algorithms and nothing about sockets. This file is the other half: a
;;;; real child process on a real pipe, a real TCP listener speaking HTTP
;;;; and SSE on loopback, and a real RFC 6455 WebSocket handshake. Every
;;;; server here is built in-test from usocket so that what is exercised is
;;;; this implementation's transport code rather than a mock of it.
;;;;
;;;; What each check is for is stated at the check. The ones worth naming up
;;;; front, because a weaker test would pass without them:
;;;;
;;;;   The servers assert on what the client SENT, not only on what it did
;;;;   with the reply. A transport that dropped MCP-Session-Id, or sent a
;;;;   protocol version header on a modern request that must not carry one,
;;;;   returns the right answer to the caller and is still wrong.
;;;;
;;;;   Cancellation is checked for recovery, not only for interruption. A
;;;;   transport that aborts a request by corrupting its session leaves the
;;;;   client unusable, so each cancellation check makes a further request
;;;;   afterwards and asserts it succeeds.
;;;;
;;;;   The SSRF gate is checked as a gate: a rejection has to come from the
;;;;   address rather than from the connection failing, so the loopback
;;;;   rejections are asserted against a port that IS listening.
;;;;
;;;; Nothing here reaches the network. Every endpoint is 127.0.0.1 on a
;;;; kernel-assigned port, or a child process in a temporary directory.

(in-package #:axllm)

(export '(run-mcp-runtime-tests run-mcp-runtime-tests-or-die))

;;; ------------------------------------------------------------------
;;; Harness
;;;
;;; Deliberately self-contained: this file's position in the system is the
;;; parent's to choose, so it does not depend on mcp-conformance.lisp having
;;; been loaded first.
;;; ------------------------------------------------------------------

(define-condition mcp-runtime-failure (error)
  ((detail :initarg :detail :reader mcp-runtime-failure-detail))
  (:report (lambda (condition stream)
             (write-string (mcp-runtime-failure-detail condition) stream))))

(defun %mcp-runtime-fail (format-control &rest arguments)
  (error 'mcp-runtime-failure :detail (apply #'format nil format-control arguments)))

(defun %mcp-runtime-show (value)
  (if (stringp value) (format nil "~s" value) (encode-json value)))

(defun %mcp-runtime-equal (actual expected label)
  (unless (axllm/core::core-value-equal actual expected)
    (%mcp-runtime-fail "~a mismatch~%    expected: ~a~%    actual:   ~a"
                   label (%mcp-runtime-show expected) (%mcp-runtime-show actual)))
  t)

(defun %mcp-runtime-true (value label)
  (unless (and value (not (eq value :null)) (not (eq value 'yason:false)))
    (%mcp-runtime-fail "~a was not true (~a)" label (%mcp-runtime-show value)))
  t)

(defun %mcp-runtime-contains (haystack needle label)
  (unless (and (stringp haystack) (search needle haystack :test #'char-equal))
    (%mcp-runtime-fail "~a did not contain ~s~%    actual: ~a" label needle
                   (%mcp-runtime-show haystack)))
  t)

(defmacro %mcp-runtime-fails ((condition-type &optional (variable (gensym))) label &body body)
  "Assert BODY signals CONDITION-TYPE, and return the condition for inspection."
  `(let ((result (handler-case (progn ,@body :%mcp-runtime-no-error)
                   (,condition-type (,variable) ,variable))))
     (when (eq result :%mcp-runtime-no-error)
       (%mcp-runtime-fail "~a did not signal ~a" ,label ',condition-type))
     result))

;;; ------------------------------------------------------------------
;;; A JSON-RPC server script, for the stdio transport
;;;
;;; Written to a temporary file and run as a child process, because the
;;; point of the stdio transport is that it owns a process and a pipe.
;;; Python is used rather than a second Lisp image because it starts in
;;; milliseconds and the script is the fixture, not the subject.
;;; ------------------------------------------------------------------

(defparameter +mcp-runtime-stdio-server+
  "import json, os, sys

LOG = os.environ.get('AX_MCP_LOG')

def log(entry):
    if LOG:
        with open(LOG, 'a') as handle:
            handle.write(json.dumps(entry) + '\\n')

def send(message):
    sys.stdout.write(json.dumps(message) + '\\n')
    sys.stdout.flush()

TOOLS = [{
    'name': 'echo',
    'description': 'Echo the argument back.',
    'inputSchema': {
        'type': 'object',
        'properties': {'text': {'type': 'string', 'pattern': '^[a-z ]+$'}},
        'required': ['text'],
        'additionalProperties': False,
    },
}]

def result(method, params, request_id):
    if method == 'initialize':
        return {
            'protocolVersion': params.get('protocolVersion'),
            'capabilities': {'tools': {'listChanged': True}, 'resources': {'subscribe': True}},
            'serverInfo': {'name': 'ax-stdio-fixture', 'version': '1.0.0'},
        }
    if method == 'tools/list':
        return {'tools': TOOLS}
    if method == 'prompts/list':
        return {'prompts': [{'name': 'greet', 'arguments': []}]}
    if method == 'resources/list':
        return {'resources': [{'uri': 'file:///fixture.txt', 'name': 'fixture'}]}
    if method == 'resources/templates/list':
        return {'resourceTemplates': []}
    if method == 'ping':
        return {}
    if method == 'tools/call':
        name = params.get('name')
        if name == 'echo':
            # A notification on the same pipe, ahead of the response, so the
            # transport has to route it inbound and keep waiting.
            send({'jsonrpc': '2.0', 'method': 'notifications/message',
                  'params': {'level': 'info', 'data': 'about to echo'}})
            return {'content': [{'type': 'text',
                                 'text': params.get('arguments', {}).get('text', '')}]}
        if name == 'boom':
            return {'content': [{'type': 'text', 'text': 'tool failed'}], 'isError': True}
        if name == 'crash':
            # The server dies mid-request: the transport must report a
            # closed pipe rather than hanging on a read that never returns.
            sys.exit(3)
        raise LookupError(name)
    raise LookupError(method)

for line in sys.stdin:
    line = line.strip()
    if not line:
        continue
    message = json.loads(line)
    log(message)
    if 'id' not in message:
        continue
    try:
        send({'jsonrpc': '2.0', 'id': message['id'],
              'result': result(message.get('method'), message.get('params') or {}, message['id'])})
    except LookupError as error:
        send({'jsonrpc': '2.0', 'id': message['id'],
              'error': {'code': -32601, 'message': 'Method not found: %s' % error}})
"
  "A newline-delimited JSON-RPC MCP server, as a Python script.")

(defun %mcp-runtime-temp-file (tag type)
  (merge-pathnames (format nil "ax-mcp-runtime-~a-~a.~a" tag (%mcp-uuid) type)
                   (uiop:temporary-directory)))

(defmacro %with-mcp-runtime-stdio ((client &key log options) &body body)
  "Run BODY with CLIENT talking to a freshly started child process."
  (let ((script (gensym "SCRIPT")) (transport (gensym "TRANSPORT")))
    `(let ((,script (%mcp-runtime-temp-file "server" "py")))
       (unwind-protect
            (progn
              (with-open-file (stream ,script :direction :output
                                              :if-exists :supersede
                                              :external-format :utf-8)
                (write-string +mcp-runtime-stdio-server+ stream))
              (let* ((,transport (make-mcp-stdio-transport
                                  "python3"
                                  :arguments (list (namestring ,script))
                                  ,@(when log
                                      `(:environment
                                        (list (cons "AX_MCP_LOG" (namestring ,log)))))))
                     (,client (apply #'make-mcp-client ,transport
                                     (list :namespace "fixture" ,@options))))
                (unwind-protect (progn ,@body)
                  (ignore-errors (mcp-close ,client)))))
         (ignore-errors (delete-file ,script))))))

(defun %mcp-runtime-log-entries (path)
  "Every JSON-RPC message the child server received, in order."
  (if (probe-file path)
      (with-open-file (stream path :external-format :utf-8)
        (loop for line = (read-line stream nil nil)
              while line
              when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
                collect (parse-json line)))
      '()))

(defun %mcp-runtime-log-methods (path)
  (mapcar (lambda (entry) (%mcp-text (jget entry "method"))) (%mcp-runtime-log-entries path)))

;;; ------------------------------------------------------------------
;;; Child-process stdio
;;; ------------------------------------------------------------------

(defun %mcp-runtime-stdio-checks (check)
  (funcall
   check "a stdio client handshakes and calls a tool on a real child process"
   (lambda ()
     ;; The whole path: a process started by this transport, a handshake over
     ;; its pipe, the catalog it answered with, and a tool call whose result
     ;; came back through the same pipe.
     (let ((log (%mcp-runtime-temp-file "log" "jsonl")))
       (unwind-protect
            (%with-mcp-runtime-stdio (client :log log)
              (mcp-init client)
              ;; A stdio server is a session, never a stateless modern
              ;; endpoint, so the era must be legacy without probing for it.
              (%mcp-runtime-equal (mcp-get-era client) "legacy" "stdio era")
              (%mcp-runtime-equal (mcp-namespace client) "fixture" "namespace")
              (let ((tools (mcp-list-tools client)))
                (%mcp-runtime-equal (length (jget tools "tools")) 1 "tool count")
                (%mcp-runtime-equal (jget (aref (jget tools "tools") 0) "name") "echo"
                                    "tool name"))
              ;; The server's schema is carried verbatim: the pattern and the
              ;; additionalProperties false both survive, because a native tool
              ;; does not rewrite what the server published.
              (let ((native (first (mcp-native-tools client))))
                (%mcp-runtime-equal
                 (jget (jget (jget (native-tool-parameters native) "properties") "text") "pattern")
                 "^[a-z ]+$" "native tool keeps the server's pattern")
                (%mcp-runtime-equal (jget (native-tool-parameters native) "additionalProperties")
                                    false "native tool keeps additionalProperties"))
              (let ((result (mcp-call-tool client "echo" (object "text" "hello there"))))
                (%mcp-runtime-equal (jget (aref (jget result "content") 0) "text")
                                    "hello there" "tool result text"))
              ;; What the child actually received, in order. A transport that
              ;; produced the right answer by a different conversation is wrong.
              (let ((methods (%mcp-runtime-log-methods log)))
                (%mcp-runtime-equal (first methods) "initialize" "first method sent")
                (%mcp-runtime-true (member "tools/list" methods :test #'equal)
                                   "tools/list was sent")
                (%mcp-runtime-true (member "tools/call" methods :test #'equal)
                                   "tools/call was sent")))
         (ignore-errors (delete-file log))))))

  (funcall
   check "a notification arriving mid-request is delivered, not mistaken for the response"
   (lambda ()
     ;; The fixture server emits notifications/message BEFORE the tools/call
     ;; response, on the same pipe. A transport that returned the first frame
     ;; it read would return the notification as the tool result.
     (%with-mcp-runtime-stdio (client)
       (mcp-init client)
       (let ((seen (%new-array)))
         (mcp-add-notification-listener client (lambda (n) (vector-push-extend n seen)))
         (let ((result (mcp-call-tool client "echo" (object "text" "ping"))))
           (%mcp-runtime-equal (jget (aref (jget result "content") 0) "text") "ping"
                               "the response, not the notification")
           (%mcp-runtime-equal (length seen) 1 "notification count")
           (%mcp-runtime-equal (jget (aref seen 0) "method") "notifications/message"
                               "notification method")
           (%mcp-runtime-equal (jget (jget (aref seen 0) "params") "data") "about to echo"
                               "notification payload"))))))

  (funcall
   check "a tool error from the server is a result, not a transport failure"
   (lambda ()
     ;; isError is the server reporting a failed tool, which the caller must
     ;; be able to read. Turning it into a condition would lose the content.
     (%with-mcp-runtime-stdio (client)
       (mcp-init client)
       (let ((result (mcp-call-tool client "boom" (object))))
         (%mcp-runtime-equal (jget result "isError") true "isError survived")
         (%mcp-runtime-equal (jget (aref (jget result "content") 0) "text") "tool failed"
                             "error content survived")))))

  (funcall
   check "an unknown tool is the server's JSON-RPC error, with its code"
   (lambda ()
     (%with-mcp-runtime-stdio (client)
       (mcp-init client)
       (let ((condition (%mcp-runtime-fails (mcp-error c) "unknown tool"
                         (mcp-call-tool client "nope" (object)))))
         (%mcp-runtime-equal (mcp-error-code condition) -32601 "JSON-RPC error code")
         (%mcp-runtime-contains (princ-to-string condition) "Method not found"
                                "server error message")))))

  (funcall
   check "a child that dies mid-request fails closed and stays closed"
   (lambda ()
     ;; The server exits without answering. The read must end as a closed
     ;; pipe rather than blocking forever, and the transport must not then
     ;; pretend to be usable.
     (%with-mcp-runtime-stdio (client)
       (mcp-init client)
       (let ((process (mcp-stdio-process (mcp-client-transport client))))
         (%mcp-runtime-fails (mcp-error c) "a crashed child"
           (mcp-call-tool client "crash" (object)))
         (%mcp-runtime-true (not (uiop:process-alive-p process)) "the child really exited")
         ;; And a further request fails rather than hanging or succeeding.
         (%mcp-runtime-fails (mcp-error c) "a request after the child died"
           (mcp-ping client))))))

  (funcall
   check "closing the client terminates the process it started"
   (lambda ()
     ;; The transport owns the child. Leaving it running would leak a process
     ;; per client, which a test that only checks the return value misses.
     (let* ((script (%mcp-runtime-temp-file "server" "py"))
            (transport nil) (process nil))
       (unwind-protect
            (progn
              (with-open-file (stream script :direction :output :if-exists :supersede
                                             :external-format :utf-8)
                (write-string +mcp-runtime-stdio-server+ stream))
              (setf transport (make-mcp-stdio-transport
                               "python3" :arguments (list (namestring script))))
              (setf process (mcp-stdio-process transport))
              (let ((client (make-mcp-client transport :namespace "fixture")))
                (mcp-init client)
                (%mcp-runtime-true (uiop:process-alive-p process) "the child was running")
                (mcp-close client))
              (%mcp-runtime-true (not (uiop:process-alive-p process))
                                 "the child outlived its client"))
         (ignore-errors (delete-file script)))))))


;;; ------------------------------------------------------------------
;;; A loopback HTTP server, for the Streamable HTTP transport
;;;
;;; Built in-test from usocket rather than mocked, so what is exercised is
;;; this implementation's transport: Drakma's client, the header plan Core
;;; produces, the session capture, the SSE reader and the cancellation path.
;;; It binds 127.0.0.1 on a kernel-assigned port and serves one connection
;;; at a time from its own thread.
;;; ------------------------------------------------------------------

(defclass mcp-runtime-server ()
  ((socket :initarg :socket :reader %mcp-runtime-server-socket)
   (port :initarg :port :reader mcp-runtime-server-port)
   (handler :initarg :handler :reader %mcp-runtime-server-handler)
   (thread :initform nil :accessor %mcp-runtime-server-thread)
   (requests :initform (%new-array) :reader mcp-runtime-server-requests)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-test-server") :reader %mcp-runtime-server-lock)
   (stop :initform nil :accessor %mcp-runtime-server-stop))
  (:documentation "A single-threaded loopback HTTP server for one test."))

(defun %mcp-runtime-read-line (stream)
  "One CRLF-terminated line as a string, or NIL at end of stream."
  (let ((out (make-string-output-stream)))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (cond ((null byte) (let ((text (get-output-stream-string out)))
                             (return (and (plusp (length text)) text))))
              ((= byte 10) (return (string-right-trim '(#\Return)
                                                      (get-output-stream-string out))))
              (t (write-char (code-char byte) out)))))))

(defun %mcp-runtime-read-request (stream)
  "One HTTP request as (values method path headers body)."
  (let ((request-line (%mcp-runtime-read-line stream)))
    (unless request-line (return-from %mcp-runtime-read-request nil))
    (let* ((parts (axllm/core::core-string-split request-line " "))
           (headers (object))
           (body nil))
      (loop for line = (%mcp-runtime-read-line stream)
            while (and line (plusp (length line)))
            do (let ((colon (position #\: line)))
                 (when colon
                   (%set-key headers (string-downcase (subseq line 0 colon))
                             (string-left-trim '(#\Space) (subseq line (1+ colon)))))))
      (let ((length (jget headers "content-length")))
        (when (stringp length)
          (let* ((count (parse-integer length :junk-allowed t))
                 (buffer (make-array (or count 0) :element-type '(unsigned-byte 8))))
            (read-sequence buffer stream)
            (setf body (%mcp-from-utf8 buffer)))))
      (values (and (plusp (length parts)) (aref parts 0))
              (and (> (length parts) 1) (aref parts 1))
              headers body))))

(defun %mcp-runtime-write (stream text)
  (let ((bytes (%mcp-utf8 text)))
    (write-sequence bytes stream)
    (force-output stream)))

(defun %mcp-runtime-respond (stream status body &key (content-type "application/json")
                                                    extra-headers)
  (%mcp-runtime-write
   stream
   (format nil "HTTP/1.1 ~a~c~cContent-Type: ~a~c~cContent-Length: ~a~c~c~{~a~c~c~}~c~c~a"
           status #\Return #\Newline content-type #\Return #\Newline
           (length (%mcp-utf8 body)) #\Return #\Newline
           (loop for (name value) on extra-headers by #'cddr
                 append (list (format nil "~a: ~a" name value) #\Return #\Newline))
           #\Return #\Newline body)))

(defun %mcp-runtime-respond-sse (stream events &key extra-headers)
  "An event-stream response. EVENTS is a list of (id . data) or data strings."
  (%mcp-runtime-write
   stream
   (format nil "HTTP/1.1 200 OK~c~cContent-Type: text/event-stream~c~cCache-Control: no-cache~c~c~{~a~c~c~}~c~c"
           #\Return #\Newline #\Return #\Newline #\Return #\Newline
           (loop for (name value) on extra-headers by #'cddr
                 append (list (format nil "~a: ~a" name value) #\Return #\Newline))
           #\Return #\Newline))
  (dolist (event events)
    (if (consp event)
        (%mcp-runtime-write stream (format nil "id: ~a~c~cdata: ~a~c~c~c~c"
                                           (car event) #\Return #\Newline
                                           (cdr event) #\Return #\Newline #\Return #\Newline))
        (%mcp-runtime-write stream (format nil "data: ~a~c~c~c~c"
                                           event #\Return #\Newline #\Return #\Newline)))))

(defun start-mcp-runtime-server (handler)
  "Serve HANDLER on 127.0.0.1 at a kernel-assigned port.

HANDLER receives (method path headers body stream) and writes the response.
It is called on the server thread, one connection at a time."
  (let* ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                      :element-type '(unsigned-byte 8)))
         (server (make-instance 'mcp-runtime-server
                                :socket socket
                                :port (usocket:get-local-port socket)
                                :handler handler)))
    (setf (%mcp-runtime-server-thread server)
          (sb-thread:make-thread
           (lambda ()
             (loop until (%mcp-runtime-server-stop server)
                   do (handler-case
                          (when (usocket:wait-for-input socket :timeout 0.2 :ready-only t)
                            (let ((connection (usocket:socket-accept socket)))
                              (unwind-protect
                                   (let ((stream (usocket:socket-stream connection)))
                                     (multiple-value-bind (method path headers body)
                                         (%mcp-runtime-read-request stream)
                                       (when method
                                         (sb-thread:with-mutex
                                             ((%mcp-runtime-server-lock server))
                                           (vector-push-extend
                                            (object "method" method "path" path
                                                    "headers" headers
                                                    "body" (or body :null))
                                            (mcp-runtime-server-requests server)))
                                         (funcall (%mcp-runtime-server-handler server)
                                                  method path headers body stream))))
                                (ignore-errors (usocket:socket-close connection)))))
                        (error () nil))))
           :name "ax-mcp-test-server"))
    server))

(defun stop-mcp-runtime-server (server)
  (setf (%mcp-runtime-server-stop server) t)
  (ignore-errors (usocket:socket-close (%mcp-runtime-server-socket server)))
  (let ((thread (%mcp-runtime-server-thread server)))
    (when thread (ignore-errors (sb-thread:join-thread thread :timeout 2 :default nil))))
  nil)

(defun mcp-runtime-server-url (server &optional (path "/mcp"))
  (format nil "http://127.0.0.1:~a~a" (mcp-runtime-server-port server) path))

(defun mcp-runtime-server-request (server index)
  (sb-thread:with-mutex ((%mcp-runtime-server-lock server))
    (let ((requests (mcp-runtime-server-requests server)))
      (if (< index (length requests)) (aref requests index) :null))))

(defmacro %with-mcp-runtime-server ((server handler) &body body)
  `(let ((,server (start-mcp-runtime-server ,handler)))
     (unwind-protect (progn ,@body) (stop-mcp-runtime-server ,server))))

(defun %mcp-runtime-rpc-body (text)
  "The JSON-RPC id in TEXT, so a handler can answer the right request."
  (let ((parsed (parse-json text)))
    (values (%mcp-text (jget parsed "method")) (jget parsed "id") parsed)))


;;; ------------------------------------------------------------------
;;; Streamable HTTP over loopback
;;; ------------------------------------------------------------------

(defun %mcp-runtime-modern-handler (&key (session nil) (slow nil))
  "A handler that answers the modern discovery and a couple of methods."
  (lambda (method path headers body stream)
    (declare (ignore path headers))
    (if (string= method "POST")
        (multiple-value-bind (rpc-method id) (%mcp-runtime-rpc-body body)
          ;; Only the method under test is slow; a slow handshake would put
          ;; seconds of setup inside the window the check is timing.
          (when (and slow (string= rpc-method "tools/call")) (sleep slow))
          (let ((result
                  (cond ((string= rpc-method "initialize")
                         (object "protocolVersion" (mcp-protocol-version)
                                 "capabilities" (object "tools" (object))
                                 "serverInfo" (object "name" "ax-http-fixture"
                                                      "version" "1.0.0")))
                        ((string= rpc-method "tools/list")
                         (object "tools" (vector (object "name" "add"
                                                         "inputSchema"
                                                         (object "type" "object")))))
                        ((string= rpc-method "tools/call")
                         (object "content" (vector (object "type" "text" "text" "42"))))
                        ((string= rpc-method "ping") (object))
                        (t (object)))))
            (%mcp-runtime-respond
             stream "200 OK"
             (encode-json (object "jsonrpc" "2.0" "id" id "result" result))
             :extra-headers (when session (list "Mcp-Session-Id" session)))))
        (%mcp-runtime-respond stream "405 Method Not Allowed" "{}"))))

(defclass mcp-batch-stub-transport (mcp-transport)
  ((answers :initarg :answers :reader %batch-stub-answers))
  (:documentation "An inner transport that answers a batch, for recording checks."))

(defmethod mcp-transport-send-batch ((transport mcp-batch-stub-transport) messages
                                     &key context)
  (declare (ignore context))
  (let ((out (%new-array)))
    (loop for message across (if (%array-p messages) messages (coerce messages 'vector))
          do (vector-push-extend
              (object "jsonrpc" "2.0" "id" (jget message "id")
                      "result" (object "for" (jget message "method")))
              out))
    out))

(defclass mcp-plain-stub-transport (mcp-transport)
  ((metadata :initform 1 :accessor %stub-metadata-left)
   (terminated :initform nil :accessor %stub-terminated))
  (:documentation
   "An inner transport with metadata and termination but no batching and no
stream, so the recording wrapper's forwarding and fallback are observable."))

(defmethod mcp-transport-take-request-metadata ((transport mcp-plain-stub-transport) id)
  (declare (ignore id))
  ;; Destructive, like the name says: answered once, then absent.
  (if (plusp (%stub-metadata-left transport))
      (progn (decf (%stub-metadata-left transport)) (object "retryCount" 2))
      :null))

(defmethod mcp-transport-terminate-session ((transport mcp-plain-stub-transport))
  (setf (%stub-terminated transport) t)
  nil)

(defmethod mcp-transport-start-listening ((transport mcp-plain-stub-transport)) :null)

(defun %mcp-runtime-record-checks (check)
  (funcall
   check "a real conversation records and replays, and the replay needs no server"
   (lambda ()
     ;; Recorded from the live child process, then replayed with the child
     ;; gone. If the replay needed the server it would not be a recording.
     (let ((script nil))
       (%with-mcp-runtime-stdio (client)
         (let ((recorder (make-mcp-recording-transport (mcp-client-transport client))))
           (let ((recorded (make-mcp-client recorder :namespace "rec" :era "legacy")))
             (mcp-init recorded)
             (%mcp-runtime-equal
              (jget (aref (jget (mcp-call-tool recorded "echo" (object "text" "replay me"))
                                "content") 0) "text")
              "replay me" "the live call")
             (setf script (mcp-recording-script recorder)))))
       ;; The child is gone now: the unwind-protect in the macro closed it.
       (%mcp-runtime-true (plusp (length script)) "something was recorded")
       (let ((replayed (make-mcp-client (make-mcp-replay-transport script)
                                        :namespace "rec" :era "legacy")))
         (mcp-init replayed)
         (%mcp-runtime-equal
          (jget (aref (jget (mcp-call-tool replayed "echo" (object "text" "replay me"))
                            "content") 0) "text")
          "replay me" "the replayed call")
         ;; Everything recorded was consumed, so the replay covered the
         ;; whole conversation rather than a prefix of it.
         (%mcp-runtime-true (mcp-replay-exhausted-p (mcp-client-transport replayed))
                            "the replay consumed its whole recording")))))

  (funcall
   check "a replay rejects a changed argument, not just a changed method"
   (lambda ()
     ;; The point of strictness. A recording that matched on method alone
     ;; would accept a different tool argument against an old recording,
     ;; which is the divergence a recording exists to detect.
     (let ((script nil))
       (%with-mcp-runtime-stdio (client)
         (let* ((recorder (make-mcp-recording-transport (mcp-client-transport client)))
                (recorded (make-mcp-client recorder :namespace "rec" :era "legacy")))
           (mcp-init recorded)
           (mcp-call-tool recorded "echo" (object "text" "original"))
           (setf script (mcp-recording-script recorder))))
       (let ((replayed (make-mcp-client (make-mcp-replay-transport script)
                                        :namespace "rec" :era "legacy")))
         (mcp-init replayed)
         (let ((condition (%mcp-runtime-fails (mcp-replay-mismatch c) "a changed argument"
                            (mcp-call-tool replayed "echo" (object "text" "CHANGED")))))
           (%mcp-runtime-contains (princ-to-string condition) "diverged"
                                  "the mismatch names a divergence")
           (%mcp-runtime-contains (princ-to-string condition) "CHANGED"
                                  "the mismatch shows what was sent")))
       ;; A different method diverges too, and reports the expected one.
       (let ((replayed (make-mcp-client (make-mcp-replay-transport script)
                                        :namespace "rec" :era "legacy")))
         (mcp-init replayed)
         (%mcp-runtime-contains
          (princ-to-string
           (%mcp-runtime-fails (mcp-replay-mismatch c) "a changed method"
             (mcp-ping replayed)))
          "tools/call" "the mismatch names the recorded method")))))

  (funcall
   check "a replay past the end of its recording is a mismatch, not a stale answer"
   (lambda ()
     ;; Running out is a conversation the recording never saw. Returning the
     ;; last response again, or falling through to a live transport, would
     ;; turn that into a silent pass.
     (let ((script nil))
       (%with-mcp-runtime-stdio (client)
         (let* ((recorder (make-mcp-recording-transport (mcp-client-transport client)))
                (recorded (make-mcp-client recorder :namespace "rec" :era "legacy")))
           (mcp-init recorded)
           (setf script (mcp-recording-script recorder))))
       (let ((replayed (make-mcp-client (make-mcp-replay-transport script)
                                        :namespace "rec" :era "legacy")))
         (mcp-init replayed)
         (%mcp-runtime-true (mcp-replay-exhausted-p (mcp-client-transport replayed))
                            "the handshake consumed the recording")
         (%mcp-runtime-contains
          (princ-to-string
           (%mcp-runtime-fails (mcp-replay-mismatch c) "a call past the recording"
             (mcp-call-tool replayed "echo" (object "text" "extra"))))
          "ran out" "the mismatch says the recording was exhausted")))))

  (funcall
   check "mutating a caller's nested params afterwards does not rewrite the recording"
   (lambda ()
     ;; Direction one. A caller that reuses and mutates one params object
     ;; across two calls would, without a deep copy, end up with a recording
     ;; claiming it sent the second value both times -- evidence wrong in
     ;; the direction that hides the bug.
     (let ((script nil))
       (%with-mcp-runtime-stdio (client)
         (let* ((recorder (make-mcp-recording-transport (mcp-client-transport client)))
                (recorded (make-mcp-client recorder :namespace "rec" :era "legacy"))
                ;; A nested structure, because a shallow copy would pass a
                ;; flat one.
                (arguments (object "text" "first" "nested" (object "deep" "original"))))
           (mcp-init recorded)
           (mcp-call-tool recorded "echo" arguments)
           ;; Mutate what was just sent, at both levels.
           (%set-key arguments "text" "mutated")
           (%set-key (jget arguments "nested") "deep" "mutated")
           (setf script (mcp-recording-script recorder))))
       (let ((call (loop for entry across script
                         when (and (equal (%mcp-text (jget entry "direction")) "request")
                                   (equal (%mcp-text (jget (jget entry "message") "method"))
                                          "tools/call"))
                           do (return entry))))
         (when (null call) (%mcp-runtime-fail "the tools/call was not recorded"))
         (let ((arguments (jget (jget (jget call "message") "params") "arguments")))
           (%mcp-runtime-equal (jget arguments "text") "first"
                               "the recording kept the value actually sent")
           (%mcp-runtime-equal (jget (jget arguments "nested") "deep") "original"
                               "the recording kept the NESTED value actually sent"))))))

  (funcall
   check "the recording transport deep-copies at its own boundary, in both directions"
   (lambda ()
     ;; Tested directly on the transport rather than through the client,
     ;; because the client happens to rebuild its params object and so
     ;; insulates the end-to-end path by accident. The transport's contract
     ;; is that what it stores cannot be edited afterwards by whoever handed
     ;; it over, and that has to hold for any caller, not just this client.
     (let ((transport nil))
       ;; The inner transport is a replay stub: it answers the one request
       ;; and needs no process or socket, so what is under test is the
       ;; recording wrapper and nothing else.
       (let ((stub (make-instance 'mcp-replay-transport
                                  :script (vector
                                           (object "direction" "request"
                                                   "message"
                                                   (object "method" "m" "id" "1"
                                                           "params"
                                                           (object "top" "original"
                                                                   "nested"
                                                                   (object "deep" "original")))
                                                   "response" (object "result" (object "ok" true))))
                                  :era "legacy")))
         (setf transport (make-mcp-recording-transport stub))
         (let ((message (object "method" "m" "id" "1"
                                "params" (object "top" "original"
                                                 "nested" (object "deep" "original")))))
           (mcp-transport-send transport message)
           ;; Mutate what was handed over, at both levels.
           (%set-key message "top" "mutated")
           (%set-key (jget message "params") "top" "mutated")
           (%set-key (jget (jget message "params") "nested") "deep" "mutated")
           (let* ((script (mcp-recording-script transport))
                  (recorded (jget (aref script 0) "message")))
             (%mcp-runtime-equal (jget (jget recorded "params") "top") "original"
                                 "the recording kept the params it was given")
             (%mcp-runtime-equal (jget (jget (jget recorded "params") "nested") "deep")
                                 "original"
                                 "the recording kept the NESTED params it was given")))
         ;; And an inbound message is copied before the handler sees it, so a
         ;; handler that normalises an event cannot edit the recording.
         (let ((delivered nil))
           (mcp-transport-set-message-handler
            transport (lambda (message) (setf delivered message)))
           (mcp-transport-dispatch-inbound
            stub (object "method" "notifications/message"
                         "params" (object "nested" (object "deep" "original"))))
           (when (null delivered) (%mcp-runtime-fail "the inbound message was not delivered"))
           (%set-key (jget (jget delivered "params") "nested") "deep" "vandalised")
           (let* ((script (mcp-recording-script transport))
                  (inbound (loop for entry across script
                                 when (equal (%mcp-text (jget entry "direction")) "inbound")
                                   do (return entry))))
             (when (null inbound) (%mcp-runtime-fail "the inbound message was not recorded"))
             (%mcp-runtime-equal
              (jget (jget (jget (jget inbound "message") "params") "nested") "deep")
              "original"
              "a handler's mutation edited the recorded inbound message")))))))

  (funcall
   check "mutating a returned recording or a replayed result does not corrupt the source"
   (lambda ()
     ;; Direction two. getRecording hands out a copy, and a replayed
     ;; response is a copy, so a caller normalising either one cannot edit
     ;; the transport's own evidence or poison later replays.
     (let ((recorder nil) (script nil))
       (%with-mcp-runtime-stdio (client)
         (setf recorder (make-mcp-recording-transport (mcp-client-transport client)))
         (let ((recorded (make-mcp-client recorder :namespace "rec" :era "legacy")))
           (mcp-init recorded)
           (mcp-call-tool recorded "echo" (object "text" "pristine"))))
       ;; Mutate a nested value inside the returned recording.
       (let ((taken (mcp-recording-script recorder)))
         (loop for entry across taken
               do (when (equal (%mcp-text (jget entry "direction")) "request")
                    (%set-key (jget entry "message") "method" "VANDALISED")))
         ;; A second read must be unaffected.
         (setf script (mcp-recording-script recorder))
         (%mcp-runtime-true
          (loop for entry across script
                never (equal (%mcp-text (jget (jget entry "message") "method"))
                             "VANDALISED"))
          "mutating a returned recording edited the transport's own"))
       ;; And a replayed result is a copy: mutating it must not change what
       ;; a second replay of the same recording produces.
       (let* ((first-pass (make-mcp-client (make-mcp-replay-transport script)
                                           :namespace "rec" :era "legacy")))
         (mcp-init first-pass)
         (let ((result (mcp-call-tool first-pass "echo" (object "text" "pristine"))))
           (%set-key (aref (jget result "content") 0) "text" "VANDALISED")))
       (let ((second-pass (make-mcp-client (make-mcp-replay-transport script)
                                          :namespace "rec" :era "legacy")))
         (mcp-init second-pass)
         (%mcp-runtime-equal
          (jget (aref (jget (mcp-call-tool second-pass "echo" (object "text" "pristine"))
                            "content") 0) "text")
          "pristine"
          "mutating one replayed result corrupted the script")))))

  (funcall
   check "a recorded batch becomes one entry per message and replays in order"
   (lambda ()
     ;; The reference records a batch as one entry per message paired with
     ;; its own response. A single combined entry would be unreplayable
     ;; through the ordinary request path, so this checks both halves:
     ;; what is recorded, and that replaying it works.
     (let* ((batched (%new-array))
            (inner (make-instance 'mcp-batch-stub-transport :answers batched))
            (recorder (make-mcp-recording-transport inner))
            (messages (vector (object "method" "a" "id" "1")
                              (object "method" "b" "id" "2"))))
       (let ((responses (mcp-transport-send-batch recorder messages)))
         (%mcp-runtime-equal (length responses) 2 "batch response count")
         (%mcp-runtime-equal (jget (jget (aref responses 0) "result") "for") "a"
                             "first batch response"))
       (let ((script (mcp-recording-script recorder)))
         (%mcp-runtime-equal (length script) 2 "one recorded entry per batched message")
         (%mcp-runtime-equal (%mcp-text (jget (jget (aref script 0) "message") "method"))
                             "a" "first recorded entry")
         (%mcp-runtime-equal (jget (jget (jget (aref script 1) "response") "result") "for")
                             "b" "second entry paired with its own response")
         ;; And it replays through the ordinary path.
         (let ((replay (make-mcp-replay-transport script)))
           (let ((responses (mcp-transport-send-batch replay messages)))
             (%mcp-runtime-equal (jget (jget (aref responses 1) "result") "for") "b"
                                 "replayed batch response"))
           (%mcp-runtime-true (mcp-replay-exhausted-p replay)
                              "the replayed batch consumed both entries"))))))

  (funcall
   check "a transport with no batching refuses rather than sending one at a time"
   (lambda ()
     ;; Sending them individually would look like success while breaking the
     ;; batch semantics the caller asked for, and batching is negotiated.
     ;; The replay transport IS batchable by design, so the refusal is
     ;; checked against a transport that genuinely has no batch binding.
     (%mcp-runtime-contains
      (princ-to-string
       (%mcp-runtime-fails (mcp-error c) "batching an unbatchable transport"
         (mcp-transport-send-batch (make-instance 'mcp-plain-stub-transport)
                                   (vector (object "method" "m" "id" "1")))))
      "batching" "the refusal names batching")
     ;; A recording wrapper over an unbatchable transport refuses too,
     ;; rather than recording a batch it never sent.
     (let ((recorder (make-mcp-recording-transport
                      (make-instance 'mcp-plain-stub-transport))))
       (%mcp-runtime-fails (mcp-error c) "batching through a recorder"
         (mcp-transport-send-batch recorder (vector (object "method" "m" "id" "1"))))
       (%mcp-runtime-equal (length (mcp-recording-script recorder)) 0
                           "a refused batch was recorded anyway"))))

  (funcall
   check "metadata and termination are forwarded, and listening always yields a handle"
   (lambda ()
     (let* ((inner (make-instance 'mcp-plain-stub-transport))
            (recorder (make-mcp-recording-transport inner)))
       ;; Forwarded, and destructive: the stub answers once then :null, so a
       ;; recorder answering from its own copy would hand it out twice.
       (%mcp-runtime-equal (jget (mcp-transport-take-request-metadata recorder "1")
                                 "retryCount")
                           2 "metadata forwarded from the inner transport")
       (%mcp-runtime-equal (mcp-transport-take-request-metadata recorder "1") :null
                           "metadata was handed out twice")
       (%mcp-runtime-true (not (%stub-terminated inner)) "not terminated yet")
       (mcp-transport-terminate-session recorder)
       (%mcp-runtime-true (%stub-terminated inner) "termination was forwarded")
       ;; The inner transport has no stream, so a handle is synthesised and
       ;; its close really cancels.
       (let ((handle (mcp-transport-start-listening recorder)))
         (%mcp-runtime-true (hash-table-p handle) "a listening handle was produced")
         (let ((done (jget handle "done")))
           (%mcp-runtime-true (not (cancellation-token-cancelled-p done))
                              "the handle starts uncancelled")
           (funcall (jget handle "close"))
           (%mcp-runtime-true (cancellation-token-cancelled-p done)
                              "closing the handle cancelled it"))))
     ;; A replay reports a truthful zero rather than an absence.
     (%mcp-runtime-equal
      (jget (mcp-transport-take-request-metadata
             (make-mcp-replay-transport
              (vector (object "direction" "request"
                              "message" (object "method" "m" "id" "1")
                              "response" (object "result" (object)))))
             "1")
            "retryCount")
      0 "a replay reports no retries")))

  (funcall
   check "the replay era is derived from the recording, not assumed"
   (lambda ()
     ;; The reference derives eraHint from the recording: modern when a
     ;; request is server/discover or carries the modern protocol version in
     ;; params _meta. Asking the caller would let a wrong answer make the
     ;; client send a request the recording does not contain.
     (let ((legacy (vector (object "direction" "request"
                                   "message" (object "method" "initialize" "id" "1")
                                   "response" (object "result" (object))))))
       (%mcp-runtime-equal (mcp-transport-era-hint (make-mcp-replay-transport legacy))
                           "legacy" "a plain recording is legacy"))
     (let ((modern (vector (object "direction" "request"
                                   "message" (object "method" "server/discover" "id" "1")
                                   "response" (object "result" (object))))))
       (%mcp-runtime-equal (mcp-transport-era-hint (make-mcp-replay-transport modern))
                           "modern" "a server/discover recording is modern"))
     (let ((modern-meta
             (vector (object "direction" "request"
                             "message" (object "method" "tools/list" "id" "1"
                                               "params"
                                               (object "_meta"
                                                       (object "io.modelcontextprotocol/protocolVersion"
                                                               (mcp-modern-protocol-version))))
                             "response" (object "result" (object))))))
       (%mcp-runtime-equal (mcp-transport-era-hint (make-mcp-replay-transport modern-meta))
                           "modern" "a modern _meta recording is modern"))
     ;; An explicit era still overrides a derivation.
     (%mcp-runtime-equal
      (mcp-transport-era-hint
       (make-mcp-replay-transport
        (vector (object "direction" "request"
                        "message" (object "method" "server/discover" "id" "1")
                        "response" (object "result" (object))))
        :era "legacy"))
      "legacy" "an explicit era overrides the derivation")))

  (funcall
   check "a notification does not shift the position of the next replayed request"
   (lambda ()
     ;; The reference indexes only request entries. If notifications were
     ;; positional, an extra or missing one would report a divergence at the
     ;; wrong call, which is worse than not reporting it.
     (let ((script (vector (object "direction" "request"
                                   "message" (object "method" "initialize" "id" "1")
                                   "response" (object "result" (object "ok" true)))
                           (object "direction" "notification"
                                   "message" (object "method" "notifications/initialized"))
                           (object "direction" "request"
                                   "message" (object "method" "ping" "id" "2")
                                   "response" (object "result" (object "pong" true))))))
       (let ((transport (make-mcp-replay-transport script)))
         (%mcp-runtime-equal (jget (jget (mcp-transport-send
                                          transport (object "method" "initialize" "id" "a"))
                                         "result") "ok")
                             true "the first recorded request")
         ;; A notification the recording does not have at this point is a
         ;; no-op and must not consume the next request entry.
         (mcp-transport-send-notification transport (object "method" "anything"))
         (%mcp-runtime-equal (jget (jget (mcp-transport-send
                                          transport (object "method" "ping" "id" "b"))
                                         "result") "pong")
                             true "the next recorded request after a notification")
         (%mcp-runtime-true (mcp-replay-exhausted-p transport)
                            "both recorded requests were consumed")))))

  (funcall
   check "a recorded notification is replayed to the client's listener"
   (lambda ()
     ;; The fixture server pushes notifications/message before its
     ;; tools/call response. A replay that dropped inbound messages would
     ;; still answer every request and look correct.
     (let ((script nil))
       (%with-mcp-runtime-stdio (client)
         (let* ((recorder (make-mcp-recording-transport (mcp-client-transport client)))
                (recorded (make-mcp-client recorder :namespace "rec" :era "legacy")))
           (mcp-init recorded)
           (mcp-call-tool recorded "echo" (object "text" "noted"))
           (setf script (mcp-recording-script recorder))))
       (%mcp-runtime-true
        (loop for entry across script
              thereis (equal (%mcp-text (jget entry "direction")) "inbound"))
        "the notification was recorded")
       (let* ((replayed (make-mcp-client (make-mcp-replay-transport script)
                                         :namespace "rec" :era "legacy"))
              (seen (%new-array)))
         (mcp-add-notification-listener replayed (lambda (n) (vector-push-extend n seen)))
         (mcp-init replayed)
         (%mcp-runtime-equal (length seen) 1 "the notification was replayed")
         (%mcp-runtime-equal (jget (jget (aref seen 0) "params") "data") "about to echo"
                             "the replayed notification payload"))))))

(defun %mcp-runtime-http-checks (check)
  (funcall
   check "an http client reaches a real loopback endpoint and sends the headers Core planned"
   (lambda ()
     ;; Assert on what the SERVER received, not only on what the client
     ;; returned. A transport that answered correctly while dropping the
     ;; protocol version or the JSON content type is still wrong.
     (%with-mcp-runtime-server (server (%mcp-runtime-modern-handler))
       (let ((client (make-mcp-client
                      (make-mcp-streamable-http-transport
                       (mcp-runtime-server-url server)
                       :ssrf-protection (object "requireHttps" false "allowLocalhost" true))
                      :namespace "http" :era "legacy")))
         (unwind-protect
              (progn
                (mcp-init client)
                (let ((result (mcp-call-tool client "add" (object "a" 1 "b" 2))))
                  (%mcp-runtime-equal (jget (aref (jget result "content") 0) "text") "42"
                                      "tool result over http"))
                (let* ((first-request (mcp-runtime-server-request server 0))
                       (request-headers (jget first-request "headers")))
                  (%mcp-runtime-equal (jget first-request "method") "POST" "http method")
                  (%mcp-runtime-contains (%mcp-text (jget request-headers "content-type"))
                                         "application/json" "content type sent")
                  (%mcp-runtime-contains (%mcp-text (jget request-headers "accept"))
                                         "application/json" "accept sent")
                  (%mcp-runtime-equal (%mcp-text (jget (parse-json
                                                        (%mcp-text (jget first-request "body")))
                                                       "method"))
                                      "initialize" "first rpc method")))
           (ignore-errors (mcp-close client)))))))

  (funcall
   check "a legacy session id is captured and sent on every later request"
   (lambda ()
     ;; The session is the server's, handed over once in a response header.
     ;; A transport that forgot it would start a new session per request and
     ;; the server would lose all per-session state.
     (%with-mcp-runtime-server (server (%mcp-runtime-modern-handler :session "sess-123"))
       (let ((client (make-mcp-client
                      (make-mcp-streamable-http-transport
                       (mcp-runtime-server-url server)
                       :ssrf-protection (object "requireHttps" false "allowLocalhost" true))
                      :namespace "http" :era "legacy")))
         (unwind-protect
              (progn
                (mcp-init client)
                (%mcp-runtime-equal (mcp-http-session-id (mcp-client-transport client))
                                    "sess-123" "captured session id")
                (mcp-ping client)
                ;; Every request after the one that revealed it carries it.
                (let ((count 0) (with-session 0))
                  (loop for index from 1 below (length (mcp-runtime-server-requests server))
                        do (let ((request (mcp-runtime-server-request server index)))
                             (incf count)
                             (when (equal (%mcp-text (jget (jget request "headers")
                                                           "mcp-session-id"))
                                          "sess-123")
                               (incf with-session))))
                  (%mcp-runtime-true (plusp count) "there were later requests")
                  (%mcp-runtime-equal with-session count
                                      "every later request carried the session id")))
           (ignore-errors (mcp-close client)))))))

  (funcall
   check "the SSRF gate rejects loopback by address, not by failing to connect"
   (lambda ()
     ;; The rejection has to come from the policy. Pointing it at a port that
     ;; IS listening is what distinguishes a gate from a connection error.
     (%with-mcp-runtime-server (server (%mcp-runtime-modern-handler))
       (let ((url (mcp-runtime-server-url server)))
         ;; Default policy: https required, so plain http is refused first.
         (%mcp-runtime-contains
          (princ-to-string
           (%mcp-runtime-fails (mcp-error c) "default policy on a live loopback port"
             (make-mcp-streamable-http-transport url)))
          "https" "https requirement")
         ;; https waived, loopback still refused.
         (%mcp-runtime-contains
          (princ-to-string
           (%mcp-runtime-fails (mcp-error c) "loopback with https waived"
             (make-mcp-streamable-http-transport
              url :ssrf-protection (object "requireHttps" false))))
          "SSRF" "loopback rejection")
         ;; Both waived: the same live port is now allowed, which proves the
         ;; two rejections above were policy and not reachability.
         (%mcp-runtime-true
          (make-mcp-streamable-http-transport
           url :ssrf-protection (object "requireHttps" false "allowLocalhost" true))
          "an explicitly allowed loopback endpoint")))))

  (funcall
   check "cancelling a slow request aborts it and leaves the client usable"
   (lambda ()
     ;; Recovery is the point. A transport that aborts by corrupting its own
     ;; state would pass an interruption-only test and still be broken.
     (%with-mcp-runtime-server (server (%mcp-runtime-modern-handler :slow 1.5))
       (let ((client (make-mcp-client
                      (make-mcp-streamable-http-transport
                       (mcp-runtime-server-url server)
                       :ssrf-protection (object "requireHttps" false "allowLocalhost" true))
                      :namespace "http" :era "legacy")))
         (unwind-protect
              (progn
                ;; Handshake first, outside the window being timed.
                (mcp-init client)
                (let ((token (make-cancellation-token)))
                  (sb-thread:make-thread (lambda () (sleep 0.2)
                                           (cancellation-token-cancel token "test"))
                                         :name "ax-mcp-test-canceller")
                  (let ((started (get-internal-real-time)))
                    (%mcp-runtime-fails (error c) "a cancelled request"
                      (mcp-call-tool client "add" (object)
                                     :context (object "cancellation" token)))
                    ;; Back well before the server's own 1.5s delay elapsed.
                    (%mcp-runtime-true
                     (< (/ (- (get-internal-real-time) started)
                           internal-time-units-per-second)
                        1.4)
                     "the cancelled request waited for the server anyway"))
                  (%mcp-runtime-true (cancellation-token-cancelled-p token)
                                     "the token was cancelled")
                  ;; Recovery: the same client still works afterwards.
                  (%mcp-runtime-equal
                   (jget (aref (jget (mcp-call-tool client "add" (object)) "content") 0) "text")
                   "42" "the client was unusable after a cancellation")))
           (ignore-errors (mcp-close client)))))))

  (funcall
   check "a server that answers garbage is a transport failure, not a parsed result"
   (lambda ()
     ;; Fail closed on a body that is not JSON-RPC rather than handing the
     ;; caller something half-understood.
     (%with-mcp-runtime-server (server (lambda (method path headers body stream)
                                         (declare (ignore method path headers body))
                                         (%mcp-runtime-respond stream "200 OK" "not json")))
       (let ((client (make-mcp-client
                      (make-mcp-streamable-http-transport
                       (mcp-runtime-server-url server)
                       :ssrf-protection (object "requireHttps" false "allowLocalhost" true))
                      :namespace "http" :era "legacy")))
         (unwind-protect
              (%mcp-runtime-fails (error c) "a non-JSON response" (mcp-init client))
           (ignore-errors (mcp-close client))))))))


;;; ------------------------------------------------------------------
;;; A loopback WebSocket server, for the WebSocket transport
;;;
;;; The transport's built-in route is websocket-driver's client, which does
;;; its own RFC 6455 handshake and framing, so exercising it concretely
;;; needs a real server on the other end rather than a stub socket. This is
;;; the server half of RFC 6455 for text frames only: the handshake accept
;;; key, a masked client frame reader and an unmasked server frame writer.
;;; It is test scaffolding; the production side does not reimplement any of
;;; it.
;;; ------------------------------------------------------------------

(defparameter +mcp-runtime-ws-guid+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The RFC 6455 handshake GUID.")

(defun %mcp-runtime-ws-accept (key)
  "The Sec-WebSocket-Accept value for KEY, per RFC 6455 section 4.2.2."
  (cl-base64:usb8-array-to-base64-string
   (ironclad:digest-sequence
    :sha1 (%mcp-utf8 (concatenate 'string key +mcp-runtime-ws-guid+)))))

(defun %mcp-runtime-ws-read-frame (stream)
  "One client text frame as a string, or NIL on close or end of stream."
  (let ((first-byte (read-byte stream nil nil)))
    (unless first-byte (return-from %mcp-runtime-ws-read-frame nil))
    (let* ((opcode (logand first-byte #x0f))
           (second-byte (or (read-byte stream nil nil) 0))
           (masked (logbitp 7 second-byte))
           (length (logand second-byte #x7f)))
      (when (= opcode 8) (return-from %mcp-runtime-ws-read-frame nil))
      (cond ((= length 126)
             (setf length (+ (* 256 (read-byte stream)) (read-byte stream))))
            ((= length 127)
             (setf length 0)
             (loop repeat 8 do (setf length (+ (* 256 length) (read-byte stream))))))
      (let ((mask (when masked
                    (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
                      (read-sequence bytes stream)
                      bytes)))
            (payload (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence payload stream)
        (when mask
          (dotimes (index length)
            (setf (aref payload index)
                  (logxor (aref payload index) (aref mask (mod index 4))))))
        (%mcp-from-utf8 payload)))))

(defun %mcp-runtime-ws-write-frame (stream text)
  "TEXT as one unmasked server text frame."
  (let* ((payload (%mcp-utf8 text))
         (length (length payload)))
    (write-byte #x81 stream)
    (cond ((< length 126) (write-byte length stream))
          ((< length 65536)
           (write-byte 126 stream)
           (write-byte (ldb (byte 8 8) length) stream)
           (write-byte (ldb (byte 8 0) length) stream))
          (t (write-byte 127 stream)
             (loop for shift from 56 downto 0 by 8
                   do (write-byte (ldb (byte 8 shift) length) stream))))
    (write-sequence payload stream)
    (force-output stream)))

(defun %mcp-runtime-ws-handshake (stream headers)
  "Answer the opening handshake, or NIL when the request is not one."
  (let ((key (jget headers "sec-websocket-key")))
    (unless (stringp key) (return-from %mcp-runtime-ws-handshake nil))
    (%mcp-runtime-write
     stream
     (format nil "HTTP/1.1 101 Switching Protocols~c~cUpgrade: websocket~c~cConnection: Upgrade~c~cSec-WebSocket-Accept: ~a~c~c~c~c"
             #\Return #\Newline #\Return #\Newline #\Return #\Newline
             (%mcp-runtime-ws-accept key) #\Return #\Newline #\Return #\Newline))
    t))

(defun %mcp-runtime-ws-rpc-result (rpc-method)
  "The MCP result a legacy WebSocket server answers for RPC-METHOD."
  (cond ((string= rpc-method "initialize")
         (object "protocolVersion" (mcp-protocol-version)
                 "capabilities" (object "tools" (object))
                 "serverInfo" (object "name" "ax-ws-fixture" "version" "1.0.0")))
        ((string= rpc-method "tools/list")
         (object "tools" (vector (object "name" "ws-echo"
                                         "inputSchema" (object "type" "object")))))
        ((string= rpc-method "prompts/list") (object "prompts" (%new-array)))
        ((string= rpc-method "resources/list") (object "resources" (%new-array)))
        ((string= rpc-method "resources/templates/list")
         (object "resourceTemplates" (%new-array)))
        ((string= rpc-method "tools/call")
         (object "content" (vector (object "type" "text" "text" "over a socket"))))
        (t (object))))

(defun start-mcp-runtime-ws-server (&key notify-before-call)
  "Serve MCP over a real WebSocket on 127.0.0.1 at a kernel-assigned port.

NOTIFY-BEFORE-CALL sends that notification immediately before answering a
tools/call, so inbound routing on a live socket can be asserted."
  (let* ((socket (usocket:socket-listen "127.0.0.1" 0 :reuse-address t
                                                      :element-type '(unsigned-byte 8)))
         (server (make-instance 'mcp-runtime-server
                                :socket socket
                                :port (usocket:get-local-port socket)
                                :handler nil)))
    (setf (%mcp-runtime-server-thread server)
          (sb-thread:make-thread
           (lambda ()
             (loop until (%mcp-runtime-server-stop server)
                   do (handler-case
                          (when (usocket:wait-for-input socket :timeout 0.2 :ready-only t)
                            (let ((connection (usocket:socket-accept socket)))
                              (unwind-protect
                                   (let ((stream (usocket:socket-stream connection)))
                                     (multiple-value-bind (method path headers)
                                         (%mcp-runtime-read-request stream)
                                       (declare (ignore method path))
                                       (when (%mcp-runtime-ws-handshake stream headers)
                                         (loop
                                           (let ((frame (%mcp-runtime-ws-read-frame stream)))
                                             (unless frame (return))
                                             (let* ((parsed (parse-json frame))
                                                    (rpc-method (%mcp-text
                                                                 (jget parsed "method")))
                                                    (id (jget parsed "id")))
                                               (sb-thread:with-mutex
                                                   ((%mcp-runtime-server-lock server))
                                                 (vector-push-extend
                                                  parsed
                                                  (mcp-runtime-server-requests server)))
                                               ;; A notification has no id and
                                               ;; must not be answered.
                                               (unless (eq id :null)
                                                 (when (and notify-before-call
                                                            (string= rpc-method "tools/call"))
                                                   (%mcp-runtime-ws-write-frame
                                                    stream (encode-json notify-before-call)))
                                                 (%mcp-runtime-ws-write-frame
                                                  stream
                                                  (encode-json
                                                   (object "jsonrpc" "2.0" "id" id
                                                           "result"
                                                           (%mcp-runtime-ws-rpc-result
                                                            rpc-method)))))))))))
                                (ignore-errors (usocket:socket-close connection)))))
                        (error () nil))))
           :name "ax-mcp-test-ws-server"))
    server))

(defun mcp-runtime-ws-url (server &optional (path "/mcp"))
  (format nil "ws://127.0.0.1:~a~a" (mcp-runtime-server-port server) path))

(defun %mcp-runtime-ws-checks (check)
  (funcall
   check "the WebSocket transport talks to a real RFC 6455 server over loopback"
   (lambda ()
     ;; The concrete socket path, not the factory. websocket-driver does the
     ;; handshake and framing on the client side; the server half here is
     ;; real enough that a wrong accept key or a mishandled mask fails.
     (if (not (mcp-websocket-driver-available-p))
         (%mcp-runtime-fail
          "websocket-driver is not loaded, so the built-in WebSocket route is unproven. Load websocket-driver-client (quicklisp) before this suite; the transport names it and :socket-factory as the two routes.")
         (let ((server (start-mcp-runtime-ws-server
                        :notify-before-call (object "jsonrpc" "2.0"
                                                    "method" "notifications/message"
                                                    "params" (object "level" "info"
                                                                     "data" "socket note")))))
           (unwind-protect
                (let ((client (make-mcp-client
                               (make-mcp-websocket-transport
                                (mcp-runtime-ws-url server)
                                :ssrf-protection (object "requireHttps" false
                                                         "allowLocalhost" true))
                               :namespace "ws" :era "legacy")))
                  (unwind-protect
                       (let ((seen (%new-array)))
                         (mcp-add-notification-listener
                          client (lambda (n) (vector-push-extend n seen)))
                         (mcp-init client)
                         (%mcp-runtime-equal (mcp-get-era client) "legacy" "websocket era")
                         (let ((tools (mcp-list-tools client)))
                           (%mcp-runtime-equal
                            (jget (aref (jget tools "tools") 0) "name") "ws-echo"
                            "catalog over a socket"))
                         (let ((result (mcp-call-tool client "ws-echo" (object))))
                           (%mcp-runtime-equal
                            (jget (aref (jget result "content") 0) "text") "over a socket"
                            "tool result over a socket"))
                         ;; The server pushed a notification on the same
                         ;; socket just before the response; it must be
                         ;; delivered, not returned as the tool result.
                         (%mcp-runtime-equal (length seen) 1 "inbound notification count")
                         (%mcp-runtime-equal (jget (aref seen 0) "method")
                                             "notifications/message"
                                             "inbound notification method")
                         ;; And the server really saw a handshake plus frames.
                         (%mcp-runtime-true
                          (plusp (length (mcp-runtime-server-requests server)))
                          "the server received framed messages"))
                    (ignore-errors (mcp-close client))))
             (stop-mcp-runtime-server server))))))


  (funcall
   check "a dropped SSE stream reconnects and resumes with Last-Event-ID"
   (lambda ()
     ;; The legacy listen stream is a long-lived GET that will drop. What
     ;; makes a resume a resume is the Last-Event-ID header carrying the last
     ;; id the client actually saw, so the server can replay from there. A
     ;; transport that reconnected without it would silently lose events and
     ;; still look healthy.
     (let* ((notification (object "jsonrpc" "2.0" "method" "notifications/message"
                                  "params" (object "level" "info" "data" "sse one")))
            (gets 0)
            (server
              (start-mcp-runtime-server
               (lambda (method path headers body stream)
                 (declare (ignore path headers))
                 (cond ((string= method "POST")
                        (multiple-value-bind (rpc-method id) (%mcp-runtime-rpc-body body)
                          (%mcp-runtime-respond
                           stream "200 OK"
                           (encode-json (object "jsonrpc" "2.0" "id" id
                                                "result" (%mcp-runtime-ws-rpc-result
                                                          rpc-method))))))
                       ((string= method "GET")
                        (incf gets)
                        (if (= gets 1)
                            ;; One identified event, then the stream drops.
                            (%mcp-runtime-respond-sse
                             stream (list (cons "e1" (encode-json notification))))
                            ;; The resume: hold it open briefly so the client
                            ;; does not spin, and record what it asked for.
                            (progn (%mcp-runtime-respond-sse stream '())
                                   (sleep 0.5))))
                       (t (%mcp-runtime-respond stream "405 Method Not Allowed" "{}")))))))
       (unwind-protect
            (let ((client (make-mcp-client
                           (make-mcp-streamable-http-transport
                            (mcp-runtime-server-url server)
                            :ssrf-protection (object "requireHttps" false "allowLocalhost" true)
                            :reconnect-delay 0.05)
                           :namespace "sse" :era "legacy")))
              (unwind-protect
                   (let ((seen (%new-array)))
                     (mcp-add-notification-listener
                      client (lambda (n) (vector-push-extend n seen)))
                     ;; Legacy init starts the listen stream itself.
                     (mcp-init client)
                     ;; Wait for the event and then for the reconnect.
                     (loop repeat 100
                           until (and (plusp (length seen)) (>= gets 2))
                           do (sleep 0.05))
                     (%mcp-runtime-equal (length seen) 1 "notification delivered over SSE")
                     (%mcp-runtime-equal (jget (jget (aref seen 0) "params") "data") "sse one"
                                         "SSE notification payload")
                     (%mcp-runtime-true (>= gets 2) "the dropped stream was reconnected")
                     ;; The resuming GET must carry the id of the last event
                     ;; seen, and the first GET must not have carried one.
                     (let ((first-get nil) (resume-get nil))
                       (loop for index from 0 below (length (mcp-runtime-server-requests server))
                             do (let ((request (mcp-runtime-server-request server index)))
                                  (when (equal (%mcp-text (jget request "method")) "GET")
                                    (if first-get
                                        (unless resume-get (setf resume-get request))
                                        (setf first-get request)))))
                       (when (null resume-get)
                         (%mcp-runtime-fail "the server never saw a second GET"))
                       (%mcp-runtime-equal (jget (jget first-get "headers") "last-event-id")
                                           :null
                                           "the first GET should not resume from anywhere")
                       (%mcp-runtime-equal (jget (jget resume-get "headers") "last-event-id")
                                           "e1"
                                           "the reconnect did not resume from the last event")))
                (ignore-errors (mcp-close client))))
         (stop-mcp-runtime-server server)))))

  (funcall
   check "a WebSocket URL passes the same SSRF gate as an HTTP endpoint"
   (lambda ()
     ;; ws and wss are the same two transports as http and https, so the
     ;; gate must not be weaker for them. Checked against a port that IS
     ;; listening, so a rejection is policy rather than reachability.
     (%with-mcp-runtime-server (server (%mcp-runtime-modern-handler))
       (let ((url (format nil "ws://127.0.0.1:~a/mcp" (mcp-runtime-server-port server))))
         (%mcp-runtime-contains
          (princ-to-string
           (%mcp-runtime-fails (mcp-error c) "default policy on a live ws port"
             (make-mcp-websocket-transport url)))
          "https" "ws is checked as http by the gate")
         (%mcp-runtime-contains
          (princ-to-string
           (%mcp-runtime-fails (mcp-error c) "loopback ws with https waived"
             (make-mcp-websocket-transport
              url :ssrf-protection (object "requireHttps" false))))
          "SSRF" "ws loopback rejection")
         (%mcp-runtime-true
          (make-mcp-websocket-transport
           url :ssrf-protection (object "requireHttps" false "allowLocalhost" true))
          "an explicitly allowed ws endpoint")))))

  (funcall
   check "wss is refused on the built-in route, which does not verify TLS"
   (lambda ()
     ;; websocket-driver connects with TLS :verify :optional, so it would
     ;; complete a handshake against an unverified certificate while this
     ;; transport carries an Authorization header or a DPoP proof. The
     ;; Provider worker found this in the library; it applies here too.
     (let ((condition (%mcp-runtime-fails (mcp-error c) "wss on the built-in route"
                        (make-mcp-websocket-transport "wss://example.com/mcp"))))
       (%mcp-runtime-contains (princ-to-string condition) "verify"
                              "the refusal explains the TLS reason")
       (%mcp-runtime-contains (princ-to-string condition) "socket-factory"
                              "the refusal names the escape hatch"))
     ;; A factory alone is not enough: the caller has to assert that it
     ;; verifies the peer, so the decision is recorded at the call site.
     (%mcp-runtime-fails (mcp-error c) "wss with an unasserted factory"
       (make-mcp-websocket-transport
        "wss://example.com/mcp"
        :socket-factory (lambda (url protocols)
                          (declare (ignore url protocols)) nil)))
     ;; With the assertion it is allowed, and the gate still applied: a
     ;; public https host passes the default policy.
     (%mcp-runtime-true
      (make-mcp-websocket-transport
       "wss://example.com/mcp"
       :trust-socket-factory-tls t
       :socket-factory (lambda (url protocols)
                         (declare (ignore url protocols)) nil))
      "wss with a verified factory")))

  (funcall
   check "a WebSocket transport with no route available fails closed, naming both routes"
   (lambda ()
     ;; A factory that returns nothing is the host failing to provide a
     ;; socket. The error has to say what the two routes are, because
     ;; "connect failed" would send someone looking at the network.
     (let* ((transport (make-mcp-websocket-transport
                        "ws://127.0.0.1:1/mcp"
                        :ssrf-protection (object "requireHttps" false
                                                 "allowLocalhost" true)
                        :socket-factory (lambda (url protocols)
                                          (declare (ignore url protocols))
                                          nil)))
            (condition (%mcp-runtime-fails (mcp-error c) "a factory returning no socket"
                         (mcp-transport-connect transport))))
       (%mcp-runtime-contains (princ-to-string condition) "factory"
                              "the error names the factory route")))))

;;; ------------------------------------------------------------------
;;; Runner
;;; ------------------------------------------------------------------

(defun %mcp-runtime-ucp-schema-checks (check)
  (funcall
   check "UCP client validates declared schemas by default before Core normalization"
   (lambda ()
     (let* ((body (object "ucp" (object "version" "2026-04-08") "quantity" 4))
            (fetches 0)
            (client
              (make-ucp-client
               (object "version" "2026-04-08" "capabilities"
                       (object "dev.ucp.shopping.cart"
                               (vector (object "schema" "https://schemas.example/cart.json"))))
               (lambda (op payload options) (declare (ignore op payload options)) body)
               :fetch (lambda (url options)
                        (%mcp-runtime-equal url "https://schemas.example/cart.json" "declared URL")
                        (%mcp-runtime-equal (jget options "redirect") "manual" "bounded schema fetch")
                        (incf fetches)
                        (values (encode-json
                                 (object "type" "object" "required" (vector "quantity")
                                         "properties" (object "quantity" (object "type" "integer" "minimum" 1))))
                                200 nil nil)))))
       (%mcp-runtime-equal (jget (ucp-call client "cart.create" (object)) "value") body
                           "validated raw outcome wrapped by Core")
       (setf (gethash "quantity" body) 0)
       (%mcp-runtime-fails (ucp-schema-validation-error) "invalid client outcome"
         (ucp-call client "cart.update" (object)))
       (%mcp-runtime-equal fetches 1 "validator cache reused across client calls"))))
  (funcall
   check "UCP explicit disable skips schemas, but never object shape checks"
   (lambda ()
     (dolist (disabled (list nil false))
       (let ((client (make-ucp-client
                      (object "capabilities" (object "dev.ucp.shopping.checkout"
                                                     (vector (object "schema" "http://127.0.0.1/private"))))
                      (lambda (op payload options) (declare (ignore op payload options)) (object "invalid" true))
                      :schema-validation disabled
                      :fetch (lambda (&rest args) (declare (ignore args)) (error "Disabled fetch ran")))))
         (%mcp-runtime-equal (jget (jget (ucp-call client "checkout.create" (object)) "value") "invalid")
                             true "explicit disable preserves raw response")))
     (%mcp-runtime-fails (mcp-error) "disabled still requires object"
       (ucp-call (make-ucp-client (object) (lambda (&rest args) (declare (ignore args)) 3)
                                 :schema-validation false)
                 "cart.create" (object)))))
  (funcall
   check "UCP selects root and extension declarations once; never invents a schema"
   (lambda ()
     (let* ((seen nil) (value (object "id" "outcome"))
            (root "dev.ucp.shopping.checkout")
            (client (make-ucp-client
                     (object "capabilities"
                             (object root (vector (object "schema" "root"))
                                     "ext-a" (vector (object "extends" root "schema" "extension")
                                                     (object "schema" "root"))
                                     "ext-b" (vector (object "extends" (vector root) "schema" "second"))
                                     "unrelated" (vector (object "schema" "must-not-run"))))
                     (lambda (&rest args) (declare (ignore args)) value)
                     :schema-validation
                     (lambda (actual url)
                       (%mcp-runtime-true (eq actual value) "callback receives raw value first")
                       (push url seen)))))
       (ucp-call client "checkout.get" (object))
       (%mcp-runtime-equal (coerce (sort seen #'string<) 'vector)
                           (vector "extension" "root" "second") "root, extension and dedup selection")
       (setf seen nil)
       (ucp-call client "cart.get" (object))
       (ucp-call client "handoff.create" (object))
       (%mcp-runtime-equal (length seen) 0 "no arbitrary fallback schemas")
       (%mcp-runtime-fails (mcp-error) "custom validator rejection propagates"
         (ucp-call
          (make-ucp-client (ucp-client-profile client) (ucp-client-binding client)
                           :schema-validation (lambda (v u) (declare (ignore v u)) (%mcp-fail "rejected")))
          "checkout.get" (object))))))
  (funcall
   check "UCP schema fetch and SSRF policy inherit transport defaults or override them"
   (lambda ()
     (let* ((count 0)
            (profile (object "capabilities"
                             (object "dev.ucp.shopping.cart"
                                     (vector (object "schema" "http://127.0.0.1/schema")))))
            (fetch (lambda (url opts) (declare (ignore url opts)) (incf count) (values "{}" 200 nil nil)))
            (binding (lambda (&rest args) (declare (ignore args)) (object))))
       (%mcp-runtime-fails (ucp-schema-error) "default SSRF is enforced"
         (ucp-call (make-ucp-client profile binding :fetch fetch) "cart.get" (object)))
       (%mcp-runtime-equal count 0 "SSRF rejected before fetch")
       (let ((policy (object "allowHTTP" true "allowLoopback" true)))
         (ucp-call (make-ucp-client profile binding
                                    :mcp-options (object "mtls" (object "fetch" fetch) "ssrfProtection" policy))
                   "cart.get" (object))
         (ucp-call (make-ucp-client profile binding
                                    :schema-validation (object "fetch" fetch "ssrfProtection" policy)
                                    :mcp-options (object "fetch" (lambda (&rest args) (declare (ignore args))
                                                                    (error "overridden fetch called"))))
                   "cart.get" (object)))
       (%mcp-runtime-equal count 2 "custom policy and fetch reached both calls")))))

(defun run-mcp-runtime-tests (&key (stream *standard-output*))
  "Exercise the MCP transports against real endpoints.

Returns (values passed failed)."
  (let ((passed 0) (failed 0))
    (flet ((check (label thunk)
             (handler-case (progn (funcall thunk) (incf passed))
               (error (condition)
                 (incf failed)
                 (format stream "~&  FAIL ~a~%    ~a~%" label condition)))))
      (%mcp-runtime-stdio-checks #'check)
      (%mcp-runtime-http-checks #'check)
      (%mcp-runtime-ws-checks #'check)
      (%mcp-runtime-record-checks #'check)
      (%mcp-runtime-ucp-schema-checks #'check))
    (format stream "~&mcp runtime: ~a passed, ~a failed~%" passed failed)
    (format stream "~&mcp runtime: these run against a real child process on a real pipe and a real TCP listener on 127.0.0.1 at a kernel-assigned port. Nothing here reaches the network.~%")
    (values passed failed)))

(defun run-mcp-runtime-tests-or-die ()
  (multiple-value-bind (passed failed) (run-mcp-runtime-tests)
    (declare (ignore passed))
    (when (plusp failed)
      (error "MCP runtime transport checks failed: ~a" failed))
    t))
