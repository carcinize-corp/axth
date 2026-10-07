;;;; mcp-app.lisp --- the MCP Apps host bridge.
;;;;
;;;; An MCP App is remote, untrusted UI shipped as a ui:// resource and run
;;;; in a sandboxed frame. The host renders it; this bridge supplies the
;;;; validated sandbox payload and the JSON-RPC dispatch between the frame
;;;; and the live MCP client.
;;;;
;;;; Every policy decision here is Core's, in ir/axcore/mcp.axir:
;;;;
;;;;   mcp_app_tool_meta           the tool's ui resourceUri and visibility
;;;;   mcp_app_tool_visible_to     whether the app principal may call a tool
;;;;   mcp_app_csp_source_list     rejects an unsafe CSP source outright
;;;;   mcp_app_resource_policy     the CSP, permission policy and sandbox
;;;;   mcp_app_resource_plan       ui:// scheme, MIME type, HTML document
;;;;   mcp_app_view_message_plan   the whole request and notification
;;;;                               dispatch, including the initialization
;;;;                               gate, reserved sandbox methods, link
;;;;                               scheme and display-mode validation
;;;;
;;;; What is native here is only effect: reading the resource through the
;;;; client, base64-decoding a blob body, calling the host's callbacks,
;;;; tracking the initialized flag and the outbound request id, and shaping
;;;; the JSON-RPC envelope Core told us to send. There is no second copy of
;;;; the CSP or visibility rules in this file; if you want to change what an
;;;; App may do, change Core.
;;;;
;;;; Two safety properties are worth stating because they are easy to lose:
;;;; a frame cannot act before it has initialized, and anything it pushes
;;;; into model context is marked untrusted with its own namespace and tool
;;;; as the source, so a prompt cannot later be mistaken for host content.

(in-package #:axllm)

(defparameter +mcp-apps-protocol-version+ "2026-01-26"
  "The MCP Apps protocol version this bridge answers ui/initialize with.")

(defparameter +mcp-app-resource-mime-type+ "text/html;profile=mcp-app"
  "The only MIME type an App resource may carry.")

(defparameter +mcp-app-display-modes+ '("inline" "fullscreen" "pip"))

(defclass mcp-app-bridge ()
  ((client :initarg :client :reader mcp-app-bridge-client)
   (tool :initarg :tool :reader mcp-app-bridge-tool)
   (options :initarg :options :reader mcp-app-bridge-options)
   (initialized :initform nil :reader mcp-app-bridge-initialized-p)
   (next-id :initform 1 :accessor %mcp-app-next-id)
   (lock :initform (sb-thread:make-mutex :name "ax-mcp-app") :reader %mcp-app-lock))
  (:documentation
   "A host-side bridge between one MCP App frame and one MCP client."))

(defun make-mcp-app-bridge (client tool &rest options)
  "A bridge for TOOL on CLIENT.

TOOL is a tool name or a tool object from the client's catalog. Host
callbacks, all optional, are given as alternating keys and values:

  :send-to-view            deliver a JSON-RPC message to the frame
  :host-capabilities       override what ui/initialize advertises
  :host-context            the host context ui/initialize reports
  :authorize               (action) predicate run before any App request
  :open-link               open an HTTP(S) URL
  :send-message            deliver an App message to the host
  :update-model-context    accept an untrusted model-context update
  :request-display-mode    (mode) returning the mode actually granted
  :log                     receive notifications/message params
  :size-changed            receive a validated {width, height}

An absent callback is a closed door: Core reports the matching request as
disabled rather than this bridge inventing a default."
  (let* ((table (object))
         (resolved (if (hash-table-p tool)
                       tool
                       (find (%mcp-text tool) (mcp-client-tools client)
                             :key (lambda (item) (%mcp-text (jget item "name")))
                             :test #'string=))))
    (loop for (key value) on options by #'cddr
          do (%set-key table (%mcp-option-name key) value))
    (unless resolved
      (%mcp-fail "MCP App tool not found: ~a" (%mcp-text tool)))
    (make-instance 'mcp-app-bridge :client client :tool resolved :options table)))

(defun %mcp-app-option (bridge name)
  (let ((value (jget (mcp-app-bridge-options bridge) name)))
    (and (functionp value) value)))

(defun mcp-app-tool-meta (tool)
  "TOOL's App metadata: resourceUri, visibility, hasVisibility."
  (axllm/core::mcp-app-tool-meta tool))

(defun mcp-app-tool-visible-to (tool principal)
  "Whether PRINCIPAL, \"model\" or \"app\", may call TOOL."
  (axllm/core::core-true-p (axllm/core::mcp-app-tool-visible-to tool principal)))

;;; ------------------------------------------------------------------
;;; Resource loading
;;; ------------------------------------------------------------------

(defun mcp-app-bridge-load-resource (bridge)
  "Read, validate and wrap this App's ui:// resource.

Returns the resource object Core assembled: uri, mimeType, html, meta,
sandbox, contentSecurityPolicy and permissionPolicy. A resource whose URI
is not ui://, whose MIME type is not the App type, whose body is not an
HTML document, or whose CSP names an unsafe source, is refused here rather
than handed to a frame."
  (let* ((tool (mcp-app-bridge-tool bridge))
         (name (%mcp-text (jget tool "name")))
         (uri (%mcp-text (jget (mcp-app-tool-meta tool) "resourceUri"))))
    (when (zerop (length uri))
      (%mcp-fail "MCP App tool ~a has no valid ui:// resource" name))
    (let* ((result (mcp-read-resource (mcp-app-bridge-client bridge) uri))
           (content (find uri (%event-array (jget result "contents"))
                          :key (lambda (item) (%mcp-text (jget item "uri")))
                          :test #'string=)))
      (unless content
        (%mcp-fail "MCP App resource ~a was not returned" uri))
      (let ((plan (axllm/core::mcp-app-resource-plan
                   name uri
                   (%mcp-text (jget content "mimeType"))
                   (%mcp-app-content-html content uri)
                   (%mcp-object-or-empty (jget (%mcp-object-or-empty (jget content "_meta")) "ui")))))
        (unless (axllm/core::core-true-p (jget plan "ok"))
          (%mcp-fail "~a" (%mcp-text (jget plan "message"))))
        (jget plan "resource")))))

(defun %mcp-app-content-html (content uri)
  "CONTENT's HTML body, decoding a base64 blob when that is how it came."
  (let ((text (jget content "text")))
    (if (stringp text)
        text
        (let ((blob (jget content "blob")))
          (unless (stringp blob)
            (%mcp-fail "MCP App resource ~a carried neither text nor blob" uri))
          (handler-case (%mcp-from-utf8 (%mcp-base64url-decode-standard blob))
            (error () (%mcp-fail "MCP App resource blob is not valid base64 HTML")))))))

(defun %mcp-base64url-decode-standard (text)
  (handler-case (cl-base64:base64-string-to-usb8-array text)
    (error () (%mcp-fail "invalid base64"))))

;;; ------------------------------------------------------------------
;;; Inbound frame messages
;;; ------------------------------------------------------------------

(defun mcp-app-bridge-handle-view-message (bridge message)
  "Dispatch one message from the frame. Returns a response, or :NULL.

Core decides what the message means; this runs the effect it named. A
request always gets a response, and a host callback that signals becomes a
JSON-RPC error rather than propagating into the frame's transport."
  (let* ((plan (axllm/core::mcp-app-view-message-plan
                message
                (json-boolean (mcp-app-bridge-initialized-p bridge))
                (%mcp-app-context bridge)))
         (action (%mcp-text (jget plan "action")))
         (id (jget plan "id"))
         (request (%mcp-present-key-p message "id")))
    (handler-case
        (cond ((string= action "initialized")
               (setf (slot-value bridge 'initialized) t)
               :null)
              ((string= action "ignore") :null)
              ((string= action "log")
               (let ((log (%mcp-app-option bridge "log")))
                 (when log (funcall log (jget plan "params"))))
               :null)
              ((string= action "size-changed")
               (let ((sized (%mcp-app-option bridge "sizeChanged")))
                 (when sized (funcall sized (jget plan "size"))))
               :null)
              ((string= action "error")
               (if request
                   (%mcp-app-error id (%mcp-text (jget plan "reason")))
                   (%mcp-fail "~a" (%mcp-text (jget plan "reason")))))
              ((string= action "respond") (%mcp-app-result id (jget plan "result")))
              (t (%mcp-app-perform bridge plan action id)))
      (mcp-error (condition)
        (if request (%mcp-app-error id (princ-to-string condition)) (error condition)))
      (error (condition)
        (if request (%mcp-app-error id (princ-to-string condition)) (error condition))))))

(defun %mcp-app-perform (bridge plan action id)
  "Run the effect Core named, after the host's authorization hook."
  (%mcp-app-authorize bridge action plan)
  (cond ((string= action "call-tool")
         ;; Core already checked that this tool exists and is app-visible.
         (%mcp-app-result id (mcp-call-tool (mcp-app-bridge-client bridge)
                                            (%mcp-text (jget plan "name"))
                                            (%mcp-object-or-empty (jget plan "arguments")))))
        ((string= action "read-resource")
         (%mcp-app-result id (mcp-read-resource (mcp-app-bridge-client bridge)
                                                (%mcp-text (jget plan "uri")))))
        ((string= action "open-link")
         (let ((open (%mcp-app-option bridge "openLink")))
           (unless open (%mcp-fail "Link opening is disabled"))
           (funcall open (%mcp-text (jget plan "url")))
           (%mcp-app-result id (object))))
        ((string= action "send-message")
         (let ((send (%mcp-app-option bridge "sendMessage")))
           (unless send (%mcp-fail "App messages are disabled"))
           (funcall send (%mcp-object-or-empty (jget plan "params")))
           (%mcp-app-result id (object))))
        ((string= action "update-model-context")
         (let ((update (%mcp-app-option bridge "updateModelContext")))
           (unless update (%mcp-fail "App model-context updates are disabled"))
           ;; Core stamped untrusted and the source; pass it through unchanged.
           (funcall update (jget plan "update"))
           (%mcp-app-result id (object))))
        ((string= action "request-display-mode")
         (let* ((requested (%mcp-text (jget plan "mode")))
                (handler (%mcp-app-option bridge "requestDisplayMode"))
                (granted (if handler (%mcp-text (funcall handler requested)) "inline")))
           ;; The host grants a mode; a host that answers with a mode the
           ;; protocol does not define is a host bug, not an App request.
           (unless (member granted +mcp-app-display-modes+ :test #'string=)
             (%mcp-fail "host granted an invalid MCP App display mode ~a" granted))
           (%mcp-app-result id (object "mode" granted))))
        (t (%mcp-fail "unhandled MCP App plan action ~a" action))))

(defun %mcp-app-authorize (bridge action plan)
  (let ((authorize (%mcp-app-option bridge "authorize")))
    (when authorize
      (let ((decision (funcall authorize
                               (object "action" action
                                       "method" (jget plan "method" action)
                                       "params" (jget plan "params" :null)
                                       "namespace" (mcp-namespace (mcp-app-bridge-client bridge))
                                       "tool" (jget (mcp-app-bridge-tool bridge) "name")))))
        (when (json-false-p decision)
          (%mcp-fail "MCP App request denied: ~a" action)))))
  nil)

(defun %mcp-app-context (bridge)
  "The host facts Core needs to plan a frame message.

Each can* flag is simply whether the host installed that callback, so Core
decides \"disabled\" instead of this file inventing the message."
  (let ((client (mcp-app-bridge-client bridge)))
    (object "namespace" (mcp-namespace client)
            "tool" (jget (mcp-app-bridge-tool bridge) "name")
            "tools" (mcp-client-tools client)
            "hostCapabilities" (jget (mcp-app-bridge-options bridge) "hostCapabilities")
            "hostContext" (jget (mcp-app-bridge-options bridge) "hostContext")
            "canOpenLink" (json-boolean (%mcp-app-option bridge "openLink"))
            "canSendMessage" (json-boolean (%mcp-app-option bridge "sendMessage"))
            "canUpdateModelContext"
            (json-boolean (%mcp-app-option bridge "updateModelContext")))))

(defun %mcp-app-result (id result)
  (object "jsonrpc" "2.0" "id" id "result" (if (eq result :null) (object) result)))

(defun %mcp-app-error (id message)
  (object "jsonrpc" "2.0" "id" id
          "error" (object "code" -32000 "message" message)))

;;; ------------------------------------------------------------------
;;; Outbound notifications
;;; ------------------------------------------------------------------

(defun %mcp-app-notify (bridge method params)
  (unless (mcp-app-bridge-initialized-p bridge)
    (%mcp-fail "MCP App is not initialized"))
  (let ((send (%mcp-app-option bridge "sendToView")))
    (when send
      (funcall send (object "jsonrpc" "2.0" "method" method "params" params))))
  nil)

(defun mcp-app-bridge-notify-tool-input (bridge arguments)
  "Tell the frame the final tool arguments."
  (%mcp-app-notify bridge "ui/notifications/tool-input"
                   (object "arguments" (%mcp-object-or-empty arguments))))

(defun mcp-app-bridge-notify-tool-input-partial (bridge arguments)
  "Tell the frame the arguments so far, while they are still streaming."
  (%mcp-app-notify bridge "ui/notifications/tool-input-partial"
                   (object "arguments" (%mcp-object-or-empty arguments))))

(defun mcp-app-bridge-notify-tool-result (bridge result)
  "Give the frame the tool result."
  (%mcp-app-notify bridge "ui/notifications/tool-result" result))

(defun mcp-app-bridge-notify-tool-cancelled (bridge reason)
  (%mcp-app-notify bridge "ui/notifications/tool-cancelled" (object "reason" reason)))

(defun mcp-app-bridge-notify-host-context-changed (bridge context)
  (%mcp-app-notify bridge "ui/notifications/host-context-changed"
                   (%mcp-object-or-empty context)))

(defun mcp-app-bridge-teardown (bridge reason)
  "Tear the frame down and require a fresh initialization afterwards."
  (let ((send (%mcp-app-option bridge "sendToView"))
        (id (sb-thread:with-mutex ((%mcp-app-lock bridge))
              (prog1 (%mcp-app-next-id bridge) (incf (%mcp-app-next-id bridge))))))
    (when send
      (funcall send (object "jsonrpc" "2.0" "id" id
                            "method" "ui/resource-teardown"
                            "params" (object "reason" reason)))))
  (setf (slot-value bridge 'initialized) nil)
  nil)

(export '(mcp-app-bridge make-mcp-app-bridge mcp-app-bridge-client
          mcp-app-bridge-tool mcp-app-bridge-initialized-p
          mcp-app-bridge-load-resource mcp-app-bridge-handle-view-message
          mcp-app-bridge-notify-tool-input mcp-app-bridge-notify-tool-input-partial
          mcp-app-bridge-notify-tool-result mcp-app-bridge-notify-tool-cancelled
          mcp-app-bridge-notify-host-context-changed mcp-app-bridge-teardown
          mcp-app-tool-meta mcp-app-tool-visible-to))
