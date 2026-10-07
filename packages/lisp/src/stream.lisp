;;;; stream.lisp --- server-sent events and the streaming HTTP transport.
;;;;
;;;; Reference: tools/axir/internal/axir/templates/python/pyAI.py's
;;;; `_iter_sse_json' for the event grammar and `_open_http_response' /
;;;; `_stream_chat' for the transport, plus ir/axcore/stream.md.
;;;;
;;;; Core owns what a chunk means: `provider-normalize-stream-delta' and
;;;; `fold-chat-response-stream' turn these events into responses.  This file
;;;; owns only the two things a portable IR cannot hold: the byte-level event
;;;; grammar, and a real HTTP response read incrementally and closed on
;;;; cancellation.
;;;;
;;;; The grammar is deliberately exact.  A provider that ends its stream
;;;; without a trailing blank line, or splits a multi-byte character across two
;;;; TCP reads, or uses bare carriage returns, must not lose an event or
;;;; corrupt a character; each of those is a real provider behaviour rather
;;;; than a hypothetical.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Incremental UTF-8 decoding
;;;
;;; A chunk boundary falls wherever the network puts it, which is regularly in
;;; the middle of a multi-byte character.  Decoding each chunk on its own would
;;; replace that character with a replacement character, so only the bytes up
;;; to the last complete character are decoded and the remainder waits for the
;;; next chunk.
;;; ------------------------------------------------------------------

(defun %utf8-sequence-length (byte)
  "How many bytes the character starting with BYTE occupies, or NIL when BYTE
is a continuation byte or invalid."
  (cond ((< byte #x80) 1)
        ((< byte #xC0) nil)
        ((< byte #xE0) 2)
        ((< byte #xF0) 3)
        ((< byte #xF8) 4)
        (t nil)))

(defun %utf8-complete-prefix-length (bytes fill)
  "How many of BYTES' first FILL bytes form complete UTF-8 characters.

Looks back at most three bytes, which is the longest incomplete tail a valid
UTF-8 stream can end with."
  (let ((limit (max 0 (- fill 3))))
    (loop for index downfrom (1- fill) to limit
          for byte = (aref bytes index)
          for needed = (%utf8-sequence-length byte)
          when needed
            do (return (if (<= (+ index needed) fill) fill index))
          finally (return fill))))

(defclass utf8-decoder ()
  ((buffer :initform (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)
           :reader %decoder-buffer))
  (:documentation "Decodes a byte stream to text across chunk boundaries."))

(defun utf8-decoder () (make-instance 'utf8-decoder))

(defun utf8-decode-chunk (decoder bytes &key (final nil))
  "The text BYTES contribute, holding back an incomplete trailing character.

With FINAL, every remaining byte is decoded, so a stream that ends mid
character fails loudly rather than silently dropping its last bytes."
  (let ((buffer (%decoder-buffer decoder)))
    (map nil (lambda (byte) (vector-push-extend byte buffer)) bytes)
    (let* ((fill (fill-pointer buffer))
           (take (if final fill (%utf8-complete-prefix-length buffer fill))))
      (if (zerop take)
          ""
          (let ((text (handler-case
                          (sb-ext:octets-to-string
                           (coerce (subseq buffer 0 take) '(vector (unsigned-byte 8)))
                           :external-format :utf-8)
                        (error ()
                          (provider-fail :response
                                         "Provider stream was not valid UTF-8.")))))
            (replace buffer buffer :start2 take :end2 fill)
            (setf (fill-pointer buffer) (- fill take))
            text)))))

;;; ------------------------------------------------------------------
;;; The server-sent events grammar
;;; ------------------------------------------------------------------

(defparameter +sse-done-marker+ "[DONE]"
  "The payload every OpenAI-compatible stream ends with.  It is not JSON, so
it terminates the stream instead of being parsed.")

(defclass sse-decoder ()
  ((line :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
         :reader %sse-line)
   (data :initform '() :accessor %sse-data)
   (pending-cr :initform nil :accessor %sse-pending-cr)
   (at-start :initform t :accessor %sse-at-start)
   (terminated :initform nil :accessor %sse-terminated)
   (ready :initform '() :accessor %sse-ready))
  (:documentation
   "A server-sent events reader.

Feed it text with `sse-feed' and take whole events with `sse-take'.  It
accumulates `data:' lines, joins them with a newline, parses the result as
JSON, and stops at the [DONE] payload.  Comment lines and every other field
are ignored, which is what the reference does."))

(defun sse-decoder () (make-instance 'sse-decoder))

(defun sse-terminated-p (decoder) (%sse-terminated decoder))

(defun %sse-flush-event (decoder)
  "Finish the event whose data lines have accumulated, if any."
  (let ((data (nreverse (%sse-data decoder))))
    (setf (%sse-data decoder) '())
    (when data
      (let ((payload (%string-join (string #\Newline) data)))
        (if (string= (%trim payload) +sse-done-marker+)
            (progn (setf (%sse-terminated decoder) t) nil)
            (let ((event (handler-case (parse-json payload)
                           (error ()
                             (provider-fail :response
                                            "Provider stream sent a data payload that was ~
not JSON.")))))
              (setf (%sse-ready decoder) (append (%sse-ready decoder) (list event)))
              event))))))

(defun %sse-process-line (decoder line)
  "Handle one complete line of the stream."
  (cond
    ;; A blank line ends the current event.
    ((zerop (length line)) (%sse-flush-event decoder))
    ;; A line beginning with a colon is a comment; providers send these as
    ;; keep-alives.
    ((char= (char line 0) #\:) nil)
    (t
     (let* ((colon (position #\: line))
            (field (if colon (subseq line 0 colon) line))
            (value (if colon (subseq line (1+ colon)) "")))
       ;; Exactly one leading space is part of the framing, not the value.
       (when (and (plusp (length value)) (char= (char value 0) #\Space))
         (setf value (subseq value 1)))
       (when (string= field "data")
         (push value (%sse-data decoder)))
       nil))))

(defun sse-feed (decoder text)
  "Add TEXT to DECODER.  Returns DECODER; take events with `sse-take'."
  (let ((text text))
    ;; A byte-order mark belongs to the stream, not to the first event.
    (when (and (%sse-at-start decoder) (plusp (length text)))
      (setf (%sse-at-start decoder) nil)
      (when (char= (char text 0) (code-char #xFEFF))
        (setf text (subseq text 1))))
    (let ((line (%sse-line decoder)))
      (flet ((finish-line ()
               ;; SUBSEQ, not COERCE: the line buffer is a fill-pointer string,
               ;; so COERCE to STRING hands back that same object and resetting
               ;; the fill pointer would blank the line already passed on.
               (let ((complete (subseq line 0)))
                 (setf (fill-pointer line) 0)
                 (%sse-process-line decoder complete))))
        (loop for char across text
              until (%sse-terminated decoder)
              do (cond
                   ;; A carriage return ends a line; a following newline is
                   ;; part of the same terminator, not an empty line.
                   ((%sse-pending-cr decoder)
                    (setf (%sse-pending-cr decoder) nil)
                    (finish-line)
                    (unless (%sse-terminated decoder)
                      (cond ((char= char #\Newline) nil)
                            ((char= char #\Return) (setf (%sse-pending-cr decoder) t))
                            (t (vector-push-extend char line)))))
                   ((char= char #\Return) (setf (%sse-pending-cr decoder) t))
                   ((char= char #\Newline) (finish-line))
                   (t (vector-push-extend char line)))))))
  decoder)

(defun sse-finish (decoder)
  "Tell DECODER the stream ended.

A provider may send its last event without the trailing blank line the
grammar asks for, so the remaining line and data are flushed rather than
discarded."
  (unless (%sse-terminated decoder)
    (let ((line (%sse-line decoder)))
      (when (or (%sse-pending-cr decoder) (plusp (fill-pointer line)))
        (setf (%sse-pending-cr decoder) nil)
        (let ((complete (subseq line 0)))
          (setf (fill-pointer line) 0)
          (%sse-process-line decoder complete))))
    (%sse-flush-event decoder))
  decoder)

(defun sse-take (decoder)
  "DECODER's next complete event, or :NULL when it has none ready."
  (if (%sse-ready decoder)
      (pop (%sse-ready decoder))
      :null))

(defun sse-events (text)
  "Every event in TEXT, as a list.  For a stream already held in memory."
  (let ((decoder (sse-decoder))
        (out '()))
    (sse-feed decoder text)
    (sse-finish decoder)
    (loop for event = (sse-take decoder)
          until (eq event :null)
          do (push event out))
    (nreverse out)))

;;; ------------------------------------------------------------------
;;; A stream handle over a chunk source
;;; ------------------------------------------------------------------

(defun sse-stream-handle (read-chunk &key closer cancellation)
  "A stream handle that decodes server-sent events from READ-CHUNK.

READ-CHUNK answers the next octet vector or string, or NIL at end of stream.
CLOSER, when given, releases the transport.  CANCELLATION, when given, is
checked before each read and closes the transport as soon as the run is
cancelled, so a stalled response body does not outlive the run."
  (let ((decoder (sse-decoder))
        (bytes (utf8-decoder))
        (finished nil))
    (make-ax-stream-handle
     (lambda ()
       (loop
         (let ((ready (sse-take decoder)))
           (unless (eq ready :null) (return ready)))
         (when (or finished (sse-terminated-p decoder)) (return :null))
         (throw-if-cancelled cancellation)
         (let ((chunk (funcall read-chunk)))
           (cond
             ((null chunk)
              (setf finished t)
              (let ((tail (utf8-decode-chunk bytes #() :final t)))
                (when (plusp (length tail)) (sse-feed decoder tail)))
              (sse-finish decoder))
             ((stringp chunk) (sse-feed decoder chunk))
             (t (sse-feed decoder (utf8-decode-chunk bytes chunk)))))))
     :closer closer)))

;;; ------------------------------------------------------------------
;;; The streaming HTTP transport
;;;
;;; The non-streaming transport reads the whole body and returns it.  A stream
;;; cannot do that: the point is to read the response as it arrives.  So a
;;; streaming transport returns a chunk reader and a closer instead of a body,
;;; and keeps the same credential rules as the blocking one -- certificate
;;; verification required, redirects refused so the credential is never
;;; replayed to another host, and nothing about the request in any condition.
;;; ------------------------------------------------------------------

(defparameter *stream-read-size* 4096
  "How many bytes a single read from a streaming response asks for.")

(defun make-default-streaming-transport (timeout)
  "The default streaming HTTP transport.

Returns a function of (url headers json-body) answering
\(values read-chunk status closer).  READ-CHUNK answers the next octet vector
or NIL at end of stream; CLOSER releases the connection and is safe to call
more than once."
  (lambda (url headers json-body)
    (handler-case
        (multiple-value-bind (stream status)
            (drakma:http-request url
                                 :method :post
                                 :additional-headers
                                 (remove "content-type" headers
                                         :key #'car :test #'string-equal)
                                 :content-type "application/json"
                                 :content json-body
                                 :external-format-out :utf-8
                                 :connection-timeout timeout
                                 ;; Drakma otherwise skips certificate
                                 ;; verification even for an HTTPS endpoint.
                                 :verify :required
                                 ;; Never follow a redirect: Drakma would
                                 ;; replay the credential to the new host.
                                 :redirect nil
                                 :force-binary t
                                 :want-stream t)
          (let ((closed nil)
                (buffer (make-array *stream-read-size* :element-type '(unsigned-byte 8))))
            (values
             (lambda ()
               (if closed
                   nil
                   (let ((read (handler-case (read-sequence buffer stream)
                                 (error (condition)
                                   (provider-fail
                                    :stream
                                    (format nil "Provider stream ended early: ~a"
                                            (substitute #\Space #\Newline
                                                        (princ-to-string condition)))
                                    :retryable t)))))
                     (if (zerop read)
                         nil
                         (subseq buffer 0 read)))))
             status
             (lambda ()
               (unless closed
                 (setf closed t)
                 ;; Closing the body is what actually releases a stalled
                 ;; response, so it must not be skipped when it fails.
                 (handler-case (close stream) (error () nil)))))))
      (provider-error (condition) (error condition))
      (error (condition)
        (provider-fail :transport
                       (format nil "HTTP transport failure: ~a"
                               (substitute #\Space #\Newline (princ-to-string condition))))))))

;;; ------------------------------------------------------------------
;;; Native streaming chat
;;; ------------------------------------------------------------------

(defun %stream-status-failure (provider status)
  "Signal for a streaming response that never became a stream."
  (cond ((= status 401)
         (provider-fail :auth
                        (format nil "Authentication rejected by provider '~a' (HTTP 401). ~
The response body is not included in this condition." provider)
                        :provider provider :status status))
        ((not (<= 200 status 299))
         (provider-fail :status
                        (format nil "Provider '~a' returned HTTP ~a for a streamed request. ~
The response body is not included in this condition." provider status)
                        :provider provider :status status
                        ;; Core's own rule decides whether this is worth
                        ;; retrying, rather than a second opinion here.
                        :retryable (axllm/core::core-true-p
                                    (axllm/core::is-retryable-status status))))))

;;; Streaming for a provider client lives in provider.lisp, with the rest of
;;; the Core-driven request path. The legacy client this method served is gone.
