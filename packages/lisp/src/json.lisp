;;;; json.lisp --- the JSON value model and the conditions Ax signals.
;;;;
;;;; Ax's portable Core code is written against JSON values, so this port
;;;; uses JSON values directly rather than converting at every boundary:
;;;;
;;;;   object   STRING-keyed EQUAL hash table, key order preserved
;;;;   array    vector
;;;;   string   string
;;;;   number   integer or float
;;;;   boolean  YASON:TRUE / YASON:FALSE
;;;;   null     :NULL
;;;;
;;;; Two choices in that list are worth stating plainly.
;;;;
;;;; Booleans are YASON:TRUE and YASON:FALSE rather than T and NIL. Core
;;;; distinguishes false from absent, and NIL cannot carry that distinction:
;;;; it would also be null, the empty list and the empty object.
;;;;
;;;; Key order is real data. JSON Schema puts required property names in an
;;;; array and Ax renders object field lists into signature text, so "the
;;;; order the fields were written in" is observable output, not a detail.
;;;; A Common Lisp hash table has no order, so this file keeps the order
;;;; beside the table in a weak side table. Objects built by OBJECT,
;;;; PARSE-JSON or Core keep their insertion order; an object built by hand
;;;; with MAKE-HASH-TABLE and SETF GETHASH has no recorded order and is
;;;; read in sorted key order, which is deterministic but not insertion
;;;; order. Prefer OBJECT.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Boolean constants
;;; ------------------------------------------------------------------

;;; The JSON booleans are the symbols YASON:TRUE and YASON:FALSE, so
;;; writing one directly in code reads it as a variable and fails with an
;;; unbound-variable error. These constants hold those same symbols, so
;;; (jget object "flag") can be compared with AX:TRUE, and AX:FALSE can be
;;; stored, without remembering to quote. :NULL is a keyword and is already
;;; self-evaluating, so it needs no counterpart.

(defconstant true 'yason:true
  "The JSON value true. EQ to YASON:TRUE.")

(defconstant false 'yason:false
  "The JSON value false. EQ to YASON:FALSE.")

;;; ------------------------------------------------------------------
;;; Conditions
;;; ------------------------------------------------------------------

(define-condition ax-error (error)
  ((message :initarg :message :initform "" :reader ax-error-message
            :documentation "The human-readable message, as Ax wrote it."))
  (:report (lambda (condition stream)
             (write-string (ax-error-message condition) stream)))
  (:documentation "Base condition for every error Ax signals."))

(define-condition signature-error (ax-error)
  ()
  (:documentation
   "An invalid signature: bad syntax, an unknown type or modifier, a field
that may not appear where it does, or a name that collides."))

(define-condition validation-error (ax-error)
  ()
  (:documentation "A value that does not satisfy its field's constraints."))

;;; ------------------------------------------------------------------
;;; Object key order
;;; ------------------------------------------------------------------

(defvar *object-key-order*
  (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Maps a JSON object to the order its STRING keys were first written in.

Weak on the key, so recording an order never keeps an object alive.")

(defun %record-key (object key)
  "Note KEY as the next key of OBJECT, if it is new and a string."
  (when (stringp key)
    (let ((order (gethash object *object-key-order*)))
      (unless order
        (setf order (make-array 0 :adjustable t :fill-pointer 0)
              (gethash object *object-key-order*) order))
      (unless (find key order :test #'string=)
        (vector-push-extend key order))))
  key)

(defun %array-index-key (key)
  "The array index KEY names as a JavaScript property name, or NIL.

JavaScript calls a key an array index when it is the canonical decimal
spelling of an integer from 0 through 2^32-2: no sign, no leading zero and
no fraction. \"0\", \"1\" and \"4294967294\" are indices; \"01\", \"-1\", \"1.5\"
and \"4294967295\" are ordinary keys."
  (let ((length (length key)))
    (when (and (plusp length)
               (or (= length 1) (char/= (char key 0) #\0))
               (every (lambda (character) (char<= #\0 character #\9)) key))
      (let ((index (parse-integer key)))
        (when (<= index 4294967294) index)))))

(defun %object-keys (object)
  "OBJECT's STRING keys in JavaScript's own-property order.

Array-index keys come first in ascending numeric order, then the remaining
keys in the order they were first written, which is what Object.keys,
for...in and JSON.stringify all follow in the reference implementation. A
key written without OBJECT has no recorded position and sorts after the
recorded ones, so an object built by hand stays deterministic instead of
exposing hash order. Non-string keys are internal record metadata and are
never returned."
  (let ((recorded (gethash object *object-key-order*))
        (keys '()))
    (let ((seen '()))
      (when recorded
        (loop for key across recorded
              when (nth-value 1 (gethash key object))
                do (push key keys) (push key seen)))
      (setf keys (nreverse keys))
      (let ((extra '()))
        (maphash (lambda (key value)
                   (declare (ignore value))
                   (when (and (stringp key) (not (member key seen :test #'string=)))
                     (push key extra)))
                 object)
        (let ((ordered (append keys (sort extra #'string<)))
              (indexed '())
              (named '()))
          (dolist (key ordered)
            (let ((index (%array-index-key key)))
              (if index (push (cons index key) indexed) (push key named))))
          (append (mapcar #'cdr (sort (nreverse indexed) #'< :key #'car))
                  (nreverse named)))))))

(defun %set-key (object key value)
  "Set OBJECT's KEY to VALUE, recording KEY's position on first write."
  (%record-key object key)
  (setf (gethash key object) value))

(defun %delete-key (object key)
  "Remove KEY from OBJECT, and forget the position it held.

Forgetting the position is the point: a key written again after being
deleted is a new key and takes the next position, as it does in
JavaScript. Leaving the old position recorded would put it back where it
used to be, so deleting a and then writing it again would read as a, b
instead of b, a."
  (remhash key object)
  (let ((order (gethash object *object-key-order*)))
    (when (and order (stringp key))
      (let ((at (position key order :test #'string=)))
        (when at
          (replace order order :start1 at :start2 (1+ at))
          (decf (fill-pointer order))))))
  object)

(defun %new-object ()
  (make-hash-table :test 'equal))

(defun %new-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defun %object-p (value)
  (hash-table-p value))

(defun %array-p (value)
  (and (vectorp value) (not (stringp value))))

;;; ------------------------------------------------------------------
;;; Public constructors and accessors
;;; ------------------------------------------------------------------

(defun object (&rest alternating-key-values)
  "Build a JSON object from alternating keys and values, in this order.

  (object \"name\" \"age\" \"type\" (object \"name\" \"number\"))

Keys are compared with STRING=; a repeated key keeps its first position and
takes the last value, as a JSON object would."
  (let ((result (%new-object)))
    (loop for rest = alternating-key-values then (cddr rest)
          while rest
          do (let ((key (first rest)))
               (unless (cdr rest)
                 (error 'ax-error
                        :message (format nil "object: key ~S has no value; arguments must be alternating keys and values"
                                         key)))
               (unless (stringp key)
                 (error 'ax-error
                        :message (format nil "object: key ~S is not a string; JSON object keys are strings" key)))
               (%set-key result key (second rest))))
    result))

(defun jget (object key &optional (default :null))
  "OBJECT's value at KEY, or DEFAULT when KEY is absent.

OBJECT may be a JSON object (string key) or a JSON array (integer index).
DEFAULT is :NULL unless given, so a missing key reads as JSON null rather
than as NIL."
  (cond ((%object-p object)
         (multiple-value-bind (value found) (gethash key object)
           (if found value default)))
        ((%array-p object)
         (if (and (integerp key) (< -1 key (length object)))
             (aref object key)
             default))
        (t default)))

;;; ------------------------------------------------------------------
;;; Parsing
;;; ------------------------------------------------------------------

;;; The largest finite double is (2 - 2^-52) x 2^1023, and a magnitude
;;; rounds to it while it stays below the halfway point to the next value,
;;; 2^1024 - 2^970. At or above that, IEEE rounding would give an infinity,
;;; which is not a JSON number, so this parser rejects the document rather
;;; than inventing one.
(defconstant +double-overflow-threshold+ (- (expt 2 1024) (expt 2 970)))

(defun %json-number-value (token)
  "The number JSON TOKEN denotes, which must already be valid JSON syntax.

An integer token becomes an exact Lisp integer, keeping every digit. A
token with a fraction or an exponent becomes the nearest double, computed
through exact rational arithmetic: Lisp's own reader overflows on
1.7976931348623157e308, the largest double there is, because it rounds
while scaling. A magnitude too large for a double is an error, as is any
token this cannot read."
  (let ((dot (position #\. token))
        (exponent-at (position-if (lambda (character) (find character "eE")) token)))
    (if (and (null dot) (null exponent-at))
        (or (ignore-errors (parse-integer token))
            (error 'ax-error :message (format nil "Invalid JSON number ~s" token)))
        (let* ((negative (char= (char token 0) #\-))
               (body-start (if negative 1 0))
               (body-end (or exponent-at (length token)))
               (whole-end (or dot body-end))
               (whole (subseq token body-start whole-end))
               (fraction (if dot (subseq token (1+ dot) body-end) ""))
               (exponent (if exponent-at
                             (parse-integer token :start (1+ exponent-at))
                             0))
               (digits (parse-integer (concatenate 'string whole fraction)))
               (scale (- exponent (length fraction)))
               (exact (* digits (expt 10 scale))))
          (when (>= exact +double-overflow-threshold+)
            (error 'ax-error :message (format nil "JSON number ~s is out of range" token)))
          ;; A magnitude below the smallest subnormal rounds to zero, as it
          ;; does in the reference implementation; that is not an error.
          (let ((value (sb-int:with-float-traps-masked (:underflow :inexact)
                         (coerce exact 'double-float))))
            (if negative (- value) value))))))

(defun parse-json (string)
  "Parse STRING as one complete JSON document.

STRING must hold exactly one JSON value; trailing content after it is an
error rather than being ignored. Signals AX-ERROR on invalid input."
  (unless (stringp string)
    (error 'ax-error :message (format nil "parse-json: expected a string, got ~S" string)))
  ;; Yason accepts trailing commas, unquoted keys and non-JSON numbers.
  ;; Enforce the grammar here; use Yason only to decode validated strings.
  (let ((at 0) (size (length string))
        (*read-base* 10) (*read-eval* nil)
        (*read-default-float-format* 'double-float))
    (labels ((invalid ()
               (error 'ax-error :message (format nil "Invalid JSON at character ~d" at)))
             (peek () (when (< at size) (char string at)))
             (take (char)
               (when (eql char (peek)) (incf at) t))
             (space ()
               (loop while (member (peek) '(#\Space #\Tab #\Newline #\Return))
                     do (incf at)))
             (hex-unit ()
               ;; The four hex digits of a \u escape, as one UTF-16 unit.
               (let ((end (+ at 4)))
                 (unless (and (<= end size)
                              (every (lambda (digit) (digit-char-p digit 16))
                                     (subseq string at end)))
                   (invalid))
                 (let ((unit (parse-integer string :start at :end end :radix 16)))
                   (setf at end)
                   unit)))
             (text-value ()
               ;; Decoded here rather than handed to a JSON library, so the
               ;; UTF-16 rules are this port's own: a surrogate pair becomes
               ;; one character, and an unpaired surrogate escape becomes
               ;; that unit, which is what JSON.parse does and what makes an
               ;; encode and parse round trip of streamed text hold. A raw
               ;; control character is still refused.
               (unless (take #\") (invalid))
               (let ((out (make-string-output-stream)))
                 (loop
                   (let ((char (peek)))
                     (unless char (invalid))
                     (incf at)
                     (cond
                       ((char= char #\") (return (get-output-stream-string out)))
                       ((< (char-code char) 32) (invalid))
                       ((char= char #\\)
                        (let ((escape (peek)))
                          (unless escape (invalid))
                          (incf at)
                          (case escape
                            (#\" (write-char #\" out))
                            (#\\ (write-char #\\ out))
                            (#\/ (write-char #\/ out))
                            (#\b (write-char #\Backspace out))
                            (#\f (write-char #\Page out))
                            (#\n (write-char #\Newline out))
                            (#\r (write-char #\Return out))
                            (#\t (write-char #\Tab out))
                            (#\u
                             (let ((unit (hex-unit)))
                               (if (and (<= #xd800 unit #xdbff)
                                        (< (1+ at) size)
                                        (char= (char string at) #\\)
                                        (char= (char string (1+ at)) #\u))
                                   ;; A high surrogate followed by a low one
                                   ;; is a single character; a high surrogate
                                   ;; followed by anything else is itself.
                                   (let* ((mark at)
                                          (next (progn (incf at 2) (hex-unit))))
                                     (if (<= #xdc00 next #xdfff)
                                         (write-char
                                          (code-char (+ #x10000
                                                        (ash (- unit #xd800) 10)
                                                        (- next #xdc00)))
                                          out)
                                         (progn (setf at mark)
                                                (write-char (code-char unit) out))))
                                   (write-char (code-char unit) out))))
                            (t (invalid)))))
                       (t (write-char char out)))))))
             (number-value ()
               (let ((start at))
                 (loop while (and (peek) (find (peek) "0123456789-+.eE")) do (incf at))
                 (let ((token (subseq string start at)))
                   (unless (cl-ppcre:scan "\\A-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\\z" token)
                     (invalid))
                   (handler-case (%json-number-value token)
                     (ax-error () (invalid))))))
             (constant-value (spelling value)
               (unless (and (<= (+ at (length spelling)) size)
                            (string= spelling string :start2 at :end2 (+ at (length spelling))))
                 (invalid))
               (incf at (length spelling))
               value)
             (value ()
               (space)
               (case (peek)
                 (#\" (text-value))
                 (#\{ (incf at)
                       (space)
                       (let ((out (%new-object)))
                         (unless (take #\})
                           (loop
                             (let ((key (text-value)))
                               (space)
                               (unless (take #\:) (invalid))
                               (%set-key out key (value)))
                             (space)
                             (when (take #\}) (return))
                             (unless (take #\,) (invalid))
                             (space)))
                         out))
                 (#\[ (incf at)
                       (space)
                       (let ((out (%new-array)))
                         (unless (take #\])
                           (loop
                             (vector-push-extend (value) out)
                             (space)
                             (when (take #\]) (return))
                             (unless (take #\,) (invalid))))
                         out))
                 (#\t (constant-value "true" true))
                 (#\f (constant-value "false" false))
                 (#\n (constant-value "null" :null))
                 (otherwise (number-value)))))
      (let ((out (value)))
        (space)
        (unless (= at size) (invalid))
        out))))

;;; ------------------------------------------------------------------
;;; Encoding
;;; ------------------------------------------------------------------

(defun %write-json-string (value stream)
  (write-char #\" stream)
  (loop for char across value
        do (case char
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Backspace (write-string "\\b" stream))
             (#\Page (write-string "\\f" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              ;; Lower-case hex, as JSON.stringify and Python's encoder
              ;; both write it: "\u001b", not "\u001B".
              (cond ((< (char-code char) 32) (format stream "\\u~(~4,'0x~)" (char-code char)))
                    ;; A string can hold one half of a surrogate pair when a
                    ;; provider split the pair across two stream events. It
                    ;; has no UTF-8 encoding, so write it as the escape
                    ;; JSON.stringify writes (well-formed JSON.stringify,
                    ;; ES2019) rather than refusing the whole document.
                    ((<= #xd800 (char-code char) #xdfff)
                     (format stream "\\u~(~4,'0x~)" (char-code char)))
                    (t (write-char char stream))))))
  (write-char #\" stream))

(defun %write-json (value stream &key indent sort-keys (depth 0))
  "Write VALUE to STREAM as JSON.

INDENT is the number of spaces per level, as JSON.stringify's third
argument; NIL writes the compact form. SORT-KEYS writes object keys in
sorted order at every level instead of JavaScript's own-property order,
which is what a stable stringification needs."
  (flet ((newline (level)
           (when indent
             (write-char #\Newline stream)
             (dotimes (i (* indent level)) (write-char #\Space stream)))))
    (cond ((eq value :null) (write-string "null" stream))
          ((eq value 'yason:true) (write-string "true" stream))
          ((eq value 'yason:false) (write-string "false" stream))
          ((null value) (write-string "null" stream))
          ((%object-p value)
           (let ((keys (let ((keys (%object-keys value)))
                         (if sort-keys (sort (copy-list keys) #'string<) keys))))
             (if (null keys)
                 (write-string "{}" stream)
                 (progn
                   (write-char #\{ stream)
                   (loop for key in keys
                         for first = t then nil
                         do (unless first (write-char #\, stream))
                            (newline (1+ depth))
                            (%write-json-string key stream)
                            (write-char #\: stream)
                            (when indent (write-char #\Space stream))
                            (%write-json (gethash key value) stream
                                         :indent indent :sort-keys sort-keys
                                         :depth (1+ depth)))
                   (newline depth)
                   (write-char #\} stream)))))
          ((%array-p value)
           (if (zerop (length value))
               (write-string "[]" stream)
               (progn
                 (write-char #\[ stream)
                 (loop for item across value
                       for first = t then nil
                       do (unless first (write-char #\, stream))
                          (newline (1+ depth))
                          (%write-json item stream
                                       :indent indent :sort-keys sort-keys
                                       :depth (1+ depth)))
                 (newline depth)
                 (write-char #\] stream))))
          ((stringp value) (%write-json-string value stream))
          ((integerp value) (format stream "~D" value))
          ((realp value) (write-string (%json-number-text value) stream))
          (t (error 'ax-error
                    :message (format nil "encode-json: ~S is not a JSON value in this model" value))))))

(defun %float-nonfinite-p (number)
  "Whether NUMBER is a NaN or an infinity."
  (or (sb-ext:float-nan-p number) (sb-ext:float-infinity-p number)))

(defun %decimal-point (value)
  "The N with 10^(N-1) <= VALUE < 10^N, for an exact positive rational."
  (let ((point 0))
    (loop while (>= value (expt 10 point)) do (incf point))
    (loop while (< value (expt 10 (1- point))) do (decf point))
    point))

(defun %float-decimal-digits (number)
  "NUMBER's shortest decimal digits that read back as NUMBER, and where its
decimal point sits: the magnitude is 0.DIGITS x 10^POINT.

NUMBER must be a non-zero finite float. The digits are found from the
float's exact rational value and checked by reading each candidate back,
so the result is the shortest round-tripping decimal rather than whatever
this implementation's printer happens to emit. SBCL's printer is not
shortest for subnormals: it writes least-positive-double-float as
4.9406564584124654e-324 where the shortest round trip is 5e-324, the text
every other Ax port produces."
  (let* ((magnitude (abs number))
         (exact (rational magnitude))
         (point (%decimal-point exact)))
    (loop for count from 1 to 17
          do (let* ((scaled (round (* exact (expt 10 (- count point)))))
                    (carried (= scaled (expt 10 count)))
                    (digits (if carried 1 scaled))
                    (point (if carried (1+ point) point)))
               ;; A candidate can round up out of range near the largest
               ;; float, which is a rejected candidate rather than an error:
               ;; one digit of 1.7976931348623157e308 reads back as 2e308,
               ;; and asking for that float raises rather than trapping.
               (when (eql magnitude
                          (ignore-errors
                           (sb-int:with-float-traps-masked (:underflow :inexact)
                             (float (* digits (expt 10 (- point count))) magnitude))))
                 (let ((text (string-right-trim "0" (format nil "~D" digits))))
                   (return (values (if (plusp (length text)) text "0") point)))))
          finally (error 'ax-error
                         :message (format nil "~S has no decimal round trip" number)))))

(defun %js-number-text (value)
  "VALUE as JavaScript's String(number) writes it.

An exact Lisp integer keeps all its digits: this port never narrows one
silently, and the reference implementation cannot produce an integer it
would have to narrow. A float follows ECMAScript's Number::toString:
shortest round-tripping digits, plain decimal notation from 1e-6 up to
1e21, exponent notation outside that range (1e-7, 1e+21), \"0\" for both
zeros, and NaN, Infinity or -Infinity for the non-finite values."
  (when (integerp value)
    (return-from %js-number-text (format nil "~D" value)))
  (unless (floatp value)
    ;; A ratio can only arrive from Lisp code: Core arithmetic never makes
    ;; one. Read it as the double it would be in the reference semantics.
    (return-from %js-number-text (%js-number-text (float value 1d0))))
  (cond ((sb-ext:float-nan-p value) "NaN")
        ((sb-ext:float-infinity-p value) (if (plusp value) "Infinity" "-Infinity"))
        ((zerop value) "0")
        (t
         (multiple-value-bind (digits point) (%float-decimal-digits value)
           (let* ((count (length digits))
                  (text
                    (cond ((<= count point 21)
                           (concatenate 'string digits (make-string (- point count)
                                                                    :initial-element #\0)))
                          ((< 0 point 21)
                           (concatenate 'string (subseq digits 0 point) "."
                                        (subseq digits point)))
                          ((< -6 point 1)
                           (concatenate 'string "0."
                                        (make-string (- point) :initial-element #\0)
                                        digits))
                          (t
                           (let ((power (1- point)))
                             (concatenate 'string
                                          (subseq digits 0 1)
                                          (if (> count 1)
                                              (concatenate 'string "." (subseq digits 1))
                                              "")
                                          (if (minusp power) "e-" "e+")
                                          (format nil "~D" (abs power))))))))
             (if (minusp value) (concatenate 'string "-" text) text))))))

(defun %json-number-text (value)
  "VALUE as JSON number text: %JS-NUMBER-TEXT, with null for a non-finite
float, exactly as JSON.stringify writes numbers."
  (if (and (floatp value) (%float-nonfinite-p value))
      "null"
      (%js-number-text value)))

(defun encode-json (value &key indent sort-keys)
  "VALUE as a JSON string.

Encodes this package's value model: objects in JavaScript's own-property
order, YASON:TRUE and YASON:FALSE as true and false, and :NULL as null.
NIL also writes as null, but :NULL is the value to pass; an empty array
must be a vector.

INDENT is the number of spaces per nesting level, as JSON.stringify's
third argument. SORT-KEYS orders every object's keys by name instead,
which a cache key or other stable form needs.

Numbers: a float is written as JavaScript writes it, with a NaN or an
infinity becoming null as JSON.stringify does. A Lisp integer keeps every
digit, including beyond 2^53, rather than being narrowed to a double
behind the caller's back. That is the one deliberate difference from the
reference implementation, which has no exact integers and so can never
produce a value this has to decide about; a caller that wants the
reference projection should convert with AXLLM/CORE::CORE-JS-NUMBER
first."
  (with-output-to-string (stream)
    (%write-json value stream :indent indent :sort-keys sort-keys)))
