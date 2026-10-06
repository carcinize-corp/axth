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

(defun %object-keys (object)
  "OBJECT's STRING keys: the recorded order first, then the rest sorted.

Sorting the remainder keeps an object built without OBJECT deterministic
instead of exposing hash order. Non-string keys are internal record
metadata and are never returned."
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
        (append keys (sort extra #'string<))))))

(defun %set-key (object key value)
  "Set OBJECT's KEY to VALUE, recording KEY's position on first write."
  (%record-key object key)
  (setf (gethash key object) value))

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
             (text-value ()
               (let ((start at))
                 (unless (take #\") (invalid))
                 (loop for char = (peek)
                       do (unless char (invalid))
                          (incf at)
                       until (char= char #\")
                       do (cond ((< (char-code char) 32) (invalid))
                                ((char= char #\\)
                                 (let ((escape (peek)))
                                   (unless (and escape (find escape "\"\\/bfnrtu")) (invalid))
                                   (incf at)
                                   (when (char= escape #\u)
                                     (dotimes (i 4)
                                       (unless (and (peek) (digit-char-p (peek) 16)) (invalid))
                                       (incf at)))))))
                 (let ((text (handler-case (yason:parse (subseq string start at))
                               (error () (invalid)))))
                   ;; Lone surrogates cannot be encoded as UTF-8.
                   (when (find-if (lambda (c) (<= #xd800 (char-code c) #xdfff)) text)
                     (invalid))
                   text)))
             (number-value ()
               (let ((start at))
                 (loop while (and (peek) (find (peek) "0123456789-+.eE")) do (incf at))
                 (let ((token (subseq string start at)))
                   (unless (cl-ppcre:scan "\\A-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\\z" token)
                     (invalid))
                   (handler-case (read-from-string token)
                     (error () (invalid))))))
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
              (cond ((< (char-code char) 32) (format stream "\\u~4,'0X" (char-code char)))
                    ((<= #xd800 (char-code char) #xdfff)
                     (error 'ax-error :message "encode-json: lone Unicode surrogate"))
                    (t (write-char char stream))))))
  (write-char #\" stream))

(defun %write-json (value stream)
  (cond ((eq value :null) (write-string "null" stream))
        ((eq value 'yason:true) (write-string "true" stream))
        ((eq value 'yason:false) (write-string "false" stream))
        ((null value) (write-string "null" stream))
        ((%object-p value)
         (write-char #\{ stream)
         (loop for key in (%object-keys value)
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (%write-json-string key stream)
                  (write-char #\: stream)
                  (%write-json (gethash key value) stream))
         (write-char #\} stream))
        ((%array-p value)
         (write-char #\[ stream)
         (loop for item across value
               for first = t then nil
               do (unless first (write-char #\, stream))
                  (%write-json item stream))
         (write-char #\] stream))
        ((stringp value) (%write-json-string value stream))
        ((integerp value) (format stream "~D" value))
        ((realp value) (write-string (%json-number-text value) stream))
        (t (error 'ax-error
                  :message (format nil "encode-json: ~S is not a JSON value in this model" value)))))

(defun %json-number-text (value)
  "VALUE as JSON number text, matching how the other Ax ports write it.

An integral value keeps integer form, so 18.0 writes as 18 and not 18.0.
Magnitudes at or beyond 1e21, and below 1e-6, fall back to Lisp exponent
notation normalised to e; Ax's signature constraints do not reach there."
  (let ((number (if (floatp value) value (float value 1d0))))
    (cond ((/= number number) (error 'ax-error :message "encode-json: NaN is not a JSON number"))
          ((or (> number most-positive-double-float) (< number most-negative-double-float))
           (error 'ax-error :message "encode-json: infinity is not a JSON number"))
          ((and (= number (fround number)) (< (abs number) 1d21))
           (format nil "~D" (round number)))
          (t (let* ((*read-default-float-format* (type-of number))
                    (text (prin1-to-string number)))
               (substitute #\e #\d text))))))

(defun encode-json (value)
  "VALUE as a JSON string.

Encodes this package's value model: objects in key order, YASON:TRUE and
YASON:FALSE as true and false, and :NULL as null. NIL also writes as null,
but :NULL is the value to pass; an empty array must be a vector."
  (with-output-to-string (stream)
    (%write-json value stream)))
