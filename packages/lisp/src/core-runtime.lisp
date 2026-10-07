;;;; core-runtime.lisp --- the native boundaries generated Core code calls.
;;;;
;;;; src/core.lisp is generated from ir/axcore and holds Ax's semantics. It
;;;; calls out to this file for everything a portable IR cannot express:
;;;; value tests, arithmetic, strings, UTF-16, collections, JSON, the record
;;;; constructors, the media and URL shape tests, digests, time zones, the
;;;; host regular expression operations, and the error constructors.
;;;;
;;;; Every function is a boundary, not a reimplementation of behavior. Where
;;;; a primitive has an observable rule, that rule comes from Ax's reference
;;;; semantics (JavaScript's string, number and truthiness behavior) and is
;;;; stated in the docstring, so a reader can check it against the other
;;;; ports instead of guessing.
;;;;
;;;; What is deliberately NOT here. Core owns its own ECMAScript matcher
;;;; (ir/axcore/regex.axir) for raw tool-argument schemas, and reaches it
;;;; through intrinsic.string.utf16_units below; the two host regular
;;;; expression operations near the end of this file are the unrelated
;;;; native ones, and must not be mistaken for that matcher. Core also owns
;;;; all date parsing and arithmetic: the only date boundaries are a zone
;;;; offset and recognising a host date value.
;;;;
;;;; Effects are confined to five places, and each is visible at its own
;;;; name: CORE-MATH-RANDOM (injectable), CORE-DATE-ZONE-OFFSET and
;;;; CORE-DATE-MILLIS (the platform zone database), CORE-CRYPTO-SHA256-HEX
;;;; (a digest library), and CORE-COVERAGE-MARK (a file, only when
;;;; AXIR_COVERAGE_FILE is set). Everything else is a pure function of its
;;;; arguments, apart from the mutation Core's own map and list operations
;;;; are defined to perform.
;;;;
;;;; Tests for this file are packages/lisp/tests/core-primitives.lisp.

(in-package #:axllm/core)

;;; ------------------------------------------------------------------
;;; Value model helpers
;;; ------------------------------------------------------------------

(declaim (inline core-bool core-object-p core-array-p))

(defun core-bool (generalized)
  "GENERALIZED as a Core boolean: YASON:TRUE or YASON:FALSE."
  (if generalized 'yason:true 'yason:false))

(defun core-object-p (value)
  (hash-table-p value))

(defun core-array-p (value)
  (and (vectorp value) (not (stringp value))))

(defun core-record-kind (value)
  "VALUE's Ax record name, or NIL when it is a plain JSON object.

Records are JSON objects carrying one extra entry under the keyword key
:RECORD. A keyword can never collide with a JSON string key, and the JSON
writer only emits string keys, so the marker stays internal."
  (when (hash-table-p value)
    (gethash :record value)))

(defun core-field-p (value)
  (equal (core-record-kind value) "Field"))

(defun core-json-value-p (value)
  "Whether VALUE belongs to the JSON model generated Core code works in.

Everything else is a host object: a provider client, a stream handle, a
tool callable, a timestamp. NIL is not in the model, so it is not a JSON
value here."
  (or (eq value :null)
      (eq value 'yason:true)
      (eq value 'yason:false)
      (stringp value)
      (realp value)
      (core-array-p value)
      (hash-table-p value)))

;;; ------------------------------------------------------------------
;;; Host objects
;;; ------------------------------------------------------------------

;;; Core reaches a host object through exactly three operations, so a
;;; subsystem adds a new kind of host object by adding methods and nothing
;;; else. CORE-GET and CORE-SET delegate here for a target outside the JSON
;;; model, and CORE-OBJECT-CALL-METHOD is the Core-visible name of
;;; CORE-HOST-CALL.
;;;
;;; These are the only host-object protocol in this port: there is no
;;; public wrapper layer over them, and a caller that needs one of these
;;; operations specialises the generic function itself.

(defgeneric core-host-get (target key &optional fallback)
  (:documentation
   "TARGET's host-object value at KEY, or FALLBACK when it has none.

A read is allowed to come up empty: Core asks objects for keys they may
not carry, and every port answers such a read with the fallback rather
than failing. KEY is a string.")
  (:method (target key &optional (fallback :null))
    (declare (ignore target key))
    fallback))

(defgeneric core-host-set (target key value)
  (:documentation
   "Store VALUE under KEY in host object TARGET, and return TARGET.

A host object that cannot take a write says so: silently dropping it would
let Core believe state was recorded.")
  (:method (target key value)
    (declare (ignore value))
    (%ax-error "core.set: ~S (~S) has no settable key ~a"
               target (type-of target) key)))

(defgeneric core-host-call (target method args)
  (:documentation
   "Call METHOD, a string, on host object TARGET with ARGS, a vector.

An unknown method is an error naming the target's type and the method. A
host boundary that answers every call with a placeholder success is worse
than one that fails: Core would carry the placeholder forward as a
result.")
  (:method (target method args)
    (declare (ignore args))
    (%ax-error "intrinsic.object.call_method: ~S (~S) has no method ~a"
               target (type-of target) method)))

(defmethod core-host-call ((target function) method args)
  ;; Core's native callbacks are ordinary functions, not method dispatchers.
  ;; Keep the allowed operations explicit so a misspelled method still fails.
  (if (member method '("call" "format_result") :test #'string=)
      (apply target (coerce args 'list))
      (call-next-method)))

(defun core-object-call-method (target method &rest args)
  "Core's method call on a host object: see CORE-HOST-CALL."
  (core-host-call target (core-js-text method) (coerce args 'vector)))

(defun %ax-error (format-control &rest arguments)
  (error 'axllm:ax-error :message (apply #'format nil format-control arguments)))

(defun %signature-error (format-control &rest arguments)
  (error 'axllm:signature-error :message (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; Truthiness, logic and comparison
;;; ------------------------------------------------------------------

(defun core-true-p (value)
  "VALUE's truthiness as a Lisp generalized boolean.

Matches the other ports: null, false, the empty string, zero, the empty
array and the empty object are false; everything else is true. Used by
generated conditionals; CORE-TRUTHY is the Core-visible version."
  (cond ((eq value :null) nil)
        ((eq value 'yason:false) nil)
        ((eq value 'yason:true) t)
        ((null value) nil)
        ((stringp value) (plusp (length value)))
        ((realp value) (/= value 0))
        ((core-array-p value) (plusp (length value)))
        ((hash-table-p value) (plusp (hash-table-count value)))
        ((consp value) t)
        (t t)))

(defun core-truthy (value)
  "VALUE's truthiness as a Core boolean."
  (core-bool (core-true-p value)))

(defun core-not (value)
  (core-bool (not (core-true-p value))))

(defun core-and (left right)
  (core-bool (and (core-true-p left) (core-true-p right))))

(defun core-or (left right)
  (core-bool (or (core-true-p left) (core-true-p right))))

(defun core-value-equal (left right)
  "Whether LEFT and RIGHT are the same Core value.

Strings compare by content, numbers by value, arrays and objects
element-wise; everything else by identity. Unlike the Python port this does
not treat 0 and false as equal, because that is a Python artifact rather
than Ax behavior."
  (cond ((and (stringp left) (stringp right)) (string= left right))
        ((and (realp left) (realp right)) (= left right))
        ((and (core-array-p left) (core-array-p right))
         (and (= (length left) (length right))
              (every #'core-value-equal left right)))
        ((and (hash-table-p left) (hash-table-p right))
         (let ((left-keys (axllm::%object-keys left))
               (right-keys (axllm::%object-keys right)))
           (and (= (length left-keys) (length right-keys))
                (every (lambda (key)
                         (and (nth-value 1 (gethash key right))
                              (core-value-equal (gethash key left) (gethash key right))))
                       left-keys))))
        (t (eql left right))))

(defun core-eq (left right)
  (core-bool (core-value-equal left right)))

(defun core-ne (left right)
  (core-bool (not (core-value-equal left right))))

(defun core-lt (left right)
  (cond ((and (realp left) (realp right)) (core-bool (< left right)))
        ((and (stringp left) (stringp right)) (core-bool (string< left right)))
        (t (%ax-error "intrinsic.lt: cannot order ~S and ~S" left right))))

(defun core-gt (left right)
  (cond ((and (realp left) (realp right)) (core-bool (> left right)))
        ((and (stringp left) (stringp right)) (core-bool (string> left right)))
        (t (%ax-error "intrinsic.gt: cannot order ~S and ~S" left right))))

(defun core-lte (left right)
  (cond ((and (realp left) (realp right)) (core-bool (<= left right)))
        ((and (stringp left) (stringp right)) (core-bool (string<= left right)))
        (t (%ax-error "intrinsic.lte: cannot order ~S and ~S" left right))))

(defun core-gte (left right)
  (cond ((and (realp left) (realp right)) (core-bool (>= left right)))
        ((and (stringp left) (stringp right)) (core-bool (string>= left right)))
        (t (%ax-error "intrinsic.gte: cannot order ~S and ~S" left right))))

(defun core-add (left right)
  "Numeric addition, or string concatenation when either side is a string.

A string on one side concatenates the text of both, as + does in the
reference semantics."
  (cond ((and (realp left) (realp right)) (+ left right))
        ((or (stringp left) (stringp right))
         (concatenate 'string (core-js-text left) (core-js-text right)))
        (t (%ax-error "intrinsic.add: cannot add ~S and ~S" left right))))

;;; ------------------------------------------------------------------
;;; Arithmetic and math
;;; ------------------------------------------------------------------

;;; Two rules hold across this section, because Core's numbers are
;;; JavaScript's.
;;;
;;; A result that can be exact stays exact: adding, multiplying or taking
;;; the absolute value of integers gives an integer, which indexes a vector
;;; and renders identically to the double the reference implementation
;;; would hold.
;;;
;;; Anything that cannot be exact is a double, never a ratio. Lisp would
;;; answer (/ 1 3) with 1/3, a value no other port can hold and no JSON
;;; document can carry, so every division and every transcendental
;;; function converts first.

(defun %core-number (value context)
  "VALUE as a number for arithmetic, or an error naming CONTEXT."
  (cond ((realp value) value)
        ((eq value :null) 0)
        (t (%ax-error "~a: ~S is not a number" context value))))

(defun %core-double (value context)
  (float (%core-number value context) 1d0))

(defun core-js-number (value)
  "VALUE as the double the reference implementation would hold.

This is the one place where an exact Lisp integer is deliberately narrowed
to binary64, for a caller that wants the reference projection of a value
Lisp can represent and JavaScript cannot."
  (if (floatp value)
      value
      (sb-int:with-float-traps-masked (:underflow :inexact)
        (float (%core-number value "core.js_number") 1d0))))

(defun core-mul (left right)
  (let ((left (%core-number left "intrinsic.mul"))
        (right (%core-number right "intrinsic.mul")))
    (* left right)))

(defun core-div (left right)
  "LEFT divided by RIGHT as a double; a zero divisor divides by one.

The zero rule is the other ports': a divisor of zero becomes one rather
than producing an infinity Core has no use for."
  (let ((left (%core-double left "intrinsic.div"))
        (right (%core-double right "intrinsic.div")))
    (/ left (if (zerop right) 1d0 right))))

(defun core-math-abs (value)
  (abs (%core-number value "intrinsic.math.abs")))

(defun core-math-floor (value)
  (floor (%core-number value "intrinsic.math.floor")))

(defun core-math-is-finite (value)
  (core-bool (and (realp value)
                  (or (not (floatp value)) (not (axllm::%float-nonfinite-p value))))))

(defparameter +double-nan+ (sb-kernel:make-double-float #x7FF80000 0)
  "The quiet NaN. Math on a value outside a function's domain produces it,
as it does in the reference semantics, instead of signalling.")

(defmacro %js-math (&body body)
  "BODY's value as JavaScript's Math would report it.

An overflow becomes an infinity rather than an error, an underflow becomes
zero, and a result outside the reals becomes NaN, because Lisp answers
(sqrt -1) with a complex number where JavaScript answers NaN."
  `(sb-int:with-float-traps-masked (:overflow :underflow :inexact :invalid :divide-by-zero)
     (let ((result (progn ,@body)))
       (if (complexp result) +double-nan+ result))))

(defun core-math-log (value)
  (let ((value (%core-double value "intrinsic.math.log")))
    (cond ((zerop value) sb-ext:double-float-negative-infinity)
          ((minusp value) +double-nan+)
          (t (%js-math (log value))))))

(defun core-math-exp (value)
  (%js-math (exp (%core-double value "intrinsic.math.exp"))))

(defun core-math-sqrt (value)
  (let ((value (%core-double value "intrinsic.math.sqrt")))
    (if (minusp value) +double-nan+ (%js-math (sqrt value)))))

(defun core-math-cos (value)
  (%js-math (cos (%core-double value "intrinsic.math.cos"))))

(defun core-math-pow (left right)
  (%js-math (expt (%core-double left "intrinsic.math.pow")
                  (%core-double right "intrinsic.math.pow"))))

(defvar *math-random-values* '()
  "Doubles CORE-MATH-RANDOM hands out before falling back to the generator.

Randomness is the one effect in this file, so it is injectable: a test
that needs a fixed draw sets this rather than reaching into Core.")

(defvar *math-random-lock* (sb-thread:make-mutex :name "ax-math-random"))

(defvar *math-random-state* (make-random-state t))

(defun set-math-random-values (values)
  "Make CORE-MATH-RANDOM return VALUES, in order, before drawing again."
  (sb-thread:with-mutex (*math-random-lock*)
    (setf *math-random-values* (mapcar (lambda (value) (float value 1d0))
                                       (coerce values 'list))))
  values)

(defun core-math-random ()
  "The next injected value, else a draw in [0, 1)."
  (sb-thread:with-mutex (*math-random-lock*)
    (if *math-random-values*
        (pop *math-random-values*)
        (random 1d0 *math-random-state*))))

(defun core-none ()
  :null)

(defun core-is-none (value)
  (core-bool (eq value :null)))

(defun core-is-not-none (value)
  (core-bool (not (eq value :null))))

(defun core-coalesce (value fallback)
  (if (eq value :null) fallback value))

;;; ------------------------------------------------------------------
;;; Collections
;;; ------------------------------------------------------------------

(defun core-new-map ()
  (axllm::%new-object))

(defun core-new-list ()
  (axllm::%new-array))

(defparameter +record-key-aliases+
  '(("is_array" . "isArray")
    ("is_optional" . "isOptional")
    ("is_internal" . "isInternal")
    ("is_cached" . "isCached")
    ("min_length" . "minLength")
    ("max_length" . "maxLength")
    ("value_descriptions" . "valueDescriptions")
    ("pattern_description" . "patternDescription")
    ("input_fields" . "inputs")
    ("output_fields" . "outputs"))
  "The record attribute names Core reads under a second spelling.

Ax's IR reads a record's snake_case keys, while the attribute maps a
caller, a fixture or the fluent builder writes use Ax's TypeScript
spelling. These ten names are where the two meet.

The table is used in exactly one place: reading the attribute map a record
constructor was handed. It is deliberately not part of CORE-GET. Core reads
a provider's wire objects with the same CORE-GET, and those carry both
spellings as distinct keys, so a read that silently tried the other
spelling would answer with the wrong one. provider.axir reads mime_type
then mimeType, file_uri then fileUri, and strict_structured_outputs then
strictStructuredOutputs, each in a stated order, and a plain JSON object
holding minLength is not a record whose min_length anybody asked for.")

(defun core-key-alias (key)
"KEY's other spelling as a record attribute name, or NIL."
(when (stringp key)
  (or (cdr (assoc key +record-key-aliases+ :test #'string=))
      (car (rassoc key +record-key-aliases+ :test #'string=)))))

(defun core-get (target key &optional (fallback :null))
"TARGET's value at KEY, or FALLBACK when it is absent.

An absent key yields FALLBACK; a key present with a null value yields that
null. The two are different in Core, so they stay different here. A target
outside the JSON model is a host object and answers through
CORE-HOST-GET.

Object keys use Core's string representation, including numeric capture
indices. Nothing here tries a second spelling: see
+RECORD-KEY-ALIASES+ for where the two spellings of a record attribute
meet, and why that is not this function."
(cond ((hash-table-p target)
       (multiple-value-bind (value found) (gethash (core-js-text key) target)
         (if found value fallback)))
        ((core-array-p target)
         (if (and (integerp key) (< -1 key (length target))) (aref target key) fallback))
        ((stringp target)
         (if (and (integerp key) (< -1 key (length target)))
             (string (char target key))
             fallback))
        ((core-json-value-p target) fallback)
        (t (core-host-get target (core-js-text key) fallback))))

(defun core-set (target key value)
  (cond ((hash-table-p target)
         (axllm::%set-key target (core-js-text key) value)
         target)
        ((core-json-value-p target)
         (%ax-error "core.set: ~S is not an object" target))
        (t (core-host-set target (core-js-text key) value))))

(defun core-append (target value)
  (unless (and (core-array-p target) (array-has-fill-pointer-p target))
    (%ax-error "core.append: ~S is not an extensible array" target))
  (vector-push-extend value target)
  target)

(defun core-elements (value)
  "VALUE's elements as a Lisp list, for generated iteration.

An object iterates over its keys, in key order, as the other ports do."
  (cond ((core-array-p value) (coerce value 'list))
        ((hash-table-p value) (axllm::%object-keys value))
        ((listp value) value)
        ((eq value :null) '())
        (t (%ax-error "core.for: ~S is not iterable" value))))

(defun core-len (value)
  "VALUE's length: characters, elements, or keys. Null is empty.

An object counts only its string keys, so a record's internal marker is
not part of its size."
  (cond ((stringp value) (length value))
        ((core-array-p value) (length value))
        ((hash-table-p value) (length (axllm::%object-keys value)))
        ((eq value :null) 0)
        ((listp value) (length value))
        (t (%ax-error "intrinsic.len: ~S has no length" value))))

(defun core-contains (container item)
  "Whether CONTAINER holds ITEM: a substring, an element, or a key."
  (core-bool
   (cond ((eq container :null) nil)
         ((stringp container)
          (and (stringp item) (search item container)))
         ((core-array-p container) (find item container :test #'core-value-equal))
         ((hash-table-p container) (nth-value 1 (gethash (core-js-text item) container)))
         ((consp container) (member item container :test #'core-value-equal))
         (t nil))))

(defun core-list-get (values index &optional (default :null))
  (if (and (core-array-p values) (integerp index) (< -1 index (length values)))
      (aref values index)
      default))

(defun core-map-contains (values key)
  "Whether VALUES has KEY under Core's string representation."
  (core-bool (and (hash-table-p values) (nth-value 1 (gethash (core-js-text key) values)))))

(defun core-map-get (values key)
  "VALUES at KEY, or null. CORE-GET with Core's map spelling."
  (core-get values key))

(defun core-map-delete (target key)
  "TARGET without KEY, mutating it, and TARGET.

The key's recorded position goes with it, so writing the key again puts it
at the end rather than back where it was."
  (when (hash-table-p target)
    (axllm::%delete-key target (core-js-text key)))
  target)

(defun core-map-keys (values)
  (let ((out (core-new-list)))
    (when (hash-table-p values)
      (dolist (key (axllm::%object-keys values))
        (vector-push-extend key out)))
    out))

(defun core-map-values (values)
  (let ((out (core-new-list)))
    (cond ((hash-table-p values)
           (dolist (key (axllm::%object-keys values))
             (vector-push-extend (gethash key values) out)))
          ((core-array-p values)
           (loop for item across values do (vector-push-extend item out))))
    out))

(defun core-sorted-strings (values)
  "VALUES as text, sorted.

Ordering is by code point, which is Lisp's STRING<. The reference
implementation sorts by UTF-16 code unit instead, so the two differ only
between an astral character and one in U+E000 through U+FFFF."
  (let ((out (core-new-list)))
    (dolist (item (sort (mapcar #'core-js-text (core-elements values)) #'string<))
      (vector-push-extend item out))
    out))

(defun core-map-merge (left right)
  (let ((out (core-new-map)))
    (when (hash-table-p left)
      (dolist (key (axllm::%object-keys left))
        (core-set out key (gethash key left))))
    (when (hash-table-p right)
      (dolist (key (axllm::%object-keys right))
        (core-set out key (gethash key right))))
    out))

(defun core-map-update (target values)
  (unless (hash-table-p target)
    (%ax-error "intrinsic.map.update: ~S is not an object" target))
  (when (hash-table-p values)
    (dolist (key (axllm::%object-keys values))
      (core-set target key (gethash key values))))
  target)

;;; ------------------------------------------------------------------
;;; Text rendering shared with the other ports
;;; ------------------------------------------------------------------

(defun core-js-number-text (value)
  "VALUE as JavaScript's String(number) writes it.

An integral value keeps integer form, so a minimum of 18 renders as 18;
magnitudes at or beyond 1e21 and below 1e-6 switch to exponent notation,
as 1e+21 and 1e-7; a NaN or an infinity is spelled out. See
AXLLM::%JS-NUMBER-TEXT for the rules."
  (if (realp value)
      (axllm::%js-number-text value)
      (princ-to-string value)))

(defun core-js-text (value)
  "VALUE's text in string.format, as every Ax port writes it.

A string is used as is, null is \"null\", a boolean is \"true\" or
\"false\", a number follows JavaScript's String(number), and an array or
object is compact JSON."
  (cond ((stringp value) value)
        ((eq value :null) "null")
        ((null value) "null")
        ((eq value 'yason:true) "true")
        ((eq value 'yason:false) "false")
        ((realp value) (core-js-number-text value))
        ;; An array or an object is compact JSON, and JSON.stringify asks a
        ;; date for its toJSON, so a date nested anywhere inside reads as
        ;; its ISO instant rather than failing the whole rendering.
        ((or (core-array-p value) (hash-table-p value))
         (axllm:encode-json (core-date-json value)))
        (t (princ-to-string value))))

(defun core-string-format (template &rest arguments)
  "TEMPLATE with each {} replaced by the next argument's text.

{{ and }} write one brace, any other brace is kept, and a {} past the last
argument is left as {}. An argument is inserted as is and never read as a
template itself."
  (let ((text (core-js-text template))
        (next arguments))
    (with-output-to-string (out)
      (loop with index = 0
            with limit = (length text)
            while (< index limit)
            do (let ((pair (when (< (1+ index) limit) (subseq text index (+ index 2)))))
                 (cond ((equal pair "{{") (write-char #\{ out) (incf index 2))
                       ((equal pair "}}") (write-char #\} out) (incf index 2))
                       ((equal pair "{}")
                        (if next
                            (progn (write-string (core-js-text (pop next)) out))
                            (write-string "{}" out))
                        (incf index 2))
                       (t (write-char (char text index) out) (incf index))))))))

;;; ------------------------------------------------------------------
;;; Strings
;;; ------------------------------------------------------------------

(defparameter +js-whitespace+
  (coerce (list #\Tab #\Newline (code-char 11) (code-char 12) #\Return #\Space
                (code-char #x00a0) (code-char #x1680)
                (code-char #x2000) (code-char #x2001) (code-char #x2002) (code-char #x2003)
                (code-char #x2004) (code-char #x2005) (code-char #x2006) (code-char #x2007)
                (code-char #x2008) (code-char #x2009) (code-char #x200a)
                (code-char #x2028) (code-char #x2029) (code-char #x202f)
                (code-char #x205f) (code-char #x3000) (code-char #xfeff))
          'string)
  "The characters JavaScript's String.prototype.trim removes.

Ax trims signature text with JavaScript's rule, which is not Lisp's
default whitespace set: it includes U+00A0, U+FEFF and the Unicode space
separators, and excludes U+001C through U+001F.")

(defun core-string-trim (value)
  (string-trim +js-whitespace+ (core-js-text value)))

(defun core-string-join (separator values)
  (let ((separator (core-js-text separator)))
    (with-output-to-string (out)
      (loop for item in (core-elements values)
            for first = t then nil
            do (unless first (write-string separator out))
               (write-string (core-js-text item) out)))))

(defun core-string-starts-with (value prefix)
  (let ((prefix (core-js-text prefix)))
    (core-bool (and (stringp value)
                    (<= (length prefix) (length value))
                    (string= prefix value :end2 (length prefix))))))

(defun core-string-ends-with (value suffix)
  (let ((text (core-js-text value))
        (suffix (core-js-text suffix)))
    (core-bool (and (<= (length suffix) (length text))
                    (string= suffix text :start2 (- (length text) (length suffix)))))))

(defun core-string-lower (value)
  (string-downcase (core-js-text value)))

(defun core-string-str (value)
  "VALUE as String(value) writes it: CORE-JS-TEXT under Core's name."
  (core-js-text value))

(defun core-string-index-of (value needle &optional (start 0))
  "The index of NEEDLE in VALUE at or after START, or -1.

Indices count code points, as they do in the Python port. The reference
implementation counts UTF-16 units, so the two differ only in a string
holding an astral character before the match."
  (let* ((text (core-js-text value))
         (needle (core-js-text needle))
         (from (max 0 (if (integerp start) start (round start))))
         (found (and (<= from (length text)) (search needle text :start2 from))))
    (or found -1)))

(defun core-string-default-if-empty (value fallback)
  "VALUE trimmed, or FALLBACK when nothing is left."
  (let ((text (core-string-trim value)))
    (if (zerop (length text)) fallback text)))

(defun core-string-remove-suffix (value suffix)
  "An object with value and removed: VALUE without SUFFIX, if it had it."
  (let ((text (core-js-text value))
        (suffix (core-js-text suffix)))
    (if (and (plusp (length suffix))
             (<= (length suffix) (length text))
             (string= suffix text :start2 (- (length text) (length suffix))))
        (axllm:object "value" (subseq text 0 (- (length text) (length suffix)))
                      "removed" 'yason:true)
        (axllm:object "value" text "removed" 'yason:false))))

(defun core-string-split (value separator)
  "VALUE split on every occurrence of SEPARATOR, keeping empty pieces.

An empty separator splits into single characters, as String.prototype.split
does; it splits code points rather than UTF-16 units, so an astral
character stays whole."
  (let ((text (core-js-text value))
        (separator (core-js-text separator))
        (out (core-new-list)))
    (if (zerop (length separator))
        (loop for character across text
              do (vector-push-extend (string character) out))
        (dolist (part (%split-literal text separator))
          (vector-push-extend part out)))
    out))

(defun core-string-split-outside-quotes (text separator)
  "TEXT split on SEPARATOR outside quoted spans, trimmed, empties dropped.

SEPARATOR is a single character here, the same as in the other ports. An
unterminated quote is a signature error."
  (let ((text (core-js-text text))
        (separator (core-js-text separator))
        (items (core-new-list))
        (current (make-string-output-stream))
        (quote-char nil)
        (escaped nil))
    (flet ((flush ()
             (let ((item (string-trim +js-whitespace+ (get-output-stream-string current))))
               (when (plusp (length item))
                 (vector-push-extend item items)))))
      (loop for character across text
            do (cond (escaped
                      (write-char character current)
                      (setf escaped nil))
                     ((char= character #\\)
                      (write-char character current)
                      (setf escaped t))
                     (quote-char
                      (write-char character current)
                      (when (char= character quote-char) (setf quote-char nil)))
                     ((or (char= character #\') (char= character #\"))
                      (write-char character current)
                      (setf quote-char character))
                     ((and (= 1 (length separator)) (char= character (char separator 0)))
                      (flush))
                     (t (write-char character current))))
      (when quote-char (%signature-error "Unterminated string"))
      (flush))
    items))

(defun core-string-lower-camel (words)
  "WORDS joined as one lowerCamelCase name.

Each word is lowercased; every word after the first takes an initial
capital. An empty word is dropped."
  (let ((items (remove-if (lambda (word) (zerop (length word)))
                          (mapcar #'core-js-text (core-elements words)))))
    (if (null items)
        ""
        (apply #'concatenate 'string
               (string-downcase (first items))
               (mapcar (lambda (word)
                         (let ((lower (string-downcase word)))
                           (concatenate 'string
                                        (string (char-upcase (char lower 0)))
                                        (subseq lower 1))))
                       (rest items))))))

(defun core-string-title-from-camel (value)
  "VALUE as a title: words split at camel-case boundaries, first capital.

This is one boundary for two reference functions, because the IR gives
them one intrinsic name. The agent runtime's titleFromFieldName turns a
trailing Code into its own word, so pythonCode titles as \"Python Code\";
the flow renderer's titleForNode also reads an underscore as a word break,
and lowercases by calling Core's string.lower between two calls of this.
Doing both here satisfies each: this never lowercases on its own, so the
flow path keeps control of that step."
  (let ((text (core-js-text value)))
    (setf text (substitute #\Space #\_ text))
    (let ((tail (search "Code" text :from-end t)))
      (when (and tail (= tail (- (length text) 4)) (plusp tail))
        (setf text (concatenate 'string (subseq text 0 tail) " Code"))))
    (setf text (cl-ppcre:regex-replace-all "([a-z0-9])([A-Z])" text "\\1 \\2"))
    (setf text (string-trim +js-whitespace+ text))
    (if (plusp (length text))
        (concatenate 'string (string (char-upcase (char text 0))) (subseq text 1))
        text)))

(defun %slice-index (index limit)
  "INDEX as a slice bound into a sequence of LIMIT elements.

Negative counts from the end and out-of-range clamps, as both JavaScript's
slice and Python's do."
  (let ((index (if (integerp index) index (round index))))
    (cond ((minusp index) (max 0 (+ limit index)))
          (t (min index limit)))))

(defun core-string-slice (value start &optional (end :null))
  (let* ((text (core-js-text value))
         (limit (length text))
         (from (%slice-index start limit))
         (to (if (eq end :null) limit (%slice-index end limit))))
    (if (< from to) (subseq text from to) "")))

(defun core-string-replace (value old new)
  "VALUE with every occurrence of OLD replaced by NEW, literally."
  (let ((text (core-js-text value))
        (old (core-js-text old))
        (new (core-js-text new)))
    (if (zerop (length old))
        text
        (with-output-to-string (out)
          (loop with index = 0
                for found = (search old text :start2 index)
                while found
                do (write-string text out :start index :end found)
                   (write-string new out)
                   (setf index (+ found (length old)))
                finally (write-string text out :start index))))))

(defun core-string-words (value)
  "VALUE split on runs of whitespace, with no empty pieces."
  (let ((out (core-new-list)))
    (dolist (part (cl-ppcre:split "\\s+" (core-js-text value)))
      (when (plusp (length part))
        (vector-push-extend part out)))
    out))

(defun core-string-split-trim-nonempty (value separator)
  "VALUE split on SEPARATOR, each piece trimmed, empty pieces dropped."
  (let ((out (core-new-list))
        (separator (core-js-text separator)))
    (dolist (part (%split-literal (core-js-text value) separator))
      (let ((trimmed (string-trim +js-whitespace+ part)))
        (when (plusp (length trimmed))
          (vector-push-extend trimmed out))))
    out))

(defun %split-literal (text separator)
  "TEXT split on every literal occurrence of SEPARATOR."
  (if (zerop (length separator))
      (list text)
      (let ((parts '()) (index 0))
        (loop for found = (search separator text :start2 index)
              while found
              do (push (subseq text index found) parts)
                 (setf index (+ found (length separator))))
        (push (subseq text index) parts)
        (nreverse parts))))

(defun core-string-split-once (value separator)
  "An object with left, right and found, splitting VALUE at SEPARATOR once."
  (let* ((text (core-js-text value))
         (separator (core-js-text separator))
         (found (and (plusp (length separator)) (search separator text))))
    (if found
        (axllm:object "left" (subseq text 0 found)
                      "right" (subseq text (+ found (length separator)))
                      "found" 'yason:true)
        (axllm:object "left" text "right" "" "found" 'yason:false))))

(defun core-string-split-top-level (text separator)
  "TEXT split on SEPARATOR, ignoring separators inside quotes or brackets.

Quoted spans, backslash escapes, parentheses and braces all nest, so a
signature's field list splits on its own commas and not on a comma inside
an object type or a quoted description. Each piece is trimmed. An
unterminated quote is a signature error, as in every port."
  (let ((text (core-js-text text))
        (separator (core-js-text separator))
        (items (core-new-list))
        (current (make-string-output-stream))
        (quote-char nil)
        (escaped nil)
        (paren-depth 0)
        (brace-depth 0)
        (index 0))
    (loop with limit = (length text)
          while (< index limit)
          do (let ((character (char text index)))
               (cond (escaped
                      (write-char character current)
                      (setf escaped nil)
                      (incf index))
                     ((char= character #\\)
                      (write-char character current)
                      (setf escaped t)
                      (incf index))
                     (quote-char
                      (write-char character current)
                      (when (char= character quote-char) (setf quote-char nil))
                      (incf index))
                     ((or (char= character #\') (char= character #\"))
                      (write-char character current)
                      (setf quote-char character)
                      (incf index))
                     (t
                      (cond ((char= character #\() (incf paren-depth))
                            ((and (char= character #\)) (plusp paren-depth)) (decf paren-depth))
                            ((char= character #\{) (incf brace-depth))
                            ((and (char= character #\}) (plusp brace-depth)) (decf brace-depth)))
                      (if (and (plusp (length separator))
                               (zerop paren-depth)
                               (zerop brace-depth)
                               (%starts-at text separator index))
                          (progn
                            (vector-push-extend
                             (string-trim +js-whitespace+ (get-output-stream-string current))
                             items)
                            (incf index (length separator)))
                          (progn (write-char character current) (incf index)))))))
    (when quote-char (%signature-error "Unterminated string"))
    (vector-push-extend (string-trim +js-whitespace+ (get-output-stream-string current)) items)
    items))

(defun %starts-at (text needle index)
  (let ((end (+ index (length needle))))
    (and (<= end (length text))
         (string= needle text :start2 index :end2 end))))

(defun core-string-find-outside-quotes (text needle)
  "The index of NEEDLE in TEXT outside any quoted span, or -1.

This is how Ax finds a signature's -> arrow without being fooled by an
arrow inside a quoted description."
  (let ((text (core-js-text text))
        (needle (core-js-text needle))
        (quote-char nil)
        (escaped nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond (escaped (setf escaped nil))
                   ((char= character #\\) (setf escaped t))
                   (quote-char (when (char= character quote-char) (setf quote-char nil)))
                   ((or (char= character #\') (char= character #\"))
                    (setf quote-char character))
                   ((%starts-at text needle index)
                    (return-from core-string-find-outside-quotes index))))
    (when quote-char (%signature-error "Unterminated string"))
    -1))

(defun core-string-extract-leading-group (text open-char close-char)
  "The balanced group TEXT opens with, as found, balanced, group and rest.

Quoted spans and escapes are skipped, so a brace or parenthesis inside a
description does not change the nesting depth."
  (let ((text (core-js-text text))
        (open-char (core-js-text open-char))
        (close-char (core-js-text close-char)))
    (when (or (zerop (length open-char))
              (zerop (length close-char))
              (not (%starts-at text open-char 0)))
      (return-from core-string-extract-leading-group
        (axllm:object "found" 'yason:false "balanced" 'yason:true "group" "" "rest" text)))
    (let ((quote-char nil) (escaped nil) (depth 0) (index 0))
      (loop with limit = (length text)
            while (< index limit)
            do (let ((character (char text index)))
                 (cond (escaped (setf escaped nil))
                       ((char= character #\\) (setf escaped t))
                       (quote-char (when (char= character quote-char) (setf quote-char nil)))
                       ((or (char= character #\') (char= character #\"))
                        (setf quote-char character))
                       ((%starts-at text open-char index)
                        (incf depth)
                        (incf index (1- (length open-char))))
                       ((%starts-at text close-char index)
                        (decf depth)
                        (when (zerop depth)
                          (return-from core-string-extract-leading-group
                            (axllm:object "found" 'yason:true
                                          "balanced" 'yason:true
                                          "group" (subseq text (length open-char) index)
                                          "rest" (subseq text (+ index (length close-char))))))
                        (incf index (1- (length close-char))))))
               (incf index))
      (when quote-char (%signature-error "Unterminated string"))
      (axllm:object "found" 'yason:true "balanced" 'yason:false
                    "group" (subseq text (length open-char)) "rest" ""))))

(defun %consume-quoted-prefix (text)
  "The quoted string TEXT starts with, as value, rest and found."
  (if (or (zerop (length text))
          (not (or (char= (char text 0) #\') (char= (char text 0) #\"))))
      (axllm:object "value" :null "rest" text "found" 'yason:false)
      (let ((quote-char (char text 0))
            (escaped nil)
            (out (make-string-output-stream)))
        (loop for index from 1 below (length text)
              for character = (char text index)
              do (cond (escaped (write-char character out) (setf escaped nil))
                       ((char= character #\\) (setf escaped t))
                       ((char= character quote-char)
                        (return-from %consume-quoted-prefix
                          (axllm:object "value" (get-output-stream-string out)
                                        "rest" (subseq text (1+ index))
                                        "found" 'yason:true)))
                       (t (write-char character out))))
        (%signature-error "Unterminated string"))))

(defun core-string-consume-optional-quoted-prefix (text)
  (%consume-quoted-prefix (core-js-text text)))

(defun core-string-extract-quoted-suffix (text)
  "The first quoted span in TEXT, with the text before it.

Returns value, index, rest, head and found; when TEXT holds no quote,
found is false and head is all of TEXT."
  (let ((text (core-js-text text))
        (escaped nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond (escaped (setf escaped nil))
                   ((char= character #\\) (setf escaped t))
                   ((or (char= character #\') (char= character #\"))
                    (let ((consumed (%consume-quoted-prefix (subseq text index))))
                      (return-from core-string-extract-quoted-suffix
                        (axllm:object "value" (core-get consumed "value")
                                      "index" index
                                      "rest" (core-get consumed "rest")
                                      "head" (subseq text 0 index)
                                      "found" 'yason:true))))))
    (axllm:object "value" :null "index" :null "rest" "" "head" text "found" 'yason:false)))

(defun core-description-append (base hint)
  "BASE with HINT appended as a second sentence.

An empty side is dropped, and BASE gains a full stop before HINT when it
does not already end with one."
  (let ((hint-text (if (eq hint :null) "" (core-js-text hint)))
        (base-text (if (eq base :null) "" (core-js-text base))))
    (cond ((zerop (length (string-trim +js-whitespace+ hint-text))) base)
          ((zerop (length (string-trim +js-whitespace+ base-text))) hint-text)
          (t (let ((text (string-trim +js-whitespace+ base-text)))
               (unless (and (plusp (length text)) (char= (char text (1- (length text))) #\.))
                 (setf text (concatenate 'string text ".")))
               (concatenate 'string text " " hint-text))))))

;;; ------------------------------------------------------------------
;;; Regular expressions
;;; ------------------------------------------------------------------

;;; Ax's patterns are ECMAScript patterns: a signature's pattern modifier,
;;; a tool argument's schema pattern and Core's own fixed patterns are all
;;; written as `new RegExp(source)` with no flags. CL-PPCRE is a Perl
;;; engine, and on several constructs Perl and ECMAScript disagree:
;;;
;;;   $      Perl also matches before a final newline; ECMAScript does not
;;;   .      Perl excludes only \n; ECMAScript excludes four line terminators
;;;   \s     Perl's set omits \v, U+00A0, U+FEFF and the Unicode spaces
;;;   \uFFFF ECMAScript's escape; Perl spells it \x{FFFF}
;;;
;;; and on others Perl cannot express ECMAScript's meaning at all. So a
;;; pattern is translated into the Perl pattern with the same meaning, and
;;; an untranslatable construct is refused by name. Nothing here claims the
;;; two engines are the same.
;;;
;;; The authority on whether a pattern matches is Core's own matcher, in
;;; ir/axcore/regex.axir, which walks UTF-16 units and needs no host
;;; engine. CORE-REGEX-MATCH uses it whenever the generated Core file
;;; provides it. The host engine stays for the two things that matcher does
;;; not do: locating a match, which a replacement and a split need, and
;;; answering at all in the experimental subset that omits it.

(defparameter +js-dot-class+
  (format nil "[^\\n\\r~a~a]"
          (string (code-char #x2028)) (string (code-char #x2029)))
  "ECMAScript's dot without /s: anything but the four line terminators.")

(defparameter +perl-class-metacharacters+ "^]\\-["
  "The characters a Perl character class reads as syntax.")

(defparameter +perl-metacharacters+ ".^$*+?()[]{}|\\/"
  "The characters a Perl pattern reads as syntax outside a class.")

(defun %perl-literal (character in-class)
  "CHARACTER as a Perl pattern that matches exactly it.

Written as the character itself, with a backslash when the engine would
otherwise read it as syntax. Not as a hex escape: CL-PPCRE does not
implement \\x{...}, and reads it as a literal x, a brace and digits, so a
character class built from hex escapes quietly matches the wrong set."
  (let ((metacharacters (if in-class +perl-class-metacharacters+ +perl-metacharacters+)))
    (if (find character metacharacters)
        (coerce (list #\\ character) 'string)
        (string character))))

(defun %js-class-body (characters)
  "CHARACTERS as the inside of a Perl character class."
  (with-output-to-string (out)
    (loop for character across characters
          do (write-string (%perl-literal character t) out))))

(defparameter +universal-classes+
  '("[\\s\\S]" "[\\S\\s]" "[\\d\\D]" "[\\D\\d]" "[\\w\\W]" "[\\W\\w]")
  "The character classes that mean \"any character\" in both engines.

A class is the union of its members, so a shorthand together with its own
negation covers everything, whatever the two engines think that shorthand
contains. This is the one place a negated shorthand inside a class can be
translated, and it is the form Ax actually uses: [\\s\\S] is how the
reference implementation writes a dot that also crosses line breaks.")

(defun %universal-class-at (text index)
  "The index after a universal character class at INDEX, or NIL."
  (dolist (form +universal-classes+)
    (when (%starts-at text form index)
      (return (+ index (length form))))))

(defun %ecmascript-reject (pattern construct)
  (%ax-error "regular expression ~s uses ~a, which the host engine cannot ~
match with ECMAScript's meaning; this pattern needs Core's matcher ~
(ir/axcore/regex.axir)"
             pattern construct))

(defun %has-named-group-p (text)
  "Whether TEXT declares a named capture group.

ECMAScript reads \\k<name> as a backreference only in a pattern that has
one, and as the identity escape k otherwise. (?<= and (?<! are lookbehind,
not a group specifier."
  (loop with index = 0
        for found = (search "(?<" text :start2 index)
        while found
        do (let ((after (+ found 3)))
             (when (and (< after (length text))
                        (not (find (char text after) "=!")))
               (return t))
             (setf index (1+ found)))))

(defun %quantifier-at (text index)
  "The index after a {n}, {n,} or {n,m} quantifier at INDEX, or NIL.

A brace that does not begin one is an ordinary character in ECMAScript,
which is why this has to be decided rather than assumed."
  (let ((at (1+ index))
        (limit (length text))
        (digits 0))
    (loop while (and (< at limit) (digit-char-p (char text at)))
          do (incf at) (incf digits))
    (when (zerop digits) (return-from %quantifier-at nil))
    (when (and (< at limit) (char= (char text at) #\,))
      (incf at)
      (loop while (and (< at limit) (digit-char-p (char text at))) do (incf at)))
    (when (and (< at limit) (char= (char text at) #\}))
      (1+ at))))

(defun %ecmascript-to-perl (pattern)
  "PATTERN, an ECMAScript source with no flags, as an equal Perl pattern.

Three kinds of difference are handled here.
\(1) Constructs the two engines spell differently or scope differently: $,
the dot, the shorthands and \\uFFFF.
\(2) Escapes that have no special meaning in a flagless ECMAScript pattern
and so stand for the letter itself, under Annex B's IdentityEscape. \\p{L}
is the four characters p{L}, not a Unicode property: with no u flag
new RegExp(\"\\\\p{L}\").test(\"p{L}\") is true and .test(\"a\") is false. The
same holds for \\P, \\8, \\9, \\u that is not followed by four hex digits,
and \\k in a pattern with no named group.
\(3) Constructs whose ECMAScript meaning a Perl engine cannot express,
which are refused by name.

A brace that does not begin a quantifier is an ordinary character, so it
is passed through as one rather than left to be read as syntax."
  (let ((text (core-js-text pattern))
        (in-class nil)
        (index 0))
    (with-output-to-string (out)
      (loop with limit = (length text)
            while (< index limit)
            do (let ((character (char text index)))
                 (cond
                   ((char= character #\\)
                    (when (>= (1+ index) limit)
                      (%ecmascript-reject text "a trailing backslash"))
                    (let ((next (char text (1+ index))))
                      (incf index 2)
                      (case next
                        ((#\d) (write-string (if in-class "0-9" "[0-9]") out))
                        ((#\w) (write-string (if in-class "A-Za-z0-9_" "[A-Za-z0-9_]") out))
                        ((#\s) (write-string
                                (if in-class
                                    (%js-class-body +js-whitespace+)
                                    (format nil "[~a]" (%js-class-body +js-whitespace+)))
                                out))
                        ((#\D #\W #\S)
                         (if in-class
                             ;; A negated shorthand inside a class is a set
                             ;; subtraction Perl cannot write.
                             (%ecmascript-reject text (format nil "\\~a inside a character class" next))
                             (write-string
                              (ecase next
                                (#\D "[^0-9]")
                                (#\W "[^A-Za-z0-9_]")
                                (#\S (format nil "[^~a]" (%js-class-body +js-whitespace+))))
                              out)))
                        ((#\u)
                         ;; Four hex digits are a code unit; anything else,
                         ;; including \u{...}, is the letter u.
                         (if (and (<= (+ index 4) limit)
                                  (every (lambda (digit) (digit-char-p digit 16))
                                         (subseq text index (+ index 4))))
                             (progn
                               (write-string
                                (%perl-literal
                                 (code-char (parse-integer text :start index :end (+ index 4)
                                                                :radix 16))
                                 in-class)
                                out)
                               (incf index 4))
                             (write-string (%perl-literal #\u in-class) out)))
                        ;; Annex B identity escapes: these stand for the
                        ;; letter in a pattern with no flags.
                        ((#\p #\P #\8 #\9)
                         (write-string (%perl-literal next in-class) out))
                        ((#\k)
                         (if (%has-named-group-p text)
                             (progn (write-char #\\ out) (write-char next out))
                             (write-string (%perl-literal next in-class) out)))
                        (t (write-char #\\ out) (write-char next out)))))
                   (in-class
                    (when (char= character #\])
                      (setf in-class nil))
                    (write-char character out)
                    (incf index))
                   ((char= character #\[)
                    ;; A class holding a shorthand and its negation is
                    ;; "any character", and both engines agree on that
                    ;; however their own \s sets differ. [\s\S] is how Ax
                    ;; writes a dot that also crosses newlines.
                    (let ((universal (%universal-class-at text index)))
                      (cond (universal
                             (write-string "[\\s\\S]" out)
                             (setf index universal))
                            (t
                             (setf in-class t)
                             (write-char character out)
                             (incf index)))))
                   ((char= character #\^)
                    ;; Without /m, ECMAScript anchors at the input start.
                    (write-string "\\A" out)
                    (incf index))
                   ((char= character #\$)
                    ;; Without /m, ECMAScript anchors at the very end, where
                    ;; Perl's $ also matches before a final newline.
                    (write-string "\\z" out)
                    (incf index))
                   ((char= character #\.)
                    (write-string +js-dot-class+ out)
                    (incf index))
                   ((char= character #\{)
                    ;; A quantifier passes through; a brace that is not one
                    ;; is an ordinary character, as it is in ECMAScript.
                    (let ((after (%quantifier-at text index)))
                      (if after
                          (progn (write-string (subseq text index after) out)
                                 (setf index after))
                          (progn (write-string "\\{" out) (incf index)))))
                   ((char= character #\})
                    (write-string "\\}" out)
                    (incf index))
                   (t (write-char character out) (incf index))))))))

(defvar *regex-cache* (make-hash-table :test 'equal :synchronized t)
  "Compiled scanners, keyed by ECMAScript pattern source.")

(defun %scanner (pattern)
  "A compiled host scanner for the ECMAScript PATTERN.

The pattern is translated first, so what the host engine runs has the
ECMAScript meaning; a source the host engine then refuses to compile is an
error naming the pattern rather than a Lisp backtrace.

Named registers are enabled, because a named capture group and \\k<name>
mean the same thing in both engines once the host engine is willing to
read them. The one construct that reaches the host engine and can still be
refused by it is a variable-length lookbehind, which ECMAScript allows and
CL-PPCRE does not implement; that comes back as an error naming the
pattern, which is the honest answer rather than a wrong match."
  (let ((pattern (core-js-text pattern)))
    (or (gethash pattern *regex-cache*)
        (setf (gethash pattern *regex-cache*)
              (let ((translated (%ecmascript-to-perl pattern))
                    (cl-ppcre:*allow-named-registers* t))
                (handler-case (cl-ppcre:create-scanner translated)
                  (axllm:ax-error (condition) (error condition))
                  (error (condition)
                    (%ax-error "regular expression ~s did not compile: ~a"
                               pattern condition))))))))

(defun core-matcher ()
  "Core's own ECMAScript matcher, when the generated Core file has it.

Looked up by name because core-runtime.lisp is loaded before core.lisp:
the matcher is Core code, generated from ir/axcore/regex.axir, and the
experimental four-root subset does not reach it."
  (let ((symbol (find-symbol "REGEX-TEST" (find-package "AXLLM/CORE"))))
    (and symbol (fboundp symbol) symbol)))

(defun core-regex-match (pattern value)
  "Whether PATTERN matches anywhere in VALUE. A non-string never matches.

Answered by Core's matcher when the generated Core file provides it, so a
pattern behaves the same here as on every other target. Without it, the
host engine answers the translated pattern."
  (if (not (stringp value))
      'yason:false
      (let ((matcher (core-matcher))
            (pattern (core-js-text pattern)))
        (if matcher
            (core-bool (core-true-p (funcall matcher pattern value)))
            (core-bool (and (cl-ppcre:scan (%scanner pattern) value) t))))))

(defun core-regex-search (pattern value)
  "Where PATTERN first matches in VALUE: the start and end, or NIL.

The host engine locates the match, which Core's matcher does not do: it
answers only whether a pattern matches. When both are available the two
must agree, and a disagreement is an error rather than a quiet answer from
whichever engine was asked, because it means the translation in
%ECMASCRIPT-TO-PERL is wrong for this pattern."
  (let* ((text (core-js-text value))
         (pattern (core-js-text pattern))
         (matcher (core-matcher)))
    (multiple-value-bind (start end) (cl-ppcre:scan (%scanner pattern) text)
      (when matcher
        (let ((matched (core-true-p (funcall matcher pattern text))))
          (unless (eq (and start t) matched)
            (%ax-error "regular expression ~s: Core's matcher ~:[finds no match~;matches~] ~
in ~s where the host engine ~:[does not~;does~]"
                       pattern matched (and start t)))))
      (when start (values start end)))))

(defun core-regex-capture (pattern value)
  "PATTERN's capture groups in VALUE, as a vector, or NIL when it misses.

A group that did not take part in the match is :NULL, as an unmatched
group is undefined in the reference semantics. The same engine agreement
CORE-REGEX-SEARCH requires applies here."
  (let* ((text (core-js-text value))
         (pattern (core-js-text pattern))
         (scanner (%scanner pattern)))
    (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings scanner text)
      (let ((matcher (core-matcher)))
        (when matcher
          (let ((matched (core-true-p (funcall matcher pattern text))))
            (unless (eq (and match t) matched)
              (%ax-error "regular expression ~s: Core's matcher and the host engine disagree on ~s"
                         pattern text)))))
      (when match
        (map 'vector (lambda (group) (or group :null)) groups)))))

;;; ------------------------------------------------------------------
;;; Type tests
;;; ------------------------------------------------------------------

(defun core-type-is (value type-name)
  "Whether VALUE is of the Core JSON type TYPE-NAME.

An Ax record is not an object here, matching the other ports, where a
record is a struct rather than a map."
  (let ((type-name (core-js-text type-name)))
    (core-bool
     (cond ((string= type-name "object")
            (and (hash-table-p value) (null (core-record-kind value))))
           ((string= type-name "list") (core-array-p value))
           ((string= type-name "string") (stringp value))
           ((string= type-name "number") (realp value))
           ((string= type-name "boolean")
            (or (eq value 'yason:true) (eq value 'yason:false)))
           ((string= type-name "null") (eq value :null))
           ((string= type-name "json")
            (or (eq value :null) (eq value 'yason:true) (eq value 'yason:false)
                (stringp value) (realp value) (core-array-p value) (hash-table-p value)))
           ;; A host date value, which a date or datetime field takes where
           ;; the reference implementation takes a Date. Core asks this in
           ;; validate.axir before it refuses a value it cannot read as
           ;; text, so without it every native date fails validation.
           ((string= type-name "date")
            (not (eq (core-date-millis value) :null)))
           ;; An unrecognised name is false, as it is in the other ports.
           ;; The IR also writes "bool" and "array" in a few agent and
           ;; event paths, where every port answers false; those are an IR
           ;; naming slip rather than a type this should model, and
           ;; answering them here would make this port alone take branches
           ;; the others never take.
           (t nil)))))

;;; ------------------------------------------------------------------
;;; Errors
;;; ------------------------------------------------------------------

(defun core-signature-error (message)
  "A signature error, as a condition object for CORE.RAISE to signal."
  (make-condition 'axllm:signature-error :message (core-js-text message)))

(defun core-validation-error (message)
  "A validation error, as a condition object for CORE.RAISE to signal."
  (make-condition 'axllm:validation-error :message (core-js-text message)))

;;; ------------------------------------------------------------------
;;; UTF-16
;;; ------------------------------------------------------------------

;;; A Lisp string holds code points, a JavaScript string holds UTF-16 code
;;; units, and Ax's semantics are written against the latter: a signature's
;;; maxLength counts units, and Core's own ECMAScript matcher
;;; (ir/axcore/regex.axir) walks units. These three boundaries are the whole
;;; of the conversion, and Core does the rest itself.
;;;
;;; A string may also hold one half of a surrogate pair, because a provider
;;; can split a pair across two stream events. Such a character is one unit
;;; and one code point, exactly as it is in a JavaScript string.

(defun core-string-utf16-units (value)
  "VALUE's UTF-16 code units.

An astral character becomes its surrogate pair; a character already in the
surrogate range is passed through as the single unit it is."
  (let ((text (core-js-text value))
        (out (core-new-list)))
    (loop for character across text
          for code = (char-code character)
          do (if (< code #x10000)
                 (vector-push-extend code out)
                 (let ((offset (- code #x10000)))
                   (vector-push-extend (+ #xd800 (ash offset -10)) out)
                   (vector-push-extend (+ #xdc00 (logand offset #x3ff)) out))))
    out))

(defun core-string-codepoint-length (value)
  (length (core-js-text value)))

(defun core-string-drop-trailing-high-surrogate (value)
  "VALUE without a trailing unpaired high surrogate."
  (let ((text (core-js-text value)))
    (if (and (plusp (length text))
             (<= #xd800 (char-code (char text (1- (length text)))) #xdbff))
        (subseq text 0 (1- (length text)))
        text)))

(defun core-string-concat-stream-text (left right)
  "LEFT and RIGHT joined, rejoining a surrogate pair split between them.

Streamed text arrives chunk by chunk, and a provider may end one chunk
with a high surrogate and start the next with its low half. Joining them
back into one character is what a UTF-16 string does on its own."
  (let ((left (core-js-text left))
        (right (core-js-text right)))
    (if (and (plusp (length left))
             (plusp (length right))
             (<= #xd800 (char-code (char left (1- (length left)))) #xdbff)
             (<= #xdc00 (char-code (char right 0)) #xdfff))
        (let ((code (+ #x10000
                       (ash (- (char-code (char left (1- (length left)))) #xd800) 10)
                       (- (char-code (char right 0)) #xdc00))))
          (concatenate 'string
                       (subseq left 0 (1- (length left)))
                       (string (code-char code))
                       (subseq right 1)))
        (concatenate 'string left right))))

;;; ------------------------------------------------------------------
;;; JSON
;;; ------------------------------------------------------------------

(defun core-json-parse (value)
  "VALUE parsed as JSON, after dropping a Markdown code fence around it.

A model asked for JSON often answers with a fenced block. Every port
strips the backticks and a leading json label before parsing, so the same
model reply reads the same way everywhere. CORE-JSON-PARSE-STRICT is the
boundary that does not."
  (let ((text (core-string-trim value)))
    (when (and (>= (length text) 3) (string= "```" text :end2 3))
      ;; Only the delimiters go: a backtick inside the document is part of
      ;; it. {\"text\": \"a`b\"} in a fenced block has to survive, so this
      ;; trims the fences from the ends rather than deleting every
      ;; backtick in the text.
      (setf text (string-trim "`" text))
      (when (and (>= (length text) 4) (string= "json" text :end2 4))
        (setf text (subseq text 4)))
      (setf text (string-trim +js-whitespace+ text)))
    (axllm:parse-json text)))

(defun core-json-parse-strict (value)
  "VALUE parsed as JSON, with no fence stripping."
  (axllm:parse-json (core-string-trim value)))

;;; These three are JSON.stringify, which asks a value for its toJSON
;;; before writing it. The only such value in this port is a host date, so
;;; each of them converts dates first, through CORE-DATE-JSON, and a value
;;; with no date in it is passed straight through by identity. AX:ENCODE-JSON
;;; itself stays strict: it is the Lisp API, it has no toJSON protocol to
;;; honour, and it still refuses anything outside the JSON model by name.

(defun core-json-stringify (value)
  "VALUE as JSON.stringify writes it: compact, keys in JavaScript's order."
  (axllm:encode-json (core-date-json value)))

(defun core-json-pretty (value)
  "VALUE as JSON.stringify(value, null, 2) writes it."
  (axllm:encode-json (core-date-json value) :indent 2))

(defun core-json-stable-stringify (value)
  "VALUE as JSON with every object's keys sorted by name.

A cache key has to be the same text for the same content, whatever order
the keys were written in, so this sorts rather than following insertion
order. Null stringifies as an empty object, as the other ports do, because
a key is built from a map."
  (axllm:encode-json (core-date-json (if (eq value :null) (core-new-map) value))
                     :sort-keys t))

;;; ------------------------------------------------------------------
;;; Records
;;; ------------------------------------------------------------------

(defun core-title-from-name (name)
  "NAME as Ax's field title.

Underscores become spaces and a word starts at a capital after a lowercase
letter or digit, at the last capital of a run that begins a word, and at
each run of digits: userID is \"User ID\", parseHTTPResponse is \"Parse
HTTP Response\", item123 is \"Item 123\"."
  (let ((text (substitute #\Space #\_ (core-js-text name))))
    (setf text (cl-ppcre:regex-replace-all "([a-z0-9])([A-Z])" text "\\1 \\2"))
    (setf text (cl-ppcre:regex-replace-all "([A-Z])([A-Z][a-z])" text "\\1 \\2"))
    (setf text (cl-ppcre:regex-replace-all "([^0-9])([0-9])" text "\\1 \\2"))
    (setf text (string-trim " " (cl-ppcre:regex-replace-all "\\s+" text " ")))
    (if (plusp (length text))
        (concatenate 'string (string (char-upcase (char text 0))) (subseq text 1))
        text)))

(defun %attr (attrs key &optional (fallback :null))
  "ATTRS at KEY, or at KEY's other spelling, or FALLBACK.

This is the one place +RECORD-KEY-ALIASES+ applies: a record constructor
is handed an attribute map written by a caller, a fixture or the fluent
builder, and those use Ax's TypeScript spelling where the IR uses the
record's own. The exact key always wins, so a map carrying both min_length
and minLength is read deterministically and not by hash order."
  (unless (hash-table-p attrs)
    (return-from %attr fallback))
  (multiple-value-bind (value found) (gethash key attrs)
    (if found
        value
        (let ((alias (core-key-alias key)))
          (if alias
              (multiple-value-bind (value found) (gethash alias attrs)
                (if found value fallback))
              fallback)))))

(defun %attr-bool (attrs key)
  (core-bool (core-true-p (%attr attrs key 'yason:false))))

(defun %attr-list (attrs key)
  (let ((value (%attr attrs key)))
    (if (core-array-p value) value (core-new-list))))

(defparameter +field-type-keys+
  '("name" "is_array" "options" "fields" "min_length" "max_length" "minimum"
    "maximum" "pattern" "pattern_description" "value_descriptions" "format"
    "language" "description")
  "A FieldType record's keys, in the order Ax declares them.")

(defun %make-field-type (attrs)
  (let ((record (core-new-map)))
    (setf (gethash :record record) "FieldType")
    (core-set record "name" (let ((name (%attr attrs "name")))
                              (if (eq name :null) "string" name)))
    (core-set record "is_array" (%attr-bool attrs "is_array"))
    (dolist (key (cddr +field-type-keys+))
      (core-set record key (%attr attrs key)))
    record))

(defun %make-field (attrs)
  (let ((record (core-new-map))
        (name (%attr attrs "name")))
    (when (eq name :null)
      (%signature-error "Field record requires a name"))
    (setf (gethash :record record) "Field")
    (core-set record "name" name)
    (core-set record "type" (let ((type (%attr attrs "type")))
                              (cond ((equal (core-record-kind type) "FieldType") type)
                                    ((hash-table-p type) (%make-field-type type))
                                    (t (%make-field-type (core-new-map))))))
    (core-set record "description" (%attr attrs "description"))
    (core-set record "title" (let ((title (%attr attrs "title")))
                               (if (eq title :null) (core-title-from-name name) title)))
    (core-set record "is_optional" (%attr-bool attrs "is_optional"))
    (core-set record "is_internal" (%attr-bool attrs "is_internal"))
    (core-set record "is_cached" (%attr-bool attrs "is_cached"))
    record))

(defun %make-signature (attrs)
  (let ((record (core-new-map)))
    (setf (gethash :record record) "AxSignature")
    ;; Ax builds this record from inputs and outputs, and reads it back as
    ;; input_fields and output_fields; the constructor is where the two
    ;; spellings meet, exactly as in the other ports.
    (core-set record "input_fields" (%attr-list attrs "inputs"))
    (core-set record "output_fields" (%attr-list attrs "outputs"))
    (core-set record "description" (%attr attrs "description"))
    record))

(defun core-record-new (name values)
  "A new Ax record of type NAME from the attribute map VALUES."
  (let ((name (core-js-text name)))
    (cond ((string= name "FieldType") (%make-field-type values))
          ((string= name "Field") (%make-field values))
          ((string= name "AxSignature") (%make-signature values))
          (t (%signature-error "Unknown record type: ~A" name)))))

(defun core-fields-from-map (fields)
  "A map of name to Field or FieldType, as a list of Field records."
  (let ((out (core-new-list)))
    (when (hash-table-p fields)
      (dolist (key (axllm::%object-keys fields))
        (let ((item (gethash key fields)))
          (vector-push-extend
           (if (core-field-p item)
               item
               (core-record-new "Field" (axllm:object "name" key "type" item)))
           out))))
    out))

(defun core-deep-copy (value)
  "A copy of VALUE that shares no mutable structure with it.

Objects and arrays are copied through, in key order, and a record keeps
its marker, so a copied Field is still a Field. A string is copied too: a
Lisp string is mutable, so sharing one would leave a caller able to change
the original through the copy, which is the thing a deep copy is for.
Numbers, booleans and null have no structure to share. A host object is
returned as it is: it is not Core's to copy."
  (cond ((hash-table-p value)
         (let ((out (core-new-map)))
           (let ((kind (gethash :record value)))
             (when kind (setf (gethash :record out) kind)))
           (dolist (key (axllm::%object-keys value))
             (axllm::%set-key out key (core-deep-copy (gethash key value))))
           out))
        ((core-array-p value)
         (let ((out (core-new-list)))
           (loop for item across value
                 do (vector-push-extend (core-deep-copy item) out))
           out))
        ((stringp value) (copy-seq value))
        (t value)))

(defun core-field-item (field)
  "FIELD as its own element type: a copy whose type is not an array.

A list<string> field describes each item as a string, so the schema and
the prompt need the field with is_array cleared. The copy is deep, because
clearing it on the original would change the field every later caller
sees."
  (let ((copy (core-deep-copy field)))
    (unless (hash-table-p copy)
      (%ax-error "intrinsic.field.item: ~S is not a field" field))
    (let ((type (core-get copy "type")))
      (when (hash-table-p type)
        (core-set type "is_array" 'yason:false)
        ;; A nested field map may carry the TypeScript spelling as well;
        ;; leaving it behind would let a later read see the array again.
        (remhash "isArray" type)))
    copy))

;;; ------------------------------------------------------------------
;;; Media value shapes
;;; ------------------------------------------------------------------

;;; These four say whether a value has the shape of an image, an audio
;;; clip, a file or a URL part. They test for the keys the reference
;;; implementation tests for, in its own camelCase spelling, and they read
;;; the object directly rather than through CORE-GET: a media part is a
;;; provider wire object, and whether it carries data or fileUri decides
;;; which branch Core takes.

(defun %has-key (value key)
  (and (hash-table-p value) (nth-value 1 (gethash key value))))

(defun valid-image (value)
  "Whether VALUE is an image part: an object with mimeType and data."
  (core-bool (and (%has-key value "mimeType") (%has-key value "data"))))

(defun valid-audio (value)
  "Whether VALUE is an audio part: text, or an object with data or id."
  (core-bool (or (stringp value)
                 (%has-key value "data")
                 (%has-key value "id"))))

(defun valid-file (value)
  "Whether VALUE is a file part: mimeType, and exactly one of data, fileUri."
  (core-bool (and (%has-key value "mimeType")
                  (not (eq (and (%has-key value "data") t)
                           (and (%has-key value "fileUri") t))))))

(defun valid-url-shape (value)
  "Whether VALUE is a URL part: text, or an object with url."
  (core-bool (or (stringp value) (%has-key value "url"))))

;;; ------------------------------------------------------------------
;;; URLs
;;; ------------------------------------------------------------------

(defun core-url-valid (value)
  "Whether VALUE begins with a scheme and \"://\".

This is the test the reference implementation applies to a url field: it
checks for an absolute URL's shape, not that the URL resolves."
  (core-bool (and (stringp value)
                  (cl-ppcre:scan "^[a-zA-Z][a-zA-Z0-9+.-]*://" value))))

(defparameter +uri-component-safe+ "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()"
  "The characters encodeURIComponent leaves alone.")

(defun core-url-encode-component (value)
  "VALUE percent-encoded as encodeURIComponent writes it.

Every UTF-8 byte outside the unreserved set becomes %XX with upper-case
hex digits."
  (let ((text (if (eq value :null) "" (core-js-text value))))
    (with-output-to-string (out)
      (loop for character across text
            do (if (find character +uri-component-safe+)
                   (write-char character out)
                   (loop for byte across (sb-ext:string-to-octets
                                          (string character) :external-format :utf-8)
                         do (format out "%~2,'0X" byte)))))))

;;; ------------------------------------------------------------------
;;; Host libraries
;;; ------------------------------------------------------------------

(defun %host-package (system package context)
  "PACKAGE, loading ASDF system SYSTEM first if it is not present yet.

Two boundaries need a library this port does not otherwise use: a SHA-256
digest and the platform's time zone database. Loading on first use keeps
them out of the load path of every other caller, and names the missing
system rather than failing with an unbound symbol."
  (or (find-package package)
      (progn
        (handler-case (asdf:load-system system)
          (error (condition)
            (%ax-error "~a needs the ~a system, which did not load: ~a"
                       context system condition)))
        (or (find-package package)
            (%ax-error "~a needs the ~a system, which loaded without ~a"
                       context system package)))))

(defun %host-call (system package name context &rest arguments)
  (let ((package (%host-package system package context)))
    (apply (or (find-symbol (string name) package)
               (%ax-error "~a: ~a has no ~a" context package name))
           arguments)))

;;; ------------------------------------------------------------------
;;; Digests
;;; ------------------------------------------------------------------

(defun core-crypto-sha256-hex (text)
  "The SHA-256 of TEXT's UTF-8 bytes, as lower-case hex.

The digest comes from Ironclad rather than from an implementation here:
a hash is a solved problem with published test vectors, and a port is not
the place to rewrite one."
  (let ((bytes (sb-ext:string-to-octets (core-js-text text) :external-format :utf-8)))
    (%host-call "ironclad" "IRONCLAD" "BYTE-ARRAY-TO-HEX-STRING" "intrinsic.crypto.sha256_hex"
                (%host-call "ironclad" "IRONCLAD" "DIGEST-SEQUENCE" "intrinsic.crypto.sha256_hex"
                            :sha256 bytes))))

;;; ------------------------------------------------------------------
;;; Dates
;;; ------------------------------------------------------------------

;;; Core does all of Ax's date parsing and arithmetic itself, in epoch
;;; milliseconds (ir/axcore/dates.axir). Two things it cannot do are asking
;;; the platform for a zone's offset, and recognising a host date value a
;;; caller passed in.

(defgeneric core-date-millis (value)
  (:documentation
   "VALUE's instant in epoch milliseconds, or :NULL when it is not a date.

Returning :NULL rather than failing is the contract: Core asks this of any
value in a date-typed field and renders the text itself when the answer is
null. A subsystem with its own date type adds a method here.")
  (:method (value)
    (if (%local-time-timestamp-p value)
        (%local-time-millis value)
        :null)))

(defun %local-time-timestamp-p (value)
  "Whether VALUE is a LOCAL-TIME:TIMESTAMP.

Tested by name so this file does not have to be read with local-time
already loaded: a caller that has timestamps has the system."
  (let ((package (find-package "LOCAL-TIME")))
    (and package
         (let ((class (find-symbol "TIMESTAMP" package)))
           (and class (typep value class))))))

(defun %local-time-millis (value)
  (let ((seconds (%host-call "local-time" "LOCAL-TIME" "TIMESTAMP-TO-UNIX"
                             "core.date_millis" value))
        (nanoseconds (%host-call "local-time" "LOCAL-TIME" "NSEC-OF"
                                 "core.date_millis" value)))
    (+ (* seconds 1000) (floor nanoseconds 1000000))))

;;; datetime covers years 1 through 9999 in the other ports. An offset is
;;; constant before a zone's first transition and follows its rule after
;;; the last, so an instant a day inside either end reads the same offset
;;; as the clamped one.
(defparameter +date-min-seconds+ -62135510400)
(defparameter +date-max-seconds+ 253402128000)

;;; The zone database is read here rather than through a library, because
;;; the Lisp libraries that read it read only a zone file's 32-bit
;;; transition block. That block stops at 2038-01-19, and a modern zone
;;; file stops recording transitions in 2037 and leaves the rest to the
;;; POSIX rule in its footer, so a 32-bit reader answers an instant in 2040
;;; with the offset of the last transition it could see: Denver would read
;;; as standard time in July. This reads the 64-bit block and the footer
;;; rule, which together cover every instant the other ports cover.
;;;
;;; The format is RFC 8536. A version 2 or 3 file repeats its header and
;;; data with 64-bit times after the legacy 32-bit copy, then ends with a
;;; POSIX TZ string giving the rule that governs after the last recorded
;;; transition.

(defstruct (zone (:constructor %make-zone))
  "One IANA zone: its recorded transitions and the rule after them."
  (transitions (vector) :type vector)   ; sorted epoch seconds
  (indexes (vector) :type vector)       ; transitions -> types
  (offsets (vector) :type vector)       ; type -> UTC offset in seconds
  (daylight (vector) :type vector)      ; type -> whether it is daylight time
  (standard 0 :type integer)            ; offset before the first transition
  (rule nil))                           ; the parsed footer rule, or NIL

(defvar *zone-cache* (make-hash-table :test 'equal :synchronized t)
  "Zones already read, by IANA name. NIL records a name with no zone.")

(defun %zone-directory ()
  (let ((override (uiop:getenv "TZDIR")))
    (if (and override (plusp (length override)))
        (uiop:ensure-directory-pathname override)
        #p"/usr/share/zoneinfo/")))

(defun %zone-name-safe-p (name)
  "Whether NAME is a plain zone name and not a path out of the database."
  (and (plusp (length name))
       (not (find #\/ name :end 1))
       (every (lambda (part)
                (and (plusp (length part))
                     (string/= part ".")
                     (string/= part "..")
                     (every (lambda (character)
                              (or (alphanumericp character)
                                  (find character "_-+.")))
                            part)))
              (%split-literal name "/"))))

(defun %read-bytes (stream count)
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (unless (= count (read-sequence bytes stream))
      (%ax-error "zone file ended early"))
    bytes))

(defun %be-integer (bytes start width &optional signed)
  (let ((value 0))
    (dotimes (offset width)
      (setf value (+ (* value 256) (aref bytes (+ start offset)))))
    (if (and signed (logbitp (1- (* 8 width)) value))
        (- value (ash 1 (* 8 width)))
        value)))

(defun %read-zone (name)
  "The zone called NAME read from the platform database, or NIL.

NIL means the database has no such zone, which is how Core tells a zone
name from an ordinary word. A file that exists but is not a zone file is
an error, because that is a broken installation rather than a bad name."
  (unless (%zone-name-safe-p name)
    (return-from %read-zone nil))
  (let ((path (merge-pathnames name (%zone-directory))))
    (unless (probe-file path)
      (return-from %read-zone nil))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (flet ((header ()
               (let ((head (%read-bytes stream 44)))
                 (unless (and (= (aref head 0) (char-code #\T))
                              (= (aref head 1) (char-code #\Z))
                              (= (aref head 2) (char-code #\i))
                              (= (aref head 3) (char-code #\f)))
                   (%ax-error "~a is not a zone file" path))
                 (list :version (aref head 4)
                       :isutcnt (%be-integer head 20 4)
                       :isstdcnt (%be-integer head 24 4)
                       :leapcnt (%be-integer head 28 4)
                       :timecnt (%be-integer head 32 4)
                       :typecnt (%be-integer head 36 4)
                       :charcnt (%be-integer head 40 4)))))
        (let* ((first-header (header))
               (version (getf first-header :version))
               (wide (>= version (char-code #\2)))
               (header (if wide
                           (progn
                             ;; Step over the whole legacy 32-bit copy.
                             (%read-bytes
                              stream
                              (+ (* 5 (getf first-header :timecnt))
                                 (* 6 (getf first-header :typecnt))
                                 (getf first-header :charcnt)
                                 (* 8 (getf first-header :leapcnt))
                                 (getf first-header :isstdcnt)
                                 (getf first-header :isutcnt)))
                             (header))
                           first-header))
               (width (if wide 8 4))
               (count (getf header :timecnt))
               (type-count (getf header :typecnt))
               (transitions (make-array count))
               (indexes (make-array count))
               (offsets (make-array type-count))
               (daylight (make-array type-count)))
          (let ((raw (%read-bytes stream (* count width))))
            (dotimes (index count)
              (setf (aref transitions index) (%be-integer raw (* index width) width t))))
          (let ((raw (%read-bytes stream count)))
            (dotimes (index count)
              (setf (aref indexes index) (aref raw index))))
          (let ((raw (%read-bytes stream (* 6 type-count))))
            (dotimes (index type-count)
              (setf (aref offsets index) (%be-integer raw (* 6 index) 4 t)
                    (aref daylight index) (plusp (aref raw (+ (* 6 index) 4))))))
          (%read-bytes stream (+ (getf header :charcnt)
                                 (* (getf header :leapcnt) (+ width 4))
                                 (getf header :isstdcnt)
                                 (getf header :isutcnt)))
          (let ((footer (when wide
                          (let ((text (make-string-output-stream)))
                            (loop for byte = (read-byte stream nil nil)
                                  while byte
                                  do (write-char (code-char byte) text))
                            (string-trim '(#\Newline #\Return #\Space)
                                         (get-output-stream-string text))))))
            (%make-zone
             :transitions transitions
             :indexes indexes
             :offsets offsets
             :daylight daylight
             ;; Before the first transition, every implementation uses the
             ;; first type that is not daylight time, else the first type.
             :standard (if (zerop type-count)
                           0
                           (aref offsets (or (position nil daylight) 0)))
             :rule (when (and footer (plusp (length footer)))
                     (%parse-posix-rule footer)))))))))

;;; ----- the POSIX TZ rule in a zone file's footer -----

(defstruct (posix-rule (:constructor %make-posix-rule))
  "A footer rule: a standard offset, and daylight time's offset and dates."
  (standard 0 :type integer)
  (daylight nil)
  (start nil)
  (end nil))

(defun %parse-posix-name (text index)
  "The zone abbreviation at INDEX, and the index after it."
  (if (and (< index (length text)) (char= (char text index) #\<))
      (let ((close (position #\> text :start index)))
        (unless close (%ax-error "unterminated zone abbreviation in ~s" text))
        (values (subseq text (1+ index) close) (1+ close)))
      (let ((end (or (position-if-not #'alpha-char-p text :start index) (length text))))
        (values (subseq text index end) end))))

(defun %parse-posix-offset (text index)
  "The signed offset at INDEX in seconds to add to local time for UTC."
  (let ((sign 1))
    (when (and (< index (length text)) (find (char text index) "+-"))
      (when (char= (char text index) #\-) (setf sign -1))
      (incf index))
    (let ((parts '()))
      (loop repeat 3
            while (and (< index (length text)) (digit-char-p (char text index)))
            do (let ((end (or (position-if-not #'digit-char-p text :start index)
                              (length text))))
                 (push (parse-integer text :start index :end end) parts)
                 (setf index end)
                 (if (and (< index (length text)) (char= (char text index) #\:))
                     (incf index)
                     (return))))
      (let* ((fields (nreverse parts))
             (hours (or (first fields) 0))
             (minutes (or (second fields) 0))
             (seconds (or (third fields) 0)))
        (values (* sign (+ (* hours 3600) (* minutes 60) seconds)) index)))))

(defun %parse-posix-date (text index)
  "The transition date at INDEX, as a list, and the index after it."
  (let ((character (char text index)))
    (cond ((char= character #\M)
           ;; The date ends at its own time or at the next rule, whichever
           ;; comes first: in "M3.5.0,M10.5.0/3" the only slash belongs to
           ;; the second rule, so looking for either one alone reads past
           ;; the comma.
           (let* ((end (min (or (position #\/ text :start index) (length text))
                            (or (position #\, text :start index) (length text))))
                  (fields (%split-literal (subseq text (1+ index) end) ".")))
             (unless (= 3 (length fields))
               (%ax-error "bad month rule in ~s" text))
             (values (list :month (parse-integer (first fields))
                           (parse-integer (second fields))
                           (parse-integer (third fields)))
                     end)))
          ((char= character #\J)
           (let ((end (or (position-if-not #'digit-char-p text :start (1+ index))
                          (length text))))
             (values (list :julian-one (parse-integer text :start (1+ index) :end end))
                     end)))
          (t
           (let ((end (or (position-if-not #'digit-char-p text :start index)
                          (length text))))
             (values (list :julian-zero (parse-integer text :start index :end end))
                     end))))))

(defun %parse-posix-transition (text index)
  "The date and time of the transition at INDEX, and the index after it."
  (multiple-value-bind (date next) (%parse-posix-date text index)
    (if (and (< next (length text)) (char= (char text next) #\/))
        (multiple-value-bind (seconds after) (%parse-posix-offset text (1+ next))
          (values (list date seconds) after))
        ;; POSIX leaves the time at 02:00 local when it is not given.
        (values (list date 7200) next))))

(defun %parse-posix-rule (text)
  "The POSIX TZ string TEXT as a rule, or NIL when it has no daylight time."
  (handler-case
      (multiple-value-bind (ignored index) (%parse-posix-name text 0)
        (declare (ignore ignored))
        (multiple-value-bind (standard index) (%parse-posix-offset text index)
          (if (>= index (length text))
              (%make-posix-rule :standard (- standard))
              (multiple-value-bind (ignored index) (%parse-posix-name text index)
                (declare (ignore ignored))
                (let ((daylight (- standard 3600)))
                  (when (and (< index (length text)) (char/= (char text index) #\,))
                    (multiple-value-setq (daylight index) (%parse-posix-offset text index)))
                  (if (and (< index (length text)) (char= (char text index) #\,))
                      (multiple-value-bind (start index)
                          (%parse-posix-transition text (1+ index))
                        (unless (and (< index (length text)) (char= (char text index) #\,))
                          (%ax-error "missing daylight end in ~s" text))
                        (multiple-value-bind (end index)
                            (%parse-posix-transition text (1+ index))
                          (declare (ignore index))
                          (%make-posix-rule :standard (- standard)
                                            :daylight (- daylight)
                                            :start start
                                            :end end)))
                      (%make-posix-rule :standard (- standard))))))))
    (axllm:ax-error (condition) (error condition))
    (error () nil)))

(defun %days-from-civil (year month day)
  "The day number of YEAR-MONTH-DAY counted from 1970-01-01."
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (year-of-era (- year (* era 400)))
         (month-prime (if (> month 2) (- month 3) (+ month 9)))
         (day-of-year (+ (floor (+ (* 153 month-prime) 2) 5) (1- day)))
         (day-of-era (+ (* year-of-era 365) (floor year-of-era 4)
                        (- (floor year-of-era 100)) day-of-year)))
    (+ (* era 146097) day-of-era -719468)))

(defun %leap-year-p (year)
  (and (zerop (mod year 4))
       (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun %posix-transition-seconds (date year)
  "The local second of DATE in YEAR, counted from that year's start."
  (let ((year-start (%days-from-civil year 1 1)))
    (* 86400
       (- (ecase (first date)
            (:month
             (destructuring-bind (month week weekday) (rest date)
               (let* ((first-of-month (%days-from-civil year month 1))
                      ;; 1970-01-01 was a Thursday, so day 0 is weekday 4.
                      (first-weekday (mod (+ first-of-month 4) 7))
                      (first-match (+ first-of-month
                                      (mod (- weekday first-weekday) 7)))
                      (days-in-month (- (%days-from-civil year (1+ month) 1)
                                        first-of-month))
                      (candidate (+ first-match (* 7 (1- week)))))
                 ;; Week 5 means the last such weekday, however many there are.
                 (loop while (>= (- candidate first-of-month) days-in-month)
                       do (decf candidate 7))
                 candidate)))
            (:julian-one
             ;; Day 1 to 365, never counting a leap day.
             (let ((day (second date)))
               (+ year-start (1- day)
                  (if (and (%leap-year-p year) (>= day 60)) 1 0))))
            (:julian-zero (+ year-start (second date))))
          year-start))))

(defun %posix-rule-offset (rule seconds)
  "RULE's UTC offset at SECONDS, an instant in epoch seconds."
  (if (null (posix-rule-daylight rule))
      (posix-rule-standard rule)
      (let* ((standard (posix-rule-standard rule))
             (daylight (posix-rule-daylight rule))
             (approximate-year (nth-value 0 (%civil-from-days (floor seconds 86400))))
             (changes '()))
        ;; Build the transitions of the years around this instant, so a
        ;; southern-hemisphere rule and a late-December instant both land
        ;; on the right side of a change.
        (loop for year from (1- approximate-year) to (1+ approximate-year)
              do (let ((year-start (* 86400 (%days-from-civil year 1 1))))
                   (push (cons (+ year-start
                                  (%posix-transition-seconds
                                   (first (posix-rule-start rule)) year)
                                  (second (posix-rule-start rule))
                                  (- standard))
                               daylight)
                         changes)
                   (push (cons (+ year-start
                                  (%posix-transition-seconds
                                   (first (posix-rule-end rule)) year)
                                  (second (posix-rule-end rule))
                                  (- daylight))
                               standard)
                         changes)))
        (setf changes (sort (nreverse changes) #'< :key #'car))
        (let ((offset nil))
          (dolist (change changes)
            (when (<= (car change) seconds)
              (setf offset (cdr change))))
          (or offset
              ;; Before the earliest change in view, the other state held.
              (let ((earliest (cdr (first changes))))
                (if (= earliest daylight) standard daylight)))))))

(defun %zone-offset (zone seconds)
  "ZONE's UTC offset in seconds at SECONDS, an instant in epoch seconds."
  (let* ((transitions (zone-transitions zone))
         (count (length transitions)))
    (cond ((or (zerop count) (< seconds (aref transitions 0)))
           (zone-standard zone))
          ;; After the last recorded transition the footer rule governs,
          ;; which is the whole point of reading the footer: a zone file
          ;; stops recording transitions in 2037.
          ((and (>= seconds (aref transitions (1- count))) (zone-rule zone))
           (%posix-rule-offset (zone-rule zone) seconds))
          (t
           (let ((low 0) (high (1- count)))
             ;; The last transition at or before this instant.
             (loop while (< low high)
                   do (let ((middle (ceiling (+ low high) 2)))
                        (if (<= (aref transitions middle) seconds)
                            (setf low middle)
                            (setf high (1- middle)))))
             (aref (zone-offsets zone) (aref (zone-indexes zone) low)))))))

(defun %find-zone (name)
  "The zone called NAME, read once and remembered."
  (multiple-value-bind (zone known) (gethash name *zone-cache*)
    (if known
        zone
        (setf (gethash name *zone-cache*) (%read-zone name)))))

(defun core-date-zone-offset (name epoch-ms)
  "The UTC offset in seconds of IANA zone NAME at EPOCH-MS.

Read from the platform's zone database: the 64-bit transition block for
recorded history, and the POSIX rule in the file's footer for instants
after the last recorded transition, which is how an instant in 2040 gets
the daylight saving offset its zone will actually use. An unknown zone is
an error, which is how Core detects that a name is not a zone."
  (let* ((zone-name (core-js-text name))
         (zone (%find-zone zone-name)))
    (unless zone
      (%ax-error "unknown time zone ~a" zone-name))
    (%zone-offset zone
                  (min (max (floor (%core-double epoch-ms "intrinsic.date.zone_offset") 1000)
                            +date-min-seconds+)
                       +date-max-seconds+))))

(defun %civil-from-days (days)
  "The year, month and day DAYS after 1970-01-01.

Howard Hinnant's civil-from-days, which is exact for any day and does not
need a calendar library."
  (let* ((shifted (+ days 719468))
         (era (floor (if (minusp shifted) (- shifted 146096) shifted) 146097))
         (day-of-era (- shifted (* era 146097)))
         (year-of-era (floor (- day-of-era
                                (floor day-of-era 1460)
                                (- (floor day-of-era 36524))
                                (floor day-of-era 146096))
                             365))
         (year (+ year-of-era (* era 400)))
         (day-of-year (- day-of-era (+ (* 365 year-of-era)
                                       (floor year-of-era 4)
                                       (- (floor year-of-era 100)))))
         (month-prime (floor (+ (* 5 day-of-year) 2) 153))
         (day (1+ (- day-of-year (floor (+ (* 153 month-prime) 2) 5))))
         (month (+ month-prime (if (< month-prime 10) 3 -9))))
    (values (if (<= month 2) (1+ year) year) month day)))

(defun core-date-iso-text (epoch-ms)
  "EPOCH-MS as Date.prototype.toISOString writes it."
  (let* ((millis (floor (%core-number epoch-ms "core.date_iso_text")))
         (days (floor millis 86400000))
         (rest (- millis (* days 86400000))))
    (multiple-value-bind (year month day) (%civil-from-days days)
      (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D.~3,'0DZ"
              year month day
              (floor rest 3600000)
              (floor (mod rest 3600000) 60000)
              (floor (mod rest 60000) 1000)
              (mod rest 1000)))))

(defun core-date-json (value)
  "VALUE with every host date in it replaced by its ISO text.

Returns the converted value and whether anything was replaced; when
nothing was, the original value comes back as it is.

This exists because a date reaches the prompt in two different shapes. A
date in a date-typed field is rendered by CORE-DATE-PROMPT-TEXT, which
follows the reference implementation's special cases: the UTC day for a
date field, the instant to the second for a datetime field. Anything else
is stringified, and in the reference implementation that means
JSON.stringify, which calls Date.prototype.toJSON on each date and so
writes the full ISO instant including milliseconds. A date inside an array
or a nested object therefore renders differently from a scalar one, in
every port, and this is that second rule:

  JSON.stringify([d], null, 2) is [\\n  \"2024-05-09T15:30:45.123Z\"\\n]

Only dates are touched. A value that is not JSON and not a date is left
exactly as it was, so the encoder still refuses it by name rather than
being quietly widened to accept whatever a caller passed. Use it as
(CORE-JSON-PRETTY (CORE-DATE-JSON value)) where the reference
implementation calls JSON.stringify(value, null, 2)."
  (let ((millis (core-date-millis value)))
    (cond ((not (eq millis :null))
           (values (core-date-iso-text millis) t))
          ((hash-table-p value)
           (let ((changed nil)
                 (out (core-new-map)))
             (let ((kind (gethash :record value)))
               (when kind (setf (gethash :record out) kind)))
             (dolist (key (axllm::%object-keys value))
               (multiple-value-bind (item item-changed)
                   (core-date-json (gethash key value))
                 (when item-changed (setf changed t))
                 (axllm::%set-key out key item)))
             (if changed (values out t) (values value nil))))
          ((core-array-p value)
           (let ((changed nil)
                 (out (core-new-list)))
             (loop for item across value
                   do (multiple-value-bind (converted item-changed) (core-date-json item)
                        (when item-changed (setf changed t))
                        (vector-push-extend converted out)))
             (if changed (values out t) (values value nil))))
          (t (values value nil)))))

(defun core-date-prompt-text (type-name value)
  "VALUE's prompt text in a TYPE-NAME field, or :NULL when it is no date.

A date field renders the UTC day, a datetime the instant to the second,
and a range the pretty JSON of its bounds, which is what the reference
implementation writes for a Date in each of those fields. :NULL means the
value is not a host date and the caller should render it as any other
value."
  (let ((type-name (core-js-text type-name)))
    (cond ((or (string= type-name "date") (string= type-name "datetime"))
           (let ((millis (core-date-millis value)))
             (if (eq millis :null)
                 :null
                 (let ((iso (core-date-iso-text millis)))
                   (if (string= type-name "date")
                       (subseq iso 0 (position #\T iso))
                       (concatenate 'string (subseq iso 0 (- (length iso) 5)) "Z"))))))
          ((and (or (string= type-name "dateRange") (string= type-name "datetimeRange"))
                (hash-table-p value)
                (%has-key value "start")
                (%has-key value "end"))
           (let ((start (core-date-millis (gethash "start" value)))
                 (end (core-date-millis (gethash "end" value)))
                 (day (string= type-name "dateRange")))
             (flet ((bound (millis)
                      (let ((iso (core-date-iso-text millis)))
                        (if day
                            (subseq iso 0 10)
                            (concatenate 'string (subseq iso 0 (- (length iso) 5)) "Z")))))
               (cond ((and (not (eq start :null)) (not (eq end :null)))
                      (core-json-pretty (axllm:object "start" (bound start)
                                                      "end" (bound end))))
                     ((some (lambda (key) (not (eq (core-date-millis (gethash key value)) :null)))
                            (axllm::%object-keys value))
                      (let ((dated (core-new-map)))
                        (dolist (key (axllm::%object-keys value))
                          (let ((millis (core-date-millis (gethash key value))))
                            (core-set dated key (if (eq millis :null)
                                                    (gethash key value)
                                                    (core-date-iso-text millis)))))
                        (core-json-pretty dated)))
                     (t :null)))))
          (t :null))))

;;; ------------------------------------------------------------------
;;; Host regular expressions
;;; ------------------------------------------------------------------

;;; Core owns the matcher that validates a tool's raw argument schema: it
;;; walks UTF-16 units itself in ir/axcore/regex.axir, so lookaround and
;;; backreferences behave the same on every target. These two boundaries
;;; are the unrelated host-engine operations the other ports also keep
;;; native, and they must not be confused with that matcher.

(defun %js-replacement (replacement)
  "REPLACEMENT, written with JavaScript's $ forms, as a CL-PPCRE template.

$$ writes one dollar, $& the whole match, and $1 through $99 a group.
$<name> has no CL-PPCRE equivalent and is refused rather than written out
literally, which is what a port that ignored it would do."
  (let ((text (core-js-text replacement))
        (parts '())
        (literal (make-string-output-stream))
        (index 0))
    (flet ((flush ()
             (let ((chunk (get-output-stream-string literal)))
               (when (plusp (length chunk)) (push chunk parts)))))
      (loop with limit = (length text)
            while (< index limit)
            do (let ((character (char text index)))
                 (if (and (char= character #\$) (< (1+ index) limit))
                     (let ((next (char text (1+ index))))
                       (cond ((char= next #\$)
                              (write-char #\$ literal)
                              (incf index 2))
                             ((char= next #\&)
                              (flush)
                              (push :match parts)
                              (incf index 2))
                             ((char= next #\<)
                              (%ax-error "intrinsic.regex.replace: named group reference ~a is not supported"
                                         (subseq text index)))
                             ((digit-char-p next)
                              (let ((end (if (and (< (+ index 2) limit)
                                                  (digit-char-p (char text (+ index 2))))
                                             (+ index 3)
                                             (+ index 2))))
                                (flush)
                                ;; CL-PPCRE numbers the registers in a
                                ;; replacement list from zero, so $1 is 0.
                                (push (1- (parse-integer text :start (1+ index) :end end))
                                      parts)
                                (setf index end)))
                             (t (write-char character literal) (incf index))))
                     (progn (write-char character literal) (incf index)))))
      (flush))
    ;; CL-PPCRE reads an empty replacement list as a function to call, so
    ;; an empty replacement has to be one empty string.
    (or (nreverse parts) (list ""))))

(defun core-regex-replace (pattern replacement value)
  "VALUE with every match of PATTERN replaced, as String.replace with /g.

The pattern goes to the host engine, as it does in every other port; it is
not the Core matcher, which exists for raw tool-argument schemas."
  (cl-ppcre:regex-replace-all (%scanner (core-js-text pattern))
                              (core-js-text value)
                              (%js-replacement replacement)))

;;; ------------------------------------------------------------------
;;; Runtime errors
;;; ------------------------------------------------------------------

(defun core-runtime-error (message)
  "A runtime error, as a condition object for CORE.RAISE to signal."
  (make-condition 'axllm:ax-error :message (core-js-text message)))

(defun core-exception-message (exception)
  "EXCEPTION's message text, for Core code that reports a failure.

An Ax condition answers with the message it was built with, unchanged: a
flow or a retry path puts this text in a result a caller reads, so losing
or decorating it would change what Ax reports. Any other condition answers
with its printed form, which is what a condition's message is in Lisp and
what str(error) gives in the Python port. An error Core is holding as a
value answers from its message key, as it does in the Go port, and a plain
string is already the message."
  (cond ((typep exception 'axllm:ax-error) (axllm:ax-error-message exception))
        ((typep exception 'condition) (princ-to-string exception))
        ((stringp exception) exception)
        ((hash-table-p exception) (core-js-text (core-get exception "message" exception)))
        (t (core-js-text exception))))

;;; ------------------------------------------------------------------
;;; Coverage marks
;;; ------------------------------------------------------------------

(defvar *coverage-marks* (make-hash-table :test 'equal :synchronized t)
  "Names already written, so a hot Core function appends once.")

(defun core-coverage-mark (name)
  "Record that Core function NAME ran, when coverage collection is on.

Generated Core code calls this at the head of each function. It writes to
the file named by AXIR_COVERAGE_FILE, one name per line, the first time it
sees a name; with that variable unset it does nothing, which is how a
normal run pays nothing for the hook. The point is to tell a fixture that
exercised real Core from one that only looked like it did."
  (let ((path (uiop:getenv "AXIR_COVERAGE_FILE"))
        (name (core-js-text name)))
    (when (and path (plusp (length path))
               (not (nth-value 1 (gethash name *coverage-marks*))))
      (setf (gethash name *coverage-marks*) t)
      (handler-case
          (with-open-file (out path :direction :output
                                    :external-format :utf-8
                                    :if-exists :append
                                    :if-does-not-exist :create)
            (write-line name out))
        ;; Coverage collection must never change what a run does.
        (error () nil))))
  :null)
