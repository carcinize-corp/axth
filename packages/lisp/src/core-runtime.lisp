;;;; core-runtime.lisp --- the native boundaries generated Core code calls.
;;;;
;;;; src/core.lisp is generated from ir/axcore and holds Ax's semantics. It
;;;; calls out to this file for everything a portable IR cannot express:
;;;; value tests, string and collection primitives, the record
;;;; constructors, and the two error constructors. The generated file's
;;;; header lists exactly which of these functions it uses, so nothing here
;;;; is speculative.
;;;;
;;;; Every function is a boundary, not a reimplementation of behavior. Where
;;;; a primitive has an observable rule, that rule comes from Ax's reference
;;;; semantics (JavaScript's string, number and truthiness behavior) and is
;;;; stated in the docstring, so a reader can check it against the other
;;;; ports instead of guessing.

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

(defun core-add (left right)
  "Numeric addition, or string concatenation when both sides are strings."
  (cond ((and (realp left) (realp right)) (+ left right))
        ((and (stringp left) (stringp right)) (concatenate 'string left right))
        (t (%ax-error "intrinsic.add: cannot add ~S and ~S" left right))))

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

(defun core-key-alias (key)
  "KEY's other spelling: snake_case for a camelCase key and vice versa.

Ax's IR reads snake_case record keys, while JSON that reaches Ax from a
caller or a fixture uses camelCase. CORE-GET tries the alias after the
exact key, the same way the Go port does, so one record serves both."
  (unless (stringp key)
    (return-from core-key-alias nil))
  (if (find #\_ key)
      (let ((parts (cl-ppcre:split "_" key)))
        (when (rest parts)
          (apply #'concatenate 'string
                 (first parts)
                 (mapcar (lambda (part)
                           (if (plusp (length part))
                               (concatenate 'string (string (char-upcase (char part 0))) (subseq part 1))
                               part))
                         (rest parts)))))
      (let ((snake (cl-ppcre:regex-replace-all "([a-z0-9])([A-Z])" key "\\1_\\2")))
        (unless (string= snake key)
          (string-downcase snake)))))

(defun core-get (target key &optional (fallback :null))
  "TARGET's value at KEY, or FALLBACK when it is absent.

An absent key yields FALLBACK; a key present with a null value yields that
null. The two are different in Core, so they stay different here."
  (cond ((hash-table-p target)
         (multiple-value-bind (value found) (gethash key target)
           (if found
               value
               (let ((alias (core-key-alias key)))
                 (if alias
                     (multiple-value-bind (value found) (gethash alias target)
                       (if found value fallback))
                     fallback)))))
        ((core-array-p target)
         (if (and (integerp key) (< -1 key (length target))) (aref target key) fallback))
        ((stringp target)
         (if (and (integerp key) (< -1 key (length target)))
             (string (char target key))
             fallback))
        (t fallback)))

(defun core-set (target key value)
  (unless (hash-table-p target)
    (%ax-error "core.set: ~S is not an object" target))
  (axllm::%set-key target key value)
  target)

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
  (cond ((stringp value) (length value))
        ((core-array-p value) (length value))
        ((hash-table-p value) (hash-table-count value))
        ((listp value) (length value))
        (t (%ax-error "intrinsic.len: ~S has no length" value))))

(defun core-contains (container item)
  "Whether CONTAINER holds ITEM: a substring, an element, or a key."
  (core-bool
   (cond ((eq container :null) nil)
         ((stringp container)
          (and (stringp item) (search item container)))
         ((core-array-p container) (find item container :test #'core-value-equal))
         ((hash-table-p container) (nth-value 1 (gethash item container)))
         ((consp container) (member item container :test #'core-value-equal))
         (t nil))))

(defun core-list-get (values index &optional (default :null))
  (if (and (core-array-p values) (integerp index) (< -1 index (length values)))
      (aref values index)
      default))

(defun core-map-contains (values key)
  (core-bool (and (hash-table-p values) (nth-value 1 (gethash key values)))))

(defun core-map-keys (values)
  (let ((out (core-new-list)))
    (when (hash-table-p values)
      (dolist (key (axllm::%object-keys values))
        (vector-push-extend key out)))
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

An integral value keeps integer form, so a minimum of 18 renders as 18.
Magnitudes at or beyond 1e21 and below 1e-6 use Lisp exponent notation
normalised to e, which Ax's signature constraints never reach."
  (cond ((integerp value) (format nil "~D" value))
        ((not (realp value)) (princ-to-string value))
        (t (axllm::%json-number-text value))))

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
        ((or (core-array-p value) (hash-table-p value)) (axllm:encode-json value))
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

(defvar *regex-cache* (make-hash-table :test 'equal :synchronized t)
  "Compiled scanners, keyed by pattern source.")

(defun %scanner (pattern)
  (or (gethash pattern *regex-cache*)
      (setf (gethash pattern *regex-cache*) (cl-ppcre:create-scanner pattern))))

(defun core-regex-match (pattern value)
  "Whether PATTERN matches anywhere in VALUE. A non-string never matches."
  (core-bool (and (stringp value)
                  (cl-ppcre:scan (%scanner (core-js-text pattern)) value)
                  t)))

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
;;; JSON
;;; ------------------------------------------------------------------

(defun core-json-parse (value)
  (axllm:parse-json (core-js-text value)))

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
  (if (hash-table-p attrs) (core-get attrs key fallback) fallback))

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
