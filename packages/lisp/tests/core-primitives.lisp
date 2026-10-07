;;;; core-primitives.lisp --- the native Core boundaries, checked directly.
;;;;
;;;; src/core-runtime.lisp holds every operation a portable IR cannot
;;;; express: value tests, arithmetic, strings, UTF-16, JSON, records, the
;;;; media and URL shape tests, digests, zones and the host regular
;;;; expression operations. Generated Core code is only as correct as those
;;;; boundaries, and a signature fixture exercises barely a third of them,
;;;; so this file checks them on their own.
;;;;
;;;; Two kinds of check live here.
;;;;
;;;; RUN-PURE-FIXTURE runs the shared AxIR fixtures whose whole content is
;;;; one of these boundaries: ir/conformance/prompt/number-format-cases,
;;;; json-stringify-cases and string-format-cases. Those expectations were
;;;; recorded from the TypeScript implementation, so nothing here is
;;;; asserted against this port's own output.
;;;;
;;;; The rest are direct checks of the rules that no shared fixture pins
;;;; down, chosen where a plausible wrong implementation would differ from
;;;; a right one: null against false against empty, a surrogate pair split
;;;; across two stream chunks, a key order that JavaScript reorders, a copy
;;;; that must not share structure, and the one place where this port
;;;; deliberately refuses to alias a key.

(defpackage #:axllm/tests/core-primitives
  (:use #:cl)
  (:local-nicknames (#:core #:axllm/core))
  (:export #:run-core-primitive-tests #:run-pure-fixture))

(in-package #:axllm/tests/core-primitives)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(defvar *failures* '())
(defvar *checks* 0)

(defun fail (format-control &rest arguments)
  (push (apply #'format nil format-control arguments) *failures*))

(defun show (value)
  (cond ((stringp value) (format nil "~s" value))
        ((eq value :null) "null")
        ((eq value 'yason:true) "true")
        ((eq value 'yason:false) "false")
        ((or (hash-table-p value) (and (vectorp value) (not (stringp value))))
         (ax:encode-json value))
        (t (format nil "~s" value))))

(defmacro check (label expected actual)
  "Check that ACTUAL equals EXPECTED as a Core value, naming LABEL."
  (let ((want (gensym)) (got (gensym)))
    `(let ((,want ,expected))
       (incf *checks*)
       (handler-case
           (let ((,got ,actual))
             (unless (core::core-value-equal ,got ,want)
               (fail "~a: expected ~a, got ~a" ,label (show ,want) (show ,got))))
         (error (condition)
           (fail "~a: raised ~a: ~a" ,label (type-of condition) condition))))))

(defun same-object-p (got want)
  "Whether GOT is the same Lisp object as WANT, for an identity check.

EQL, widened to two comparisons it is unreliable for. Two equal strings
need not be EQL, and neither need two equal bignums, which would make a
check pass or fail depending on whether the file was compiled or loaded as
source and on the word size. A number keeps its exactness in the
comparison, so an integer is still not the same answer as a float, and 0
is still not the same answer as false."
  (or (eql got want)
      (and (stringp got) (stringp want) (string= got want))
      (and (numberp got) (numberp want)
           (= got want)
           (eq (not (floatp got)) (not (floatp want))))))

(defmacro check-same (label expected actual)
  "Check that ACTUAL is the same value as EXPECTED: false and 0 stay apart."
  (let ((want (gensym)) (got (gensym)))
    `(let ((,want ,expected))
       (incf *checks*)
       (handler-case
           (let ((,got ,actual))
             (unless (same-object-p ,got ,want)
               (fail "~a: expected ~a, got ~a" ,label (show ,want) (show ,got))))
         (error (condition)
           (fail "~a: raised ~a: ~a" ,label (type-of condition) condition))))))

(defmacro check-raises (label &body body)
  "Check that BODY signals an Ax error rather than answering."
  `(progn
     (incf *checks*)
     (handler-case (let ((value (progn ,@body)))
                     (fail "~a: expected a failure, got ~a" ,label (show value)))
       (ax:ax-error () nil)
       (error (condition)
         (fail "~a: expected an Ax error, got ~a: ~a" ,label (type-of condition) condition)))))

;;; ------------------------------------------------------------------
;;; Shared pure fixtures
;;; ------------------------------------------------------------------

(defun conformance-directory ()
  (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
    (if (and override (plusp (length override)))
        (uiop:ensure-directory-pathname override)
        (asdf:system-relative-pathname "axllm" "../../ir/conformance/"))))

(defun %double-from-text (text)
  "TEXT as the double the fixtures mean by it.

The shared number cases are JavaScript number text read the way the other
ports read it: Python's float(), Go's ParseFloat. The three non-finite
spellings have no literal syntax in Lisp, and an integer literal is
projected to binary64, which is the whole point of cases such as
9007199254740993."
  (cond ((string= text "NaN") core::+double-nan+)
        ((string= text "Infinity") sb-ext:double-float-positive-infinity)
        ((string= text "-Infinity") sb-ext:double-float-negative-infinity)
        ((string= text "-0") (- 0d0))
        (t (core::core-js-number (axllm::%json-number-value text)))))

(defun run-number-format (fixture)
  "Every case's String(x) and JSON.stringify(x), through this port's
boundaries: string.str, string.format's {}, json.stringify, json.pretty
and json.stable_stringify all write the same number the same way."
  (loop for case across (ax:jget fixture "cases")
        do (let* ((input (ax:jget case "input"))
                  (number (%double-from-text input))
                  (listed (format nil "[~a]" (ax:jget case "json"))))
             (check (format nil "String(~a)" input)
                    (ax:jget case "string") (core::core-string-str number))
             (check (format nil "format {} of ~a" input)
                    (ax:jget case "string") (core::core-string-format "{}" number))
             (check (format nil "json.stringify [~a]" input)
                    listed (core::core-json-stringify (vector number)))
             (check (format nil "json.stable_stringify [~a]" input)
                    listed (core::core-json-stable-stringify (vector number)))
             (check (format nil "json.pretty [~a]" input)
                    (format nil "[~%  ~a~%]" (ax:jget case "json"))
                    (core::core-json-pretty (vector number))))))

(defun run-json-stringify (fixture)
  "Every case's parsed input back out of json.stringify, which must put
array-index keys first in numeric order and keep the rest as written."
  (loop for case across (ax:jget fixture "cases")
        do (let ((input (ax:jget case "input")))
             (check (format nil "json.stringify ~a" input)
                    (ax:jget case "json")
                    (core::core-json-stringify (ax:parse-json input))))))

(defun run-string-format (fixture)
  "Every string.format and string.str case."
  (loop for case across (ax:jget fixture "format_cases")
        do (let ((template (ax:jget case "template"))
                 (arguments (coerce (ax:jget case "input") 'list)))
             (check (format nil "format ~s" template)
                    (ax:jget case "expected")
                    (apply #'core::core-string-format template arguments))))
  (loop for case across (ax:jget fixture "str_cases")
        do (check (format nil "str ~a" (show (ax:jget case "input")))
                  (ax:jget case "expected")
                  (core::core-string-str (ax:jget case "input")))))

(defparameter +pure-fixture-runners+
  '(("number_format" . run-number-format)
    ("json_stringify" . run-json-stringify)
    ("string_format" . run-string-format))
  "The shared fixture kinds that are entirely native boundaries.")

(defun run-pure-fixture (fixture)
  "Run FIXTURE, a shared fixture whose content is one of these boundaries.

An unclaimed kind fails naming itself, so a fixture nobody runs can never
be mistaken for a passing one."
  (let* ((kind (ax:jget fixture "kind"))
         (runner (cdr (assoc kind +pure-fixture-runners+ :test #'equal))))
    (unless runner
      (fail "fixture kind ~s is not a pure Core boundary kind" kind)
      (return-from run-pure-fixture nil))
    (funcall runner fixture)
    t))

(defun run-pure-fixtures ()
  (dolist (name '("number-format-cases" "json-stringify-cases" "string-format-cases"))
    (let ((path (merge-pathnames (format nil "prompt/~a.json" name) (conformance-directory))))
      (if (probe-file path)
          (let ((before *failures*))
            (run-pure-fixture (ax:parse-json (uiop:read-file-string path)))
            (when (eq before *failures*)
              (axllm/conformance:record-result "prompt" path :semantic)))
          (fail "shared fixture ~a is missing at ~a" name path)))))

;;; ------------------------------------------------------------------
;;; Null, false and empty are three different values
;;; ------------------------------------------------------------------

(defun run-absence-tests ()
  ;; Truthiness. "0" and " " are true because they are non-empty strings,
  ;; which is where a port that leaned on its own language's rules would
  ;; disagree.
  (dolist (case (list (list :null nil "null")
                      (list ax:false nil "false")
                      (list ax:true t "true")
                      (list "" nil "empty string")
                      (list "0" t "the string zero")
                      (list " " t "a space")
                      (list 0 nil "zero")
                      (list 0d0 nil "zero as a double")
                      (list 1 t "one")
                      (list (ax:object) nil "empty object")
                      (list (vector) nil "empty array")
                      (list (vector :null) t "array holding null")))
    (destructuring-bind (value truth label) case
      (check-same (format nil "truthy ~a" label)
                  (if truth 'yason:true 'yason:false)
                  (core::core-truthy value))))

  ;; Null is null; false, zero and NIL are not.
  (check-same "is_none null" ax:true (core::core-is-none :null))
  (check-same "is_none false" ax:false (core::core-is-none ax:false))
  (check-same "is_none zero" ax:false (core::core-is-none 0))
  (check-same "is_none empty string" ax:false (core::core-is-none ""))
  (check-same "is_none NIL" ax:false (core::core-is-none nil))
  (check-same "is_not_none false" ax:true (core::core-is-not-none ax:false))

  ;; An absent key and a key holding null are different reads.
  (let ((object (ax:object "present" :null "flag" ax:false)))
    (check-same "absent key takes the fallback" :missing (core::core-get object "absent" :missing))
    (check-same "a null value is not the fallback" :null (core::core-get object "present" :missing))
    (check-same "a false value survives the read" ax:false (core::core-get object "flag" :missing))
    (check-same "map.contains sees a null value" ax:true (core::core-map-contains object "present"))
    (check-same "map.contains misses an absent key" ax:false (core::core-map-contains object "absent")))

  ;; Coalesce replaces only null.
  (check-same "coalesce leaves false alone" ax:false (core::core-coalesce ax:false "fallback"))
  (check-same "coalesce leaves the empty string alone" "" (core::core-coalesce "" "fallback"))
  (check-same "coalesce leaves zero alone" 0 (core::core-coalesce 0 "fallback"))
  (check-same "coalesce replaces null" "fallback" (core::core-coalesce :null "fallback"))

  ;; Equality keeps them apart, including the pairs a looser language would
  ;; merge: 0 and false, "" and false, null and NIL.
  (check-same "null is not false" ax:false (core::core-eq :null ax:false))
  (check-same "zero is not false" ax:false (core::core-eq 0 ax:false))
  (check-same "the empty string is not false" ax:false (core::core-eq "" ax:false))
  (check-same "null is not NIL" ax:false (core::core-eq :null nil))
  (check-same "false equals false" ax:true (core::core-eq ax:false ax:false))
  (check-same "2 equals 2.0" ax:true (core::core-eq 2 2d0))

  ;; Length treats null as empty and never counts a record's marker.
  (check-same "len of null" 0 (core::core-len :null))
  (check-same "len of the empty string" 0 (core::core-len ""))
  (check-same "len of an empty object" 0 (core::core-len (ax:object)))
  ;; A Field has name, type, description, title and three flags; its
  ;; internal record marker is not a key.
  (check-same "len of a Field record counts only its keys"
              7 (core::core-len (core::core-record-new "Field" (ax:object "name" "x"))))

  ;; The wire forms.
  (check "null encodes as null" "null" (ax:encode-json :null))
  (check "false encodes as false" "false" (ax:encode-json ax:false))
  (check "an empty object encodes as {}" "{}" (ax:encode-json (ax:object)))
  (check "an empty array encodes as []" "[]" (ax:encode-json (vector)))
  (check-same "null parses as :NULL, not NIL" :null (ax:parse-json "null"))
  (check-same "false parses as the false symbol" ax:false (ax:parse-json "false")))

;;; ------------------------------------------------------------------
;;; UTF-16, astral characters and split surrogate pairs
;;; ------------------------------------------------------------------

(defun run-unicode-tests ()
  (let ((grin (string (code-char #x1f600)))      ; U+1F600, one code point
        (high (string (code-char #xd83d)))       ; its leading surrogate
        (low (string (code-char #xde00))))       ; its trailing surrogate
    ;; A code point count and a UTF-16 unit count differ for an astral
    ;; character, which is the whole reason Core asks for units.
    (check-same "codepoint length of an astral character" 1
                (core::core-string-codepoint-length grin))
    (check "utf16 units of an astral character" (vector #xd83d #xde00)
           (core::core-string-utf16-units grin))
    (check-same "codepoint length around it" 3
                (core::core-string-codepoint-length (concatenate 'string "a" grin "b")))
    (check "utf16 units around it"
           (vector (char-code #\a) #xd83d #xde00 (char-code #\b))
           (core::core-string-utf16-units (concatenate 'string "a" grin "b")))
    ;; A lone surrogate is one unit and one code point, as in a JS string.
    (check "utf16 units of a lone high surrogate" (vector #xd83d)
           (core::core-string-utf16-units high))

    ;; Stream chunks: a pair split across two events joins back into one
    ;; character, and nothing else is joined.
    (check-same "a split pair rejoins into one character" 1
                (length (core::core-string-concat-stream-text high low)))
    (check "the rejoined character is the original" grin
           (core::core-string-concat-stream-text high low))
    (check "a high surrogate before ordinary text is left alone"
           (concatenate 'string high "a")
           (core::core-string-concat-stream-text high "a"))
    (check "a low surrogate after ordinary text is left alone"
           (concatenate 'string "a" low)
           (core::core-string-concat-stream-text "a" low))
    (check "ordinary chunks just concatenate" "ab"
           (core::core-string-concat-stream-text "a" "b"))
    (check "concatenating keeps a whole astral character whole"
           (concatenate 'string grin grin)
           (core::core-string-concat-stream-text grin grin))

    ;; Dropping a dangling high surrogate, and only that.
    (check "a trailing high surrogate is dropped" "ab"
           (core::core-string-drop-trailing-high-surrogate (concatenate 'string "ab" high)))
    (check "a trailing low surrogate is kept"
           (concatenate 'string "ab" low)
           (core::core-string-drop-trailing-high-surrogate (concatenate 'string "ab" low)))
    (check "a complete astral character is kept"
           (concatenate 'string "ab" grin)
           (core::core-string-drop-trailing-high-surrogate (concatenate 'string "ab" grin)))
    (check "an empty string survives" ""
           (core::core-string-drop-trailing-high-surrogate ""))

    ;; String operations index code points, so an astral character is one
    ;; position rather than two.
    (check "slice counts an astral character once" "b"
           (core::core-string-slice (concatenate 'string grin "b") 1))
    (check-same "index_of counts an astral character once" 1
                (core::core-string-index-of (concatenate 'string grin "b") "b"))
    (check-same "index_of reports a miss as -1" -1
                (core::core-string-index-of "abc" "z"))
    (check-same "index_of honours a start" 2
                (core::core-string-index-of "abab" "ab" 1))

    ;; The wire: a whole character is written as text, a lone surrogate as
    ;; the escape JSON.stringify writes, and a lone surrogate is still
    ;; refused on the way in.
    (check "an astral character encodes as text"
           (format nil "\"~a\"" grin) (ax:encode-json grin))
    (check "a lone surrogate encodes as an escape" "\"\\ud83d\"" (ax:encode-json high))
    (check "a surrogate pair parses into one character" grin
           (ax:parse-json "\"\\uD83D\\uDE00\""))
    ;; And the escape reads back, or streamed text that ends mid pair
    ;; cannot survive an encode and parse round trip. JSON.parse accepts an
    ;; unpaired surrogate escape as well.
    (check "a lone surrogate escape parses to that unit" high
           (ax:parse-json "\"\\uD83D\""))
    (check "a lone surrogate survives a round trip" high
           (ax:parse-json (ax:encode-json high)))
    (check "so does a lone low surrogate" low
           (ax:parse-json (ax:encode-json low)))
    (check "and a half pair inside ordinary text"
           (concatenate 'string "a" high "b")
           (ax:parse-json (ax:encode-json (concatenate 'string "a" high "b"))))

    ;; Percent-encoding and digests run over UTF-8 bytes.
    (check "an astral character percent-encodes as four bytes" "%F0%9F%98%80"
           (core::core-url-encode-component grin))
    (check "a non-ASCII digest hashes the UTF-8 bytes"
           "a53c56966616f0ec3ed9db7ea07a6034430149528bbaba8966c284f9373669c2"
           (core::core-crypto-sha256-hex (concatenate 'string "h" (string (code-char #xe9))
                                                      "llo " grin)))))

;;; ------------------------------------------------------------------
;;; Numbers
;;; ------------------------------------------------------------------

(defun run-number-tests ()
  ;; The boundaries of JavaScript's plain decimal range, which the earlier
  ;; implementation wrote in Lisp's exponent form instead.
  (check "1e21 switches to exponent form" "1e+21" (core::core-string-str 1d21))
  (check "1e20 stays plain" "100000000000000000000" (core::core-string-str 1d20))
  (check "1e-7 switches to exponent form" "1e-7" (core::core-string-str 1d-7))
  (check "1e-6 stays plain" "0.000001" (core::core-string-str 1d-6))
  (check "a negative small number keeps its sign" "-1e-7" (core::core-string-str -1d-7))
  (check "an integral double drops its point" "18" (core::core-string-str 18d0))
  (check "the shortest round trip wins" "0.1" (core::core-string-str 0.1d0))
  (check "a subnormal prints shortest" "5e-324"
         (core::core-string-str least-positive-double-float))
  (check "the largest double prints exactly" "1.7976931348623157e+308"
         (core::core-string-str most-positive-double-float))
  (check "minus zero is zero" "0" (core::core-string-str (- 0d0)))

  ;; Non-finite values: spelled out as text, null on the wire.
  (check "NaN has a name in text" "NaN" (core::core-string-str core::+double-nan+))
  (check "infinity has a name in text" "Infinity"
         (core::core-string-str sb-ext:double-float-positive-infinity))
  (check "NaN is null on the wire" "[null]"
         (core::core-json-stringify (vector core::+double-nan+)))
  (check "infinity is null on the wire" "[null]"
         (core::core-json-stringify (vector sb-ext:double-float-negative-infinity)))

  ;; An exact Lisp integer is never narrowed behind the caller's back, and
  ;; CORE-JS-NUMBER is where a caller asks for the reference projection.
  (check "an exact integer keeps every digit" "9007199254740993"
         (ax:encode-json 9007199254740993))
  (check "the reference projection narrows it" "9007199254740992"
         (ax:encode-json (core::core-js-number 9007199254740993)))

  ;; Division never produces a ratio, which no other port and no JSON
  ;; document can carry.
  (let ((third (core::core-div 1 3)))
    (check-same "division gives a float" t (floatp third))
    (check "a third writes as a decimal" "0.3333333333333333" (core::core-string-str third)))
  (check "a zero divisor divides by one" "7" (core::core-string-str (core::core-div 7 0)))
  (check "multiplication of integers stays exact" "6" (core::core-string-str (core::core-mul 2 3)))
  (check "addition of integers stays exact" "5" (core::core-string-str (core::core-add 2 3)))
  (check "addition concatenates when a side is text" "n1" (core::core-add "n" 1))

  ;; Math outside a function's domain answers as JavaScript answers, rather
  ;; than signalling or returning a complex number.
  (check "the square root of a negative is NaN" "NaN"
         (core::core-string-str (core::core-math-sqrt -1)))
  (check "the log of zero is negative infinity" "-Infinity"
         (core::core-string-str (core::core-math-log 0)))
  (check "the log of a negative is NaN" "NaN"
         (core::core-string-str (core::core-math-log -1)))
  (check-same "is_finite rejects infinity" ax:false
              (core::core-math-is-finite sb-ext:double-float-positive-infinity))
  (check-same "is_finite rejects NaN" ax:false
              (core::core-math-is-finite core::+double-nan+))
  (check-same "is_finite accepts an integer" ax:true (core::core-math-is-finite 3))
  (check-same "floor rounds down past zero" -2 (core::core-math-floor -1.5d0))
  (check-same "abs of an integer stays exact" 7 (core::core-math-abs -7))
  (check "pow of a fractional root of a negative is NaN" "NaN"
         (core::core-string-str (core::core-math-pow -8 0.5d0)))

  ;; Randomness is injectable, which is the only way a test can pin it.
  (core::set-math-random-values '(0.25d0 0.5d0))
  (check-same "the first injected draw" 0.25d0 (core::core-math-random))
  (check-same "the second injected draw" 0.5d0 (core::core-math-random))
  (let ((drawn (core::core-math-random)))
    (check-same "a real draw lands in [0, 1)" t (and (<= 0 drawn) (< drawn 1)))))

;;; ------------------------------------------------------------------
;;; Key order and copying
;;; ------------------------------------------------------------------

(defun run-structure-tests ()
  ;; Written order is kept, because a signature's field list and a schema's
  ;; required array are both observable output.
  (let ((object (ax:object "zeta" 1 "alpha" 2 "mid" 3)))
    (check "written key order is kept" "{\"zeta\":1,\"alpha\":2,\"mid\":3}"
           (ax:encode-json object))
    (check "map.keys follows the same order" (vector "zeta" "alpha" "mid")
           (core::core-map-keys object))
    (check "map.values follows the same order" (vector 1 2 3)
           (core::core-map-values object)))

  ;; Regex capture indices are numbers in Core. They must name the same
  ;; property as their string spelling, including when copied or deleted.
  (let ((captures (core::core-new-map)))
    (core::core-set captures 2 (vector 3 7))
    (core::core-set captures 1 (vector 0 1))
    (check "numeric keys participate in key ordering" (vector "1" "2")
           (core::core-map-keys captures))
    (check "numeric get reads a string-keyed value" (vector 3 7)
           (core::core-get captures "2"))
    (check "regex copies preserve captures" (vector 0 1)
           (core::core-get (core::regex-copy-map captures) 1))
    (check "numeric keys survive deep copies" (vector 3 7)
           (core::core-map-get (core::core-deep-copy captures) 2))
    (check "numeric map membership" ax:true (core::core-map-contains captures 1))
    (check "numeric object membership" ax:true (core::core-contains captures 2))
    (core::core-map-delete captures 1)
    (check "numeric deletion removes the string property" ax:false
           (core::core-map-contains captures "1")))

  ;; JavaScript moves array-index keys to the front in numeric order, and
  ;; only canonical ones: "01", "-1", "1.5" and 2^32-1 stay ordinary keys.
  (check "index keys come first in numeric order" "{\"2\":3,\"10\":2,\"b\":1}"
         (core::core-json-stringify (ax:object "b" 1 "10" 2 "2" 3)))
  (check "only canonical index keys move"
         "{\"0\":7,\"1\":2,\"4294967294\":5,\"01\":1,\"-1\":3,\"4294967295\":4,\"1.5\":6}"
         (core::core-json-stringify
          (ax:object "01" 1 "1" 2 "-1" 3 "4294967295" 4 "4294967294" 5 "1.5" 6 "0" 7)))

  ;; A stable stringification sorts at every level; an ordinary one does not.
  (check "stable stringify sorts recursively"
         "{\"a\":{\"x\":1,\"y\":2},\"b\":2}"
         (core::core-json-stable-stringify
          (ax:object "b" 2 "a" (ax:object "y" 2 "x" 1))))
  (check "stable stringify of null is an empty object" "{}"
         (core::core-json-stable-stringify :null))

  ;; A deep copy shares nothing mutable, keeps order, and keeps a record a
  ;; record.
  (let* ((original (ax:object "outer" (ax:object "inner" (vector 1 2))))
         (copy (core::core-deep-copy original)))
    (core::core-set (core::core-get copy "outer") "inner" (vector 9))
    (check "the original is untouched by a change to the copy"
           "{\"outer\":{\"inner\":[1,2]}}" (ax:encode-json original))
    (check "the copy carries the change" "{\"outer\":{\"inner\":[9]}}" (ax:encode-json copy)))
  (let ((copy (core::core-deep-copy (ax:object "z" 1 "a" 2))))
    (check "a copy keeps the written order" "{\"z\":1,\"a\":2}" (ax:encode-json copy)))
  (let ((copy (core::core-deep-copy (core::core-record-new "Field" (ax:object "name" "x")))))
    (check "a copied Field is still a Field" "Field" (core::core-record-kind copy)))
  ;; A Lisp string is mutable, so a copy that shares one is not a deep copy:
  ;; a caller could change the original through it.
  (let* ((original (ax:object "text" (copy-seq "abc")
                              "list" (vector (copy-seq "xy"))))
         (copy (core::core-deep-copy original)))
    (setf (char (core::core-get copy "text") 0) #\z)
    (setf (char (aref (core::core-get copy "list") 0) 0) #\z)
    (check "changing a copied string leaves the original alone"
           "{\"text\":\"abc\",\"list\":[\"xy\"]}" (ax:encode-json original))
    (check "and the copy has the change"
           "{\"text\":\"zbc\",\"list\":[\"zy\"]}" (ax:encode-json copy)))

  ;; A deleted key loses its position, so writing it again puts it last.
  ;; Leaving the position recorded would read as a, b instead of b, a.
  (let ((object (ax:object "a" 1 "b" 2)))
    (core::core-map-delete object "a")
    (core::core-set object "a" 3)
    (check "a reinserted key takes the next position, not its old one"
           "{\"b\":2,\"a\":3}" (ax:encode-json object)))
  (let ((object (ax:object "a" 1 "b" 2 "c" 3)))
    (core::core-map-delete object "b")
    (check "deleting from the middle keeps the rest in order"
           "{\"a\":1,\"c\":3}" (ax:encode-json object))
    (core::core-set object "b" 9)
    (check "and the rewritten key goes last" "{\"a\":1,\"c\":3,\"b\":9}"
           (ax:encode-json object)))

  ;; Merging and updating keep the left side's order and take the right
  ;; side's values.
  (check "merge keeps order and takes the newer value"
         "{\"a\":1,\"b\":9,\"c\":3}"
         (ax:encode-json (core::core-map-merge (ax:object "a" 1 "b" 2)
                                               (ax:object "b" 9 "c" 3))))
  (let ((target (ax:object "a" 1)))
    (core::core-map-update target (ax:object "b" 2))
    (check "update writes into the target" "{\"a\":1,\"b\":2}" (ax:encode-json target)))
  (let ((target (ax:object "a" 1 "b" 2)))
    (core::core-map-delete target "a")
    (check "delete removes the key" "{\"b\":2}" (ax:encode-json target))
    (check-same "deleting an absent key is quiet" ax:false
                (core::core-map-contains (core::core-map-delete target "nope") "nope"))))

;;; ------------------------------------------------------------------
;;; Records, and the one alias this port refuses
;;; ------------------------------------------------------------------

(defun run-record-tests ()
  ;; A FieldType carries its whole declared shape, in order, with the
  ;; defaults Ax gives it.
  (let ((type (core::core-record-new "FieldType" (ax:object "name" "number"))))
    (check "a FieldType has its full shape, in order"
           (concatenate 'string
                        "{\"name\":\"number\",\"is_array\":false,\"options\":null,"
                        "\"fields\":null,\"min_length\":null,\"max_length\":null,"
                        "\"minimum\":null,\"maximum\":null,\"pattern\":null,"
                        "\"pattern_description\":null,\"value_descriptions\":null,"
                        "\"format\":null,\"language\":null,\"description\":null}")
           (ax:encode-json type))
    (check "a FieldType defaults to string"
           "string" (core::core-get (core::core-record-new "FieldType" (ax:object)) "name")))

  ;; A Field derives its title the way Ax does, and keeps its flags.
  (let ((field (core::core-record-new "Field" (ax:object "name" "parseHTTPResponse"))))
    (check "a title is derived from the name" "Parse HTTP Response"
           (core::core-get field "title"))
    (check-same "a flag defaults to false" ax:false (core::core-get field "is_optional")))
  (check "a given title wins" "Mine"
         (core::core-get (core::core-record-new "Field" (ax:object "name" "x" "title" "Mine"))
                         "title"))
  (check-raises "a Field needs a name" (core::core-record-new "Field" (ax:object)))
  (check-raises "an unknown record type is refused"
    (core::core-record-new "Nonesuch" (ax:object)))

  ;; A signature is built from inputs and outputs and read back as
  ;; input_fields and output_fields.
  (let ((signature (core::core-record-new
                    "AxSignature"
                    (ax:object "inputs" (vector "a") "outputs" (vector "b")))))
    (check "inputs become input_fields" (vector "a") (core::core-get signature "input_fields"))
    (check "outputs become output_fields" (vector "b") (core::core-get signature "output_fields")))

  ;; Alias confinement. A record constructor reads its attribute map under
  ;; either spelling, because a caller, a fixture and the fluent builder
  ;; write Ax's TypeScript names. A plain read does not, at all: a JSON
  ;; object holding minLength is not a record whose min_length anybody
  ;; asked for, and a schema that requires min_length must not be satisfied
  ;; by minLength arriving on a wire object.
  (check-same "a plain object does not answer the other spelling"
              :null (core::core-get (ax:object "minLength" 5) "min_length"))
  (check-same "nor the other way round"
              :null (core::core-get (ax:object "min_length" 5) "minLength"))
  (check-same "a wire object's camelCase key is not an alias"
              :null (core::core-get (ax:object "mimeType" "image/png") "mime_type"))
  (check-same "nor its snake_case key"
              :null (core::core-get (ax:object "file_uri" "gs://x") "fileUri"))
  (check-same "map.contains does not invent an alias either"
              ax:false (core::core-map-contains (ax:object "minLength" 5) "min_length"))
  ;; Even a marked record answers only the key it carries, which is the one
  ;; its constructor normalised to.
  (let ((type (core::core-record-new "FieldType" (ax:object "name" "string"
                                                            "minLength" 2))))
    (check-same "a constructor takes the TypeScript spelling" 2
                (core::core-get type "min_length"))
    (check-same "and stores only the record's own spelling"
                :null (core::core-get type "minLength")))
  ;; With both spellings present the exact key wins, so the result does not
  ;; depend on hash order.
  (let ((type (core::core-record-new "FieldType"
                                     (ax:object "name" "string"
                                                "minLength" 9 "min_length" 4))))
    (check-same "the record's own spelling wins over the alias" 4
                (core::core-get type "min_length")))
  (let ((type (core::core-record-new "FieldType"
                                     (ax:object "name" "string" "min_length" 4
                                                "minLength" 9))))
    (check-same "whichever order they were written in" 4
                (core::core-get type "min_length")))
  ;; A nested field map reaches the same constructor, so it takes both too.
  (let ((fields (core::core-fields-from-map
                 (ax:object "tags" (ax:object "name" "string" "isArray" ax:true)))))
    (check-same "a nested field map takes the TypeScript spelling"
                ax:true (core::core-get (core::core-get (aref fields 0) "type") "is_array")))

  ;; field.item clears the array on a copy, never on the original.
  (let* ((field (core::core-record-new
                 "Field"
                 (ax:object "name" "tags"
                            "type" (ax:object "name" "string" "is_array" ax:true))))
         (item (core::core-field-item field)))
    (check-same "the item is not an array" ax:false
                (core::core-get (core::core-get item "type") "is_array"))
    (check-same "the original field still is" ax:true
                (core::core-get (core::core-get field "type") "is_array")))
  (let ((item (core::core-field-item
               (ax:object "name" "tags" "type" (ax:object "name" "string" "isArray" ax:true)))))
    (check-same "the TypeScript spelling of the flag is cleared too" ax:false
                (core::core-get (core::core-get item "type") "is_array")))

  ;; fields.from_map turns a map of names into Field records, in order.
  (let ((fields (core::core-fields-from-map (ax:object "b" "string" "a" "number"))))
    (check-same "one Field per entry" 2 (length fields))
    (check "in the written order" (vector "b" "a")
           (vector (core::core-get (aref fields 0) "name")
                   (core::core-get (aref fields 1) "name")))))

;;; ------------------------------------------------------------------
;;; Host objects
;;; ------------------------------------------------------------------

(defstruct (probe (:constructor make-probe)) (slot "stored"))

(defmethod core::core-host-get ((target probe) key &optional (fallback :null))
  (if (string= key "slot") (probe-slot target) fallback))

(defmethod core::core-host-call ((target probe) method args)
  (if (string= method "echo")
      (format nil "~a:~a" (probe-slot target) (if (plusp (length args)) (aref args 0) ""))
      (call-next-method)))

(defun run-host-object-tests ()
  (let ((probe (make-probe)))
    ;; A specialised host object answers the three operations.
    (check "a host read reaches the method" "stored" (core::core-get probe "slot"))
    (check-same "an unknown host key takes the fallback" :null (core::core-get probe "other"))
    (check "a host method call reaches the method" "stored:x"
           (core::core-object-call-method probe "echo" "x"))
    ;; An unimplemented operation fails, naming what was asked for, rather
    ;; than answering with a placeholder a caller would carry forward.
    (check-raises "an unknown host method fails"
      (core::core-object-call-method probe "missing"))
    (check-raises "a host write with no method fails" (core::core-set probe "slot" 1)))

  ;; An unspecialised host object still reads as absent rather than
  ;; failing, because Core asks any value for keys it may not have.
  (check-same "an unspecialised host object reads as absent"
              :null (core::core-get #'identity "anything"))
  (check-raises "calling a method on it fails"
    (core::core-object-call-method #'identity "run"))

  ;; Native callbacks receive their arguments, not the dispatch method name.
  (check "a callback call preserves argument order" 7
         (core::core-object-call-method #'- "call" 11 4))
  (check "a result formatter receives its result" "hello"
         (core::function-result-text-impl "hello" (ax:object)))
  (check "Core invokes the selected native result picker" 1
         (core::select-sample-index
          (vector (ax:object "answer" "first") (ax:object "answer" "second"))
          (ax:object "resultPicker"
                     (lambda (payload) (declare (ignore payload)) 1))))
  (let ((failure (make-condition 'simple-error :format-control "callback failure")))
    (check-same "a callback failure preserves the condition" t
                (eq failure
                    (handler-case
                        (core::core-object-call-method
                         (lambda () (error failure)) "call")
                      (error (condition) condition)))))

  ;; A JSON value is not a host object, and still refuses a write.
  (check-raises "a string is not a settable object" (core::core-set "text" "k" 1)))

;;; ------------------------------------------------------------------
;;; Error values
;;; ------------------------------------------------------------------

(defun run-error-tests ()
  ;; A constructor returns a condition for Core to raise, not a raise.
  (dolist (case (list (list #'core::core-signature-error 'ax:signature-error)
                      (list #'core::core-validation-error 'ax:validation-error)
                      (list #'core::core-runtime-error 'ax:ax-error)))
    (destructuring-bind (constructor type) case
      (let ((condition (funcall constructor "the trouble")))
        (check-same (format nil "~a is built, not signalled" type) t
                    (typep condition type))
        (check (format nil "~a keeps its message" type)
               "the trouble" (ax:ax-error-message condition)))))

  ;; An Ax condition's message survives the round trip exactly: a flow puts
  ;; this text into a result a caller reads.
  (check "an Ax message is returned unchanged"
         "Field \"x\": unbalanced \"{\" in object type"
         (core::core-exception-message
          (core::core-signature-error "Field \"x\": unbalanced \"{\" in object type")))
  (check "an empty Ax message stays empty" ""
         (core::core-exception-message (core::core-runtime-error "")))
  (check "a validation message is returned unchanged" "value too long"
         (core::core-exception-message (core::core-validation-error "value too long")))
  ;; An ordinary condition answers with its printed form, which is what a
  ;; condition's message is in Lisp.
  (check "an ordinary condition answers with its report"
         (princ-to-string (make-condition 'simple-error
                                          :format-control "plain ~a"
                                          :format-arguments '("trouble")))
         (core::core-exception-message
          (make-condition 'simple-error :format-control "plain ~a"
                                        :format-arguments '("trouble"))))
  (check-same "an ordinary condition's text is not empty" t
              (plusp (length (core::core-exception-message
                              (make-condition 'division-by-zero)))))
  ;; Core sometimes holds a failure as a value rather than a condition.
  (check "a string is already the message" "already text"
         (core::core-exception-message "already text"))
  (check "an error object answers from its message key" "from the map"
         (core::core-exception-message (ax:object "message" "from the map")))
  (check "an object with no message falls back to its own text"
         "{\"code\":7}" (core::core-exception-message (ax:object "code" 7))))

;;; ------------------------------------------------------------------
;;; Media, URL and the remaining boundaries
;;; ------------------------------------------------------------------

(defun run-shape-tests ()
  ;; An image needs both keys.
  (check-same "an image part has mimeType and data" ax:true
              (core::valid-image (ax:object "mimeType" "image/png" "data" "AA==")))
  (check-same "an image without data is not one" ax:false
              (core::valid-image (ax:object "mimeType" "image/png")))
  (check-same "a string is not an image" ax:false (core::valid-image "data"))

  ;; A file needs a mime type and exactly one source: both is as wrong as
  ;; neither, which a port testing only presence would miss.
  (check-same "a file with data is one" ax:true
              (core::valid-file (ax:object "mimeType" "text/plain" "data" "AA==")))
  (check-same "a file with a URI is one" ax:true
              (core::valid-file (ax:object "mimeType" "text/plain" "fileUri" "gs://x")))
  (check-same "a file with both is not one" ax:false
              (core::valid-file (ax:object "mimeType" "text/plain" "data" "AA=="
                                           "fileUri" "gs://x")))
  (check-same "a file with neither is not one" ax:false
              (core::valid-file (ax:object "mimeType" "text/plain")))

  (check-same "audio may be plain text" ax:true (core::valid-audio "spoken"))
  (check-same "audio may carry an id" ax:true (core::valid-audio (ax:object "id" "a1")))
  (check-same "an empty object is not audio" ax:false (core::valid-audio (ax:object)))
  (check-same "a url part may be text" ax:true (core::valid-url-shape "https://example.com"))
  (check-same "a url part may be an object" ax:true
              (core::valid-url-shape (ax:object "url" "https://example.com")))
  (check-same "a number is not a url part" ax:false (core::valid-url-shape 7))

  ;; url.valid tests for an absolute URL's shape.
  (check-same "https is a URL" ax:true (core::core-url-valid "https://example.com"))
  (check-same "a custom scheme is a URL" ax:true (core::core-url-valid "s3+ssl://bucket/key"))
  (check-same "mailto is not, having no slashes" ax:false (core::core-url-valid "mailto:a@b.c"))
  (check-same "a relative path is not" ax:false (core::core-url-valid "/images/x.png"))
  (check-same "a non-string is not" ax:false (core::core-url-valid 7))

  ;; encodeURIComponent's exact safe set.
  (check "the unreserved set is untouched" "-_.!~*'()Az0"
         (core::core-url-encode-component "-_.!~*'()Az0"))
  (check "a slash and a space are encoded" "a%2Fb%20c"
         (core::core-url-encode-component "a/b c"))
  (check "null encodes as nothing" "" (core::core-url-encode-component :null))

  ;; Published digest vectors, not this port's own output.
  (check "the digest of the empty string"
         "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
         (core::core-crypto-sha256-hex ""))
  (check "the digest of abc"
         "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
         (core::core-crypto-sha256-hex "abc")))

;;; ------------------------------------------------------------------
;;; Strings that no signature fixture reaches
;;; ------------------------------------------------------------------

(defun run-string-tests ()
  (check "lower camel from words" "cSharpCode"
         (core::core-string-lower-camel (vector "c" "sharp" "code")))
  (check "lower camel of nothing" "" (core::core-string-lower-camel (vector)))
  (check "a trailing Code becomes its own word" "Python Code"
         (core::core-string-title-from-camel "pythonCode"))
  (check "an underscore is a word break" "My node"
         (core::core-string-title-from-camel
          (core::core-string-lower (core::core-string-title-from-camel "my_node"))))
  (check "a camel id titles as the flow renderer writes it" "My node id"
         (core::core-string-title-from-camel
          (core::core-string-lower (core::core-string-title-from-camel "myNodeId"))))

  (check "a suffix is removed and reported"
         (ax:object "value" "file" "removed" ax:true)
         (core::core-string-remove-suffix "file.txt" ".txt"))
  (check "a missing suffix is reported too"
         (ax:object "value" "file.txt" "removed" ax:false)
         (core::core-string-remove-suffix "file.txt" ".md"))

  (check "a blank value falls back" "fallback"
         (core::core-string-default-if-empty "   " "fallback"))
  (check "a value is trimmed rather than replaced" "kept"
         (core::core-string-default-if-empty "  kept  " "fallback"))

  (check "split keeps empty pieces" (vector "a" "" "b")
         (core::core-string-split "a,,b" ","))
  (check "an empty separator splits characters" (vector "a" "b")
         (core::core-string-split "ab" ""))
  (check-same "ends_with is exact" ax:true (core::core-string-ends-with "file.txt" ".txt"))
  (check-same "ends_with rejects a longer suffix" ax:false
              (core::core-string-ends-with ".txt" "file.txt"))

  ;; The quotes stay in the piece: this splits a signature's field list
  ;; without unquoting it, and a later boundary reads the quoted span.
  (check "a quoted separator does not split" (vector "\"a,b\"" "c")
         (core::core-string-split-outside-quotes "\"a,b\",c" ","))
  (check-raises "an unterminated quote is a signature error"
    (core::core-string-split-outside-quotes "\"open, c" ","))

  (check "sorted strings order by code point" (vector "a" "b" "c")
         (core::core-sorted-strings (vector "c" "a" "b")))

  ;; The host regular expression operations, with JavaScript's $ forms.
  ;; The greedy run gives back one capital so the second group can match,
  ;; so HTTPResponse splits after HTTP and not after HTTPR.
  (check "a group reference substitutes" "parseHTTP Response"
         (core::core-regex-replace "([A-Z]+)([A-Z][a-z])" "$1 $2" "parseHTTPResponse"))
  (check "replacement is global" "a-b-c"
         (core::core-regex-replace "_" "-" "a_b_c"))
  (check "a doubled dollar is literal" "$x"
         (core::core-regex-replace "y" "$$x" "y"))
  (check "the whole match substitutes" "[ab]"
         (core::core-regex-replace "ab" "[$&]" "ab"))
  (check-raises "a named group reference is refused, not written out"
    (core::core-regex-replace "(a)" "$<word>" "a"))

  ;; A fenced model reply parses; the strict boundary refuses it.
  (check "a fenced reply parses" (ax:object "a" 1)
         (core::core-json-parse (format nil "```json~%{\"a\": 1}~%```")))
  (check "a fence with no language parses" (ax:object "a" 1)
         (core::core-json-parse (format nil "```~%{\"a\": 1}~%```")))
  ;; Only the delimiters go. A backtick inside the document is part of the
  ;; document, so stripping every backtick would turn a`b into ab and hand
  ;; back a value the model did not send.
  (check "a backtick inside the JSON survives the fence"
         (ax:object "text" "a`b")
         (core::core-json-parse (format nil "```json~%{\"text\": \"a`b\"}~%```")))
  (check "and so does a fenced code span inside a string"
         (ax:object "text" "see `x` here")
         (core::core-json-parse
          (format nil "```~%{\"text\": \"see `x` here\"}~%```")))
  (check "an unfenced reply is untouched" (ax:object "text" "a`b")
         (core::core-json-parse "{\"text\": \"a`b\"}"))
  (check "plain JSON parses either way" (ax:object "a" 1)
         (core::core-json-parse-strict " {\"a\": 1} "))
  (check-raises "the strict boundary refuses a fence"
    (core::core-json-parse-strict (format nil "```~%{\"a\": 1}~%```"))))

;;; ------------------------------------------------------------------
;;; Dates
;;; ------------------------------------------------------------------

(defun run-date-tests ()
  ;; ISO text, including before the epoch, from Core's own calendar.
  (check "the epoch" "1970-01-01T00:00:00.000Z" (core::core-date-iso-text 0))
  (check "a millisecond before it" "1969-12-31T23:59:59.999Z" (core::core-date-iso-text -1))
  (check "a leap day" "2024-02-29T12:00:00.000Z"
         (core::core-date-iso-text 1709208000000))

  ;; Zone offsets, against Python's zoneinfo reading the same tz database.
  ;; Every expectation below was produced by zoneinfo, not by this port.
  ;;
  ;; The cases past 2038 are the point of the list. A zone file stops
  ;; recording transitions in 2037 and leaves the rest to the POSIX rule in
  ;; its footer, so a reader that stops at the 32-bit block answers July
  ;; 2040 in Denver with standard time. Both seasons of several zones are
  ;; here, including a southern-hemisphere rule, two offsets that are not a
  ;; whole hour, and Dublin, whose rule runs daylight time backwards.
  (dolist (case '(("America/Denver" 2210241600000 -25200)   ; 2040-01-15, MST
                  ("America/Denver" 2225966400000 -21600)   ; 2040-07-15, MDT
                  ("America/Denver" 2215069199000 -25200)   ; one second before
                  ("America/Denver" 2215069200000 -21600)   ; the 2040 change
                  ("Europe/Berlin" 2541499200000 7200)      ; 2050-07-15, CEST
                  ("Australia/Lord_Howe" 2368094400000 39600) ; 2045 southern
                  ("Pacific/Chatham" 4103697600000 49500)   ; 2100, quarter hour
                  ("Europe/Dublin" 2225966400000 3600)      ; negative daylight
                  ("America/St_Johns" 2225966400000 -9000)  ; 2040, half hour
                  ("Asia/Kolkata" 4119336000000 19800)      ; 2100, no daylight
                  ("America/Denver" 1705320000000 -25200)   ; 2024-01-15
                  ("America/Denver" 1721044800000 -21600)   ; 2024-07-15
                  ("America/Denver" -2523268800000 -25200)  ; 1890
                  ("Asia/Kolkata" -2207736000000 19270)     ; 1900, local mean
                  ("UTC" 0 0)))
    (destructuring-bind (zone millis offset) case
      (check-same (format nil "~a at ~a" zone millis) offset
                  (core::core-date-zone-offset zone millis))))
  (check-raises "an unknown zone fails, which is how Core tests a name"
    (core::core-date-zone-offset "Mars/Olympus" 0))
  (check-raises "a zone name cannot climb out of the database"
    (core::core-date-zone-offset "../../etc/passwd" 0))
  (check-raises "nor can an absolute path"
    (core::core-date-zone-offset "/etc/passwd" 0))

  ;; Host date values, through the generic Core asks.
  (check-same "a string is not a host date" :null (core::core-date-millis "2024-05-09"))
  (check-same "a number is not a host date either" :null (core::core-date-millis 0))
  (check-same "and neither is null" :null (core::core-date-millis :null))
  ;; type_is "date" is how validation lets a native date through, so it has
  ;; to answer the same question core-date-millis answers.
  (check-same "a string is not the date type" ax:false
              (core::core-type-is "2024-05-09" "date"))
  (check-same "a number is not the date type" ax:false
              (core::core-type-is 1715268645000 "date"))
  (check-same "null is not the date type" ax:false
              (core::core-type-is :null "date"))
  (check-same "a plain value has no date prompt text" :null
              (core::core-date-prompt-text "date" "2024-05-09"))
  (check-same "a non-date field has none either" :null
              (core::core-date-prompt-text "string" "text"))

  ;; With real milliseconds, so the two rules can be told apart: a scalar
  ;; datetime field drops them and a date inside a structure keeps them.
  (let ((timestamp (ignore-errors
                    (core::%host-call "local-time" "LOCAL-TIME" "UNIX-TO-TIMESTAMP"
                                      "test" 1715268645 :nsec 123000000))))
    (if (null timestamp)
        (fail "local-time did not load, so the host date boundary is unchecked")
        (progn
          (check-same "a timestamp reports its instant, milliseconds and all"
                      1715268645123 (core::core-date-millis timestamp))
          (check-same "and is the date type, so validation lets it through"
                      ax:true (core::core-type-is timestamp "date"))
          (check "a date field renders the UTC day" "2024-05-09"
                 (core::core-date-prompt-text "date" timestamp))
          (check "a datetime field renders to the second" "2024-05-09T15:30:45Z"
                 (core::core-date-prompt-text "datetime" timestamp))
          (check "a date range renders as pretty JSON"
                 (format nil "{~%  \"start\": \"2024-05-09\",~%  \"end\": \"2024-05-09\"~%}")
                 (core::core-date-prompt-text
                  "dateRange" (ax:object "start" timestamp "end" timestamp)))
          (check-same "a range of plain values has no date text" :null
                      (core::core-date-prompt-text
                       "dateRange" (ax:object "start" "a" "end" "b")))

          ;; A date that is not a scalar date field goes through
          ;; JSON.stringify in the reference implementation, which calls
          ;; Date.prototype.toJSON and so writes the full instant with
          ;; milliseconds. That is a different rule from the scalar one
          ;; above, and it is the rule for a date in an array or nested in
          ;; an object. Every expectation here came from Node.
          (check "a date on its own converts to its full ISO instant"
                 "2024-05-09T15:30:45.123Z" (core::core-date-json timestamp))
          (check "an array of dates pretty-prints as Node writes it"
                 (format nil "[~%  \"2024-05-09T15:30:45.123Z\"~%]")
                 (core::core-json-pretty (core::core-date-json (vector timestamp))))
          (check "a nested date does too"
                 (format nil "{~%  \"when\": \"2024-05-09T15:30:45.123Z\",~%  \"list\": [~%    \"2024-05-09T15:30:45.123Z\"~%  ]~%}")
                 (core::core-json-pretty
                  (core::core-date-json
                   (ax:object "when" timestamp "list" (vector timestamp)))))
          (check "and an array of ranges"
                 (format nil "[~%  {~%    \"start\": \"2024-05-09T15:30:45.123Z\",~%    \"end\": \"2024-05-09T15:30:45.123Z\"~%  }~%]")
                 (core::core-json-pretty
                  (core::core-date-json
                   (vector (ax:object "start" timestamp "end" timestamp)))))
          ;; The conversion reports whether it did anything, and leaves a
          ;; value with no date in it alone rather than copying it.
          (check-same "a converted value is reported as changed" t
                      (nth-value 1 (core::core-date-json (vector timestamp))))
          (let ((plain (ax:object "a" (vector 1 "two" :null))))
            (check-same "a value with no date is not reported as changed" nil
                        (nth-value 1 (core::core-date-json plain)))
            (check-same "and comes back as the very same object" t
                        (eq plain (core::core-date-json plain))))
          ;; Only dates are touched: the encoder still refuses a value that
          ;; is not JSON, rather than being widened to accept anything.
          (check-raises "a non-JSON value is still refused by the encoder"
            (core::core-json-pretty (core::core-date-json (vector #'identity))))

          ;; Every Core stringify is JSON.stringify, which asks a value for
          ;; its toJSON, so a nested date reads as its ISO instant on each
          ;; of them rather than failing the whole rendering. These three
          ;; answers came from Node, and string.str and format's {} are the
          ;; paths a gen or agent caller reaches with an arbitrary value.
          (check "string.str of an array holding a date"
                 "[1,\"2024-05-09T15:30:45.123Z\",\"x\"]"
                 (core::core-string-str (vector 1 timestamp "x")))
          (check "format {} of an object holding a date"
                 "{\"when\":\"2024-05-09T15:30:45.123Z\"}"
                 (core::core-string-format "{}" (ax:object "when" timestamp)))
          (check "json.stringify of a nested date"
                 "{\"when\":\"2024-05-09T15:30:45.123Z\"}"
                 (core::core-json-stringify (ax:object "when" timestamp)))
          (check "json.stable_stringify of a nested date"
                 "{\"b\":\"2024-05-09T15:30:45.123Z\",\"when\":1}"
                 (core::core-json-stable-stringify
                  (ax:object "when" 1 "b" timestamp)))
          ;; Converting an already converted value changes nothing, so the
          ;; caller-side conversion the prompt does is safe to keep.
          (check "converting twice is the same as converting once"
                 (core::core-json-pretty (vector timestamp))
                 (core::core-json-pretty (core::core-date-json (vector timestamp))))
          ;; And the strict Lisp API is untouched: it has no toJSON
          ;; protocol to honour and still refuses a host object by name.
          (check-raises "ax:encode-json still refuses a date"
            (ax:encode-json (vector timestamp)))))))

;;; ------------------------------------------------------------------
;;; Entry point
;;; ------------------------------------------------------------------

(defun run-core-primitive-tests ()
  "Run every check in this file. Returns T when they all passed."
  (let ((*failures* '())
        (*checks* 0))
    (run-pure-fixtures)
    (run-absence-tests)
    (run-unicode-tests)
    (run-number-tests)
    (run-structure-tests)
    (run-record-tests)
    (run-host-object-tests)
    (run-error-tests)
    (run-shape-tests)
    (run-string-tests)
    (run-date-tests)
    (let ((failures (nreverse *failures*)))
      (format t "~&core primitives: ~d checks, ~d failed~%" *checks* (length failures))
      (dolist (failure failures)
        (format t "~&  FAIL ~a~%" failure))
      (null failures))))
