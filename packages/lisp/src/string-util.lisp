;;;; string-util.lisp --- the native AxStringUtil facade.
;;;;
;;;; Arrays are vectors and objects use AX:OBJECT, as everywhere else in
;;;; this port.
;;;;
;;;; Three of these take or contain a regular expression, and they are
;;;; ECMAScript regular expressions: SPLIT-INTO-TWO is handed the caller's
;;;; own pattern, and the index-prefix helpers carry the pattern from
;;;; src/ax/dsp/strutil.ts verbatim. None of them hands a pattern to the
;;;; host engine as written. They go through AXLLM/CORE's regular
;;;; expression boundary, which translates an ECMAScript source into the
;;;; Perl pattern with the same meaning, refuses by name any construct
;;;; where the two engines cannot be made to agree, and checks its answer
;;;; against Core's own matcher (ir/axcore/regex.axir) whenever the
;;;; generated Core file provides it.
;;;;
;;;; That matcher decides whether a pattern matches; it does not report
;;;; where. Locating the match, which a split needs, is the host engine's
;;;; job, and a disagreement between the two is an error rather than a
;;;; quiet answer from whichever engine was asked.

(in-package #:axllm)

(defun trim-non-alpha-num (text)
  "Trim ASCII non-word boundaries; underscore is a word character."
  (flet ((word-p (c)
           (or (char<= #\a c #\z) (char<= #\A c #\Z)
               (char<= #\0 c #\9) (char= c #\_))))
    (let ((start (position-if #'word-p text))
          (end (position-if #'word-p text :from-end t)))
      (if start (subseq text start (1+ end)) ""))))

(defun split-into-two (text separator)
  "Split TEXT at the first whole match of SEPARATOR, dropping the match.

SEPARATOR is an ECMAScript pattern source, not a literal separator: the
reference implementation passes it straight to String.prototype.search and
String.prototype.match, which read a string as a pattern. A pattern whose
ECMAScript meaning this host cannot reproduce is refused by name rather
than matched approximately.

Returns a vector of the two parts, or of TEXT alone when the pattern does
not match. The split is at the whole match, never at a captured group."
  (let ((text (axllm/core::core-js-text text)))
    (multiple-value-bind (start end)
        (axllm/core::core-regex-search separator text)
      (if start
          (vector (subseq text 0 start) (subseq text end))
          (vector text)))))

(defun dedup (strings)
  "Deduplicate strings, retaining their FIRST occurrence and its order."
  (let ((seen (make-hash-table :test 'equal)) (out (%new-array)))
    (map nil (lambda (text)
               (check-type text string)
               (unless (gethash text seen)
                 (setf (gethash text seen) t)
                 (vector-push-extend text out))) strings)
    out))

(defparameter +index-prefix-pattern+ "^(\\d+)[.,\\s]+(.*)$"
  "The pattern src/ax/dsp/strutil.ts matches a numbered list line with.

Carried across as written. Its ECMAScript reading differs from a Perl
engine's in three places, and the translation in AXLLM/CORE handles each:
\\d is the ASCII digits, \\s is ECMAScript's whitespace set, which includes
U+00A0 and U+FEFF, the dot excludes all four line terminators, and without
the m flag the trailing $ requires the actual end of the input.")

(defun %string-util-index-match (text)
  (axllm/core::core-regex-capture +index-prefix-pattern+ text))

(defun extract-id-and-text (text)
  "Read a numeric list prefix and trimmed text into {id, text}."
  (let ((parts (%string-util-index-match text)))
    (unless parts
      (error 'ax-error :message
             "line must start with a number, a dot and then text. e.g. \"1. hello\""))
    (object "id" (parse-integer (aref parts 0))
            "text" (axllm/core::core-string-trim (aref parts 1)))))

(defun extract-index-prefixed-text (text)
  "Strip a numeric list prefix, or return unmatched TEXT unchanged."
  (let ((parts (%string-util-index-match text)))
    (if parts (axllm/core::core-string-trim (aref parts 1)) text)))

(defun batch-array (array size)
  "Partition ARRAY into vectors of at most SIZE elements.
Reject non-positive/non-integral sizes rather than looping indefinitely."
  (unless (and (integerp size) (plusp size))
    (error 'ax-error :message "batch-array: size must be a positive integer"))
  (let ((out (%new-array)))
    (loop for start from 0 below (length array) by size
          do (vector-push-extend
              (coerce (subseq array start (min (length array) (+ start size))) 'vector)
              out))
    out))
