;;;; string-util.lisp --- AxStringUtil, and the regular expression boundary.
;;;;
;;;; Ax's patterns are ECMAScript patterns with no flags. CL-PPCRE is a
;;;; Perl engine, and the two disagree on several constructs, so
;;;; AXLLM/CORE translates a pattern before the host engine sees it and
;;;; refuses by name anything it cannot translate. This file is where that
;;;; claim is tested, because a translation nobody checks is a worse
;;;; failure than a refusal: it answers confidently and wrongly.
;;;;
;;;; Three kinds of check run here.
;;;;
;;;; Every pattern Ax's own Core code uses with core.regex_match or
;;;; intrinsic.regex.replace is listed, and each one must translate and
;;;; compile. A Core author who adds a pattern this host cannot reproduce
;;;; finds out from this list rather than from a wrong answer in
;;;; production. The list was extracted from ir/axcore/*.axir.
;;;;
;;;; The cases that separate the two engines carry expectations taken from
;;;; Node's own RegExp, not from this port: a trailing newline against $, a
;;;; no-break space and a zero-width no-break space against \s, a line
;;;; separator against the dot. A Perl engine answers several of these
;;;; differently, which is the whole reason the translation exists.
;;;;
;;;; The constructs that cannot be translated must fail, and say what they
;;;; are. A silent approximation would be the real defect.

(defpackage #:axllm/tests/string-util
  (:use #:cl)
  (:local-nicknames (#:core #:axllm/core))
  (:export #:run-string-util-tests))

(in-package #:axllm/tests/string-util)

(defvar *failures* '())
(defvar *checks* 0)

(defun fail (format-control &rest arguments)
  (push (apply #'format nil format-control arguments) *failures*))

(defmacro check (label expected actual)
  (let ((want (gensym)) (got (gensym)))
    `(let ((,want ,expected))
       (incf *checks*)
       (handler-case
           (let ((,got ,actual))
             (unless (core::core-value-equal ,got ,want)
               (fail "~a: expected ~a, got ~a" ,label
                     (ax:encode-json ,want) (ax:encode-json ,got))))
         (error (condition)
           (fail "~a: raised ~a: ~a" ,label (type-of condition) condition))))))

(defmacro check-raises (label fragment &body body)
  "Check that BODY fails with an Ax error whose message names FRAGMENT."
  `(progn
     (incf *checks*)
     (handler-case (let ((value (progn ,@body)))
                     (fail "~a: expected a failure, got ~s" ,label value))
       (ax:ax-error (condition)
         (let ((message (ax:ax-error-message condition)))
           (unless (search ,fragment message)
             (fail "~a: expected a message naming ~s, got ~s" ,label ,fragment message))))
       (error (condition)
         (fail "~a: expected an Ax error, got ~a: ~a" ,label (type-of condition) condition)))))

;;; ------------------------------------------------------------------
;;; Every pattern Ax's Core code actually uses
;;; ------------------------------------------------------------------

(defparameter +core-patterns+
  (list "#"
        "(-\\\\.+->|={2,}>|---|~~~)"
        "([A-Z]+)([A-Z][a-z])"
        "([a-z0-9])([A-Z])"
        "-\\d{2,}(-[a-zA-Z0-9-]+)?$"
        "-\\d{8}$"
        "-latest$"
        "-v\\d+$"
        "-v\\d+:\\d+$"
        "-v\\d+@\\d{8}$"
        ":$"
        "<think>[\\s\\S]*?</think>"
        "@\\d{8}$"
        "[-!\"#$%&'()*+,./:;<=>?@\\[\\]^_`{|}~]"
        "[\\s-]+"
        "[^A-Za-z0-9]+"
        "[^A-Za-z0-9_-]"
        "[^\\n]*</think>"
        "[^\\t -~]"
        "\\+"
        "\\n?```[ \\t]*$"
        "\\n{3,}"
        "\\s$"
        "\\s*=+$"
        "\\s+"
        "\\s+-\\s+.*$"
        "\\s+in\\s+.*$"
        "^#+"
        "^%%"
        "^%%ax\\s+"
        "^(?:(?:[a-z]+(?:-[a-z]+)*\\.)?openai\\.)?gpt-5\\.6($|-)"
        "^(?:(?:[a-z]+(?:-[a-z]+)*\\.)?openai\\.)?gpt-6-(astra|sol|luna)($|-)"
        "^(?:(?:[a-z]+(?:-[a-z]+)*\\.)?openai\\.)?gpt-6-astra($|-)"
        "^(?:[a-z]+(?:-[a-z]+)*\\.)?(?:anthropic|openai)\\."
        "^(?:[a-z]+(?:-[a-z]+)*\\.)?openai\\."
        "^(?:o1|o1-mini|o1-pro|o3|o3-mini|o3-pro|o4-mini)$"
        "^(?:openai|openai-responses|openai-compatible|anthropic|google-gemini|typesafe)$"
        "^(subgraph\\b|end\\b|style\\b|classDef\\b|class\\b|linkStyle\\b|click\\b|direction\\b)"
        "^-?(0|[1-9][0-9]*)$"
        "^-?[0-9]+(\\.[0-9]+)?$"
        "^.*-"
        "^0[bB][01]+$"
        "^0[oO][0-7]+$"
        "^0[xX][0-9a-fA-F]+$"
        "^=+\\s*"
        "^FAILED\\s+"
        "^[ \\n\\r\\t]$"
        "^[!#$%&'*+\\-.^_`|~0-9A-Za-z]+$"
        "^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$"
        "^[-*+]$"
        "^[0-9]"
        "^[0-9]$"
        "^[0-9]+$"
        "^[0-9]+(\\.[0-9]+)?$"
        "^[0-9]+\\s*[.)\\]]"
        "^[0-9][eE.+-]$"
        "^[A-Za-z0-9_-]*[ 	]*$"
        "^[A-Za-z]$"
        "^[A-Za-z][A-Za-z0-9_]*$"
        "^[A-Za-z_][A-Za-z0-9_.-]*$"
        "^[A-Za-z_][A-Za-z0-9_]*$"
        "^[\\s`]*$"
        "^[\\t ]|[\\t ]$"
        "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$"
        "^[a-zA-Z0-9]$"
        "^[a-z][a-z0-9_]{0,31}$"
        "^[eE][+-]$"
        "^\\s"
        "^```([A-Za-z0-9_-]+)?[ \\t]*\\n"
        "^```[a-zA-Z]*\\s*$"
        "```([A-Za-z0-9_-]+)?[ 	]*$"
        "```[A-Za-z0-9_-]+[ 	]*$"
        "claude-fable-5(?:$|[^0-9-]|-(?:[0-9]{3,}|[^0-9]))"
        "claude-fable-5-1(?:$|[^0-9])"
        "claude-opus-5(?:$|[^0-9-]|-(?:[0-9]{3,}|[^0-9]))"
        "claude-opus-5-5(?:$|[^0-9])"
        "claude-sonnet-5(?:$|[^0-9-]|-(?:[0-9]{3,}|[^0-9]))"
        "claude-sonnet-5-5(?:$|[^0-9])"
        "gpt-6\\.1(?:$|[^0-9])"
        "^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), [0-9]{2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} GMT$"
        "^(?:gemini-3\\.8-flash|gemini-3\\.7-flash|gemini-3\\.6-flash|gemini-3\\.5-flash|gemini-3\\.5-flash-lite|gemini-3\\.1-pro-preview|gemini-3\\.1-flash-lite|gemini-2\\.5-flash|gemini-2\\.5-pro)$")
  "Every pattern ir/axcore passes to core.regex_match or regex.replace.

Each one has to translate and compile here. A pattern this host cannot
reproduce has to be found when it is added to Core, not when a provider
name or a tool argument is checked against it in a running program.")

(defun run-core-pattern-tests ()
  (dolist (pattern +core-patterns+)
    (incf *checks*)
    (handler-case (core::%scanner pattern)
      (error (condition)
        (fail "Core pattern ~s does not translate: ~a" pattern condition))))
  ;; The one construct in that list a Perl class cannot hold, which is
  ;; translatable only because a shorthand with its own negation is every
  ;; character in either engine.
  (check "a shorthand and its negation is any character"
         "ab" (core::core-regex-replace "<think>[\\s\\S]*?</think>" ""
                                        (format nil "a<think>x~%y</think>b"))))

;;; ------------------------------------------------------------------
;;; Where the two engines disagree
;;; ------------------------------------------------------------------

(defparameter +engine-divergence+
  ;; pattern, input, whether Node's RegExp matches. Every expectation was
  ;; produced by Node, and a Perl engine given the pattern unchanged
  ;; answers the marked ones differently.
  (list
   ;; Without the m flag, $ is the very end of the input. Perl's $ also
   ;; matches just before a final newline.
   (list "c$" "abc" t)
   (list "c$" "abc
" nil)                               ; Perl: matches
   (list "^abc$" "abc
" nil)                               ; Perl: matches
   (list "[0-9]+$" "12
" nil)                                ; Perl: matches
   ;; ECMAScript's \s includes the vertical tab, the no-break space, the
   ;; zero-width no-break space and the Unicode space separators. Perl's
   ;; does not.
   (list "^\\s$" " " t)
   (list "^\\s$" (string (code-char #x000b)) t)   ; Perl: no match
   (list "^\\s$" (string (code-char #x00a0)) t)   ; Perl: no match
   (list "^\\s$" (string (code-char #xfeff)) t)   ; Perl: no match
   (list "^\\s$" (string (code-char #x2003)) t)   ; Perl: no match
   (list "^\\s$" "a" nil)
   (list "^\\S$" (string (code-char #x00a0)) nil) ; Perl: matches
   (list "^\\S$" "a" t)
   ;; The dot excludes all four line terminators, not just the newline.
   (list "^a.b$" "a b" t)
   (list "^a.b$" (format nil "a~ab" (code-char #x2028)) nil)  ; Perl: matches
   (list "^a.b$" (format nil "a~ab" (code-char #x2029)) nil)  ; Perl: matches
   (list "^a.b$" (format nil "a~ab" #\Return) nil)            ; Perl: matches
   (list "^a.b$" (format nil "a~ab" #\Newline) nil)
   ;; \d and \w are ASCII in ECMAScript, and so in CL-PPCRE.
   (list "^\\d$" "5" t)
   (list "^\\d$" (string (code-char #x0660)) nil)  ; Arabic-Indic five
   (list "^\\w+$" "a_1" t)
   (list "^\\w$" (string (code-char #x00e9)) nil)
   ;; An ECMAScript \uXXXX escape, which Perl spells another way.
   (list "^\\u00e9$" (string (code-char #xe9)) t)
   (list "^[\\u00e9a]+$" (format nil "a~a" (code-char #xe9)) t)
   ;; A class holding a shorthand and its negation crosses a newline.
   (list "^a[\\s\\S]b$" (format nil "a~ab" #\Newline) t)
   ;; Annex B identity escapes. With no u flag these are the letter, not a
   ;; Unicode property, a code point escape or a legacy octal, so \p{L}
   ;; matches the four characters p{L} and matches no letter at all. Every
   ;; answer here came from Node.
   (list "^\\p{L}+$" "abc" nil)
   (list "^\\p{L}+$" "p{L}" t)
   (list "^\\p{L}+$" "p{L}}}" t)
   (list "\\P{L}" "P{L}" t)
   (list "\\8" "8" t)
   (list "\\9" "9" t)
   (list "\\u{1f600}" "u{1f600}" t)
   (list "\\u{1f600}" (string (code-char #x1f600)) nil)
   ;; \k is a backreference only in a pattern that declares a named group.
   (list "(?<x>a)\\k<x>" "aa" t)
   (list "(?:a)\\k<x>" "ak<x>" t)
   (list "(?:a)\\k<x>" "a" nil)
   ;; Named groups and lookbehind are ordinary ECMAScript, and both engines
   ;; can read them.
   (list "(?<word>a)b" "ab" t)
   (list "(?<=a)b" "ab" t)
   (list "(?<!a)b" "ab" nil)
   (list "(?<!a)b" "zb" t)
   ;; A brace that does not begin a quantifier is a character.
   (list "a{2}" "aa" t)
   (list "a{2,3}" "aa" t)
   (list "^a{$" "a{" t)
   (list "^{L}$" "{L}" t)
   (list "^a}+$" "a}}" t)
   ;; Ordinary constructs, to show the translation does not break them.
   (list "^[A-Za-z_][A-Za-z0-9_]*$" "field_1" t)
   (list "^[A-Za-z_][A-Za-z0-9_]*$" "1field" nil)
   (list "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$" "a@b.co" t)
   (list "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$" "a b@c.co" nil)
   (list "(?=.*x)" "axb" t)
   (list "(?!x)y" "zy" t)
   (list "(a)\\1" "aa" t)
   (list "(a)\\1" "ab" nil))
  "Cases where ECMAScript and Perl differ, with Node's answers.")

(defun run-divergence-tests ()
  (dolist (case +engine-divergence+)
    (destructuring-bind (pattern input matches) case
      (check (format nil "~s against ~s" pattern input)
             (if matches ax:true ax:false)
             (core::core-regex-match pattern input))))
  ;; A non-string never matches, whatever the pattern says.
  (check "a number never matches" ax:false (core::core-regex-match "^.*$" 7))
  (check "null never matches" ax:false (core::core-regex-match "^.*$" :null)))

;;; ------------------------------------------------------------------
;;; What cannot be translated must fail by name
;;; ------------------------------------------------------------------

(defun run-rejection-tests ()
  ;; Two things are left that a Perl engine cannot be made to do, and both
  ;; are checked through the host paths rather than through
  ;; core-regex-match. Matching is answered by Core's own matcher whenever
  ;; the generated Core file provides it, so a refusal demanded from
  ;; core-regex-match would be testing for a restriction that exists only
  ;; while the matcher is absent. What must refuse is every path that needs
  ;; the host engine to locate a match, because that is where a silent
  ;; approximation produces a wrong string rather than a wrong boolean.
  (dolist (case '(("a negated shorthand inside a class" "[a\\S]" "character class")
                  ("a trailing backslash" "abc\\" "trailing backslash")))
    (destructuring-bind (label pattern fragment) case
      (check-raises (format nil "~a is refused by a replacement" label) fragment
        (core::core-regex-replace pattern "" "abc"))
      (check-raises (format nil "~a is refused by a search" label) fragment
        (core::core-regex-search pattern "abc"))
      (check-raises (format nil "~a is refused by a capture" label) fragment
        (core::core-regex-capture pattern "abc"))))
  ;; The message has to name the pattern, so a failure in a running program
  ;; says which schema or signature caused it.
  (check-raises "the message names the pattern" (prin1-to-string "[a\\S]")
    (core::core-regex-replace "[a\\S]" "" "A"))

  ;; A variable-length lookbehind is the one construct ECMAScript allows
  ;; and the host engine does not implement. With Core's matcher present
  ;; the ECMAScript answer comes back; without it the host engine refuses,
  ;; naming the pattern, rather than answering approximately. Node says
  ;; this one matches.
  (if (core::core-matcher)
      (check "a variable-length lookbehind matches, through Core's matcher"
             ax:true (core::core-regex-match "(?<=ab?)c" "abc"))
      (check-raises "a variable-length lookbehind is refused by the host engine"
                    "(?<=ab?)c"
        (core::core-regex-match "(?<=ab?)c" "abc")))
  ;; And it is refused on the host paths either way, because those need the
  ;; host engine whatever Core can do.
  (check-raises "a variable-length lookbehind is refused by a search" "(?<=ab?)c"
    (core::core-regex-search "(?<=ab?)c" "abc")))

;;; ------------------------------------------------------------------
;;; Replacement
;;; ------------------------------------------------------------------

(defparameter +replacements+
  ;; pattern, replacement, input, Node's String.prototype.replace with /g.
  (list
   (list "([A-Z]+)([A-Z][a-z])" "$1 $2" "parseHTTPResponse" "parseHTTP Response")
   (list "([a-z0-9])([A-Z])" "$1 $2" "myNodeId" "my Node Id")
   (list "[^A-Za-z0-9]+" " " "a-b_c!d" "a b c d")
   (list "\\s+" " " (format nil "a ~a~a b" #\Tab (code-char #x00a0)) "a b")
   (list "\\s*=+$" "" "count ===" "count")
   (list "^=+\\s*" "" "=== hello" "hello")
   (list "#" " Sharp " "C#" "C Sharp ")
   (list "\\+" " Plus " "C++" "C Plus  Plus ")
   (list "(a)(b)" "[$2$1]" "ab" "[ba]")
   (list "x" "$$" "x" "$")
   (list "ab" "[$&]" "zabz" "z[ab]z")
   (list "\\s$" "" "trailing " "trailing")
   (list "[^\\n]*</think>" "" "junk</think>tail" "tail"))
  "Replacement cases, with Node's answers.")

(defun run-replacement-tests ()
  (dolist (case +replacements+)
    (destructuring-bind (pattern replacement input expected) case
      (check (format nil "replace ~s in ~s" pattern input)
             expected (core::core-regex-replace pattern replacement input))))

  ;; An empty replacement, which is how Core deletes a match, has to reach
  ;; CL-PPCRE as one empty string. CL-PPCRE reads an empty replacement list
  ;; as a function to call, so returning NIL for it makes every deletion
  ;; fail with an undefined function rather than removing anything.
  (check "an empty replacement is a list with an empty string in it"
         1 (length (core::%js-replacement "")))
  (check "and that element is the empty string"
         "" (first (core::%js-replacement "")))
  ;; Each of these, with Node's answer, deletes rather than failing.
  (dolist (case '(("x" "" "axb" "ab")
                  ("[0-9]+" "" "a1b22c" "abc")
                  ("^\\s+" "" "  text" "text")
                  ("\\s+$" "" "text  " "text")
                  ("(a)(b)" "" "zabz" "zz")
                  (".*" "" "anything" "")))
    (destructuring-bind (pattern replacement input expected) case
      (check (format nil "deleting ~s from ~s" pattern input)
             expected (core::core-regex-replace pattern replacement input))))
  (check-raises "a named group reference in a replacement is refused"
                "named group reference"
    (core::core-regex-replace "(a)" "$<name>" "a")))

;;; ------------------------------------------------------------------
;;; The public AxStringUtil surface
;;; ------------------------------------------------------------------

(defparameter +splits+
  ;; text, separator, Node's AxStringUtil.splitIntoTwo result.
  (list (list "a, b" ", " (vector "a" "b"))
        (list "key: value" ":\\s*" (vector "key" "value"))
        (list "nosep" "X" (vector "nosep"))
        (list "a1b" "\\d" (vector "a" "b"))
        (list "abc" "^a" (vector "" "bc"))
        (list (format nil "a~ab" #\Newline) "\\s" (vector "a" "b"))
        (list "x=1" "[=]" (vector "x" "1")))
  "splitIntoTwo cases, with the reference implementation's answers.")

(defun run-split-tests ()
  (dolist (case +splits+)
    (destructuring-bind (text separator expected) case
      (check (format nil "split ~s on ~s" text separator)
             expected (ax:split-into-two text separator))))
  ;; A separator is a pattern, not a literal: this is the behaviour the
  ;; reference implementation has, because it hands the string to
  ;; String.prototype.search. An unescaped dot therefore matches the first
  ;; character, which is what Node does with this input too.
  (check "a separator is read as a pattern" (vector "" ".b")
         (ax:split-into-two "a.b" "."))
  (check "an escaped separator is read literally" (vector "a" "b")
         (ax:split-into-two "a.b" "\\."))
  ;; The split is at the whole match, never at a captured group.
  (check "the whole match is dropped, not a group" (vector "a" "c")
         (ax:split-into-two "a(b)c" "\\((b)\\)"))
  ;; A flagless \p{L} is the four characters p{L}, so this separator is an
  ;; ordinary one that simply does not match here.
  (check "a property escape is a literal, not a property" (vector "abc")
         (ax:split-into-two "abc" "\\p{L}"))
  (check "and it matches what it actually means" (vector "a" "b")
         (ax:split-into-two "ap{L}b" "\\p{L}"))
  (check-raises "an untranslatable separator fails rather than guessing"
                "character class"
    (ax:split-into-two "abc" "[a\\S]")))

(defun run-facade-tests ()
  ;; trimNonAlphaNum keeps an underscore, which is a word character.
  (check "non-word edges are trimmed" "a_b" (ax:trim-non-alpha-num "!!a_b!!"))
  (check "an all-punctuation string trims to nothing" "" (ax:trim-non-alpha-num "!!!"))
  (check "an inner space survives" "a b" (ax:trim-non-alpha-num " a b "))

  ;; The index prefix, with the pattern from the reference implementation.
  (check "a numbered line splits into id and text"
         (ax:object "id" 1 "text" "hello") (ax:extract-id-and-text "1. hello"))
  (check "a comma separates as well"
         (ax:object "id" 12 "text" "hello") (ax:extract-id-and-text "12,hello"))
  ;; A no-break space counts as whitespace in ECMAScript, so this line is a
  ;; numbered line. A Perl engine would not match it.
  (check "a no-break space separates too"
         (ax:object "id" 3 "text" "hello")
         (ax:extract-id-and-text (format nil "3.~ahello" (code-char #x00a0))))
  (check-raises "a line with no number is refused" "must start with a number"
    (ax:extract-id-and-text "hello"))
  (check "an unmatched line is returned unchanged" "hello"
         (ax:extract-index-prefixed-text "hello"))
  (check "a matched line loses its prefix" "hello"
         (ax:extract-index-prefixed-text "7. hello"))
  ;; Without the m flag, the trailing $ needs the real end of the input, so
  ;; a second line is not a numbered line at all.
  (check "a second line stops it being a numbered line"
         (format nil "1. a~ab" #\Newline)
         (ax:extract-index-prefixed-text (format nil "1. a~ab" #\Newline)))

  (check "duplicates keep their first position"
         (vector "b" "a" "c") (ax:dedup (vector "b" "a" "b" "c" "a")))
  (check "an empty list dedups to nothing" (vector) (ax:dedup (vector)))

  (check "a list batches into vectors"
         (vector (vector 1 2) (vector 3 4) (vector 5))
         (ax:batch-array (vector 1 2 3 4 5) 2))
  (check "an empty list batches to nothing" (vector) (ax:batch-array (vector) 3))
  (check-raises "a zero batch size is refused" "positive integer"
    (ax:batch-array (vector 1) 0)))

;;; ------------------------------------------------------------------
;;; Entry point
;;; ------------------------------------------------------------------

(defun run-string-util-tests ()
  "Run every check in this file. Returns T when they all passed."
  (let ((*failures* '())
        (*checks* 0))
    (run-core-pattern-tests)
    (run-divergence-tests)
    (run-rejection-tests)
    (run-replacement-tests)
    (run-split-tests)
    (run-facade-tests)
    (let ((failures (nreverse *failures*)))
      (format t "~&string util and regex: ~d checks, ~d failed~%" *checks* (length failures))
      (dolist (failure failures)
        (format t "~&  FAIL ~a~%" failure))
      (null failures))))
