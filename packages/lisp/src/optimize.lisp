;;;; optimize.lisp --- native optimizer engines for the Common Lisp Ax port.
;;;;
;;;; Scope: the three engines Ax ships natively -- GEPA (seeded reflective
;;;; Pareto search), BootstrapFewShot (demo mining) and the ACE driver
;;;; (Generator -> Reflector -> Curator) -- plus the evaluator and callback
;;;; contracts that AxGen, AxAgent and AxFlow compose with.  Every engine
;;;; really runs: it evaluates candidates, spends a metric budget, and
;;;; returns an artifact whose component map was selected from measured
;;;; scores.  Nothing here reports optimizer metadata it did not compute.
;;;;
;;;; Division of labour, which this file does not cross:
;;;;
;;;;   axllm/core   owns portable semantics: artifact shape and validation,
;;;;                dataset/metric normalization, scalarization, action
;;;;                adjustment, eval rows and results, the optimizer request
;;;;                and engine-response contracts, the evidence batch, and
;;;;                every ACE playbook mutation.  Generated from
;;;;                ir/axcore/optimize.axir; see src/core.lisp.
;;;;
;;;;   this file    owns the search: candidate generation and selection,
;;;;                the seeded RNG, the component bandit, minibatching,
;;;;                budget and cancellation boundaries, demo mining, and the
;;;;                ACE round driver.  It is the Common Lisp sibling of the
;;;;                Python template's AxGEPA / AxBootstrapFewShot / AxACE.
;;;;
;;;;   the program  owns components and rollouts, through the generic
;;;;                functions FORWARD, PROGRAM-OPTIMIZABLE-COMPONENTS and
;;;;                PROGRAM-APPLY-OPTIMIZED-COMPONENTS defined by src/gen.lisp.
;;;;
;;;; Teacher traffic is injected, never constructed here: a reflector,
;;;; curator or generator is a Lisp function.  MAKE-AI-REFLECTION-CALLBACK
;;;; builds one from a real provider client when a caller wants one, so an
;;;; engine can be exercised end to end with no provider and no credentials.
;;;;
;;;; JSON values follow the model in json.lisp throughout: :NULL is null,
;;;; AX:TRUE / AX:FALSE are the booleans, objects are ordered EQUAL hash
;;;; tables and arrays are fill-pointer vectors.  Scores are double floats;
;;;; counts stay integers, as the other ports' JSON does.

(in-package #:axllm)

(defparameter +optimizer-exports+
 '(;; conditions
   optimize-error optimize-error-kind
   ;; program protocol this file adds to the gen-owned generics
   program-kind program-optimizer-trace program-function-calls program-set-demos
   program-evaluate-task
   ;; engine protocol
   optimizer-engine optimizer-engine-name optimizer-engine-version run-optimizer-engine
   ;; evaluator protocol
   evaluate-candidate program-evaluator make-program-evaluator
   evaluator-metric-calls evaluator-budget-remaining
   ;; deterministic randomness
   optimizer-rng make-optimizer-rng optimizer-rng-next optimizer-rng-seed
   ;; component value validators
   optimizable-snake-case-identifier optimizable-preserves-placeholders
   optimizable-non-empty validate-component-value
   ;; evaluation metrics
   em-score f1-score novel-f1-score normalize-eval-text
   ;; cost tracking
   cost-tracker make-cost-tracker track-tokens cost-tracker-cost
   cost-tracker-token-usage cost-tracker-total-tokens
   cost-tracker-limit-reached-p reset-cost-tracker record-optimizer-resource-usage
   ;; optimizer run state, checkpoints and statistics
   optimizer-state make-optimizer-state optimizer-stats optimizer-score-history
   optimizer-configuration-history optimizer-current-round optimizer-state-cost-tracker
   record-optimizer-round reset-optimizer
   optimizer-checkpoint load-optimizer-checkpoint
   ;; optimized program records.  There is no OPTIMIZED-PROGRAM type to
   ;; export: a record is a plain JSON object, so the constructor and the
   ;; operations on it are the whole surface.
   make-optimized-program optimized-program-json
   parse-optimized-program apply-optimized-program optimized-program-artifact
   ;; engines
   gepa make-gepa gepa-selector
   gepa-component-selector make-gepa-component-selector
   gepa-selector-pick gepa-selector-record-proposal gepa-selector-record-result
   gepa-selector-snapshot
   bootstrap-few-shot make-bootstrap-few-shot
   ace make-ace ace-compile ace-apply-online-update ace-playbook ace-artifact
   ace-reset ace-base-instruction ace-render
   ;; the ACE roles as real programs
   ace-reflector-signature ace-curator-signature
   make-gen-reflector make-gen-curator playbook-reflector-signature
   make-playbook playbook-evolve playbook-target playbook-load playbook-json
   playbook-state playbook-evolve-agent evolve-miner-signature
   ;; teacher callbacks
   make-ai-reflection-callback
   ;; driver
   optimize-program optimize-program-stream optimize-pareto
   apply-optimization optimizer-evidence-batch
   artifact-text parse-artifact)
  "Every symbol this file adds to the public AXLLM API.

Named rather than inlined into EXPORT so a test can check that each one
really defines something: an exported name that defines nothing is a public
API that fails at the call site.")

(export +optimizer-exports+)

;;; ------------------------------------------------------------------
;;; Condition
;;; ------------------------------------------------------------------

(define-condition optimize-error (ax-error)
  ((kind :initarg :kind :initform :optimize :reader optimize-error-kind))
  (:documentation
   "An optimizer failure.  KIND is one of

  :config      an option or argument the engine cannot run with
  :evaluator   an engine needs a candidate evaluator and was given none
  :components  the program exposes nothing this engine can optimize
  :budget      the metric-call budget is exhausted or too small to start
  :cancelled   the caller's cancellation check asked the run to stop
  :artifact    an artifact or component map that cannot be applied
  :engine      an engine returned something that is not an artifact"))

(defun optimize-fail (kind format-control &rest arguments)
  (error 'optimize-error :kind kind
                         :message (apply #'format nil format-control arguments)))

;;; ------------------------------------------------------------------
;;; JSON helpers, local to this file
;;; ------------------------------------------------------------------

(defun %opt-object-p (value) (hash-table-p value))

(defun %opt-array-p (value) (and (vectorp value) (not (stringp value))))

(defun %opt-clone (value)
  "A deep copy of VALUE, preserving object key order and array order."
  (cond ((%opt-object-p value)
         (let ((out (%new-object)))
           (dolist (key (%object-keys value))
             (%set-key out key (%opt-clone (gethash key value))))
           out))
        ((%opt-array-p value)
         (let ((out (%new-array)))
           (loop for item across value do (vector-push-extend (%opt-clone item) out))
           out))
        (t value)))

(defun %opt-array (&optional items)
  "A JSON array holding ITEMS, a Lisp list."
  (let ((out (%new-array)))
    (dolist (item items) (vector-push-extend item out))
    out))

(defun %opt-list (value)
  "VALUE's elements as a Lisp list.  :NULL and NIL are empty."
  (cond ((%opt-array-p value) (coerce value 'list))
        ((eq value :null) '())
        ((null value) '())
        ((listp value) value)
        (t (list value))))

(defun %opt-count (value)
  (cond ((%opt-array-p value) (length value))
        ((%opt-object-p value) (hash-table-count value))
        ((eq value :null) 0)
        (t 0)))

(defun %opt-finite-p (value)
  (and (realp value)
       (or (not (floatp value))
           (and (= value value) (< (abs value) most-positive-double-float)))))

(defun %opt-num (value &optional (default 0d0))
  "VALUE as a double float, or DEFAULT when it is not a finite number."
  (if (%opt-finite-p value) (float value 1d0) (float default 1d0)))

(defun %opt-int (value &optional (default 0) minimum maximum)
  "VALUE floored to an integer, clamped into [MINIMUM, MAXIMUM] when given."
  (let ((out (floor (%opt-num value (float default 1d0)))))
    (when minimum (setf out (max minimum out)))
    (when maximum (setf out (min maximum out)))
    out))

(defun %opt-present (value)
  (if (eq value :null) nil value))

(defun %opt-key-present-p (object key)
  "Whether OBJECT carries KEY, under its own spelling or Core's alias."
  (and (%opt-object-p object)
       (or (nth-value 1 (gethash key object))
           (let ((alias (axllm/core::core-key-alias key)))
             (and alias (nth-value 1 (gethash alias object)))))))

(defun %opt-option (options keys &optional (default :null))
  "The first of KEYS present and non-null in OPTIONS, else DEFAULT."
  (dolist (key keys default)
    (let ((value (jget options key)))
      (unless (eq value :null)
        (return value)))))

(defun %opt-flag (options keys default)
  "A tri-state option: AX:FALSE means off, absent means DEFAULT."
  (let ((value (%opt-option options keys :null)))
    (cond ((eq value :null) default)
          ((json-false-p value) nil)
          (t (json-true-p value)))))

(defun %opt-merge (&rest objects)
  "A new object with each of OBJECTS written over the previous, in order."
  (let ((out (%new-object)))
    (dolist (source objects out)
      (when (%opt-object-p source)
        (dolist (key (%object-keys source))
          (%set-key out key (gethash key source)))))))

(defun %opt-string (value &optional (default ""))
  (if (stringp value) value default))

(defun %opt-function (value name)
  (cond ((null value) nil)
        ((eq value :null) nil)
        ((functionp value) value)
        ((and (symbolp value) (fboundp value)) (fdefinition value))
        (t (optimize-fail :config "~a must be a function of one argument, got ~s." name value))))

(defun %opt-same-p (left right)
  (axllm/core::core-value-equal left right))

;;; ------------------------------------------------------------------
;;; Deterministic randomness
;;; ------------------------------------------------------------------
;;;
;;; GEPA explores with randomness, and a run has to be reproducible from a
;;; seed alone.  CL:RANDOM reads and writes *RANDOM-STATE*, a process-global
;;; the caller also uses, so it is never touched here: each engine carries
;;; its own 32-bit xorshift state.  The generator is the one every Ax port
;;; uses (TypeScript src/ax/dsp/optimizers/gepa.ts), so the same seed and
;;; the same scores pick the same candidates in every language.

(defstruct (optimizer-rng (:constructor %make-optimizer-rng (state seed)))
  (state 123456789 :type (unsigned-byte 32))
  (seed 123456789 :type integer))

(defun make-optimizer-rng (&optional seed)
  "A seeded generator.  A missing, zero or non-integral SEED uses 123456789,
the shared Ax default, so an unseeded run is still reproducible."
  (let* ((requested (if (%opt-finite-p seed) (floor (%opt-num seed 0d0)) 0))
         (state (if (zerop requested) 123456789 (ldb (byte 32 0) requested))))
    (%make-optimizer-rng (if (zerop state) 123456789 state)
                         (if (zerop requested) 123456789 requested))))

(defun optimizer-rng-next (rng)
  "The next double in [0,1), advancing RNG and nothing else."
  (let ((state (optimizer-rng-state rng)))
    (setf state (ldb (byte 32 0) (logxor state (ldb (byte 32 0) (ash state 13)))))
    (setf state (ldb (byte 32 0) (logxor state (ash state -17))))
    (setf state (ldb (byte 32 0) (logxor state (ldb (byte 32 0) (ash state 5)))))
    (setf (optimizer-rng-state rng) state)
    (/ (float state 1d0) 4294967296d0)))

;;; ------------------------------------------------------------------
;;; Component value validators
;;; ------------------------------------------------------------------
;;;
;;; A validator answers T, or the reason the value is unusable.  The rules
;;; are TypeScript's axOptimizableValidators (src/ax/dsp/optimizable.ts):
;;; trim first, reject empty, cap the length, and require an identifier to
;;; start with a letter.
;;;
;;; This deliberately disagrees with the Python template's
;;; _gepa_validate_component_value, which tests ^[a-z_][a-z0-9_]*$ against
;;; the untrimmed value and so accepts a leading underscore and surrounding
;;; whitespace.  A component id is written into prompts and callable names,
;;; where a leading underscore is reserved, so the TypeScript rule is the
;;; one Ax actually means and the one this port follows.

(defparameter +js-whitespace-chars+ axllm/core::+js-whitespace+
  "The characters JavaScript's String.prototype.trim removes.

Shared with Core so a value trimmed here and a value trimmed inside a Core
op agree, including on U+00A0 and U+FEFF.")

(defun optimizable-snake-case-identifier (&optional (max-length 32))
  "A validator for a snake_case identifier of at most MAX-LENGTH characters.

The value is trimmed first.  A leading underscore is rejected: an
identifier must start with a lowercase letter."
  (lambda (value)
    (let ((text (string-trim +js-whitespace-chars+ (%opt-string value ""))))
      (cond ((zerop (length text)) "identifier must not be empty")
            ((> (length text) max-length)
             (format nil "identifier must be <= ~a chars" max-length))
            ((not (cl-ppcre:scan "^[a-z][a-z0-9_]*$" text))
             "identifier must be snake_case (a-z, 0-9, _; starting with a letter)")
            (t t)))))

(defun optimizable-preserves-placeholders (required)
  "A validator requiring every placeholder in REQUIRED to survive."
  (let ((required (mapcar (lambda (item) (%opt-string item (princ-to-string item)))
                          (%opt-list required))))
    (lambda (value)
      (let ((text (%opt-string value "")))
        (or (dolist (placeholder required nil)
              (unless (search placeholder text)
                (return (format nil "must preserve placeholder ~a" placeholder))))
            t)))))

(defun optimizable-non-empty ()
  "A validator requiring a value that is not blank once trimmed."
  (lambda (value)
    (if (plusp (length (string-trim +js-whitespace-chars+ (%opt-string value ""))))
        t
        "value must not be empty")))

(defun validate-component-value (component value)
  "T when VALUE may replace COMPONENT's current value, else the reason.

Reads the declaration a component carries: \"format\" (\"snake_case\"),
\"maxLength\", and \"preserve\", a list of literals that must survive."
  (let ((text (%opt-string value nil)))
    (cond
      ((null text) "component value must be a non-empty string")
      (t
       (let* ((format (%opt-present (jget component "format")))
              (max-length (%opt-present (jget component "maxLength")))
              (preserve (%opt-present (jget component "preserve")))
              (checks (list (optimizable-non-empty))))
         (when (equal format "snake_case")
           (push (optimizable-snake-case-identifier
                  (if (%opt-finite-p max-length) (floor (%opt-num max-length 32d0)) 32))
                 checks))
         (when (and (%opt-finite-p max-length) (not (equal format "snake_case")))
           (let ((limit (floor (%opt-num max-length 0d0))))
             (push (lambda (candidate)
                     (if (> (length (%opt-string candidate "")) limit)
                         (format nil "must be at most ~a characters" limit)
                         t))
                   checks)))
         (when preserve
           (push (optimizable-preserves-placeholders preserve) checks))
         (or (dolist (check (nreverse checks) nil)
               (let ((result (funcall check text)))
                 (unless (eq result t) (return result))))
             t))))))

;;; ------------------------------------------------------------------
;;; Evaluation metrics
;;; ------------------------------------------------------------------
;;;
;;; The metric helpers a caller scores rollouts with, ported from
;;; src/ax/dsp/eval.ts.  Text is normalized the way that file does, in that
;;; order: NFD, drop the articles a/an/the, squash whitespace runs to one
;;; space, delete ASCII punctuation, lowercase.  Tokens are then split on a
;;; single space, as JavaScript's String.split(' ') does, so an empty token
;;; left by punctuation removal still counts in a denominator.

(defparameter +eval-stopwords+
  (let ((table (make-hash-table :test 'equal)))
    (dolist (word
             '(
    "0o" "0s" "3a" "3b" "3d" "6b" "6o" "a" "a1" "a2" "a3" "a4" "ab" "able"
    "about" "above" "abst" "ac" "accordance" "according" "accordingly"
    "across" "act" "actually" "ad" "added" "adj" "ae" "af" "affected"
    "affecting" "affects" "after" "afterwards" "ag" "again" "against" "ah"
    "ain" "ain't" "aj" "al" "all" "allow" "allows" "almost" "alone" "along"
    "already" "also" "although" "always" "am" "among" "amongst" "amoungst"
    "amount" "an" "and" "announce" "another" "any" "anybody" "anyhow"
    "anymore" "anyone" "anything" "anyway" "anyways" "anywhere" "ao" "ap"
    "apart" "apparently" "appear" "appreciate" "appropriate" "approximately"
    "ar" "are" "aren" "arent" "aren't" "arise" "around" "as" "a's" "aside"
    "ask" "asking" "associated" "at" "au" "auth" "av" "available" "aw"
    "away" "awfully" "ax" "ay" "az" "b" "b1" "b2" "b3" "ba" "back" "bc" "bd"
    "be" "became" "because" "become" "becomes" "becoming" "been" "before"
    "beforehand" "begin" "beginning" "beginnings" "begins" "behind" "being"
    "believe" "below" "beside" "besides" "best" "better" "between" "beyond"
    "bi" "bill" "biol" "bj" "bk" "bl" "bn" "both" "bottom" "bp" "br" "brief"
    "briefly" "bs" "bt" "bu" "but" "bx" "by" "c" "c1" "c2" "c3" "ca" "call"
    "came" "can" "cannot" "cant" "can't" "cause" "causes" "cc" "cd" "ce"
    "certain" "certainly" "cf" "cg" "ch" "changes" "ci" "cit" "cj" "cl"
    "clearly" "cm" "c'mon" "cn" "co" "com" "come" "comes" "con" "concerning"
    "consequently" "consider" "considering" "contain" "containing"
    "contains" "corresponding" "could" "couldn" "couldnt" "couldn't"
    "course" "cp" "cq" "cr" "cry" "cs" "c's" "ct" "cu" "currently" "cv" "cx"
    "cy" "cz" "d" "d2" "da" "date" "dc" "dd" "de" "definitely" "describe"
    "described" "despite" "detail" "df" "di" "did" "didn" "didn't"
    "different" "dj" "dk" "dl" "do" "does" "doesn" "doesn't" "doing" "don"
    "done" "don't" "down" "downwards" "dp" "dr" "ds" "dt" "du" "due"
    "during" "dx" "dy" "e" "e2" "e3" "ea" "each" "ec" "ed" "edu" "ee" "ef"
    "effect" "eg" "ei" "eight" "eighty" "either" "ej" "el" "eleven" "else"
    "elsewhere" "em" "empty" "en" "end" "ending" "enough" "entirely" "eo"
    "ep" "eq" "er" "es" "especially" "est" "et" "et-al" "etc" "eu" "ev"
    "even" "ever" "every" "everybody" "everyone" "everything" "everywhere"
    "ex" "exactly" "example" "except" "ey" "f" "f2" "fa" "far" "fc" "few"
    "ff" "fi" "fifteen" "fifth" "fify" "fill" "find" "fire" "first" "five"
    "fix" "fj" "fl" "fn" "fo" "followed" "following" "follows" "for"
    "former" "formerly" "forth" "forty" "found" "four" "fr" "from" "front"
    "ft" "fu" "full" "further" "furthermore" "fy" "g" "ga" "gave" "ge" "get"
    "gets" "getting" "gi" "give" "given" "gives" "giving" "gj" "gl" "go"
    "goes" "going" "gone" "got" "gotten" "gr" "greetings" "gs" "gy" "h" "h2"
    "h3" "had" "hadn" "hadn't" "happens" "hardly" "has" "hasn" "hasnt"
    "hasn't" "have" "haven" "haven't" "having" "he" "hed" "he'd" "he'll"
    "hello" "help" "hence" "her" "here" "hereafter" "hereby" "herein"
    "heres" "here's" "hereupon" "hers" "herself" "hes" "he's" "hh" "hi"
    "hid" "him" "himself" "his" "hither" "hj" "ho" "home" "hopefully" "how"
    "howbeit" "however" "how's" "hr" "hs" "http" "hu" "hundred" "hy" "i"
    "i2" "i3" "i4" "i6" "i7" "i8" "ia" "ib" "ibid" "ic" "id" "i'd" "ie" "if"
    "ig" "ignored" "ih" "ii" "ij" "il" "i'll" "im" "i'm" "immediate"
    "immediately" "importance" "important" "in" "inasmuch" "inc" "indeed"
    "index" "indicate" "indicated" "indicates" "information" "inner"
    "insofar" "instead" "interest" "into" "invention" "inward" "io" "ip"
    "iq" "ir" "is" "isn" "isn't" "it" "itd" "it'd" "it'll" "its" "it's"
    "itself" "iv" "i've" "ix" "iy" "iz" "j" "jj" "jr" "js" "jt" "ju" "just"
    "k" "ke" "keep" "keeps" "kept" "kg" "kj" "km" "know" "known" "knows"
    "ko" "l" "l2" "la" "largely" "last" "lately" "later" "latter" "latterly"
    "lb" "lc" "le" "least" "les" "less" "lest" "let" "lets" "let's" "lf"
    "like" "liked" "likely" "line" "little" "lj" "ll" "ln" "lo" "look"
    "looking" "looks" "los" "lr" "ls" "lt" "ltd" "m" "m2" "ma" "made"
    "mainly" "make" "makes" "many" "may" "maybe" "me" "mean" "means"
    "meantime" "meanwhile" "merely" "mg" "might" "mightn" "mightn't" "mill"
    "million" "mine" "miss" "ml" "mn" "mo" "more" "moreover" "most" "mostly"
    "move" "mr" "mrs" "ms" "mt" "mu" "much" "mug" "must" "mustn" "mustn't"
    "my" "myself" "model" "n" "n2" "na" "name" "namely" "nay" "nc" "nd" "ne"
    "near" "nearly" "necessarily" "necessary" "need" "needn" "needn't"
    "needs" "neither" "never" "nevertheless" "new" "next" "ng" "ni" "nine"
    "ninety" "nj" "nl" "nn" "no" "nobody" "non" "none" "nonetheless" "noone"
    "nor" "normally" "nos" "not" "noted" "nothing" "novel" "now" "nowhere"
    "nr" "ns" "nt" "ny" "o" "oa" "ob" "obtain" "obtained" "obviously" "oc"
    "od" "of" "off" "often" "og" "oh" "oi" "oj" "ok" "okay" "ol" "old" "om"
    "omitted" "on" "once" "one" "ones" "only" "onto" "oo" "op" "oq" "or"
    "ord" "os" "ot" "other" "others" "otherwise" "ou" "ought" "our" "ours"
    "ourselves" "out" "outside" "over" "overall" "ow" "owing" "own" "ox"
    "oz" "p" "p1" "p2" "p3" "page" "pagecount" "pages" "par" "part"
    "particular" "particularly" "pas" "past" "pc" "pd" "pe" "per" "perhaps"
    "pf" "ph" "pi" "pj" "pk" "pl" "placed" "please" "plus" "pm" "pn" "po"
    "poorly" "possible" "possibly" "potentially" "pp" "pq" "pr"
    "predominantly" "present" "presumably" "previously" "primarily"
    "probably" "promptly" "proud" "provides" "ps" "pt" "pu" "put" "py" "q"
    "qj" "qu" "que" "quickly" "quite" "qv" "r" "r2" "ra" "ran" "rather" "rc"
    "rd" "re" "readily" "really" "reasonably" "recent" "recently" "ref"
    "refs" "regarding" "regardless" "regards" "related" "relatively"
    "research" "research-articl" "respectively" "resulted" "resulting"
    "results" "rf" "rh" "ri" "right" "rj" "rl" "rm" "rn" "ro" "rq" "rr" "rs"
    "rt" "ru" "run" "rv" "ry" "s" "s2" "sa" "said" "same" "saw" "say"
    "saying" "says" "sc" "sd" "se" "sec" "second" "secondly" "section" "see"
    "seeing" "seem" "seemed" "seeming" "seems" "seen" "self" "selves"
    "sensible" "sent" "serious" "seriously" "seven" "several" "sf" "shall"
    "shan" "shan't" "she" "shed" "she'd" "she'll" "shes" "she's" "should"
    "shouldn" "shouldn't" "should've" "show" "showed" "shown" "showns"
    "shows" "si" "side" "significant" "significantly" "similar" "similarly"
    "since" "sincere" "six" "sixty" "sj" "sl" "slightly" "sm" "sn" "so"
    "some" "somebody" "somehow" "someone" "somethan" "something" "sometime"
    "sometimes" "somewhat" "somewhere" "soon" "sorry" "sp" "specifically"
    "specified" "specify" "specifying" "sq" "sr" "ss" "st" "still" "stop"
    "strongly" "sub" "substantially" "successfully" "such" "sufficiently"
    "suggest" "sup" "sure" "sy" "system" "sz" "t" "t1" "t2" "t3" "take"
    "taken" "taking" "tb" "tc" "td" "te" "tell" "ten" "tends" "tf" "th"
    "than" "thank" "thanks" "thanx" "that" "that'll" "thats" "that's"
    "that've" "the" "their" "theirs" "them" "themselves" "then" "thence"
    "there" "thereafter" "thereby" "thered" "therefore" "therein" "there'll"
    "thereof" "therere" "theres" "there's" "thereto" "thereupon" "there've"
    "these" "they" "theyd" "they'd" "they'll" "theyre" "they're" "they've"
    "thickv" "thin" "think" "third" "this" "thorough" "thoroughly" "those"
    "thou" "though" "thoughh" "thousand" "three" "throug" "through"
    "throughout" "thru" "thus" "ti" "til" "tip" "tj" "tl" "tm" "tn" "to"
    "together" "too" "took" "top" "toward" "towards" "tp" "tq" "tr" "tried"
    "tries" "truly" "try" "trying" "ts" "t's" "tt" "tv" "twelve" "twenty"
    "twice" "two" "tx" "u" "u201d" "ue" "ui" "uj" "uk" "um" "un" "under"
    "unfortunately" "unless" "unlike" "unlikely" "until" "unto" "uo" "up"
    "upon" "ups" "ur" "us" "use" "used" "useful" "usefully" "usefulness"
    "uses" "using" "usually" "ut" "v" "va" "value" "various" "vd" "ve"
    "very" "via" "viz" "vj" "vo" "vol" "vols" "volumtype" "vq" "vs" "vt"
    "vu" "w" "wa" "want" "wants" "was" "wasn" "wasnt" "wasn't" "way" "we"
    "wed" "we'd" "welcome" "well" "we'll" "well-b" "went" "were" "we're"
    "weren" "werent" "weren't" "we've" "what" "whatever" "what'll" "whats"
    "what's" "when" "whence" "whenever" "when's" "where" "whereafter"
    "whereas" "whereby" "wherein" "wheres" "where's" "whereupon" "wherever"
    "whether" "which" "while" "whim" "whither" "who" "whod" "whoever"
    "whole" "who'll" "whom" "whomever" "whos" "who's" "whose" "why" "why's"
    "wi" "widely" "will" "willing" "wish" "with" "within" "without" "wo"
    "won" "wonder" "wont" "won't" "words" "world" "would" "wouldn" "wouldnt"
    "wouldn't" "www" "x" "x1" "x2" "x3" "xf" "xi" "xj" "xk" "xl" "xn" "xo"
    "xs" "xt" "xv" "xx" "y" "y2" "yes" "yet" "yj" "yl" "you" "youd" "you'd"
    "you'll" "your" "youre" "you're" "yours" "yourself" "yourselves"
    "you've" "yr" "ys" "yt" "z" "zero" "zi" "zz" "task"
               )
             table)
      (setf (gethash word table) t)))
  "The stopword set src/ax/dsp/stopwords.ts defines, as an EQUAL table.

Transcribed from that file, deduplicated, order irrelevant.  The novel-F1
metric is meaningless without it: it is what tells \"the capital of France
is Paris\" apart from \"Paris\".")

(defun %eval-split-whitespace (text)
  "TEXT split on runs of whitespace, as JavaScript's split(/\\s+/) does.

JavaScript keeps the empty piece a leading or trailing whitespace run
produces, and those empty pieces survive into the token lists the metrics
count, so they are kept here too: dropping them would make this port score
differently from every other one."
  (let ((parts '())
        (start 0)
        (limit (length text)))
    (loop
      (let ((run (position-if (lambda (character)
                                (find character #.(coerce '(#\Space #\Tab #\Newline #\Return
                                                            #\Page #\Linefeed)
                                                          'string)))
                              text :start start)))
        (unless run
          (push (subseq text start) parts)
          (return))
        (push (subseq text start run) parts)
        (setf start run)
        (loop while (and (< start limit)
                         (find (char text start)
                               #.(coerce '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)
                                         'string)))
              do (incf start))))
    (nreverse parts)))

(defun normalize-eval-text (text)
  "TEXT normalized the way every Ax evaluation metric normalizes it.

The article removal is case sensitive, as the reference regular expression
is, and it runs before the lowercasing: \"The dog\" keeps its article and
\"the dog\" loses it.  That is the shared behaviour, not an oversight here."
  (let* ((text (sb-unicode:normalize-string (%opt-string text "") :nfd))
         (text (cl-ppcre:regex-replace-all "\\b(a|an|the)\\b" text " "))
         (text (format nil "~{~a~^ ~}" (%eval-split-whitespace text)))
         (text (remove-if (lambda (character)
                            (find character "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))
                          text)))
    (string-downcase text)))

(defun %eval-split-space (text)
  "TEXT split on single spaces, empty pieces kept, as JavaScript does."
  (let ((parts '())
        (start 0))
    (loop for index = (position #\Space text :start start)
          while index
          do (push (subseq text start index) parts)
             (setf start (1+ index)))
    (push (subseq text start) parts)
    (nreverse parts)))

(defun %eval-tokens (text) (%eval-split-space (normalize-eval-text text)))

(defun %eval-counts (tokens)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (token tokens table)
      (setf (gethash token table) (1+ (gethash token table 0))))))

(defun %eval-overlap (prediction-tokens truth-tokens)
  "How many tokens the two bags share, counting repeats."
  (let ((predicted (%eval-counts prediction-tokens))
        (truth (%eval-counts truth-tokens))
        (same 0))
    (maphash (lambda (token count)
               (incf same (min count (gethash token truth 0))))
             predicted)
    same))

(defun %eval-f1 (overlap prediction-tokens truth-tokens return-recall)
  "The F1 (or recall) of OVERLAP against the two token lists.

Zero overlap and an empty side both score 0 rather than dividing by zero."
  (if (or (zerop overlap) (null prediction-tokens) (null truth-tokens))
      0d0
      (let ((precision (/ (float overlap 1d0) (length prediction-tokens)))
            (recall (/ (float overlap 1d0) (length truth-tokens))))
        (if return-recall
            recall
            (if (zerop (+ precision recall))
                0d0
                (/ (* 2d0 precision recall) (+ precision recall)))))))

(defun em-score (prediction ground-truth)
  "1 when PREDICTION and GROUND-TRUTH normalize to the same text, else 0."
  (if (string= (normalize-eval-text prediction) (normalize-eval-text ground-truth))
      1d0
      0d0))

(defun f1-score (prediction ground-truth)
  "The token-overlap F1 of PREDICTION against GROUND-TRUTH."
  (let ((prediction-tokens (%eval-tokens prediction))
        (truth-tokens (%eval-tokens ground-truth)))
    (%eval-f1 (%eval-overlap prediction-tokens truth-tokens)
              prediction-tokens truth-tokens nil)))

(defun novel-f1-score (history prediction ground-truth &optional return-recall)
  "F1 over the tokens PREDICTION contributes that HISTORY did not.

Stopwords and every token already in HISTORY are dropped from both sides
first, so repeating the context back scores nothing and only new, contentful
agreement counts.  RETURN-RECALL asks for recall instead of F1.

This is the metric src/ax/dsp/eval.ts documents but does not compute: that
implementation fixes the overlap at zero (`const numSame = 0`), so every
call divides zero by zero and returns NaN, and recall is always 0.  This
port implements the documented arithmetic instead of reproducing the
placeholder; a caller moving from the TypeScript helper will see real
scores where it saw NaN."
  (let* ((excluded (%eval-tokens history))
         (keep (lambda (tokens)
                 (remove-if (lambda (token)
                              (or (gethash token +eval-stopwords+)
                                  (member token excluded :test #'string=)))
                            tokens)))
         (prediction-tokens (funcall keep (%eval-tokens prediction)))
         (truth-tokens (funcall keep (%eval-tokens ground-truth))))
    (%eval-f1 (%eval-overlap prediction-tokens truth-tokens)
              prediction-tokens truth-tokens return-recall)))

;;; ------------------------------------------------------------------
;;; Program protocol
;;; ------------------------------------------------------------------
;;;
;;; src/gen.lisp owns FORWARD, PROGRAM-OPTIMIZABLE-COMPONENTS,
;;; PROGRAM-APPLY-OPTIMIZED-COMPONENTS, PROGRAM-TRACES, PROGRAM-CHAT-LOG and
;;; PROGRAM-USAGE.  The optimizer adds the four below, which only it needs.

(defgeneric program-kind (program)
  (:documentation
   "PROGRAM's kind for the optimizer contract.

These are wire values the fixtures pin, not labels: \"axgen\", \"axagent\"
and \"flow\".  The value reaches the engine as request.programKind and the
artifact as provenance.sourceProgramKind, so an agent reporting \"agent\"
fails scripted-engine-apply.json.")
  (:method ((program t)) "unknown")
  (:method ((program generator)) "axgen"))

(defgeneric program-optimizer-trace (program)
  (:documentation
   "The trace an optimizer request carries for PROGRAM.

The default is the shape every port sends: the program's recorded traces
and chat log.  A program with another record specializes this.")
  (:method ((program t))
    (object "traces" (%opt-array (%opt-list (program-traces program)))
            "chat_log" (%opt-array (%opt-list (program-chat-log program))))))

(defgeneric program-function-calls (program)
  (:documentation
   "The tool or runtime calls PROGRAM made during its last rollout.

They are what a task's expectedActions and forbiddenActions are scored
against.  There is deliberately no default: a program that can call tools
and reports an empty history would score as though it called nothing, which
silently inflates or deflates every action-adjusted score.  A program that
records calls must say so; one that cannot is refused by name the moment a
task actually depends on its calls, and is otherwise left alone.")
  (:method ((program generator)) (generator-function-call-traces program)))

(defun %opt-records-calls-p (program)
  "Whether PROGRAM can report the calls it made."
  (and (compute-applicable-methods #'program-function-calls (list program)) t))

(defun %opt-function-calls (program)
  "PROGRAM's calls, or :NULL when it cannot report them."
  (if (%opt-records-calls-p program)
      (%opt-array (%opt-list (program-function-calls program)))
      :null))

(defun %opt-task-needs-calls-p (task)
  (or (plusp (%opt-count (jget task "expectedActions")))
      (plusp (%opt-count (jget task "forbiddenActions")))))

(defgeneric program-evaluate-task (program client task &key options)
  (:documentation
   "Run TASK through PROGRAM against CLIENT and return its eval prediction.

The prediction a playbook round is judged and diagnosed on: its
completionType, output, actionLog, failureSignals, functionCalls and
toolErrors are what the failure clustering and the weakness miner read.

There is deliberately no default.  A program that cannot be evaluated but
quietly answered with an empty prediction would make every task look like a
failed run, and evolve would mine weaknesses out of its own inability to
run the program rather than out of anything the program did.  A program
that can be evaluated says so; one that cannot is refused by name."))

(defgeneric program-set-demos (program demos)
  (:documentation
   "Install DEMOS, the few-shot traces an artifact carries, on PROGRAM.

Only called when an artifact actually carries demos.  The default refuses,
because silently dropping mined demos would make a BootstrapFewShot run
look applied when it was not.")
  (:method ((program t) demos)
    (declare (ignore demos))
    (optimize-fail :artifact
                   "~s does not accept optimizer demos; implement PROGRAM-SET-DEMOS for it."
                   (type-of program))))

;;; ------------------------------------------------------------------
;;; Engine and evaluator protocols
;;; ------------------------------------------------------------------

(defclass optimizer-engine () ()
  (:documentation "Base class for an optimizer engine."))

(defgeneric optimizer-engine-name (engine)
  (:documentation "ENGINE's name, written into the artifact as optimizerName."))

(defgeneric optimizer-engine-version (engine)
  (:documentation "ENGINE's version, written into the artifact as optimizerVersion."))

(defgeneric run-optimizer-engine (engine request evaluator)
  (:documentation
   "Run ENGINE over REQUEST and return an optimized artifact.

REQUEST is the Core optimizer request: contractVersion, programKind,
components, dataset, options, trace and evaluator.  EVALUATOR answers
EVALUATE-CANDIDATE, or is NIL when the caller supplied no client; an engine
that needs measured scores must refuse rather than invent them.

The returned object is normalized and validated by Core before it is
applied, so an engine may leave artifactVersion, optimizerName,
optimizerVersion, metadata, provenance and evidence out."))

(defgeneric evaluate-candidate (evaluator candidate-map options)
  (:documentation
   "Score CANDIDATE-MAP and return a Core eval result.

OPTIONS is a JSON object; the keys every engine here sends are \"dataset\"
(the tasks to run, defaulting to the evaluator's own), \"phase\" (a label
that reaches the result) and \"captureTraces\".  The result carries phase,
candidateMap, rows, sum, avg and count, built by Core."))

;;; ------------------------------------------------------------------
;;; The native candidate evaluator
;;; ------------------------------------------------------------------
;;;
;;; One evaluator serves AxGen, AxAgent and AxFlow: it only speaks the
;;; program protocol.  A candidate is applied, every task is rolled out, and
;;; the program is restored -- on an error, a cancellation and a budget stop
;;; as well, so an aborted trial never leaves the caller's program mutated.

(defclass program-evaluator ()
  ((program :initarg :program :reader evaluator-program)
   (client :initarg :client :reader evaluator-client)
   (dataset :initarg :dataset :reader evaluator-dataset)
   (options :initarg :options :reader evaluator-options)
   (rollout :initarg :rollout :reader evaluator-rollout)
   (metric :initarg :metric :reader evaluator-metric)
   (max-metric-calls :initarg :max-metric-calls :reader evaluator-max-metric-calls)
   (metric-calls :initform 0 :accessor evaluator-metric-calls)
   (cancel :initarg :cancel :reader evaluator-cancel))
  (:documentation
   "Scores candidate component maps by really running PROGRAM.

Created with MAKE-PROGRAM-EVALUATOR.  EVALUATOR-METRIC-CALLS counts the
rollouts spent so far, across every candidate."))

(defun make-program-evaluator (program client &key dataset options rollout metric
                                                   max-metric-calls cancel)
  "An evaluator that scores candidates by running PROGRAM against CLIENT.

DATASET is the default task set, a JSON array or a {train, validation}
object; a call may override it.  ROLLOUT, when given, replaces FORWARD and
is called as (ROLLOUT PROGRAM CLIENT INPUTS OPTIONS), returning the output
value and optionally a usage object.  METRIC, when given, is called as
(METRIC TASK PREDICTION) and returns a raw score: a number, or a JSON
object of named scores.  MAX-METRIC-CALLS is a hard ceiling across the
whole run; exceeding it signals an OPTIMIZE-ERROR of kind :budget.  CANCEL
is a function of no arguments checked before every rollout; a true result
signals kind :cancelled."
  (make-instance 'program-evaluator
                 :program program
                 :client client
                 :dataset (or dataset (%new-array))
                 :options (if (%opt-object-p options) options (%new-object))
                 :rollout (%opt-function rollout "rollout")
                 :metric (%opt-function metric "metric")
                 :max-metric-calls (when max-metric-calls (%opt-int max-metric-calls 0 0))
                 :cancel (%opt-function cancel "cancel")))

(defun evaluator-budget-remaining (evaluator)
  "Rollouts EVALUATOR may still spend, or :UNLIMITED when it has no ceiling."
  (let ((limit (evaluator-max-metric-calls evaluator)))
    (if limit
        (max 0 (- limit (evaluator-metric-calls evaluator)))
        :unlimited)))

(defun %opt-check-cancelled (cancel)
  (when (and cancel (funcall cancel))
    (optimize-fail :cancelled "Optimization was cancelled by the caller's cancellation check.")))

(defun %opt-spend-metric-call (evaluator)
  (let ((limit (evaluator-max-metric-calls evaluator)))
    (when (and limit (>= (evaluator-metric-calls evaluator) limit))
      (optimize-fail :budget
                     "max metric calls exceeded: the budget of ~a rollout(s) is spent"
                     limit))
    (incf (evaluator-metric-calls evaluator))))

(defun %opt-task-inputs (task)
  "The input object a task rolls out with."
  (let ((input (jget task "input")))
    (cond ((%opt-object-p input) input)
          ((%opt-object-p task) task)
          (t (%new-object)))))

(defun %opt-raw-score (task prediction metric)
  "The unnormalized score for one rollout.

A task that carries its own score wins, so a fixture dataset decides the
ranking rather than the program.  Otherwise a failed rollout scores 0 and a
completed one scores 1, which is what every other port does."
  (cond (metric (funcall metric task prediction))
        ((%opt-key-present-p task "metric_score") (jget task "metric_score"))
        ((%opt-key-present-p task "scores") (jget task "scores"))
        ((%opt-key-present-p task "score") (jget task "score"))
        ((equal (%opt-present (jget prediction "completionType")) "error") 0)
        (t 1)))

(defun %opt-score-prediction (task prediction options metric program)
  "(values SCORES SCALAR) for one rollout, every step Core-owned.

A task that names expected or forbidden actions is scored against the calls
the program really made, so a program that cannot report its calls is
refused here rather than scored as though it made none."
  (when (and (%opt-task-needs-calls-p task) (not (%opt-records-calls-p program)))
    (optimize-fail :components
                   "~s cannot report the calls it made, so a task with expectedActions or forbiddenActions cannot be scored against it; implement PROGRAM-FUNCTION-CALLS for it."
                   (type-of program)))
  (let* ((scores (axllm/core::normalize-optimization-metric-scores
                  (%opt-raw-score task prediction metric)))
         (scalar (axllm/core::scalarize-optimization-scores scores options))
         (adjusted (axllm/core::adjust-optimization-score-for-actions scalar task prediction)))
    (values scores (%opt-num adjusted 0d0))))

(defun %opt-rollout (evaluator task options)
  "Run one task and return its prediction object, errors included."
  (let* ((program (evaluator-program evaluator))
         (client (evaluator-client evaluator))
         (rollout (evaluator-rollout evaluator))
         (inputs (%opt-task-inputs task))
         (forward-options (let ((given (%opt-option options '("forward_options" "forwardOptions"))))
                            (if (%opt-object-p given) given (%new-object)))))
    (handler-case
        (multiple-value-bind (output usage)
            (if rollout
                (funcall rollout program client inputs forward-options)
                (forward program client inputs forward-options))
          (values (object "completionType" "final"
                          "output" output
                          "finalOutput" output
                          "functionCalls" (%opt-function-calls program)
                          "actionLog" (%opt-array (%opt-list (program-chat-log program)))
                          "usage" (if (%opt-object-p usage) usage (%new-object))
                          "trace" (object "traces" (%opt-array (%opt-list (program-traces program)))))
                  :null))
      ;; A cancellation or a budget stop is the run ending, not one task
      ;; failing: it must not be scored as a bad candidate.
      (optimize-error (condition)
        (if (member (optimize-error-kind condition) '(:cancelled :budget))
            (error condition)
            (%opt-failed-prediction evaluator condition)))
      (error (condition)
        (%opt-failed-prediction evaluator condition)))))

(defun %opt-failed-prediction (evaluator condition)
  (let ((program (evaluator-program evaluator))
        (error-object (object "message" (princ-to-string condition))))
    (values (object "completionType" "error"
                    "error" error-object
                    "functionCalls" (%opt-function-calls program)
                    "actionLog" (%opt-array (%opt-list (program-chat-log program)))
                    "usage" (%new-object)
                    "trace" (object "traces" (%opt-array (%opt-list (program-traces program)))))
            error-object)))

(defmethod evaluate-candidate ((evaluator program-evaluator) candidate-map options)
  (let* ((options (if (%opt-object-p options) options (%new-object)))
         (merged (%opt-merge (evaluator-options evaluator) options))
         (program (evaluator-program evaluator))
         (candidate (if (%opt-object-p candidate-map) candidate-map (%new-object)))
         (dataset (let ((given (%opt-option merged '("dataset"))))
                    (if (eq given :null) (evaluator-dataset evaluator) given)))
         (normalized (axllm/core::normalize-optimization-dataset dataset))
         (tasks (%opt-list (jget normalized "train")))
         (phase (%opt-string (%opt-present (jget merged "phase")) "train"))
         (original (axllm/core::optimization-component-current-map
                    (program-optimizable-components program)))
         (rows (%new-array)))
    (unwind-protect
         (progn
           (when (plusp (hash-table-count candidate))
             (program-apply-optimized-components program candidate))
           (dolist (task tasks)
             (%opt-check-cancelled (evaluator-cancel evaluator))
             (%opt-spend-metric-call evaluator)
             (multiple-value-bind (prediction failure) (%opt-rollout evaluator task merged)
               (multiple-value-bind (scores scalar)
                   (%opt-score-prediction (if (%opt-object-p task) task (%new-object))
                                          prediction merged (evaluator-metric evaluator)
                                          program)
                 (vector-push-extend
                  (axllm/core::build-optimization-eval-row
                   task prediction scores scalar (jget prediction "trace") failure)
                  rows))))
           (axllm/core::build-optimization-eval-result rows (%opt-clone candidate) phase))
      ;; Restoring the program is the evaluator's whole contract with its
      ;; caller: a candidate is a measurement, never an edit.
      (program-apply-optimized-components program original))))

(defun optimizer-evidence-batch (eval-result components)
  "The Core evidence batch for EVAL-RESULT over COMPONENTS.

Engines that reflect on measured rollouts send this to their teacher."
  (axllm/core::build-optimizer-evidence-batch eval-result components))

;;; ------------------------------------------------------------------
;;; Shared engine helpers
;;; ------------------------------------------------------------------

(defun %opt-request-components (request)
  "REQUEST's components, deep-copied, keeping only those with a string value.

A component whose current value is not a string is not text an engine can
propose a replacement for, so it is left alone rather than corrupted."
  (let ((out '()))
    (dolist (component (%opt-list (jget request "components")) (nreverse out))
      (when (and (%opt-object-p component)
                 (stringp (jget component "current" "")))
        (push (%opt-clone component) out)))))

(defun %opt-request-options (engine-options request)
  (%opt-merge engine-options (jget request "options")))

(defun %opt-dataset-split (request)
  "(values TRAIN VALIDATION) from REQUEST's dataset, validation defaulting to train."
  (let* ((dataset (axllm/core::normalize-optimization-dataset (jget request "dataset")))
         (train (%opt-list (jget dataset "train")))
         (validation (%opt-list (jget dataset "validation"))))
    (values train (or validation train))))

(defun %opt-dataset-object (tasks)
  (object "train" (%opt-array tasks) "validation" (%new-array)))

(defun %opt-component-ids (components)
  (mapcar (lambda (component) (%opt-string (jget component "id") "")) components))

(defun %opt-current-map (components)
  (axllm/core::optimization-component-current-map (%opt-array components)))

(defun %opt-demo (row)
  "The demo one accepted row contributes."
  (object "programId" "root"
          "traces" (%opt-array (list (%opt-clone
                                      (let ((prediction (jget row "prediction")))
                                        (if (eq prediction :null)
                                            (jget row "input" (%new-object))
                                            prediction)))))))

(defun %opt-row-scalar (row) (%opt-num (jget row "scalar") 0d0))

(defun %opt-result-rows (result) (%opt-list (jget result "rows")))

(defun %opt-avg-vector (rows)
  "The mean of each named score across ROWS, in sorted key order."
  (let ((sums (%new-object))
        (counts (make-hash-table :test 'equal))
        (keys '()))
    (dolist (row rows)
      (let ((scores (jget row "scores")))
        (when (%opt-object-p scores)
          (dolist (key (%object-keys scores))
            (let ((value (gethash key scores)))
              (when (%opt-finite-p value)
                (pushnew key keys :test #'string=)
                (%set-key sums key (+ (%opt-num (jget sums key) 0d0) (%opt-num value 0d0)))
                (setf (gethash key counts) (1+ (gethash key counts 0)))))))))
    (let ((out (%new-object)))
      (dolist (key (sort keys #'string<) out)
        (%set-key out key (/ (%opt-num (jget sums key) 0d0)
                             (float (max 1 (gethash key counts 1)) 1d0)))))))

(defun %opt-scalarize (scores options)
  (%opt-num (axllm/core::scalarize-optimization-scores scores options) 0d0))

(defun %opt-dominates-p (left right epsilon)
  "Whether LEFT dominates RIGHT: at least as good everywhere, better somewhere."
  (let ((keys '())
        (at-least t)
        (strictly nil))
    (dolist (source (list left right))
      (when (%opt-object-p source)
        (dolist (key (%object-keys source))
          (pushnew key keys :test #'string=))))
    (dolist (key (nreverse keys))
      (let ((a (%opt-num (jget left key) 0d0))
            (b (%opt-num (jget right key) 0d0)))
        (when (< (+ a epsilon) b)
          (setf at-least nil)
          (return))
        (when (> a (+ b epsilon))
          (setf strictly t))))
    (and at-least strictly)))

(defun %opt-pareto-front (candidates epsilon)
  "The non-dominated CANDIDATES, each with how many others it dominates.

CANDIDATES is a vector of objects carrying a \"scores\" object.  Returns a
list of (INDEX SCORES DOMINATED-COUNT)."
  (let ((front '())
        (count (length candidates)))
    (dotimes (i count (nreverse front))
      (let ((item (aref candidates i))
            (dominated nil)
            (dominates 0))
        (dotimes (j count)
          (unless (= i j)
            (let ((other (aref candidates j)))
              (when (%opt-dominates-p (jget other "scores") (jget item "scores") epsilon)
                (setf dominated t)
                (return))
              (when (%opt-dominates-p (jget item "scores") (jget other "scores") epsilon)
                (incf dominates)))))
        (unless dominated
          (push (list i (%opt-clone (jget item "scores")) dominates) front))))))

(defun %opt-hypervolume-2d (score-objects)
  "The 2D hypervolume of a front, or :NULL when it is not two-objective."
  (if (null score-objects)
      :null
      (let ((keys (and (%opt-object-p (first score-objects))
                       (%object-keys (first score-objects)))))
        (if (/= (length keys) 2)
            :null
            (let* ((k1 (first keys))
                   (k2 (second keys))
                   (sorted (sort (copy-list score-objects) #'>
                                 :key (lambda (point) (%opt-num (jget point k1) 0d0))))
                   (hypervolume 0d0)
                   (previous-y 0d0))
              (dolist (point sorted hypervolume)
                (let* ((x (%opt-num (jget point k1) 0d0))
                       (y (%opt-num (jget point k2) 0d0))
                       (dy (max (- y previous-y) 0d0)))
                  (incf hypervolume (* x dy))
                  (setf previous-y (max previous-y y)))))))))

;;; ------------------------------------------------------------------
;;; BootstrapFewShot
;;; ------------------------------------------------------------------

(defclass bootstrap-few-shot (optimizer-engine)
  ((options :initarg :options :reader bootstrap-options))
  (:documentation
   "Mines few-shot demos from the training set by really scoring each example.

An example becomes a demo only when its measured scalar reaches
qualityThreshold, so the artifact's demo list is evidence, not a copy of the
input.  The component map is left empty: this engine changes examples, not
prompt text."))

(defun make-bootstrap-few-shot (&optional options)
  "A BootstrapFewShot engine.  OPTIONS is a JSON object; a request's own
options are layered over it.  Recognized keys, with defaults:

  qualityThreshold  0.5   the scalar an example must reach to be kept
  maxRounds         3     passes over the sample
  maxExamples       16    training examples sampled
  maxDemos          4     demos to collect before stopping
  batchSize         1     examples evaluated per inner batch
  teacherOptions    none   forwarded to the evaluator as forward_options"
  (make-instance 'bootstrap-few-shot
                 :options (if (%opt-object-p options) (%opt-clone options) (%new-object))))

(defmethod optimizer-engine-name ((engine bootstrap-few-shot)) "BootstrapFewShot")
(defmethod optimizer-engine-version ((engine bootstrap-few-shot)) "axir-bootstrap-fewshot-v1")

(defmethod run-optimizer-engine ((engine bootstrap-few-shot) request evaluator)
  (unless evaluator
    (optimize-fail :evaluator "AxBootstrapFewShot requires an OptimizerEvaluator."))
  (let* ((options (%opt-request-options (bootstrap-options engine) request))
         (components (%opt-request-components request))
         (threshold (%opt-num (%opt-option options '("qualityThreshold" "quality_threshold")) 0.5d0))
         (max-rounds (%opt-int (%opt-option options '("maxRounds" "max_rounds")) 3 1))
         (max-examples (%opt-int (%opt-option options '("maxExamples" "max_examples")) 16 1))
         (max-demos (%opt-int (%opt-option options '("maxDemos" "max_demos")) 4 1))
         (batch-size (%opt-int (%opt-option options '("batchSize" "batch_size")) 1 1))
         (teacher-options (%opt-option options '("teacherOptions" "teacher_options")))
         (base-cfg (%opt-current-map components))
         (demos '())
         (demo-count 0)
         (accepted '())
         (total-calls 0))
    (multiple-value-bind (train) (%opt-dataset-split request)
      (let ((sampled (subseq train 0 (min max-examples (length train)))))
        (block mining
          (dotimes (round-index max-rounds)
            (when (>= demo-count max-demos) (return-from mining))
            (loop for offset from 0 below (max 1 (length sampled)) by batch-size
                  while (< demo-count max-demos)
                  do (dolist (example (subseq sampled
                                              (min offset (length sampled))
                                              (min (+ offset batch-size) (length sampled))))
                       (when (>= demo-count max-demos) (return-from mining))
                       (unless (find example accepted :test #'%opt-same-p)
                         (let* ((eval-options
                                  (object "dataset" (%opt-dataset-object (list example))
                                          "phase" "bootstrap"
                                          "round" round-index))
                                (result (progn
                                          (when (%opt-object-p teacher-options)
                                            (%set-key eval-options "forward_options"
                                                      (%opt-merge teacher-options
                                                                  (jget options "forward_options"))))
                                          (evaluate-candidate evaluator (%opt-clone base-cfg)
                                                              eval-options)))
                                (rows (%opt-result-rows result)))
                           (incf total-calls (%opt-int (jget result "count")
                                                       (max 1 (length rows))))
                           (when rows
                             (let ((row (first rows)))
                               (when (>= (%opt-row-scalar row) threshold)
                                 (push example accepted)
                                 (push (%opt-demo row) demos)
                                 (incf demo-count)))))))
                  while (< demo-count max-demos))))))
    (object "artifactVersion" "axir-optimized-artifact-v1"
            "optimizerName" (optimizer-engine-name engine)
            "optimizerVersion" (optimizer-engine-version engine)
            "componentMap" (%new-object)
            "demos" (%opt-array (nreverse demos))
            "metadata" (object "optimizer" (optimizer-engine-name engine)
                               "qualityThreshold" threshold
                               "totalMetricCalls" total-calls
                               "demosGenerated" demo-count)
            "evidence" (object "count" total-calls)
            "provenance" (object "sourceProgramKind"
                                 (%opt-string (%opt-present (jget request "programKind")) "unknown")))))

;;; ------------------------------------------------------------------
;;; GEPA
;;; ------------------------------------------------------------------
;;;
;;; A reflective evolutionary search.  Each trial picks a component with a
;;; bandit, asks the reflection callback for a replacement value, measures
;;; parent and child on the same minibatch, and keeps the child only when it
;;; really scored better.  The archive is a Pareto front over the averaged
;;; score vectors; the artifact's component map is the best front member.

(defclass gepa (optimizer-engine)
  ((reflection :initarg :reflection :reader gepa-reflection)
   (options :initarg :options :reader gepa-options)
   (rng :initarg :rng :reader gepa-rng)
   (selector :initform nil :accessor gepa-selector))
  (:documentation
   "Seeded reflective Pareto search over a program's optimizable components."))

(defun make-gepa (&key reflection options seed)
  "A GEPA engine.

REFLECTION is the teacher: a function of one argument, the reflection
payload object (componentKey, componentKind, currentValue,
previousValidationError, minibatch, traceDataset), returning the proposed
replacement text.  MAKE-AI-REFLECTION-CALLBACK builds one from a provider
client; a test supplies a plain function and needs no provider at all.

OPTIONS is a JSON object layered under the request's options.  SEED, or
options.seed, fixes the search: the same seed and the same scores explore
the same candidates.  Recognized option keys, with defaults:

  maxMetricCalls          required, positive; the metric budget
  numTrials               30    reflective trials
  minibatch               true  evaluate trials on a rotating minibatch
  minibatchSize           20
  earlyStoppingTrials     5     consecutive rejections that stop the search
  minImprovementThreshold 0     improvement a child must beat to be accepted
  paretoSetSize           max(10, min(200, minibatchSize * 3))
  tieEpsilon              0     slack in the dominance test
  perfectScore            1     a minibatch at or above this is skipped
  skipPerfectScore        true
  maxReflectionAttempts   2     retries when a proposal fails validation
  bootstrap               off   {scoreThreshold, maxBootstrapDemos,
                                 maxBootstrapMetricCalls} to mine demos first
  paretoMetricKey         none  scalarize by one named score instead of the mean
  logger                  none  called with a Notification object on teacher failure"
  (let ((options (if (%opt-object-p options) (%opt-clone options) (%new-object))))
    (make-instance 'gepa
                   :reflection (%opt-function reflection "reflection callback")
                   :options options
                   :rng (make-optimizer-rng (if seed seed (%opt-present (jget options "seed")))))))

(defmethod optimizer-engine-name ((engine gepa)) "GEPA")
(defmethod optimizer-engine-version ((engine gepa)) "axir-gepa-v1")

(defclass gepa-component-selector ()
  ((state :initarg :state :reader gepa-selector-state-table)
   (components :initarg :components :reader gepa-selector-components))
  (:documentation
   "GEPA's component bandit: which component to try changing next.

Per component it keeps proposals, accepts, lastAcceptIter and stagnation.
A component that keeps failing is tried less; one that has not been
accepted for a while is revisited.  The state is plain JSON, so a run can
be resumed: pass a previous GEPA-SELECTOR-SNAPSHOT, or an artifact's
metadata.selectorState, back in as :STATE.

Picking consumes an OPTIMIZER-RNG, never CL:RANDOM, so a seed fixes the
whole search."))

(defun make-gepa-component-selector (components &key state)
  "A selector over COMPONENTS, resuming from STATE when one is given."
  (let ((table (%new-object)))
    (dolist (component components)
      (let* ((id (%opt-string (jget component "id") ""))
             (previous (and (%opt-object-p state) (jget state id))))
        (%set-key table id
                  (object "proposals" (max 0 (%opt-int (jget previous "proposals") 0))
                          "accepts" (max 0 (%opt-int (jget previous "accepts") 0))
                          "lastAcceptIter" (%opt-int (jget previous "lastAcceptIter") -1)
                          "stagnation" (max 0 (%opt-int (jget previous "stagnation") 0))))))
    (make-instance 'gepa-component-selector :state table :components components)))

(defun gepa-selector-snapshot (selector)
  "A copy of SELECTOR's state, ready to be stored and resumed from."
  (%opt-clone (gepa-selector-state-table selector)))

(defun gepa-selector-record-proposal (selector id)
  "Note that a proposal was made for component ID."
  (let ((entry (jget (gepa-selector-state-table selector) id)))
    (when (%opt-object-p entry)
      (%set-key entry "proposals" (1+ (%opt-int (jget entry "proposals") 0))))
    selector))

(defun gepa-selector-record-result (selector id accepted iteration)
  "Note whether component ID's proposal was ACCEPTED on ITERATION."
  (let ((entry (jget (gepa-selector-state-table selector) id)))
    (when (%opt-object-p entry)
      (if accepted
          (progn
            (%set-key entry "accepts" (1+ (%opt-int (jget entry "accepts") 0)))
            (%set-key entry "lastAcceptIter" iteration)
            (%set-key entry "stagnation" 0))
          (%set-key entry "stagnation" (1+ (%opt-int (jget entry "stagnation") 0)))))
    selector))

(defun gepa-selector-pick (selector iteration rng)
  "The component to try next: a 10% uniform explore, else a softmax draw.

The weight rewards a low accept rate, stagnation and staleness, and
penalizes a component that has already had most of the proposals."
  (let ((components (gepa-selector-components selector))
        (state (gepa-selector-state-table selector)))
    (cond
      ((= (length components) 1) (first components))
      ((< (optimizer-rng-next rng) 0.1d0)
       (nth (min (1- (length components))
                 (floor (* (optimizer-rng-next rng) (length components))))
            components))
      (t
       (let* ((total (max 1 (reduce #'+ (mapcar (lambda (id)
                                                  (%opt-int (jget (jget state id) "proposals") 0))
                                                (%opt-component-ids components)))))
              (weights (mapcar
                        (lambda (component)
                          (let* ((entry (jget state (%opt-string (jget component "id") "")))
                                 (proposals (%opt-int (jget entry "proposals") 0))
                                 (accepts (%opt-int (jget entry "accepts") 0))
                                 (last-accept (%opt-int (jget entry "lastAcceptIter") -1))
                                 (stagnation (%opt-int (jget entry "stagnation") 0))
                                 (accept-rate (if (zerop proposals) 0d0 (/ (float accepts 1d0) proposals)))
                                 (pressure (/ (float proposals 1d0) total))
                                 (stale (if (minusp last-accept)
                                            (min (1+ iteration) 10)
                                            (min (- iteration last-accept) 10))))
                            (+ (* 1.4d0 (- 1d0 accept-rate))
                               (* 0.8d0 stagnation)
                               (* 0.2d0 stale)
                               (* -0.7d0 pressure))))
                        components))
              (maximum (reduce #'max weights))
              (exponentials (mapcar (lambda (weight) (exp (- weight maximum))) weights))
              (threshold (* (optimizer-rng-next rng) (reduce #'+ exponentials))))
         (loop for component in components
               for weight in exponentials
               do (decf threshold weight)
                  (when (<= threshold 0) (return component))
               finally (return (car (last components)))))))))

(defun %gepa-component-group (component components)
  "COMPONENT and everything it depends on, depth first, each once."
  (let ((by-id (%new-object))
        (seen '())
        (out '()))
    (dolist (item components)
      (%set-key by-id (%opt-string (jget item "id") "") item))
    (labels ((visit (id)
               (when (and (stringp id)
                          (not (member id seen :test #'string=))
                          (%opt-object-p (jget by-id id)))
                 (push id seen)
                 (let ((item (jget by-id id)))
                   (push item out)
                   (dolist (dependency (%opt-list (%opt-option item '("dependsOn" "depends_on"))))
                     (visit dependency))))))
      (visit (%opt-string (jget component "id") "")))
    (nreverse out)))

(defun %gepa-next-minibatch (train iteration size)
  "A rotating window of SIZE tasks, so successive trials see different data."
  (cond ((null train) '())
        ((or (<= size 0) (>= size (length train))) (copy-list train))
        (t (let ((start (mod (* iteration size) (length train))))
             (loop for i from 0 below size
                   collect (nth (mod (+ start i) (length train)) train))))))

(defstruct (gepa-eval (:conc-name gepa-eval-))
  rows avg-scores avg sum count scalars candidate-map)

(defun %gepa-evaluate (evaluator cfg tasks phase budget capture-traces)
  "Evaluate CFG on TASKS inside BUDGET, a (USED . MAX) cons it updates.

Returns NIL when the remaining budget cannot cover the tasks, so a caller
stops instead of running a partial, incomparable measurement."
  (let ((needed (length tasks)))
    (when (> (+ (car budget) needed) (cdr budget))
      (return-from %gepa-evaluate nil))
    (let* ((result (evaluate-candidate evaluator (%opt-clone cfg)
                                       (object "dataset" (%opt-dataset-object tasks)
                                               "phase" phase
                                               "captureTraces" (json-boolean capture-traces))))
           (rows (%opt-result-rows result))
           (scalars (mapcar #'%opt-row-scalar rows))
           (count (%opt-int (jget result "count") (length rows))))
      (incf (car budget) count)
      (make-gepa-eval :rows rows
                      :avg-scores (%opt-avg-vector rows)
                      :avg (%opt-num (jget result "avg")
                                     (if scalars (/ (reduce #'+ scalars) (length scalars)) 0d0))
                      :sum (%opt-num (jget result "sum") (if scalars (reduce #'+ scalars) 0d0))
                      :count count
                      :scalars scalars
                      :candidate-map (%opt-clone cfg)))))

(defun %gepa-score-object (evaluation)
  (let ((vector (gepa-eval-avg-scores evaluation)))
    (if (plusp (hash-table-count vector))
        vector
        (object "score" (gepa-eval-avg evaluation)))))

(defun %opt-custom-labels (options)
  "The labels a metric carries: the shared globals, then this run's."
  (merge-custom-labels (get-global "customLabels") (jget options "customLabels")))

(defun %opt-optimizer-logger (options)
  "The optimizer logger to report through, or NIL when logging is off.

Resolution is the shared one: this run's logger, else the process-wide
`optimizerLogger` global, else Ax's default optimizer logger.  The optimizer
keeps no logger default of its own."
  (unless (%opt-flag options '("verbose") t)
    (return-from %opt-optimizer-logger nil))
  (or (%opt-function (%opt-present (%opt-option options '("logger" "optimizerLogger"))) "logger")
      (%opt-function (%opt-present (get-global "optimizerLogger")) "optimizerLogger global")
      *default-optimizer-logger*))

(defun %gepa-notify (options action condition)
  "Report a teacher failure the way every port does, then carry on."
  (let ((logger (%opt-optimizer-logger options)))
    (when logger
      (funcall logger (object "name" "Notification"
                              "id" "gepa_teacher"
                              "value" (format nil "GEPA teacher call failed while ~a: ~a"
                                              action condition))))))

(defun %gepa-reflect (engine component current minibatch trace-dataset options)
  "Ask the teacher for a replacement value for COMPONENT.

Keeps the current value when every attempt fails validation or the teacher
errors, so a broken teacher degrades the search instead of corrupting the
program.  A cancellation is re-signalled: it ends the run."
  (let ((reflection (gepa-reflection engine)))
    (unless reflection
      (optimize-fail :config "AxGEPA requires a reflection client for reflective trials"))
    (let ((attempts (max 1 (%opt-int (%opt-option options '("maxReflectionAttempts"
                                                            "max_reflection_attempts"))
                                     2)))
          (previous-error :null)
          (last-condition nil))
      (dotimes (attempt attempts)
        (setf last-condition nil)
        (let ((payload (object "componentKey" (jget component "id")
                               "componentKind" (jget component "kind")
                               "currentValue" current
                               "previousValidationError" previous-error
                               "minibatch" minibatch
                               "traceDataset" trace-dataset
                               "teacherOptions" (let ((teacher (%opt-option options '("teacherOptions"
                                                                                      "teacher_options"))))
                                                  (if (%opt-object-p teacher) teacher (%new-object)))
                               "model" (%opt-option options '("reflectionModel" "reflection_model")))))
          (let ((candidate
                  (handler-case (funcall reflection payload)
                    (optimize-error (condition)
                      (if (eq (optimize-error-kind condition) :cancelled)
                          (error condition)
                          (progn (setf last-condition condition) nil)))
                    (error (condition) (setf last-condition condition) nil))))
            (when candidate
              (let* ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                        (%opt-string candidate "")))
                     (validation (validate-component-value component text)))
                (if (eq validation t)
                    (return-from %gepa-reflect text)
                    (setf previous-error validation)))))))
      (when last-condition
        (%gepa-notify options
                      (format nil "proposing a new value for ~a; keeping the current value"
                              (%opt-string (jget component "id") ""))
                      last-condition))
      current)))

(defun %gepa-bootstrap (evaluator base-cfg train options budget)
  "Mine demos before the search, inside the same metric budget."
  (let ((raw (%opt-present (jget options "bootstrap"))))
    (if (or (null raw) (json-false-p raw))
        '()
        (let* ((opts (if (%opt-object-p raw) raw (%new-object)))
               (threshold (%opt-num (%opt-option opts '("scoreThreshold" "score_threshold")) 0.8d0))
               (max-demos (%opt-int (%opt-option opts '("maxBootstrapDemos" "max_bootstrap_demos")) 4 1))
               (default-calls (max 1 (min (length train) 8)))
               (max-calls (%opt-int (%opt-option opts '("maxBootstrapMetricCalls"
                                                        "max_bootstrap_metric_calls"))
                                    default-calls 1))
               (demos '())
               (calls 0))
          (dolist (example train (nreverse demos))
            (when (or (>= calls max-calls) (>= (length demos) max-demos))
              (return (nreverse demos)))
            (let ((result (%gepa-evaluate evaluator base-cfg (list example) "bootstrap" budget nil)))
              (incf calls)
              (when (and result (gepa-eval-rows result))
                (let ((row (first (gepa-eval-rows result))))
                  (when (>= (%opt-row-scalar row) threshold)
                    (push (%opt-demo row) demos))))))))))

(defmethod run-optimizer-engine ((engine gepa) request evaluator)
  (unless evaluator
    (optimize-fail :evaluator "AxGEPA requires an OptimizerEvaluator."))
  (let* ((options (%opt-request-options (gepa-options engine) request))
         (components (%opt-request-components request)))
    (unless components
      (optimize-fail :components "AxGEPA: program exposes no optimizable components"))
    (multiple-value-bind (train validation) (%opt-dataset-split request)
      (let* ((max-calls (%opt-int (%opt-option options '("maxMetricCalls" "max_metric_calls")) 0))
             (num-trials (%opt-int (%opt-option options '("numTrials" "num_trials")) 30 0))
             (minibatch-p (%opt-flag options '("minibatch") t))
             (minibatch-size (%opt-int (%opt-option options '("minibatchSize" "minibatch_size")) 20 1))
             (early-stop (%opt-int (%opt-option options '("earlyStoppingTrials" "early_stopping_trials")) 5 1))
             (min-improvement (%opt-num (%opt-option options '("minImprovementThreshold"
                                                               "min_improvement_threshold"))
                                        0d0))
             (default-pareto (max 10 (min 200 (* minibatch-size 3))))
             (pareto-size (%opt-int (%opt-option options '("paretoSetSize" "pareto_set_size"))
                                    default-pareto default-pareto 1000))
             (tie-epsilon (%opt-num (%opt-option options '("tieEpsilon" "tie_epsilon")) 0d0))
             (base-cfg (%opt-current-map components))
             (pareto-set (subseq validation 0 (min pareto-size (length validation))))
             (budget (cons 0 max-calls))
             (candidates (%new-array))
             (per-instance '())
             (stagnation 0))
        (when (<= max-calls 0)
          (optimize-fail :config "AxGEPA: options.maxMetricCalls must be set to a positive integer"))
        (setf (gepa-selector engine)
              (make-gepa-component-selector
               components
               :state (%opt-option options '("selectorState" "selector_state"))))
        (let* ((demos (%gepa-bootstrap evaluator base-cfg train options budget))
               (base-eval (%gepa-evaluate evaluator base-cfg pareto-set "initial Pareto evaluation"
                                          budget nil)))
          (unless base-eval
            ;; The exact text every port reports, asserted by
            ;; ir/conformance/axoptimize/gepa-max-metric-calls-error.json.
            (optimize-fail :budget
                           "AxGEPA: options.maxMetricCalls=~a is too small to evaluate the initial Pareto set; need at least ~a metric calls"
                           max-calls (length pareto-set)))
          (vector-push-extend (object "cfg" (%opt-clone base-cfg)
                                      "scores" (%gepa-score-object base-eval)
                                      "parent" :null)
                              candidates)
          (setf per-instance (list (gepa-eval-scalars base-eval)))
          (block trials
            (dotimes (iteration num-trials)
              (when (>= (car budget) (cdr budget)) (return-from trials))
              (block trial
              (let* ((parent-index
                       (let ((best 0) (best-mean nil))
                         (loop for scalars in per-instance
                               for index from 0
                               do (let ((mean (if scalars
                                                  (/ (reduce #'+ scalars) (length scalars))
                                                  0d0)))
                                    (when (or (null best-mean) (> mean best-mean))
                                      (setf best-mean mean best index))))
                         best))
                     (mini (if minibatch-p
                               (%gepa-next-minibatch train iteration minibatch-size)
                               train))
                     (parent (aref candidates parent-index))
                     (parent-eval (%gepa-evaluate evaluator (jget parent "cfg") mini
                                                  "parent minibatch" budget t)))
                (unless parent-eval (return-from trials))
                (let ((perfect (%opt-num (%opt-option options '("perfectScore" "perfect_score")) 1d0)))
                  (when (and (%opt-flag options '("skipPerfectScore" "skip_perfect_score") t)
                             (gepa-eval-scalars parent-eval)
                             (every (lambda (score) (>= score perfect)) (gepa-eval-scalars parent-eval)))
                    ;; Nothing left to improve on this minibatch: skip the
                    ;; trial without spending a reflection or a child run.
                    (return-from trial)))
                (let* ((target (gepa-selector-pick (gepa-selector engine) iteration (gepa-rng engine)))
                       (group (%gepa-component-group target components))
                       (proposed (%opt-clone (jget parent "cfg")))
                       (rows (gepa-eval-rows parent-eval))
                       (tuples (%opt-array
                                (mapcar (lambda (row)
                                          (object "input" (jget row "input")
                                                  "prediction" (jget row "prediction")
                                                  "score" (%opt-row-scalar row)))
                                        rows)))
                       (trace-dataset (%opt-array
                                       (mapcar (lambda (row)
                                                 (object "score" (%opt-row-scalar row)
                                                         "trace" (jget row "trace")
                                                         "output" (jget row "prediction")))
                                               rows))))
                  (dolist (component group)
                    (let ((id (%opt-string (jget component "id") "")))
                      (gepa-selector-record-proposal (gepa-selector engine) id)
                      (%set-key proposed id
                                (%gepa-reflect engine component (%opt-string (jget proposed id) "")
                                               tuples trace-dataset options))))
                  (let ((child-mini (%gepa-evaluate evaluator proposed mini "child minibatch" budget nil)))
                    (unless child-mini (return-from trials))
                    (let ((accepted (> (gepa-eval-sum child-mini)
                                       (+ (gepa-eval-sum parent-eval) min-improvement))))
                      (dolist (component group)
                        (gepa-selector-record-result (gepa-selector engine)
                                                     (%opt-string (jget component "id") "")
                                                     accepted iteration))
                      (if (not accepted)
                          (progn
                            (incf stagnation)
                            (when (>= stagnation early-stop) (return-from trials)))
                          (let ((child-eval (%gepa-evaluate evaluator proposed pareto-set
                                                            "validation evaluation" budget nil)))
                            (unless child-eval (return-from trials))
                            (vector-push-extend (object "cfg" (%opt-clone proposed)
                                                        "scores" (%gepa-score-object child-eval)
                                                        "parent" parent-index)
                                                candidates)
                            (setf per-instance (append per-instance (list (gepa-eval-scalars child-eval))))
                            (setf stagnation 0))))))))))
          (%gepa-artifact engine request options components candidates demos tie-epsilon
                          pareto-set (car budget)))))))

(defun %gepa-artifact (engine request options components candidates demos tie-epsilon
                       pareto-set total-calls)
  (let* ((front (%opt-pareto-front candidates tie-epsilon))
         (best-index (if front (first (first front)) 0))
         (best-score nil))
    (dolist (item front)
      (let ((score (%opt-scalarize (second item) options)))
        (when (or (null best-score)
                  (> score best-score)
                  (and (= score best-score) (> (first item) best-index)))
          (setf best-score score
                best-index (first item)))))
    (let* ((best-cfg (%opt-clone (jget (aref candidates best-index) "cfg")))
           (owners (%new-object))
           (pareto-meta (%opt-array
                         (mapcar (lambda (item)
                                   (object "candidate" (first item)
                                           "scores" (second item)
                                           "dominatedSolutions" (third item)
                                           "componentMap" (%opt-clone
                                                           (jget (aref candidates (first item)) "cfg"))))
                                 front)))
           (hypervolume (%opt-hypervolume-2d (mapcar #'second front)))
           (reported-best (if best-score best-score 0d0)))
      (dolist (component components)
        (let ((id (%opt-string (jget component "id") "")))
          (%set-key owners id
                    (%opt-string (%opt-present (jget component "owner"))
                                 (let ((separator (search "::" id)))
                                   (if separator (subseq id 0 separator) id))))))
      ;; The frontier this run found, reported through the shared optimizer
      ;; instruments rather than a counter of the optimizer's own.  Recording
      ;; is fail-open and a no-op when no meter is configured.
      (record-pareto-metric (get-or-create-optimizer-metrics-instruments)
                            (length front)
                            (length candidates)
                            (optimizer-engine-name engine)
                            :hypervolume (if (eq hypervolume :null) :null hypervolume)
                            :custom-labels (%opt-custom-labels options))
      (object "artifactVersion" "axir-optimized-artifact-v1"
              "optimizerName" (optimizer-engine-name engine)
              "optimizerVersion" (optimizer-engine-version engine)
              "componentMap" best-cfg
              "demos" (%opt-array demos)
              "metadata" (object "optimizer" (optimizer-engine-name engine)
                                 "selectorState" (gepa-selector-snapshot (gepa-selector engine))
                                 "paretoFront" pareto-meta
                                 "bestScore" reported-best
                                 "totalMetricCalls" total-calls
                                 "candidatesExplored" (length candidates)
                                 "report"
                                 (object "summary" "GEPA Multi-Objective Optimization Complete"
                                         "statistics" (object "totalEvaluations" total-calls
                                                              "candidatesExplored" (length candidates)
                                                              "converged" true)
                                         "paretoFrontier"
                                         (object "solutionCount" (length front)
                                                 "hypervolume" (if (eq hypervolume :null)
                                                                   0d0
                                                                   hypervolume))))
              "evidence" (object "avg" reported-best
                                 "count" (length pareto-set)
                                 "totalMetricCalls" total-calls)
              "provenance" (object "sourceProgramKind"
                                   (%opt-string (%opt-present (jget request "programKind")) "unknown")
                                   "componentOwners" owners)))))

;;; ------------------------------------------------------------------
;;; A reflection callback over a real provider client
;;; ------------------------------------------------------------------

(defun %gepa-extract-text (content)
  "The proposed value inside a teacher reply.

A reply may answer bare, prefixed with \"New Value:\", or inside a fenced
block whose first line is a language tag.  Everything else is used as is."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (%opt-string content ""))))
    (cond
      ((and (>= (length text) 10) (string= "New Value:" text :end2 10))
       (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq text 10)))
      (t
       (let* ((fence (format nil "~a~a~a" #\` #\` #\`))
              (start (search fence text))
              (end (search fence text :from-end t)))
         (if (and start end (> end start))
             (let ((inner (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (subseq text (+ start 3) end))))
               (let ((newline (position #\Newline inner)))
                 (if (and newline
                          (plusp newline)
                          (every (lambda (character)
                                   (or (alphanumericp character) (char= character #\_)))
                                 (string-trim '(#\Space #\Tab #\Return) (subseq inner 0 newline))))
                     (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq inner (1+ newline)))
                     inner)))
             text))))))

(defun make-ai-reflection-callback (client)
  "A GEPA reflection callback backed by CLIENT, a provider client.

The payload is sent as one JSON user message and the reply is read with the
same rules every port uses.  This issues real provider calls and so needs a
credentialled client; an engine test should pass a plain function instead.

options.reflectionModel reaches the provider as the request's model, so a
run that asks for a different reflection model gets it rather than being
answered by the client's default.  Without one the client's own model is
used."
  ;; A client is anything that can say which model it speaks for, which is
  ;; what CHAT needs of it.  Naming a concrete class here would refuse a
  ;; session boundary or any other service that wraps one, and the point of
  ;; the guard is only to catch the easy mistake this function's own
  ;; docstring warns about: handing it the plain callback an engine test
  ;; would pass, which would otherwise fail somewhere inside the first
  ;; reflection instead of here.
  (when (or (functionp client)
            (null (compute-applicable-methods #'ai-model (list client))))
    (optimize-fail :config
                   "make-ai-reflection-callback needs a provider client, not ~s; pass a plain function to an engine directly if you want a scripted reflection."
                   (type-of client)))
  (lambda (payload)
    (let* ((requested (%opt-present (jget payload "model")))
           (response (if (and (stringp requested) (plusp (length requested)))
                         (chat client (%opt-array (list (message "user" (encode-json payload))))
                               :model requested)
                         (chat client (%opt-array (list (message "user" (encode-json payload))))))))
      (%gepa-extract-text (%opt-present (jget response "content"))))))

;;; ------------------------------------------------------------------
;;; ACE: Generator -> Reflector -> Curator
;;; ------------------------------------------------------------------
;;;
;;; Every playbook mutation is a Core op, so the playbook this driver builds
;;; is byte-identical to the other ports'.  What lives here is the round
;;; structure: when to run the generator, how many reflection rounds to
;;; take, when a reflection is resolved, and what the feedback and delta
;;; histories record.

(defparameter +ace-default-config+
  '(("maxEpochs" . 1)
    ("maxReflectorRounds" . 2)
    ("maxSectionSize" . 25)
    ("maxSerializedFieldChars" . 2000)
    ("similarityThreshold" . 0.95d0)
    ("allowDynamicSections" . :true))
  "ACE's defaults, shared with every other port.")

(defclass ace ()
  ((reflector :initarg :reflector :reader ace-reflector)
   (curator :initarg :curator :reader ace-curator)
   (generator :initarg :generator :reader ace-generator)
   (metric :initarg :metric :accessor ace-metric)
   (options :initarg :options :reader ace-options)
   (config :initarg :config :accessor ace-config)
   (initial-playbook :initarg :initial-playbook :accessor ace-initial-playbook)
   (playbook :initform :null :accessor ace-playbook-slot)
   (base-instruction :initform :null :accessor ace-base-instruction-slot)
   (feedback :initform nil :accessor ace-feedback)
   (deltas :initform nil :accessor ace-deltas)
   (last-prediction :initform :null :accessor ace-last-prediction)
   ;; How the evolve roles were built, so a per-call option can rebuild
   ;; them: a run that names another teacher must really use it, not score
   ;; the client the playbook happened to be constructed with.
   (program :initform nil :accessor ace-program)
   (signature :initform nil :accessor ace-signature)
   (student :initform nil :accessor ace-student)
   (teacher :initform nil :accessor ace-teacher)
   (forward-options :initform nil :accessor ace-forward-options))
  (:documentation
   "The ACE context-engineering driver: Generator, then Reflector, then Curator.

The three roles are injected functions, so a run is reproducible without a
provider.  The reflector receives {question, generator_answer,
generator_reasoning, playbook, feedback, previous_reflection} and returns a
reflection object; the curator receives {playbook, reflection,
question_context, token_budget} and returns {operations}; the generator
receives one example and returns a prediction."))

(defun %ace-wall-clock ()
  "The current UTC time, as JavaScript's toISOString writes it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~3,'0dZ"
            year month day hour minute second
            (mod (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second) 1000))))

(defun make-ace (&key reflector curator generator metric options)
  "An ACE driver.

REFLECTOR, CURATOR and GENERATOR are functions of one JSON argument; METRIC
is called as (METRIC PREDICTION EXAMPLE) and returns a number.  OPTIONS is a
JSON object; it may carry maxEpochs, maxReflectorRounds, maxSectionSize,
maxSerializedFieldChars, similarityThreshold, allowDynamicSections, an
initialPlaybook, and \"now\" to pin the timestamp every change is stamped
with."
  (let* ((options (if (%opt-object-p options) (%opt-clone options) (%new-object)))
         (config (%new-object)))
    (loop for (key . value) in +ace-default-config+
          do (%set-key config key (if (eq value :true) true value)))
    (dolist (entry +ace-default-config+)
      (let ((given (%opt-present (jget options (car entry)))))
        (when given (%set-key config (car entry) given))))
    (let ((instance (make-instance 'ace
                                   :reflector (%opt-function reflector "ACE reflector")
                                   :curator (%opt-function curator "ACE curator")
                                   :generator (%opt-function generator "ACE generator")
                                   :metric (%opt-function metric "ACE metric")
                                   :options options
                                   :config config
                                   :initial-playbook
                                   (%opt-present (%opt-option options
                                                              '("initialPlaybook" "initial_playbook"))))))
      (ace-reset instance)
      instance)))

(defun %ace-now (ace)
  (let ((given (%opt-present (jget (ace-options ace) "now"))))
    (if (stringp given) given (%ace-wall-clock))))

(defun ace-reset (ace)
  "Clear ACE's playbook and histories back to its initial playbook."
  (setf (ace-playbook-slot ace)
        (if (ace-initial-playbook ace)
            (%opt-clone (ace-initial-playbook ace))
            (axllm/core::ace-empty-playbook :null (%ace-now ace)))
        (ace-base-instruction-slot ace) :null
        (ace-feedback ace) nil
        (ace-deltas ace) nil)
  ace)

(defun ace-playbook (ace)
  "A copy of ACE's current playbook."
  (%opt-clone (ace-playbook-slot ace)))

(defun ace-base-instruction (ace)
  "The program description ACE started from, or :NULL."
  (ace-base-instruction-slot ace))

(defun ace-render (ace)
  "ACE's playbook rendered as the markdown its teachers read."
  (axllm/core::ace-render-playbook (%opt-clone (ace-playbook-slot ace))))

(defun ace-artifact (ace)
  "ACE's artifact: the playbook, the feedback events, and the applied deltas."
  (object "playbook" (%opt-clone (ace-playbook-slot ace))
          "feedback" (%opt-array (mapcar #'%opt-clone (reverse (ace-feedback ace))))
          "history" (%opt-array (mapcar #'%opt-clone (reverse (ace-deltas ace))))))

(defun %ace-generator-output (prediction)
  (object "reasoning" (let ((thought (%opt-present (jget prediction "thought"))))
                        (if thought (%opt-string thought (princ-to-string thought)) ""))
          "answer" prediction
          "bulletIds" (let ((ids (jget prediction "bullet_ids")))
                        (if (%opt-array-p ids) (%opt-clone ids) (%new-array)))))

(defun %ace-run-reflector (ace example generator-output feedback previous)
  (let ((reflector (ace-reflector ace)))
    (when reflector
      (funcall reflector
               (object "question" example
                       "generator_answer" (jget generator-output "answer")
                       "generator_reasoning" (jget generator-output "reasoning")
                       "playbook" (ace-render ace)
                       "feedback" (if feedback feedback :null)
                       "previous_reflection" (if previous previous :null))))))

(defun %ace-reflection-resolved-p (reflection)
  "Whether a reflection says there is nothing left to fix."
  (let* ((text (string-downcase
                (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (%opt-string (%opt-present (jget reflection "errorIdentification")) ""))))
         (metadata (jget reflection "metadata"))
         (resolved (and (%opt-object-p metadata) (jget metadata "resolved"))))
    (or (and resolved (json-true-p resolved))
        (string= text "")
        (and (>= (length text) 8) (string= "no error" text :end2 8))
        (and (>= (length text) 8) (string= "resolved" text :end2 8)))))

(defun %ace-reflection-rounds (ace example generator-output feedback)
  (let ((rounds (max 1 (%opt-int (jget (ace-config ace) "maxReflectorRounds") 1)))
        (previous nil))
    (dotimes (round rounds previous)
      (let ((reflection (%ace-run-reflector ace example generator-output feedback previous)))
        (when (or (null reflection) (eq reflection :null))
          (return previous))
        (let ((copy (%opt-merge reflection)))
          (%set-key copy "bulletTags" (axllm/core::ace-normalize-reflection-bullet-tags reflection))
          (setf previous copy)
          (when (%ace-reflection-resolved-p copy)
            (return previous)))))))

(defun %ace-run-curator (ace example reflection)
  (let ((curator (ace-curator ace)))
    (when (and curator reflection)
      (funcall curator (object "playbook" (ace-render ace)
                               "reflection" reflection
                               "question_context" example
                               "token_budget" 1024)))))

(defun %ace-protected-ids (operations)
  (let ((out (%new-array)))
    (dolist (operation (%opt-list operations) out)
      (when (and (equal (%opt-present (jget operation "type")) "UPDATE")
                 (%opt-present (jget operation "bulletId")))
        (vector-push-extend (jget operation "bulletId") out)))))

(defun %ace-apply-operations (ace resolved curator-result)
  "Apply RESOLVED to ACE's playbook; returns the applied bullet ids."
  (let* ((protected (%ace-protected-ids resolved))
         (result (axllm/core::ace-apply-curator-operations
                  (ace-playbook-slot ace)
                  resolved
                  (object "maxSectionSize" (jget (ace-config ace) "maxSectionSize")
                          "allowDynamicSections" (jget (ace-config ace) "allowDynamicSections")
                          "enableAutoPrune" true
                          "protectedBulletIds" protected)
                  (%ace-now ace)))
         (applied (jget result "updatedBulletIds"))
         (auto-removed (%opt-list (jget result "autoRemoved"))))
    (setf (ace-playbook-slot ace) (jget result "playbook"))
    (when auto-removed
      (let ((combined (%opt-array (append (%opt-list resolved) auto-removed))))
        (when (%opt-object-p curator-result)
          (%set-key curator-result "operations" combined))))
    (if (%opt-array-p applied) applied (%new-array))))

(defun %ace-tag-bullets (ace reflection)
  (when (%opt-object-p reflection)
    (dolist (tag (%opt-list (axllm/core::ace-normalize-reflection-bullet-tags reflection)))
      (setf (ace-playbook-slot ace)
            (axllm/core::ace-update-bullet-feedback
             (ace-playbook-slot ace)
             (%opt-string (%opt-present (jget tag "id")) "")
             (%opt-string (%opt-present (jget tag "tag")) "")
             (%ace-now ace))))))

(defun %ace-process (ace example prediction score feedback source epoch index)
  "One Reflector/Curator round over one example.  Returns the curator result."
  (let* ((generator-output (%ace-generator-output prediction))
         (reflection (%ace-reflection-rounds ace example generator-output feedback))
         (raw-curator (%ace-run-curator ace example reflection))
         (operations (axllm/core::ace-normalize-curator-operations
                      (if (%opt-object-p raw-curator) (jget raw-curator "operations") :null)))
         (resolved (axllm/core::ace-resolve-curator-operation-targets
                    operations (ace-playbook-slot ace)
                    (or reflection :null) generator-output))
         (curator-result (when (or (%opt-object-p raw-curator) (plusp (%opt-count resolved)))
                           (let ((copy (%opt-merge (if (%opt-object-p raw-curator)
                                                       raw-curator
                                                       (%new-object)))))
                             (%set-key copy "operations" resolved)
                             copy)))
         (applied (%new-array)))
    ;; compile tags bullets after applying operations; the online path tags
    ;; first.  Both orders are the reference's, and both are kept.
    (if (eq source :online)
        (progn
          (%ace-tag-bullets ace reflection)
          (when (plusp (%opt-count resolved))
            (setf applied (%ace-apply-operations ace resolved curator-result))
            (setf (ace-playbook-slot ace) (axllm/core::ace-dedupe-playbook (ace-playbook-slot ace)))))
        (progn
          (when (plusp (%opt-count resolved))
            (setf applied (%ace-apply-operations ace resolved curator-result)))
          (%ace-tag-bullets ace reflection)
          (when (and (plusp (%opt-count resolved)) (plusp (%opt-count applied)))
            (setf (ace-playbook-slot ace) (axllm/core::ace-dedupe-playbook (ace-playbook-slot ace))))))
    (push (object "example" example
                  "prediction" prediction
                  "score" (if (%opt-finite-p score) score 0)
                  "generatorOutput" generator-output
                  "reflection" (or reflection :null)
                  "curator" (or curator-result :null)
                  "timestamp" (%ace-now ace))
          (ace-feedback ace))
    (when (and (plusp (%opt-count applied))
               curator-result
               (plusp (%opt-count (jget curator-result "operations"))))
      (push (object "source" (if (eq source :online) "online" "compile")
                    "epoch" epoch
                    "exampleIndex" index
                    "operations" (jget curator-result "operations")
                    "updatedBulletIds" (%opt-clone applied))
            (ace-deltas ace)))
    curator-result))

(defun %ace-metric-feedback (score)
  (if (%opt-finite-p score)
      (concatenate 'string "Metric score: " (axllm/core::core-js-number-text score))
      :null))

(defun ace-compile (ace examples &key metric)
  "Run ACE over EXAMPLES and return its result.

The result carries the final playbook, the artifact, the best metric score
observed, and the configuration the run used.  METRIC overrides the metric
given at construction."
  (let ((metric (or (%opt-function metric "ACE metric") (ace-metric ace)))
        (examples (%opt-list examples))
        (best :null))
    (ace-reset ace)
    (let ((epochs (max 1 (%opt-int (jget (ace-config ace) "maxEpochs") 1))))
      (dotimes (epoch epochs)
        (loop for example in examples
              for index from 0
              do (let* ((prediction (if (ace-generator ace)
                                        (funcall (ace-generator ace) example)
                                        (%new-object)))
                        (score (if metric (funcall metric prediction example) 0)))
                   (setf (ace-last-prediction ace) prediction)
                   (when (%opt-finite-p score)
                     (setf best (if (%opt-finite-p best) (max best score) score)))
                   (%ace-process ace example prediction score (%ace-metric-feedback score)
                                 :compile epoch index))))
      (object "playbook" (%opt-clone (ace-playbook-slot ace))
              "artifact" (ace-artifact ace)
              "bestScore" (if (%opt-finite-p best) best 0)
              "finalConfiguration" (object "strategy" "ace" "epochs" epochs)))))

(defun ace-apply-online-update (ace arguments)
  "Fold one live example and prediction into ACE's playbook.

ARGUMENTS carries example, prediction and optional feedback.  Returns the
curator result, or :NULL when nothing changed."
  (let* ((arguments (if (%opt-object-p arguments) arguments (%new-object)))
         (example (jget arguments "example"))
         (prediction (jget arguments "prediction")))
    (setf (ace-last-prediction ace) prediction)
    (or (%ace-process ace example prediction 0 (%opt-present (jget arguments "feedback"))
                      :online -1 (length (ace-feedback ace)))
        :null)))

;;; ------------------------------------------------------------------
;;; Driver: request, run, normalize, apply
;;; ------------------------------------------------------------------

(defparameter +optimize-default-max-metric-calls+ 100
  "The metric budget the optimize helper uses when the caller names none.")

(defparameter +optimize-bootstrap-example-limit+ 8
  "Training sets at or below this size are bootstrapped by default.")


(defun artifact-text (artifact)
  "ARTIFACT serialized to the portable JSON every Ax port reads."
  (axllm/core::serialize-optimized-artifact artifact))

(defun parse-artifact (text components)
  "TEXT read back as an artifact, validated against COMPONENTS."
  (axllm/core::deserialize-optimized-artifact text (%opt-array (%opt-list components))))

(defun apply-optimization (program artifact)
  "Validate ARTIFACT against PROGRAM's components and apply it.

ARTIFACT is an artifact object or its serialized text.  Validation is
Core's: the version, the optimizer name and version, the component map's
keys and value shapes, and the provenance owners all have to match, so an
artifact built for another program is rejected rather than half-applied.
Returns the validated artifact."
  (let* ((components (program-optimizable-components program))
         (validated (if (stringp artifact)
                        (axllm/core::deserialize-optimized-artifact artifact components)
                        (axllm/core::validate-optimized-artifact
                         (if (%opt-object-p artifact) artifact (%new-object))
                         components)))
         (demos (jget validated "demos")))
    (when (and (%opt-array-p demos) (plusp (length demos)))
      (program-set-demos program demos))
    (program-apply-optimized-components program
                                        (let ((map (jget validated "componentMap")))
                                          (if (%opt-object-p map) map (%new-object))))
    validated))

(defun optimize-program (program dataset &key engine client options evaluator)
  "Optimize PROGRAM over DATASET with ENGINE and return the artifact.

CLIENT is the provider client rollouts run against; without one, and
without an explicit EVALUATOR, the request tells the engine no evaluator is
available and an engine that needs measured scores refuses.  OPTIONS is a
JSON object; it reaches the engine as request.options, minus the keys that
name local objects (client, ai, engine, optimizer), and may carry

  target          \"all\", a component id, a list of ids, or \"actor\" /
                  \"responder\" / \"flow\"
  apply           AX:FALSE to return the artifact without applying it
  maxMetricCalls  also used as the evaluator's hard rollout ceiling
  metric          (METRIC TASK PREDICTION) -> number or score object
  rollout         (ROLLOUT PROGRAM CLIENT INPUTS OPTIONS) -> output, usage
  cancel          a function of no arguments; a true result stops the run"
  (let* ((options (if (%opt-object-p options) options (%new-object)))
         (helper (null engine))
         (helper-demos '())
         (components (program-optimizable-components program))
         (evaluator (or evaluator
                        (when client
                          (make-program-evaluator
                           program client
                           :dataset dataset
                           :options options
                           :rollout (%opt-present (jget options "rollout"))
                           :metric (%opt-present (jget options "metric"))
                           :max-metric-calls (let ((limit (%opt-option options '("maxMetricCalls"
                                                                                 "max_metric_calls"))))
                                               (when (%opt-finite-p limit) (%opt-int limit 0 0)))
                           :cancel (%opt-present (jget options "cancel"))))))
         (engine (or engine
                     ;; No engine named: this is the optimize helper.  Mine
                     ;; demos first when the training set is small enough,
                     ;; then search with GEPA, exactly as the reference does.
                     (progn
                       (unless (%opt-finite-p (%opt-option options '("maxMetricCalls"
                                                                     "max_metric_calls")))
                         (%set-key options "maxMetricCalls" +optimize-default-max-metric-calls+))
                       (setf helper-demos
                             (%opt-helper-bootstrap program evaluator options dataset))
                       ;; The helper has already mined its demos, so the
                       ;; search must not bootstrap again.  This is set on
                       ;; the options the request carries, not only on the
                       ;; engine's own, because a request's options win.
                       (%set-key options "bootstrap" false)
                       (make-gepa :reflection (%opt-present (%opt-option options '("reflection")))
                                  :options options
                                  :seed (%opt-present (jget options "seed"))))))
         (run (axllm/core::prepare-optimizer-run
               (program-kind program)
               components
               (if (eq dataset :null) (%new-array) dataset)
               options
               (program-optimizer-trace program)
               (json-boolean evaluator)))
         (request (jget run "request"))
         (started (get-internal-real-time))
         (response (run-optimizer-engine engine request evaluator))
         (artifact (axllm/core::normalize-optimizer-engine-response
                    response
                    (optimizer-engine-name engine)
                    (optimizer-engine-version engine)
                    components)))
    ;; What the run spent, reported before the outcome so a failed apply
    ;; still leaves the cost on record.
    (let ((tracker (%opt-present (%opt-option options '("costTracker" "cost_tracker")))))
      (when (typep tracker 'cost-tracker)
        (record-optimizer-resource-usage tracker
                                         :optimizer-type (optimizer-engine-name engine)
                                         :custom-labels (%opt-custom-labels options))))
    ;; One completed optimization, through the shared optimizer instruments.
    (record-optimization-metric (get-or-create-optimizer-metrics-instruments)
                                (round (* 1000 (- (get-internal-real-time) started))
                                       internal-time-units-per-second)
                                t
                                (optimizer-engine-name engine)
                                :program-signature (program-kind program)
                                :custom-labels (%opt-custom-labels options))
    (when (%opt-flag options '("apply") t)
      (apply-optimization program artifact))
    ;; The helper's own mined demos are attached after the artifact is
    ;; applied, and installed only on a program that can hold them.
    ;; metadata.demosInstalled records which happened, so a mined demo is
    ;; reported either way instead of being quietly lost.
    (when helper
      (let ((installed (and helper-demos
                            (%opt-accepts-demos-p program)
                            (%opt-flag options '("apply") t))))
        (when installed (program-set-demos program (%opt-array helper-demos)))
        (%set-key artifact "demos" (%opt-array helper-demos))
        (let ((metadata (jget artifact "metadata")))
          (when (%opt-object-p metadata)
            (%set-key metadata "demosInstalled" (json-boolean installed))))))
    artifact))

(defun %opt-accepts-demos-p (program)
  "Whether PROGRAM has a real PROGRAM-SET-DEMOS, not the refusing default."
  (let ((methods (compute-applicable-methods #'program-set-demos (list program nil))))
    (and methods
         (notevery (lambda (method)
                     (equal (mapcar (lambda (specializer)
                                      (and (typep specializer 'class) (class-name specializer)))
                                    (sb-mop:method-specializers method))
                            '(t t)))
                   methods))))

(defun %opt-helper-bootstrap (program evaluator options dataset)
  "The demos the optimize helper mines before it searches.

Bootstrapping is on when options.bootstrap says so, and otherwise when the
training set is small; options.bootstrap may also be the BootstrapFewShot
options object."
  (let* ((raw (%opt-option options '("bootstrap")))
         (normalized (axllm/core::normalize-optimization-dataset
                      (if (eq dataset :null) (%new-array) dataset)))
         (train (%opt-list (jget normalized "train")))
         (wanted (cond ((json-false-p raw) nil)
                       ((%opt-object-p raw) t)
                       ((json-true-p raw) t)
                       ((eq raw :null) (<= (length train) +optimize-bootstrap-example-limit+))
                       (t nil))))
    (when (and wanted evaluator)
      (let* ((engine (make-bootstrap-few-shot
                      (%opt-merge options (if (%opt-object-p raw) raw (%new-object)))))
             (request (object "programKind" (program-kind program)
                              "components" (program-optimizable-components program)
                              "dataset" normalized
                              "options" (%opt-merge options
                                                    (if (%opt-object-p raw) raw (%new-object)))))
             (artifact (run-optimizer-engine engine request evaluator)))
        (%opt-list (jget artifact "demos"))))))

;;; ------------------------------------------------------------------
;;; Cost tracking
;;; ------------------------------------------------------------------
;;;
;;; What stops a run before it spends more than the caller agreed to.  Cost
;;; is derived from the tokens tracked, never stored: there is one number to
;;; keep right, and changing a model's price re-prices the run.

(defclass cost-tracker ()
  ((usage :initform (%new-object) :reader cost-tracker-usage)
   (total :initform 0 :accessor cost-tracker-total)
   (cost-per-model :initarg :cost-per-model :reader cost-tracker-cost-per-model)
   (max-cost :initarg :max-cost :reader cost-tracker-max-cost)
   (max-tokens :initarg :max-tokens :reader cost-tracker-max-tokens))
  (:documentation
   "Tokens spent per model, and the limits that stop a run.

Price is per 1000 tokens.  A model with no entry in costPerModel is priced
at the shared 0.001 fallback rather than free, so an unpriced model cannot
make a run look costless."))

(defun make-cost-tracker (&key cost-per-model max-cost max-tokens)
  "A cost tracker.  COST-PER-MODEL is a JSON object of model name to price
per 1000 tokens.  MAX-COST and MAX-TOKENS are the limits; either may be
omitted, and a tracker with neither only reports."
  (make-instance 'cost-tracker
                 :cost-per-model (if (%opt-object-p cost-per-model)
                                     (%opt-clone cost-per-model)
                                     (%new-object))
                 :max-cost (when (%opt-finite-p max-cost) (%opt-num max-cost 0d0))
                 :max-tokens (when (%opt-finite-p max-tokens) (%opt-int max-tokens 0 0))))

(defun track-tokens (tracker count model)
  "Add COUNT tokens spent on MODEL."
  (let ((count (%opt-int count 0))
        (model (%opt-string model "")))
    (%set-key (cost-tracker-usage tracker) model
              (+ (%opt-int (jget (cost-tracker-usage tracker) model) 0) count))
    (incf (cost-tracker-total tracker) count)
    tracker))

(defun cost-tracker-cost (tracker)
  "The run's cost so far, derived from the tokens tracked."
  (let ((total 0d0))
    (dolist (model (%object-keys (cost-tracker-usage tracker)) total)
      (let ((tokens (%opt-num (jget (cost-tracker-usage tracker) model) 0d0))
            (price (let ((given (jget (cost-tracker-cost-per-model tracker) model)))
                     (if (%opt-finite-p given) (%opt-num given 0d0) 0.001d0))))
        (incf total (* (/ tokens 1000d0) price))))))

(defun cost-tracker-token-usage (tracker)
  "A copy of the per-model token counts."
  (%opt-clone (cost-tracker-usage tracker)))

(defun cost-tracker-total-tokens (tracker)
  (cost-tracker-total tracker))

(defun cost-tracker-limit-reached-p (tracker)
  "Whether TRACKER has reached either configured limit."
  (or (and (cost-tracker-max-tokens tracker)
           (>= (cost-tracker-total tracker) (cost-tracker-max-tokens tracker)))
      (and (cost-tracker-max-cost tracker)
           (>= (cost-tracker-cost tracker) (cost-tracker-max-cost tracker)))))

(defun record-optimizer-resource-usage (tracker &key (optimizer-type "unknown") custom-labels
                                                     memory-usage)
  "Report TRACKER's tokens and derived cost through the shared instruments.

Cost is recomputed from the tokens at the moment of the call, so the figure
reported is the one the limits are checked against, not a stale copy."
  (record-resource-usage-metric (get-or-create-optimizer-metrics-instruments)
                                (cost-tracker-total-tokens tracker)
                                (cost-tracker-cost tracker)
                                (%opt-string optimizer-type "unknown")
                                :memory-usage (if (%opt-finite-p memory-usage) memory-usage :null)
                                :custom-labels custom-labels)
  tracker)

(defun reset-cost-tracker (tracker)
  "Forget every token tracked so far, keeping the limits."
  (let ((usage (cost-tracker-usage tracker)))
    (dolist (model (%object-keys usage))
      (remhash model usage)))
  (setf (cost-tracker-total tracker) 0)
  tracker)

;;; ------------------------------------------------------------------
;;; Optimizer run state, statistics and checkpoints
;;; ------------------------------------------------------------------

(defclass optimizer-state ()
  ((stats :accessor optimizer-state-stats)
   (score-history :initform '() :accessor optimizer-state-score-history)
   (configuration-history :initform '() :accessor optimizer-state-configuration-history)
   (current-round :initform 0 :accessor optimizer-current-round)
   (cost-tracker :initarg :cost-tracker :reader optimizer-state-cost-tracker)
   (started-at :initform (get-internal-real-time) :accessor optimizer-state-started-at))
  (:documentation
   "The bookkeeping a long optimizer run carries: statistics, the score and
configuration history, the round counter and the cost tracker.

Shared by every engine, so a checkpoint written by one reads in another."))

(defun %optimizer-initial-stats ()
  (object "totalCalls" 0
          "successfulDemos" 0
          "estimatedTokenUsage" 0
          "earlyStopped" false
          "resourceUsage" (object "totalTokens" 0
                                  "totalTime" 0
                                  "avgLatencyPerEval" 0
                                  "costByModel" (%new-object))
          "convergenceInfo" (object "converged" false
                                    "finalImprovement" 0
                                    "stagnationRounds" 0
                                    "convergenceThreshold" 0.01d0)
          "bestScore" 0
          "bestConfiguration" (%new-object)))

(defun make-optimizer-state (&key cost-tracker)
  "Fresh run state, optionally sharing COST-TRACKER."
  (let ((state (make-instance 'optimizer-state :cost-tracker cost-tracker)))
    (setf (optimizer-state-stats state) (%optimizer-initial-stats))
    state))

(defun optimizer-stats (state)
  "A copy of STATE's statistics, with live resource usage folded in."
  (let* ((stats (%opt-clone (optimizer-state-stats state)))
         (resource (jget stats "resourceUsage"))
         (tracker (optimizer-state-cost-tracker state))
         (elapsed (round (* 1000 (- (get-internal-real-time) (optimizer-state-started-at state)))
                         internal-time-units-per-second))
         (calls (%opt-int (jget stats "totalCalls") 0)))
    (%set-key resource "totalTime" elapsed)
    (%set-key resource "avgLatencyPerEval" (if (plusp calls) (/ (float elapsed 1d0) calls) 0))
    (when tracker
      (%set-key resource "totalTokens" (cost-tracker-total-tokens tracker))
      (%set-key resource "costByModel" (cost-tracker-token-usage tracker)))
    stats))

(defun optimizer-score-history (state)
  (%opt-array (reverse (optimizer-state-score-history state))))

(defun optimizer-configuration-history (state)
  (%opt-array (mapcar #'%opt-clone (reverse (optimizer-state-configuration-history state)))))

(defun record-optimizer-round (state round score configuration)
  "Record one finished round, updating the best score and the convergence info.

A round that does not improve on the best by more than the convergence
threshold counts as stagnation; an improving round clears it."
  (let* ((stats (optimizer-state-stats state))
         (score (%opt-num score 0d0))
         (configuration (if (%opt-object-p configuration) configuration (%new-object)))
         (convergence (jget stats "convergenceInfo"))
         (best (%opt-num (jget stats "bestScore") 0d0))
         (improvement (- score best))
         (threshold (%opt-num (jget convergence "convergenceThreshold") 0.01d0)))
    (setf (optimizer-current-round state) (%opt-int round 0))
    (push score (optimizer-state-score-history state))
    (push (%opt-clone configuration) (optimizer-state-configuration-history state))
    (%set-key stats "totalCalls" (1+ (%opt-int (jget stats "totalCalls") 0)))
    (if (> improvement threshold)
        (progn
          (%set-key stats "bestScore" score)
          (%set-key stats "bestConfiguration" (%opt-clone configuration))
          (%set-key convergence "finalImprovement" improvement)
          (%set-key convergence "stagnationRounds" 0))
        (%set-key convergence "stagnationRounds"
                  (1+ (%opt-int (jget convergence "stagnationRounds") 0))))
    state))

(defun reset-optimizer (state)
  "Clear STATE back to a fresh run, including its cost tracker."
  (setf (optimizer-state-stats state) (%optimizer-initial-stats)
        (optimizer-state-score-history state) '()
        (optimizer-state-configuration-history state) '()
        (optimizer-current-round state) 0
        (optimizer-state-started-at state) (get-internal-real-time))
  (when (optimizer-state-cost-tracker state)
    (reset-cost-tracker (optimizer-state-cost-tracker state)))
  state)

(defun optimizer-checkpoint (state &key (optimizer-type "unknown") optimizer-config
                                        best-score best-configuration engine-state)
  "A checkpoint object describing STATE, ready to be stored as JSON.

totalRounds reports the round reached only once the run has spent measurable
time, which is the reference's rule: a checkpoint taken before any work
claims no rounds."
  (let* ((started (get-internal-real-time))
         (stats (optimizer-stats state))
         (elapsed (%opt-int (jget (jget stats "resourceUsage") "totalTime") 0)))
    (record-checkpoint-metric (get-or-create-optimizer-metrics-instruments)
                              "save"
                              (round (* 1000 (- (get-internal-real-time) started))
                                     internal-time-units-per-second)
                              t
                              (%opt-string optimizer-type "unknown"))
    (object "version" "1.0.0"
            "timestamp" (get-universal-time)
            "optimizerType" (%opt-string optimizer-type "unknown")
            "optimizerConfig" (if (%opt-object-p optimizer-config)
                                  (%opt-clone optimizer-config)
                                  (%new-object))
            "currentRound" (optimizer-current-round state)
            "totalRounds" (if (plusp elapsed) (optimizer-current-round state) 0)
            "bestScore" (if (%opt-finite-p best-score)
                            (%opt-num best-score 0d0)
                            (jget stats "bestScore"))
            "bestConfiguration" (if (%opt-object-p best-configuration)
                                    (%opt-clone best-configuration)
                                    (jget stats "bestConfiguration"))
            "scoreHistory" (optimizer-score-history state)
            "configurationHistory" (optimizer-configuration-history state)
            "stats" stats
            "optimizerState" (if (%opt-object-p engine-state)
                                 (%opt-clone engine-state)
                                 (%new-object))
            "examples" (%new-array))))

(defun load-optimizer-checkpoint (state checkpoint)
  "Restore STATE from CHECKPOINT and return the engine state it carried.

The round counter, both histories and the statistics come back, so a resumed
run continues its own numbering instead of starting over."
  (unless (%opt-object-p checkpoint)
    (optimize-fail :config "A checkpoint must be an object."))
  (let ((version (%opt-present (jget checkpoint "version"))))
    (unless (equal version "1.0.0")
      (optimize-fail :config "Unsupported optimizer checkpoint version ~s." version)))
  (setf (optimizer-current-round state) (%opt-int (jget checkpoint "currentRound") 0)
        (optimizer-state-score-history state)
        (reverse (mapcar (lambda (score) (%opt-num score 0d0))
                         (%opt-list (jget checkpoint "scoreHistory"))))
        (optimizer-state-configuration-history state)
        (reverse (mapcar #'%opt-clone (%opt-list (jget checkpoint "configurationHistory")))))
  (let ((stats (jget checkpoint "stats")))
    (when (%opt-object-p stats)
      (setf (optimizer-state-stats state) (%opt-clone stats))))
  (let ((engine-state (jget checkpoint "optimizerState")))
    (if (%opt-object-p engine-state) engine-state (%new-object))))

;;; ------------------------------------------------------------------
;;; Optimized program records
;;; ------------------------------------------------------------------
;;;
;;; The portable record a finished run hands back: what was learned, how
;;; well it scored, and enough state to resume.  It round-trips through
;;; JSON, and APPLY-OPTIMIZED-PROGRAM installs it on a program through the
;;; same Core validation an artifact goes through.

(defparameter +optimized-program-fields+
  '("bestScore" "stats" "componentMap" "selectorState" "demos" "examples"
    "modelConfig" "optimizerType" "optimizationTime" "totalRounds" "converged"
    "scoreHistory" "configurationHistory" "artifactFormatVersion" "instructionSchema")
  "The fields an optimized-program record carries, in their canonical order.")

(defun make-optimized-program (&key (best-score 0) stats component-map selector-state
                                    demos examples model-config (optimizer-type "unknown")
                                    (optimization-time 0) (total-rounds 0) converged
                                    score-history configuration-history
                                    (artifact-format-version 1) instruction-schema)
  "An optimized-program record.

COMPONENT-MAP is the text the run selected, SELECTOR-STATE the bandit state
a later run can resume from, DEMOS the few-shot traces it mined."
  (let ((out (%new-object)))
    (%set-key out "bestScore" (%opt-num best-score 0d0))
    (%set-key out "stats" (if (%opt-object-p stats) (%opt-clone stats) (%optimizer-initial-stats)))
    (%set-key out "componentMap" (if (%opt-object-p component-map)
                                     (%opt-clone component-map)
                                     (%new-object)))
    (%set-key out "selectorState" (if (%opt-object-p selector-state)
                                      (%opt-clone selector-state)
                                      (%new-object)))
    (%set-key out "demos" (%opt-array (mapcar #'%opt-clone (%opt-list demos))))
    (%set-key out "examples" (%opt-array (mapcar #'%opt-clone (%opt-list examples))))
    (%set-key out "modelConfig" (if (%opt-object-p model-config)
                                    (%opt-clone model-config)
                                    (%new-object)))
    (%set-key out "optimizerType" (%opt-string optimizer-type "unknown"))
    (%set-key out "optimizationTime" (%opt-int optimization-time 0 0))
    (%set-key out "totalRounds" (%opt-int total-rounds 0 0))
    (%set-key out "converged" (json-boolean converged))
    (%set-key out "scoreHistory" (%opt-array (mapcar (lambda (score) (%opt-num score 0d0))
                                                     (%opt-list score-history))))
    (%set-key out "configurationHistory"
              (%opt-array (mapcar #'%opt-clone (%opt-list configuration-history))))
    (%set-key out "artifactFormatVersion" (%opt-int artifact-format-version 1 0))
    (%set-key out "instructionSchema" (if (stringp instruction-schema) instruction-schema :null))
    out))

(defun optimized-program-json (optimized)
  "OPTIMIZED serialized to JSON text, fields in their canonical order."
  (unless (%opt-object-p optimized)
    (optimize-fail :artifact "An optimized program must be an object."))
  (let ((out (%new-object)))
    (dolist (field +optimized-program-fields+)
      (when (%opt-key-present-p optimized field)
        (%set-key out field (%opt-clone (jget optimized field)))))
    (encode-json out)))

(defun parse-optimized-program (text)
  "TEXT read back as an optimized-program record.

Every field is rebuilt through MAKE-OPTIMIZED-PROGRAM, so a record missing
or carrying a wrong-typed field comes back with that field's documented
default instead of propagating the damage."
  (let ((parsed (if (stringp text) (parse-json text) text)))
    (unless (%opt-object-p parsed)
      (optimize-fail :artifact "An optimized program must be a JSON object."))
    (make-optimized-program
     :best-score (jget parsed "bestScore" 0)
     :stats (jget parsed "stats")
     :component-map (jget parsed "componentMap")
     :selector-state (jget parsed "selectorState")
     :demos (jget parsed "demos")
     :examples (jget parsed "examples")
     :model-config (jget parsed "modelConfig")
     :optimizer-type (%opt-string (%opt-present (jget parsed "optimizerType")) "unknown")
     :optimization-time (jget parsed "optimizationTime" 0)
     :total-rounds (jget parsed "totalRounds" 0)
     :converged (json-true-p (jget parsed "converged"))
     :score-history (jget parsed "scoreHistory")
     :configuration-history (jget parsed "configurationHistory")
     :artifact-format-version (jget parsed "artifactFormatVersion" 1)
     :instruction-schema (%opt-present (jget parsed "instructionSchema")))))

(defun optimized-program-artifact (optimized &key (program-kind "unknown"))
  "OPTIMIZED as the portable artifact Core validates and applies."
  (object "artifactVersion" "axir-optimized-artifact-v1"
          "optimizerName" (%opt-string (%opt-present (jget optimized "optimizerType")) "unknown")
          "optimizerVersion" (format nil "optimized-program-v~a"
                                     (%opt-int (jget optimized "artifactFormatVersion") 1))
          "componentMap" (%opt-clone (jget optimized "componentMap" (%new-object)))
          "demos" (%opt-clone (jget optimized "demos" (%new-array)))
          "metadata" (object "optimizer" (jget optimized "optimizerType")
                             "bestScore" (jget optimized "bestScore")
                             "selectorState" (%opt-clone (jget optimized "selectorState"
                                                               (%new-object)))
                             "totalRounds" (jget optimized "totalRounds")
                             "converged" (jget optimized "converged"))
          "evidence" (object "avg" (jget optimized "bestScore")
                             "count" (%opt-count (jget optimized "scoreHistory")))
          "provenance" (object "sourceProgramKind" (%opt-string program-kind "unknown"))))

(defun apply-optimized-program (optimized program)
  "Install OPTIMIZED on PROGRAM and return the validated artifact.

The record is converted to the portable artifact first, so the same Core
checks run as for any other artifact: a record built for another program is
rejected rather than half-applied."
  (apply-optimization program
                      (optimized-program-artifact optimized
                                                  :program-kind (program-kind program))))

;;; ------------------------------------------------------------------
;;; Multi-objective optimization
;;; ------------------------------------------------------------------

(defun %pareto-weight-combinations (objectives)
  "The weightings a Pareto sweep runs: each objective alone, then equal, then
a granular sweep for two objectives and three fixed mixes for three."
  (let ((combinations '())
        (count (length objectives)))
    (dolist (focus objectives)
      (let ((weights (%new-object)))
        (dolist (objective objectives)
          (%set-key weights objective (if (string= objective focus) 1 0)))
        (push weights combinations)))
    (let ((equal-weights (%new-object)))
      (dolist (objective objectives)
        (%set-key equal-weights objective (/ 1d0 (max 1 count))))
      (push equal-weights combinations))
    (when (= count 2)
      (loop for first = 0.1d0 then (+ first 0.2d0)
            while (<= first 0.9000001d0)
            do (let ((weights (%new-object)))
                 (%set-key weights (first objectives) first)
                 (%set-key weights (second objectives) (- 1d0 first))
                 (push weights combinations))))
    (when (= count 3)
      (dolist (mix '((0.5d0 0.3d0 0.2d0) (0.3d0 0.5d0 0.2d0) (0.2d0 0.3d0 0.5d0)))
        (let ((weights (%new-object)))
          (loop for objective in objectives
                for value in mix
                do (%set-key weights objective value))
          (push weights combinations))))
    (nreverse combinations)))

(defun %pareto-weighted-metric (multi-metric weights)
  (lambda (task prediction)
    (let ((scores (funcall multi-metric task prediction))
          (total 0d0))
      (when (%opt-object-p scores)
        (dolist (key (%object-keys scores))
          (incf total (* (%opt-num (gethash key scores) 0d0)
                         (%opt-num (jget weights key) 0d0)))))
      total)))

(defun %pareto-constraint-metric (multi-metric primary)
  "PRIMARY's score, penalized for every other objective below 0.3."
  (lambda (task prediction)
    (let ((scores (funcall multi-metric task prediction))
          (penalty 0d0))
      (unless (%opt-object-p scores) (return-from %pareto-constraint-metric 0d0))
      (dolist (key (%object-keys scores))
        (unless (string= key primary)
          (let ((score (%opt-num (gethash key scores) 0d0)))
            (when (< score 0.3d0)
              (incf penalty (* (- 0.3d0 score) 2d0))))))
      (- (%opt-num (jget scores primary) 0d0) penalty))))

(defun %pareto-measure (program client dataset multi-metric component-map options)
  "The mean of each named score for COMPONENT-MAP, really run."
  (let ((evaluator (make-program-evaluator
                    program client
                    :dataset dataset
                    :options options
                    :rollout (%opt-present (jget options "rollout"))
                    :metric (lambda (task prediction)
                              (funcall multi-metric task prediction))
                    :cancel (%opt-present (jget options "cancel")))))
    (%opt-avg-vector (%opt-result-rows
                      (evaluate-candidate evaluator component-map
                                          (object "phase" "pareto"))))))

(defun optimize-pareto (program dataset &key engine client options multi-metric)
  "Optimize PROGRAM against several objectives and return the Pareto frontier.

MULTI-METRIC is called as (MULTI-METRIC TASK PREDICTION) and returns a JSON
object of named scores.  The sweep runs ENGINE once per weighting of those
objectives, and once more per objective with the others constrained, then
keeps the solutions nothing else dominates.  Each solution really ran: its
scores are measured with MULTI-METRIC after its component map was applied.

Returns an object with paretoFront, solutions, hypervolume and bestScore.
A weighting whose run fails is dropped, as the reference does, so one bad
corner of the sweep does not lose the rest."
  (unless multi-metric
    (optimize-fail :config "optimize-pareto needs a multi-objective metric."))
  (unless client
    (optimize-fail :config "optimize-pareto needs a client to measure solutions with."))
  (let* ((options (if (%opt-object-p options) (%opt-clone options) (%new-object)))
         (tasks (%opt-list (jget (axllm/core::normalize-optimization-dataset dataset) "train")))
         (multi-metric (%opt-function multi-metric "multi-objective metric")))
    (unless tasks
      (optimize-fail :config "optimize-pareto needs at least one example."))
    (let* ((sample (%pareto-measure program client (%opt-array (list (first tasks)))
                                    multi-metric (%new-object) options))
           (objectives (%object-keys sample))
           (solutions (%new-array)))
      (unless objectives
        (optimize-fail :config "The multi-objective metric returned no named scores."))
      (flet ((run (metric configuration)
               (handler-case
                   (let* ((run-options (%opt-merge options (object "apply" false)))
                          (artifact (progn (%set-key run-options "metric" metric)
                                           (optimize-program program dataset
                                                             :engine engine
                                                             :client client
                                                             :options run-options)))
                          (component-map (jget artifact "componentMap" (%new-object)))
                          (scores (%pareto-measure program client dataset multi-metric
                                                   component-map options)))
                     (vector-push-extend (object "scores" scores
                                                 "componentMap" (%opt-clone component-map)
                                                 "demos" (jget artifact "demos" (%new-array))
                                                 "configuration" configuration)
                                         solutions))
                 ;; One failed corner of the sweep must not lose the others.
                 (optimize-error (condition)
                   (when (eq (optimize-error-kind condition) :cancelled) (error condition))
                   nil)
                 (error () nil))))
        (dolist (weights (%pareto-weight-combinations objectives))
          (run (%pareto-weighted-metric multi-metric weights)
               (object "weights" (%opt-clone weights) "strategy" "weighted_combination")))
        (dolist (primary objectives)
          (run (%pareto-constraint-metric multi-metric primary)
               (object "primaryObjective" primary "strategy" "constraint_based"))))
      (let* ((front (%opt-pareto-front solutions
                                       (%opt-num (%opt-option options '("tieEpsilon")) 0d0)))
             (hypervolume (%opt-hypervolume-2d (mapcar #'second front)))
             (best 0d0))
        (dolist (item front)
          (dolist (key (%object-keys (second item)))
            (setf best (max best (%opt-num (gethash key (second item)) 0d0)))))
        (object "paretoFront"
                (%opt-array (mapcar (lambda (item)
                                      (let ((solution (aref solutions (first item))))
                                        (object "solution" (first item)
                                                "scores" (second item)
                                                "dominatedSolutions" (third item)
                                                "componentMap" (%opt-clone
                                                                (jget solution "componentMap"))
                                                "demos" (%opt-clone (jget solution "demos"))
                                                "configuration" (%opt-clone
                                                                 (jget solution "configuration")))))
                                    front))
                "solutions" solutions
                "solutionCount" (length front)
                "hypervolume" (if (eq hypervolume :null) 0d0 hypervolume)
                "bestScore" best)))))

(defun optimize-program-stream (program dataset &key engine client options)
  "Run OPTIMIZE-PROGRAM and return (values ARTIFACT PROGRESS).

PROGRESS is always empty.  This mirrors the reference optimizer's
compileStream, which awaits the compile and yields no progress items:
progress reaches a caller through the progress callback it installed, not
through the stream.  The empty sequence is reported rather than hidden so
nobody waits on events that never come."
  (values (optimize-program program dataset :engine engine :client client :options options)
          (%new-array)))

;;; ------------------------------------------------------------------
;;; Playbook: the ACE roles as real Ax programs
;;; ------------------------------------------------------------------
;;;
;;; The ACE driver takes its Reflector and Curator as functions so a run can
;;; be reproduced without a provider.  This is the other half: the same two
;;; roles backed by real AxGen programs over the signatures every port uses,
;;; so a caller gets a playbook that actually learns from a model.
;;;
;;; The signatures are the ones TypeScript builds in
;;; src/ax/dsp/optimizers/ace.ts.  They are built with the field builder
;;; rather than signature text because the curator's operations description
;;; contains double quotes, which signature text cannot carry.

(defun ace-reflector-signature ()
  "The Reflector's signature: judge a generator answer against the playbook."
  (s :inputs (object
              "question" (f "string" :description "Original task input serialized as JSON")
              "generator_answer" (f "string" :description "Generator output serialized as JSON")
              "generator_reasoning" (f "string" :description "Generator reasoning trace"
                                                :optional t)
              "playbook" (f "string" :description "Current context playbook rendered as markdown")
              "expected_answer" (f "string"
                                   :description "Expected output when ground truth is available"
                                   :optional t)
              "feedback" (f "string" :description "External feedback or reward signal" :optional t)
              "previous_reflection" (f "string"
                                       :description "Most recent reflection JSON when running multi-round refinement"
                                       :optional t))
     :outputs (object
               "reasoning" (f "string" :description "Step-by-step analysis of generator performance")
               "errorIdentification" (f "string" :description "Specific mistakes detected")
               "rootCauseAnalysis" (f "string" :description "Underlying cause of the error")
               "correctApproach" (f "string" :description "What the generator should do differently")
               "keyInsight" (f "string" :description "Reusable insight to remember")
               "bulletTags" (f "json"
                               :description "Array of {id, tag} entries referencing playbook bullets"))))

(defparameter +ace-curator-operations-description+
  (concatenate 'string
               "List of operations, each {type: \"ADD\"|\"UPDATE\"|\"REMOVE\", section, content}. "
               "Emit an operation ONLY when the playbook should actually change. "
               "If nothing should change, return an empty array "
               (format nil "~c" (code-char #x2014))
               " never emit an ADD whose content "
               "just acknowledges that no change is needed (e.g. \"No update required\", "
               "\"Keep the existing rule unchanged\"). "
               "Each ADD content must be a standalone, reusable rule.")
  "The Curator's operations description, as every port writes it.

It carries double quotes, which is why the Curator's signature is built with
the field builder instead of signature text.")

(defun ace-curator-signature ()
  "The Curator's signature: turn a reflection into playbook operations."
  (s :inputs (object
              "playbook" (f "string" :description "Current playbook serialized as JSON")
              "reflection" (f "string" :description "Latest reflection output serialized as JSON")
              "question_context" (f "string" :description "Original task input serialized as JSON")
              "token_budget" (f "number" :description "Approximate token budget for curator response"
                                         :optional t))
     :outputs (object
               "reasoning" (f "string" :description "Justification for the proposed updates")
               "operations" (f "json" :description +ace-curator-operations-description+))))

(defun %playbook-text (value)
  "VALUE as the role signatures take it: text stays text, else compact JSON."
  (cond ((stringp value) value)
        ((eq value :null) "")
        ((or (%opt-object-p value) (%opt-array-p value)) (encode-json value))
        (t (axllm/core::core-js-text value))))

(defun make-gen-reflector (client &key options)
  "An ACE Reflector callback backed by a real AxGen program over CLIENT.

The payload the driver passes is rendered into the Reflector's input fields,
and the program's typed outputs are returned as the reflection object, so
the driver's bulletTags normalization and resolution run on real output."
  (let ((program (ax (ace-reflector-signature))))
    (lambda (payload)
      (let ((inputs (object "question" (%playbook-text (jget payload "question"))
                            "generator_answer" (%playbook-text (jget payload "generator_answer"))
                            "playbook" (%playbook-text (jget payload "playbook")))))
        (dolist (key '("generator_reasoning" "feedback" "previous_reflection" "expected_answer"))
          (let ((value (%opt-present (jget payload key))))
            (when value (%set-key inputs key (%playbook-text value)))))
        (forward program client inputs (if (%opt-object-p options) options (%new-object)))))))

(defun make-gen-curator (client &key options)
  "An ACE Curator callback backed by a real AxGen program over CLIENT.

Returns the program's typed output, whose \"operations\" the driver then
normalizes and resolves through Core before anything is applied."
  (let ((program (ax (ace-curator-signature))))
    (lambda (payload)
      (let ((inputs (object "playbook" (%playbook-text (jget payload "playbook"))
                            "reflection" (%playbook-text (jget payload "reflection"))
                            "question_context" (%playbook-text (jget payload "question_context")))))
        (let ((budget (%opt-present (jget payload "token_budget"))))
          (when (%opt-finite-p budget) (%set-key inputs "token_budget" budget)))
        (forward program client inputs (if (%opt-object-p options) options (%new-object)))))))

(defun make-playbook (&key program target signature student teacher generator metric options)
  "A playbook: the ACE driver with its Reflector and Curator as real programs.

STUDENT is the client the generator runs against; TEACHER is the client the
Reflector and Curator run against, defaulting to STUDENT.  GENERATOR
overrides how an example is answered; without one, PROGRAM is run through
FORWARD against STUDENT.  Everything else is the ACE driver's, so a
playbook built here and an ACE driver built with scripted callbacks take the
same options and produce the same playbook structure.

PROGRAM is the whole program, not one stage: a playbook is judged on the
answer a run really produced, so the generator runs the program end to end
and the metric sees what the caller sees.  TARGET names the component the
rendered playbook is attached to, for a host that injects it into one stage
rather than the whole program.  It is recorded under "target" and read by
the attach, never by the driver, so binding a playbook to a stage and
evolving it against the full run stay separate decisions.  PLAYBOOK-TARGET
reads it back.

Returns the ACE driver, so ACE-COMPILE, ACE-APPLY-ONLINE-UPDATE,
ACE-PLAYBOOK, ACE-RENDER and ACE-ARTIFACT all apply to it."
  (let* ((options (if (%opt-object-p options) (%opt-clone options) (%new-object)))
         (student (or student (%opt-present (%opt-option options '("studentAI" "student_ai")))))
         (teacher (or teacher
                      (%opt-present (%opt-option options '("teacherAI" "teacher_ai")))
                      student)))
    (unless teacher
      (optimize-fail :config
                     "make-playbook needs a teacher or student client for its Reflector and Curator."))
    (when target
      (%set-key options "target" target))
    (let* ((signature (or signature
                          (when (typep program 'generator) (generator-signature program))))
           (driver (make-ace :metric metric :options options)))
      ;; The roles close over the driver so each call reads the playbook as
      ;; it stands, not as it stood when the playbook was built.
      (setf (ace-program driver) program
            (ace-signature driver) signature
            (ace-student driver) student
            (ace-teacher driver) teacher
            (ace-forward-options driver) (%new-object))
      (%playbook-bind-roles driver options)
      (when generator (setf (slot-value driver 'generator) generator))
      driver)))

(defun %playbook-teacher-options (options)
  "The call options the Reflector and the Curator run under.

They run against the teacher, so they carry the teacher's options and not
the playbook's whole configuration: a client that requires confirmation
before it will answer is confirmed here or refuses, and passing the
configuration object instead would send the driver's own settings to a
provider as if they were request options."
  (let ((given (%opt-present (%opt-option options '("teacherOptions" "teacher_options")))))
    (if (%opt-object-p given) (%opt-clone given) (%new-object))))

(defun %playbook-bind-roles (driver options)
  "Bind DRIVER's three roles against its recorded clients and OPTIONS."
  (let ((teacher (ace-teacher driver))
        (student (ace-student driver))
        (program (ace-program driver))
        (signature (ace-signature driver)))
    (setf (slot-value driver 'reflector)
          (%playbook-reflector driver teacher signature (%playbook-teacher-options options))
          (slot-value driver 'curator)
          (%playbook-curator driver teacher (%playbook-teacher-options options)))
    (when program
      (setf (slot-value driver 'generator)
            (lambda (example)
              (unless student
                (optimize-fail :config
                               "a playbook needs a student client to answer an example."))
              (forward program student
                       (%playbook-example-task example signature)
                       (%opt-clone (ace-forward-options driver))))))
    driver))

(defun playbook-reflector-signature ()
  "The Reflector's signature for playbook evolution.

Five inputs, not the ACE Reflector's seven: this surface never carries a
generator reasoning trace or a previous reflection, and the rendered prompt
is pinned by ir/conformance/axoptimize/playbook-evolve-teacher-inputs.json,
so an extra declared field would add a spec line and diverge."
  (s :inputs (object
              "question" (f "string" :description "Original task input serialized as JSON")
              "generator_answer" (f "string" :description "Generator output serialized as JSON")
              "playbook" (f "string" :description "Current context playbook rendered as markdown")
              "expected_answer" (f "string"
                                   :description "Expected output when ground truth is available")
              "feedback" (f "string" :description "External feedback or reward signal"))
     :outputs (object
               "reasoning" (f "string" :description "Step-by-step analysis of generator performance")
               "errorIdentification" (f "string" :description "Specific mistakes detected")
               "rootCauseAnalysis" (f "string" :description "Underlying cause of the error")
               "correctApproach" (f "string" :description "What the generator should do differently")
               "keyInsight" (f "string" :description "Reusable insight to remember")
               "bulletTags" (f "json"
                               :description "Array of {id, tag} entries referencing playbook bullets"))))

(defun %playbook-example-task (example signature)
  "The task EXAMPLE poses, whatever shape the dataset uses.

An optimizer dataset nests the inputs under \"input\" and carries a score
beside them; a plain example carries its fields at the top level.  Both are
real shapes, so both are unwrapped here rather than one of them being the
caller's problem."
  (let ((input (jget example "input")))
    (if (%opt-object-p input)
        (%playbook-project input signature :input)
        (%playbook-project example signature :input))))

(defun %playbook-example-truth (example signature)
  "The ground truth EXAMPLE carries, or an empty object when it carries none."
  (let ((expected (%opt-option example '("expectedOutput" "expected_output" "expected"))))
    (if (%opt-object-p expected)
        (%playbook-project expected signature :output)
        (let ((input (jget example "input")))
          (if (%opt-object-p input)
              ;; A dataset example keeps its inputs under "input", so the
              ;; rest of the object is score and metadata, not ground truth.
              (%new-object)
              (%playbook-project example signature :output))))))

(defun %playbook-project (example signature side)
  "EXAMPLE restricted to SIGNATURE's fields on SIDE, in declaration order.

Without a signature there is nothing to split on, so an input projection is
the example as it came and an output projection is empty: a playbook built
without a program still runs, it just cannot name the ground truth."
  (when (null signature)
    (return-from %playbook-project
      (if (eq side :input) (%opt-clone example) (%new-object))))
  (let ((out (%new-object)))
    (when (%opt-object-p example)
      (loop for field across (signature-fields signature :side side)
            do (let ((name (%opt-string (jget field "name") "")))
                 (when (%opt-key-present-p example name)
                   (%set-key out name (jget example name))))))
    out))

(defun %playbook-field (driver)
  "The Playbook input every evolve role receives.

Both the render and the structure, as the fixture pins: a Reflector reads
the markdown and a Curator reasons over the sections, so sending only one of
them would make one of the two roles guess."
  (encode-json (object "markdown" (ace-render driver)
                       "structured" (ace-playbook driver))))

(defun %playbook-reflector (driver client signature options)
  "The evolve Reflector: a real program over PLAYBOOK-REFLECTOR-SIGNATURE.

The payload's \"question\" is the whole example, so this projects it onto the
program's own input and output fields: the task goes to Question and the
ground truth to Expected answer."
  (let ((program (ax (playbook-reflector-signature))))
    (lambda (payload)
      (let* ((example (jget payload "question"))
             (inputs (object "question" (encode-json (%playbook-example-task example signature))
                             "generator_answer" (%playbook-text (jget payload "generator_answer"))
                             "playbook" (%playbook-field driver)
                             "expected_answer" (encode-json
                                               (%playbook-example-truth example signature))
                             "feedback" (%playbook-text (jget payload "feedback")))))
        (forward program client inputs (if (%opt-object-p options) options (%new-object)))))))

(defun %playbook-curator (driver client options)
  "The evolve Curator: a real program over ACE-CURATOR-SIGNATURE."
  (let ((program (ax (ace-curator-signature))))
    (lambda (payload)
      (let ((inputs (object "playbook" (%playbook-field driver)
                            "reflection" (%playbook-text (jget payload "reflection"))
                            "question_context" (%playbook-text (jget payload "question_context")))))
        (let ((budget (%opt-present (jget payload "token_budget"))))
          (when (%opt-finite-p budget) (%set-key inputs "token_budget" budget)))
        (forward program client inputs (if (%opt-object-p options) options (%new-object)))))))

(defun playbook-target (playbook)
  "The component the rendered playbook is attached to, or :NULL.

The driver never reads this: it is the contract between whoever built the
playbook and whoever attaches it to a program."
  (let ((target (%opt-present (jget (ace-options playbook) "target"))))
    (if target target :null)))

(defun playbook-load (playbook state)
  "Restore STATE into PLAYBOOK and return it.

Both the public seed loader and the exact undo a rejected proposal needs.
The playbook, the feedback events and the applied deltas all come back
together, so reloading a state captured before an update leaves no trace of
that update: a rejected rule costs the artifact nothing, and a before and
after comparison over one round really spans that round.

The restored playbook also becomes what ACE-RESET returns to, so a host
that loads a configured seed and later resets lands back on the seed rather
than on an empty playbook."
  (unless (or (null state) (eq state :null) (%opt-object-p state))
    (optimize-fail :config "playbook-load: a state must be a JSON object."))
  (when (%opt-object-p state)
    ;; A full state nests the playbook under "playbook"; a caller holding
    ;; only a playbook passes it bare.  The two are told apart by shape
    ;; rather than by a flag, because a playbook always carries "sections"
    ;; and a state never does, so neither can be mistaken for the other and
    ;; a bare playbook is not silently loaded as an empty one.
    (let* ((bare (and (not (%opt-key-present-p state "playbook"))
                      (%opt-key-present-p state "sections")))
           (inner (if bare state (jget state "playbook")))
           (artifact (if bare :null (jget state "artifact"))))
      (setf (ace-playbook-slot playbook)
            (if (%opt-object-p inner)
                (%opt-clone inner)
                (axllm/core::ace-empty-playbook :null (%ace-now playbook)))
            (ace-initial-playbook playbook)
            (when (%opt-object-p inner) (%opt-clone inner))
            (ace-base-instruction-slot playbook) :null)
      (if (%opt-object-p artifact)
          (setf (ace-feedback playbook)
                (reverse (mapcar #'%opt-clone (%opt-list (jget artifact "feedback"))))
                (ace-deltas playbook)
                (reverse (mapcar #'%opt-clone (%opt-list (jget artifact "history")))))
          (setf (ace-feedback playbook) nil
                (ace-deltas playbook) nil))))
  playbook)

(defun %playbook-examples (examples)
  "The examples to evolve over, from any dataset shape Ax uses.

A bare array is the examples; a {train, validation} object is the optimizer
dataset and the training split is what a playbook learns from.  Normalizing
here rather than in the caller keeps evolve taking the same dataset every
other optimizer entry point takes."
  (let ((normalized (axllm/core::normalize-optimization-dataset
                     (if (eq examples :null) (%new-array) examples))))
    (%opt-array (%opt-list (jget normalized "train")))))

(defparameter +playbook-evolve-config-keys+
  '("maxEpochs" "maxReflectorRounds" "maxSectionSize" "maxSerializedFieldChars"
    "similarityThreshold" "allowDynamicSections")
  "The driver configuration an evolve call may override for that call.")

;;; ------------------------------------------------------------------
;;; Playbook evolution: baseline, failure clusters, mined weaknesses
;;; ------------------------------------------------------------------

(defparameter +evolve-default-max-proposals+ 4
  "Failure clusters mined in one round when the caller names no bound.")

(defparameter +evolve-default-epsilon+ 0.01d0
  "How far held-out may slip before an otherwise good proposal is a regression.")

(defparameter +evolve-default-min-held-in-gain+ 0.05d0
  "The held-in gain a proposal must clear to be kept.")

(defparameter +evolve-default-score-threshold+ 0.7d0
  "At or above this a run passed; below it the run is a failure worth mining.")

(defparameter +evolve-behavioral-cluster-signature+ "behavioral:no_error"
  "Where a run lands when it failed without naming an error.")

(defparameter +evolve-evidence-quote-limit+ 3
  "Grounding quotes carried into a proposal's feedback.")

(defparameter +evolve-playbook-failure-section+ "failures_to_avoid"
  "The playbook section an evolve proposal asks the curator to curate into.")

(defparameter +evolve-miner-description+
  (concatenate
   'string
   "You are a failure analyst for an LLM agent harness. You receive one "
   "cluster of failed agent runs sharing an error signature, with excerpts "
   "of what the agent actually did. Identify the single recurring weakness, "
   "its root cause, and one narrow, durable avoidance rule the agent should "
   "recall while acting. Ground every claim: evidenceQuotes must be verbatim "
   "substrings copied from the excerpts. Keep proposedGuidance concise, "
   "imperative, and general to the failure mode (not one task). Use "
   "configRecommendations only for setup problems no prompt text can fix "
   "(missing tools, timeouts, model choice).")
  "The miner's task definition, pinned by the miner-system-prompt fixture.")

(defun evolve-miner-signature ()
  "The weakness miner's signature.

Six declared inputs, two of them optional: a cluster with no tool calls and
no tool errors renders four input lines, which is what
ir/conformance/axagent/agent-playbook-evolve-miner-system-prompt.json pins
byte for byte.  Declaring the optional pair unconditionally would add two
spec lines to every miner prompt and diverge from every other port.

The task definition belongs to the signature because that is where the
prompt renderer reads it from; passing it to the generator instead renders
no <task_definition> block at all, which is a prompt that silently differs
from every other port's."
  (s :description +evolve-miner-description+
     :inputs (object
              "clusterSignature" (f "string" :description "Shared error signature of the cluster.")
              "taskSummaries" (f "string" :description "One line per failing task.")
              "actionLogExcerpts" (f "string"
                                     :description "Excerpts of the failing runs, centered on the failure.")
              "functionCallSummary" (f "string"
                                       :description "Digest of runtime/tool calls in the failing runs."
                                       :optional t)
              "toolErrors" (f "string" :description "Tool errors observed." :optional t)
              "currentPlaybook" (f "string"
                                   :description "The failure-avoidance playbook currently applied."
                                   :optional t))
     :outputs (object
               "weaknessDescription" (f "string" :description "The recurring weakness, one sentence.")
               "rootCause" (f "string" :description "Why the runs fail, mechanically.")
               "proposedGuidance" (f "string"
                                     :description
                                     (format nil "The avoidance rule to add to the playbook ~c concise, imperative."
                                             (code-char #x2014)))
               "evidenceQuotes" (f "string" :array t
                                   :description
                                   "Verbatim substrings from actionLogExcerpts proving the weakness.")
               "configRecommendations" (f "string" :array t :optional t
                                          :description
                                          "Setup/config suggestions no prompt text can fix."))))

(defun %evolve-text (value)
  "VALUE as a string, or \"\" when it is absent."
  (cond ((stringp value) value)
        ((or (null value) (eq value :null)) "")
        (t (princ-to-string value))))

(defun %evolve-number (value &optional (default 0))
  (if (and (realp value) (%opt-finite-p value)) value default))

(defun %evolve-error-signature (text)
  "TEXT reduced to the error line that names it, or its leading characters.

The same reduction Core's context manager applies, so a run that threw and
a run whose log merely mentions the throw land in one cluster rather than
two."
  (let ((text (%evolve-text text)))
    (multiple-value-bind (whole groups)
        (cl-ppcre:scan-to-strings "(?m)^(\\w+Error:\\s*.{0,60})" text)
      (declare (ignore whole))
      (if (and groups (plusp (length groups)) (stringp (aref groups 0)))
          (aref groups 0)
          (subseq text 0 (min 80 (length text)))))))

(defun %evolve-first-line (text)
  (let* ((text (%evolve-text text))
         (break (position #\Newline text)))
    (if break (subseq text 0 break) text)))

(defun %evolve-dominant-signal (signals)
  "The signature SIGNALS reported most often, counting occurrences.

Ties go to the one reported first, so a run that reported two signals
equally often clusters the same way on every run."
  (let ((counts (make-hash-table :test #'equal))
        (order '()))
    (dolist (signal (%opt-list signals))
      (let ((signature (%opt-present (jget signal "signature"))))
        (when (stringp signature)
          (unless (nth-value 1 (gethash signature counts))
            (push signature order)
            (setf (gethash signature counts) 0))
          (incf (gethash signature counts)
                (%evolve-number (jget signal "occurrences") 1)))))
    (let ((best nil) (best-count 0))
      (dolist (signature (nreverse order) best)
        (let ((count (gethash signature counts 0)))
          (when (> count best-count)
            (setf best signature best-count count)))))))

(defun %evolve-record-signature (record)
  "RECORD's cluster fingerprint.

What the run reported about its own failure is preferred to what can be
scraped out of its log: the dominant failure signal, then the first tool
error, then the error it threw, then an error line in the action log, and
only then the behavioral cluster.  A run that both threw and reported
signals is better described by what it reported."
  (let* ((prediction (jget record "prediction"))
         (signals (jget prediction "failureSignals")))
    (or (and (plusp (%opt-count signals)) (%evolve-dominant-signal signals))
        (let ((tool-error (first (%opt-list (jget prediction "toolErrors")))))
          (when (stringp tool-error)
            (let ((line (%evolve-first-line tool-error)))
              (subseq line 0 (min 100 (length line))))))
        (let ((thrown (%opt-present (jget record "error"))))
          (when (stringp thrown) (%evolve-error-signature thrown)))
        (let ((log (%opt-present (jget prediction "actionLog"))))
          (when (stringp log)
            (multiple-value-bind (whole groups)
                (cl-ppcre:scan-to-strings "(?m)^\\s*(\\w+Error:\\s*.{0,60})" log)
              (declare (ignore whole))
              (when (and groups (plusp (length groups)) (stringp (aref groups 0)))
                (%evolve-error-signature (aref groups 0))))))
        +evolve-behavioral-cluster-signature+)))

(defun %evolve-failure-record-p (record threshold)
  "Whether RECORD is a failure worth mining.

A run that threw, a run that stopped to ask for clarification, and a run
that scored below THRESHOLD are all failures: the first two produced no
answer at all, and asking for clarification when the task was answerable is
the failure mode the playbook exists to correct."
  (let ((prediction (jget record "prediction")))
    (or (stringp (%opt-present (jget record "error")))
        (equal (%opt-present (jget prediction "completionType")) "askClarification")
        (< (%evolve-number (jget record "score")) threshold))))

(defun %evolve-cluster-failures (records threshold max-clusters)
  "RECORDS' failures grouped by signature, worst first, capped at MAX-CLUSTERS.

Severity is the cluster's size times its mean miss, so a signature that
fails often and badly outranks one that failed once and nearly passed.
Clusters past the cap are not mined this round rather than mined cheaply.
Returns a list of (signature records task-ids severity)."
  (let ((groups (make-hash-table :test #'equal))
        (order '())
        (index -1))
    (dolist (record (%opt-list records))
      (incf index)
      (when (%evolve-failure-record-p record threshold)
        (let* ((signature (%evolve-record-signature record))
               (entry (gethash signature groups)))
          (unless entry
            (setf entry (list '() '())
                  (gethash signature groups) entry)
            (push signature order))
          (push record (first entry))
          (push (let ((id (%opt-present (jget (jget record "task") "id"))))
                  (if (stringp id) id (format nil "task-~a" index)))
                (second entry)))))
    (let ((clusters '()))
      (dolist (signature (nreverse order))
        (let* ((entry (gethash signature groups))
               (cluster-records (nreverse (first entry)))
               (ids (nreverse (second entry)))
               (miss (/ (reduce #'+ cluster-records
                                :key (lambda (record)
                                       (- 1 (%evolve-number (jget record "score"))))
                                :initial-value 0)
                        (length cluster-records))))
          (push (list signature cluster-records ids
                      (coerce (* (length cluster-records) miss) 'double-float))
                clusters)))
      (let ((sorted (stable-sort (nreverse clusters) #'> :key #'fourth)))
        (subseq sorted 0 (min (max 0 max-clusters) (length sorted)))))))

(defun %evolve-coerce-array (value)
  "VALUE as a list of entries.

The structured-JSON extraction path passes a scalar through unchanged, so
an array-typed output can arrive as one value; wrapping it keeps the single
quote a model produced instead of discarding the only evidence it gave."
  (cond ((%opt-array-p value) (%opt-list value))
        ((or (null value) (eq value :null)) '())
        (t (list value))))

(defun %evolve-collapse (text)
  "TEXT with every run of whitespace reduced to one space."
  (string-trim " " (cl-ppcre:regex-replace-all "\\s+" (%evolve-text text) " ")))

(defun %evolve-verify-evidence-quotes (quotes excerpts)
  "The QUOTES that really appear in EXCERPTS.

Whitespace-insensitive because a model reflows what it copies, but
otherwise literal: a quote the excerpts do not contain is a quote the model
invented, and a weakness grounded only in invented quotes is discarded by
the caller rather than curated into the playbook."
  (let ((haystack (%evolve-collapse excerpts)))
    (remove-if-not (lambda (quote)
                     (let ((needle (%evolve-collapse quote)))
                       (and (plusp (length needle))
                            (search needle haystack))))
                   (mapcar #'%evolve-text quotes))))

(defstruct (evolve-budget (:constructor %make-evolve-budget (remaining)))
  "The run budget an evolve round spends, shared across all its batches."
  (remaining 0))

(defun %evolve-run-batch (driver client tasks metric threshold budget runs-per-task options)
  "Run TASKS through DRIVER's program and return (values records mean exhausted).

One budget unit per run, taken before the run, so a run that threw still
costs what it spent.  A task whose first run never started is dropped
rather than recorded as a zero: a task that did not run is not evidence
that it fails, and recording it would let an exhausted budget invent a
weakness out of tasks nobody tried.

Scoring is the optimizer's existing Core-owned path, so a dataset that
carries its own score decides the ranking here exactly as it does for every
other optimizer entry point, and a task naming expected or forbidden
actions is refused against a program that cannot report its calls."
  (let ((records '())
        (exhausted nil)
        (program (ace-program driver)))
    (block batch
      (dolist (task (%opt-list tasks))
        (let ((scores '())
              (prediction nil)
              (last-error nil))
          (dotimes (run runs-per-task)
            (when (<= (evolve-budget-remaining budget) 0)
              (setf exhausted t)
              (return))
            (decf (evolve-budget-remaining budget))
            (handler-case
                (let ((answer (program-evaluate-task
                               program client task
                               :options (%opt-clone (ace-forward-options driver)))))
                  (push (%evolve-number
                         (nth-value 1 (%opt-score-prediction task answer options metric program)))
                        scores)
                  (setf prediction answer))
              (error (condition)
                (push 0 scores)
                (setf last-error (princ-to-string condition)))))
          (when (null scores) (return-from batch))
          (let* ((scores (nreverse scores))
                 (mean (/ (reduce #'+ scores) (length scores)))
                 (record (object "task" task
                                 "score" mean
                                 "passed" (if (and (>= mean threshold)
                                                   prediction
                                                   (equal (%opt-present
                                                           (jget prediction "completionType"))
                                                          "final"))
                                              true false))))
            (when prediction (%set-key record "prediction" prediction))
            ;; A run can fail two ways: by throwing, or by completing with an
            ;; error prediction.  They are the same fact, so they have to
            ;; become the same record, or one failure clusters by its message
            ;; and the other lands in the behavioral bucket and the two never
            ;; meet.  The excerpt the miner reads is built from this key.
            (let ((reported (and prediction
                                 (equal (%opt-present (jget prediction "completionType")) "error")
                                 ;; Core nests it as error.message and older
                                 ;; shapes carry it flat; both are the same
                                 ;; fact, so both are read rather than one
                                 ;; being assumed.
                                 (or (%opt-present (jget (jget prediction "error") "message"))
                                     (%opt-present (jget prediction "message"))))))
              (cond ((and last-error (not prediction)) (%set-key record "error" last-error))
                    ((stringp reported) (%set-key record "error" reported))))
            (push record records))
          (when exhausted (return-from batch)))))
    (let ((records (nreverse records))
          (weight-sum 0)
          (score-sum 0))
      (dolist (record records)
        (let ((weight (%evolve-number (jget (jget record "task") "weight") 1)))
          (incf weight-sum weight)
          (incf score-sum (* weight (%evolve-number (jget record "score"))))))
      (values records
              (if (plusp weight-sum) (coerce (/ score-sum weight-sum) 'double-float) 0)
              exhausted))))

(defun %evolve-mine-weakness (driver cluster index teacher teacher-options)
  "Mine one weakness from CLUSTER, or NIL when nothing was grounded.

The miner's inputs are Core's, not this port's: the task summaries, the
excerpt windows and the call digest all come from
AGENT-PLAYBOOK-MINER-INPUTS, so the prompt a Lisp host sends is the prompt
every other port sends."
  (destructuring-bind (signature records task-ids severity) cluster
    (declare (ignore severity))
    (let* ((rendered (ace-render driver))
           (inputs (axllm/core::agent-playbook-miner-inputs
                    signature
                    (%opt-array records)
                    (if (and (stringp rendered) (plusp (length (string-trim '(#\Space #\Newline #\Tab) rendered))))
                        rendered
                        :null)))
           (excerpts (%evolve-text (%opt-present (jget inputs "actionLogExcerpts"))))
           ;; Core answers :NULL for a cluster it cannot describe, such as one
           ;; whose runs threw before producing any action log.  That is an
           ;; answer, not a fault: there is nothing for a miner to read, so the
           ;; cluster is skipped here rather than sent to the model as a
           ;; malformed request whose error would be reported as a miner
           ;; failure and hide the real reason.
           (mined (when (%opt-object-p inputs)
                    (forward (ax (evolve-miner-signature))
                             teacher inputs (%opt-clone teacher-options))))
           (quotes (when mined
                     (%evolve-verify-evidence-quotes
                      (%evolve-coerce-array (jget mined "evidenceQuotes"))
                      excerpts))))
      (when quotes
        (object "id" (format nil "weakness-~a" (1+ index))
                "clusterSignature" signature
                "description" (%evolve-text (%opt-present (jget mined "weaknessDescription")))
                "rootCause" (%evolve-text (%opt-present (jget mined "rootCause")))
                "proposedGuidance" (%evolve-text (%opt-present (jget mined "proposedGuidance")))
                "evidenceQuotes" (%opt-array quotes)
                "taskIds" (%opt-array task-ids)
                "configRecommendations"
                (%opt-array (mapcar #'%evolve-text
                                    (%evolve-coerce-array
                                     (jget mined "configRecommendations")))))))))

(defun %evolve-build-proposal (weakness)
  "The curator feedback WEAKNESS becomes.

One rule per weakness, carrying the grounding quotes so the curator can see
what the rule is for, and naming the section so a port that allows dynamic
sections still lands the bullet where every other port lands it."
  (let ((quotes (format nil "~{- ~a~^~%~}"
                        (let ((all (%opt-list (jget weakness "evidenceQuotes"))))
                          (subseq all 0 (min +evolve-evidence-quote-limit+ (length all)))))))
    (object "weaknessId" (jget weakness "id")
            "clusterSignature" (jget weakness "clusterSignature")
            "feedback"
            (format nil
                    "A recurring agent weakness was diagnosed from real failed runs.~2%~
                     Weakness: ~a~%Root cause: ~a~%Error signature: [~a]~%Grounding excerpts:~%~a~2%~
                     Curate ONE durable rule into the playbook (suggested section: \"~a\"): ~a~%~
                     UPDATE an existing bullet if one already covers this failure mode."
                    (%evolve-text (jget weakness "description"))
                    (%evolve-text (jget weakness "rootCause"))
                    (%evolve-text (jget weakness "clusterSignature"))
                    quotes
                    +evolve-playbook-failure-section+
                    (%evolve-text (jget weakness "proposedGuidance"))))))

(defun %evolve-apply-proposal (driver proposal)
  "Apply PROPOSAL to DRIVER's playbook and return the snapshot that undoes it.

The snapshot is taken before the update rather than reconstructed after it,
so a rejected proposal is undone exactly, including the feedback and delta
history the update appended."
  (let ((snapshot (playbook-state driver)))
    (ace-apply-online-update
     driver
     (object "example" (object "task" "playbook.evolve(): repair a diagnosed agent weakness"
                               "failureSignatures"
                               (%opt-array (list (jget proposal "clusterSignature"))))
             "prediction" (%new-object)
             "feedback" (jget proposal "feedback")))
    snapshot))

(defun playbook-evolve (playbook examples &key metric options)
  "Run PLAYBOOK over EXAMPLES and return what it learned.

The same result ACE-COMPILE returns: the final playbook, the artifact, the
best metric score seen, and the configuration the run used.

OPTIONS is this call's own configuration, layered over the playbook's.
Three keys change how the run executes rather than only what it records:

  teacherAI   the client the Reflector and Curator run against for this
              call.  The roles are rebound, so a run that names another
              teacher really uses it instead of scoring the client the
              playbook was built with.
  runtime     reaches the generator as a forward option, for a program that
              holds only a runtime descriptor and is handed the real
              runtime on the evolve call.
  maxEpochs   and the other driver configuration keys, applied for this run.

The overrides last for the call: the playbook is left configured as it was,
so two evolve calls with different options do not contaminate each other."
  (let* ((options (if (%opt-object-p options) (%opt-clone options) (%new-object)))
         (previous-config (%opt-clone (ace-config playbook)))
         (previous-teacher (ace-teacher playbook))
         (previous-forward (%opt-clone (ace-forward-options playbook)))
         (teacher (%opt-present (%opt-option options '("teacherAI" "teacher_ai" "teacher"))))
         (runtime (%opt-present (jget options "runtime")))
         (rebind nil))
    (unwind-protect
         (progn
           (dolist (key +playbook-evolve-config-keys+)
             (let ((value (%opt-present (jget options key))))
               (when value (%set-key (ace-config playbook) key value))))
           (when teacher
             (setf (ace-teacher playbook) teacher rebind t))
           (when runtime
             (%set-key (ace-forward-options playbook) "runtime" runtime))
           (when (or rebind runtime)
             (%playbook-bind-roles playbook (%opt-merge (ace-options playbook) options)))
           (ace-compile playbook (%playbook-examples examples) :metric metric))
      (setf (ace-config playbook) previous-config
            (ace-teacher playbook) previous-teacher
            (ace-forward-options playbook) previous-forward)
      (when (or rebind runtime)
        (%playbook-bind-roles playbook (ace-options playbook))))))

(defun %evolve-option (options names &optional default)
  (let ((value (%opt-present (%opt-option options names))))
    (if value value default)))

(defun %evolve-positive-int (value default)
  (let ((given (if (realp value) (floor value) default)))
    (max 1 given)))

(defun playbook-evolve-agent (playbook dataset &key metric options)
  "Grow PLAYBOOK from DATASET by measured rounds and report what each decided.

The agent-level surface, and the one the agent_playbook_evolve fixtures
drive.  PLAYBOOK-EVOLVE is the program-level surface: it grows a playbook
from labelled examples and returns the playbook it reached.  This one grows
an agent's playbook from a task set, and every rule it keeps has been
measured against the runs it was supposed to improve.  They are separate
because the evidence is: an example carries its own answer, a task only
carries what the agent did with it.

A round is: measure, diagnose, propose, re-measure, keep or undo.  The
baseline runs every training task and records what each one did.  The runs
that failed are clustered by error signature and the worst clusters mined
for one weakness each.  Every weakness becomes one curated playbook rule,
applied and then re-measured; a rule that does not pay for itself is rolled
back exactly, so a round can only leave the playbook better than it found
it or exactly as it was.

The Reflector and Curator run only inside a proposal's update.  They never
see the dataset directly, because a playbook is judged on the runs it
changed, not on the examples it was shown.

OPTIONS is this call's own configuration layered over the playbook's:

  verify         re-measure before keeping a rule.  Default true; false is
                 the trust-batch, which keeps every mined rule unmeasured.
  minHeldInGain  the held-in gain a rule must earn to be kept.
  maxProposals   how many clusters are mined this round.
  maxMetricCalls the run budget for the whole round.
  runsPerTask    runs averaged into one record, for a flaky program.
  epsilon        how far held-out may slip before a rule counts as a regression.
  scoreThreshold at or above which a run passed.
  apply          false undoes every accepted rule before returning, for a
                 caller that wants the diagnosis without the change.
  teacherAI      the client the miner, Reflector and Curator run against.
  teacherOptions the forward options those teacher calls carry.
  runtime        reaches the program as a forward option, for a program
                 that holds only a runtime descriptor.

The overrides last for the call: the playbook is left configured as it was."
  (let* ((options (if (%opt-object-p options) (%opt-clone options) (%new-object)))
         (normalized (axllm/core::normalize-optimization-dataset
                      (if (eq dataset :null) (%new-array) dataset)))
         (train (%opt-array (%opt-list (jget normalized "train"))))
         (validation (%opt-array (%opt-list (jget normalized "validation"))))
         (previous-config (%opt-clone (ace-config playbook)))
         (previous-teacher (ace-teacher playbook))
         (previous-forward (%opt-clone (ace-forward-options playbook)))
         (runtime (%evolve-option options '("runtime"))))
    (when (zerop (%opt-count train))
      (optimize-fail :config "playbook-evolve: at least one training task is required."))
    (unwind-protect
         (progn
           (dolist (key +playbook-evolve-config-keys+)
             (let ((value (%opt-present (jget options key))))
               (when value (%set-key (ace-config playbook) key value))))
           ;; The call's teacher reaches the miner and stops there.  The
           ;; Reflector and Curator keep the client and the options the
           ;; playbook was built with, because the handle is the agent's and
           ;; evolve is only borrowing it: a round that names a teacher for
           ;; its own diagnosis must not silently re-point the roles that
           ;; curate the playbook, which a caller configured separately and
           ;; may deliberately have pointed somewhere else.
           (when runtime
             (%set-key (ace-forward-options playbook) "runtime" runtime))
           (%playbook-evolve-round playbook train validation metric options))
      (setf (ace-config playbook) previous-config
            (ace-teacher playbook) previous-teacher
            (ace-forward-options playbook) previous-forward))))

(defun %playbook-evolve-round (playbook train validation metric options)
  "One evolve round over TRAIN and VALIDATION.  See PLAYBOOK-EVOLVE."
  (let* ((client (or (%evolve-option options '("studentAI" "student_ai")) (ace-student playbook)))
         (teacher (or (%evolve-option options '("teacherAI" "teacher_ai" "teacher"))
                      (ace-teacher playbook)
                      client))
         (teacher-options (let ((given (%evolve-option options '("teacherOptions" "teacher_options"))))
                            (if (%opt-object-p given) given (%new-object))))
         (verify (not (eq (jget options "verify") false)))
         (apply-accepted (not (eq (jget options "apply") false)))
         (max-proposals (%evolve-positive-int (jget options "maxProposals")
                                              +evolve-default-max-proposals+))
         (runs-per-task (%evolve-positive-int (jget options "runsPerTask") 1))
         (dataset-size (* (+ (%opt-count train) (%opt-count validation)) runs-per-task))
         (max-metric-calls (%evolve-positive-int
                            (jget options "maxMetricCalls")
                            (max +optimize-default-max-metric-calls+
                                 (* (1+ max-proposals) dataset-size))))
         (epsilon (%evolve-number (jget options "epsilon") +evolve-default-epsilon+))
         (min-gain (%evolve-number (jget options "minHeldInGain") +evolve-default-min-held-in-gain+))
         (threshold (%evolve-number (jget options "scoreThreshold") +evolve-default-score-threshold+))
         (budget (%make-evolve-budget max-metric-calls))
         (outcomes '())
         (accepted '())
         (weaknesses '()))
    (unless client
      (optimize-fail :config "playbook-evolve: a student client is required to run a task."))
    (flet ((spent () (- max-metric-calls (evolve-budget-remaining budget)))
           (batch (tasks)
             (%evolve-run-batch playbook client tasks metric threshold budget
                                runs-per-task options)))
      (multiple-value-bind (records held-in) (batch train)
        (let* ((held-out (when (plusp (%opt-count validation))
                           (nth-value 1 (batch validation))))
               (baseline-records records)
               (baseline-held-in held-in)
               (baseline-held-out held-out))
          ;; ---- diagnose ----
          (let ((index -1))
            (dolist (cluster (%evolve-cluster-failures records threshold max-proposals))
              (incf index)
              (let ((weakness (handler-case
                                  (%evolve-mine-weakness playbook cluster index
                                                         teacher teacher-options)
                                ;; A miner that fails costs this round one
                                ;; cluster, never the round itself.
                                (error () nil))))
                (when weakness (push weakness weaknesses)))))
          (setf weaknesses (nreverse weaknesses))
          ;; ---- propose, verify, keep or undo ----
          (dolist (weakness weaknesses)
            (let ((proposal (%evolve-build-proposal weakness))
                  (required (* (+ (%opt-count train) (%opt-count validation)) runs-per-task)))
              (cond
                ((and verify (< (evolve-budget-remaining budget) required))
                 (push (object "proposal" proposal "accepted" false
                               "reason" "metric_budget exhausted before validation"
                               "heldIn" (object "before" held-in "after" held-in))
                       outcomes))
                (t
                 (let ((snapshot (handler-case (%evolve-apply-proposal playbook proposal)
                                   (error (condition)
                                     (push (object "proposal" proposal "accepted" false
                                                   "reason" (format nil "apply failed: ~a" condition)
                                                   "heldIn" (object "before" held-in "after" held-in))
                                           outcomes)
                                     :failed))))
                   (unless (eq snapshot :failed)
                     (if (not verify)
                         (progn
                           (push snapshot accepted)
                           (push (object "proposal" proposal "accepted" true
                                         "reason" "applied without verification (verify: false)"
                                         "heldIn" (object "before" held-in "after" held-in))
                                 outcomes))
                         (multiple-value-bind (reval-records reval-held-in reval-exhausted)
                             (batch train)
                           (declare (ignore reval-records))
                           (let* ((reval-held-out nil)
                                  (held-out-exhausted nil))
                             (when (plusp (%opt-count validation))
                               (multiple-value-bind (r m e) (batch validation)
                                 (declare (ignore r))
                                 (setf reval-held-out m held-out-exhausted e)))
                             ;; A re-evaluation that ran out mid-way produced a
                             ;; mean over a subset; comparing it with a
                             ;; full-set baseline would accept or reject on an
                             ;; arithmetic artefact, so it is refused instead.
                             (let* ((complete (not (or reval-exhausted held-out-exhausted)))
                                    (gain-ok (and complete (>= (- reval-held-in held-in) min-gain)))
                                    (held-out-ok (or (null reval-held-out) (null held-out)
                                                     (>= (- reval-held-out held-out) (- epsilon))))
                                    (accept (and complete gain-ok held-out-ok))
                                    (outcome (object "proposal" proposal
                                                     "accepted" (if accept true false)
                                                     "reason"
                                                     (cond
                                                       ((not complete)
                                                        "metric_budget exhausted during re-evaluation")
                                                       (accept
                                                        (if (null held-out)
                                                            (format nil "held-in improved (no held-out set provided ~c consider one)"
                                                                    (code-char #x2014))
                                                            "held-in improved, held-out non-regressing"))
                                                       ((not gain-ok)
                                                        (format nil "held-in gain ~a below ~a"
                                                                (%evolve-fixed (- reval-held-in held-in))
                                                                (axllm/core::core-js-number-text min-gain)))
                                                       (t (format nil "held-out regressed ~a"
                                                                  (%evolve-fixed
                                                                   (- (or reval-held-out 0)
                                                                      (or held-out 0))))))
                                                     "heldIn" (object "before" held-in
                                                                      "after" reval-held-in))))
                               (when (and reval-held-out held-out)
                                 (%set-key outcome "heldOut"
                                           (object "before" held-out "after" reval-held-out)))
                               (push outcome outcomes)
                               (if accept
                                   (progn (push snapshot accepted)
                                          (setf held-in reval-held-in)
                                          (when reval-held-out (setf held-out reval-held-out)))
                                   (playbook-load playbook snapshot)))))))))))) 
          (setf outcomes (nreverse outcomes))
          ;; ---- finalize ----
          (let ((snapshot (when accepted (playbook-state playbook))))
            (unless apply-accepted
              ;; Undo newest first so each load restores the state its own
              ;; proposal replaced.
              (let ((oldest (car (last accepted))))
                (when oldest (playbook-load playbook oldest))))
            (let ((result (object "baseline" (let ((b (object "heldIn" baseline-held-in)))
                                                (when baseline-held-out
                                                  (%set-key b "heldOut" baseline-held-out))
                                                b)
                                  "final" (let ((f (object "heldIn" held-in)))
                                            (when held-out (%set-key f "heldOut" held-out))
                                            f)
                                  "weaknesses" (%opt-array weaknesses)
                                  "outcomes" (%opt-array outcomes)
                                  "recommendations"
                                  (%opt-array (loop for weakness in weaknesses
                                                    append (%opt-list
                                                            (jget weakness "configRecommendations"))))
                                  "metricCallsUsed" (spent)
                                  "records" (%opt-array baseline-records))))
              (when snapshot (%set-key result "playbookSnapshot" snapshot))
              result)))))))

(defun %evolve-fixed (value)
  "VALUE to three decimal places, the way every port reports a gain."
  (format nil "~,3F" (coerce value 'double-float)))

(defun playbook-state (playbook)
  "PLAYBOOK's serializable state: its playbook and its artifact.

The snapshot a caller compares before and after a round, and the exact undo
a rejected proposal is restored from."
  (object "playbook" (ace-playbook playbook)
          "artifact" (ace-artifact playbook)))

(defun playbook-json (playbook)
  "PLAYBOOK's state as JSON text.

Text rather than a structure because two structures have to be walked to be
compared and two strings do not; PLAYBOOK-STATE returns the same state as a
JSON value when a caller wants to read it."
  (encode-json (playbook-state playbook)))

