;;;; synth.lisp --- tests for synthetic example generation.
;;;;
;;;; Entry point for the repository runner:
;;;;
;;;;   (axllm:run-synth-tests)           ; => (values passed failed)
;;;;
;;;; Covers src/synth.lisp. Load tests/refine.lisp first: this file reuses
;;;; its SCRIPTED-PROGRAM and REFINE-EXPECT harness.
;;;;
;;;; Every request goes through a scripted program, so the tests show what
;;;; synthesis actually sends and how many times: one generation request
;;;; per batch, one labelling request per surviving input, the teacher as
;;;; the client in both, and no paid call anywhere. The assertions are
;;;; asymmetric against the obvious shortcuts: fabricating examples
;;;; locally, labelling without the teacher, counting an unlabelled input
;;;; as generated, or reporting a success rate that ignores failures all
;;;; fail here.

(in-package #:axllm)

(export '(run-synth-tests))

(defvar *synth-tests* '())

(defmacro define-synth-test (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *synth-tests*
           (append (remove ',name *synth-tests* :key #'car) (list (cons ',name #',name))))
     ',name))

(defun run-synth-tests ()
  (run-refine-test-list *synth-tests* "synth"))

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(defstruct (synth-harness (:conc-name sh-))
  "Scripts handed out by the synthesis generator factory.

GENERATION-SCRIPTS is consumed by the input and edge case generators, in
call order; LABEL-SCRIPTS by the teacher labellers. Each element is a
SCRIPTED-PROGRAM script. An empty script makes the program signal, which
is how a failing labelling is scripted."
  (generation-scripts '())
  (label-scripts '())
  (generators '())
  (labelers '()))

(defun synth-harness-factory (harness)
  (lambda (signature)
    (if (stringp signature)
        (let ((program (make-instance 'scripted-program
                                      :native nil
                                      :script (pop (sh-generation-scripts harness)))))
          (refine-expect-equal signature +synth-input-signature+
                               "input generation uses the count/examples signature")
          (push program (sh-generators harness))
          program)
        (let ((program (make-instance 'scripted-program
                                      :native nil
                                      :script (pop (sh-label-scripts harness)))))
          (refine-expect (equal (axllm/core::core-record-kind signature) "AxSignature")
                         "labelling uses the signature itself")
          (push program (sh-labelers harness))
          program))))

(defun sh-generator (harness index) (nth index (reverse (sh-generators harness))))
(defun sh-labeler (harness index) (nth index (reverse (sh-labelers harness))))
(defun sh-generator-count (harness) (length (sh-generators harness)))
(defun sh-labeler-count (harness) (length (sh-labelers harness)))

(defun raw-script (&rest entries)
  "A script of ENTRIES, each a SCRIPTED-PROGRAM script entry."
  entries)

(defun examples-script (&rest inputs)
  "A one-call script returning INPUTS as the generated examples array."
  (raw-script (list :outputs (object "examples" (coerce inputs 'vector)))))

(defun label-script (outputs &key usage)
  "A one-call script returning OUTPUTS as the teacher's label."
  (raw-script (list :outputs outputs :usage (or usage (usage-object 1 1)))))

(defparameter +synth-signature-text+
  "question:string \"a user question\", hint?:string -> answer:string, sentiment:class \"positive, negative\"")

(defun make-test-synth (&rest options)
  (apply #'synth +synth-signature-text+
         :teacher (object "name" "teacher-client")
         options))

;;; ------------------------------------------------------------------
;;; Generation and labelling
;;; ------------------------------------------------------------------

(define-synth-test test-synth-generates-then-labels-with-the-teacher
  (let* ((harness (make-synth-harness
                   :generation-scripts
                   (list (raw-script (list :outputs (object "examples"
                                                            (vector (object "question" "one")
                                                                    (object "question" "two")))
                                           :usage (usage-object 30 10))))
                   :label-scripts
                   (list (label-script (object "answer" "first" "sentiment" "positive")
                                       :usage (usage-object 4 2))
                         (label-script (object "answer" "second" "sentiment" "negative")
                                       :usage (usage-object 4 2)))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth :domain "customer support")))
    (multiple-value-bind (examples stats) (synth-generate synth 2)
      (refine-expect-equal (sh-generator-count harness) 1 "one generation request for one batch")
      (refine-expect-equal (sh-labeler-count harness) 2 "one labelling request per input")
      (refine-expect-equal (length examples) 2 "both inputs became examples")
      (let ((first-example (aref examples 0)))
        (refine-expect-equal (jget (jget first-example "input") "question") "one"
                             "the example keeps the generated input")
        (refine-expect-equal (jget (jget first-example "expected") "answer") "first"
                             "the teacher's output is the expected output")
        (refine-expect-equal (jget first-example "category") "normal"
                             "a plain example is categorised normal"))
      ;; The teacher is the client for both halves; nothing is produced locally.
      (refine-expect-equal (jget (first (scripted-call (sh-generator harness 0) 0)) "name")
                           "teacher-client" "generation calls the teacher")
      (refine-expect-equal (jget (first (scripted-call (sh-labeler harness 1) 0)) "name")
                           "teacher-client" "labelling calls the teacher")
      (refine-expect-equal (jget (second (scripted-call (sh-generator harness 0) 0)) "count") 2
                           "the generator is asked for the batch size")
      (refine-expect-equal (jget (second (scripted-call (sh-labeler harness 1) 0)) "question") "two"
                           "each generated input is labelled as it stands")
      (refine-expect-equal (jget stats "requested") 2 "stats report the requested count")
      (refine-expect-equal (jget stats "generated") 2 "stats report the produced count")
      (refine-expect-equal (jget stats "labelingSuccessRate") 1.0d0 "every labelling succeeded")
      (refine-expect (realp (jget stats "durationMs")) "stats report a duration")
      (let ((usage (jget stats "usage")))
        (refine-expect-equal (jget usage "promptTokens") 38
                             "usage sums generation and every labelling call")
        (refine-expect-equal (jget usage "completionTokens") 14
                             "completion tokens are summed too")))))

(define-synth-test test-synth-instruction-describes-the-signature-and-domain
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (examples-script (object "question" "one")))
                   :label-scripts (list (label-script (object "answer" "a")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth :domain "customer support")))
    (synth-generate synth 1)
    (let ((instruction (scripted-instruction (sh-generator harness 0))))
      (refine-expect-contains instruction "Domain: customer support" "the domain is included")
      (refine-expect-contains instruction "- question: string: a user question"
                              "input fields are described with their descriptions")
      (refine-expect-contains instruction "- hint: string (optional)"
                              "an optional input field is marked optional")
      (refine-expect-contains instruction "- sentiment: class (options: positive, negative)"
                              "output field options are described for labelling context")
      (refine-expect-contains instruction "Generate 1 diverse, realistic input examples"
                              "the instruction asks for the batch size")
      (refine-expect (null (search "edge case" instruction))
                     "a plain batch is not an edge case batch"))))

(define-synth-test test-synth-batches-requests-and-truncates-overlong-batches
  (let* ((harness (make-synth-harness
                   :generation-scripts
                   (list (examples-script (object "question" "a") (object "question" "b")
                                          (object "question" "extra"))
                         (examples-script (object "question" "c") (object "question" "d"))
                         (examples-script (object "question" "e")))
                   :label-scripts (loop repeat 5
                                        collect (label-script (object "answer" "x")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth)))
    (multiple-value-bind (examples stats) (synth-generate synth 5 :batch-size 2)
      (refine-expect-equal (sh-generator-count harness) 3
                           "five examples in batches of two is three requests")
      (refine-expect-equal (loop for index below 3
                                 collect (jget (second (scripted-call (sh-generator harness index) 0))
                                               "count"))
                           '(2 2 1) "the final batch asks only for the remainder")
      (refine-expect-equal (length examples) 5
                           "an over-long batch is truncated to the batch size")
      (refine-expect-equal (sh-labeler-count harness) 5 "only the kept inputs are labelled")
      (refine-expect-equal (jget stats "generated") 5 "stats count the kept examples"))))

(define-synth-test test-synth-adds-edge-cases-when-hints-are-given
  (let* ((harness (make-synth-harness
                   :generation-scripts
                   (list (examples-script (object "question" "a") (object "question" "b")
                                          (object "question" "c") (object "question" "d")
                                          (object "question" "e"))
                         (examples-script (object "question" "")))
                   :label-scripts (loop repeat 6
                                        collect (label-script (object "answer" "x")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth :edge-cases '("empty inputs" "very long queries"))))
    (multiple-value-bind (examples stats) (synth-generate synth 5)
      (refine-expect-equal (sh-generator-count harness) 2
                           "edge cases are a second generation request")
      (refine-expect-equal (jget (second (scripted-call (sh-generator harness 1) 0)) "count") 1
                           "a fifth as many edge cases are requested")
      (refine-expect-equal (length examples) 6 "the edge case example is kept too")
      (refine-expect-equal (jget (aref examples 5) "category") "edge_case"
                           "an edge case example is categorised edge_case")
      (refine-expect-equal (jget stats "generated") 6 "stats count every example")
      (let ((instruction (scripted-instruction (sh-generator harness 1))))
        (refine-expect-contains instruction "challenging edge case input data"
                                "the edge case instruction says so")
        (refine-expect-contains instruction "- empty inputs" "every hint is listed")
        (refine-expect-contains instruction "- very long queries" "every hint is listed")
        (refine-expect (null (search "OUTPUT fields" instruction))
                       "the edge case prompt describes only the input fields")))))

(define-synth-test test-synth-skips-no-edge-cases-without-hints
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (examples-script (object "question" "a")))
                   :label-scripts (list (label-script (object "answer" "x")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth)))
    (synth-generate synth 1)
    (refine-expect-equal (sh-generator-count harness) 1
                         "no hints means no edge case request")))

;;; ------------------------------------------------------------------
;;; Validity and failure accounting
;;; ------------------------------------------------------------------

(define-synth-test test-an-unlabelled-input-is-not-an-example
  (let* ((harness (make-synth-harness
                   :generation-scripts
                   (list (examples-script (object "question" "a") (object "question" "b")
                                          (object "question" "c")))
                   ;; The second labelling has an empty script, so it signals.
                   :label-scripts (list (label-script (object "answer" "first"))
                                        '()
                                        (label-script (object "answer" "third")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth))
         (warnings 0))
    (multiple-value-bind (examples stats)
        (handler-bind ((warning (lambda (condition)
                                  (declare (ignore condition))
                                  (incf warnings)
                                  (muffle-warning))))
          (synth-generate synth 3))
      (refine-expect-equal warnings 1 "the failed labelling is reported once")
      (refine-expect-equal (length examples) 2 "the unlabelled input is left out")
      (refine-expect-equal (map 'list (lambda (e) (jget (jget e "input") "question")) examples)
                           '("a" "c") "the surviving examples keep their own inputs")
      (refine-expect-equal (jget stats "generated") 2 "generated counts examples, not inputs")
      (refine-expect-equal (jget stats "labelingSuccessRate")
                           (coerce 2/3 'double-float)
                           "the success rate counts every labelling attempt"))))

(define-synth-test test-a-generated-entry-that-is-not-an-object-is-rejected
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (examples-script "just a string"
                                                              (object "question" "b")))
                   :label-scripts (list (label-script (object "answer" "b")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth))
         (warnings 0))
    (multiple-value-bind (examples stats)
        (handler-bind ((warning (lambda (condition)
                                  (declare (ignore condition))
                                  (incf warnings)
                                  (muffle-warning))))
          (synth-generate synth 2))
      (refine-expect-equal warnings 1 "the invalid entry is reported")
      (refine-expect-equal (length examples) 1 "only the object entry became an example")
      (refine-expect-equal (sh-labeler-count harness) 1
                           "an invalid entry is never sent to the teacher")
      (refine-expect-equal (jget stats "labelingSuccessRate") 0.5d0
                           "the invalid entry still counts as an attempt"))))

(define-synth-test test-a-teacher-output-that-is-not-an-object-is-rejected
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (examples-script (object "question" "a")))
                   :label-scripts (list (raw-script (list :outputs "not an object")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth)))
    (multiple-value-bind (examples stats)
        (without-test-warnings (synth-generate synth 1))
      (refine-expect-equal (length examples) 0 "an unusable label produces no example")
      (refine-expect-equal (jget stats "labelingSuccessRate") 0.0d0
                           "nothing was labelled successfully"))))

(define-synth-test test-a-generator-that-returns-no-array-produces-nothing
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (raw-script
                                              (list :outputs (object "examples" "nope"))))
                   :label-scripts '()))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth)))
    (multiple-value-bind (examples stats)
        (without-test-warnings (synth-generate synth 3))
      (refine-expect-equal (length examples) 0 "no inputs means no examples")
      (refine-expect-equal (sh-labeler-count harness) 0 "nothing is labelled")
      (refine-expect-equal (jget stats "requested") 3 "the request is still reported")
      (refine-expect-equal (jget stats "labelingSuccessRate") 0 "no attempt means a zero rate"))))

(define-synth-test test-a-failing-generation-request-does-not-end-the-run
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (raw-script (list :error "generation refused"))
                                             (examples-script (object "question" "b")))
                   :label-scripts (list (label-script (object "answer" "b")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth)))
    (multiple-value-bind (examples stats)
        (without-test-warnings (synth-generate synth 2 :batch-size 1))
      (refine-expect-equal (sh-generator-count harness) 2 "the second batch still runs")
      (refine-expect-equal (length examples) 1 "the surviving batch produces its example")
      (refine-expect-equal (jget stats "generated") 1 "stats report what was produced"))))

;;; ------------------------------------------------------------------
;;; Options
;;; ------------------------------------------------------------------

(define-synth-test test-synth-sends-the-model-and-temperature
  (let* ((harness (make-synth-harness
                   :generation-scripts (list (examples-script (object "question" "a")))
                   :label-scripts (list (label-script (object "answer" "x")))))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth :model "gpt-6-luna" :temperature 0.3d0)))
    (synth-generate synth 1)
    (let ((generation-options (scripted-call-options (sh-generator harness 0) 0))
          (label-options (scripted-call-options (sh-labeler harness 0) 0)))
      (refine-expect-equal (jget generation-options "model") "gpt-6-luna"
                           "the model is sent for generation")
      (refine-expect-equal (jget (jget generation-options "modelConfig") "temperature") 0.3d0
                           "the temperature is sent for generation")
      (refine-expect-equal (jget label-options "model") "gpt-6-luna"
                           "the model is sent for labelling")
      (refine-expect (null (jget label-options "modelConfig" nil))
                     "labelling does not raise the temperature"))))

(define-synth-test test-synth-validates-its-options
  (refine-expect-contains
   (ax-error-message (refine-expect-error ax-error (synth +synth-signature-text+)))
   "teacher is required" "a teacher is required")
  (refine-expect-error ax-error
    (synth +synth-signature-text+ :teacher (object "name" "t") :diversity :clustered))
  (refine-expect-error ax-error
    (synth +synth-signature-text+ :teacher (object "name" "t") :temperature "hot"))
  (refine-expect-error signature-error
    (synth "not a signature" :teacher (object "name" "t")))
  (let ((synth (synth +synth-signature-text+ :teacher (object "name" "t"))))
    (refine-expect-equal (synth-diversity synth) :none "diversity defaults to none")
    (refine-expect-equal (jget (synth-teacher synth) "name") "t" "the teacher is readable")
    (refine-expect-contains (signature-string (synth-signature synth)) "sentiment:class"
                            "the parsed signature is readable")
    (refine-expect-error ax-error (synth-generate synth "five"))))

(define-synth-test test-a-zero-count-request-calls-nothing
  (let* ((harness (make-synth-harness :generation-scripts '() :label-scripts '()))
         (*synth-generator-factory* (synth-harness-factory harness))
         (synth (make-test-synth)))
    (multiple-value-bind (examples stats) (synth-generate synth 0)
      (refine-expect-equal (sh-generator-count harness) 0 "nothing is requested")
      (refine-expect-equal (length examples) 0 "nothing is produced")
      (refine-expect-equal (jget stats "requested") 0 "the zero request is reported")
      (refine-expect-equal (jget stats "labelingSuccessRate") 0 "no attempt means a zero rate"))))

;;; ------------------------------------------------------------------
;;; Against the real generator
;;; ------------------------------------------------------------------

(define-synth-test test-synth-runs-the-real-generator-end-to-end
  ;; No generator factory override: synthesis builds real programs with AX
  ;; and runs them through the real FORWARD. Only the transport is scripted,
  ;; so the synthesis prompt, the JSON parsing of the generated inputs and
  ;; the labelling of each one are the generator's own work.
  (multiple-value-bind (teacher script)
      (refine-scripted-client
       (list (format nil "Examples: [{\"question\": \"How do I reset my password?\"}, {\"question\": \"Where is my order?\"}]")
             (format nil "Answer: Use the reset link.~%Sentiment: positive")
             (format nil "Answer: It shipped yesterday.~%Sentiment: negative")))
    (let ((synth (synth +synth-signature-text+ :teacher teacher :domain "customer support")))
      (multiple-value-bind (examples stats) (synth-generate synth 2)
        (refine-expect-equal (cs-request-count script) 3
                             "one generation request and one labelling request per input")
        (refine-expect-equal (length examples) 2 "both generated inputs were labelled")
        (refine-expect-equal (jget (jget (aref examples 0) "input") "question")
                             "How do I reset my password?"
                             "the input comes from the model's JSON, not from a local template")
        (refine-expect-equal (jget (jget (aref examples 0) "expected") "answer")
                             "Use the reset link." "the teacher's answer is the expected output")
        (refine-expect-equal (jget (jget (aref examples 1) "expected") "sentiment") "negative"
                             "a class output is parsed and kept")
        (refine-expect-contains (cs-request script 0) "Domain: customer support"
                                "the synthesis instruction really reaches the provider")
        (refine-expect-contains (cs-request script 1) "How do I reset my password?"
                                "the labelling request carries the generated input")
        (refine-expect-equal (jget stats "generated") 2 "stats report both examples")
        (refine-expect-equal (jget stats "labelingSuccessRate") 1.0d0 "both labellings succeeded")
        (refine-expect-equal (jget (jget stats "usage") "promptTokens") 15
                             "usage sums all three real calls")
        (refine-expect-equal (jget (jget stats "usage") "totalTokens") 21
                             "total tokens sum all three real calls")))))

(define-synth-test test-synth-drops-an-input-the-real-teacher-cannot-label
  ;; The teacher refuses the second input: the generator exhausts its
  ;; correction budget and signals, so that input must not become an
  ;; example, and the success rate must show it.
  (multiple-value-bind (teacher script)
      (refine-scripted-client
       (list "Examples: [{\"question\": \"one\"}, {\"question\": \"two\"}]"
             (format nil "Answer: fine~%Sentiment: positive")
             ;; Four refusals, not three.  The budget is the reference's rule,
             ;; maxRetries extra attempts after the first (generate.ts:2449)
             ;; with a default of 3 (generate.ts:1954), so a teacher that
             ;; never complies is asked four times.  The count below follows
             ;; from that rule and from the budget the generator really
             ;; applies, not from ax's declared default, which is stale at 2;
             ;; correcting that declaration must not move this number, and a
             ;; change in the effective budget should.
             "I will not answer" "I will not answer" "I will not answer"
             "I will not answer"))
    (let ((synth (synth +synth-signature-text+ :teacher teacher)))
      (multiple-value-bind (examples stats)
          (without-test-warnings (synth-generate synth 2))
        (refine-expect-equal (length examples) 1 "only the labelled input became an example")
        (refine-expect-equal (jget (jget (aref examples 0) "input") "question") "one"
                             "the surviving example is the labelled one")
        (refine-expect-equal (jget stats "labelingSuccessRate") 0.5d0
                             "the refused input counts as a failed attempt")
        (refine-expect-equal (cs-request-count script) 6
                             "the refused input used its initial call and all three corrections")))))
