;;;; synth.lisp --- synthetic example generation for a signature.
;;;;
;;;; Port of src/ax/dsp/synth.ts (AxSynth). Synthesis solves the cold start
;;;; problem: given only a signature, it asks a teacher client for diverse
;;;; realistic inputs, optionally for edge cases, and then labels every
;;;; input by running the signature itself against the same teacher.
;;;;
;;;; Both halves really call the client through FORWARD, so a scripted
;;;; client sees one generation request per batch and one labelling request
;;;; per surviving input. Nothing is fabricated locally: an input that the
;;;; generator did not return, or that the teacher cannot label, does not
;;;; become an example, and the labelling success rate reports it.
;;;;
;;;; Load after src/refine.lisp, which creates the frozen program generic
;;;; functions when gen.lisp has not yet done so.

(in-package #:axllm)

(defparameter +synth-input-signature+ "count:number -> examples:json")

(defvar *synth-generator-factory*
  (lambda (signature) (ax signature))
  "How synthesis builds a generator from a signature.

The default calls AX. A test binds this to supply scripted programs
instead of provider-backed generators.")

(defclass synthesizer ()
  ((signature :initarg :signature :reader synth-signature)
   (teacher :initarg :teacher :reader synth-teacher)
   (diversity :initarg :diversity :reader synth-diversity)
   (domain :initarg :domain :initform nil :reader synth-domain)
   (edge-cases :initarg :edge-cases :initform nil :reader synth-edge-cases)
   (temperature :initarg :temperature :reader synth-temperature)
   (model :initarg :model :initform nil :reader synth-model))
  (:documentation "A signature plus the teacher that labels examples for it."))

(defmethod print-object ((synth synthesizer) stream)
  (print-unreadable-object (synth stream :type t)
    (format stream "~a" (signature-string (synth-signature synth)))))

(defun synth (signature &key teacher (diversity :none) domain edge-cases
                             (temperature 0.8d0) model)
  "A synthesizer for SIGNATURE, labelled by TEACHER.

SIGNATURE is signature text or a parsed signature. DOMAIN is prose context
for the generated inputs. EDGE-CASES is a list of hints; when it is
non-empty, SYNTH-GENERATE also produces a fifth as many challenging
examples, labelled with category edge_case. TEMPERATURE is used for input
generation. MODEL, when given, is passed to every request.

DIVERSITY is :none, :lexical or :semantic. It is recorded and reported but
does not yet filter candidates, the same as in the TypeScript."
  (unless teacher
    (error 'ax-error :message "synth: :teacher is required"))
  (unless (member diversity '(:none :lexical :semantic))
    (error 'ax-error
           :message (format nil "synth: :diversity must be :none, :lexical or :semantic, got ~S"
                            diversity)))
  (unless (realp temperature)
    (error 'ax-error
           :message (format nil "synth: :temperature must be a number, got ~S" temperature)))
  (make-instance 'synthesizer
                 :signature (if (stringp signature) (parse-signature signature) signature)
                 :teacher teacher
                 :diversity diversity
                 :domain domain
                 :edge-cases (and edge-cases (coerce edge-cases 'list))
                 :temperature temperature
                 :model model))

;;; ------------------------------------------------------------------
;;; Field descriptions
;;; ------------------------------------------------------------------

(defun %synth-type-name (field)
  (let ((name (jget (jget field "type") "name")))
    (if (stringp name) name "string")))

(defun %synth-array-suffix (field)
  (if (json-true-p (jget (jget field "type") "isArray")) "[]" ""))

(defun %synth-describe-inputs (fields)
  (with-output-to-string (out)
    (loop for field across fields
          for first = t then nil
          do (unless first (terpri out))
             (format out "- ~a: ~a~a~a~a"
                     (jget field "name")
                     (%synth-type-name field)
                     (%synth-array-suffix field)
                     (if (json-true-p (jget field "isOptional")) " (optional)" "")
                     (let ((description (jget field "description")))
                       (if (stringp description) (format nil ": ~a" description) ""))))))

(defun %synth-describe-outputs (fields)
  (with-output-to-string (out)
    (loop for field across fields
          for first = t then nil
          do (unless first (terpri out))
             (format out "- ~a: ~a~a~a~a"
                     (jget field "name")
                     (%synth-type-name field)
                     (%synth-array-suffix field)
                     (let ((options (jget (jget field "type") "options")))
                       (if (%array-p options)
                           (format nil " (options: ~{~a~^, ~})" (coerce options 'list))
                           ""))
                     (let ((description (jget field "description")))
                       (if (stringp description) (format nil ": ~a" description) ""))))))

;;; ------------------------------------------------------------------
;;; Usage
;;; ------------------------------------------------------------------

(defun %synth-add-usage (total usage)
  "Add USAGE's token counts into TOTAL, a usage object."
  (dolist (entry (%refine-usage-list usage))
    (when (hash-table-p entry)
      (dolist (key +usage-token-keys+)
        (let ((value (jget entry key)))
          (when (realp value)
            (%set-key total key (+ (let ((current (jget total key)))
                                     (if (realp current) current 0))
                                   value)))))))
  total)

;;; ------------------------------------------------------------------
;;; Generation
;;; ------------------------------------------------------------------

(defun %synth-input-instruction (synth count)
  (let* ((signature (synth-signature synth))
         (description (jget signature "description"))
         (inputs (signature-fields signature :side :input))
         (outputs (signature-fields signature :side :output)))
    (format nil "You are generating realistic input data for an AI system.

~@[Task description: ~a~]
~@[Domain: ~a~]

The system expects these INPUT fields:
~a

The system produces these OUTPUT fields:
~a

Generate ~a diverse, realistic input examples as a JSON array.
Each example should be an object with the input fields defined above.
Make the examples varied and realistic for the domain.

Output ONLY the JSON array, no explanation."
            (and (stringp description) description)
            (synth-domain synth)
            (%synth-describe-inputs inputs)
            (%synth-describe-outputs outputs)
            count)))

(defun %synth-edge-case-instruction (synth count)
  (format nil "You are generating challenging edge case input data to test an AI system's robustness.

The system expects these INPUT fields:
~a

Generate ~a edge case examples as a JSON array.
Focus on these types of edge cases:
~{- ~a~^~%~}

Output ONLY the JSON array, no explanation."
          (%synth-describe-inputs (signature-fields (synth-signature synth) :side :input))
          count
          (synth-edge-cases synth)))

(defun %synth-request-options (synth)
  (let ((options (%new-object)))
    (when (synth-model synth) (%set-key options "model" (synth-model synth)))
    ;; The TypeScript stores the temperature and never sends it. Sending it
    ;; is the whole point of the option, so it is sent here.
    (%set-key options "modelConfig" (object "temperature" (synth-temperature synth)))
    options))

(defun %synth-generate-inputs (synth instruction count usage)
  "COUNT candidate inputs from the teacher, as a list of JSON objects."
  (let ((generator (funcall *synth-generator-factory* +synth-input-signature+)))
    (program-set-instruction generator instruction)
    (handler-case
        (multiple-value-bind (outputs call-usage)
            (forward generator (synth-teacher synth) (object "count" count)
                     (%synth-request-options synth))
          (%synth-add-usage usage call-usage)
          (let ((examples (and (hash-table-p outputs) (jget outputs "examples"))))
            (if (%array-p examples)
                (coerce (subseq examples 0 (min count (length examples))) 'list)
                (progn
                  (warn "synth: input generation returned ~S, not a JSON array" examples)
                  '()))))
      (error (condition)
        (warn "synth: input generation failed: ~a" condition)
        '()))))

(defun %synth-label (synth input usage)
  "INPUT labelled by the teacher, or a signalled error."
  (unless (hash-table-p input)
    (error 'ax-error
           :message (format nil "synth: generated input ~S is not a JSON object" input)))
  (let ((generator (funcall *synth-generator-factory* (synth-signature synth)))
        (options (%new-object)))
    (when (synth-model synth) (%set-key options "model" (synth-model synth)))
    (multiple-value-bind (outputs call-usage)
        (forward generator (synth-teacher synth) input options)
      (%synth-add-usage usage call-usage)
      (unless (hash-table-p outputs)
        (error 'ax-error
               :message (format nil "synth: teacher returned ~S, not a JSON object" outputs)))
      outputs)))

(defun synth-generate (synth count &key batch-size)
  "Generate COUNT labelled examples with SYNTH.

Returns (values EXAMPLES STATS). EXAMPLES is a JSON array of objects with
input, expected and category; category is normal or edge_case. STATS is a
JSON object with requested, generated, labelingSuccessRate, durationMs and
usage, where usage sums every generation and labelling call.

Inputs are requested in batches of BATCH-SIZE, which defaults to COUNT
capped at ten. An input the teacher cannot label is warned about and left
out, which lowers labelingSuccessRate."
  (unless (and (realp count) (>= count 0))
    (error 'ax-error :message (format nil "synth-generate: count must be a number, got ~S" count)))
  (let* ((count (floor count))
         (size (max 1 (floor (or batch-size (min count 10)))))
         (start (get-internal-real-time))
         (usage (usage-object 0 0 0))
         (examples (%new-array))
         (successes 0)
         (attempts 0))
    (flet ((label-into (inputs category)
             (dolist (input inputs)
               (incf attempts)
               (handler-case
                   (let ((expected (%synth-label synth input usage)))
                     (vector-push-extend
                      (object "input" input "expected" expected "category" category)
                      examples)
                     (incf successes))
                 (error (condition)
                   (warn "synth: failed to label ~a input: ~a" category condition))))))
      (loop for offset from 0 below count by size
            do (let ((batch (min size (- count offset))))
                 (label-into (%synth-generate-inputs
                              synth (%synth-input-instruction synth batch) batch usage)
                             "normal")))
      (when (synth-edge-cases synth)
        (let ((edge-count (ceiling (* count 2) 10)))
          (when (plusp edge-count)
            (label-into (%synth-generate-inputs
                         synth (%synth-edge-case-instruction synth edge-count)
                         edge-count usage)
                        "edge_case")))))
    (values examples
            (object "requested" count
                    "generated" (length examples)
                    "labelingSuccessRate" (if (plusp attempts)
                                              (coerce (/ successes attempts) 'double-float)
                                              0)
                    "durationMs" (round (* 1000 (- (get-internal-real-time) start))
                                        internal-time-units-per-second)
                    "usage" usage))))
