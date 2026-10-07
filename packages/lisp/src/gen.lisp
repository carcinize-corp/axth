;;;; gen.lisp --- structured generation for the experimental Common Lisp Ax port.
;;;;
;;;; Core owns generation, streaming extraction, validation, correction and
;;;; native-session transitions. Lisp owns callable tools, pull handles, native
;;;; hook identity, telemetry, and nonlocal-exit cleanup. Native session clients
;;;; supply a pull-based host protocol; no provider transport is invented here.
;;;;
;;;; Model output is never passed to READ or EVAL.  Every value is produced by
;;;; explicit parsers in this file or by the foundation's strict `parse-json'.

(in-package #:axllm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(define-condition generation-error (ax-generate-error)
  ((kind :initarg :kind :initform :generation :reader generation-error-kind)
   (problems :initarg :problems :initform nil :reader generation-error-problems))
  (:documentation
   "A generation failure.  KIND is one of :config, :unsupported, :validation,
:tool or :steps.  PROBLEMS holds the individual validation messages when
KIND is :validation."))

(defun generation-fail (kind message &key problems)
  (error 'generation-error :kind kind :message message :problems problems))


;;; ------------------------------------------------------------------
;;; Field accessors over Core's field objects
;;; ------------------------------------------------------------------

(defun %field-name (field) (%present (jget field "name")))

(defun %field-title (field)
  (let ((title (%present (jget field "title"))))
    (if (%blankp title) (%field-name field) title)))

(defun %field-flag-p (field key)
  (json-true-p (jget field key)))

(defun %field-internal-p (field)
  (or (%field-flag-p field "isInternal") (%field-flag-p field "is_internal")))

;;; ------------------------------------------------------------------
;;; Memory
;;; ------------------------------------------------------------------
;;;
;;; The conversation a program accumulated, as JSON objects so a caller, an
;;; optimizer or a trace can read it without Lisp-specific types.
;;;
;;; Tags mark a point to roll back to.  `memory-rewind-to-tag' removes the
;;; FIRST item carrying the tag and everything after it, which is the only
;;; reading that makes a rollback a rollback: a retry must drop the whole
;;; failed turn, not just its tail.  A tag this memory has never seen is a
;;; programming error and is signalled; a tag that was seen and has already
;;; been rewound past removes nothing.

(defclass memory ()
  ((items :initform '() :accessor %memory-items)
   (seen-tags :initform (make-hash-table :test #'equal) :reader %memory-seen-tags)))

(defun %memory-entry (role &key session-id index extra)
  (let ((entry (object "role" role)))
    (when extra
      (loop for (key value) on extra by #'cddr
            do (setf (gethash key entry) value)))
    (setf (gethash "session_id" entry) (or session-id :null))
    (when index (setf (gethash "index" entry) index))
    (setf (gethash "tags" entry) (%new-array))
    entry))

(defun %memory-push (memory entry)
  (setf (%memory-items memory) (append (%memory-items memory) (list entry)))
  memory)

(defun memory-add-request (memory messages &key session-id index)
  "Record the request MESSAGES that were sent to the provider."
  (%memory-push memory (%memory-entry "request" :session-id session-id :index index
                                                :extra (list "messages" messages))))

(defun %memory-response-meaningful-p (response)
  "True when RESPONSE said something worth remembering.

An all-whitespace completion with no tool calls is the provider failing to
answer; keeping it would make a correction turn replay the blank.  The reading is
shape-neutral, because the same memory holds turns from any service."
  (cond ((not (hash-table-p response)) (and response (not (eq response :null))))
        (t (let ((content (%response-content response))
                 (calls (%response-tool-calls response)))
             (or (and (stringp content) (not (%blankp content)))
                 (plusp (length calls))
                 (not (null (%present (jget response "audio")))))))))

(defun memory-add-response (memory response &key session-id index)
  "Record RESPONSE unless it carried nothing."
  (if (%memory-response-meaningful-p response)
      (%memory-push memory (%memory-entry "assistant" :session-id session-id :index index
                                                      :extra (list "response" response)))
      memory))

(defun memory-add-function-results (memory results &key session-id index)
  "Record tool RESULTS (a vector of result objects) as one function turn."
  (%memory-push memory
                (%memory-entry "function" :session-id session-id :index index
                                          :extra (list "results"
                                                       (if (and (vectorp results)
                                                                (not (stringp results)))
                                                           results
                                                           (vector results))))))

(defun memory-history (memory &key session-id index)
  "MEMORY's items, oldest first, as a fresh list.

SESSION-ID and INDEX narrow the result to one session or one sample index."
  (remove-if-not
   (lambda (entry)
     (and (or (null session-id) (equal (%present (jget entry "session_id")) session-id))
          (or (null index) (eql (%present (jget entry "index")) index))))
   (copy-list (%memory-items memory))))

(defun memory-add-tag (memory name)
  "Tag the most recent item with NAME.  An empty memory records nothing."
  (unless (stringp name)
    (generation-fail :config "memory-add-tag: tag name must be a string."))
  (let ((last (car (last (%memory-items memory)))))
    (when last
      (let ((tags (%present (jget last "tags"))))
        (unless (and (vectorp tags) (not (stringp tags)))
          (setf tags (%new-array))
          (setf (gethash "tags" last) tags))
        (unless (find name tags :test #'equal)
          (vector-push-extend name tags))
        (setf (gethash name (%memory-seen-tags memory)) t))))
  memory)

(defun %memory-tagged-p (entry name)
  (let ((tags (%present (jget entry "tags"))))
    (and (vectorp tags) (not (stringp tags)) (find name tags :test #'equal) t)))

(defun memory-rewind-to-tag (memory name)
  "Remove the first item tagged NAME and every item after it; return them.

Signals a `generation-error' when NAME was never applied to this memory, so a
typo in a retry tag fails loudly instead of silently rewinding nothing."
  (unless (stringp name)
    (generation-fail :config "memory-rewind-to-tag: tag name must be a string."))
  (let* ((items (%memory-items memory))
         (position (position-if (lambda (entry) (%memory-tagged-p entry name)) items)))
    (cond ((null position)
           (unless (gethash name (%memory-seen-tags memory))
             (generation-fail :config (format nil "Tag \"~a\" not found" name)))
           (%new-array))
          (t
           (let ((removed (subseq items position)))
             (setf (%memory-items memory) (subseq items 0 position))
             (coerce removed 'vector))))))

(defun memory-remove-by-tag (memory name)
  "Remove every item tagged NAME, oldest first, and return them."
  (unless (stringp name)
    (generation-fail :config "memory-remove-by-tag: tag name must be a string."))
  (let ((kept '())
        (removed '()))
    (dolist (entry (%memory-items memory))
      (if (%memory-tagged-p entry name)
          (push entry removed)
          (push entry kept)))
    (setf (%memory-items memory) (nreverse kept))
    (coerce (nreverse removed) 'vector)))

;;; ------------------------------------------------------------------
;;; Generator
;;; ------------------------------------------------------------------

(defclass generator ()
  ((signature :initarg :signature :accessor generator-signature)
   (description :initarg :description :accessor generator-description)
   (instruction :initarg :instruction :accessor generator-instruction)
   ;; The options this generator was built with.  A call's own options win over
   ;; them, which is how Core reads a program: the construction settles the
   ;; program's contract and the call adjusts it.
   (base-options :initarg :base-options :accessor generator-base-options)
   (program-id :initarg :program-id :reader generator-program-id)
   (tools :initarg :tools :reader generator-tools)
   (tool-index :initarg :tool-index :reader generator-tool-index)
   (processor :initarg :processor :accessor generator-function-processor)
   (inputs :initarg :inputs :reader generator-input-fields)
   (outputs :initarg :outputs :reader generator-output-fields)
   (max-steps :initarg :max-steps :reader generator-max-steps)
   (max-retries :initarg :max-retries :reader generator-max-retries)
   ;; Run state.  A program keeps what it did, so an optimizer or a caller can
   ;; read the conversation, the per-model token cost and the traces without
   ;; the generator having to hand them back through `forward''s values.
   (memory :initform (make-instance 'memory) :reader generator-memory)
   (assertions :initform '() :accessor generator-assertions)
   (field-processors :initform '() :accessor generator-field-processors)
   (feedback-processors :initform '() :accessor generator-feedback-processors)
   (stop-functions :initform '() :accessor generator-stop-functions)
   (examples :initform (%new-array) :accessor generator-examples)
   (demos :initform (%new-array) :accessor generator-demos)
   (chat-log :initform '() :accessor generator-chat-log-entries)
   (traces :initform '() :accessor generator-trace-entries)
   (function-call-traces :initform '() :accessor generator-function-call-trace-entries)
   (usage :initform '() :accessor generator-usage-entries)))

(defmethod print-object ((gen generator) stream)
  (print-unreadable-object (gen stream :type t)
    (format stream "~a" (signature-string (generator-signature gen)))))

(defun ax (signature &key description tools (max-steps 25) (max-retries 3) id instruction
                       (options (object)))
  "Create a generator for SIGNATURE (a signature string or parsed signature).

TOOLS is a list or vector of specs from `tool'.  MAX-STEPS bounds tool-call
rounds.  MAX-RETRIES is the retry budget for one step, shared between the
correction turns an invalid output costs and the attempts an unhealthy service
costs, which is what makes a run's total request count predictable; a call can
override the validation half alone with \"validationRetries\".
ID names the program for optimizable-component ids and defaults to \"root\";
INSTRUCTION is optimizable prompt instruction text."
  (let* ((sig (if (stringp signature) (parse-signature signature) signature))
         (inputs (signature-fields sig :side :input))
         (outputs (signature-fields sig :side :output))
         (tool-list (if (null tools) nil (coerce (if (listp tools) tools (coerce tools 'list))
                                                 'list))))
    (unless (and (integerp max-steps) (>= max-steps 0))
      (generation-fail :config "ax: :max-steps must be a non-negative integer."))
    (unless (and (integerp max-retries) (>= max-retries 0))
      (generation-fail :config "ax: :max-retries must be a non-negative integer."))
    (unless (or (null id) (stringp id))
      (generation-fail :config "ax: :id must be a string."))
    (unless (or (null instruction) (stringp instruction))
      (generation-fail :config "ax: :instruction must be a string."))
    (unless (or (null options) (eq options :null) (hash-table-p options))
      (generation-fail :config "ax: :options must be a JSON object."))
    (when (zerop (length outputs))
      (generation-fail :config "ax: signature declares no output fields."))
    (make-instance 'generator
                   :signature sig
                   :description (or description (%present (jget sig "description")))
                   :instruction (or instruction "")
                   :base-options (axllm/core::core-map-merge
                                  (object "max_steps" max-steps "max_retries" max-retries)
                                  (if (hash-table-p options) options (object)))
                   :program-id (if (%blankp id) "root" id)
                   :tools tool-list
                   :tool-index (if tool-list (tool-index tool-list) (make-hash-table :test #'equal))
                   :processor (make-function-processor tool-list)
                   :inputs inputs
                   :outputs outputs
                   :max-steps max-steps
                   :max-retries max-retries)))


;;; ------------------------------------------------------------------
;;; Prompt and output, through Core
;;; ------------------------------------------------------------------
;;;
;;; Rendering a signature into messages and reading a model's reply back into
;;; typed values are Core's, not this file's.  Every port shares the labels, the
;;; type coercions, the absent-value rules and the validation wording because
;;; they all call the same generated code; a second implementation here would
;;; be a second set of answers to the same questions.

(defun %runtime-options (gen options)
  "The options for one call: the generator's, with the call's own winning."
  (let ((merged (axllm/core::core-map-merge (generator-base-options gen)
                                          (if (hash-table-p options) options (object)))))
    (setf (gethash "customLabels" merged)
          (merge-custom-labels (jget (generator-base-options gen) "customLabels")
                               (jget options "customLabels")))
    merged))

(defun %prompt-options (gen options &optional selection)
  "The prompt options for one call.

The generator's instruction and description, whatever the caller passed that
Core's renderer understands, and the render options Core derives from the output
rung.  The last of those matters: the rung decides which contract the prompt
describes, so a structured run whose prompt still asks for labelled lines is
telling the model one thing and the provider another."
  (let ((prompt-options (%runtime-options gen options)))
    (when selection
      (setf prompt-options
            (axllm/core::core-map-merge
             prompt-options
             (axllm/core::structured-output-render-options-impl selection))))
    (unless (%blankp (generator-instruction gen))
      (setf (gethash "instruction" prompt-options) (generator-instruction gen)))
    (unless (%blankp (generator-description gen))
      (setf (gethash "description" prompt-options) (generator-description gen)))
    prompt-options))

(defun %prompt-messages (gen inputs options &optional selection)
  "The system and user messages for INPUTS, as Core renders them.

An input key the signature does not declare is dropped, which is what the
other ports do: a program running as a node in a flow or an agent stage is
handed the whole shared state, and reading only its own fields out of it is the
normal case rather than a mistake."
  (let ((prompt-options (%prompt-options gen options selection)))
    (coerce (render-prompt
             (generator-signature gen) inputs
             ;; A rung that routes the output through a function adds it to the
             ;; callables the prompt describes, so the model is told about the
             ;; one it is meant to call.
             :functions (concatenate 'vector
                                     (coerce (mapcar #'tool-request-spec (generator-tools gen))
                                             'vector)
                                     (let ((extra (%present (jget prompt-options
                                                                 "extra_functions"))))
                                       (if (and extra (vectorp extra) (not (stringp extra)))
                                           extra
                                           (%new-array))))
             :options prompt-options)
            'list)))

(defun %client-features (client model)
  "CLIENT's advertised features, as Core reads them to choose an output rung."
  (if (fboundp 'axllm/core::core-ai-client-features)
      (handler-case (axllm/core::core-ai-client-features client model)
        (error () (object)))
      (object)))

(defun %output-selection (gen client options)
  "Core's whole output-rung selection for this call, as its request builder reads it."
  (let* ((runtime (%runtime-options gen options))
         (model (%forward-option runtime "model" "model"))
         (features (%client-features client model)))
    (axllm/core::select-structured-output-rung (generator-signature gen) features runtime)))

(defun %output-rung (gen client options)
  "The output contract Core selects for this call: NIL means the text contract."
  (let* ((runtime (%runtime-options gen options))
         (model (%forward-option runtime "model" "model"))
         (features (%client-features client model))
         (selection (axllm/core::select-structured-output-rung
                     (generator-signature gen) features runtime)))
    (let ((rung (%present (jget selection "rung"))))
      (and rung (not (eq rung :null)) rung))))

(defun %parse-outputs (gen client response options)
  "Read RESPONSE into typed values.  Returns (values outputs samples problems).

Core does the whole job: it picks the output contract from what the client
advertises, extracts labelled text or a structured object accordingly, recovers
JSON a model wrote into a text field, coerces and validates each value, runs the
field processors and assertions, strips the internal fields and folds in the
thought.  A problem is returned rather than signalled so `forward' can choose
between a correction turn and giving up."
  (handler-case
      (let* ((rung (%output-rung gen client options))
             (parse-state (object))
             ;; A run that asked for parsed dates gets date fields, not strings:
             ;; Core decides which fields that applies to, from the generator's
             ;; options and this call's.
             (fields (axllm/core::date-parse-fields-impl
                      (generator-output-fields gen) (generator-base-options gen)
                      (or options (object))))
             (bundle (axllm/core::parse-sample-outputs
                      gen fields response
                      (json-boolean (equal rung "json_object"))
                      "thought" "" (json-boolean (null rung)) *json-false*
                      parse-state))
             (failure (%present (jget bundle "assertion_failure"))))
        (cond
          (failure
           ;; An assertion that failed with a message is something the model can
           ;; fix; one that failed without a message, or raised, is not.
           (let ((message (if (typep failure 'condition)
                              (ax-error-message-text failure)
                              (princ-to-string failure))))
             (values (object) (%new-array) (list message))))
          (t
           (let ((outputs (%present (jget bundle "outputs"))))
             (values (if (and outputs (plusp (length outputs))) (aref outputs 0) (object))
                     (or (%present (jget bundle "samples")) (%new-array))
                     nil)))))
    (ax-error (condition)
      (values (object) (%new-array) (list (ax-error-message condition))))))

(defun %requested-sample-count (options)
  "How many candidates this call asked the provider for, or 1."
  (let ((asked (or (%present (jget (or options (object)) "sampleCount"))
                   (%present (jget (or options (object)) "sample_count"))
                   (%present (jget (or options (object)) "n")))))
    (if (and (integerp asked) (plusp asked)) asked 1)))

(defun %check-sample-count (samples options)
  "Refuse a native-sampled call that did not come back with what it asked for.

A caller that asks for several candidates and scores the best of them is relying
on having actually received several.  Not every provider dialect carries a sample
count: the OpenAI Responses API has no `n', so a request built for it is sent
without one and answers a single candidate.  Scoring that one candidate and
reporting it as the best of several is a wrong answer that nothing would surface,
which is worse than a failure, so this says so instead.

The check is a measurement of what came back rather than a guess about which
dialect was used, so it holds for any provider that drops the count, including one
added later, and it cannot disagree with the request that was really sent."
  (let ((asked (%requested-sample-count options))
        (got (if (and samples (vectorp samples) (not (stringp samples)))
                 (length samples)
                 0)))
    (when (and (> asked 1) (< got asked))
      (generation-fail
       :config
       (format nil "Asked the provider for ~a candidates and received ~a. ~
This provider or dialect does not carry a sample count, so the request was sent ~
without one; sample serially instead of natively, or use a provider whose chat ~
dialect supports it."
               asked got)))))

(defun %pick-sample (generator default samples options)
  "Which of SAMPLES the run realizes.

With one completion, or no picker, the first is the answer.  With several, the
caller's picker chooses, and Core enforces the contract: it is handed
{\"type\": \"fields\", \"results\": [{\"index\", \"sample\"}, ...]} and must answer an
index that exists, so a picker that returns nonsense fails the run rather than
silently realizing the wrong candidate."
  (declare (ignorable generator))
  (if (or (not (and samples (vectorp samples) (not (stringp samples))))
          (<= (length samples) 1))
      default
      (let ((index (axllm/core::select-sample-index samples (or options (object)))))
        (let ((chosen (aref samples index)))
          (or (%present (jget chosen "sample")) default)))))

(defun %visible-outputs (gen values*)
  "VALUES* as the caller receives them.

Core has already dropped the internal fields and folded in the thought, so this
keeps what Core produced rather than filtering it back down to the declared
fields: a thought the signature never declared is still part of the result."
  (declare (ignorable gen))
  (if (hash-table-p values*) values* (object)))

(defun %accumulate-usage (total usage)
  (when (hash-table-p usage)
    (incf (gethash "promptTokens" total) (%integer-or-zero (jget usage "prompt_tokens")))
    (incf (gethash "completionTokens" total) (%integer-or-zero (jget usage "completion_tokens")))
    (incf (gethash "totalTokens" total) (%integer-or-zero (jget usage "total_tokens"))))
  total)

(defun %unable-to-fix-text (message output)
  "Core's wording for a validation failure the correction turns could not fix.

The model's own last output is quoted, because the next reader of this failure is
a person deciding whether the model or the signature is wrong."
  (format nil "Unable to fix validation error: ~a~c~cLLM Output:~c~a"
          message #\Newline #\Newline #\Newline output))

(defun %generate-failed-text (text)
  "TEXT as the run's failure, in Core's wording.

Wrapping is applied once: a failure that already says the run failed is passed
through, so a nested program does not stutter the prefix."
  (if (%prefix-p "Generate failed: " text)
      text
      (concatenate 'string "Generate failed: " text)))

(defun %prefix-p (prefix text)
  (and (stringp text) (>= (length text) (length prefix))
       (string= prefix text :end2 (length prefix))))

(defun %validation-cause (condition)
  (cond ((typep condition 'validation-error) condition)
        ((typep condition 'ax-generate-error)
         (%validation-cause (ax-generate-error-cause condition)))
        (t nil)))

(defmacro %as-generate-failure (&body body)
  "Run BODY, reporting any failure as Core words a failed run.

The prefix says which layer gave up, which is what a cross-port failure report
compares; the condition's own type survives, because a caller that catches a
provider failure must still catch it."
  `(handler-bind
       ((ax-error (lambda (condition)
                    (let ((text (ax-error-message condition)))
                      (cond
                        ((and (typep condition 'ax-generate-error)
                              (not (typep condition 'generation-error)))
                         (let* ((cause (ax-generate-error-cause condition))
                                (validation (%validation-cause cause)))
                           (error 'generation-error :message text :cause cause
                                  :kind (cond (validation :validation)
                                              ((search "Max steps reached:" text) :steps)
                                              (t :generation))
                                  :problems (when validation (list (ax-error-message validation))))))
                        ((not (%prefix-p "Generate failed: " text))
                         (error (%regenerate-failure
                                 condition (%generate-failed-text text)))))))))
     ,@body))


(defun %correction-prompt (problems)
  (with-output-to-string (out)
    (write-line "Your previous response could not be used. Fix every problem listed below and return the complete output again." out)
    (dolist (problem problems)
      (format out "- ~a~%" problem))
    (write-line "Return only the labelled output lines. Do not call any functions and do not repeat earlier commentary." out)))

;;; ------------------------------------------------------------------
;;; Program contract
;;; ------------------------------------------------------------------
;;;
;;; Every Ax program -- a generator, an agent, a flow -- answers the same six
;;; questions, so an optimizer can read and rewrite one without knowing which
;;; kind it holds.  `forward' is the seventh: the generic that runs it.

(defgeneric forward (program client inputs &optional options)
  (:documentation
   "Run PROGRAM against CLIENT with INPUTS (a hash table with string keys).

Returns (values outputs usage).  OPTIONS is a JSON object of per-call settings
and defaults to an empty object; a method ignores keys it does not implement.
Recognized here: \"maxSteps\" and \"maxRetries\" (also accepted in snake_case)
override the generator's budgets for this call alone."))

(defgeneric program-streaming-forward (program client inputs &optional options)
  (:documentation
   "Run PROGRAM against CLIENT with INPUTS, streaming its output.

OPTIONS carries a \"sink\": a function of one delta envelope, called as each
delta is produced.  Returns (values outputs usage) once the run completes, like
`forward'.

There is deliberately no default method.  A program that scores or judges a
complete result cannot be driven by a prefix, so a wrapper refuses this call
rather than inheriting a silent fallback to `forward' that would let it score
half an answer."))

(defgeneric program-signature (program)
  (:documentation
   "PROGRAM's parsed signature, or :NULL when it declares none.

A flow infers a node's reads and writes from this, so a program that answers
:NULL is one whose inputs and outputs the caller must declare itself."))

(defmethod program-signature ((program t)) :null)

(defmethod program-signature ((gen generator)) (generator-signature gen))

(defgeneric program-native-sample-capable-p (program)
  (:documentation
   "True when PROGRAM can produce several candidates from one `forward' call.

An optimizer asks before choosing between native sampling and running the
program once per candidate.  A program that answers false is sampled serially
rather than silently scored on one candidate."))

(defmethod program-native-sample-capable-p ((program t)) nil)

(defmethod program-native-sample-capable-p ((gen generator))
  "A generator asks the provider for several completions in one call.

The request carries the sample count, Core parses every completion, and a result
picker chooses which one is realized, so an optimizer gets n candidates for one
round trip instead of n rounds."
  t)

(defgeneric program-tools (program)
  (:documentation "PROGRAM's tool specs as a vector."))

(defgeneric program-set-tools (program tools)
  (:documentation
   "Replace PROGRAM's tools with TOOLS (a list or vector of specs from `tool').

Returns PROGRAM.  An agent stage rebuilds a program's callable set between
steps, so this re-indexes everything the program resolves names through."))

(defgeneric program-function-call-traces (program)
  (:documentation
   "PROGRAM's tool calls so far as a vector of objects with \"name\", \"id\",
\"args\", \"status\" and \"result\"."))

(defgeneric program-clear-function-call-traces (program)
  (:documentation
   "Drop PROGRAM's recorded tool calls.  Returns PROGRAM.

An agent stage reads the traces of one step; without a reset it would read
every earlier step's calls again."))

(defgeneric program-set-function-call-traces (program records)
  (:documentation
   "Replace PROGRAM's recorded tool calls with RECORDS.  Returns PROGRAM.

An agent stage lends a program extra tools for one call, reads which of them
ran, and then puts the program back exactly as it found it; restoring the
records is the second half of that."))

(defmethod program-tools ((gen generator))
  (coerce (generator-tools gen) 'vector))

(defmethod program-set-tools ((gen generator) tools)
  (let ((list (coerce (if (listp tools) tools (coerce tools 'list)) 'list)))
    (setf (slot-value gen 'tools) list)
    (let ((index (generator-tool-index gen)))
      (clrhash index)
      (maphash (lambda (name spec) (setf (gethash name index) spec))
               (if list (tool-index list) (make-hash-table :test #'equal))))
    (setf (generator-function-processor gen) (make-function-processor list)))
  gen)

(defmethod program-function-call-traces ((gen generator))
  (coerce (generator-function-call-trace-entries gen) 'vector))

(defmethod program-clear-function-call-traces ((gen generator))
  (setf (generator-function-call-trace-entries gen) '())
  gen)

(defmethod program-set-function-call-traces ((gen generator) records)
  (setf (generator-function-call-trace-entries gen)
        (coerce (if (listp records) records (coerce records 'list)) 'list))
  gen)

(defgeneric program-chat-log (program)
  (:documentation
   "PROGRAM's recorded provider turns, oldest first, as a vector of objects
with \"model\", \"messages\", \"response\", \"usage\" and \"function_calls\"."))

(defgeneric program-usage (program)
  (:documentation
   "PROGRAM's token usage so far as a vector of per-model objects with \"ai\",
\"model\", \"promptTokens\", \"completionTokens\" and \"totalTokens\"."))

(defgeneric program-traces (program)
  (:documentation
   "PROGRAM's completed runs as a vector of objects with \"status\", \"input\",
\"output\", \"chat_log\" and \"function_calls\"."))

(defgeneric program-set-instruction (program text)
  (:documentation "Replace PROGRAM's prompt instruction text.  Returns PROGRAM."))

(defgeneric program-optimizable-components (program)
  (:documentation
   "The parts of PROGRAM an optimizer may rewrite, as a vector of component
objects.  Each carries \"id\", \"owner\", \"kind\", \"current\", \"description\",
\"constraints\", \"dependsOn\", \"preserve\", \"format\" and \"validation\"."))

(defgeneric program-apply-optimized-components (program component-map)
  (:documentation
   "Apply COMPONENT-MAP (component id -> new text) to PROGRAM.  Returns
PROGRAM.  An id PROGRAM does not own is ignored, so one map can be applied to
every program in a composition."))

(defun %program-component (id owner kind current description constraints
                           &key (preserve nil) (format "markdown") (validation nil))
  (object "id" id
          "owner" owner
          "kind" kind
          "current" current
          "description" description
          "constraints" (coerce constraints 'vector)
          "dependsOn" (%new-array)
          "preserve" (json-boolean preserve)
          "format" format
          "validation" (or validation (object "required_placeholders" (%new-array)))))

(defmethod program-chat-log ((gen generator))
  (coerce (generator-chat-log-entries gen) 'vector))

(defmethod program-usage ((gen generator))
  (coerce (mapcar #'cdr (generator-usage-entries gen)) 'vector))

(defun program-usage-by-model (gen)
  "GEN's usage as a vector of objects with \"ai\", \"model\" and the token counts.

`program-usage' answers with the token counts alone, because that list is
compared by value; this is where to look when the provider and model matter."
  (coerce (mapcar (lambda (entry)
                    (axllm/core::core-map-merge
                     (object "ai" (car (car entry)) "model" (cdr (car entry)))
                     (cdr entry)))
                  (generator-usage-entries gen))
          'vector))

(defmethod program-traces ((gen generator))
  (coerce (generator-trace-entries gen) 'vector))

(defun generator-function-call-traces (gen)
  "GEN's tool calls so far as a vector of objects with \"name\", \"id\",
\"args\", \"status\" and \"result\"."
  (coerce (generator-function-call-trace-entries gen) 'vector))

(defmethod program-set-instruction ((gen generator) text)
  (unless (stringp text)
    (generation-fail :config "program-set-instruction: text must be a string."))
  (setf (generator-instruction gen) text)
  gen)

(defparameter +optimized-tool-name-scanner+
  (cl-ppcre:create-scanner "^[a-z][a-z0-9_]{0,31}$"))

(defmethod program-optimizable-components ((gen generator))
  (let ((owner (generator-program-id gen))
        (components '())
        (seen (make-hash-table :test #'equal)))
    (unless (%blankp (generator-description gen))
      (push (%program-component (format nil "~a::description" owner) owner "description"
                                (generator-description gen)
                                "Program signature description."
                                '("Preserve the task intent and field references."))
            components))
    (push (%program-component (format nil "~a::instruction" owner) owner "instruction"
                              (generator-instruction gen)
                              "Prompt instruction text used by this generator."
                              '("Keep required input and output fields intact."))
          components)
    (dolist (spec (generator-tools gen))
      (let ((name (jget spec "name")))
        (unless (or (%blankp name) (gethash name seen))
          (setf (gethash name seen) t)
          (push (%program-component (format nil "~a::fn:~a:desc" owner name) owner "fn-desc"
                                    (or (%present (jget spec "description")) "")
                                    (format nil "Description for tool ~a." name)
                                    '("Non-empty, concise, and faithful to the tool behavior.")
                                    :format "text"
                                    :validation (object "maxLength" 320))
                components)
          (push (%program-component (format nil "~a::fn:~a:name" owner name) owner "fn-name"
                                    name
                                    (format nil "Callable name for tool ~a." name)
                                    '("snake_case" "32 characters or fewer" "unique among tools")
                                    :preserve t
                                    :format "snake_case"
                                    :validation (object "pattern" "^[a-z][a-z0-9_]{0,31}$"))
                components))))
    (coerce (nreverse components) 'vector)))

(defmethod program-apply-optimized-components ((gen generator) component-map)
  (unless (hash-table-p component-map)
    (generation-fail :config
                     "program-apply-optimized-components: component map must be a JSON object."))
  (let ((owner (generator-program-id gen)))
    (flet ((update (id)
             (multiple-value-bind (value present) (gethash id component-map)
               (and present (if (or (null value) (eq value :null)) "" value)))))
      (let ((description (update (format nil "~a::description" owner))))
        (when description
          (unless (stringp description)
            (generation-fail :config (format nil "Optimized description for '~a' must be a string."
                                             owner)))
          (setf (generator-description gen) description)))
      (let ((instruction (update (format nil "~a::instruction" owner))))
        (when instruction
          (unless (stringp instruction)
            (generation-fail :config (format nil "Optimized instruction for '~a' must be a string."
                                             owner)))
          (program-set-instruction gen instruction)))
      ;; Renames are applied against the names the components were produced
      ;; from, then re-indexed in one step, so two tools can never collide
      ;; half-way through the map.
      (let ((renames '()))
        (dolist (spec (generator-tools gen))
          (let* ((name (jget spec "name"))
                 (new-description (update (format nil "~a::fn:~a:desc" owner name)))
                 (new-name (update (format nil "~a::fn:~a:name" owner name))))
            (when new-description
              (unless (stringp new-description)
                (generation-fail :config
                                 (format nil "Optimized description for tool '~a' must be a string."
                                         name)))
              (setf (gethash "description" spec) new-description))
            (when new-name
              (let ((trimmed (and (stringp new-name) (%trim new-name))))
                (unless (and trimmed (cl-ppcre:scan +optimized-tool-name-scanner+ trimmed))
                  (generation-fail :config
                                   (format nil "invalid optimized function name: ~a" new-name)))
                (push (cons spec trimmed) renames)))))
        (let ((proposed (make-hash-table :test #'equal)))
          (dolist (spec (generator-tools gen))
            (let ((rename (assoc spec renames)))
              (let ((name (if rename (cdr rename) (jget spec "name"))))
                (when (gethash name proposed)
                  (generation-fail :config
                                   (format nil "duplicate optimized function name: ~a" name)))
                (setf (gethash name proposed) spec))))
          (dolist (rename renames)
            (setf (gethash "name" (car rename)) (cdr rename)))
          (let ((index (generator-tool-index gen)))
            (clrhash index)
            (maphash (lambda (name spec) (setf (gethash name index) spec)) proposed))
          ;; The processor caches its own name tables, so a rename has to rebuild
          ;; it or the next call would still resolve the old name.
          (setf (generator-function-processor gen)
                (make-function-processor (generator-tools gen)))))))
  gen)

;;; ------------------------------------------------------------------
;;; Assertions, field processors and stop functions
;;; ------------------------------------------------------------------
;;;
;;; An assertion decides whether an output is acceptable.  A failure that
;;; carries a message is something the model can fix, so the run asks again
;;; with that message; a failure with no message, or an error the assertion
;;; itself raises, ends the run, because nothing has been said that the model
;;; could act on.

(defun add-assert (gen assertion &key message)
  "Add ASSERTION to GEN.  Returns GEN.

ASSERTION is a function of the output object returning NIL or true to pass, a
string to fail with that text, or false to fail with MESSAGE."
  (unless (functionp assertion)
    (generation-fail :config "add-assert: the assertion must be a function of one argument."))
  (setf (generator-assertions gen)
        (append (generator-assertions gen)
                (list (object "fn" assertion "message" (or message :null)))))
  gen)

(defun add-field-transform (gen field processor)
  "Rewrite FIELD's final value with PROCESSOR, a function of the value.

This changes the value the caller receives; it does not ask the model again."
  (unless (functionp processor)
    (generation-fail :config "add-field-transform: the processor must be a function."))
  (setf (generator-field-processors gen)
        (append (generator-field-processors gen)
                (list (object "field" field "processor" processor))))
  gen)

(defun set-stop-functions (gen names)
  "Stop the tool loop after any of NAMES is called.  Returns GEN."
  (setf (generator-stop-functions gen)
        (coerce (if (listp names) names (coerce names 'list)) 'list))
  gen)

(defun set-examples (gen examples)
  "Show EXAMPLES, a vector of input/output objects, in the prompt.  Returns GEN."
  (setf (generator-examples gen)
        (if (and (vectorp examples) (not (stringp examples)))
            examples
            (coerce examples 'vector)))
  gen)

(defun set-demos (gen demos)
  "Show DEMOS, a vector of recorded traces, in the prompt.  Returns GEN."
  (setf (generator-demos gen)
        (if (and (vectorp demos) (not (stringp demos))) demos (coerce demos 'vector)))
  gen)

;;; ------------------------------------------------------------------
;;; Core axgen boundaries
;;; ------------------------------------------------------------------
;;;
;;; Core drives assertions, processors, memory, traces and the step decision
;;; through these.  Each one answers for the program it is given, so a program
;;; with no assertions passes and a program with no processors changes nothing:
;;; that is an answer, not a placeholder.

(defun %axgen-assertions (gen)
  (if (typep gen 'generator) (generator-assertions gen) '()))

(defun axllm/core::core-axgen-run-assertions (gen output)
  "Core @axgen_run_assertions: {\"status\": \"pass\"|\"fail\"|\"error\"}.

A failure with a message is reported as \"fail\" with that message so the run can
ask again; anything the assertion raises is \"error\" and ends the run."
  (dolist (entry (%axgen-assertions gen))
    (let ((result (handler-case (funcall (jget entry "fn") output)
                    (error (condition)
                      (return-from axllm/core::core-axgen-run-assertions
                        (object "status" "error" "error" condition))))))
      (cond ((or (null result) (json-true-p result) (eq result t)))
            ((stringp result)
             (return-from axllm/core::core-axgen-run-assertions
               (object "status" "fail" "message" result)))
            ((json-false-p result)
             (let ((message (%present (jget entry "message"))))
               (return-from axllm/core::core-axgen-run-assertions
                 (if message
                     (object "status" "fail" "message" message)
                     (object "status" "fail")))))
            (t nil))))
  (object "status" "pass"))

(defun axllm/core::core-axgen-apply-field-processors (gen values)
  "Core @axgen_apply_field_processors: VALUES with each field transform applied."
  (let ((processors (if (typep gen 'generator) (generator-field-processors gen) '())))
    (if (or (null processors) (not (hash-table-p values)))
        values
        (let ((out (axllm/core::core-map-merge (object) values)))
          (dolist (entry processors)
            (let ((name (jget entry "field")))
              (multiple-value-bind (value present) (gethash name out)
                (when present
                  (setf (gethash name out) (funcall (jget entry "processor") value))))))
          (%memory-push (generator-memory gen)
                        (object "role" "processor" "output" out "tags" (vector "processor")))
          out))))

(defun axllm/core::core-axgen-call-processor (spec value context)
  "Core @axgen_call_processor: run one processor over VALUE."
  (let ((processor (and (hash-table-p spec) (%present (jget spec "processor")))))
    (if (functionp processor) (funcall processor value context) :null)))

(defun axllm/core::core-axgen-should-continue-steps (gen calls)
  "Core @axgen_should_continue_steps: false once a stop function has been called."
  (let ((stops (if (typep gen 'generator) (generator-stop-functions gen) '())))
    (if (null stops)
        true
        (let ((hit nil))
          (map nil (lambda (call)
                     (when (and (hash-table-p call)
                                (member (jget call "name") stops :test #'equal))
                       (setf hit t)))
               (if (and (vectorp calls) (not (stringp calls))) calls (coerce calls 'vector)))
          (json-boolean (not hit))))))

(defun axllm/core::core-axgen-render-examples (gen)
  "Core @axgen_render_examples: the example turns to put in the prompt."
  (%render-demonstrations gen (generator-examples gen) "Example"))

(defun axllm/core::core-axgen-render-demos (gen)
  "Core @axgen_render_demos: the demonstration turns to put in the prompt."
  (%render-demonstrations gen (generator-demos gen) "Demo"))

(defun %render-demonstrations (gen items label &optional in-system)
  "Host demonstration envelopes; Core formats and validates their field values."
  (let ((messages (%new-array)))
    (unless (and (not in-system) (json-true-p (jget (generator-base-options gen) "examplesInSystem")))
      (loop for item across items do
        (dolist (side '("input" "output"))
          (let* ((values (jget item side (object)))
                 (fields (signature-fields (generator-signature gen)
                                           :side (if (equal side "input") :input :output)))
                 (text (axllm/core::prompt-field-group-content-impl
                        (generator-signature gen) values fields))
                 (prefix (format nil "~a ~a:~%" label (if (equal side "input") "Input" "Output"))))
            (vector-push-extend
             (object "role" (if (equal side "input") "user" "assistant")
                     "content" (if (stringp text)
                                   (concatenate 'string prefix (string-right-trim '(#\Newline) text))
                                   (concatenate 'vector (vector (object "type" "text" "text" prefix)) text)))
             messages)))))
    messages))

(defun axllm/core::core-axgen-function-result-formatter ()
  "Core @axgen_function_result_formatter: the process-wide tool-result formatter."
  (let ((global (ignore-errors (get-global "functionResultFormatter"))))
    (if (functionp global) global :null)))

(defun axllm/core::core-axgen-memory-add-request (gen messages)
  ;; Core keeps appending to this array after the request has been recorded.
  (when (typep gen 'generator)
    (memory-add-request (generator-memory gen) (copy-seq messages)))
  :null)

(defun axllm/core::core-axgen-memory-add-response (gen request response)
  (declare (ignore request))
  (when (typep gen 'generator) (memory-add-response (generator-memory gen) response))
  :null)

(defun axllm/core::core-axgen-memory-add-function-result (gen call result ok result-text)
  (declare (ignore ok))
  (when (typep gen 'generator)
    (memory-add-function-results
     (generator-memory gen)
     (vector (object "functionId" (jget call "id")
                     "name" (jget call "name")
                     "result" (if (%blankp result-text) result result-text)
                     "result_text" result-text))))
  :null)

(defun axllm/core::core-axgen-memory-add-correction (gen response error)
  "Record the rejected turn, tagged so a later cleanup can drop the retries."
  (declare (ignore error))
  (when (typep gen 'generator)
    (when (%memory-response-meaningful-p response)
      (memory-add-tag (generator-memory gen) "correction")))
  :null)

(defun axllm/core::core-axgen-memory-cleanup-corrections (gen)
  "Drop the turns a correction added, so a later read sees the run, not its retries."
  (when (typep gen 'generator)
    (ignore-errors (memory-remove-by-tag (generator-memory gen) "correction")))
  :null)

(defun axllm/core::core-axgen-record-chat-log (gen request response)
  (when (typep gen 'generator)
    (%record-core-turn gen request response))
  :null)

(defun axllm/core::core-axgen-record-function-call (gen call result status)
  (when (typep gen 'generator)
    (%record-function-call gen call (ignore-errors (%function-call-arguments call))
                          (cond ((stringp result) result)
                                ((typep result 'condition) (princ-to-string result))
                                (t (encode-json result)))
                          status))
  :null)

(defun axllm/core::core-axgen-record-trace (gen input output status)
  (when (typep gen 'generator) (%record-trace gen input output status))
  :null)

(defun %handler-takes-context-p (handler)
  "True when HANDLER accepts a second argument, the call context."
  (let ((arity (ignore-errors
                (let ((symbol (find-symbol "FUNCTION-LAMBDA-LIST" "SB-INTROSPECT")))
                  (and symbol (funcall symbol handler))))))
    (and (listp arity) (> (length (remove-if (lambda (x) (member x '(&optional &key &rest)))
                                             arity))
                          1))))

(defun axllm/core::core-tool-invoke (fn params extras)
  "Core @tool_invoke: run tool FN with PARAMS.

EXTRAS is the call context Core assembled.  A handler that wants it takes two
arguments; one that does not keeps taking one, so a tool written before there
was a context does not have to change."
  (let ((handler (and (hash-table-p fn) (tool-handler fn))))
    (unless (functionp handler)
      (%function-call-fail "No handler for function: ~a"
                           (and (hash-table-p fn) (jget fn "name"))))
    (let ((arguments (if (hash-table-p params) params (object))))
      (let ((problems (validate-tool-arguments fn arguments)))
        (when problems (%function-call-fail "~a" (%string-join " " problems))))
      (if (%handler-takes-context-p handler)
          (funcall handler arguments extras)
          (funcall handler arguments)))))

(defun axllm/core::core-axgen-speak (client request options)
  "Core @axgen_speak: synthesise speech for an audio output field.

Delegates to the service's own `ax-speak'.  A service that does not implement it
says so, rather than this returning a silent artifact that would make an audio
field look rendered when nothing was spoken."
  (declare (ignorable options))
  (unless (fboundp 'ax-speak)
    (generation-fail :unsupported "Audio output needs a service that implements ax-speak."))
  (handler-case (ax-speak client request options)
    (ax-error (condition) (error condition))
    (error (condition)
      (generation-fail :unsupported
                       (format nil "Audio output is not available from this service: ~a"
                               (ax-error-message-text condition))))))

(defun axllm/core::core-axgen-apply-context-cache (gen messages options)
  "Core @axgen_apply_context_cache: mark the messages a provider may cache.

The breakpoint is the last message a run wants cached, so everything up to it is
stable prompt and everything after it is this call's own. Return a fresh mutable
history even when caching is disabled: Core appends tool and correction turns to
this array in both forward and streaming-forward."
  (let* ((options (%runtime-options gen options))
         ;; COERCE/MAP to VECTOR would produce a fixed-size array. Preserve
         ;; Core's extensible-array contract, and copy maps before cache edits.
         (messages (let ((out (%new-array)))
                     (map nil (lambda (message)
                                (vector-push-extend
                                 (axllm/core::core-map-merge (object) message) out))
                          messages)
                     out))
         (cache (or (%present (jget options "contextCache"))
                    (%present (jget options "context_cache"))))
         (enabled (and cache (not (json-false-p cache)))))
    (when (and (plusp (length messages)) (json-true-p (jget options "examplesInSystem")))
      (let ((turns (concatenate 'vector
                                (%render-demonstrations gen (generator-examples gen) "Example" t)
                                (%render-demonstrations gen (generator-demos gen) "Demo" t))))
        (when (plusp (length turns))
          (setf (gethash "content" (aref messages 0))
                (format nil "~a~%~%--- EXAMPLES ---~%~a~%--- END OF EXAMPLES ---"
                        (jget (aref messages 0) "content")
                        (%string-join (format nil "~%~%")
                                      (map 'list (lambda (message) (jget message "content")) turns)))))))
    (when enabled
      (let ((breakpoint (or (%present (jget cache "cacheBreakpoint"))
                            (%present (jget cache "cache_breakpoint"))
                            "after-examples")))
        (if (equal breakpoint "system")
            (loop for message across messages
                  when (equal (%present (jget message "role")) "system")
                    do (setf (gethash "cache" message) true))
            ;; Without a system-only breakpoint the last stable message carries
            ;; it, which is the one before this call's own turn.
            (when (> (length messages) 1)
              (setf (gethash "cache" (aref messages (- (length messages) 2))) true)))))
    messages))

(defvar *generation-stop-tag* nil)
(defvar *generation-open-streams* :inactive)

(defmethod ax-stream :around ((service t) request &optional options)
  (declare (ignore request options))
  (let ((handle (call-next-method)))
    (unless (eq *generation-open-streams* :inactive)
      (pushnew handle *generation-open-streams* :test #'eq))
    handle))

(defmethod ax-stream-close :around ((handle t))
  (unwind-protect (call-next-method)
    (unless (eq *generation-open-streams* :inactive)
      (setf *generation-open-streams* (delete handle *generation-open-streams* :test #'eq)))))

(defun axllm/core::core-axgen-emit-delta (sink envelope)
  "Core @axgen_emit_delta: hand ENVELOPE to the caller's sink.

A sink that raises stops the run: a caller consuming a stream has said it cannot
go on, and swallowing that would keep producing output nobody is reading.
Returning AX:FALSE closes the stream normally, without another upstream read."
  (when (functionp sink)
    (when (and (eq (funcall sink envelope) false) *generation-stop-tag*)
      (throw *generation-stop-tag* :consumer-stopped)))
  :null)

(defun axllm/core::core-axgen-check-streaming-assertion (spec text done)
  "Core @axgen_check_streaming_assertion: nil to pass, or the failure message.

A streaming assertion sees a field's text so far, so it can stop a run early
rather than waiting for output that is already wrong."
  (let ((fn (and (hash-table-p spec) (%present (jget spec "fn"))))
        (not-contains (and (hash-table-p spec) (%present (jget spec "not_contains"))))
        (message (and (hash-table-p spec) (%present (jget spec "message")))))
    (cond
      ((functionp fn)
       (let ((result (funcall fn text done)))
         (cond ((or (null result) (json-true-p result) (eq result t)) (object "status" "pass"))
               ((stringp result) (object "status" "fail" "message" result))
               (t (object "status" "fail")))))
      ((and (stringp not-contains) (search not-contains text))
       (if message (object "status" "fail" "message" message) (object "status" "fail")))
      (t (object "status" "pass")))))

;;; ------------------------------------------------------------------
;;; Core program boundaries
;;; ------------------------------------------------------------------
;;;
;;; Core reaches a program through three boundaries so a flow or an optimizer
;;; can read and rewrite a node without knowing what kind of program it is.
;;; Each one is a thin adapter over the public generic above, which is where a
;;; new program kind implements its behaviour.

(defun axllm/core::core-program-signature (program)
  "Core @program_signature: PROGRAM's signature as text, or \"\" when it has none."
  (let ((signature (program-signature program)))
    (if (or (null signature) (eq signature :null))
        ""
        (signature-string signature))))

(defun axllm/core::core-program-components (program)
  "Core @program_components: PROGRAM's optimizable components."
  (program-optimizable-components program))

(defun axllm/core::core-program-apply-components (program component-map)
  "Core @program_apply_components: apply COMPONENT-MAP to PROGRAM."
  (program-apply-optimized-components program component-map))

;;; A generator answers the host-object reads and calls Core makes on a program.
;;; A `core-host-get' it does not implement already answers with the fallback,
;;; so only the keys Core actually asks a program for are listed here.

(defmethod axllm/core::core-host-get ((gen generator) key &optional (fallback :null))
  (cond ((equal key "program_id") (generator-program-id gen))
        ((equal key "signature") (generator-signature gen))
        ((equal key "options") (generator-base-options gen))
        ((equal key "prompt_template") gen)
        ((equal key "functions")
         (coerce (generator-tools gen) 'vector))
        ((equal key "field_processors") (coerce (generator-field-processors gen) 'vector))
        ((equal key "feedback_processors") (coerce (generator-feedback-processors gen) 'vector))
        ((equal key "streaming_field_processors")
         (jget (generator-base-options gen) "streaming_field_processors" (%new-array)))
        ((equal key "streaming_assertions")
         (jget (generator-base-options gen) "streaming_assertions" (%new-array)))
        ((equal key "instruction") (generator-instruction gen))
        ((equal key "description") (or (generator-description gen) fallback))
        ((equal key "traces") (program-traces gen))
        ((equal key "chat_log") (program-chat-log gen))
        ((equal key "function_call_traces") (program-function-call-traces gen))
        (t fallback)))

(defmethod axllm/core::core-host-call ((gen generator) method args)
  (cond ((equal method "signature") (generator-signature gen))
        ((equal method "get_signature") (generator-signature gen))
        ((equal method "render")
         (coerce (%prompt-messages gen (aref args 0) (aref args 1)) 'vector))
        ;; A generator runs in the calling thread and holds no state a worker
        ;; could not rebuild, so it offers no owned worker: answering :NULL is
        ;; what makes a caller fall back to running it serially.
        ((equal method "owned_worker_factory") :null)
        ((equal method "get_optimizable_components") (program-optimizable-components gen))
        ((equal method "apply_optimized_components")
         (program-apply-optimized-components gen (aref args 0)))
        ((equal method "set_instruction") (program-set-instruction gen (aref args 0)))
        ((equal method "get_traces") (program-traces gen))
        ((equal method "get_chat_log") (program-chat-log gen))
        ((equal method "get_function_call_traces") (program-function-call-traces gen))
        (t (call-next-method))))

;;; ------------------------------------------------------------------
;;; Core cache boundaries
;;; ------------------------------------------------------------------
;;;
;;; A caching function is one Lisp function of (key &optional value): the key
;;; alone reads and answers with the stored value or :NULL, and a key with a
;;; value writes.  One function for both directions keeps a cache a single
;;; object a caller can hand around, rather than a pair that can disagree.

(defun axllm/core::core-axgen-caching-function (generator-or-options call-options)
  "Core @axgen_caching_function: the caching function for this call.

The caller's wins over the process-wide one, so a flow can cache a run without
changing what anything else caches."
  (let* ((options (if (typep generator-or-options 'generator)
                      (%runtime-options generator-or-options call-options)
                      generator-or-options))
         (fallback (unless (typep generator-or-options 'generator) call-options)))
  ;; A run that carries a run control is being watched or steered, so its
  ;; output must not be served from, or written to, a cache: the caller asked
  ;; for this run, not for a remembered one.
  (when (%present (jget options "control"))
    (return-from axllm/core::core-axgen-caching-function :null))
  (let ((local (or (%present (jget options "cachingFunction"))
                   (%present (jget options "caching_function")))))
    (cond ((functionp local) local)
          ((functionp fallback) fallback)
          (t (let ((global (ignore-errors (get-global "cachingFunction"))))
               (if (functionp global) global :null)))))))

(defun axllm/core::core-axgen-cache-read (cache-function key)
  "Core @axgen_cache_read: the stored value for KEY, or :NULL."
  (if (functionp cache-function)
      (let ((hit (funcall cache-function key)))
        (if (null hit) :null hit))
      :null))

(defun axllm/core::core-axgen-cache-write (cache-function key value)
  "Core @axgen_cache_write: store VALUE under KEY.  Returns VALUE."
  (when (functionp cache-function)
    (funcall cache-function key value))
  value)

;;; ------------------------------------------------------------------
;;; forward
;;; ------------------------------------------------------------------

(defun %forward-option (options camel snake)
  "OPTIONS' value for CAMEL, falling back to SNAKE, or NIL when neither is set."
  (when (hash-table-p options)
    (multiple-value-bind (value present) (gethash camel options)
      (if (and present (not (eq value :null)))
          value
          (multiple-value-bind (value present) (gethash snake options)
            (and present (not (eq value :null)) value))))))

(defun %forward-budget (options camel snake fallback what)
  (let ((value (%forward-option options camel snake)))
    (cond ((null value) fallback)
          ((and (integerp value) (>= value 0)) value)
          (t (generation-fail :config
                              (format nil "forward: ~a must be a non-negative integer." what))))))

(defun %tool-batch-problem (call index)
  "Return NIL when CALL can be executed, else why it cannot.

Only what this layer needs is checked.  A tool result is keyed by its call id,
so a call with no usable id cannot be answered at all; the provider layer owns
the rest of the wire shape (see ir/axcore/ai.axir) and rejects a malformed
response before it ever reaches here."
  (cond
    ((or (null call) (eq call :null))
     (format nil "Function call at index ~a cannot be null or undefined." index))
    ((not (hash-table-p call))
     (format nil "Function call at index ~a must be an object, received: ~a"
             index (encode-json call)))
    ((not (let ((id (jget call "id"))) (and (stringp id) (not (%blankp id)))))
     (format nil "Function call at index ~a must have a non-empty string id." index))
    ((not (let ((name (jget call "name"))) (and (stringp name) (not (%blankp name)))))
     (format nil "Function call at index ~a must have a non-empty function name." index))
    (t nil)))

(defun %validate-tool-batch (calls seen-call-ids)
  "Signal on the first unusable call in CALLS.  Nothing is executed here.

Only failures the model cannot correct are checked: a call with no usable id or
name, a call kind that is not a function, and a reused id.  A wrong tool name
or a failing handler is recoverable and is reported to the model per call, so
it is not checked here."
  (let ((batch-ids (make-hash-table :test #'equal)))
    (loop for call in calls
          for position from 0
          do (let ((problem (%tool-batch-problem call position)))
               (when problem (generation-fail :tool problem)))
             (let ((id (jget call "id")))
               (when (or (gethash id seen-call-ids) (gethash id batch-ids))
                 (generation-fail :tool (format nil "The provider reused tool call id '~a'." id)))
               (setf (gethash id batch-ids) t)))
    (maphash (lambda (id value) (setf (gethash id seen-call-ids) value)) batch-ids))
  t)

(defun %tool-error-text (message)
  "MESSAGE as the result text a failed tool call sends back to the model.

Core wraps it in an object so the model can tell a reported failure from a
tool that happened to answer with prose."
  (encode-json (object "error" message)))

(defun %recorded-response (response)
  "RESPONSE as the chat log and the memory record it: Core's completion shape.

Two different shapes are in play and both are deliberate.  A service answers
Core's chat response (`results', `model_usage'), which is what ax-chat promises
and what multi-sample parsing needs.  The chat log and the conversation memory are
read by people and by other ports, and the reference writes a completion there:
the first result's content, tool calls, thought and finish reason lifted to the
top, with `results' still carried for the other samples.  Core itself converts at
exactly this point, so this is its decision rather than a shape invented here, and
it is a projection for recording only -- nothing downstream reads it back."
  (if (and (hash-table-p response)
           (not (nth-value 1 (gethash "content" response))))
      (axllm/core::chat-response-to-completion response)
      response))

(defun %record-chat-log (gen model history response)
  "Record one provider turn under MODEL, the model the request actually named."
  (setf (generator-chat-log-entries gen)
        (append (generator-chat-log-entries gen)
                (list (object "model" model
                              ;; COERCE alone aliases Core's growing vector.
                              "messages" (copy-seq (coerce history 'vector))
                              ;; The content and the tool calls are lifted beside
                              ;; the response so a reader of the log does not have
                              ;; to know which shape the service answered in.
                              "content" (%response-content response)
                              "response" (%recorded-response response)
                              "usage" (or (%response-usage response) :null)
                              "function_calls" (%response-tool-calls response)
                              "thought_blocks" (or (%present (jget response "thought_blocks"))
                                                   (%present (jget (%response-primary response) "thought_blocks"))
                                                   (%new-array))
                              "thought" (%response-thought response)))))
  gen)

(defun %record-usage (gen client model usage)
  "Fold USAGE into GEN's totals for MODEL, attributed to CLIENT's provider."
  (%record-usage-for gen (%service-name client) model usage))

(defun %record-usage-for (gen provider model usage)
  "Fold USAGE into GEN's totals for MODEL.

MODEL is the model the request named, which is the call's override when it has
one and the client's default otherwise.  Attributing a run to the client's
default when it was sent somewhere else would make the usage report, and the
chat log, disagree with the request that produced them.

A usage entry is exactly the three token counts, in the snake_case spelling
every port compares by value, so a subset assertion over a usage list matches.
The provider and model that produced them are reported separately, by
`program-usage-by-model', rather than carried inside the entry where they would
break that comparison."
  (when (hash-table-p usage)
    (let* ((name provider)
           (key (cons name model))
           (entry (cdr (assoc key (generator-usage-entries gen) :test #'equal))))
      (unless entry
        (setf entry (object "prompt_tokens" 0 "completion_tokens" 0 "total_tokens" 0))
        (setf (generator-usage-entries gen)
              (append (generator-usage-entries gen) (list (cons key entry)))))
      (dolist (key '("prompt_tokens" "completion_tokens" "total_tokens"))
        (setf (gethash key entry)
              (+ (%integer-or-zero (jget entry key)) (%integer-or-zero (jget usage key)))))))
  gen)

(defun %record-function-call (gen call arguments result status)
  (setf (generator-function-call-trace-entries gen)
        (append (generator-function-call-trace-entries gen)
                (list (object "name" (jget call "name")
                              "id" (jget call "id")
                              "args" (if (hash-table-p arguments) arguments (object))
                              "status" status
                              "result" (if (stringp result) result :null)))))
  gen)

(defun %record-trace (gen inputs output status)
  (setf (generator-trace-entries gen)
        (append (generator-trace-entries gen)
                (list (object "status" status
                              "input" inputs
                              "output" (or output :null)
                              "chat_log" (program-chat-log gen)
                              "function_calls" (generator-function-call-traces gen)))))
  gen)

(defun %response-results (response)
  "RESPONSE's completions, as a vector.

Core's chat response carries its completions under \"results\"; a response with
none is treated as one completion, which is the same reading Core's own parser
uses."
  (let ((results (%present (jget response "results"))))
    (if (and results (vectorp results) (not (stringp results)))
        results
        (vector response))))

(defun %response-primary (response)
  "The completion a single-sample run reads."
  (let ((results (%response-results response)))
    (if (plusp (length results)) (aref results 0) response)))

(defun %response-content (response)
  "The text of RESPONSE's first completion."
  (or (%present (jget (%response-primary response) "content")) ""))

(defun %response-all-content (response)
  "Every completion's text, joined as the reference joins them.

A failure report for a multi-sample run has to show what each candidate said; a
report quoting only the first would hide the sample that was actually wrong."
  (%string-join " --- "
                (map 'list (lambda (completion)
                             (or (%present (jget completion "content")) ""))
                     (%response-results response))))

(defun %response-thought (response)
  (or (%present (jget (%response-primary response) "thought")) ""))

(defun %response-finish-reason (response)
  (or (%present (jget (%response-primary response) "finish_reason"))
      (%present (jget (%response-primary response) "finishReason"))
      ""))

(defun %response-tool-calls (response)
  "RESPONSE's tool calls, flattened to what this layer answers them by.

Core carries a call as {id, type, function:{name, params}}; the id and the name
are what a result is keyed and resolved by, so they are lifted out once here
rather than at each use."
  (let* ((completion (%response-primary response))
         (calls (or (%present (jget completion "function_calls"))
                    (%present (jget completion "functionCalls"))
                    (%present (jget completion "toolCalls")))))
    (if (and calls (vectorp calls) (not (stringp calls)))
        (map 'vector
             (lambda (call)
               (if (hash-table-p call)
                   (let ((nested (%present (jget call "function"))))
                     (if nested
                         (object "id" (jget call "id")
                                 "name" (jget nested "name")
                                 "arguments" (let ((params (jget nested "params")))
                                               (cond ((stringp params) params)
                                                     ((or (null params) (eq params :null)) "{}")
                                                     (t (encode-json params)))))
                         call))
                   call))
             calls)
        (%new-array))))

(defun %response-usage (response)
  "RESPONSE's token counts as one flat object, or NIL when it reported none.

Core reports them under model_usage.tokens in snake_case; this port's own client
reported a flat camelCase usage.  Both are read here, once, so nothing
downstream has to know which service answered."
  (let* ((model-usage (or (%present (jget response "model_usage"))
                          (%present (jget response "modelUsage"))))
         (tokens (or (and model-usage (%present (jget model-usage "tokens")))
                     model-usage
                     (%present (jget response "usage")))))
    (when (hash-table-p tokens)
      (flet ((count-of (camel snake)
               (%integer-or-zero (or (%present (jget tokens camel))
                                     (%present (jget tokens snake))))))
        (object "prompt_tokens" (count-of "promptTokens" "prompt_tokens")
                "completion_tokens" (count-of "completionTokens" "completion_tokens")
                "total_tokens" (count-of "totalTokens" "total_tokens"))))))

(defun %response-attribution (response fallback-name fallback-model)
  "The provider and model RESPONSE says served it, else the service's own.

A response that names its model is the better authority: a router or a balancer
chooses per call, and recording the service's default would misattribute the run."
  (let ((model-usage (or (%present (jget response "model_usage"))
                         (%present (jget response "modelUsage")))))
    (values (or (and model-usage (%present (jget model-usage "ai"))) fallback-name)
            (or (and model-usage (%present (jget model-usage "model"))) fallback-model))))

(defun %service-name (service)
  "SERVICE's provider name, through the service protocol."
  (or (ignore-errors (ax-service-name service))
      (ignore-errors (ai-name service))
      "service"))

(defun %service-model (service)
  "The model SERVICE will use when a call does not override it."
  (or (%present (jget (ignore-errors (ax-options service)) "model"))
      (ignore-errors (ai-model service))
      ""))

(defun %infrastructure-failure-p (condition)
  "True when CONDITION is the service failing rather than the model answering badly.

A network error, a timeout and a 5xx are worth trying again; a 4xx is the request
itself being wrong, so repeating it would only waste the budget."
  (and (ignore-errors (axllm/core::core-exception-is-infrastructure condition))
       (not (json-false-p (axllm/core::core-exception-is-infrastructure condition)))
       t))

(defun %chat-with-infra-retry (client request options budget)
  "One turn, retrying BUDGET times while the service is the thing that failed.

Returns (values response attempts-used).  The attempts come out of the same
per-step budget the correction turns use, so an unhealthy service and a
confused model cannot together exceed the run's request count."
  (let ((attempt 0))
    (loop
      (handler-case
          (return (values (ax-chat client request options) attempt))
        (ax-error (condition)
          (unless (and (%infrastructure-failure-p condition) (< attempt budget))
            (error condition))
          (incf attempt)
          (ignore-errors (axllm/core::core-retry-sleep attempt client options)))))))

(defun %chat-request (generator history tool-choice options selection step)
  "One turn as Core's chat request object, built by Core.

Core owns what a request carries: the model config, the function specs, the
function-call mode, the response format the selected rung asks for, and the
metadata that records which rung was selected.  Building any of it here would be
a second answer to a question Core already answers, and the rung would then be
visible in the prompt but not in the request."
  (let ((request (axllm/core::build-gen-chat-request
                  generator (coerce history 'vector)
                  (%correction-request-options options tool-choice)
                  (or selection (object))
                  step)))
    request))

(defun %correction-request-options (options tool-choice)
  "OPTIONS for one request.

A correction turn asks for no new call only where a provider can express that;
Core decides, from the same option a caller would set."
  (declare (ignorable tool-choice))
  (if (hash-table-p options) options (object)))

(defun %stream-sink (options)
  "The caller's delta sink, or NIL."
  (let ((sink (or (%present (jget options "sink")) (%present (jget options "on_delta")))))
    (and (functionp sink) sink)))

(defmethod program-streaming-forward ((generator generator) client inputs
                                      &optional (options (object)))
  "Run GENERATOR against CLIENT with INPUTS, streaming each field's text.

OPTIONS carries \"sink\", a function of one delta envelope.  Each chunk is read,
extracted and emitted before the next one is asked for, so a sink that cannot
keep up, or that refuses to go on, stops the work upstream instead of being
handed a replay of a turn that already finished.  Core decides what a delta is:
it holds a field back until the value can be read, so the sink never sees a
half-parsed number or a field a later label turns out to have ended.

A delta's \"version\" is the attempt it belongs to, counting from zero.  A
correction turn restarts the fields at the next version, so a consumer that
rendered the first attempt knows to replace it rather than append to it.

Returns (values outputs usage), the same two values as `forward', because a
caller that streamed still needs the finished, validated result."
  (unless (hash-table-p inputs)
    (generation-fail :config "program-streaming-forward: inputs must be a hash table."))
  (%as-generate-failure
   (%core-generation-run generator client inputs options t)))

(defmacro %stream-correctable (thunk)
  "Run THUNK, reporting a validation failure as a problem instead of raising it.

Returns (values result nil) or (values nil problem-text).  Core reads a chunk and
finalizes a turn with the run's stage set to validation, which is its way of
saying that what goes wrong here is the model's to fix on the next turn rather
than the end of the run."
  `(handler-case (values (funcall ,thunk) nil)
     (ax-error (condition) (values nil (ax-error-message condition)))))


(defun %stream-config (generator client options selection)
  "The streaming config Core's chunk reader asks for.

Which fields are held back, whether the text is structured, whether JSON strings
are parsed, where the thought goes: all of it follows from the rung Core selected
and the generator's own options, so it is read from those rather than decided
here.  Getting this wrong is invisible in a passing unstreamed run and shows up
only as a delta that was emitted too early."
  (let* ((runtime (%runtime-options generator options))
         (base (generator-base-options generator))
         (fields (generator-output-fields generator))
         (rung (%present (jget (or selection (object)) "rung")))
         (rung (and rung (not (eq rung :null)) rung))
         (complex (axllm/core::signature-has-complex-fields
                   (generator-signature generator) runtime))
         (native (equal rung "native"))
         (json-object (equal rung "json_object"))
         (features (%client-features client (%forward-option runtime "model" "model")))
         (functions (generator-tools generator))
         (config (object)))
    (setf (gethash "fields" config)
          (axllm/core::date-parse-fields-impl fields base (or options (object))))
    (setf (gethash "held" config)
          (axllm/core::stream-held-fields-impl generator (jget config "fields")))
    (setf (gethash "structured" config)
          (json-boolean (or (axllm/core::core-true-p complex) native json-object)))
    (setf (gethash "strict_json" config)
          (json-boolean (or json-object
                            (and native (not (axllm/core::core-true-p complex))))))
    (setf (gethash "parse_json_strings" config)
          (json-boolean (and (axllm/core::core-true-p complex)
                             (not (equal rung "function")))))
    (setf (gethash "thought_field" config) (%thought-field-name generator))
    (setf (gethash "strict_mode" config)
          (axllm/core::strict-mode-option-impl base (or options (object))))
    ;; A generator whose tools get a chain of thought must not fail a field early:
    ;; the turn that calls a tool is allowed to say nothing else.
    (setf (gethash "skip_early_fail" config)
          (json-boolean (and (axllm/core::core-true-p
                              (or (%present (jget features "functionCot"))
                                  (%present (jget features "function_cot"))))
                             (plusp (length (or functions (%new-array)))))))
    config))

(defun %thought-field-name (generator)
  "Where this generator's thought goes, which a signature can rename."
  (let* ((base (or (generator-base-options generator) (object)))
         (name (or (%present (jget base "thoughtFieldName"))
                   (%present (jget base "thought_field_name")))))
    (if (and (stringp name) (plusp (length name))) name "thought")))

(defun %stream-one-attempt (generator client request options config sink version
                            sample-count buffered)
  "Read one streamed turn, emitting deltas as Core decides them.

Returns the response the chunks fold up to, in Core's shape.  Each chunk is read,
handed to Core, and emitted before the next is asked for, so a sink that refuses
stops the upstream read rather than being handed a replay.

Core owns every judgement here: which part of a chunk can be shown yet, when a
held field is complete, what a thought delta is, and when a finish reason ends the
run.  A terminal finish reason is checked before that chunk's text is emitted,
because a run that was cut off must not first show the text it was cut off in the
middle of."
  (let* ((events (%new-array))
         (states (let ((list (%new-array)))
                   (dotimes (index (max 1 sample-count))
                     (vector-push-extend (axllm/core::stream-state-impl index) list))
                   list))
         (run (axllm/core::stream-run-state-impl (or sink :null)
                                                (json-boolean buffered)
                                                (jget config "thought_field")))
         (ctx (object "run" run "committed" (object) "current" (object)
                      "version" version))
         (thought-field (jget config "thought_field"))
         (problems '())
         (handle (ax-stream client request options)))
    (unwind-protect
         (loop for chunk = (ax-stream-next handle)
               until (or (null chunk) (eq chunk :null))
               do (vector-push-extend chunk events)
                  (loop for result across (coerce (%response-results chunk) 'vector)
                        do (let ((problem (%stream-one-result generator config ctx states
                                                              thought-field result)))
                             (when problem (push problem problems)))))
      (ignore-errors (ax-stream-close handle)))
    ;; The turn is over, so Core releases what it was holding back: a field whose
    ;; value could not be shown until it was known to be complete, and the last
    ;; field, which has no following label to end it.  Without this the final
    ;; field of every streamed turn silently never reaches the sink.
    ;;
    ;; Core separates two outcomes here and so must this.  A value the model got
    ;; wrong is something the next turn can fix, so it becomes a problem the
    ;; caller can correct; a failed assertion or field processor is not, so it
    ;; ends the run.  Collapsing the two would either fail a run that only needed
    ;; another turn, or retry something that will never come right.
    (loop for state across (coerce states 'vector)
          do (multiple-value-bind (finalized problem)
                 (%stream-correctable (lambda ()
                                        (axllm/core::stream-finalize-impl
                                         generator config ctx state)))
               (cond
                 (problem (push problem problems))
                 (t
                  (let ((failure (%present (jget finalized "failure"))))
                    (when (and failure (typep failure 'condition)) (error failure)))
                  (loop for text across (coerce (or (%present (jget finalized "feedback"))
                                                    (%new-array))
                                                'vector)
                        do (push text problems))))))
    (values (axllm/core::fold-chat-response-stream events) (nreverse problems))))

(defun %stream-one-result (generator config ctx states thought-field result)
  "Read one result out of one chunk: its finish reason, thought and text."
  (let ((finish (or (%present (jget result "finish_reason"))
                    (%present (jget result "finishReason")))))
    (when (equal finish "error")
      (error 'ax-error :message "Streaming response failed"))
    (let* ((content (%present (jget result "content")))
           (thought (%present (jget result "thought")))
           (calls (or (%present (jget result "function_calls"))
                      (%present (jget result "functionCalls"))))
           (substantive (or (and (stringp content) (plusp (length content)))
                            (and (stringp thought) (plusp (length thought)))
                            (and (vectorp calls) (not (stringp calls))
                                 (plusp (length calls)))
                            (%present (jget result "phase"))
                            (equal finish "length"))))
      (unless substantive (return-from %stream-one-result))
      (let* ((index (or (%present (jget result "index")) 0))
             (state (if (< index (length states)) (aref states index) nil)))
        (unless state
          (error 'ax-error
                 :message (format nil "No state found for result (index: ~a)" index)))
        ;; A thought arrives as its own field rather than as labelled text, so it
        ;; is joined and emitted directly.
        (when (and (stringp thought) (plusp (length thought)))
          (let ((values* (jget state "values")))
            (setf (gethash thought-field values*)
                  (concatenate 'string (or (%present (jget values* thought-field)) "")
                               thought))
            (setf (gethash "values" state) values*))
          (axllm/core::stream-emit-deltas-impl
           ctx index (vector (object thought-field thought))))
        (when (equal finish "length")
          (error 'ax-error
                 :message (format nil "Max tokens reached before completion~%Content: ~a"
                                  (concatenate 'string
                                               (or (%present (jget state "content")) "")
                                               (or (and (stringp content) content) "")))))
        (multiple-value-bind (deltas problem)
            (%stream-correctable (lambda ()
                                   (axllm/core::stream-chunk-content-impl
                                    generator config state result)))
          (when problem (return-from %stream-one-result problem))
          ;; A sink that refused, or a field processor that failed, ends the run:
          ;; neither is something a further turn could put right.
          (let ((fatal (%present (jget state "fatal_error"))))
            (when (and fatal (typep fatal 'condition)) (error fatal)))
          (axllm/core::stream-emit-deltas-impl ctx index deltas)
          nil)))))

(defun %streaming-forward-unwrapped (generator client inputs options)
  "The streamed run itself.  `program-streaming-forward' wraps a failure."
  (let* ((sink (%stream-sink options))
         (usage (usage-object 0 0 0))
         (model (%forward-option options "model" "model"))
         (control (%forward-option options "control" "control"))
         (execution-path (or (%forward-option options "executionPath" "execution_path") "root"))
         (selection (%output-selection generator client options))
         (config (%stream-config generator client options selection))
         (sample-count (or (%forward-option options "sampleCount" "sample_count")
                           (%present (jget (or options (object)) "n"))
                           1))
         (picker (%forward-option options "resultPicker" "result_picker"))
         (max-retries (%forward-budget options "maxRetries" "max_retries"
                                       (generator-max-retries generator) "maxRetries"))
         (validation-retries (%forward-budget options "validationRetries" "validation_retries"
                                             max-retries "validationRetries"))
         (history (%prompt-messages generator inputs options selection))
         (version 0)
         (completed nil))
    (%emit-control-event control "started" execution-path)
    (unwind-protect
         (loop
           (let* ((request (%chat-request generator history :auto options selection version))
                  (effective-model (if (and model (not (%blankp model)))
                                       model
                                       (%service-model client))))
             (memory-add-request (generator-memory generator) (coerce history 'vector))
             (when (plusp version)
               (memory-add-tag (generator-memory generator) "correction"))
             (multiple-value-bind (response streamed-problems)
                 (%stream-one-attempt generator client request options config
                                      sink version sample-count (and picker t))
               ;; Core checks the folded turn's calls, and raises or corrects
               ;; according to the caller's functionCallValidation.  A streamed
               ;; turn has to be held to the same contract as an unstreamed one:
               ;; a call whose shape is unusable must not reach a handler just
               ;; because the text arrived in pieces.
               (axllm/core::check-completion-function-calls response options)
               (let ((response-usage (%response-usage response)))
                 (%accumulate-usage usage response-usage)
                 (%record-usage-for generator (%service-name client) effective-model
                                    response-usage))
               (%record-chat-log generator effective-model (coerce history 'vector) response)
               (memory-add-response (generator-memory generator)
                                    (%recorded-response response))
               (multiple-value-bind (values* samples parsed-problems)
                   (%parse-outputs generator client response options)
                 ;; Core already judged the streamed text as it arrived, so its
                 ;; problems are the authority; parsing the folded turn again is
                 ;; only there to catch what a chunk reader cannot see.
                 (let ((problems (or streamed-problems parsed-problems)))
                 (cond
                   ((null problems)
                    (%check-sample-count samples options)
                    (let ((outputs (%visible-outputs
                                    generator
                                    (%pick-sample generator values* samples options))))
                      (setf completed t)
                      (axllm/core::core-axgen-memory-cleanup-corrections generator)
                      (%record-trace generator inputs outputs "ok")
                      (%emit-control-event control "completed" execution-path)
                      (return (values outputs usage))))
                   ((< version validation-retries)
                    ;; The next attempt restarts the fields at the next version, so
                    ;; a consumer replaces what it rendered rather than appending.
                    (incf version)
                    (setf history
                          (append history
                                  (list (object "role" "assistant"
                                                "content" (%response-content response))
                                        (object "role" "user"
                                                "content" (%correction-prompt problems))))))
                   (t
                    (generation-fail
                     :validation
                     (%generate-failed-text
                      (%unable-to-fix-text (%string-join " " problems)
                                           (%response-all-content response)))
                     :problems problems))))))))
      (unless completed
        (%record-trace generator inputs nil "error")
        (%emit-control-event control "failed" execution-path)))))

(defun %forward-cache-function (options)
  "The caching function for this call, through Core's boundary, or NIL."
  (let ((resolved (axllm/core::core-axgen-caching-function (or options (object)) :null)))
    (and (functionp resolved) resolved)))

(defun %forward-cache-key (generator inputs)
  "A key for INPUTS that does not depend on the order their keys were written in."
  (axllm/core::core-json-stable-stringify
   (object "signature" (signature-string (generator-signature generator))
           "program" (generator-program-id generator)
           "values" inputs)))

(defun %cache-read (cache-function key)
  "The stored output for KEY, or NIL when nothing is stored."
  (let ((hit (axllm/core::core-axgen-cache-read cache-function key)))
    (and (hash-table-p hit) hit)))

(defun %cache-write (cache-function key value)
  (axllm/core::core-axgen-cache-write cache-function key value))

(defun %function-result-message (call text)
  "One tool result as Core's chat_prompt entry.

Core's own message shape, not this port's, because the service boundary speaks
Core: a generator that built native messages would only work with one provider
implementation."
  (object "role" "function"
          "functionId" (jget call "id")
          "name" (jget call "name")
          "result" text))

(defun %emit-control-event (control type path)
  "Tell CONTROL that this program's run reached TYPE at PATH.

A run control is how a caller watches or stops nested work, so a program that
runs as a node reports its own lifecycle at its own path rather than letting the
flow around it speak for it."
  (when (and control (not (eq control :null)))
    (axllm/core::core-host-call control "_emit"
                                (vector (object "type" type "path" path))))
  control)

(defun %regenerate-failure (condition text)
  "CONDITION with TEXT as its message, keeping what a handler needs to catch it.

A generation failure keeps its kind and its problem list, so a caller still sees
which stage gave up and why; anything else is rewrapped by the provider layer,
which is where that condition's details live."
  (if (typep condition 'generation-error)
      (make-condition 'generation-error
                      :kind (generation-error-kind condition)
                      :problems (generation-error-problems condition)
                      :message text)
      (or (ignore-errors (axllm/core::core-exception-rewrap condition text))
          condition)))

(defmethod forward ((generator generator) client inputs &optional (options (object)))
  "Run GENERATOR against CLIENT with INPUTS (a hash table with string keys).

Returns (values outputs usage): OUTPUTS maps output field names to typed
values, USAGE is this call's accumulated usage object.  INPUTS is never mutated
and the conversation history is built locally, so a correction turn cannot
replay a tool call."
  (unless (hash-table-p inputs)
    (generation-fail :config "forward: inputs must be a hash table with string keys."))
  (unless (or (null options) (eq options :null) (hash-table-p options))
    (generation-fail :config "forward: options must be a JSON object."))
  ;; The cache read precedes generation's error boundary, as in TypeScript.
  ;; Store failures remain Core-owned and do not fail a successful generation.
  (let* ((options (copy-runtime-options options))
         (lookup (axllm/core::cache-lookup-impl generator inputs options false)))
    (when (json-true-p (jget lookup "hit"))
      (return-from forward
        (values (axllm/core::render-audio-outputs-impl generator client (jget lookup "value") options)
                (usage-object 0 0 0))))
    (setf (gethash "_ax_cache_lookup" options) lookup)
    (%as-generate-failure
     (%core-generation-run generator client inputs options nil))))

(defvar *generation-client* nil)
(defvar *generation-options* nil)
(defvar *generation-usage* nil)

(defun %record-core-turn (gen request response)
  "Adapt Core's completed turn to Lisp's logs and per-call usage."
  (let* ((usage (%response-usage response))
         (model (or (%present (jget request "model"))
                    (%service-model *generation-client*))))
    (let ((ids (make-hash-table :test #'equal)))
      (loop for call across (%response-tool-calls response) do
        (let ((id (%present (jget call "id"))))
          (when (and id (gethash id ids))
            (generation-fail :tool (format nil "The provider reused tool call id '~a'." id)))
          (when id (setf (gethash id ids) t)))))
    (when *generation-usage* (%accumulate-usage *generation-usage* usage))
    (%record-usage gen *generation-client* model usage)
    (%record-chat-log gen model (jget request "chat_prompt") response)
    (let ((metadata (%present (jget request "provider_metadata"))))
      (when metadata
        (setf (gethash "providerMetadata" (car (last (generator-chat-log-entries gen)))) metadata)))
    (when (zerop (length (%response-tool-calls response)))
      (%check-sample-count (coerce (%response-results response) 'vector)
                           *generation-options*))))

;;; Native session host boundary. Providers expose a bound open_chat_session
;;; callback through CORE-HOST-GET. Sessions expose next, submit, continue,
;;; steer, thinking and close through the same method-table/host protocol used
;;; by telemetry-call. Core owns response identity, budgets, tool validation,
;;; continuation decisions, formatting, and reconciliation with streamed fields.
(defclass native-gen-service (boundary-service)
  ((generator :initarg :generator :reader native-gen-generator)
   (streaming :initarg :streaming :reader native-gen-streaming)))

(defmethod ax-take-control-updates ((service native-gen-service)) #())
(defmethod ax-pending-control-count ((service native-gen-service)) 0)

(defun %native-gen-start-tool (service state raw-call)
  (let* ((gen (native-gen-generator service))
         (call (axllm/core::chat-session-normalize-call raw-call))
         (id (jget call "id"))
         (name (jget (jget call "function") "name"))
         (tool (gethash name (generator-tool-index gen)))
         (options (%boundary-options service))
         (result :null) (ok nil))
    (when (equal name "__axOutput")
      (axllm/core::chat-session-defer-final-call state call)
      (return-from %native-gen-start-tool))
    (when (nth-value 1 (gethash id (jget state "pending")))
      (return-from %native-gen-start-tool))
    (axllm/core::chat-session-register-call state call "blocking")
    (handler-case
        (let* ((raw (jget (jget call "function") "params"))
               (args (if (stringp raw) (parse-json raw) raw)))
          (unless tool (error "Function '~a' not found" name))
          (let ((fixing (%present (axllm/core::chat-session-tool-argument-error
                                  name (jget tool "parameters") args))))
            (if fixing (setf result fixing)
                (progn
                  (setf (gethash "params" call) args
                        (gethash "params" (jget call "function")) args)
                  (%emit-control-event (boundary-control service) "tool.started" (boundary-path service))
                  (setf result (axllm/core::core-tool-invoke
                                tool args (axllm/core::tool-call-extras options name))
                        ok t)
                  (%emit-control-event (boundary-control service) "tool.completed" (boundary-path service))))))
      (error (condition)
        (setf result (jget (axllm/core::tool-error-message-impl call condition) "result"))))
    ;; A synchronous tool can settle immediately, but its result is committed
    ;; only after the assistant turn that requested it, just like queued workers.
    (lambda ()
      (axllm/core::chat-session-record-result gen state call result (if ok true false) options))))

(defun %native-gen-stream (service request options)
  (let* ((gen (native-gen-generator service))
         (run-options (%boundary-options service))
         (control (boundary-control service))
         (path (boundary-path service))
         (opener (axllm/core::core-host-get (boundary-inner service) "open_chat_session"))
         (session (funcall opener request (axllm/core::core-map-merge run-options options)))
         (limit (- (%forward-budget run-options "maxSteps" "max_steps" 25 "maxSteps")
                   (jget request "_ax_step_index" 0)))
         (state (axllm/core::chat-session-create-state
                 (axllm/core::core-get session "model" (%service-model (boundary-inner service))) path limit))
         (updates-after 0) (boundary-updates nil) (last-response nil)
         (pending nil) (closed nil) (final-p nil))
    (labels ((close-session ()
               (unless closed
                 (setf closed t)
                 (unwind-protect (telemetry-call session "close")
                   (axllm/core::chat-session-record-unresolved gen state)
                   (axllm/core::chat-session-close-state state))))
             (item (kind response-id &optional response)
               (let ((info (object "type" kind "response_id" response-id
                                   "turns" (jget state "turns"))))
                 (when (equal kind "partial")
                   (setf (gethash "calls_started" info)
                         (json-boolean (plusp (hash-table-count (jget state "pending")))))
                   (setf (gethash "pending_calls" info) (axllm/core::chat-session-unresolved state)))
                 (axllm/core::core-map-merge (or response (object)) (object "session" info))))
             (apply-updates ()
               (when control
                 (loop for update across (axllm/core::core-host-call
                                          control "pending" (vector path updates-after)) do
                   (setf updates-after (jget update "id"))
                   (when (json-true-p (axllm/core::chat-session-queue-update state update))
                     (let ((timing (if (equal (jget update "type") "steer")
                                       (telemetry-call session "steer" (jget update "text"))
                                       (telemetry-call session "thinking" (jget update "level")))))
                       (if (equal timing "native")
                           (axllm/core::chat-session-native-update state updates-after)
                           (push updates-after boundary-updates)))))))
             (advance ()
               (when (%control-aborted-p control)
                 (provider-fail :aborted "Run aborted during chat session"))
               (apply-updates)
               (let* ((event (telemetry-call session "next"))
                      (type (jget event "type"))
                      (id (jget event "response_id"))
                      (response (jget event "response"))
                      (ready nil))
                 (unless (%present event)
                   (generation-fail :session "Session disconnected; work was not replayed"))
                 (cond
                   ((equal type "tool.call")
                    (push (%native-gen-start-tool service state (jget event "call")) ready))
                   ((equal type "response")
                    (axllm/core::chat-session-observe-output gen state event)
                    (when (native-gen-streaming service)
                      (push (item "partial" id response) pending)))
                   ((equal type "steering") (axllm/core::chat-session-native-event state event))
                   ((equal type "response.completed")
                    (when (json-true-p (axllm/core::chat-session-complete-response state id))
                      (setf last-response response)
                      (let ((completion (axllm/core::chat-session-completion response id)))
                        (loop for call across (%response-tool-calls completion) do
                          (push (%native-gen-start-tool service state call) ready))
                        (when (json-true-p (axllm/core::chat-session-has-continuation-work state))
                          (axllm/core::chat-session-record-response gen state request completion)))
                      (when (native-gen-streaming service)
                        (push (item "completed" id) pending))))
                   (t (generation-fail :session (format nil "Unknown session event: ~a" type))))
                 (dolist (commit (nreverse ready)) (when commit (funcall commit))))
               (let* ((action (axllm/core::chat-session-boundary-action state))
                      (type (jget action "type")))
                 (cond
                   ((member type '("submit" "continue") :test #'equal)
                    (when (>= (jget state "steps") (jget state "max_steps"))
                      (error (axllm/core::chat-session-step-limit-error state)))
                    (let ((results (jget action "results" #())))
                      (when (plusp (length results)) (telemetry-call session "submit" results))
                      (telemetry-call session "continue")
                      (axllm/core::chat-session-mark-submitted
                       state (map 'vector (lambda (result) (jget result "function_id")) results)))
                    (dolist (id (reverse boundary-updates))
                      (axllm/core::chat-session-transition state (object "type" "update.applied" "id" id))
                      (%emit-control-event control "applied" path))
                    (setf boundary-updates nil))
                   ((and (equal type "validate") last-response)
                    (let ((response (axllm/core::chat-session-final-result state last-response)))
                      (push (if (native-gen-streaming service)
                                (item "final" (jget state "response_id") response) response) pending))
                    (setf final-p t))))
               (setf pending (nreverse pending)))
             (next ()
               (handler-case
                   (loop
                     (when pending (return (pop pending)))
                     (when (or closed final-p) (close-session) (return :null))
                     (advance))
                 (error (condition) (close-session) (error condition)))))
      (make-ax-stream-handle #'next :closer #'close-session))))

(defmethod ax-stream ((service native-gen-service) request &optional options)
  (%native-gen-stream service request options))

(defmethod ax-chat ((service native-gen-service) request &optional options)
  (let ((handle (%native-gen-stream service request options)))
    (unwind-protect (ax-stream-next handle) (ax-stream-close handle))))

(defun %core-generation-run (gen client inputs options streaming)
  "Execute Core's generation state machine; Lisp owns only host boundaries."
  (let* ((*generation-client* client)
         (*generation-options* (%runtime-options gen options))
         (*generation-usage* (usage-object 0 0 0))
         (*generation-stop-tag* (gensym "GENERATION-STOP"))
         (*generation-open-streams* nil)
         (started-at (get-internal-real-time))
         (globals (let ((frame (get-runtime-hook-frame options)))
                    (if (typep frame 'runtime-hook-frame)
                        (runtime-hook-frame-globals frame) (globals-snapshot))))
         (original-memory (generator-memory gen))
         (control (%present (jget *generation-options* "control")))
         (path (or (%forward-option *generation-options* "executionPath" "execution_path") "root"))
         (completed nil) (stopped nil))
    (dolist (entry '(("maxSteps" "max_steps") ("maxRetries" "max_retries")
                     ("validationRetries" "validation_retries") ("infraRetries" "infra_retries")))
      (%forward-budget *generation-options* (first entry) (second entry) 0 (first entry)))
    (setf options (copy-runtime-options options))
    (setf (gethash "customLabels" options) (jget *generation-options* "customLabels"))
    (let ((timeout (%present (jget *generation-options* "timeout"))))
      (when (and timeout (not (%present (jget *generation-options* "timeoutMs"))))
        (setf (gethash "timeoutMs" options) timeout)))
    (when (json-true-p (%forward-option *generation-options* "freshMemory" "fresh_memory"))
      (setf (slot-value gen 'memory) (make-instance 'memory)))
    (cond
      ((and (json-true-p (axllm/core::chat-session-mode-enabled *generation-options*))
            (functionp (axllm/core::core-host-get client "open_chat_session"))
            (json-true-p (jget (ax-features client) "asyncTools"))
            control)
       (setf client (make-instance 'native-gen-service :inner client :control control
                                   :path path :options *generation-options* :generator gen :streaming streaming))
       ;; A native session must not be replayed by infrastructure retry.
       (setf (gethash "infraRetries" options) 0 (gethash "infra_retries" options) 0))
      (control
      ;; The existing request-boundary adapter owns control routing and applies
      ;; Core's queued updates. Instantiate without its public started event:
      ;; this generation already reports its lifecycle below.
      (setf client (make-instance 'boundary-service :inner client :control control
                                  :path path :options *generation-options*))))
    (%emit-control-event control "started" path)
    (unwind-protect
         (progn
           (let ((token (%call-cancellation *generation-options*)))
             (when (cancelled-p token)
               (provider-fail :aborted (format nil "Request aborted: ~a" (cancellation-reason token)))))
           (let ((output (catch *generation-stop-tag* (if streaming
                           (axllm/core::streaming-forward-impl
                            gen client inputs options (or (%stream-sink options) :null))
                           (axllm/core::forward-impl gen client inputs options)))))
           (when (eq output :consumer-stopped)
             (setf stopped t output (object)))
           (setf completed t)
           (%emit-control-event control (if stopped "aborted" "completed") path)
           (values output *generation-usage*)))
      ;; Core closes ordinary/error paths. Lisp also has nonlocal exits (THROW,
      ;; RETURN-FROM), which must release outstanding pull handles as well.
      (dolist (handle (copy-list *generation-open-streams*))
        (ignore-errors (ax-stream-close handle)))
      (setf (slot-value gen 'memory) original-memory)
      (unless completed
        (%record-trace gen inputs nil "error")
        (%emit-control-event control "failed" path))
      ;; Instrumentation must never replace the operation's result or failure.
      ;; Cache hits return before entering this scope, matching TS forward.
      (handler-case
          (let* ((service-options (ax-options *generation-client*))
                 (meter (or (%present (jget *generation-options* "meter"))
                            (%present (jget service-options "meter"))
                            (%present (jget globals "meter"))))
                 (instruments (if meter (get-or-create-gen-metrics-instruments meter) :null)))
            (when (and (%present instruments)
                       (json-true-p (jget (get-metrics-config) "enabled")))
              (record-generation-metric
               instruments (* 1000d0 (/ (- (get-internal-real-time) started-at)
                                        internal-time-units-per-second))
               (if (and completed (not stopped)) true false)
               :signature-name (or (generator-description gen) "unknown_signature")
               :ai-service (%service-name *generation-client*)
               :model (%present (jget *generation-options* "model"))
               :custom-labels (merge-custom-labels (jget globals "customLabels")
                                                   (jget service-options "customLabels")
                                                   (jget *generation-options* "customLabels")))))
        (error () nil)))))

(defun %forward-unwrapped (generator client inputs options)
  "The run itself.  `forward' wraps a failure; this produces one."
  ;; The tools reach the request through Core, which reads them off the program,
  ;; so this no longer carries its own copy of them.
  (let* ((processor (generator-function-processor generator))
         ;; A caller that runs several attempts of the same program wants each
         ;; one to start from nothing, so it can read back one attempt's turns
         ;; without the earlier attempts in the way.  The program's chat log,
         ;; usage and traces still accumulate across attempts: those are the
         ;; program's history, not the attempt's conversation.
         (memory (if (json-true-p (%forward-option options "freshMemory" "fresh_memory"))
                     (make-instance 'memory)
                     (generator-memory generator)))
         (model (%forward-option options "model" "model"))
         ;; What the request will actually name, so the usage and the chat log
         ;; agree with where the turn was sent.
         (effective-model (if (and model (not (%blankp model)))
                              model
                              (%service-model client)))
         (control (%forward-option options "control" "control"))
         (execution-path (or (%forward-option options "executionPath" "execution_path") "root"))
         (cache-function (%forward-cache-function options))
         (cache-key (and cache-function (%forward-cache-key generator inputs)))
         ;; The rung is chosen once for the run, from what the service advertises,
         ;; and travels with every request of it.
         (selection (%output-selection generator client options))
         (max-steps (%forward-budget options "maxSteps" "max_steps"
                                     (generator-max-steps generator) "maxSteps"))
         (max-retries (%forward-budget options "maxRetries" "max_retries"
                                       (generator-max-retries generator) "maxRetries"))
         ;; The validation half of the budget can be set on its own, for a caller
         ;; that wants a confused model corrected fewer times than an unhealthy
         ;; service is retried, or the other way round.
         (validation-retries (%forward-budget options "validationRetries" "validation_retries"
                                             max-retries "validationRetries"))
         (history (%prompt-messages generator inputs options selection))
         (usage (usage-object 0 0 0))
         (seen-call-ids (make-hash-table :test #'equal))
         (steps 0)
         (retries 0)
         (max-calls (+ 1 max-steps max-retries))
         (calls 0)
         (correcting nil)
         (completed nil))
    (when cache-function
      (let ((hit (%cache-read cache-function cache-key)))
        (when hit
          ;; A stored output is the run: it records no usage and no trace,
          ;; because nothing ran.
          (return-from %forward-unwrapped (values hit usage)))))
    (%emit-control-event control "started" execution-path)
    (unwind-protect
         (loop
           (when (> (incf calls) max-calls)
             (generation-fail :steps
                              (format nil "Exceeded the provider call budget of ~a request(s)."
                                      max-calls)))
           (memory-add-request memory (coerce history 'vector))
           ;; A correction turn is part of how the run got there, not part of
           ;; what it did, so it is tagged and dropped once the run succeeds.
           (when correcting (memory-add-tag memory "correction"))
           (let* ((request-history (coerce history 'vector))
                  ;; Core's shape is kept as it arrives.  Flattening it to one
                  ;; completion here would discard the other samples of a
                  ;; multi-sample turn before anything could choose between them,
                  ;; so the readers below are shape-neutral instead.
                  (response (multiple-value-bind (answer used)
                                (%chat-with-infra-retry
                                 client
                                 (%chat-request generator request-history
                                                (if correcting :none :auto)
                                                options selection steps)
                                 options
                                 (max 0 (- max-retries retries)))
                              (incf calls used)
                              answer))
                  (content (%response-content response))
                  (tool-calls (%response-tool-calls response))
                  (response-usage (%response-usage response)))
             ;; A truncated completion is a failed run, not a short answer: the
             ;; model was cut off mid-sentence and the output cannot be trusted.
             ;; Core decides what counts as truncated.
             (let ((cut (ignore-errors (axllm/core::max-tokens-error-impl response))))
               (when (and cut (not (eq cut :null)) (typep cut 'condition))
                 (error cut)))
             (multiple-value-bind (served-by served-model)
                 (%response-attribution response (%service-name client) effective-model)
               (%accumulate-usage usage response-usage)
               (%record-usage-for generator served-by served-model response-usage)
               (%record-chat-log generator served-model request-history response))
             (memory-add-response memory (%recorded-response response))
             (cond
               ((and tool-calls (plusp (length tool-calls)))
                ;; A tool call during a correction turn is allowed: a model told
                ;; its output was wrong may legitimately need another lookup to
                ;; fix it, and the step budget is what bounds that, not a ban.
                (setf correcting nil)
                (when (>= steps max-steps)
                  (generation-fail :steps
                                   (format nil "Exceeded the tool step budget of ~a round(s)."
                                           max-steps)))
                (incf steps)
                (let ((call-list (coerce tool-calls 'list))
                      (results '()))
                  ;; The whole batch is checked first.  A handler runs only once
                  ;; every call in the batch is known to be executable, so a
                  ;; malformed later call cannot leave an earlier tool's effect
                  ;; behind with no way to report it.
                  (%validate-tool-batch call-list seen-call-ids)
                  (dolist (call call-list)
                    (progn
                      (handler-case
                          ;; The parsed arguments go to the handler unchanged: a
                          ;; JSON array, null or number must be rejected by the
                          ;; validator, never coerced into an empty object that
                          ;; a zero-argument handler would accept.
                          (multiple-value-bind (text raw arguments)
                              (execute-function-with-details processor call)
                            (declare (ignore raw))
                            (%record-function-call generator call arguments text "ok")
                            (push (%function-result-message call text) results))
                        (function-call-error (condition)
                          ;; A wrong name, bad arguments or a failing backend is
                          ;; the model's next turn, not the end of the run.
                          (let ((text (%tool-error-text (ax-error-message condition))))
                            (%record-function-call generator call nil text "error")
                            (push (%function-result-message call text) results))))))
                  (setf results (nreverse results))
                  (memory-add-function-results memory (coerce results 'vector))
                  (setf history (append history
                                        (list (object "role" "assistant"
                                                      "content" content
                                                      "functionCalls" tool-calls))
                                        results))))
               (t
                (multiple-value-bind (values* samples problems)
                    (%parse-outputs generator client response options)
                  (cond
                    ((null problems)
                     (%check-sample-count samples options)
                     (let ((outputs (%visible-outputs
                                     generator
                                     (%pick-sample generator values* samples options))))
                       (setf completed t)
                       (axllm/core::core-axgen-memory-cleanup-corrections generator)
                       (%record-trace generator inputs outputs "ok")
                       (when cache-function (%cache-write cache-function cache-key outputs))
                       (%emit-control-event control "completed" execution-path)
                       (return (values outputs usage))))
                    ((< retries validation-retries)
                     (incf retries)
                     (setf correcting t)
                     (setf history (append history
                                           (list (object "role" "assistant" "content" content)
                                                 (object "role" "user"
                                                         "content" (%correction-prompt problems))))))
                    (t
                     ;; Core's own wording, so a cross-port failure report reads
                     ;; the same: the cause names the field that could not be
                     ;; fixed, and the wrapper says the run failed.
                     (generation-fail
                      :validation
                      (%generate-failed-text
                       (%unable-to-fix-text (%string-join " " problems)
                                            (%response-all-content response)))
                      :problems problems))))))))
      ;; A run that failed is still a run: the trace records what was tried so
      ;; an optimizer can score the failure instead of losing it.
      (unless completed
        (%record-trace generator inputs nil "error")
        (%emit-control-event control "failed" execution-path)))))
