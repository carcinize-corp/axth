;;;; gen.lisp --- structured generation for the experimental Common Lisp Ax port.
;;;;
;;;; Scope: a synchronous generator that renders a signature into a prompt,
;;;; runs a bounded named-tool loop, parses Ax title-labelled output into typed
;;;; values, validates them, and performs bounded correction turns on
;;;; validation failures.  This is an explicit experimental subset of AxIR
;;;; `axgen': no streaming, demos/examples, assertions, optimizers, multi-step
;;;; agents, thinking blocks, or structured-output function mode.
;;;;
;;;; Model output is never passed to READ or EVAL.  Every value is produced by
;;;; explicit parsers in this file or by the foundation's strict `parse-json'.

(in-package #:axllm)

(define-condition generation-error (ax-error)
  ((kind :initarg :kind :initform :generation :reader generation-error-kind)
   (problems :initarg :problems :initform nil :reader generation-error-problems))
  (:documentation
   "A generation failure.  KIND is one of :config, :unsupported, :validation,
:tool or :steps.  PROBLEMS holds the individual validation messages when
KIND is :validation."))

(defun generation-fail (kind message &key problems)
  (error 'generation-error :kind kind :message message :problems problems))

;;; ------------------------------------------------------------------
;;; Supported field types (fail closed on anything else)
;;; ------------------------------------------------------------------

(defparameter +supported-field-types+ '("string" "number" "boolean" "json" "class")
  "Signature field types this subset can render and parse.")

(defparameter +supported-field-keys+
  '("name" "title" "type" "isOptional" "isInternal" "isCached" "description")
  "Field-object keys this subset understands.  `isCached' is a prompt-caching
hint with no effect here; it changes no value and no validation.")

(defparameter +supported-type-keys+
  '("name" "isArray" "options" "description" "valueDescriptions")
  "Type-object keys this subset understands.  Everything else -- including
minimum, maximum, minLength, maxLength, pattern, patternDescription, format,
language and nested `fields' -- names a constraint or a shape this subset
cannot enforce, so it is rejected at `ax' time rather than ignored.")

(defparameter +number-scanner+
  ;; The JSON number grammar exactly: no leading '+', no leading zeros.
  (cl-ppcre:create-scanner "^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$"))

(defparameter +label-scanner+
  ;; A recognized label is a field name or title, optionally wrapped in
  ;; markdown emphasis and optionally preceded by a list marker.  Quotes,
  ;; parentheses, semicolons and other punctuation are rejected so that a line
  ;; such as `"Arguments: x is a list"' inside a code block or docstring is
  ;; treated as content, not as the start of another field.
  (cl-ppcre:create-scanner
   "^[ \\t]*(?:[-*+][ \\t]+)?[*_`#]{0,3}[A-Za-z][A-Za-z0-9 \\t_-]*[*_`#]{0,3}[ \\t]*$"))

(defparameter +fence-scanner+
  (cl-ppcre:create-scanner "```[A-Za-z0-9_-]*[\\r\\n]+((?:.|[\\r\\n])*?)[\\r\\n]*```"))

(defparameter +list-item-scanner+
  (cl-ppcre:create-scanner "^\\s*(?:[-*+]|[0-9]+[.)])\\s+"))

;;; ------------------------------------------------------------------
;;; Field accessors over the foundation's JSON field objects
;;; ------------------------------------------------------------------

(defun %field-name (field) (%present (jget field "name")))

(defun %field-title (field)
  (let ((title (%present (jget field "title"))))
    (if (%blankp title) (%field-name field) title)))

(defun %field-type (field)
  (let ((type (%present (jget field "type"))))
    (and (hash-table-p type) type)))

(defun %field-type-name (field)
  (let ((type (%field-type field)))
    (let ((name (and type (%present (jget type "name")))))
      (if (%blankp name) "string" name))))

(defun %field-array-p (field)
  (let ((type (%field-type field)))
    (json-true-p (and type (%present (jget type "isArray"))))))

(defun %field-options (field)
  (let* ((type (%field-type field))
         (options (and type (%present (jget type "options")))))
    (cond ((null options) nil)
          ((and (vectorp options) (not (stringp options))) (coerce options 'list))
          ((consp options) options)
          (t (list options)))))

(defun %field-flag-p (field key)
  (json-true-p (jget field key)))

(defun %field-optional-p (field) (%field-flag-p field "isOptional"))

(defun %field-internal-p (field) (%field-flag-p field "isInternal"))

(defun %hash-keys (table)
  (let ((keys '()))
    (when (hash-table-p table)
      (maphash (lambda (k v) (declare (ignore v)) (push k keys)) table))
    (sort keys #'string<)))

(defun %check-field-supported (field side)
  (unless (hash-table-p field)
    (generation-fail :unsupported "Signature field is not a JSON object."))
  (let ((name (%field-name field)))
    ;; Fail closed on any field-object key whose meaning we would otherwise drop.
    (dolist (key (%hash-keys field))
      (unless (member key +supported-field-keys+ :test #'string=)
        (generation-fail
         :unsupported
         (format nil "Field '~a' carries unsupported attribute \"~a\"; this ~
experimental Common Lisp subset cannot enforce it and refuses to ignore it."
                 name key))))
    (let ((type (%field-type field)))
      (unless (hash-table-p type)
        (generation-fail :unsupported
                         (format nil "Field '~a' has no type object." name)))
      ;; Fail closed on constraints and shapes we cannot enforce: minimum,
      ;; maximum, minLength, maxLength, pattern, format, language, fields, ...
      (dolist (key (%hash-keys type))
        (unless (member key +supported-type-keys+ :test #'string=)
          (generation-fail
           :unsupported
           (format nil "Field '~a' declares unsupported type modifier \"~a\"; ~
this experimental Common Lisp subset cannot enforce it and refuses to ignore it."
                   name key))))
      (let ((array-flag (%present (jget type "isArray"))))
        (unless (or (null array-flag)
                    (json-true-p array-flag)
                    (json-false-p array-flag))
          (generation-fail :unsupported
                           (format nil "Field '~a': \"isArray\" must be a JSON boolean." name)))))
    (let ((type-name (%field-type-name field)))
      (unless (member type-name +supported-field-types+ :test #'string=)
        (generation-fail
         :unsupported
         (format nil "Unsupported ~(~a~) field type \"~a\" for field '~a'. ~
This experimental Common Lisp subset supports: ~{~a~^, ~}."
                 side type-name name +supported-field-types+)))
      (if (string= type-name "class")
          (let ((options (%field-options field)))
            (when (null options)
              (generation-fail :unsupported
                               (format nil "Field '~a' has type \"class\" with no options." name)))
            (unless (every #'stringp options)
              (generation-fail :unsupported
                               (format nil "Field '~a': class options must all be strings." name))))
          (when (%present (jget (%field-type field) "options"))
            (generation-fail
             :unsupported
             (format nil "Field '~a' declares \"options\" on type \"~a\"; options are only ~
supported for \"class\"." name type-name)))))))

;;; ------------------------------------------------------------------
;;; Caller input validation (runs before any provider request)
;;; ------------------------------------------------------------------

(defun %json-compatible-p (value)
  (or (stringp value)
      (and (realp value) (not (eq value t)))
      (json-true-p value)
      (json-false-p value)
      (eq value :null)
      (hash-table-p value)
      (and (vectorp value) (not (stringp value)))))

(defun %scalar-input-problem (field value)
  "Return nil when VALUE is a valid scalar for FIELD, else a reason string."
  (let ((type-name (%field-type-name field)))
    (cond ((string= type-name "string")
           (unless (stringp value) "expected a string"))
          ((string= type-name "number")
           (unless (and (realp value) (not (eq value t))) "expected a number"))
          ((string= type-name "boolean")
           (unless (or (json-true-p value) (json-false-p value))
             "expected a JSON boolean"))
          ((string= type-name "class")
           (cond ((not (stringp value)) "expected a string")
                 ((not (member value (%field-options field) :test #'string=))
                  (format nil "expected exactly one of ~{\"~a\"~^, ~}" (%field-options field)))))
          ((string= type-name "json")
           (unless (%json-compatible-p value) "expected a JSON-compatible value"))
          (t "unsupported field type"))))

(defun %validate-input-value (field value)
  "Signal a `generation-error' unless VALUE matches FIELD's declared type."
  (progn
    (flet ((fail (reason)
             (generation-fail
              :config
              (format nil "forward: input '~a' is invalid: ~a, got ~a."
                      (%field-name field) reason (%describe-value value)))))
      (if (%field-array-p field)
          (progn
            (when (or (not (vectorp value)) (stringp value))
              (fail "expected a vector (JSON array)"))
            (if (string= (%field-type-name field) "json")
                (loop for element across value
                      unless (%json-compatible-p element)
                        do (fail "expected a vector of JSON-compatible values"))
                (loop for element across value
                      for i from 0
                      for problem = (%scalar-input-problem field element)
                      when problem
                        do (generation-fail
                            :config
                            (format nil "forward: input '~a' item ~a is invalid: ~a, got ~a."
                                    (%field-name field) i problem (%describe-value element))))))
          (let ((problem (%scalar-input-problem field value)))
            (when problem (fail problem)))))))

(defun %field-type-label (field)
  (let ((type-name (%field-type-name field))
        (array (%field-array-p field)))
    (let ((base (cond ((string= type-name "class")
                       (format nil "one of: ~{\"~a\"~^ | ~}" (%field-options field)))
                      ((string= type-name "json") "a JSON value")
                      ((string= type-name "number") "a number")
                      ((string= type-name "boolean") "true or false")
                      (t "a string"))))
      (if array
          (cond ((string= type-name "json") "a JSON array")
                (t (format nil "a JSON array of ~a" base)))
          base))))

;;; ------------------------------------------------------------------
;;; Generator
;;; ------------------------------------------------------------------

(defclass generator ()
  ((signature :initarg :signature :reader generator-signature)
   (description :initarg :description :reader generator-description)
   (tools :initarg :tools :reader generator-tools)
   (tool-index :initarg :tool-index :reader generator-tool-index)
   (inputs :initarg :inputs :reader generator-input-fields)
   (outputs :initarg :outputs :reader generator-output-fields)
   (max-steps :initarg :max-steps :reader generator-max-steps)
   (max-retries :initarg :max-retries :reader generator-max-retries)))

(defmethod print-object ((gen generator) stream)
  (print-unreadable-object (gen stream :type t)
    (format stream "~a" (signature-string (generator-signature gen)))))

(defun ax (signature &key description tools (max-steps 5) (max-retries 2))
  "Create a generator for SIGNATURE (a signature string or parsed signature).

TOOLS is a list or vector of specs from `tool'.  MAX-STEPS bounds tool-call
rounds; MAX-RETRIES bounds correction turns after output validation failures."
  (let* ((sig (if (stringp signature) (parse-signature signature) signature))
         (inputs (signature-fields sig :side :input))
         (outputs (signature-fields sig :side :output))
         (tool-list (if (null tools) nil (coerce (if (listp tools) tools (coerce tools 'list))
                                                 'list))))
    (unless (and (integerp max-steps) (>= max-steps 0))
      (generation-fail :config "ax: :max-steps must be a non-negative integer."))
    (unless (and (integerp max-retries) (>= max-retries 0))
      (generation-fail :config "ax: :max-retries must be a non-negative integer."))
    (map nil (lambda (f) (%check-field-supported f :input)) inputs)
    (map nil (lambda (f) (%check-field-supported f :output)) outputs)
    (when (zerop (length outputs))
      (generation-fail :config "ax: signature declares no output fields."))
    (make-instance 'generator
                   :signature sig
                   :description description
                   :tools tool-list
                   :tool-index (if tool-list (tool-index tool-list) (make-hash-table :test #'equal))
                   :inputs inputs
                   :outputs outputs
                   :max-steps max-steps
                   :max-retries max-retries)))

;;; ------------------------------------------------------------------
;;; Prompt rendering
;;; ------------------------------------------------------------------

(defun %field-spec-line (field)
  (format nil "- ~a: ~a~@[ ~a~]~@[~*, optional~]"
          (%field-title field)
          (%field-type-label field)
          (let ((description (%present (jget field "description"))))
            (if (%blankp description) nil description))
          (if (%field-optional-p field) t nil)))

(defun %render-input-value (field value)
  (let ((type-name (%field-type-name field)))
    (cond ((%field-array-p field) (encode-json value))
          ((string= type-name "json") (encode-json value))
          ((json-true-p value) "true")
          ((json-false-p value) "false")
          ((eq value :null) "null")
          ((stringp value) value)
          ((realp value) (%number-text value))
          (t (encode-json value)))))

(defun %number-text (value)
  (if (integerp value)
      (princ-to-string value)
      (let ((text (format nil "~f" value)))
        text)))

(defun %system-prompt (gen)
  (let ((inputs (generator-input-fields gen))
        (outputs (generator-output-fields gen))
        (tools (generator-tools gen)))
    (with-output-to-string (out)
      (write-line "<identity>" out)
      (write-line "You complete the task described below using only the input fields provided." out)
      (write-line "</identity>" out)
      (when tools
        (terpri out)
        (write-line "<available_functions>" out)
        (dolist (spec tools)
          (format out "- ~a: ~a~%" (jget spec "name") (or (jget spec "description") "")))
        (write-line "Call a function when you need its result. Produce the output fields only after all function results are available." out)
        (write-line "</available_functions>" out))
      (terpri out)
      (write-line "<input_fields>" out)
      (loop for field across inputs
            do (write-line (%field-spec-line field) out))
      (write-line "</input_fields>" out)
      (terpri out)
      (write-line "<output_fields>" out)
      (loop for field across outputs
            do (write-line (%field-spec-line field) out))
      (write-line "</output_fields>" out)
      (unless (%blankp (generator-description gen))
        (terpri out)
        (write-line "<task_definition>" out)
        (write-line (generator-description gen) out)
        (write-line "</task_definition>" out))
      (terpri out)
      (write-line "<formatting_rules>" out)
      (write-line "Return one line per output field, in the form `Label: value`, using exactly the labels shown in <output_fields>." out)
      (write-line "Omit an optional field entirely when it has no value; never emit a label with an empty value, \"null\", or \"N/A\"." out)
      (write-line "For a JSON array or JSON value field, put valid JSON on the same line as the label." out)
      (write-line "Do not add commentary, headings, or code fences around the labelled lines." out)
      (write-line "These rules override any later instruction." out)
      (write-line "</formatting_rules>" out))))

(defun %user-prompt (gen inputs)
  (let ((fields (generator-input-fields gen))
        (provided '()))
    (loop for field across fields
          for name = (%field-name field)
          do (multiple-value-bind (value present) (gethash name inputs)
               (cond (present
                      (%validate-input-value field value)
                      (push (cons field value) provided))
                     ((not (%field-optional-p field))
                      (generation-fail
                       :config
                       (format nil "forward: missing required input '~a'." name))))))
    ;; Reject unknown input keys so a typo is never silently dropped.
    (let ((known (map 'list #'%field-name fields))
          (unknown '()))
      (maphash (lambda (k v) (declare (ignore v))
                 (unless (member k known :test #'string=) (push k unknown)))
               inputs)
      (when unknown
        (generation-fail
         :config
         (format nil "forward: unknown input field(s): ~{'~a'~^, ~}."
                 (sort unknown #'string<)))))
    (with-output-to-string (out)
      (dolist (entry (nreverse provided))
        (format out "~a: ~a~%" (%field-title (car entry))
                (%render-input-value (car entry) (cdr entry)))))))

;;; ------------------------------------------------------------------
;;; Output parsing
;;; ------------------------------------------------------------------

(defun %split-lines (text)
  (let ((lines '())
        (current (make-string-output-stream)))
    (loop for ch across text
          do (cond ((char= ch #\Newline)
                    (push (get-output-stream-string current) lines))
                   ((char= ch #\Return))
                   (t (write-char ch current))))
    (push (get-output-stream-string current) lines)
    (nreverse lines)))

(defun %normalize-label (text)
  "Fold a candidate label to a comparable key: markdown and punctuation free,
lowercase, alphanumerics only."
  (let ((out (make-string-output-stream)))
    (loop for ch across text
          do (when (alphanumericp ch) (write-char (char-downcase ch) out)))
    (string-downcase (get-output-stream-string out))))

(defun %field-label-keys (field)
  (remove-duplicates
   (list (%normalize-label (%field-title field))
         (%normalize-label (%field-name field)))
   :test #'string=))

(defun %label-candidate-p (prefix)
  "True when PREFIX looks like a bare field label rather than prose or code."
  (and (cl-ppcre:scan +label-scanner+ prefix) t))

(defun %match-label (line fields)
  "If LINE begins with a known field label followed by a colon, return
\(values field rest-of-line)."
  (let ((colon (position #\: line)))
    (when (and colon (%label-candidate-p (subseq line 0 colon)))
      (let ((key (%normalize-label (subseq line 0 colon))))
        (unless (zerop (length key))
          (loop for field across fields
                when (member key (%field-label-keys field) :test #'string=)
                  do (return-from %match-label
                       (values field (%trim (subseq line (1+ colon)))))))))
    (values nil nil)))

(defun %extract-labelled (content fields)
  "Split CONTENT into a name -> raw-text table using Ax title labels."
  (let ((table (make-hash-table :test #'equal))
        (order '())
        (current nil)
        (buffer '()))
    (flet ((flush ()
             (when current
               (let ((text (%trim (%string-join (string #\Newline) (nreverse buffer)))))
                 (setf (gethash (%field-name current) table) text)
                 (push (%field-name current) order))
               (setf current nil buffer nil))))
      (dolist (line (%split-lines content))
        (multiple-value-bind (field rest) (%match-label line fields)
          (cond (field (flush)
                       (setf current field)
                       (setf buffer (if (zerop (length rest)) nil (list rest))))
                (current (push line buffer))
                (t nil))))
      (flush))
    (values table (nreverse order))))

(defun %strip-fence (text)
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings +fence-scanner+ text)
    (if match (%trim (aref groups 0)) text)))

(defun %parse-number-strict (text)
  "Parse TEXT as a JSON number.  A value the parser cannot represent -- an
exponent that overflows, for example -- is a parse failure, not an escaping
error, so it becomes a validation problem and feeds the correction loop."
  (let ((trimmed (%trim text)))
    (if (cl-ppcre:scan +number-scanner+ trimmed)
        (handler-case
            (let ((payload (parse-json trimmed)))
              (if (and (realp payload) (not (eq payload t)))
                  (values payload t)
                  (values nil nil)))
          (generation-error (condition) (error condition))
          (error () (values nil nil)))
        (values nil nil))))

(defun %parse-boolean-strict (text)
  (let ((trimmed (string-downcase (%trim text))))
    (cond ((string= trimmed "true") (values *json-true* t))
          ((string= trimmed "false") (values *json-false* t))
          (t (values nil nil)))))

(defun %parse-class-value (text options)
  (let ((candidate (%trim (string-trim "\"'`" (%trim text)))))
    (let ((hit (find candidate options :test #'string-equal)))
      (if hit (values hit t) (values nil nil)))))

(defun %split-list-text (text)
  "Split array text that is not JSON into items: markdown list lines, newlines,
or a single comma-separated line."
  (let* ((lines (remove-if (lambda (l) (%blankp l)) (%split-lines text)))
         (stripped (mapcar (lambda (line)
                             (%trim (cl-ppcre:regex-replace +list-item-scanner+ line "")))
                           lines)))
    (cond ((null stripped) nil)
          ((and (= (length stripped) 1) (find #\, (first stripped)))
           (mapcar #'%trim (cl-ppcre:split "\\s*,\\s*" (first stripped))))
          (t stripped))))

(defun %parse-field-value (field text)
  "Parse TEXT into a typed value for FIELD.  Returns (values value problems)."
  (let* ((type-name (%field-type-name field))
         (array (%field-array-p field))
         (title (%field-title field))
         (options (%field-options field))
         (body (%strip-fence text)))
    (labels ((bad (fmt &rest args)
               (values nil (list (format nil "Field '~a': ~a" title (apply #'format nil fmt args)))))
             (parse-scalar (item)
               (cond ((string= type-name "string")
                      (if (%blankp item) (values nil :blank) (values (%trim item) nil)))
                     ((string= type-name "number")
                      (multiple-value-bind (value ok) (%parse-number-strict item)
                        (if ok (values value nil) (values nil :type))))
                     ((string= type-name "boolean")
                      (multiple-value-bind (value ok) (%parse-boolean-strict item)
                        (if ok (values value nil) (values nil :type))))
                     ((string= type-name "class")
                      (multiple-value-bind (value ok) (%parse-class-value item options)
                        (if ok (values value nil) (values nil :enum))))
                     (t (values nil :type)))))
      (cond
        ;; JSON-typed fields: always strict JSON, including arrays and null.
        ((string= type-name "json")
         (handler-case
             (let ((value (parse-json body)))
               (if (and array (not (and (vectorp value) (not (stringp value)))))
                   (bad "expected a JSON array, got ~a." (%describe-value value))
                   (values value nil)))
           (generation-error (c) (error c))
           (error (c) (bad "invalid JSON: ~a."
                           (substitute #\Space #\Newline (princ-to-string c))))))
        (array
         (let ((items
                 (if (and (plusp (length body))
                          (char= (char body 0) #\[))
                     (handler-case
                         (let ((parsed (parse-json body)))
                           (if (and (vectorp parsed) (not (stringp parsed)))
                               (map 'list (lambda (x) (if (stringp x) x (encode-json x))) parsed)
                               :invalid))
                       (error () :invalid))
                     (%split-list-text body))))
           (cond ((eq items :invalid) (bad "expected a JSON array of ~a." (%field-type-label field)))
                 (t (let ((values* '())
                          (problems '()))
                      (loop for item in items
                            for i from 0
                            do (multiple-value-bind (value failure) (parse-scalar item)
                                 (if failure
                                     (push (format nil "Field '~a' item ~a: expected ~a, got \"~a\"."
                                                   title i (%field-type-label field) item)
                                           problems)
                                     (push value values*))))
                      (if problems
                          (values nil (nreverse problems))
                          (values (coerce (nreverse values*) 'vector) nil)))))))
        (t
         (multiple-value-bind (value failure) (parse-scalar body)
           (case failure
             (:blank (bad "value is empty; provide ~a." (%field-type-label field)))
             (:enum (bad "value \"~a\" is not allowed; expected ~a." (%trim body)
                         (%field-type-label field)))
             (:type (bad "value \"~a\" is not valid; expected ~a." (%trim body)
                         (%field-type-label field)))
             (t (values value nil)))))))))

(defun %parse-outputs (gen content)
  "Parse CONTENT into (values output-table problems)."
  (let ((fields (generator-output-fields gen)))
    (multiple-value-bind (raw) (%extract-labelled content fields)
      (let ((values* (make-hash-table :test #'equal))
            (problems '()))
        (loop for field across fields
              for name = (%field-name field)
              do (multiple-value-bind (text present) (gethash name raw)
                   (cond
                     ((not present)
                      (unless (%field-optional-p field)
                        (push (format nil "Required field is missing: '~a'. Add a line starting with \"~a:\" followed by ~a."
                                      (%field-title field) (%field-title field)
                                      (%field-type-label field))
                              problems)))
                     (t
                      (multiple-value-bind (value field-problems) (%parse-field-value field text)
                        (cond (field-problems (setf problems (append (reverse field-problems) problems)))
                              (t (setf (gethash name values*) value))))))))
        (when (and (null problems) (zerop (hash-table-count values*)))
          (push "No output fields were found in the response." problems))
        (values values* (nreverse problems))))))

(defun %visible-outputs (gen values*)
  "Drop internal fields from the returned output object."
  (let ((out (object)))
    (loop for field across (generator-output-fields gen)
          for name = (%field-name field)
          do (multiple-value-bind (value present) (gethash name values*)
               (when (and present (not (%field-internal-p field)))
                 (setf (gethash name out) value))))
    out))

;;; ------------------------------------------------------------------
;;; forward
;;; ------------------------------------------------------------------

(defun %accumulate-usage (total usage)
  (when usage
    (incf (gethash "promptTokens" total) (%integer-or-zero (jget usage "promptTokens")))
    (incf (gethash "completionTokens" total) (%integer-or-zero (jget usage "completionTokens")))
    (incf (gethash "totalTokens" total) (%integer-or-zero (jget usage "totalTokens"))))
  total)

(defun %correction-prompt (problems)
  (with-output-to-string (out)
    (write-line "Your previous response could not be used. Fix every problem listed below and return the complete output again." out)
    (dolist (problem problems)
      (format out "- ~a~%" problem))
    (write-line "Return only the labelled output lines. Do not call any functions and do not repeat earlier commentary." out)))

(defun forward (generator client inputs)
  "Run GENERATOR against CLIENT with INPUTS (a hash table with string keys).

Returns (values outputs usage): OUTPUTS maps output field names to typed
values, USAGE is an accumulated usage object.  INPUTS is never mutated and the
conversation history is built locally, so a correction turn cannot replay a
tool call."
  (check-type generator generator)
  (check-type client ai-client)
  (unless (hash-table-p inputs)
    (generation-fail :config "forward: inputs must be a hash table with string keys."))
  (let* ((tools (generator-tools generator))
         (request-tools (and tools (mapcar #'tool-request-spec tools)))
         (index (generator-tool-index generator))
         (history (list (message "system" (%system-prompt generator))
                        (message "user" (%user-prompt generator inputs))))
         (usage (usage-object 0 0 0))
         (seen-call-ids (make-hash-table :test #'equal))
         (steps 0)
         (retries 0)
         (max-calls (+ 1 (generator-max-steps generator) (generator-max-retries generator)))
         (calls 0)
         (correcting nil))
    (loop
      (when (> (incf calls) max-calls)
        (generation-fail :steps
                         (format nil "Exceeded the provider call budget of ~a request(s)." max-calls)))
      (let* ((response (chat client (coerce history 'vector)
                             ;; Tool definitions stay in every request: both
                             ;; providers reject a history containing tool
                             ;; calls or results when the tools are gone.  A
                             ;; correction turn forbids a new call instead.
                             :tools request-tools
                             :tool-choice (if correcting :none :auto)))
             (content (or (%present (jget response "content")) ""))
             (tool-calls (%present (jget response "toolCalls"))))
        (%accumulate-usage usage (jget response "usage"))
        (cond
          ((and tool-calls (plusp (length tool-calls)))
           (when correcting
             (generation-fail :tool "The provider requested a tool call during a correction turn."))
           (when (>= steps (generator-max-steps generator))
             (generation-fail :steps
                              (format nil "Exceeded the tool step budget of ~a round(s)."
                                      (generator-max-steps generator))))
           (incf steps)
           (let ((call-list (coerce tool-calls 'list))
                 (results '()))
             (dolist (call call-list)
               (let ((id (%present (jget call "id")))
                     (name (%present (jget call "name"))))
                 (when (%blankp id)
                   (generation-fail :tool "The provider returned a tool call without an id."))
                 (when (gethash id seen-call-ids)
                   (generation-fail :tool
                                    (format nil "The provider reused tool call id '~a'." id)))
                 (setf (gethash id seen-call-ids) t)
                 (let ((spec (and (stringp name) (gethash name index))))
                   (unless spec
                     (generation-fail :tool
                                      (format nil "The provider requested unknown tool '~a'. Known tools: ~{'~a'~^, ~}."
                                              name
                                              (sort (loop for k being the hash-keys of index collect k)
                                                    #'string<))))
                   (let* ((arguments-text (or (%present (jget call "arguments")) "{}"))
                          (arguments (handler-case (parse-json arguments-text)
                                       (error (c)
                                         (declare (ignore c))
                                         :malformed))))
                     (cond
                       ((eq arguments :malformed)
                        (push (message "tool"
                                       (format nil "Error: arguments for '~a' were not valid JSON. ~
Call it again with a single valid JSON object." name)
                                       :tool-call-id id)
                              results))
                       (t
                        (multiple-value-bind (result problems)
                            ;; Pass the parsed value through unchanged: a JSON
                            ;; array, null or number must be rejected by the
                            ;; validator, never coerced into an empty object
                            ;; that a zero-argument handler would accept.
                            (invoke-tool spec arguments)
                          (push (message "tool"
                                         (if problems
                                             (format nil "Error: ~a Call it again with corrected arguments."
                                                     (%string-join " " problems))
                                             result)
                                         :tool-call-id id)
                                results))))))))
             (setf history (append history
                                   (list (message "assistant" content :tool-calls tool-calls))
                                   (nreverse results)))))
          (t
           (multiple-value-bind (values* problems) (%parse-outputs generator content)
             (cond
               ((null problems)
                (return (values (%visible-outputs generator values*) usage)))
               ((< retries (generator-max-retries generator))
                (incf retries)
                (setf correcting t)
                (setf history (append history
                                      (list (message "assistant" content)
                                            (message "user" (%correction-prompt problems))))))
               (t
                (generation-fail
                 :validation
                 (format nil "Output validation failed after ~a correction attempt(s): ~{~a~^ ~}"
                         retries problems)
                 :problems problems))))))))))
