;;;; tools.lisp --- named tools and fail-closed JSON Schema argument validation.
;;;;
;;;; Scope: a named function tool with a JSON Schema object for its
;;;; parameters, plus an argument validator used both before handler
;;;; invocation and (optionally) by callers.  This is an explicit
;;;; experimental subset: no MCP, no streaming tools, no agent delegation.
;;;;
;;;; The validator fails closed.  Any schema keyword it does not implement is
;;;; reported as an error rather than silently ignored, so a tool can never be
;;;; invoked with arguments that were only partially checked.

(in-package #:axllm)

(define-condition tool-error (ax-error) ()
  (:documentation "A tool definition or tool invocation failure."))

(defun tool-fail (message)
  (error 'tool-error :message message))

(defparameter +tool-name-scanner+
  (cl-ppcre:create-scanner "[A-Za-z_][A-Za-z0-9_-]{0,63}"))

(defun %valid-tool-name-p (name)
  "True only when NAME matches the identifier pattern end to end.  A regex
anchor would also accept a trailing newline, which is not a valid name."
  (and (stringp name)
       (plusp (length name))
       (multiple-value-bind (start end) (cl-ppcre:scan +tool-name-scanner+ name)
         (and start (= start 0) (= end (length name))))))

(defparameter +supported-json-types+
  '("string" "number" "integer" "boolean" "object" "array" "null"))

(defparameter +primitive-json-types+ '("string" "number" "integer" "boolean" "null"))

(defun supported-schema-keywords (type)
  "The schema keywords this port enforces *for TYPE*.

Keywords stay type-specific on purpose: \"items\" on a string, or \"properties\"
on an array, would advertise a constraint that is never checked.  Everything
listed here is enforced by Core's argument validator, which is the same code the
reference runs, so this list says what is checked rather than what was easy."
  (append '("type" "title" "description" "enum" "const" "default" "examples")
          (when (member type '("number" "integer") :test #'equal)
            '("minimum" "maximum" "exclusiveMinimum" "exclusiveMaximum" "multipleOf"))
          (when (equal type "string") '("minLength" "maxLength" "pattern" "format"))
          (when (equal type "object")
            '("properties" "required" "additionalProperties" "minProperties" "maxProperties"))
          (when (equal type "array") '("items" "minItems" "maxItems" "uniqueItems"))))

;;; ------------------------------------------------------------------
;;; tool factory
;;; ------------------------------------------------------------------

(defun tool (&key name description parameters handler)
  "Define a named tool.

NAME must be a short identifier.  PARAMETERS is a JSON Schema object (a hash
table with string keys) describing an object of arguments; when omitted the
tool takes no arguments.  HANDLER is a function of one argument: a hash table
with string keys holding the validated arguments.  It returns a string, or any
JSON-compatible value which is encoded for the model."
  (when (%blankp name)
    (tool-fail "tool: :name is required."))
  (unless (%valid-tool-name-p name)
    (tool-fail (format nil "tool: invalid name '~a'; use letters, digits, '_' or '-'." name)))
  (unless (functionp handler)
    (tool-fail (format nil "tool '~a': :handler must be a function of one argument." name)))
  (let ((schema (cond ((null parameters)
                       (object "type" "object" "properties" (object) "required" (vector)))
                      ((hash-table-p parameters) parameters)
                      (t (tool-fail (format nil "tool '~a': :parameters must be a JSON Schema object."
                                            name))))))
    ;; Reject an unusable schema at definition time rather than mid-run.
    (let ((schema-problems (validate-schema-support schema (format nil "tool '~a' parameters" name))))
      (when schema-problems
        (tool-fail (%string-join " " schema-problems))))
    (let ((spec (object "name" name
                        "description" (or description "")
                        "parameters" schema)))
      (setf (gethash "handler" spec) handler)
      spec)))

(defun tool-handler (spec)
  (gethash "handler" spec))

(defun tool-request-spec (spec)
  "The JSON-encodable part of a tool spec (no handler)."
  (object "name" (jget spec "name")
          "description" (or (jget spec "description") "")
          "parameters" (jget spec "parameters")))

(defun tool-index (tools)
  "Build a name -> spec table, rejecting duplicate tool names."
  (let ((index (make-hash-table :test #'equal)))
    (map nil
         (lambda (spec)
           (unless (hash-table-p spec)
             (tool-fail "Each tool must be a tool spec created by `tool'."))
           (let ((name (jget spec "name")))
             (when (%blankp name)
               (tool-fail "Each tool requires a \"name\"."))
             (unless (functionp (tool-handler spec))
               (tool-fail (format nil "Tool '~a' has no handler." name)))
             (when (gethash name index)
               (tool-fail (format nil "Duplicate tool name '~a'." name)))
             (setf (gethash name index) spec)))
         (if (listp tools) (coerce tools 'vector) tools))
    index))

;;; ------------------------------------------------------------------
;;; Schema support check (fail closed)
;;; ------------------------------------------------------------------

(defun %schema-keys (schema)
  (let ((keys '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) schema)
    (sort keys #'string<)))

(defun %json-array-p (value)
  "A JSON array in this representation is a non-string vector.  A Lisp string
is also a vector, so it must never be mistaken for an array."
  (and (vectorp value) (not (stringp value))))

(defun %primitive-enum-value-p (value)
  (or (stringp value)
      (and (realp value) (not (eq value t)))
      (json-true-p value)
      (json-false-p value)
      (eq value :null)))

(defun %validate-schema-node (schema context &key top-level)
  "Recursively check that every keyword present in SCHEMA is one this subset
actually enforces for that node's type.  Returns a list of problems."
  (if (not (hash-table-p schema))
      (list (format nil "~a: schema must be a JSON object." context))
      (let* ((problems '())
             (declared (%present (jget schema "type")))
             ;; A top-level tool schema may omit "type"; it is an object.
             (type (if (and top-level (null declared)) "object" declared)))
        (cond ((null type)
               (push (format nil "~a: \"type\" is required." context) problems))
              ((not (stringp type))
               (push (format nil "~a: \"type\" must be a string (unions are not supported)." context)
                     problems))
              ((not (member type +supported-json-types+ :test #'string=))
               (push (format nil "~a: unsupported type \"~a\"." context type) problems))
              ((and top-level (not (string= type "object")))
               (push (format nil "~a: top-level schema type must be \"object\", got \"~a\"."
                             context type)
                     problems)))
        (let ((allowed (supported-schema-keywords (and (stringp type) type))))
          (dolist (key (%schema-keys schema))
            (unless (member key allowed :test #'string=)
              (push (format nil "~a: unsupported schema keyword \"~a\"~@[ for type \"~a\"~]; ~
this validator fails closed."
                            context key (and (stringp type) type))
                    problems))))
        (let ((enum (%present (jget schema "enum"))))
          (when enum
            (cond ((not (or (%json-array-p enum) (consp enum)))
                   (push (format nil "~a: \"enum\" must be an array." context) problems))
                  (t
                   (let ((items (if (%json-array-p enum) (coerce enum 'list) enum)))
                     (when (null items)
                       (push (format nil "~a: \"enum\" must not be empty." context) problems))
                     (unless (every #'%primitive-enum-value-p items)
                       ;; Membership is EQUAL, which is not JSON structural
                       ;; equality, so composite enum values are refused
                       ;; outright rather than matched incorrectly.
                       (push (format nil "~a: \"enum\" values must be primitives ~
\(string, number, boolean or null); composite values are not supported." context)
                             problems)))))))
        (when (equal type "object")
          (let ((properties (%present (jget schema "properties")))
                (required (%present (jget schema "required")))
                (additional (%present (jget schema "additionalProperties"))))
            (cond ((null properties))
                  ((not (hash-table-p properties))
                   (push (format nil "~a: \"properties\" must be a JSON object." context) problems))
                  (t
                   (dolist (name (%schema-keys properties))
                     (setf problems
                           (append (%validate-schema-node
                                    (gethash name properties)
                                    (format nil "~a property \"~a\"" context name))
                                   problems)))))
            (cond ((null required))
                  ((not (or (%json-array-p required) (consp required)))
                   (push (format nil "~a: \"required\" must be an array of property names." context)
                         problems))
                  (t
                   (let ((names (if (%json-array-p required) (coerce required 'list) required)))
                     (unless (every #'stringp names)
                       (push (format nil "~a: \"required\" must contain only strings." context)
                             problems))
                     (dolist (name names)
                       (when (and (stringp name)
                                  (hash-table-p properties)
                                  (not (nth-value 1 (gethash name properties))))
                         (push (format nil "~a: \"required\" names undeclared property \"~a\"."
                                       context name)
                               problems))))))
            ;; A boolean says whether extra properties are allowed; an object is
            ;; the schema every extra property must match, and Core checks it.
            (unless (or (null additional)
                        (json-true-p additional)
                        (json-false-p additional)
                        (hash-table-p additional))
              (push (format nil "~a: \"additionalProperties\" must be a JSON boolean ~
or a schema object." context)
                    problems))
            (when (hash-table-p additional)
              (setf problems
                    (append (%validate-schema-node
                             additional (format nil "~a additionalProperties" context))
                            problems)))))
        (when (equal type "array")
          (let ((items (%present (jget schema "items"))))
            (if (null items)
                (push (format nil "~a: array schema requires \"items\"." context) problems)
                (setf problems
                      (append (%validate-schema-node items (format nil "~a items" context))
                              problems)))))
        (nreverse problems))))

(defun validate-schema-support (schema context)
  "Return a list of problems describing schema constructs this subset cannot
enforce.  An empty list means every keyword present is implemented."
  (%validate-schema-node schema context :top-level t))

;;; ------------------------------------------------------------------
;;; Argument validation
;;; ------------------------------------------------------------------

(defun %required-names (schema)
  (let ((required (%present (jget schema "required"))))
    (cond ((null required) nil)
          ((%json-array-p required) (coerce required 'list))
          ((consp required) required)
          (t nil))))

(defun %json-type-matches-p (type value)
  (cond ((string= type "string") (stringp value))
        ((string= type "number") (and (realp value) (not (eq value t))))
        ((string= type "integer") (integerp value))
        ((string= type "boolean") (or (json-true-p value) (json-false-p value)))
        ((string= type "object") (hash-table-p value))
        ((string= type "array") (%json-array-p value))
        ((string= type "null") (eq value :null))
        (t nil)))

(defun %describe-value (value)
  (cond ((stringp value) (format nil "a string"))
        ((integerp value) "an integer")
        ((realp value) "a number")
        ((json-true-p value) "true")
        ((json-false-p value) "false")
        ((eq value :null) "null")
        ((hash-table-p value) "an object")
        ((vectorp value) "an array")
        (t "an unsupported value")))

(defun %enum-member-p (enum value)
  (let ((items (if (%json-array-p enum) (coerce enum 'list) enum)))
    (member value items :test #'equal)))

(defun %enum-text (enum)
  (let ((items (if (%json-array-p enum) (coerce enum 'list) enum)))
    (%string-join ", " (mapcar (lambda (item)
                                 (if (stringp item) (format nil "\"~a\"" item)
                                     (princ-to-string item)))
                               items))))

(defun %validate-value (schema value context)
  (let ((problems '())
        (type (%present (jget schema "type"))))
    (if (not (%json-type-matches-p type value))
        (push (format nil "~a must be of type \"~a\", got ~a." context type (%describe-value value))
              problems)
        (progn
          (let ((enum (%present (jget schema "enum"))))
            (when (and enum (not (%enum-member-p enum value)))
              (push (format nil "~a must be one of ~a." context (%enum-text enum)) problems)))
          (when (string= type "array")
            (let ((items (%present (jget schema "items"))))
              (loop for element across value
                    for i from 0
                    do (setf problems
                             (append (%validate-value items element
                                                      (format nil "~a[~a]" context i))
                                     problems)))))
          (when (string= type "object")
            (setf problems (append (%validate-object schema value context) problems)))))
    (nreverse problems)))

(defun %validate-object (schema value context)
  (let* ((problems '())
         (properties (%present (jget schema "properties")))
         (required (%required-names schema))
         (allow-extra (json-true-p (%present (jget schema "additionalProperties")))))
    (dolist (name required)
      (unless (nth-value 1 (gethash name value))
        (push (format nil "~a is missing required argument \"~a\"." context name) problems)))
    (let ((names '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k names)) value)
      (dolist (name (sort names #'string<))
        (let ((subschema (and (hash-table-p properties) (gethash name properties))))
          (cond ((null subschema)
                 (unless allow-extra
                   (push (format nil "~a has unknown argument \"~a\"." context name) problems)))
                (t
                 (setf problems
                       (append (%validate-value subschema (gethash name value)
                                                (format nil "~a argument \"~a\"" context name))
                               problems)))))))
    (nreverse problems)))

(defun validate-tool-arguments (spec arguments)
  "Validate ARGUMENTS against SPEC's JSON Schema.

Returns a list of human-readable problems; an empty list means the arguments are
safe to pass to the handler.

The checking itself is Core's, so this port agrees with the reference on every
edge the hand-written version got wrong: an integer is a number with an integral
value, so 1.0 is accepted; a non-finite number is rejected before a handler can
see it; enum membership compares values rather than Lisp objects, so 1 matches
1.0; and an extra property is allowed unless the schema says
\"additionalProperties\": false, which is what JSON Schema means."
  (let* ((name (jget spec "name"))
         (schema (%present (jget spec "parameters")))
         (context (format nil "Tool '~a'" name)))
    (cond
      ((not (hash-table-p arguments))
       (list (format nil "~a arguments must be a JSON object." context)))
      ((null schema) '())
      (t
       (let ((errors (axllm/core::chat-session-tool-argument-errors schema arguments))
             (problems '()))
         (map nil
              (lambda (entry)
                (let ((field (%present (jget entry "field")))
                      (message (or (%present (jget entry "message")) "is invalid")))
                  (push (if (and field (not (%blankp field)))
                            (format nil "~a argument \"~a\": ~a." context field message)
                            (format nil "~a: ~a." context message))
                        problems)))
              (if (and (vectorp errors) (not (stringp errors)))
                  errors
                  (coerce (or errors '()) 'vector)))
         (nreverse problems))))))

;;; ------------------------------------------------------------------
;;; Result text for the model
;;; ------------------------------------------------------------------

(defun %write-json-indented (value stream depth)
  "Write VALUE as JSON with two-space indentation, matching the Core
function-result formatter's `JSON.stringify(value, null, 2)'."
  (flet ((pad (level) (dotimes (i (* 2 level)) (write-char #\Space stream))))
    (cond
      ((%object-p value)
       (let ((keys (%object-keys value)))
         (if (null keys)
             (write-string "{}" stream)
             (progn
               (write-string "{" stream)
               (loop for key in keys
                     for first = t then nil
                     do (unless first (write-string "," stream))
                        (terpri stream)
                        (pad (1+ depth))
                        (%write-json-string key stream)
                        (write-string ": " stream)
                        (%write-json-indented (gethash key value) stream (1+ depth)))
               (terpri stream)
               (pad depth)
               (write-string "}" stream)))))
      ((%json-array-p value)
       (if (zerop (length value))
           (write-string "[]" stream)
           (progn
             (write-string "[" stream)
             (loop for item across value
                   for first = t then nil
                   do (unless first (write-string "," stream))
                      (terpri stream)
                      (pad (1+ depth))
                      (%write-json-indented item stream (1+ depth)))
             (terpri stream)
             (pad depth)
             (write-string "]" stream))))
      (t (%write-json value stream)))))

(defun encode-json-pretty (value)
  "VALUE as JSON text indented with two spaces, key order preserved.

This is the shape Core writes tool results in, so a model reading a result
object sees the same text in every Ax port."
  (with-output-to-string (stream) (%write-json-indented value stream 0)))

(defun tool-result-text (result)
  "RESULT as the text the model sees for a tool call.

A string passes through unchanged, a missing value renders as nothing, and any
other JSON value is pretty-printed.  A result that renders as nothing is
reported as \"done\": a tool that ran and returned no value succeeded, and
saying so is not the same as telling the model the result was null."
  (let ((text (cond ((stringp result) result)
                    ((or (null result) (eq result :null)) "")
                    (t (encode-json-pretty result)))))
    (if (string= text "") "done" text)))

(defun invoke-tool (spec arguments)
  "Validate ARGUMENTS and invoke SPEC's handler.  Returns (values result-string
problems).  When PROBLEMS is non-nil the handler was NOT invoked."
  (let ((problems (validate-tool-arguments spec arguments)))
    (if problems
        (values nil problems)
        (values (tool-result-text (funcall (tool-handler spec) arguments)) nil))))

;;; ------------------------------------------------------------------
;;; Function processor
;;; ------------------------------------------------------------------
;;;
;;; One place that turns a model's tool call into a result.  A caller that
;;; wants the raw value as well as the text the model sees asks for details;
;;; a caller that only needs the text calls `execute-function'.
;;;
;;; Every failure here is recoverable: a `function-call-error' carries text the
;;; model can act on, so a wrong name or a failing backend becomes another turn
;;; rather than the end of the run.

(define-condition function-call-error (tool-error) ()
  (:documentation
   "A tool call that could not be completed, with text the model can correct
from.  Distinct from `tool-error': a definition mistake is the programmer's, a
`function-call-error' is the model's or the backend's."))

(defun %function-call-fail (format-control &rest arguments)
  (error 'function-call-error :message (apply #'format nil format-control arguments)))

(defclass function-processor ()
  ((functions :initarg :functions :reader function-processor-functions)
   (index :initarg :index :reader function-processor-index)
   (normalized :initarg :normalized :reader %function-processor-normalized)))

(defun %normalized-function-name (name)
  "NAME with every non-alphanumeric character dropped, lowercased."
  (let ((out (make-string-output-stream)))
    (when (stringp name)
      (loop for char across name
            do (when (alphanumericp char) (write-char (char-downcase char) out))))
    (get-output-stream-string out)))

(defun make-function-processor (tools)
  "A processor over TOOLS (a list or vector of specs from `tool')."
  (let* ((list (coerce (if (listp tools) tools (coerce tools 'list)) 'list))
         (index (tool-index list))
         (normalized (make-hash-table :test #'equal)))
    ;; A normalized name that two tools share is ambiguous, so it resolves
    ;; nothing rather than picking one of them.
    (dolist (spec list)
      (let ((key (%normalized-function-name (jget spec "name"))))
        (setf (gethash key normalized)
              (if (nth-value 1 (gethash key normalized)) :ambiguous spec))))
    (make-instance 'function-processor :functions list :index index :normalized normalized)))

(defun function-processor-resolve (processor name)
  "PROCESSOR's spec for NAME: an exact match first, then a normalized one.

Exact before normalized matters when a tool is called `get_weather' and another
`getWeather': the name the model wrote wins over the one that merely folds to
the same letters."
  (or (and (stringp name) (gethash name (function-processor-index processor)))
      (let ((hit (gethash (%normalized-function-name name)
                          (%function-processor-normalized processor))))
        (and hit (not (eq hit :ambiguous)) hit))))

(defun %function-call-arguments (call)
  "CALL's arguments as a parsed JSON value.

A string is parsed; an object the provider already decoded is used as it is.
An absent value is an empty object: a tool with no parameters is called with
no arguments, not refused."
  (let* ((nested (%present (jget call "function")))
         (raw (or (%present (jget (or nested call) "params"))
                  (%present (jget call "arguments"))
                  (%present (jget call "args")))))
    (cond ((null raw) (object))
          ((stringp raw)
           (if (zerop (length raw))
               (object)
               (handler-case (parse-json raw)
                 (error () (%function-call-fail "Invalid function arguments: ~a" raw)))))
          (t raw))))

(defun execute-function-with-details (processor call)
  "Run CALL against PROCESSOR.  Returns (values formatted raw-result parsed-args).

FORMATTED is the text the model sees, RAW-RESULT the handler's own value, and
PARSED-ARGS the arguments as the handler received them."
  (unless (hash-table-p call)
    (%function-call-fail "A function call must be an object."))
  (let* ((name (%present (jget call "name")))
         (spec (function-processor-resolve processor name)))
    (unless spec
      (let ((available (%string-join ", " (mapcar (lambda (s) (jget s "name"))
                                                  (function-processor-functions processor)))))
        (%function-call-fail "Function not found: ~a. Available functions: ~a. ~
Call one of these exact function names."
                             name (if (string= available "") "(none)" available))))
    (unless (functionp (tool-handler spec))
      (%function-call-fail "No handler for function: ~a" name))
    (let ((arguments (%function-call-arguments call)))
      (let ((problems (validate-tool-arguments spec arguments)))
        (when problems (%function-call-fail "~a" (%string-join " " problems))))
      (let ((raw (handler-case (funcall (tool-handler spec) arguments)
                   (function-call-error (condition) (error condition))
                   (error (condition)
                     ;; A handler that failed is reported to the model, not to
                     ;; the caller: a backend outage is something the model can
                     ;; work around, and the run keeps its earlier results.
                     (%function-call-fail "~a" (ax-error-message-text condition))))))
        (values (tool-result-text raw) raw arguments)))))

(defun execute-function (processor call)
  "The text PROCESSOR produces for CALL."
  (values (execute-function-with-details processor call)))

(defun ax-error-message-text (condition)
  "CONDITION's message as one line, with no Lisp type noise around it."
  (let ((text (if (typep condition 'ax-error)
                  (ax-error-message condition)
                  (princ-to-string condition))))
    (substitute #\Space #\Newline (or text ""))))
