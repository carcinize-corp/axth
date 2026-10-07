;;;; prompt.lisp --- native prompt rendering boundaries and public templates.
(in-package #:axllm/core)

(defparameter +default-dspy-template+
  "<identity>
{{ identityText }}
</identity>{{ if hasFunctions }}

<available_functions>
**Available Functions**: You can call the following functions to complete the task:

{{ functionsList }}

## Function Call Instructions
- Complete the task, using the functions defined earlier in this prompt.
- Output fields should only be generated after all functions have been called.
- Use the function results to generate the output fields.
</available_functions>{{ /if }}

<input_fields>
{{ inputFieldsSection }}
</input_fields>{{ if hasOutputFields }}

<output_fields>
{{ outputFieldsSection }}
</output_fields>{{ /if }}
{{ if hasTaskDefinition }}

<task_definition>
{{ taskDefinitionText }}
</task_definition>{{ /if }}

<formatting_rules>
{{ if hasStructuredOutputFunction }}
Return the complete output by calling `{{ structuredOutputFunctionName }}`.
{{ else }}{{ if hasComplexFields }}
Return one valid JSON object matching <output_fields>. Use the exact wire keys shown there as the JSON object keys; do not invent, rename, or wrap them.
{{ else }}
Return one `field name: value` pair per line for the required output fields only, using each exact wire key shown in <output_fields> as the field name.
{{ /if }}{{ /if }}Above rules override later instructions.

</formatting_rules>
{{ if hasExampleDemonstrations }}

## Example Demonstrations
The following User/Assistant turns are examples only until --- END OF EXAMPLES ---, not context for the current task.
{{ /if }}
")

(defun %prompt-option (options snake camel &optional (fallback :null))
  (axllm:jget options snake (axllm:jget options camel fallback)))

(defun %prompt-outputs (signature)
  (remove-if (lambda (field) (core-true-p (core-get field "is_internal")))
             (core-elements (core-get signature "output_fields"))))

(defun %prompt-provided-p (value)
  (and (not (eq value :null))
       (not (and (or (stringp value) (core-array-p value)) (zerop (length value))))))

(defun %prompt-inputs (signature values &optional include-optional)
  (let ((fields (stable-sort (core-elements (core-get signature "input_fields"))
                            #'< :key (lambda (field)
                                       (if (core-true-p (core-get field "is_cached")) 0 1)))))
    (if (or include-optional (not (hash-table-p values))) fields
        (remove-if (lambda (field)
                     (and (core-true-p (core-get field "is_optional"))
                          (not (%prompt-provided-p (axllm:jget values (core-get field "name"))))))
                   fields))))

(defun %prompt-description (text)
  (let ((value (if (stringp text) (core-string-trim text) "")))
    (if (zerop (length value)) ""
        (concatenate 'string (string-upcase (subseq value 0 1)) (subseq value 1)
                     (if (char= (char value (1- (length value))) #\.) "" ".")))))

(defun %prompt-field-map (signature)
  (let ((out (axllm:object)))
    (dolist (field (append (core-elements (core-get signature "input_fields"))
                           (%prompt-outputs signature)))
      (core-set out (core-get field "name") (core-get field "title")))
    out))

(defun %prompt-references (text field-map)
  (let ((out text))
    (dolist (name (stable-sort (axllm::%object-keys field-map) #'> :key #'length) out)
      (let ((title (axllm:jget field-map name)))
        (dolist (pair '(("`" . "`") ("\"" . "\"") ("'" . "'") ("[" . "]") ("(" . ")")))
          (setf out (core-string-replace out (concatenate 'string (car pair) name (cdr pair))
                                        (concatenate 'string (car pair) title (cdr pair)))))
        (setf out (cl-ppcre:regex-replace-all
                   (concatenate 'string "\\$" (cl-ppcre:quote-meta-chars name) "\\b") out
                   (lambda (&rest ignored) (declare (ignore ignored))
                     (concatenate 'string "`" title "`"))))))))

(defun %prompt-type-text (type)
  (let* ((name (core-get type "name" "string"))
         (base
           (cond
             ((equal name "object")
              (let ((fields (core-get type "fields")))
                (if (not (core-true-p fields)) "object"
                    (format nil "object { ~{~A~^, ~} }"
                            (loop for field across (core-fields-from-map fields)
                                  collect (format nil "~A~A: ~A" (core-get field "name")
                                                  (if (core-true-p (core-get field "is_optional")) "?" "")
                                                  (%prompt-type-text (core-get field "type"))))))))
             (t (or (cdr (assoc name
                                '(("string" . "string") ("number" . "number")
                                  ("boolean" . "boolean (true or false)")
                                  ("date" . "date (YYYY-MM-DD, e.g. 2024-05-09)")
                                  ("datetime" . "datetime (ISO 8601 with timezone, e.g. 2024-05-09T14:30:00Z or 2024-05-09T14:30:00-07:00)")
                                  ("dateRange" . "date range ({ \"start\": \"YYYY-MM-DD\", \"end\": \"YYYY-MM-DD\" }, e.g. {\"start\":\"2024-05-09\",\"end\":\"2024-05-12\"})")
                                  ("datetimeRange" . "datetime range ({ \"start\": ISO datetime, \"end\": ISO datetime }, e.g. {\"start\":\"2024-05-09T14:30:00Z\",\"end\":\"2024-05-09T15:30:00Z\"})")
                                  ("json" . "JSON object") ("class" . "classification class")
                                  ("code" . "code") ("file" . "file (with filename, mimeType, and data)")
                                  ("audio" . "speech script (plain text to synthesize as audio)")
                                  ("url" . "URL (string or object with url, title, description)"))
                                :test #'equal)) "string")))))
    (if (core-true-p (core-get type "is_array")) (format nil "json array of ~A items" base) base)))

(defun %prompt-placeholder (type)
  (let* ((name (core-get type "name"))
         (value
           (cond ((equal name "number") 0) ((equal name "boolean") axllm:true)
                 ((member name '("object" "json") :test #'equal)
                  (let ((out (axllm:object)))
                    (loop for field across (core-fields-from-map (core-get type "fields"))
                          unless (core-true-p (core-get field "is_internal"))
                            do (core-set out (core-get field "name")
                                         (%prompt-placeholder (core-get field "type")))) out))
                 ((equal name "class") (core-get (core-get type "options") 0 "<allowed value>"))
                 ((member name '("dateRange" "datetimeRange") :test #'equal)
                  (let ((text (if (equal name "dateRange") "<YYYY-MM-DD>" "<ISO 8601 datetime>")))
                    (axllm:object "start" text "end" text)))
                 (t (or (cdr (assoc name '(("code" . "<complete source>") ("date" . "<YYYY-MM-DD>")
                                           ("datetime" . "<ISO 8601 datetime>") ("url" . "<url>"))
                                    :test #'equal)) "<string>")))))
    (if (core-true-p (core-get type "is_array")) (vector value) value)))

(defun %prompt-fields-section (fields field-map output-p)
  (let ((rows nil))
    (dolist (field fields)
      (let* ((type (core-get field "type"))
             (raw (signature-describe-field-values-impl field))
             (description
               (if (core-true-p raw)
                   (concatenate 'string " "
                                (%prompt-references
                                 (if (or (core-true-p (core-get type "value_descriptions"))
                                         (and output-p (equal (core-get type "name") "class")))
                                     raw (%prompt-description raw)) field-map)) "")))
        (when (and output-p (core-true-p (core-get type "options")))
          (setf description (concatenate 'string description (if (plusp (length description)) ". " "")
                                         "Allowed values: " (core-string-join ", " (core-get type "options")))))
        (push (core-string-trim
               (if output-p
                   (format nil "~A (wire key: `~A`): (~A ~A field ~A)~A"
                           (core-get field "title") (core-get field "name")
                           (if (core-true-p (core-get field "is_optional")) "Only include this" "This")
                           (%prompt-type-text type)
                           (if (core-true-p (core-get field "is_optional")) "if its value is available" "must be included")
                           description)
                   (format nil "~A:~A" (core-get field "title") description))) rows)
        (loop for row across (signature-nested-value-descriptions-impl (core-get type "fields")
                                                                     (core-get field "name"))
              do (push row rows))))
    (core-string-join (string #\Newline) (coerce (nreverse rows) 'vector))))

(defun core-prompt-structured (signature values functions options)
  (let* ((field-map (%prompt-field-map signature))
         (include-optional (core-true-p (%prompt-option options "include_optional_input_fields_in_system_prompt"
                                                       "includeOptionalInputFieldsInSystemPrompt" axllm:false)))
         (inputs (%prompt-inputs signature values include-optional))
         (outputs (%prompt-outputs signature))
         (complex-option (%prompt-option options "structured_output" "structuredOutput"))
         (complex (if (eq complex-option :null)
                      (core-true-p (signature-has-complex-fields signature (axllm:object)))
                      (core-true-p complex-option)))
         (instruction (let ((value (core-get options "instruction")))
                        (core-string-trim (if (core-true-p value) (core-js-text value) ""))))
         (description (core-string-trim (let ((v (core-get signature "description")))
                                         (if (stringp v) v ""))))
         (task (%prompt-references
                (core-string-join (format nil "~%~%")
                                  (coerce (loop for part in (list instruction (if (equal instruction description) "" description))
                                                when (plusp (length part)) collect (%prompt-description part)) 'vector))
                field-map))
         (funcs (remove-if-not (lambda (f) (core-true-p (core-get f "name"))) (core-elements functions)))
         (output-section (%prompt-fields-section outputs field-map t))
         (source (%prompt-option options "custom_template" "customTemplate"))
         (output-function (let ((value (%prompt-option options "structured_output_function_name" "structuredOutputFunctionName")))
                            (if (core-true-p value) value ""))))
    (when complex
      (let ((shape (axllm:object)))
        (dolist (field outputs) (core-set shape (core-get field "name") (%prompt-placeholder (core-get field "type"))))
        (setf output-section (format nil "~A~%~%**Exact JSON shape**: `~A`" output-section (axllm:encode-json shape)))))
    (let ((vars
            (axllm:object
             "hasFunctions" (core-bool funcs) "hasTaskDefinition" (core-bool (plusp (length task)))
             "hasExampleDemonstrations" (core-bool (core-true-p (%prompt-option options "has_example_demonstrations" "hasExampleDemonstrations" axllm:false)))
             "hasOutputFields" (core-bool outputs) "hasComplexFields" (core-bool complex)
             "hasStructuredOutputFunction" (core-bool (and complex (core-true-p output-function)))
             "identityText" (format nil "You will be provided with the following fields: ~{`~A`~^, ~}. Your task is to generate new fields: ~{`~A`~^, ~}."
                                    (mapcar (lambda (f) (core-get f "title")) inputs)
                                    (mapcar (lambda (f) (core-get f "title")) outputs))
             "taskDefinitionText" task
             "functionsList" (format nil "~{~A~^~%~}" (mapcar (lambda (f) (format nil "- `~A`: ~A" (core-get f "name")
                                                                                                  (%prompt-description (core-get f "description")))) funcs))
             "inputFieldsSection" (format nil "**Input Fields**: The following fields will be provided to you:~%~%~A"
                                          (%prompt-fields-section inputs field-map nil))
             "outputFieldsSection" (format nil "**Output Fields**: You must generate the following fields:~%~%~A" output-section)
             "structuredOutputFunctionName" output-function)))
      (core-string-trim
       (axllm::render-template-content (if (eq source :null) +default-dspy-template+ source) vars
                                      (if (eq source :null) "template:dsp/dspy.md" "inline-template"))))))

(defun %prompt-media-value (kind value)
  "Normalize native media key spellings before type checks or rendering."
  (let ((aliases (cdr (assoc kind '(("image" ("mime_type" . "mimeType"))
                                    ("audio" ("audio" . "data") ("mime_type" . "mimeType") ("sample_rate" . "sampleRate"))
                                    ("file" ("mime_type" . "mimeType") ("file_uri" . "fileUri") ("extracted_text" . "extractedText"))
                                    ("url" ("cached_content" . "cachedContent"))) :test #'equal))))
    (if (hash-table-p value)
        (let ((copy (core-map-merge value (axllm:object))))
          (dolist (pair aliases copy)
            (when (and (nth-value 1 (gethash (car pair) copy)) (not (nth-value 1 (gethash (cdr pair) copy))))
              (core-set copy (cdr pair) (gethash (car pair) copy)))))
        value)))

(defun %prompt-media-part (kind value)
  (let ((out (axllm:object "type" kind)))
    (when (and (equal kind "url") (stringp value)) (setf value (axllm:object "url" value)))
    (unless (hash-table-p value) (%ax-error "~A field value must be an object." (string-capitalize kind)))
    (labels ((required (key &optional (wire key))
               (unless (nth-value 1 (gethash key value)) (%ax-error "~A field must have ~A" (string-capitalize kind) key))
               (core-set out wire (gethash key value)))
             (optional (keys)
               (dolist (key keys)
                 (when (nth-value 1 (gethash key value)) (core-set out key (gethash key value))))))
      (cond
        ((equal kind "image") (required "mimeType") (required "data" "image") (optional '("details" "cache" "optimize" "altText")))
        ((equal kind "audio")
         (core-set out "format" (let ((v (axllm:jget value "format"))) (if (eq v :null) "wav" v)))
         (required "data") (optional '("mimeType" "sampleRate" "channels" "cache" "transcription" "duration")))
        ((equal kind "file")
         (required "mimeType")
         (let ((data (nth-value 1 (gethash "data" value))) (uri (nth-value 1 (gethash "fileUri" value))))
           (when (and data uri) (%ax-error "File field cannot have both data and fileUri"))
           (unless (or data uri) (%ax-error "File field must have either data or fileUri"))
           (required (if uri "fileUri" "data")))
         (optional '("filename" "cache" "extractedText")))
        ((equal kind "url")
         (required "url")
         (dolist (key '("title" "description"))
           (when (core-true-p (axllm:jget value key)) (core-set out key (axllm:jget value key))))
         (optional '("cachedContent" "cache")))))
    out))

(defun core-prompt-user-content (signature values)
  (let ((parts nil))
    (dolist (field (%prompt-inputs signature values))
      (let* ((name (core-get field "name")) (value (axllm:jget values name))
             (type (core-get field "type")) (kind (core-get type "name")))
        (cond
          ((not (%prompt-provided-p value))
           (unless (or (core-true-p (core-get field "is_optional")) (core-true-p (core-get field "is_internal")))
             (%ax-error "Value for input field '~A' is required." name)))
          (t
           (when (member kind '("image" "audio" "file" "url") :test #'equal)
             (setf value (if (core-array-p value)
                             (map 'vector (lambda (item) (%prompt-media-value kind item)) value)
                             (%prompt-media-value kind value))))
           (validate-prompt-value field value)
           (let ((dated (core-date-prompt-text kind value)))
             (unless (eq dated :null) (setf value dated)))
           (when (and (equal kind "audio") (hash-table-p value) (stringp (axllm:jget value "transcript")))
             (setf value (axllm:jget value "transcript")))
           (if (or (member kind '("image" "file" "url") :test #'equal)
                   (and (equal kind "audio") (not (stringp value))))
               (progn
                 (push (axllm:object "type" "text" "text" (format nil "~A: ~%" (core-get field "title"))) parts)
                 (if (core-true-p (core-get type "is_array"))
                     (progn
                       (unless (core-array-p value) (%ax-error "~A field value must be an array." (string-capitalize kind)))
                       (loop for item across value do (push (%prompt-media-part kind item) parts)))
                     (push (%prompt-media-part kind value) parts)))
               (let ((part (axllm:object "type" "text" "text"
                                         (format nil "~A: ~A~%" (core-get field "title")
                                                 (if (stringp value) value
                                                     (core-json-pretty (core-date-json value)))))))
                 (when (core-true-p (core-get field "is_cached")) (core-set part "cache" axllm:true))
                 (push part parts)))))))
    (setf parts (nreverse parts))
    (if (every (lambda (p) (equal (axllm:jget p "type") "text")) parts)
        (core-string-join (string #\Newline) (coerce (mapcar (lambda (p) (axllm:jget p "text")) parts) 'vector))
        (let ((out nil))
          (dolist (part parts)
            (if (and out (equal (axllm:jget part "type") "text") (equal (axllm:jget (car out) "type") "text"))
                (progn (core-set (car out) "text" (format nil "~A~%~A" (axllm:jget (car out) "text") (axllm:jget part "text")))
                       (when (core-true-p (axllm:jget part "cache")) (core-set (car out) "cache" axllm:true)))
                (push part out)))
          (coerce (nreverse out) 'vector)))))

(in-package #:axllm)

(defun render-prompt (signature values &key (functions #()) (options (object)))
  "Render a signature and input values as system/user messages through Core."
  (axllm/core::render-prompt signature values functions options))

(defstruct (prompt-template (:constructor %make-prompt-template))
  signature (functions #()) (options (object)) (instruction :null))

(defun prompt-template (signature &key (functions #()) custom-template
                         structured-output-function-name include-optional-input-fields-in-system-prompt)
  (%make-prompt-template
   :signature (if (stringp signature) (parse-signature signature) signature) :functions functions
   :options (let ((options (object)))
              (when custom-template (%set-key options "custom_template" custom-template))
              (when structured-output-function-name (%set-key options "structured_output_function_name" structured-output-function-name))
              (when include-optional-input-fields-in-system-prompt (%set-key options "include_optional_input_fields_in_system_prompt" true))
              options)))

(defun render-prompt-template (template values &optional (options (object)))
  (let* ((merged (axllm/core::core-map-merge (prompt-template-options template) options))
         (functions (concatenate 'vector (prompt-template-functions template) (jget options "extra_functions" #()))))
    (unless (eq (prompt-template-instruction template) :null)
      (%set-key merged "instruction" (prompt-template-instruction template)))
    (render-prompt (prompt-template-signature template) values :functions functions :options merged)))
