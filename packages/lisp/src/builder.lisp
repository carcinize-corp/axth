;;;; builder.lisp --- declarative, native Lisp signature construction.
(in-package #:axllm)

(defun f (type &key description fields options array
          array-description optional internal cache min max email url pattern
          pattern-description value-descriptions format language)
  "Describe a field for S. Flags are Lisp booleans; values use the JSON ABI."
  (let ((spec (object "type" type)))
    (loop for (key value) on
          (list "description" description "fields" fields "options" options
                "arrayDescription" array-description "min" min "max" max
                "pattern" pattern "patternDescription" pattern-description
                "valueDescriptions" value-descriptions "format" format
                "language" language)
            by #'cddr
          when value do (%set-key spec key value))
    (loop for (key value) on
          (list "array" array "optional" optional "internal" internal
                "cache" cache "email" email "url" url)
            by #'cddr
          when value do (%set-key spec key true))
    spec))

(defun %builder-fields (spec &optional nested)
  (unless (or (eq spec :null) (hash-table-p spec))
    (error 'signature-error :message "Signature fields must be a name-to-field object."))
  (let ((out (if nested (object) (%new-array))))
    (when (hash-table-p spec)
      (dolist (name (%object-keys spec))
        (let ((field (%builder-field name (gethash name spec))))
          (if nested
              (progn
                (let* ((type (axllm/core::core-get field "type"))
                       (description (axllm/core::core-get field "description")))
                  (when (and (not (eq description :null))
                             (eq (axllm/core::core-get type "description") :null))
                    (axllm/core::core-set type "description" description)))
                (%set-key out name field))
              (vector-push-extend field out)))))
    out))

(defun %builder-field (name spec)
  (unless (hash-table-p spec)
    (error 'signature-error :message "A field specification must be an object."))
  (let* ((type-name (jget spec "type" "string"))
         (type (axllm/core::core-record-new
                "FieldType" (object "name" type-name "is_array" false)))
         (description (jget spec "description"))
         (array-p (eq (jget spec "array") true)))
    (when (equal type-name "class")
      (let ((options (jget spec "options")))
        (unless (and (axllm/core::core-array-p options) (plusp (length options))
                     (every #'stringp options))
          (error 'signature-error :message "classification() requires at least one string option"))
        (axllm/core::core-set type "options" (copy-seq options))))
    (when (equal type-name "object")
      (axllm/core::core-set type "fields" (%builder-fields (jget spec "fields") t)))
    (when array-p
      (axllm/core::core-set type "is_array" true)
      (axllm/core::core-set type "description" description)
      (setf description (jget spec "arrayDescription" description)))
    (dolist (pair '(("min" . "minimum") ("max" . "maximum")))
      (let ((value (jget spec (car pair))))
        (unless (eq value :null)
          (axllm/core::core-set
           type (if (equal type-name "number") (cdr pair)
                    (if (equal (car pair) "min") "min_length" "max_length"))
           value))))
    (when (eq (jget spec "email") true) (axllm/core::core-set type "format" "email"))
    (when (eq (jget spec "url") true) (axllm/core::core-set type "format" "uri"))
    (dolist (pair '(("pattern" . "pattern") ("format" . "format")
                    ("language" . "language") ("valueDescriptions" . "value_descriptions")))
      (let ((value (jget spec (car pair))))
        (unless (eq value :null) (axllm/core::core-set type (cdr pair) value))))
    (unless (eq (jget spec "pattern") :null)
      (axllm/core::core-set type "pattern_description"
                           (jget spec "patternDescription" (jget spec "pattern"))))
    (unless (eq (jget spec "valueDescriptions") :null)
      (axllm/core::signature-validate-value-descriptions-impl type type-name))
    (axllm/core::core-record-new
     "Field" (object "name" name "type" type "description" description
                     "is_optional" (jget spec "optional" false)
                     "is_internal" (jget spec "internal" false)
                     "is_cached" (jget spec "cache" false)))))

(defun signature-from-spec (spec)
  "Build and validate a signature from ordered INPUTS/OUTPUTS field objects."
  (unless (hash-table-p spec)
    (error 'signature-error :message "A signature specification must be an object."))
  (let ((signature
          (axllm/core::core-record-new
           "AxSignature" (object "inputs" (%builder-fields (jget spec "inputs"))
                                 "outputs" (%builder-fields (jget spec "outputs"))
                                 "description" (jget spec "description")))))
    (axllm/core::validate-signature signature)
    signature))

(defun s (&key (inputs (object)) (outputs (object)) (description :null))
  "Construct a validated signature with F field specs in ordered objects."
  (signature-from-spec (object "inputs" inputs "outputs" outputs "description" description)))
