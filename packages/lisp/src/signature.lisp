;;;; signature.lisp --- the public signature and JSON Schema API.
;;;;
;;;; Everything here is a thin boundary over generated Core code. Parsing,
;;;; validation, rendering and schema generation all happen in
;;;; src/core.lisp; this file chooses the Lisp-facing names, turns Ax's
;;;; internal snake_case records into the camelCase JSON that Ax's other
;;;; ports expose, and nothing else. No signature rule is decided here.

(in-package #:axllm)

;;; ------------------------------------------------------------------
;;; Signatures
;;; ------------------------------------------------------------------

(defun parse-signature (text)
  "Parse TEXT as an Ax signature and return the signature.

  (parse-signature \"question:string -> answer:string\")

TEXT is the usual Ax form: input fields, an arrow, output fields, with
optional types, modifiers and quoted descriptions. The result is validated
before it is returned, so a parsed signature is always a usable one.

Signals SIGNATURE-ERROR when TEXT is not a valid signature: bad syntax, an
unknown type or modifier, a field in a section that forbids it, a duplicate
field name, or a name used as both an input and an output."
  (unless (stringp text)
    (error 'ax-error :message (format nil "parse-signature: expected a string, got ~S" text)))
  (let ((signature (axllm/core::parse-signature text)))
    ;; Parsing builds the signature; validation is a separate Core entry
    ;; point, and the cross-field rules (duplicates, input/output
    ;; collisions) live only there. Running both is what makes
    ;; PARSE-SIGNATURE total.
    (axllm/core::validate-signature signature)
    signature))

(defun signature-string (signature)
  "SIGNATURE rendered back to Ax signature text.

The result parses to an equal signature, so this is the inverse of
PARSE-SIGNATURE for every signature Ax can represent."
  (axllm/core::signature-to-string (%check-signature signature 'signature-string)))

(defun signature-fields (signature &key (side :input))
  "SIGNATURE's fields on SIDE, as a vector of JSON objects.

SIDE is :INPUT or :OUTPUT. Each field is a JSON object in Ax's published
camelCase shape:

  name, title, type, isOptional, isInternal, isCached
  description   only when the field has one

and each type is

  name, isArray
  options, description, fields, minLength, maxLength, minimum, maximum,
  pattern, valueDescriptions, patternDescription, format, language
                only when the type has them

A nested object type's fields is an object from field name to a field in
this same shape."
  (let ((fields (%side-fields (%check-signature signature 'signature-fields) side))
        (out (%new-array)))
    (loop for field across fields
          do (vector-push-extend (%field-json field) out))
    out))

(defun json-schema (signature &key (side :input) (title "Schema") strict
                                   flexible-json-as-string options)
  "A JSON Schema for SIGNATURE's fields on SIDE, as a JSON object.

SIDE is :INPUT or :OUTPUT. TITLE becomes the schema's title. Internal
fields are excluded, as they never reach a model.

  strict                   when true, set strictStructuredOutputs: every
                           property becomes required and an optional one
                           becomes nullable
  flexible-json-as-string  when true, set flexibleJsonFieldsAsString:
                           render an unshaped json or object field as a
                           string
  options                  a JSON object of raw Ax schema options, applied
                           first, so the two keywords above win over it

Signals SIGNATURE-ERROR when a field's constraints cannot be expressed."
  (let ((schema-options (%new-object)))
    (when options
      (unless (hash-table-p options)
        (error 'ax-error
               :message (format nil "json-schema: :options must be a JSON object, got ~S" options)))
      (dolist (key (%object-keys options))
        (%set-key schema-options key (gethash key options))))
    (when strict
      (%set-key schema-options "strictStructuredOutputs" 'yason:true))
    (when flexible-json-as-string
      (%set-key schema-options "flexibleJsonFieldsAsString" 'yason:true))
    (axllm/core::to-json-schema
     (%side-fields (%check-signature signature 'json-schema) side)
     title
     schema-options)))

;;; ------------------------------------------------------------------
;;; Internals
;;; ------------------------------------------------------------------

(defun %check-signature (signature caller)
  (unless (equal (axllm/core::core-record-kind signature) "AxSignature")
    (error 'ax-error
           :message (format nil "~(~A~): expected a signature from parse-signature, got ~S"
                            caller signature)))
  signature)

(defun %side-fields (signature side)
  "SIGNATURE's input or output field vector, for SIDE."
  (let ((key (case side
               ((:input :inputs) "input_fields")
               ((:output :outputs) "output_fields")
               (t (error 'ax-error
                         :message (format nil ":side must be :input or :output, got ~S" side))))))
    (let ((fields (gethash key signature :null)))
      (if (and (vectorp fields) (not (stringp fields)))
          fields
          (%new-array)))))

(defun %field-json (field)
  "One Field record as its published JSON object."
  (let ((out (%new-object)))
    (%set-key out "name" (axllm/core::core-get field "name"))
    (%set-key out "title" (axllm/core::core-get field "title"))
    (%set-key out "type" (%type-json (axllm/core::core-get field "type")))
    (%set-key out "isOptional" (%json-bool (axllm/core::core-get field "is_optional")))
    (%set-key out "isInternal" (%json-bool (axllm/core::core-get field "is_internal")))
    (%set-key out "isCached" (%json-bool (axllm/core::core-get field "is_cached")))
    (%put-present out "description" (axllm/core::core-get field "description"))
    out))

(defun %type-json (type)
  "One FieldType record as its published JSON object."
  (let ((out (%new-object)))
    (%set-key out "name" (axllm/core::core-get type "name"))
    (%set-key out "isArray" (%json-bool (axllm/core::core-get type "is_array")))
    (%put-present out "options" (axllm/core::core-get type "options"))
    (%put-present out "description" (axllm/core::core-get type "description"))
    (let ((fields (axllm/core::core-get type "fields")))
      (when (hash-table-p fields)
        (let ((nested (%new-object)))
          (loop for field across (axllm/core::core-fields-from-map fields)
                do (%set-key nested (axllm/core::core-get field "name") (%field-json field)))
          (%set-key out "fields" nested))))
    (%put-present out "minLength" (axllm/core::core-get type "min_length"))
    (%put-present out "maxLength" (axllm/core::core-get type "max_length"))
    (%put-present out "minimum" (axllm/core::core-get type "minimum"))
    (%put-present out "maximum" (axllm/core::core-get type "maximum"))
    (%put-present out "pattern" (axllm/core::core-get type "pattern"))
    (%put-present out "valueDescriptions" (axllm/core::core-get type "value_descriptions"))
    (%put-present out "patternDescription" (axllm/core::core-get type "pattern_description"))
    (%put-present out "format" (axllm/core::core-get type "format"))
    (%put-present out "language" (axllm/core::core-get type "language"))
    out))

(defun %put-present (object key value)
  "Set KEY only when VALUE is present, so an absent field stays absent."
  (unless (eq value :null)
    (%set-key object key value)))

(defun %json-bool (value)
  "VALUE as a JSON boolean, so a field flag is always true or false."
  (if (axllm/core::core-true-p value) 'yason:true 'yason:false))
