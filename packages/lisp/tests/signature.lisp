;;;; signature.lisp --- Core signature and JSON Schema conformance.
;;;;
;;;; These tests read ir/conformance/signature and ir/conformance/schema
;;;; directly, the same fixtures every other Ax port runs, and compare
;;;; against each fixture's recorded expectation. Nothing is asserted
;;;; against this implementation's own output.
;;;;
;;;; Three fixture kinds appear in those directories and all three run
;;;; here:
;;;;
;;;;   signature        parse, then compare the whole field payload; when
;;;;                    the fixture records expected_to_string, also
;;;;                    compare the rendering and re-parse it to check the
;;;;                    round trip
;;;;   signature_error  expect a failure, and check both the error category
;;;;                    and the recorded message fragment
;;;;   json_schema      build the signature, generate the schema for the
;;;;                    fixture's target side with its options, and compare
;;;;                    the whole schema
;;;;
;;;; A kind this file does not handle is a failure naming the kind, never a
;;;; silent pass: an unrun fixture must not look like a passing one.
;;;;
;;;; One piece of scaffolding lives here rather than in the library. Ten
;;;; schema fixtures and three signature fixtures describe their signature
;;;; with signature_spec, Ax's fluent field builder, instead of signature
;;;; text. The builder is target-side API and is not part of this
;;;; experimental subset, so this file builds the Core Field records that
;;;; the builder would produce and feeds them to Core. The code under test
;;;; is still entirely Core: validation, rendering and schema generation.

(defpackage #:axllm/tests
  (:use #:cl)
  (:export #:run-all-tests))

(in-package #:axllm/tests)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(define-condition fixture-failure (error)
  ((detail :initarg :detail :reader fixture-failure-detail))
  (:report (lambda (condition stream)
             (write-string (fixture-failure-detail condition) stream))))

(defun fail (format-control &rest arguments)
  (error 'fixture-failure :detail (apply #'format nil format-control arguments)))

(defun conformance-directory ()
  "Where the shared AxIR fixtures live."
  (let ((override (uiop:getenv "AXIR_CONFORMANCE_DIR")))
    (if (and override (plusp (length override)))
        (uiop:ensure-directory-pathname override)
        (asdf:system-relative-pathname "axllm" "../../ir/conformance/"))))

(defun fixture-files (suite)
  (sort (directory (merge-pathnames (concatenate 'string suite "/*.json")
                                    (conformance-directory)))
        #'string< :key #'namestring))

(defun read-fixture (path)
  (ax:parse-json (uiop:read-file-string path)))

(defun same-value-p (left right)
  "Whether two JSON values are equal: arrays by order, objects by key set."
  (axllm/core::core-value-equal left right))

(defun assert-equal (actual expected label)
  (unless (same-value-p actual expected)
    (fail "~a mismatch~%    expected: ~a~%    actual:   ~a"
          label (show expected) (show actual))))

(defun show (value)
  (if (stringp value) (format nil "~s" value) (ax:encode-json value)))

(defun truthy (value)
  (axllm/core::core-true-p value))

;;; ------------------------------------------------------------------
;;; Signature payloads
;;; ------------------------------------------------------------------

(defun signature-payload (signature)
  "SIGNATURE in the shape the shared fixtures record it in."
  (ax:object "description" (ax:jget signature "description")
             "inputs" (ax:signature-fields signature :side :input)
             "outputs" (ax:signature-fields signature :side :output)))

(defun build-signature (fixture)
  (let ((spec (ax:jget fixture "signature_spec"))
        (text (ax:jget fixture "signature")))
    (cond ((ax:jget fixture "signature_spec" nil) (signature-from-spec spec))
          ((stringp text) (ax:parse-signature text))
          (t (fail "fixture has neither a signature string nor a signature_spec")))))

;;; ------------------------------------------------------------------
;;; signature_spec: the Core records Ax's fluent builder would produce
;;; ------------------------------------------------------------------

(defstruct (fluent (:constructor %make-fluent))
  type
  (description :null)
  (item-description :null)
  (optional 'yason:false)
  (internal 'yason:false)
  (cached 'yason:false))

(defun new-field-type (name fields)
  (axllm/core::core-record-new
   "FieldType"
   (ax:object "name" name "is_array" 'yason:false "fields" (or fields :null))))

(defun type-get (type key)
  (axllm/core::core-get type key))

(defun type-set (type key value)
  (axllm/core::core-set type key value))

(defun copy-json-array (value)
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (when (and (vectorp value) (not (stringp value)))
      (loop for item across value do (vector-push-extend item out)))
    out))

(defun nested-field-map (fields-spec)
  "A field-name to Field map, as the fluent builder's object() produces."
  (when (hash-table-p fields-spec)
    (let ((out (axllm::%new-object)))
      (dolist (key (axllm::%object-keys fields-spec))
        (let* ((nested (fluent-to-field (fluent-from-spec (gethash key fields-spec)) key))
               (type (axllm/core::core-get nested "type"))
               (description (axllm/core::core-get nested "description")))
          ;; The builder pushes a nested field's own description onto its
          ;; type when the type has none, which is what makes a nested
          ;; description survive into the rendered signature.
          (when (and (not (eq description :null)) (eq (type-get type "description") :null))
            (type-set type "description" description))
          (axllm::%set-key out key nested)))
      out)))

(defun fluent-from-spec (spec)
  "One fixture field spec as the fluent field the builder would build."
  (let* ((raw-type (ax:jget spec "type" "string"))
         (type-name (if (eq raw-type :null) "string" raw-type))
         (description (ax:jget spec "description"))
         (field (%make-fluent :type nil)))
    (cond ((string= type-name "class")
           (let ((options (ax:jget spec "options")))
             (unless (and (vectorp options) (not (stringp options)) (plusp (length options)))
               (error 'ax:signature-error
                      :message "classification() requires at least one option"))
             (setf (fluent-type field) (new-field-type "class" nil))
             (type-set (fluent-type field) "options" (copy-json-array options))))
          ((string= type-name "object")
           (setf (fluent-type field)
                 (new-field-type "object" (nested-field-map (ax:jget spec "fields")))))
          (t (setf (fluent-type field) (new-field-type type-name nil))))
    (setf (fluent-description field) description
          (fluent-item-description field) description)
    (let ((type (fluent-type field)))
      ;; The builder applies these in a fixed order, and the order is
      ;; observable: array() can move the item description onto the type
      ;; before min()/max() read the type's name.
      (when (truthy (ax:jget spec "array" 'yason:false))
        (type-set type "is_array" 'yason:true)
        (when (and (not (eq (fluent-item-description field) :null))
                   (eq (type-get type "description") :null))
          (type-set type "description" (fluent-item-description field)))
        (let ((array-description (ax:jget spec "arrayDescription")))
          (unless (eq array-description :null)
            (setf (fluent-description field) array-description))))
      (when (truthy (ax:jget spec "optional" 'yason:false))
        (setf (fluent-optional field) 'yason:true))
      (when (truthy (ax:jget spec "internal" 'yason:false))
        (setf (fluent-internal field) 'yason:true))
      (when (truthy (ax:jget spec "cache" 'yason:false))
        (setf (fluent-cached field) 'yason:true))
      (let ((minimum (ax:jget spec "min")))
        (unless (eq minimum :null)
          (if (string= (type-get type "name") "number")
              (type-set type "minimum" minimum)
              (type-set type "min_length" (round minimum)))))
      (let ((maximum (ax:jget spec "max")))
        (unless (eq maximum :null)
          (if (string= (type-get type "name") "number")
              (type-set type "maximum" maximum)
              (type-set type "max_length" (round maximum)))))
      (when (truthy (ax:jget spec "email" 'yason:false))
        (type-set type "format" "email"))
      (when (truthy (ax:jget spec "url" 'yason:false))
        (type-set type "format" "uri"))
      (let ((value-descriptions (ax:jget spec "valueDescriptions")))
        (unless (eq value-descriptions :null)
          (type-set type "value_descriptions" value-descriptions)
          ;; describe_values() validates through Core, so a fixture with a
          ;; bad described value fails in Core and not in this harness.
          (axllm/core::signature-validate-value-descriptions-impl
           type (type-get type "name"))))
      (let ((pattern (ax:jget spec "pattern")))
        (unless (eq pattern :null)
          (let ((pattern-description (ax:jget spec "patternDescription")))
            (when (eq pattern-description :null)
              (setf pattern-description pattern))
            (type-set type "pattern" pattern)
            (type-set type "pattern_description" pattern-description)))))
    field))

(defun fluent-to-field (field name)
  (axllm/core::core-record-new
   "Field"
   (ax:object "name" name
              "type" (fluent-type field)
              "description" (fluent-description field)
              "is_optional" (fluent-optional field)
              "is_internal" (fluent-internal field)
              "is_cached" (fluent-cached field))))

(defun signature-from-spec (spec)
  "A validated signature built from a fixture's signature_spec."
  (let ((inputs (make-array 0 :adjustable t :fill-pointer 0))
        (outputs (make-array 0 :adjustable t :fill-pointer 0))
        (input-spec (ax:jget spec "inputs"))
        (output-spec (ax:jget spec "outputs")))
    (when (hash-table-p input-spec)
      (dolist (name (axllm::%object-keys input-spec))
        (vector-push-extend (fluent-to-field (fluent-from-spec (gethash name input-spec)) name)
                            inputs)))
    (when (hash-table-p output-spec)
      (dolist (name (axllm::%object-keys output-spec))
        (vector-push-extend (fluent-to-field (fluent-from-spec (gethash name output-spec)) name)
                            outputs)))
    (let ((signature (axllm/core::core-record-new
                      "AxSignature"
                      (ax:object "inputs" inputs
                                 "outputs" outputs
                                 "description" (ax:jget spec "description")))))
      ;; The builder's build() validates, so an invalid fluent signature
      ;; fails the same way an invalid signature string does.
      (axllm/core::validate-signature signature)
      signature)))

;;; ------------------------------------------------------------------
;;; Fixture kinds
;;; ------------------------------------------------------------------

(defun run-signature (fixture)
  (let ((signature (build-signature fixture)))
    (assert-equal (signature-payload signature)
                  (ax:jget fixture "expected_signature")
                  "signature")
    (let ((expected-text (ax:jget fixture "expected_to_string")))
      (unless (eq expected-text :null)
        (let ((rendered (ax:signature-string signature)))
          (assert-equal rendered expected-text "signature rendering")
          ;; Re-parsing the rendering must give the same signature, which
          ;; is what makes the rendering lossless rather than merely
          ;; plausible.
          (assert-equal (signature-payload (ax:parse-signature rendered))
                        (ax:jget fixture "expected_signature")
                        "signature round trip"))))))

(defun error-category (condition)
  (typecase condition
    (ax:signature-error "signature")
    (ax:validation-error "validation")
    (ax:ax-error "runtime")
    (t "runtime")))

(defun run-signature-error (fixture)
  (handler-case
      (build-signature fixture)
    (fixture-failure (condition) (error condition))
    (error (condition)
      (let ((expected-category (ax:jget fixture "expected_error_category"))
            (actual-category (error-category condition))
            (message (princ-to-string condition)))
        (unless (eq expected-category :null)
          (unless (string= expected-category actual-category)
            (fail "expected error category ~s, got ~s (~a: ~a)"
                  expected-category actual-category (type-of condition) message)))
        (let ((fragment (ax:jget fixture "expected_error_contains")))
          (unless (eq fragment :null)
            (unless (search fragment message)
              (fail "expected an error containing ~s, got ~s" fragment message))))
        (return-from run-signature-error t)))
    (:no-error (value)
      (declare (ignore value))
      (fail "expected signature construction to fail, but it succeeded"))))

(defun run-json-schema (fixture)
  (let* ((signature (build-signature fixture))
         (target (ax:jget fixture "target" "outputs"))
         (side (cond ((string= target "inputs") :input)
                     ((string= target "outputs") :output)
                     (t (fail "unsupported schema target ~s" target))))
         (title (let ((value (ax:jget fixture "schema_title" "Schema")))
                  (if (eq value :null) "Schema" value)))
         (options (let ((value (ax:jget fixture "schema_options")))
                    (if (hash-table-p value) value (axllm::%new-object)))))
    (assert-equal (ax:json-schema signature :side side :title title :options options)
                  (ax:jget fixture "expected_schema")
                  "json schema")))

(defparameter +fixture-runners+
  '(("signature" . run-signature)
    ("signature_error" . run-signature-error)
    ("json_schema" . run-json-schema))
  "The fixture kinds this subset claims, and how each one runs.")

(defun run-fixture (fixture)
  (let* ((kind (ax:jget fixture "kind"))
         (runner (cdr (assoc kind +fixture-runners+ :test #'equal))))
    (unless runner
      (fail "fixture kind ~s is not handled by the Lisp subset; implement it or remove the claim"
            kind))
    (funcall runner fixture)))

;;; ------------------------------------------------------------------
;;; Entry point
;;; ------------------------------------------------------------------

(defun run-suite (suite)
  "Run every fixture in SUITE. Returns the pass count and the failures."
  (let ((files (fixture-files suite))
        (passed 0)
        (failures '()))
    (when (null files)
      (push (cons suite (format nil "no fixtures found under ~a" (conformance-directory)))
            failures))
    (dolist (path files)
      (let ((name (pathname-name path)))
        (handler-case
            (progn
              (run-fixture (read-fixture path))
              (axllm/conformance:record-result suite path :semantic)
              (incf passed))
          (error (condition)
            (push (cons name (princ-to-string condition)) failures)))))
    (values passed (nreverse failures))))

(defun run-all-tests ()
  "Run the signature and schema conformance suites. Returns T on success."
  (let ((total-passed 0)
        (total-failed 0))
    (dolist (suite '("signature" "schema"))
      (multiple-value-bind (passed failures) (run-suite suite)
        (incf total-passed passed)
        (incf total-failed (length failures))
        (format t "~&~a: ~d passed, ~d failed (of ~d fixtures)~%"
                suite passed (length failures) (+ passed (length failures)))
        (dolist (failure failures)
          (format t "~&  FAIL ~a~%    ~a~%" (car failure) (cdr failure)))))
    (format t "~&~%total: ~d passed, ~d failed~%" total-passed total-failed)
    (zerop total-failed)))
