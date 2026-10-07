;;;; validation.lisp --- public boundaries over Core's field validators.
(in-package #:axllm)

(defun validate-fields (signature values &key (side :input) (context "input"))
  "Validate VALUES against one side of SIGNATURE; signal VALIDATION-ERROR."
  (axllm/core::validate-fields (%side-fields (%check-signature signature 'validate-fields) side)
                              values context))

(defun validate-output (signature values)
  "Validate and normalize output field aliases using Core."
  (axllm/core::validate-output (%side-fields (%check-signature signature 'validate-output) :output) values))

(defun validate-value (field value &optional (path "value"))
  "Validate VALUE against a Field object, including nested constraints."
  (axllm/core::validate-value field value path))

(defun strip-internal (signature values)
  "Return the public output values, excluding internal fields."
  (axllm/core::strip-internal (%side-fields (%check-signature signature 'strip-internal) :output) values))
