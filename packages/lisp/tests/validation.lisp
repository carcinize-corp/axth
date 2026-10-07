(in-package #:axllm/tests)

(defun run-validation-fixture (fixture)
  (let ((kind (ax:jget fixture "kind")) (result :null) (failure nil))
    (handler-case
        (setf result
              (cond
                ((equal kind "validate_value")
                 (axllm::validate-value (axllm::%builder-field (ax:jget fixture "field_name")
                                                              (ax:jget fixture "field"))
                                         (ax:jget fixture "value")))
                ((member kind '("validate_output" "strip_internal") :test #'equal)
                 (let ((signature (ax:signature-from-spec (ax:jget fixture "signature_spec"))))
                   (funcall (if (equal kind "validate_output") #'axllm::validate-output #'axllm::strip-internal)
                            signature (ax:jget fixture "values"))))
                (t (fail "Unhandled validation kind ~A" kind))))
      (ax:validation-error (condition) (setf failure condition)))
    (if (nth-value 1 (gethash "expected_error_contains" fixture))
        (progn
          (assert failure () "~A unexpectedly accepted invalid values" (ax:jget fixture "name"))
          (assert (search (ax:jget fixture "expected_error_contains") (princ-to-string failure)))
          (when (nth-value 1 (gethash "expected_error_message" fixture))
            (assert-equal (princ-to-string failure) (ax:jget fixture "expected_error_message") "validation diagnostic")))
        (progn
          (when failure (error failure))
          (dolist (key '("expected_values" "expected_output"))
            (when (nth-value 1 (gethash key fixture)) (assert-equal result (ax:jget fixture key) key)))))))

(defun run-validation-tests ()
  (let ((passed 0) (failures nil))
    (dolist (path (fixture-files "validation"))
      (handler-case (progn
                      (run-validation-fixture (read-fixture path))
                      (axllm/conformance:record-result "validation" path :semantic)
                      (incf passed))
        (error (condition) (push (cons path (princ-to-string condition)) failures))))
    (dolist (failure (reverse failures)) (format t "FAIL ~A~%~A~%" (car failure) (cdr failure)))
    (format t "Field validation: ~D passed, ~D failed~%" passed (length failures))
    (assert (and (plusp passed) (null failures)))))
