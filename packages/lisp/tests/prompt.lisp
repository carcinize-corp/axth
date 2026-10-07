(in-package #:axllm/tests)

(defun run-prompt-fixture (fixture)
  (let* ((spec (ax:jget fixture "signature_spec"))
         (signature (if (hash-table-p spec) (ax:signature-from-spec spec)
                        (ax:parse-signature (ax:jget fixture "signature"))))
         (options (ax:jget fixture "options" (ax:object)))
         (template (axllm::prompt-template
                    signature :functions (ax:jget fixture "tools" #())
                    :custom-template (ax:jget options "customTemplate" nil)
                    :structured-output-function-name (ax:jget options "structuredOutputFunctionName" nil)
                    :include-optional-input-fields-in-system-prompt
                    (eq (ax:jget options "includeOptionalInputFieldsInSystemPrompt") ax:true))))
    (multiple-value-bind (instruction present) (gethash "instruction" fixture)
      (when present (setf (axllm::prompt-template-instruction template) instruction)))
    (assert-equal (axllm::render-prompt-template template (ax:jget fixture "input" (ax:object)) options)
                  (ax:jget fixture "expected_messages") (ax:jget fixture "name"))))

(defun run-prompt-value-tests ()
  ;; Expectations follow src/ax/dsp/util.ts validateValue, not output
  ;; validation: numeric strings and mixed arrays fail without coercion.
  (dolist (case '(("age:number -> answer:string" "age" "oops"
                   "Validation failed: Expected 'age' to be a number instead got 'string' (\"oops\")")
                  ("ages:number[] -> answer:string" "ages" #(1 "oops")
                   "Validation failed: Expected 'ages' to be a an array of number instead got 'array' ([1,\"oops\"])")
                  ("ages:number[] -> answer:string" "ages" 12
                   "Validation failed: Expected 'ages' to be a an array of number instead got 'number' (12)")
                  ("query:string -> answer:string" "query" 42
                   "Validation failed: Expected 'query' to be a string instead got 'number' (42)")
                  ("enabled:boolean -> answer:string" "enabled" "false"
                   "Validation failed: Expected 'enabled' to be a boolean instead got 'string' (\"false\")")
                  ("payload:json -> answer:string" "payload" 7
                   "Validation failed: Expected 'payload' to be a json instead got 'number' (7)")))
    (destructuring-bind (signature name value expected) case
      (let ((failure (handler-case
                         (progn (ax:render-prompt-template (ax:prompt-template signature)
                                                           (ax:object name value)) nil)
                       (ax:validation-error (condition) condition))))
        (assert failure () "Prompt accepted invalid ~A: ~S" name value)
        (assert-equal (princ-to-string failure) expected "prompt input type diagnostic"))))
  ;; Do not implement the fix by invoking constrained output validation.
  ;; TS accepts these values in prompts, including an overlong UTF-16 string.
  (dolist (case (list (list (ax:f "string" :max 1) "😀")
                     (list (ax:f "number" :min 10 :max 20) 3)
                     (list (ax:f "number" :array t) #(1 2))
                     (list (ax:f "boolean") ax:false)
                     (list (ax:f "json") #(1 "two"))
                     (list (ax:f "object" :fields (ax:object "required" (ax:f "number")))
                           (ax:object "other" "not recursively validated"))))
    (destructuring-bind (field value) case
      (let* ((signature (ax:s :inputs (ax:object "payload" field)
                             :outputs (ax:object "answer" (ax:f "string"))))
             (messages (ax:render-prompt-template (ax:prompt-template signature)
                                                  (ax:object "payload" value))))
        (assert (= (length messages) 2))
        (assert (search "Payload:" (ax:jget (aref messages 1) "content"))))))
  (let ((start (local-time:unix-to-timestamp 1715268645 :nsec 123000000))
        (end (local-time:unix-to-timestamp 1715400000)))
    (dolist (case (list (list "date" start "2024-05-09")
                       (list "datetime" start "2024-05-09T15:30:45Z")
                       (list "dateRange" (ax:object "start" start "end" end)
                             (format nil "{~%  \"start\": \"2024-05-09\",~%  \"end\": \"2024-05-11\"~%}"))
                       (list "datetimeRange" (ax:object "start" start "end" end)
                             (format nil "{~%  \"start\": \"2024-05-09T15:30:45Z\",~%  \"end\": \"2024-05-11T04:00:00Z\"~%}"))
                       ;; Arrays take JSON.stringify's full-instant path, not
                       ;; the scalar date/day and datetime/second formatting.
                       (list "date[]" (vector start "2024-05-11")
                             (format nil "[~%  \"2024-05-09T15:30:45.123Z\",~%  \"2024-05-11\"~%]"))
                       (list "datetime[]" (vector start end)
                             (format nil "[~%  \"2024-05-09T15:30:45.123Z\",~%  \"2024-05-11T04:00:00.000Z\"~%]"))
                       (list "dateRange[]" (vector (ax:object "start" start "end" end))
                             (format nil "[~%  {~%    \"start\": \"2024-05-09T15:30:45.123Z\",~%    \"end\": \"2024-05-11T04:00:00.000Z\"~%  }~%]"))
                       (list "json" (ax:object "when" start "list" (vector end))
                             (format nil "{~%  \"when\": \"2024-05-09T15:30:45.123Z\",~%  \"list\": [~%    \"2024-05-11T04:00:00.000Z\"~%  ]~%}"))
                       (list "json" start "\"2024-05-09T15:30:45.123Z\"")))
      (destructuring-bind (type value text) case
        (let* ((template (ax:prompt-template (format nil "payload:~A -> answer:string" type)))
               (messages (ax:render-prompt-template template (ax:object "payload" value))))
          (assert-equal (ax:jget (aref messages 1) "content") (format nil "Payload: ~A~%" text)
                        "native date prompt rendering")))))
  (format t "Prompt input types and native dates: PASS~%"))

(defun run-prompt-tests ()
  (let ((passed 0) (failures nil))
    (dolist (path (fixture-files "prompt"))
      (let ((fixture (read-fixture path)))
        (when (equal (ax:jget fixture "kind") "prompt")
          (handler-case (progn
                          (run-prompt-fixture fixture)
                          (axllm/conformance:record-result "prompt" path :semantic)
                          (incf passed))
            (error (condition) (push (cons path (princ-to-string condition)) failures))))))
    (dolist (failure (reverse failures)) (format t "FAIL ~A~%~A~%" (car failure) (cdr failure)))
    (format t "Prompt rendering: ~D passed, ~D failed~%" passed (length failures))
    (assert (and (plusp passed) (null failures))))
  (run-prompt-value-tests))
