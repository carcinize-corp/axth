;;;; template.lisp --- native boundary for Ax's limited prompt templates.
;;;; Grammar and diagnostics follow src/ax/agent/templateEngine.ts. This is
;;;; not a Lisp evaluator: tags are identifiers, comments, or if/else blocks.

(in-package #:axllm/core)

(defparameter +template-identifier-pattern+
  "\\A[A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z_][A-Za-z0-9_]*)*\\z")

(defparameter +template-equality-pattern+
  (format nil
          "\\A([A-Za-z_][A-Za-z0-9_]*(?:\\.[A-Za-z_][A-Za-z0-9_]*)*)[~A]*===[~A]*(?:'([^']*)'|\"([^\"]*)\")\\z"
          +js-whitespace+ +js-whitespace+))

(defun %template-equality (condition)
  "Return the path and literal, or NIL when CONDITION is not equality."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings +template-equality-pattern+ condition)
    (when match
      (values (aref groups 0) (or (aref groups 1) (aref groups 2))))))

(defun %template-utf16-length (source &optional (end (length source)))
  (loop for i below end sum (if (> (char-code (char source i)) #xffff) 2 1)))

(defun %template-error (context source index control &rest arguments)
  ;; Node offsets and columns are UTF-16 code units, as in JavaScript, not
  ;; Lisp character offsets. Only LF starts a new diagnostic line.
  (let ((line 1) (column 1) (offset 0))
    (loop for char across source
          while (< offset index)
          for width = (if (> (char-code char) #xffff) 2 1)
          do (incf offset width)
             (if (char= char #\Newline)
                 (setf line (1+ line) column 1)
                 (incf column width)))
    (%ax-error "~A:~D:~D ~A" context line column
               (apply #'format nil control arguments))))

(defun %template-tokenize (source)
  ;; Equivalent to /{{\s*([^}]+?)\s*}}/g followed by JS trim. Unmatched
  ;; braces are text, including {{}}; {{ }} is a matched, invalid empty tag.
  (let ((tokens (make-array 0 :adjustable t :fill-pointer 0))
        (last-index 0) (cursor 0))
    (loop for start = (search "{{" source :start2 cursor)
          while start
          for end = (position #\} source :start (+ start 2))
          do (if (and end (> end (+ start 2))
                      (< (1+ end) (length source))
                      (char= (char source (1+ end)) #\}))
                 (progn
                   (when (> start last-index)
                     (vector-push-extend
                      (axllm:object "type" "text" "value" (subseq source last-index start))
                      tokens))
                   (vector-push-extend
                    (axllm:object "type" "tag"
                                 "value" (core-string-trim (subseq source (+ start 2) end))
                                 "index" (%template-utf16-length source start))
                    tokens)
                   (setf last-index (+ end 2) cursor last-index))
                 (setf cursor (1+ start))))
    (when (< last-index (length source))
      (vector-push-extend
       (axllm:object "type" "text" "value" (subseq source last-index)) tokens))
    tokens))

(defun %template-parse-range (tokens source context start terminators)
  (let ((nodes (make-array 0 :adjustable t :fill-pointer 0)) (i start))
    (loop while (< i (length tokens))
          for token = (aref tokens i)
          for tag = (axllm:jget token "value")
          for index = (axllm:jget token "index")
          do (cond
               ((equal (axllm:jget token "type") "text")
                (vector-push-extend token nodes) (incf i))
               ((member tag terminators :test #'string=)
                (return-from %template-parse-range (values nodes i tag)))
               ((and (>= (length tag) 3) (string= "if " tag :end2 3))
                (let ((condition (core-string-trim (subseq tag 3))))
                  (unless (or (cl-ppcre:scan +template-identifier-pattern+ condition)
                              (%template-equality condition))
                    (%template-error context source index "Invalid if condition '~A'" condition))
                  (multiple-value-bind (then-nodes next terminator)
                      (%template-parse-range tokens source context (1+ i) '("else" "/if"))
                    (unless terminator
                      (%template-error context source index "Unclosed 'if' block"))
                    (let ((else-nodes #()))
                      (when (string= terminator "else")
                        (multiple-value-bind (otherwise end closing)
                            (%template-parse-range tokens source context (1+ next) '("/if"))
                          (unless (equal closing "/if")
                            (%template-error context source index "Unclosed 'if' block"))
                          (setf else-nodes otherwise next end)))
                      ;; Core's native boundary uses then/else (the Python
                      ;; reference shape), rather than TS-private thenNodes.
                      (vector-push-extend
                       (axllm:object "type" "if" "condition" condition
                                    "then" then-nodes "else" else-nodes "index" index)
                       nodes)
                      (setf i (1+ next))))))
               ((member tag '("else" "/if") :test #'string=)
                (%template-error context source index "Unexpected '~A'" tag))
               ((and (plusp (length tag)) (char= (char tag 0) #\!)) (incf i))
               ((and (>= (length tag) 8) (string= "include " tag :end2 8))
                (%template-error context source index
                                 "Unexpected 'include' directive at runtime (includes must be compiled)"))
               ((not (cl-ppcre:scan +template-identifier-pattern+ tag))
                (%template-error context source index "Invalid tag '~A'" tag))
               (t (vector-push-extend
                   (axllm:object "type" "var" "name" tag "index" index) nodes)
                  (incf i))))
    (values nodes i nil)))

(defun core-template-parse (source context)
  (%template-parse-range (%template-tokenize source) source context 0 nil))

(defun %template-resolve (vars path source context index)
  (let ((current vars) (missing (gensym "MISSING")))
    (dolist (part (cl-ppcre:split "\\." path) current)
      (setf current
            (cond ((hash-table-p current) (axllm:jget current part missing))
                  ;; JS arrays are objects: length is a valid dotted segment.
                  ((and (core-array-p current) (string= part "length"))
                   (length current))
                  (t missing)))
      (when (eq current missing)
        (%template-error context source index "Missing template variable '~A'" path)))))

(defun core-template-render-tree (nodes vars source context)
  (with-output-to-string (out)
    (loop for node across nodes
          for kind = (axllm:jget node "type")
          for index = (axllm:jget node "index")
          do (cond
               ((string= kind "text") (write-string (axllm:jget node "value") out))
               ((string= kind "var")
                (let* ((name (axllm:jget node "name"))
                       (value (%template-resolve vars name source context index)))
                  (unless (or (stringp value) (realp value)
                              (eq value axllm:true) (eq value axllm:false))
                    (%template-error context source index
                                     "Variable '~A' must be string, number, or boolean" name))
                  (write-string (core-js-text value) out)))
               (t
                (let* ((condition (axllm:jget node "condition"))
                       (truth
                         (multiple-value-bind (path expected) (%template-equality condition)
                           (let ((value (%template-resolve vars (or path condition)
                                                           source context index)))
                             (if path
                                 (and (stringp value) (string= value expected))
                                 (progn
                                   (unless (or (eq value axllm:true) (eq value axllm:false))
                                     (%template-error context source index
                                                      "Condition '~A' must be boolean" condition))
                                   (eq value axllm:true)))))))
                  (write-string
                   (core-template-render-tree (axllm:jget node (if truth "then" "else"))
                                              vars source context)
                   out)))))))

(defun core-template-collect-vars (nodes)
  (let ((names (make-hash-table :test #'equal)))
    (labels ((walk (nodes)
               (loop for node across nodes
                     for kind = (axllm:jget node "type")
                     do (cond
                          ((string= kind "var") (setf (gethash (axllm:jget node "name") names) t))
                          ((string= kind "if")
                           (let ((condition (axllm:jget node "condition")))
                             (setf (gethash (or (%template-equality condition) condition) names) t))
                           (walk (axllm:jget node "then"))
                           (walk (axllm:jget node "else")))))))
      (walk nodes))
    (coerce (sort (loop for name being the hash-keys of names collect name) #'string<) 'vector)))

(defun core-template-validate (source context &optional (required-variables #()))
  (handler-case
      (let ((present (core-template-collect-vars (core-template-parse source context))))
        (loop for variable in (core-elements required-variables)
              unless (find variable present :test #'equal)
                do (return-from core-template-validate
                     (format nil "must preserve template variable {{~A}}" variable)))
        axllm:true)
    (error (condition) (princ-to-string condition))))

(in-package #:axllm)

(defun render-template-content (source &optional (vars (object)) (context "inline-template"))
  "Render SOURCE with JSON object VARS using Ax's limited template grammar."
  (axllm/core::core-template-render-tree
   (axllm/core::core-template-parse source context) vars source context))

(defun collect-template-variable-names (source &optional (context "template-vars"))
  "Return a sorted, deduplicated vector of variable paths in both branches."
  (axllm/core::core-template-collect-vars (axllm/core::core-template-parse source context)))

(defun validate-prompt-template-syntax
    (source &optional (context "template-validate") (required-variables #()))
  "Return AX:TRUE when valid, otherwise the exact syntax/preservation error."
  (axllm/core::core-template-validate source context required-variables))
