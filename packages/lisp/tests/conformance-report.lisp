;;;; A receipt of fixtures actually executed by the native test runners.
;;;; Standalone subsystem tests do not record until START-REPORT enables it.
(defpackage #:axllm/conformance
  (:use #:cl)
  (:export #:start-report #:record-result #:finish-report #:run-report-tests))

(in-package #:axllm/conformance)

(defvar *expected* nil)
(defvar *results* nil)
(defparameter +suites+
  '("signature" "schema" "validation" "prompt" "axgen" "axprogram"
    "axai" "axagent" "axagent-real" "axoptimize" "axflow" "axmcp" "axevent"))

(defun start-report (root)
  "Require a nonempty inventory for every suite before recording any result."
  (let ((expected (make-hash-table :test #'equal)))
    (dolist (suite +suites+)
      (let ((files (directory
                    (merge-pathnames (format nil "~A/*.json" suite)
                                     (uiop:ensure-directory-pathname root)))))
        (unless files (error "Missing or empty conformance suite: ~A" suite))
        (dolist (file files)
          (setf (gethash (format nil "~A/~A" suite (file-namestring file)) expected) t))))
    (setf *expected* expected
          *results* (make-hash-table :test #'equal))))

(defun record-result (suite file category)
  "Record a successful fixture dispatch, using its disk filename, not its title."
  (when *expected*
    (let ((id (format nil "~A/~A" suite (file-namestring file)))
          (category (string-downcase (string category))))
      (unless (gethash id *expected*)
        (error "Conformance result is outside the fixture inventory: ~A" id))
      (when (gethash id *results*)
        (error "Duplicate conformance result: ~A" id))
      (unless (member category '("semantic" "validation-error" "transport-boundary")
                      :test #'string=)
        (error "Conformance fixture ~A did not pass: ~A" id category))
      (setf (gethash id *results*) category))))

(defun report-value ()
  (unless (and *expected* (plusp (hash-table-count *expected*)))
    (error "No conformance inventory was loaded"))
  (let ((ids (sort (loop for id being the hash-keys of *expected* collect id) #'string<)))
    (dolist (id ids)
      (unless (gethash id *results*)
        (error "Conformance fixture was not executed: ~A" id)))
    (ax:object "schema_version" "axir-lisp-conformance-v1"
               "fixtures" (map 'vector
                               (lambda (id)
                                 (ax:object "id" id "category" (gethash id *results*)))
                               ids))))

(defun finish-report (&optional (path (uiop:getenv "AXIR_CONFORMANCE_REPORT")))
  "Check completeness even when no receipt file was requested."
  (let ((value (report-value)))
    (when (and path (plusp (length path)))
      (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
        (write-line (ax:encode-json value) out)))
    (format t "Conformance receipt: ~D fixtures, complete~%"
            (length (ax:jget value "fixtures")))
    t))

(defun run-report-tests ()
  (let ((*expected* (make-hash-table :test #'equal))
        (*results* (make-hash-table :test #'equal)))
    (flet ((refused (thunk)
             (assert (handler-case (progn (funcall thunk) nil) (error () t)))))
      (refused #'report-value)
      (setf (gethash "axgen/a.json" *expected*) t
            (gethash "axgen/b.json" *expected*) t)
      (record-result "axgen" #P"/anywhere/a.json" :semantic)
      (refused #'report-value)
      (refused (lambda () (record-result "axgen" "a.json" :semantic)))
      (refused (lambda () (record-result "axgen" "unknown.json" :semantic)))
      (dolist (category '(:failed :blocked :partial :skipped :explicitly-not-claimed))
        (refused (lambda () (record-result "axgen" "b.json" category))))
      (record-result "axgen" "b.json" :validation-error)
      (assert (string=
               "{\"schema_version\":\"axir-lisp-conformance-v1\",\"fixtures\":[{\"id\":\"axgen/a.json\",\"category\":\"semantic\"},{\"id\":\"axgen/b.json\",\"category\":\"validation-error\"}]}"
               (ax:encode-json (report-value))))))
  (format t "Conformance receipt guards: PASS~%")
  t)
