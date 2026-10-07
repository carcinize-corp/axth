;;;; ax-example:start
;;;; title: Common Lisp Production Telemetry And Budgets
;;;; group: providers
;;;; description: Runs a program under a cost tracker, a process-wide logger, and a response cache, then reports usage.
;;;; provider: openai
;;;; env: OPENAI_API_KEY, OPENAI_APIKEY
;;;; level: advanced
;;;; order: 30
;;;; ax-example:end

;;;; What changes when a program goes to production: a budget that can stop it,
;;;; a logger that redacts, and a cache so a repeated call costs nothing. All
;;;; three are process-wide globals plus per-call options, not a wrapper.

(defpackage #:ax-example/production-telemetry
  (:use #:cl))

(in-package #:ax-example/production-telemetry)

(defun api-key ()
  (or (uiop:getenv "OPENAI_API_KEY")
      (uiop:getenv "OPENAI_APIKEY")
      (error "Set OPENAI_API_KEY or OPENAI_APIKEY to run this example.")))

(defun client ()
  (ax:ai :name "openai"
         :model (or (uiop:getenv "AX_OPENAI_MODEL") "gpt-5.4-mini")
         :api-key (api-key)))

(defparameter +responses+ (make-hash-table :test #'equal)
  "A toy response cache. A caching function reads with a key alone and writes
when it is given a value, so one function serves both directions.")

(defun caching-function (key &optional (value nil value-supplied))
  (if value-supplied
      (progn (setf (gethash key +responses+) value) :null)
      (multiple-value-bind (hit found) (gethash key +responses+)
        (if found hit :null))))

(let* ((shared (client))
       (tracker (ax:make-cost-tracker :max-tokens 20000))
       (program (ax:ax "question:string -> answer:string"))
       (previous (ax:get-global "logger")))
  (unwind-protect
       (progn
         ;; A logger that hides content keeps prompts out of the log.
         (ax:set-global "logger" (ax:create-default-text-logger nil ax:true))
         (dotimes (attempt 2)
           (multiple-value-bind (output usage)
               (ax:forward program shared
                           (ax:object "question" "Name one benefit of bounded retries.")
                           (ax:object "cachingFunction" #'caching-function))
             (format t "~&attempt ~d  : ~a~%" (1+ attempt) (ax:jget output "answer"))
             (let ((total (ax:jget usage "total_tokens")))
               (when (realp total)
                 (ax:track-tokens tracker total (ax:ai-model shared))))))
         (format t "~&cached     : ~d key(s)~%" (hash-table-count +responses+))
         (format t "~&tokens     : ~a~%" (ax:cost-tracker-total-tokens tracker))
         (format t "~&cost       : ~a~%" (ax:cost-tracker-cost tracker))
         (format t "~&over budget: ~a~%" (ax:cost-tracker-limit-reached-p tracker))
         (format t "~&usage      : ~a~%" (ax:encode-json (ax:program-usage program))))
    (ax:set-global "logger" previous)))
