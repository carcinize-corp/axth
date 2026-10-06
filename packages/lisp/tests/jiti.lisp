(in-package #:cl-user)

(defun run-jiti-tests ()
  (assert (handler-case (progn (ax:make-jiti-proposer nil) nil)
            (type-error () t)))
  (flet ((rejects (output &optional view)
           (assert (handler-case (progn (axllm::jiti-action output view) nil)
                     (axllm::jiti-action-error () t)))))
    (assert (equal '(:action :execute :source "(+ 2 7)" :preview nil)
                   (axllm::jiti-action
                    (ax:object "action" "execute" "source" "(+ 2 7)"
                               "preview" 'yason:false) nil)))
    (assert (equal '(:action :execute :source "(+ 2 7)" :preview t)
                   (axllm::jiti-action
                    (ax:object "action" "execute" "source" "(+ 2 7)"
                               "preview" 'yason:true) nil)))
    (rejects (ax:object "action" "execute" "source" "(+ 2 7)"))
    (rejects (ax:object "action" "execute" "source" "(+ 2 7)" "preview" "false"))
    (rejects (ax:object "action" "abort" "source" "(error :unexpected)"))
    (rejects (ax:object "action" "develop" "source" (make-string 16385)))
    (rejects (ax:object "action" "resume" "restartId" "old" "arguments" "nil")
             '(:restarts ((:id "current" :name "RETRY"))))
    (assert (equal '(:action :resume :restart-id "current" :arguments "(list 8)")
                   (axllm::jiti-action
                    (ax:object "action" "resume" "restartId" "current" "arguments" "(list 8)")
                    '(:restarts ((:id "current" :name "RETRY"))))))
    (assert (equal '(:action :abort) (axllm::jiti-action (ax:object "action" "abort") nil))))
  ;; The menu comes last in Jiti's view. A large earlier value must not hide it.
  (let* ((view (list :status :paused :generation 17
                     :observation (make-string 20000 :initial-element #\x)
                     :goal "Repair the failing operation."
                     :condition "choose a restart"
                     :restarts '((:id "17/0" :name "RETRY" :report "first")
                                 (:id "17/1" :name "RETRY" :report "second"))))
         (sent (ax:parse-json (axllm::jiti-observation view 80))))
    (assert (equal "paused" (ax:jget sent "status")))
    (assert (= 17 (ax:jget sent "generation")))
    (assert (equal "choose a restart" (ax:jget sent "condition")))
    (assert (equal "Repair the failing operation." (ax:jget sent "goal")))
    (assert (equalp #("17/0" "17/1")
                    (map 'vector (lambda (r) (ax:jget r "id"))
                         (ax:jget sent "currentRestarts"))))
    (assert (search "[truncated]" (ax:jget sent "context")))
    (assert (< (length (ax:jget sent "context")) 100)))
  (format t "Jiti adapter validation: PASS~%"))
