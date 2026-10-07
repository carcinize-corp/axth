(in-package #:cl-user)

(defun run-jiti-tests ()
  (dolist (invalid (list nil (lambda () :not-a-service)))
    (assert (handler-case (progn (ax:make-jiti-proposer invalid) nil)
              (axllm::jiti-action-error () t))))
  ;; Exercise the public factory and a service wrapper, not just action
  ;; validation. A concrete client class guard rejects the wrapper even
  ;; though it supports the same chat protocol.
  (let* ((requests '())
         (client (ax:ai :name "openai" :model "gpt-5.4-mini" :api-key "test-key"
                        :transport
                        (lambda (url headers body)
                          (declare (ignore url headers))
                          (push (ax:parse-json body) requests)
                          (values
                           (ax:encode-json
                            (ax:object "choices"
                                       (vector (ax:object
                                                "index" 0 "finish_reason" "stop"
                                                "message" (ax:object "role" "assistant"
                                                                     "content" "Action: abort")))))
                           200))))
         (boundary (ax:boundary-service client (ax:object))))
    (dolist (service (list client boundary))
      (assert (equal '(:action :abort)
                     (funcall (ax:make-jiti-proposer service)
                              '(:status :paused :goal "Stop without executing code.")))))
    (assert (= 2 (length requests)))
    (dolist (request requests)
      (assert (equal "gpt-5.4-mini" (ax:jget request "model")))
      (assert (search "Stop without executing code." (ax:encode-json request)))))
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
