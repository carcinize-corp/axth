;;;; Run from any directory with JITI_ROOT pointing to a checkout of ghuntley/jiti.
(require :asdf)
(let ((root (uiop:getenv "JITI_ROOT")))
  (unless root (error "Set JITI_ROOT to a Jiti checkout."))
  (asdf:load-asd (merge-pathnames "image-agent.asd" (uiop:ensure-directory-pathname root))))
(asdf:load-asd (merge-pathnames "../axllm.asd" *load-truename*))
(asdf:load-system "axllm/jiti")
(asdf:load-system "image-agent/store")

(defun request-observation (request)
  (let* ((messages (ax:jget request "input"))
         (parts (ax:jget (aref messages 0) "content"))
         (content (ax:jget (aref parts 0) "text"))
         (prefix "Observation: "))
    (assert (and (>= (length content) (length prefix))
                 (string= prefix content :end2 (length prefix))))
    (ax:parse-json (subseq content (length prefix)))))

(let* ((world (image-agent:make-reference-world :initial '((:x . 0))))
       (session (image-agent:make-session
                 world :budget 8
                 :goals (list (cons :repaired
                                    (lambda () (= 7 (gethash :x (image-agent:reference-table world))))))))
       (calls 0)
       (client
         ;; The scripted wire response below is Responses dialect. Pin its
         ;; profile so a model-catalog change cannot silently reinterpret it.
         (ax:ai :name "openai-responses" :model "gpt-6-luna" :api-key "test-only"
                :transport
                (lambda (url headers body)
                  (declare (ignore url headers))
                  (let* ((request (ax:parse-json body))
                         (observation (request-observation request))
                         (text
                           (case (incf calls)
                             (1 (format nil "Action: develop~%Source: (defun next-value () (error \"repair\"))"))
                             (2 (format nil "Action: execute~%Source: (setf (gethash :x *state*) (restart-case (next-value) (retry () (next-value))))~%Preview: false"))
                             (3 (assert (equal "paused" (ax:jget observation "status")))
                                (format nil "Action: develop~%Source: (defun next-value () 7)"))
                             (4 (let ((restart (find "RETRY" (ax:jget observation "currentRestarts")
                                                     :key (lambda (r) (ax:jget r "name")) :test #'equal)))
                                  (assert restart)
                                  (assert (search "[truncated]" (ax:jget observation "context")))
                                  (format nil "Action: resume~%Restart Id: ~a~%Arguments: nil" (ax:jget restart "id"))))
                             (otherwise (error "Unexpected model request.")))))
                    (assert (equal "gpt-6-luna" (ax:jget request "model")))
                    (values
                     (ax:encode-json
                      (ax:object "status" "completed" "output"
                                 (vector (ax:object "type" "message" "role" "assistant"
                                                   "content" (vector (ax:object "type" "output_text"
                                                                                 "text" text))))
                                 "usage" (ax:object "input_tokens" 9 "output_tokens" 4)))
                     200)))))
       (propose (ax:make-jiti-proposer client :observation-limit 80)))
  (unwind-protect
       (let ((result (image-agent:run session propose)))
         (assert (eq :success (getf result :status)))
         (assert (= 7 (gethash :x (image-agent:reference-table world))))
         (assert (= 4 calls))
         ;; Jiti marks closed before its final ownership cleanup finishes.
         (sb-thread:join-thread (image-agent::session-thread session)
                                :timeout 5 :default :still-running)
         (assert (not (sb-thread:thread-alive-p (image-agent::session-thread session))))
         (let ((next (image-agent:make-session world :interactive t :budget 1)))
           (image-agent:close-session next))
         (format t "Jiti live worker: develop -> execute -> pause -> repair -> resume -> success; closed: PASS~%"))
    (image-agent:close-session session)))
