;;;; JITI_ROOT=/path/to/jiti OPENAI_MODEL=gpt-6-luna OPENAI_API_KEY=...
;;;; sbcl --script packages/lisp/examples/jiti.lisp
;;;; WARNING: Jiti executes model-proposed Lisp. Run only in an isolated image.
(require :asdf)
(asdf:load-asd (merge-pathnames "../axllm.asd" *load-truename*))
(asdf:load-system "axllm/jiti")
(let ((root (uiop:getenv "JITI_ROOT")))
  (unless root (error "Set JITI_ROOT to your Jiti checkout."))
  (asdf:load-asd (merge-pathnames "image-agent.asd" (uiop:ensure-directory-pathname root))))
(asdf:load-system "image-agent/store")

(let* ((client (ax:ai :name "openai" :model (uiop:getenv "OPENAI_MODEL")))
       (world (image-agent:make-reference-world :initial '((:answer . 0))))
       (session (image-agent:make-session
                 world :budget 4 :goal "Set :answer in *state* to 42."
                 :goals (list (cons :answered
                                    (lambda () (= 42 (gethash :answer (image-agent:reference-table world)))))))))
  (unwind-protect
       (let ((result (image-agent:run session (ax:make-jiti-proposer client))))
         (format t "Status: ~a; answer: ~a~%" (getf result :status)
                 (gethash :answer (image-agent:reference-table world))))
    (image-agent:close-session session)))
