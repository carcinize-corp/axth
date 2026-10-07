;;;; Deterministic public-API smoke test. No provider key or network needed.
;;;; sbcl --script packages/lisp/examples/no-key.lisp
(require :asdf)
(asdf:load-asd (merge-pathnames "../axllm.asd" *load-truename*))
(asdf:load-system "axllm")

;; Replace only HTTP. Requests still go through the real provider mapping,
;; generator, output parsing and flow scheduler.
(let* ((responses (list "Outline: Validate, then execute."
                        "Answer: Check the input before running the tool."))
       (requests 0)
       (client (ax:ai
                :name "openai" :model "gpt-5.4-mini" :api-key "not-a-secret"
                :transport
                (lambda (url headers body)
                  (declare (ignore headers))
                  (assert (search "/chat/completions" url))
                  (assert (equal "gpt-5.4-mini" (ax:jget (ax:parse-json body) "model")))
                  (assert responses)
                  (incf requests)
                  (values
                   (ax:encode-json
                    (ax:object "choices"
                               (vector (ax:object "index" 0 "finish_reason" "stop"
                                                  "message" (ax:object "role" "assistant"
                                                                       "content" (pop responses))))))
                   200))))
       (pipeline (ax:flow (ax:object "id" "no-key"))))
  (assert (plusp (length (ax:supported-ai-models))))
  (assert (not (eq :null (ax:model-info "openai" "gpt-5.4-mini"))))
  (ax:flow-execute pipeline "outline" (ax:ax "topic:string -> outline:string"))
  (ax:flow-execute pipeline "polish" (ax:ax "outline:string -> answer:string"))
  (ax:flow-returns pipeline (ax:object "answer" "answer"))
  (let ((output (ax:forward pipeline client (ax:object "topic" "Safe tool calls"))))
    (assert (equal "Check the input before running the tool." (ax:jget output "answer")))
    (assert (= requests 2))
    (assert (null responses))
    (write-line (ax:encode-json output))))
(write-line "No-key catalogue, provider, generation and flow: PASS")
