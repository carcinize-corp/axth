;;;; OPENAI_API_KEY=... sbcl --script packages/lisp/examples/generate.lisp
;;;; Optional: AX_PROVIDER=anthropic, AX_MODEL=..., AX_BASE_URL=...
(require :asdf)
(asdf:load-asd (merge-pathnames "../axllm.asd" *load-truename*))
(asdf:load-system "axllm")

(defparameter *provider* (or (uiop:getenv "AX_PROVIDER") "openai"))
(defparameter *client*
  (ax:ai :name *provider*
         :model (or (uiop:getenv "AX_MODEL")
                    (if (string-equal *provider* "anthropic")
                        "claude-fable-5-1" "gpt-6-luna"))
         :base-url (uiop:getenv "AX_BASE_URL")))

(defparameter *word-count*
  (ax:tool :name "count_words"
           :description "Count whitespace-separated words in the document."
           :parameters (ax:object "type" "object"
                                  "properties" (ax:object "document" (ax:object "type" "string"))
                                  "required" #("document"))
           :handler (lambda (arguments)
                      (length (remove "" (cl-ppcre:split "\\s+" (ax:jget arguments "document"))
                                      :test #'equal)))))

(defparameter *summarize*
  (ax:ax "documentText:string -> summary:string, wordCount:number, topics:string[]"
         :description "Summarize the document. Use count_words for the exact word count."
         :tools (list *word-count*) :max-steps 2 :max-retries 1))

(let ((output (ax:forward *summarize* *client*
                          (ax:object "documentText"
                                     "The release shipped on time. The cache reduced latency and support tickets."))))
  (write-line (ax:encode-json output)))
