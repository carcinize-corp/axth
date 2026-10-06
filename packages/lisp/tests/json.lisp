(in-package #:axllm/tests)

(defun run-json-tests ()
  ;; Check wire spelling independently: a permissive parser can round-trip
  ;; illegal raw control characters without noticing its encoder's mistake.
  (loop for code below 32
        for char = (code-char code)
        for encoded = (ax:encode-json (string char))
        do (assert (not (find char encoded)))
           (assert (string= (string char) (ax:parse-json encoded))))
  (assert (string= "\"\\u0000\"" (ax:encode-json (string (code-char 0)))))
  (assert (string= "{\"\\u001B\":true}"
                   (ax:encode-json (ax:object (string (code-char 27)) ax:true))))
  (assert (string= "São Paulo — λ 😀" (ax:parse-json "\"São Paulo — λ \\uD83D\\uDE00\"")))
  (let ((*read-base* 16))
    (assert (= 10 (ax:parse-json "10"))))
  (assert (string= "0.1" (ax:encode-json 0.1)))
  (assert (string= "0.1" (ax:encode-json 0.1d0)))
  (assert (eq ax:false (ax:parse-json "false")))
  (assert (eq :null (ax:parse-json "null")))
  (assert (hash-table-p (ax:parse-json "{}")))
  (assert (equalp #() (ax:parse-json "[]")))
  (assert (string= "{\"z\":1,\"a\":2}"
                   (ax:encode-json (ax:parse-json "{\"z\":1,\"a\":2}"))))
  (dolist (bad (append '("" "01" "1." "1e" "1e9999" "-" "+1" "[1,]"
                         "{\"x\":1,}" "{x:1}" "true false" "1 2" "[true false]"
                         "\"\\u+123\"" "\"\\uD800\"" "\"\\uDC00\"")
                      (list (format nil "\"raw~ccontrol\"" (code-char 0))
                            (format nil "null~c" #\Page))))
    (assert (handler-case (progn (ax:parse-json bad) nil)
              (ax:ax-error () t)) () "Accepted invalid JSON: ~s" bad))
  (assert (handler-case (progn (ax:signature-fields (ax:object)) nil)
            (ax:ax-error () t)))
  (format t "JSON boundaries: PASS~%"))
