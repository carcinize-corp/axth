(in-package #:axllm/tests)

(defun run-json-tests ()
  ;; Check wire spelling independently: a permissive parser can round-trip
  ;; illegal raw control characters without noticing its encoder's mistake.
  (loop for code below 32
        for char = (code-char code)
        for encoded = (ax:encode-json (string char))
        do (assert (not (find char encoded)))
           (assert (string= (string char) (ax:parse-json encoded))))
  ;; Lower-case hex in an escape, as JSON.stringify and Python's encoder
  ;; both write it; an upper-case escape would be valid JSON but would not
  ;; match the bytes the other ports put on the wire.
  (assert (string= "\"\\u0000\"" (ax:encode-json (string (code-char 0)))))
  (assert (string= "{\"\\u001b\":true}"
                   (ax:encode-json (ax:object (string (code-char 27)) ax:true))))
  ;; A lone surrogate has no UTF-8 encoding. It can reach the encoder from
  ;; a provider that split a surrogate pair across two stream events, and
  ;; it is written as the escape rather than failing the whole document.
  (assert (string= "\"\\ud83d\"" (ax:encode-json (string (code-char #xd83d)))))
  ;; And it has to read back, or an encode and parse round trip of streamed
  ;; text loses the half pair. JSON.parse accepts an unpaired surrogate
  ;; escape too; this is a UTF-16 unit, not malformed input.
  (dolist (code (list #xd800 #xd83d #xdc00 #xdfff))
    (let ((text (string (code-char code))))
      (assert (string= text (ax:parse-json (ax:encode-json text)))
              () "Lone surrogate ~x did not round trip" code)))
  (assert (string= (string (code-char #xd800)) (ax:parse-json "\"\\uD800\"")))
  (assert (string= (string (code-char #xdc00)) (ax:parse-json "\"\\uDC00\"")))
  ;; A high surrogate followed by something other than a low one stays a
  ;; lone unit, and the character after it survives.
  (assert (string= (coerce (list (code-char #xd800) #\x) 'string)
                   (ax:parse-json "\"\\uD800x\"")))
  (assert (string= (coerce (list (code-char #xd800) (code-char #xd800)) 'string)
                   (ax:parse-json "\"\\uD800\\uD800\"")))
  ;; A real pair is still one character.
  (assert (= 1 (length (ax:parse-json "\"\\uD83D\\uDE00\""))))
  ;; Every other escape still decodes, and an unknown one is still refused.
  (assert (string= (coerce (list #\" #\\ #\/ #\Backspace #\Page #\Newline #\Return #\Tab) 'string)
                   (ax:parse-json "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"")))
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
  ;; Written order is kept for ordinary keys, but an array-index key comes
  ;; first in numeric order, because that is JavaScript's own-property
  ;; order and so JSON.stringify's output order.
  (assert (string= "{\"2\":1,\"10\":2,\"b\":3}"
                   (ax:encode-json (ax:parse-json "{\"b\":3,\"10\":2,\"2\":1}"))))
  (assert (string= "{\"0\":1,\"01\":2,\"-1\":3}"
                   (ax:encode-json (ax:parse-json "{\"01\":2,\"-1\":3,\"0\":1}"))))
  ;; The writer's two shapes: indented as JSON.stringify's third argument
  ;; writes it, and key-sorted for a stable form.
  (assert (string= (format nil "{~%  \"b\": [~%    1~%  ],~%  \"a\": {}~%}")
                   (ax:encode-json (ax:object "b" (vector 1) "a" (ax:object)) :indent 2)))
  (assert (string= "{\"a\":{\"x\":1,\"y\":2},\"b\":3}"
                   (ax:encode-json (ax:object "b" 3 "a" (ax:object "y" 2 "x" 1))
                                   :sort-keys t)))
  ;; Numbers on the wire. The largest double is a number this parser used
  ;; to reject, because Lisp's reader overflows while scaling it.
  (assert (= most-positive-double-float (ax:parse-json "1.7976931348623157e308")))
  (assert (string= "1.7976931348623157e+308"
                   (ax:encode-json (ax:parse-json "1.7976931348623157e308"))))
  (assert (string= "1e-7" (ax:encode-json (ax:parse-json "0.0000001"))))
  (assert (string= "1e+21" (ax:encode-json (ax:parse-json "1e21"))))
  (assert (string= "5e-324" (ax:encode-json (ax:parse-json "5e-324"))))
  (assert (zerop (ax:parse-json "1e-400")))
  ;; An integer keeps its digits through the port, since JSON has no
  ;; precision limit and nothing here narrows a value silently.
  (assert (= 9007199254740993 (ax:parse-json "9007199254740993")))
  (assert (string= "9007199254740993" (ax:encode-json (ax:parse-json "9007199254740993"))))
  ;; Key order metadata goes when a key does, so a key written again after
  ;; being deleted takes the next position rather than its old one.
  (let ((object (ax:object "a" 1 "b" 2)))
    (axllm::%delete-key object "a")
    (assert (string= "{\"b\":2}" (ax:encode-json object)))
    (axllm::%set-key object "a" 3)
    (assert (string= "{\"b\":2,\"a\":3}" (ax:encode-json object))))
  (dolist (bad (append '("" "01" "1." "1e" "1e9999" "-" "+1" "[1,]"
                         "{\"x\":1,}" "{x:1}" "true false" "1 2" "[true false]"
                         "\"\\u+123\"" "\"\\uD80\"" "\"\\q\"" "\"\\\"" "\"\\u\"")
                      (list (format nil "\"raw~ccontrol\"" (code-char 0))
                            (format nil "null~c" #\Page))))
    (assert (handler-case (progn (ax:parse-json bad) nil)
              (ax:ax-error () t)) () "Accepted invalid JSON: ~s" bad))
  (assert (handler-case (progn (ax:signature-fields (ax:object)) nil)
            (ax:ax-error () t)))
  (format t "JSON boundaries: PASS~%"))
