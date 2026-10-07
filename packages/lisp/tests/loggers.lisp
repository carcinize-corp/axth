;;;; Exact native, no-key output tests. Markers below are test-only ANSI
;;;; notation, not calls into the implementation's formatting helpers.
(defpackage #:axllm/logger-tests
  (:use #:cl)
  (:export #:run-logger-tests))
(in-package #:axllm/logger-tests)

(defvar *checks* 0)

(defun check (expected actual)
  (incf *checks*)
  (assert (equal expected actual) () "Expected ~S, got ~S" expected actual))

(defun expected (template color)
  (let* ((text (format nil template))
         (d (make-string 60 :initial-element #\─))
         (light (make-string 50 :initial-element #\─))
         (heavy (make-string 50 :initial-element #\━)))
    (setf text (cl-ppcre:regex-replace-all "<D>" text
                                         (if color (format nil "<90>~A~%<0>" d) d)))
    (setf text (cl-ppcre:regex-replace-all "<L>" text (format nil "<90>~A<0>" light)))
    (setf text (cl-ppcre:regex-replace-all "<H>" text (format nil "<90>~A<0>" heavy)))
    (cl-ppcre:regex-replace-all "<([0-9;]+)>" text
                              (lambda (whole code)
                                (declare (ignore whole))
                                (if color (format nil "~C[~Am" #\Escape code) ""))
                              :simple-calls t)))

(defun capture (factory data)
  (let ((calls '()))
    (funcall (funcall factory (lambda (text) (assert (stringp text)) (push text calls))) data)
    (nreverse calls)))

(defun event-check (json text color &optional optimizer)
  (let ((data (ax:parse-json json)))
    (check (list (expected text nil))
           (capture (if optimizer #'axllm::create-default-optimizer-text-logger
                        #'axllm::create-default-text-logger) data))
    (check (list (expected color t))
           (capture (if optimizer #'axllm::create-default-optimizer-color-logger
                        #'axllm::create-default-color-logger) data))))

(defun run-logger-tests ()
  (let ((*checks* 0))
    (event-check "{\"name\":\"ChatRequestChatPrompt\",\"step\":2,\"value\":[{\"role\":\"system\",\"content\":\"rules\"},{\"role\":\"assistant\",\"name\":\"A\",\"content\":\"ok\"}]}"
                 "~%[ CHAT REQUEST Step 2 ]~%<D>~%[ SYSTEM ]~%rules~%<D>~%[ ASSISTANT A ]~%ok~%~%<D>"
                 "~%<94>[ CHAT REQUEST Step 2 ]<0>~%<D>~%<95>[ SYSTEM ]<0>~%<35>rules<0>~%<D>~%<96>[ ASSISTANT<0> A ]~%<36>ok<0>~%~%<D>")
    (event-check "{\"name\":\"FunctionResults\",\"value\":[{\"functionId\":\"a\",\"result\":\"yes\"},{\"functionId\":\"b\",\"result\":\"no\"}]}"
                 "~%[ FUNCTION RESULTS ]~%<D>~%Function: a~%Result: yes~%<D>~%Function: b~%Result: no"
                 "~%<93>[ FUNCTION RESULTS ]<0>~%<33>Function: a~%Result: yes<0>~%<D>~%<33>Function: b~%Result: no<0>")
    (event-check "{\"name\":\"ChatResponseResults\",\"value\":[{\"thought\":\"why\",\"content\":\"yes\",\"thoughtBlocks\":[{\"encrypted\":true}]},{}]}"
                 "~%[ CHAT RESPONSE ]~%[thought (redacted)] why~%yes~%<D>~%[No content]"
                 "~%<96>[ CHAT RESPONSE ]<0>~%<90>[THOUGHT (redacted)]~%why<0>~%<36>yes<0>~%<D>~%<90>[No content]<0>")
    (event-check "{\"name\":\"ChatResponseStreamingDoneResult\",\"value\":{\"content\":\"done\",\"thought\":\"why\",\"thoughtBlocks\":[{\"encrypted\":false}],\"functionCalls\":[]}}"
                 "~%[ CHAT RESPONSE ]~%done~%[thought] why[]"
                 "~%<96>[ CHAT RESPONSE ]<0>~%<D>~%<96>done<0>~%<90>[THOUGHT]~%why<0><96>[]<0>")
    (event-check "{\"name\":\"ChatResponseStreamingDoneResult\",\"value\":{}}"
                 "~%[ CHAT RESPONSE ]~%" "~%<96>[ CHAT RESPONSE ]<0>~%<D>~%")
    (dolist (case '(("{\"thought\":\"why\",\"delta\":\"d\",\"content\":\"c\"}" "<90>[THOUGHT]~%why<0>")
                    ("{\"delta\":\"d\",\"content\":\"c\"}" "<96>d<0>")
                    ("{\"delta\":\"\",\"content\":\"c\"}" "<96>c<0>")
                    ("{}" "<96><0>")))
      (let ((data (ax:object "name" "ChatResponseStreamingResult" "value" (ax:parse-json (first case)))))
        (check nil (capture #'axllm::create-default-text-logger data))
        (check (list (expected (second case) t)) (capture #'axllm::create-default-color-logger data))))
    (dolist (kind '("Function" "Validation"))
      (event-check (format nil "{\"name\":\"~AError\",\"index\":0,\"fixingInstructions\":\"fix\",\"error\":\"bad\"}" kind)
                   (format nil "~~%[ ~A ERROR #0 ]~~%<D>~~%fix~~%Error: bad" (string-upcase kind))
                   (format nil "~~%<91>[ ~A ERROR #0 ]<0>~~%<D>~~%<37>fix<0>~~%<91>Error: bad<0>" (string-upcase kind))))
    (event-check "{\"name\":\"ResultPickerUsed\",\"selectedIndex\":0,\"sampleCount\":2,\"latency\":1.125}"
                 "[ RESULT PICKER ]~%<D>~%Selected sample 1 of 2 (1.13ms)"
                 "<92>[ RESULT PICKER ]<0>~%<D>~%<32>Selected sample 1 of 2 (1.13ms)<0>")
    (event-check "{\"name\":\"Notification\",\"id\":\"n\",\"value\":\"notice\"}"
                 "[ NOTIFICATION n ]~%<D>~%notice"
                 "<90>[ NOTIFICATION n ]<0>~%<D>~%<37>notice<0>")
    (event-check "{\"name\":\"EmbedRequest\",\"embedModel\":\"test\",\"value\":[\"one\",\"two\"]}"
                 "[ EMBED REQUEST test ]~%<D>~%Text 1: one~%<D>~%Text 2: two"
                 "<38;5;208>[ EMBED REQUEST test ]<0>~%<D>~%<37>Text 1: one<0>~%<D>~%<37>Text 2: two<0>")
    (event-check "{\"name\":\"EmbedResponse\",\"totalEmbeddings\":2,\"value\":[{\"sample\":[0,0.25,-1],\"truncated\":true,\"length\":5},{\"sample\":[],\"truncated\":false,\"length\":0}]}"
                 "[ EMBED RESPONSE (2 embeddings) ]~%<D>~%Embedding 1: [0, 0.25, -1, ...] (length: 5)~%<D>~%Embedding 2: [] (length: 0)"
                 "<38;5;208>[ EMBED RESPONSE (2 embeddings) ]<0>~%<D>~%<37>Embedding 1: [0, 0.25, -1, ...] (length: 5)<0>~%<D>~%<37>Embedding 2: [] (length: 0)<0>")
    (event-check "{\"name\":\"ChatResponseUsage\",\"value\":{\"ai\":\"test\",\"model\":\"test\",\"systemPromptCharacters\":0,\"tokens\":{\"totalTokens\":3,\"promptTokens\":2,\"completionTokens\":1,\"thoughtsTokens\":0,\"serviceTier\":\"standard\"},\"estimatedCost\":0.125}}"
                 "~%[ CHAT RESPONSE USAGE ]~%AI: test~%Model: test~%System Prompt Characters: 0~%Total Tokens: 3~%Prompt Tokens: 2~%Completion Tokens: 1~%Thoughts Tokens: 0~%Service Tier: standard~%Estimated Cost: $0.125000~%<D>~%"
                 "<92>~%[ CHAT RESPONSE USAGE ]<0>~%<37>AI:<0> test~%<37>Model:<0> test~%<37>System Prompt Characters:<0> 0~%<37>Total Tokens:<0> 3~%<37>Prompt Tokens:<0> 2~%<37>Completion Tokens:<0> 1~%<37>Thoughts Tokens:<0> 0~%<37>Service Tier:<0> standard~%<37>Estimated Cost:<0> $0.125000~%<D>")
    (event-check "{\"name\":\"ChatResponseUsage\",\"value\":{\"ai\":\"test\",\"model\":\"test\"}}"
                 "~%[ CHAT RESPONSE USAGE ]~%AI: test~%Model: test~%<D>~%"
                 "<92>~%[ CHAT RESPONSE USAGE ]<0>~%<37>AI:<0> test~%<37>Model:<0> test~%<D>")
    (event-check "{\"name\":\"ChatResponseCitations\",\"value\":[{\"title\":\"Title\",\"url\":\"u\",\"description\":\"desc\"},{\"title\":\"\",\"url\":\"v\"}]}"
                 "~%[ CHAT RESPONSE CITATIONS ]~%- Title~%  desc~%- v~%<D>~%"
                 "<94>~%[ CHAT RESPONSE CITATIONS ]<0>~%<37>- <0><36>Title<0>~%  <90>desc<0>~%<37>- <0><36>v<0>~%<D>")
    (event-check "{\"name\":\"other\",\"value\":null}"
                 "{~%  \"name\": \"other\",~%  \"value\": null~%}"
                 "<90>{~%  \"name\": \"other\",~%  \"value\": null~%}<0>")
    ;; Every optimizer category has separate text and color expectations.
    (event-check "{\"name\":\"OptimizationStart\",\"value\":{\"optimizerType\":\"test\",\"exampleCount\":2,\"validationCount\":1,\"config\":{\"z\":true,\"a\":null}}}"
                 "[ OPTIMIZATION START: test ]~%<D>~%Config: {~%  \"z\": true,~%  \"a\": null~%}~%Examples: 2, Validation: 1~%<D>"
                 "~%<94>● <0><97>Optimization Started<0>~%<L>~%  <37>Optimizer:<0> <36>test<0>~%  <37>Examples:<0> <32>2<0> training, <32>1<0> validation~%  <37>Config:<0> <37>{\"z\":true,\"a\":null}<0>~%<H>~%" t)
    (event-check "{\"name\":\"RoundProgress\",\"value\":{\"round\":1,\"currentScore\":0.75,\"bestScore\":0.5,\"configuration\":{\"trialNumber\":0,\"totalRounds\":4,\"temperature\":0.25,\"bootstrappedDemos\":0,\"other\":2,\"ignored\":\"x\"}}}"
                 "[ ROUND 1/undefined ]~%Current Score: 0.750, Best: 0.500~%Config: {\"trialNumber\":0,\"totalRounds\":4,\"temperature\":0.25,\"bootstrappedDemos\":0,\"other\":2,\"ignored\":\"x\"}~%<D>"
                 "<93>● <0><97>Round 1/4<0><90> [Trial #0]<0>~%  <37>Score:<0> <32>0.750<0> <37>(best:<0> <92>0.500<0><37>)<0><92> ↑0.250<0>~%  <37>Config:<0> <36>T=0.25, demos=0, totalRounds=4.00, other=2.00<0>~%" t)
    (event-check "{\"name\":\"RoundProgress\",\"value\":{\"round\":2,\"currentScore\":0.25,\"bestScore\":0.5}}"
                 "[ ROUND 2/undefined ]~%Current Score: 0.250, Best: 0.500~%Config: undefined~%<D>"
                 "<93>● <0><97>Round 2/0<0>~%  <37>Score:<0> <32>0.250<0> <37>(best:<0> <92>0.500<0><37>)<0><91> ↓0.250<0>~%" t)
    (event-check "{\"name\":\"EarlyStopping\",\"value\":{\"round\":2,\"reason\":\"stop\",\"finalScore\":0.5}}"
                 "[ EARLY STOPPING at Round 2 ]~%Reason: stop~%Final Score: 0.500~%<D>"
                 "~%<91>● <0><97>Early Stopping<0>~%<L>~%  <37>Round:<0> <93>2<0>~%  <37>Reason:<0> <93>stop<0>~%  <37>Final Score:<0> <32>0.500<0>~%<H>~%" t)
    (event-check "{\"name\":\"OptimizationComplete\",\"value\":{\"bestScore\":0.5,\"bestConfiguration\":{},\"explanation\":\"summary\",\"performanceAssessment\":\"fast\",\"recommendations\":[\"try\"]}}"
                 "[ OPTIMIZATION COMPLETE ]~%<D>~%Best Score: 0.500~%Best Config: {}~%Stats: undefined~%<D>"
                 "~%<32>● <0><97>Optimization Complete<0>~%<L>~%  <37>Best Score:<0> <92>0.500<0>~%  <37>Best Config:<0> <36>{}<0>~%  <37>Total Calls:<0> <37>N/A<0>~%  <37>Success Rate:<0> <32>0.0%<0>~%~%<94>📊 Summary:<0>~%  <37>summary<0>~%~%<93>⚡ Performance:<0>~%  <37>fast<0>~%~%<92>💡 Recommendations:<0>~%  <37>1.<0> <37>try<0>~%<H>~%" t)
    (event-check "{\"name\":\"ConfigurationProposal\",\"value\":{\"type\":\"test\",\"count\":4,\"proposals\":[\"a\",{},\"c\",\"d\"]}}"
                 "[ CONFIG PROPOSAL: test ]~%Count: 4~%Proposals: [~%  \"a\",~%  {},~%  \"c\"~%] ... (truncated)~%<D>"
                 "<35>● <0><97>test Proposals<0> <37>(4)<0>~%  <37>Candidates:<0> <37>\"a...\", {}...<0>~%" t)
    (event-check "{\"name\":\"BootstrappedDemos\",\"value\":{\"count\":3,\"demos\":[1,2,3]}}"
                 "[ BOOTSTRAPPED DEMOS ]~%Count: 3~%Demos: [~%  1,~%  2~%] ... (truncated)~%<D>"
                 "<36>● <0><97>Bootstrapped Demos<0> <37>(3)<0>~%  <37>Generated:<0> <32>3<0> demonstration examples~%" t)
    (event-check "{\"name\":\"BestConfigFound\",\"value\":{\"score\":0.5,\"config\":{}}}"
                 "[ BEST CONFIG FOUND ]~%Score: 0.500~%Config: {}~%<D>"
                 "<32>● <0><97>Best Configuration Found<0>~%  <37>Score:<0> <92>0.500<0>~%  <37>Config:<0> <36>{}<0>~%" t)
    (event-check "{}" "[ UNKNOWN OPTIMIZER EVENT ]~%{}~%<D>"
                 "<91>● <0><97>Unknown Event<0>~%  <37>{}<0>~%" t)
    (run-message-tests)
    (run-logger-boundary-tests)
    (format t "Loggers: ~D exact assertions PASS~%" *checks*)
    t))

(defun run-message-tests ()
  (dolist (color '(nil t))
    (loop for (json want) in
          '(("{\"role\":\"system\",\"content\":\"s\"}" "<95>[ SYSTEM ]<0>~%<35>s<0>")
            ("{\"role\":\"user\",\"content\":\"u\"}" "<92>[ USER ]<0>~%<32>u<0>")
            ("{\"role\":\"function\"}" "<93>[ FUNCTION RESULT ]<0>~%<33>[No result]<0>")
            ("{\"role\":\"function\",\"result\":null}" "<93>[ FUNCTION RESULT ]<0>~%<33>[No result]<0>")
            ("{\"role\":\"function\",\"result\":\"\"}" "<93>[ FUNCTION RESULT ]<0>~%<33><0>")
            ("{\"role\":\"assistant\"}" "<96>[ ASSISTANT<0> ]~%<90>[No content]<0>")
            ("{\"role\":\"assistant\",\"functionCalls\":[{\"id\":\"1\",\"function\":{\"name\":\"a\",\"params\":{\"b\":false}}},{\"id\":\"2\",\"function\":{\"name\":\"b\",\"params\":\"raw\"}}]}"
             "<96>[ ASSISTANT<0> ]~%<93>[ FUNCTION CALLS ]<0>~%<33>1. a({~%  \"b\": false~%}) [id: 1]<0>~%<33>2. b(raw) [id: 2]<0>~%")
            ("{\"role\":\"new\"}" "<91>[ UNKNOWN ]<0>~%<90>{\"role\":\"new\"}<0>"))
          do (check (expected want color) (axllm::format-chat-message (ax:parse-json json) :color color)))
    (let ((message (ax:parse-json "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"literal text\"},{\"type\":\"image\",\"image\":\"pixels\"},{\"type\":\"audio\",\"data\":\"samples\"},{\"type\":\"other\"}]}")))
      (check (expected "<92>[ USER ]<0>~%<32>literal text<0>~%<32>[Image: pixels]<0>~%<32>[Audio: samples]<0>~%<90>[Unknown content type]<0>" color)
             (axllm::format-chat-message message :color color))
      (check (expected "<92>[ USER ]<0>~%<32>literal text<0>~%<32>[Image]<0>~%<32>[Audio]<0>~%<90>[Unknown content type]<0>" color)
             (axllm::format-chat-message message :color color :hide-content t)))))

(defun run-logger-boundary-tests ()
  ;; Failure is not swallowed; text streaming skips even a failing callback.
  (let* ((condition (make-condition 'simple-error :format-control "callback failed"))
         (callback (lambda (text) (declare (ignore text)) (error condition))))
    (dolist (factory (list #'axllm::create-default-color-logger #'axllm::create-default-text-logger
                           #'axllm::create-default-optimizer-color-logger #'axllm::create-default-optimizer-text-logger))
      (check condition (handler-case (funcall (funcall factory callback) (ax:object)) (error (e) e))))
    (check nil (funcall (axllm::create-default-text-logger callback)
                        (ax:object "name" "ChatResponseStreamingResult"))))
  (dolist (factory (list #'axllm::create-default-color-logger #'axllm::create-default-text-logger
                         #'axllm::create-default-optimizer-color-logger #'axllm::create-default-optimizer-text-logger))
    (let ((data (ax:object)))
      (check (format nil "~A~%" (first (capture factory data)))
             (with-output-to-string (*standard-output*) (funcall (funcall factory) data)))))
  (check (first (capture #'axllm::create-default-color-logger (ax:object)))
         (string-right-trim '(#\Newline)
                            (with-output-to-string (*standard-output*) (funcall axllm::*default-logger* (ax:object)))))
  (check (format nil "~A~%" (first (capture #'axllm::create-default-optimizer-color-logger (ax:object))))
         (with-output-to-string (*standard-output*) (funcall axllm::*default-optimizer-logger* (ax:object))))
  (check "😀" (axllm::%logger-prefix "😀a" 2))
  (check (string (code-char #xd83d)) (axllm::%logger-prefix "😀a" 1))
  (check (format nil "~C~%" (code-char #xfffd))
         (with-output-to-string (*standard-output*) (axllm::%logger-output (axllm::%logger-prefix "😀a" 1))))
  (dolist (case '((1.125d0 2 "1.13") (-1.125d0 2 "-1.13") (1.005d0 2 "1.00") (0 6 "0.000000")))
    (check (third case) (axllm::%logger-fixed (first case) (second case))))
  (let* ((text (make-string 101 :initial-element #\x))
         (data (ax:object "name" "EmbedRequest" "embedModel" "test" "value" (vector text))))
    (check (list (format nil "[ EMBED REQUEST test ]~%~A~%Text 1: ~A..."
                        (make-string 60 :initial-element #\─) (subseq text 0 100)))
           (capture #'axllm::create-default-text-logger data)))
  ;; No fabricated cost/tokens/round counts in the text logger.
  (check (list (expected "~%[ CHAT RESPONSE USAGE ]~%AI: undefined~%Model: undefined~%<D>~%" nil))
         (capture #'axllm::create-default-text-logger (ax:object "name" "ChatResponseUsage" "value" (ax:object))))
  (dolist (factory (list #'axllm::create-default-color-logger #'axllm::create-default-text-logger))
    (check t (handler-case
                 (progn (capture factory (ax:object "name" "ResultPickerUsed" "selectedIndex" 0 "sampleCount" 1)) nil)
               (error () t))))
  (dolist (factory (list #'axllm::create-default-optimizer-color-logger #'axllm::create-default-optimizer-text-logger))
    (check t (handler-case
                 (progn (capture factory (ax:object "name" "BestConfigFound" "value" (ax:object))) nil)
               (error () t))))
  (dolist (factory (list #'axllm::create-default-color-logger #'axllm::create-default-text-logger))
    (dolist (data (list (ax:object "name" "ChatResponseUsage")
                        (ax:object "name" "ChatResponseStreamingDoneResult" "value" :null)))
      (let ((calls 0))
        (check t (handler-case
                     (progn (funcall (funcall factory (lambda (text) (declare (ignore text)) (incf calls))) data) nil)
                   (error () t)))
        (check 0 calls))))
  (let ((data (ax:parse-json "{\"name\":\"ChatRequestChatPrompt\",\"step\":1,\"value\":[{\"role\":\"user\",\"content\":[{\"type\":\"image\",\"image\":\"pixels\"},{\"type\":\"audio\",\"data\":\"samples\"}]}]}")))
    (loop for color in '(nil t)
          for factory in (list #'axllm::create-default-text-logger #'axllm::create-default-color-logger)
          do (let ((result nil))
               (funcall (funcall factory (lambda (s) (setf result s)) t) data)
               (check (expected "~%<94>[ CHAT REQUEST Step 1 ]<0>~%<D>~%<92>[ USER ]<0>~%<32>[Image]<0>~%<32>[Audio]<0>~%<D>" color)
                      result))))
  ;; The trailing space after [] is part of the text format, even with no truncation.
  (event-check "{\"name\":\"ConfigurationProposal\",\"value\":{\"type\":\"test\",\"count\":0,\"proposals\":[]}}"
               "[ CONFIG PROPOSAL: test ]~%Count: 0~%Proposals: [] ~%<D>"
               "<35>● <0><97>test Proposals<0> <37>(0)<0>~%  <37>Candidates:<0> <37><0>~%" t)
  (event-check "{\"name\":\"BootstrappedDemos\",\"value\":{\"count\":0,\"demos\":[]}}"
               "[ BOOTSTRAPPED DEMOS ]~%Count: 0~%Demos: [] ~%<D>"
               "<36>● <0><97>Bootstrapped Demos<0> <37>(0)<0>~%  <37>Generated:<0> <32>0<0> demonstration examples~%" t)
  (event-check "{\"name\":\"OptimizationComplete\",\"value\":{\"bestScore\":1,\"bestConfiguration\":{},\"stats\":{\"totalCalls\":0,\"successfulDemos\":0}}}"
               "[ OPTIMIZATION COMPLETE ]~%<D>~%Best Score: 1.000~%Best Config: {}~%Stats: {~%  \"totalCalls\": 0,~%  \"successfulDemos\": 0~%}~%<D>"
               "~%<32>● <0><97>Optimization Complete<0>~%<L>~%  <37>Best Score:<0> <92>1.000<0>~%  <37>Best Config:<0> <36>{}<0>~%  <37>Total Calls:<0> <37>0<0>~%  <37>Success Rate:<0> <32>0.0%<0>~%<H>~%" t)
  ;; Source includes numeric config entries in insertion order, and omits a
  ;; zero score improvement instead of printing an arrow or a fallback.
  (event-check "{\"name\":\"RoundProgress\",\"value\":{\"round\":1,\"totalRounds\":2,\"currentScore\":0,\"bestScore\":0,\"configuration\":{\"z\":2,\"a\":1}}}"
               "[ ROUND 1/2 ]~%Current Score: 0.000, Best: 0.000~%Config: {\"z\":2,\"a\":1}~%<D>"
               "<93>● <0><97>Round 1/2<0>~%  <37>Score:<0> <32>0.000<0> <37>(best:<0> <92>0.000<0><37>)<0>~%  <37>Config:<0> <36>z=2.00, a=1.00<0>~%" t))
