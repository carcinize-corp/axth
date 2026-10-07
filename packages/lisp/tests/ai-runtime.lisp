;;;; ai-runtime.lisp --- native runtime tests for the Ax Lisp provider layer.
;;;;
;;;; Entry points:
;;;;
;;;;   (axllm/ai-tests:run-ai-runtime-tests)        ; => (values passed failed)
;;;;   (axllm/ai-tests:run-ai-runtime-tests-or-die) ; exits non-zero on failure
;;;;
;;;; These are the host-boundary tests: tool-call shape validation, the
;;;; streaming transport and its SSE decoder, cancellation, retries, run
;;;; control, exception classification, embeddings and audio mapping, the
;;;; profile registry, the router and balancer service objects, and the AxGen
;;;; host hooks.  Coverage is by scripted transport plus real Drakma requests
;;;; against a loopback HTTP server.  No network egress and no provider
;;;; credentials are required.
;;;;
;;;; The suite lives in its own package so it cannot collide with the
;;;; generation suite's harness in tests/provider.lisp, which is owned
;;;; elsewhere.  It reaches internal symbols through AXLLM:: on purpose: these
;;;; are boundary tests, not public-API examples.

(defpackage #:axllm/ai-tests
  (:use #:cl)
  (:export #:run-ai-runtime-tests #:run-ai-runtime-tests-or-die))

(in-package #:axllm/ai-tests)

;;; ------------------------------------------------------------------
;;; Harness
;;; ------------------------------------------------------------------

(define-condition check-failed (error)
  ((text :initarg :text :reader check-failed-text))
  (:report (lambda (c s) (write-string (check-failed-text c) s))))

(defvar *cases* '()
  "Registered cases, in definition order.")

(defmacro defcase (name &body body)
  `(progn
     (defun ,name () ,@body)
     (setf *cases* (append (remove ',name *cases* :key #'car) (list (cons ',name #',name))))
     ',name))

(defun ok (condition description)
  (unless condition (error 'check-failed :text description))
  t)

(defun is (actual expected description)
  (ok (equal actual expected)
      (format nil "~a (expected ~s, got ~s)" description expected actual)))

(defun has-substring (haystack needle description)
  (ok (and (stringp haystack) (search needle haystack))
      (format nil "~a (~s not found in ~s)" description needle haystack)))

(defun lacks-substring (haystack needle description)
  (ok (not (and (stringp haystack) (search needle haystack)))
      (format nil "~a (~s unexpectedly present)" description needle)))

(defmacro signals-provider-error (kind &body body)
  "Run BODY, require an AXLLM:PROVIDER-ERROR whose kind is KIND, and return
the condition."
  (let ((c (gensym "CONDITION")))
    `(handler-case (progn ,@body
                          (error 'check-failed
                                 :text (format nil "expected a provider-error of kind ~a, ~
nothing signalled" ,kind)))
       (axllm:provider-error (,c)
         (ok (eq (axllm:provider-error-kind ,c) ,kind)
             (format nil "expected provider-error kind ~a, got ~a: ~a"
                     ,kind (axllm:provider-error-kind ,c) ,c))
         ,c))))

(defun run-ai-runtime-tests ()
  (let ((passed 0) (failed 0))
    (dolist (case *cases*)
      (handler-case (progn (funcall (cdr case))
                           (incf passed)
                           (format t "ok   ~a~%" (car case)))
        (error (condition)
          (incf failed)
          (format t "FAIL ~a: ~a~%" (car case) condition))))
    (format t "~a passed, ~a failed~%" passed failed)
    (values passed failed)))

(defun run-ai-runtime-tests-or-die ()
  (multiple-value-bind (passed failed) (run-ai-runtime-tests)
    (declare (ignore passed))
    (unless (zerop failed) (uiop:quit 1))
    t))

;;; ------------------------------------------------------------------
;;; Shared fixtures
;;; ------------------------------------------------------------------

(defparameter +openai-model+ "gpt-5.4-mini"
  "A model Core routes to Chat Completions.

Core selects the Responses dialect for the gpt-6 family, so a case that
scripts a `choices' body has to name a Chat Completions model or it is
scripting the wrong API and proves nothing.  +responses-dialect-model+ is the
one to name when the Responses dialect is the point.")

(defparameter +responses-dialect-model+ "gpt-6-luna"
  "A model Core routes to the Responses dialect.")
(defparameter +anthropic-model+ "claude-fable-5-1")

(defstruct (tape (:conc-name tape-)) queue (calls '()))

(defun scripted-transport (responses)
  "RESPONSES is a list of body strings or (status . body) conses.  Returns
\(values transport tape)."
  (let ((tape (make-tape :queue (copy-list responses))))
    (values (lambda (url headers json-body)
              (push (list url headers json-body) (tape-calls tape))
              (let ((next (if (tape-queue tape)
                              (pop (tape-queue tape))
                              (error "scripted transport exhausted"))))
                (if (consp next)
                    (values (cdr next) (car next))
                    (values next 200))))
            tape)))

(defun tape-call-count (tape) (length (tape-calls tape)))

(defun tape-request (tape n)
  (let ((calls (reverse (tape-calls tape))))
    (ok (< n (length calls)) (format nil "expected at least ~a transport call(s)" (1+ n)))
    (axllm:parse-json (third (nth n calls)))))

(defun scripted-client (provider responses &key model (api-key "sk-test-not-a-real-key"))
  (multiple-value-bind (transport tape) (scripted-transport responses)
    (values (axllm:ai :name provider
                      :model (or model (if (string= provider "anthropic")
                                           +anthropic-model+
                                           +openai-model+))
                      :api-key api-key
                      :transport transport)
            tape)))

(defun openai-tool-response (calls &key (content :null) (finish "tool_calls"))
  (axllm:encode-json
   (axllm:object "choices" (vector (axllm:object "index" 0
                                                 "finish_reason" finish
                                                 "message" (axllm:object "role" "assistant"
                                                                         "content" content
                                                                         "tool_calls"
                                                                         (coerce calls 'vector))))
                 "usage" (axllm:object "prompt_tokens" 5 "completion_tokens" 3
                                       "total_tokens" 8))))

(defun openai-wire-call (id name arguments &key (type "function"))
  (let ((call (axllm:object "id" id
                            "function" (axllm:object "name" name "arguments" arguments))))
    (unless (eq type :omit)
      (setf (gethash "type" call) type))
    call))

;;; ------------------------------------------------------------------
;;; Tool-call shape validation
;;;
;;; Reference: src/ax/ai/validate.ts (axValidateChatRequestMessage and
;;; validateChatResponseFunctionCalls) and ir/axcore/ai.axir's
;;; @chat_result_function_call_problems, which states the rule for both the
;;; nested reference shape and this port's flat completion shape.
;;; ------------------------------------------------------------------

(defcase request-rejects-a-non-function-tool-call-type
  ;; The finding: the type used to be discarded, so a call declaring
  ;; type "not_function" was rewritten to "function" and run.
  (multiple-value-bind (client tape)
      (scripted-client "openai" (list (axllm:encode-json
                                       (axllm:object "choices" (vector)))))
    (let ((condition
            (signals-provider-error :config
              (axllm:chat client
                          (vector (axllm:message "user" "hi")
                                  (axllm:message
                                   "assistant" ""
                                   :tool-calls
                                   (vector (axllm:object
                                            "id" "call_1"
                                            "type" "not_function"
                                            "function" (axllm:object "name" "run"
                                                                     "params" "{}")))))))))
      (has-substring (princ-to-string condition) "must have type 'function'"
                     "the condition names the type check")
      (has-substring (princ-to-string condition) "\"not_function\""
                     "the condition reports the rejected value")
      (is (tape-call-count tape) 0
          "the request is rejected before any transport call"))))

(defcase request-rejects-a-non-function-type-on-a-flat-call
  (multiple-value-bind (client tape)
      (scripted-client "anthropic" (list (axllm:encode-json (axllm:object))))
    (signals-provider-error :config
      (axllm:chat client
                  (vector (axllm:message "user" "hi")
                          (axllm:message "assistant" ""
                                         :tool-calls
                                         (vector (axllm:object "id" "call_1"
                                                               "type" "custom"
                                                               "name" "run"
                                                               "arguments" "{}"))))))
    (is (tape-call-count tape) 0 "anthropic mapping rejects it before the transport too")))

(defcase request-accepts-the-flat-shape-without-a-type
  ;; Core's rule: a flat {id, name, arguments} call has no type and no
  ;; function object, so only its id, name and arguments are checked.
  (multiple-value-bind (client tape)
      (scripted-client "openai" (list (axllm:encode-json
                                       (axllm:object
                                        "choices"
                                        (vector (axllm:object
                                                 "index" 0 "finish_reason" "stop"
                                                 "message" (axllm:object "role" "assistant"
                                                                         "content" "done")))))))
    (let ((result (axllm:chat client
                              (vector (axllm:message "user" "hi")
                                      (axllm:message "assistant" ""
                                                     :tool-calls
                                                     (vector (axllm:object "id" "call_1"
                                                                           "name" "run"
                                                                           "arguments" "{}")))
                                      (axllm:message "tool" "ok" :tool-call-id "call_1")))))
      (is (axllm:jget result "content") "done" "the flat call is accepted")
      (let* ((body (tape-request tape 0))
             (messages (axllm:jget body "messages"))
             (assistant (aref messages 1))
             (wire-call (aref (axllm:jget assistant "tool_calls") 0)))
        (is (axllm:jget wire-call "type") "function"
            "the wire call is sent as a function call")))))

(defcase request-accepts-the-nested-reference-shape
  ;; A Core-normalized response carries {id, type, function:{name, params}}.
  ;; The request mapper has to accept exactly that, or the tool loop breaks
  ;; the moment Core normalization replaces this port's own normalizer.
  (multiple-value-bind (client tape)
      (scripted-client "openai" (list (axllm:encode-json
                                       (axllm:object
                                        "choices"
                                        (vector (axllm:object
                                                 "index" 0 "finish_reason" "stop"
                                                 "message" (axllm:object "role" "assistant"
                                                                         "content" "done")))))))
    (axllm:chat client
                (vector (axllm:message "user" "hi")
                        (axllm:message "assistant" ""
                                       :tool-calls
                                       (vector (axllm:object
                                                "id" "call_1"
                                                "type" "function"
                                                "function" (axllm:object
                                                            "name" "lookup"
                                                            "params" (axllm:object "city" "Oslo")))))
                        (axllm:message "tool" "ok" :tool-call-id "call_1")))
    (let* ((body (tape-request tape 0))
           (assistant (aref (axllm:jget body "messages") 1))
           (wire-call (aref (axllm:jget assistant "tool_calls") 0)))
      (is (axllm:jget (axllm:jget wire-call "function") "name") "lookup"
          "the nested name is read from the function object")
      (is (axllm:jget (axllm:jget wire-call "function") "arguments")
          "{\"city\":\"Oslo\"}"
          "nested object params are encoded as the wire arguments string"))))

(defcase response-rejects-a-non-function-tool-call-type
  ;; A provider can answer with a call this port cannot run, such as an
  ;; OpenAI `custom' tool call.  It must not reach the tool loop.
  (multiple-value-bind (client tape)
      (scripted-client "openai"
                       (list (openai-tool-response
                              (list (openai-wire-call "call_1" "run" "{}" :type "custom")))))
    (declare (ignore tape))
    (let ((condition (signals-provider-error :response
                       (axllm:chat client (vector (axllm:message "user" "hi"))))))
      (has-substring (princ-to-string condition) "must have type 'function'"
                     "the response condition names the type check")
      (has-substring (princ-to-string condition) "\"custom\""
                     "the response condition reports the rejected value"))))

(defcase response-rejects-a-tool-call-without-an-id
  (multiple-value-bind (client tape)
      (scripted-client "openai"
                       (list (openai-tool-response
                              (list (axllm:object
                                     "type" "function"
                                     "function" (axllm:object "name" "run" "arguments" "{}"))))))
    (declare (ignore tape))
    (let ((condition (signals-provider-error :response
                       (axllm:chat client (vector (axllm:message "user" "hi"))))))
      (has-substring (princ-to-string condition) "must have a non-empty string id"
                     "an id-less call never escapes normalization"))))

(defcase response-rejects-a-tool-call-with-a-blank-name
  (multiple-value-bind (client tape)
      (scripted-client "openai"
                       (list (openai-tool-response
                              (list (openai-wire-call "call_1" "   " "{}")))))
    (declare (ignore tape))
    (let ((condition (signals-provider-error :response
                       (axllm:chat client (vector (axllm:message "user" "hi"))))))
      (has-substring (princ-to-string condition) "must have a non-empty function name"
                     "a whitespace-only name never escapes normalization"))))

(defcase response-accepts-a-tool-call-without-a-wire-type
  ;; The reference reads id and function only, so an entry that omits type
  ;; is a function call.  Failing it closed would reject real providers.
  (multiple-value-bind (client tape)
      (scripted-client "openai"
                       (list (openai-tool-response
                              (list (openai-wire-call "call_1" "run" "{\"a\":1}" :type :omit)))))
    (declare (ignore tape))
    (let* ((result (axllm:chat client (vector (axllm:message "user" "hi"))))
           (call (aref (axllm:jget result "toolCalls") 0)))
      (is (axllm:jget call "id") "call_1" "the id survives")
      (is (axllm:jget call "name") "run" "the name survives")
      (is (axllm:jget call "arguments") "{\"a\":1}" "the arguments survive")
      (is (nth-value 1 (gethash "type" call)) nil
          "the normalized call keeps this port's flat shape, with no type key"))))

(defcase anthropic-tool-use-without-an-id-falls-back-to-the-tool-name
  ;; Measured against Core rather than assumed: Core's Anthropic normalizer
  ;; uses the tool name as the id when the block carries none, so the call is
  ;; still addressable and there is nothing unsafe to reject. The guard is for
  ;; a call that cannot be addressed at all, which the next case covers.
  (multiple-value-bind (client tape)
      (scripted-client "anthropic"
                       (list (axllm:encode-json
                              (axllm:object
                               "id" "msg_1" "model" +anthropic-model+
                               "stop_reason" "tool_use"
                               "content" (vector (axllm:object "type" "tool_use"
                                                               "name" "run"
                                                               "input" (axllm:object)))
                               "usage" (axllm:object "input_tokens" 1 "output_tokens" 1)))))
    (declare (ignore tape))
    (let* ((result (axllm:chat client (vector (axllm:message "user" "hi"))))
           (call (aref (axllm:jget result "toolCalls") 0)))
      (is (axllm:jget call "id") "run"
          "the id falls back to the tool name, so the call stays addressable")
      (is (axllm:jget call "name") "run" "and the name is unchanged"))))

(defcase anthropic-tool-use-with-no-usable-name-is-rejected
  ;; With neither an id nor a usable name there is nothing to address the call
  ;; by, so it must not escape into the tool loop.
  (multiple-value-bind (client tape)
      (scripted-client "anthropic"
                       (list (axllm:encode-json
                              (axllm:object
                               "id" "msg_1" "model" +anthropic-model+
                               "stop_reason" "tool_use"
                               "content" (vector (axllm:object "type" "tool_use"
                                                               "name" "   "
                                                               "input" (axllm:object)))
                               "usage" (axllm:object "input_tokens" 1 "output_tokens" 1)))))
    (declare (ignore tape))
    (let ((condition (signals-provider-error :response
                       (axllm:chat client (vector (axllm:message "user" "hi"))))))
      (has-substring (princ-to-string condition) "Function call at index 0"
                     "the refusal names the offending call"))))

(defcase tool-call-problems-classifies-like-core
  ;; Core's @chat_result_function_call_problems splits failures into the ones
  ;; a port's default corrects (a call without a usable name) and the ones it
  ;; runs as given (id, type and params).  The batch helper reports both so a
  ;; caller preflighting a whole response can tell them apart.
  (let ((problems (axllm::tool-call-problems
                   (vector (axllm:object "id" "ok_1" "name" "run" "arguments" "{}")
                           (axllm:object "id" "" "name" "run" "arguments" "{}")
                           (axllm:object "id" "ok_2" "name" "  " "arguments" "{}")
                           (axllm:object "id" "ok_3" "type" "custom" "name" "run")
                           (axllm:object "id" "ok_4" "name" "run" "arguments" 7)))))
    (is (length problems) 4 "the passing call reports no problem")
    (is (axllm:jget (aref problems 0) "index") 1 "problems carry the call index")
    (is (axllm:jget (aref problems 0) "kind") "call" "a bad id runs as given")
    (is (axllm:jget (aref problems 1) "kind") "unnamed" "a blank name is corrected")
    (is (axllm:jget (aref problems 2) "kind") "call" "a bad type runs as given")
    (is (axllm:jget (aref problems 3) "kind") "call" "numeric arguments run as given")
    (has-substring (axllm:jget (aref problems 2) "message") "\"custom\""
                   "the reported value is the rejected one")))

(defcase tool-call-problems-accepts-object-arguments
  ;; The reference's typeof check passes a string or an object; null and
  ;; arrays are objects in JavaScript, so they pass the shape check and fail
  ;; later in argument validation instead.
  (is (length (axllm::tool-call-problems
               (vector (axllm:object "id" "a" "name" "run" "arguments" (axllm:object "x" 1))
                       (axllm:object "id" "b" "name" "run" "arguments" (vector 1 2))
                       (axllm:object "id" "c" "name" "run" "arguments" :null))))
      0
      "object, array and null arguments pass the shape check"))

(defcase tool-call-shape-errors-never-carry-the-credential
  (let ((secret "sk-live-shape-check-secret-value"))
    (multiple-value-bind (client tape)
        (scripted-client "openai"
                         (list (openai-tool-response
                                (list (openai-wire-call "call_1" "run" "{}" :type "custom"))))
                         :api-key secret)
      (declare (ignore tape))
      (let ((condition (signals-provider-error :response
                         (axllm:chat client (vector (axllm:message "user" "hi"))))))
        (lacks-substring (princ-to-string condition) secret
                         "the shape failure is redacted like every other provider error")))))

;;; ------------------------------------------------------------------
;;; A scripted service, to exercise the native service generics
;;;
;;; This is a service object like any other: the tests dispatch the same
;;; generics on it that Gen, Flow and Agent dispatch, so a method that only
;;; works for AI-CLIENT shows up here.
;;; ------------------------------------------------------------------

(defclass recording-service ()
  ((requests :initform '() :accessor recorded-requests)
   (reply :initarg :reply :initform nil :reader service-reply)
   (chunks :initarg :chunks :initform nil :reader service-chunks)))

(defun recording-service (&key reply chunks)
  (make-instance 'recording-service :reply reply :chunks chunks))

(defun recorded-request (service n)
  (let ((all (reverse (recorded-requests service))))
    (ok (< n (length all)) (format nil "expected at least ~a request(s)" (1+ n)))
    (nth n all)))

(defmethod axllm::ax-service-name ((service recording-service)) "recording")
(defmethod axllm::ax-id ((service recording-service)) "recording-1")
(defmethod axllm::ax-features ((service recording-service) &optional model)
  (declare (ignore model))
  (axllm:object "functions" axllm:true "streaming" axllm:true))

(defmethod axllm::ax-chat ((service recording-service) request &optional options)
  (declare (ignore options))
  (push request (recorded-requests service))
  (or (service-reply service)
      (axllm:object "results" (vector (axllm:object "index" 0 "content" "ok"
                                                    "finish_reason" "stop")))))

(defmethod axllm::ax-stream ((service recording-service) request &optional options)
  (declare (ignore options))
  (push request (recorded-requests service))
  (axllm::ax-stream-handle-over (or (service-chunks service) (list))))

(defmethod axllm::ax-embed ((service recording-service) request &optional options)
  (declare (ignore options))
  (push request (recorded-requests service))
  (axllm:object "embeddings" (vector (vector 0.1d0 0.2d0))))

(defun core-request (&key (prompt "hi") (config nil))
  (axllm:object "chat_prompt" (vector (axllm:object "role" "user" "content" prompt))
                "model_config" (or config (axllm:object))))

;;; ------------------------------------------------------------------
;;; Token-usage rate limiter
;;;
;;; Reference: src/ax/util/rate-limit.ts and rate-limit.test.ts.
;;; ------------------------------------------------------------------

(defcase rate-limiter-passes-requests-inside-the-budget
  (let ((limiter (axllm::rate-limiter-token-usage 100 10)))
    (ok (< (axllm::rate-limiter-acquire limiter 50) 0.2d0)
        "the first request inside the budget does not wait")
    (ok (< (axllm::rate-limiter-acquire limiter 50) 0.2d0)
        "a second request inside the remaining budget does not wait either")))

(defcase rate-limiter-resolves-a-request-larger-than-the-bucket
  ;; Regression the reference calls out: a refill caps the balance at the
  ;; bucket size, so waiting for more than the bucket would never finish.
  (let ((limiter (axllm::rate-limiter-token-usage 100 1000)))
    (ok (< (axllm::rate-limiter-acquire limiter 150) 1.0d0)
        "an oversized request completes instead of hanging")))

(defcase rate-limiter-lets-the-balance-go-negative-for-an-oversized-request
  ;; The discriminating check: an implementation that clamps the balance at
  ;; zero instead of borrowing passes the test above but fails here, and then
  ;; silently serves more than the configured average rate.
  (let ((limiter (axllm::rate-limiter-token-usage 100 100)))
    (axllm::rate-limiter-acquire limiter 150)
    (let ((available (axllm::rate-limiter-available limiter)))
      (ok (< available 0)
          (format nil "the borrowed capacity is still owed (balance ~,2f)" available)))))

(defcase rate-limiter-repays-borrowed-capacity-before-the-next-oversized-request
  ;; 100-token bucket refilling at 100 tokens a second.  The first oversized
  ;; request leaves the balance at -50, so the next one has to wait for the
  ;; debt and a full bucket: 150 tokens at 100 a second is 1.5 seconds.  A
  ;; limiter that forgave the debt would wait only 1.0 second.
  (let ((limiter (axllm::rate-limiter-token-usage 100 100)))
    (axllm::rate-limiter-acquire limiter 150)
    (let ((waited (axllm::rate-limiter-acquire limiter 150)))
      (ok (> waited 1.35d0)
          (format nil "the second oversized request repays the debt first (waited ~,3fs)" waited))
      (ok (< waited 3.0d0)
          (format nil "and does not wait substantially longer than the rate implies (~,3fs)"
                  waited)))))

(defcase rate-limiter-rejects-a-nonsense-configuration
  (signals-provider-error :config (axllm::rate-limiter-token-usage 0 10))
  (signals-provider-error :config (axllm::rate-limiter-token-usage 100 0)))

(defcase rate-limiter-stops-waiting-when-the-run-is-cancelled
  (let ((limiter (axllm::rate-limiter-token-usage 10 1))
        (token (axllm::cancellation-token)))
    (axllm::rate-limiter-acquire limiter 10)
    (axllm::cancel token "user stopped")
    (signals-provider-error :aborted
      (axllm::rate-limiter-acquire limiter 10 :cancellation token))))

;;; ------------------------------------------------------------------
;;; Cancellation
;;; ------------------------------------------------------------------

(defcase cancellation-is-recorded-once
  (let ((token (axllm::cancellation-token)))
    (is (axllm::cancelled-p token) nil "a fresh token is not cancelled")
    (is (axllm::cancel token "stopped") t "the first cancel takes effect")
    (is (axllm::cancel token "again") nil "a second cancel is a no-op")
    (is (axllm::cancellation-reason token) "stopped" "the first reason is kept")))

(defcase cancellation-wait-returns-at-once-when-cancelled
  (let ((token (axllm::cancellation-token))
        (started (axllm::%monotonic-seconds)))
    (axllm::cancel token)
    (is (axllm::cancellation-wait token 5) t "waiting on a cancelled token returns true")
    (ok (< (- (axllm::%monotonic-seconds) started) 1.0d0)
        "and does not sleep out the timeout")))

(defcase cancellation-wait-times-out-without-a-cancel
  (let ((token (axllm::cancellation-token)))
    (is (axllm::cancellation-wait token 0.05d0) nil
        "an uncancelled token reports that it is still running")))

(defcase cancellation-throws-the-aborted-kind
  (let ((token (axllm::cancellation-token)))
    (is (axllm::throw-if-cancelled token) token "an uncancelled token passes through")
    (axllm::cancel token "user stopped")
    (let ((condition (signals-provider-error :aborted (axllm::throw-if-cancelled token))))
      (has-substring (princ-to-string condition) "user stopped"
                     "the reason reaches the condition"))))

(defcase cancellation-notifies-subscribers-exactly-once
  (let* ((token (axllm::cancellation-token))
         (calls 0)
         (remove (axllm::cancellation-subscribe token (lambda () (incf calls)))))
    (is (axllm::cancellation-subscription-count token) 1 "the subscription is registered")
    (axllm::cancel token)
    (axllm::cancel token)
    (is calls 1 "the listener runs once, not once per cancel")
    (funcall remove)
    ;; A listener added after the fact still learns, so a late subscriber does
    ;; not wait forever on a token that is already cancelled.
    (let ((late 0))
      (axllm::cancellation-subscribe token (lambda () (incf late)))
      (is late 1 "a subscriber added after cancellation is told immediately"))))

(defcase cancellation-survives-a-failing-subscriber
  (let ((token (axllm::cancellation-token))
        (reached nil))
    (axllm::cancellation-subscribe token (lambda () (error "listener exploded")))
    (axllm::cancellation-subscribe token (lambda () (setf reached t)))
    (is (axllm::cancel token) t "a failing listener does not break the cancel")
    (is reached t "and the other listeners still run")))

;;; ------------------------------------------------------------------
;;; Retry backoff (intrinsic.retry.sleep)
;;; ------------------------------------------------------------------

(defcase retry-sleep-backs-off-and-grows
  (let ((started (axllm::%monotonic-seconds)))
    (axllm/core::core-retry-sleep 0 nil :null)
    (let ((waited (- (axllm::%monotonic-seconds) started)))
      (ok (>= waited 0.2d0) (format nil "attempt 0 waits about a quarter second (~,3fs)" waited))
      (ok (< waited 1.0d0) "and not much more"))))

(defcase retry-sleep-is-capped
  (let ((started (axllm::%monotonic-seconds)))
    (axllm/core::core-retry-sleep 99 nil :null)
    (let ((waited (- (axllm::%monotonic-seconds) started)))
      (ok (< waited 1.6d0)
          (format nil "a high attempt is capped at a second, not multiplied (~,3fs)" waited)))))

(defcase retry-sleep-fails-fast-on-a-cancelled-run
  (let* ((token (axllm::cancellation-token))
         (options (axllm:object "cancellation" token))
         (started (axllm::%monotonic-seconds)))
    (axllm::cancel token "stopped")
    (signals-provider-error :aborted (axllm/core::core-retry-sleep 3 nil options))
    (ok (< (- (axllm::%monotonic-seconds) started) 0.5d0)
        "a cancelled run does not sleep out its backoff before failing")))

(defcase retry-sleep-rejects-a-bogus-cancellation-value
  (signals-provider-error :config
    (axllm/core::core-retry-sleep 0 nil (axllm:object "cancellation" "not-a-token"))))

;;; ------------------------------------------------------------------
;;; Failure classification (intrinsic.exception.is_*)
;;; ------------------------------------------------------------------

(defcase classification-matches-the-reference-retry-policy
  (flet ((infra (kind &optional status)
           (axllm/core::core-true-p
            (axllm/core::core-exception-is-infrastructure
             (axllm::make-provider-error kind "x" :status status)))))
    (is (infra :transport) t "a transport failure is retried unchanged")
    (is (infra :network) t "so is a network failure")
    (is (infra :timeout) t "so is a timeout")
    (is (infra :stream) t "so is a terminated stream")
    (is (infra :status 503) t "a 5xx is infrastructure")
    (is (infra :http 500) t "including the kind this port signalled before Core")
    ;; The discriminating half: a 4xx is the request's own fault and retrying
    ;; it unchanged would just burn the budget.
    (is (infra :status 400) nil "a 4xx is not infrastructure")
    (is (infra :auth 401) nil "neither is an auth rejection")
    (is (infra :response) nil "neither is a malformed response")
    (is (infra :refusal) nil "neither is a refusal")))

(defcase classification-separates-refusal-abort-and-validation
  (flet ((asks (fn kind)
           (axllm/core::core-true-p
            (funcall fn (axllm::make-provider-error kind "x")))))
    (is (asks #'axllm/core::core-exception-is-refusal :refusal) t "a refusal is a refusal")
    (is (asks #'axllm/core::core-exception-is-refusal :response) nil
        "a malformed response is not")
    (is (asks #'axllm/core::core-exception-is-aborted :aborted) t "an abort is an abort")
    (is (asks #'axllm/core::core-exception-is-aborted :transport) nil
        "a transport failure is not an abort")
    (is (axllm/core::core-true-p
         (axllm/core::core-exception-is-validation
          (make-condition 'axllm:validation-error :message "bad")))
        t "a validation error is recognized by class")
    (is (asks #'axllm/core::core-exception-is-validation :response) nil
        "a provider response error is not a validation error")))

(defcase core-error-constructors-preserve-the-public-contract
  ;; Core builds the condition and raises it separately, and whatever it
  ;; builds still has to be catchable as provider-error with the documented
  ;; kind/provider/status readers.
  (let ((status-error (axllm/core::core-ai-error-status "boom" 529 "overloaded" :null :null
                                                       axllm:true)))
    (ok (typep status-error 'axllm:provider-error) "a Core status error is a provider-error")
    (is (axllm:provider-error-kind status-error) :status "its kind is :status")
    (is (axllm:provider-error-status status-error) 529 "its status survives")
    (is (axllm::provider-error-code status-error) "overloaded" "its provider code survives")
    (is (axllm::provider-error-retryable-p status-error) t "and so does retryability")
    (is (handler-case (error status-error) (axllm:provider-error (c) (princ-to-string c)))
        "boom" "it is catchable as provider-error and reports its message"))
  (is (axllm:provider-error-kind (axllm/core::core-ai-error-unsupported "no realtime"))
      :unsupported "an unsupported capability is its own kind")
  (is (axllm:provider-error-kind (axllm/core::core-ai-error-refusal "declined"))
      :refusal "a refusal is its own kind")
  (is (axllm::provider-error-retryable-p (axllm/core::core-ai-error-stream "cut" :null
                                                                          axllm:true))
      t "a terminated stream defaults to retryable"))

(defcase core-exception-rewrap-keeps-the-kind-and-the-cause
  (let* ((original (axllm::make-provider-error :status "original" :status 503 :provider "openai"))
         (wrapped (axllm/core::core-exception-rewrap original "clearer message")))
    (ok (typep wrapped 'axllm:provider-error)
        "a rewrapped provider failure is still a provider-error, so existing handlers catch it")
    (is (axllm:provider-error-kind wrapped) :status "the kind is kept")
    (is (axllm:provider-error-status wrapped) 503 "the status is kept")
    (is (axllm:provider-error-provider wrapped) "openai" "the provider is kept")
    (is (princ-to-string wrapped) "clearer message" "the message is the new one")
    (ok (eq (axllm::provider-error-cause wrapped) original) "and the original is the cause")
    ;; Still classified the same way, or a rewrap would quietly change whether
    ;; the run retries.
    (is (axllm/core::core-true-p (axllm/core::core-exception-is-infrastructure wrapped)) t
        "a rewrapped 5xx is still infrastructure")))

(defcase core-exception-generate-wraps-without-claiming-the-kind
  (let* ((original (axllm::make-provider-error :response "bad json"))
         (wrapped (axllm/core::core-exception-generate original "generate failed")))
    (ok (typep wrapped 'axllm::ax-generate-error) "it is the reference's generate error")
    (ok (not (typep wrapped 'axllm:provider-error))
        "and not a provider-error, so a provider handler does not swallow it")
    (ok (eq (axllm::ax-generate-error-cause wrapped) original) "the cause is the original")
    (is (axllm/core::core-exception-message wrapped) "generate failed"
        "and the message is readable through the Core boundary")))

;;; ------------------------------------------------------------------
;;; Stream handles and stream.event_content_parts
;;; ------------------------------------------------------------------

(defcase stream-handle-ends-with-core-none
  (let ((handle (axllm::ax-stream-handle-over (list "a" "b"))))
    (is (axllm::ax-stream-next handle) "a" "the first chunk")
    (is (axllm::ax-stream-next handle) "b" "the second chunk")
    (is (axllm::ax-stream-next handle) :null "exhaustion is Core's none, not NIL")
    (is (axllm::ax-stream-next handle) :null "and stays exhausted")))

(defcase stream-handle-close-is-idempotent-and-shields-a-failing-closer
  (let* ((closes 0)
         (handle (axllm::make-ax-stream-handle (lambda () :null)
                                               :closer (lambda ()
                                                         (incf closes)
                                                         (error "transport already gone")))))
    (is (axllm::ax-stream-close handle) :null
        "closing an abandoned stream does not raise a second failure")
    (axllm::ax-stream-close handle)
    (is closes 1 "and the transport is released once")))

(defcase default-stream-uses-the-services-single-response
  (let* ((service (recording-service))
         (handle (axllm::ax-stream-handle-over
                  (list (axllm::ax-chat service (core-request))))))
    (ok (not (eq (axllm::ax-stream-next handle) :null))
        "a service without native streaming still answers one chunk")
    (is (axllm::ax-stream-next handle) :null "and then ends")))

(defcase stream-event-content-parts-reads-each-shape
  (flet ((parts (event) (coerce (axllm/core::core-stream-event-content-parts event) 'list)))
    (is (parts "raw") (list "raw") "a bare string is its own content")
    (is (parts (axllm:object "results"
                             (vector (axllm:object "content" "one")
                                     (axllm:object "content" "two"))))
        (list "one" "two") "a normalized response answers one string per result")
    (is (parts (axllm:object "delta" "d")) (list "d") "a delta chunk answers its delta")
    (is (parts (axllm:object "data" (axllm:object "text" "t"))) (list "t")
        "a wrapped event is unwrapped")
    (is (parts (axllm:object "type" "done" "content" "ignored")) '()
        "a terminal event has no content, even when it carries a field that looks like some")
    (is (parts (axllm:object "type" "message_stop")) '()
        "and neither does the Anthropic terminal event")
    (is (parts 7) '() "a value that is not an event answers nothing")))

;;; ------------------------------------------------------------------
;;; intrinsic.ai.complete_once
;;; ------------------------------------------------------------------

(defcase complete-once-uses-chat-for-a-nonstreaming-request
  (let* ((service (recording-service))
         (completion (axllm/core::core-ai-complete-once service (core-request) :null)))
    (is (length (recorded-requests service)) 1 "exactly one turn")
    (is (axllm:jget completion "content") "ok" "the completion carries the content")))

(defcase complete-once-folds-a-streamed-request
  ;; The reference folds a streamed forward's chunks into one response, so a
  ;; caller that asked for streaming and one that did not see the same thing.
  (let* ((service (recording-service
                   :chunks (list (axllm:object "results"
                                               (vector (axllm:object "index" 0 "content" "Hel")))
                                 (axllm:object "results"
                                               (vector (axllm:object "index" 0 "content" "lo"
                                                                     "finish_reason" "stop"))))))
         (completion (axllm/core::core-ai-complete-once
                      service
                      (core-request :config (axllm:object "stream" axllm:true))
                      :null)))
    (is (axllm:jget completion "content") "Hello"
        "the streamed chunks are folded into one completion")))

(defcase client-features-never-fabricates-capabilities
  (is (axllm::%object-keys (axllm/core::core-ai-client-features nil :null)) '()
      "a missing client reports no capabilities rather than inventing them")
  (let ((features (axllm/core::core-ai-client-features (recording-service) :null)))
    (is (axllm:jget features "functions") axllm:true "a service's own answer is used")))

;;; ------------------------------------------------------------------
;;; Run controls, through the host protocol only
;;;
;;; The run control itself belongs to the agent runtime; this port has exactly
;;; one implementation of it.  The provider layer reaches a control only
;;; through the host protocol in axllm/core, so these cases drive that
;;; protocol with a double rather than a second control class.  Anything that
;;; answers CORE-HOST-GET "aborted" and CORE-HOST-CALL "take_pending" works.
;;; ------------------------------------------------------------------

(defclass stub-control ()
  ((pending :initarg :pending :initform '() :accessor stub-pending)
   (aborted :initarg :aborted :initform nil :accessor stub-aborted)
   (events :initform '() :accessor stub-events)
   (emit-name :initarg :emit-name :initform "emit" :reader stub-emit-name)))

(defun stub-control (&key pending aborted (emit-name "emit"))
  (make-instance 'stub-control :pending (copy-list pending) :aborted aborted
                               :emit-name emit-name))

(defun stub-event-types (control)
  (mapcar (lambda (event) (axllm:jget event "type")) (reverse (stub-events control))))

(defmethod axllm/core::core-host-get ((target stub-control) key &optional (fallback :null))
  (cond ((equal key "aborted") (axllm/core::core-bool (stub-aborted target)))
        ((equal key "pending_count") (length (stub-pending target)))
        (t fallback)))

(defmethod axllm/core::core-host-call ((target stub-control) method args)
  (cond
    ((equal method (stub-emit-name target))
     (push (aref args 0) (stub-events target))
     (aref args 0))
    ((equal method "take_pending")
     (let ((pending (stub-pending target)))
       (setf (stub-pending target) '())
       (coerce pending 'vector)))
    ((equal method "pending_count") (length (stub-pending target)))
    ;; The agent runtime's control queues an update under "steer"; a boundary
    ;; returns another stage's update that way, so the double has it too.
    ((equal method "steer")
     (setf (stub-pending target) (append (stub-pending target) (list (aref args 0))))
     target)
    (t (call-next-method))))

(defclass minimal-control ()
  ((pending :initarg :pending :initform '() :accessor minimal-pending)))

(defmethod axllm/core::core-host-get ((target minimal-control) key &optional (fallback :null))
  (cond ((equal key "aborted") axllm:false)
        ((equal key "pending_count") (length (minimal-pending target)))
        (t fallback)))

(defmethod axllm/core::core-host-call ((target minimal-control) method args)
  (declare (ignore args))
  (cond ((equal method "take_pending")
         (let ((pending (minimal-pending target)))
           (setf (minimal-pending target) '())
           (coerce pending 'vector)))
        ((equal method "pending_count") (length (minimal-pending target)))
        (t (call-next-method))))

(defun steer-update (text &key target id)
  (let ((update (axllm:object "type" "steer" "text" text)))
    (when target (setf (gethash "target" update) target))
    (when id (setf (gethash "id" update) id))
    update))

(defcase a-queued-steer-reaches-the-next-request
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (inner (recording-service))
         (service (axllm::boundary-service inner control)))
    (axllm::ax-chat service (core-request :prompt "explain Ax"))
    (let* ((sent (recorded-request inner 0))
           (prompt (axllm:jget sent "chat_prompt")))
      (is (length prompt) 2 "the steering turn is added to the request")
      (is (axllm:jget (aref prompt 1) "content") "be brief" "and carries the caller's text")
      (is (axllm:jget (aref prompt 1) "role") "user"
          "as a user turn, which is what the providers accept"))
    (is (stub-event-types control) (list "started" "applied")
        "the boundary announces its start and the update it applied")
    (is (axllm:jget (second (reverse (stub-events control))) "timing") "next-response"
        "and is honest about when the change took effect")))

(defcase an-applied-update-is-not-applied-twice
  ;; Replaying a steer would repeat the user's instruction in every later
  ;; request of the run.
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (inner (recording-service))
         (service (axllm::boundary-service inner control)))
    (axllm::ax-chat service (core-request))
    (axllm::ax-chat service (core-request))
    (is (length (axllm:jget (recorded-request inner 0) "chat_prompt")) 2
        "the first request carries the steer")
    (is (length (axllm:jget (recorded-request inner 1) "chat_prompt")) 1
        "the second does not carry it again")))

(defcase applying-updates-does-not-rewrite-the-callers-request
  ;; Core folds a steering turn in by appending to the request's own prompt,
  ;; so a boundary that handed Core the caller's request would leave the steer
  ;; in the caller's history and send it again on the next turn.
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (inner (recording-service))
         (service (axllm::boundary-service inner control))
         (request (core-request :prompt "explain Ax")))
    (axllm::ax-chat service request)
    (is (length (axllm:jget request "chat_prompt")) 1
        "the caller's request still holds only its own turn")
    (is (length (axllm:jget (recorded-request inner 0) "chat_prompt")) 2
        "while the request that went out carries the steer")))

(defcase a-thinking-budget-update-is-applied-by-core-not-reinvented
  ;; The budget is Core's decision; the boundary only carries the update and
  ;; remembers the level Core settled on, so a later request keeps it.
  (let* ((control (stub-control :pending (list (axllm:object "type" "thinking"
                                                            "level" "low"))))
         (inner (recording-service))
         (service (axllm::boundary-service inner control)))
    (axllm::ax-chat service (core-request))
    (is (axllm::%boundary-level service) "low"
        "the level Core applied is remembered for the requests that follow")))

(defcase the-forward-takes-the-updates-so-the-boundary-does-not-reapply-them
  ;; intrinsic.ai.control_take_pending: a forward applies the queued updates
  ;; when a step starts, so the next request boundary must skip them.
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (inner (recording-service))
         (service (axllm::boundary-service inner control)))
    (is (axllm/core::core-ai-control-pending-count service) 1 "one update is waiting")
    (let ((taken (axllm/core::core-ai-control-take-pending service)))
      (is (length taken) 1 "the forward receives it")
      (is (axllm:jget (aref taken 0) "text") "be brief" "with its text"))
    (is (axllm/core::core-ai-control-pending-count service) 0
        "and the boundary no longer considers it pending")
    (axllm::ax-chat service (core-request))
    (is (length (axllm:jget (recorded-request inner 0) "chat_prompt")) 1
        "so the request does not apply it a second time")))

(defcase counting-pending-updates-does-not-consume-them
  ;; A count that drained the queue would silently cancel the steer the next
  ;; request was going to apply.
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (service (axllm::boundary-service (recording-service) control)))
    (is (axllm/core::core-ai-control-pending-count service) 1 "the update is counted")
    (is (axllm/core::core-ai-control-pending-count service) 1 "and still there")
    (is (length (axllm/core::core-ai-control-take-pending service)) 1
        "so the forward can still take it")))

(defcase a-service-without-a-control-reports-no-pending-updates
  (let ((service (recording-service)))
    (is (length (axllm/core::core-ai-control-take-pending service)) 0
        "a plain service answers an empty list rather than failing")
    (is (axllm/core::core-ai-control-pending-count service) 0 "and a zero count")))

(defcase aborting-stops-the-run-before-the-next-request
  (let* ((control (stub-control :aborted t))
         (inner (recording-service))
         (service (axllm::boundary-service inner control)))
    (signals-provider-error :aborted (axllm::ax-chat service (core-request)))
    (is (length (recorded-requests inner)) 0
        "an aborted run sends nothing, rather than sending and discarding")))

(defcase an-aborted-json-control-object-also-stops-the-run
  ;; Core reads the same "aborted" key off a plain object, so a caller that
  ;; passes one instead of a control gets the same behaviour.
  (let* ((inner (recording-service))
         (service (axllm::boundary-service inner (axllm:object "aborted" axllm:true))))
    (signals-provider-error :aborted (axllm::ax-chat service (core-request)))
    (is (length (recorded-requests inner)) 0 "and nothing is sent")))

(defcase no-control-at-all-is-not-an-error
  (let* ((inner (recording-service))
         (service (axllm::boundary-service inner :null)))
    (axllm::ax-chat service (core-request))
    (is (length (recorded-requests inner)) 1
        "a boundary with no control simply forwards the request")))

(defcase the-boundary-tolerates-either-emit-name
  ;; Flow reports lifecycle through core-host-call "_emit"; the agent
  ;; runtime's control answers "emit".  The boundary must work with either, or
  ;; one of the two subsystems loses its events.
  (dolist (name (list "emit" "_emit"))
    (let* ((control (stub-control :emit-name name))
           (service (axllm::boundary-service (recording-service) control)))
      (axllm::boundary-close service)
      (is (stub-event-types control) (list "started" "completed")
          (format nil "a control answering ~s hears the whole lifecycle" name)))))

(defcase a-node-scoped-update-is-held-for-its-own-stage
  ;; Core decides which path an update reaches.  A boundary that drained the
  ;; control's queue regardless of target would silently cancel a sibling
  ;; stage's steering instead of leaving it for that stage.
  (let* ((control (stub-control :pending (list (steer-update "team only"
                                                            :target "root/team"))))
         (team-inner (recording-service))
         (other-inner (recording-service))
         (other (axllm::boundary-service other-inner control :path "root/other"))
         (team (axllm::boundary-service team-inner control :path "root/team")))
    ;; The sibling runs first and must not consume the team's update.
    (axllm::ax-chat other (core-request))
    (is (length (axllm:jget (recorded-request other-inner 0) "chat_prompt")) 1
        "the sibling stage does not apply another stage's update")
    (axllm::ax-chat team (core-request))
    (is (length (axllm:jget (recorded-request team-inner 0) "chat_prompt")) 2
        "and the targeted stage still receives it")))

(defcase a-control-that-cannot-take-an-update-back-does-not-lose-it
  ;; A control with no "steer" method cannot be given an update back, so the
  ;; boundary holds it rather than dropping it.  Losing a steer silently is
  ;; worse than applying it one stage late.
  (let* ((control (make-instance 'minimal-control
                                 :pending (list (steer-update "team only"
                                                              :target "root/team"))))
         (inner (recording-service))
         (other (axllm::boundary-service inner control :path "root/other")))
    (axllm::ax-chat other (core-request))
    (is (length (axllm:jget (recorded-request inner 0) "chat_prompt")) 1
        "the wrong stage does not apply it")
    (is (length (axllm::%boundary-held other)) 1
        "and the update is still held rather than discarded")))

(defcase an-untargeted-update-reaches-every-stage
  ;; A control that does not scope its updates keeps working: an update with
  ;; no target is a root update.
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (inner (recording-service))
         (service (axllm::boundary-service inner control :path "root/team")))
    (axllm::ax-chat service (core-request))
    (is (length (axllm:jget (recorded-request inner 0) "chat_prompt")) 2
        "an update with no target applies at any path")))

(defcase the-boundary-is-a-service-like-any-other
  ;; Gen, Flow and Agent dispatch the generics; none of them may need to know
  ;; that this particular service is a control boundary.
  (let* ((control (stub-control))
         (inner (recording-service))
         (service (axllm::boundary-service inner control)))
    (is (axllm::ax-service-name service) "recording" "the name comes from the inner service")
    (is (axllm::ax-id service) "recording-1" "and so does the identity")
    (is (axllm:jget (axllm::ax-features service) "streaming") axllm:true
        "features are forwarded")
    (is (length (axllm:jget (axllm::ax-embed service (axllm:object "texts" (vector "a")))
                            "embeddings"))
        1 "embeddings are forwarded")
    (ok (axllm:jget (axllm::ax-metrics service) "latency") "metrics are forwarded")
    (is (axllm::ax-estimated-cost service) 0 "and so is cost")
    (is (axllm/core::core-host-get service "execution_path") "root"
        "and Core can read the path it runs at")))

(defcase a-streamed-turn-goes-through-the-same-boundary
  (let* ((control (stub-control :pending (list (steer-update "be brief"))))
         (inner (recording-service :chunks (list (axllm:object "delta" "x"))))
         (service (axllm::boundary-service inner control)))
    (let ((handle (axllm::ax-stream service (core-request))))
      (is (axllm:jget (axllm::ax-stream-next handle) "delta") "x" "the chunk arrives")
      (is (axllm::ax-stream-next handle) :null "and the stream ends")
      (axllm::ax-stream-close handle))
    (is (length (axllm:jget (recorded-request inner 0) "chat_prompt")) 2
        "a streamed request applies the control updates too")))

(defcase closing-a-boundary-reports-the-outcome
  (let* ((control (stub-control))
         (service (axllm::boundary-service (recording-service) control)))
    (axllm::boundary-close service)
    (is (stub-event-types control) (list "started" "completed") "a clean run completes"))
  (let* ((control (stub-control))
         (service (axllm::boundary-service (recording-service) control)))
    (axllm::boundary-close service "transport died")
    (is (stub-event-types control) (list "started" "failed") "a failed run says so")
    (has-substring (axllm:jget (second (reverse (stub-events control))) "error")
                   "transport died" "and carries the reason")))

;;; ------------------------------------------------------------------
;;; chat's :model override
;;; ------------------------------------------------------------------

(defcase chat-accepts-a-per-call-model-without-changing-the-client
  (multiple-value-bind (client tape)
      (scripted-client "openai"
                       (list (axllm:encode-json
                              (axllm:object
                               "choices"
                               (vector (axllm:object "index" 0 "finish_reason" "stop"
                                                     "message" (axllm:object
                                                                "role" "assistant"
                                                                "content" "ok"))))))
                       :model "gpt-6-luna")
    (axllm:chat client (vector (axllm:message "user" "hi")) :model "gpt-6-astra")
    (is (axllm:jget (tape-request tape 0) "model") "gpt-6-astra"
        "the per-call model is what goes on the wire")
    (is (axllm:ai-model client) "gpt-6-luna"
        "and the client's own model is unchanged")))

(defcase chat-model-override-reaches-anthropic-too
  (multiple-value-bind (client tape)
      (scripted-client "anthropic"
                       (list (axllm:encode-json
                              (axllm:object "stop_reason" "end_turn"
                                            "content" (vector (axllm:object "type" "text"
                                                                            "text" "ok"))
                                            "usage" (axllm:object "input_tokens" 1
                                                                  "output_tokens" 1)))))
    (axllm:chat client (vector (axllm:message "user" "hi")) :model "claude-opus-5-5")
    (is (axllm:jget (tape-request tape 0) "model") "claude-opus-5-5"
        "the Messages request carries the per-call model")))

(defcase chat-rejects-a-blank-model-override
  (multiple-value-bind (client tape)
      (scripted-client "openai" (list "{}"))
    (signals-provider-error :config
      (axllm:chat client (vector (axllm:message "user" "hi")) :model "   "))
    (is (tape-call-count tape) 0 "and sends nothing")))

(defcase the-requests-model-wins-over-the-clients-default
  ;; Core selects the model before the request reaches a service, so a service
  ;; that ignored request["model"] would quietly answer from the wrong model.
  (multiple-value-bind (client tape)
      (scripted-client "openai"
                       (list (axllm:encode-json
                              (axllm:object
                               "choices"
                               (vector (axllm:object "index" 0 "finish_reason" "stop"
                                                     "message" (axllm:object
                                                                "role" "assistant"
                                                                "content" "ok"))))))
                       :model "gpt-6-luna")
    (let ((request (core-request)))
      (setf (gethash "model" request) "gpt-6-astra")
      (axllm::ax-chat client request))
    (is (axllm:jget (tape-request tape 0) "model") "gpt-6-astra"
        "the service honours the model Core chose")))

;;; ------------------------------------------------------------------
;;; The guarantee behind the batch preflight, checked at this layer
;;;
;;; A tool call is a side effect, and rejecting the batch afterwards cannot
;;; undo it.  The generation suite owns the full-batch preflight; what this
;;; case proves is that the provider layer's own earlier rejection does not
;;; weaken the guarantee: still no handler runs, and still only one provider
;;; request is made.
;;; ------------------------------------------------------------------

(defun effect-tool (name log)
  (axllm:tool :name name
              :description (format nil "Record a call to ~a" name)
              :parameters (axllm:object "type" "object"
                                        "properties" (axllm:object
                                                      "key" (axllm:object "type" "string"))
                                        "required" (vector "key"))
              :handler (lambda (args)
                         (push (cons name (axllm:jget args "key")) (cdr log))
                         "recorded")))

(defcase a-malformed-later-call-still-runs-no-earlier-handler
  (dolist (broken (list
                   ;; No id at all.
                   (axllm:object "type" "function"
                                 "function" (axllm:object "name" "effect_b"
                                                          "arguments" "{\"key\":\"b\"}"))
                   ;; A blank id.
                   (axllm:object "id" "  " "type" "function"
                                 "function" (axllm:object "name" "effect_b"
                                                          "arguments" "{\"key\":\"b\"}"))
                   ;; A blank name.
                   (axllm:object "id" "call_b" "type" "function"
                                 "function" (axllm:object "name" ""
                                                          "arguments" "{\"key\":\"b\"}"))
                   ;; A call kind this port cannot run.
                   (axllm:object "id" "call_b" "type" "not_function"
                                 "function" (axllm:object "name" "effect_b"
                                                          "arguments" "{\"key\":\"b\"}"))))
    (let* ((log (list :log))
           (tool-a (effect-tool "effect_a" log))
           (tool-b (effect-tool "effect_b" log)))
      (multiple-value-bind (client tape)
          (scripted-client
           "openai"
           (list (axllm:encode-json
                  (axllm:object
                   "choices"
                   (vector (axllm:object
                            "index" 0 "finish_reason" "tool_calls"
                            "message" (axllm:object
                                       "role" "assistant" "content" ""
                                       "tool_calls"
                                       (vector (axllm:object
                                                "id" "call_a" "type" "function"
                                                "function" (axllm:object
                                                            "name" "effect_a"
                                                            "arguments" "{\"key\":\"a\"}"))
                                               broken))))))))
        (let ((gen (axllm:ax "question:string -> answer:string"
                             :tools (list tool-a tool-b))))
          (handler-case (progn (axllm:forward gen client (axllm:object "question" "go"))
                               (error 'check-failed
                                      :text (format nil "the batch ~a should not have run"
                                                    (axllm:encode-json broken))))
            (axllm:ax-error () nil))
          (is (cdr log) '()
              (format nil "no handler ran for a batch whose second call is ~a"
                      (axllm:encode-json broken)))
          (ok (plusp (tape-call-count tape))
              "the request was made"))))))

;;; ------------------------------------------------------------------
;;; The server-sent-events decoder
;;;
;;; Reference: pyAI.py's _iter_sse_json.  Every case below is a real provider
;;; behaviour, and each is one a plausible wrong decoder gets wrong: a
;;; fill-pointer line buffer handed on without copying, a chunk split inside a
;;; multi-byte character, bare carriage returns, a stream that ends without its
;;; final blank line.
;;; ------------------------------------------------------------------

(defun sse-event-list (text) (axllm::sse-events text))

(defun feed-in-slices (text size)
  "Decode TEXT through a handle that hands over SIZE bytes at a time."
  (let* ((bytes (sb-ext:string-to-octets text :external-format :utf-8))
         (offset 0)
         (handle (axllm::sse-stream-handle
                  (lambda ()
                    (when (< offset (length bytes))
                      (let ((end (min (length bytes) (+ offset size))))
                        (prog1 (subseq bytes offset end) (setf offset end)))))))
         (out '()))
    (loop for event = (axllm::ax-stream-next handle)
          until (eq event :null)
          do (push event out))
    (axllm::ax-stream-close handle)
    (nreverse out)))

(defcase sse-decodes-a-plain-event-stream
  (let ((events (sse-event-list
                 (format nil "data: {\"a\":1}~%~%data: {\"a\":2}~%~%data: [DONE]~%~%"))))
    (is (length events) 2 "both events are decoded and [DONE] is not one of them")
    (is (axllm:jget (first events) "a") 1 "the first event's payload")
    (is (axllm:jget (second events) "a") 2 "the second event's payload")))

(defcase sse-stops-at-the-done-sentinel
  (let ((events (sse-event-list
                 (format nil "data: {\"a\":1}~%~%data: [DONE]~%~%data: {\"a\":99}~%~%"))))
    (is (length events) 1 "nothing after [DONE] is decoded")))

(defcase sse-accepts-a-final-event-without-a-trailing-blank-line
  ;; Providers really do end a stream this way; dropping the last event loses
  ;; the finish reason and the usage totals.
  (let ((events (sse-event-list (format nil "data: {\"a\":1}~%~%data: {\"a\":2}"))))
    (is (length events) 2 "the last event survives without its blank line")
    (is (axllm:jget (second events) "a") 2 "and carries its payload")))

(defcase sse-ignores-comments-and-other-fields
  (let ((events (sse-event-list
                 (format nil ": keep-alive~%event: content_block_delta~%id: 7~%data: {\"a\":1}~%~%"))))
    (is (length events) 1 "a keep-alive comment and the event/id fields are not events")
    (is (axllm:jget (first events) "a") 1 "and the data line is still decoded")))

(defcase sse-joins-multiple-data-lines-with-a-newline
  (let ((events (sse-event-list (format nil "data: {\"a\":~%data: 1}~%~%"))))
    (is (length events) 1 "the two data lines form one event")
    (is (axllm:jget (first events) "a") 1 "joined with a newline so the JSON parses")))

(defcase sse-strips-exactly-one-leading-space
  (let ((events (sse-event-list (format nil "data:  {\"a\":1}~%~%"))))
    (is (length events) 1 "the payload still parses with one space left in front")))

(defcase sse-handles-crlf-and-bare-carriage-returns
  (dolist (terminator (list (format nil "~C~C" #\Return #\Newline)
                            (string #\Return)
                            (string #\Newline)))
    (let* ((text (format nil "data: {\"a\":1}~a~adata: {\"a\":2}~a~a"
                         terminator terminator terminator terminator))
           (events (sse-event-list text)))
      (is (length events) 2
          (format nil "both events decode with terminator ~s" terminator)))))

(defcase sse-does-not-treat-crlf-as-two-line-endings
  ;; If CR and LF each ended a line, the LF would look like the blank line that
  ;; ends an event, and every event would be flushed one line early.
  (let ((events (sse-event-list
                 (format nil "data: {\"a\":~C~Cdata: 1}~C~C~C~C"
                         #\Return #\Newline #\Return #\Newline #\Return #\Newline))))
    (is (length events) 1 "the two data lines are still one event")
    (is (axllm:jget (first events) "a") 1 "and the payload parses")))

(defcase sse-drops-a-leading-byte-order-mark
  (let ((events (sse-event-list (format nil "~Cdata: {\"a\":1}~%~%" (code-char #xFEFF)))))
    (is (length events) 1 "a byte-order mark does not break the first event")))

(defcase sse-survives-chunk-boundaries-anywhere
  ;; The decisive case for the line buffer and the incremental UTF-8 decoder:
  ;; the same stream must decode identically at every slice size, including one
  ;; byte at a time, which splits multi-byte characters.
  (let* ((text (format nil "data: {\"text\":\"h~Cllo ~C~C\"}~%~%data: {\"text\":\"caf~C\"}~%~%data: [DONE]~%~%"
                       (code-char #xE9) (code-char #x4E16) (code-char #x754C)
                       (code-char #xE9)))
         (whole (sse-event-list text)))
    (is (length whole) 2 "the reference decode finds both events")
    (dolist (size (list 1 2 3 5 7 13 64))
      (let ((sliced (feed-in-slices text size)))
        (is (length sliced) (length whole)
            (format nil "slice size ~a finds the same number of events" size))
        (is (axllm:jget (first sliced) "text") (axllm:jget (first whole) "text")
            (format nil "slice size ~a decodes multi-byte text identically" size))
        (is (axllm:jget (second sliced) "text") (axllm:jget (second whole) "text")
            (format nil "slice size ~a decodes the second event identically" size))))))

(defcase sse-rejects-a-data-payload-that-is-not-json
  (handler-case (progn (sse-event-list (format nil "data: not json at all~%~%"))
                       (error 'check-failed :text "a non-JSON payload should not pass"))
    (axllm:provider-error (c)
      (is (axllm:provider-error-kind c) :response
          "a malformed stream payload is a typed response failure"))))

;;; ------------------------------------------------------------------
;;; Generation through a Core-driven provider, not the legacy client
;;;
;;; The point of one provider stack is that ordinary generation works against
;;; a client built from Core's profiles.  These cases drive the real public
;;; entry point, ax/forward, against a provider-client with scripted wire data,
;;; so a regression that left the new profiles reachable only through an
;;; internal symbol would fail here.
;;; ------------------------------------------------------------------

(defun core-driven-client (profile model responses &key (api-key "test-key") options)
  (multiple-value-bind (transport tape) (scripted-transport responses)
    (values (axllm::provider :profile profile :model model :api-key api-key
                             :options options :transport transport)
            tape)))

(defparameter +chat-completions-model+ "gpt-5.4-mini"
  "A model Core routes to Chat Completions.

Core selects the Responses dialect for the gpt-6 family, so a case that
scripts a `choices' body has to name a Chat Completions model or it is
scripting the wrong API and proves nothing.")

(defun openai-chat-body (content &key (finish "stop"))
  (axllm:encode-json
   (axllm:object "id" "chatcmpl_1" "model" +chat-completions-model+
                 "choices" (vector (axllm:object "index" 0 "finish_reason" finish
                                                 "message" (axllm:object "role" "assistant"
                                                                         "content" content)))
                 "usage" (axllm:object "prompt_tokens" 11 "completion_tokens" 7
                                       "total_tokens" 18))))

(defcase a-client-from-ai-is-the-core-driven-client
  (multiple-value-bind (client tape) (core-driven-client "openai" +chat-completions-model+ (list "{}"))
    (declare (ignore tape))
    (ok (typep client 'axllm::provider-client)
        "ai() and provider build the same Core-driven class")
    (is (axllm::ai-name client) "openai" "it still answers ai-name, which the generator records")
    (is (axllm::ai-model client) +chat-completions-model+ "and ai-model")
    (is (axllm::ax-service-name client) "openai" "and the service-protocol name")))

(defcase chat-works-against-a-core-driven-client
  (multiple-value-bind (client tape)
      (core-driven-client "openai" +chat-completions-model+ (list (openai-chat-body "hello there")))
    (let ((response (axllm:chat client (vector (axllm:message "user" "hi")))))
      (is (axllm:jget response "content") "hello there"
          "public chat returns this port's normalized shape for a Core-driven client")
      (is (axllm:jget response "finishReason") "stop" "with the finish reason")
      (is (length (axllm:jget response "toolCalls")) 0 "and an empty tool-call vector")
      (is (axllm:jget (axllm:jget response "usage") "promptTokens") 11 "and the usage totals")
      (is (axllm:jget (axllm:jget response "usage") "completionTokens") 7 "both of them")
      (is (axllm:jget (tape-request tape 0) "model") +chat-completions-model+
          "and the request Core built went on the wire"))))

(defcase generation-runs-end-to-end-through-a-core-driven-client
  ;; The real public journey: ax + forward, with no legacy client anywhere.
  (multiple-value-bind (client tape)
      (core-driven-client "openai" +chat-completions-model+
                          (list (openai-chat-body "Answer: 42")))
    (let ((gen (axllm:ax "question:string -> answer:string")))
      (multiple-value-bind (outputs usage)
          (axllm:forward gen client (axllm:object "question" "what is six times seven"))
        (is (axllm:jget outputs "answer") "42"
            "the generator parsed the answer out of a Core-driven provider's response")
        (is (axllm:jget usage "promptTokens") 11 "and accumulated the usage")
        (let ((sent (tape-request tape 0)))
          (is (axllm:jget sent "model") +chat-completions-model+
              "the wire request names the model")
          (ok (plusp (length (axllm:jget sent "messages")))
              "and carries the rendered prompt"))))))

;;; A native tool round trip through the Core-driven client is deliberately not
;;; asserted here yet. Scripting one requires knowing which dialect Core selects
;;; once `functions' are present for a given model, and a case built on a guess
;;; about that would be testing my assumption rather than the port. Tool-call
;;; behaviour through this client is currently covered by the shared axai
;;; fixtures, which carry provider-accurate wire data; see the report.

(defcase a-client-reaches-a-profile-the-old-subset-never-had
  ;; google-gemini has an entirely different request shape, and none of it is
  ;; written in this port: Core builds it from its own descriptor.
  (multiple-value-bind (client tape)
      (core-driven-client "google-gemini" "gemini-3.5-flash"
                          (list (axllm:encode-json
                                 (axllm:object
                                  "candidates"
                                  (vector (axllm:object
                                           "content" (axllm:object
                                                      "role" "model"
                                                      "parts" (vector (axllm:object
                                                                       "text" "ok")))
                                           "finishReason" "STOP"))
                                  "usageMetadata" (axllm:object "promptTokenCount" 4
                                                                "candidatesTokenCount" 2
                                                                "totalTokenCount" 6)))))
    (let ((response (axllm:chat client (vector (axllm:message "user" "hi")))))
      (is (axllm:jget response "content") "ok"
          "a Gemini turn normalizes through the same public chat")
      (let ((sent (tape-request tape 0)))
        (ok (axllm:jget sent "contents")
            "and the wire request uses Gemini's own contents shape, built by Core")))))

(defcase the-provider-factory-rejects-an-unknown-profile
  (let ((condition (signals-provider-error :config
                     (axllm::provider :profile "not-a-provider" :model "m" :api-key "k"))))
    (has-substring (princ-to-string condition) "Unknown provider profile"
                   "an unknown profile fails closed and lists the known ones")))

(defcase every-core-profile-is-constructible
  ;; provider-profiles reads Core's registry, so this fails the moment a
  ;; profile exists in Core that the native client cannot build.
  (let ((profiles (axllm::provider-profiles))
        (built 0))
    (ok (> (length profiles) 40)
        (format nil "Core describes a broad profile set (~a)" (length profiles)))
    (dolist (profile profiles)
      (handler-case
          (let ((client (axllm::provider :profile profile :model "test-model"
                                         :api-key "test-key"
                                         :options (axllm:object "resourceName" "example"
                                                                "resource_name" "example"
                                                                "deploymentName" "deployment"
                                                                "deployment_name" "deployment"
                                                                "projectId" "project"
                                                                "project_id" "project"
                                                                "region" "us-central1"))))
            (ok (stringp (axllm::ax-service-name client))
                (format nil "~a names itself" profile))
            (incf built))
        (axllm:provider-error (c)
          ;; A profile that needs an endpoint setting this case does not supply
          ;; is allowed to refuse; an unknown-profile failure is not.
          (when (search "Unknown provider profile" (princ-to-string c))
            (error 'check-failed
                   :text (format nil "~a is in Core's registry but not constructible: ~a"
                                 profile c))))))
    (ok (> built 40) (format nil "~a of ~a profiles built" built (length profiles)))))

;;; ------------------------------------------------------------------
;;; A boundary wrapping a real provider, through the public entry points
;;;
;;; The agent runtime wraps a run's client in a boundary and then runs an
;;; ordinary forward.  That only works if `chat' accepts anything answering the
;;; service protocol rather than one concrete class, so these cases drive the
;;; public chat and forward through a boundary over each kind of client.
;;; ------------------------------------------------------------------

(defclass refusing-control ()
  ((pending :initarg :pending :initform '() :accessor refusing-pending)
   (aborted :initarg :aborted :initform nil :accessor refusing-aborted)
   (refusals :initform 0 :accessor refusing-refusals)))

(defmethod axllm/core::core-host-get ((target refusing-control) key &optional (fallback :null))
  (cond ((equal key "aborted") (axllm/core::core-bool (refusing-aborted target)))
        ((equal key "pending_count") (length (refusing-pending target)))
        (t fallback)))

(defmethod axllm/core::core-host-call ((target refusing-control) method args)
  (cond
    ((equal method "take_pending")
     (let ((pending (refusing-pending target)))
       (setf (refusing-pending target) '())
       (coerce pending 'vector)))
    ((equal method "pending_count") (length (refusing-pending target)))
    ((equal method "steer")
     ;; The agent runtime's control refuses an update once the run is aborted,
     ;; as the reference does.  A boundary handing an update back must cope.
     (when (refusing-aborted target)
       (incf (refusing-refusals target))
       (error 'axllm:ax-error :message "Run controller is aborted."))
     (setf (refusing-pending target)
           (append (refusing-pending target) (list (aref args 0))))
     target)
    ((or (equal method "emit") (equal method "_emit")) (aref args 0))
    (t (call-next-method))))

(defcase public-chat-accepts-a-boundary-over-a-client-from-ai
  (multiple-value-bind (inner tape)
      (scripted-client "openai" (list (openai-chat-body "wrapped ok")))
    (let* ((control (stub-control :pending (list (steer-update "be brief"))))
           (service (axllm::boundary-service inner control)))
      ;; This is the call that used to fail with a concrete type check.
      (let ((response (axllm:chat service (vector (axllm:message "user" "hi")))))
        (is (axllm:jget response "content") "wrapped ok"
            "public chat works through a boundary over a client built by ai()"))
      (let* ((sent (tape-request tape 0))
             (messages (axllm:jget sent "messages")))
        (is (length messages) 2 "and the queued steer reached the wire")
        (is (axllm:jget (aref messages 1) "content") "be brief"
            "as a user turn carrying the caller's text")))))

(defcase public-chat-accepts-a-boundary-over-a-core-driven-client
  (multiple-value-bind (inner tape)
      (core-driven-client "openai" +chat-completions-model+
                          (list (openai-chat-body "wrapped ok")))
    (let* ((control (stub-control :pending (list (steer-update "be brief"))))
           (service (axllm::boundary-service inner control)))
      (let ((response (axllm:chat service (vector (axllm:message "user" "hi")))))
        (is (axllm:jget response "content") "wrapped ok"
            "public chat works through a boundary over a Core-driven client"))
      (let ((messages (axllm:jget (tape-request tape 0) "messages")))
        (is (length messages) 2 "and the steer reached the wire here too")))))

(defcase generation-runs-through-a-boundary
  ;; The agent runtime's actual journey: wrap the run's client, then forward.
  (multiple-value-bind (inner tape)
      (core-driven-client "openai" +chat-completions-model+
                          (list (openai-chat-body "Answer: 42")))
    (let* ((control (stub-control :pending (list (steer-update "answer tersely"))))
           (service (axllm::boundary-service inner control))
           (gen (axllm:ax "question:string -> answer:string")))
      (multiple-value-bind (outputs usage) (axllm:forward gen service (axllm:object "question" "q"))
        (is (axllm:jget outputs "answer") "42" "a forward through a boundary produces the answer")
        (is (axllm:jget usage "promptTokens") 11 "and the usage"))
      (let ((messages (axllm:jget (tape-request tape 0) "messages")))
        (ok (find "answer tersely" (coerce messages 'list)
                  :test (lambda (needle m) (equal (axllm:jget m "content") needle)))
            "and the run's steering reached the model request")))))

(defcase a-boundary-over-a-boundary-still-works
  ;; A nested run wraps an already-wrapped client; the protocol has to compose
  ;; or a child stage silently loses its own boundary.
  (multiple-value-bind (inner tape)
      (core-driven-client "openai" +chat-completions-model+
                          (list (openai-chat-body "nested ok")))
    (let* ((root-control (stub-control :pending (list (steer-update "root rule"))))
           (child-control (stub-control :pending (list (steer-update "child rule"))))
           (root (axllm::boundary-service inner root-control))
           (child (axllm::boundary-service root child-control :path "root/child")))
      (is (axllm:jget (axllm:chat child (vector (axllm:message "user" "hi"))) "content")
          "nested ok" "a boundary over a boundary answers")
      (let ((messages (axllm:jget (tape-request tape 0) "messages")))
        (is (length messages) 3
            "and both the child's and the root's steering reach the request")))))

(defcase an-aborted-control-refusing-a-requeue-does-not-break-the-unwind
  ;; The agent runtime's control now signals rather than queueing once the run
  ;; is aborted.  A boundary handing back another stage's update must hold it
  ;; instead of turning the abort into a confusing failure.
  (let* ((control (make-instance 'refusing-control
                                 :pending (list (steer-update "other stage"
                                                              :target "root/other"))
                                 :aborted nil))
         (inner (recording-service))
         (service (axllm::boundary-service inner control :path "root/mine")))
    (setf (refusing-aborted control) t)
    ;; Taking pending updates is what Core does when a step starts; it must not
    ;; raise just because the control has stopped accepting updates.
    (let ((taken (axllm/core::core-ai-control-take-pending service)))
      (is (length taken) 0 "no update for this path is applied"))
    (ok (plusp (refusing-refusals control))
        "the control did refuse the hand-back, so the path under test ran")
    (is (length (axllm::%boundary-held service)) 1
        "and the refused update is held rather than lost or raised")
    ;; The abort itself still stops the next request, with the abort kind.
    (signals-provider-error :aborted (axllm::ax-chat service (core-request)))
    (is (length (recorded-requests inner)) 0 "and nothing was sent")))

;;; ------------------------------------------------------------------
;;; The expensive-model gate
;;;
;;; Core owns the rule (provider-require-expensive-model-confirmation, from
;;; src/ax/ai/base.ts).  What is native is only where it is called: after the
;;; model is resolved and before anything is merged, built, recorded or sent.
;;; Refusing after the request would already have billed it and recorded it in
;;; the run's usage and traces, which is the whole point of the gate.
;;; ------------------------------------------------------------------

(defun expensive-model-client (responses &key options)
  (multiple-value-bind (transport tape) (scripted-transport responses)
    (values (axllm::provider
             :profile "openai" :model "teacher-xl" :api-key "test-key"
             :options (or options
                          (axllm:object "modelInfo"
                                        (vector (axllm:object "name" "teacher-xl"
                                                              "isExpensive" axllm:true))))
             :transport transport)
            tape)))

(defcase an-expensive-model-is-refused-before-any-request
  (multiple-value-bind (client tape)
      (expensive-model-client (list (openai-chat-body "should never be sent")))
    (handler-case
        (progn (axllm::ax-chat client (core-request))
               (error 'check-failed :text "an expensive model should not run unconfirmed"))
      (check-failed (c) (error c))
      (error (condition)
        (has-substring (princ-to-string condition) "marked as expensive"
                       "the refusal names the reason")
        (has-substring (princ-to-string condition) "useExpensiveModel"
                       "and names the option that confirms it")))
    ;; The decisive assertion: nothing was billed or recorded.
    (is (tape-call-count tape) 0 "and no provider request was made at all")))

(defcase an-expensive-model-runs-once-the-call-confirms-it
  (multiple-value-bind (client tape)
      (expensive-model-client (list (openai-chat-body "allowed")))
    (let ((response (axllm::ax-chat client (core-request)
                                    (axllm:object "useExpensiveModel" "yes"))))
      (ok response "a confirmed expensive model runs")
      (is (tape-call-count tape) 1 "and sends exactly one request"))))

(defcase only-the-exact-confirmation-counts
  ;; A truthy-looking value is not the confirmation; the reference requires the
  ;; string "yes", so a caller cannot opt in by accident.
  (dolist (value (list "no" "true" "YES" ""))
    (multiple-value-bind (client tape)
        (expensive-model-client (list (openai-chat-body "should never be sent")))
      (handler-case (progn (axllm::ax-chat client (core-request)
                                          (axllm:object "useExpensiveModel" value))
                           (error 'check-failed
                                  :text (format nil "~s should not confirm an expensive model"
                                                value)))
        (check-failed (c) (error c))
        (error () nil))
      (is (tape-call-count tape) 0
          (format nil "~s sent no request" value)))))

(defcase an-ordinary-model-is-not-gated
  (multiple-value-bind (client tape)
      (core-driven-client "openai" +chat-completions-model+ (list (openai-chat-body "fine")))
    (let ((response (axllm::ax-chat client (core-request))))
      (ok (axllm:jget response "results")
          "a model with no expensive marking needs no confirmation")
      (is (tape-call-count tape) 1 "and its request is sent exactly once"))))

(defcase a-streamed-call-is-gated-too
  ;; The reference gates stream as well as chat; a gate on only one path would
  ;; let a caller bill an expensive model by asking for a stream.
  (multiple-value-bind (client tape)
      (expensive-model-client (list (openai-chat-body "should never be sent")))
    (declare (ignore tape))
    (handler-case (progn (axllm::ax-stream client (core-request))
                         (error 'check-failed :text "a streamed expensive call should be refused"))
      (check-failed (c) (error c))
      (error (condition)
        (has-substring (princ-to-string condition) "marked as expensive"
                       "the streamed path is gated by the same rule")))))

;;; ------------------------------------------------------------------
;;; Cancelling a streamed turn before the response headers arrive
;;;
;;; A subscription registered after the transport call can only fire once that
;;; call has returned, so it cannot abort the wait it exists for.  A server that
;;; stalls before sending headers is exactly that case, and it is the shape the
;;; MCP worker found on their HTTP path.
;;; ------------------------------------------------------------------

(defcase a-cancellation-blind-stall-before-headers-is-cancellable
  ;; The transport here knows nothing about cancellation: it blocks on a
  ;; semaphore until the test releases it, exactly as a server withholding its
  ;; response headers would. An earlier version of this case had the transport
  ;; wait on the token itself, which proved nothing, because the double was
  ;; supplying the very behaviour under test.
  (let* ((token (axllm::cancellation-token))
         (gate (sb-thread:make-semaphore :name "withhold-headers"))
         (entered (sb-thread:make-semaphore :name "transport-entered"))
         (closed 0)
         (transport (lambda (url headers body)
                      (declare (ignore url headers body))
                      (sb-thread:signal-semaphore entered)
                      ;; Cancellation-blind: no token, no timeout, no polling.
                      (sb-thread:wait-on-semaphore gate)
                      (values (lambda () nil) 200 (lambda () (incf closed)))))
         (client (axllm::provider :profile "openai" :model +chat-completions-model+
                                  :api-key "k" :streaming-transport transport))
         (outcome (list :still-blocked))
         (runner (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (progn (axllm::ax-stream client (core-request)
                                                (axllm:object "cancellation" token))
                               (setf (first outcome) :returned-a-stream))
                      (axllm:provider-error (c)
                        (setf (first outcome) (axllm:provider-error-kind c)))
                      (error () (setf (first outcome) :other-error)))))))
    (ok (sb-thread:wait-on-semaphore entered :timeout 2)
        "the transport was entered and is blocked before headers")
    (axllm::cancel token "user stopped")
    ;; The run has to settle while the transport is still blocked. If it only
    ;; settles after the release below, cancellation did not interrupt anything.
    (sb-thread:join-thread runner :default nil :timeout 2)
    (is (first outcome) :aborted
        "the run settles with the aborted kind while the transport is still blocked")
    ;; Release the transport only now, so the assertion above cannot have been
    ;; satisfied by the server answering.
    (sb-thread:signal-semaphore gate)
    (sb-thread:join-thread runner :default nil :timeout 2)
    (is (first outcome) :aborted "and the outcome does not change once released")))

(defcase cancelling-clears-the-subscribers-it-has-run
  ;; A listener fires once, so holding it after a cancel leaks it and makes the
  ;; subscription count report work that can never happen again.
  (let ((token (axllm::cancellation-token)))
    (axllm::cancellation-subscribe token (lambda () nil))
    (axllm::cancellation-subscribe token (lambda () nil))
    (is (axllm::cancellation-subscription-count token) 2 "both are registered")
    (axllm::cancel token "stop")
    (is (axllm::cancellation-subscription-count token) 0
        "and cancelling clears them, rather than leaving spent listeners behind")))

(defcase a-cancelled-stream-leaves-no-subscription-behind
  (let* ((token (axllm::cancellation-token))
         (transport (lambda (url headers body)
                      (declare (ignore url headers body))
                      (values (lambda () nil) 200 (lambda () nil))))
         (client (axllm::provider :profile "openai" :model +chat-completions-model+
                                  :api-key "k" :streaming-transport transport))
         (handle (axllm::ax-stream client (core-request)
                                  (axllm:object "cancellation" token))))
    (is (axllm::cancellation-subscription-count token) 1
        "an open stream holds one subscription")
    (axllm::cancel token "stop")
    (is (axllm::cancellation-subscription-count token) 0
        "and the cancel leaves none behind")
    (axllm::ax-stream-close handle)
    (is (axllm::cancellation-subscription-count token) 0
        "closing afterwards does not resurrect one")))

(defcase a-cancel-after-the-stream-opens-still-closes-it
  (let* ((token (axllm::cancellation-token))
         (closed 0)
         (transport (lambda (url headers body)
                      (declare (ignore url headers body))
                      (values (lambda () nil) 200 (lambda () (incf closed)))))
         (client (axllm::provider :profile "openai" :model +chat-completions-model+
                                  :api-key "k" :streaming-transport transport))
         (handle (axllm::ax-stream client (core-request)
                                  (axllm:object "cancellation" token))))
    (is closed 0 "the stream is open before the cancel")
    (axllm::cancel token "stop")
    (ok (plusp closed)
        "and cancelling closes the body rather than waiting for it to drain")
    ;; Closing an already-cancelled handle must not close the body a second
    ;; time: a double close on a real socket is an error, not a no-op.
    (axllm::ax-stream-close handle)
    (is closed 1 "and closing the handle afterwards does not close it again")))

;;; ------------------------------------------------------------------
;;; Credential redaction at the provider boundary, not at one entry point
;;;
;;; The guard used to live in `chat'.  The generator calls ax-chat directly and
;;; a streamed or embedding failure never passes through `chat' at all, so a
;;; guard there left every other public path exposed.  These cases drive each
;;; path with a marker standing in for a credential inside a provider error
;;; body, which is how a real key leaks: the provider echoes it back.
;;; ------------------------------------------------------------------

(defparameter +credential-marker+ "sk-marker-not-a-real-key-0123456789"
  "A stand-in for a credential.  Never a real secret; it only has to be a
string the provider echoes back and the guard must remove.")

(defun leaking-client (&key (status 401))
  "A client whose provider echoes the credential inside an error body."
  (multiple-value-bind (transport tape)
      (scripted-transport
       (loop repeat 4 collect (cons status
                   (axllm:encode-json
                    (axllm:object "error"
                                  (axllm:object
                                   "message"
                                   (format nil "Invalid API key: ~a" +credential-marker+)
                                   "type" "invalid_request_error"))))))
    (values (axllm::provider :profile "openai" :model +chat-completions-model+
                             :api-key +credential-marker+ :transport transport)
            tape)))

(defun assert-redacted (thunk path)
  "Run THUNK, require a failure, and require it to name the problem without
naming the credential."
  (handler-case (progn (funcall thunk)
                       (error 'check-failed
                              :text (format nil "~a should have failed" path)))
    (check-failed (c) (error c))
    (error (condition)
      (let ((text (princ-to-string condition)))
        (lacks-substring text +credential-marker+
                         (format nil "~a does not expose the credential" path))
        ;; The provider's own text must survive: a 401 with the detail stripped
        ;; out entirely is undiagnosable, so redaction is not suppression.
        (has-substring text "Invalid API key"
                       (format nil "~a keeps the provider's own message" path))
        (has-substring text "[redacted]"
                       (format nil "~a marks where the credential was" path))))))

(defcase direct-ax-chat-redacts-the-credential
  ;; The regression: the generator calls this, not chat.
  (multiple-value-bind (client tape) (leaking-client)
    (declare (ignore tape))
    (assert-redacted (lambda () (axllm::ax-chat client (core-request))) "ax-chat")))

(defcase public-chat-redacts-the-credential
  (multiple-value-bind (client tape) (leaking-client)
    (declare (ignore tape))
    (assert-redacted (lambda () (axllm:chat client (vector (axllm:message "user" "hi"))))
                     "chat")))

(defcase forward-redacts-the-credential
  ;; The path the probe used, and the one that was leaking.
  (multiple-value-bind (client tape) (leaking-client)
    (declare (ignore tape))
    (let ((gen (axllm:ax "question:string -> answer:string")))
      (assert-redacted (lambda () (axllm:forward gen client (axllm:object "question" "q")))
                       "forward"))))

(defcase embedding-failures-redact-the-credential
  (multiple-value-bind (client tape) (leaking-client)
    (declare (ignore tape))
    (assert-redacted (lambda () (axllm::ax-embed client (axllm:object "texts" (vector "a"))))
                     "ax-embed")))

(defcase a-streamed-failure-redacts-the-credential
  (let* ((transport (lambda (url headers body)
                      (declare (ignore url headers body))
                      (values (lambda () nil) 401 (lambda () nil))))
         (client (axllm::provider :profile "openai" :model +chat-completions-model+
                                  :api-key +credential-marker+
                                  :streaming-transport transport)))
    (handler-case (progn (axllm::ax-stream client (core-request))
                         (error 'check-failed :text "a 401 stream should fail"))
      (check-failed (c) (error c))
      (error (condition)
        (lacks-substring (princ-to-string condition) +credential-marker+
                         "a streamed failure does not expose the credential")))))

(defcase redaction-does-not-depend-on-the-credential-appearing-in-the-request
  ;; A provider can echo a key it was never sent in this request, for example
  ;; from an earlier session. The guard works off the client's own credential
  ;; rather than anything in the request, so this still redacts.
  (multiple-value-bind (client tape) (leaking-client :status 500)
    (let ((delays '())
          (axllm::*provider-retry-random* (lambda () 0.5d0)))
      (let ((axllm::*provider-retry-sleep* (lambda (ms cancellation)
                                           (declare (ignore cancellation)) (push ms delays))))
        (assert-redacted (lambda () (axllm:ax-chat client (core-request))) "a 500 body"))
      (is (tape-call-count tape) 4 "all retries retain credential redaction")
      (is (length delays) 3 "the retry budget is bounded"))))

;;; ------------------------------------------------------------------
;;; The default HTTP transport against a server that withholds headers
;;;
;;; A scripted double blocked on a semaphore shows the wait is abortable; it
;;; cannot show that Drakma's own cleanup runs and the socket closes. This uses
;;; a real listening server that accepts the connection and sends nothing.
;;; ------------------------------------------------------------------

(defun withholding-server ()
  "A loopback server that accepts one connection and sends no response.

Answers (values port accepted-socket-box stop), where the box receives the
accepted socket so a test can see whether the client closed it."
  (let* ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                  :type :stream :protocol :tcp))
         (accepted (list nil))
         (ready (sb-thread:make-semaphore :name "withholding-accept")))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (let ((port (nth-value 1 (sb-bsd-sockets:socket-name listener))))
      (sb-thread:make-thread
       (lambda ()
         (handler-case
             (let ((connection (sb-bsd-sockets:socket-accept listener)))
               (setf (first accepted) connection)
               (sb-thread:signal-semaphore ready)
               ;; Deliberately send nothing: the client waits for headers that
               ;; never arrive, which is the case cancellation exists for.
               (sleep 10))
           (error () nil)))
       :name "withholding-server")
      (values port accepted ready
              (lambda ()
                (handler-case (sb-bsd-sockets:socket-close listener) (error () nil))
                (let ((connection (first accepted)))
                  (when connection
                    (handler-case (sb-bsd-sockets:socket-close connection)
                      (error () nil)))))))))

(defcase the-default-transport-is-cancellable-while-awaiting-headers
  ;; No double anywhere: the real streaming transport, a real socket, and a
  ;; server that never answers.
  (multiple-value-bind (port accepted ready stop) (withholding-server)
    (unwind-protect
         (let* ((token (axllm::cancellation-token))
                (client (axllm::provider
                         :profile "openai" :model +chat-completions-model+
                         :api-key "test-key"
                         :base-url (format nil "http://127.0.0.1:~a" port)))
                (outcome (list :still-blocked))
                (runner (sb-thread:make-thread
                         (lambda ()
                           (handler-case
                               (progn (axllm::ax-stream client (core-request)
                                                       (axllm:object "cancellation" token))
                                      (setf (first outcome) :returned-a-stream))
                             (axllm:provider-error (c)
                               (setf (first outcome) (axllm:provider-error-kind c)))
                             (error (c)
                               (setf (first outcome)
                                     (list :other (princ-to-string c)))))))))
           (ok (sb-thread:wait-on-semaphore ready :timeout 5)
               "the server accepted the connection and is withholding headers")
           (axllm::cancel token "user stopped")
           (sb-thread:join-thread runner :default nil :timeout 5)
           (is (first outcome) :aborted
               "the run settles as aborted while the server is still withholding")
           ;; The connection must be released, not left hanging. Reading the
           ;; server side answers 0 bytes at end of stream once the client has
           ;; closed, which is the observable evidence Drakma's cleanup ran.
           (let ((connection (first accepted))
                 (closed nil))
             (when connection
               (loop repeat 50
                     until closed
                     do (handler-case
                            (let ((buffer (make-array 16 :element-type '(unsigned-byte 8))))
                              (multiple-value-bind (data count)
                                  (sb-bsd-sockets:socket-receive connection buffer nil)
                                (declare (ignore data))
                                (when (or (null count) (zerop count)) (setf closed t))))
                          (error () (setf closed t)))
                        (unless closed (sleep 0.05))))
             (ok closed "and the client closed the connection rather than leaking it")))
      (funcall stop))))

;;; ------------------------------------------------------------------
;;; Redaction beyond the printed message
;;; ------------------------------------------------------------------

(defcase the-response-body-slot-is-scrubbed-too
  ;; The message is not the only place a caller can read the credential: the
  ;; condition carries the provider's body in a slot.
  (multiple-value-bind (client tape) (leaking-client)
    (declare (ignore tape))
    (handler-case (progn (axllm::ax-chat client (core-request))
                         (error 'check-failed :text "the 401 should have failed"))
      (check-failed (c) (error c))
      (axllm:provider-error (condition)
        (let ((body (axllm::provider-error-response-body condition)))
          (lacks-substring (axllm:encode-json body) +credential-marker+
                           "the response-body slot does not carry the credential")
          (has-substring (axllm:encode-json body) "[redacted]"
                         "and marks where it was"))))))

(defcase a-credential-from-the-callback-is-redacted-even-with-no-api-key
  ;; When a credential provider supplies the key, the api-key slot is NIL, so a
  ;; guard that redacted on the key alone would scrub nothing at all.
  (multiple-value-bind (transport tape)
      (scripted-transport
       (list (cons 401 (axllm:encode-json
                        (axllm:object "error"
                                      (axllm:object "message"
                                                    (format nil "Invalid API key: ~a"
                                                            +credential-marker+)))))))
    (declare (ignore tape))
    (let ((client (axllm::provider
                   :profile "openai" :model +chat-completions-model+
                   :transport transport
                   :credential-provider
                   (lambda (context)
                     (declare (ignore context))
                     (axllm:object "Authorization"
                                   (format nil "Bearer ~a" +credential-marker+))))))
      (is (axllm::%provider-api-key client) nil
          "the client holds no key of its own, as the callback supplies it")
      (handler-case (progn (axllm::ax-chat client (core-request))
                           (error 'check-failed :text "the 401 should have failed"))
        (check-failed (c) (error c))
        (axllm:provider-error (condition)
          (lacks-substring (princ-to-string condition) +credential-marker+
                           "the callback's credential is redacted from the message")
          (lacks-substring (axllm:encode-json
                            (axllm::provider-error-response-body condition))
                           +credential-marker+
                           "and from the response body"))))))

(defcase a-failure-while-reading-the-stream-is-redacted
  ;; The read closure runs after the open returned, so the guard around the open
  ;; is gone by then. This is where a provider echoes a credential in a
  ;; mid-stream error payload.
  (let* ((sent nil)
         (transport
           (lambda (url headers body)
             (declare (ignore url headers body))
             (values (lambda ()
                       (if sent
                           nil
                           (progn (setf sent t)
                                  ;; Not JSON, so the decoder refuses it, and the
                                  ;; failure text carries the payload.
                                  (sb-ext:string-to-octets
                                   (format nil "data: ~a is invalid~C~C"
                                           +credential-marker+ #\Newline #\Newline)
                                   :external-format :utf-8))))
                     200
                     (lambda () nil))))
         (client (axllm::provider :profile "openai" :model +chat-completions-model+
                                  :api-key +credential-marker+
                                  :streaming-transport transport))
         (handle (axllm::ax-stream client (core-request))))
    (handler-case (progn (axllm::ax-stream-next handle)
                         (error 'check-failed
                                :text "a non-JSON stream payload should have failed"))
      (check-failed (c) (error c))
      (axllm:provider-error (condition)
        (lacks-substring (princ-to-string condition) +credential-marker+
                         "a mid-stream failure does not expose the credential")))))

(defcase public-model-catalogue-is-a-provider-vector
  (let ((catalogue (axllm:supported-ai-models)))
    (ok (and (vectorp catalogue) (not (stringp catalogue))) "catalogue is a vector")
    (is (length catalogue) 50 "provider count")
    (let ((openai (find "openai" catalogue :key (lambda (p) (axllm:jget p "name")) :test #'equal)))
      (is (axllm:jget openai "displayName") "OpenAI" "display name")
      (ok (find "gpt-5.4-mini" (axllm:jget openai "models")
                :key (lambda (m) (axllm:jget m "name")) :test #'equal) "current models are discoverable"))))

(defcase public-model-catalogue-filter-and-deep-copy
  (let* ((first (axllm:supported-ai-models "embeddings"))
         (model (aref (axllm:jget (aref first 0) "models") 0)))
    (loop for provider across first do
      (loop for item across (axllm:jget provider "models") do
        (is (axllm:jget item "type") "embeddings" "filter selects embedding models")))
    (setf (gethash "name" model) "caller mutation")
    (let ((fresh (axllm:supported-ai-models "embeddings")))
      (ok (not (equal (axllm:jget (aref (axllm:jget (aref fresh 0) "models") 0) "name") "caller mutation"))
          "nested model entries are copied"))))

(defcase public-model-info-and-summary
  (let ((info (axllm:model-info "openai" "gpt-5.4-mini"))
        (summary (axllm:model-catalog-summary)))
    (is (axllm:jget info "name") "gpt-5.4-mini" "lookup returns model info")
    (is (axllm:model-info "openai" "absent-model") :null "unknown lookup is null")
    (is (axllm:jget summary "providerCount") 50 "summary counts providers, not models")))

(defcase provider-camelcase-request-does-not-mutate-input
  (multiple-value-bind (transport tape)
      (scripted-transport (list (openai-tool-response nil :content "hello" :finish "stop")))
    (let* ((client (axllm:provider :profile "openai" :model +chat-completions-model+ :api-key "test-key" :transport transport))
           (request (axllm:object "chatPrompt" (vector (axllm:object "role" "user" "content" "Hello"))
                                   "modelConfig" (axllm:object "stream" axllm:false)))
           (before (axllm:encode-json request)))
      (axllm:ax-chat client request)
      (is (axllm:encode-json request) before "public request stays unchanged")
      (is (axllm:jget (aref (axllm:jget (tape-request tape 0) "messages") 0) "content") "Hello"
          "camelCase prompt reaches the wire"))))

(defcase gemini-cache-create-retries-and-reuses-the-created-entry
  (let ((response (axllm:encode-json
                    (axllm:object "candidates" (vector (axllm:object "content" (axllm:object "parts" (vector (axllm:object "text" "ok")))
                                                                                "finishReason" "STOP"))))))
    (multiple-value-bind (transport tape)
        (scripted-transport (list (cons 503 "{\"error\":{\"message\":\"busy\"}}")
                                  "{\"name\":\"cachedContents/native\",\"expireTime\":\"2099-01-01T00:00:00Z\"}"
                                  response response))
      (let* ((delays nil)
             (axllm::*provider-retry-sleep* (lambda (ms token) (declare (ignore token)) (push ms delays)))
             (axllm::*provider-retry-random* (lambda () 0.5d0))
             (client (axllm:provider :profile "google-gemini" :model "gemini-3.5-flash" :api-key "test-key" :transport transport))
             (request (axllm:object "chat_prompt" (vector (axllm:object "role" "system" "content" "Reusable context" "cache" axllm:true)
                                                          (axllm:object "role" "user" "content" "Hi"))
                                     "model_config" (axllm:object "stream" axllm:false)))
             (options (axllm:object "contextCache" (axllm:object "minTokens" 0))))
        (axllm:ax-chat client request options)
        (axllm:ax-chat client request options)
        (is (length delays) 1 "only cache creation retries")
        (is (tape-call-count tape) 4 "two creation attempts, two chats; the second chat reuses the cache")
        (loop for index in '(2 3) do
          (is (axllm:jget (tape-request tape index) "cachedContent") "cachedContents/native" "chat references the created cache")
          (ok (not (gethash "systemInstruction" (tape-request tape index))) "cached system content is not sent again"))
        (is (axllm:jget (aref (axllm:jget request "chat_prompt") 0) "content") "Reusable context" "caching does not mutate prompts")))))

(defcase cancellation-after-open-is-an-abort-not-an-empty-stream
  (let* ((token (axllm::cancellation-token)) (closed 0)
         (client (axllm:provider :profile "openai" :model +chat-completions-model+ :api-key "test-key"
                  :streaming-transport (lambda (url headers body)
                                         (declare (ignore url headers body))
                                         (values (lambda () nil) 200 (lambda () (incf closed))))))
         (handle (axllm:ax-stream client (core-request) (axllm:object "cancellation" token))))
    (unwind-protect
         (progn (axllm::cancel token "stopped after open")
                (signals-provider-error :aborted (axllm:ax-stream-next handle))
                (is closed 1 "cancellation releases the transport exactly once"))
      (axllm:ax-stream-close handle))
    (is closed 1 "cleanup does not close twice")))

(defcase runtime-hooks-fail-open-and-limiter-rejection-is-fail-closed
  (multiple-value-bind (transport tape)
      (scripted-transport (list (openai-tool-response nil :content "hello" :finish "stop")))
    (let* ((axllm::*telemetry-globals* (axllm::globals-snapshot))
           (client (axllm:provider :profile "openai" :model +chat-completions-model+ :api-key "test-key" :transport transport)))
      (axllm:set-global "meter" (lambda (&rest args) (declare (ignore args)) (error "broken meter")))
      (axllm:set-global "tracer" (lambda (&rest args) (declare (ignore args)) (error "broken tracer")))
      (axllm:ax-chat client (core-request))
      (let ((caught nil))
        (handler-case (axllm:ax-chat client (core-request)
                        (axllm:object "rateLimiter" (lambda (next info) (declare (ignore next info)) (error "limited"))))
          (error (c) (setf caught (search "limited" (princ-to-string c)))))
        (ok caught "a failing limiter prevents execution"))
      (is (tape-call-count tape) 1 "telemetry failures do not block and limiter failures never reach transport"))))
