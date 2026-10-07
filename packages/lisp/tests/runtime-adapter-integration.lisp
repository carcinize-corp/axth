;;;; Exercise the production JavaScript adapter, not the protocol test worker.
;;;; Run from the repository root after npm ci:
;;;;   sbcl --script packages/lisp/tests/runtime-adapter-integration.lisp
(require :asdf)
(asdf:load-asd (merge-pathnames "../axllm.asd" *load-truename*))
(asdf:load-system "axllm")

(let* ((root (truename (merge-pathnames "../../../" (uiop:pathname-directory-pathname *load-truename*))))
       (runtime (ax:make-process-runtime
                 (list "node" "--import=tsx"
                       (namestring (merge-pathnames "tools/axir/adapters/axjs-runtime-server.ts" root)))
                 :language "JavaScript" :timeout 20))
       (calls 0))
  (unwind-protect
       (progn
         (assert (ax:runtime-supports-callables-p runtime))
         (ax:runtime-register-callable
          runtime "crm.lookup"
          (lambda (params)
            (incf calls)
            (assert (equal "c-7" (ax:jget params "id")))
            (ax:object "tier" "gold")))
         (ax:runtime-register-callable
          runtime "llmQuery"
          (lambda (params)
            (incf calls)
            (assert (and (vectorp params) (not (stringp params)) (= 2 (length params))))
            (assert (equal "first" (ax:jget (aref params 0) "query")))
            (assert (equal "second" (ax:jget (aref params 1) "query")))
            (vector "one" "two")))
         (ax:runtime-register-callable
          runtime "breaks"
          (lambda (params) (declare (ignore params)) (error "host refused this call")))
         (let ((session (ax:runtime-create-session runtime (ax:object) (ax:object))))
           (flet ((check-final (code expected)
                    (let ((result (ax:session-execute session code (ax:object))))
                      (assert (equal "final" (ax:jget result "type")) ()
                              "Expected final, got ~A" (ax:encode-json result))
                      (assert (equal expected (ax:jget (aref (ax:jget result "args") 0) "answer"))))))
             (check-final "const r = await crm.lookup({id:'c-7'}); await final({answer:r.tier})" "gold")
             (check-final "const q = await llmQuery([{query:'first'}, {query:'second'}]); await final({answer:q.join('/')})" "one/two")
             (check-final "try { await breaks({}); } catch(e) { return await final({answer:e.message}); }" "host refused this call")
             (assert (= calls 2))
             (ax::runtime-retire-callables runtime)
             (let ((result (ax:session-execute session "await crm.lookup({id:'c-7'})" (ax:object))))
               (assert (eq ax:true (ax:jget result "is_error")))
               (assert (search "no host callable named crm.lookup is registered" (ax:jget result "error"))))
             (assert (= calls 2))))
         (format t "Production JavaScript adapter: object and array callbacks, host errors, retired callables: PASS~%"))
    (ax:runtime-shutdown runtime)))
