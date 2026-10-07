;;;; run.lisp --- run the conformance suites and exit with their verdict.
;;;;
;;;; Usage, from packages/lisp:
;;;;   sbcl --script tests/run.lisp
;;;;
;;;; Exits 0 only when every fixture passed, so CI does not need to read
;;;; the output to tell success from failure. Set AXIR_CONFORMANCE_DIR to
;;;; point at a different ir/conformance tree.
;;;;
;;;; Any warning raised while Ax is compiled is a failure too, including
;;;; undefined functions and style warnings. Loading compiled macros can
;;;; legitimately redefine their compile-time definitions in SBCL.

(require :asdf)

;; Dependency compilation may legitimately redefine its own macros. Keep that
;; outside the warning gate; every warning in Ax itself still fails this run.
(dolist (dependency '("websocket-driver-client" "yason" "cl-ppcre" "drakma" "cl-base64" "cffi" "puri"
                     "ironclad" "local-time" "sqlite" "sb-bsd-sockets" "sb-posix" "usocket"))
  (asdf:load-system dependency))

(let ((here (uiop:pathname-directory-pathname *load-truename*))
      (uiop:*compile-file-warnings-behaviour* :error))
  (push (uiop:pathname-parent-directory-pathname here) asdf:*central-registry*)
  ;; Cached FASLs can still warn when their declarations conflict at load time.
  ;; Compilation's warning policy alone does not catch those integration errors.
  (handler-bind ((warning (lambda (condition)
                            (unless (typep condition 'sb-kernel:redefinition-with-defmacro)
                              (error "Ax load warning: ~A" condition)))))
    ;; Check current sources, not a cached FASL from an earlier integration.
    (asdf:load-system "axllm/tests" :force '("axllm" "axllm/jiti" "axllm/tests")))
  (let ((undefined nil))
    (do-external-symbols (symbol (find-package "AXLLM"))
      (unless (or (fboundp symbol) (boundp symbol) (find-class symbol nil)
                  (fboundp (list 'setf symbol)))
        (push (symbol-name symbol) undefined)))
    (when undefined
      (error "Ax exported names without definitions: ~{~A~^, ~}" (sort undefined #'string<))))
  (asdf:test-system "axllm")
  (uiop:quit 0))
