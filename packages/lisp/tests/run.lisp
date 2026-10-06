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
(dolist (dependency '("yason" "cl-ppcre" "drakma" "sb-bsd-sockets" "sb-posix"))
  (asdf:load-system dependency))

(let ((here (uiop:pathname-directory-pathname *load-truename*))
      (uiop:*compile-file-warnings-behaviour* :error))
  (push (uiop:pathname-parent-directory-pathname here) asdf:*central-registry*)
  (asdf:test-system "axllm")
  (uiop:quit 0))
