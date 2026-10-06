;;;; axllm.asd --- Ax for Common Lisp (experimental subset)
;;;;
;;;; This system is the native Common Lisp port of Ax. It currently covers
;;;; the signature and JSON-schema surface: signature parsing, validation,
;;;; rendering, field introspection and JSON Schema generation, all driven
;;;; by Core code generated from ir/axcore by tools/axir/cmd/lisp-core.
;;;;
;;;; src/core.lisp is generated. Regenerate it from tools/axir with
;;;;   go run ./cmd/lisp-core --out ../../packages/lisp/src/core.lisp
;;;; and check freshness with
;;;;   go run ./cmd/lisp-core --check
;;;;
(defsystem "axllm"
  :description "Native Ax signatures, providers, typed generation and tools for SBCL"
  :author "Ax contributors"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("yason" "cl-ppcre" "drakma" "uiop")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "json")
                             (:file "core-runtime")
                             (:file "core")
                             (:file "signature")
                             (:file "ai")
                             (:file "tools")
                             (:file "gen"))))
  :in-order-to ((test-op (test-op "axllm/tests"))))

(defsystem "axllm/jiti"
  :description "Optional Ax proposer callback for Jiti; no Jiti dependency"
  :license "Apache-2.0"
  :depends-on ("axllm")
  :components ((:file "src/jiti")))

(defsystem "axllm/tests"
  :description "Shared Core fixtures and native provider, generation, tool and adapter tests"
  :license "Apache-2.0"
  :depends-on ("axllm/jiti" "sb-bsd-sockets" "sb-posix")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "signature")
                             (:file "json")
                             (:file "provider")
                             (:file "jiti"))))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (unless (uiop:symbol-call :axllm/tests :run-all-tests)
                      (error "Ax signature/schema conformance failed."))
                    (uiop:symbol-call :axllm/tests :run-json-tests)
                    (multiple-value-bind (passed failed)
                        (uiop:symbol-call :axllm :run-provider-tests)
                      (declare (ignore passed))
                      (unless (zerop failed) (error "Ax provider tests failed.")))
                    (uiop:symbol-call :cl-user :run-jiti-tests)))
