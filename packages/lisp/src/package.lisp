;;;; package.lisp --- packages for the Ax Common Lisp port.
;;;;
;;;; Two packages, with one rule between them:
;;;;
;;;;   axllm       the public API. Every exported symbol is a plain Lisp name
;;;;               that does not exist in COMMON-LISP, so a caller can
;;;;               (:use #:cl #:axllm) without a single shadowing form.
;;;;
;;;;   axllm/core  the internal Core layer: generated code from ir/axcore
;;;;               (src/core.lisp) plus the native boundaries it calls
;;;;               (src/core-runtime.lisp). Nothing here is exported, and
;;;;               the public API is the only intended way in.

(defpackage #:axllm
  (:nicknames #:ax)
  (:use #:cl)
  (:documentation
   "Ax for Common Lisp: signatures and JSON Schema.

JSON value model, shared by every Ax Lisp surface:

  object   STRING-keyed EQUAL hash table, key order preserved
  array    vector (adjustable, with a fill pointer, when Ax built it)
  string   string
  number   integer or float
  boolean  YASON:TRUE / YASON:FALSE
  null     :NULL

NIL is not a JSON value in this model; it is neither false nor null.

The booleans are symbols, so a bare YASON:TRUE in code is an unbound
variable rather than the value. Write AXLLM:TRUE and AXLLM:FALSE, the
constants bound to those symbols, or quote them: \'YASON:TRUE. The null
value, :NULL, is a keyword and needs no quoting.")
  (:export
   ;; JSON values
   #:true
   #:false
   #:object
   #:jget
   #:parse-json
   #:encode-json
   ;; Signatures
   #:parse-signature
   #:signature-string
   #:signature-fields
   #:json-schema
   ;; Providers, generation and tools
   #:ai #:chat #:ax #:forward #:tool #:message
   #:ai-name #:ai-model #:ai-base-url #:ai-timeout #:ai-max-tokens #:ai-transport
   #:tool-request-spec #:tool-handler #:tool-index #:invoke-tool
   #:validate-tool-arguments #:validate-schema-support #:usage-object
   #:make-default-transport
   #:json-true-p #:json-false-p #:json-boolean-p #:json-boolean
   ;; Optional axllm/jiti system
   #:make-jiti-proposer #:jiti-action-error
   ;; Conditions
   #:provider-error #:provider-error-kind #:provider-error-provider #:provider-error-status
   #:generation-error #:generation-error-kind #:generation-error-problems
   #:tool-error
   #:ax-error
   #:ax-error-message
   #:signature-error
   #:validation-error))

(defpackage #:axllm/core
  (:use #:cl)
  (:documentation
   "Internal Core layer for AXLLM.

src/core.lisp is generated from ir/axcore by tools/axir/cmd/lisp-core and
holds Ax's portable semantics. src/core-runtime.lisp implements the native
boundaries that generated code calls. Neither is a public API: load AXLLM
and call its exported functions."))
