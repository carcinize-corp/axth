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
   #:f #:s #:signature-from-spec
   ;; Templates
   #:render-template-content #:collect-template-variable-names
   #:validate-prompt-template-syntax
   #:render-prompt #:prompt-template #:render-prompt-template
   #:prompt-template-instruction
   #:validate-fields #:validate-output #:validate-value #:strip-internal
   ;; Native logging and string utilities
   #:format-chat-message #:create-default-color-logger #:create-default-text-logger
   #:create-default-optimizer-color-logger #:create-default-optimizer-text-logger
   #:*default-logger* #:*default-optimizer-logger*
   #:trim-non-alpha-num #:split-into-two #:dedup
   #:extract-id-and-text #:extract-index-prefixed-text #:batch-array
   ;; Providers, generation and tools
   #:ai #:chat #:ax #:forward #:tool #:message
   #:program-streaming-forward #:program-signature
   #:program-tools #:program-set-tools #:program-function-call-traces
   #:program-set-function-call-traces #:program-clear-function-call-traces
   #:program-chat-log #:program-usage #:program-traces #:program-set-instruction
   #:program-optimizable-components #:program-apply-optimized-components
   #:memory #:memory-add-request #:memory-add-response #:memory-add-function-results
   #:memory-history #:memory-add-tag #:memory-rewind-to-tag #:memory-remove-by-tag
   #:tool-result-text #:encode-json-pretty #:tool-call-problems
   #:generator-memory #:generator-function-call-traces
   #:ai-name #:ai-model #:ai-base-url #:ai-timeout #:ai-max-tokens #:ai-transport
   #:tool-request-spec #:tool-handler #:tool-index #:invoke-tool
   #:validate-tool-arguments #:validate-schema-support #:usage-object
   #:function-processor #:make-function-processor #:function-processor-resolve
   #:execute-function #:execute-function-with-details #:function-call-error
   #:make-default-transport
   #:json-true-p #:json-false-p #:json-boolean-p #:json-boolean
   #:provider #:provider-profiles #:provider-profile #:provider-name
   #:provider-model #:provider-base-url #:provider-descriptor-of
   #:supported-ai-models #:model-catalog-summary #:model-info
   ;; Service protocol and cancellation
   #:ax-chat #:ax-stream #:ax-embed #:ax-transcribe #:ax-speak #:ax-complete
   #:ax-stream-next #:ax-stream-close #:ax-stream-handle #:make-ax-stream-handle
   #:ax-features #:ax-service-name #:ax-id #:ax-metrics #:ax-options
   #:ax-estimated-cost #:ax-owned-worker-factory
   #:ax-take-control-updates #:ax-pending-control-count
   #:boundary-service #:boundary-close
   #:cancellation-token #:cancel #:cancelled-p #:cancellation-reason
   #:cancellation-wait #:throw-if-cancelled #:cancellation-subscribe
   #:cancellation-subscription-count
   #:rate-limiter-token-usage #:rate-limiter-acquire #:rate-limiter-available
   ;; Agents and native runtime sessions
   #:agent #:ax-agent #:agent-forward #:agent-streaming-forward #:agent-test
   #:agent-execute-actor-step #:agent-inspect-runtime #:agent-export-session-state
   #:agent-restore-session-state #:agent-close-runtime-session #:agent-state #:agent-set-state
   #:agent-chat-log #:agent-action-log #:agent-trace #:agent-replay-trace #:agent-usage
   #:agent-runtime-contract #:agent-policy #:agent-policy-registry #:agent-callable-inventory
   #:agent-discovery-catalog #:agent-discover #:agent-recall #:agent-used #:agent-invoke-callable
   #:agent-export-runtime-state #:agent-restore-runtime-state #:agent-set-signature #:agent-add-child
   #:agent-instruction #:agent-set-instruction #:agent-add-actor-instruction
   #:agent-optimizer-metadata #:agent-optimizable-components #:agent-apply-optimized-components
   #:agent-clarification-error #:agent-clarification #:agent-clarification-state #:agent-clarification-payload
   #:code-runtime #:code-session #:runtime-executable-p #:runtime-language #:runtime-usage-instructions
   #:runtime-create-session #:runtime-supports-callables-p #:runtime-register-callable #:runtime-shutdown
   #:session-execute #:session-inspect-globals #:session-snapshot-globals #:session-patch-globals
   #:session-export-state #:session-restore-state #:session-close #:session-closed-p
   #:runtime-capabilities #:process-runtime #:make-process-runtime
   #:runtime-protocol-error #:runtime-protocol-error-category
   #:envelope-result #:envelope-error #:envelope-session-closed #:envelope-timeout #:envelope-final
   #:envelope-ask-clarification #:envelope-discover #:envelope-recall #:envelope-used
   #:envelope-status #:envelope-guide-agent
   #:docker-session #:make-docker-session #:make-docker-transport #:docker-pull-image
   #:docker-list-containers #:docker-create-container #:docker-find-or-create-container
   #:docker-connect-to-container #:docker-start-container #:docker-container-logs
   #:docker-execute-command #:docker-stop-containers #:docker-session-tool
   #:context-metrics-collector #:make-context-metrics-collector #:context-metrics-observe
   #:context-metrics-handler #:context-metrics-summary
   ;; Flow programs
   #:flow #:flow-p #:flow-state #:flow-error #:flow-callable #:flow-callable-function
   #:flow-step #:flow-execute #:flow-derive #:flow-map #:flow-branch #:flow-while
   #:flow-feedback #:flow-parallel #:flow-parallel-merge #:flow-node-extended #:flow-nx
   #:flow-returns #:flow-set-demos #:flow-plan #:flow-traces #:flow-chat-log #:flow-usage
   #:flow-components #:flow-apply-components #:flow-mermaid #:flow-streaming-forward
   #:flow-cancellation #:flow-cancel #:flow-cancelled-p
   ;; Candidate selection, refinement, synthetic examples and evaluation
   #:best-of-n #:refine #:refine-error #:refine-error-attempts #:program-attempts
   #:program-native-sample-capable-p #:attempt-number #:attempt-round #:attempt-sample-index
   #:attempt-strategy #:attempt-input #:attempt-prediction #:attempt-reward
   #:attempt-met-threshold #:attempt-traces #:attempt-chat-log #:attempt-usage #:attempt-error
   #:attempt-advice #:attempt-advice-applied #:attempt-json #:best-of-n-count #:refine-rounds
   #:refine-samples-per-round #:refine-threshold #:refine-program #:*refine-feedback-generator-factory*
   #:synth #:synth-generate #:synth-signature #:synth-teacher #:synth-diversity #:synth-domain
   #:synth-edge-cases #:synth-temperature #:synth-model #:*synth-generator-factory*
   #:test-prompt #:run-test-prompt #:test-prompt-client #:test-prompt-program
   #:test-prompt-examples #:test-prompt-debug
   ;; Protocol chat and bounded UCP schema validation
   #:mcp-chat #:execution-context-resolve-context-prompt
   #:ucp-schema-validator #:make-ucp-schema-validator #:ucp-schema-validate
   #:ucp-schema-clear-cache #:ucp-schema-validation-callback
   #:ucp-schema-error #:ucp-schema-validation-error
   #:ucp-schema-error-instance-path #:ucp-schema-error-schema-path
   ;; Optional axllm/jiti system
   #:make-jiti-proposer #:jiti-action-error
   ;; Conditions
   #:provider-error #:provider-error-kind #:provider-error-provider #:provider-error-status
   #:provider-error-code #:provider-error-response-body #:provider-error-request
   #:provider-error-retryable-p #:provider-error-cause
   #:provider-error-aborted-p #:provider-error-infrastructure-p #:provider-error-refusal-p
   #:ax-generate-error #:ax-generate-error-cause
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
