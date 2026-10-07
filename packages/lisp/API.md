# axllm API reference

Common Lisp package for Ax, loaded as the ASDF system `axllm`.
Every form below is evaluated in a package that uses `axllm`; the examples qualify each name with the `ax` nickname.

JSON values are shared by every surface: objects are string-keyed `equal` hash tables with their key order preserved, arrays are vectors, booleans are `ax:true` and `ax:false`, and null is `:null`. `nil` is not a JSON value.

```lisp
(require :asdf)
(asdf:load-system "axllm")
```

## Signatures

Parse, render and introspect Ax signatures, and build JSON Schema from them. Signature semantics come from generated Core; this package chooses the Lisp names.

### `s`

- Qualified: `ax:s`
- Kind: function
- Canonical Ax symbol: `s`
- Form: `(ax:s &key inputs outputs description)`
- Options: `:inputs`, `:outputs`, `:description`
- Returns: a signature record (a JSON object Core owns)
- Defined in: `src/builder.lisp`

Build a validated signature from field specs built with f, as TypeScript's s() with a field map. Signature text goes through parse-signature instead.

```lisp
(ax:s :inputs (ax:object "question" (ax:f "string")) :outputs (ax:object "answer" (ax:f "string")))
```

### `parse-signature`

- Qualified: `ax:parse-signature`
- Kind: function
- Canonical Ax symbol: `parse_signature`
- Form: `(ax:parse-signature text)`
- Returns: a validated signature record
- Defined in: `src/signature.lisp`

Parse and validate signature text.

```lisp
(ax:parse-signature "review:string -> sentiment:class \"positive, negative\"")
```

### `f`

- Qualified: `ax:f`
- Kind: function
- Canonical Ax symbol: `f`
- Form: `(ax:f type &key description fields options array array-description optional internal cache min max email url pattern pattern-description value-descriptions format language)`
- Returns: a field-type record
- Defined in: `src/builder.lisp`

Build a field type for a signature built from specs.

```lisp
(ax:f "number")
```

### `signature-string`

- Qualified: `ax:signature-string`
- Kind: function
- Canonical Ax symbol: `signature_to_string`
- Form: `(ax:signature-string signature)`
- Returns: a string that parses to an equal signature
- Defined in: `src/signature.lisp`

Render a signature back to signature text.

```lisp
(ax:signature-string (ax:parse-signature "question:string -> answer:string"))
```

### `signature-fields`

- Qualified: `ax:signature-fields`
- Kind: function
- Canonical Ax symbol: `signature_fields`
- Form: `(ax:signature-fields signature &key side)`
- Options: `:side :input`, `:side :output`
- Returns: a JSON array of field objects
- Defined in: `src/signature.lisp`

The signature's fields on one side, in Ax's published camelCase shape.

```lisp
(ax:signature-fields (ax:parse-signature "question:string -> answer:string") :side :output)
```

### `json-schema`

- Qualified: `ax:json-schema`
- Kind: function
- Canonical Ax symbol: `to_json_schema`
- Form: `(ax:json-schema signature &key side title strict flexible-json-as-string options)`
- Options: `:side`, `:title`, `:strict`, `:flexible-json-as-string`
- Returns: a JSON Schema object
- Defined in: `src/signature.lisp`

A JSON Schema for the signature's fields on one side.

```lisp
(ax:json-schema (ax:parse-signature "question:string -> answer:string") :side :output :strict t)
```

### `render-prompt`

- Qualified: `ax:render-prompt`
- Kind: function
- Canonical Ax symbol: `render_prompt`
- Form: `(ax:render-prompt signature values &key functions options)`
- Returns: a JSON array of chat messages
- Defined in: `src/prompt.lisp`

Render the system and user messages Core builds for a signature and its input values.

```lisp
(ax:render-prompt (ax:parse-signature "question:string -> answer:string") (ax:object "question" "why?"))
```

## AxGen

Typed generation over a signature: prompt rendering, output parsing, correction turns, bounded tool rounds, and the program hooks an optimizer reads and rewrites.

### `ax`

- Qualified: `ax:ax`
- Kind: function
- Canonical Ax symbol: `ax`
- Form: `(ax:ax signature &key description tools max-steps max-retries id instruction options)`
- Options: `:description`, `:tools`, `:max-steps`, `:max-retries`, `:id`, `:instruction`
- Returns: a generator
- Defined in: `src/gen.lisp`

Create a generator for a signature.

```lisp
(ax:ax "question:string -> answer:string" :max-retries 1)
```

### `forward`

- Qualified: `ax:forward`
- Kind: generic function
- Canonical Ax symbol: `AxGen.forward`
- Form: `(ax:forward program client inputs &optional options)`
- Options: `"maxSteps"`, `"maxRetries"`, `"freshMemory"`, `"model"`
- Returns: two values: the typed outputs object and this call's usage object
- Defined in: `src/gen.lisp`

Run a program against a client. Options are a JSON object of per-call settings; a method ignores keys it does not implement.

```lisp
(ax:forward (ax:ax "question:string -> answer:string") client (ax:object "question" "why?"))
```

### `program-streaming-forward`

- Qualified: `ax:program-streaming-forward`
- Kind: generic function
- Canonical Ax symbol: `AxGen.streamingForward`
- Form: `(ax:program-streaming-forward program client inputs &optional options)`
- Returns: two values: the outputs object and the usage object
- Defined in: `src/gen.lisp`

Run a program with a streaming sink. A program that cannot be driven by a prefix refuses this call rather than inheriting a silent fallback to forward.

```lisp
(ax:program-streaming-forward program client inputs (ax:object "sink" sink))
```

### `program-usage`

- Qualified: `ax:program-usage`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.getUsage`
- Form: `(ax:program-usage program)`
- Returns: a JSON array of usage objects
- Defined in: `src/gen.lisp`

The program's token usage so far, per ai and model.

```lisp
(ax:program-usage program)
```

### `program-traces`

- Qualified: `ax:program-traces`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.getTraces`
- Form: `(ax:program-traces program)`
- Returns: a JSON array of trace objects
- Defined in: `src/gen.lisp`

The program's completed runs, each with status, input, output, chat log and function calls.

```lisp
(ax:program-traces program)
```

### `program-chat-log`

- Qualified: `ax:program-chat-log`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.getChatLog`
- Form: `(ax:program-chat-log program)`
- Returns: a JSON array of chat-log objects
- Defined in: `src/gen.lisp`

The provider turns the program recorded, oldest first.

```lisp
(ax:program-chat-log program)
```

### `program-set-instruction`

- Qualified: `ax:program-set-instruction`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.setInstruction`
- Form: `(ax:program-set-instruction program text)`
- Returns: the program
- Defined in: `src/gen.lisp`

Replace the program's prompt instruction text.

```lisp
(ax:program-set-instruction program "Answer in one sentence.")
```

### `program-optimizable-components`

- Qualified: `ax:program-optimizable-components`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.getOptimizableComponents`
- Form: `(ax:program-optimizable-components program)`
- Returns: a JSON array of component objects
- Defined in: `src/gen.lisp`

The parts of the program an optimizer may rewrite, each with an id, owner, kind, current value and constraints.

```lisp
(ax:program-optimizable-components program)
```

### `program-apply-optimized-components`

- Qualified: `ax:program-apply-optimized-components`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.applyOptimizedComponents`
- Form: `(ax:program-apply-optimized-components program component-map)`
- Returns: the program
- Defined in: `src/gen.lisp`

Apply a component id to text map. An id the program does not own is ignored, so one map can be applied to a whole composition.

```lisp
(ax:program-apply-optimized-components program (ax:object "root::instruction" "Be terse."))
```

### `program-signature`

- Qualified: `ax:program-signature`
- Kind: generic function
- Canonical Ax symbol: `AxProgram.getSignature`
- Form: `(ax:program-signature program)`
- Returns: a signature record or :null
- Defined in: `src/gen.lisp`

The program's parsed signature, or :null when it declares none.

```lisp
(ax:program-signature program)
```

### `best-of-n`

- Qualified: `ax:best-of-n`
- Kind: function
- Canonical Ax symbol: `bestOfN`
- Form: `(ax:best-of-n program &key n reward-fn threshold fail-count model-config strategy on-attempt)`
- Options: `:n`, `:reward-fn`, `:threshold`, `:fail-count`, `:strategy`, `:on-attempt`
- Returns: a program that scores candidates
- Defined in: `src/refine.lisp`

Score several candidates of a program with a reward function and return the best.

```lisp
(ax:best-of-n program :n 3 :reward-fn reward)
```

### `refine`

- Qualified: `ax:refine`
- Kind: function
- Canonical Ax symbol: `refine`
- Form: `(ax:refine program &key rounds samples-per-round reward-fn threshold fail-count model-config strategy feedback-client feedback-model-config reward-description program-description on-attempt)`
- Options: `:rounds`, `:samples-per-round`, `:reward-fn`, `:threshold`, `:feedback-client`
- Returns: a program that refines over rounds
- Defined in: `src/refine.lisp`

Refine a program over reward-scored rounds, appending advice to its instruction components and restoring them afterwards.

```lisp
(ax:refine program :rounds 2 :reward-fn reward)
```

### `synth`

- Qualified: `ax:synth`
- Kind: function
- Canonical Ax symbol: `synth`
- Form: `(ax:synth signature &key teacher diversity domain edge-cases temperature model)`
- Options: `:teacher`, `:domain`, `:edge-cases`, `:temperature`, `:model`
- Returns: a synthesizer
- Defined in: `src/synth.lisp`

Generate synthetic labelled examples for a signature with a teacher client.

```lisp
(ax:synth "question:string -> answer:string" :teacher teacher :domain "support")
```

### `test-prompt`

- Qualified: `ax:test-prompt`
- Kind: class
- Canonical Ax symbol: `AxTestPrompt`
- Form: `ax:test-prompt`
- Options: `:client`, `:program`, `:examples`, `:debug`
- Returns: a test prompt
- Defined in: `src/evaluate.lisp`

Score a program over labelled examples with a metric function.

```lisp
(ax:test-prompt :client client :program program :examples examples)
```

## AxAI

Provider clients and the service protocol every client answers, plus cancellation, usage and rate-limit boundaries.

### `ai`

- Qualified: `ax:ai`
- Kind: function
- Canonical Ax symbol: `ai`
- Form: `(ax:ai &key name model api-key base-url transport timeout max-tokens)`
- Options: `:name`, `:model`, `:api-key`, `:base-url`, `:transport`, `:timeout`, `:max-tokens`
- Returns: an AI client
- Defined in: `src/provider.lisp`

Create a provider client. The provider is selected by name rather than by a per-provider class. Omit :model to use the profile's default from Core's descriptor; a profile without a default requires :model.

```lisp
(ax:ai :name "openai" :model "gpt-6-luna")
```

### `ax-chat`

- Qualified: `ax:ax-chat`
- Kind: generic function
- Canonical Ax symbol: `AxAIService.chat`
- Form: `(ax:ax-chat service request &optional options)`
- Returns: a normalized chat response object
- Defined in: `src/ai.lisp`

The service protocol's chat call, which every client answers.

```lisp
(ax:ax-chat client request (ax:object))
```

### `ax-stream`

- Qualified: `ax:ax-stream`
- Kind: generic function
- Canonical Ax symbol: `AxAIService.stream`
- Form: `(ax:ax-stream service request &optional options)`
- Returns: a stream handle
- Defined in: `src/ai.lisp`

The service protocol's streaming chat call.

```lisp
(ax:ax-stream client request (ax:object))
```

### `ax-embed`

- Qualified: `ax:ax-embed`
- Kind: generic function
- Canonical Ax symbol: `AxAIService.embed`
- Form: `(ax:ax-embed service request &optional options)`
- Returns: a normalized embeddings response object
- Defined in: `src/ai.lisp`

The service protocol's embeddings call.

```lisp
(ax:ax-embed client request (ax:object))
```

### `ax-features`

- Qualified: `ax:ax-features`
- Kind: generic function
- Canonical Ax symbol: `AxAIService.getFeatures`
- Form: `(ax:ax-features service &optional model)`
- Returns: a features object
- Defined in: `src/ai.lisp`

What the client supports for a model: functions, streaming, media and more.

```lisp
(ax:ax-features client "gpt-6-luna")
```

### `chat`

- Qualified: `ax:chat`
- Kind: function
- Canonical Ax symbol: `chat`
- Form: `(ax:chat client messages &key tools tool-choice model)`
- Options: `:tools`, `:tool-choice`, `:model`
- Returns: a response object with content, toolCalls and usage
- Defined in: `src/ai.lisp`

One normalized chat request against a client, as the generator issues it.

```lisp
(ax:chat client messages :tool-choice :auto)
```

### `supported-ai-models`

- Qualified: `ax:supported-ai-models`
- Kind: function
- Canonical Ax symbol: `get_supported_ai_models`
- Form: `(ax:supported-ai-models &optional model-type)`
- Options: `model-type`
- Returns: a JSON array of provider entries with their model catalogues
- Defined in: `src/provider.lisp`

Core's provider-model catalogue, optionally narrowed by model type, such as "code" or "embeddings". The filter is best-effort: types are trimmed and lowercased, and an omitted, blank or unknown type returns the whole catalogue. These are catalogue entries, not provider profile ids or a live provider inventory.

```lisp
(ax:supported-ai-models "code")
```

### `model-catalog-summary`

- Qualified: `ax:model-catalog-summary`
- Kind: function
- Canonical Ax symbol: `model_catalog_summary`
- Form: `(ax:model-catalog-summary)`
- Returns: a JSON object describing catalogue coverage
- Defined in: `src/provider.lisp`

Core's catalogue audit: its version, descriptor-covered provider ids and deferred provider ids. This is coverage metadata, not a model list; supported-ai-models returns the catalogue.

```lisp
(ax:model-catalog-summary)
```

### `model-info`

- Qualified: `ax:model-info`
- Kind: function
- Canonical Ax symbol: `model_info`
- Form: `(ax:model-info profile model)`
- Returns: the model's catalogue entry, or :null
- Defined in: `src/provider.lisp`

Look up a model's catalogue entry under a provider profile. This is the metadata the expensive-model gate reads; a model absent from the catalogue returns :null.

```lisp
(ax:model-info "openai" "gpt-6-luna")
```

### `provider-profiles`

- Qualified: `ax:provider-profiles`
- Kind: function
- Canonical Ax symbol: `provider_profiles`
- Form: `(ax:provider-profiles)`
- Returns: a Lisp list of profile id strings
- Defined in: `src/provider.lisp`

Every provider profile Core knows, sorted, read from Core's registry. These are profile ids, not models; supported-ai-models returns the separate provider-model catalogue.

```lisp
(ax:provider-profiles)
```

### `rate-limiter-token-usage`

- Qualified: `ax:rate-limiter-token-usage`
- Kind: class
- Canonical Ax symbol: `AxRateLimitInfo`
- Form: `ax:rate-limiter-token-usage`
- Returns: a rate-limiter token usage object
- Defined in: `src/ai.lisp`

The token usage a rate limiter is given for a call, with the remaining budget read back through rate-limiter-available.

```lisp
(ax:rate-limiter-available limiter)
```

### `ax-metrics`

- Qualified: `ax:ax-metrics`
- Kind: generic function
- Canonical Ax symbol: `AxAIService.getMetrics`
- Form: `(ax:ax-metrics service)`
- Returns: a metrics object
- Defined in: `src/ai.lisp`

A service's latency and error metrics, read from the client. This is not a host telemetry meter.

```lisp
(ax:ax-metrics client)
```

### `provider`

- Qualified: `ax:provider`
- Kind: function
- Canonical Ax symbol: `provider`
- Form: `(ax:provider &key profile name model embed-model api-key base-url api-version options model-config transport streaming-transport timeout credential-provider)`
- Options: `:profile`, `:model`, `:api-key`, `:base-url`, `:transport`, `:credential-provider`
- Returns: a provider client
- Defined in: `src/provider.lisp`

Create a provider client for a profile. Omit :model to use the profile's default from Core's descriptor; a profile without a default requires :model. :credential-provider takes a per-client function that receives a JSON object of profile, operation, method and url and returns a header object (a hash table). A non-function is a configuration error; a non-object result is an authentication error. The ai factory does not accept this callback keyword.

```lisp
(ax:provider :profile "openai" :model "gpt-6-luna" :credential-provider handler)
```

### `globals-snapshot`

- Qualified: `ax:globals-snapshot`
- Kind: function
- Canonical Ax symbol: `AxGlobals`
- Form: `(ax:globals-snapshot)`
- Returns: a JSON object of the current globals
- Defined in: `src/telemetry.lisp`

An isolated snapshot of the process-wide Ax globals. The names are fixed and camelCase: signatureStrict, tracer, meter, rateLimiter, logger, optimizerLogger, debug, abortSignal, customLabels, onUsage, cachingFunction and functionResultFormatter. This is runtime context, not a serializable export.

```lisp
(ax:globals-snapshot)
```

### `set-global`

- Qualified: `ax:set-global`
- Kind: function
- Canonical Ax symbol: `set_tracer`
- Form: `(ax:set-global name value)`
- Returns: the value that was set
- Defined in: `src/telemetry.lisp`

Install a process-wide global by its camelCase name, such as "tracer". There is no setter per global, and a name the globals do not have is rejected rather than stored.

```lisp
(ax:set-global "tracer" tracer)
```

### `update-globals`

- Qualified: `ax:update-globals`
- Kind: function
- Canonical Ax symbol: `set_meter`
- Form: `(ax:update-globals config)`
- Returns: a snapshot of the globals after the update
- Defined in: `src/telemetry.lisp`

Apply several globals at once, such as "meter" and "rateLimiter". Every name is validated before anything is applied, so a misspelling changes nothing.

```lisp
(ax:update-globals (ax:object "meter" meter))
```

### `reset-globals`

- Qualified: `ax:reset-globals`
- Kind: function
- Canonical Ax symbol: `set_rate_limiter`
- Form: `(ax:reset-globals)`
- Returns: a snapshot of the restored globals
- Defined in: `src/telemetry.lisp`

Globals are process state, so a test or a host that installed a tracer, meter or rate limiter can put the defaults back. A limiter itself is installed with set-global under "rateLimiter".

```lisp
(ax:reset-globals)
```

### `start-active-span-fail-open`

- Qualified: `ax:start-active-span-fail-open`
- Kind: function
- Canonical Ax symbol: `start_active_span`
- Form: `(ax:start-active-span-fail-open tracer name options parent-context operation)`
- Returns: the operation's values
- Defined in: `src/telemetry.lisp`

Run an operation inside a span from an explicit tracer. It fails open: a tracer that signals, or :null instead of a tracer, still runs the operation exactly once and preserves its values and its original error. No span is ended on the caller's behalf.

```lisp
(ax:start-active-span-fail-open tracer "ax.gen" (ax:object) :null handler)
```

### `runtime-hook-frame`

- Qualified: `ax:runtime-hook-frame`
- Kind: struct
- Canonical Ax symbol: `AxRuntimeHooks`
- Form: `(ax:make-runtime-hook-frame &key globals resolved)`
- Returns: a runtime hook frame
- Defined in: `src/telemetry.lisp`

One call's resolved globals, carried in that call's options under a symbol key so JSON, cache and export see string keys only and a concurrent call cannot read another call's frame.

```lisp
(ax:options-with-runtime-hook-frame options (ax:make-runtime-hook-frame))
```

### `provider-descriptor-of`

- Qualified: `ax:provider-descriptor-of`
- Kind: accessor
- Canonical Ax symbol: `AxProviderDescriptor`
- Form: `ax:provider-descriptor-of`
- Returns: a provider descriptor record
- Defined in: `src/provider.lisp`

The provider descriptor behind a client, which is how provider mapping stays Core-owned.

```lisp
(ax:provider-name (ax:provider-descriptor-of client))
```

### `ax-service-name`

- Qualified: `ax:ax-service-name`
- Kind: generic function
- Canonical Ax symbol: `AxAIService.getName`
- Form: `(ax:ax-service-name service)`
- Returns: a string
- Defined in: `src/ai.lisp`

The service name a client reports, which is how a caller tells providers apart without a per-provider class.

```lisp
(ax:ax-service-name client)
```

### `cancellation-token`

- Qualified: `ax:cancellation-token`
- Kind: class
- Canonical Ax symbol: `AxCancellationToken`
- Form: `(ax:make-cancellation-token)`
- Returns: a cancellation token
- Defined in: `src/ai.lisp`

A cancellation token a caller passes into a call and cancels from another thread.

```lisp
(ax:cancel (make-instance 'ax:cancellation-token) "user stopped")
```

### `provider-error-aborted-p`

- Qualified: `ax:provider-error-aborted-p`
- Kind: function
- Canonical Ax symbol: `AxAIServiceAbortedError`
- Form: `(ax:provider-error-aborted-p condition)`
- Returns: a generalized boolean
- Defined in: `src/ai.lisp`

Whether a signalled provider error is a cancellation. The Lisp port reports an aborted call as a kind on the single provider-error condition rather than a separate class.

```lisp
(ax:provider-error-aborted-p condition)
```

### `rate-limiter-acquire`

- Qualified: `ax:rate-limiter-acquire`
- Kind: function
- Canonical Ax symbol: `AxRateLimiter`
- Form: `(ax:rate-limiter-acquire limiter tokens &key cancellation)`
- Returns: nil once the call may proceed
- Defined in: `src/ai.lisp`

The rate-limiter protocol a host implements: acquire before a call, with the token usage and remaining budget read back.

```lisp
(ax:rate-limiter-acquire limiter (ax:usage-object 10 2))
```

### `usage-object`

- Qualified: `ax:usage-object`
- Kind: function
- Canonical Ax symbol: `AxUsage`
- Form: `(ax:usage-object prompt completion &optional total)`
- Returns: a usage object with promptTokens, completionTokens and totalTokens
- Defined in: `src/ai.lisp`

Build the usage object every response and program carries.

```lisp
(ax:usage-object 11 7)
```

## Agents And RLM

Agents, their actor steps and action logs, and the host code-runtime boundary a runtime language plugs into.

### `agent`

- Qualified: `ax:agent`
- Kind: function
- Canonical Ax symbol: `agent`
- Form: `(ax:agent signature &key options)`
- Options: `:options`
- Returns: an agent
- Defined in: `src/agent.lisp`

Create an agent for a signature.

```lisp
(ax:agent "question:string -> answer:string")
```

### `agent-forward`

- Qualified: `ax:agent-forward`
- Kind: function
- Canonical Ax symbol: `AxAgent.forward`
- Form: `(ax:agent-forward agent client values &key options)`
- Returns: two values: the outputs object and the usage object
- Defined in: `src/agent.lisp`

Run the agent's staged pipeline against a client.

```lisp
(ax:agent-forward agent client (ax:object "question" "why?"))
```

### `agent-streaming-forward`

- Qualified: `ax:agent-streaming-forward`
- Kind: function
- Canonical Ax symbol: `AxAgent.streaming_forward`
- Form: `(ax:agent-streaming-forward agent client values sink &key options)`
- Returns: two values: the outputs object and the usage object
- Defined in: `src/agent.lisp`

Run the agent with a streaming sink.

```lisp
(ax:agent-streaming-forward agent client inputs (ax:object "sink" sink))
```

### `agent-add-child`

- Qualified: `ax:agent-add-child`
- Kind: function
- Canonical Ax symbol: `AxAgent.add_child_agent`
- Form: `(ax:agent-add-child agent namespace name child)`
- Returns: the agent
- Defined in: `src/agent.lisp`

Add a child agent, which becomes a namespaced callable in the actor's inventory.

```lisp
(ax:agent-add-child parent "research" "summarize" child)
```

### `agent-action-log`

- Qualified: `ax:agent-action-log`
- Kind: function
- Canonical Ax symbol: `AxAgent.getActionLog`
- Form: `(ax:agent-action-log agent)`
- Returns: a JSON array of action records
- Defined in: `src/agent.lisp`

The agent's action log, in the order Core wrote the records.

```lisp
(ax:agent-action-log agent)
```

### `agent-discover`

- Qualified: `ax:agent-discover`
- Kind: function
- Canonical Ax symbol: `AxAgent.discover`
- Form: `(ax:agent-discover agent request)`
- Returns: a discovery payload object
- Defined in: `src/agent.lisp`

The effect-only discovery call that loads full docs for a callable.

```lisp
(ax:agent-discover agent (ax:object "callables" (vector "tools.search")))
```

### `code-runtime`

- Qualified: `ax:code-runtime`
- Kind: class
- Canonical Ax symbol: `AxCodeRuntime`
- Form: `ax:code-runtime`
- Returns: a code runtime
- Defined in: `src/agent-runtime.lisp`

The host code-runtime boundary: a runtime language, its usage instructions and the sessions it creates.

```lisp
(ax:runtime-language runtime)
```

### `code-session`

- Qualified: `ax:code-session`
- Kind: class
- Canonical Ax symbol: `AxCodeSession`
- Form: `ax:code-session`
- Returns: a code session
- Defined in: `src/agent-runtime.lisp`

One runtime session: execute an actor step, inspect or patch globals, export and restore state, and close.

```lisp
(ax:session-execute session code (ax:object))
```

### `agent-set-state`

- Qualified: `ax:agent-set-state`
- Kind: function
- Canonical Ax symbol: `AxAgent.setState`
- Form: `(ax:agent-set-state agent state)`
- Returns: the agent
- Defined in: `src/agent.lisp`

Replace the agent's minimal state, as a state round trip restores it.

```lisp
(ax:agent-set-state agent (ax:object "notes" "none"))
```

## Flow

Composable program graphs: steps, branches, loops, parallel merges, traces and Mermaid rendering.

### `flow`

- Qualified: `ax:flow`
- Kind: class
- Canonical Ax symbol: `flow`
- Form: `ax:flow`
- Returns: a flow
- Defined in: `src/flow.lisp`

Create a flow program graph.

```lisp
(ax:flow)
```

### `flow-node-extended`

- Qualified: `ax:flow-node-extended`
- Kind: function
- Canonical Ax symbol: `AxFlow.node`
- Form: `(ax:flow-node-extended flow name base-signature &key extended-signature options)`
- Returns: the flow
- Defined in: `src/flow.lisp`

Declare a node in the graph, with its program and signature.

```lisp
(ax:flow-node-extended graph "qa" "question:string -> answer:string")
```

### `flow-execute`

- Qualified: `ax:flow-execute`
- Kind: function
- Canonical Ax symbol: `AxFlow.execute`
- Form: `(ax:flow-execute flow name program &optional options)`
- Returns: the flow
- Defined in: `src/flow.lisp`

Execute a node with state mapped into its inputs.

```lisp
(ax:flow-execute graph "qa" program)
```

### `flow-branch`

- Qualified: `ax:flow-branch`
- Kind: function
- Canonical Ax symbol: `AxFlow.branch`
- Form: `(ax:flow-branch flow name predicate branches &optional options)`
- Returns: the flow
- Defined in: `src/flow.lisp`

Branch the graph on a predicate over the state.

```lisp
(ax:flow-branch graph "route" predicate branches)
```

### `flow-parallel`

- Qualified: `ax:flow-parallel`
- Kind: function
- Canonical Ax symbol: `AxFlow.parallel`
- Form: `(ax:flow-parallel flow name results &optional options)`
- Returns: the flow
- Defined in: `src/flow.lisp`

Run independent branches and merge their reports in plan order.

```lisp
(ax:flow-parallel graph "fanout" branches)
```

### `flow-returns`

- Qualified: `ax:flow-returns`
- Kind: function
- Canonical Ax symbol: `AxFlow.returns`
- Form: `(ax:flow-returns flow returns)`
- Returns: the flow
- Defined in: `src/flow.lisp`

Map the final state to the flow's outputs.

```lisp
(ax:flow-returns graph mapper)
```

### `flow-mermaid`

- Qualified: `ax:flow-mermaid`
- Kind: function
- Canonical Ax symbol: `AxFlow.mermaid`
- Form: `(ax:flow-mermaid flow &optional options)`
- Returns: a Mermaid diagram string
- Defined in: `src/flow.lisp`

Render the graph as Mermaid, for documentation and review.

```lisp
(ax:flow-mermaid graph)
```

### `flow-usage`

- Qualified: `ax:flow-usage`
- Kind: function
- Canonical Ax symbol: `AxFlow.getUsage`
- Form: `(ax:flow-usage flow)`
- Returns: a JSON array of usage objects
- Defined in: `src/flow.lisp`

The flow's merged token usage.

```lisp
(ax:flow-usage graph)
```

## Tools

Tool definitions, argument validation and bounded execution, including the function processor generated code resolves names through.

### `tool`

- Qualified: `ax:tool`
- Kind: function
- Canonical Ax symbol: `fn`
- Form: `(ax:tool &key name description parameters handler)`
- Options: `:name`, `:description`, `:parameters`, `:handler`
- Returns: a tool spec
- Defined in: `src/tools.lisp`

Define a tool from a name, a JSON Schema for its arguments and a handler.

```lisp
(ax:tool :name "lookup" :description "Look up a key" :handler handler)
```

### `tool-request-spec`

- Qualified: `ax:tool-request-spec`
- Kind: function
- Canonical Ax symbol: `AxFunctionJSONSchema`
- Form: `(ax:tool-request-spec spec)`
- Returns: a JSON object with name, description and parameters
- Defined in: `src/tools.lisp`

The wire spec for a tool, as a provider request carries it.

```lisp
(ax:tool-request-spec spec)
```

### `function-processor`

- Qualified: `ax:function-processor`
- Kind: class
- Canonical Ax symbol: `AxFunctionProcessor`
- Form: `(ax:make-function-processor tools)`
- Returns: a function processor
- Defined in: `src/tools.lisp`

The processor generated code resolves tool names through, so a renamed tool stays callable.

```lisp
(ax:function-processor-resolve processor "lookup")
```

### `validate-tool-arguments`

- Qualified: `ax:validate-tool-arguments`
- Kind: function
- Canonical Ax symbol: `validateJSONSchema`
- Form: `(ax:validate-tool-arguments spec arguments)`
- Returns: two values: the validated arguments and a list of problems
- Defined in: `src/tools.lisp`

Validate a tool call's arguments against its schema before the handler runs.

```lisp
(ax:validate-tool-arguments spec arguments)
```

### `function-call-error`

- Qualified: `ax:function-call-error`
- Kind: condition
- Canonical Ax symbol: `AxFunctionError`
- Form: `ax:function-call-error`
- Returns: a condition
- Defined in: `src/tools.lisp`

A tool call that could not be executed, with the problems that stopped it.

```lisp
(ax:execute-function spec arguments)
```

## MCP

MCP clients, transports, tasks, the Apps host bridge, and the OAuth boundary.

### `mcp-client`

- Qualified: `ax:mcp-client`
- Kind: class
- Canonical Ax symbol: `AxMCPClient`
- Form: `(ax:make-mcp-client transport &rest options)`
- Returns: an MCP client
- Defined in: `src/mcp.lisp`

An MCP client over a transport: catalogs, tools, prompts, resources, tasks and subscriptions.

```lisp
(ax:make-mcp-client transport (ax:object "era" "modern"))
```

### `mcp-init`

- Qualified: `ax:mcp-init`
- Kind: function
- Canonical Ax symbol: `AxMCPClient.init`
- Form: `(ax:mcp-init client)`
- Returns: the client
- Defined in: `src/mcp.lisp`

Initialize the session: negotiate the protocol version and era and load the catalogs.

```lisp
(ax:mcp-init client)
```

### `mcp-call-tool`

- Qualified: `ax:mcp-call-tool`
- Kind: function
- Canonical Ax symbol: `AxMCPClient.callTool`
- Form: `(ax:mcp-call-tool client name arguments &key context task-handling)`
- Returns: the tool result object
- Defined in: `src/mcp.lisp`

A tool call waits for a modern server's task by default, can expose it as its CreateTaskResult, or can return the call's outcome, as TypeScript's callTool taskHandling and callToolOutcome. A legacy server's task-shaped result is a complete result.

```lisp
(ax:mcp-call-tool client "lookup" (ax:object "key" "a"))
```

### `mcp-stdio-transport`

- Qualified: `ax:mcp-stdio-transport`
- Kind: class
- Canonical Ax symbol: `AxMCPStdioTransport`
- Form: `(ax:make-mcp-stdio-transport command &key arguments environment directory)`
- Returns: a transport
- Defined in: `src/mcp-transport-stdio.lisp`

The stdio transport, with Ax's line framing.

```lisp
(ax:make-mcp-stdio-transport "server" :arguments (list "--stdio"))
```

### `mcp-streamable-http-transport`

- Qualified: `ax:mcp-streamable-http-transport`
- Kind: class
- Canonical Ax symbol: `AxMCPStreambleHTTPTransport`
- Form: `(ax:make-mcp-streamable-http-transport endpoint &rest options)`
- Returns: a transport
- Defined in: `src/mcp-transport-http.lisp`

The streamable HTTP transport, including session headers and the OAuth boundary.

```lisp
(ax:make-mcp-streamable-http-transport "https://example.com/mcp" (ax:object))
```

### `mcp-transport`

- Qualified: `ax:mcp-transport`
- Kind: class
- Canonical Ax symbol: `AxMCPTransport`
- Form: `ax:mcp-transport`
- Returns: a transport
- Defined in: `src/mcp.lisp`

The transport protocol every MCP transport answers: send a request, a notification or a response, set the handlers, and open or close a request stream.

```lisp
(ax:mcp-transport-send transport message)
```

### `mcp-app-bridge`

- Qualified: `ax:mcp-app-bridge`
- Kind: class
- Canonical Ax symbol: `AxMCPApp`
- Form: `(ax:make-mcp-app-bridge client tool &rest options)`
- Returns: an App bridge
- Defined in: `src/mcp-app.lisp`

The Apps host bridge: it validates the ui:// resource and runs the host's callbacks for a frame's requests.

```lisp
(ax:mcp-app-bridge-load-resource bridge)
```

## Runtime Profiles

The runtime protocol a host code runtime speaks, and the envelopes it exchanges.

### `process-runtime`

- Qualified: `ax:process-runtime`
- Kind: class
- Canonical Ax symbol: `ProcessCodeRuntime`
- Form: `(ax:make-process-runtime command &key cwd env language timeout)`
- Returns: a code runtime
- Defined in: `src/agent-runtime.lisp`

The process runtime profile: a host runtime spoken to over the runtime protocol on stdio.

```lisp
(ax:make-process-runtime (list "node" "runtime-server.mjs") :language "JavaScript")
```

### `runtime-capabilities`

- Qualified: `ax:runtime-capabilities`
- Kind: function
- Canonical Ax symbol: `RuntimeCapabilities`
- Form: `(ax:runtime-capabilities &key inspect snapshot patch abort language usage-instructions)`
- Returns: a capabilities object
- Defined in: `src/agent-runtime.lisp`

What a runtime reports it can do, which is what a caller checks before using callables.

```lisp
(ax:runtime-capabilities :language "JavaScript" :patch nil)
```

### `envelope-result`

- Qualified: `ax:envelope-result`
- Kind: function
- Canonical Ax symbol: `RuntimeEnvelope`
- Form: `(ax:envelope-result value)`
- Returns: a result envelope object
- Defined in: `src/agent-runtime.lisp`

The runtime protocol's envelopes are plain JSON records built by constructor functions, one per envelope kind, rather than an envelope class.

```lisp
(ax:envelope-result (ax:object "value" 1))
```

### `runtime-protocol-error`

- Qualified: `ax:runtime-protocol-error`
- Kind: condition
- Canonical Ax symbol: `RuntimeProtocolError`
- Form: `ax:runtime-protocol-error`
- Returns: a condition
- Defined in: `src/agent-runtime.lisp`

A runtime that broke the protocol, with the category that says how.

```lisp
(ax:runtime-protocol-error-category condition)
```

## Optimizers

Optimizer engines, evaluators, cost tracking, checkpoints and optimized-program records.

### `optimize-program`

- Qualified: `ax:optimize-program`
- Kind: function
- Canonical Ax symbol: `optimize`
- Form: `(ax:optimize-program program dataset &key engine client options evaluator)`
- Options: `:engine`, `:evaluator`, `:examples`, `:budget`
- Returns: an optimized-program record (a JSON object)
- Defined in: `src/optimize.lisp`

Run an optimizer engine over a program and return its optimized-program record.

```lisp
(ax:optimize-program program examples :engine engine :client client :evaluator evaluator)
```

### `bootstrap-few-shot`

- Qualified: `ax:bootstrap-few-shot`
- Kind: class
- Canonical Ax symbol: `AxBootstrapFewShot`
- Form: `(ax:make-bootstrap-few-shot &optional options)`
- Returns: an optimizer engine
- Defined in: `src/optimize.lisp`

The bootstrap few-shot engine, which selects demonstrations from scored examples.

```lisp
(ax:make-bootstrap-few-shot (ax:object "maxDemos" 4))
```

### `gepa`

- Qualified: `ax:gepa`
- Kind: class
- Canonical Ax symbol: `AxGEPA`
- Form: `(ax:make-gepa &key reflection options seed)`
- Returns: an optimizer engine
- Defined in: `src/optimize.lisp`

The GEPA engine, with its Pareto component selector.

```lisp
(ax:make-gepa :options (ax:object "maxIterations" 8))
```

### `optimizer-engine`

- Qualified: `ax:optimizer-engine`
- Kind: class
- Canonical Ax symbol: `OptimizerEngine`
- Form: `ax:optimizer-engine`
- Returns: an optimizer engine
- Defined in: `src/optimize.lisp`

The engine protocol: a name, a version and one run call Core drives.

```lisp
(ax:run-optimizer-engine engine request evaluator)
```

### `program-evaluator`

- Qualified: `ax:program-evaluator`
- Kind: class
- Canonical Ax symbol: `AxMetricFn`
- Form: `(ax:make-program-evaluator program client &key dataset options rollout metric max-metric-calls cancel)`
- Returns: a program evaluator
- Defined in: `src/optimize.lisp`

The evaluator a candidate is scored with, together with its metric-call and budget accounting.

```lisp
(ax:make-program-evaluator program client :metric metric :dataset examples)
```

### `make-optimized-program`

- Qualified: `ax:make-optimized-program`
- Kind: function
- Canonical Ax symbol: `AxOptimizedProgram`
- Form: `(ax:make-optimized-program &key best-score stats component-map selector-state demos examples model-config optimizer-type optimization-time total-rounds converged score-history configuration-history artifact-format-version instruction-schema)`
- Returns: an optimized-program record
- Defined in: `src/optimize.lisp`

An optimized program is a plain JSON record rather than a type: these functions build it, parse it and apply it to a program.

```lisp
(ax:apply-optimized-program program record)
```

### `optimizer-checkpoint`

- Qualified: `ax:optimizer-checkpoint`
- Kind: function
- Canonical Ax symbol: `AxOptimizerCheckpoint`
- Form: `(ax:optimizer-checkpoint state &key optimizer-type optimizer-config best-score best-configuration engine-state)`
- Returns: a checkpoint object
- Defined in: `src/optimize.lisp`

A checkpoint of an optimizer run, which a later run loads to continue.

```lisp
(ax:load-optimizer-checkpoint (ax:make-optimizer-state) checkpoint)
```

### `f1-score`

- Qualified: `ax:f1-score`
- Kind: function
- Canonical Ax symbol: `f1_score`
- Form: `(ax:f1-score prediction ground-truth)`
- Returns: a number between 0 and 1
- Defined in: `src/optimize.lisp`

The F1 evaluation metric over predicted and expected text.

```lisp
(ax:f1-score "a b c" "a b")
```

## Errors And Values

The JSON value model every surface shares, and the condition hierarchy Ax signals.

### `object`

- Qualified: `ax:object`
- Kind: function
- Canonical Ax symbol: `AxJSONValue`
- Form: `(ax:object &rest alternating-key-values)`
- Returns: a JSON object
- Defined in: `src/json.lisp`

Build a JSON object. Objects are string-keyed equal hash tables with their key order preserved; arrays are vectors, booleans are ax:true and ax:false, and null is :null.

```lisp
(ax:object "question" "why?" "count" 2)
```

### `jget`

- Qualified: `ax:jget`
- Kind: function
- Canonical Ax symbol: `AxJSONValue.get`
- Form: `(ax:jget object key &optional default)`
- Returns: the value at the key, or the default
- Defined in: `src/json.lisp`

Read a key or index, defaulting to :null so a missing key reads as JSON null rather than nil.

```lisp
(ax:jget outputs "answer")
```

### `parse-json`

- Qualified: `ax:parse-json`
- Kind: function
- Canonical Ax symbol: `parse_json`
- Form: `(ax:parse-json string)`
- Returns: a JSON value
- Defined in: `src/json.lisp`

Parse one complete JSON document into the shared value model.

```lisp
(ax:parse-json "{\"a\":[1,2]}")
```

### `encode-json`

- Qualified: `ax:encode-json`
- Kind: function
- Canonical Ax symbol: `encode_json`
- Form: `(ax:encode-json value &key indent sort-keys)`
- Returns: a JSON string
- Defined in: `src/json.lisp`

Render a JSON value as compact JSON text, preserving object key order.

```lisp
(ax:encode-json (ax:object "a" 1))
```

### `ax-error`

- Qualified: `ax:ax-error`
- Kind: condition
- Canonical Ax symbol: `AxError`
- Form: `ax:ax-error`
- Returns: a condition
- Defined in: `src/json.lisp`

The base condition every Ax error inherits, carrying the message Ax wrote.

```lisp
(ax:ax-error-message condition)
```

### `provider-error`

- Qualified: `ax:provider-error`
- Kind: condition
- Canonical Ax symbol: `AxAIServiceError`
- Form: `ax:provider-error`
- Returns: a condition
- Defined in: `src/ai.lisp`

A typed provider failure: its kind, provider, status and retryability, with messages redacted of credentials.

```lisp
(ax:provider-error-kind condition)
```

### `generation-error`

- Qualified: `ax:generation-error`
- Kind: condition
- Canonical Ax symbol: `AxGenerateError`
- Form: `ax:generation-error`
- Returns: a condition
- Defined in: `src/gen.lisp`

A generation failure, with the validation problems when the kind is :validation.

```lisp
(ax:generation-error-problems condition)
```

### `signature-error`

- Qualified: `ax:signature-error`
- Kind: condition
- Canonical Ax symbol: `AxSignatureValidationError`
- Form: `ax:signature-error`
- Returns: a condition
- Defined in: `src/json.lisp`

An invalid signature: bad syntax, an unknown type or modifier, or a colliding field name.

```lisp
(handler-case (ax:parse-signature "not a signature")
  (ax:signature-error (condition) (ax:ax-error-message condition)))
```

### `cancel`

- Qualified: `ax:cancel`
- Kind: function
- Canonical Ax symbol: `AxCancellationToken.cancel`
- Form: `(ax:cancel token &optional reason)`
- Returns: the token
- Defined in: `src/ai.lisp`

Cancel a token, which ends the waits and calls subscribed to it.

```lisp
(ax:cancel token "user stopped")
```

## Not in this package

These canonical Ax symbols have no Common Lisp counterpart. They are listed rather than documented, so no form here names something that cannot be evaluated.

- `AnthropicClient` (axai): the anthropic profile is reached as (ai :name "anthropic") against one client type, so there is no per-provider class
- `AxCredentialProvider` (axai): the boundary exists as provider's :credential-provider callback, documented above, but it is a plain function per client: no credential-provider type, global registration or ai keyword is exported
- `AxCredentialRequest` (axai): the credential callback receives a plain JSON object of profile, operation, method and url, so there is no named credential-request type to document
- `AxMeter` (axai): no meter type is exported: a host meter is stored under the "meter" global and called through telemetry-call; ax-metrics returns service statistics, not a telemetry meter
- `AxTracer` (axai): no tracer type is exported: a host tracer object or callback is passed to start-span-fail-open and start-active-span-fail-open, or stored under the "tracer" global, and reached through telemetry-call by camelCase method name
- `AxUsageContext` (axai): no named usage-context type is exported: provider options and call options carry plain usageContext objects, merged by Core for chat usage events
- `AxUsageEvent` (axai): no named usage-event type is exported: the provider chat path builds a plain event object with Core and passes it to the onUsage callback when usage is available
- `AxUsageObserver` (axai): no observer type is exported: onUsage is a plain callback in globals or provider/call options; the provider chat path invokes it and ignores callback errors, without implying observer coverage for every operation
- `GoogleGeminiClient` (axai): the google-gemini profile is reached as (ai :name "google-gemini") against one client type, so there is no per-provider class
- `OpenAICompatibleClient` (axai): one client type serves every provider: (ai :name "openai-compatible") selects the profile and provider-profiles lists the profiles Core knows, so there is no per-provider class to document
- `OpenAIResponsesClient` (axai): the openai-responses profile is implemented and appears in provider-profiles; it is reached as (ai :name "openai-responses") rather than through a per-provider class
- `set_usage_observer` (axai): no dedicated observer setter is exported: use set-global with "onUsage" or provider/call options; the provider chat path reads that callback, but this does not establish stream or embedding observer parity

