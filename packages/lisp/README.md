# Ax for Common Lisp

`axllm` is a native Common Lisp port for SBCL, installed as an ASDF system.
Providers and generation run in Lisp without a Node.js or Python bridge.
JavaScript agent code uses a separate Node.js runtime worker.

The package provides:

- Signature parsing, rendering, and JSON Schema generated from Ax Core.
- Fluent signatures, prompt templates, field validation, and telemetry.
- A provider factory that uses Core profiles, including OpenAI Responses, Chat Completions, Anthropic Messages, and Gemini.
- Typed generation, output correction, and bounded tool execution.
- Agent runtimes, flows, optimization, MCP, and event delivery implementations.
- A proposer callback for [Jiti](https://github.com/ghuntley/jiti).

The full native test suite is the integration gate for these implementations.
Do not infer full conformance from generated Core compiling successfully.
The compiler claims only suites declared in `axir-conformance.json` and executes their runner during verification.
Verification requires a receipt for every shared fixture, including real-runtime agent fixtures.
It also checks native boundaries and runs the declared no-key examples.
The Jiti adapter is not an implementation of AxAgent or Jiti's interactive chat transport.
The other five generated backends retain their own verification gates.

## Install and use

Install SBCL and the ASDF dependencies listed in `axllm.asd`.
Quicklisp can provide the Lisp dependencies. On Debian or Ubuntu, use:

```sh
sudo apt-get install sbcl cl-yason cl-alexandria cl-ppcre cl-drakma \
  cl-base64 cl-cffi cl-puri cl-ironclad cl-local-time cl-sqlite
```

Clone this repository and register its ASDF system:

```lisp
(require :asdf)
(asdf:load-asd #P"/absolute/path/to/axth/packages/lisp/axllm.asd")
(asdf:load-system "axllm")

(defparameter *classify*
  (ax:ax "review:string -> sentiment:class \"positive, negative, neutral\""))

;; Set OPENAI_API_KEY in the process environment before starting SBCL.
(defparameter *client* (ax:ai :name "openai" :model "gpt-6-luna"))
(ax:forward *classify* *client* (ax:object "review" "The repair worked."))
```

`ax` is a nickname for the `axllm` package.
JSON objects use string-keyed hash tables. Arrays use vectors, including `#()` for an empty array.
Booleans use `ax:true` and `ax:false` (constants holding the quoted Yason symbols).
JSON null uses `:null`. `ax:jget` defaults missing keys to `:null`;
use the second value of `gethash` to distinguish an absent key from explicit null.
Lisp `nil` is not a substitute for these distinct JSON values.
Use `ax:parse-json` and `ax:encode-json` at the wire boundary.

Specify a model explicitly, or omit `:model` to use the profile's default.
A profile without a default rejects an omitted model.
For Anthropic, use `:name "anthropic"` and set `ANTHROPIC_API_KEY`.
Use `:base-url` for a compatible endpoint and `:api-key` to supply a credential directly.
Do not commit credentials or include them in logs.

`ax:supported-ai-models` reads Core's model catalogue. Its optional argument filters by model type.
`ax:provider-profiles` lists deployment profiles, not models.
`ax:model-info` looks up one model. `ax:model-catalog-summary` reports catalogue coverage, not a model list.

The transport callback has this contract:

```lisp
(lambda (url headers json-body)
  ;; Return the response body and HTTP status as two values.
  (values response-json-string 200))
```

Pass it as `:transport` to `ax:ai` for tests or another HTTP library.
The default transport uses Drakma, verifies HTTPS certificates, and does not follow redirects.
Provider errors retain the provider's error text, with configured credentials removed from the message and response body.
This applies to generation, direct service calls, and stream reads, including credentials supplied by a callback.
Conditions can also carry request data for explicit inspection. Do not log payloads without filtering sensitive data.
Cancellation can interrupt a streamed request before response headers arrive and closes the connection.
Tool handlers run application code. Grant each handler only the permissions it needs.
Generation reports handler failures to the model as tool results within its step limit.
Do not assume that a failed handler rolled back its external effects.

Generation uses Core for the field language, output parsing, and validation.
This includes `code`, dates, nested objects, and validation modifiers, as well as scalar and array fields.
Ax signatures do not allow `class` inputs.
Optional fields can be omitted. Internal fields are excluded from the returned object.
Input prompts enforce field types and ignore values absent from the signature.
Native `local-time` timestamps render as dates or datetimes in scalar fields.
Inside arrays and JSON objects, timestamps render as full ISO instants with milliseconds.
Support for a signature field does not by itself establish support for provider audio or streaming behavior.
Tool schemas support types, primitive enums, arrays, nested properties, required fields, and boolean `additionalProperties`.
Unsupported schema keywords are rejected before a handler can run.

## Run the public examples

The public, provider-backed examples live beside the other languages' under
[`src/examples/lisp/`](../../src/examples/lisp) and run through the repository's
example CLI from the repo root:

```sh
npm run example -- list
npm run example -- lisp src/examples/lisp/generation/axgen-openai.lisp
npm run example:lisp src/examples/lisp/short-agents/agent-openai.lisp
```

The runner puts this package on ASDF's registry, loads the `axllm` system and
then loads the example, so an example file is ordinary Ax code with no loader
boilerplate. Credentials come from the repository `.env`; every example states
the variables it needs in its `ax-example` header.

Add `--compile-only` to build an example without calling a provider. Every
warning is fatal there, so an example that names an API this package does not
implement fails immediately:

```sh
npm run example -- lisp src/examples/lisp/flows/flow-openai.lisp --compile-only
```

`AX_LISP_PACKAGE_DIR` points the runner at a different checkout of this
package, which is useful while the full port is still landing:

```sh
AX_LISP_PACKAGE_DIR=/path/to/integrated/packages/lisp \
  npm run example -- lisp src/examples/lisp/flows/flow-openai.lisp --compile-only
```

The agent, context-metrics and capstone examples need the JavaScript actor
runtime worker the other ports use. The example CLI exports
`AXIR_AXJS_RUNTIME_SERVER` and `AXIR_REPO_ROOT` for it, so run those examples
through `npm run example` rather than `sbcl --script`. The MCP examples need
`AX_MCP_COMMAND` set to an MCP server command, and the tool-call example
accepts `AX_MCP_TOOL`.

### What the examples claim

| Group | Levels | Covers |
|---|---|---|
| `signatures` | beginner, intermediate | fluent signatures and schemas; programs instead of prompts |
| `generation` | beginner, intermediate, advanced | typed generation, tools and traces, validated-output recovery |
| `short-agents` | beginner, intermediate, advanced | code-runtime actor, delegation, session pause and resume |
| `flows` | beginner, intermediate, advanced | chained nodes, branch and map nodes, parallel groups and Mermaid |
| `optimization` | beginner, intermediate, advanced | BootstrapFewShot, GEPA, refinement and playbooks |
| `audio` | beginner, intermediate, advanced | speech synthesis and transcription, audio output fields, audio across a flow |
| `mcp` | beginner, intermediate, advanced | stdio tools, tool calls, MCP tools inside a generator |
| `long-agents` | beginner, intermediate, advanced | context fields keeping bulk input in the runtime; context-pressure metrics; the composed capstone; a reused context map |
| `providers` | intermediate, advanced | event notifications; cost budgets, logging and caching |

Audio goes through the `ax-speak` and `ax-transcribe` generics on the service,
and through a signature's `:audio` output field with the `renderAudio` forward
option, which makes the generator synthesise the field rather than return it
unrendered. Richer audio behaviour beyond that boundary is still being ported.

The examples compile against the native API with warnings treated as failures.
Compile checks do not call live providers. The no-key example exercises catalogue
lookup, provider mapping, generation and a two-node flow without network access:

```sh
sbcl --script packages/lisp/examples/no-key.lisp
```

## Use with Jiti

This optional example adapter does not make the base library depend on Jiti.
Load `axllm/jiti` and pass its proposer to `image-agent:run`:

```lisp
(asdf:load-system "axllm/jiti")
;; SESSION is a Jiti session created by your application.
(image-agent:run session (ax:make-jiti-proposer *client*))
```

The callback converts Ax output into Jiti's `develop`, `execute`, `resume`, or `abort` action plist.
It validates action-specific fields and checks restart IDs against the current observation.
It never reads or evaluates the returned source. Jiti owns evaluation, generation counters, rollback, and session closure.
Ax validation failures signal an error. Jiti's `run` handles this as a failed proposal.
Current restart IDs remain in the prompt when diagnostic context is truncated.

Run Jiti only in an isolated image with appropriate host permissions.
Jiti executes model-proposed Lisp and is not a security sandbox.
The adapter does not change that trust boundary.

The example requires a local Jiti checkout and provider credentials:

```sh
JITI_ROOT=/path/to/jiti OPENAI_MODEL=gpt-6-luna \
  sbcl --script packages/lisp/examples/jiti.lisp
```

Jiti's interactive `make-chat` expects OpenAI Responses SSE and context endpoints.
This package does not emulate that interface. Use the proposer interface shown above.

## Test and maintain

The test system also needs `usocket`, `websocket-driver-client`, and the `openssl` command.
Applications do not need `websocket-driver-client` when they supply their own socket factory.
On Debian or Ubuntu, run `.agents/setup` from the repository root to install the test dependencies.
It installs pinned WebSocket sources from `.agents/lisp-dependencies.lock` and registers them with ASDF.
This registration selects Bordeaux Threads 0.9.4 before Debian's older version, which lacks the required `BT2` package.
The WebSocket tests use real loopback connections and fail if the client dependency is unavailable.

The built-in MCP WebSocket route rejects `wss://` because the pinned driver does not require certificate verification.
For `wss://`, supply a socket factory that verifies the certificate and hostname, and set `:trust-socket-factory-tls` explicitly.
WebSocket URLs also pass the HTTP endpoint policy before connection.

Then run from the repository root:

```sh
sbcl --script packages/lisp/tests/run.lisp
JITI_ROOT=/path/to/jiti sbcl --script packages/lisp/tests/jiti-integration.lisp
```

The first command runs shared fixtures and native tests for each component named in `axllm.asd`.
It rejects Ax compile and load warnings, undefined public exports, failed fixtures, and incomplete claimed suites.
The second uses a real Jiti worker with scripted model responses.
It exercises definition, execution, pause, repair, restart, and successful session closure without provider credentials.

`src/core.lisp` and `src/core-boundaries.json` form one generated artifact. Do not edit either file by hand.
Use the Go version from `.agents/setup`. From `tools/axir`, run:

```sh
go run ./cmd/lisp-core
go run ./cmd/lisp-core --check
go run ./cmd/lisp-core --verify-runtime
go test ./internal/axir -run Lisp
```

An alternate `--out` path also relocates the boundary manifest beside that output.
The emitter follows Core entry points across signatures, schema, prompts, providers, generators, agents, flows, programs, and MCP.
Native intrinsics, public wrappers, transports, and Jiti integration live in the other Lisp source files.
Shared fixtures come directly from `ir/conformance`, rather than a copied Lisp fixture set.
The Common Lisp CI workflow tests changes to these sources and fixtures and rejects stale generated output.

To emit a standalone package, run this command from `tools/axir`:

```sh
go run . compile --target lisp --out /tmp/ax-lisp ../../ir/axcore/root.axir
```

The compiler copies native sources from this package and regenerates Core and its manifests.
Set `AXIR_LISP_NATIVE_DIR` only when the native package lives outside the usual repository layout.
Standalone fixture tests need `AXIR_CONFORMANCE_DIR` set to the repository's `ir/conformance` directory.

When importing an upstream Ax update, regenerate Core and run both test commands before merging.
Review changes to `src/ax/ai`, `src/ax/dsp`, and `ir/axcore` for changes outside the generated subset.
Extend this package's supported scope only with behavior tests and an accurate capability description.
No Quicklisp registration, package publication, or recurring upstream merge is configured.
