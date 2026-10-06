# Ax for Common Lisp

`axllm` is a native Common Lisp port for SBCL, installed as an ASDF system.
It runs in the Lisp process without a Node.js or Python bridge.
This package is experimental and implements a subset of Ax, not full feature parity.

The package provides:

- Signature parsing, rendering, and JSON Schema generated from Ax Core.
- Synchronous OpenAI-compatible Chat Completions and Anthropic Messages clients.
- Typed generation, output correction, and bounded tool execution.
- A proposer callback for [Jiti](https://github.com/ghuntley/jiti).

Streaming, embeddings, audio, MCP, AxAgent, AxFlow, and optimizers are not included.
The Jiti adapter is not an implementation of AxAgent or Jiti's interactive chat transport.
The five full generated backends retain their separate AxIR verification gates.

## Install and use

Install SBCL and the `yason`, `cl-ppcre`, and `drakma` ASDF systems.
Quicklisp can provide these dependencies. On Debian or Ubuntu, use:

```sh
sudo apt-get install sbcl cl-yason cl-alexandria cl-ppcre cl-drakma
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

Specify a model explicitly. For Anthropic, use `:name "anthropic"` and set `ANTHROPIC_API_KEY`.
Use `:base-url` for a compatible endpoint and `:api-key` to supply a credential directly.
Do not commit credentials or include them in logs.

The transport callback has this contract:

```lisp
(lambda (url headers json-body)
  ;; Return the response body and HTTP status as two values.
  (values response-json-string 200))
```

Pass it as `:transport` to `ax:ai` for tests or another HTTP library.
The default transport uses Drakma. Provider errors do not include response bodies or credentials.
Tool handlers run application code. Grant each handler only the permissions it needs.
Handler errors propagate to the caller and are not retried automatically.

Generation supports `string`, `number`, `boolean`, `json`, and arrays of these types,
plus `class` for classification outputs. Ax signatures do not allow `class` inputs.
Optional fields can be omitted. Internal fields are excluded from the returned object.
Other field types and validation modifiers are rejected by `ax:ax`, even when the signature parser can describe them.
Tool schemas support types, primitive enums, arrays, nested properties, required fields, and boolean `additionalProperties`.
Unsupported schema keywords are rejected before a handler can run.

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

Run from the repository root:

```sh
sbcl --script packages/lisp/tests/run.lisp
JITI_ROOT=/path/to/jiti sbcl --script packages/lisp/tests/jiti-integration.lisp
```

The first command runs shared signature/schema fixtures plus native provider, generation, tool, and adapter tests.
The second uses a real Jiti worker with scripted model responses.
It exercises definition, execution, pause, repair, restart, and successful session closure without provider credentials.

`src/core.lisp` is generated. Do not edit it by hand.
With Go 1.22 or newer, regenerate it from `tools/axir`:

```sh
go run ./cmd/lisp-core --out ../../packages/lisp/src/core.lisp
go run ./cmd/lisp-core --check --out ../../packages/lisp/src/core.lisp
go test ./internal/axir -run Lisp
```

The emitter follows the dependency closure of the signature and schema entry points.
Native intrinsics, public wrappers, transports, and Jiti integration live in the other Lisp source files.
Shared fixtures come directly from `ir/conformance`, rather than a copied Lisp fixture set.
The Common Lisp CI workflow tests changes to these sources and fixtures and rejects stale generated output.

When importing an upstream Ax update, regenerate Core and run both test commands before merging.
Review changes to `src/ax/ai`, `src/ax/dsp`, and `ir/axcore` for changes outside the generated subset.
Extend this package's supported scope only with behavior tests and an accurate capability description.
No Quicklisp registration, package publication, or recurring upstream merge is configured.
