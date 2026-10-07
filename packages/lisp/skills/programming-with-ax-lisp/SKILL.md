---
name: programming-with-ax-lisp
description: Builds typed LLM programs with the native Ax Common Lisp ASDF system. Use for Lisp signatures, providers, generation, tools, agents, flows, optimization, and MCP.
---

# Ax for Common Lisp

Use the native `axllm` ASDF system in SBCL. `ax` is its package nickname.
Do not translate Python method calls into Lisp or call generated `axllm/core`
internals from application code. Read the package's `API.md` for native forms.

## Load and generate

From the repository root, with the dependencies in `axllm.asd` installed:

```lisp
(require :asdf)
(asdf:load-asd (truename "packages/lisp/axllm.asd"))
(asdf:load-system "axllm")

;; Set OPENAI_API_KEY in the environment before starting SBCL.
(defparameter *client* (ax:ai :name "openai" :model "gpt-6-luna"))
(defparameter *answer* (ax:ax "question:string -> answer:string"))
(multiple-value-bind (output usage)
    (ax:forward *answer* *client* (ax:object "question" "What is a hash table?"))
  (format t "~A~%" (ax:jget output "answer")))
```

Always choose a model explicitly. Keep credentials in the process environment;
do not print keys, request headers, or credential-bearing objects.

## Keep Lisp values and JSON values distinct

| Meaning | Native value |
| --- | --- |
| Object | `(ax:object "name" value)`; string-keyed hash table |
| Array | `(vector item ...)`; `#()` for an empty array |
| JSON true / false | `ax:true` / `ax:false` |
| JSON null | `:null` |
| Missing object property | Use the second value of `gethash` |

`nil` is not JSON false, null, or an empty array. `ax:jget` returns `:null`
when a key is absent unless you supply a different fallback.
Use `ax:parse-json` and `ax:encode-json` at serialization boundaries.
Options that carry callback functions are host objects; do not encode them.

## Signatures and tools

Use a string signature with `ax:ax`, or build one with keyword arguments to
`ax:s` and field descriptions from `ax:f`:

```lisp
(ax:s :inputs (ax:object "question" (ax:f "string"))
      :outputs (ax:object "answer" (ax:f "string")))

(ax:tool
 :name "lookup"
 :description "Read a record by identifier"
 :parameters (ax:object "type" "object"
                        "properties" (ax:object "id" (ax:object "type" "string"))
                        "required" #("id")
                        "additionalProperties" ax:false)
 :handler (lambda (arguments) (lookup-record (ax:jget arguments "id"))))
```

`lookup-record` is an application function. Pass tool definitions in `:tools`
when constructing the program. Arguments are validated before the handler runs.
Omitted `additionalProperties` allows extra arguments; set it to `ax:false`
when the tool contract forbids them. A failed handler can have external effects;
do not assume generation retries roll them back.

## Compose programs through their protocols

- `ax:forward` takes a program, a service, inputs, and an optional options object.
  It returns outputs and usage as multiple values.
- `ax:program-streaming-forward` takes the same arguments. Its options object's
  `"sink"` function receives delta envelopes. Use the returned final output for
  validation-sensitive work; a displayed prefix is not a completed result.
- `ax:agent` takes a signature and `:options`. Supply a real code runtime for
  actor execution; a configured runtime is not an execution sandbox by itself.
- `ax:flow` constructs a flow. Add steps with `ax:flow-execute`, project results
  with `ax:flow-returns`, and run it through `ax:forward`.
- `ax:optimize-program` takes a program and dataset, with `:engine`, `:client`,
  `:options`, and `:evaluator`. Set explicit rollout/metric budgets. Use
  `"apply"` = `ax:false` to inspect an artifact before applying it.
- Provider adapters implement `ax:ax-chat`, `ax:ax-stream`, `ax:ax-features`, and
  the other service generics. Return the shared normalized response contract;
  do not return provider wire JSON from `ax:ax-chat`.
- Close a low-level stream with `ax:ax-stream-close` in `unwind-protect`.
  Close agent runtime sessions and MCP clients when their owner finishes.

## Verify the claimed behavior

Run `sbcl --script packages/lisp/tests/run.lisp` from the repository root.
The integration gate treats warnings, failed fixtures, unclaimed behavior,
partial results, and missing fixture inventories as failures. Generated Core
compiling, exported names existing, or a scripted transport passing does not
prove a real transport or an entire subsystem works.

Use the package's conformance declaration and executed test results when
describing support. Do not infer full parity from this API guide. Keep local
transport tests separate from credentialed provider smoke tests.
