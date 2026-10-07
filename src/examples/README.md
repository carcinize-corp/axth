# Ax Examples

Public examples live under `src/examples/<language>/<group>/` and are generated
from each file's `ax-example` metadata header. Every public example calls a real
provider API and may require environment variables from the repo `.env`.

The public catalog currently requires beginner, intermediate, and advanced
examples for each language in `generation`, `short-agents`, `long-agents`,
`flows`, `optimization`, and `audio`, except where a language declares a
narrower claim in `publicExampleLanguages` (Common Lisp does; see below). The
`long-agents` group holds the
flagship DSPy+RLM+Peek agents (large context, native tools at scale, and skills
+ memory) in all six languages. Add `story: <number>` to a header only when the
example should appear in the website Advanced Start path.

List the current catalog:

```bash
npm run example -- list
npm run example -- list --json
```

Run an example from the repo root:

```bash
npm run example -- typescript src/examples/typescript/generation/axgen-openai.ts
npm run example -- python src/examples/python/generation/axgen-openai.py
npm run example -- java src/examples/java/generation/BasicGenerationExample.java
npm run example -- cpp src/examples/cpp/generation/basic_generation.cpp
npm run example -- go src/examples/go/generation/basic_generation.go
npm run example -- rust src/examples/rust/generation/basic_generation.rs
npm run example -- lisp src/examples/lisp/generation/axgen-openai.lisp
```

Any example can be checked without calling a provider by adding
`--compile-only`, which builds it and fails on any warning:

```bash
npm run example -- lisp src/examples/lisp/flows/flow-openai.lisp --compile-only
```

Internal generated package fixtures remain under `packages/<language>/examples`
for AxIR verification, but they are not part of the public examples catalog.

## Common Lisp

The Common Lisp examples under [`lisp/`](lisp/) run on SBCL against the native
`packages/lisp` ASDF system, with no Node.js or Python bridge in the path. The
runner puts that package on ASDF's registry and loads the example, the way the
Go and Rust runners assemble their scratch modules:

```bash
npm run example -- lisp src/examples/lisp/short-agents/agent-openai.lisp
npm run example:lisp src/examples/lisp/optimization/gepa-instructions.lisp
```

Install SBCL and the ASDF dependencies first; see
[`packages/lisp/README.md`](../../packages/lisp/README.md).

This language claims `generation`, `short-agents`, `flows`, `optimization`,
`audio` and `mcp`, at all three levels. The claim is declared per language in
`scripts/example-catalog.mjs`.

Integration of the full Common Lisp port is still in progress. The examples are
written against the complete native API and are compile-checked against it; the
`packages/lisp` tree committed here may still be the earlier signature-and-schema
subset, in which case `--compile-only` names the public symbols that are
missing. Point `AX_LISP_PACKAGE_DIR` at an integrated checkout to run the
compile gate against it.

The older standalone [generation and Jiti scripts](../../packages/lisp/README.md)
remain under `packages/lisp/examples` and are run directly with
`sbcl --script`.

## Typesafe / Jev (TypeScript)

| Example | Purpose |
|---|---|
| [typesafe.ts](typescript/generation/typesafe.ts) | Boolean/class signatures, criteria, and provider-level `trueThreshold` |
| [typesafe-native.ts](typescript/generation/typesafe-native.ts) | Native Noul/Choice/Score, structured criteria, probabilities, and model discovery |
| [typesafe-hybrid.ts](typescript/generation/typesafe-hybrid.ts) | Typesafe decisions followed by a generative summary |
| [value-descriptions.ts](typescript/generation/value-descriptions.ts) | One described signature through Typesafe and OpenAI |

Run from the repo root with `npm run tsx` followed by the example path. The
first two require `TYPESAFE_API_KEY` or `TYPESAFE_APIKEY` in `.env`; the last two
also require `OPENAI_API_KEY` or `OPENAI_APIKEY`. See the
[Typesafe/Jev skill](../ax/skills/ax-typesafe.md) for supported outputs,
native-only scoring, and question design.

Typesafe/Jev examples are available in every language’s `generation` directory:
signature decisions with `trueThreshold`, native rich criteria and Score, and an
explicit two-program hybrid reply. Set `TYPESAFE_APIKEY`; hybrid examples also need
`OPENAI_API_KEY` or `OPENAI_APIKEY`. Run them with `npm run example -- <language> <path>`.
