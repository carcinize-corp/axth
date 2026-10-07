package axir

// Common Lisp emission for the whole Core registry.
//
// This emits packages/lisp/src/core.lisp from every Core-bodied function in
// BuildCoreFuncRegistry -- the same set python, java and cpp emit -- read
// through the same BuildCoreFuncRegistry/BuildCoreBody seams. Core keeps the
// semantics; the generated Lisp carries no hand-written behavior, and the
// native boundaries it needs are declared in the generated manifest
// (packages/lisp/src/core-boundaries.json) and defined natively:
// pure boundaries in packages/lisp/src/core-runtime.lisp, host boundaries in
// the subsystem files beside it.
//
// Compilation alone makes no parity claim: declared capabilities must pass
// native conformance verification. A construct this emitter cannot express is a
// build error naming the construct, never a silent omission or a
// placeholder body.

import (
	"encoding/json"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

// LispCoreMarker identifies generated Lisp Core output. The generated file
// carries it so a reader (and the freshness check) can tell emitted Core
// from the hand-written runtime beside it.
const LispCoreMarker = "AXIR-CORE-LISP-V1"

// LispCorePackage is the internal package generated Core code lives in. The
// public axllm package wraps it, so emitted names can never collide with a
// CL symbol or with the published API.
const LispCorePackage = "axllm/core"

// LispBoundaryManifestVersion is bumped when the manifest's shape changes,
// so a consumer that reads it can fail loudly instead of guessing.
const LispBoundaryManifestVersion = 1

// lispIntrinsicNameExceptions holds the intrinsics whose Lisp boundary name
// is NOT the mechanical transform of the Python helper name. Every entry
// here is a deliberate, reported decision, not a convenience.
//
// intrinsic.object.call_method is the only one. Core calls it as
// (target, method, ...args) with a variable number of trailing arguments;
// the Core host object protocol chosen for this package is
// core-host-call(target, method, args-vector), so the emitter packs the
// trailing arguments into a Core array instead of spreading them. Naming the
// boundary core-object-call-method would leave the generated file calling a
// function the host protocol never defines.
var lispIntrinsicNameExceptions = map[CoreIntrinsic]string{
	IntrinsicObjectCallMethod: "core-host-call",
}

// lispIntrinsicLegacyNames pins the boundary names the signature/schema
// subset already shipped. LispIntrinsicName derives names from
// coreIntrinsicPython, so a rename on the Python side would otherwise
// silently rename a Lisp boundary that core-runtime.lisp already defines.
// TestLispCoreIntrinsicNamesAreStable checks this table against the
// derivation, and a mismatch is a build failure naming the intrinsic.
var lispIntrinsicLegacyNames = map[CoreIntrinsic]string{
	IntrinsicAdd:                 "core-add",
	IntrinsicAnd:                 "core-and",
	IntrinsicCoalesce:            "core-coalesce",
	IntrinsicContains:            "core-contains",
	IntrinsicDescriptionAppend:   "core-description-append",
	IntrinsicEq:                  "core-eq",
	IntrinsicSignatureError:      "core-signature-error",
	IntrinsicValidationError:     "core-validation-error",
	IntrinsicNestedFields:        "core-fields-from-map",
	IntrinsicGT:                  "core-gt",
	IntrinsicIsNone:              "core-is-none",
	IntrinsicIsNotNone:           "core-is-not-none",
	IntrinsicJSONParse:           "core-json-parse",
	IntrinsicLen:                 "core-len",
	IntrinsicListGet:             "core-list-get",
	IntrinsicLT:                  "core-lt",
	IntrinsicMapContains:         "core-map-contains",
	IntrinsicMapKeys:             "core-map-keys",
	IntrinsicMapMerge:            "core-map-merge",
	IntrinsicMapUpdate:           "core-map-update",
	IntrinsicNe:                  "core-ne",
	IntrinsicNone:                "core-none",
	IntrinsicNot:                 "core-not",
	IntrinsicOr:                  "core-or",
	IntrinsicRecordNew:           "core-record-new",
	IntrinsicStringConsumeOpt:    "core-string-consume-optional-quoted-prefix",
	IntrinsicStringExtractGroup:  "core-string-extract-leading-group",
	IntrinsicStringExtractSuf:    "core-string-extract-quoted-suffix",
	IntrinsicStringFindQuoted:    "core-string-find-outside-quotes",
	IntrinsicStringFormat:        "core-string-format",
	IntrinsicStringJoin:          "core-string-join",
	IntrinsicStringReplace:       "core-string-replace",
	IntrinsicStringSlice:         "core-string-slice",
	IntrinsicStringSplitOnce:     "core-string-split-once",
	IntrinsicStringSplitTopLevel: "core-string-split-top-level",
	IntrinsicStringSplitTrim:     "core-string-split-trim-nonempty",
	IntrinsicStringStartsWith:    "core-string-starts-with",
	IntrinsicStringWords:         "core-string-words",
	IntrinsicTruthy:              "core-truthy",
}

// LispIntrinsicName returns the native Lisp boundary for one Core
// intrinsic. The contract is mechanical: take the Python helper name, drop
// its leading underscore, and turn the remaining underscores into hyphens.
// So intrinsic.ai.stream_open (_core_ai_stream_open) becomes
// core-ai-stream-open and intrinsic.media.valid_image (_valid_image)
// becomes valid-image. The only departures are in
// lispIntrinsicNameExceptions, which exist to meet the host object protocol.
//
// Deriving the name rather than storing a second table means a new Core
// intrinsic needs no Lisp-side bookkeeping, and an intrinsic with no Python
// helper fails here instead of emitting a call into nothing.
func LispIntrinsicName(intrinsic CoreIntrinsic) (string, error) {
	if name, ok := lispIntrinsicNameExceptions[intrinsic]; ok {
		return name, nil
	}
	helper, ok := coreIntrinsicPython[intrinsic]
	if !ok {
		return "", fmt.Errorf("intrinsic %q has no Python helper name, so its Lisp boundary name cannot be derived; add it to coreIntrinsicPython", intrinsic)
	}
	name := strings.ReplaceAll(strings.TrimPrefix(helper, "_"), "_", "-")
	if name == "" {
		return "", fmt.Errorf("intrinsic %q maps to the empty Lisp name", intrinsic)
	}
	return name, nil
}

// lispCoverageMarkBoundary is the native boundary that records which Core
// functions a run actually entered. It mirrors the python port's
// _core_coverage_mark, and the emitted call is the only instrumentation the
// generated file carries.
const lispCoverageMarkBoundary = "core-coverage-mark"

// lispEmitCoverageMarks turns the per-function coverage mark on.
//
// It is off until packages/lisp defines core-coverage-mark. Emitting the
// call first would put an undefined-function error in the path of every
// Core function and break the signature and schema tests that pass today;
// a forward declaration makes such a file compile, which is exactly why it
// must not be mistaken for an implementation. Turning this on is a one-line
// change once the boundary lands, and
// TestLispCoreCoverageMarksAreEmittedWhenEnabled already exercises the
// emission, the manifest entry and the provenance guard for the on state.
//
// It is a var rather than a const only so that test can exercise the on
// state; nothing at run time changes it.
var lispEmitCoverageMarks = true

// lispOpBoundaries are the native boundaries the emitter calls for Core
// statement forms rather than for an intrinsic. They are listed here so the
// generated manifest can declare them with the same rigor as intrinsics
// instead of leaving a reader to grep the generated file.
var lispOpBoundaries = map[string]string{
	"append":      "core-append",
	"for":         "core-elements",
	"get":         "core-get",
	"if":          "core-true-p",
	"list":        "core-new-list",
	"map":         "core-new-map",
	"regex_match": "core-regex-match",
	"set":         "core-set",
	"string_join": "core-string-join",
	"string_trim": "core-string-trim",
	"type_is":     "core-type-is",
}

// lispCLSymbolNames are COMMON-LISP symbol names a generated function must
// not take. Generated locals are sigil-prefixed and so cannot collide, but a
// function name comes straight from the Core registry. The list covers the
// single-word names AxIR naming could plausibly produce, plus every operator
// this emitter itself writes; anything it misses still fails loudly, because
// SBCL's package lock refuses to redefine a COMMON-LISP symbol when the
// package is compiled.
var lispCLSymbolNames = map[string]bool{
	"append": true, "apply": true, "array": true, "block": true, "break": true,
	"car": true, "cdr": true, "class": true, "close": true, "code-char": true,
	"coerce": true, "concatenate": true, "cons": true, "count": true,
	"declaim": true, "declare": true, "defun": true, "describe": true,
	"do": true, "dolist": true,
	"error": true, "eval": true, "every": true, "fill": true, "find": true,
	"first": true, "format": true, "ftype": true, "function": true,
	"get": true, "handler-case": true,
	"hash-table": true, "identity": true, "if": true, "ignorable": true,
	"in-package": true, "keywordp": true, "last": true, "length": true,
	"let": true, "list": true, "load": true, "loop": true, "map": true,
	"mapcar": true, "max": true, "member": true, "merge": true, "min": true,
	"nil": true, "number": true, "parse-integer": true, "pop": true,
	"position": true, "print": true, "progn": true, "push": true, "quote": true,
	"read": true, "reduce": true, "remove": true, "replace": true,
	"rest": true, "return": true, "return-from": true, "reverse": true,
	"search": true, "set": true, "setf": true, "signal": true, "some": true,
	"sort": true, "string": true, "sublis": true, "subst": true, "t": true,
	"throw": true, "type": true, "type-of": true, "union": true, "values": true,
	"vector": true, "warn": true, "write": true,
}

// lispEmitterOperators are the Common Lisp operators this emitter writes.
// The generated-call audit uses the same list, so the two cannot drift.
var lispEmitterOperators = []string{
	"block", "code-char", "concatenate", "declaim", "declare", "defun",
	"dolist", "error", "ftype", "function", "handler-case", "if",
	"ignorable", "in-package", "let", "loop", "progn", "quote",
	"return-from", "setf", "string", "vector",
}

// LispCoreFuncName converts a registry native name to its Lisp name:
// underscores become hyphens and a private helper's leading underscore is
// dropped, since every generated function lives unexported in the internal
// axllm/core package. A name that would redefine a COMMON-LISP symbol is a
// generation error; SBCL's package lock would also reject it at build time,
// but failing here names the IR symbol instead of a compile trace.
func LispCoreFuncName(name string) string {
	return strings.ReplaceAll(strings.TrimPrefix(name, "_"), "_", "-")
}

// lispVarName converts a Core %value ref to a Lisp variable name, keeping
// the IR's % sigil. The sigil is not decoration: it guarantees a generated
// local can never name a COMMON-LISP symbol such as STRING, ERROR or
// VALUES, and binding one of those is undefined behavior that SBCL rejects
// with a package-lock error.
func lispVarName(ref string) (string, error) {
	name := strings.ReplaceAll(strings.TrimPrefix(ref, "%"), "_", "-")
	if name == "" {
		return "", fmt.Errorf("empty Core value ref %q", ref)
	}
	return "%" + name, nil
}

// lispArgDefault gives the optional-argument default one Core block arg
// needs when a Core signature takes fewer arguments than the body block
// declares. It is derived from pythonArgDefault rather than restated, so
// the two ports cannot disagree about which arguments are optional. Both
// arguments are the IR's own names (underscored, no % sigil).
func lispArgDefault(nativeFunc, nativeArg string) (string, error) {
	pyDefault := pythonArgDefault(nativeFunc, nativeArg)
	switch pyDefault {
	case "":
		return "", nil
	case "None":
		return ":null", nil
	case "True":
		return "'yason:true", nil
	case "False":
		return "'yason:false", nil
	}
	text, err := strconv.Unquote(pyDefault)
	if err != nil {
		return "", fmt.Errorf("argument default %s for %s(%s) has no Lisp representation", pyDefault, nativeFunc, nativeArg)
	}
	return lispString(text), nil
}

// lispFuncArity records one emitted function's lambda list shape so a Core
// call site can be checked before the file reaches SBCL. Common Lisp does
// report a wrong argument count at compile time, but as a warning inside a
// multi-megabyte file; failing here names the calling Core symbol.
type lispFuncArity struct {
	Required int
	Optional int
}

// lispBoundaryUse accumulates everything the manifest needs to say about one
// native boundary: who calls it, with how many arguments, and what the Core
// intrinsic table declares about it.
type lispBoundaryUse struct {
	name        string
	intrinsics  map[string]bool
	ops         map[string]bool
	callers     map[string]bool
	arities     map[int]bool
	callSites   int
	variadicDef bool
}

type lispEmitState struct {
	// names maps a Core symbol to the Lisp function name it emits as.
	names map[string]string
	// byEmitted maps a registry native name (the cross-target name) to its
	// Core symbol, so string callees resolve exactly like @refs.
	byEmitted map[string]string
	// arity maps a Core symbol to its emitted lambda list shape.
	arity map[string]lispFuncArity
	// fn is the Lisp name of the function being emitted, for return-from.
	fn string
	// symbol is the Core symbol being emitted, for error messages.
	symbol string
	// loops is the stack of enclosing loop block names, innermost last.
	loops []lispLoopFrame
	// depth numbers loops so block and element names stay unique.
	depth int
	// boundaries records every native boundary the emitted code called.
	boundaries map[string]*lispBoundaryUse
}

type lispLoopFrame struct {
	breakBlock    string
	continueBlock string
}

// note records one call to a native boundary and returns its name.
func (st *lispEmitState) note(name, intrinsic, op string, argCount int) string {
	use := st.boundaries[name]
	if use == nil {
		use = &lispBoundaryUse{
			name:       name,
			intrinsics: map[string]bool{},
			ops:        map[string]bool{},
			callers:    map[string]bool{},
			arities:    map[int]bool{},
		}
		st.boundaries[name] = use
	}
	if intrinsic != "" {
		use.intrinsics[intrinsic] = true
		if info, ok := coreIntrinsicInfo[intrinsic]; ok && info.MaxArgs < 0 {
			use.variadicDef = true
		}
	}
	if op != "" {
		use.ops[op] = true
	}
	if st.fn != "" {
		use.callers[st.fn] = true
	}
	use.arities[argCount] = true
	use.callSites++
	return name
}

func (st *lispEmitState) noteOp(op string, argCount int) (string, error) {
	name, ok := lispOpBoundaries[op]
	if !ok {
		return "", fmt.Errorf("core.%s has no declared Lisp boundary", op)
	}
	return st.note(name, "", op, argCount), nil
}

// calleeName resolves a Core callee to the Lisp name to call, checking the
// argument count against the callee's lambda list on the way.
//
// coreArgs is the number of arguments Core passes and emittedArgs the number
// the generated call actually carries. They differ only where a boundary's
// protocol reshapes them, as the host object protocol does; the Core count
// is what Core's arity rules check, and the emitted count is what the
// manifest reports so a boundary gets the right lambda list.
func (st *lispEmitState) calleeName(callee string, coreArgs, emittedArgs int) (string, error) {
	if strings.HasPrefix(callee, "intrinsic.") {
		intrinsic := CoreIntrinsic(callee)
		target, err := LispIntrinsicName(intrinsic)
		if err != nil {
			return "", err
		}
		if err := validateCoreIntrinsicArgs(callee, coreArgs); err != nil {
			return "", err
		}
		return st.note(target, callee, "", emittedArgs), nil
	}
	symbol := ""
	if strings.HasPrefix(callee, "@") {
		symbol = Symbol(callee)
	} else {
		// A bare callee is a registry native name; resolve it the same way
		// an @ref resolves so an emitted-name call cannot escape the set.
		resolved, ok := st.byEmitted[callee]
		if !ok {
			return "", fmt.Errorf("callee %q is neither an intrinsic nor a Core function", callee)
		}
		symbol = resolved
	}
	name, ok := st.names[symbol]
	if !ok {
		return "", fmt.Errorf("callee %q resolves to @%s, which is not an emitted Core function", callee, symbol)
	}
	shape, ok := st.arity[symbol]
	if !ok {
		return "", fmt.Errorf("callee @%s has no recorded lambda list", symbol)
	}
	if coreArgs < shape.Required || coreArgs > shape.Required+shape.Optional {
		want := strconv.Itoa(shape.Required)
		if shape.Optional > 0 {
			want = fmt.Sprintf("%d-%d", shape.Required, shape.Required+shape.Optional)
		}
		return "", fmt.Errorf("calls %s (@%s) with %d argument(s); it takes %s. Give the trailing Core arguments defaults in pythonArgDefault so every port agrees",
			name, symbol, coreArgs, want)
	}
	return name, nil
}

// LispCoreFunctions returns every Core-bodied function this target emits,
// in this target's deterministic order. It is the full registry, not a
// subset.
//
// The order is the registry's own -- emit module by dependency rank, then
// IR line, then symbol -- with one refinement: modules of equal rank are
// grouped by name rather than interleaved by line. Two modules share rank 6
// (flow and program), and interleaving them would scatter each module's
// functions through the generated file and repeat its section heading.
// Grouping leaves the ordering within every module untouched.
func LispCoreFunctions(model AxRuntimeModel) ([]CoreFuncSpec, error) {
	specs, err := BuildCoreFuncRegistry(model)
	if err != nil {
		return nil, err
	}
	out := append([]CoreFuncSpec(nil), specs...)
	sort.SliceStable(out, func(i, j int) bool {
		if coreModuleRank[out[i].Module] != coreModuleRank[out[j].Module] {
			return coreModuleRank[out[i].Module] < coreModuleRank[out[j].Module]
		}
		return out[i].Module < out[j].Module
	})
	return out, nil
}

// lispEmission is one complete pass over the registry: the rendered
// function bodies plus everything the header and the manifest report.
type lispEmission struct {
	specs      []CoreFuncSpec
	bodies     map[string]CoreBody
	arity      map[string]lispFuncArity
	names      map[string]string
	boundaries []*lispBoundaryUse
	text       string
}

func emitLispCore(model AxRuntimeModel) (*lispEmission, error) {
	specs, err := LispCoreFunctions(model)
	if err != nil {
		return nil, err
	}
	if len(specs) == 0 {
		return nil, fmt.Errorf("the Core registry is empty; nothing to emit")
	}
	st := &lispEmitState{
		names:      map[string]string{},
		byEmitted:  map[string]string{},
		arity:      map[string]lispFuncArity{},
		boundaries: map[string]*lispBoundaryUse{},
	}
	bodies := make(map[string]CoreBody, len(specs))
	nameOwner := map[string]string{}

	// First pass: names and lambda list shapes. Both must exist for every
	// function before any body is emitted, because Core calls freely in
	// both directions within a module.
	for _, spec := range specs {
		op, ok := model.Symbols[spec.Symbol]
		if !ok {
			return nil, fmt.Errorf("missing Core function @%s", spec.Symbol)
		}
		if model.BodySources[spec.Symbol] != "core" {
			return nil, fmt.Errorf("Core function @%s is missing body_source=core", spec.Symbol)
		}
		name := LispCoreFuncName(spec.Name)
		if lispCLSymbolNames[name] {
			return nil, fmt.Errorf("Core function @%s emits the Lisp name %s, which is a COMMON-LISP symbol; set an emit_name in the IR", spec.Symbol, name)
		}
		if owner, dup := nameOwner[name]; dup {
			return nil, fmt.Errorf("Core functions @%s and @%s both emit the Lisp name %s", owner, spec.Symbol, name)
		}
		nameOwner[name] = spec.Symbol
		body, err := BuildCoreBody(op)
		if err != nil {
			return nil, fmt.Errorf("@%s: %w", spec.Symbol, err)
		}
		if len(body.Blocks) != 1 {
			return nil, fmt.Errorf("@%s has %d Core body blocks; the Lisp target emits single-block bodies", spec.Symbol, len(body.Blocks))
		}
		shape, err := lispLambdaShape(spec, body.Blocks[0])
		if err != nil {
			return nil, fmt.Errorf("@%s: %w", spec.Symbol, err)
		}
		bodies[spec.Symbol] = body
		st.names[spec.Symbol] = name
		st.byEmitted[spec.Name] = spec.Symbol
		st.arity[spec.Symbol] = shape
	}

	// Second pass: bodies, grouped by emit module so the file reads in
	// dependency order and a reader can find a module's functions.
	var b strings.Builder
	fmt.Fprintf(&b, ";;;; %s\n\n", provenanceBeginFunctions)
	lastModule := ""
	for _, spec := range specs {
		if spec.Module != lastModule {
			if lastModule != "" {
				b.WriteByte('\n')
			}
			count := 0
			for _, other := range specs {
				if other.Module == spec.Module {
					count++
				}
			}
			b.WriteString(";;; ------------------------------------------------------------------\n")
			fmt.Fprintf(&b, ";;; emit module %s (%d functions)\n", spec.Module, count)
			b.WriteString(";;; ------------------------------------------------------------------\n\n")
			lastModule = spec.Module
		}
		text, err := emitLispCoreFunction(st, model.Symbols[spec.Symbol], spec, bodies[spec.Symbol])
		if err != nil {
			return nil, err
		}
		b.WriteString(text)
		b.WriteByte('\n')
	}
	fmt.Fprintf(&b, ";;;; %s\n", provenanceEndFunctions)

	// An emitted function must not shadow a native boundary: Common Lisp has
	// one function namespace per package, so the later definition would
	// silently win and the generated call would run the wrong code.
	for name := range st.boundaries {
		if owner, clash := nameOwner[name]; clash {
			return nil, fmt.Errorf("Core function @%s emits the Lisp name %s, which is also a native boundary; set an emit_name in the IR", owner, name)
		}
	}

	emission := &lispEmission{
		specs:      specs,
		bodies:     bodies,
		arity:      st.arity,
		names:      st.names,
		boundaries: st.sortedBoundaries(),
		text:       b.String(),
	}
	emission.text = renderLispForwardDeclarations(emission) + emission.text
	return emission, nil
}

// renderLispForwardDeclarations emits the file's forward declarations.
//
// Common Lisp resolves a function name at call time, so a reference to a
// name that is not yet defined is legal but makes SBCL note an undefined
// function at the end of the compilation unit. This file is full of such
// references in both directions: Core functions call each other regardless
// of definition order, and every native boundary a host subsystem owns is
// compiled after this file. Proclaiming each name's FTYPE marks it as
// declared, which is the standard way to say "this will exist" without
// defining anything.
//
// These are declarations, not definitions: nothing here provides a body,
// and a boundary that no native file ever defines still fails -- loudly, at
// the first call, and visibly to `lisp-core --verify-runtime` and to an
// FBOUNDP sweep over packages/lisp/src/core-boundaries.json.
//
// Generated functions get their exact lambda list shape, so SBCL keeps
// checking argument counts at every generated call site. Native boundaries
// get a wildcard argument list, because this file does not own their lambda
// lists and a narrower claim here could contradict the real definition.
func renderLispForwardDeclarations(emission *lispEmission) string {
	byShape := map[lispFuncArity][]string{}
	for _, spec := range emission.specs {
		shape := emission.arity[spec.Symbol]
		byShape[shape] = append(byShape[shape], emission.names[spec.Symbol])
	}
	shapes := make([]lispFuncArity, 0, len(byShape))
	for shape := range byShape {
		shapes = append(shapes, shape)
	}
	sort.Slice(shapes, func(i, j int) bool {
		if shapes[i].Required != shapes[j].Required {
			return shapes[i].Required < shapes[j].Required
		}
		return shapes[i].Optional < shapes[j].Optional
	})

	var b strings.Builder
	fmt.Fprintf(&b, ";;;; %s\n", provenanceBeginDeclarations)
	b.WriteString(";;; ------------------------------------------------------------------\n")
	b.WriteString(";;; Forward declarations\n")
	b.WriteString(";;;\n")
	b.WriteString(";;; Core functions call each other in any order, and the native\n")
	b.WriteString(";;; boundaries below are compiled after this file, so both are\n")
	b.WriteString(";;; proclaimed here. These declare names, never behavior: a boundary\n")
	b.WriteString(";;; nothing defines still fails at its first call, and\n")
	b.WriteString(";;; core-boundaries.json lists every one for an FBOUNDP sweep.\n")
	b.WriteString(";;; ------------------------------------------------------------------\n\n")
	fmt.Fprintf(&b, ";;; Generated Core functions (%d), by lambda list shape.\n", len(emission.specs))
	for _, shape := range shapes {
		names := byShape[shape]
		sort.Strings(names)
		fmt.Fprintf(&b, "(declaim (ftype %s\n", lispFunctionTypeText(shape))
		for i, name := range names {
			if i == len(names)-1 {
				fmt.Fprintf(&b, "                %s))\n", name)
				continue
			}
			fmt.Fprintf(&b, "                %s\n", name)
		}
	}
	b.WriteByte('\n')
	fmt.Fprintf(&b, ";;; Native boundaries (%d). Argument lists are deliberately wildcards:\n", len(emission.boundaries))
	b.WriteString(";;; the native definition owns them, and core-boundaries.json records the\n")
	b.WriteString(";;; exact argument counts this file passes.\n")
	b.WriteString("(declaim (ftype (function * t)\n")
	for i, use := range emission.boundaries {
		if i == len(emission.boundaries)-1 {
			fmt.Fprintf(&b, "                %s))\n", use.name)
			continue
		}
		fmt.Fprintf(&b, "                %s\n", use.name)
	}
	fmt.Fprintf(&b, ";;;; %s\n\n", provenanceEndDeclarations)
	return b.String()
}

// lispFunctionTypeText renders a FUNCTION type specifier for one lambda
// list shape. Every Core argument and result is a Core value, so each slot
// is T; the shape is what carries information.
func lispFunctionTypeText(shape lispFuncArity) string {
	parts := make([]string, 0, shape.Required+shape.Optional+1)
	for i := 0; i < shape.Required; i++ {
		parts = append(parts, "t")
	}
	if shape.Optional > 0 {
		parts = append(parts, "&optional")
		for i := 0; i < shape.Optional; i++ {
			parts = append(parts, "t")
		}
	}
	return "(function (" + strings.Join(parts, " ") + ") t)"
}

func (st *lispEmitState) sortedBoundaries() []*lispBoundaryUse {
	out := make([]*lispBoundaryUse, 0, len(st.boundaries))
	for _, use := range st.boundaries {
		out = append(out, use)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].name < out[j].name })
	return out
}

// lispLambdaShape reports how many required and optional arguments one
// emitted function takes.
func lispLambdaShape(spec CoreFuncSpec, block CoreBlock) (lispFuncArity, error) {
	shape := lispFuncArity{}
	for _, arg := range block.Args {
		def, err := lispArgDefault(spec.Name, arg.Name)
		if err != nil {
			return shape, err
		}
		if def != "" {
			shape.Optional++
			continue
		}
		if shape.Optional > 0 {
			return shape, fmt.Errorf("argument %s follows an optional argument without a default; give it one in pythonArgDefault", arg.Name)
		}
		shape.Required++
	}
	return shape, nil
}

// BuildLispCore renders packages/lisp/src/core.lisp from the runtime model.
func BuildLispCore(model AxRuntimeModel) (string, error) {
	emission, err := emitLispCore(model)
	if err != nil {
		return "", err
	}
	return renderLispCoreHeader(emission) + emission.text, nil
}

// LispBoundaryEntry is one native boundary the generated file calls.
type LispBoundaryEntry struct {
	// Name is the Lisp function the generated code calls.
	Name string `json:"name"`
	// Kind is "intrinsic" for a Core intrinsic boundary, "statement" for a
	// Core statement form's boundary, or "both" when a name serves each.
	Kind string `json:"kind"`
	// Intrinsics are the Core intrinsics this boundary implements.
	Intrinsics []string `json:"intrinsics,omitempty"`
	// Ops are the Core statement kinds this boundary implements.
	Ops []string `json:"ops,omitempty"`
	// HostBoundary is true when Core declares the intrinsic as a host
	// effect (IO, time, randomness, a callback into user code) and false
	// when Core declares it pure. It is null when Core's intrinsic table
	// does not classify the intrinsic at all, which is not the same as
	// pure: an unclassified boundary needs an owner decided by hand, so
	// the manifest says so rather than guessing.
	HostBoundary *bool `json:"hostBoundary"`
	// Variadic is true when Core declares an unbounded argument count.
	Variadic bool `json:"variadic"`
	// DeclaredMinArgs and DeclaredMaxArgs come from Core's intrinsic
	// table; MaxArgs is -1 when unbounded and both are null when Core
	// declares no arity (statement boundaries and untabled intrinsics).
	DeclaredMinArgs *int `json:"declaredMinArgs"`
	DeclaredMaxArgs *int `json:"declaredMaxArgs"`
	// ObservedArities are the exact argument counts the generated file
	// passes, so a boundary can be written with the right lambda list.
	ObservedArities []int `json:"observedArities"`
	// CallSites counts the calls the generated file makes.
	CallSites int `json:"callSites"`
	// Callers are the emitted Lisp functions that call this boundary.
	Callers []string `json:"callers"`
}

// LispFunctionEntry is one emitted Core function, so a consumer can prove
// that a Core call resolves to something this file defines.
type LispFunctionEntry struct {
	Name         string `json:"name"`
	Symbol       string `json:"symbol"`
	Module       string `json:"module"`
	Line         int    `json:"line"`
	RequiredArgs int    `json:"requiredArgs"`
	OptionalArgs int    `json:"optionalArgs"`
}

// LispBoundaryManifest is the machine-readable contract between the
// generated Core file and the native code under it: every function the file
// defines, and every name it calls but does not define.
type LispBoundaryManifest struct {
	Marker          string              `json:"marker"`
	ManifestVersion int                 `json:"manifestVersion"`
	Generator       string              `json:"generator"`
	Package         string              `json:"package"`
	GeneratedFile   string              `json:"generatedFile"`
	PureRuntimeFile string              `json:"pureRuntimeFile"`
	ErrorCondition  string              `json:"errorCondition"`
	HostProtocol    []string            `json:"hostObjectProtocol"`
	LispOperators   []string            `json:"commonLispOperators"`
	Modules         []LispModuleEntry   `json:"modules"`
	Functions       []LispFunctionEntry `json:"functions"`
	Boundaries      []LispBoundaryEntry `json:"boundaries"`
}

// LispModuleEntry counts the functions one emit module contributes.
type LispModuleEntry struct {
	Module    string `json:"module"`
	Rank      int    `json:"rank"`
	Functions int    `json:"functions"`
}

// BuildLispCoreBoundaryManifest renders the JSON manifest that declares
// every native dependency of the generated Core file. It is generated from
// the same emission pass as core.lisp, so the two cannot disagree: a
// consumer can intersect the manifest with the definitions a native file
// provides and prove there is no undefined dependency.
func BuildLispCoreBoundaryManifest(model AxRuntimeModel) (string, error) {
	emission, err := emitLispCore(model)
	if err != nil {
		return "", err
	}
	manifest := LispBoundaryManifest{
		Marker:          LispCoreMarker,
		ManifestVersion: LispBoundaryManifestVersion,
		Generator:       "tools/axir/cmd/lisp-core (BuildLispCoreBoundaryManifest)",
		Package:         LispCorePackage,
		GeneratedFile:   "packages/lisp/src/core.lisp",
		PureRuntimeFile: "packages/lisp/src/core-runtime.lisp",
		ErrorCondition:  "axllm:ax-error",
		HostProtocol: []string{
			"core-host-get (target key &optional (fallback :null))",
			"core-host-set (target key value)",
			"core-host-call (target method args-vector)",
		},
		LispOperators: append([]string(nil), lispEmitterOperators...),
		Modules:       lispModuleEntries(emission.specs),
	}
	for _, spec := range emission.specs {
		shape := emission.arity[spec.Symbol]
		manifest.Functions = append(manifest.Functions, LispFunctionEntry{
			Name:         emission.names[spec.Symbol],
			Symbol:       spec.Symbol,
			Module:       spec.Module,
			Line:         spec.Line,
			RequiredArgs: shape.Required,
			OptionalArgs: shape.Optional,
		})
	}
	for _, use := range emission.boundaries {
		manifest.Boundaries = append(manifest.Boundaries, lispBoundaryEntry(use))
	}
	out, err := json.MarshalIndent(manifest, "", "  ")
	if err != nil {
		return "", err
	}
	return string(out) + "\n", nil
}

func lispModuleEntries(specs []CoreFuncSpec) []LispModuleEntry {
	counts := map[string]int{}
	var order []string
	for _, spec := range specs {
		if counts[spec.Module] == 0 {
			order = append(order, spec.Module)
		}
		counts[spec.Module]++
	}
	sort.Slice(order, func(i, j int) bool {
		if coreModuleRank[order[i]] != coreModuleRank[order[j]] {
			return coreModuleRank[order[i]] < coreModuleRank[order[j]]
		}
		return order[i] < order[j]
	})
	out := make([]LispModuleEntry, 0, len(order))
	for _, module := range order {
		out = append(out, LispModuleEntry{Module: module, Rank: coreModuleRank[module], Functions: counts[module]})
	}
	return out
}

func lispBoundaryEntry(use *lispBoundaryUse) LispBoundaryEntry {
	entry := LispBoundaryEntry{
		Name:            use.name,
		Intrinsics:      sortedStrings(mapKeys(use.intrinsics)),
		Ops:             sortedStrings(mapKeys(use.ops)),
		Variadic:        use.variadicDef,
		ObservedArities: sortedInts(use.arities),
		CallSites:       use.callSites,
		Callers:         sortedStrings(mapKeys(use.callers)),
	}
	switch {
	case use.name == lispCoverageMarkBoundary:
		// Not a Core concept: the emitter adds it. Recording that a
		// function ran mutates host state, so it has a host owner.
		entry.Kind = "instrumentation"
		host := true
		entry.HostBoundary = &host
	case len(entry.Intrinsics) > 0 && len(entry.Ops) > 0:
		entry.Kind = "both"
	case len(entry.Intrinsics) > 0:
		entry.Kind = "intrinsic"
	default:
		// A Core statement form's boundary is a pure value operation by
		// construction: core.get, core.set and friends have no effects
		// beyond the Core value they are given.
		entry.Kind = "statement"
		pure := false
		entry.HostBoundary = &pure
	}
	// Core declares arity and host-effect status per intrinsic. A boundary
	// serving several intrinsics takes the widest declared range, and is a
	// host boundary when any of them is.
	for _, name := range entry.Intrinsics {
		info, ok := coreIntrinsicInfo[name]
		if !ok {
			continue
		}
		if entry.HostBoundary == nil || (info.HostBoundary && !*entry.HostBoundary) {
			host := info.HostBoundary
			entry.HostBoundary = &host
		}
		if info.MaxArgs < 0 {
			entry.Variadic = true
		}
		minArgs, maxArgs := info.MinArgs, info.MaxArgs
		if entry.DeclaredMinArgs == nil || minArgs < *entry.DeclaredMinArgs {
			entry.DeclaredMinArgs = &minArgs
		}
		if entry.DeclaredMaxArgs == nil || maxArgs < 0 || (*entry.DeclaredMaxArgs >= 0 && maxArgs > *entry.DeclaredMaxArgs) {
			entry.DeclaredMaxArgs = &maxArgs
		}
	}
	return entry
}

func mapKeys[K comparable](in map[K]bool) []K {
	out := make([]K, 0, len(in))
	for key := range in {
		out = append(out, key)
	}
	return out
}

func sortedInts(in map[int]bool) []int {
	out := make([]int, 0, len(in))
	for value := range in {
		out = append(out, value)
	}
	sort.Ints(out)
	return out
}

func renderLispCoreHeader(emission *lispEmission) string {
	var b strings.Builder
	b.WriteString(";;;; -*- mode: lisp; -*-\n")
	b.WriteString(";;;; GENERATED FILE - DO NOT EDIT.\n")
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, ";;;; Source marker: %s\n", LispCoreMarker)
	b.WriteString(";;;; Generator:     tools/axir/cmd/lisp-core (BuildLispCore)\n")
	b.WriteString(";;;; Source of truth: ir/axcore/*.axir Core bodies, read through\n")
	b.WriteString(";;;;   BuildCoreFuncRegistry / BuildCoreBody. Change Core, not this file.\n")
	b.WriteString(";;;;\n")
	b.WriteString(";;;; Regenerate (from tools/axir):\n")
	b.WriteString(";;;;   go run ./cmd/lisp-core --out ../../packages/lisp/src/core.lisp\n")
	b.WriteString(";;;; Check freshness (from tools/axir):\n")
	b.WriteString(";;;;   go run ./cmd/lisp-core --check\n")
	b.WriteString(";;;; Prove every boundary below is defined (from tools/axir):\n")
	b.WriteString(";;;;   go run ./cmd/lisp-core --verify-runtime\n")
	b.WriteString(";;;;\n")
	b.WriteString(";;;; FULL CORE REGISTRY, NO PARITY CLAIM YET. This file is every\n")
	b.WriteString(";;;; Core-bodied function in BuildCoreFuncRegistry, the same set the\n")
	b.WriteString(";;;; python, java and cpp targets emit. It is not registered in Compile\n")
	b.WriteString(";;;; and claims no parity: parity is claimed only once the AxIR\n")
	b.WriteString(";;;; conformance suites run green against this package.\n")
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, ";;;; Emitted functions (%d), by emit module in dependency order:\n", len(emission.specs))
	for _, entry := range lispModuleEntries(emission.specs) {
		fmt.Fprintf(&b, ";;;;   %-10s rank %d  %4d functions\n", entry.Module, entry.Rank, entry.Functions)
	}
	b.WriteString(";;;;\n")
	b.WriteString(";;;; Each function's Core provenance (@symbol and IR line) is in its\n")
	b.WriteString(";;;; docstring. The machine-readable index of every emitted function and\n")
	b.WriteString(";;;; every native boundary below is packages/lisp/src/core-boundaries.json.\n")
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, ";;;; Native boundaries this file calls (%d). A \"pure\" boundary belongs in\n", len(emission.boundaries))
	b.WriteString(";;;; packages/lisp/src/core-runtime.lisp; a \"host\" boundary is a declared\n")
	b.WriteString(";;;; Core host effect and belongs to its subsystem's native file. A \"?\"\n")
	b.WriteString(";;;; boundary is one Core's intrinsic table does not classify, so its\n")
	b.WriteString(";;;; owner is a decision, not a derivation.\n")
	b.WriteString(";;;;\n")
	for _, use := range emission.boundaries {
		entry := lispBoundaryEntry(use)
		fmt.Fprintf(&b, ";;;;   %-52s %-4s args=%s\n", entry.Name, lispBoundaryKindText(entry), lispArityText(entry))
	}
	b.WriteString(";;;;\n")
	b.WriteString(";;;; Generated errors are signalled as axllm:ax-error. The Core host\n")
	b.WriteString(";;;; object protocol is core-host-get / core-host-set / core-host-call.\n")
	b.WriteString(";;;;\n")
	b.WriteString(";;;; Every name above is proclaimed with a forward FTYPE declaration at\n")
	b.WriteString(";;;; the top of this file, so this file compiles on its own and before\n")
	b.WriteString(";;;; the native files that define the boundaries. A declaration is not a\n")
	b.WriteString(";;;; definition: a boundary nothing implements still fails at its first\n")
	b.WriteString(";;;; call, and the manifest lists every one for an FBOUNDP sweep.\n")
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, "(in-package #:%s)\n\n", LispCorePackage)
	return b.String()
}

// lispBoundaryKindText names who owns a boundary: core-runtime.lisp for a
// pure one, a subsystem for a declared host effect, and nobody yet for one
// Core leaves unclassified.
func lispBoundaryKindText(entry LispBoundaryEntry) string {
	switch {
	case entry.HostBoundary == nil:
		return "?"
	case *entry.HostBoundary:
		return "host"
	default:
		return "pure"
	}
}

func lispArityText(entry LispBoundaryEntry) string {
	if entry.Variadic {
		if len(entry.ObservedArities) > 0 {
			return fmt.Sprintf("%d+", entry.ObservedArities[0])
		}
		return "0+"
	}
	parts := make([]string, 0, len(entry.ObservedArities))
	for _, value := range entry.ObservedArities {
		parts = append(parts, strconv.Itoa(value))
	}
	return strings.Join(parts, ",")
}

func emitLispCoreFunction(st *lispEmitState, op Operation, spec CoreFuncSpec, body CoreBody) (string, error) {
	block := body.Blocks[0]
	name := st.names[spec.Symbol]
	st.fn = name
	st.symbol = spec.Symbol
	st.depth = 0
	st.loops = nil

	params := make([]string, 0, len(block.Args))
	paramSet := map[string]bool{}
	var lambda []string
	optional := false
	for _, arg := range block.Args {
		argName, err := lispVarName("%" + arg.Name)
		if err != nil {
			return "", fmt.Errorf("@%s: %w", spec.Symbol, err)
		}
		params = append(params, argName)
		paramSet[argName] = true
		def, err := lispArgDefault(spec.Name, arg.Name)
		if err != nil {
			return "", fmt.Errorf("@%s: %w", spec.Symbol, err)
		}
		if def != "" {
			if !optional {
				lambda = append(lambda, "&optional")
				optional = true
			}
			lambda = append(lambda, fmt.Sprintf("(%s %s)", argName, def))
			continue
		}
		if optional {
			return "", fmt.Errorf("@%s: argument %s follows an optional argument without a default", spec.Symbol, argName)
		}
		lambda = append(lambda, argName)
	}

	locals, err := collectLispLocals(block.Stmts, paramSet)
	if err != nil {
		return "", fmt.Errorf("@%s: %w", spec.Symbol, err)
	}

	stmts, err := emitLispCoreBlock(st, block)
	if err != nil {
		return "", fmt.Errorf("@%s: %w", spec.Symbol, err)
	}
	if lispEmitCoverageMarks {
		// The mark records that this function ran, so it has to be the
		// first thing the body does: a later position would miss every
		// path that returns early.
		//
		// The argument is the registry native name (parse_signature), not
		// this target's Lisp name (parse-signature). AuditCoverage matches
		// the trace against CoreFuncSpec.Name, so passing the Lisp name
		// would report every Core function as unexercised -- the exact
		// failure the hook exists to catch. Python's emitter passes its
		// emitted name only because for Python the two strings are equal.
		mark := lispCall(st.note(lispCoverageMarkBoundary, "", "coverage", 1), lispString(spec.Name))
		stmts = append([]string{mark}, stmts...)
	}
	// Core void functions fall off the end; return the Core none value
	// explicitly so a generated function never leaks the last form's value.
	if !lispBlockExits(block.Stmts) {
		stmts = append(stmts, ":null")
	}

	var b strings.Builder
	fmt.Fprintf(&b, "(defun %s (%s)\n", name, strings.Join(lambda, " "))
	fmt.Fprintf(&b, "  \"Core @%s (%s module, %s:%d). Generated; see header.\"\n",
		op.Symbol, spec.Module, lispCoreSourceFile(spec.Module), spec.Line)
	indent := "  "
	if len(params) > 0 {
		fmt.Fprintf(&b, "  (declare (ignorable %s))\n", strings.Join(params, " "))
	}
	if len(locals) > 0 {
		b.WriteString("  (let (")
		for i, local := range locals {
			if i > 0 {
				b.WriteByte(' ')
			}
			fmt.Fprintf(&b, "(%s :null)", local)
		}
		b.WriteString(")\n")
		fmt.Fprintf(&b, "    (declare (ignorable %s))\n", strings.Join(locals, " "))
		indent = "    "
		stmts[len(stmts)-1] += ")"
	}
	stmts[len(stmts)-1] += ")"
	for _, line := range stmts {
		fmt.Fprintf(&b, "%s%s\n", indent, line)
	}
	return b.String(), nil
}

func lispCoreSourceFile(module string) string {
	return "ir/axcore/" + module + ".axir"
}

// lispBlockExits reports whether a block always leaves the function, so the
// emitter knows not to append an unreachable trailing value. break and
// continue leave a loop rather than the function, and Core rejects them
// outside one, so they cannot terminate a function's top-level block.
func lispBlockExits(stmts []CoreStmt) bool {
	if len(stmts) == 0 {
		return false
	}
	last := stmts[len(stmts)-1]
	switch last.Kind {
	case "return", "raise":
		return true
	case "if", "try":
		first := firstBodyBlock(last)
		var second CoreBlock
		if len(last.Regions) > 1 && len(last.Regions[1].Blocks) > 0 {
			second = last.Regions[1].Blocks[0]
		}
		return lispBlockExits(first.Stmts) && lispBlockExits(second.Stmts)
	default:
		return false
	}
}

// collectLispLocals gathers every name a Core body assigns, excluding the
// parameters it rebinds. Core rebinds a result name across branches, so the
// emitted function binds all of them once as mutable locals rather than
// trying to express the body as nested lets.
//
// A core.try error binding is deliberately left out: HANDLER-CASE binds it
// itself, and Core scopes it to the catch region only.
func collectLispLocals(stmts []CoreStmt, params map[string]bool) ([]string, error) {
	seen := map[string]bool{}
	var order []string
	add := func(ref string) error {
		if ref == "" {
			return nil
		}
		name, err := lispVarName(ref)
		if err != nil {
			return err
		}
		if params[name] || seen[name] {
			return nil
		}
		seen[name] = true
		order = append(order, name)
		return nil
	}
	var walk func(stmts []CoreStmt) error
	walk = func(stmts []CoreStmt) error {
		for _, stmt := range stmts {
			if err := add(stmt.Result); err != nil {
				return err
			}
			if err := add(stmt.Item); err != nil {
				return err
			}
			for _, region := range stmt.Regions {
				for _, block := range region.Blocks {
					if err := walk(block.Stmts); err != nil {
						return err
					}
				}
			}
		}
		return nil
	}
	if err := walk(stmts); err != nil {
		return nil, err
	}
	return order, nil
}

func emitLispCoreBlock(st *lispEmitState, block CoreBlock) ([]string, error) {
	var lines []string
	for _, stmt := range block.Stmts {
		next, err := emitLispCoreStmt(st, stmt)
		if err != nil {
			return nil, err
		}
		lines = append(lines, next...)
	}
	return lines, nil
}

func emitLispCoreStmt(st *lispEmitState, stmt CoreStmt) ([]string, error) {
	switch stmt.Kind {
	case "call":
		args := make([]string, 0, len(stmt.Args))
		for _, arg := range stmt.Args {
			text, err := lispLiteral(arg)
			if err != nil {
				return nil, err
			}
			args = append(args, text)
		}
		coreArgs := len(args)
		if stmt.Callee == string(IntrinsicObjectCallMethod) {
			// The host object protocol takes the method arguments as one
			// Core array, so the generated call shape is fixed at three
			// arguments however many Core passes.
			args = lispPackHostCallArgs(args)
		}
		callee, err := st.calleeName(stmt.Callee, coreArgs, len(args))
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee, args...))
	case "const", "let":
		value, err := lispAttrValue(stmt.Op, "value")
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, value)
	case "map":
		callee, err := st.noteOp("map", 0)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee))
	case "list":
		callee, err := st.noteOp("list", 0)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee))
	case "get":
		if stmt.Result == "" || stmt.Target == "" {
			return nil, fmt.Errorf("core.get missing result or target")
		}
		target, err := lispLiteral(stmt.Target)
		if err != nil {
			return nil, err
		}
		key, err := lispLiteral(stmt.Key)
		if err != nil {
			return nil, err
		}
		fallback := ":null"
		if _, ok := Attr(stmt.Op, "default"); ok {
			fallback, err = lispAttrValue(stmt.Op, "default")
			if err != nil {
				return nil, err
			}
		}
		callee, err := st.noteOp("get", 3)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee, target, key, fallback))
	case "set":
		target, err := lispLiteral(stmt.Target)
		if err != nil {
			return nil, err
		}
		key, err := lispLiteral(stmt.Key)
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		callee, err := st.noteOp("set", 3)
		if err != nil {
			return nil, err
		}
		return []string{lispCall(callee, target, key, value)}, nil
	case "append":
		target, err := lispLiteral(stmt.Target)
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		callee, err := st.noteOp("append", 2)
		if err != nil {
			return nil, err
		}
		return []string{lispCall(callee, target, value)}, nil
	case "string_trim":
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		callee, err := st.noteOp("string_trim", 1)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee, value))
	case "string_join":
		sep, err := lispAttrValue(stmt.Op, "sep")
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		callee, err := st.noteOp("string_join", 2)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee, sep, value))
	case "type_is":
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		typeName, err := lispAttrValue(stmt.Op, "type")
		if err != nil {
			return nil, err
		}
		callee, err := st.noteOp("type_is", 2)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee, value, typeName))
	case "regex_match":
		pattern, err := lispAttrValue(stmt.Op, "pattern")
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		callee, err := st.noteOp("regex_match", 2)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(callee, pattern, value))
	case "return":
		if _, ok := Attr(stmt.Op, "value"); !ok {
			return []string{fmt.Sprintf("(return-from %s :null)", st.fn)}, nil
		}
		value, err := lispAttrValue(stmt.Op, "value")
		if err != nil {
			return nil, err
		}
		return []string{fmt.Sprintf("(return-from %s %s)", st.fn, value)}, nil
	case "raise":
		if _, ok := Attr(stmt.Op, "error"); ok {
			value, err := lispAttrValue(stmt.Op, "error")
			if err != nil {
				return nil, err
			}
			return []string{lispCall("error", value)}, nil
		}
		if stmt.Message == "" {
			return nil, fmt.Errorf("core.raise has neither an error value nor a message")
		}
		// A message-only raise has no Core error value to signal, so it
		// signals the package's own condition directly. :message is a
		// literal, never a format control, so a message containing ~ or %
		// cannot become a format directive.
		return []string{fmt.Sprintf("(error 'axllm:ax-error :message %s)", lispString(stmt.Message))}, nil
	case "if":
		return emitLispIf(st, stmt)
	case "for":
		return emitLispFor(st, stmt)
	case "loop":
		return emitLispLoop(st, stmt)
	case "break":
		frame, err := st.currentLoop("break")
		if err != nil {
			return nil, err
		}
		return []string{fmt.Sprintf("(return-from %s)", frame.breakBlock)}, nil
	case "continue":
		frame, err := st.currentLoop("continue")
		if err != nil {
			return nil, err
		}
		return []string{fmt.Sprintf("(return-from %s)", frame.continueBlock)}, nil
	case "try":
		return emitLispTry(st, stmt)
	default:
		return nil, fmt.Errorf("Core op %q has no Lisp lowering; implement it in lisp_core_emit.go before Core starts using it", stmt.Op.Name)
	}
}

// lispPackHostCallArgs turns a Core intrinsic.object.call_method argument
// list into the host object protocol's (target method args-vector) shape.
func lispPackHostCallArgs(args []string) []string {
	target, method := ":null", ":null"
	if len(args) > 0 {
		target = args[0]
	}
	if len(args) > 1 {
		method = args[1]
	}
	rest := []string(nil)
	if len(args) > 2 {
		rest = args[2:]
	}
	return []string{target, method, lispCall("vector", rest...)}
}

func (st *lispEmitState) currentLoop(kind string) (lispLoopFrame, error) {
	if len(st.loops) == 0 {
		return lispLoopFrame{}, fmt.Errorf("core.%s outside a loop", kind)
	}
	return st.loops[len(st.loops)-1], nil
}

func emitLispIf(st *lispEmitState, stmt CoreStmt) ([]string, error) {
	if stmt.Cond == "" {
		return nil, fmt.Errorf("core.if missing condition")
	}
	cond, err := lispLiteral(stmt.Cond)
	if err != nil {
		return nil, err
	}
	thenLines, err := emitLispCoreBlock(st, firstBodyBlock(stmt))
	if err != nil {
		return nil, err
	}
	var elseBlock CoreBlock
	if len(stmt.Regions) > 1 && len(stmt.Regions[1].Blocks) > 0 {
		elseBlock = stmt.Regions[1].Blocks[0]
	}
	elseLines, err := emitLispCoreBlock(st, elseBlock)
	if err != nil {
		return nil, err
	}
	callee, err := st.noteOp("if", 1)
	if err != nil {
		return nil, err
	}
	lines := []string{fmt.Sprintf("(if %s", lispCall(callee, cond))}
	lines = append(lines, lispProgn(thenLines, "  ")...)
	if len(elseLines) == 0 {
		lines[len(lines)-1] += ")"
		return lines, nil
	}
	lines = append(lines, lispProgn(elseLines, "  ")...)
	lines[len(lines)-1] += ")"
	return lines, nil
}

// lispProgn renders a statement list as one indented form: a bare statement
// when there is exactly one, (progn ...) otherwise, and :null when empty so
// an if-branch always has a form.
func lispProgn(lines []string, indent string) []string {
	switch len(lines) {
	case 0:
		return []string{indent + ":null"}
	case 1:
		return []string{indent + lines[0]}
	}
	out := []string{indent + "(progn"}
	for _, line := range lines {
		out = append(out, indent+"  "+line)
	}
	out[len(out)-1] += ")"
	return out
}

func emitLispFor(st *lispEmitState, stmt CoreStmt) ([]string, error) {
	if stmt.Item == "" || stmt.Iter == "" {
		return nil, fmt.Errorf("core.for missing item or in")
	}
	item, err := lispVarName(stmt.Item)
	if err != nil {
		return nil, err
	}
	iter, err := lispLiteral(stmt.Iter)
	if err != nil {
		return nil, err
	}
	body := firstBodyBlock(stmt)
	bodyLines, depth, err := st.emitLoopBody(body)
	if err != nil {
		return nil, err
	}
	callee, err := st.noteOp("for", 1)
	if err != nil {
		return nil, err
	}
	element := fmt.Sprintf("core-element-%d", depth)
	lines := []string{
		fmt.Sprintf("(dolist (%s %s)", element, lispCall(callee, iter)),
		fmt.Sprintf("  (setf %s %s)", item, element),
	}
	if len(bodyLines) == 0 {
		lines = append(lines, "  :null")
	}
	for _, line := range bodyLines {
		lines = append(lines, "  "+line)
	}
	lines[len(lines)-1] += ")"
	return st.wrapLoopBreak(lines, body, depth), nil
}

func emitLispLoop(st *lispEmitState, stmt CoreStmt) ([]string, error) {
	body := firstBodyBlock(stmt)
	bodyLines, depth, err := st.emitLoopBody(body)
	if err != nil {
		return nil, err
	}
	// LOOP with no keywords is Common Lisp's infinite loop; its body forms
	// must be compound forms, so wrap them. Core guarantees a core.loop
	// leaves through break, return or raise.
	lines := []string{"(loop"}
	inner := lispProgn(bodyLines, "  ")
	lines = append(lines, inner...)
	lines[len(lines)-1] += ")"
	return st.wrapLoopBreak(lines, body, depth), nil
}

// emitLoopBody emits one loop body, pushing the loop's break and continue
// block names so a nested break or continue targets this loop and not an
// outer one. The continue block is emitted only when the body uses
// continue, so a loop that does not need it stays readable.
func (st *lispEmitState) emitLoopBody(body CoreBlock) ([]string, int, error) {
	st.depth++
	depth := st.depth
	frame := lispLoopFrame{
		breakBlock:    fmt.Sprintf("core-loop-%d", depth),
		continueBlock: fmt.Sprintf("core-iteration-%d", depth),
	}
	st.loops = append(st.loops, frame)
	lines, err := emitLispCoreBlock(st, body)
	st.loops = st.loops[:len(st.loops)-1]
	st.depth--
	if err != nil {
		return nil, depth, err
	}
	if !lispLoopUses(body.Stmts, "continue") {
		return lines, depth, nil
	}
	out := []string{fmt.Sprintf("(block %s", frame.continueBlock)}
	if len(lines) == 0 {
		lines = []string{":null"}
	}
	for _, line := range lines {
		out = append(out, "  "+line)
	}
	out[len(out)-1] += ")"
	return out, depth, nil
}

// wrapLoopBreak wraps a loop in its named block, but only when the body
// uses break: an unused block would change every existing generated loop
// for no behavior.
func (st *lispEmitState) wrapLoopBreak(lines []string, body CoreBlock, depth int) []string {
	if !lispLoopUses(body.Stmts, "break") {
		return lines
	}
	out := []string{fmt.Sprintf("(block core-loop-%d", depth)}
	for _, line := range lines {
		out = append(out, "  "+line)
	}
	out[len(out)-1] += ")"
	return out
}

// lispLoopUses reports whether a loop body contains a break or continue
// bound to THIS loop. It stops at a nested for or loop, because those
// capture their own break and continue.
func lispLoopUses(stmts []CoreStmt, kind string) bool {
	for _, stmt := range stmts {
		if stmt.Kind == kind {
			return true
		}
		if stmt.Kind == "for" || stmt.Kind == "loop" {
			continue
		}
		for _, region := range stmt.Regions {
			for _, block := range region.Blocks {
				if lispLoopUses(block.Stmts, kind) {
					return true
				}
			}
		}
	}
	return false
}

func emitLispTry(st *lispEmitState, stmt CoreStmt) ([]string, error) {
	if len(stmt.Regions) != 2 {
		return nil, fmt.Errorf("core.try must contain exactly try and catch regions")
	}
	errorRef := AttrString(stmt.Op, "error")
	if errorRef == "" {
		return nil, fmt.Errorf("core.try missing error binding")
	}
	errorName, err := lispVarName(errorRef)
	if err != nil {
		return nil, err
	}
	tryLines, err := emitLispCoreBlock(st, firstBodyBlock(stmt))
	if err != nil {
		return nil, err
	}
	var catchBlock CoreBlock
	if len(stmt.Regions[1].Blocks) > 0 {
		catchBlock = stmt.Regions[1].Blocks[0]
	}
	catchLines, err := emitLispCoreBlock(st, catchBlock)
	if err != nil {
		return nil, err
	}
	// HANDLER-CASE unwinds before running the handler, which is what Core's
	// try/catch means and what every other port does. ERROR is the
	// condition class that matches Core's raise and the boundaries'
	// signals; a serious condition that is not an error stays unhandled, as
	// in the other ports.
	lines := []string{"(handler-case"}
	lines = append(lines, lispProgn(tryLines, "  ")...)
	lines = append(lines, fmt.Sprintf("  (error (%s)", errorName))
	lines = append(lines, fmt.Sprintf("    (declare (ignorable %s))", errorName))
	lines = append(lines, lispProgn(catchLines, "    ")...)
	lines[len(lines)-1] += "))"
	return lines, nil
}

func lispAssign(result, value string) ([]string, error) {
	if result == "" {
		return []string{value}, nil
	}
	name, err := lispVarName(result)
	if err != nil {
		return nil, err
	}
	return []string{fmt.Sprintf("(setf %s %s)", name, value)}, nil
}

func lispCall(callee string, args ...string) string {
	if len(args) == 0 {
		return "(" + callee + ")"
	}
	return "(" + callee + " " + strings.Join(args, " ") + ")"
}

func lispAttrValue(op Operation, name string) (string, error) {
	attr, ok := Attr(op, name)
	if !ok {
		return ":null", nil
	}
	return lispLiteral(attr.Value)
}

// lispLiteral compiles one Core attribute value. Core's value model maps
// onto the package's JSON value model exactly: none and JSON null are
// :null, booleans are yason:true and yason:false, and a %ref is a variable.
func lispLiteral(value interface{}) (string, error) {
	switch v := value.(type) {
	case nil:
		return ":null", nil
	case string:
		if strings.HasPrefix(v, "%") {
			return lispVarName(v)
		}
		return lispString(v), nil
	case QuotedString:
		return lispString(string(v)), nil
	case bool:
		// Quoted: YASON:TRUE and YASON:FALSE are symbols, not variables, so
		// a bare reference would be an unbound-variable error at run time.
		if v {
			return "'yason:true", nil
		}
		return "'yason:false", nil
	case int:
		return strconv.Itoa(v), nil
	case int64:
		return strconv.FormatInt(v, 10), nil
	case float64:
		return lispFloatLiteral(v)
	default:
		return "", fmt.Errorf("Core literal %#v has no Lisp representation", value)
	}
}

// maxExactIntegralFloat is the largest magnitude at which a float64 still
// represents every integer exactly. Limit integer emission to this range;
// larger doubles retain float syntax without an out-of-range int64 conversion.
const maxExactIntegralFloat = 1 << 53

// lispFloatLiteral renders a Core number as a Common Lisp literal.
//
// Two traps live here. First, Common Lisp reads an unsuffixed 0.1 as a
// SINGLE-FLOAT, and (= 0.1 0.1d0) is false: a bare decimal would silently
// lose about 1e-9 of precision against every other Ax port, whose numbers
// are IEEE doubles. Every non-integral literal therefore carries the d
// exponent marker. Second, an integral float only becomes an integer when
// it is small enough for that to be exact and for the conversion to be
// defined.
func lispFloatLiteral(value float64) (string, error) {
	switch {
	case math.IsNaN(value):
		return "", fmt.Errorf("Core literal NaN has no Lisp representation")
	case math.IsInf(value, 0):
		return "", fmt.Errorf("Core literal %v has no Lisp representation", value)
	}
	if value == math.Trunc(value) && math.Abs(value) <= maxExactIntegralFloat {
		// Keep an integral Core number an integer so emitted JSON and
		// arithmetic match the other ports instead of printing 2.0.
		return strconv.FormatInt(int64(value), 10), nil
	}
	// Prefer plain decimal digits with a d0 suffix, because -40.5d0 is far
	// easier to read in a diff than -4.05d1. Switch to exponent form for very
	// small or large values to avoid a wall of zeroes. These are Lisp source
	// formatting choices, not JavaScript's String(number) thresholds.
	// The round-trip check is a safety net: a form that
	// does not read back as this exact double is never emitted.
	if magnitude := math.Abs(value); magnitude >= 1e-4 && magnitude < 1e16 {
		plain := strconv.FormatFloat(value, 'f', -1, 64)
		if back, err := strconv.ParseFloat(plain, 64); err == nil && back == value {
			return plain + "d0", nil
		}
	}
	text := strconv.FormatFloat(value, 'e', -1, 64)
	marker := strings.IndexByte(text, 'e')
	if marker < 0 {
		return "", fmt.Errorf("cannot render Core number %v as a Lisp double", value)
	}
	exponent, err := strconv.Atoi(text[marker+1:])
	if err != nil {
		return "", fmt.Errorf("cannot render Core number %v as a Lisp double: %w", value, err)
	}
	return text[:marker] + "d" + strconv.Itoa(exponent), nil
}

// lispString renders a Common Lisp string literal. CL string syntax escapes
// only \ and "; a control character has no escape, so it is emitted through
// a concatenate form rather than being written raw into the literal, which
// keeps the generated file printable and byte-stable.
func lispString(text string) string {
	if !strings.ContainsFunc(text, func(r rune) bool { return r < 0x20 || r == 0x7f }) {
		return `"` + lispEscape(text) + `"`
	}
	var parts []string
	var plain strings.Builder
	flush := func() {
		if plain.Len() > 0 {
			parts = append(parts, `"`+lispEscape(plain.String())+`"`)
			plain.Reset()
		}
	}
	for _, r := range text {
		if r < 0x20 || r == 0x7f {
			flush()
			parts = append(parts, fmt.Sprintf("(string (code-char %d))", r))
			continue
		}
		plain.WriteRune(r)
	}
	flush()
	if len(parts) == 1 {
		return parts[0]
	}
	return "(concatenate 'string " + strings.Join(parts, " ") + ")"
}

func lispEscape(text string) string {
	var b strings.Builder
	for _, r := range text {
		if r == '\\' || r == '"' {
			b.WriteByte('\\')
		}
		b.WriteRune(r)
	}
	return b.String()
}

// ---------------------------------------------------------------------
// Compile and verify target registration
// ---------------------------------------------------------------------

// LispTargetIdiom is the Lisp target's idiom contract, recorded in the
// capability manifest so a reader can see which language conventions the
// generated package follows.
func LispTargetIdiom() TargetIdiom {
	return TargetIdiom{
		ModuleNaming:     "packages-and-asdf-systems",
		MethodNaming:     "lisp-case",
		AsyncPolicy:      "sync-first",
		CollectionPolicy: "hash-table-vector-at-dynamic-boundaries",
		ErrorPolicy:      "condition-hierarchy-boundary",
	}
}

// lispUnclaimedCapabilities is what this target does NOT yet claim. It is
// the honest half of the capability manifest: native implementations and
// tests exist, but only a declared and verified suite establishes coverage.
//
// This list shrinks only with explicit native declarations. Default
// verification rejects a package with any remaining gaps.
func lispUnclaimedCapabilities() []string {
	return []string{
		"conformance-runner: complete suite coverage has not been declared in axir-conformance.json",
		"native-boundaries: complete native boundary support has not been declared",
		"provider-transport: no provider HTTP transport is claimed for the generated surface",
		"examples: no no-key examples are declared",
		"runtime-profiles: no agent runtime profile is claimed",
	}
}

// LispConformanceDeclarationFile is where the native side declares which
// AxIR conformance suites its runner actually covers, relative to the
// package root.
const LispConformanceDeclarationFile = "axir-conformance.json"

// LispConformanceDeclaration is the native side's statement about its own
// conformance runner. The compiler never invents it: a suite is claimed
// only because this file says a runner covers it, and verify then runs that
// runner. Absent file means nothing is claimed.
type LispConformanceDeclaration struct {
	// Runner is the package-relative entry point, for example
	// tests/run.lisp. It must exist.
	Runner string `json:"runner"`
	// Command is the argv to run it with, relative to the package root.
	Command []string `json:"command"`
	// Suites are the AxIR conformance suites that runner covers.
	Suites []string `json:"suites"`
	// ScriptedTransport and RealNetwork record transport claims the runner
	// exercises.
	ScriptedTransport bool `json:"scriptedTransport"`
	RealNetwork       bool `json:"realNetwork"`
	// NativeBoundaries obliges verification of every emitted boundary and arity.
	NativeBoundaries bool `json:"nativeBoundaries"`
	// NoKeyExamples are package-relative Lisp scripts run by SBCL with keys scrubbed.
	NoKeyExamples []string `json:"noKeyExamples,omitempty"`
	// RuntimeProfiles currently accepts only javascript-process, exercised by axagent-real.
	RuntimeProfiles []string `json:"runtimeProfiles,omitempty"`
}

// LoadLispConformanceDeclaration reads the native conformance declaration
// from a package directory. A missing file is not an error: it means no
// complete suite coverage has been declared, even if a runner exists.
//
// This is the hook that keeps the manifest from being permanently
// not-claimed by construction. As each native suite lands, the native side
// adds it here, the capability manifest starts claiming it, and verify
// starts running it. The compiler cannot claim a suite on its own.
func LoadLispConformanceDeclaration(packageDir string) (*LispConformanceDeclaration, error) {
	data, err := os.ReadFile(filepath.Join(packageDir, LispConformanceDeclarationFile))
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, err
	}
	var declaration LispConformanceDeclaration
	if err := json.Unmarshal(data, &declaration); err != nil {
		return nil, fmt.Errorf("read %s: %w", LispConformanceDeclarationFile, err)
	}
	if strings.TrimSpace(declaration.Runner) == "" {
		return nil, fmt.Errorf("%s declares suites but no runner", LispConformanceDeclarationFile)
	}
	if _, err := os.Stat(filepath.Join(packageDir, filepath.FromSlash(declaration.Runner))); err != nil {
		return nil, fmt.Errorf("%s names runner %q, which does not exist: %w", LispConformanceDeclarationFile, declaration.Runner, err)
	}
	if len(declaration.Command) == 0 {
		return nil, fmt.Errorf("%s names runner %q but no command to run it with", LispConformanceDeclarationFile, declaration.Runner)
	}
	known := map[string]bool{}
	for _, suite := range lispConformanceSuites() {
		known[suite] = true
	}
	seen := map[string]bool{}
	for _, suite := range declaration.Suites {
		if !known[suite] {
			return nil, fmt.Errorf("%s claims unknown suite %q", LispConformanceDeclarationFile, suite)
		}
		if seen[suite] {
			return nil, fmt.Errorf("%s repeats suite %q", LispConformanceDeclarationFile, suite)
		}
		seen[suite] = true
	}
	for _, profile := range declaration.RuntimeProfiles {
		if profile != "javascript-process" || !seen["axagent"] {
			return nil, fmt.Errorf("runtime profile %q requires javascript-process and the axagent suite", profile)
		}
	}
	for _, example := range declaration.NoKeyExamples {
		if !filepath.IsLocal(example) || !strings.HasPrefix(filepath.ToSlash(example), "examples/") || filepath.Ext(example) != ".lisp" {
			return nil, fmt.Errorf("no-key example %q must be a package-relative examples/*.lisp script", example)
		}
		if info, err := os.Stat(filepath.Join(packageDir, example)); err != nil || !info.Mode().IsRegular() {
			return nil, fmt.Errorf("no-key example %q is not a regular file", example)
		}
	}
	return &declaration, nil
}

// lispDeclaredConformance reads the declaration from the native sources, so
// an emitted manifest reflects what the native side actually has.
func lispDeclaredConformance() (*LispConformanceDeclaration, error) {
	nativeDir, err := LispNativeSourceDir()
	if err != nil {
		return nil, nil // no native tree in reach; claim nothing
	}
	return LoadLispConformanceDeclaration(nativeDir)
}

// BuildLispCapabilityManifest builds the Lisp target's capability manifest.
//
// It deliberately does not go through BuildCapabilityManifest, which claims
// every conformance suite for any registered target. Here a suite is
// claimed only when the native conformance declaration says a runner covers
// it, and verify runs that runner, so the claim and the evidence land
// together. With no declaration the manifest claims nothing and names what
// is missing.
func BuildLispCapabilityManifest(model AxRuntimeModel) (CapabilityManifest, error) {
	declaration, err := lispDeclaredConformance()
	if err != nil {
		return CapabilityManifest{}, err
	}
	return buildLispCapabilityManifest(model, declaration)
}

func buildLispCapabilityManifest(model AxRuntimeModel, declaration *LispConformanceDeclaration) (CapabilityManifest, error) {
	idiom, ok := model.TargetIdioms["lisp"]
	if !ok {
		return CapabilityManifest{}, fmt.Errorf("the runtime model has no target idiom for lisp")
	}
	manifest := CapabilityManifest{
		AxIRVersion:              "0.1",
		Target:                   "lisp",
		PackageName:              packageNameForTarget("lisp"),
		SupportedSuites:          []string{},
		ProviderMode:             "not-claimed",
		ScriptedTransportSupport: false,
		RealNetworkSupport:       false,
		RuntimeProfiles:          nil,
		UnsupportedCapabilities:  lispUnclaimedCapabilities(),
		CoreOwnedFeatureGroups:   []string{},
		PublicSymbols:            []string{},
		TargetIdiom:              idiom,
	}
	if declaration == nil {
		return manifest, nil
	}
	manifest.SupportedSuites = append([]string(nil), declaration.Suites...)
	sort.Strings(manifest.SupportedSuites)
	manifest.ScriptedTransportSupport = declaration.ScriptedTransport
	manifest.RealNetworkSupport = declaration.RealNetwork
	for _, profile := range declaration.RuntimeProfiles {
		manifest.RuntimeProfiles = append(manifest.RuntimeProfiles, RuntimeProfileManifestEntry{
			ID: profile, ActorLanguage: "JavaScript", SupportMode: "process",
			DependencyMode: "external", VerificationCommand: "axir verify --targets lisp",
		})
	}
	manifest.ProviderMode = "provider-descriptor-registry-openai-compatible-openai-responses-google-gemini-anthropic"
	if !declaration.ScriptedTransport && !declaration.RealNetwork {
		manifest.ProviderMode = "not-claimed"
	}
	// Whatever the runner now covers stops being an unclaimed capability.
	claimed := map[string]bool{}
	for _, suite := range manifest.SupportedSuites {
		claimed[suite] = true
	}
	var remaining []string
	for _, capability := range manifest.UnsupportedCapabilities {
		if strings.HasPrefix(capability, "conformance-runner:") && len(claimed) == len(lispConformanceSuites()) {
			continue
		}
		if strings.HasPrefix(capability, "provider-transport:") && (declaration.ScriptedTransport || declaration.RealNetwork) {
			continue
		}
		if strings.HasPrefix(capability, "native-boundaries:") && declaration.NativeBoundaries {
			continue
		}
		if strings.HasPrefix(capability, "examples:") && len(declaration.NoKeyExamples) > 0 {
			continue
		}
		if strings.HasPrefix(capability, "runtime-profiles:") && len(declaration.RuntimeProfiles) > 0 {
			continue
		}
		remaining = append(remaining, capability)
	}
	manifest.UnsupportedCapabilities = remaining
	return manifest, nil
}

// lispConformanceSuites are the suites a default AxIR target must pass. The
// Lisp coverage manifest lists every one of them so the gap is visible
// rather than absent, and classifies each as explicitly-not-claimed.
func lispConformanceSuites() []string {
	return []string{
		"signature", "schema", "validation", "prompt", "axgen",
		"axai", "axagent", "axoptimize", "axprogram", "axflow",
		"axmcp", "axevent",
	}
}

// BuildLispConformanceCoverage builds the Lisp target's coverage manifest:
// every suite present and every suite honestly marked as not claimed. An
// absent suite would look like an oversight; explicitly-not-claimed says
// the work has not been done.
func BuildLispConformanceCoverage(model AxRuntimeModel) (ConformanceCoverageManifest, error) {
	manifest, err := BuildLispCapabilityManifest(model)
	if err != nil {
		return ConformanceCoverageManifest{}, err
	}
	declaration, err := lispDeclaredConformance()
	if err != nil {
		return ConformanceCoverageManifest{}, err
	}
	claimed := map[string]bool{}
	runner := "not claimed: no axir-conformance.json declaration"
	if declaration != nil {
		runner = declaration.Runner
		for _, suite := range declaration.Suites {
			claimed[suite] = true
		}
	}
	coverage := ConformanceCoverageManifest{
		AxIRVersion: manifest.AxIRVersion,
		Target:      manifest.Target,
		PackageName: manifest.PackageName,
		Suites:      map[string][]ConformanceCoverageEntry{},
	}
	for _, suite := range lispConformanceSuites() {
		entry := ConformanceCoverageEntry{
			Suite:    suite,
			Kind:     "suite",
			Runner:   runner,
			Category: "explicitly-not-claimed",
		}
		if claimed[suite] {
			// The declaration says this runner exercises the suite, and
			// verify runs it, so the claim has evidence behind it.
			entry.Category = "semantic"
		} else if declaration != nil {
			entry.Runner = "none: " + declaration.Runner + " does not cover this suite yet"
		}
		coverage.Suites[suite] = []ConformanceCoverageEntry{entry}
	}
	for _, profile := range manifest.RuntimeProfiles {
		coverage.Suites["axagent"] = append(coverage.Suites["axagent"], ConformanceCoverageEntry{
			Suite: "axagent", Kind: "agent_runtime_profile", Operation: profile.ID,
			Runner: runner, Category: "semantic",
		})
	}
	return coverage, nil
}

// LispNativeSourceEnv overrides where the Lisp target's hand-written
// sources are read from.
const LispNativeSourceEnv = "AXIR_LISP_NATIVE_DIR"

// lispNativeSourceAnchor is the file that identifies the native package
// directory, so the search cannot latch onto some other directory.
const lispNativeSourceAnchor = "axllm.asd"

// LispNativeSourceDir locates the Lisp package's hand-written sources.
//
// The Lisp target has no template copies of its native code. The axllm
// facade, the native boundaries, the ASDF system, the tests and the
// examples are the source of truth and live in packages/lisp; keeping a
// second hand-written copy under templates/ would be two files to edit and
// one of them would be wrong. So the packaging step reads them from where
// they are maintained.
//
// It fails loudly rather than emitting a Core-only package: a four-file
// directory that cannot be loaded would look like a successful compile.
func LispNativeSourceDir() (string, error) {
	if override := os.Getenv(LispNativeSourceEnv); override != "" {
		if _, err := os.Stat(filepath.Join(override, lispNativeSourceAnchor)); err != nil {
			return "", fmt.Errorf("%s=%s has no %s", LispNativeSourceEnv, override, lispNativeSourceAnchor)
		}
		return override, nil
	}
	start, err := os.Getwd()
	if err != nil {
		return "", err
	}
	dir := start
	for {
		candidate := filepath.Join(dir, "packages", "lisp")
		if _, err := os.Stat(filepath.Join(candidate, lispNativeSourceAnchor)); err == nil {
			return candidate, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", fmt.Errorf("cannot find packages/lisp/%s above %s; run inside the repository or set %s",
				lispNativeSourceAnchor, start, LispNativeSourceEnv)
		}
		dir = parent
	}
}

// lispGeneratedPackagePaths are the package-relative paths the compiler
// owns. Everything else in the package is hand-written and is copied
// verbatim, so a native file can never be silently replaced by generated
// content or the other way round.
func lispGeneratedPackagePaths() map[string]bool {
	return map[string]bool{
		lispProvenanceCoreFile:      true,
		lispBoundaryManifestFile:    true,
		"axir-capabilities.json":    true,
		"conformance-coverage.json": true,
		"axir-provenance.json":      true,
		"axir-api.json":             true,
		"API.md":                    true,
	}
}

// EmitLisp writes a complete, loadable Ax package for Common Lisp.
//
// The emitted Core comes from the IR; everything else is copied from the
// native sources that are its source of truth (see LispNativeSourceDir).
// Compiling into packages/lisp itself regenerates the Core file in place
// and leaves the native files untouched.
func EmitLisp(model AxRuntimeModel, outDir string) error {
	nativeDir, err := LispNativeSourceDir()
	if err != nil {
		return err
	}
	core, err := BuildLispCore(model)
	if err != nil {
		return err
	}
	boundaries, err := BuildLispCoreBoundaryManifest(model)
	if err != nil {
		return err
	}
	capabilities, err := BuildLispCapabilityManifest(model)
	if err != nil {
		return err
	}
	capabilitiesJSON, err := json.MarshalIndent(capabilities, "", "  ")
	if err != nil {
		return err
	}
	coverage, err := BuildLispConformanceCoverage(model)
	if err != nil {
		return err
	}
	coverageJSON, err := json.MarshalIndent(coverage, "", "  ")
	if err != nil {
		return err
	}
	if err := copyLispNativeSources(nativeDir, outDir); err != nil {
		return err
	}
	if err := writeFiles(outDir, map[string]string{
		lispProvenanceCoreFile:      core,
		lispBoundaryManifestFile:    boundaries,
		"axir-capabilities.json":    string(capabilitiesJSON) + "\n",
		"conformance-coverage.json": string(coverageJSON) + "\n",
	}); err != nil {
		return err
	}
	// The reference inventories ASDF components, including generated Core.
	// Emit those first so fresh and in-place compilation see the same tree.
	apiFiles, err := EmitLispAPIReferenceFiles(outDir)
	if err != nil {
		return err
	}
	if err := writeFiles(outDir, apiFiles); err != nil {
		return err
	}
	return ValidateLispPackageIsLoadable(outDir)
}

// copyLispNativeSources copies every hand-written file of the package.
// Copying into the native directory itself is a no-op rather than an
// error, so regenerating in place is safe.
func copyLispNativeSources(nativeDir, outDir string) error {
	sameDir, err := lispSamePath(nativeDir, outDir)
	if err != nil {
		return err
	}
	if sameDir {
		return nil
	}
	generated := lispGeneratedPackagePaths()
	return filepath.Walk(nativeDir, func(path string, info os.FileInfo, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		rel, err := filepath.Rel(nativeDir, path)
		if err != nil {
			return err
		}
		slashRel := filepath.ToSlash(rel)
		if info.IsDir() {
			// Build artifacts are not part of the package.
			if slashRel != "." && (info.Name() == ".git" || info.Name() == "_build") {
				return filepath.SkipDir
			}
			return nil
		}
		if generated[slashRel] {
			return nil
		}
		if filepath.Ext(path) == ".fasl" || slashRel == "tests/conformance-coverage.json" {
			return nil
		}
		content, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		target := filepath.Join(outDir, filepath.FromSlash(slashRel))
		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}
		return os.WriteFile(target, content, info.Mode().Perm())
	})
}

func lispSamePath(left, right string) (bool, error) {
	leftAbs, err := filepath.Abs(left)
	if err != nil {
		return false, err
	}
	rightAbs, err := filepath.Abs(right)
	if err != nil {
		return false, err
	}
	if leftAbs == rightAbs {
		return true, nil
	}
	leftInfo, leftErr := os.Stat(leftAbs)
	rightInfo, rightErr := os.Stat(rightAbs)
	if leftErr != nil || rightErr != nil {
		return false, nil
	}
	return os.SameFile(leftInfo, rightInfo), nil
}

var lispASDFComponentRe = regexp.MustCompile(`\(:file\s+"([^"]+)"\)`)
var lispASDFModuleRe = regexp.MustCompile(`\(:module\s+"([^"]+)"`)

// ValidateLispPackageIsLoadable checks the emitted package is a package and
// not a pile of files: the ASDF system definition must be present, it must
// name the generated Core file as a component, and every component it names
// must exist on disk.
//
// This is what makes "compile --target lisp" mean something. A directory
// holding only the generated Core would compile cleanly here and then fail
// for a consumer at (asdf:load-system "axllm"), which is exactly the kind
// of success that is worse than an error.
func ValidateLispPackageIsLoadable(outDir string) error {
	asdPath := filepath.Join(outDir, lispNativeSourceAnchor)
	asd, err := os.ReadFile(asdPath)
	if err != nil {
		return fmt.Errorf("the emitted package has no %s, so nothing can load it: %w", lispNativeSourceAnchor, err)
	}
	text := string(asd)
	modules := lispASDFModuleRe.FindAllStringSubmatch(text, -1)
	components := lispASDFComponentRe.FindAllStringSubmatch(text, -1)
	if len(components) == 0 {
		return fmt.Errorf("%s names no components", lispNativeSourceAnchor)
	}
	// Resolve each component against the module directories the system
	// declares, which is how ASDF itself resolves them.
	prefixes := []string{""}
	for _, module := range modules {
		prefixes = append(prefixes, module[1]+"/")
	}
	coreComponent := strings.TrimSuffix(filepath.Base(lispProvenanceCoreFile), ".lisp")
	foundCore := false
	var missing []string
	for _, component := range components {
		name := component[1]
		if name == coreComponent || strings.HasSuffix(name, "/"+coreComponent) {
			foundCore = true
		}
		resolved := false
		for _, prefix := range prefixes {
			if _, err := os.Stat(filepath.Join(outDir, filepath.FromSlash(prefix+name+".lisp"))); err == nil {
				resolved = true
				break
			}
		}
		if !resolved {
			missing = append(missing, name+".lisp")
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		return fmt.Errorf("%s names %d component(s) the emitted package does not contain: %s",
			lispNativeSourceAnchor, len(missing), strings.Join(missing, ", "))
	}
	if !foundCore {
		return fmt.Errorf("%s does not load the generated %s, so the emitted Core would never run",
			lispNativeSourceAnchor, lispProvenanceCoreFile)
	}
	// The native boundaries the generated Core calls have to be loaded
	// before or with it; a package that omits the runtime file would load
	// and then fail at the first Core call.
	if _, err := os.Stat(filepath.Join(outDir, "src", "core-runtime.lisp")); err != nil {
		return fmt.Errorf("the emitted package has no src/core-runtime.lisp, which defines the boundaries the generated Core calls: %w", err)
	}
	return nil
}

// lispBoundaryManifestFile is where the boundary manifest lives, relative
// to the package root.
const lispBoundaryManifestFile = "src/core-boundaries.json"

// VerifyLispManifest checks manifests against each other and the native
// declaration. Partial declarations can be compiled, but default verification
// additionally requires a complete declaration.
func VerifyLispManifest(outDir string) error {
	capabilitiesData, err := os.ReadFile(filepath.Join(outDir, "axir-capabilities.json"))
	if err != nil {
		return err
	}
	var capabilities CapabilityManifest
	if err := json.Unmarshal(capabilitiesData, &capabilities); err != nil {
		return fmt.Errorf("read axir-capabilities.json: %w", err)
	}
	if capabilities.Target != "lisp" {
		return fmt.Errorf("manifest target is %q, want lisp", capabilities.Target)
	}
	coverageData, err := os.ReadFile(filepath.Join(outDir, "conformance-coverage.json"))
	if err != nil {
		return err
	}
	var coverage ConformanceCoverageManifest
	if err := json.Unmarshal(coverageData, &coverage); err != nil {
		return fmt.Errorf("read conformance-coverage.json: %w", err)
	}
	if err := ValidateConformanceCoverage(capabilities, coverage); err != nil {
		return err
	}
	// Every suite must be accounted for, claimed or not, so a missing
	// suite cannot pass as an unclaimed one.
	for _, suite := range lispConformanceSuites() {
		if len(coverage.Suites[suite]) == 0 {
			return fmt.Errorf("conformance coverage does not mention suite %q", suite)
		}
	}
	// A claimed suite with no runner is the dishonest state this guard
	// exists to prevent.
	for _, suite := range capabilities.SupportedSuites {
		for _, entry := range coverage.Suites[suite] {
			if entry.Category == "explicitly-not-claimed" {
				return fmt.Errorf("suite %q is claimed in axir-capabilities.json but conformance coverage marks it as not claimed", suite)
			}
		}
	}
	if len(capabilities.SupportedSuites) == 0 && len(capabilities.UnsupportedCapabilities) == 0 {
		return fmt.Errorf("the manifest claims no suites and names nothing unsupported; one of the two must be true")
	}
	declaration, err := LoadLispConformanceDeclaration(outDir)
	if err != nil {
		return err
	}
	expected, err := buildLispCapabilityManifest(AxRuntimeModel{TargetIdioms: map[string]TargetIdiom{"lisp": LispTargetIdiom()}}, declaration)
	if err != nil {
		return err
	}
	want, _ := json.Marshal(expected)
	got, _ := json.Marshal(capabilities)
	if string(want) != string(got) {
		return fmt.Errorf("axir-capabilities.json disagrees with %s; regenerate the package", LispConformanceDeclarationFile)
	}
	var boundaries LispBoundaryManifest
	boundaryData, err := os.ReadFile(filepath.Join(outDir, lispBoundaryManifestFile))
	if err != nil {
		return err
	}
	if err := json.Unmarshal(boundaryData, &boundaries); err != nil {
		return fmt.Errorf("read %s: %w", lispBoundaryManifestFile, err)
	}
	if boundaries.Marker != LispCoreMarker {
		return fmt.Errorf("%s has marker %q, want %q", lispBoundaryManifestFile, boundaries.Marker, LispCoreMarker)
	}
	if len(boundaries.Functions) == 0 || len(boundaries.Boundaries) == 0 {
		return fmt.Errorf("%s declares %d functions and %d boundaries; both must be present",
			lispBoundaryManifestFile, len(boundaries.Functions), len(boundaries.Boundaries))
	}
	return nil
}

// lispCompileDriver is the SBCL program that compiles the generated file on
// its own. It defines only the package scaffolding and the condition the
// generated code signals, and no boundary at all, so a clean result proves
// the forward declarations really do cover every name the file calls.
const lispCompileDriver = `(defpackage #:yason (:use) (:export #:true #:false))
(defpackage #:axllm (:use #:cl) (:export #:ax-error #:ax-error-message))
(in-package #:axllm)
(define-condition ax-error (error)
  ((message :initarg :message :initform "" :reader ax-error-message)))
(defpackage #:axllm/core (:use #:cl))
(in-package #:cl-user)
(let ((full 0) (style 0) (shown 0))
  (handler-bind
      ((sb-ext:compiler-note #'muffle-warning)
       (style-warning (lambda (c)
                        (incf style)
                        (when (< shown 25) (incf shown) (format t "~&DIAG: ~a~%" c))
                        (muffle-warning c)))
       (warning (lambda (c)
                  (incf full)
                  (when (< shown 25) (incf shown) (format t "~&DIAG: ~a~%" c))
                  (muffle-warning c))))
    (multiple-value-bind (fasl warnings failure)
        (compile-file (second sb-ext:*posix-argv*)
                      :output-file (third sb-ext:*posix-argv*)
                      :verbose nil :print nil)
      (declare (ignore warnings))
      (when (or (null fasl) failure)
        (format t "~&lisp-compile: FAILED~%")
        (sb-ext:exit :code 1))))
  (format t "~&lisp-compile: full-warnings=~d style-warnings=~d~%" full style)
  (unless (and (zerop full) (zerop style))
    (sb-ext:exit :code 1)))
`

// VerifyLispGeneratedFileCompiles compiles the emitted Core file with SBCL
// and no boundary definitions present.
//
// This is the strongest check available before the native boundaries exist.
// It proves the whole emitted file is structurally valid Common Lisp, that
// every name it calls is either defined in the file or forward-declared,
// and that the file can be compiled ahead of the subsystem wrappers. It
// proves nothing about behavior, which is what conformance is for.
func VerifyLispGeneratedFileCompiles(sbcl, coreFile, workDir string) (string, error) {
	driverPath := filepath.Join(workDir, "axir-lisp-compile.lisp")
	if err := os.WriteFile(driverPath, []byte(lispCompileDriver), 0o644); err != nil {
		return "", err
	}
	fasl := filepath.Join(workDir, "axir-lisp-core.fasl")
	output, err := exec.Command(sbcl, "--dynamic-space-size", "4096", "--script", driverPath, coreFile, fasl).CombinedOutput()
	text := strings.TrimSpace(string(output))
	if err != nil {
		return text, fmt.Errorf("sbcl could not compile %s cleanly: %v\n%s", coreFile, err, text)
	}
	if !strings.Contains(text, "full-warnings=0 style-warnings=0") {
		return text, fmt.Errorf("compiling %s is not warning-free:\n%s", coreFile, text)
	}
	return text, nil
}

// ---------------------------------------------------------------------
// Native boundary arity checking
// ---------------------------------------------------------------------

// LispLambdaShape is what a native definition will accept.
type LispLambdaShape struct {
	// Name is the defined function.
	Name string
	// File is the native file that defines it.
	File string
	// Required and Optional count the positional parameters.
	Required int
	Optional int
	// Unbounded is true for &rest or &body, which accept any extra count.
	Unbounded bool
	// Keyword is true for &key, where a positional count says nothing.
	Keyword bool
}

// Accepts reports whether a call with this many positional arguments is
// legal for the definition.
func (s LispLambdaShape) Accepts(args int) bool {
	if args < s.Required {
		return false
	}
	if s.Unbounded || s.Keyword {
		return true
	}
	return args <= s.Required+s.Optional
}

// Describe renders the accepted argument counts for an error message.
func (s LispLambdaShape) Describe() string {
	switch {
	case s.Unbounded || s.Keyword:
		return fmt.Sprintf("%d or more", s.Required)
	case s.Optional == 0:
		return strconv.Itoa(s.Required)
	default:
		return fmt.Sprintf("%d-%d", s.Required, s.Required+s.Optional)
	}
}

// defgeneric is included because the native runtime defines the host
// object protocol as a generic function, and a generic function's lambda
// list constrains a call exactly as a defun's does.
var lispDefinitionHeadRe = regexp.MustCompile(`(?mi)^\((?:defun|defmacro|defgeneric)[ \t]+([^\s()]+)[ \t]*\(`)

// lispSymbolInPackage splits a Lisp symbol token into its bare name and
// reports whether it names a symbol in pkg.
//
// A definition can name its symbol three ways, and they do not mean the
// same thing: a bare name defines a symbol in whatever package is current,
// pkg::name or pkg:name defines one in pkg from anywhere, and other::name
// defines a symbol in some other package that only looks similar. The third
// case is why this cannot be a string trim: axllm::core-get and
// axllm/core::core-get are different functions.
//
// qualified reports whether the token carried a package qualifier, so the
// caller knows whether the current package still matters.
func lispSymbolInPackage(token, pkg string) (name string, inPackage, qualified bool) {
	separator := strings.Index(token, ":")
	if separator < 0 {
		return token, true, false
	}
	qualifier := token[:separator]
	bare := strings.TrimLeft(token[separator:], ":")
	if qualifier == "" || bare == "" {
		// A keyword such as :foo, which is never a function name here.
		return token, false, true
	}
	return bare, strings.EqualFold(qualifier, pkg), true
}

// lispPackageAt reports the package in effect at an offset in a Lisp file,
// by taking the last in-package form before it.
func lispPackageAt(text string, offset int) string {
	current := ""
	for _, match := range lispInPackageRe.FindAllStringSubmatchIndex(text, -1) {
		if match[0] > offset {
			break
		}
		current = strings.ToLower(text[match[2]:match[3]])
	}
	return current
}

// ParseLispLambdaLists reads every top-level function definition in one
// Lisp source file that defines a symbol in pkg, and reports the argument
// counts each one accepts.
//
// This exists because Common Lisp resolves a call at run time: a generated
// call that passes three arguments to a two-argument boundary loads without
// complaint and fails only when that path runs. Compiling the real native
// runtime would catch it, but that needs the package's Lisp dependencies
// installed, so the shapes are read from the source instead and checked
// against the exact argument counts the generated file passes.
//
// The pkg filter is not optional detail. The native sources define the
// public axllm facade and the internal axllm/core boundaries in the same
// directory, and a boundary may be written either inside
// (in-package #:axllm/core) or as (defun axllm/core::core-program-components
// ...) from elsewhere. Reading the token verbatim makes the second form
// look like an undefined boundary, and ignoring packages makes a facade
// function of the same name look like a defined one.
func ParseLispLambdaLists(file, text, pkg string) map[string]LispLambdaShape {
	out := map[string]LispLambdaShape{}
	for _, match := range lispDefinitionHeadRe.FindAllStringSubmatchIndex(text, -1) {
		token := text[match[2]:match[3]]
		name, inPackage, qualified := lispSymbolInPackage(token, pkg)
		if !inPackage {
			continue
		}
		if !qualified && !strings.EqualFold(lispPackageAt(text, match[0]), pkg) {
			// A bare name outside pkg defines a different symbol.
			continue
		}
		// match[1] is the end of the whole match, which is just past the
		// opening paren of the lambda list.
		list, ok := lispBalancedList(text, match[1]-1)
		if !ok {
			continue
		}
		out[name] = lispLambdaShapeOf(name, file, list)
	}
	return out
}

// lispBalancedList returns the text inside the list that opens at start.
func lispBalancedList(text string, start int) (string, bool) {
	if start < 0 || start >= len(text) || text[start] != '(' {
		return "", false
	}
	depth := 0
	inString := false
	for i := start; i < len(text); i++ {
		switch {
		case inString:
			if text[i] == '\\' {
				i++
				continue
			}
			if text[i] == '"' {
				inString = false
			}
		case text[i] == '"':
			inString = true
		case text[i] == ';':
			for i < len(text) && text[i] != '\n' {
				i++
			}
		case text[i] == '(':
			depth++
		case text[i] == ')':
			depth--
			if depth == 0 {
				return text[start+1 : i], true
			}
		}
	}
	return "", false
}

func lispLambdaShapeOf(name, file, list string) LispLambdaShape {
	shape := LispLambdaShape{Name: name, File: file}
	section := "required"
	depth := 0
	token := strings.Builder{}
	flush := func() {
		text := token.String()
		token.Reset()
		if text == "" {
			return
		}
		switch strings.ToLower(text) {
		case "&optional":
			section = "optional"
			return
		case "&rest", "&body":
			shape.Unbounded = true
			section = "rest"
			return
		case "&key":
			shape.Keyword = true
			section = "key"
			return
		case "&aux", "&allow-other-keys", "&environment", "&whole":
			section = "ignored"
			return
		}
		switch section {
		case "required":
			shape.Required++
		case "optional":
			shape.Optional++
		}
	}
	for i := 0; i < len(list); i++ {
		switch list[i] {
		case ';':
			for i < len(list) && list[i] != '\n' {
				i++
			}
		case '(':
			if depth == 0 {
				flush()
				// A grouped parameter such as (fallback :null) counts once;
				// its default value is not a parameter.
				switch section {
				case "required":
					shape.Required++
				case "optional":
					shape.Optional++
				}
			}
			depth++
		case ')':
			depth--
		case ' ', '\t', '\n', '\r':
			if depth == 0 {
				flush()
			}
		default:
			if depth == 0 {
				token.WriteByte(list[i])
			}
		}
	}
	flush()
	return shape
}

// LispBoundaryArityProblem is one native definition whose argument counts
// cannot serve the generated calls.
type LispBoundaryArityProblem struct {
	Boundary string
	File     string
	Accepts  string
	Called   []int
}

func (p LispBoundaryArityProblem) String() string {
	return fmt.Sprintf("%s in %s accepts %s argument(s) but the generated Core calls it with %v",
		p.Boundary, p.File, p.Accepts, p.Called)
}

// CheckLispBoundaryArities compares every declared boundary that a native
// file defines against the argument counts the generated file passes it.
// A mismatch is a real defect that would surface only when that code path
// first runs, so it is worth catching from the source text.
func CheckLispBoundaryArities(manifest LispBoundaryManifest, shapes map[string]LispLambdaShape) []LispBoundaryArityProblem {
	var problems []LispBoundaryArityProblem
	for _, boundary := range manifest.Boundaries {
		shape, ok := shapes[boundary.Name]
		if !ok {
			continue // undefined boundaries are reported separately
		}
		var bad []int
		for _, arity := range boundary.ObservedArities {
			if !shape.Accepts(arity) {
				bad = append(bad, arity)
			}
		}
		if len(bad) > 0 {
			problems = append(problems, LispBoundaryArityProblem{
				Boundary: boundary.Name,
				File:     shape.File,
				Accepts:  shape.Describe(),
				Called:   bad,
			})
		}
	}
	sort.Slice(problems, func(i, j int) bool { return problems[i].Boundary < problems[j].Boundary })
	return problems
}

// LoadLispNativeLambdaShapes reads the lambda lists of every hand-written
// Lisp file in a directory, skipping the generated Core file. Only
// definitions that name a symbol in the generated code's package count,
// whether they are written inside that package or qualified into it.
func LoadLispNativeLambdaShapes(dir, generatedName string) (map[string]LispLambdaShape, int, error) {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil, 0, err
	}
	shapes := map[string]LispLambdaShape{}
	read := 0
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".lisp") || entry.Name() == generatedName {
			continue
		}
		text, err := os.ReadFile(filepath.Join(dir, entry.Name()))
		if err != nil {
			return nil, 0, err
		}
		read++
		for name, shape := range ParseLispLambdaLists(entry.Name(), string(text), LispCorePackage) {
			if _, exists := shapes[name]; !exists {
				shapes[name] = shape
			}
		}
	}
	return shapes, read, nil
}

// ---------------------------------------------------------------------
// Conformance reports
// ---------------------------------------------------------------------

// LispConformanceReportEnv names the absolute, per-run path the native
// conformance runner must write its report to.
//
// The verifier chooses the path and deletes anything already there, so a
// report left behind by an earlier run can never be mistaken for evidence
// about this one. That is why the path is passed in rather than fixed
// inside the package.
const LispConformanceReportEnv = "AXIR_CONFORMANCE_REPORT"

// LispConformanceReportSchema is the report shape this verifier reads.
const LispConformanceReportSchema = "axir-lisp-conformance-v1"

// lispAcceptedCategories are the categories that mean a fixture was
// actually exercised.
//
// Everything else is rejected by name rather than ignored. "skipped",
// "blocked", "failed", "partial" and "explicitly-not-claimed" are all ways
// a run can finish without proving the fixture, and a report carrying one
// must not pass a suite the capability manifest marks semantic.
// "presence-only" is rejected for the same reason the shared coverage
// validator rejects it.
var lispAcceptedCategories = map[string]bool{
	"semantic":           true,
	"validation-error":   true,
	"transport-boundary": true,
}

// LispConformanceFixture is one fixture a run actually dispatched.
type LispConformanceFixture struct {
	// ID is the fixture's path relative to the conformance root, including
	// its suite directory, such as axgen/field-processors.json or
	// axagent-real/agent-runtime-real-javascript-final.json.
	ID string `json:"id"`
	// Category is how the fixture was exercised.
	Category string `json:"category"`
}

// LispConformanceReport is the machine-readable evidence that a declared
// conformance run really executed the fixtures its suites claim.
//
// Exit status alone cannot carry this. A runner that does nothing and exits
// zero is indistinguishable from one that passed everything, while
// BuildLispConformanceCoverage marks every declared suite semantic on the
// strength of the declaration alone. The report closes that gap by naming
// each fixture that ran, and the verifier reconciles those names against
// the fixtures actually on disk.
type LispConformanceReport struct {
	SchemaVersion string                   `json:"schema_version"`
	Fixtures      []LispConformanceFixture `json:"fixtures"`
}

// LoadLispConformanceReport reads the report a conformance run wrote to the
// absolute path the verifier gave it.
func LoadLispConformanceReport(path string) (*LispConformanceReport, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, fmt.Errorf("the conformance run wrote no report to %s=%s; a zero exit status alone does not show that any fixture ran",
				LispConformanceReportEnv, path)
		}
		return nil, err
	}
	var report LispConformanceReport
	if err := json.Unmarshal(data, &report); err != nil {
		return nil, fmt.Errorf("read conformance report %s: %w", path, err)
	}
	return &report, nil
}

// LispSuiteFixtureInventory returns the authoritative fixture set for one
// conformance suite: the fixture files that are actually on disk, as ids
// relative to the conformance root. This is what a report is reconciled
// against, so a runner cannot define its own idea of what a suite holds.
func LispSuiteFixtureInventory(conformanceRoot, suite string) ([]string, error) {
	dir := filepath.Join(conformanceRoot, suite)
	var out []string
	err := filepath.Walk(dir, func(path string, info os.FileInfo, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if info.IsDir() || filepath.Ext(path) != ".json" {
			return nil
		}
		rel, relErr := filepath.Rel(conformanceRoot, path)
		if relErr != nil {
			return relErr
		}
		out = append(out, filepath.ToSlash(rel))
		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("read conformance suite %s: %w", suite, err)
	}
	sort.Strings(out)
	return out, nil
}

// lispRequiredSuites expands a declaration's suites into every suite
// directory the claim actually obliges.
//
// axagent is the one that expands: the agent surface has a separate
// axagent-real tree whose fixtures execute model-authored code through a
// real engine. Claiming axagent while running only the scripted tree would
// leave the real-engine path unverified.
func lispRequiredSuites(declared []string) []string {
	seen := map[string]bool{}
	var out []string
	add := func(suite string) {
		if suite == "" || seen[suite] {
			return
		}
		seen[suite] = true
		out = append(out, suite)
	}
	for _, suite := range declared {
		add(suite)
		if suite == "axagent" {
			add("axagent-real")
		}
	}
	sort.Strings(out)
	return out
}

// LispRequiredConformanceSuites is lispRequiredSuites for callers outside
// this file, so a consumer can see which trees a claim obliges.
func LispRequiredConformanceSuites(declared []string) []string {
	return lispRequiredSuites(declared)
}

// ReconcileLispConformanceReport checks a report against the declaration
// that produced it and against the fixtures on disk. It reports every
// problem it finds rather than the first, so one run names all the work.
//
// A suite passes only when the report accounts for every fixture that
// suite's directory holds, exactly once, with a category that means the
// fixture was exercised.
func ReconcileLispConformanceReport(report *LispConformanceReport, declaration *LispConformanceDeclaration, conformanceRoot string) error {
	if report == nil {
		return fmt.Errorf("no conformance report")
	}
	if declaration == nil {
		return fmt.Errorf("a conformance report was produced without a declaration to reconcile it against")
	}
	var problems []string
	if report.SchemaVersion != LispConformanceReportSchema {
		problems = append(problems, fmt.Sprintf("report schema_version is %q, want %q", report.SchemaVersion, LispConformanceReportSchema))
	}

	required := lispRequiredSuites(declaration.Suites)
	requiredSet := map[string]bool{}
	for _, suite := range required {
		requiredSet[suite] = true
	}

	seen := map[string]bool{}
	accepted := map[string]bool{}
	for _, fixture := range report.Fixtures {
		id := filepath.ToSlash(strings.TrimSpace(fixture.ID))
		if id == "" {
			problems = append(problems, "report has a fixture with an empty id")
			continue
		}
		suite, _, ok := strings.Cut(id, "/")
		if !ok || suite == "" {
			problems = append(problems, fmt.Sprintf("report id %q does not name a suite; ids are relative to the conformance root, such as axgen/field-processors.json", id))
			continue
		}
		if !requiredSet[suite] {
			problems = append(problems, fmt.Sprintf("report records %s, but the declaration does not claim suite %q; a run cannot bank credit for an unclaimed suite", id, suite))
			continue
		}
		if seen[id] {
			problems = append(problems, fmt.Sprintf("report records %s more than once", id))
			continue
		}
		seen[id] = true
		if !lispAcceptedCategories[fixture.Category] {
			problems = append(problems, fmt.Sprintf("report categorises %s as %q, which does not show the fixture was exercised", id, fixture.Category))
			continue
		}
		accepted[id] = true
	}

	for _, suite := range required {
		fixtures, err := LispSuiteFixtureInventory(conformanceRoot, suite)
		if err != nil {
			problems = append(problems, err.Error())
			continue
		}
		if len(fixtures) == 0 {
			problems = append(problems, fmt.Sprintf("suite %q is claimed but holds no fixtures, so the claim proves nothing", suite))
			continue
		}
		known := map[string]bool{}
		var missing []string
		for _, id := range fixtures {
			known[id] = true
			if !accepted[id] {
				missing = append(missing, id)
			}
		}
		if len(missing) > 0 {
			shown := missing
			if len(shown) > 8 {
				shown = append(append([]string(nil), shown[:8]...), fmt.Sprintf("... and %d more", len(missing)-8))
			}
			problems = append(problems, fmt.Sprintf("suite %q is claimed but %d of %d fixture(s) are unaccounted for: %s",
				suite, len(missing), len(fixtures), strings.Join(shown, ", ")))
		}
		for id := range seen {
			if strings.HasPrefix(id, suite+"/") && !known[id] {
				problems = append(problems, fmt.Sprintf("report records %s, which is not a fixture on disk", id))
			}
		}
	}

	if len(problems) > 0 {
		sort.Strings(problems)
		return fmt.Errorf("conformance report does not account for the declared suites:\n    %s", strings.Join(problems, "\n    "))
	}
	return nil
}

// LispConformanceReportContract renders the contract as text, so the native
// runner's author can read it from the compiler rather than from a message.
func LispConformanceReportContract() string {
	var b strings.Builder
	fmt.Fprintf(&b, "The runner named by %s must write its report to the absolute path in %s.\n",
		LispConformanceDeclarationFile, LispConformanceReportEnv)
	b.WriteString("The verifier chooses that path per run and removes any existing file first,\n")
	b.WriteString("so a stale report can never be read as evidence about this run.\n\n")
	b.WriteString("Shape:\n")
	fmt.Fprintf(&b, "  {\"schema_version\": %q,\n", LispConformanceReportSchema)
	b.WriteString("   \"fixtures\": [{\"id\": \"axgen/field-processors.json\", \"category\": \"semantic\"}, ...]}\n\n")
	b.WriteString("  id        the fixture path relative to the conformance root, including its\n")
	b.WriteString("            suite directory; axagent-real/*.json ids are required when axagent\n")
	b.WriteString("            is claimed\n")
	b.WriteString("  category  one of ")
	b.WriteString(strings.Join(sortedStrings(mapKeys(lispAcceptedCategories)), ", "))
	b.WriteString("\n\nReconciliation, all of which must hold:\n")
	b.WriteString("  every fixture on disk in every claimed suite appears exactly once\n")
	b.WriteString("  no record names an unclaimed suite or a fixture that is not on disk\n")
	b.WriteString("  no record carries skipped, blocked, failed, partial, presence-only or\n")
	b.WriteString("    explicitly-not-claimed; those finish a run without proving the fixture\n")
	b.WriteString("  record a fixture only from an actually successful dispatch\n")
	return b.String()
}
