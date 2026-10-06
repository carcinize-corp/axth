package axir

// Common Lisp emission for the experimental signature/schema subset.
//
// This is deliberately NOT a full AxIR target. It emits one file,
// packages/lisp/src/core.lisp, holding the transitive Core closure of four
// root functions (parse_signature, validate_signature, signature_to_string,
// to_json_schema) taken from the same BuildCoreFuncRegistry/BuildCoreBody
// seams every other backend uses. Core keeps the semantics; the generated
// Lisp carries no hand-written behavior, and the native boundaries the
// closure needs live in packages/lisp/src/core-runtime.lisp.
//
// There is no entry in Compile, no capability manifest, and no parity claim:
// a file this emitter cannot express is a build error naming the construct,
// never a silent omission or a placeholder body.

import (
	"fmt"
	"math"
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

// lispCoreRoots is the explicit root set of this subset. Everything else in
// the generated file is reachable from these four by Core calls alone.
var lispCoreRoots = []string{
	"parse_signature",
	"validate_signature",
	"signature_to_string",
	"to_json_schema",
}

// LispCoreRoots returns the root set, for tests and documentation.
func LispCoreRoots() []string {
	return append([]string(nil), lispCoreRoots...)
}

// coreIntrinsicLisp maps every intrinsic this subset is allowed to use to
// its native Lisp boundary in packages/lisp/src/core-runtime.lisp. The map
// is an allowlist, not a fallback: an intrinsic reached by the closure but
// missing here fails generation with the intrinsic name, so a widened Core
// body can never be emitted against a boundary nobody implemented.
var coreIntrinsicLisp = map[CoreIntrinsic]string{
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

// lispArgDefault gives the optional-argument defaults a Core block arg needs
// when the public Core signature takes fewer arguments than the body block
// declares. It mirrors pythonArgDefault, restricted to this subset: only
// to_json_schema has optional arguments here, and an unexpected extra
// argument elsewhere fails emission rather than silently becoming required.
//
// argName is the hyphenated Core argument name WITHOUT the % sigil that the
// emitted binding carries, because this table is keyed by the IR's own
// argument names.
func lispArgDefault(funcName, argName string) string {
	if funcName == "to-json-schema" {
		switch argName {
		case "schema-title":
			return `"Schema"`
		case "options":
			return ":null"
		}
	}
	return ""
}

// lispCLSymbolNames are COMMON-LISP symbol names a generated function must
// not take. Generated locals are sigil-prefixed and so cannot collide, but a
// function name comes straight from the Core registry. The list covers the
// single-word names AxIR naming could plausibly produce; anything it misses
// still fails loudly, because SBCL's package lock refuses to redefine a
// COMMON-LISP symbol when the package is compiled.
var lispCLSymbolNames = map[string]bool{
	"append": true, "apply": true, "array": true, "break": true, "car": true,
	"cdr": true, "class": true, "close": true, "coerce": true, "concatenate": true,
	"cons": true, "count": true, "describe": true, "error": true, "eval": true,
	"every": true, "fill": true, "find": true, "first": true, "format": true,
	"get": true, "hash-table": true, "identity": true, "keywordp": true,
	"last": true, "length": true, "list": true, "load": true, "map": true,
	"mapcar": true, "max": true, "member": true, "merge": true, "min": true,
	"nil": true, "number": true, "parse-integer": true, "pop": true,
	"position": true, "print": true, "push": true, "read": true, "reduce": true,
	"remove": true, "replace": true, "rest": true, "return": true, "reverse": true,
	"search": true, "set": true, "signal": true, "some": true, "sort": true,
	"string": true, "sublis": true, "subst": true, "t": true, "throw": true,
	"type": true, "type-of": true, "union": true, "values": true,
	"vector": true, "warn": true, "write": true,
}

type lispEmitState struct {
	// names maps a Core symbol to the Lisp function name it emits as.
	names map[string]string
	// byEmitted maps a registry native name (the cross-target name) to its
	// Core symbol, so string callees resolve exactly like @refs.
	byEmitted map[string]string
	// fn is the Lisp name of the function being emitted, for return-from.
	fn string
	// depth numbers nested loops so break/continue blocks stay unique.
	depth int
	// runtime records every native boundary the emitted code called, so the
	// generated header lists exactly what core-runtime.lisp must provide.
	runtime map[string]bool
}

func (st *lispEmitState) note(name string) string {
	st.runtime[name] = true
	return name
}

func (st *lispEmitState) calleeName(callee string) (string, error) {
	if strings.HasPrefix(callee, "@") {
		symbol := Symbol(callee)
		name, ok := st.names[symbol]
		if !ok {
			return "", fmt.Errorf("callee @%s is outside the emitted closure", symbol)
		}
		return name, nil
	}
	if strings.HasPrefix(callee, "intrinsic.") {
		target, ok := coreIntrinsicLisp[CoreIntrinsic(callee)]
		if !ok {
			return "", fmt.Errorf("intrinsic %q has no Lisp boundary; add it to coreIntrinsicLisp and implement it in packages/lisp/src/core-runtime.lisp", callee)
		}
		return st.note(target), nil
	}
	// A bare callee is a registry native name; resolve it the same way an
	// @ref resolves so an emitted-name call cannot escape the closure.
	symbol, ok := st.byEmitted[callee]
	if !ok {
		return "", fmt.Errorf("callee %q is neither an intrinsic nor a Core function in the emitted closure", callee)
	}
	name, ok := st.names[symbol]
	if !ok {
		return "", fmt.Errorf("callee %q resolves to @%s, which is outside the emitted closure", callee, symbol)
	}
	return name, nil
}

// LispCoreClosure returns the Core functions this subset emits: the roots
// plus everything they reach, in the registry's deterministic order. It
// fails when a root is missing, when a reached callee has no Core body, or
// when the closure escapes the signature and schema modules, which is the
// boundary this experimental subset claims.
func LispCoreClosure(model AxRuntimeModel) ([]CoreFuncSpec, error) {
	specs, err := BuildCoreFuncRegistry(model)
	if err != nil {
		return nil, err
	}
	bySymbol := make(map[string]CoreFuncSpec, len(specs))
	byEmitted := make(map[string]string, len(specs))
	for _, spec := range specs {
		bySymbol[spec.Symbol] = spec
		byEmitted[spec.Name] = spec.Symbol
	}
	reached := map[string]bool{}
	var visit func(symbol string) error
	visit = func(symbol string) error {
		if reached[symbol] {
			return nil
		}
		spec, ok := bySymbol[symbol]
		if !ok {
			return fmt.Errorf("@%s has no Core body in the registry", symbol)
		}
		if spec.Module != "signature" && spec.Module != "schema" {
			return fmt.Errorf("@%s is in emit module %q; the Lisp subset covers only the signature and schema modules", symbol, spec.Module)
		}
		reached[symbol] = true
		body, err := BuildCoreBody(model.Symbols[symbol])
		if err != nil {
			return fmt.Errorf("@%s: %w", symbol, err)
		}
		var walk func(stmts []CoreStmt) error
		walk = func(stmts []CoreStmt) error {
			for _, stmt := range stmts {
				callee := stmt.Callee
				switch {
				case callee == "":
				case strings.HasPrefix(callee, "@"):
					if err := visit(Symbol(callee)); err != nil {
						return err
					}
				case strings.HasPrefix(callee, "intrinsic."):
					if _, ok := coreIntrinsicLisp[CoreIntrinsic(callee)]; !ok {
						return fmt.Errorf("@%s calls intrinsic %q, which has no Lisp boundary", symbol, callee)
					}
				default:
					target, ok := byEmitted[callee]
					if !ok {
						return fmt.Errorf("@%s calls %q, which is not a Core function", symbol, callee)
					}
					if err := visit(target); err != nil {
						return err
					}
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
		for _, block := range body.Blocks {
			if err := walk(block.Stmts); err != nil {
				return err
			}
		}
		return nil
	}
	for _, root := range lispCoreRoots {
		symbol, ok := byEmitted[root]
		if !ok {
			return nil, fmt.Errorf("Lisp subset root %q is not a Core-bodied function; the root set must match the Core registry", root)
		}
		if err := visit(symbol); err != nil {
			return nil, err
		}
	}
	var out []CoreFuncSpec
	for _, spec := range specs {
		if reached[spec.Symbol] {
			out = append(out, spec)
		}
	}
	return out, nil
}

// BuildLispCore renders packages/lisp/src/core.lisp from the runtime model.
func BuildLispCore(model AxRuntimeModel) (string, error) {
	closure, err := LispCoreClosure(model)
	if err != nil {
		return "", err
	}
	st := &lispEmitState{
		names:     map[string]string{},
		byEmitted: map[string]string{},
		runtime:   map[string]bool{},
	}
	for _, spec := range closure {
		name := LispCoreFuncName(spec.Name)
		if lispCLSymbolNames[name] {
			return "", fmt.Errorf("Core function @%s emits the Lisp name %s, which is a COMMON-LISP symbol; set an emit_name in the IR", spec.Symbol, name)
		}
		for symbol, existing := range st.names {
			if existing == name {
				return "", fmt.Errorf("Core functions @%s and @%s both emit the Lisp name %s", symbol, spec.Symbol, name)
			}
		}
		st.names[spec.Symbol] = name
		st.byEmitted[spec.Name] = spec.Symbol
	}
	var body strings.Builder
	for _, spec := range closure {
		op, ok := model.Symbols[spec.Symbol]
		if !ok {
			return "", fmt.Errorf("missing Core function @%s", spec.Symbol)
		}
		if model.BodySources[spec.Symbol] != "core" {
			return "", fmt.Errorf("Core function @%s is missing body_source=core", spec.Symbol)
		}
		text, err := emitLispCoreFunction(st, op, spec)
		if err != nil {
			return "", err
		}
		body.WriteString(text)
		body.WriteByte('\n')
	}
	header, err := renderLispCoreHeader(closure, st)
	if err != nil {
		return "", err
	}
	return header + body.String(), nil
}

func renderLispCoreHeader(closure []CoreFuncSpec, st *lispEmitState) (string, error) {
	var intrinsics []string
	for _, name := range st.runtimeNames() {
		intrinsics = append(intrinsics, name)
	}
	// Every statement form the emitted bodies used also needs a boundary;
	// collecting both in one list keeps core-runtime.lisp auditable.
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
	b.WriteString(";;;;\n")
	b.WriteString(";;;; EXPERIMENTAL SUBSET. This is not an AxIR language target and makes\n")
	b.WriteString(";;;; no parity claim: it is the transitive Core closure of four roots,\n")
	b.WriteString(";;;; limited to the signature and schema emit modules.\n")
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, ";;;; Root set (%d):\n", len(lispCoreRoots))
	for _, root := range lispCoreRoots {
		fmt.Fprintf(&b, ";;;;   %s\n", LispCoreFuncName(root))
	}
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, ";;;; Transitive closure (%d functions, emitted in this order):\n", len(closure))
	for _, spec := range closure {
		fmt.Fprintf(&b, ";;;;   %-46s module=%-9s @%s:%d\n",
			LispCoreFuncName(spec.Name), spec.Module, spec.Symbol, spec.Line)
	}
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, ";;;; Native boundaries this file calls (%d), all defined in\n", len(intrinsics))
	b.WriteString(";;;; packages/lisp/src/core-runtime.lisp:\n")
	for _, name := range intrinsics {
		fmt.Fprintf(&b, ";;;;   %s\n", name)
	}
	b.WriteString(";;;;\n")
	fmt.Fprintf(&b, "(in-package #:%s)\n\n", LispCorePackage)
	return b.String(), nil
}

func (st *lispEmitState) runtimeNames() []string {
	out := make([]string, 0, len(st.runtime))
	for name := range st.runtime {
		out = append(out, name)
	}
	sort.Strings(out)
	return out
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

func emitLispCoreFunction(st *lispEmitState, op Operation, spec CoreFuncSpec) (string, error) {
	body, err := BuildCoreBody(op)
	if err != nil {
		return "", fmt.Errorf("@%s: %w", op.Symbol, err)
	}
	if len(body.Blocks) != 1 {
		return "", fmt.Errorf("@%s has %d Core body blocks; the Lisp subset emits single-block bodies", op.Symbol, len(body.Blocks))
	}
	block := body.Blocks[0]
	name := st.names[spec.Symbol]
	st.fn = name
	st.depth = 0

	params := make([]string, 0, len(block.Args))
	paramSet := map[string]bool{}
	var lambda []string
	optional := false
	for _, arg := range block.Args {
		argName, err := lispVarName("%" + arg.Name)
		if err != nil {
			return "", fmt.Errorf("@%s: %w", op.Symbol, err)
		}
		params = append(params, argName)
		paramSet[argName] = true
		// lispArgDefault is keyed by the IR's argument name, so look it up
		// with the unsigiled name rather than the emitted binding.
		if def := lispArgDefault(name, strings.ReplaceAll(arg.Name, "_", "-")); def != "" {
			if !optional {
				lambda = append(lambda, "&optional")
				optional = true
			}
			lambda = append(lambda, fmt.Sprintf("(%s %s)", argName, def))
			continue
		}
		if optional {
			return "", fmt.Errorf("@%s: argument %s follows an optional argument without a default", op.Symbol, argName)
		}
		lambda = append(lambda, argName)
	}

	locals, err := collectLispLocals(block.Stmts, paramSet)
	if err != nil {
		return "", fmt.Errorf("@%s: %w", op.Symbol, err)
	}

	stmts, err := emitLispCoreBlock(st, block)
	if err != nil {
		return "", fmt.Errorf("@%s: %w", op.Symbol, err)
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
	}
	if len(locals) > 0 {
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
// emitter knows not to append an unreachable trailing value.
func lispBlockExits(stmts []CoreStmt) bool {
	if len(stmts) == 0 {
		return false
	}
	last := stmts[len(stmts)-1]
	switch last.Kind {
	case "return", "raise":
		return true
	case "if":
		then := firstBodyBlock(last)
		var other CoreBlock
		if len(last.Regions) > 1 && len(last.Regions[1].Blocks) > 0 {
			other = last.Regions[1].Blocks[0]
		}
		return lispBlockExits(then.Stmts) && lispBlockExits(other.Stmts)
	default:
		return false
	}
}

// collectLispLocals gathers every name a Core body assigns, excluding the
// parameters it rebinds. Core rebinds a result name across branches, so the
// emitted function binds all of them once as mutable locals rather than
// trying to express the body as nested lets.
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
		callee, err := st.calleeName(stmt.Callee)
		if err != nil {
			return nil, err
		}
		args := make([]string, 0, len(stmt.Args))
		for _, arg := range stmt.Args {
			text, err := lispLiteral(arg)
			if err != nil {
				return nil, err
			}
			args = append(args, text)
		}
		call := lispCall(callee, args...)
		return lispAssign(stmt.Result, call)
	case "map":
		return lispAssign(stmt.Result, lispCall(st.note("core-new-map")))
	case "list":
		return lispAssign(stmt.Result, lispCall(st.note("core-new-list")))
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
		return lispAssign(stmt.Result, lispCall(st.note("core-get"), target, key, fallback))
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
		return []string{lispCall(st.note("core-set"), target, key, value)}, nil
	case "append":
		target, err := lispLiteral(stmt.Target)
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		return []string{lispCall(st.note("core-append"), target, value)}, nil
	case "string_trim":
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(st.note("core-string-trim"), value))
	case "string_join":
		sep, err := lispAttrValue(stmt.Op, "sep")
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(st.note("core-string-join"), sep, value))
	case "type_is":
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		typeName, err := lispAttrValue(stmt.Op, "type")
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(st.note("core-type-is"), value, typeName))
	case "regex_match":
		pattern, err := lispAttrValue(stmt.Op, "pattern")
		if err != nil {
			return nil, err
		}
		value, err := lispLiteral(stmt.Value)
		if err != nil {
			return nil, err
		}
		return lispAssign(stmt.Result, lispCall(st.note("core-regex-match"), pattern, value))
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
		return nil, fmt.Errorf("core.raise without an error value is outside the Lisp subset (message %q); raise a constructed error instead", stmt.Message)
	case "if":
		return emitLispIf(st, stmt)
	case "for":
		return emitLispFor(st, stmt)
	default:
		return nil, fmt.Errorf("Core op %q is outside the Lisp subset; implement it in lisp_core_emit.go before Core starts using it here", stmt.Op.Name)
	}
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
	lines := []string{fmt.Sprintf("(if %s", lispCall(st.note("core-true-p"), cond))}
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
	st.depth++
	depth := st.depth
	bodyLines, err := emitLispCoreBlock(st, firstBodyBlock(stmt))
	st.depth--
	if err != nil {
		return nil, err
	}
	element := fmt.Sprintf("core-element-%d", depth)
	lines := []string{
		fmt.Sprintf("(dolist (%s %s)", element, lispCall(st.note("core-elements"), iter)),
		fmt.Sprintf("  (setf %s %s)", item, element),
	}
	if len(bodyLines) == 0 {
		lines = append(lines, "  :null")
	}
	for _, line := range bodyLines {
		lines = append(lines, "  "+line)
	}
	lines[len(lines)-1] += ")"
	return lines, nil
}

func lispAssign(result, call string) ([]string, error) {
	if result == "" {
		return []string{call}, nil
	}
	name, err := lispVarName(result)
	if err != nil {
		return nil, err
	}
	return []string{fmt.Sprintf("(setf %s %s)", name, call)}, nil
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
