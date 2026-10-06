package axir

import (
	"fmt"
	"math"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"testing"
)

// These tests guard the experimental Common Lisp subset. The three failures
// worth guarding against are: generated code calling a native boundary
// nobody implemented, generated code binding a COMMON-LISP symbol (which
// SBCL refuses with a package-lock error), and the checked-in file drifting
// from Core. Each has a test below.

func lispTestModel(t *testing.T) AxRuntimeModel {
	t.Helper()
	bundle, err := LoadBundle(rootPath())
	if err != nil {
		t.Fatalf("load core bundle: %v", err)
	}
	if ds := Check(bundle); ds.HasErrors() {
		t.Fatalf("core bundle has errors: %v", ds)
	}
	model, err := BuildRuntimeModel(LowerToCore(bundle))
	if err != nil {
		t.Fatalf("build runtime model: %v", err)
	}
	return model
}

func lispGeneratedPath() string {
	return filepath.Join(repoRootPath(), "packages", "lisp", "src", "core.lisp")
}

func lispRuntimePath() string {
	return filepath.Join(repoRootPath(), "packages", "lisp", "src", "core-runtime.lisp")
}

// TestLispCoreClosureIsRootedAndBounded proves the emitted set is exactly
// the four roots plus what they reach, and that it stays inside the two
// emit modules this subset claims. A closure that silently grew past the
// signature and schema modules would mean the package ships Core it never
// runs and cannot test.
func TestLispCoreClosureIsRootedAndBounded(t *testing.T) {
	model := lispTestModel(t)
	closure, err := LispCoreClosure(model)
	if err != nil {
		t.Fatalf("closure: %v", err)
	}
	if len(closure) == 0 {
		t.Fatal("closure is empty")
	}

	inClosure := map[string]CoreFuncSpec{}
	byEmitted := map[string]string{}
	for _, spec := range closure {
		inClosure[spec.Symbol] = spec
		byEmitted[spec.Name] = spec.Symbol
		if spec.Module != "signature" && spec.Module != "schema" {
			t.Errorf("@%s is in emit module %q; the subset claims only signature and schema", spec.Symbol, spec.Module)
		}
	}
	for _, root := range LispCoreRoots() {
		if _, ok := byEmitted[root]; !ok {
			t.Errorf("root %q is missing from the closure", root)
		}
	}

	// Recompute reachability here rather than trusting LispCoreClosure, so
	// a bug that drops a callee fails instead of shrinking the closure.
	reachable := map[string]bool{}
	var visit func(symbol string)
	visit = func(symbol string) {
		if reachable[symbol] {
			return
		}
		reachable[symbol] = true
		body, err := BuildCoreBody(model.Symbols[symbol])
		if err != nil {
			t.Fatalf("@%s: %v", symbol, err)
		}
		var walk func(stmts []CoreStmt)
		walk = func(stmts []CoreStmt) {
			for _, stmt := range stmts {
				if callee := stmt.Callee; callee != "" && !strings.HasPrefix(callee, "intrinsic.") {
					if strings.HasPrefix(callee, "@") {
						visit(Symbol(callee))
					} else if target, ok := byEmitted[callee]; ok {
						visit(target)
					} else {
						t.Errorf("@%s calls %q, which is outside the closure", symbol, callee)
					}
				}
				for _, region := range stmt.Regions {
					for _, block := range region.Blocks {
						walk(block.Stmts)
					}
				}
			}
		}
		for _, block := range body.Blocks {
			walk(block.Stmts)
		}
	}
	for _, root := range LispCoreRoots() {
		visit(byEmitted[root])
	}
	for symbol := range reachable {
		if _, ok := inClosure[symbol]; !ok {
			t.Errorf("@%s is reachable from a root but is not emitted", symbol)
		}
	}
	for symbol := range inClosure {
		if !reachable[symbol] {
			t.Errorf("@%s is emitted but is not reachable from any root", symbol)
		}
	}
}

// TestLispCoreIntrinsicAllowlistMatchesClosure keeps the intrinsic
// allowlist honest in both directions: a missing entry would mean emitting
// a call into nothing, and a spare entry would mean core-runtime.lisp
// carries a boundary no generated code uses.
func TestLispCoreIntrinsicAllowlistMatchesClosure(t *testing.T) {
	model := lispTestModel(t)
	closure, err := LispCoreClosure(model)
	if err != nil {
		t.Fatalf("closure: %v", err)
	}
	used := map[CoreIntrinsic]bool{}
	for _, spec := range closure {
		body, err := BuildCoreBody(model.Symbols[spec.Symbol])
		if err != nil {
			t.Fatalf("@%s: %v", spec.Symbol, err)
		}
		var walk func(stmts []CoreStmt)
		walk = func(stmts []CoreStmt) {
			for _, stmt := range stmts {
				if strings.HasPrefix(stmt.Callee, "intrinsic.") {
					used[CoreIntrinsic(stmt.Callee)] = true
				}
				for _, region := range stmt.Regions {
					for _, block := range region.Blocks {
						walk(block.Stmts)
					}
				}
			}
		}
		for _, block := range body.Blocks {
			walk(block.Stmts)
		}
	}
	for intrinsic := range used {
		if _, ok := coreIntrinsicLisp[intrinsic]; !ok {
			t.Errorf("intrinsic %s is reached by the closure but has no Lisp boundary", intrinsic)
		}
	}
	for intrinsic := range coreIntrinsicLisp {
		if !used[intrinsic] {
			t.Errorf("intrinsic %s is in the Lisp allowlist but no emitted function calls it; drop the claim", intrinsic)
		}
	}
}

// TestLispCoreEmissionIsDeterministic checks that regenerating produces
// byte-identical output, which is what makes the --check freshness gate
// meaningful.
func TestLispCoreEmissionIsDeterministic(t *testing.T) {
	model := lispTestModel(t)
	first, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("first emission: %v", err)
	}
	for i := 0; i < 3; i++ {
		again, err := BuildLispCore(lispTestModel(t))
		if err != nil {
			t.Fatalf("emission %d: %v", i, err)
		}
		if again != first {
			t.Fatalf("emission %d differs from the first; generated output must be deterministic", i)
		}
	}
}

// TestLispCoreGeneratedFileIsFresh is the same check the lisp-core command
// performs with --check, run as a test so the checked-in file cannot drift
// from Core without a red build.
func TestLispCoreGeneratedFileIsFresh(t *testing.T) {
	want, err := BuildLispCore(lispTestModel(t))
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	have, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	if string(have) != want {
		haveLines := strings.Split(string(have), "\n")
		wantLines := strings.Split(want, "\n")
		for i := 0; i < len(haveLines) || i < len(wantLines); i++ {
			var a, b string
			if i < len(haveLines) {
				a = haveLines[i]
			}
			if i < len(wantLines) {
				b = wantLines[i]
			}
			if a != b {
				t.Fatalf("packages/lisp/src/core.lisp is stale at line %d\n  on disk:   %s\n  generated: %s\nregenerate with: cd tools/axir && go run ./cmd/lisp-core --out ../../packages/lisp/src/core.lisp",
					i+1, a, b)
			}
		}
		t.Fatal("packages/lisp/src/core.lisp is stale")
	}
	if !strings.Contains(string(have), LispCoreMarker) {
		t.Errorf("generated file is missing the source marker %q", LispCoreMarker)
	}
	if !strings.Contains(string(have), "(in-package #:"+LispCorePackage+")") {
		t.Errorf("generated file does not enter the %s package", LispCorePackage)
	}
}

var (
	// A call head never carries the % sigil, because every emitted
	// function name has it stripped and every emitted variable keeps it.
	// That split is what lets this match calls and not binding positions.
	lispFormHeadRe  = regexp.MustCompile(`\(([A-Za-z][A-Za-z0-9*/+<>=-]*)[\s)]`)
	lispLoopVarRe   = regexp.MustCompile(`\(dolist \(([^\s]+)`)
	lispDefunRe     = regexp.MustCompile(`(?m)^\(defun ([^\s(]+)`)
	lispLetBindRe   = regexp.MustCompile(`\(([A-Za-z%][A-Za-z0-9%-]*) :null\)`)
	lispDefunLineRe = regexp.MustCompile(`(?m)^\(defun [^\s(]+ (\(.*)$`)
)

// lispMaskStrings blanks string literals and comments so an audit over the
// generated text cannot be confused by a pattern inside a Core string
// literal, such as a regex containing a parenthesis.
func lispMaskStrings(text string) string {
	out := []byte(text)
	inString := false
	for i := 0; i < len(out); i++ {
		switch {
		case inString:
			if out[i] == '\\' {
				if i+1 < len(out) && out[i+1] != '\n' {
					out[i] = ' '
					out[i+1] = ' '
					i++
				}
				continue
			}
			if out[i] == '"' {
				inString = false
				continue
			}
			out[i] = ' '
		case out[i] == '"':
			inString = true
		case out[i] == ';':
			for i < len(out) && out[i] != '\n' {
				out[i] = ' '
				i++
			}
		}
	}
	return string(out)
}

// TestLispCoreGeneratedCallsAreAllDefined is the audit that matters most.
// Common Lisp resolves a function name at call time, so emitted code that
// calls a boundary nobody wrote loads without complaint and fails only
// when that path runs. This walks every form head in the generated file and
// requires it to be an emitted function, a boundary defined in
// core-runtime.lisp, or one of the few Common Lisp operators the emitter
// uses.
func TestLispCoreGeneratedCallsAreAllDefined(t *testing.T) {
	generated, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	runtime, err := os.ReadFile(lispRuntimePath())
	if err != nil {
		t.Fatalf("read core-runtime.lisp: %v", err)
	}

	defined := map[string]bool{
		// The only Common Lisp operators the emitter produces.
		"defun": true, "declare": true, "ignorable": true, "let": true,
		"if": true, "progn": true, "setf": true, "return-from": true,
		"dolist": true, "error": true, "in-package": true,
		"concatenate": true, "string": true, "code-char": true, "quote": true,
	}
	for _, match := range lispDefunRe.FindAllStringSubmatch(string(generated), -1) {
		defined[match[1]] = true
	}
	for _, match := range lispDefunRe.FindAllStringSubmatch(string(runtime), -1) {
		defined[match[1]] = true
	}
	// Loop element variables are bound by the generated DOLIST itself.
	for _, match := range lispLoopVarRe.FindAllStringSubmatch(string(generated), -1) {
		defined[match[1]] = true
	}
	if len(defined) < 40 {
		t.Fatalf("only found %d definitions; the audit is not reading the sources", len(defined))
	}

	missing := map[string]bool{}
	for _, match := range lispFormHeadRe.FindAllStringSubmatch(lispMaskStrings(string(generated)), -1) {
		if !defined[match[1]] {
			missing[match[1]] = true
		}
	}
	if len(missing) > 0 {
		names := make([]string, 0, len(missing))
		for name := range missing {
			names = append(names, name)
		}
		sort.Strings(names)
		t.Errorf("generated Lisp calls %d name(s) that nothing defines: %s\n  implement them in packages/lisp/src/core-runtime.lisp",
			len(names), strings.Join(names, ", "))
	}

	// Every boundary the header advertises must also exist.
	for _, name := range coreIntrinsicLisp {
		if !defined[name] {
			t.Errorf("intrinsic boundary %s is mapped but is not defined in core-runtime.lisp", name)
		}
	}
}

// TestLispCoreGeneratedBindsNoCommonLispSymbols guards the package-lock
// failure class: a Core value named %string or %error must not emit a
// binding of COMMON-LISP:STRING or COMMON-LISP:ERROR. Every generated
// variable keeps the IR's % sigil, which no COMMON-LISP symbol has.
func TestLispCoreGeneratedBindsNoCommonLispSymbols(t *testing.T) {
	generated, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	text := lispMaskStrings(string(generated))

	bindings := map[string]bool{}
	for _, match := range lispLetBindRe.FindAllStringSubmatch(text, -1) {
		bindings[match[1]] = true
	}
	for _, match := range lispDefunLineRe.FindAllStringSubmatch(text, -1) {
		for _, name := range lispLambdaBindings(match[1]) {
			bindings[name] = true
		}
	}
	if len(bindings) == 0 {
		t.Fatal("found no generated bindings; the audit is not reading the file")
	}
	for name := range bindings {
		if !strings.HasPrefix(name, "%") {
			t.Errorf("generated binding %q has no %% sigil; binding a COMMON-LISP symbol is undefined behavior and SBCL rejects it", name)
		}
	}
	// The loop element variables are the deliberate exception, and they are
	// named so they cannot be COMMON-LISP symbols either.
	for _, name := range lispLoopVarRe.FindAllStringSubmatch(text, -1) {
		if !strings.HasPrefix(name[1], "core-element-") {
			t.Errorf("unexpected loop variable %q", name[1])
		}
	}
}

// lispLambdaBindings returns the names a lambda list binds. A name is either
// a bare token or the first token of an &optional group such as
// (%schema-title "Schema"), so a default value is never mistaken for a
// binding name.
func lispLambdaBindings(lambdaList string) []string {
	// Take the balanced lambda list, which is the text up to the paren that
	// closes the one it opens with.
	depth := 0
	end := -1
	for i := 0; i < len(lambdaList); i++ {
		switch lambdaList[i] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				end = i
			}
		}
		if end >= 0 {
			break
		}
	}
	if end < 0 {
		return nil
	}
	var names []string
	for _, token := range strings.Fields(strings.NewReplacer("(", " ( ", ")", " ) ").Replace(lambdaList[1:end])) {
		switch token {
		case "(", ")", "&optional", "&rest", "&key":
			continue
		}
		if strings.HasPrefix(token, "%") {
			names = append(names, token)
			continue
		}
		// A non-sigilled token here is either a default value, which is not
		// a binding, or a binding that should have carried a sigil. Report
		// only the latter by keeping names that look like identifiers.
		if regexp.MustCompile(`^[A-Za-z][A-Za-z0-9-]*$`).MatchString(token) {
			names = append(names, token)
		}
	}
	return names
}

// TestLispCoreGeneratedHasNoPlaceholderBodies checks that every emitted
// function actually does Core work. A generated function that only returned
// a constant would mean a Core body the emitter quietly failed to express.
func TestLispCoreGeneratedHasNoPlaceholderBodies(t *testing.T) {
	generated, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	forms := strings.Split(string(generated), "\n(defun ")
	if len(forms) < 20 {
		t.Fatalf("found %d generated functions; expected the whole closure", len(forms)-1)
	}
	for _, form := range forms[1:] {
		name := form[:strings.IndexByte(form, ' ')]
		body := lispMaskStrings(form)
		if !strings.Contains(body, "(core-") && !strings.Contains(body, "-impl ") {
			t.Errorf("generated function %s has no Core or boundary call; it looks like a placeholder:\n%s", name, form)
		}
		if strings.Contains(body, "not implemented") || strings.Contains(body, "unsupported") {
			t.Errorf("generated function %s contains a placeholder marker", name)
		}
	}
}

// TestLispCoreRejectsConstructsOutsideTheSubset checks the subset's edges
// fail loudly and by name. The point of an explicit subset is that Core
// growing past it breaks the build instead of emitting something wrong.
func TestLispCoreRejectsConstructsOutsideTheSubset(t *testing.T) {
	cases := []struct {
		name   string
		op     Operation
		expect string
	}{
		{
			name: "unsupported core op",
			op: lispFuncOp("probe", Operation{
				Name: "core.loop", Line: 2,
				Regions: []Region{{Name: "body", Blocks: []Block{{Name: "entry"}}}},
			}),
			expect: "core.loop",
		},
		{
			name: "intrinsic without a boundary",
			op: lispFuncOp("probe", Operation{
				Name: "core.call", Line: 2,
				Attributes: []Attribute{
					{Kind: "attr", Name: "result", Value: "%out"},
					{Kind: "attr", Name: "callee", Value: "intrinsic.math.floor"},
					{Kind: "attr", Name: "args", Values: []interface{}{"%text"}},
				},
			}),
			expect: "intrinsic.math.floor",
		},
		{
			name: "raise without an error value",
			op: lispFuncOp("probe", Operation{
				Name: "core.raise", Line: 2,
				Attributes: []Attribute{{Kind: "attr", Name: "message", Value: "boom"}},
			}),
			expect: "core.raise without an error value",
		},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			st := &lispEmitState{
				names:     map[string]string{"probe": "probe"},
				byEmitted: map[string]string{"probe": "probe"},
				runtime:   map[string]bool{},
			}
			_, err := emitLispCoreFunction(st, testCase.op, CoreFuncSpec{Symbol: "probe", Name: "probe", Module: "signature"})
			if err == nil {
				t.Fatalf("expected emission to fail")
			}
			if !strings.Contains(err.Error(), testCase.expect) {
				t.Fatalf("error %q does not name %q", err, testCase.expect)
			}
		})
	}
}

// lispFuncOp wraps statement ops in the shape BuildCoreBody expects.
func lispFuncOp(symbol string, stmts ...Operation) Operation {
	return Operation{
		Name:   "core.func",
		Symbol: symbol,
		Line:   1,
		Attributes: []Attribute{
			{Kind: "attr", Name: "body_source", Value: "core"},
			{Kind: "attr", Name: "signature", Value: "(string) -> json throws"},
		},
		Regions: []Region{{
			Name: "body",
			Blocks: []Block{{
				Name: "entry",
				Args: []Value{{Name: "text", Type: Type{Name: "string"}}},
				Ops:  stmts,
			}},
		}},
	}
}

// TestLispCoreNameAndLiteralMapping pins the two mappings a reader of the
// generated file depends on: how a Core name becomes a Lisp name, and how a
// Core literal becomes a Lisp literal. The boolean case is the one with
// teeth: YASON:TRUE is a symbol, so an unquoted reference would be an
// unbound variable at run time rather than a compile error.
func TestLispCoreNameAndLiteralMapping(t *testing.T) {
	for _, testCase := range []struct{ in, want string }{
		{"parse_signature", "parse-signature"},
		{"to_json_schema", "to-json-schema"},
		{"_signature_parse_impl", "signature-parse-impl"},
		{"_schema_to_json_schema_impl", "schema-to-json-schema-impl"},
	} {
		if got := LispCoreFuncName(testCase.in); got != testCase.want {
			t.Errorf("LispCoreFuncName(%q) = %q, want %q", testCase.in, got, testCase.want)
		}
	}

	for _, testCase := range []struct {
		in   interface{}
		want string
	}{
		{nil, ":null"},
		{true, "'yason:true"},
		{false, "'yason:false"},
		{"plain", `"plain"`},
		{"%field_name", "%field-name"},
		{QuotedString("%literal"), `"%literal"`},
		{`quote " and \ backslash`, `"quote \" and \\ backslash"`},
		{2, "2"},
		{float64(2), "2"},
		{float64(-40.5), "-40.5d0"},
		{float64(0.1), "0.1d0"},
		{"\tleading tab", `"	leading tab"`},
	} {
		got, err := lispLiteral(testCase.in)
		if err != nil {
			t.Errorf("lispLiteral(%#v): %v", testCase.in, err)
			continue
		}
		if testCase.in == "\tleading tab" {
			// A control character has no Common Lisp string escape, so it is
			// emitted through code-char rather than written into the literal.
			if !strings.Contains(got, "code-char 9") {
				t.Errorf("lispLiteral of a tab = %q; want a code-char form", got)
			}
			continue
		}
		if got != testCase.want {
			t.Errorf("lispLiteral(%#v) = %q, want %q", testCase.in, got, testCase.want)
		}
	}

	if _, err := lispVarName("%x"); err != nil {
		t.Errorf("lispVarName: %v", err)
	}
	if name, _ := lispVarName("%string"); name != "%string" {
		t.Errorf("a Core value named %%string must keep its sigil, got %q", name)
	}
}

// TestLispCoreFloatLiteralsAreDoubles guards a silent-divergence bug. Common
// Lisp reads an unsuffixed 0.1 as a SINGLE-FLOAT, and (= 0.1 0.1d0) is
// false, so a bare decimal literal would make this port disagree with every
// other Ax port by about 1e-9 while still compiling and still passing any
// test that only checks integers. Every non-integral literal must therefore
// carry the d exponent marker.
func TestLispCoreFloatLiteralsAreDoubles(t *testing.T) {
	for _, testCase := range []struct {
		in   float64
		want string
	}{
		// Integral and exactly representable: an integer literal, so Core
		// arithmetic and emitted JSON read 18 rather than 18.0.
		{0, "0"},
		{2, "2"},
		{-7, "-7"},
		{18, "18"},
		{1e15, "1000000000000000"},
		// Non-integral: a double literal, in readable plain form.
		{-40.5, "-40.5d0"},
		{0.1, "0.1d0"},
		{60.25, "60.25d0"},
		{0.3333333333333333, "0.3333333333333333d0"},
		// Large and tiny magnitudes fall back to exponent form rather than
		// a wall of zeroes, and still carry the d marker.
		{1e21, "1d21"},
		{1.5e-9, "1.5d-9"},
		{maxExactIntegralFloat * 4, "3.602879701896397d16"},
	} {
		got, err := lispFloatLiteral(testCase.in)
		if err != nil {
			t.Errorf("lispFloatLiteral(%v): %v", testCase.in, err)
			continue
		}
		if got != testCase.want {
			t.Errorf("lispFloatLiteral(%v) = %q, want %q", testCase.in, got, testCase.want)
		}
		// Whatever form was chosen must name the value exactly. A literal
		// that reads back as a different number is the bug this test exists
		// for, so the round trip is checked rather than assumed.
		readable := strings.Replace(strings.TrimSuffix(got, "d0"), "d", "e", 1)
		back, parseErr := strconv.ParseFloat(readable, 64)
		if parseErr != nil {
			t.Errorf("lispFloatLiteral(%v) = %q, which does not parse back: %v", testCase.in, got, parseErr)
			continue
		}
		if back != testCase.in {
			t.Errorf("lispFloatLiteral(%v) = %q, which reads back as %v", testCase.in, got, back)
		}
	}

	// A float with no Lisp representation must fail rather than emit
	// something the reader would reject.
	for _, bad := range []float64{math.NaN(), math.Inf(1), math.Inf(-1)} {
		if _, err := lispFloatLiteral(bad); err == nil {
			t.Errorf("lispFloatLiteral(%v) should fail", bad)
		}
	}

	// Past the exact-integer range an integral float must not become an
	// integer literal: int64 conversion there is undefined in Go.
	huge, err := lispFloatLiteral(1e300)
	if err != nil {
		t.Fatalf("lispFloatLiteral(1e300): %v", err)
	}
	if !strings.Contains(huge, "d") {
		t.Errorf("lispFloatLiteral(1e300) = %q; a value past the exact-integer range must stay a double", huge)
	}
}

// TestLispCoreOptionalArgumentsAreEmitted guards the other half of the same
// class of bug. The defaults table is keyed by the IR's argument name, but
// the emitted binding carries a % sigil; looking the table up with the
// sigilled name silently matched nothing, so to-json-schema took three
// required arguments instead of one required and two optional.
func TestLispCoreOptionalArgumentsAreEmitted(t *testing.T) {
	if got := lispArgDefault("to-json-schema", "schema-title"); got != `"Schema"` {
		t.Errorf(`lispArgDefault("to-json-schema", "schema-title") = %q, want "\"Schema\""`, got)
	}
	if got := lispArgDefault("to-json-schema", "options"); got != ":null" {
		t.Errorf(`lispArgDefault("to-json-schema", "options") = %q, want ":null"`, got)
	}
	if got := lispArgDefault("to-json-schema", "%schema-title"); got != "" {
		t.Errorf("the defaults table must be keyed by the unsigilled name, but %%schema-title matched %q", got)
	}

	generated, err := BuildLispCore(lispTestModel(t))
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	const want = `(defun to-json-schema (%fields &optional (%schema-title "Schema") (%options :null))`
	if !strings.Contains(generated, want) {
		start := strings.Index(generated, "(defun to-json-schema")
		actual := "not found"
		if start >= 0 {
			actual = generated[start : start+strings.IndexByte(generated[start:], '\n')]
		}
		t.Errorf("generated to-json-schema does not take its optional arguments\n  want: %s\n  got:  %s", want, actual)
	}
}

// TestLispCoreRejectsCommonLispFunctionNames checks the function-name guard,
// since a function name comes straight from the Core registry and a name
// such as "string" would make SBCL refuse to compile the package.
func TestLispCoreRejectsCommonLispFunctionNames(t *testing.T) {
	for _, name := range []string{"string", "error", "length", "list", "map", "append", "remove"} {
		if !lispCLSymbolNames[name] {
			t.Errorf("%q should be treated as a COMMON-LISP symbol name", name)
		}
	}
	for _, name := range []string{"parse-signature", "to-json-schema", "signature-parse-impl", "schema-field-schema-impl"} {
		if lispCLSymbolNames[name] {
			t.Errorf("%q is a real emitted name and must not be rejected", name)
		}
	}
}

// TestLispCoreHeaderDocumentsTheSubset checks the generated header carries
// what a reader needs: the marker, the roots, the whole closure with its IR
// provenance, and the boundary list. The header is how someone finds out
// this is a subset rather than a full target.
func TestLispCoreHeaderDocumentsTheSubset(t *testing.T) {
	model := lispTestModel(t)
	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	header := generated[:strings.Index(generated, "(in-package")]
	for _, want := range []string{
		LispCoreMarker,
		"EXPERIMENTAL SUBSET",
		"go run ./cmd/lisp-core --check",
		"ir/axcore",
	} {
		if !strings.Contains(header, want) {
			t.Errorf("generated header is missing %q", want)
		}
	}
	closure, err := LispCoreClosure(model)
	if err != nil {
		t.Fatalf("closure: %v", err)
	}
	for _, spec := range closure {
		entry := fmt.Sprintf("@%s:%d", spec.Symbol, spec.Line)
		if !strings.Contains(header, entry) {
			t.Errorf("generated header does not record %s", entry)
		}
	}
	for _, root := range LispCoreRoots() {
		if !strings.Contains(header, LispCoreFuncName(root)) {
			t.Errorf("generated header does not list root %q", root)
		}
	}
}
