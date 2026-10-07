package axir

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
	"testing"
)

// These tests guard the Common Lisp emission of the whole Core registry.
// The failure classes worth guarding against are: a Core function silently
// not emitted, generated code calling a boundary nobody declared, generated
// code binding a COMMON-LISP symbol (which SBCL refuses with a package-lock
// error), a control-flow form lowered to something that does not mean what
// Core meant, and the checked-in files drifting from Core. Each has a test
// below, and the control-flow and whole-file checks run in real SBCL.

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

func lispManifestPath() string {
	return filepath.Join(repoRootPath(), "packages", "lisp", "src", "core-boundaries.json")
}

func lispRuntimePath() string {
	return filepath.Join(repoRootPath(), "packages", "lisp", "src", "core-runtime.lisp")
}

func lispTestManifest(t *testing.T) LispBoundaryManifest {
	t.Helper()
	text, err := BuildLispCoreBoundaryManifest(lispTestModel(t))
	if err != nil {
		t.Fatalf("manifest: %v", err)
	}
	var manifest LispBoundaryManifest
	if err := json.Unmarshal([]byte(text), &manifest); err != nil {
		t.Fatalf("manifest is not valid JSON: %v", err)
	}
	return manifest
}

// TestLispCoreEmitsTheWholeRegistry is the test that defines this target's
// scope. The Lisp port claims the full Core registry, so every Core-bodied
// symbol in the model must be emitted exactly once; a function quietly left
// out is the failure this catches.
func TestLispCoreEmitsTheWholeRegistry(t *testing.T) {
	model := lispTestModel(t)
	registry, err := BuildCoreFuncRegistry(model)
	if err != nil {
		t.Fatalf("registry: %v", err)
	}
	emitted, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	if len(emitted) != len(registry) {
		t.Fatalf("emitting %d of %d Core functions; the Lisp target claims the whole registry", len(emitted), len(registry))
	}

	// Recompute the expected set from BodySources rather than trusting the
	// registry call above, so a registry bug that drops a symbol fails here
	// instead of shrinking both sides of the comparison together.
	want := map[string]bool{}
	for symbol, source := range model.BodySources {
		if source == "core" {
			want[symbol] = true
		}
	}
	if len(want) == 0 {
		t.Fatal("the model declares no Core-bodied symbols; the test is not reading it")
	}
	got := map[string]int{}
	for _, spec := range emitted {
		got[spec.Symbol]++
	}
	for symbol := range want {
		switch got[symbol] {
		case 1:
		case 0:
			t.Errorf("@%s has a Core body but is not emitted", symbol)
		default:
			t.Errorf("@%s is emitted %d times", symbol, got[symbol])
		}
	}
	for symbol := range got {
		if !want[symbol] {
			t.Errorf("@%s is emitted but has no Core body", symbol)
		}
	}

	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	for _, spec := range emitted {
		if !strings.Contains(generated, "\n(defun "+LispCoreFuncName(spec.Name)+" (") {
			t.Errorf("@%s (%s) has no generated defun", spec.Symbol, LispCoreFuncName(spec.Name))
		}
	}
}

// TestLispCoreModuleOrderIsDeterministic checks the generated file is laid
// out by emit module in dependency rank order, with every function of a
// module together. That ordering is what makes the file reviewable and the
// byte-level freshness check meaningful.
func TestLispCoreModuleOrderIsDeterministic(t *testing.T) {
	model := lispTestModel(t)
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	var moduleOrder []string
	seen := map[string]bool{}
	for i, spec := range specs {
		if !seen[spec.Module] {
			seen[spec.Module] = true
			moduleOrder = append(moduleOrder, spec.Module)
		} else if specs[i-1].Module != spec.Module {
			t.Fatalf("module %q resumes at index %d after module %q; a module's functions must be contiguous",
				spec.Module, i, specs[i-1].Module)
		}
	}
	for i := 1; i < len(moduleOrder); i++ {
		prev, next := moduleOrder[i-1], moduleOrder[i]
		if coreModuleRank[prev] > coreModuleRank[next] {
			t.Errorf("module %q (rank %d) is emitted before %q (rank %d); modules must follow dependency rank",
				prev, coreModuleRank[prev], next, coreModuleRank[next])
		}
	}

	// The same order must show up in the file itself, not only in the spec
	// list, and the header's module summary must agree with it.
	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	at := -1
	for _, module := range moduleOrder {
		marker := fmt.Sprintf(";;; emit module %s (", module)
		index := strings.Index(generated, marker)
		if index < 0 {
			t.Fatalf("generated file has no section for module %q", module)
		}
		if index < at {
			t.Errorf("module %q section is out of order in the generated file", module)
		}
		at = index
	}
}

// TestLispCoreIntrinsicNameContract pins the naming rule the whole port
// depends on: a boundary name is the Python helper name with its leading
// underscore dropped and its underscores hyphenated. The two examples
// agreed with the parent thread are checked by name, because they are the
// two shapes the rule has to get right: a _core_-prefixed helper and a
// bare private helper.
func TestLispCoreIntrinsicNameContract(t *testing.T) {
	for _, testCase := range []struct {
		intrinsic CoreIntrinsic
		want      string
	}{
		{IntrinsicAIStreamOpen, "core-ai-stream-open"},
		{IntrinsicValidImage, "valid-image"},
		{IntrinsicStringSplitTrim, "core-string-split-trim-nonempty"},
		{IntrinsicMathIsFinite, "core-math-is-finite"},
	} {
		got, err := LispIntrinsicName(testCase.intrinsic)
		if err != nil {
			t.Errorf("LispIntrinsicName(%s): %v", testCase.intrinsic, err)
			continue
		}
		if got != testCase.want {
			t.Errorf("LispIntrinsicName(%s) = %q, want %q", testCase.intrinsic, got, testCase.want)
		}
	}

	// An intrinsic with no Python helper must fail rather than emit a call
	// into a name nobody can implement.
	if _, err := LispIntrinsicName(CoreIntrinsic("intrinsic.not.a.thing")); err == nil {
		t.Error("an intrinsic with no Python helper must not get a derived Lisp name")
	}

	// Every intrinsic the Python port names must resolve, so the two ports
	// can never disagree about which intrinsics have a boundary.
	for intrinsic := range coreIntrinsicPython {
		if _, err := LispIntrinsicName(intrinsic); err != nil {
			t.Errorf("%s has no Lisp boundary name: %v", intrinsic, err)
		}
	}

	// The invariant that matters at generation time: every intrinsic any
	// Core body actually calls must resolve. An intrinsic Core declares but
	// nothing calls needs no name in any port, and Python has none for it
	// either.
	model := lispTestModel(t)
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	called := 0
	for _, spec := range specs {
		body, err := BuildCoreBody(model.Symbols[spec.Symbol])
		if err != nil {
			t.Fatalf("@%s: %v", spec.Symbol, err)
		}
		var walk func(stmts []CoreStmt)
		walk = func(stmts []CoreStmt) {
			for _, stmt := range stmts {
				if strings.HasPrefix(stmt.Callee, "intrinsic.") {
					called++
					if _, err := LispIntrinsicName(CoreIntrinsic(stmt.Callee)); err != nil {
						t.Errorf("@%s calls %s, which has no Lisp boundary name: %v", spec.Symbol, stmt.Callee, err)
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
	if called == 0 {
		t.Fatal("no Core body calls an intrinsic; the walk is not reading the model")
	}
}

// TestLispCoreIntrinsicNamesAreStable is the regression guard for the names
// the signature/schema subset already shipped. LispIntrinsicName derives
// names from the Python table, so a rename there would otherwise silently
// rename a boundary that core-runtime.lisp already defines and leave the
// generated call pointing at nothing.
func TestLispCoreIntrinsicNamesAreStable(t *testing.T) {
	if len(lispIntrinsicLegacyNames) < 30 {
		t.Fatalf("only %d legacy names pinned; the table is not the shipped set", len(lispIntrinsicLegacyNames))
	}
	for intrinsic, want := range lispIntrinsicLegacyNames {
		got, err := LispIntrinsicName(intrinsic)
		if err != nil {
			t.Errorf("%s: %v", intrinsic, err)
			continue
		}
		if got != want {
			t.Errorf("%s now maps to %q but core-runtime.lisp defines %q; preserve the shipped name or rename the native boundary deliberately",
				intrinsic, got, want)
		}
	}
	// The deliberate departures from the rule are listed, not incidental.
	if got := lispIntrinsicNameExceptions[IntrinsicObjectCallMethod]; got != "core-host-call" {
		t.Errorf("intrinsic.object.call_method must use the host object protocol name core-host-call, got %q", got)
	}
	for intrinsic := range lispIntrinsicNameExceptions {
		if _, legacy := lispIntrinsicLegacyNames[intrinsic]; legacy {
			t.Errorf("%s is both a shipped name and an exception; one of them is wrong", intrinsic)
		}
	}
}

// TestLispCoreHostObjectProtocol checks the one place the emitter reshapes
// a Core call. Core's intrinsic.object.call_method is variadic, but the
// host object protocol takes the method arguments as a single Core array,
// so the generated call must always carry exactly three arguments.
func TestLispCoreHostObjectProtocol(t *testing.T) {
	manifest := lispTestManifest(t)
	var entry *LispBoundaryEntry
	for i := range manifest.Boundaries {
		if manifest.Boundaries[i].Name == "core-host-call" {
			entry = &manifest.Boundaries[i]
		}
	}
	if entry == nil {
		t.Fatal("core-host-call is not in the manifest; nothing calls the host object protocol")
	}
	if want := []int{3}; len(entry.ObservedArities) != 1 || entry.ObservedArities[0] != want[0] {
		t.Errorf("core-host-call is called with arities %v; the protocol is (target method args-vector), always 3", entry.ObservedArities)
	}
	if entry.CallSites == 0 {
		t.Error("core-host-call has no call sites")
	}

	generated, err := BuildLispCore(lispTestModel(t))
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	// Core calls it with one and with two trailing arguments; both must
	// arrive as one vector rather than spread across the lambda list.
	if !strings.Contains(generated, "(core-host-call ") {
		t.Fatal("no generated call to core-host-call")
	}
	for _, call := range regexp.MustCompile(`\(core-host-call [^\n]*`).FindAllString(generated, -1) {
		if !strings.Contains(call, "(vector ") {
			t.Errorf("generated host call does not pack its arguments into a vector: %s", call)
		}
	}
}

// TestLispCoreEmissionIsDeterministic checks that regenerating produces
// byte-identical output, which is what makes the --check freshness gate
// meaningful.
func TestLispCoreEmissionIsDeterministic(t *testing.T) {
	model := lispTestModel(t)
	firstCore, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("first emission: %v", err)
	}
	firstManifest, err := BuildLispCoreBoundaryManifest(model)
	if err != nil {
		t.Fatalf("first manifest: %v", err)
	}
	for i := 0; i < 3; i++ {
		fresh := lispTestModel(t)
		againCore, err := BuildLispCore(fresh)
		if err != nil {
			t.Fatalf("emission %d: %v", i, err)
		}
		if againCore != firstCore {
			t.Fatalf("core emission %d differs from the first; generated output must be deterministic", i)
		}
		againManifest, err := BuildLispCoreBoundaryManifest(fresh)
		if err != nil {
			t.Fatalf("manifest %d: %v", i, err)
		}
		if againManifest != firstManifest {
			t.Fatalf("manifest emission %d differs from the first; generated output must be deterministic", i)
		}
	}
}

// TestLispCoreGeneratedFilesAreFresh is the same check the lisp-core command
// performs with --check, run as a test so the checked-in files cannot drift
// from Core without a red build.
func TestLispCoreGeneratedFilesAreFresh(t *testing.T) {
	model := lispTestModel(t)
	wantCore, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	wantManifest, err := BuildLispCoreBoundaryManifest(model)
	if err != nil {
		t.Fatalf("manifest: %v", err)
	}
	for _, testCase := range []struct{ path, want, flag string }{
		{lispGeneratedPath(), wantCore, "--out ../../packages/lisp/src/core.lisp"},
		{lispManifestPath(), wantManifest, "--manifest ../../packages/lisp/src/core-boundaries.json"},
	} {
		have, err := os.ReadFile(testCase.path)
		if err != nil {
			t.Fatalf("read %s: %v", testCase.path, err)
		}
		if string(have) == testCase.want {
			continue
		}
		haveLines := strings.Split(string(have), "\n")
		wantLines := strings.Split(testCase.want, "\n")
		for i := 0; i < len(haveLines) || i < len(wantLines); i++ {
			var a, b string
			if i < len(haveLines) {
				a = haveLines[i]
			}
			if i < len(wantLines) {
				b = wantLines[i]
			}
			if a != b {
				t.Fatalf("%s is stale at line %d\n  on disk:   %s\n  generated: %s\nregenerate with: cd tools/axir && go run ./cmd/lisp-core %s",
					testCase.path, i+1, a, b, testCase.flag)
			}
		}
		t.Fatalf("%s is stale", testCase.path)
	}

	have, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	for _, want := range []string{
		LispCoreMarker,
		"(in-package #:" + LispCorePackage + ")",
		";;;; " + provenanceBeginFunctions,
		";;;; " + provenanceEndFunctions,
		";;;; " + provenanceBeginDeclarations,
		";;;; " + provenanceEndDeclarations,
	} {
		if !strings.Contains(string(have), want) {
			t.Errorf("generated file is missing %q", want)
		}
	}
}

// TestLispCoreProvenanceRegionsBracketEverything checks the markers the
// repo's provenance audit reads actually enclose what they claim: every
// generated defun inside the functions region and nothing outside it, and
// every declaim inside the declarations region.
func TestLispCoreProvenanceRegionsBracketEverything(t *testing.T) {
	model := lispTestModel(t)
	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	functions, err := lispRegion(generated, provenanceBeginFunctions, provenanceEndFunctions)
	if err != nil {
		t.Fatal(err)
	}
	declarations, err := lispRegion(generated, provenanceBeginDeclarations, provenanceEndDeclarations)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(declarations, "(defun ") {
		t.Error("the declarations region contains a definition; declarations must declare names only")
	}
	if strings.Contains(functions, "(declaim ") {
		t.Error("the functions region contains a declaim; declarations belong in their own region")
	}
	outside := strings.Replace(generated, functions, "", 1)
	if count := strings.Count(outside, "\n(defun "); count != 0 {
		t.Errorf("%d generated defun(s) sit outside the emitted-functions region", count)
	}
	if count := strings.Count(functions, "\n(defun "); count != len(specs) {
		t.Errorf("the emitted-functions region holds %d defuns, want %d", count, len(specs))
	}
}

func lispRegion(text, begin, end string) (string, error) {
	start := strings.Index(text, ";;;; "+begin)
	stop := strings.Index(text, ";;;; "+end)
	if start < 0 || stop < 0 || stop < start {
		return "", fmt.Errorf("generated file does not bracket %q ... %q", begin, end)
	}
	return text[start : stop+len(";;;; "+end)], nil
}

// TestLispCoreForwardDeclarationsCoverEveryName checks the forward
// declarations are complete and are declarations only. A missing entry
// would make SBCL note an undefined function for a name that does exist;
// a spare entry would declare something nothing calls.
func TestLispCoreForwardDeclarationsCoverEveryName(t *testing.T) {
	model := lispTestModel(t)
	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	declarations, err := lispRegion(generated, provenanceBeginDeclarations, provenanceEndDeclarations)
	if err != nil {
		t.Fatal(err)
	}
	declared := map[string]bool{}
	for _, line := range strings.Split(declarations, "\n") {
		trimmed := strings.TrimSpace(strings.TrimRight(line, ")"))
		if trimmed == "" || strings.HasPrefix(trimmed, ";") || strings.HasPrefix(trimmed, "(") {
			continue
		}
		declared[trimmed] = true
	}
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	for _, spec := range specs {
		if !declared[LispCoreFuncName(spec.Name)] {
			t.Errorf("emitted function %s has no forward declaration", LispCoreFuncName(spec.Name))
		}
	}
	manifest := lispTestManifest(t)
	for _, boundary := range manifest.Boundaries {
		if !declared[boundary.Name] {
			t.Errorf("native boundary %s has no forward declaration", boundary.Name)
		}
	}
	if want := len(specs) + len(manifest.Boundaries); len(declared) != want {
		t.Errorf("%d names are declared, want %d (%d functions + %d boundaries)",
			len(declared), want, len(specs), len(manifest.Boundaries))
	}

	// The optional arguments must survive into the declared type, or SBCL
	// would reject a call that relies on a default.
	if !strings.Contains(declarations, "(function (t &optional t t) t)") {
		t.Error("no declaration carries a one-required two-optional shape; to-json-schema needs one")
	}
}

var (
	// A call head never carries the % sigil, because every emitted
	// function name has it stripped and every emitted variable keeps it.
	// That split is what lets this match calls and not binding positions.
	lispFormHeadRe  = regexp.MustCompile(`\(([A-Za-z][A-Za-z0-9*/+<>=-]*)[\s)]`)
	lispLoopVarRe   = regexp.MustCompile(`\(dolist \(([^\s]+)`)
	lispBlockNameRe = regexp.MustCompile(`\(block ([^\s)]+)`)
	lispDefunRe     = regexp.MustCompile(`(?m)^\(defun ([^\s(]+)`)
	lispLetBindRe   = regexp.MustCompile(`\(([A-Za-z%][A-Za-z0-9%-]*) :null\)`)
	lispDefunLineRe = regexp.MustCompile(`(?m)^\(defun [^\s(]+ (\(.*)$`)
	lispHandlerVar  = regexp.MustCompile(`\(error \((%[^\s)]+)\)`)
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

// TestLispCoreGeneratedCallsAreAllDeclared is the audit that matters most.
// Common Lisp resolves a function name at call time, so emitted code that
// calls a boundary nobody declared loads without complaint and fails only
// when that path runs. This walks every form head in the generated file and
// requires it to be an emitted function, a boundary the manifest declares,
// a block the file itself establishes, or one of the Common Lisp operators
// the emitter uses.
func TestLispCoreGeneratedCallsAreAllDeclared(t *testing.T) {
	generated, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	manifest := lispTestManifest(t)

	defined := map[string]bool{
		// T appears inside the FTYPE declarations' argument lists.
		"t": true,
	}
	for _, name := range lispEmitterOperators {
		defined[name] = true
	}
	for _, function := range manifest.Functions {
		defined[function.Name] = true
	}
	for _, boundary := range manifest.Boundaries {
		defined[boundary.Name] = true
	}
	text := lispMaskStrings(string(generated))
	// Loop element variables and loop/iteration blocks are established by
	// the generated forms themselves.
	for _, match := range lispLoopVarRe.FindAllStringSubmatch(text, -1) {
		defined[match[1]] = true
	}
	for _, match := range lispBlockNameRe.FindAllStringSubmatch(text, -1) {
		defined[match[1]] = true
	}
	// Derive the floor from the manifest rather than hard-coding a count:
	// the registry grows as Core grows, and a hard number would either
	// break on growth or stop catching a shrink.
	if want := len(manifest.Functions) + len(manifest.Boundaries); len(defined) < want {
		t.Fatalf("only found %d names, want at least %d; the audit is not reading the sources", len(defined), want)
	}

	missing := map[string]bool{}
	for _, match := range lispFormHeadRe.FindAllStringSubmatch(text, -1) {
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
		t.Errorf("generated Lisp calls %d name(s) that nothing declares: %s", len(names), strings.Join(names, ", "))
	}

	// Every manifest boundary must actually be called, or the manifest
	// would ask for native code nothing needs.
	for _, boundary := range manifest.Boundaries {
		if !strings.Contains(text, "("+boundary.Name+" ") && !strings.Contains(text, "("+boundary.Name+")") {
			t.Errorf("manifest declares boundary %s but the generated file never calls it", boundary.Name)
		}
		if len(boundary.Callers) == 0 {
			t.Errorf("manifest boundary %s records no callers", boundary.Name)
		}
		if boundary.CallSites == 0 {
			t.Errorf("manifest boundary %s records no call sites", boundary.Name)
		}
		for _, caller := range boundary.Callers {
			if !defined[caller] {
				t.Errorf("manifest boundary %s names caller %s, which is not an emitted function", boundary.Name, caller)
			}
		}
	}
}

// lispSignatureSchemaBoundariesPending is the ratchet for the signature and
// schema surface. It is empty: every boundary those two modules reach is
// now defined natively, so any new gap is a regression rather than
// outstanding work. An entry here would be a temporary, named exemption.
var lispSignatureSchemaBoundariesPending = map[string]bool{}

// TestLispCoreSignatureSchemaBoundariesAreImplemented preserves the
// guarantee the shipped subset already had: every boundary the signature
// and schema surface reaches is defined in core-runtime.lisp, except the
// ones explicitly listed above as still being written. The rest of the
// registry belongs to the runtime and subsystem owners, and
// `lisp-core --verify-runtime` is the gate for those.
func TestLispCoreSignatureSchemaBoundariesAreImplemented(t *testing.T) {
	// Use the same lambda-list parser the arity check uses, so the two
	// cannot disagree about what the native sources define. It covers
	// defgeneric as well, which the host object protocol needs.
	shapes, read, err := LoadLispNativeLambdaShapes(filepath.Dir(lispRuntimePath()), filepath.Base(lispGeneratedPath()))
	if err != nil {
		t.Fatalf("read native sources: %v", err)
	}
	if read == 0 {
		t.Fatal("found no native Lisp files; the audit is not reading them")
	}
	defined := map[string]bool{}
	for name := range shapes {
		defined[name] = true
	}
	if len(defined) < 40 {
		t.Fatalf("found only %d native definitions across %d file(s); the audit is not reading them", len(defined), read)
	}

	manifest := lispTestManifest(t)
	shipped := map[string]bool{}
	for _, function := range manifest.Functions {
		if function.Module == "signature" || function.Module == "schema" {
			shipped[function.Name] = true
		}
	}
	if len(shipped) == 0 {
		t.Fatal("the manifest lists no signature or schema functions")
	}
	checked := 0
	for _, boundary := range manifest.Boundaries {
		reached := false
		for _, caller := range boundary.Callers {
			if shipped[caller] {
				reached = true
				break
			}
		}
		if !reached {
			continue
		}
		checked++
		switch {
		case defined[boundary.Name] && lispSignatureSchemaBoundariesPending[boundary.Name]:
			// The ratchet can be tightened; this is progress, not a fault,
			// so it is reported rather than failed.
			t.Logf("boundary %s is now defined; remove it from lispSignatureSchemaBoundariesPending", boundary.Name)
		case !defined[boundary.Name] && !lispSignatureSchemaBoundariesPending[boundary.Name]:
			t.Errorf("boundary %s is reached from the signature/schema surface but core-runtime.lisp does not define it, and it is not in the pending list",
				boundary.Name)
		}
	}
	if checked < 30 {
		t.Fatalf("only %d boundaries are reached from signature/schema; the audit is not resolving callers", checked)
	}
	// The pending list must describe reality: a name nothing reaches would
	// be dead bookkeeping.
	reached := map[string]bool{}
	for _, boundary := range manifest.Boundaries {
		for _, caller := range boundary.Callers {
			if shipped[caller] {
				reached[boundary.Name] = true
				break
			}
		}
	}
	for name := range lispSignatureSchemaBoundariesPending {
		if !reached[name] {
			t.Errorf("%s is in the pending list but the signature/schema surface does not reach it; drop the entry", name)
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
	for _, match := range lispHandlerVar.FindAllStringSubmatch(text, -1) {
		bindings[match[1]] = true
	}
	if len(bindings) == 0 {
		t.Fatal("found no generated bindings; the audit is not reading the file")
	}
	for name := range bindings {
		if !strings.HasPrefix(name, "%") {
			t.Errorf("generated binding %q has no %% sigil; binding a COMMON-LISP symbol is undefined behavior and SBCL rejects it", name)
		}
	}
	// Loop element variables are the deliberate exception, and they are
	// named so they cannot be COMMON-LISP symbols either.
	for _, name := range lispLoopVarRe.FindAllStringSubmatch(text, -1) {
		if !strings.HasPrefix(name[1], "core-element-") {
			t.Errorf("unexpected loop variable %q", name[1])
		}
	}
	// So are the loop and iteration blocks.
	for _, name := range lispBlockNameRe.FindAllStringSubmatch(text, -1) {
		if !strings.HasPrefix(name[1], "core-loop-") && !strings.HasPrefix(name[1], "core-iteration-") {
			t.Errorf("unexpected block name %q", name[1])
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
	identifier := regexp.MustCompile(`^[A-Za-z][A-Za-z0-9-]*$`)
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
		if identifier.MatchString(token) {
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
	manifest := lispTestManifest(t)
	boundaries := map[string]bool{}
	for _, boundary := range manifest.Boundaries {
		boundaries[boundary.Name] = true
	}
	emitted := map[string]bool{}
	for _, function := range manifest.Functions {
		emitted[function.Name] = true
	}

	forms := strings.Split(string(generated), "\n(defun ")
	if len(forms)-1 != len(manifest.Functions) {
		t.Fatalf("found %d generated functions, expected %d", len(forms)-1, len(manifest.Functions))
	}
	for _, form := range forms[1:] {
		name := form[:strings.IndexByte(form, ' ')]
		body := lispMaskStrings(form)
		// A real Core body either calls a boundary, calls another Core
		// function, or returns a Core value it computed. Requiring a call
		// to something catches a body the emitter dropped on the floor.
		works := false
		for _, match := range lispFormHeadRe.FindAllStringSubmatch(body, -1) {
			if boundaries[match[1]] || emitted[match[1]] {
				works = true
				break
			}
		}
		if !works {
			t.Errorf("generated function %s calls no boundary and no Core function; it looks like a placeholder:\n%s", name, form)
		}
	}
	// A "not implemented" or "unsupported" marker is not the placeholder
	// test for this file. Every byte of every body comes from a Core body,
	// and Core legitimately owns unsupported-capability errors such as
	// intrinsic.ai.error.unsupported; the marker words therefore appear in
	// correct output. The meaningful check is the structural one above:
	// did the emitter express the Core body, or drop it?
}

// TestLispCoreManifestDescribesEveryBoundaryHonestly checks the manifest is
// usable as the contract it claims to be: arities that match the calls, an
// owner for each boundary that Core actually classifies, and an explicit
// null where Core does not classify one. Reporting "pure" for a boundary
// Core never classified would send a host effect to the wrong owner.
func TestLispCoreManifestDescribesEveryBoundaryHonestly(t *testing.T) {
	manifest := lispTestManifest(t)
	if manifest.Marker != LispCoreMarker {
		t.Errorf("manifest marker is %q, want %q", manifest.Marker, LispCoreMarker)
	}
	if manifest.ManifestVersion != LispBoundaryManifestVersion {
		t.Errorf("manifest version is %d, want %d", manifest.ManifestVersion, LispBoundaryManifestVersion)
	}
	if manifest.ErrorCondition != "axllm:ax-error" {
		t.Errorf("manifest error condition is %q, want axllm:ax-error", manifest.ErrorCondition)
	}
	if len(manifest.HostProtocol) != 3 {
		t.Errorf("manifest should state all three host object protocol functions, got %v", manifest.HostProtocol)
	}
	if len(manifest.Boundaries) == 0 || len(manifest.Functions) == 0 {
		t.Fatal("manifest is empty")
	}

	total := 0
	for _, module := range manifest.Modules {
		total += module.Functions
	}
	if total != len(manifest.Functions) {
		t.Errorf("module counts sum to %d but the manifest lists %d functions", total, len(manifest.Functions))
	}

	unclassified := 0
	for _, boundary := range manifest.Boundaries {
		if len(boundary.ObservedArities) == 0 {
			t.Errorf("boundary %s records no observed arity; a native implementation cannot be written against it", boundary.Name)
		}
		for _, arity := range boundary.ObservedArities {
			if boundary.DeclaredMinArgs != nil && arity < *boundary.DeclaredMinArgs {
				t.Errorf("boundary %s is called with %d args but Core declares a minimum of %d", boundary.Name, arity, *boundary.DeclaredMinArgs)
			}
			if boundary.DeclaredMaxArgs != nil && *boundary.DeclaredMaxArgs >= 0 && arity > *boundary.DeclaredMaxArgs && boundary.Name != "core-host-call" {
				t.Errorf("boundary %s is called with %d args but Core declares a maximum of %d", boundary.Name, arity, *boundary.DeclaredMaxArgs)
			}
		}
		switch boundary.Kind {
		case "intrinsic", "statement", "both", "instrumentation":
		default:
			t.Errorf("boundary %s has kind %q", boundary.Name, boundary.Kind)
		}
		if boundary.Kind == "statement" && boundary.HostBoundary == nil {
			t.Errorf("boundary %s serves a Core statement form, which is pure by construction, but is unclassified", boundary.Name)
		}
		if boundary.HostBoundary == nil {
			unclassified++
		}
	}
	// A host boundary Core does classify must be reported as such; this
	// pins a few whose owner matters, so a regression in the mapping from
	// Core's table cannot quietly reassign them.
	wantHost := map[string]bool{
		"core-ai-stream-open": true, "core-ai-complete-once": true,
		"core-tool-invoke": true, "core-math-random": true,
		"core-retry-sleep": true, "core-host-call": true,
	}
	wantPure := map[string]bool{
		"core-get": true, "core-set": true, "core-true-p": true,
		"core-string-slice": true, "core-map-keys": true,
	}
	for _, boundary := range manifest.Boundaries {
		if wantHost[boundary.Name] {
			if boundary.HostBoundary == nil || !*boundary.HostBoundary {
				t.Errorf("boundary %s is a Core host effect but the manifest does not say so", boundary.Name)
			}
		}
		if wantPure[boundary.Name] {
			if boundary.HostBoundary == nil || *boundary.HostBoundary {
				t.Errorf("boundary %s is a pure value operation but the manifest does not say so", boundary.Name)
			}
		}
	}
	if unclassified == 0 {
		t.Log("every boundary is classified by Core; the unclassified case is no longer exercised")
	}

	for _, function := range manifest.Functions {
		if function.Name == "" || function.Symbol == "" || function.Module == "" {
			t.Errorf("manifest function entry is incomplete: %+v", function)
		}
		if function.RequiredArgs < 0 || function.OptionalArgs < 0 {
			t.Errorf("manifest function %s has a negative arity", function.Name)
		}
	}
}

// TestLispCoreRejectsConstructsItCannotExpress checks the emitter's edges
// fail loudly and by name. An emitter that guessed at a construct it does
// not understand would produce Lisp that compiles and means something else.
func TestLispCoreRejectsConstructsItCannotExpress(t *testing.T) {
	cases := []struct {
		name   string
		op     Operation
		expect string
	}{
		{
			name: "core op with no lowering",
			op: lispFuncOp("probe", Operation{
				Name: "core.switch", Line: 2,
				Regions: []Region{{Name: "body", Blocks: []Block{{Name: "entry"}}}},
			}),
			expect: "core.switch",
		},
		{
			name: "intrinsic with no boundary name",
			op: lispFuncOp("probe", Operation{
				Name: "core.call", Line: 2,
				Attributes: []Attribute{
					{Kind: "attr", Name: "result", Value: "%out"},
					{Kind: "attr", Name: "callee", Value: "intrinsic.len"},
					{Kind: "attr", Name: "args", Values: []interface{}{"%text"}},
				},
			}),
			expect: "intrinsic.len",
		},
		{
			name: "raise with neither error nor message",
			op: lispFuncOp("probe", Operation{
				Name: "core.raise", Line: 2,
				Attributes: []Attribute{{Kind: "attr", Name: "error", Value: "%missing"}},
			}),
			expect: "%missing",
		},
		{
			name: "break outside a loop",
			op: lispFuncOp("probe", Operation{
				Name: "core.break", Line: 2,
			}),
			expect: "outside loop",
		},
	}
	for _, testCase := range cases {
		t.Run(testCase.name, func(t *testing.T) {
			st := lispProbeState()
			if testCase.name == "intrinsic with no boundary name" {
				// Make the derivation fail by hiding the Python helper the
				// name is derived from, which is the real failure mode when
				// Core adds an intrinsic no port has named yet.
				saved := coreIntrinsicPython[IntrinsicLen]
				delete(coreIntrinsicPython, IntrinsicLen)
				defer func() { coreIntrinsicPython[IntrinsicLen] = saved }()
			}
			_, err := lispEmitProbe(st, testCase.op)
			if err == nil {
				t.Fatalf("expected emission to fail")
			}
			if !strings.Contains(err.Error(), testCase.expect) {
				t.Fatalf("error %q does not name %q", err, testCase.expect)
			}
		})
	}
}

// TestLispCoreRejectsWrongCoreCallArity guards the failure the full port
// made possible: with the whole registry calling into itself, a Core call that
// passes fewer arguments than the callee declares would reach SBCL as a
// warning buried in a multi-megabyte file. Generation must fail instead,
// naming the caller and the callee.
func TestLispCoreRejectsWrongCoreCallArity(t *testing.T) {
	st := lispProbeState()
	st.arity["other"] = lispFuncArity{Required: 2}
	st.names["other"] = "other"
	st.byEmitted["other"] = "other"
	_, err := lispEmitProbe(st, Operation{
		Name: "core.call", Line: 2,
		Attributes: []Attribute{
			{Kind: "attr", Name: "result", Value: "%out"},
			{Kind: "attr", Name: "callee", Value: "@other"},
			{Kind: "attr", Name: "args", Values: []interface{}{"%text"}},
		},
	})
	if err == nil {
		t.Fatal("a Core call with too few arguments must fail generation")
	}
	for _, want := range []string{"other", "1 argument"} {
		if !strings.Contains(err.Error(), want) {
			t.Errorf("error %q does not mention %q", err, want)
		}
	}

	// An optional argument makes the same call legal.
	st = lispProbeState()
	st.arity["other"] = lispFuncArity{Required: 1, Optional: 1}
	st.names["other"] = "other"
	st.byEmitted["other"] = "other"
	if _, err := lispEmitProbe(st, Operation{
		Name: "core.call", Line: 2,
		Attributes: []Attribute{
			{Kind: "attr", Name: "result", Value: "%out"},
			{Kind: "attr", Name: "callee", Value: "@other"},
			{Kind: "attr", Name: "args", Values: []interface{}{"%text"}},
		},
	}); err != nil {
		t.Errorf("a call that relies on an optional argument must be allowed: %v", err)
	}
}

func lispProbeState() *lispEmitState {
	return &lispEmitState{
		names:      map[string]string{"probe": "probe"},
		byEmitted:  map[string]string{"probe": "probe"},
		arity:      map[string]lispFuncArity{"probe": {Required: 1}},
		boundaries: map[string]*lispBoundaryUse{},
	}
}

// lispEmitProbe emits one statement inside a one-argument probe function.
// A bare statement op is wrapped first, because BuildCoreBody reads a
// core.func's body region.
func lispEmitProbe(st *lispEmitState, op Operation) (string, error) {
	if op.Name != "core.func" {
		op = lispFuncOp("probe", op)
	}
	body, err := BuildCoreBody(op)
	if err != nil {
		return "", err
	}
	return emitLispCoreFunction(st, op, CoreFuncSpec{Symbol: "probe", Name: "probe", Module: "signature"}, body)
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

// TestLispCoreOptionalArgumentsFollowPython checks the optional-argument
// defaults are derived from the Python reference rather than restated, so
// the two ports cannot disagree about which arguments a caller may omit.
func TestLispCoreOptionalArgumentsFollowPython(t *testing.T) {
	for _, testCase := range []struct{ fn, arg, want string }{
		{"to_json_schema", "schema_title", `"Schema"`},
		{"to_json_schema", "options", ":null"},
		{"validate_fields", "context", `"value"`},
		{"validate_value", "path", ":null"},
		{"render_prompt", "options", ":null"},
		{"to_json_schema", "fields", ""},
	} {
		got, err := lispArgDefault(testCase.fn, testCase.arg)
		if err != nil {
			t.Errorf("lispArgDefault(%q, %q): %v", testCase.fn, testCase.arg, err)
			continue
		}
		if got != testCase.want {
			t.Errorf("lispArgDefault(%q, %q) = %q, want %q", testCase.fn, testCase.arg, got, testCase.want)
		}
	}

	// Keying the table by the Lisp name instead of the IR name is the bug
	// the shipped subset actually had, so check a hyphenated key finds
	// nothing rather than silently matching.
	if got, _ := lispArgDefault("to-json-schema", "schema-title"); got != "" {
		t.Errorf("the defaults table is keyed by the IR's own names, but a hyphenated key matched %q", got)
	}

	// Every Python default must have a Lisp form, or a port would diverge
	// on which arguments are optional.
	model := lispTestModel(t)
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	optionalFunctions := 0
	for _, spec := range specs {
		body, err := BuildCoreBody(model.Symbols[spec.Symbol])
		if err != nil {
			t.Fatalf("@%s: %v", spec.Symbol, err)
		}
		hasOptional := false
		for _, arg := range body.Blocks[0].Args {
			python := pythonArgDefault(spec.Name, arg.Name)
			lisp, err := lispArgDefault(spec.Name, arg.Name)
			if err != nil {
				t.Errorf("@%s argument %s: %v", spec.Symbol, arg.Name, err)
				continue
			}
			if (python == "") != (lisp == "") {
				t.Errorf("@%s argument %s: python default %q but Lisp default %q", spec.Symbol, arg.Name, python, lisp)
			}
			if lisp != "" {
				hasOptional = true
			}
		}
		if hasOptional {
			optionalFunctions++
		}
	}
	if optionalFunctions == 0 {
		t.Fatal("no function has an optional argument; the derivation is not running")
	}

	generated, err := BuildLispCore(model)
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
// such as "string" would make SBCL refuse to compile the package. Every
// operator the emitter itself writes is covered too, because a Core
// function named "block" or "declaim" would break the forms around it.
func TestLispCoreRejectsCommonLispFunctionNames(t *testing.T) {
	for _, name := range []string{"string", "error", "length", "list", "map", "append", "remove"} {
		if !lispCLSymbolNames[name] {
			t.Errorf("%q should be treated as a COMMON-LISP symbol name", name)
		}
	}
	for _, name := range lispEmitterOperators {
		if !lispCLSymbolNames[name] {
			t.Errorf("the emitter writes %q, so a Core function must not be allowed to take that name", name)
		}
	}
	for _, name := range []string{"parse-signature", "to-json-schema", "signature-parse-impl", "schema-field-schema-impl", "render-prompt"} {
		if lispCLSymbolNames[name] {
			t.Errorf("%q is a real emitted name and must not be rejected", name)
		}
	}
}

// TestLispCoreHeaderDocumentsTheTarget checks the generated header carries
// what a reader needs: the marker, the honest scope and parity statement,
// the module layout, and the boundary list with its owner.
func TestLispCoreHeaderDocumentsTheTarget(t *testing.T) {
	model := lispTestModel(t)
	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	header := generated[:strings.Index(generated, "(in-package")]
	for _, want := range []string{
		LispCoreMarker,
		"FULL CORE REGISTRY, NO PARITY CLAIM YET",
		"go run ./cmd/lisp-core --check",
		"go run ./cmd/lisp-core --verify-runtime",
		"core-boundaries.json",
		"axllm:ax-error",
		"core-host-get / core-host-set / core-host-call",
	} {
		if !strings.Contains(header, want) {
			t.Errorf("generated header is missing %q", want)
		}
	}
	for _, entry := range lispModuleEntries(mustLispSpecs(t, model)) {
		if !strings.Contains(header, entry.Module) {
			t.Errorf("generated header does not list module %q", entry.Module)
		}
	}
	manifest := lispTestManifest(t)
	for _, boundary := range manifest.Boundaries {
		if !strings.Contains(header, boundary.Name) {
			t.Errorf("generated header does not list boundary %q", boundary.Name)
		}
	}
	// A docstring carries each function's Core provenance.
	for _, function := range manifest.Functions[:20] {
		want := fmt.Sprintf("\"Core @%s (%s module", function.Symbol, function.Module)
		if !strings.Contains(generated, want) {
			t.Errorf("generated file does not record provenance %s", want)
		}
	}
}

func mustLispSpecs(t *testing.T, model AxRuntimeModel) []CoreFuncSpec {
	t.Helper()
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	return specs
}

// ---------------------------------------------------------------------
// Control-flow lowering, checked by running it in SBCL
// ---------------------------------------------------------------------
//
// Core's return, break, continue and try/catch all become non-local exits
// in Common Lisp, and a plausible wrong lowering compiles cleanly while
// meaning something else: break lowered as continue, continue lowered as
// break, a nested break escaping the outer loop, or a try/catch built on
// CATCH/THROW that swallows a return. Reading the emitted text cannot tell
// those apart, so each one is emitted from synthetic Core and then run.
//
// Each probe's input is asymmetric on purpose: elements appear after the
// one that stops the loop, so a lowering that confuses break with continue
// produces a different answer rather than the same one.

func lispAttr(name string, value interface{}) Attribute {
	return Attribute{Kind: "attr", Name: name, Value: value}
}

func lispStmt(name string, attrs ...Attribute) Operation {
	return Operation{Name: name, Line: 2, Attributes: attrs}
}

func lispRegionOf(name string, ops ...Operation) Region {
	return Region{Name: name, Blocks: []Block{{Name: "entry", Ops: ops}}}
}

// lispIf builds a core.if, which Core requires to carry both regions.
func lispIf(cond string, then []Operation, otherwise []Operation) Operation {
	return Operation{
		Name: "core.if", Line: 2,
		Attributes: []Attribute{lispAttr("condition", cond)},
		Regions:    []Region{lispRegionOf("then", then...), lispRegionOf("else", otherwise...)},
	}
}

func lispFor(item, in string, body ...Operation) Operation {
	return Operation{
		Name: "core.for", Line: 2,
		Attributes: []Attribute{lispAttr("item", item), lispAttr("in", in)},
		Regions:    []Region{lispRegionOf("body", body...)},
	}
}

func lispLoop(body ...Operation) Operation {
	return Operation{
		Name: "core.loop", Line: 2,
		Regions: []Region{lispRegionOf("body", body...)},
	}
}

func lispTry(errRef string, try []Operation, catch []Operation) Operation {
	return Operation{
		Name: "core.try", Line: 2,
		Attributes: []Attribute{lispAttr("error", errRef)},
		Regions:    []Region{lispRegionOf("try", try...), lispRegionOf("catch", catch...)},
	}
}

// lispProbeFunc wraps probe statements in a core.func taking one argument.
func lispProbeFunc(symbol, arg string, stmts ...Operation) Operation {
	return Operation{
		Name:   "core.func",
		Symbol: symbol,
		Line:   1,
		Attributes: []Attribute{
			lispAttr("body_source", "core"),
			lispAttr("signature", "(json) -> json throws"),
		},
		Regions: []Region{{
			Name: "body",
			Blocks: []Block{{
				Name: "entry",
				Args: []Value{{Name: arg, Type: Type{Name: "json"}}},
				Ops:  stmts,
			}},
		}},
	}
}

// lispControlFlowProbes are the Core bodies the SBCL run exercises, with
// the answer each one must produce. The expectations are derived from Core's
// own semantics (the same ones the Python port implements), not from the
// emitter's output.
func lispControlFlowProbes() []struct {
	symbol   string
	arg      string
	stmts    []Operation
	call     string
	expected string
} {
	eq := func(result, left string, right interface{}) Operation {
		return Operation{
			Name: "core.call", Line: 2,
			Attributes: []Attribute{
				lispAttr("result", result),
				lispAttr("callee", "intrinsic.eq"),
				{Kind: "attr", Name: "args", Values: []interface{}{left, right}},
			},
		}
	}
	add := func(result, left string, right int) Operation {
		return Operation{
			Name: "core.call", Line: 2,
			Attributes: []Attribute{
				lispAttr("result", result),
				lispAttr("callee", "intrinsic.add"),
				{Kind: "attr", Name: "args", Values: []interface{}{left, right}},
			},
		}
	}
	message := func(result, errRef string) Operation {
		return Operation{
			Name: "core.call", Line: 2,
			Attributes: []Attribute{
				lispAttr("result", result),
				lispAttr("callee", "intrinsic.exception.message"),
				{Kind: "attr", Name: "args", Values: []interface{}{errRef}},
			},
		}
	}
	validationError := func(result, text string) Operation {
		return Operation{
			Name: "core.call", Line: 2,
			Attributes: []Attribute{
				lispAttr("result", result),
				lispAttr("callee", "intrinsic.error.validation"),
				{Kind: "attr", Name: "args", Values: []interface{}{QuotedString(text)}},
			},
		}
	}
	newList := func(result string) Operation {
		return lispStmt("core.list", lispAttr("result", result))
	}
	appendTo := func(target string, value interface{}) Operation {
		return lispStmt("core.append", lispAttr("target", target), lispAttr("value", value))
	}
	ret := func(value interface{}) Operation {
		return lispStmt("core.return", lispAttr("value", value))
	}

	return []struct {
		symbol   string
		arg      string
		stmts    []Operation
		call     string
		expected string
	}{
		{
			// return from inside a loop must leave the function, carrying
			// the element it found, not merely end the iteration.
			symbol: "cf_return", arg: "items",
			stmts: []Operation{
				newList("%seen"),
				lispFor("%item", "%items",
					eq("%hit", "%item", "stop"),
					lispIf("%hit", []Operation{ret("%item")}, nil),
					appendTo("%seen", "%item"),
				),
				ret(QuotedString("missing")),
			},
			call:     `(cf-return (vector "a" "stop" "b"))`,
			expected: "stop",
		},
		{
			symbol: "cf_return_absent", arg: "items",
			stmts: []Operation{
				lispFor("%item", "%items",
					eq("%hit", "%item", "stop"),
					lispIf("%hit", []Operation{ret("%item")}, nil),
				),
				ret(QuotedString("missing")),
			},
			call:     `(cf-return-absent (vector "a" "b"))`,
			expected: "missing",
		},
		{
			// break must stop the loop. "c" follows "stop", so a lowering
			// that skipped the iteration instead would include it.
			symbol: "cf_break", arg: "items",
			stmts: []Operation{
				newList("%out"),
				lispFor("%item", "%items",
					eq("%hit", "%item", "stop"),
					lispIf("%hit", []Operation{lispStmt("core.break")}, nil),
					appendTo("%out", "%item"),
				),
				ret("%out"),
			},
			call:     `(cf-break (vector "a" "b" "stop" "c"))`,
			expected: "[a,b]",
		},
		{
			// continue must skip one iteration. "b" follows "skip", so a
			// lowering that broke out would drop it.
			symbol: "cf_continue", arg: "items",
			stmts: []Operation{
				newList("%out"),
				lispFor("%item", "%items",
					eq("%hit", "%item", "skip"),
					lispIf("%hit", []Operation{lispStmt("core.continue")}, nil),
					appendTo("%out", "%item"),
				),
				ret("%out"),
			},
			call:     `(cf-continue (vector "a" "skip" "b"))`,
			expected: "[a,b]",
		},
		{
			// An inner break must leave only the inner loop. If it escaped
			// the outer one, the second row would never run.
			symbol: "cf_nested_break", arg: "rows",
			stmts: []Operation{
				newList("%out"),
				lispFor("%row", "%rows",
					lispFor("%item", "%row",
						eq("%hit", "%item", "stop"),
						lispIf("%hit", []Operation{lispStmt("core.break")}, nil),
						appendTo("%out", "%item"),
					),
					appendTo("%out", QuotedString("|")),
				),
				ret("%out"),
			},
			call:     `(cf-nested-break (vector (vector "a" "stop" "b") (vector "c")))`,
			expected: "[a,|,c,|]",
		},
		{
			// An unbounded core.loop with both break and continue. 2 is
			// skipped, 5 stops it, so swapping the two gives a different
			// answer rather than the same one.
			symbol: "cf_loop", arg: "unused",
			stmts: []Operation{
				newList("%out"),
				lispStmt("core.const", lispAttr("result", "%n"), lispAttr("value", 0)),
				lispLoop(
					add("%n", "%n", 1),
					eq("%skip", "%n", 2),
					lispIf("%skip", []Operation{lispStmt("core.continue")}, nil),
					eq("%stop", "%n", 5),
					lispIf("%stop", []Operation{lispStmt("core.break")}, nil),
					appendTo("%out", "%n"),
				),
				ret("%out"),
			},
			call:     `(cf-loop :null)`,
			expected: "[1,3,4]",
		},
		{
			// A message-only raise must signal axllm:ax-error with that
			// exact message, and the catch region must see it and then let
			// the function carry on.
			symbol: "cf_try_message", arg: "unused",
			stmts: []Operation{
				newList("%out"),
				lispTry("%err",
					[]Operation{
						appendTo("%out", QuotedString("before")),
						lispStmt("core.raise", lispAttr("message", "boom")),
					},
					[]Operation{
						message("%text", "%err"),
						appendTo("%out", "%text"),
					},
				),
				appendTo("%out", QuotedString("after")),
				ret("%out"),
			},
			call:     `(cf-try-message :null)`,
			expected: "[before,boom,after]",
		},
		{
			// A raise of a constructed Core error value must be catchable
			// the same way.
			symbol: "cf_try_value", arg: "unused",
			stmts: []Operation{
				lispTry("%err",
					[]Operation{
						validationError("%built", "bad input"),
						lispStmt("core.raise", lispAttr("error", "%built")),
					},
					[]Operation{
						message("%text", "%err"),
						ret("%text"),
					},
				),
				ret(QuotedString("not reached")),
			},
			call:     `(cf-try-value :null)`,
			expected: "bad input",
		},
		{
			// A return inside try must leave the function. A try/catch
			// built on CATCH/THROW would intercept it and return
			// "from-catch" instead.
			symbol: "cf_try_return", arg: "unused",
			stmts: []Operation{
				lispTry("%err",
					[]Operation{ret(QuotedString("from-try"))},
					[]Operation{ret(QuotedString("from-catch"))},
				),
				ret(QuotedString("fell through")),
			},
			call:     `(cf-try-return :null)`,
			expected: "from-try",
		},
		{
			// A break inside try must unwind out of the handler and stop
			// the loop, not be caught as if it were an error.
			symbol: "cf_break_in_try", arg: "items",
			stmts: []Operation{
				newList("%out"),
				lispFor("%item", "%items",
					lispTry("%err",
						[]Operation{
							eq("%hit", "%item", "stop"),
							lispIf("%hit", []Operation{lispStmt("core.break")}, nil),
							appendTo("%out", "%item"),
						},
						[]Operation{appendTo("%out", QuotedString("caught"))},
					),
				),
				ret("%out"),
			},
			call:     `(cf-break-in-try (vector "a" "stop" "b"))`,
			expected: "[a]",
		},
	}
}

// lispProbeRuntime is the smallest set of boundaries the probes call. It is
// deliberately hand-written and tiny: these tests are about control-flow
// lowering, not about boundary semantics, which core-runtime.lisp owns and
// the conformance suites check.
const lispProbeRuntime = `
(defpackage #:yason (:use) (:export #:true #:false))
(defpackage #:axllm (:use #:cl) (:export #:ax-error #:ax-error-message))
(in-package #:axllm)
(define-condition ax-error (error)
  ((message :initarg :message :initform "" :reader ax-error-message)))
(defpackage #:axllm/core (:use #:cl))
(in-package #:axllm/core)

(defun core-true-p (value)
  (not (or (null value) (eq value :null) (eq value 'yason:false))))
(defun core-eq (left right)
  (if (equal left right) 'yason:true 'yason:false))
(defun core-add (left right) (+ left right))
(defun core-new-list () (make-array 0 :adjustable t :fill-pointer t))
(defun core-append (target value) (vector-push-extend value target) target)
(defun core-elements (value) (coerce value 'list))
(defun core-exception-message (condition) (axllm:ax-error-message condition))
(defun core-validation-error (text)
  (make-condition 'axllm:ax-error :message text))
;; Coverage instrumentation is emitted into every generated function, so the
;; probe runtime has to provide it too. The real one records the name; here
;; it only has to exist and return a Core value.
(defun core-coverage-mark (name) (declare (ignore name)) :null)

(defun probe-render (value)
  (cond ((stringp value) value)
        ((eq value :null) "null")
        ((eq value 'yason:true) "true")
        ((eq value 'yason:false) "false")
        ((and (vectorp value) (not (stringp value)))
         (format nil "[~{~a~^,~}]" (map 'list #'probe-render value)))
        (t (format nil "~a" value))))
`

// TestLispCoreControlFlowRunsCorrectlyInSBCL emits each control-flow probe,
// runs it in SBCL, and compares the answer with what Core means. This is
// the only check that can tell a correct lowering from one that compiles
// and means something else.
func TestLispCoreControlFlowRunsCorrectlyInSBCL(t *testing.T) {
	sbcl := lispFindSBCL(t)
	probes := lispControlFlowProbes()

	st := &lispEmitState{
		names:      map[string]string{},
		byEmitted:  map[string]string{},
		arity:      map[string]lispFuncArity{},
		boundaries: map[string]*lispBoundaryUse{},
	}
	for _, probe := range probes {
		st.names[probe.symbol] = LispCoreFuncName(probe.symbol)
		st.byEmitted[probe.symbol] = probe.symbol
		st.arity[probe.symbol] = lispFuncArity{Required: 1}
	}

	var source strings.Builder
	source.WriteString(lispProbeRuntime)
	for _, probe := range probes {
		op := lispProbeFunc(probe.symbol, probe.arg, probe.stmts...)
		body, err := BuildCoreBody(op)
		if err != nil {
			t.Fatalf("%s: Core body is not valid: %v", probe.symbol, err)
		}
		text, err := emitLispCoreFunction(st, op, CoreFuncSpec{Symbol: probe.symbol, Name: probe.symbol, Module: "signature"}, body)
		if err != nil {
			t.Fatalf("%s: emit: %v", probe.symbol, err)
		}
		source.WriteString(text)
		source.WriteByte('\n')
	}
	source.WriteString("(in-package #:axllm/core)\n")
	for _, probe := range probes {
		fmt.Fprintf(&source, "(format t \"~&%s=~a~%%\" (probe-render %s))\n", probe.symbol, probe.call)
	}

	dir := t.TempDir()
	path := filepath.Join(dir, "probe.lisp")
	if err := os.WriteFile(path, []byte(source.String()), 0o644); err != nil {
		t.Fatalf("write probe: %v", err)
	}
	output, err := exec.Command(sbcl, "--script", path).CombinedOutput()
	if err != nil {
		t.Fatalf("sbcl failed: %v\n%s\n--- source ---\n%s", err, output, source.String())
	}
	results := map[string]string{}
	for _, line := range strings.Split(string(output), "\n") {
		if key, value, ok := strings.Cut(strings.TrimSpace(line), "="); ok {
			results[key] = value
		}
	}
	for _, probe := range probes {
		got, ok := results[probe.symbol]
		if !ok {
			t.Errorf("%s produced no result; sbcl said:\n%s", probe.symbol, output)
			continue
		}
		if got != probe.expected {
			t.Errorf("%s = %q, want %q", probe.symbol, got, probe.expected)
		}
	}
	if t.Failed() {
		t.Logf("sbcl output:\n%s", output)
	}
}

// TestLispCoreControlFlowStructure pins the lowering shapes the SBCL run
// proves correct, so a change in them is visible in a diff and so the
// cheap check still runs when SBCL is unavailable. It also checks the loop
// and iteration blocks are emitted only where they are needed, which is
// what keeps the already-shipped signature and schema output unchanged.
func TestLispCoreControlFlowStructure(t *testing.T) {
	st := &lispEmitState{
		names:      map[string]string{"probe": "probe"},
		byEmitted:  map[string]string{"probe": "probe"},
		arity:      map[string]lispFuncArity{"probe": {Required: 1}},
		boundaries: map[string]*lispBoundaryUse{},
	}
	emit := func(t *testing.T, stmts ...Operation) string {
		t.Helper()
		op := lispProbeFunc("probe", "items", stmts...)
		body, err := BuildCoreBody(op)
		if err != nil {
			t.Fatalf("Core body is not valid: %v", err)
		}
		text, err := emitLispCoreFunction(st, op, CoreFuncSpec{Symbol: "probe", Name: "probe", Module: "signature"}, body)
		if err != nil {
			t.Fatalf("emit: %v", err)
		}
		return text
	}

	plain := emit(t, lispFor("%item", "%items",
		lispStmt("core.append", lispAttr("target", "%items"), lispAttr("value", "%item")),
	))
	if strings.Contains(plain, "(block ") {
		t.Errorf("a loop with no break or continue must not emit a block:\n%s", plain)
	}

	withBreak := emit(t, lispFor("%item", "%items", lispStmt("core.break")))
	if !strings.Contains(withBreak, "(block core-loop-1") || !strings.Contains(withBreak, "(return-from core-loop-1)") {
		t.Errorf("break must return from the loop's own block:\n%s", withBreak)
	}
	if strings.Contains(withBreak, "core-iteration-1") {
		t.Errorf("a loop with no continue must not emit an iteration block:\n%s", withBreak)
	}

	withContinue := emit(t, lispFor("%item", "%items", lispStmt("core.continue")))
	if !strings.Contains(withContinue, "(block core-iteration-1") || !strings.Contains(withContinue, "(return-from core-iteration-1)") {
		t.Errorf("continue must return from the iteration block:\n%s", withContinue)
	}
	if strings.Contains(withContinue, "(block core-loop-1") {
		t.Errorf("a loop with no break must not emit a loop block:\n%s", withContinue)
	}

	// A nested break must name the inner loop's block, not the outer one.
	nested := emit(t, lispFor("%row", "%items",
		lispFor("%item", "%row", lispStmt("core.break")),
	))
	if !strings.Contains(nested, "(return-from core-loop-2)") {
		t.Errorf("an inner break must target the inner loop's block:\n%s", nested)
	}
	if strings.Contains(nested, "(return-from core-loop-1)") {
		t.Errorf("an inner break must not target the outer loop's block:\n%s", nested)
	}

	try := emit(t, lispTry("%err",
		[]Operation{lispStmt("core.return", lispAttr("value", "%items"))},
		[]Operation{lispStmt("core.raise", lispAttr("error", "%err"))},
	))
	for _, want := range []string{"(handler-case", "(error (%err)", "(declare (ignorable %err))"} {
		if !strings.Contains(try, want) {
			t.Errorf("try lowering is missing %q:\n%s", want, try)
		}
	}
	if strings.Contains(try, "(catch ") || strings.Contains(try, "(throw ") {
		t.Errorf("try must not be built on CATCH/THROW, which would intercept a return:\n%s", try)
	}

	raise := emit(t, lispStmt("core.raise", lispAttr("message", "boom ~a 100%")))
	if !strings.Contains(raise, `(error 'axllm:ax-error :message "boom ~a 100%")`) {
		t.Errorf("a message-only raise must signal axllm:ax-error with the message as a literal, never a format control:\n%s", raise)
	}

	// core.loop must be an unbounded Common Lisp loop whose body is a
	// compound form; a bare keyword body would be read as an extended LOOP.
	infinite := emit(t, lispLoop(lispStmt("core.break")))
	if !strings.Contains(infinite, "(loop") {
		t.Errorf("core.loop must emit a LOOP form:\n%s", infinite)
	}
	if strings.Contains(infinite, "(loop\n  :null") {
		t.Errorf("a LOOP body must be a compound form, not a keyword:\n%s", infinite)
	}
}

// TestLispCoreGeneratedFileCompilesInSBCL compiles the checked-in file with
// no boundary definitions at all. It proves three things at once: the
// whole-registry file is structurally valid Lisp, the forward declarations
// really do cover every name, and the file can be compiled before the
// native boundaries it calls exist. Both full warnings and style warnings
// must be zero, because SBCL reports an undefined function as a style
// warning and a wrong argument count as a full warning.
func TestLispCoreGeneratedFileCompilesInSBCL(t *testing.T) {
	sbcl := lispFindSBCL(t)
	generated, err := filepath.Abs(lispGeneratedPath())
	if err != nil {
		t.Fatalf("resolve generated path: %v", err)
	}
	dir := t.TempDir()
	driver := fmt.Sprintf(`
(defpackage #:yason (:use) (:export #:true #:false))
(defpackage #:axllm (:use #:cl) (:export #:ax-error))
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
                        (when (< shown 25) (incf shown) (format t "~&DIAG: ~a~%%" c))
                        (muffle-warning c)))
       (warning (lambda (c)
                  (incf full)
                  (when (< shown 25) (incf shown) (format t "~&DIAG: ~a~%%" c))
                  (muffle-warning c))))
    (multiple-value-bind (fasl warnings failure)
        (compile-file %q :output-file %q :verbose nil :print nil)
      (format t "~&compiled=~a warnings=~a failure=~a~%%" (and fasl t) warnings failure)))
  (format t "~&full-warnings=~d style-warnings=~d~%%" full style))
`, generated, filepath.Join(dir, "core.fasl"))
	path := filepath.Join(dir, "compile.lisp")
	if err := os.WriteFile(path, []byte(driver), 0o644); err != nil {
		t.Fatalf("write driver: %v", err)
	}
	output, err := exec.Command(sbcl, "--dynamic-space-size", "4096", "--script", path).CombinedOutput()
	if err != nil {
		t.Fatalf("sbcl failed to compile the generated file: %v\n%s", err, output)
	}
	text := string(output)
	if !strings.Contains(text, "compiled=T failure=NIL") && !strings.Contains(text, "compiled=T warnings=NIL failure=NIL") {
		t.Errorf("generated file did not compile cleanly:\n%s", text)
	}
	if !strings.Contains(text, "full-warnings=0 style-warnings=0") {
		t.Errorf("compiling the generated file is not warning-free; every called name must be defined or forward-declared:\n%s", text)
	}
}

func lispFindSBCL(t *testing.T) string {
	t.Helper()
	path, err := exec.LookPath("sbcl")
	if err != nil {
		t.Skip("sbcl is not installed; install SBCL to run the Common Lisp checks")
	}
	return path
}

// ---------------------------------------------------------------------
// Compile and verify target registration
// ---------------------------------------------------------------------

func lispPackageDir() string {
	return filepath.Join(repoRootPath(), "packages", "lisp")
}

// TestLispProvenanceAuditAcceptsTheRealPackage runs the provenance audit
// over the checked-in packages/lisp tree, which is the only place the
// public axllm facade and the generated axllm/core code sit side by side.
// It is the test that proves the audit is usable: every one of the
// emitted Core functions must be found exactly once inside the emitted region, and
// nothing in the facade may be reported as a shadow.
func TestLispProvenanceAuditAcceptsTheRealPackage(t *testing.T) {
	model := lispTestModel(t)
	report, err := AuditProvenanceDir(model, "lisp", lispPackageDir())
	if err != nil {
		t.Fatalf("audit: %v", err)
	}
	if !report.Enforced {
		t.Error("the Lisp provenance audit must be enforced, not report-only")
	}
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	if report.EmittedFunctions != len(specs) {
		t.Errorf("audit found %d emitted functions, want %d", report.EmittedFunctions, len(specs))
	}
	if len(report.Violations) > 0 {
		t.Errorf("provenance audit reports %d violation(s) against the real package:\n  %s",
			len(report.Violations), strings.Join(report.Violations, "\n  "))
	}
	metrics, ok := report.Files[lispProvenanceCoreFile]
	if !ok {
		t.Fatalf("audit did not measure %s", lispProvenanceCoreFile)
	}
	if metrics.EmittedLines == 0 || metrics.EmittedLines >= metrics.TotalLines {
		t.Errorf("%s reports %d emitted of %d total lines", lispProvenanceCoreFile, metrics.EmittedLines, metrics.TotalLines)
	}
}

// TestLispShadowAuditIsPackageAware is the test for the audit's one subtle
// rule. Common Lisp has a function namespace per package, so AXLLM:FOO and
// AXLLM/CORE:FOO are different functions and only the second can shadow a
// Core-owned one. A name-only audit would report every public facade method
// named after an Ax concept as a hand-written shadow, which is both wrong
// and the kind of false alarm that gets an audit switched off.
//
// Each case below uses the same function name and differs only in the
// package it is defined in, so a name-only audit passes none of them and a
// package-aware audit passes all four.
func TestLispShadowAuditIsPackageAware(t *testing.T) {
	model := lispTestModel(t)
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	core, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	// Pick a real emitted name so the case is not hypothetical.
	victim := ""
	for _, spec := range specs {
		if spec.Name == "to_json_schema" {
			victim = LispCoreFuncName(spec.Name)
		}
	}
	if victim == "" {
		t.Fatal("to_json_schema is not in the registry; pick another emitted name")
	}

	for _, testCase := range []struct {
		name    string
		file    string
		content string
		shadow  bool
	}{
		{
			name: "public facade method in package axllm is not a shadow",
			file: "src/signature.lisp",
			content: "(in-package #:axllm)\n\n(defun " + victim + " (fields)\n" +
				"  (axllm/core::" + victim + " fields))\n",
			shadow: false,
		},
		{
			name:    "the same name defined in axllm/core is a shadow",
			file:    "src/core-runtime.lisp",
			content: "(in-package #:axllm/core)\n\n(defun " + victim + " (fields) fields)\n",
			shadow:  true,
		},
		{
			name:    "a package-qualified definition reaching into axllm/core is a shadow",
			file:    "src/tools.lisp",
			content: "(in-package #:axllm)\n\n(defun axllm/core::" + victim + " (fields) fields)\n",
			shadow:  true,
		},
		{
			name:    "a facade method named after a Core function in a third package is not a shadow",
			file:    "src/jiti.lisp",
			content: "(in-package #:axllm/jiti)\n\n(defun " + victim + " (fields) fields)\n",
			shadow:  false,
		},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			files := map[string]string{
				lispProvenanceCoreFile: string(core),
				testCase.file:          testCase.content,
			}
			report, err := AuditProvenance(model, "lisp", files)
			if err != nil {
				t.Fatalf("audit: %v", err)
			}
			flagged := false
			for _, violation := range report.Violations {
				if strings.Contains(violation, "hand-written shadow") && strings.Contains(violation, testCase.file) {
					flagged = true
				}
			}
			if flagged != testCase.shadow {
				t.Errorf("shadow reported = %v, want %v; violations:\n  %s",
					flagged, testCase.shadow, strings.Join(report.Violations, "\n  "))
			}
		})
	}
}

// TestLispCorePackageTextMasksForeignPackages checks the masking directly,
// including the detail that line numbers must survive it: a violation that
// pointed at the wrong line would be worse than none.
func TestLispCorePackageTextMasksForeignPackages(t *testing.T) {
	source := strings.Join([]string{
		"(in-package #:axllm)",
		"(defun facade-only () nil)",
		"(in-package #:axllm/core)",
		"(defun core-only () nil)",
		"(in-package :axllm)",
		"(defun late-facade () nil)",
		"(defun axllm/core::reaches-in () nil)",
	}, "\n")
	masked := lispCorePackageText(source, LispCorePackage)
	if strings.Count(masked, "\n") != strings.Count(source, "\n") {
		t.Errorf("masking changed the line count: %d vs %d", strings.Count(masked, "\n"), strings.Count(source, "\n"))
	}
	for _, want := range []string{"core-only", "reaches-in"} {
		if !strings.Contains(masked, want) {
			t.Errorf("masking dropped %q, which can define a symbol in %s", want, LispCorePackage)
		}
	}
	for _, unwanted := range []string{"facade-only", "late-facade"} {
		if strings.Contains(masked, unwanted) {
			t.Errorf("masking kept %q, which is in a different package and cannot shadow Core", unwanted)
		}
	}
	// A file with no in-package form defines nothing in the Core package.
	if got := lispCorePackageText("(defun stray () nil)", LispCorePackage); strings.Contains(got, "stray") {
		t.Error("a file with no in-package form must not be read as the Core package")
	}
}

// TestLispCapabilityManifestMakesNoParityClaim is the honesty test for the
// registration. The target emits every Core body but has run no conformance
// suite, so the manifest must claim no suite and must say what is missing.
// A manifest that claimed suites would advertise parity this target has not
// earned.
func TestLispCapabilityManifestMakesNoParityClaim(t *testing.T) {
	lispUseUndeclaredNativeFixture(t)
	model := lispTestModel(t)
	manifest, err := BuildCapabilityManifest(model, "lisp")
	if err != nil {
		t.Fatalf("capability manifest: %v", err)
	}
	if len(manifest.SupportedSuites) != 0 {
		t.Errorf("the Lisp manifest claims suites %v before any conformance suite has run", manifest.SupportedSuites)
	}
	if len(manifest.UnsupportedCapabilities) == 0 {
		t.Error("the Lisp manifest claims nothing and admits nothing; it must name what is missing")
	}
	if manifest.RealNetworkSupport || manifest.ScriptedTransportSupport {
		t.Error("the Lisp manifest claims transport support that nothing verifies")
	}
	if manifest.PackageName != "axllm" {
		t.Errorf("package name is %q, want axllm", manifest.PackageName)
	}
	if manifest.TargetIdiom.MethodNaming != "lisp-case" {
		t.Errorf("missing Lisp idiom contract: %#v", manifest.TargetIdiom)
	}

	coverage, err := BuildConformanceCoverageManifest(model, "lisp")
	if err != nil {
		t.Fatalf("coverage manifest: %v", err)
	}
	for _, suite := range lispConformanceSuites() {
		entries := coverage.Suites[suite]
		if len(entries) == 0 {
			t.Errorf("coverage omits suite %q; an absent suite looks like an oversight", suite)
			continue
		}
		for _, entry := range entries {
			if entry.Category != "explicitly-not-claimed" {
				t.Errorf("suite %q is categorised %q while no runner exists", suite, entry.Category)
			}
			if strings.TrimSpace(entry.Runner) == "" {
				t.Errorf("suite %q has an empty runner", suite)
			}
		}
	}
	if err := ValidateConformanceCoverage(manifest, coverage); err != nil {
		t.Errorf("the honest manifests do not validate against each other: %v", err)
	}
}

// TestVerifyLispManifestRejectsAClaimWithoutARunner checks the manifest
// gate has teeth: the moment someone adds a suite to supported_suites
// without a runner behind it, verification must fail.
func TestVerifyLispManifestRejectsAClaimWithoutARunner(t *testing.T) {
	lispUseUndeclaredNativeFixture(t)
	model := lispTestModel(t)
	dir := t.TempDir()
	if err := EmitLisp(model, dir); err != nil {
		t.Fatalf("emit: %v", err)
	}
	if err := VerifyLispManifest(dir); err != nil {
		t.Fatalf("the emitted manifests must validate: %v", err)
	}

	path := filepath.Join(dir, "axir-capabilities.json")
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read manifest: %v", err)
	}
	var manifest CapabilityManifest
	if err := json.Unmarshal(data, &manifest); err != nil {
		t.Fatalf("parse manifest: %v", err)
	}
	manifest.SupportedSuites = []string{"signature"}
	patched, err := json.MarshalIndent(manifest, "", "  ")
	if err != nil {
		t.Fatalf("marshal manifest: %v", err)
	}
	if err := os.WriteFile(path, patched, 0o644); err != nil {
		t.Fatalf("write manifest: %v", err)
	}
	err = VerifyLispManifest(dir)
	if err == nil {
		t.Fatal("claiming a suite whose coverage says not-claimed must fail verification")
	}
	if !strings.Contains(err.Error(), "signature") {
		t.Errorf("error %q does not name the falsely claimed suite", err)
	}
}

// Default verification includes Lisp and requires native evidence, not just compilation.
func TestLispIsInDefaultVerifyTargets(t *testing.T) {
	defaults := normalizeVerifyTargets(nil)
	if strings.Join(defaults, ",") != "python,java,cpp,go,rust,lisp" {
		t.Errorf("default verify targets are %v; expected all six targets", defaults)
	}
	// It must still be reachable when asked for by name.
	if got := normalizeVerifyTargets([]string{"lisp"}); len(got) != 1 || got[0] != "lisp" {
		t.Errorf("normalizeVerifyTargets([lisp]) = %v; the target must be selectable", got)
	}
}

// TestLispCompileEmitsALoadablePackage checks the owned outcome of the
// compile target: a standalone, loadable Ax package, not a handful of
// generated Core files. The ASDF system must be present, every component it
// names must exist, and the native boundaries must be there, because a
// directory that is missing any of those loads for nobody.
func TestLispCompileEmitsALoadablePackage(t *testing.T) {
	model := lispTestModel(t)
	dir := t.TempDir()
	if err := EmitLisp(model, dir); err != nil {
		t.Fatalf("emit: %v", err)
	}
	if err := ValidateLispPackageIsLoadable(dir); err != nil {
		t.Fatalf("the emitted package is not loadable: %v", err)
	}
	// A fresh output directory must see generated Core before the API scan,
	// just like in-place regeneration. Otherwise the published docs invent a gap.
	api, err := os.ReadFile(filepath.Join(dir, "axir-api.json"))
	if err != nil {
		t.Fatal(err)
	}
	if strings.Contains(string(api), "unread_source_components") {
		t.Fatal("fresh package API reference reports missing source components")
	}

	var written []string
	err = filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() {
			return err
		}
		rel, relErr := filepath.Rel(dir, path)
		if relErr != nil {
			return relErr
		}
		written = append(written, filepath.ToSlash(rel))
		return nil
	})
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	have := map[string]bool{}
	for _, name := range written {
		have[name] = true
	}
	for _, want := range []string{
		"axllm.asd",
		"src/core.lisp",
		"src/core-runtime.lisp",
		"src/package.lisp",
		"src/core-boundaries.json",
		"axir-capabilities.json",
		"conformance-coverage.json",
		"README.md",
		"LICENSE",
	} {
		if !have[want] {
			t.Errorf("the emitted package is missing %s", want)
		}
	}
	if len(written) < 20 {
		t.Errorf("the emitted package holds %d files; a standalone Ax package needs its native sources too", len(written))
	}

	// The native files must be copied verbatim. A packaging step that
	// rewrote them would make the package disagree with its own source of
	// truth.
	nativeDir, err := LispNativeSourceDir()
	if err != nil {
		t.Fatalf("native sources: %v", err)
	}
	for _, name := range []string{"axllm.asd", "src/core-runtime.lisp", "src/package.lisp"} {
		want, err := os.ReadFile(filepath.Join(nativeDir, filepath.FromSlash(name)))
		if err != nil {
			t.Fatalf("read native %s: %v", name, err)
		}
		got, err := os.ReadFile(filepath.Join(dir, filepath.FromSlash(name)))
		if err != nil {
			t.Fatalf("read emitted %s: %v", name, err)
		}
		if string(got) != string(want) {
			t.Errorf("%s was rewritten during packaging; native sources must be copied verbatim", name)
		}
	}

	// The generated Core must be generated, not copied.
	emittedCore, err := os.ReadFile(filepath.Join(dir, filepath.FromSlash(lispProvenanceCoreFile)))
	if err != nil {
		t.Fatalf("read emitted core: %v", err)
	}
	wantCore, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("build core: %v", err)
	}
	if string(emittedCore) != wantCore {
		t.Error("the emitted src/core.lisp is not what the generator produces")
	}
}

func TestLispNativeCopyExcludesExecutionArtifacts(t *testing.T) {
	source, destination := t.TempDir(), t.TempDir()
	files := map[string]bool{
		"src/native.lisp":                 true,
		"tests/runtime-protocol.py":       true,
		"tests/fixture.json":              true,
		"src/native.fasl":                 false,
		"src/core.lisp":                   false,
		"conformance-coverage.json":       false,
		"tests/conformance-coverage.json": false,
	}
	for name := range files {
		path := filepath.Join(source, filepath.FromSlash(name))
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(name), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if err := copyLispNativeSources(source, destination); err != nil {
		t.Fatal(err)
	}
	for name, wantCopied := range files {
		data, err := os.ReadFile(filepath.Join(destination, filepath.FromSlash(name)))
		if wantCopied {
			if err != nil || string(data) != name {
				t.Errorf("native file %s changed or was omitted: %q, %v", name, data, err)
			}
		} else if !os.IsNotExist(err) {
			t.Errorf("execution/generated artifact %s was copied: %v", name, err)
		}
	}
}

// TestLispPackageValidationRejectsAnIncompletePackage is the negative case
// for the packaging guard. Without it, a Core-only directory would be
// reported as a successful compile and fail later at
// (asdf:load-system "axllm").
func TestLispPackageValidationRejectsAnIncompletePackage(t *testing.T) {
	model := lispTestModel(t)
	for _, testCase := range []struct {
		name   string
		mutate func(t *testing.T, dir string)
		expect string
	}{
		{
			name:   "no ASDF system",
			mutate: func(t *testing.T, dir string) { mustRemoveForTest(t, filepath.Join(dir, "axllm.asd")) },
			expect: "axllm.asd",
		},
		{
			name: "a component the system names is absent",
			mutate: func(t *testing.T, dir string) {
				mustRemoveForTest(t, filepath.Join(dir, "src", "package.lisp"))
			},
			expect: "package.lisp",
		},
		{
			name: "the native boundaries are absent",
			mutate: func(t *testing.T, dir string) {
				mustRemoveForTest(t, filepath.Join(dir, "src", "core-runtime.lisp"))
			},
			expect: "core-runtime.lisp",
		},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			dir := t.TempDir()
			if err := EmitLisp(model, dir); err != nil {
				t.Fatalf("emit: %v", err)
			}
			testCase.mutate(t, dir)
			err := ValidateLispPackageIsLoadable(dir)
			if err == nil {
				t.Fatal("an incomplete package must not validate")
			}
			if !strings.Contains(err.Error(), testCase.expect) {
				t.Errorf("error %q does not name %q", err, testCase.expect)
			}
		})
	}
}

func mustRemoveForTest(t *testing.T, path string) {
	t.Helper()
	if err := os.Remove(path); err != nil {
		t.Fatalf("remove %s: %v", path, err)
	}
}

// TestLispCoreCoverageMarksInstrumentEveryFunction checks the coverage
// instrumentation now that packages/lisp defines core-coverage-mark. The
// mark has to be the first form of every body: a later position would miss
// every path that returns early, and a missing one would make the coverage
// report silently under-count rather than fail.
func TestLispCoreCoverageMarksInstrumentEveryFunction(t *testing.T) {
	if !lispEmitCoverageMarks {
		t.Fatal("coverage instrumentation is off; the native boundary exists, so it should be on")
	}
	model := lispTestModel(t)
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	generated, err := os.ReadFile(lispGeneratedPath())
	if err != nil {
		t.Fatalf("read generated file: %v", err)
	}
	if got := strings.Count(string(generated), "("+lispCoverageMarkBoundary+" "); got != len(specs) {
		t.Errorf("%d coverage marks for %d functions; every Core function must record its own", got, len(specs))
	}

	// The native boundary must exist, or every Core call would fail at run
	// time while still compiling cleanly.
	shapes, _, err := LoadLispNativeLambdaShapes(filepath.Dir(lispRuntimePath()), filepath.Base(lispGeneratedPath()))
	if err != nil {
		t.Fatalf("read native sources: %v", err)
	}
	shape, ok := shapes[lispCoverageMarkBoundary]
	if !ok {
		t.Fatalf("%s is called by every emitted function but no native file defines it", lispCoverageMarkBoundary)
	}
	if !shape.Accepts(1) {
		t.Errorf("%s accepts %s argument(s) but is called with 1", lispCoverageMarkBoundary, shape.Describe())
	}

	manifest := lispTestManifest(t)
	found := false
	for _, boundary := range manifest.Boundaries {
		if boundary.Name != lispCoverageMarkBoundary {
			continue
		}
		found = true
		if boundary.Kind != "instrumentation" {
			t.Errorf("%s has kind %q, want instrumentation", boundary.Name, boundary.Kind)
		}
		if boundary.HostBoundary == nil || !*boundary.HostBoundary {
			t.Errorf("%s records host state, so it must be a host boundary", boundary.Name)
		}
		if len(boundary.ObservedArities) != 1 || boundary.ObservedArities[0] != 1 {
			t.Errorf("%s is called with arities %v, want exactly [1]", boundary.Name, boundary.ObservedArities)
		}
		if boundary.CallSites != len(specs) {
			t.Errorf("%s has %d call sites, want %d", boundary.Name, boundary.CallSites, len(specs))
		}
	}
	if !found {
		t.Errorf("%s is emitted but the manifest does not declare it", lispCoverageMarkBoundary)
	}

	// Position: the mark must precede the first real body form.
	start := strings.Index(string(generated), "(defun to-json-schema ")
	if start < 0 {
		t.Fatal("to-json-schema is missing from the generated output")
	}
	window := string(generated)[start : start+700]
	markAt := strings.Index(window, "("+lispCoverageMarkBoundary+" ")
	if markAt < 0 {
		t.Fatalf("to-json-schema carries no coverage mark:\n%s", window)
	}
	if setfAt := strings.Index(window, "(setf "); setfAt >= 0 && setfAt < markAt {
		t.Errorf("the coverage mark is not the first body form:\n%s", window)
	}
	if !strings.Contains(window, "("+lispCoverageMarkBoundary+" \"to_json_schema\")") {
		t.Errorf("the coverage mark does not name its own function:\n%s", window)
	}

	// The guard must reject a partly instrumented file, which would
	// under-report coverage instead of failing.
	stripped := strings.Replace(string(generated), "("+lispCoverageMarkBoundary+" \"to_json_schema\")", ":null", 1)
	partial := ProvenanceReport{Files: map[string]ProvenanceFileMetrics{}}
	auditLispProvenanceExtras(model, specs, map[string]string{lispProvenanceCoreFile: stripped}, &partial)
	complained := false
	for _, violation := range partial.Violations {
		if strings.Contains(violation, "coverage marks") {
			complained = true
		}
	}
	if !complained {
		t.Error("the coverage guard accepts a partly instrumented file")
	}

	// Turning instrumentation off must remove every mark and the manifest
	// entry together, so the two can never disagree.
	lispEmitCoverageMarks = false
	defer func() { lispEmitCoverageMarks = true }()
	offCore, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit with instrumentation off: %v", err)
	}
	if strings.Contains(offCore, "("+lispCoverageMarkBoundary+" ") {
		t.Error("instrumentation is off but marks are still emitted")
	}
	offManifestJSON, err := BuildLispCoreBoundaryManifest(model)
	if err != nil {
		t.Fatalf("manifest with instrumentation off: %v", err)
	}
	if strings.Contains(offManifestJSON, lispCoverageMarkBoundary) {
		t.Error("instrumentation is off but the manifest still declares the boundary")
	}
}

// TestLispBoundaryAritiesMatchTheNativeDefinitions is the check that a
// boundary existing is not the same as a boundary being callable. Common
// Lisp resolves a call at run time, so passing three arguments to a
// two-argument boundary compiles and loads cleanly and fails only when that
// path first runs.
func TestLispBoundaryAritiesMatchTheNativeDefinitions(t *testing.T) {
	manifest := lispTestManifest(t)
	shapes, read, err := LoadLispNativeLambdaShapes(filepath.Dir(lispRuntimePath()), filepath.Base(lispGeneratedPath()))
	if err != nil {
		t.Fatalf("read native sources: %v", err)
	}
	if read == 0 {
		t.Fatal("no native Lisp files were read")
	}
	covered := 0
	for _, boundary := range manifest.Boundaries {
		if _, ok := shapes[boundary.Name]; ok {
			covered++
		}
	}
	if covered == 0 {
		t.Fatal("no declared boundary is defined natively; the parser is not matching definitions")
	}
	if problems := CheckLispBoundaryArities(manifest, shapes); len(problems) > 0 {
		for _, problem := range problems {
			t.Errorf("%s", problem)
		}
	}
	t.Logf("%d of %d declared boundaries are defined natively, all with compatible argument counts",
		covered, len(manifest.Boundaries))
}

// TestLispLambdaListParsing pins the lambda-list reader the arity check
// depends on. Each case is a shape the native sources actually use, and a
// misread would either hide a real arity bug or invent one.
func TestLispLambdaListParsing(t *testing.T) {
	source := strings.Join([]string{
		"(defun plain (a b) a)",
		"(defun with-optional (target key &optional (fallback :null)) target)",
		"(defun variadic (template &rest arguments) template)",
		"(defun none () nil)",
		"(defgeneric generic-call (target method args))",
		"(defun keyworded (value &key stream) value)",
	}, "\n")
	shapes := ParseLispLambdaLists("probe.lisp", "(in-package #:axllm/core)\n"+source, LispCorePackage)
	for _, testCase := range []struct {
		name     string
		required int
		optional int
		accepts  []int
		rejects  []int
	}{
		{name: "plain", required: 2, accepts: []int{2}, rejects: []int{1, 3}},
		{name: "with-optional", required: 2, optional: 1, accepts: []int{2, 3}, rejects: []int{1, 4}},
		{name: "variadic", required: 1, accepts: []int{1, 2, 9}, rejects: []int{0}},
		{name: "none", accepts: []int{0}, rejects: []int{1}},
		{name: "generic-call", required: 3, accepts: []int{3}, rejects: []int{2, 4}},
		{name: "keyworded", required: 1, accepts: []int{1, 3}, rejects: []int{0}},
	} {
		shape, ok := shapes[testCase.name]
		if !ok {
			t.Errorf("%s was not parsed", testCase.name)
			continue
		}
		if shape.Required != testCase.required {
			t.Errorf("%s required = %d, want %d", testCase.name, shape.Required, testCase.required)
		}
		if testCase.optional > 0 && shape.Optional != testCase.optional {
			t.Errorf("%s optional = %d, want %d", testCase.name, shape.Optional, testCase.optional)
		}
		for _, arity := range testCase.accepts {
			if !shape.Accepts(arity) {
				t.Errorf("%s should accept %d argument(s) (%s)", testCase.name, arity, shape.Describe())
			}
		}
		for _, arity := range testCase.rejects {
			if shape.Accepts(arity) {
				t.Errorf("%s should reject %d argument(s) (%s)", testCase.name, arity, shape.Describe())
			}
		}
	}

	// And the check built on it must actually report a mismatch.
	wrong := map[string]LispLambdaShape{"core-get": {Name: "core-get", File: "probe.lisp", Required: 2}}
	problems := CheckLispBoundaryArities(LispBoundaryManifest{
		Boundaries: []LispBoundaryEntry{{Name: "core-get", ObservedArities: []int{3}}},
	}, wrong)
	if len(problems) != 1 {
		t.Fatalf("a two-argument definition called with three arguments must be reported, got %d problem(s)", len(problems))
	}
	if !strings.Contains(problems[0].String(), "core-get") {
		t.Errorf("problem %q does not name the boundary", problems[0])
	}
}

// TestLispConformanceDeclarationDrivesTheClaim checks the hook that keeps
// the manifest from being permanently not-claimed. A suite becomes claimed
// because the native side declares a runner that covers it, never because
// the compiler decided to say so.
func TestLispConformanceDeclarationDrivesTheClaim(t *testing.T) {
	lispUseUndeclaredNativeFixture(t)
	model := lispTestModel(t)

	// With no declaration nothing is claimed.
	manifest, err := BuildLispCapabilityManifest(model)
	if err != nil {
		t.Fatalf("capability manifest: %v", err)
	}
	nativeDir, err := LispNativeSourceDir()
	if err != nil {
		t.Fatalf("native sources: %v", err)
	}
	declaration, err := LoadLispConformanceDeclaration(nativeDir)
	if err != nil {
		t.Fatalf("declaration: %v", err)
	}
	if declaration == nil && len(manifest.SupportedSuites) != 0 {
		t.Errorf("no runner is declared but the manifest claims %v", manifest.SupportedSuites)
	}

	// A declaration must name a runner that exists.
	dir := t.TempDir()
	write := func(t *testing.T, body string) {
		t.Helper()
		if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile), []byte(body), 0o644); err != nil {
			t.Fatalf("write declaration: %v", err)
		}
	}
	write(t, `{"runner":"tests/missing.lisp","command":["sbcl","--script","tests/missing.lisp"],"suites":["signature"]}`)
	if _, err := LoadLispConformanceDeclaration(dir); err == nil {
		t.Error("a declaration naming a runner that does not exist must fail")
	}

	if err := os.MkdirAll(filepath.Join(dir, "tests"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	// LispNativeSourceDir identifies a package by its ASDF system, so the
	// probe directory needs one before it can stand in for packages/lisp.
	if err := os.WriteFile(filepath.Join(dir, "axllm.asd"), []byte(`(defsystem "axllm" :components ((:file "src/core")))`), 0o644); err != nil {
		t.Fatalf("write asd: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "tests", "run.lisp"), []byte("; runner\n"), 0o644); err != nil {
		t.Fatalf("write runner: %v", err)
	}
	write(t, `{"runner":"tests/run.lisp","command":["sbcl","--script","tests/run.lisp"],"suites":["signature","nonsense"]}`)
	if _, err := LoadLispConformanceDeclaration(dir); err == nil {
		t.Error("a declaration claiming an unknown suite must fail")
	}
	write(t, `{"runner":"tests/run.lisp","suites":["signature"]}`)
	if _, err := LoadLispConformanceDeclaration(dir); err == nil {
		t.Error("a declaration with no command must fail")
	}
	write(t, `{"runner":"tests/run.lisp","command":["sbcl","--script","tests/run.lisp"],"suites":["signature","schema"]}`)
	accepted, err := LoadLispConformanceDeclaration(dir)
	if err != nil {
		t.Fatalf("a valid declaration must load: %v", err)
	}
	if len(accepted.Suites) != 2 {
		t.Errorf("declaration suites = %v", accepted.Suites)
	}

	// Pointing the emitter at that package must claim exactly those suites
	// and mark them semantic in the coverage manifest.
	t.Setenv(LispNativeSourceEnv, dir)
	claimedManifest, err := BuildLispCapabilityManifest(model)
	if err != nil {
		t.Fatalf("capability manifest with a declaration: %v", err)
	}
	if strings.Join(claimedManifest.SupportedSuites, ",") != "schema,signature" {
		t.Errorf("claimed suites = %v, want the declared two", claimedManifest.SupportedSuites)
	}
	coverage, err := BuildLispConformanceCoverage(model)
	if err != nil {
		t.Fatalf("coverage with a declaration: %v", err)
	}
	for _, suite := range []string{"signature", "schema"} {
		if coverage.Suites[suite][0].Category != "semantic" {
			t.Errorf("declared suite %q is categorised %q", suite, coverage.Suites[suite][0].Category)
		}
	}
	for _, suite := range []string{"axgen", "axai"} {
		if coverage.Suites[suite][0].Category != "explicitly-not-claimed" {
			t.Errorf("undeclared suite %q is categorised %q", suite, coverage.Suites[suite][0].Category)
		}
	}
	if err := ValidateConformanceCoverage(claimedManifest, coverage); err != nil {
		t.Errorf("a partially claimed manifest must still validate: %v", err)
	}
}

// TestLispProvenanceGuardsCatchMissingDeclarations checks the declarations
// guard, which is what keeps the forward declarations complete. Without it,
// a dropped declaration would show up only as SBCL noise about an undefined
// function, hiding a genuinely missing native dependency in the same noise.
func TestLispProvenanceGuardsCatchMissingDeclarations(t *testing.T) {
	model := lispTestModel(t)
	specs, err := LispCoreFunctions(model)
	if err != nil {
		t.Fatalf("functions: %v", err)
	}
	generated, err := BuildLispCore(model)
	if err != nil {
		t.Fatalf("emit: %v", err)
	}
	clean := ProvenanceReport{Files: map[string]ProvenanceFileMetrics{}}
	auditLispProvenanceExtras(model, specs, map[string]string{lispProvenanceCoreFile: generated}, &clean)
	if len(clean.Violations) > 0 {
		t.Fatalf("the generated file fails its own guards:\n  %s", strings.Join(clean.Violations, "\n  "))
	}

	for _, testCase := range []struct{ name, remove, expect string }{
		{"a dropped function declaration", "\n                to-json-schema\n", "to-json-schema"},
		{"a dropped boundary declaration", "\n                core-string-slice\n", "core-string-slice"},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			if !strings.Contains(generated, testCase.remove) {
				t.Fatalf("the declarations do not contain %q, so the mutation is not valid", strings.TrimSpace(testCase.remove))
			}
			broken := strings.Replace(generated, testCase.remove, "\n", 1)
			report := ProvenanceReport{Files: map[string]ProvenanceFileMetrics{}}
			auditLispProvenanceExtras(model, specs, map[string]string{lispProvenanceCoreFile: broken}, &report)
			named := false
			for _, violation := range report.Violations {
				if strings.Contains(violation, testCase.expect) && strings.Contains(violation, "forward declaration") {
					named = true
				}
			}
			if !named {
				t.Errorf("removing %q was not reported; violations:\n  %s",
					strings.TrimSpace(testCase.remove), strings.Join(report.Violations, "\n  "))
			}
		})
	}
}

// TestLispVerifyCompilesTheEmittedPackage runs the verify step itself, so
// the registration is covered end to end rather than only through its
// pieces: emit a package, then compile it in SBCL with no boundary defined.
func TestLispVerifyCompilesTheEmittedPackage(t *testing.T) {
	sbcl := lispFindSBCL(t)
	model := lispTestModel(t)
	dir := t.TempDir()
	if err := EmitLisp(model, dir); err != nil {
		t.Fatalf("emit: %v", err)
	}
	if err := VerifyLispManifest(dir); err != nil {
		t.Fatalf("manifest: %v", err)
	}
	message, err := VerifyLispGeneratedFileCompiles(sbcl, filepath.Join(dir, filepath.FromSlash(lispProvenanceCoreFile)), dir)
	if err != nil {
		t.Fatalf("sbcl compile: %v", err)
	}
	if !strings.Contains(message, "full-warnings=0 style-warnings=0") {
		t.Errorf("unexpected compile result: %s", message)
	}

	// The check must actually fail on a file that calls an undeclared
	// name, or a clean result would prove nothing.
	core, err := os.ReadFile(filepath.Join(dir, filepath.FromSlash(lispProvenanceCoreFile)))
	if err != nil {
		t.Fatalf("read emitted core: %v", err)
	}
	broken := strings.Replace(string(core), "(core-string-slice ", "(core-undeclared-boundary ", 1)
	if broken == string(core) {
		t.Fatal("could not introduce an undeclared call; the mutation is not valid")
	}
	brokenPath := filepath.Join(dir, "broken.lisp")
	if err := os.WriteFile(brokenPath, []byte(broken), 0o644); err != nil {
		t.Fatalf("write broken file: %v", err)
	}
	if _, err := VerifyLispGeneratedFileCompiles(sbcl, brokenPath, dir); err == nil {
		t.Error("a call to an undeclared boundary must fail the compile check")
	}
}

// ---------------------------------------------------------------------
// Conformance runner execution
// ---------------------------------------------------------------------

// lispWriteConformanceDeclaration writes a declaration naming a shell
// runner, so the execution path can be exercised without SBCL or the Lisp
// dependencies. The runner is a real program and its exit status is real;
// only the work it does is stubbed.
func lispWriteConformanceDeclaration(t *testing.T, dir, script string, suites []string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Join(dir, "tests"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	runner := filepath.Join("tests", "probe-runner.sh")
	if err := os.WriteFile(filepath.Join(dir, runner), []byte(script), 0o755); err != nil {
		t.Fatalf("write runner: %v", err)
	}
	declaration := LispConformanceDeclaration{
		Runner:  filepath.ToSlash(runner),
		Command: []string{"sh", filepath.ToSlash(runner)},
		Suites:  suites,
	}
	data, err := json.MarshalIndent(declaration, "", "  ")
	if err != nil {
		t.Fatalf("marshal declaration: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile), data, 0o644); err != nil {
		t.Fatalf("write declaration: %v", err)
	}
}

func lispStepByPrefix(report VerifyTargetReport, prefix string) (VerifyStep, bool) {
	for _, step := range report.Steps {
		if strings.HasPrefix(step.Name, prefix) {
			return step, true
		}
	}
	return VerifyStep{}, false
}

// TestLispVerifyRunsTheDeclaredConformanceRunner covers the execution path
// of the conformance hook. The cases that matter are the ones where a
// claimed suite could end up with nothing behind it: a runner that fails, a
// runner whose interpreter is missing, and a runner that cannot find the
// conformance fixtures because the environment was not passed through.
func TestLispVerifyRunsTheDeclaredConformanceRunner(t *testing.T) {
	lispUseUndeclaredNativeFixture(t)
	model := lispTestModel(t)
	conformanceRoot := filepath.Join(repoRootPath(), "ir", "conformance")
	absConformanceRoot, err := filepath.Abs(conformanceRoot)
	if err != nil {
		t.Fatalf("resolve conformance root: %v", err)
	}

	emit := func(t *testing.T) string {
		t.Helper()
		dir := t.TempDir()
		if err := EmitLisp(model, dir); err != nil {
			t.Fatalf("emit: %v", err)
		}
		return dir
	}

	// lispReportScript writes a conformance report covering exactly the
	// fixtures the given suites hold on disk, which is what an honest run
	// produces. The script also records the environment it was handed.
	lispReportScript := func(t *testing.T, suites []string, mutate func(fixtures []LispConformanceFixture) []LispConformanceFixture) string {
		t.Helper()
		var fixtures []LispConformanceFixture
		for _, suite := range LispRequiredConformanceSuites(suites) {
			ids, err := LispSuiteFixtureInventory(absConformanceRoot, suite)
			if err != nil {
				t.Fatalf("inventory %s: %v", suite, err)
			}
			for _, id := range ids {
				fixtures = append(fixtures, LispConformanceFixture{ID: id, Category: "semantic"})
			}
		}
		if mutate != nil {
			fixtures = mutate(fixtures)
		}
		payload, err := json.Marshal(LispConformanceReport{SchemaVersion: LispConformanceReportSchema, Fixtures: fixtures})
		if err != nil {
			t.Fatalf("marshal report: %v", err)
		}
		return "#!/bin/sh\nprintf '%s' \"$AXIR_CONFORMANCE_DIR\" > seen-conformance-dir\nprintf '%s' \"$AXIR_AXJS_RUNTIME_SERVER\" > seen-runtime-server\ncat > \"$AXIR_CONFORMANCE_REPORT\" <<'REPORT'\n" +
			string(payload) + "\nREPORT\nexit 0\n"
	}

	t.Run("a runner that exits 0 without a report fails", func(t *testing.T) {
		// This is the hole this whole mechanism exists to close: the old
		// positive test used exactly this script and passed.
		dir := emit(t)
		lispWriteConformanceDeclaration(t, dir, "#!/bin/sh\nprintf '%s' \"$AXIR_CONFORMANCE_DIR\" > seen-conformance-dir\nexit 0\n", []string{"signature"})
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		if err == nil {
			t.Fatal("a runner that dispatches nothing and exits 0 must not pass a claimed suite")
		}
		step, ok := lispStepByPrefix(report, "conformance evidence")
		if !ok {
			t.Fatal("no conformance evidence step was recorded")
		}
		if step.Status != "fail" {
			t.Errorf("conformance evidence status = %q, want fail", step.Status)
		}
		if !strings.Contains(step.Message, "zero exit status alone") {
			t.Errorf("the failure does not explain that exit 0 is not evidence: %q", step.Message)
		}
	})

	t.Run("a runner with a complete report is reported ok", func(t *testing.T) {
		dir := emit(t)
		lispWriteConformanceDeclaration(t, dir, lispReportScript(t, []string{"signature"}, nil), []string{"signature"})
		declaration, err := LoadLispConformanceDeclaration(dir)
		if err != nil {
			t.Fatal(err)
		}
		declaration.NoKeyExamples = []string{"examples/probe.lisp"}
		data, err := json.Marshal(declaration)
		if err != nil {
			t.Fatal(err)
		}
		if err := writeFiles(dir, map[string]string{
			LispConformanceDeclarationFile: string(data),
			"examples/probe.lisp": `(assert (not (sb-ext:posix-getenv "OPENAI_API_KEY")))
(assert (not (sb-ext:posix-getenv "OPENAI_APIKEY")))
(with-open-file (out "example-ran" :direction :output) (write-line "ran" out))`,
		}); err != nil {
			t.Fatal(err)
		}
		t.Setenv("OPENAI_API_KEY", "test-key-not-a-secret")
		t.Setenv("OPENAI_APIKEY", "test-key-not-a-secret")
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		if err != nil {
			t.Fatalf("a complete report must pass: %v", err)
		}
		step, ok := lispStepByPrefix(report, "conformance evidence")
		if !ok {
			t.Fatal("no conformance evidence step was recorded")
		}
		if step.Status != "ok" {
			t.Errorf("conformance evidence status = %q, want ok (%s)", step.Status, step.Message)
		}
		seen, err := os.ReadFile(filepath.Join(dir, "seen-conformance-dir"))
		if err != nil {
			t.Fatalf("the runner did not record its environment: %v", err)
		}
		// The emitted runner resolves fixtures against the repository, not
		// against the temporary package directory it was copied into.
		if string(seen) != conformanceRoot && string(seen) != absConformanceRoot {
			t.Errorf("runner saw AXIR_CONFORMANCE_DIR=%q, want the verify conformance root %q", seen, conformanceRoot)
		}
		server, err := os.ReadFile(filepath.Join(dir, "seen-runtime-server"))
		wantServer := filepath.Join(filepath.Dir(filepath.Dir(absConformanceRoot)), "tools", "axir", "adapters", "axjs-runtime-server.ts")
		if err != nil || string(server) != wantServer {
			t.Errorf("temporary package resolved production adapter to %q, want %q: %v", server, wantServer, err)
		}
		if _, err := os.Stat(filepath.Join(dir, "example-ran")); err != nil {
			t.Fatalf("the declared no-key example did not execute: %v", err)
		}
	})

	t.Run("a failing declared example fails verification", func(t *testing.T) {
		dir := emit(t)
		lispWriteConformanceDeclaration(t, dir, lispReportScript(t, []string{"signature"}, nil), []string{"signature"})
		declaration, err := LoadLispConformanceDeclaration(dir)
		if err != nil {
			t.Fatal(err)
		}
		declaration.NoKeyExamples = []string{"examples/fails.lisp"}
		data, err := json.Marshal(declaration)
		if err != nil {
			t.Fatal(err)
		}
		if err := writeFiles(dir, map[string]string{
			LispConformanceDeclarationFile: string(data),
			"examples/fails.lisp":          "(error \"example failed\")\n",
		}); err != nil {
			t.Fatal(err)
		}
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		step, ok := lispStepByPrefix(report, "example ")
		if err == nil || !ok || step.Status != "fail" {
			t.Fatalf("failed example passed verification: %+v, %v", report, err)
		}
	})

	t.Run("a stale report from an earlier run is not reused", func(t *testing.T) {
		dir := emit(t)
		// Write a complete report where the verifier will look, then give
		// it a runner that writes nothing. If the path were fixed and not
		// cleared, this would pass on last run's evidence.
		lispWriteConformanceDeclaration(t, dir, "#!/bin/sh\nexit 0\n", []string{"signature"})
		stale := lispReportScript(t, []string{"signature"}, nil)
		_ = stale
		payload, err := json.Marshal(LispConformanceReport{SchemaVersion: LispConformanceReportSchema,
			Fixtures: []LispConformanceFixture{{ID: "signature/stale.json", Category: "semantic"}}})
		if err != nil {
			t.Fatalf("marshal: %v", err)
		}
		if err := os.WriteFile(filepath.Join(dir, "axir-lisp-conformance-report.json"), payload, 0o644); err != nil {
			t.Fatalf("write stale report: %v", err)
		}
		if _, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot); err == nil {
			t.Fatal("a stale report must not satisfy a run that produced none")
		}
	})

	t.Run("a failing runner fails verification", func(t *testing.T) {
		dir := emit(t)
		lispWriteConformanceDeclaration(t, dir, "#!/bin/sh\necho 'signature: 3 fixtures failed' >&2\nexit 1\n", []string{"signature"})
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		if err == nil {
			t.Fatal("a runner that exits nonzero must fail verification; a claimed suite cannot pass on a red runner")
		}
		step, ok := lispStepByPrefix(report, "conformance")
		if !ok {
			t.Fatal("no conformance step was recorded")
		}
		if step.Status != "fail" {
			t.Errorf("conformance status = %q, want fail", step.Status)
		}
		if !strings.Contains(step.Message, "fixtures failed") {
			t.Errorf("the failure message does not carry the runner's output: %q", step.Message)
		}
	})

	t.Run("a declared runner with no interpreter fails rather than skips", func(t *testing.T) {
		dir := emit(t)
		lispWriteConformanceDeclaration(t, dir, "#!/bin/sh\nexit 0\n", []string{"signature"})
		// Replace the command with an interpreter that does not exist.
		declaration := LispConformanceDeclaration{
			Runner:  "tests/probe-runner.sh",
			Command: []string{"axir-no-such-interpreter", "tests/probe-runner.sh"},
			Suites:  []string{"signature"},
		}
		data, err := json.MarshalIndent(declaration, "", "  ")
		if err != nil {
			t.Fatalf("marshal: %v", err)
		}
		if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile), data, 0o644); err != nil {
			t.Fatalf("write declaration: %v", err)
		}
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		if err == nil {
			t.Fatal("a declared runner whose interpreter is missing must fail: the manifest already claims its suites")
		}
		step, ok := lispStepByPrefix(report, "conformance")
		if !ok {
			t.Fatal("no conformance step was recorded")
		}
		if step.Status != "fail" {
			t.Errorf("conformance status = %q, want fail; skipping would leave the claim standing with nothing behind it", step.Status)
		}
		if !strings.Contains(step.Message, "axir-no-such-interpreter") {
			t.Errorf("the failure does not name the missing interpreter: %q", step.Message)
		}
	})

	t.Run("no declaration fails required verification", func(t *testing.T) {
		dir := emit(t)
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		if err == nil {
			t.Fatal("compilation without a declaration must not pass")
		}
		step, ok := lispStepByPrefix(report, "conformance")
		if !ok {
			t.Fatal("no conformance step was recorded")
		}
		if step.Status != "fail" {
			t.Errorf("conformance status = %q, want fail when nothing is claimed", step.Status)
		}
		if !strings.Contains(step.Message, LispConformanceDeclarationFile) {
			t.Errorf("the skip does not say what is missing: %q", step.Message)
		}
	})

	t.Run("a malformed declaration fails rather than being ignored", func(t *testing.T) {
		dir := emit(t)
		if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile),
			[]byte(`{"runner":"tests/absent.lisp","command":["sh"],"suites":["signature"]}`), 0o644); err != nil {
			t.Fatalf("write declaration: %v", err)
		}
		if _, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot); err == nil {
			t.Fatal("a declaration naming a runner that does not exist must fail verification")
		}
	})

	t.Run("the package shape is verified before conformance", func(t *testing.T) {
		dir := emit(t)
		mustRemoveForTest(t, filepath.Join(dir, "src", "core-runtime.lisp"))
		report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: dir}, conformanceRoot)
		if err == nil {
			t.Fatal("a package missing its native boundaries must fail verification")
		}
		step, ok := lispStepByPrefix(report, "package shape")
		if !ok {
			t.Fatal("no package shape step was recorded")
		}
		if step.Status != "fail" {
			t.Errorf("package shape status = %q, want fail", step.Status)
		}
	})
}

// TestLispLambdaListParsingIsPackageAware is the regression test for a
// scanner defect that produced false negatives: a boundary written as
// (defun axllm/core::core-program-components (program) ...) was recorded
// under the token "axllm/core::core-program-components", so --verify-runtime
// reported it undefined even though the native file defines it.
//
// The mirror-image mistake is just as bad: counting a same-named definition
// in another package would report a boundary as implemented when nothing
// implements it. Both directions are checked here.
func TestLispLambdaListParsingIsPackageAware(t *testing.T) {
	for _, testCase := range []struct {
		name     string
		source   string
		defines  bool
		required int
	}{
		{
			name:     "qualified definition from another package",
			source:   "(in-package #:axllm)\n(defun axllm/core::core-program-components (program) program)",
			defines:  true,
			required: 1,
		},
		{
			name:     "single-colon qualified definition",
			source:   "(in-package #:axllm)\n(defun axllm/core:core-program-components (program) program)",
			defines:  true,
			required: 1,
		},
		{
			name:     "bare definition inside the Core package",
			source:   "(in-package #:axllm/core)\n(defun core-program-components (program) program)",
			defines:  true,
			required: 1,
		},
		{
			name:    "bare definition in the facade package is a different symbol",
			source:  "(in-package #:axllm)\n(defun core-program-components (program) program)",
			defines: false,
		},
		{
			name:    "same name qualified into another package is a different symbol",
			source:  "(in-package #:axllm/core)\n(defun axllm::core-program-components (program) program)",
			defines: false,
		},
		{
			name:    "a third package with a similar prefix is still a different symbol",
			source:  "(in-package #:axllm)\n(defun axllm/core-extras::core-program-components (program) program)",
			defines: false,
		},
		{
			name:    "a definition before any in-package form is not in the Core package",
			source:  "(defun core-program-components (program) program)",
			defines: false,
		},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			shapes := ParseLispLambdaLists("probe.lisp", testCase.source, LispCorePackage)
			shape, ok := shapes["core-program-components"]
			if ok != testCase.defines {
				t.Fatalf("defines core-program-components = %v, want %v (parsed %v)", ok, testCase.defines, shapes)
			}
			if !testCase.defines {
				// It must not have been recorded under its raw token either,
				// which is how the original defect hid.
				for name := range shapes {
					if strings.Contains(name, "core-program-components") {
						t.Errorf("recorded %q, which is not a symbol in %s", name, LispCorePackage)
					}
				}
				return
			}
			if shape.Required != testCase.required {
				t.Errorf("required = %d, want %d", shape.Required, testCase.required)
			}
			if !shape.Accepts(1) {
				t.Errorf("should accept 1 argument, accepts %s", shape.Describe())
			}
		})
	}

	// And the end-to-end effect: a qualified definition must satisfy the
	// boundary check rather than being reported missing.
	manifest := LispBoundaryManifest{Boundaries: []LispBoundaryEntry{
		{Name: "core-program-components", ObservedArities: []int{1}},
	}}
	shapes := ParseLispLambdaLists("flow.lisp",
		"(in-package #:axllm)\n(defun axllm/core::core-program-components (program) program)", LispCorePackage)
	if _, ok := shapes["core-program-components"]; !ok {
		t.Fatal("a qualified definition must count as defining the boundary")
	}
	if problems := CheckLispBoundaryArities(manifest, shapes); len(problems) > 0 {
		t.Errorf("a correct qualified definition must not be reported: %v", problems)
	}
}

// TestLispNativeSourcesResolveQualifiedDefinitions checks the real native
// tree, so the fix is proven against the files the package actually ships
// rather than only against synthetic input.
func TestLispNativeSourcesResolveQualifiedDefinitions(t *testing.T) {
	dir := filepath.Dir(lispRuntimePath())
	shapes, read, err := LoadLispNativeLambdaShapes(dir, filepath.Base(lispGeneratedPath()))
	if err != nil {
		t.Fatalf("read native sources: %v", err)
	}
	if read == 0 {
		t.Fatal("no native Lisp files were read")
	}
	for name := range shapes {
		if strings.Contains(name, ":") {
			t.Errorf("recorded %q with its package qualifier still attached; boundary lookups would miss it", name)
		}
	}
	// Any boundary the native tree defines with a qualifier must resolve.
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read dir: %v", err)
	}
	qualified := regexp.MustCompile(`(?mi)^\((?:defun|defmacro|defgeneric)[ \t]+` +
		regexp.QuoteMeta(LispCorePackage) + `::?([^\s()]+)`)
	found := 0
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".lisp") || entry.Name() == filepath.Base(lispGeneratedPath()) {
			continue
		}
		text, err := os.ReadFile(filepath.Join(dir, entry.Name()))
		if err != nil {
			t.Fatalf("read %s: %v", entry.Name(), err)
		}
		for _, match := range qualified.FindAllStringSubmatch(string(text), -1) {
			found++
			if _, ok := shapes[match[1]]; !ok {
				t.Errorf("%s defines %s::%s but the scanner did not record it", entry.Name(), LispCorePackage, match[1])
			}
		}
	}
	t.Logf("%d qualified native definition(s) resolved across %d file(s); %d names recorded", found, read, len(shapes))
}

// TestLispConformanceReportReconciliation covers every way a report can
// finish a run without proving the suites the manifest claims. Each case is
// a shape a real runner could plausibly emit, and each one must be refused
// by name rather than ignored.
func TestLispConformanceReportReconciliation(t *testing.T) {
	conformanceRoot, err := filepath.Abs(filepath.Join(repoRootPath(), "ir", "conformance"))
	if err != nil {
		t.Fatalf("resolve conformance root: %v", err)
	}
	complete := func(t *testing.T, suites ...string) []LispConformanceFixture {
		t.Helper()
		var out []LispConformanceFixture
		for _, suite := range LispRequiredConformanceSuites(suites) {
			ids, err := LispSuiteFixtureInventory(conformanceRoot, suite)
			if err != nil {
				t.Fatalf("inventory %s: %v", suite, err)
			}
			for _, id := range ids {
				out = append(out, LispConformanceFixture{ID: id, Category: "semantic"})
			}
		}
		if len(out) == 0 {
			t.Fatalf("no fixtures found for %v; the inventory is not reading the tree", suites)
		}
		return out
	}

	t.Run("a complete report reconciles", func(t *testing.T) {
		declaration := &LispConformanceDeclaration{Runner: "r", Command: []string{"sh"}, Suites: []string{"schema"}}
		report := &LispConformanceReport{SchemaVersion: LispConformanceReportSchema, Fixtures: complete(t, "schema")}
		if err := ReconcileLispConformanceReport(report, declaration, conformanceRoot); err != nil {
			t.Errorf("a complete report must reconcile: %v", err)
		}
	})

	for _, testCase := range []struct {
		name   string
		suites []string
		mutate func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture
		schema string
		expect string
	}{
		{
			name:   "an absent fixture",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture { return fixtures[1:] },
			expect: "unaccounted for",
		},
		{
			name:   "a duplicate record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				return append(fixtures, fixtures[0])
			},
			expect: "more than once",
		},
		{
			name:   "a failed record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				fixtures[0].Category = "failed"
				return fixtures
			},
			expect: "does not show the fixture was exercised",
		},
		{
			name:   "a blocked record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				fixtures[0].Category = "blocked"
				return fixtures
			},
			expect: "does not show the fixture was exercised",
		},
		{
			name:   "a partial record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				fixtures[0].Category = "partial"
				return fixtures
			},
			expect: "does not show the fixture was exercised",
		},
		{
			name:   "a skipped record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				fixtures[0].Category = "skipped"
				return fixtures
			},
			expect: "does not show the fixture was exercised",
		},
		{
			name:   "an explicitly-not-claimed record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				fixtures[0].Category = "explicitly-not-claimed"
				return fixtures
			},
			expect: "does not show the fixture was exercised",
		},
		{
			name:   "a presence-only record",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				fixtures[0].Category = "presence-only"
				return fixtures
			},
			expect: "does not show the fixture was exercised",
		},
		{
			name:   "a record for an unclaimed suite",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				return append(fixtures, LispConformanceFixture{ID: "axgen/anything.json", Category: "semantic"})
			},
			expect: "does not claim suite",
		},
		{
			name:   "a record for a fixture that is not on disk",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				return append(fixtures, LispConformanceFixture{ID: "schema/invented.json", Category: "semantic"})
			},
			expect: "not a fixture on disk",
		},
		{
			name:   "an id with no suite segment",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				return append(fixtures, LispConformanceFixture{ID: "loose.json", Category: "semantic"})
			},
			expect: "does not name a suite",
		},
		{
			name:   "an empty id",
			suites: []string{"schema"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				return append(fixtures, LispConformanceFixture{ID: "  ", Category: "semantic"})
			},
			expect: "empty id",
		},
		{
			name:   "the wrong schema version",
			suites: []string{"schema"},
			schema: "axir-lisp-conformance-v0",
			expect: "schema_version",
		},
		{
			name:   "claiming axagent without the real-engine tree",
			suites: []string{"axagent"},
			mutate: func(t *testing.T, fixtures []LispConformanceFixture) []LispConformanceFixture {
				var out []LispConformanceFixture
				for _, fixture := range fixtures {
					if !strings.HasPrefix(fixture.ID, "axagent-real/") {
						out = append(out, fixture)
					}
				}
				return out
			},
			expect: "axagent-real",
		},
	} {
		t.Run(testCase.name, func(t *testing.T) {
			fixtures := complete(t, testCase.suites...)
			if testCase.mutate != nil {
				fixtures = testCase.mutate(t, fixtures)
			}
			schema := LispConformanceReportSchema
			if testCase.schema != "" {
				schema = testCase.schema
			}
			declaration := &LispConformanceDeclaration{Runner: "r", Command: []string{"sh"}, Suites: testCase.suites}
			err := ReconcileLispConformanceReport(&LispConformanceReport{SchemaVersion: schema, Fixtures: fixtures}, declaration, conformanceRoot)
			if err == nil {
				t.Fatalf("%s must be refused", testCase.name)
			}
			if !strings.Contains(err.Error(), testCase.expect) {
				t.Errorf("error does not name %q:\n%v", testCase.expect, err)
			}
		})
	}

	// Claiming the agent surface obliges the real-engine tree, which is
	// where model-authored code actually runs.
	required := LispRequiredConformanceSuites([]string{"axagent"})
	if strings.Join(required, ",") != "axagent,axagent-real" {
		t.Errorf("claiming axagent requires %v; the real-engine tree must be included", required)
	}
	if got := LispRequiredConformanceSuites([]string{"schema"}); strings.Join(got, ",") != "schema" {
		t.Errorf("claiming schema must not pull in other suites, got %v", got)
	}

	// The contract text the native runner's author reads must name the
	// things a runner has to get right.
	contract := LispConformanceReportContract()
	for _, want := range []string{
		LispConformanceReportEnv, LispConformanceReportSchema,
		"axagent-real", "semantic", "validation-error", "transport-boundary",
		"exactly once", "actually successful dispatch",
	} {
		if !strings.Contains(contract, want) {
			t.Errorf("the published contract does not mention %q", want)
		}
	}
}
