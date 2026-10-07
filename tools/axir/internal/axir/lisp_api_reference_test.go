package axir

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// lispAPIReferencePackageDir is the real package this metadata documents. The tests read
// it rather than a fixture: the point of this file is that the published API
// matches the package as it actually is.
func lispAPIReferencePackageDir(t *testing.T) string {
	t.Helper()
	dir, err := filepath.Abs(filepath.Join("..", "..", "..", "..", "packages", "lisp"))
	if err != nil {
		t.Fatalf("resolve packages/lisp: %v", err)
	}
	if _, err := os.Stat(filepath.Join(dir, "src", "package.lisp")); err != nil {
		t.Skipf("packages/lisp is not present: %v", err)
	}
	return dir
}

func loadLispAPI(t *testing.T) LispPackageAPI {
	t.Helper()
	api, err := LoadLispPackageAPI(lispAPIReferencePackageDir(t))
	if err != nil {
		t.Fatalf("LoadLispPackageAPI: %v", err)
	}
	return api
}

func TestLoadLispPackageAPIReadsEveryExportForm(t *testing.T) {
	api := loadLispAPI(t)
	if len(api.Exports) < 200 {
		t.Fatalf("expected the package to export hundreds of names, got %d", len(api.Exports))
	}
	// One name from each of the three export forms the package uses:
	// defpackage's (:export #:name ...), a file-local (export '(name ...)),
	// and (export +optimizer-exports+) naming a defparameter list.
	for name, form := range map[string]string{
		"forward":          "defpackage (:export ...)",
		"mcp-app-bridge":   "(export '(...)) in mcp-app.lisp",
		"optimize-program": "(export +optimizer-exports+) in optimize.lisp",
	} {
		export, ok := api.Export(name)
		if !ok {
			t.Fatalf("export %q from %s was not read", name, form)
		}
		if !export.Defined() {
			t.Fatalf("export %q from %s resolved to no definition", name, form)
		}
	}
}

func TestLoadLispPackageAPIResolvesDefinitions(t *testing.T) {
	api := loadLispAPI(t)
	cases := []struct {
		name      string
		kind      string
		lambdaHas string
	}{
		{"ax", "function", "signature"},
		{"forward", "generic function", "options"},
		{"ai", "function", "name"},
		{"best-of-n", "function", "reward-fn"},
		{"refine-error", "condition", ""},
		{"synth-generate", "function", "count"},
	}
	for _, test := range cases {
		export, ok := api.Export(test.name)
		if !ok {
			t.Fatalf("%q is not exported", test.name)
		}
		if export.Kind != test.kind {
			t.Errorf("%q kind = %q, want %q", test.name, export.Kind, test.kind)
		}
		// The file a symbol lives in is not pinned: the package moves
		// definitions between facade files, and pinning one made this test
		// fail for a correct parser when the ai factory moved from ai.lisp
		// to provider.lisp. What must hold is that the definition comes from
		// the hand-written facade rather than the generated Core, and that
		// it is a real source location.
		if export.File == "" || export.Line <= 0 {
			t.Errorf("%q has no source location", test.name)
		}
		if lispGeneratedCoreFile(export.File) {
			t.Errorf("%q resolved to generated Core (%s); the facade must win", test.name, export.File)
		}
		if !strings.HasSuffix(export.File, ".lisp") {
			t.Errorf("%q resolved to %q, which is not a Lisp source file", test.name, export.File)
		}
		if test.lambdaHas != "" && !strings.Contains(export.LambdaList, test.lambdaHas) {
			t.Errorf("%q lambda list %q does not mention %q", test.name, export.LambdaList, test.lambdaHas)
		}
	}
	// One symbol the generated Core also defines, to pin the precedence the
	// website depends on.
	if export, ok := api.Export("parse-signature"); !ok || lispGeneratedCoreFile(export.File) {
		t.Errorf("parse-signature resolved to %q, want the facade", export.File)
	}
}

// The parser must not invent exports: a name the package only defines
// internally stays internal, and an unknown name is absent.
func TestLoadLispPackageAPIKeepsInternalsInternal(t *testing.T) {
	api := loadLispAPI(t)
	for _, name := range []string{"%refine-reward", "%synth-label", "definitely-not-an-export"} {
		if _, ok := api.Export(name); ok {
			t.Errorf("%q must not be reported as an export", name)
		}
	}
	if !api.Defines("%refine-reward") {
		t.Errorf("the package defines %%refine-reward; the parser lost it")
	}
}

func TestBuildLispAPIReferenceManifestUsesRealExports(t *testing.T) {
	api := loadLispAPI(t)
	manifest, gaps, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	if manifest.Target != "lisp" || manifest.PackageName != "axllm" {
		t.Fatalf("manifest target/package = %q/%q", manifest.Target, manifest.PackageName)
	}
	required := []string{"signatures", "axgen", "axai", "agents-rlm", "flow", "tools", "mcp", "runtime-profiles", "optimizers", "errors-values"}
	sections := map[string]APIReferenceSection{}
	for _, section := range manifest.Sections {
		sections[section.ID] = section
	}
	symbolCount := 0
	for _, section := range manifest.Sections {
		symbolCount += len(section.Symbols)
	}
	t.Logf("inventory: %d exports, %d undefined, %d unread components; reference: %d sections, %d symbols, %d canonical gaps",
		len(api.Exports), len(api.UndefinedExports()), len(api.MissingComponents), len(manifest.Sections), symbolCount, len(gaps))
	for _, id := range required {
		section, ok := sections[id]
		if !ok {
			t.Fatalf("missing required section %q", id)
		}
		if len(section.Symbols) == 0 {
			t.Fatalf("section %q documents nothing", id)
		}
	}
	// Every documented symbol, form and example names only real exports.
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			name := symbol.PublicName
			export, ok := api.Export(name)
			if !ok || !export.Defined() {
				t.Errorf("section %q documents %q, which is not a defined export", section.ID, symbol.PublicName)
				continue
			}
			// The website prints public_name verbatim, so it must be the
			// exported Lisp name in lisp-case, not a qualified or camelCase
			// spelling.
			if strings.ContainsAny(symbol.PublicName, ":() ") || strings.ToLower(symbol.PublicName) != symbol.PublicName {
				t.Errorf("public_name %q is not a bare lisp-case export name", symbol.PublicName)
			}
			if strings.TrimSpace(symbol.Example) == "" {
				t.Errorf("symbol %q has no example", symbol.PublicName)
			}
			for _, text := range []string{symbol.Form, symbol.Example} {
				referenced := LispAPIReferencedExports(text)
				if len(referenced) == 0 {
					t.Errorf("symbol %q has a form/example that references no export: %q", symbol.PublicName, text)
				}
				for _, other := range referenced {
					if other, ok := api.Export(other); !ok || !other.Defined() {
						t.Errorf("symbol %q references %q, which is not a defined export", symbol.PublicName, text)
					}
				}
			}
			if !strings.Contains(symbol.Example, "ax:"+name) && !strings.Contains(symbol.Example, "ax:"+strings.TrimPrefix(name, "make-")) {
				// Covered by the reference check above; logged so a reviewer
				// can see which examples drive a symbol indirectly.
				// An example may drive a symbol through a sibling export (a
				// constructor or an accessor), but it must mention the
				// package at least once, which the reference check above
				// already proved.
				t.Logf("note: example for %q drives it through a sibling export: %s", symbol.PublicName, symbol.Example)
			}
		}
	}
	if len(gaps) == 0 {
		t.Fatalf("expected honest gaps for canonical symbols the package lacks")
	}
	for _, gap := range gaps {
		if strings.TrimSpace(gap.Reason) == "" || strings.TrimSpace(gap.Section) == "" {
			t.Errorf("gap %q has no reason or section", gap.CanonicalName)
		}
		for _, section := range manifest.Sections {
			for _, symbol := range section.Symbols {
				if symbol.CanonicalName == gap.CanonicalName {
					t.Errorf("%q is reported as missing and documented at the same time", gap.CanonicalName)
				}
			}
		}
	}
}

// The gaps are the honest ones: a canonical symbol with no counterpart must
// be reported, never silently mapped onto an unrelated export.
func TestLispAPIReferenceReportsKnownGaps(t *testing.T) {
	api := loadLispAPI(t)
	_, gaps, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	reported := map[string]string{}
	for _, gap := range gaps {
		reported[gap.CanonicalName] = gap.Reason
	}
	for _, canonical := range []string{
		"OpenAICompatibleClient", "OpenAIResponsesClient",
		"GoogleGeminiClient", "AnthropicClient", "AxCredentialRequest", "AxCredentialProvider",
		"AxUsageContext", "AxUsageEvent", "AxUsageObserver", "AxTracer", "AxMeter", "set_usage_observer",
	} {
		if reported[canonical] == "" {
			t.Errorf("canonical symbol %q is neither documented nor reported as a gap", canonical)
		}
	}
	// A symbol the package really has must not be reported as a gap. The
	// last four were reported absent by an earlier run that read an
	// incomplete source tree, which is what this list guards against.
	for _, canonical := range []string{
		"s", "ax", "ai", "agent", "flow", "fn", "optimize", "AxMCPClient",
		"AxProviderDescriptor", "provider_profiles", "AxRateLimitInfo", "AxAIService.getMetrics",
		// These five were reported absent by an earlier run against a tree
		// without provider.lisp and telemetry.lisp: provider takes a
		// :credential-provider callback, and the globals and the fail-open
		// tracing boundary are exported.
		"provider", "AxGlobals", "set_tracer", "set_meter", "set_rate_limiter",
		"AxRuntimeHooks", "start_active_span",
		"get_supported_ai_models", "model_catalog_summary", "model_info",
	} {
		if reason, ok := reported[canonical]; ok {
			t.Errorf("canonical symbol %q is implemented but reported missing: %s", canonical, reason)
		}
	}
	// Every reason must be specific: it names what the package does instead,
	// and any symbol it names must be a real export, so a reason cannot
	// describe an API that does not exist either.
	for _, gap := range gaps {
		if len(gap.Reason) < 40 {
			t.Errorf("gap %q has a vague reason %q", gap.CanonicalName, gap.Reason)
		}
		for _, word := range strings.FieldsFunc(gap.Reason, func(r rune) bool {
			return !(r == '-' || r == '*' || (r >= 'a' && r <= 'z'))
		}) {
			if len(word) < 6 || !strings.Contains(word, "-") {
				continue
			}
			if _, isExport := api.Export(word); isExport {
				continue
			}
			// A hyphenated word that is not an export may still be prose
			// ("per-provider"); only flag the ones that look like calls.
			if strings.HasPrefix(word, "ax-") || strings.HasPrefix(word, "program-") || strings.HasPrefix(word, "provider-") || strings.HasPrefix(word, "mcp-") {
				t.Errorf("gap %q names %q, which is not an export", gap.CanonicalName, word)
			}
		}
	}
}

func TestLispAPIReferenceCatalogueAndBoundaryContracts(t *testing.T) {
	api := loadLispAPI(t)
	manifest, gaps, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatal(err)
	}
	symbols := map[string]APIReferenceSymbol{}
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			symbols[symbol.CanonicalName] = symbol
		}
	}
	for _, test := range []struct{ canonical, name, form, returns string }{
		{"get_supported_ai_models", "supported-ai-models", "(ax:supported-ai-models &optional model-type)", "a JSON array of provider entries with their model catalogues"},
		{"model_catalog_summary", "model-catalog-summary", "(ax:model-catalog-summary)", "a JSON object describing catalogue coverage"},
		{"model_info", "model-info", "(ax:model-info profile model)", "the model's catalogue entry, or :null"},
		{"provider_profiles", "provider-profiles", "(ax:provider-profiles)", "a Lisp list of profile id strings"},
	} {
		symbol := symbols[test.canonical]
		if symbol.PublicName != test.name || symbol.Form != test.form || symbol.Returns != test.returns {
			t.Errorf("%s: got name=%q form=%q returns=%q", test.canonical, symbol.PublicName, symbol.Form, symbol.Returns)
		}
	}
	for canonical, phrases := range map[string][]string{
		"get_supported_ai_models": {"best-effort", "unknown type returns the whole catalogue"},
		"model_catalog_summary":   {"audit", "not a model list"},
		"ai":                      {"Omit :model", "without a default requires :model"},
		"provider":                {"Omit :model", "profile, operation, method and url", "header object", "authentication error", "ai factory does not accept"},
		"AxAIService.getMetrics":  {"not a host telemetry meter"},
	} {
		for _, phrase := range phrases {
			if !strings.Contains(symbols[canonical].Description, phrase) {
				t.Errorf("%s description omits contract %q", canonical, phrase)
			}
		}
	}
	for _, gap := range gaps {
		if gap.CanonicalName == "set_usage_observer" && (!strings.Contains(gap.Reason, "onUsage") || !strings.Contains(gap.Reason, "chat path reads")) {
			t.Errorf("observer setter gap must acknowledge the working chat callback: %s", gap.Reason)
		}
	}
}

// Catalogue entries are public now. Losing an export or its definition must
// fail the build, not downgrade it to a gap through an internal-symbol fallback.
func TestLispAPIReferenceCatalogueRequiresDefinedExports(t *testing.T) {
	for _, name := range []string{"supported-ai-models", "model-catalog-summary", "model-info"} {
		t.Run(name, func(t *testing.T) {
			api := loadLispAPI(t)
			export, ok := api.Export(name)
			if !ok || !export.Defined() {
				t.Fatalf("%s must be a defined export", name)
			}
			delete(api.byName, name)
			if _, _, err := BuildLispAPIReferenceManifest(api); err == nil || !strings.Contains(err.Error(), "does not export") {
				t.Fatalf("missing export must fail: %v", err)
			}
			export.Kind = ""
			api.byName[name] = export
			if _, _, err := BuildLispAPIReferenceManifest(api); err == nil || !strings.Contains(err.Error(), "never defined") {
				t.Fatalf("undefined export must fail: %v", err)
			}
		})
	}
}

// A documented entry that drifts away from the package must fail the build
// rather than ship a form nobody can evaluate.
func TestValidateLispAPIReferenceManifestRejectsUnknownExports(t *testing.T) {
	api := loadLispAPI(t)
	manifest, _, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	if err := ValidateLispAPIReferenceManifest(manifest, api); err != nil {
		t.Fatalf("the built manifest must validate: %v", err)
	}

	renamed := cloneLispManifest(manifest)
	renamed.Sections[0].Symbols[0].PublicName = "ax:no-such-function"
	if err := ValidateLispAPIReferenceManifest(renamed, api); err == nil {
		t.Errorf("a public name that is not an export was accepted")
	}

	badExample := cloneLispManifest(manifest)
	badExample.Sections[0].Symbols[0].Example = "(ax:not-exported 1)"
	if err := ValidateLispAPIReferenceManifest(badExample, api); err == nil {
		t.Errorf("an example referencing a missing export was accepted")
	}

	badForm := cloneLispManifest(manifest)
	badForm.Sections[0].Symbols[0].Form = "(ax:imaginary-form x)"
	if err := ValidateLispAPIReferenceManifest(badForm, api); err == nil {
		t.Errorf("a form referencing a missing export was accepted")
	}

	dropped := cloneLispManifest(manifest)
	dropped.Sections = dropped.Sections[1:]
	if err := ValidateLispAPIReferenceManifest(dropped, api); err == nil {
		t.Errorf("a manifest missing a required section was accepted")
	}

	empty := cloneLispManifest(manifest)
	empty.Sections[0].Symbols = nil
	if err := ValidateLispAPIReferenceManifest(empty, api); err == nil {
		t.Errorf("a section with no symbols was accepted")
	}
}

func cloneLispManifest(manifest APIReferenceManifest) APIReferenceManifest {
	out := manifest
	out.Sections = make([]APIReferenceSection, len(manifest.Sections))
	for index, section := range manifest.Sections {
		copied := section
		copied.Symbols = append([]APIReferenceSymbol{}, section.Symbols...)
		out.Sections[index] = copied
	}
	return out
}

// The website maps subsystem pages by section title, so the Lisp titles are
// the same ten the generated targets use. A renamed title silently drops a
// page, so it is pinned here.
func TestLispAPIReferenceSectionTitlesMatchTheWebsiteMapping(t *testing.T) {
	api := loadLispAPI(t)
	manifest, _, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	want := []struct{ id, title string }{
		{"signatures", "Signatures"},
		{"axgen", "AxGen"},
		{"axai", "AxAI"},
		{"agents-rlm", "Agents And RLM"},
		{"flow", "Flow"},
		{"tools", "Tools"},
		{"mcp", "MCP"},
		{"runtime-profiles", "Runtime Profiles"},
		{"optimizers", "Optimizers"},
		{"errors-values", "Errors And Values"},
	}
	if len(manifest.Sections) != len(want) {
		t.Fatalf("got %d sections, want %d", len(manifest.Sections), len(want))
	}
	for index, expected := range want {
		if manifest.Sections[index].ID != expected.id || manifest.Sections[index].Title != expected.title {
			t.Errorf("section %d = %q/%q, want %q/%q", index, manifest.Sections[index].ID, manifest.Sections[index].Title, expected.id, expected.title)
		}
	}
}

func TestLispAPIReferenceFormsAreNative(t *testing.T) {
	api := loadLispAPI(t)
	manifest, _, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	forms := map[string]string{}
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			forms[symbol.CanonicalName] = symbol.Form
		}
	}
	// The form comes from the real lambda list, with default values removed.
	if got := forms["ax"]; !strings.HasPrefix(got, "(ax:ax signature &key description tools max-steps max-retries") {
		t.Errorf("ax form = %q, want the real lambda list without defaults", got)
	}
	if got := forms["AxGen.forward"]; got != "(ax:forward program client inputs &optional options)" {
		t.Errorf("forward form = %q", got)
	}
	for canonical, form := range forms {
		if strings.Contains(form, "Ax.") || strings.Contains(form, "::") || strings.Contains(form, "new ") {
			t.Errorf("%q has a non-Lisp form %q", canonical, form)
		}
		if strings.Contains(form, "(") && !strings.HasPrefix(form, "(") {
			t.Errorf("%q form %q is not a Lisp form", canonical, form)
		}
	}
}

func TestLispAPIReferenceRendersJSONAndMarkdown(t *testing.T) {
	api := loadLispAPI(t)
	manifest, gaps, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	encoded, err := LispAPIReferenceJSON(manifest, gaps)
	if err != nil {
		t.Fatalf("LispAPIReferenceJSON: %v", err)
	}
	for _, want := range []string{`"target": "lisp"`, `"package_name": "axllm"`, `"schema_version": "axir-api-v1"`, `"missing_canonical_symbols"`, `"public_name": "forward"`, `"title": "Agents And RLM"`} {
		if !strings.Contains(encoded, want) {
			t.Errorf("axir-api.json is missing %s", want)
		}
	}
	if strings.Contains(encoded, `"python"`) {
		t.Errorf("axir-api.json leaked a Python name")
	}

	markdown := LispAPIReferenceMarkdown(manifest, gaps, api)
	if !strings.Contains(markdown, "```lisp\n") {
		t.Errorf("API.md has no Lisp fence")
	}
	if strings.Contains(markdown, "```python") || strings.Contains(markdown, "```rust") {
		t.Errorf("API.md has a fence for another language")
	}
	for _, want := range []string{
		"# axllm API reference",
		"## Not in this package",
		"`ax:forward`",
		"- Defined in: `src/gen.lisp`",
		"get_supported_ai_models",
	} {
		if !strings.Contains(markdown, want) {
			t.Errorf("API.md is missing %q", want)
		}
	}
	// Every fenced example in the markdown must still reference only real
	// exports; a fence is where a reader copies from.
	for _, block := range strings.Split(markdown, "```lisp\n")[1:] {
		code := strings.SplitN(block, "```", 2)[0]
		for _, name := range LispAPIReferencedExports(code) {
			if export, ok := api.Export(name); !ok || !export.Defined() {
				t.Errorf("API.md example references %q, which is not a defined export:\n%s", name, code)
			}
		}
	}
}

// The emitter wiring is one call, and it produces exactly the two files the
// website prepare step reads from packages/lisp.
func TestEmitLispAPIReferenceFiles(t *testing.T) {
	files, err := EmitLispAPIReferenceFiles(lispAPIReferencePackageDir(t))
	if err != nil {
		t.Fatalf("EmitLispAPIReferenceFiles: %v", err)
	}
	if len(files) != 2 {
		t.Fatalf("got %d files, want axir-api.json and API.md", len(files))
	}
	if !strings.Contains(files["axir-api.json"], `"target": "lisp"`) {
		t.Errorf("axir-api.json is not the Lisp manifest")
	}
	if !strings.HasPrefix(files["API.md"], "# axllm API reference") {
		t.Errorf("API.md does not start with the package heading")
	}
	if _, err := EmitLispAPIReferenceFiles(filepath.Join(os.TempDir(), "definitely-not-a-lisp-package")); err == nil {
		t.Errorf("a missing package directory was accepted")
	}
}

// An inventory read from fewer files than axllm.asd declares is reported, not
// silently published: that is what made an earlier run claim 11 defined
// exports were undefined.
func TestLoadLispPackageAPIReportsAnIncompleteInventory(t *testing.T) {
	complete := loadLispAPI(t)

	thin := t.TempDir()
	if err := os.MkdirAll(filepath.Join(thin, "src"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	asd := `(defsystem "axllm" :components ((:module "src" :components ((:file "package") (:file "gen") (:file "provider")))))`
	if err := os.WriteFile(filepath.Join(thin, "axllm.asd"), []byte(asd), 0o644); err != nil {
		t.Fatalf("write asd: %v", err)
	}
	source := `(defpackage #:axllm (:use #:cl) (:export #:only-here #:defined-elsewhere))
(defun only-here (x) "Only this one is here." x)
`
	if err := os.WriteFile(filepath.Join(thin, "src", "package.lisp"), []byte(source), 0o644); err != nil {
		t.Fatalf("write source: %v", err)
	}
	api, err := LoadLispPackageAPI(thin)
	if err != nil {
		t.Fatalf("LoadLispPackageAPI: %v", err)
	}
	if api.Complete() {
		t.Fatalf("an inventory missing gen and provider reported itself complete")
	}
	want := map[string]bool{"gen": true, "provider": true}
	for _, component := range api.MissingComponents {
		if !want[component] {
			t.Errorf("unexpected missing component %q", component)
		}
		delete(want, component)
	}
	if len(want) != 0 {
		t.Errorf("missing components not reported: %v", want)
	}
	// An export whose definition lives in a file that was not read must not
	// be presented as a package defect.
	if undefined := api.UndefinedExports(); len(undefined) != 1 || undefined[0] != "defined-elsewhere" {
		t.Errorf("undefined exports = %v", undefined)
	}

	// The incompleteness travels into both artifacts.
	manifest, gaps, err := BuildLispAPIReferenceManifest(complete)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	encoded, err := LispAPIReferenceJSONFor(manifest, gaps, api)
	if err != nil {
		t.Fatalf("LispAPIReferenceJSONFor: %v", err)
	}
	if !strings.Contains(encoded, `"unread_source_components"`) || !strings.Contains(encoded, `"provider"`) {
		t.Errorf("axir-api.json does not record the unread components")
	}
	markdown := LispAPIReferenceMarkdown(manifest, gaps, api)
	if !strings.Contains(markdown, "## Incomplete source inventory") || !strings.Contains(markdown, "`src/gen.lisp`") {
		t.Errorf("API.md does not record the unread components")
	}

	// A complete inventory says nothing about unread components.
	if complete.Complete() {
		clean, err := LispAPIReferenceJSONFor(manifest, gaps, complete)
		if err != nil {
			t.Fatalf("LispAPIReferenceJSONFor: %v", err)
		}
		if strings.Contains(clean, "unread_source_components") {
			t.Errorf("a complete inventory still reported unread components")
		}
		if strings.Contains(LispAPIReferenceMarkdown(manifest, gaps, complete), "## Incomplete source inventory") {
			t.Errorf("a complete inventory still rendered the incomplete-inventory section")
		}
	}
}

// With every declared component present the package has no undefined
// exports; the parent's strict gate checks the same thing from Lisp.
func TestLispPackageHasNoUndefinedExportsWhenComplete(t *testing.T) {
	api := loadLispAPI(t)
	if !api.Complete() {
		t.Skipf("inventory is missing %v; cannot judge undefined exports", api.MissingComponents)
	}
	if undefined := api.UndefinedExports(); len(undefined) > 0 {
		t.Errorf("exported but undefined: %v", undefined)
	}
}

// Every published form matches the lambda list of the function it documents.
func TestLispAPIReferenceFormsMatchTheirLambdaLists(t *testing.T) {
	api := loadLispAPI(t)
	manifest, _, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	for _, mismatch := range LispAPIFormMismatches(manifest, api) {
		t.Errorf("%s", mismatch)
	}
}

// Every published example is compiled by SBCL against the loaded package, so
// a wrong argument count or an unknown keyword fails here. Nothing runs: the
// examples live in functions that are never called and every free variable
// is bound to NIL, so no provider, transport or runtime is reached.
//
// Only a missing SBCL skips this check. An SBCL that is present and cannot
// load the package is a failure: skipping there would turn a broken package
// into a green gate, which is the thing this check exists to catch.
func TestLispAPIReferenceExamplesCompile(t *testing.T) {
	dir := lispAPIReferencePackageDir(t)
	api := loadLispAPI(t)
	manifest, _, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	source, err := LispAPIExampleCheckSource(manifest)
	if err != nil {
		t.Fatalf("LispAPIExampleCheckSource: %v", err)
	}
	output, runErr := runLispExampleCompileCheck(t, dir, source)
	if runErr != nil {
		t.Fatalf("published examples did not compile cleanly: %v\n%s", runErr, output)
	}
	for _, line := range strings.Split(output, "\n") {
		if strings.HasPrefix(line, "RESULT: ") {
			t.Log(line)
		}
	}
}

// Negative control: with a package that cannot be loaded, the same check must
// go red rather than skip. Without this, a load error silently passed the
// gate.
func TestLispAPIReferenceExampleCheckFailsOnABrokenPackage(t *testing.T) {
	if _, err := exec.LookPath("sbcl"); err != nil {
		t.Skipf("sbcl is not installed: %v", err)
	}
	broken := t.TempDir()
	if err := os.MkdirAll(filepath.Join(broken, "src"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	asd := "(defsystem \"axllm\" :serial t :components ((:module \"src\" :components ((:file \"broken\")))))\n"
	if err := os.WriteFile(filepath.Join(broken, "axllm.asd"), []byte(asd), 0o644); err != nil {
		t.Fatalf("write asd: %v", err)
	}
	source := "(error \"this package is deliberately broken\")\n"
	if err := os.WriteFile(filepath.Join(broken, "src", "broken.lisp"), []byte(source), 0o644); err != nil {
		t.Fatalf("write source: %v", err)
	}
	examples := ";;;; negative control\n(in-package #:cl-user)\n(defun ax-api-example-1 () (let ((client nil)) (declare (ignorable client)) client))\n"
	output, err := runLispExampleCompileCheck(t, broken, examples)
	if err == nil {
		t.Fatalf("a package that cannot be loaded passed the example check:\n%s", output)
	}
	if !strings.Contains(output, "deliberately broken") {
		t.Errorf("the failure does not name the load error: %v\n%s", err, output)
	}
	t.Logf("negative control is red, as required: %v", err)
}

// runLispExampleCompileCheck loads the package at dir and compiles source
// against it. It returns the SBCL output and an error when the package fails
// to load, when the compiler reports any warning, or when compile-file
// reports warnings-p or failure-p. Nothing in source is executed.
func runLispExampleCompileCheck(t *testing.T, dir, source string) (string, error) {
	t.Helper()
	sbcl, err := exec.LookPath("sbcl")
	if err != nil {
		t.Skipf("sbcl is not installed: %v", err)
	}
	work := t.TempDir()
	examples := filepath.Join(work, "examples.lisp")
	if err := os.WriteFile(examples, []byte(source), 0o644); err != nil {
		return "", fmt.Errorf("write examples: %w", err)
	}
	// There is deliberately no handler around the load: an unhandled error
	// ends the script with a non-zero status, which is the failure this
	// check has to report instead of skipping.
	script := `(require :asdf)
(asdf:load-asd #p"` + filepath.Join(dir, "axllm.asd") + `")
(asdf:load-system "axllm")
(let ((diagnostics '()))
  (multiple-value-bind (fasl warnings-p failure-p)
      (handler-bind ((warning (lambda (condition)
                                (push (princ-to-string condition) diagnostics)
                                (muffle-warning condition))))
        (compile-file #p"` + examples + `" :output-file #p"` + filepath.Join(work, "examples.fasl") + `" :verbose nil :print nil))
    (declare (ignore fasl))
    (dolist (diagnostic (reverse diagnostics))
      (format t "~&DIAGNOSTIC: ~a~%" (substitute #\Space #\Newline diagnostic)))
    (format t "~&RESULT: diagnostics=~a warnings-p=~a failure-p=~a~%"
            (length diagnostics) (if warnings-p "yes" "no") (if failure-p "yes" "no"))))
(uiop:quit 0)
`
	driver := filepath.Join(work, "driver.lisp")
	if err := os.WriteFile(driver, []byte(script), 0o644); err != nil {
		return "", fmt.Errorf("write driver: %w", err)
	}
	command := exec.Command(sbcl, "--script", driver)
	command.Dir = dir
	raw, runErr := command.CombinedOutput()
	output := string(raw)
	if runErr != nil {
		return output, fmt.Errorf("sbcl could not load the package or compile the examples: %w", runErr)
	}
	for _, line := range strings.Split(output, "\n") {
		if strings.HasPrefix(line, "DIAGNOSTIC: ") {
			t.Errorf("example diagnostic: %s", strings.TrimPrefix(line, "DIAGNOSTIC: "))
		}
	}
	if !strings.Contains(output, "RESULT: diagnostics=0 warnings-p=no failure-p=no") {
		if !strings.Contains(output, "RESULT: ") {
			return output, fmt.Errorf("the compile check produced no result line")
		}
		return output, fmt.Errorf("compile-file reported warnings or a failure")
	}
	return output, nil
}

// The website links each API row to /lisp/api/reference/#<public_name>, so
// every documented name must be anchor-safe and API.md must carry exactly
// that heading. A qualified heading would anchor as ax-parse-signature and
// leave every row pointing at a dead fragment.
func TestLispAPIReferenceHeadingsMatchTheWebsiteAnchors(t *testing.T) {
	api := loadLispAPI(t)
	manifest, gaps, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		t.Fatalf("BuildLispAPIReferenceManifest: %v", err)
	}
	for _, name := range LispAPIAnchorUnsafeNames(manifest) {
		t.Errorf("public name %q does not slug to itself; the website anchor would not match", name)
	}
	markdown := LispAPIReferenceMarkdown(manifest, gaps, api)
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			heading := "### `" + symbol.PublicName + "`\n"
			if !strings.Contains(markdown, heading) {
				t.Errorf("API.md has no heading %q", strings.TrimSpace(heading))
			}
			if strings.Contains(markdown, "### `ax:"+symbol.PublicName+"`") {
				t.Errorf("API.md heading for %q is package-qualified, which breaks its anchor", symbol.PublicName)
			}
			if !strings.Contains(markdown, "- Qualified: `ax:"+symbol.PublicName+"`") {
				t.Errorf("API.md does not show the qualified name for %q", symbol.PublicName)
			}
		}
	}
}
