// Lisp API reference metadata.
//
// The website language integration reads axir-api.json and API.md for every
// language. For the five generated backends those come from
// BuildAPIReferenceManifest, whose symbol names are derived from one
// canonical name per target. The Common Lisp package is not generated: it is
// a hand-written ASDF system whose public surface is whatever
// packages/lisp/src exports. Deriving its names from the Python column of
// mapTarget would document functions that do not exist.
//
// So this file reads the package instead. It parses the exports and the
// definitions behind them out of the real sources, builds the ten required
// API sections from exports that are actually there, renders native Lisp
// forms and examples, and reports every canonical symbol with no Lisp
// counterpart as a gap rather than inventing a class to satisfy the name.
//
// Nothing here emits a symbol the package does not export; that is enforced
// by ValidateLispAPIReferenceManifest and by lisp_api_reference_test.go.

package axir

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// LispAPITarget is the target name used in axir-api.json for the Common Lisp
// package, and LispAPIPackageName is its ASDF system name.
const (
	LispAPITarget      = "lisp"
	LispAPIPackageName = "axllm"
	lispAPIPackage     = "axllm"
	lispAPINickname    = "ax"
)

// LispExport is one exported symbol and the definition that backs it.
type LispExport struct {
	Name       string `json:"name"`
	Kind       string `json:"kind"`
	LambdaList string `json:"lambda_list,omitempty"`
	Doc        string `json:"doc,omitempty"`
	File       string `json:"file,omitempty"`
	Line       int    `json:"line,omitempty"`
}

// Defined reports whether the package has a definition for this export. An
// exported name with no definition is a public API that fails at the call
// site, so it is never documented.
func (export LispExport) Defined() bool { return export.Kind != "" }

// LispPackageAPI is the public surface of packages/lisp as its sources
// declare it: every exported name, and the definition each one resolves to.
type LispPackageAPI struct {
	Dir     string
	Exports []LispExport
	// SourceFiles are the .lisp files read and DeclaredComponents are the
	// components axllm.asd names. MissingComponents is the difference: a
	// declared component with no file on disk means this inventory is not
	// the whole package, which is reported rather than guessed around,
	// because a thin inventory silently produces thin metadata and false
	// "exported but undefined" claims.
	SourceFiles        []string
	DeclaredComponents []string
	MissingComponents  []string
	byName             map[string]LispExport
	defsOnly           map[string]LispExport
}

// Complete reports whether every component axllm.asd declares was read.
func (api LispPackageAPI) Complete() bool { return len(api.MissingComponents) == 0 }

// Export returns the exported symbol with this name.
func (api LispPackageAPI) Export(name string) (LispExport, bool) {
	export, ok := api.byName[name]
	return export, ok
}

// Defines reports whether the package defines this name at all, exported or
// not. Used to tell "exported but undefined" from "internal".
func (api LispPackageAPI) Defines(name string) bool {
	_, ok := api.defsOnly[name]
	return ok
}

// UndefinedExports lists exported names with no definition in the package.
func (api LispPackageAPI) UndefinedExports() []string {
	out := []string{}
	for _, export := range api.Exports {
		if !export.Defined() {
			out = append(out, export.Name)
		}
	}
	sort.Strings(out)
	return out
}

var (
	lispHashNameRe     = regexp.MustCompile(`#:([^\s()]+)`)
	lispQuotedExportRe = regexp.MustCompile(`(?s)\(export\s+'\(([^)]*)\)`)
	lispVarExportRe    = regexp.MustCompile(`\(export\s+(\+[^\s()]+\+)\s*\)`)
	lispCommentRe      = regexp.MustCompile(`;[^\n]*`)
	lispComponentRe    = regexp.MustCompile(`\(:file\s+"([^"]+)"\)`)
	lispBareNameRe     = regexp.MustCompile(`[^\s()']+`)
	// A definition form: the opening paren, the defining operator, and the
	// name, which may be bare (defun x) or parenthesized (defstruct (x ...)).
	lispDefRe = regexp.MustCompile(`(?m)^\((def[a-z-]+|define-condition|define-symbol-macro)\s+\(?\s*([^\s()]+)`)
	// A lambda list follows the name of a function-like definition.
	lispFunctionKinds = map[string]bool{
		"defun": true, "defgeneric": true, "defmacro": true, "defmethod": true,
	}
)

// LoadLispPackageAPI reads the Lisp package sources under dir (the directory
// holding src/) and returns what they export and define.
//
// Exports are declared three ways in this package and all three count:
// defpackage's (:export #:name ...), a file-local (export '(name ...)), and
// (export +some-exports+) naming a defparameter list, which optimize.lisp
// uses so a test can check each name defines something.
func LoadLispPackageAPI(dir string) (LispPackageAPI, error) {
	srcDir := filepath.Join(dir, "src")
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return LispPackageAPI{}, fmt.Errorf("read lisp package sources: %w", err)
	}
	files := []string{}
	for _, entry := range entries {
		if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".lisp") {
			continue
		}
		files = append(files, entry.Name())
	}
	sort.Strings(files)
	if len(files) == 0 {
		return LispPackageAPI{}, fmt.Errorf("no .lisp sources under %s", srcDir)
	}

	api := LispPackageAPI{Dir: dir, SourceFiles: files, byName: map[string]LispExport{}, defsOnly: map[string]LispExport{}}
	declared, err := lispDeclaredComponents(dir)
	if err != nil {
		return LispPackageAPI{}, err
	}
	api.DeclaredComponents = declared
	present := map[string]bool{}
	for _, file := range files {
		present[strings.TrimSuffix(file, ".lisp")] = true
	}
	for _, component := range declared {
		if !present[component] {
			api.MissingComponents = append(api.MissingComponents, component)
		}
	}
	exported := map[string]bool{}
	order := []string{}
	note := func(name string) {
		name = strings.ToLower(strings.TrimSpace(name))
		if name == "" || exported[name] {
			return
		}
		exported[name] = true
		order = append(order, name)
	}

	for _, file := range files {
		text, err := os.ReadFile(filepath.Join(srcDir, file))
		if err != nil {
			return LispPackageAPI{}, err
		}
		body := string(text)
		// Definitions first: every file contributes them.
		for _, match := range lispDefRe.FindAllStringSubmatchIndex(body, -1) {
			kind := body[match[2]:match[3]]
			name := strings.ToLower(body[match[4]:match[5]])
			if name == "" {
				continue
			}
			line := 1 + strings.Count(body[:match[0]], "\n")
			definition := LispExport{
				Name: name,
				Kind: lispDefinitionKind(kind),
				File: file,
				Line: line,
				Doc:  lispFirstDocLine(body[match[1]:]),
			}
			if lispFunctionKinds[kind] {
				definition.LambdaList = lispLambdaList(body[match[5]:])
			}
			// A later defmethod never replaces the defgeneric or defun that
			// introduced the name, so the documented form stays the contract.
			primary := true
			if existing, ok := api.defsOnly[name]; ok {
				// The generated Core (src/core.lisp, src/core-runtime.lisp)
				// defines some of the same names internally. The facade is
				// the public surface, so a facade definition replaces a Core
				// one and never the other way round.
				switch {
				case lispGeneratedCoreFile(existing.File) && !lispGeneratedCoreFile(file):
					primary = true
				case kind == "defmethod" || existing.Kind != "method":
					primary = false
				}
			}
			if primary {
				api.defsOnly[name] = definition
			}
			// A class, struct or condition also defines its accessors, which
			// are a large part of this package's public surface. They are
			// collected even when the type's own name is already known, so a
			// class named after an inherited accessor still contributes.
			if kind == "defclass" || kind == "defstruct" || kind == "define-condition" {
				form := lispBalancedForm(body[match[0]:])
				for _, accessor := range lispSlotAccessors(kind, name, form) {
					if _, ok := api.defsOnly[accessor]; ok {
						continue
					}
					api.defsOnly[accessor] = LispExport{
						Name: accessor,
						Kind: "accessor",
						File: file,
						Line: line,
						Doc:  fmt.Sprintf("Reads the %s %s.", name, lispDefinitionKind(kind)),
					}
				}
			}
		}
		// Exports.
		if strings.HasSuffix(file, "package.lisp") {
			// Only the (:export ...) clause counts: (:use #:cl) and the
			// second defpackage are in the same file.
			for _, clause := range lispBalancedClauses(body, "(:export") {
				for _, name := range lispHashNameRe.FindAllStringSubmatch(clause, -1) {
					note(name[1])
				}
			}
		}
		for _, section := range lispQuotedExportRe.FindAllStringSubmatch(body, -1) {
			for _, name := range lispBareNameRe.FindAllString(lispCommentRe.ReplaceAllString(section[1], ""), -1) {
				note(name)
			}
		}
		for _, match := range lispVarExportRe.FindAllStringSubmatch(body, -1) {
			for _, name := range lispExportListVariable(body, match[1]) {
				note(name)
			}
		}
	}

	for _, name := range order {
		export := LispExport{Name: name}
		if definition, ok := api.defsOnly[name]; ok {
			export = definition
		}
		api.Exports = append(api.Exports, export)
		api.byName[name] = export
	}
	return api, nil
}

// lispExportListVariable resolves (export +name+) to the symbols in the
// defparameter list that variable holds.
// lispDeclaredComponents reads the src components axllm.asd declares, so an
// inventory can say whether it read the whole package.
func lispDeclaredComponents(dir string) ([]string, error) {
	text, err := os.ReadFile(filepath.Join(dir, "axllm.asd"))
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, fmt.Errorf("read axllm.asd: %w", err)
	}
	// Only the axllm system's src module counts. axllm.asd also defines
	// axllm/tests and other systems whose components live elsewhere, and
	// counting those made an earlier run report fifteen "missing" files for
	// a package that loads cleanly.
	body := string(text)
	system := lispBalancedForm(body[strings.Index(body, `(defsystem "axllm"`):])
	if system == "" {
		return nil, nil
	}
	srcModule := ""
	for _, module := range lispBalancedClauses(system, "(:module") {
		if strings.Contains(module, `(:module "src"`) {
			srcModule = module
			break
		}
	}
	if srcModule == "" {
		srcModule = system
	}
	seen := map[string]bool{}
	out := []string{}
	for _, match := range lispComponentRe.FindAllStringSubmatch(srcModule, -1) {
		name := strings.TrimPrefix(match[1], "src/")
		if name == "" || seen[name] {
			continue
		}
		seen[name] = true
		out = append(out, name)
	}
	sort.Strings(out)
	return out, nil
}

func lispExportListVariable(body, variable string) []string {
	start := strings.Index(body, "(defparameter "+variable)
	if start < 0 {
		return nil
	}
	rest := body[start:]
	open := strings.Index(rest, "'(")
	if open < 0 {
		return nil
	}
	depth := 0
	end := -1
	for index := open + 1; index < len(rest); index++ {
		switch rest[index] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				end = index
			}
		}
		if end >= 0 {
			break
		}
	}
	if end < 0 {
		return nil
	}
	return lispBareNameRe.FindAllString(lispCommentRe.ReplaceAllString(rest[open+2:end], ""), -1)
}

// lispBalancedClauses returns every balanced form in body that starts with
// the given prefix, so a clause is read to its own closing paren rather than
// to the end of the file.
func lispBalancedClauses(body, prefix string) []string {
	out := []string{}
	for index := 0; ; {
		start := strings.Index(body[index:], prefix)
		if start < 0 {
			return out
		}
		start += index
		form := lispBalancedForm(body[start:])
		if form == "" {
			return out
		}
		out = append(out, form)
		index = start + len(form)
	}
}

// lispBalancedForm returns the balanced parenthesized form at the start of
// text, ignoring parens inside string literals.
func lispBalancedForm(text string) string {
	depth := 0
	inString := false
	for index := 0; index < len(text); index++ {
		switch text[index] {
		case '\\':
			if inString {
				index++
			}
		case '"':
			inString = !inString
		case '(':
			if !inString {
				depth++
			}
		case ')':
			if !inString {
				depth--
				if depth == 0 {
					return text[:index+1]
				}
			}
		}
	}
	return ""
}

var (
	lispAccessorOptionRe = regexp.MustCompile(`:(?:reader|accessor|writer)\s+([^\s()]+)`)
	lispConcNameRe       = regexp.MustCompile(`:conc-name\s+([^\s()]+)`)
)

// lispSlotAccessors returns the accessor names a defclass or defstruct
// defines: the :reader/:accessor/:writer options of a class, and the
// conc-name plus slot names of a struct.
func lispSlotAccessors(kind, name, form string) []string {
	out := []string{}
	seen := map[string]bool{}
	add := func(accessor string) {
		accessor = strings.ToLower(strings.TrimSpace(accessor))
		if accessor == "" || seen[accessor] {
			return
		}
		seen[accessor] = true
		out = append(out, accessor)
	}
	if kind == "defclass" || kind == "define-condition" {
		for _, match := range lispAccessorOptionRe.FindAllStringSubmatch(form, -1) {
			add(match[1])
		}
		return out
	}
	prefix := name + "-"
	if match := lispConcNameRe.FindStringSubmatch(form); match != nil {
		prefix = strings.Trim(match[1], "|")
	}
	// Struct slots are the forms after the name/options: (slot default ...)
	// or a bare slot name.
	body := form
	if open := strings.Index(body, "("); open >= 0 {
		body = body[open+1:]
	}
	fields := lispBalancedClauses(body, "(")
	for _, field := range fields {
		inner := strings.TrimSpace(strings.TrimPrefix(field, "("))
		slot := strings.Fields(inner + " ")[0]
		slot = strings.TrimSuffix(slot, ")")
		if strings.HasPrefix(slot, ":") || slot == "" {
			continue
		}
		add(prefix + slot)
	}
	for _, token := range strings.Fields(lispCommentRe.ReplaceAllString(body, "")) {
		if strings.ContainsAny(token, "()\"':") || token == "" {
			continue
		}
		add(prefix + strings.TrimSuffix(token, ")"))
	}
	return out
}

// lispGeneratedCoreFile reports whether a file is generated Core rather than
// the hand-written facade.
func lispGeneratedCoreFile(file string) bool {
	return file == "core.lisp" || file == "core-runtime.lisp"
}

func lispDefinitionKind(operator string) string {
	switch operator {
	case "defun":
		return "function"
	case "defmacro":
		return "macro"
	case "defgeneric":
		return "generic function"
	case "defmethod":
		return "method"
	case "defclass":
		return "class"
	case "defstruct":
		return "struct"
	case "define-condition":
		return "condition"
	case "defconstant":
		return "constant"
	case "defparameter", "defvar":
		return "variable"
	default:
		return strings.TrimPrefix(operator, "def")
	}
}

// lispLambdaList returns the balanced lambda list that starts at the first
// open paren of rest, with its default forms removed so a reader sees the
// parameter names rather than implementation values.
func lispLambdaList(rest string) string {
	open := strings.Index(rest, "(")
	if open < 0 {
		return ""
	}
	depth := 0
	for index := open; index < len(rest); index++ {
		switch rest[index] {
		case '(':
			depth++
		case ')':
			depth--
			if depth == 0 {
				return lispCommentRe.ReplaceAllString(strings.Join(strings.Fields(rest[open:index+1]), " "), "")
			}
		}
	}
	return ""
}

// lispFirstDocLine returns the first line of a definition's docstring, which
// is the package's own one-line description of the symbol.
func lispFirstDocLine(rest string) string {
	limit := len(rest)
	if limit > 4000 {
		limit = 4000
	}
	window := rest[:limit]
	quote := strings.Index(window, "\"")
	if quote < 0 {
		return ""
	}
	// Only a docstring that starts on the definition's first lines counts;
	// a string literal deeper in the body is code, not documentation.
	if strings.Count(window[:quote], "\n") > 3 {
		return ""
	}
	end := strings.Index(window[quote+1:], "\"")
	if end < 0 {
		return ""
	}
	text := window[quote+1 : quote+1+end]
	if line := strings.TrimSpace(strings.SplitN(text, "\n", 2)[0]); line != "" {
		return line
	}
	return ""
}

// APIReferenceGap is a canonical Ax symbol the Common Lisp package does not
// have. It is reported instead of being documented, so the website never
// shows a form a caller cannot evaluate.
type APIReferenceGap struct {
	CanonicalName string `json:"canonical_name"`
	Section       string `json:"section"`
	Reason        string `json:"reason"`
}

// lispAPIEntry maps one canonical Ax symbol to the Lisp export that really
// implements it, with the metadata the website needs. Example is a form that
// uses only exported names.
type lispAPIEntry struct {
	section     string
	canonical   string
	lisp        string
	kindFor     string
	description string
	options     []string
	returns     string
	form        string
	example     string
	// absentReason documents, in the manifest-building code, why a canonical
	// symbol has no Lisp counterpart. An entry with no lisp name and no
	// absentReason is a programming error, caught by the test.
	absentReason string
}

func lispAPISections() []struct {
	id      string
	title   string
	summary string
} {
	return []struct {
		id      string
		title   string
		summary string
	}{
		{"signatures", "Signatures", "Parse, render and introspect Ax signatures, and build JSON Schema from them. Signature semantics come from generated Core; this package chooses the Lisp names."},
		{"axgen", "AxGen", "Typed generation over a signature: prompt rendering, output parsing, correction turns, bounded tool rounds, and the program hooks an optimizer reads and rewrites."},
		{"axai", "AxAI", "Provider clients and the service protocol every client answers, plus cancellation, usage and rate-limit boundaries."},
		{"agents-rlm", "Agents And RLM", "Agents, their actor steps and action logs, and the host code-runtime boundary a runtime language plugs into."},
		{"flow", "Flow", "Composable program graphs: steps, branches, loops, parallel merges, traces and Mermaid rendering."},
		{"tools", "Tools", "Tool definitions, argument validation and bounded execution, including the function processor generated code resolves names through."},
		{"mcp", "MCP", "MCP clients, transports, tasks, the Apps host bridge, and the OAuth boundary."},
		{"runtime-profiles", "Runtime Profiles", "The runtime protocol a host code runtime speaks, and the envelopes it exchanges."},
		{"optimizers", "Optimizers", "Optimizer engines, evaluators, cost tracking, checkpoints and optimized-program records."},
		{"errors-values", "Errors And Values", "The JSON value model every surface shares, and the condition hierarchy Ax signals."},
	}
}

// lispAPIEntries is the whole documented surface. Every lisp name here is
// checked against the real exports before anything is written.
func lispAPIEntries() []lispAPIEntry {
	return []lispAPIEntry{
		// ---- signatures ----
		{section: "signatures", canonical: "s", lisp: "s", options: []string{":inputs", ":outputs", ":description"}, description: "Build a validated signature from field specs built with f, as TypeScript's s() with a field map. Signature text goes through parse-signature instead.", returns: "a signature record (a JSON object Core owns)", example: `(ax:s :inputs (ax:object "question" (ax:f "string")) :outputs (ax:object "answer" (ax:f "string")))`},
		{section: "signatures", canonical: "parse_signature", lisp: "parse-signature", description: "Parse and validate signature text.", returns: "a validated signature record", example: `(ax:parse-signature "review:string -> sentiment:class \"positive, negative\"")`},
		{section: "signatures", canonical: "f", lisp: "f", description: "Build a field type for a signature built from specs.", returns: "a field-type record", example: `(ax:f "number")`},
		{section: "signatures", canonical: "signature_to_string", lisp: "signature-string", description: "Render a signature back to signature text.", returns: "a string that parses to an equal signature", example: `(ax:signature-string (ax:parse-signature "question:string -> answer:string"))`},
		{section: "signatures", canonical: "signature_fields", lisp: "signature-fields", options: []string{":side :input", ":side :output"}, description: "The signature's fields on one side, in Ax's published camelCase shape.", returns: "a JSON array of field objects", example: `(ax:signature-fields (ax:parse-signature "question:string -> answer:string") :side :output)`},
		{section: "signatures", canonical: "to_json_schema", lisp: "json-schema", options: []string{":side", ":title", ":strict", ":flexible-json-as-string"}, description: "A JSON Schema for the signature's fields on one side.", returns: "a JSON Schema object", example: `(ax:json-schema (ax:parse-signature "question:string -> answer:string") :side :output :strict t)`},
		{section: "signatures", canonical: "render_prompt", lisp: "render-prompt", description: "Render the system and user messages Core builds for a signature and its input values.", returns: "a JSON array of chat messages", example: `(ax:render-prompt (ax:parse-signature "question:string -> answer:string") (ax:object "question" "why?"))`},

		// ---- axgen ----
		{section: "axgen", canonical: "ax", lisp: "ax", options: []string{":description", ":tools", ":max-steps", ":max-retries", ":id", ":instruction"}, description: "Create a generator for a signature.", returns: "a generator", example: `(ax:ax "question:string -> answer:string" :max-retries 1)`},
		{section: "axgen", canonical: "AxGen.forward", lisp: "forward", options: []string{`"maxSteps"`, `"maxRetries"`, `"freshMemory"`, `"model"`}, description: "Run a program against a client. Options are a JSON object of per-call settings; a method ignores keys it does not implement.", returns: "two values: the typed outputs object and this call's usage object", example: `(ax:forward (ax:ax "question:string -> answer:string") client (ax:object "question" "why?"))`},
		{section: "axgen", canonical: "AxGen.streamingForward", lisp: "program-streaming-forward", description: "Run a program with a streaming sink. A program that cannot be driven by a prefix refuses this call rather than inheriting a silent fallback to forward.", returns: "two values: the outputs object and the usage object", example: `(ax:program-streaming-forward program client inputs (ax:object "sink" sink))`},
		{section: "axgen", canonical: "AxProgram.getUsage", lisp: "program-usage", description: "The program's token usage so far, per ai and model.", returns: "a JSON array of usage objects", example: `(ax:program-usage program)`},
		{section: "axgen", canonical: "AxProgram.getTraces", lisp: "program-traces", description: "The program's completed runs, each with status, input, output, chat log and function calls.", returns: "a JSON array of trace objects", example: `(ax:program-traces program)`},
		{section: "axgen", canonical: "AxProgram.getChatLog", lisp: "program-chat-log", description: "The provider turns the program recorded, oldest first.", returns: "a JSON array of chat-log objects", example: `(ax:program-chat-log program)`},
		{section: "axgen", canonical: "AxProgram.setInstruction", lisp: "program-set-instruction", description: "Replace the program's prompt instruction text.", returns: "the program", example: `(ax:program-set-instruction program "Answer in one sentence.")`},
		{section: "axgen", canonical: "AxProgram.getOptimizableComponents", lisp: "program-optimizable-components", description: "The parts of the program an optimizer may rewrite, each with an id, owner, kind, current value and constraints.", returns: "a JSON array of component objects", example: `(ax:program-optimizable-components program)`},
		{section: "axgen", canonical: "AxProgram.applyOptimizedComponents", lisp: "program-apply-optimized-components", description: "Apply a component id to text map. An id the program does not own is ignored, so one map can be applied to a whole composition.", returns: "the program", example: `(ax:program-apply-optimized-components program (ax:object "root::instruction" "Be terse."))`},
		{section: "axgen", canonical: "AxProgram.getSignature", lisp: "program-signature", description: "The program's parsed signature, or :null when it declares none.", returns: "a signature record or :null", example: `(ax:program-signature program)`},
		{section: "axgen", canonical: "bestOfN", lisp: "best-of-n", options: []string{":n", ":reward-fn", ":threshold", ":fail-count", ":strategy", ":on-attempt"}, description: "Score several candidates of a program with a reward function and return the best.", returns: "a program that scores candidates", example: `(ax:best-of-n program :n 3 :reward-fn reward)`},
		{section: "axgen", canonical: "refine", lisp: "refine", options: []string{":rounds", ":samples-per-round", ":reward-fn", ":threshold", ":feedback-client"}, description: "Refine a program over reward-scored rounds, appending advice to its instruction components and restoring them afterwards.", returns: "a program that refines over rounds", example: `(ax:refine program :rounds 2 :reward-fn reward)`},
		{section: "axgen", canonical: "synth", lisp: "synth", options: []string{":teacher", ":domain", ":edge-cases", ":temperature", ":model"}, description: "Generate synthetic labelled examples for a signature with a teacher client.", returns: "a synthesizer", example: `(ax:synth "question:string -> answer:string" :teacher teacher :domain "support")`},
		{section: "axgen", canonical: "AxTestPrompt", lisp: "test-prompt", options: []string{":client", ":program", ":examples", ":debug"}, description: "Score a program over labelled examples with a metric function.", returns: "a test prompt", example: `(ax:test-prompt :client client :program program :examples examples)`},

		// ---- axai ----
		{section: "axai", canonical: "ai", lisp: "ai", options: []string{":name", ":model", ":api-key", ":base-url", ":transport", ":timeout", ":max-tokens"}, description: "Create a provider client. The provider is selected by name rather than by a per-provider class. Omit :model to use the profile's default from Core's descriptor; a profile without a default requires :model.", returns: "an AI client", example: `(ax:ai :name "openai" :model "gpt-6-luna")`},
		{section: "axai", canonical: "AxAIService.chat", lisp: "ax-chat", description: "The service protocol's chat call, which every client answers.", returns: "a normalized chat response object", example: `(ax:ax-chat client request (ax:object))`},
		{section: "axai", canonical: "AxAIService.stream", lisp: "ax-stream", description: "The service protocol's streaming chat call.", returns: "a stream handle", example: `(ax:ax-stream client request (ax:object))`},
		{section: "axai", canonical: "AxAIService.embed", lisp: "ax-embed", description: "The service protocol's embeddings call.", returns: "a normalized embeddings response object", example: `(ax:ax-embed client request (ax:object))`},
		{section: "axai", canonical: "AxAIService.getFeatures", lisp: "ax-features", description: "What the client supports for a model: functions, streaming, media and more.", returns: "a features object", example: `(ax:ax-features client "gpt-6-luna")`},
		{section: "axai", canonical: "chat", lisp: "chat", options: []string{":tools", ":tool-choice", ":model"}, description: "One normalized chat request against a client, as the generator issues it.", returns: "a response object with content, toolCalls and usage", example: `(ax:chat client messages :tool-choice :auto)`},
		{section: "axai", canonical: "get_supported_ai_models", lisp: "supported-ai-models", options: []string{"model-type"}, description: "Core's provider-model catalogue, optionally narrowed by model type, such as \"code\" or \"embeddings\". The filter is best-effort: types are trimmed and lowercased, and an omitted, blank or unknown type returns the whole catalogue. These are catalogue entries, not provider profile ids or a live provider inventory.", returns: "a JSON array of provider entries with their model catalogues", example: `(ax:supported-ai-models "code")`},
		{section: "axai", canonical: "model_catalog_summary", lisp: "model-catalog-summary", description: "Core's catalogue audit: its version, descriptor-covered provider ids and deferred provider ids. This is coverage metadata, not a model list; supported-ai-models returns the catalogue.", returns: "a JSON object describing catalogue coverage", example: `(ax:model-catalog-summary)`},
		{section: "axai", canonical: "model_info", lisp: "model-info", description: "Look up a model's catalogue entry under a provider profile. This is the metadata the expensive-model gate reads; a model absent from the catalogue returns :null.", returns: "the model's catalogue entry, or :null", example: `(ax:model-info "openai" "gpt-6-luna")`},
		{section: "axai", canonical: "provider_profiles", lisp: "provider-profiles", description: "Every provider profile Core knows, sorted, read from Core's registry. These are profile ids, not models; supported-ai-models returns the separate provider-model catalogue.", returns: "a Lisp list of profile id strings", example: `(ax:provider-profiles)`},
		{section: "axai", canonical: "AxRateLimitInfo", lisp: "rate-limiter-token-usage", kindFor: "class", description: "The token usage a rate limiter is given for a call, with the remaining budget read back through rate-limiter-available.", returns: "a rate-limiter token usage object", example: `(ax:rate-limiter-available limiter)`},
		{section: "axai", canonical: "AxAIService.getMetrics", lisp: "ax-metrics", description: "A service's latency and error metrics, read from the client. This is not a host telemetry meter.", returns: "a metrics object", example: `(ax:ax-metrics client)`},
		{section: "axai", canonical: "provider", lisp: "provider", options: []string{":profile", ":model", ":api-key", ":base-url", ":transport", ":credential-provider"}, description: "Create a provider client for a profile. Omit :model to use the profile's default from Core's descriptor; a profile without a default requires :model. :credential-provider takes a per-client function that receives a JSON object of profile, operation, method and url and returns a header object (a hash table). A non-function is a configuration error; a non-object result is an authentication error. The ai factory does not accept this callback keyword.", returns: "a provider client", example: `(ax:provider :profile "openai" :model "gpt-6-luna" :credential-provider handler)`},
		{section: "axai", canonical: "AxGlobals", lisp: "globals-snapshot", description: "An isolated snapshot of the process-wide Ax globals. The names are fixed and camelCase: signatureStrict, tracer, meter, rateLimiter, logger, optimizerLogger, debug, abortSignal, customLabels, onUsage, cachingFunction and functionResultFormatter. This is runtime context, not a serializable export.", returns: "a JSON object of the current globals", example: `(ax:globals-snapshot)`},
		{section: "axai", canonical: "set_tracer", lisp: "set-global", description: "Install a process-wide global by its camelCase name, such as \"tracer\". There is no setter per global, and a name the globals do not have is rejected rather than stored.", returns: "the value that was set", example: `(ax:set-global "tracer" tracer)`},
		{section: "axai", canonical: "set_meter", lisp: "update-globals", description: "Apply several globals at once, such as \"meter\" and \"rateLimiter\". Every name is validated before anything is applied, so a misspelling changes nothing.", returns: "a snapshot of the globals after the update", example: `(ax:update-globals (ax:object "meter" meter))`},
		{section: "axai", canonical: "set_rate_limiter", lisp: "reset-globals", description: "Globals are process state, so a test or a host that installed a tracer, meter or rate limiter can put the defaults back. A limiter itself is installed with set-global under \"rateLimiter\".", returns: "a snapshot of the restored globals", example: `(ax:reset-globals)`},
		{section: "axai", canonical: "start_active_span", lisp: "start-active-span-fail-open", description: "Run an operation inside a span from an explicit tracer. It fails open: a tracer that signals, or :null instead of a tracer, still runs the operation exactly once and preserves its values and its original error. No span is ended on the caller's behalf.", returns: "the operation's values", example: `(ax:start-active-span-fail-open tracer "ax.gen" (ax:object) :null handler)`},
		{section: "axai", canonical: "AxRuntimeHooks", lisp: "runtime-hook-frame", kindFor: "struct", description: "One call's resolved globals, carried in that call's options under a symbol key so JSON, cache and export see string keys only and a concurrent call cannot read another call's frame.", returns: "a runtime hook frame", example: `(ax:options-with-runtime-hook-frame options (ax:make-runtime-hook-frame))`},
		{section: "axai", canonical: "AxProviderDescriptor", lisp: "provider-descriptor-of", description: "The provider descriptor behind a client, which is how provider mapping stays Core-owned.", returns: "a provider descriptor record", example: `(ax:provider-name (ax:provider-descriptor-of client))`},
		{section: "axai", canonical: "AxAIService.getName", lisp: "ax-service-name", description: "The service name a client reports, which is how a caller tells providers apart without a per-provider class.", returns: "a string", example: `(ax:ax-service-name client)`},
		{section: "axai", canonical: "AxCancellationToken", lisp: "cancellation-token", kindFor: "class", description: "A cancellation token a caller passes into a call and cancels from another thread.", returns: "a cancellation token", example: `(ax:cancel (make-instance 'ax:cancellation-token) "user stopped")`},
		{section: "axai", canonical: "AxAIServiceAbortedError", lisp: "provider-error-aborted-p", description: "Whether a signalled provider error is a cancellation. The Lisp port reports an aborted call as a kind on the single provider-error condition rather than a separate class.", returns: "a generalized boolean", example: `(ax:provider-error-aborted-p condition)`},
		{section: "axai", canonical: "AxRateLimiter", lisp: "rate-limiter-acquire", description: "The rate-limiter protocol a host implements: acquire before a call, with the token usage and remaining budget read back.", returns: "nil once the call may proceed", example: `(ax:rate-limiter-acquire limiter (ax:usage-object 10 2))`},
		{section: "axai", canonical: "AxUsage", lisp: "usage-object", description: "Build the usage object every response and program carries.", returns: "a usage object with promptTokens, completionTokens and totalTokens", example: `(ax:usage-object 11 7)`},

		// ---- agents-rlm ----
		{section: "agents-rlm", canonical: "agent", lisp: "agent", options: []string{":options"}, description: "Create an agent for a signature.", returns: "an agent", example: `(ax:agent "question:string -> answer:string")`},
		{section: "agents-rlm", canonical: "AxAgent.forward", lisp: "agent-forward", description: "Run the agent's staged pipeline against a client.", returns: "two values: the outputs object and the usage object", example: `(ax:agent-forward agent client (ax:object "question" "why?"))`},
		{section: "agents-rlm", canonical: "AxAgent.streaming_forward", lisp: "agent-streaming-forward", description: "Run the agent with a streaming sink.", returns: "two values: the outputs object and the usage object", example: `(ax:agent-streaming-forward agent client inputs (ax:object "sink" sink))`},
		{section: "agents-rlm", canonical: "AxAgent.add_child_agent", lisp: "agent-add-child", description: "Add a child agent, which becomes a namespaced callable in the actor's inventory.", returns: "the agent", example: `(ax:agent-add-child parent "research" "summarize" child)`},
		{section: "agents-rlm", canonical: "AxAgent.getActionLog", lisp: "agent-action-log", description: "The agent's action log, in the order Core wrote the records.", returns: "a JSON array of action records", example: `(ax:agent-action-log agent)`},
		{section: "agents-rlm", canonical: "AxAgent.discover", lisp: "agent-discover", description: "The effect-only discovery call that loads full docs for a callable.", returns: "a discovery payload object", example: `(ax:agent-discover agent (ax:object "callables" (vector "tools.search")))`},
		{section: "agents-rlm", canonical: "AxCodeRuntime", lisp: "code-runtime", kindFor: "class", description: "The host code-runtime boundary: a runtime language, its usage instructions and the sessions it creates.", returns: "a code runtime", example: `(ax:runtime-language runtime)`},
		{section: "agents-rlm", canonical: "AxCodeSession", lisp: "code-session", kindFor: "class", description: "One runtime session: execute an actor step, inspect or patch globals, export and restore state, and close.", returns: "a code session", example: `(ax:session-execute session code (ax:object))`},
		{section: "agents-rlm", canonical: "AxAgent.setState", lisp: "agent-set-state", description: "Replace the agent's minimal state, as a state round trip restores it.", returns: "the agent", example: `(ax:agent-set-state agent (ax:object "notes" "none"))`},

		// ---- flow ----
		{section: "flow", canonical: "flow", lisp: "flow", description: "Create a flow program graph.", returns: "a flow", example: `(ax:flow)`},
		{section: "flow", canonical: "AxFlow.node", lisp: "flow-node-extended", description: "Declare a node in the graph, with its program and signature.", returns: "the flow", example: `(ax:flow-node-extended graph "qa" "question:string -> answer:string")`},
		{section: "flow", canonical: "AxFlow.execute", lisp: "flow-execute", description: "Execute a node with state mapped into its inputs.", returns: "the flow", example: `(ax:flow-execute graph "qa" program)`},
		{section: "flow", canonical: "AxFlow.branch", lisp: "flow-branch", description: "Branch the graph on a predicate over the state.", returns: "the flow", example: `(ax:flow-branch graph "route" predicate branches)`},
		{section: "flow", canonical: "AxFlow.parallel", lisp: "flow-parallel", description: "Run independent branches and merge their reports in plan order.", returns: "the flow", example: `(ax:flow-parallel graph "fanout" branches)`},
		{section: "flow", canonical: "AxFlow.returns", lisp: "flow-returns", description: "Map the final state to the flow's outputs.", returns: "the flow", example: `(ax:flow-returns graph mapper)`},
		{section: "flow", canonical: "AxFlow.mermaid", lisp: "flow-mermaid", description: "Render the graph as Mermaid, for documentation and review.", returns: "a Mermaid diagram string", example: `(ax:flow-mermaid graph)`},
		{section: "flow", canonical: "AxFlow.getUsage", lisp: "flow-usage", description: "The flow's merged token usage.", returns: "a JSON array of usage objects", example: `(ax:flow-usage graph)`},

		// ---- tools ----
		{section: "tools", canonical: "fn", lisp: "tool", options: []string{":name", ":description", ":parameters", ":handler"}, description: "Define a tool from a name, a JSON Schema for its arguments and a handler.", returns: "a tool spec", example: `(ax:tool :name "lookup" :description "Look up a key" :handler handler)`},
		{section: "tools", canonical: "AxFunctionJSONSchema", lisp: "tool-request-spec", description: "The wire spec for a tool, as a provider request carries it.", returns: "a JSON object with name, description and parameters", example: `(ax:tool-request-spec spec)`},
		{section: "tools", canonical: "AxFunctionProcessor", lisp: "function-processor", kindFor: "class", description: "The processor generated code resolves tool names through, so a renamed tool stays callable.", returns: "a function processor", example: `(ax:function-processor-resolve processor "lookup")`},
		{section: "tools", canonical: "validateJSONSchema", lisp: "validate-tool-arguments", description: "Validate a tool call's arguments against its schema before the handler runs.", returns: "two values: the validated arguments and a list of problems", example: `(ax:validate-tool-arguments spec arguments)`},
		{section: "tools", canonical: "AxFunctionError", lisp: "function-call-error", kindFor: "condition", description: "A tool call that could not be executed, with the problems that stopped it.", returns: "a condition", example: `(ax:execute-function spec arguments)`},

		// ---- mcp ----
		{section: "mcp", canonical: "AxMCPClient", lisp: "mcp-client", kindFor: "class", description: "An MCP client over a transport: catalogs, tools, prompts, resources, tasks and subscriptions.", returns: "an MCP client", example: `(ax:make-mcp-client transport (ax:object "era" "modern"))`},
		{section: "mcp", canonical: "AxMCPClient.init", lisp: "mcp-init", description: "Initialize the session: negotiate the protocol version and era and load the catalogs.", returns: "the client", example: `(ax:mcp-init client)`},
		{section: "mcp", canonical: "AxMCPClient.callTool", lisp: "mcp-call-tool", description: mcpTaskHandlingDescription("lisp"), returns: "the tool result object", example: `(ax:mcp-call-tool client "lookup" (ax:object "key" "a"))`},
		{section: "mcp", canonical: "AxMCPStdioTransport", lisp: "mcp-stdio-transport", kindFor: "class", description: "The stdio transport, with Ax's line framing.", returns: "a transport", example: `(ax:make-mcp-stdio-transport "server" :arguments (list "--stdio"))`},
		{section: "mcp", canonical: "AxMCPStreambleHTTPTransport", lisp: "mcp-streamable-http-transport", kindFor: "class", description: "The streamable HTTP transport, including session headers and the OAuth boundary.", returns: "a transport", example: `(ax:make-mcp-streamable-http-transport "https://example.com/mcp" (ax:object))`},
		{section: "mcp", canonical: "AxMCPTransport", lisp: "mcp-transport", kindFor: "class", description: "The transport protocol every MCP transport answers: send a request, a notification or a response, set the handlers, and open or close a request stream.", returns: "a transport", example: `(ax:mcp-transport-send transport message)`},
		{section: "mcp", canonical: "AxMCPApp", lisp: "mcp-app-bridge", kindFor: "class", description: "The Apps host bridge: it validates the ui:// resource and runs the host's callbacks for a frame's requests.", returns: "an App bridge", example: `(ax:mcp-app-bridge-load-resource bridge)`},

		// ---- runtime-profiles ----
		{section: "runtime-profiles", canonical: "ProcessCodeRuntime", lisp: "process-runtime", kindFor: "class", description: "The process runtime profile: a host runtime spoken to over the runtime protocol on stdio.", returns: "a code runtime", example: `(ax:make-process-runtime (list "node" "runtime-server.mjs") :language "JavaScript")`},
		{section: "runtime-profiles", canonical: "RuntimeCapabilities", lisp: "runtime-capabilities", description: "What a runtime reports it can do, which is what a caller checks before using callables.", returns: "a capabilities object", example: `(ax:runtime-capabilities :language "JavaScript" :patch nil)`},
		{section: "runtime-profiles", canonical: "RuntimeEnvelope", lisp: "envelope-result", description: "The runtime protocol's envelopes are plain JSON records built by constructor functions, one per envelope kind, rather than an envelope class.", returns: "a result envelope object", example: `(ax:envelope-result (ax:object "value" 1))`},
		{section: "runtime-profiles", canonical: "RuntimeProtocolError", lisp: "runtime-protocol-error", kindFor: "condition", description: "A runtime that broke the protocol, with the category that says how.", returns: "a condition", example: `(ax:runtime-protocol-error-category condition)`},

		// ---- optimizers ----
		{section: "optimizers", canonical: "optimize", lisp: "optimize-program", options: []string{":engine", ":evaluator", ":examples", ":budget"}, description: "Run an optimizer engine over a program and return its optimized-program record.", returns: "an optimized-program record (a JSON object)", example: `(ax:optimize-program program examples :engine engine :client client :evaluator evaluator)`},
		{section: "optimizers", canonical: "AxBootstrapFewShot", lisp: "bootstrap-few-shot", kindFor: "class", description: "The bootstrap few-shot engine, which selects demonstrations from scored examples.", returns: "an optimizer engine", example: `(ax:make-bootstrap-few-shot (ax:object "maxDemos" 4))`},
		{section: "optimizers", canonical: "AxGEPA", lisp: "gepa", kindFor: "class", description: "The GEPA engine, with its Pareto component selector.", returns: "an optimizer engine", example: `(ax:make-gepa :options (ax:object "maxIterations" 8))`},
		{section: "optimizers", canonical: "OptimizerEngine", lisp: "optimizer-engine", kindFor: "class", description: "The engine protocol: a name, a version and one run call Core drives.", returns: "an optimizer engine", example: `(ax:run-optimizer-engine engine request evaluator)`},
		{section: "optimizers", canonical: "AxMetricFn", lisp: "program-evaluator", kindFor: "class", description: "The evaluator a candidate is scored with, together with its metric-call and budget accounting.", returns: "a program evaluator", example: `(ax:make-program-evaluator program client :metric metric :dataset examples)`},
		{section: "optimizers", canonical: "AxOptimizedProgram", lisp: "make-optimized-program", description: "An optimized program is a plain JSON record rather than a type: these functions build it, parse it and apply it to a program.", returns: "an optimized-program record", example: `(ax:apply-optimized-program program record)`},
		{section: "optimizers", canonical: "AxOptimizerCheckpoint", lisp: "optimizer-checkpoint", description: "A checkpoint of an optimizer run, which a later run loads to continue.", returns: "a checkpoint object", example: `(ax:load-optimizer-checkpoint (ax:make-optimizer-state) checkpoint)`},
		{section: "optimizers", canonical: "f1_score", lisp: "f1-score", description: "The F1 evaluation metric over predicted and expected text.", returns: "a number between 0 and 1", example: `(ax:f1-score "a b c" "a b")`},

		// ---- errors-values ----
		{section: "errors-values", canonical: "AxJSONValue", lisp: "object", description: "Build a JSON object. Objects are string-keyed equal hash tables with their key order preserved; arrays are vectors, booleans are ax:true and ax:false, and null is :null.", returns: "a JSON object", example: `(ax:object "question" "why?" "count" 2)`},
		{section: "errors-values", canonical: "AxJSONValue.get", lisp: "jget", description: "Read a key or index, defaulting to :null so a missing key reads as JSON null rather than nil.", returns: "the value at the key, or the default", example: `(ax:jget outputs "answer")`},
		{section: "errors-values", canonical: "parse_json", lisp: "parse-json", description: "Parse one complete JSON document into the shared value model.", returns: "a JSON value", example: `(ax:parse-json "{\"a\":[1,2]}")`},
		{section: "errors-values", canonical: "encode_json", lisp: "encode-json", description: "Render a JSON value as compact JSON text, preserving object key order.", returns: "a JSON string", example: `(ax:encode-json (ax:object "a" 1))`},
		{section: "errors-values", canonical: "AxError", lisp: "ax-error", kindFor: "condition", description: "The base condition every Ax error inherits, carrying the message Ax wrote.", returns: "a condition", example: `(ax:ax-error-message condition)`},
		{section: "errors-values", canonical: "AxAIServiceError", lisp: "provider-error", kindFor: "condition", description: "A typed provider failure: its kind, provider, status and retryability, with messages redacted of credentials.", returns: "a condition", example: `(ax:provider-error-kind condition)`},
		{section: "errors-values", canonical: "AxGenerateError", lisp: "generation-error", kindFor: "condition", description: "A generation failure, with the validation problems when the kind is :validation.", returns: "a condition", example: `(ax:generation-error-problems condition)`},
		{section: "errors-values", canonical: "AxSignatureValidationError", lisp: "signature-error", kindFor: "condition", description: "An invalid signature: bad syntax, an unknown type or modifier, or a colliding field name.", returns: "a condition", example: "(handler-case (ax:parse-signature \"not a signature\")\n  (ax:signature-error (condition) (ax:ax-error-message condition)))"},
		{section: "errors-values", canonical: "AxCancellationToken.cancel", lisp: "cancel", description: "Cancel a token, which ends the waits and calls subscribed to it.", returns: "the token", example: `(ax:cancel token "user stopped")`},

		// ---- canonical names with no Lisp counterpart ----
		{section: "axai", canonical: "OpenAICompatibleClient", absentReason: "one client type serves every provider: (ai :name \"openai-compatible\") selects the profile and provider-profiles lists the profiles Core knows, so there is no per-provider class to document"},
		{section: "axai", canonical: "OpenAIResponsesClient", absentReason: "the openai-responses profile is implemented and appears in provider-profiles; it is reached as (ai :name \"openai-responses\") rather than through a per-provider class"},
		{section: "axai", canonical: "GoogleGeminiClient", absentReason: "the google-gemini profile is reached as (ai :name \"google-gemini\") against one client type, so there is no per-provider class"},
		{section: "axai", canonical: "AnthropicClient", absentReason: "the anthropic profile is reached as (ai :name \"anthropic\") against one client type, so there is no per-provider class"},
		{section: "axai", canonical: "AxCredentialRequest", absentReason: "the credential callback receives a plain JSON object of profile, operation, method and url, so there is no named credential-request type to document"},
		{section: "axai", canonical: "AxCredentialProvider", absentReason: "the boundary exists as provider's :credential-provider callback, documented above, but it is a plain function per client: no credential-provider type, global registration or ai keyword is exported"},
		{section: "axai", canonical: "AxTracer", absentReason: "no tracer type is exported: a host tracer object or callback is passed to start-span-fail-open and start-active-span-fail-open, or stored under the \"tracer\" global, and reached through telemetry-call by camelCase method name"},
		{section: "axai", canonical: "AxMeter", absentReason: "no meter type is exported: a host meter is stored under the \"meter\" global and called through telemetry-call; ax-metrics returns service statistics, not a telemetry meter"},
		{section: "axai", canonical: "AxUsageContext", absentReason: "no named usage-context type is exported: provider options and call options carry plain usageContext objects, merged by Core for chat usage events"},
		{section: "axai", canonical: "AxUsageEvent", absentReason: "no named usage-event type is exported: the provider chat path builds a plain event object with Core and passes it to the onUsage callback when usage is available"},
		{section: "axai", canonical: "AxUsageObserver", absentReason: "no observer type is exported: onUsage is a plain callback in globals or provider/call options; the provider chat path invokes it and ignores callback errors, without implying observer coverage for every operation"},
		{section: "axai", canonical: "set_usage_observer", absentReason: "no dedicated observer setter is exported: use set-global with \"onUsage\" or provider/call options; the provider chat path reads that callback, but this does not establish stream or embedding observer parity"},
	}
}

// BuildLispAPIReferenceManifest builds the Lisp API reference from the real
// package surface. It returns the manifest, the canonical symbols the
// package does not have, and an error when a documented symbol is not an
// export, so a stale entry fails the build instead of shipping.
func BuildLispAPIReferenceManifest(api LispPackageAPI) (APIReferenceManifest, []APIReferenceGap, error) {
	manifest := APIReferenceManifest{
		SchemaVersion: "axir-api-v1",
		// Mirrors BuildCapabilityManifest's AxIRVersion for the generated
		// targets, so the website compares like with like.
		AxIRVersion: "0.1",
		Target:      LispAPITarget,
		PackageName: LispAPIPackageName,
	}
	gaps := []APIReferenceGap{}
	sectionIndex := map[string]int{}
	for _, section := range lispAPISections() {
		sectionIndex[section.id] = len(manifest.Sections)
		manifest.Sections = append(manifest.Sections, APIReferenceSection{
			ID:      section.id,
			Title:   section.title,
			Summary: section.summary,
		})
	}

	for _, entry := range lispAPIEntries() {
		index, ok := sectionIndex[entry.section]
		if !ok {
			return APIReferenceManifest{}, nil, fmt.Errorf("lisp api entry %q names unknown section %q", entry.canonical, entry.section)
		}
		if entry.lisp == "" {
			if strings.TrimSpace(entry.absentReason) == "" {
				return APIReferenceManifest{}, nil, fmt.Errorf("lisp api entry %q has neither a lisp symbol nor a reason", entry.canonical)
			}
			gaps = append(gaps, APIReferenceGap{CanonicalName: entry.canonical, Section: entry.section, Reason: entry.absentReason})
			continue
		}
		export, found := api.Export(entry.lisp)
		if !found {
			return APIReferenceManifest{}, nil, fmt.Errorf("lisp api entry %q documents %q, which packages/lisp does not export", entry.canonical, entry.lisp)
		}
		if !export.Defined() {
			return APIReferenceManifest{}, nil, fmt.Errorf("lisp api entry %q documents %q, which is exported but never defined", entry.canonical, entry.lisp)
		}
		kind := export.Kind
		if entry.kindFor != "" {
			kind = entry.kindFor
		}
		description := entry.description
		if strings.TrimSpace(description) == "" {
			description = export.Doc
		}
		if strings.TrimSpace(description) == "" {
			return APIReferenceManifest{}, nil, fmt.Errorf("lisp api entry %q has no description and %q has no docstring", entry.canonical, entry.lisp)
		}
		form := entry.form
		if form == "" {
			form = lispAPIForm(api, export, kind)
		}
		manifest.Sections[index].Symbols = append(manifest.Sections[index].Symbols, APIReferenceSymbol{
			TargetName:    LispAPITarget,
			CanonicalName: entry.canonical,
			// The website prints public_name verbatim and slugs it, so it is
			// the exported name as the package spells it. The package prefix
			// belongs in the form and the example, which a reader copies.
			PublicName:       export.Name,
			Kind:             kind,
			Description:      description,
			Form:             form,
			ImportantOptions: entry.options,
			Returns:          entry.returns,
			Example:          entry.example,
		})
	}

	sort.Slice(gaps, func(left, right int) bool {
		if gaps[left].Section != gaps[right].Section {
			return gaps[left].Section < gaps[right].Section
		}
		return gaps[left].CanonicalName < gaps[right].CanonicalName
	})
	if err := ValidateLispAPIReferenceManifest(manifest, api); err != nil {
		return APIReferenceManifest{}, nil, err
	}
	return manifest, gaps, nil
}

// lispAPIForm renders the native form a caller writes: the real lambda list
// for a function-like export, the exported constructor's lambda list for a
// type that has one, and otherwise the package-qualified name, because a
// type is named rather than called and the symbol's kind already says so.
func lispAPIForm(api LispPackageAPI, export LispExport, kind string) string {
	name := lispAPINickname + ":" + export.Name
	if export.LambdaList != "" {
		return lispCallForm(name, export.LambdaList)
	}
	switch kind {
	case "class", "struct", "condition":
		if constructor, ok := api.Export("make-" + export.Name); ok && constructor.LambdaList != "" {
			return lispCallForm(lispAPINickname+":"+constructor.Name, constructor.LambdaList)
		}
		return name
	default:
		return name
	}
}

func lispCallForm(name, lambdaList string) string {
	inner := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(lambdaList, "("), ")"))
	if inner == "" {
		return "(" + name + ")"
	}
	return "(" + name + " " + lispFormArguments(inner) + ")"
}

// lispFormArguments strips default forms from a lambda list so the published
// form shows parameter names: (max-steps 5) becomes max-steps.
func lispFormArguments(inner string) string {
	out := []string{}
	depth := 0
	token := strings.Builder{}
	flush := func() {
		text := strings.TrimSpace(token.String())
		token.Reset()
		if text == "" {
			return
		}
		if strings.HasPrefix(text, "(") {
			text = strings.TrimSpace(strings.TrimPrefix(text, "("))
			text = strings.Fields(text + " ")[0]
			text = strings.TrimSuffix(text, ")")
		}
		if text != "" {
			out = append(out, text)
		}
	}
	for _, char := range inner {
		switch char {
		case '(':
			if depth == 0 && token.Len() > 0 {
				flush()
			}
			depth++
			token.WriteRune(char)
		case ')':
			depth--
			token.WriteRune(char)
			if depth == 0 {
				flush()
			}
		case ' ', '\t', '\n':
			if depth == 0 {
				flush()
			} else {
				token.WriteRune(char)
			}
		default:
			token.WriteRune(char)
		}
	}
	flush()
	return strings.Join(out, " ")
}

// ValidateLispAPIReferenceManifest checks the shared manifest rules plus the
// two the Lisp package adds: every documented public name is a real export,
// and every form and example mentions only real exports.
func ValidateLispAPIReferenceManifest(manifest APIReferenceManifest, api LispPackageAPI) error {
	if manifest.SchemaVersion != "axir-api-v1" {
		return fmt.Errorf("api reference schema_version %q is not axir-api-v1", manifest.SchemaVersion)
	}
	if manifest.Target != LispAPITarget || manifest.PackageName != LispAPIPackageName {
		return fmt.Errorf("lisp api reference target/package is %q/%q", manifest.Target, manifest.PackageName)
	}
	required := []string{"signatures", "axgen", "axai", "agents-rlm", "flow", "tools", "mcp", "runtime-profiles", "optimizers", "errors-values"}
	seen := map[string]bool{}
	canonical := map[string]bool{}
	for _, section := range manifest.Sections {
		if strings.TrimSpace(section.ID) == "" || strings.TrimSpace(section.Title) == "" || strings.TrimSpace(section.Summary) == "" {
			return fmt.Errorf("lisp api reference has an incomplete section %q", section.ID)
		}
		if seen[section.ID] {
			return fmt.Errorf("lisp api reference has duplicate section %q", section.ID)
		}
		seen[section.ID] = true
		if len(section.Symbols) == 0 {
			return fmt.Errorf("lisp api reference section %q has no symbols", section.ID)
		}
		for _, symbol := range section.Symbols {
			if symbol.TargetName != LispAPITarget {
				return fmt.Errorf("lisp api reference symbol %q has target_name %q", symbol.CanonicalName, symbol.TargetName)
			}
			if strings.TrimSpace(symbol.CanonicalName) == "" || strings.TrimSpace(symbol.PublicName) == "" ||
				strings.TrimSpace(symbol.Kind) == "" || strings.TrimSpace(symbol.Description) == "" ||
				strings.TrimSpace(symbol.Form) == "" || strings.TrimSpace(symbol.Returns) == "" {
				return fmt.Errorf("lisp api reference section %q has incomplete symbol %q", section.ID, symbol.CanonicalName)
			}
			if canonical[symbol.CanonicalName] {
				return fmt.Errorf("lisp api reference documents %q twice", symbol.CanonicalName)
			}
			canonical[symbol.CanonicalName] = true
			if export, ok := api.Export(symbol.PublicName); !ok || !export.Defined() {
				return fmt.Errorf("lisp api reference symbol %q names %q, which is not a defined export", symbol.CanonicalName, symbol.PublicName)
			}
			for _, text := range []string{symbol.Form, symbol.Example} {
				for _, referenced := range LispAPIReferencedExports(text) {
					if export, ok := api.Export(referenced); !ok || !export.Defined() {
						return fmt.Errorf("lisp api reference symbol %q references %q, which is not a defined export", symbol.CanonicalName, referenced)
					}
				}
			}
		}
	}
	for _, id := range required {
		if !seen[id] {
			return fmt.Errorf("lisp api reference missing section %q", id)
		}
	}
	return nil
}

var lispQualifiedRe = regexp.MustCompile(`\b(?:ax|axllm):([a-z0-9*+<>=/-]+)`)

// lispAnchorSafeRe is what a published name must look like for its Markdown
// heading to slug to exactly that name: the website links each API row to
// /lisp/api/reference/#<public_name>, so a name carrying *, ? or ! would
// anchor differently and leave a dead fragment.
var lispAnchorSafeRe = regexp.MustCompile(`^[a-z0-9][a-z0-9-]*$`)

// LispAPIAnchorUnsafeNames returns the documented public names whose
// Markdown heading would not slug to the name itself.
func LispAPIAnchorUnsafeNames(manifest APIReferenceManifest) []string {
	out := []string{}
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			if !lispAnchorSafeRe.MatchString(symbol.PublicName) {
				out = append(out, symbol.PublicName)
			}
		}
	}
	return out
}

// LispAPIReferencedExports returns the package-qualified names a form or
// example mentions, so each one can be checked against the real exports.
func LispAPIReferencedExports(text string) []string {
	out := []string{}
	seen := map[string]bool{}
	for _, match := range lispQualifiedRe.FindAllStringSubmatch(text, -1) {
		name := strings.TrimSuffix(strings.TrimSuffix(match[1], ")"), "'")
		if name == "" || seen[name] {
			continue
		}
		seen[name] = true
		out = append(out, name)
	}
	return out
}

// LispAPIReferenceJSON renders axir-api.json. The gaps travel with the
// manifest under a Lisp-specific key so the website can show "not in this
// package" instead of a missing section.
func LispAPIReferenceJSON(manifest APIReferenceManifest, gaps []APIReferenceGap) (string, error) {
	return lispAPIReferenceJSON(manifest, gaps, nil)
}

// LispAPIReferenceJSONFor renders axir-api.json and records the components
// the inventory could not read, so a manifest built from an incomplete
// checkout says so instead of looking like the whole package.
func LispAPIReferenceJSONFor(manifest APIReferenceManifest, gaps []APIReferenceGap, api LispPackageAPI) (string, error) {
	return lispAPIReferenceJSON(manifest, gaps, api.MissingComponents)
}

func lispAPIReferenceJSON(manifest APIReferenceManifest, gaps []APIReferenceGap, missingComponents []string) (string, error) {
	payload := struct {
		APIReferenceManifest
		MissingCanonicalSymbols []APIReferenceGap `json:"missing_canonical_symbols,omitempty"`
		UnreadSourceComponents  []string          `json:"unread_source_components,omitempty"`
	}{APIReferenceManifest: manifest, MissingCanonicalSymbols: gaps, UnreadSourceComponents: missingComponents}
	encoded, err := json.MarshalIndent(payload, "", "  ")
	if err != nil {
		return "", err
	}
	return string(encoded) + "\n", nil
}

// EmitLispAPIReferenceFiles builds the API reference files for the Common
// Lisp package at packageDir (the directory holding src/), keyed by the name
// each one is written under. It is the whole wiring an emitter needs:
//
//	files, err := EmitLispAPIReferenceFiles(outDir)
//	if err != nil {
//		return err
//	}
//	for name, body := range files {
//		if err := os.WriteFile(filepath.Join(outDir, name), []byte(body), 0o644); err != nil {
//			return err
//		}
//	}
//
// An entry that documents something the package no longer exports is an
// error rather than a stale file, so a rename fails the build.
func EmitLispAPIReferenceFiles(packageDir string) (map[string]string, error) {
	api, err := LoadLispPackageAPI(packageDir)
	if err != nil {
		return nil, err
	}
	manifest, gaps, err := BuildLispAPIReferenceManifest(api)
	if err != nil {
		return nil, err
	}
	encoded, err := LispAPIReferenceJSONFor(manifest, gaps, api)
	if err != nil {
		return nil, err
	}
	return map[string]string{
		"axir-api.json": encoded,
		"API.md":        LispAPIReferenceMarkdown(manifest, gaps, api),
	}, nil
}

// LispAPIReferenceMarkdown renders API.md: native forms, Lisp-fenced
// examples, and an explicit list of the canonical symbols this package does
// not have.
func LispAPIReferenceMarkdown(manifest APIReferenceManifest, gaps []APIReferenceGap, api LispPackageAPI) string {
	out := strings.Builder{}
	out.WriteString("# " + manifest.PackageName + " API reference\n\n")
	out.WriteString("Common Lisp package for Ax, loaded as the ASDF system `" + manifest.PackageName + "`.\n")
	out.WriteString("Every form below is evaluated in a package that uses `" + lispAPIPackage + "`; the examples qualify each name with the `" + lispAPINickname + "` nickname.\n\n")
	out.WriteString("JSON values are shared by every surface: objects are string-keyed `equal` hash tables with their key order preserved, arrays are vectors, booleans are `ax:true` and `ax:false`, and null is `:null`. `nil` is not a JSON value.\n\n")
	out.WriteString("```lisp\n(require :asdf)\n(asdf:load-system \"" + manifest.PackageName + "\")\n```\n\n")
	for _, section := range manifest.Sections {
		out.WriteString("## " + section.Title + "\n\n")
		out.WriteString(section.Summary + "\n\n")
		for _, symbol := range section.Symbols {
			// The heading is the unqualified export name: the website links
			// to /lisp/api/reference/#<slugify(public_name)>, so a qualified
			// heading would anchor as ax-parse-signature and break every
			// API row. The qualified name a caller types is the next line.
			out.WriteString("### `" + symbol.PublicName + "`\n\n")
			out.WriteString("- Qualified: `" + lispAPINickname + ":" + symbol.PublicName + "`\n")
			out.WriteString("- Kind: " + symbol.Kind + "\n")
			out.WriteString("- Canonical Ax symbol: `" + symbol.CanonicalName + "`\n")
			out.WriteString("- Form: `" + symbol.Form + "`\n")
			if len(symbol.ImportantOptions) > 0 {
				quoted := make([]string, 0, len(symbol.ImportantOptions))
				for _, option := range symbol.ImportantOptions {
					quoted = append(quoted, "`"+option+"`")
				}
				out.WriteString("- Options: " + strings.Join(quoted, ", ") + "\n")
			}
			out.WriteString("- Returns: " + symbol.Returns + "\n")
			if export, ok := api.Export(symbol.PublicName); ok && export.File != "" {
				out.WriteString("- Defined in: `src/" + export.File + "`\n")
			}
			out.WriteString("\n" + symbol.Description + "\n")
			if strings.TrimSpace(symbol.Example) != "" {
				out.WriteString("\n```lisp\n" + symbol.Example + "\n```\n")
			}
			out.WriteString("\n")
		}
	}
	if len(gaps) > 0 {
		out.WriteString("## Not in this package\n\n")
		out.WriteString("These canonical Ax symbols have no Common Lisp counterpart. They are listed rather than documented, so no form here names something that cannot be evaluated.\n\n")
		for _, gap := range gaps {
			out.WriteString("- `" + gap.CanonicalName + "` (" + gap.Section + "): " + gap.Reason + "\n")
		}
		out.WriteString("\n")
	}
	if !api.Complete() {
		out.WriteString("## Incomplete source inventory\n\n")
		out.WriteString("`axllm.asd` declares these components and this reference was built without them, so it may omit public symbols. Regenerate from a checkout that has every component.\n\n")
		for _, component := range api.MissingComponents {
			out.WriteString("- `src/" + component + ".lisp`\n")
		}
		out.WriteString("\n")
	}
	if undefined := api.UndefinedExports(); len(undefined) > 0 {
		out.WriteString("## Exported but undefined\n\n")
		out.WriteString("The sources read here export these names without defining them. With the inventory above incomplete this means a file was not read; with a complete inventory it is a public API that fails at the call site.\n\n")
		for _, name := range undefined {
			out.WriteString("- `" + lispAPINickname + ":" + name + "`\n")
		}
		out.WriteString("\n")
	}
	return out.String()
}

// ---------------------------------------------------------------------------
// Static check of the published forms and examples
// ---------------------------------------------------------------------------
//
// Checking that a name is exported is not enough: an example can name a real
// function and still call it wrongly, which is how a published example came
// to pass a positional list to a &key constructor. So every example is
// compiled by SBCL against the loaded package, where a wrong argument count
// or an unknown keyword is a compiler warning rather than a runtime surprise.
//
// Nothing is executed. The examples are compiled inside functions that are
// never called, and every free variable is bound to NIL, so no provider,
// transport or runtime is reached.

// lispExamplePlaceholders are the free variables an example may use. A new
// placeholder has to be declared here, so an example cannot quietly depend
// on an unbound name.
var lispExamplePlaceholders = []string{
	"agent", "arguments", "bridge", "branches", "checkpoint", "child", "client",
	"code", "condition", "engine", "evaluator", "examples", "graph", "handler",
	"inputs", "limiter", "mapper", "message", "messages", "metric", "options",
	"outputs", "parent", "predicate", "processor", "program", "record", "request",
	"meter", "reward", "runtime", "session", "sink", "spec", "teacher", "token",
	"tracer", "transport",
}

// lispExampleOperators are the Common Lisp operators an example may use.
var lispExampleOperators = map[string]bool{
	"handler-case": true, "let": true, "let*": true, "lambda": true, "list": true,
	"make-instance": true, "quote": true, "setf": true, "t": true, "nil": true,
	"vector":  true,
	"declare": true, "ignorable": true, "ignore": true, "progn": true,
}

// A name worth resolving: not inside a string (removed first), not a
// keyword (:side), and not package-qualified (ax:forward).
var lispExampleTokenRe = regexp.MustCompile(`(?:^|[\s('])([A-Za-z][A-Za-z0-9*+<>=/?!-]*)(:?)`)

// LispAPIExampleCheckSource renders a Lisp file that compiles every example
// in the manifest, and reports an example that uses an undeclared free
// variable. Compiling the result against the loaded package is what proves
// each example's arity and keywords.
func LispAPIExampleCheckSource(manifest APIReferenceManifest) (string, error) {
	placeholders := map[string]bool{}
	for _, name := range lispExamplePlaceholders {
		placeholders[name] = true
	}
	out := strings.Builder{}
	out.WriteString(";;;; Generated by LispAPIExampleCheckSource. Compiled, never run.\n")
	out.WriteString("(in-package #:cl-user)\n\n")
	index := 0
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			example := strings.TrimSpace(symbol.Example)
			if example == "" {
				continue
			}
			for _, match := range lispExampleTokenRe.FindAllStringSubmatch(lispStripStrings(example), -1) {
				token := match[1]
				if match[2] == ":" {
					// Package-qualified: checked against the real exports
					// elsewhere, and resolved by the compiler here.
					continue
				}
				lowered := strings.ToLower(token)
				if lispExampleOperators[lowered] || placeholders[lowered] {
					continue
				}
				return "", fmt.Errorf("example for %q uses undeclared free name %q; add it to lispExamplePlaceholders or qualify it", symbol.CanonicalName, token)
			}
			index++
			out.WriteString(fmt.Sprintf(";;; %s -> %s\n", symbol.CanonicalName, symbol.PublicName))
			out.WriteString(fmt.Sprintf("(defun ax-api-example-%d ()\n  (let (%s)\n    (declare (ignorable %s))\n    %s))\n\n",
				index,
				strings.Join(lispExampleBindings(), " "),
				strings.Join(lispExamplePlaceholders, " "),
				example))
		}
	}
	if index == 0 {
		return "", fmt.Errorf("no examples to check")
	}
	return out.String(), nil
}

// lispStripStrings removes string literals, whose contents are data rather
// than names to resolve.
func lispStripStrings(text string) string {
	out := strings.Builder{}
	inString := false
	for index := 0; index < len(text); index++ {
		switch text[index] {
		case '\\':
			if inString {
				index++
				continue
			}
			out.WriteByte(text[index])
		case '"':
			inString = !inString
			out.WriteByte(' ')
		default:
			if !inString {
				out.WriteByte(text[index])
			}
		}
	}
	return out.String()
}

func lispExampleBindings() []string {
	out := make([]string, 0, len(lispExamplePlaceholders))
	for _, name := range lispExamplePlaceholders {
		out = append(out, "("+name+" nil)")
	}
	return out
}

// LispAPIFormMismatches returns the published forms whose argument names do
// not match the lambda list the package defines, which is how a hand-written
// form drifts from the function it documents.
func LispAPIFormMismatches(manifest APIReferenceManifest, api LispPackageAPI) []string {
	out := []string{}
	for _, section := range manifest.Sections {
		for _, symbol := range section.Symbols {
			export, ok := api.Export(symbol.PublicName)
			if !ok {
				continue
			}
			source := export
			if export.LambdaList == "" {
				if constructor, found := api.Export("make-" + export.Name); found && constructor.LambdaList != "" {
					source = constructor
				}
			}
			if source.LambdaList == "" {
				if strings.HasPrefix(symbol.Form, "(") {
					out = append(out, fmt.Sprintf("%s: form %q is a call but %s has no lambda list", symbol.CanonicalName, symbol.Form, source.Name))
				}
				continue
			}
			want := lispCallForm(lispAPINickname+":"+source.Name, source.LambdaList)
			if symbol.Form != want {
				out = append(out, fmt.Sprintf("%s: form %q does not match the lambda list %q", symbol.CanonicalName, symbol.Form, want))
			}
		}
	}
	return out
}
