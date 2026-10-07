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

const (
	provenanceBeginFunctions    = "BEGIN AXIR CORE EMITTED FUNCTIONS"
	provenanceEndFunctions      = "END AXIR CORE EMITTED FUNCTIONS"
	provenanceBeginDeclarations = "BEGIN AXIR CORE EMITTED DECLARATIONS"
	provenanceEndDeclarations   = "END AXIR CORE EMITTED DECLARATIONS"
)

// provenanceEnforced lists the targets whose generated packages must prove
// that every Core-owned function is emitted from the IR. Rust stays
// report-only until its real core emitter lands.
var provenanceEnforced = map[string]bool{
	"python": true,
	"java":   true,
	"cpp":    true,
	"go":     true,
	"rust":   true,
	"lisp":   true,
}

func provenanceEnforcedFor(target string) bool {
	if os.Getenv("AXIR_PROVENANCE") == "report" {
		return false
	}
	return provenanceEnforced[target]
}

type ProvenanceFileMetrics struct {
	EmittedLines int `json:"emitted_lines"`
	TotalLines   int `json:"total_lines"`
}

type ProvenanceReport struct {
	Target           string                           `json:"target"`
	Enforced         bool                             `json:"enforced"`
	EmittedFunctions int                              `json:"emitted_functions"`
	Files            map[string]ProvenanceFileMetrics `json:"files"`
	Violations       []string                         `json:"violations,omitempty"`
}

type provenanceExpectation struct {
	file    string
	name    string
	inside  string         // exact definition line expected inside the emitted region
	outside *regexp.Regexp // shadow-definition pattern checked outside the region; nil disables
}

// specArgList renders the argument list the emitters generate for a spec so
// audit patterns can match full signatures; java and cpp templates carry
// convenience overloads (same name, different arity) that must not be
// misread as shadows.
func specArgList(model AxRuntimeModel, spec CoreFuncSpec, typeName string, nameFn func(string) string) (string, error) {
	body, err := BuildCoreBody(model.Symbols[spec.Symbol])
	if err != nil {
		return "", fmt.Errorf("@%s: %w", spec.Symbol, err)
	}
	if len(body.Blocks) == 0 {
		return "", fmt.Errorf("@%s has no Core body blocks", spec.Symbol)
	}
	var args []string
	for _, arg := range body.Blocks[0].Args {
		args = append(args, typeName+" "+nameFn("%"+arg.Name))
	}
	return strings.Join(args, ", "), nil
}

func cppSpecArgs(model AxRuntimeModel, spec CoreFuncSpec) (string, error) {
	return specArgList(model, spec, "Value", cppName)
}

func provenanceExpectations(model AxRuntimeModel, target string, specs []CoreFuncSpec) ([]provenanceExpectation, error) {
	var out []provenanceExpectation
	for _, spec := range specs {
		switch target {
		case "lisp":
			// Common Lisp has one function namespace per package, so a
			// shadow is a definition of the same name in the generated
			// code's own package. defmacro and defgeneric shadow a
			// function just as defun does, and the Lisp reader upcases
			// symbol names, so the match is case-insensitive. The text
			// this runs against has already had every form outside the
			// axllm/core package masked out; see lispCorePackageText.
			name := LispCoreFuncName(spec.Name)
			out = append(out, provenanceExpectation{
				file:   lispProvenanceCoreFile,
				name:   name,
				inside: "(defun " + name + " ",
				outside: regexp.MustCompile(`(?mi)^\((?:defun|defmacro|defgeneric)[ \t]+(?:axllm/core::?)?` +
					regexp.QuoteMeta(name) + `[ \t(]`),
			})
		case "python":
			out = append(out, provenanceExpectation{
				file:    "axllm/" + pythonCoreModuleFile(spec.Module) + ".py",
				name:    spec.Name,
				inside:  "def " + spec.Name + "(",
				outside: regexp.MustCompile(`(?m)^def ` + regexp.QuoteMeta(spec.Name) + `\(`),
			})
		case "go":
			out = append(out, provenanceExpectation{
				file:    "axllm.go",
				name:    spec.Name,
				inside:  "func " + spec.Name + "(",
				outside: regexp.MustCompile(`(?m)^func ` + regexp.QuoteMeta(spec.Name) + `\(`),
			})
		case "rust":
			out = append(out, provenanceExpectation{
				file:    "src/lib.rs",
				name:    spec.Name,
				inside:  "fn " + spec.Name + "(",
				outside: regexp.MustCompile(`(?m)^fn ` + regexp.QuoteMeta(spec.Name) + `\(`),
			})
		case "java":
			args, err := specArgList(model, spec, "Object", javaName)
			if err != nil {
				return nil, err
			}
			definition := fmt.Sprintf("static Object %s(%s) {", spec.Name, args)
			out = append(out, provenanceExpectation{
				file:    "dev/axllm/ax/Core.java",
				name:    spec.Name,
				inside:  "  " + definition,
				outside: regexp.MustCompile(`(?m)^[ \t]*` + regexp.QuoteMeta(definition)),
			})
		case "cpp":
			args, err := cppSpecArgs(model, spec)
			if err != nil {
				return nil, err
			}
			definition := fmt.Sprintf("Value Core::%s(%s) {", spec.Name, args)
			out = append(out, provenanceExpectation{
				file:    "axllm/axllm.cpp",
				name:    spec.Name,
				inside:  definition,
				outside: regexp.MustCompile(`(?m)^` + regexp.QuoteMeta(definition)),
			})
		default:
			return nil, fmt.Errorf("no provenance expectations for target %q", target)
		}
	}
	return out, nil
}

type provenanceRegion struct {
	inside  string
	outside string
}

func splitProvenanceRegion(content, begin, end string) (provenanceRegion, error) {
	if strings.Count(content, begin) != 1 || strings.Count(content, end) != 1 {
		return provenanceRegion{}, fmt.Errorf("want exactly one %q/%q region, found %d/%d",
			begin, end, strings.Count(content, begin), strings.Count(content, end))
	}
	start := strings.Index(content, begin)
	stop := strings.Index(content, end)
	if stop < start {
		return provenanceRegion{}, fmt.Errorf("%q region ends before it begins", begin)
	}
	return provenanceRegion{
		inside:  content[start:stop],
		outside: content[:start] + content[stop:],
	}, nil
}

// lispProvenanceCoreFile is where the Lisp target's emitted Core lives,
// relative to the package root.
const lispProvenanceCoreFile = "src/core.lisp"

var lispInPackageRe = regexp.MustCompile(`(?mi)^\(in-package[ \t]+[#']?:?"?([A-Za-z0-9/_.+-]+)"?`)

// lispCorePackageText returns one Lisp source file as the shadow audit must
// see it: only the text that can define a symbol in the generated code's
// package.
//
// This is the difference between a useful audit and a broken one. The Lisp
// package has a public axllm facade (signature.lisp, ai.lisp, gen.lisp,
// tools.lisp, json.lisp) whose methods are deliberately named after the Ax
// concepts they expose, and the generated Core lives in the separate
// internal axllm/core package. A facade (defun to-json-schema ...) in
// package axllm defines AXLLM:TO-JSON-SCHEMA, which is a different symbol
// from AXLLM/CORE:TO-JSON-SCHEMA and cannot shadow it. Matching on the name
// alone would report every such facade method as a hand-written shadow of a
// Core-owned function and make the audit unusable as the facade grows.
//
// So each top-level form is kept only when it could actually define a
// symbol in pkg: either the reader is inside pkg at that point, or the line
// names pkg explicitly with a package qualifier. Lines are blanked rather
// than removed so reported line numbers stay true.
func lispCorePackageText(text, pkg string) string {
	lines := strings.Split(text, "\n")
	// Before any in-package form the file is in whatever package the loader
	// left current, which is not pkg for any file in this package: every
	// Lisp source here opens with its own in-package. Treat it as foreign.
	current := ""
	qualifier := pkg + ":"
	for i, line := range lines {
		if match := lispInPackageRe.FindStringSubmatch(line); match != nil {
			current = strings.ToLower(match[1])
			continue
		}
		if strings.EqualFold(current, pkg) {
			continue
		}
		// A package-qualified definition reaches into pkg from anywhere, so
		// it stays visible to the audit even in a foreign package.
		if strings.Contains(strings.ToLower(line), qualifier) {
			continue
		}
		lines[i] = ""
	}
	return strings.Join(lines, "\n")
}

// provenanceAuditText adapts one source file for the shadow audit. Most
// targets have a single namespace per file and need no adaptation; Lisp
// needs its foreign-package forms masked out.
func provenanceAuditText(target, text string) string {
	if target != "lisp" {
		return text
	}
	return lispCorePackageText(text, LispCorePackage)
}

func countDefinitionLines(text, definition string) int {
	count := strings.Count(text, "\n"+definition)
	if strings.HasPrefix(text, definition) {
		count++
	}
	return count
}

// AuditProvenance verifies that, for the given target, every Core-owned
// function in the registry is defined exactly once inside the emitted-region
// markers of its expected generated file and nowhere else in the package.
// files maps package-relative paths to contents.
func AuditProvenance(model AxRuntimeModel, target string, files map[string]string) (ProvenanceReport, error) {
	report := ProvenanceReport{
		Target:   target,
		Enforced: provenanceEnforcedFor(target),
		Files:    map[string]ProvenanceFileMetrics{},
	}
	specs, err := BuildCoreFuncRegistry(model)
	if err != nil {
		return report, err
	}
	expectations, err := provenanceExpectations(model, target, specs)
	if err != nil {
		return report, err
	}

	expectedFiles := map[string]bool{}
	for _, exp := range expectations {
		expectedFiles[exp.file] = true
	}
	regions := map[string]provenanceRegion{}
	for file := range expectedFiles {
		content, ok := files[file]
		if !ok {
			report.Violations = append(report.Violations, fmt.Sprintf("%s: expected generated file is missing", file))
			continue
		}
		region, err := splitProvenanceRegion(content, provenanceBeginFunctions, provenanceEndFunctions)
		if err != nil {
			report.Violations = append(report.Violations, fmt.Sprintf("%s: %v", file, err))
			continue
		}
		regions[file] = region
		report.Files[file] = ProvenanceFileMetrics{
			EmittedLines: strings.Count(region.inside, "\n"),
			TotalLines:   strings.Count(content, "\n") + 1,
		}
	}

	var sourceFiles []string
	for name := range files {
		switch filepath.Ext(name) {
		case ".py", ".go", ".java", ".cpp", ".hpp", ".rs", ".lisp":
			sourceFiles = append(sourceFiles, name)
		}
	}
	sort.Strings(sourceFiles)

	for _, exp := range expectations {
		region, ok := regions[exp.file]
		if !ok {
			continue // file-level violation already recorded
		}
		if got := countDefinitionLines(region.inside, exp.inside); got != 1 {
			report.Violations = append(report.Violations,
				fmt.Sprintf("%s: %s defined %d times inside the emitted region, want exactly once", exp.file, exp.name, got))
			continue
		}
		report.EmittedFunctions++
		if exp.outside == nil {
			continue
		}
		for _, name := range sourceFiles {
			text := files[name]
			if name == exp.file {
				text = region.outside
			}
			text = provenanceAuditText(target, text)
			if loc := exp.outside.FindStringIndex(text); loc != nil {
				report.Violations = append(report.Violations,
					fmt.Sprintf("%s: %s is also defined outside the emitted region in %s (hand-written shadow of a Core-owned function)", exp.file, exp.name, name))
			}
		}
	}

	if target == "lisp" {
		auditLispProvenanceExtras(model, specs, files, &report)
	}

	if target == "cpp" {
		if content, ok := files["axllm/axllm.hpp"]; !ok {
			report.Violations = append(report.Violations, "axllm/axllm.hpp: expected generated header is missing")
		} else if region, err := splitProvenanceRegion(content, provenanceBeginDeclarations, provenanceEndDeclarations); err != nil {
			report.Violations = append(report.Violations, fmt.Sprintf("axllm/axllm.hpp: %v", err))
		} else {
			for _, spec := range specs {
				args, err := cppSpecArgs(model, spec)
				if err != nil {
					return report, err
				}
				decl := fmt.Sprintf("static Value %s(%s);", spec.Name, args)
				if got := countDefinitionLines(strings.ReplaceAll(region.inside, "  static Value", "static Value"), decl); got != 1 {
					report.Violations = append(report.Violations,
						fmt.Sprintf("axllm/axllm.hpp: declaration for %s found %d times inside the declarations region, want exactly once", spec.Name, got))
				}
			}
		}
	}

	sort.Strings(report.Violations)
	return report, nil
}

// auditLispProvenanceExtras adds the two guards that are specific to the
// Lisp target: its forward declarations must name every emitted function
// and every native boundary, and its coverage instrumentation must be all
// present or all absent.
//
// The declarations guard matters because those declarations are what let
// core.lisp compile before the native boundaries exist. A missing one does
// not break the build, it just makes SBCL note an undefined function, which
// is easy to ignore and hides a real missing dependency behind noise.
func auditLispProvenanceExtras(model AxRuntimeModel, specs []CoreFuncSpec, files map[string]string, report *ProvenanceReport) {
	content, ok := files[lispProvenanceCoreFile]
	if !ok {
		return // file-level violation already recorded
	}
	region, err := splitProvenanceRegion(content, provenanceBeginDeclarations, provenanceEndDeclarations)
	if err != nil {
		report.Violations = append(report.Violations, fmt.Sprintf("%s: %v", lispProvenanceCoreFile, err))
		return
	}
	declared := map[string]bool{}
	for _, line := range strings.Split(region.inside, "\n") {
		trimmed := strings.TrimSpace(strings.TrimRight(line, ")"))
		if trimmed == "" || strings.HasPrefix(trimmed, ";") || strings.HasPrefix(trimmed, "(") {
			continue
		}
		declared[trimmed] = true
	}
	for _, spec := range specs {
		name := LispCoreFuncName(spec.Name)
		if !declared[name] {
			report.Violations = append(report.Violations,
				fmt.Sprintf("%s: %s has no forward declaration; add it to the emitted declarations region", lispProvenanceCoreFile, name))
		}
	}
	manifestJSON, err := BuildLispCoreBoundaryManifest(model)
	if err != nil {
		report.Violations = append(report.Violations, fmt.Sprintf("%s: boundary manifest: %v", lispProvenanceCoreFile, err))
		return
	}
	var manifest LispBoundaryManifest
	if err := json.Unmarshal([]byte(manifestJSON), &manifest); err != nil {
		report.Violations = append(report.Violations, fmt.Sprintf("%s: boundary manifest: %v", lispProvenanceCoreFile, err))
		return
	}
	for _, boundary := range manifest.Boundaries {
		if !declared[boundary.Name] {
			report.Violations = append(report.Violations,
				fmt.Sprintf("%s: native boundary %s is called but has no forward declaration", lispProvenanceCoreFile, boundary.Name))
		}
	}
	// Coverage instrumentation is all or nothing. A partially instrumented
	// file would report coverage for the functions that happen to carry a
	// mark and silently omit the rest, which is worse than no coverage.
	functions, err := splitProvenanceRegion(content, provenanceBeginFunctions, provenanceEndFunctions)
	if err != nil {
		return // already reported above
	}
	marks := strings.Count(functions.inside, "("+lispCoverageMarkBoundary+" ")
	switch {
	case lispEmitCoverageMarks && marks != len(specs):
		report.Violations = append(report.Violations,
			fmt.Sprintf("%s: %d coverage marks for %d emitted functions; every Core function must record its own coverage",
				lispProvenanceCoreFile, marks, len(specs)))
	case !lispEmitCoverageMarks && marks != 0:
		report.Violations = append(report.Violations,
			fmt.Sprintf("%s: %d coverage marks are emitted while coverage instrumentation is off; turn lispEmitCoverageMarks on or remove them",
				lispProvenanceCoreFile, marks))
	}
}

// AuditProvenanceDir audits a written package directory.
func AuditProvenanceDir(model AxRuntimeModel, target, dir string) (ProvenanceReport, error) {
	files := map[string]string{}
	err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() {
			return err
		}
		switch filepath.Ext(path) {
		case ".py", ".go", ".java", ".cpp", ".hpp", ".rs", ".lisp":
			rel, err := filepath.Rel(dir, path)
			if err != nil {
				return err
			}
			content, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			files[filepath.ToSlash(rel)] = string(content)
		}
		return nil
	})
	if err != nil {
		return ProvenanceReport{Target: target}, err
	}
	return AuditProvenance(model, target, files)
}

// WriteProvenanceManifest records the audit metrics next to the package's
// capability manifest.
func WriteProvenanceManifest(dir string, report ProvenanceReport) error {
	payload, err := json.MarshalIndent(report, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(filepath.Join(dir, "axir-provenance.json"), append(payload, '\n'), 0o644)
}
