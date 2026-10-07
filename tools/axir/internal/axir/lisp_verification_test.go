package axir

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Never depend on the repository's current declaration being absent.
func lispUseUndeclaredNativeFixture(t *testing.T) string {
	t.Helper()
	native, err := LispNativeSourceDir()
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	if err := copyLispNativeSources(native, dir); err != nil {
		t.Fatal(err)
	}
	if err := os.Remove(filepath.Join(dir, LispConformanceDeclarationFile)); err != nil && !os.IsNotExist(err) {
		t.Fatal(err)
	}
	t.Setenv(LispNativeSourceEnv, dir)
	return dir
}

func TestLispDeclarationCompleteness(t *testing.T) {
	model := lispTestModel(t)
	for _, level := range []string{"absent", "partial", "full"} {
		t.Run(level, func(t *testing.T) {
			dir := t.TempDir()
			if err := writeFiles(dir, map[string]string{
				"axllm.asd": "", "tests/run.lisp": "; test declaration only\n",
				"examples/no-key.lisp": "(assert (= 2 (+ 1 1)))\n",
			}); err != nil {
				t.Fatal(err)
			}
			t.Setenv(LispNativeSourceEnv, dir)
			declaration := LispConformanceDeclaration{
				Runner: "tests/run.lisp", Command: []string{"sbcl", "--script", "tests/run.lisp"},
				Suites: []string{"signature"},
			}
			if level == "full" {
				declaration.Suites = lispConformanceSuites()
				declaration.NativeBoundaries = true
				declaration.ScriptedTransport = true
				declaration.RuntimeProfiles = []string{"javascript-process"}
				declaration.NoKeyExamples = []string{"examples/no-key.lisp"}
			}
			if level != "absent" {
				data, err := json.Marshal(declaration)
				if err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile), data, 0o644); err != nil {
					t.Fatal(err)
				}
			}
			manifest, err := BuildLispCapabilityManifest(model)
			if err != nil {
				t.Fatal(err)
			}
			coverage, err := BuildLispConformanceCoverage(model)
			if err != nil {
				t.Fatal(err)
			}
			if err := ValidateConformanceCoverage(manifest, coverage); err != nil {
				t.Fatal(err)
			}
			wantSuites := map[string]int{"absent": 0, "partial": 1, "full": 12}[level]
			if len(manifest.SupportedSuites) != wantSuites {
				t.Fatalf("suites = %v", manifest.SupportedSuites)
			}
			if (len(manifest.UnsupportedCapabilities) == 0) != (level == "full") {
				t.Fatalf("unsupported = %v", manifest.UnsupportedCapabilities)
			}
			if err := requireLispFullDeclaration(dir); (err == nil) != (level == "full") {
				t.Fatalf("default verification gate for %s: %v", level, err)
			}
			if manifest.RealNetworkSupport {
				t.Fatal("scripted transport must not manufacture a real-network claim")
			}
			boundaries, err := BuildLispCoreBoundaryManifest(model)
			if err != nil {
				t.Fatal(err)
			}
			caps, _ := json.Marshal(manifest)
			cover, _ := json.Marshal(coverage)
			if err := writeFiles(dir, map[string]string{
				"axir-capabilities.json": string(caps), "conformance-coverage.json": string(cover),
				lispBoundaryManifestFile: boundaries,
			}); err != nil {
				t.Fatal(err)
			}
			if err := VerifyLispManifest(dir); err != nil {
				t.Fatalf("consistent %s manifests: %v", level, err)
			}
			if level == "full" {
				declaration.NativeBoundaries = false
				data, _ := json.Marshal(declaration)
				if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile), data, 0o644); err != nil {
					t.Fatal(err)
				}
				if err := VerifyLispManifest(dir); err == nil {
					t.Fatal("a manifest retaining a removed declaration claim must fail")
				}
			}
		})
	}
}

func TestLispDeclarationRejectsUnverifiableClaims(t *testing.T) {
	dir := t.TempDir()
	if err := writeFiles(dir, map[string]string{"tests/run.lisp": "", "examples/ok.lisp": ""}); err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name   string
		mutate func(*LispConformanceDeclaration)
	}{
		{"duplicate suite", func(d *LispConformanceDeclaration) { d.Suites = []string{"axagent", "axagent"} }},
		{"unknown runtime", func(d *LispConformanceDeclaration) { d.RuntimeProfiles = []string{"unverified-engine"} }},
		{"runtime without agent", func(d *LispConformanceDeclaration) {
			d.Suites = []string{"signature"}
			d.RuntimeProfiles = []string{"javascript-process"}
		}},
		{"missing example", func(d *LispConformanceDeclaration) { d.NoKeyExamples = []string{"examples/missing.lisp"} }},
		{"escaping example", func(d *LispConformanceDeclaration) { d.NoKeyExamples = []string{"../elsewhere.lisp"} }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			d := LispConformanceDeclaration{Runner: "tests/run.lisp", Command: []string{"sbcl", "--script", "tests/run.lisp"}, Suites: []string{"axagent"}}
			tc.mutate(&d)
			data, _ := json.Marshal(d)
			if err := os.WriteFile(filepath.Join(dir, LispConformanceDeclarationFile), data, 0o644); err != nil {
				t.Fatal(err)
			}
			if _, err := LoadLispConformanceDeclaration(dir); err == nil {
				t.Fatal("unverifiable declaration accepted")
			}
		})
	}
}

func TestLispNativeBoundaryVerificationRejectsMissingAndWrongArity(t *testing.T) {
	dir := t.TempDir()
	if err := writeFiles(dir, map[string]string{
		"src/native.lisp": "(in-package #:axllm/core)\n(defun present (a) a)\n",
	}); err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name  string
		arity int
		want  string
	}{
		{"present", 1, ""},
		{"absent", 1, "undefined native boundary absent"},
		{"present", 2, "accepts 1 argument(s)"},
	} {
		err := verifyLispNativeBoundaries(dir, LispBoundaryManifest{
			Boundaries: []LispBoundaryEntry{{Name: tc.name, ObservedArities: []int{tc.arity}}},
		})
		if tc.want == "" && err != nil || tc.want != "" && (err == nil || !strings.Contains(err.Error(), tc.want)) {
			t.Errorf("%s/%d: %v", tc.name, tc.arity, err)
		}
	}
}

func TestLispRequiredSBCLCannotSkip(t *testing.T) {
	t.Setenv("PATH", t.TempDir())
	report, err := verifyLispTarget(VerifyTargetReport{Target: "lisp", OutDir: t.TempDir()}, t.TempDir())
	if err == nil || len(report.Steps) != 1 || report.Steps[0].Status != "fail" {
		t.Fatalf("missing SBCL passed: %+v, %v", report, err)
	}
}
