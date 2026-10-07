// Command lisp-core regenerates the Common Lisp emission of the Core
// registry: packages/lisp/src/core.lisp and the boundary manifest beside it,
// packages/lisp/src/core-boundaries.json.
//
// It is a standalone command rather than an axir compile target on purpose:
// the Lisp package is not yet a claimed AxIR language backend, so it stays
// out of Compile and out of default verify until conformance runs green.
//
// From tools/axir:
//
//	go run ./cmd/lisp-core --out ../../packages/lisp/src/core.lisp
//	go run ./cmd/lisp-core --check
//	go run ./cmd/lisp-core --verify-runtime
package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

	"github.com/ax-llm/ax/tools/axir/internal/axir"
)

const (
	defaultRoot     = "../../ir/axcore/root.axir"
	defaultOut      = "../../packages/lisp/src/core.lisp"
	defaultManifest = "../../packages/lisp/src/core-boundaries.json"
	defaultNative   = "../../packages/lisp/src"
)

func main() {
	if err := run(os.Args[1:]); err != nil {
		fmt.Fprintln(os.Stderr, "lisp-core: "+err.Error())
		os.Exit(1)
	}
}

func run(args []string) error {
	fs := flag.NewFlagSet("lisp-core", flag.ContinueOnError)
	fs.SetOutput(os.Stderr)
	root := fs.String("root", defaultRoot, "root .axir module to load")
	out := fs.String("out", defaultOut, "generated Common Lisp file to write")
	manifestOut := fs.String("manifest", defaultManifest, "generated boundary manifest to write")
	check := fs.Bool("check", false, "verify the files on disk match what this generator would write; write nothing")
	verifyRuntime := fs.Bool("verify-runtime", false, "report every manifest boundary that no native Lisp file defines; write nothing")
	nativeDir := fs.String("native-dir", defaultNative, "directory of hand-written .lisp files that define native boundaries")
	if err := fs.Parse(args); err != nil {
		return err
	}
	if rest := fs.Args(); len(rest) > 0 {
		return fmt.Errorf("unexpected argument %q", rest[0])
	}

	// The Core file and its boundary manifest are one artifact pair: the
	// manifest describes exactly the calls that file makes. Writing the
	// file somewhere else while leaving the manifest on its default path
	// silently puts the official manifest ahead of the official source, so
	// an --out without an explicit --manifest moves the manifest with it.
	explicit := map[string]bool{}
	fs.Visit(func(f *flag.Flag) { explicit[f.Name] = true })
	if explicit["out"] && !explicit["manifest"] {
		*manifestOut = filepath.Join(filepath.Dir(*out), filepath.Base(defaultManifest))
	}
	if explicit["manifest"] && !explicit["out"] {
		return fmt.Errorf("--manifest without --out would write the manifest for a Core file that is not being written; pass both or neither")
	}

	generated, manifest, err := generate(*root)
	if err != nil {
		return err
	}

	if *verifyRuntime {
		return reportMissingBoundaries(manifest, *nativeDir, *out)
	}

	if *check {
		if err := checkFile(*out, generated, "go run ./cmd/lisp-core --out "+*out); err != nil {
			return err
		}
		return checkFile(*manifestOut, manifest, "go run ./cmd/lisp-core --manifest "+*manifestOut)
	}

	if err := writeFile(*out, generated); err != nil {
		return err
	}
	return writeFile(*manifestOut, manifest)
}

func writeFile(path, content string) error {
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		return err
	}
	fmt.Printf("lisp-core: wrote %s (%d bytes)\n", path, len(content))
	return nil
}

func checkFile(path, want, regenerate string) error {
	existing, err := os.ReadFile(path)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return fmt.Errorf("%s does not exist; run: %s", path, regenerate)
		}
		return err
	}
	if string(existing) == want {
		fmt.Printf("lisp-core: %s is up to date (%d bytes)\n", path, len(want))
		return nil
	}
	return fmt.Errorf("%s is stale: on disk %d bytes, generated %d bytes%s\n  regenerate with: %s",
		path, len(existing), len(want), firstDifference(string(existing), want), regenerate)
}

func generate(root string) (string, string, error) {
	bundle, err := axir.LoadBundle(root)
	if err != nil {
		return "", "", err
	}
	if ds := axir.Check(bundle); ds.HasErrors() {
		return "", "", ds
	}
	model, err := axir.BuildRuntimeModel(axir.LowerToCore(bundle))
	if err != nil {
		return "", "", err
	}
	generated, err := axir.BuildLispCore(model)
	if err != nil {
		return "", "", err
	}
	manifest, err := axir.BuildLispCoreBoundaryManifest(model)
	if err != nil {
		return "", "", err
	}
	return generated, manifest, nil
}

var lispDefunRe = regexp.MustCompile(`(?m)^\((?:defun|defmacro|defgeneric)\s+([^\s()]+)`)

// reportMissingBoundaries is the integration gate: it intersects the
// generated manifest with the function definitions the hand-written native
// files provide and fails naming every boundary nobody has written yet.
// Common Lisp resolves a function name at call time, so without this check
// a missing boundary would load quietly and fail only when its path runs.
func reportMissingBoundaries(manifestJSON, nativeDir, generatedPath string) error {
	var manifest axir.LispBoundaryManifest
	if err := json.Unmarshal([]byte(manifestJSON), &manifest); err != nil {
		return err
	}
	generatedName := filepath.Base(generatedPath)
	shapes, read, err := axir.LoadLispNativeLambdaShapes(nativeDir, generatedName)
	if err != nil {
		return err
	}
	if read == 0 {
		return fmt.Errorf("no hand-written .lisp files found in %s", nativeDir)
	}
	defined := map[string][]string{}
	for name, shape := range shapes {
		defined[name] = append(defined[name], shape.File)
	}

	// A boundary that exists but takes the wrong number of arguments is a
	// worse failure than a missing one: it loads cleanly and fails only
	// when its code path first runs.
	arityProblems := axir.CheckLispBoundaryArities(manifest, shapes)
	groups := map[string][]string{}
	missing := 0
	for _, boundary := range manifest.Boundaries {
		if len(defined[boundary.Name]) > 0 {
			continue
		}
		missing++
		group := "unclassified by Core; owner is a decision"
		if boundary.HostBoundary != nil {
			group = "pure; belongs in core-runtime.lisp"
			if *boundary.HostBoundary {
				group = "host effect; belongs to its subsystem's native file"
			}
		}
		groups[group] = append(groups[group],
			fmt.Sprintf("%s (args=%v, %d call sites)", boundary.Name, boundary.ObservedArities, boundary.CallSites))
	}
	fmt.Printf("lisp-core: %d boundaries declared, %d defined across %d native file(s) in %s\n",
		len(manifest.Boundaries), len(manifest.Boundaries)-missing, read, nativeDir)
	if len(arityProblems) > 0 {
		var b strings.Builder
		for _, problem := range arityProblems {
			fmt.Fprintf(&b, "\n    %s", problem)
		}
		return fmt.Errorf("%d defined boundary/boundaries cannot serve the generated calls:%s", len(arityProblems), b.String())
	}
	if missing == 0 {
		fmt.Printf("lisp-core: every declared boundary is defined with a compatible argument count\n")
		return nil
	}
	names := make([]string, 0, len(groups))
	for group := range groups {
		names = append(names, group)
	}
	sort.Strings(names)
	var b strings.Builder
	for _, group := range names {
		sort.Strings(groups[group])
		fmt.Fprintf(&b, "\n  %d %s:\n", len(groups[group]), group)
		for _, name := range groups[group] {
			fmt.Fprintf(&b, "    %s\n", name)
		}
	}
	return fmt.Errorf("%d of %d native boundaries are undefined:%s", missing, len(manifest.Boundaries), b.String())
}

// firstDifference names the first differing line, so --check says where the
// drift is instead of only that two byte counts differ.
func firstDifference(have, want string) string {
	haveLines, wantLines := splitLines(have), splitLines(want)
	for i := 0; i < len(haveLines) || i < len(wantLines); i++ {
		var h, w string
		if i < len(haveLines) {
			h = haveLines[i]
		}
		if i < len(wantLines) {
			w = wantLines[i]
		}
		if h != w {
			return fmt.Sprintf("\n  first difference at line %d:\n    on disk:   %s\n    generated: %s", i+1, truncate(h), truncate(w))
		}
	}
	return ""
}

func splitLines(text string) []string {
	var lines []string
	start := 0
	for i := 0; i < len(text); i++ {
		if text[i] == '\n' {
			lines = append(lines, text[start:i])
			start = i + 1
		}
	}
	if start < len(text) {
		lines = append(lines, text[start:])
	}
	return lines
}

func truncate(text string) string {
	const limit = 120
	if len(text) <= limit {
		return text
	}
	return text[:limit] + "..."
}
