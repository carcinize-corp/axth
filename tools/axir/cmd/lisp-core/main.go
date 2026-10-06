// Command lisp-core regenerates packages/lisp/src/core.lisp, the Common Lisp
// emission of the experimental signature/schema Core subset.
//
// It is a standalone command rather than an axir compile target on purpose:
// the Lisp package is an explicit experimental subset, not a claimed AxIR
// language backend, so it stays out of Compile and out of default verify.
//
// From tools/axir:
//
//	go run ./cmd/lisp-core --out ../../packages/lisp/src/core.lisp
//	go run ./cmd/lisp-core --check
package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/ax-llm/ax/tools/axir/internal/axir"
)

const (
	defaultRoot = "../../ir/axcore/root.axir"
	defaultOut  = "../../packages/lisp/src/core.lisp"
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
	check := fs.Bool("check", false, "verify the file on disk matches what this generator would write; write nothing")
	if err := fs.Parse(args); err != nil {
		return err
	}
	if rest := fs.Args(); len(rest) > 0 {
		return fmt.Errorf("unexpected argument %q", rest[0])
	}

	generated, err := generate(*root)
	if err != nil {
		return err
	}

	if *check {
		existing, err := os.ReadFile(*out)
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return fmt.Errorf("%s does not exist; run: go run ./cmd/lisp-core --out %s", *out, *out)
			}
			return err
		}
		if string(existing) == generated {
			fmt.Printf("lisp-core: %s is up to date (%d bytes)\n", *out, len(generated))
			return nil
		}
		return fmt.Errorf("%s is stale: on disk %d bytes, generated %d bytes%s\n  regenerate with: go run ./cmd/lisp-core --out %s",
			*out, len(existing), len(generated), firstDifference(string(existing), generated), *out)
	}

	if err := os.MkdirAll(filepath.Dir(*out), 0o755); err != nil {
		return err
	}
	if err := os.WriteFile(*out, []byte(generated), 0o644); err != nil {
		return err
	}
	fmt.Printf("lisp-core: wrote %s (%d bytes)\n", *out, len(generated))
	return nil
}

func generate(root string) (string, error) {
	bundle, err := axir.LoadBundle(root)
	if err != nil {
		return "", err
	}
	if ds := axir.Check(bundle); ds.HasErrors() {
		return "", ds
	}
	model, err := axir.BuildRuntimeModel(axir.LowerToCore(bundle))
	if err != nil {
		return "", err
	}
	return axir.BuildLispCore(model)
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
