#!/usr/bin/env node
// Compile every Common Lisp snippet the website publishes against the real
// loaded `axllm` system.
//
// Checking that a symbol is exported is not enough: a snippet can name the
// right function and still pass the wrong number of arguments or a keyword the
// lambda list does not accept. SBCL reports those as STYLE-WARNINGs, not full
// warnings, so muffling style-warnings makes this check vacuous. Measured
// texts from SBCL 2.2.9:
//
//   The function AXLLM:JGET is called with zero arguments, but wants at least two.
//   :BOGUS-KEYWORD is not a known argument keyword.
//   undefined function: PROBE::MY-HELPER
//   undefined variable: PROBE::SOME-FREE-VAR
//
// So every warning is collected and only two kinds are allowed: an undefined
// function or variable outside the AXLLM package, which is a placeholder such
// as `#'metric` or a free `client`. An undefined name *inside* AXLLM means the
// snippet published a name the package does not have, and fails.
//
// Sources checked:
//   website/content-src/languages/lisp.json   snippets, snippetGroups, academy.snippets
//   scripts/website-prepare.mjs               the `lisp` entry in generatedPackageSnippets
//   website/data/homepage_languages.yaml      the lisp homepage block
//
// Usage: node scripts/website-lisp-snippet-check.mjs [--print]
// Requires SBCL and the ASDF dependencies in packages/lisp/README.md.

import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parse as parseYaml } from 'yaml';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, '..');
const asd = path.join(repoRoot, 'packages', 'lisp', 'axllm.asd');

const snippets = [
  ...fromLanguageJson(),
  ...fromPrepareScript(),
  ...fromHomepageData(),
];

if (snippets.length === 0) {
  fail('found no Lisp snippets to check; the extractors are out of date');
}

if (process.argv.includes('--print')) {
  for (const snippet of snippets) {
    console.log(`----- ${snippet.id}\n${snippet.code}`);
  }
}

const results = compileAll(snippets);
const failures = results.filter((result) => result.warnings.length > 0);

for (const failure of failures) {
  console.error(`\n✗ ${failure.id}`);
  for (const warning of failure.warnings) {
    console.error(`    ${warning.replaceAll('\n', '\n    ')}`);
  }
  console.error(`  source:\n    ${failure.code.replaceAll('\n', '\n    ')}`);
}

if (failures.length > 0) {
  fail(
    `${failures.length} of ${snippets.length} Lisp snippets do not compile against the loaded axllm system`
  );
}

console.log(
  `all ${snippets.length} website Lisp snippets compile against packages/lisp (no full warnings)`
);

function fail(message) {
  console.error(`website Lisp snippet check failed: ${message}`);
  process.exit(1);
}

// A snippet value is either an array of lines, a string, or a metadata object
// with `code`. Anything carrying its own non-lisp fence is skipped.
function snippetCode(value) {
  if (Array.isArray(value)) return value.join('\n');
  if (typeof value === 'string') return value;
  if (value && typeof value === 'object') {
    if (value.fence && value.fence !== 'lisp') return undefined;
    return snippetCode(value.code ?? value.lines ?? value.snippet);
  }
  return undefined;
}

function collect(node, trail, out) {
  const code = snippetCode(node);
  if (code !== undefined) {
    if (code.trim()) out.push({ id: trail.join('.'), code });
    return;
  }
  if (node && typeof node === 'object' && !Array.isArray(node)) {
    for (const [key, child] of Object.entries(node)) {
      collect(child, [...trail, key], out);
    }
  }
}

function fromLanguageJson() {
  const file = path.join(repoRoot, 'website/content-src/languages/lisp.json');
  const language = JSON.parse(readFileSync(file, 'utf8'));
  const out = [];
  collect(language.snippets, ['lisp.json', 'snippets'], out);
  collect(language.snippetGroups, ['lisp.json', 'snippetGroups'], out);
  collect(language.academy?.snippets, ['lisp.json', 'academy'], out);
  return out;
}

function fromPrepareScript() {
  const file = path.join(repoRoot, 'scripts/website-prepare.mjs');
  const source = readFileSync(file, 'utf8');
  const start = source.indexOf('\n    lisp: {');
  if (start === -1) {
    fail(
      'scripts/website-prepare.mjs has no `lisp:` entry in generatedPackageSnippets'
    );
  }
  const end = source.indexOf('\n    },\n  };', start);
  if (end === -1) {
    fail('could not find the end of the `lisp:` snippet entry');
  }
  const literal = source.slice(start, end + '\n    }'.length);
  // The entry is a plain object literal of string arrays, so evaluating it is
  // how we check the exact text that ships rather than a copy of it.
  const entry = new Function(`return {${literal}};`)().lisp;
  const out = [];
  collect(entry, ['website-prepare.mjs', 'generatedPackageSnippets'], out);
  return out;
}

function fromHomepageData() {
  const file = path.join(repoRoot, 'website/data/homepage_languages.yaml');
  const data = parseYaml(readFileSync(file, 'utf8'));
  const language = (data.languages ?? []).find((row) => row.id === 'lisp');
  if (!language) {
    fail('website/data/homepage_languages.yaml has no lisp language');
  }
  const out = [];
  for (const key of [
    'classifier',
    'audio',
    'signatureString',
    'signatureFluent',
    'signatureSchema',
    'agent',
    'provider',
  ]) {
    const code = language[key]?.code;
    if (code?.trim()) {
      out.push({ id: `homepage_languages.yaml.${key}`, code });
    }
  }
  for (const [name, code] of Object.entries(language.patterns ?? {})) {
    if (typeof code === 'string' && code.trim()) {
      out.push({ id: `homepage_languages.yaml.patterns.${name}`, code });
    }
  }
  return out;
}

function compileAll(items) {
  const dir = mkdtempSync(path.join(tmpdir(), 'lisp-snippet-check-'));
  const manifest = path.join(dir, 'snippets.json');
  const report = path.join(dir, 'report.json');
  writeFileSync(manifest, JSON.stringify(items), 'utf8');
  const driver = path.join(dir, 'check.lisp');
  writeFileSync(driver, lispDriver(manifest, report, dir), 'utf8');

  const run = spawnSync('sbcl', ['--script', driver], {
    encoding: 'utf8',
    maxBuffer: 64 * 1024 * 1024,
    env: { ...process.env, TMPDIR: dir },
  });
  if (run.status !== 0) {
    console.error(run.stdout ?? '');
    console.error(run.stderr ?? '');
    fail(
      'SBCL did not finish; install sbcl and the ASDF systems in packages/lisp/README.md'
    );
  }
  let parsed;
  try {
    parsed = JSON.parse(readFileSync(report, 'utf8'));
  } catch {
    console.error(run.stdout ?? '');
    fail('the SBCL driver wrote no readable report');
  }
  return parsed.map((row, index) => ({ ...items[index], ...row }));
}

// Free variables in a snippet are bound to NIL so that an unbound-variable
// warning cannot mask a real arity warning; the function is never called.
// Undefined-variable warnings are collected and fed back as bindings, because
// the set of placeholder names differs per snippet.
function lispDriver(manifestPath, reportPath, scratchDir) {
  return `(require :asdf)
(require :sb-introspect)
(asdf:load-asd #P"${asd}")
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "axllm"))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system "yason"))

(defpackage #:snippet-check (:use #:cl))
(in-package #:snippet-check)

(defun read-forms (text)
  (with-input-from-string (in text)
    (let ((forms '()) (*read-eval* nil))
      (loop for form = (read in nil :snippet-check-eof)
            until (eq form :snippet-check-eof)
            do (push form forms))
      (nreverse forms))))

(defun name-after (prefix text)
  (let ((marker (search prefix text)))
    (when marker
      (let* ((rest (subseq text (+ marker (length prefix))))
             (end (position-if (lambda (c) (member c '(#\\Space #\\Newline))) rest)))
        (string-trim "." (if end (subseq rest 0 end) rest))))))

(defun undefined-variable-name (text)
  (name-after "undefined variable: " text))

(defun axllm-name-p (printed)
  "True when PRINTED, as SBCL prints an undefined name, lives in AXLLM."
  (let ((colon (position #\\: printed)))
    (and colon (string-equal "AXLLM" (subseq printed 0 colon)))))

(defun allowed-warning-p (text)
  "A placeholder helper or free variable outside AXLLM is expected.
An undefined name inside AXLLM is a published name the package lacks."
  (let ((fn (name-after "undefined function: " text))
        (var (undefined-variable-name text)))
    (cond (fn (not (axllm-name-p fn)))
          (var (not (axllm-name-p var)))
          (t nil))))

(defun compile-once (index forms bound)
  "Compile FORMS as an uncalled function. Returns (values warnings new-bound)."
  (let* ((name (intern (format nil "SNIPPET-~d" index) :snippet-check))
         (bindings (mapcar (lambda (s) (list s nil)) bound))
         (source (if bindings
                     \`(defun ,name ()
                        (let ,bindings
                          (declare (ignorable ,@bound))
                          ,@forms))
                     \`(defun ,name () ,@forms)))
         (file (merge-pathnames (format nil "snippet-~d.lisp" index)
                                #P"${scratchDir}/"))
         (warnings '()))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (let ((*package* (find-package :snippet-check))
            (*print-readably* nil)
            (*print-circle* nil))
        (write source :stream out :pretty t :escape t)
        (terpri out)))
    (handler-bind
        ((warning (lambda (c)
                    (push (princ-to-string c) warnings)
                    (muffle-warning c))))
      (handler-case
          (multiple-value-bind (fasl warned failed)
              (compile-file file :verbose nil :print nil
                                 :output-file (merge-pathnames
                                               (format nil "snippet-~d.fasl" index)
                                               #P"${scratchDir}/"))
            (declare (ignore fasl warned failed)))
        (error (e) (push (format nil "READ/COMPILE ERROR: ~a" e) warnings))))
    (let ((new '()))
      (dolist (w warnings)
        (let ((var (undefined-variable-name w)))
          (when (and var (not (axllm-name-p var)))
            (let ((sym (ignore-errors (read-from-string var))))
              (when (and (symbolp sym) sym (not (member sym bound)))
                (push sym new))))))
      (values (nreverse warnings) (append bound new)))))

(defun check-snippet (index text)
  (handler-case
      (let ((forms (read-forms text))
            (bound '()))
        (if (null forms)
            '()
            (let ((warnings '()))
              ;; Up to three passes: each pass binds the placeholder variables
              ;; the previous pass reported, so what is left is a real problem.
              (dotimes (pass 3)
                (declare (ignore pass))
                (multiple-value-bind (w b) (compile-once index forms bound)
                  (setf warnings w)
                  (if (equal b bound) (return) (setf bound b))))
              (remove-if #'allowed-warning-p warnings))))
    (error (e) (list (format nil "READ ERROR: ~a" e)))))

(let* ((text (with-open-file (in #P"${manifestPath}")
               (let ((s (make-string (file-length in))))
                 (subseq s 0 (read-sequence s in)))))
       (items (yason:parse text))
       (results '())
       (index 0))
  (dolist (item items)
    (let ((warnings (check-snippet index (gethash "code" item))))
      (push (let ((row (make-hash-table :test #'equal)))
              (setf (gethash "id" row) (gethash "id" item))
              (setf (gethash "warnings" row) (coerce warnings 'vector))
              row)
            results)
      (incf index)))
  (with-open-file (out #P"${reportPath}" :direction :output :if-exists :supersede)
    (yason:encode (coerce (nreverse results) 'vector) out)))
(format t "~&snippet check finished~%")
`;
}
