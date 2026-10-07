import { spawnSync } from 'node:child_process';
import {
  existsSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';

import {
  publicExampleLanguageById,
  readPublicExampleCatalog,
  resolvePublicExample,
} from './example-catalog.mjs';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(scriptDir, '..');
const runner = path.join(scriptDir, 'run-example.mjs');
const lispPackage = path.join(repoRoot, 'packages', 'lisp');

const catalog = await readPublicExampleCatalog({ repoRoot });
const lispExamples = catalog.byLanguage.lisp ?? [];

function hasSbcl() {
  const probe = spawnSync('sbcl', ['--version'], { stdio: 'ignore' });
  return !probe.error && probe.status === 0;
}

function lispPathname(value) {
  const escaped = value.replace(/\\/g, '/').replace(/(["\\])/g, '\\$1');
  return `#p"${escaped}"`;
}

// The examples are written against the full Common Lisp port, so these public
// names must exist. A missing name, or a package that will not load, is a
// failure rather than a reason to skip the compile gate: a skip that the suite
// still reports as green is the one failure that looks exactly like a pass.
// Only an absent SBCL skips. Point AX_LISP_PACKAGE_DIR at another checkout of
// the package to gate against that tree instead of the committed one.
const requiredPublicNames = [
  'ax',
  'forward',
  'tool',
  'generator-function-call-traces',
  'agent',
  'agent-forward',
  'make-process-runtime',
  'flow',
  'flow-execute',
  'optimize-program',
  'make-bootstrap-few-shot',
  'make-mcp-client',
];

function missingPublicNames() {
  const packageDir = process.env.AX_LISP_PACKAGE_DIR
    ? path.resolve(repoRoot, process.env.AX_LISP_PACKAGE_DIR)
    : lispPackage;
  const forms = [
    '(require :asdf)',
    // Only the third-party dependencies are muffled, and only for warnings a
    // dependency may legitimately signal while compiling. Ax itself is loaded
    // unprotected: if the package will not load, the probe must say so rather
    // than report an empty missing list.
    `(handler-bind ((warning #'muffle-warning)) (dolist (s '("yason" "cl-ppcre" "drakma" "cl-base64" "cffi" "puri" "ironclad" "local-time" "sqlite")) (asdf:load-system s)))`,
    `(push ${lispPathname(`${packageDir}/`)} asdf:*central-registry*)`,
    '(asdf:load-system "axllm")',
    `(format t "~&MISSING:~{ ~a~}~%" (remove-if (lambda (n) (let ((s (find-symbol (string-upcase n) "AXLLM"))) (and s (eq (nth-value 1 (find-symbol (string-upcase n) "AXLLM")) :external)))) '(${requiredPublicNames.map((name) => `"${name}"`).join(' ')})))`,
  ];
  const args = [
    '--noinform',
    '--disable-debugger',
    '--no-sysinit',
    '--no-userinit',
  ];
  for (const form of forms) args.push('--eval', form);
  args.push('--quit');
  const probe = spawnSync('sbcl', args, { cwd: repoRoot, encoding: 'utf8' });
  const line = (probe.stdout ?? '')
    .split(/\r?\n/)
    .find((row) => row.startsWith('MISSING:'));
  if (!line) return ['axllm (the system did not load)'];
  return line.slice('MISSING:'.length).trim().split(/\s+/).filter(Boolean);
}

const sbclPresent = hasSbcl();
const missingNames = sbclPresent ? missingPublicNames() : [];

describe('run-example Common Lisp support', () => {
  it('registers the language with Lisp comment headers and its own claimed groups', () => {
    const language = publicExampleLanguageById.get('lisp');
    expect(language).toBeDefined();
    expect(language.extensions).toEqual(['.lisp']);
    expect(language.comment).toBe(';;');
    // The claim is the shared required groups plus mcp. It is asserted as a
    // set rather than as an exclusion list: a previous version of this test
    // pinned the absence of audio, which turned a gap in the port into a rule
    // the test then defended after the surface landed.
    expect(language.requiredGroups).toEqual([
      'generation',
      'short-agents',
      'flows',
      'optimization',
      'audio',
      'mcp',
    ]);
  });

  it('exposes three levels in each claimed group', () => {
    expect(lispExamples.length).toBeGreaterThanOrEqual(15);
    for (const group of publicExampleLanguageById.get('lisp').requiredGroups) {
      const rows = lispExamples.filter((example) => example.group === group);
      expect(
        rows.map((example) => example.level).sort(),
        `group ${group}`
      ).toEqual(['advanced', 'beginner', 'intermediate']);
    }
  });

  it('keeps every example provider-backed and free of mock markers', () => {
    for (const example of lispExamples) {
      expect(example.provider, example.sourcePath).toBeTruthy();
      expect(example.env.length, example.sourcePath).toBeGreaterThan(0);
      const source = readFileSync(
        path.join(repoRoot, example.sourcePath),
        'utf8'
      );
      // The public catalog is the real-provider surface; scripted transports
      // belong in the port's own tests, not here.
      expect(/\bscripted\b/i.test(source), example.sourcePath).toBe(false);
      expect(/\bmock\b/i.test(source), example.sourcePath).toBe(false);
    }
  });

  it('resolves a Lisp example from every accepted language alias', () => {
    const wanted = 'src/examples/lisp/generation/axgen-openai.lisp';
    for (const alias of ['lisp', 'cl', 'sbcl', 'common-lisp']) {
      const resolved = resolvePublicExample(catalog, alias, wanted);
      expect(resolved?.sourcePath, alias).toBe(wanted);
    }
  });

  it('prints the Lisp invocation in its usage text', () => {
    const result = spawnSync(process.execPath, [runner], {
      cwd: repoRoot,
      encoding: 'utf8',
    });
    expect(result.status).toBe(1);
    expect(result.stderr).toContain('npm run example -- lisp ');
  });

  it('lists the Lisp examples in the JSON catalog', () => {
    const result = spawnSync(process.execPath, [runner, 'list', '--json'], {
      cwd: repoRoot,
      encoding: 'utf8',
      // The whole catalog is well over spawnSync's 1 MiB default.
      maxBuffer: 16 * 1024 * 1024,
    });
    expect(result.status).toBe(0);
    const parsed = JSON.parse(result.stdout);
    expect(parsed.byLanguage.lisp.length).toBe(lispExamples.length);
    for (const example of parsed.byLanguage.lisp) {
      expect(example.command).toContain('npm run example -- lisp ');
    }
  });

  it('ships the native package the runner loads', () => {
    expect(existsSync(path.join(lispPackage, 'axllm.asd'))).toBe(true);
  });

  // The decisive check: every example compiles against the real package with
  // warnings fatal, so an example naming an API the port does not implement
  // fails here rather than at a provider call. Skipped only when SBCL is
  // absent, which keeps the suite runnable on a machine without a Lisp. A
  // missing export or a package that will not load fails the test below
  // instead of skipping this one.
  it.skipIf(!sbclPresent)(
    'compiles every Lisp example against the native package with warnings fatal',
    () => {
      for (const example of lispExamples) {
        const result = spawnSync(
          process.execPath,
          [runner, 'lisp', example.sourcePath, '--compile-only'],
          { cwd: repoRoot, encoding: 'utf8' }
        );
        expect(
          result.status,
          `${example.sourcePath}\n${result.stdout}\n${result.stderr}`
        ).toBe(0);
      }
    },
    600_000
  );

  // The cleanup used to sit in a `finally`, which never ran: the shared `run`
  // helper calls process.exit on a nonzero status. So a failing compile left
  // its scratch directory behind, and before that an output file beside the
  // source. This drives an example that cannot compile and requires both to be
  // true afterwards: the command failed, and nothing was left behind.
  it.skipIf(!sbclPresent)(
    'cleans up after a failing compile and still reports the failure',
    () => {
      // The header matters: without it the runner rejects the path during
      // catalog resolution and never reaches the compiler, which would make
      // this test pass without exercising anything. An earlier version of it
      // did exactly that.
      const broken = path.join(
        repoRoot,
        'src/examples/lisp/generation/_broken-fixture.lisp'
      );
      writeFileSync(
        broken,
        [
          ';;;; ax-example:start',
          ';;;; title: Deliberately Broken Fixture',
          ';;;; group: generation',
          ';;;; description: A temporary fixture that cannot compile.',
          ';;;; provider: openai',
          ';;;; env: OPENAI_API_KEY',
          ';;;; level: beginner',
          ';;;; order: 999',
          ';;;; ax-example:end',
          '(ax:this-symbol-does-not-exist)',
          '',
        ].join('\n')
      );
      // The child is given its own TMPDIR, so this owns every scratch
      // directory it can observe. Scanning the shared /tmp instead would fail
      // whenever an unrelated legitimate compile happened to be running.
      const ownTmp = mkdtempSync(path.join(tmpdir(), 'ax-lisp-test-tmp-'));
      try {
        const result = spawnSync(
          process.execPath,
          [runner, 'lisp', broken, '--compile-only'],
          {
            cwd: repoRoot,
            encoding: 'utf8',
            env: { ...process.env, TMPDIR: ownTmp },
          }
        );
        expect(result.status, 'a broken example must fail the gate').not.toBe(
          0
        );
        // Prove the failure came from SBCL compiling the file, not from the
        // runner rejecting the path before it ever got there. Without this a
        // catalog-resolution error would satisfy the status assertion and the
        // test would pass against unfixed cleanup code.
        expect(
          `${result.stdout ?? ''}${result.stderr ?? ''}`,
          'the failure must come from SBCL, not catalog resolution'
        ).toContain('THIS-SYMBOL-DOES-NOT-EXIST');
        expect(existsSync(`${broken.slice(0, -5)}.fasl`)).toBe(false);
        expect(
          readdirSync(ownTmp),
          'the runner must not leave a scratch directory behind'
        ).toEqual([]);
      } finally {
        rmSync(broken, { force: true });
        rmSync(ownTmp, { recursive: true, force: true });
      }
    },
    300_000
  );

  // A missing public name used to skip the compile gate while this test still
  // passed, which reported green for a package the examples cannot be checked
  // against. It is now a failure, named.
  it.skipIf(!sbclPresent)(
    'exports every public name the examples are written against',
    () => {
      expect(
        missingNames,
        missingNames.length === 0
          ? ''
          : `packages/lisp does not export: ${missingNames.join(', ')}. Set AX_LISP_PACKAGE_DIR to another checkout of the package to gate against that tree.`
      ).toEqual([]);
    },
    120_000
  );
});
