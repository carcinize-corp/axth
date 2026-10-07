import { stdin as input, stdout as output } from 'node:process';
import { createInterface } from 'node:readline';

import {
  extractDurableWriteTargets,
  extractReadIdentifiers,
  getQualifiedCallableUsages,
} from '../../../src/ax/agent/contextManager.js';
import { AxJSRuntime } from '../../../src/ax/funcs/jsRuntime.js';

type JsonObject = Record<string, unknown>;

type ProtocolMessage = {
  id?: string | number;
  op?: string;
  session_id?: string;
  payload?: JsonObject;
  ok?: boolean;
  result?: unknown;
  error?: { category?: string; message?: string };
};

type RuntimeSession = {
  execute(code: string, options?: JsonObject): Promise<unknown>;
  executeWithStatus?(
    code: string,
    options?: JsonObject
  ): Promise<{ value: unknown; isError: boolean }>;
  inspectGlobals?(options?: JsonObject): Promise<unknown>;
  snapshotGlobals?(options?: JsonObject): Promise<unknown>;
  patchGlobals?(globals: JsonObject, options?: JsonObject): Promise<unknown>;
  close(): void;
};

type RuntimeLike = {
  readonly language?: string;
  getUsageInstructions(): string;
  createSession(globals?: JsonObject, options?: JsonObject): RuntimeSession;
};

function errorCategory(error: unknown): string {
  if (error && typeof error === 'object') {
    const value = error as Record<string, unknown>;
    if (typeof value.error_category === 'string') return value.error_category;
    if (typeof value.category === 'string') return value.category;
    if (value.name === 'AbortError') return 'abort';
  }
  return 'runtime';
}

function errorMessage(error: unknown): string {
  if (error instanceof Error) return error.message;
  return String(error);
}

function ok(id: ProtocolMessage['id'], result: unknown, extra?: JsonObject) {
  return { id, ok: true, result, ...(extra ?? {}) };
}

function fail(id: ProtocolMessage['id'], error: unknown, category?: string) {
  return {
    id,
    ok: false,
    error: {
      category: category ?? errorCategory(error),
      message: errorMessage(error),
    },
  };
}

function withRuntimePrimitives(
  globals: JsonObject,
  complete: (value: JsonObject) => JsonObject
): JsonObject {
  return {
    ...globals,
    final: (...args: unknown[]) => complete({ type: 'final', args }),
    respond: (...args: unknown[]) => complete({ type: 'respond', args }),
    askClarification: (...args: unknown[]) =>
      complete({
        type: 'askClarification',
        args,
      }),
    discover: (request: unknown) =>
      complete({ kind: 'discover', discover: request }),
    recall: (request: unknown) => complete({ kind: 'recall', recall: request }),
    used: (idOrRequest: unknown, reason?: string) =>
      complete({
        kind: 'used',
        used:
          idOrRequest && typeof idOrRequest === 'object'
            ? idOrRequest
            : { id: idOrRequest, ...(reason ? { reason } : {}) },
      }),
    reportSuccess: (message: string) =>
      complete({
        kind: 'status',
        status: { type: 'success', message },
      }),
    reportFailure: (message: string) =>
      complete({
        kind: 'status',
        status: { type: 'failed', message },
      }),
    guideAgent: (guidance: string) =>
      complete({
        type: 'guide_agent',
        guidance,
      }),
  };
}

class FixtureSession implements RuntimeSession {
  private globals: JsonObject;
  private closed = false;

  constructor(globals: JsonObject) {
    this.globals = { ...globals };
  }

  async execute(code: string): Promise<unknown> {
    if (this.closed)
      throw Object.assign(new Error('session closed'), {
        category: 'session_closed',
      });
    if (code === 'timeout()')
      throw Object.assign(new Error('fixture timeout'), {
        category: 'timeout',
      });
    this.globals.answer = 'fixture';
    if (code.includes('askClarification')) {
      return { type: 'askClarification', args: [{ question: 'Need detail?' }] };
    }
    return { type: 'final', args: [{ answer: this.globals.answer }] };
  }

  async inspectGlobals(): Promise<unknown> {
    return { ...this.globals };
  }

  async snapshotGlobals(): Promise<unknown> {
    return {
      version: 1,
      entries: [],
      bindings: { ...this.globals },
      globals: { ...this.globals },
      closed: this.closed,
    };
  }

  async patchGlobals(globals: JsonObject): Promise<unknown> {
    const bindings =
      globals.bindings && typeof globals.bindings === 'object'
        ? (globals.bindings as JsonObject)
        : globals;
    this.globals = { ...bindings };
    return this.snapshotGlobals();
  }

  close(): void {
    this.closed = true;
  }
}

class FixtureRuntime implements RuntimeLike {
  readonly language = 'JavaScript';

  getUsageInstructions(): string {
    return 'Fixture runtime for deterministic AxIR adapter protocol tests.';
  }

  createSession(globals?: JsonObject): RuntimeSession {
    return new FixtureSession(globals ?? {});
  }
}

class RuntimeProtocolServer {
  private readonly sessions = new Map<string, RuntimeSession>();
  private readonly completions = new Map<string, JsonObject>();
  private readonly activeRequests = new Map<string, ProtocolMessage>();
  private readonly callbacks = new Map<
    string,
    {
      request: ProtocolMessage;
      resolve: (value: unknown) => void;
      reject: (error: Error) => void;
    }
  >();
  private nextSessionId = 0;
  private nextCallbackId = 0;

  constructor(
    private readonly runtime: RuntimeLike,
    private readonly emitHostCall?: (frame: JsonObject) => void
  ) {}

  acceptCallbackReply(message: ProtocolMessage): boolean {
    if (message.op !== undefined || typeof message.ok !== 'boolean')
      return false;
    const id = String(message.id);
    const pending = this.callbacks.get(id);
    // Retired replies cannot resolve a later invocation's callback.
    if (!pending) return true;
    this.callbacks.delete(id);
    if (message.ok) pending.resolve(message.result ?? null);
    else {
      pending.reject(
        Object.assign(new Error(message.error?.message ?? 'host call failed'), {
          category: message.error?.category ?? 'runtime',
        })
      );
    }
    return true;
  }

  rejectCallbacks(error: Error, request?: ProtocolMessage): void {
    for (const [id, pending] of this.callbacks) {
      if (request && pending.request !== request) continue;
      this.callbacks.delete(id);
      pending.reject(error);
    }
  }

  private hostCall(
    sessionId: string,
    name: string,
    params: unknown
  ): Promise<unknown> {
    const request = this.activeRequests.get(sessionId);
    if (!request || !this.emitHostCall) {
      return Promise.reject(new Error(`host call ${name} outside an execute`));
    }
    const callbackId = `host-${++this.nextCallbackId}`;
    return new Promise((resolve, reject) => {
      this.callbacks.set(callbackId, { request, resolve, reject });
      try {
        this.emitHostCall!({
          op: 'host_call',
          callback_id: callbackId,
          request_id: request.id,
          session_id: sessionId,
          name,
          params: params ?? null,
        });
      } catch (error) {
        this.callbacks.delete(callbackId);
        reject(error);
      }
    });
  }

  private addHostCalls(
    globals: JsonObject,
    names: unknown,
    sessionId: string
  ): void {
    if (names === undefined) return;
    if (!this.emitHostCall || !Array.isArray(names)) {
      throw new Error(
        'host_calls requires a callback transport and an array of names'
      );
    }
    for (const name of names) {
      if (typeof name !== 'string')
        throw new Error('host callable name must be a string');
      const parts = name.split('.');
      if (
        parts.some(
          (part) =>
            !/^[A-Za-z_$][\w$]*$/.test(part) ||
            ['__proto__', 'prototype', 'constructor'].includes(part)
        )
      ) {
        throw new Error(`invalid host callable name: ${name}`);
      }
      let target = globals;
      for (const part of parts.slice(0, -1)) {
        if (!Object.hasOwn(target, part)) target[part] = {};
        const nested = target[part];
        if (!nested || typeof nested !== 'object' || Array.isArray(nested)) {
          throw new Error(
            `host callable namespace conflicts with a global: ${name}`
          );
        }
        target = nested as JsonObject;
      }
      const leaf = parts.at(-1)!;
      if (Object.hasOwn(target, leaf)) {
        throw new Error(`host callable conflicts with a global: ${name}`);
      }
      target[leaf] = (params: unknown) =>
        this.hostCall(sessionId, name, params);
    }
  }

  async handle(message: ProtocolMessage): Promise<unknown> {
    try {
      switch (message.op) {
        case 'capabilities':
          return ok(message.id, {
            language: this.runtime.language ?? 'JavaScript',
            usage_instructions: this.runtime.getUsageInstructions(),
            inspect: true,
            snapshot: true,
            patch: true,
            // AbortSignal is not serializable and this protocol has no abort op.
            abort: false,
            host_calls: Boolean(this.emitHostCall),
          });
        case 'create_session': {
          const payload = message.payload ?? {};
          const sessionId = `s${++this.nextSessionId}`;
          const globals = withRuntimePrimitives(
            (payload.globals && typeof payload.globals === 'object'
              ? payload.globals
              : {}) as JsonObject,
            (value) => {
              this.completions.set(sessionId, value);
              return value;
            }
          );
          this.addHostCalls(globals, payload.host_calls, sessionId);
          const session = this.runtime.createSession(
            globals,
            (payload.options && typeof payload.options === 'object'
              ? payload.options
              : {}) as JsonObject
          );
          // Keep evidence in the worker by reference across agent phases.
          // A host callback alone would copy it across the protocol boundary.
          if (session.executeWithStatus)
            await session.execute(`(() => {
            const hostFinal = globalThis.final;
            globalThis.final = function (...args) {
              if (!Object.hasOwn(globalThis.inputs ?? {}, 'executorRequest') && args.length === 2 &&
                  args[1] && typeof args[1] === 'object' && !Array.isArray(args[1])) {
                globalThis.distilledContext = args[1];
              }
              return hostFinal(...args);
            };
          })()`);
          this.sessions.set(sessionId, session);
          return ok(
            message.id,
            { session_id: sessionId },
            { session_id: sessionId }
          );
        }
        case 'execute': {
          const session = this.session(message);
          const payload = message.payload ?? {};
          const code = String(payload.code ?? '');
          const options = {
            ...(payload.options && typeof payload.options === 'object'
              ? (payload.options as JsonObject)
              : {}),
          };
          if (this.activeRequests.has(message.session_id!)) {
            return fail(
              message.id,
              new Error('execute already in flight'),
              'protocol'
            );
          }
          this.activeRequests.set(message.session_id!, message);
          this.completions.delete(message.session_id!);
          try {
            if (session.executeWithStatus) {
              // AxJSRuntime returns errors in the code (ReferenceError, …) as
              // text; hand them to the port as a runtime error envelope.
              const { value, isError } = await session.executeWithStatus(
                code,
                options
              );
              const completion = this.completions.get(message.session_id!);
              const result = isError
                ? {
                    kind: 'error',
                    is_error: true,
                    error_category: 'runtime',
                    error: String(value),
                  }
                : (completion ??
                  (value && typeof value === 'object'
                    ? value
                    : { kind: 'result', result: value ?? null }));
              const envelope = {
                ...result,
                output: !isError && typeof value === 'string' ? value : '',
                analysis: {
                  producedVars: extractDurableWriteTargets(code),
                  readVars: [...extractReadIdentifiers(code)],
                  callables: getQualifiedCallableUsages({
                    code,
                    turn: 0,
                    output: '',
                    tags: [],
                  }),
                },
              };
              return ok(message.id, envelope, {
                session_id: message.session_id,
              });
            }
            const result = await session.execute(code, options);
            return ok(message.id, result, { session_id: message.session_id });
          } finally {
            this.completions.delete(message.session_id!);
            this.activeRequests.delete(message.session_id!);
            this.rejectCallbacks(
              new Error('execute finished before host call settled'),
              message
            );
          }
        }
        case 'inspect_globals': {
          const session = this.session(message);
          if (!session.inspectGlobals) {
            return fail(
              message.id,
              new Error('inspectGlobals unavailable'),
              'unavailable'
            );
          }
          const result = await session.inspectGlobals(message.payload ?? {});
          return ok(message.id, result, { session_id: message.session_id });
        }
        case 'snapshot_globals': {
          const session = this.session(message);
          if (!session.snapshotGlobals) {
            return fail(
              message.id,
              new Error('snapshotGlobals unavailable'),
              'unavailable'
            );
          }
          const options = { ...message.payload };
          // Evidence is protected from actor reassignment but remains visible
          // in the executor's state summary, unlike other reserved globals.
          if (Array.isArray(options.reservedNames)) {
            options.reservedNames = options.reservedNames.filter(
              (name) => name !== 'distilledContext'
            );
          }
          const result = await session.snapshotGlobals(options);
          return ok(message.id, result, { session_id: message.session_id });
        }
        case 'patch_globals': {
          const session = this.session(message);
          if (!session.patchGlobals) {
            return fail(
              message.id,
              new Error('patchGlobals unavailable'),
              'unavailable'
            );
          }
          const payload = message.payload ?? {};
          const snapshot = (payload.globals ?? {}) as JsonObject;
          const bindings = (snapshot.bindings ??
            snapshot.globals ??
            snapshot) as JsonObject;
          const result = await session.patchGlobals(
            bindings,
            (payload.options && typeof payload.options === 'object'
              ? payload.options
              : {}) as JsonObject
          );
          if (snapshot.merge === true && session.executeWithStatus) {
            await session.execute(`if (typeof distilledContext !== 'undefined' && globalThis.inputs) {
              globalThis.inputs.distilledContext = distilledContext;
            }`);
          }
          const patched =
            result ??
            (session.snapshotGlobals
              ? await session.snapshotGlobals(payload.options as JsonObject)
              : { patched: true });
          return ok(message.id, patched, {
            session_id: message.session_id,
          });
        }
        case 'close': {
          const session = this.session(message);
          session.close();
          if (message.session_id) this.sessions.delete(message.session_id);
          return ok(
            message.id,
            { closed: true },
            { session_id: message.session_id }
          );
        }
        case 'shutdown':
          this.rejectCallbacks(new Error('runtime shut down'));
          for (const session of this.sessions.values()) session.close();
          this.sessions.clear();
          return ok(message.id, { shutdown: true });
        default:
          return fail(
            message.id,
            new Error(`unknown runtime protocol op: ${message.op}`),
            'protocol'
          );
      }
    } catch (error) {
      return fail(message.id, error);
    }
  }

  private session(message: ProtocolMessage): RuntimeSession {
    const sessionId = message.session_id;
    if (!sessionId || !this.sessions.has(sessionId)) {
      throw Object.assign(new Error('session closed or unknown'), {
        category: 'session_closed',
      });
    }
    return this.sessions.get(sessionId)!;
  }
}

async function selfTest(): Promise<void> {
  const server = new RuntimeProtocolServer(
    new AxJSRuntime() as unknown as RuntimeLike
  );
  const created = (await server.handle({
    id: '1',
    op: 'create_session',
    payload: {
      globals: { inputs: { question: 'adapter' } },
      options: { reservedNames: ['inputs', 'final'] },
    },
  })) as JsonObject;
  const sessionId = String(created.session_id);
  const execute = async (id: string, code: string) =>
    (await server.handle({
      id,
      op: 'execute',
      session_id: sessionId,
      payload: {
        code,
        options: { reservedNames: ['inputs', 'final'] },
      },
    })) as JsonObject;
  const expect = async (
    id: string,
    code: string,
    key: string,
    value: string
  ) => {
    const executed = await execute(id, code);
    const result = executed.result as JsonObject;
    if (result[key] !== value) {
      throw new Error(
        `self-test expected ${key}=${value}, got ${JSON.stringify(executed)}`
      );
    }
    return result;
  };
  await expect(
    '2',
    'answer = inputs.question; await final({ answer })',
    'type',
    'final'
  );
  const counter1 = (
    await execute(
      '3',
      "counter = (typeof counter === 'undefined' ? 0 : counter) + 1; await final({ counter })"
    )
  ).result as JsonObject;
  const counter2 = (
    await execute('4', 'counter = counter + 1; await final({ counter })')
  ).result as JsonObject;
  const counterPayload = (counter2.args as JsonObject[])[0] as JsonObject;
  if (counterPayload.counter !== 2) {
    throw new Error(
      `self-test persistent state failed: ${JSON.stringify(counter1)} ${JSON.stringify(counter2)}`
    );
  }
  await expect(
    '5',
    "await askClarification('more?')",
    'type',
    'askClarification'
  );
  await expect(
    '6',
    "await discover({ tools: ['search'] })",
    'kind',
    'discover'
  );
  await expect('7', "await recall({ query: 'docs' })", 'kind', 'recall');
  await expect('8', "await used('mem1', 'helpful')", 'kind', 'used');
  await expect('9', "await reportSuccess('ok')", 'kind', 'status');
  await expect('10', "await reportFailure('bad')", 'kind', 'status');
  await expect('11', "await guideAgent('try this')", 'type', 'guide_agent');
  const codeError = await expect('11b', 'brokenHelper()', 'kind', 'error');
  if (
    codeError.is_error !== true ||
    !String(codeError.error).startsWith(
      'ReferenceError: brokenHelper is not defined'
    )
  ) {
    throw new Error(
      `self-test code error envelope failed: ${JSON.stringify(codeError)}`
    );
  }
  const snapshot = (await server.handle({
    id: '12',
    op: 'snapshot_globals',
    session_id: sessionId,
  })) as JsonObject;
  if (!snapshot.ok) throw new Error('self-test snapshot failed');
  await server.handle({
    id: '13',
    op: 'patch_globals',
    session_id: sessionId,
    payload: {
      globals: (snapshot.result as JsonObject).bindings as JsonObject,
    },
  });
  await server.handle({ id: '14', op: 'close', session_id: sessionId });
  const closed = (await server.handle({
    id: '15',
    op: 'execute',
    session_id: sessionId,
    payload: { code: 'await final({})' },
  })) as JsonObject;
  if (closed.ok !== false) throw new Error('self-test closed session failed');
  await server.handle({ id: '16', op: 'shutdown' });

  const fixture = new RuntimeProtocolServer(new FixtureRuntime());
  const fixtureSession = (await fixture.handle({
    id: 'f1',
    op: 'create_session',
    payload: { globals: { inputs: {} } },
  })) as JsonObject;
  const fixtureOut = (await fixture.handle({
    id: 'f2',
    op: 'execute',
    session_id: String(fixtureSession.session_id),
    payload: { code: 'final()' },
  })) as JsonObject;
  if ((fixtureOut.result as JsonObject).type !== 'final') {
    throw new Error('fixture mode self-test failed');
  }
  console.log('axjs-runtime-server-self-test-ok');
}

async function runServer(fixtureMode: boolean): Promise<void> {
  const runtime = fixtureMode
    ? new FixtureRuntime()
    : (new AxJSRuntime() as unknown as RuntimeLike);
  let inputOpen = true;
  const server = new RuntimeProtocolServer(runtime, (frame) => {
    if (!inputOpen) throw new Error('host input closed');
    output.write(`${JSON.stringify(frame)}\n`);
  });
  const rl = createInterface({ input, crlfDelay: Number.POSITIVE_INFINITY });
  let requests = Promise.resolve();
  try {
    for await (const line of rl) {
      if (!line.trim()) continue;
      try {
        const message = JSON.parse(line) as ProtocolMessage;
        // Callback replies bypass the request queue. Awaiting execute in the
        // reader would deadlock it against the reply that it needs to finish.
        if (server.acceptCallbackReply(message)) continue;
        if (message.op === 'shutdown') {
          // Retire callbacks before queueing shutdown behind their execute.
          inputOpen = false;
          server.rejectCallbacks(new Error('runtime shut down'));
        }
        requests = requests.then(async () => {
          const response = await server.handle(message);
          output.write(`${JSON.stringify(response)}\n`);
          if ((response as JsonObject).ok && (response as JsonObject).result) {
            const result = (response as JsonObject).result as JsonObject;
            if (result.shutdown) rl.close();
          }
        });
      } catch (error) {
        output.write(`${JSON.stringify(fail(undefined, error, 'protocol'))}\n`);
      }
    }
    inputOpen = false;
    server.rejectCallbacks(new Error('host input closed'));
    await requests;
  } finally {
    inputOpen = false;
    await server.handle({ op: 'shutdown' });
    rl.close();
  }
}

const args = new Set(process.argv.slice(2));
const npmSelfTest = process.env.npm_config_self_test === 'true';
const npmFixture = process.env.npm_config_fixture === 'true';
if (args.has('--self-test') || npmSelfTest) {
  await selfTest();
} else {
  await runServer(args.has('--fixture') || npmFixture);
}
