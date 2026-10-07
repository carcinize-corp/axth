// A real JavaScript runtime protocol worker, for testing that the Lisp
// PROCESS-RUNTIME client drives an actual engine and not only a fixture.
//
// Ax ships QuickJS and Pyodide protocol servers; the Lisp side is the client,
// so what has to be proved here is that a genuine engine behind the pipe
// works: a session whose globals survive between steps, the actor primitives
// that produce a completion, and snapshot/patch as pause and resume.
//
// Node's `vm` module supplies the engine. This is a test worker, so it is
// deliberately plain: one context per session, no sandbox hardening, and no
// claim to be a secure runtime. Nothing in packages/lisp depends on it.

import { createContext, runInContext } from 'node:vm';
import { createInterface } from 'node:readline';

const PRELUDE = `
  function axComplete(value) { globalThis.__ax_completion = value; return value; }
  function final(...args) { return axComplete({ type: 'final', args }); }
  function respond(...args) { return axComplete({ type: 'respond', args }); }
  function askClarification(...args) { return axComplete({ type: 'askClarification', args }); }
  function discover(request) { return axComplete({ kind: 'discover', discover: request }); }
  function recall(request) { return axComplete({ kind: 'recall', recall: request }); }
  function used(id, reason) {
    const payload = (id && typeof id === 'object') ? { ...id } : { id };
    if (reason !== undefined && reason !== null) payload.reason = String(reason);
    return axComplete({ kind: 'used', used: payload });
  }
  function reportSuccess(message) {
    return axComplete({ kind: 'status', status: { type: 'success', message: String(message ?? '') } });
  }
  function reportFailure(message) {
    return axComplete({ kind: 'status', status: { type: 'failed', message: String(message ?? '') } });
  }
  function guideAgent(guidance) {
    return axComplete({ type: 'guide_agent', guidance: String(guidance ?? '') });
  }
  globalThis.__ax_logs = [];
  function __ax_render(value) {
    if (typeof value === 'string') return value;
    try { return JSON.stringify(value); } catch { return String(value); }
  }
  function __ax_record(...args) {
    globalThis.__ax_logs.push(args.map(__ax_render).join(' '));
  }
  // A real engine's console output is evidence the actor can act on: the
  // step that logged it is over by the time the agent reads it, so it has
  // to come back with the step rather than go to this process's stdout,
  // which is the protocol pipe.
  globalThis.console = {
    log: __ax_record, info: __ax_record, warn: __ax_record,
    error: __ax_record, debug: __ax_record,
  };
  globalThis.__ax_reserved = Object.create(null);
  for (const name of Object.getOwnPropertyNames(globalThis)) globalThis.__ax_reserved[name] = 1;
`;

const sessions = new Map();
let nextSession = 0;

// Callbacks awaiting the host's answer, by callback id.
const pendingCallbacks = new Map();
let nextCallback = 0;

function hostCall(session, name, params) {
  // The frame names the request it interrupts and the session it belongs to,
  // so the host can refuse one that arrives out of band or after the run that
  // registered the name is over. Without that correlation a worker could ask
  // the host to run anything at any time.
  if (!session.request) {
    return Promise.reject(new Error(`host call ${name} outside an execute`));
  }
  nextCallback += 1;
  const callbackId = `cb${nextCallback}`;
  const frame = {
    op: 'host_call',
    callback_id: callbackId,
    request_id: session.request.id,
    session_id: session.request.sessionId,
    name,
    params,
  };
  return new Promise((resolve, reject) => {
    pendingCallbacks.set(callbackId, { resolve, reject });
    process.stdout.write(`${JSON.stringify(frame)}\n`);
  });
}

function bindings(session) {
  const out = {};
  for (const name of Object.getOwnPropertyNames(session.context)) {
    if (name.startsWith('__ax_') || session.reserved[name]) continue;
    const value = session.context[name];
    if (typeof value === 'function' || typeof value === 'undefined') continue;
    try {
      JSON.parse(JSON.stringify(value));
      out[name] = value;
    } catch {
      // A value that cannot cross the pipe is not part of the snapshot.
    }
  }
  return out;
}

function snapshot(session) {
  const values = bindings(session);
  return {
    version: 1,
    entries: Object.entries(values).map(([name, value]) => ({
      name,
      type: Array.isArray(value) ? 'array' : typeof value,
      preview: JSON.stringify(value),
    })),
    bindings: values,
    globals: values,
    closed: session.closed,
  };
}

function reply(message, result, sessionId) {
  const out = { id: message.id, ok: true, result: result ?? {} };
  if (sessionId !== undefined) out.session_id = sessionId;
  return out;
}

function fail(message, category, text) {
  return { id: message?.id ?? null, ok: false, error: { category, message: text } };
}

async function handle(message) {
  const payload = (message.payload && typeof message.payload === 'object') ? message.payload : {};
  const session = sessions.get(message.session_id ?? '');

  switch (message.op) {
    case 'capabilities':
      return reply(message, {
        language: 'JavaScript',
        usage_instructions: 'State is session-scoped: top-level assignments persist across steps.',
        inspect: true,
        snapshot: true,
        patch: true,
        abort: false,
        // Opt in to the callback extension. A server that omits this keeps the
        // old contract, where the host registers nothing and the worker owns
        // whatever callables it has.
        host_calls: true,
      });

    case 'create_session': {
      nextSession += 1;
      const id = `js${nextSession}`;
      const context = createContext({});
      runInContext(PRELUDE, context);
      const reserved = { ...context.__ax_reserved };
      for (const [name, value] of Object.entries(payload.globals ?? {})) {
        context[name] = value;
        reserved[name] = 1;
      }
      const session = { context, reserved, closed: false, request: null };
      // Each name the host registered becomes an async function that asks the
      // host to run it and waits for the answer. A dotted name becomes a
      // nested object, so crm.lookup reads in the code exactly as it reads in
      // the registry.
      for (const name of payload.host_calls ?? []) {
        const call = async (params) => hostCall(session, String(name), params ?? {});
        const parts = String(name).split('.');
        let target = context;
        for (let i = 0; i < parts.length - 1; i += 1) {
          if (typeof target[parts[i]] !== 'object' || target[parts[i]] === null) {
            target[parts[i]] = {};
          }
          target = target[parts[i]];
        }
        target[parts[parts.length - 1]] = call;
        reserved[parts[0]] = 1;
      }
      sessions.set(id, session);
      return reply(message, { session_id: id }, id);
    }

    case 'execute': {
      if (!session || session.closed) return fail(message, 'session_closed', 'session closed or unknown');
      session.context.__ax_completion = undefined;
      // Each step reports only its own output: carrying the previous step's
      // logs forward would show the agent a probe it already acted on.
      session.context.__ax_logs = [];
      const code = String(payload.code ?? '');
      session.request = { id: message.id, sessionId: message.session_id ?? '' };
      let value;
      try {
        value = runInContext(code, session.context);
      } catch (error) {
        // A model writes `await final(...)` freely, and top-level await is a
        // syntax error to a plain script. Re-running it as an async body is
        // what a real engine does for a module, so the retry is the normal
        // path for this code rather than a fallback for a broken one. Bare
        // assignments still reach the session's globals, which is what the
        // actor relies on between steps.
        // The error comes from the vm's own realm, so it is not an instance of
        // this realm's SyntaxError; its name and message are what identify it.
        const text = String(error?.message ?? error);
        const syntax = (error?.name ?? error?.constructor?.name) === 'SyntaxError';
        if (syntax && /await is only valid|Unexpected reserved word/.test(text)) {
          try {
            value = await runInContext(`(async () => {\n${code}\n})()`, session.context);
          } catch (inner) {
            session.request = null;
            return fail(message, 'runtime', String(inner?.message ?? inner));
          }
        } else {
          session.request = null;
          return fail(message, 'runtime', text);
        }
      }
      if (value && typeof value.then === 'function') {
        try {
          value = await value;
        } catch (inner) {
          session.request = null;
          return fail(message, 'runtime', String(inner?.message ?? inner));
        }
      }
      session.request = null;
      const logs = session.context.__ax_logs.slice();
      const completion = session.context.__ax_completion;
      if (completion !== undefined) {
        const out = (completion && typeof completion === 'object' && !Array.isArray(completion))
          ? { ...completion }
          : { kind: 'result', result: completion ?? null };
        if (logs.length) out.logs = logs;
        return reply(message, out, message.session_id);
      }
      const out = { kind: 'result', result: value ?? null };
      if (logs.length) out.logs = logs;
      return reply(message, out, message.session_id);
    }

    case 'inspect_globals':
      if (!session) return fail(message, 'session_closed', 'session closed or unknown');
      return reply(message, bindings(session), message.session_id);

    case 'snapshot_globals':
      if (!session) return fail(message, 'session_closed', 'session closed or unknown');
      return reply(message, snapshot(session), message.session_id);

    case 'patch_globals': {
      if (!session) return fail(message, 'session_closed', 'session closed or unknown');
      const raw = (payload.globals && typeof payload.globals === 'object') ? payload.globals : {};
      const values = (raw.bindings && typeof raw.bindings === 'object') ? raw.bindings : raw;
      for (const [name, value] of Object.entries(values)) session.context[name] = value;
      return reply(message, snapshot(session), message.session_id);
    }

    case 'close':
      if (session) session.closed = true;
      return reply(message, { closed: true }, message.session_id);

    case 'shutdown':
      return reply(message, { shutdown: true });

    default:
      return fail(message, 'protocol', `unknown runtime protocol op: ${message.op}`);
  }
}

const input = createInterface({ input: process.stdin });
for await (const line of input) {
  if (!line.trim()) continue;
  let message;
  try {
    message = JSON.parse(line);
  } catch (error) {
    process.stdout.write(`${JSON.stringify(fail(null, 'protocol', String(error?.message ?? error)))}\n`);
    continue;
  }
  const pending = message && message.id !== undefined
    ? pendingCallbacks.get(String(message.id))
    : undefined;
  if (pending) {
    // A reply to one of our own host calls, not a new request.
    pendingCallbacks.delete(String(message.id));
    if (message.ok === false) {
      pending.reject(new Error(message.error?.message ?? 'host call failed'));
    } else {
      pending.resolve(message.result ?? null);
    }
    continue;
  }
  // Not awaited: an execute that calls back into the host needs this loop to
  // keep reading so the reply can arrive. The host sends one request at a
  // time, so responses cannot interleave.
  void handle(message).then((response) => {
    process.stdout.write(`${JSON.stringify(response)}\n`);
    if (message.op === 'shutdown') process.exit(0);
  });
}
