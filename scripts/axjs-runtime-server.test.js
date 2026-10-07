import { spawn } from 'node:child_process';
import { createInterface } from 'node:readline';
import { expect, it } from 'vitest';

it('captures console output and preserves terminal completion across logs and later executions', async () => {
  await withAdapter(async ({ send, read }) => {
    send({ id: 'create', op: 'create_session' });
    const { session_id } = await read();
    for (const [code, expected] of [
      [
        "const kept = 41; console.log('probe', kept + 1)",
        {
          output: 'probe 42',
          analysis: { producedVars: ['kept'], callables: ['console.log'] },
        },
      ],
      [
        "console.log('before'); final('task', {answer:42}); console.log('after')",
        { type: 'final', args: ['task', { answer: 42 }] },
      ],
      [
        "respond('direct', {answer:17})",
        { type: 'respond', args: ['direct', { answer: 17 }] },
      ],
      ["console.log('next')", { kind: 'result', output: 'next' }],
      [
        "await Promise.resolve(); final('async', {})",
        { type: 'final', args: ['async', {}] },
      ],
      [
        "console.log('working'); reportSuccess('done'); console.log('logged')",
        { kind: 'status', status: { type: 'success', message: 'done' } },
      ],
    ]) {
      send({ id: code, op: 'execute', session_id, payload: { code } });
      expect(await read()).toMatchObject({
        id: code,
        ok: true,
        result: expected,
      });
    }
    send({ id: 'stop', op: 'shutdown' });
    expect(await read()).toMatchObject({ id: 'stop', ok: true });
  });
}, 15000);

it('patches snapshot bindings without leaking metadata and keeps evidence by reference across phases', async () => {
  await withAdapter(async ({ send, read }) => {
    send({
      id: 'create',
      op: 'create_session',
      payload: { globals: { inputs: {} } },
    });
    const { session_id } = await read();
    send({
      id: 'distill',
      op: 'execute',
      session_id,
      payload: { code: "const evidence = {count: 2}; final('task', evidence)" },
    });
    expect(await read()).toMatchObject({
      id: 'distill',
      result: { type: 'final' },
    });
    send({
      id: 'patch',
      op: 'patch_globals',
      session_id,
      payload: {
        globals: {
          version: 1,
          entries: [],
          closed: false,
          merge: true,
          bindings: { inputs: { executorRequest: '' } },
        },
      },
    });
    expect(await read()).toMatchObject({ id: 'patch', ok: true });
    send({
      id: 'executor',
      op: 'execute',
      session_id,
      payload: {
        code: 'evidence.count = 7; final({same: inputs.distilledContext === evidence, count: distilledContext.count, metadata: typeof bindings})',
      },
    });
    expect(await read()).toMatchObject({
      id: 'executor',
      result: {
        type: 'final',
        args: [{ same: true, count: 7, metadata: 'undefined' }],
      },
    });
    send({
      id: 'complete',
      op: 'execute',
      session_id,
      payload: { code: "final('done', {unrelated: true})" },
    });
    expect(await read()).toMatchObject({
      id: 'complete',
      result: { type: 'final' },
    });
    send({
      id: 'snapshot',
      op: 'snapshot_globals',
      session_id,
      payload: { reservedNames: ['inputs', 'distilledContext'] },
    });
    const snapshot = await read();
    expect(snapshot.result.bindings.distilledContext).toEqual({ count: 7 });
    expect(snapshot.result.bindings).not.toHaveProperty('inputs');
    send({ id: 'stop', op: 'shutdown' });
    expect(await read()).toMatchObject({ id: 'stop', ok: true });
  });
}, 15000);

async function withAdapter(run) {
  const child = spawn(
    process.execPath,
    ['--import=tsx', 'tools/axir/adapters/axjs-runtime-server.ts'],
    { stdio: ['pipe', 'pipe', 'pipe'] }
  );
  let stderr = '';
  child.stderr.on('data', (chunk) => {
    stderr += chunk;
  });
  const exited = new Promise((resolve) => {
    child.on('exit', (code, signal) => resolve({ code, signal }));
  });
  const lines = createInterface({ input: child.stdout });
  const iterator = lines[Symbol.asyncIterator]();
  async function bounded(promise) {
    let timer;
    try {
      return await Promise.race([
        promise,
        new Promise((_, reject) => {
          timer = setTimeout(
            () => reject(new Error(`adapter stalled: ${stderr}`)),
            5000
          );
        }),
      ]);
    } finally {
      clearTimeout(timer);
    }
  }
  const send = (message) => child.stdin.write(`${JSON.stringify(message)}\n`);
  const read = async () => {
    const line = await bounded(iterator.next());
    if (line.done) throw new Error(`adapter ended before replying: ${stderr}`);
    return JSON.parse(line.value);
  };
  try {
    await run({ send, read, end: () => child.stdin.end() });
    child.stdin.end();
    expect(await bounded(exited), stderr).toEqual({ code: 0, signal: null });
  } finally {
    lines.close();
    if (child.exitCode === null) child.kill('SIGKILL');
  }
}

it('serves correlated host calls over real stdin without blocking replies or reordering requests', async () => {
  await withAdapter(async ({ send, read }) => {
    // Pipelined ordinary requests retain their original serial ordering.
    send({ id: 'caps', op: 'capabilities' });
    send({
      id: 'create',
      op: 'create_session',
      payload: {
        host_calls: ['llmQuery', 'crm.lookup'],
      },
    });
    expect(await read()).toMatchObject({
      id: 'caps',
      result: { host_calls: true, abort: false },
    });
    const created = await read();
    expect(created).toMatchObject({ id: 'create', ok: true });
    const session_id = created.session_id;
    send({
      id: 'execute-1',
      op: 'execute',
      session_id,
      payload: {
        code: 'const [a,b] = await Promise.all([llmQuery({text:"why"}), crm.lookup({key:"x"})]); await final({answer:a.answer+" "+b.value})',
      },
    });
    const frames = [await read(), await read()];
    expect(frames.map((frame) => frame.name).sort()).toEqual([
      'crm.lookup',
      'llmQuery',
    ]);
    expect(new Set(frames.map((frame) => frame.callback_id)).size).toBe(2);
    for (const frame of frames.reverse()) {
      expect(frame).toMatchObject({
        op: 'host_call',
        request_id: 'execute-1',
        session_id,
      });
      expect(frame.params).toEqual(
        frame.name === 'llmQuery' ? { text: 'why' } : { key: 'x' }
      );
      send({
        id: frame.callback_id,
        ok: true,
        result:
          frame.name === 'llmQuery' ? { answer: 'because' } : { value: 17 },
      });
    }
    expect(await read()).toMatchObject({
      id: 'execute-1',
      ok: true,
      result: {
        type: 'final',
        args: [{ answer: 'because 17' }],
      },
    });

    // A stale callback reply cannot supply the next execute's answer.
    send({ id: frames[0].callback_id, ok: true, result: { answer: 'stale' } });
    send({
      id: 'execute-2',
      op: 'execute',
      session_id,
      payload: {
        code: 'try { await crm.lookup({key:"denied"}); } catch (error) { return await final({error:error.message}); }',
      },
    });
    const failed = await read();
    expect(failed).toMatchObject({
      op: 'host_call',
      request_id: 'execute-2',
      name: 'crm.lookup',
    });
    send({
      id: failed.callback_id,
      ok: false,
      error: { category: 'runtime', message: 'denied exactly' },
    });
    expect(await read()).toMatchObject({
      id: 'execute-2',
      result: {
        type: 'final',
        args: [{ error: 'denied exactly' }],
      },
    });
    send({ id: 'close', op: 'close', session_id });
    expect(await read()).toMatchObject({ id: 'close', ok: true });
    send({
      id: 'closed',
      op: 'execute',
      session_id,
      payload: { code: 'await crm.lookup({})' },
    });
    expect(await read()).toMatchObject({
      id: 'closed',
      ok: false,
      error: { category: 'session_closed' },
    });
    send({ id: 'stop', op: 'shutdown' });
    expect(await read()).toMatchObject({ id: 'stop', ok: true });
  });
}, 15000);

it('refuses unsafe callback namespaces and closes a pending callback on EOF', async () => {
  await withAdapter(async ({ send, read, end }) => {
    for (const name of [
      '__proto__.polluted',
      'crm.constructor.call',
      'final',
    ]) {
      send({ id: name, op: 'create_session', payload: { host_calls: [name] } });
      expect(await read()).toMatchObject({ id: name, ok: false });
    }
    send({
      id: 'create',
      op: 'create_session',
      payload: { host_calls: ['lookup'] },
    });
    const created = await read();
    expect(created.ok).toBe(true);
    send({
      id: 'pending',
      op: 'execute',
      session_id: created.session_id,
      payload: { code: 'await lookup({})' },
    });
    expect(await read()).toMatchObject({
      op: 'host_call',
      request_id: 'pending',
    });
    end();
    const result = await read();
    expect(result).toMatchObject({
      id: 'pending',
      ok: false,
      error: { category: 'runtime', message: 'host input closed' },
    });
  });
}, 15000);

it('does not queue shutdown behind an unanswered host call', async () => {
  await withAdapter(async ({ send, read }) => {
    send({
      id: 'create',
      op: 'create_session',
      payload: { host_calls: ['lookup'] },
    });
    const created = await read();
    expect(created.ok).toBe(true);
    send({
      id: 'pending',
      op: 'execute',
      session_id: created.session_id,
      payload: { code: 'await lookup({})' },
    });
    expect(await read()).toMatchObject({
      op: 'host_call',
      request_id: 'pending',
    });
    send({ id: 'stop', op: 'shutdown' });
    expect(await read()).toMatchObject({
      id: 'pending',
      ok: false,
      error: { message: 'runtime shut down' },
    });
    expect(await read()).toMatchObject({ id: 'stop', ok: true });
  });
}, 15000);
