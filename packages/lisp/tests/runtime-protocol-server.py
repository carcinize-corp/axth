#!/usr/bin/env python3
"""A runtime protocol worker, for testing the Lisp PROCESS-RUNTIME client.

The runtime protocol is language-agnostic on purpose: Ax ships protocol
servers for QuickJS and Pyodide, and the Lisp client's job is to drive any of
them correctly. This worker stands in for one, and its behaviour is the same
as the Python port's fixture server (packages/python/axllm/conformance.py), so
the ir/conformance/axagent/runtime-protocol-*.json fixtures mean the same
thing on both sides.

AXIR_RUNTIME_PROTOCOL_FIXTURE_MODE selects a failure to rehearse:

  normal            answer every op correctly
  unavailable       report inspect/snapshot/patch unavailable, and refuse them
  id_mismatch       answer with the wrong request id
  session_mismatch  answer an execute with the wrong session id
  malformed_json    write a line that is not JSON
  eof               close the pipe without answering
  nonzero           write to standard error and exit 7
  stall             never answer, so the client's own timeout must fire
"""

import copy
import json
import os
import sys
import time

TAG = "com.example.tag"


def respond(message, result=None, *, session_id=None, mode="normal"):
    out = {"id": "mismatch" if mode == "id_mismatch" else message.get("id"), "ok": True}
    out["result"] = {} if result is None else result
    if session_id is not None:
        out["session_id"] = session_id
    return out


def fail(message, category, text, *, mode="normal"):
    return {
        "id": "mismatch" if mode == "id_mismatch" else message.get("id"),
        "ok": False,
        "error": {"category": category, "message": text},
    }


def snapshot(session):
    bindings = copy.deepcopy(session.get("globals") or {})
    return {
        "version": 1,
        "entries": [
            {"name": key, "type": type(value).__name__, "preview": str(value)}
            for key, value in bindings.items()
        ],
        "bindings": bindings,
        "globals": copy.deepcopy(bindings),
        "closed": bool(session.get("closed")),
    }


def main() -> int:
    mode = os.environ.get("AXIR_RUNTIME_PROTOCOL_FIXTURE_MODE", "normal")
    sessions: dict[str, dict] = {}
    next_session = 0

    for line in sys.stdin:
        if mode == "eof":
            return 0
        if mode == "stall":
            # Answer nothing, ever. The client must give up on its own and
            # clean this process up rather than waiting for a reply.
            while True:
                time.sleep(3600)
        if mode == "malformed_json":
            print("{not-json", flush=True)
            return 0
        if mode == "nonzero":
            print("fixture stderr before nonzero exit", file=sys.stderr, flush=True)
            return 7

        try:
            message = json.loads(line)
        except json.JSONDecodeError as exc:
            print(json.dumps(fail({"id": None}, "protocol", str(exc))), flush=True)
            continue

        op = message.get("op")
        payload = message.get("payload") if isinstance(message.get("payload"), dict) else {}
        session_id = message.get("session_id")
        session = sessions.get(session_id or "")

        if op == "capabilities":
            response = respond(message, {
                "language": "JavaScript",
                "usage_instructions": "fixture protocol runtime",
                "inspect": mode != "unavailable",
                "snapshot": mode != "unavailable",
                "patch": mode != "unavailable",
                "abort": True,
            }, mode=mode)
        elif op == "create_session":
            next_session += 1
            new_id = f"s{next_session}"
            globals_ = copy.deepcopy(payload.get("globals") if isinstance(payload.get("globals"), dict) else {})
            globals_["__create_options"] = copy.deepcopy(
                payload.get("options") if isinstance(payload.get("options"), dict) else {}
            )
            sessions[new_id] = {"globals": globals_, "closed": False}
            response = respond(message, {"session_id": new_id}, session_id=new_id, mode=mode)
        elif op == "execute":
            if not session or session.get("closed"):
                response = fail(message, "session_closed", "session closed or unknown", mode=mode)
            else:
                code = str(payload.get("code") or "")
                session["globals"]["__last_execute_options"] = copy.deepcopy(
                    payload.get("options") if isinstance(payload.get("options"), dict) else {}
                )
                if code == "timeout()":
                    response = fail(message, "timeout", "fixture timeout", mode=mode)
                elif code == "sessionClosed()":
                    response = fail(message, "session_closed", "fixture session closed", mode=mode)
                elif code == "abort()":
                    response = fail(message, "abort", "fixture abort", mode=mode)
                elif code == "userError()":
                    response = fail(message, "user_error", "fixture user error", mode=mode)
                elif code == "stall()":
                    # Answer this one op never, so the client's own request
                    # timeout is the only thing that can end the wait.
                    while True:
                        time.sleep(3600)
                elif code.startswith("count()"):
                    # A counter in the session's own globals, so a test can
                    # tell a reused session from a fresh one.
                    session["globals"]["counter"] = int(session["globals"].get("counter", 0)) + 1
                    response = respond(message, {"type": "final", "args": [{"counter": session["globals"]["counter"]}]},
                                       session_id=session_id, mode=mode)
                elif code.startswith("echo "):
                    response = respond(message, {"type": "final", "args": [{"echo": code[len("echo "):]}]},
                                       session_id=session_id, mode=mode)
                else:
                    session["globals"]["answer"] = "fixture"
                    response = respond(message, {"type": "final", "args": [{"answer": "fixture"}]},
                                       session_id=session_id, mode=mode)
                if mode == "session_mismatch" and response.get("ok"):
                    response["session_id"] = "wrong-session"
        elif op == "inspect_globals":
            if mode == "unavailable":
                response = fail(message, "unavailable", "inspectGlobals unavailable", mode=mode)
            else:
                response = respond(message, copy.deepcopy((session or {}).get("globals") or {}),
                                   session_id=session_id, mode=mode)
        elif op == "snapshot_globals":
            if mode == "unavailable":
                response = fail(message, "unavailable", "snapshotGlobals unavailable", mode=mode)
            else:
                response = respond(message, snapshot(session or {}), session_id=session_id, mode=mode)
        elif op == "patch_globals":
            if mode == "unavailable":
                response = fail(message, "unavailable", "patchGlobals unavailable", mode=mode)
            else:
                raw = payload.get("globals") if isinstance(payload.get("globals"), dict) else {}
                bindings = raw.get("bindings") if isinstance(raw.get("bindings"), dict) else raw
                if session is not None:
                    session["globals"] = copy.deepcopy(bindings)
                response = respond(message, snapshot(session or {}), session_id=session_id, mode=mode)
        elif op == "close":
            if session is not None:
                session["closed"] = True
            response = respond(message, {"closed": True}, session_id=session_id, mode=mode)
        elif op == "shutdown":
            print(json.dumps(respond(message, {"shutdown": True}, mode=mode)), flush=True)
            return 0
        else:
            response = fail(message, "protocol", f"unknown runtime protocol op: {op}", mode=mode)

        print(json.dumps(response, separators=(",", ":")), flush=True)

    return 0


if __name__ == "__main__":
    sys.exit(main())
