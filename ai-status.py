#!/usr/bin/env python3
"""Read agent quota/status without starting a conversation or refreshing login.

Standard library only. Providers run independently with a six-second overall
deadline. Codex uses the latest local quota snapshot (not a live account check).
"""

import datetime as dt
import json
import os
from pathlib import Path
import queue
import shutil
import subprocess
import threading
import time
import urllib.error
import urllib.request

TIMEOUT = 2


def read_json(path):
    with open(path) as stream:
        return json.load(stream)


def stamp(value):
    """Display a reset/snapshot timestamp in the local timezone."""
    if not value:
        return "unknown"
    if isinstance(value, (int, float)):
        value = dt.datetime.fromtimestamp(value, dt.timezone.utc)
    else:
        value = dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
    return value.astimezone().strftime("%b %d %H:%M %Z")


def claude():
    root = Path(os.getenv("CLAUDE_CONFIG_DIR", Path.home() / ".claude"))
    status = "not logged in (no local OAuth credentials)"
    try:
        oauth = read_json(root / ".credentials.json").get("claudeAiOauth", {})
        if oauth.get("accessToken"):
            # Same read-only endpoint used by Claude Code's /usage command.
            request = urllib.request.Request(
                "https://api.anthropic.com/api/oauth/usage",
                headers={"Authorization": "Bearer " + oauth["accessToken"],
                         "anthropic-beta": "oauth-2025-04-20"})
            with urllib.request.urlopen(request, timeout=TIMEOUT) as response:
                usage = json.load(response)
            rows = []
            for key, label in (("five_hour", "5h"), ("seven_day", "weekly"),
                               ("seven_day_sonnet", "weekly Sonnet"),
                               ("seven_day_opus", "weekly Opus")):
                window = usage.get(key)
                if window and isinstance(window.get("utilization"), (int, float)):
                    rows.append(f"{label}: {window['utilization']:g}% used; "
                                f"resets {stamp(window.get('resets_at'))}")
            if rows:
                return ["live quota"] + rows
            status = "reachable; quota not reported"
    except urllib.error.HTTPError as error:
        status = f"usage unavailable (HTTP {error.code})"
    except FileNotFoundError:
        pass  # Missing credentials means this is not a local OAuth login.
    except (urllib.error.URLError, TimeoutError, OSError):
        status = "usage unreachable or local credentials unavailable"
    except (ValueError, TypeError, KeyError):
        status = "usage data unavailable"
    # The stats cache is activity, never a substitute for remaining quota.
    try:
        stats = read_json(root / "stats-cache.json")
        return [status, f"cached activity through {stats.get('lastComputedDate', '?')}: "
                f"{stats.get('totalSessions', '?')} sessions, "
                f"{stats.get('totalMessages', '?')} messages (all time)"]
    except (OSError, ValueError, TypeError):
        return [status]


def codex():
    root = Path(os.getenv("CODEX_HOME", Path.home() / ".codex")) / "sessions"
    # Bound the search to recent date directories and file tails; never scan
    # whole transcripts. Older snapshots are explicitly dated in the popup.
    days = sorted(root.glob("*/*/*"), reverse=True)[:3]
    files = [path for day in days for path in day.glob("*.jsonl")]
    files.sort(key=lambda path: path.stat().st_mtime, reverse=True)
    for path in files[:8]:
        with path.open("rb") as stream:
            size = path.stat().st_size
            stream.seek(max(0, size - 262144))
            lines = stream.read().splitlines()
        for line in reversed(lines):
            try:
                event = json.loads(line)
            except ValueError:
                continue  # A partial first/last line is normal in a live file.
            payload = event.get("payload") or {}
            if event.get("type") != "event_msg" or payload.get("type") != "token_count":
                continue
            limits = payload.get("rate_limits") or {}
            rows = []
            for key in ("primary", "secondary"):
                window = limits.get(key)
                if window and isinstance(window.get("used_percent"), (int, float)):
                    minutes = window.get("window_minutes")
                    label = {300: "5h", 10080: "weekly"}.get(minutes, f"{minutes}m")
                    rows.append(f"{label}: {window['used_percent']:g}% used; "
                                f"resets {stamp(window.get('resets_at'))}")
            if rows:
                return [f"cached quota as of {stamp(event.get('timestamp'))}"] + rows
    return ["quota unavailable; run /status in a Codex session to update its snapshot"]


def antigravity():
    """Read the built-in /usage command without starting an agent turn.

    https://antigravity.google/docs/cli/commands/usage
    Print mode returns structured quota groups alongside human-readable text.
    """
    result = subprocess.run(["agy", "--log-file", os.devnull,
                             "-p", "/usage", "--output-format", "json"],
                            capture_output=True, text=True, timeout=5)
    if result.returncode:
        return ["usage unavailable (agy /usage failed)"]
    data = json.loads(result.stdout)
    command = data.get("command") or {}
    if data.get("status") != "SUCCESS" or command.get("name") != "usage":
        return ["usage unavailable (unexpected agy response)"]
    rows = []
    for group in command.get("data", {}).get("groups", []):
        for bucket in group.get("buckets", []):
            remaining = bucket.get("remaining_fraction")
            if isinstance(remaining, (int, float)) and 0 <= remaining <= 1:
                rows.append(f"{group['name']} ({bucket['window']}): "
                            f"{100 * (1 - remaining):.1f}% used; "
                            f"resets {stamp(bucket.get('reset_time'))}")
    return ["live quota"] + rows if rows else ["reachable; quota not reported"]


def main():
    providers = (("Claude", "claude", claude), ("Codex", "codex", codex),
                 ("Antigravity", "agy", antigravity))
    results = queue.Queue()

    def probe(name, executable, function):
        try:
            rows = function() if shutil.which(executable) else ["not installed / not on PATH"]
        except subprocess.TimeoutExpired:
            rows = ["unreachable (timed out)"]
        except Exception:
            # Never print raw exceptions: command/HTTP errors can carry secrets.
            rows = ["status unavailable (local data or service error)"]
        results.put((name, rows))

    print("LLM agent status\n")
    for name, executable, function in providers:
        threading.Thread(target=probe, args=(name, executable, function), daemon=True).start()
    deadline = time.monotonic() + 6
    pending = {name for name, _, _ in providers}
    while pending:
        try:
            name, rows = results.get(timeout=max(0, deadline - time.monotonic()))
        except queue.Empty:
            break
        pending.remove(name)
        print(name + "\n  " + "\n  ".join(rows) + "\n", flush=True)
    for name in sorted(pending):
        print(name + "\n  unreachable (6s deadline)\n")
    print("g: refresh    q: close    Percentages show quota used.")


if __name__ == "__main__":
    main()
