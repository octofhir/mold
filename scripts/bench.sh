#!/usr/bin/env bash
# Reproducible formatter benchmark: banshee vs pgFormatter vs sqlfluff.
#
# Generates a Postgres SQL corpus, then times each tool formatting it, including
# process startup (what a user/CI actually pays per file). Prints a Markdown row
# per tool: median wall time over N runs and relative speed vs banshee.
#
#   scripts/bench.sh [RUNS] [STATEMENTS]
#
# Requires: a release banshee on PATH or at target/release/banshee, plus any of
# pg_format / sqlfluff that are installed (missing tools are skipped).
set -euo pipefail

RUNS="${1:-15}"
STATEMENTS="${2:-400}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BANSHEE="${BANSHEE:-$ROOT/target/release/banshee}"
command -v "$BANSHEE" >/dev/null 2>&1 || BANSHEE="$(command -v banshee || true)"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
CORPUS="$WORK/corpus.sql"

# --- generate corpus -------------------------------------------------------
: > "$CORPUS"
i=0
while [ "$i" -lt "$STATEMENTS" ]; do
  cat >> "$CORPUS" <<SQL
select u.id, u.name, count(o.id) as orders from users u left join orders o on o.user_id = u.id where u.active = true and u.created_at > now() - interval '30 days' group by u.id, u.name having count(o.id) > $i order by orders desc limit 50;
with recent as (select id, total from orders where created_at > now() - interval '7 days') select r.id, r.total, p.name from recent r join products p on p.id = r.id where r.total between 10 and 1000;
update accounts set balance = balance - 100, updated_at = now() where id = $i and balance >= 100;
insert into audit (account_id, action, at) values ($i, 'debit', now());
select coalesce(sum(amount), 0) as total, date_trunc('day', created_at) as day from payments where status = 'settled' group by day order by day;
SQL
  i=$((i + 5))
done
lines=$(wc -l < "$CORPUS"); bytes=$(wc -c < "$CORPUS")
echo "corpus: $lines statements, $bytes bytes, $RUNS runs each" >&2

# --- timing harness (python3 for a portable high-res clock) ----------------
timeit() { # name  cmd...
  local name="$1"; shift
  [ -n "${1:-}" ] && command -v "${1%% *}" >/dev/null 2>&1 || { command -v "$1" >/dev/null 2>&1 || [ -x "$1" ]; } || { echo "skip $name (not installed)" >&2; return; }
  CORPUS="$CORPUS" RUNS="$RUNS" NAME="$name" python3 - "$@" <<'PY'
import os, sys, subprocess, time, statistics
runs = int(os.environ["RUNS"]); corpus = os.environ["CORPUS"]; name = os.environ["NAME"]
cmd = [a.replace("@CORPUS@", corpus) for a in sys.argv[1:]]
ts = []
for _ in range(runs):
    t = time.perf_counter()
    subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)
    ts.append((time.perf_counter() - t) * 1000)
med = statistics.median(ts)
print(f"{name}\t{med:.1f}")
PY
}

echo "=== results (median ms over $RUNS runs) ===" >&2
{
  timeit banshee     "$BANSHEE" format @CORPUS@
  timeit pgFormatter pg_format @CORPUS@
  timeit sqlfluff    sqlfluff format --dialect postgres @CORPUS@
} | sort -t$'\t' -k2 -n | awk -F'\t' '
  NR==1 {base=$2}
  {printf "| %-11s | %8.1f | %5.1fx |\n", $1, $2, $2/base}
  END {}
' | (echo "| tool | median ms | vs fastest |"; echo "|------|-----------|------------|"; cat)
