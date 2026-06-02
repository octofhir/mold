//! `mold rules` — list the built-in lint rules. Also the shared rule catalog
//! consumed by `mold explain`.

use anyhow::Result;

use super::exit;

/// A built-in rule's catalog entry.
pub(crate) struct RuleDoc {
    pub code: &'static str,
    pub fixable: bool,
    pub summary: &'static str,
    /// Long-form explanation shown by `mold explain`.
    pub explanation: &'static str,
}

pub(crate) const RULES: &[RuleDoc] = &[
    RuleDoc {
        code: "AM04",
        fixable: true,
        summary: "Avoid SELECT *; list columns explicitly (fix needs schema)",
        explanation: "\
`SELECT *` makes a query's result shape depend on the table definition. When a
column is added, dropped or reordered, the query silently changes what it
returns, breaking downstream consumers and obscuring intent.

  bad:  SELECT * FROM patient;
  good: SELECT id, name FROM patient;",
    },
    RuleDoc {
        code: "AM05",
        fixable: false,
        summary: "Implicit cross join; use an explicit JOIN clause",
        explanation: "\
Comma-separated tables in FROM produce a cross join joined only by the WHERE
clause. A missing predicate then yields an accidental cartesian product. An
explicit JOIN ... ON states the relationship and fails loudly when it is
missing.

  bad:  SELECT * FROM patient, orders WHERE patient.id = orders.patient_id;
  good: SELECT * FROM patient JOIN orders ON orders.patient_id = patient.id;",
    },
    RuleDoc {
        code: "AM01",
        fixable: false,
        summary: "DISTINCT is redundant with GROUP BY",
        explanation: "\
`GROUP BY` already yields one row per group, so a leading `DISTINCT` only adds a
needless sort. Drop the `DISTINCT`. (`DISTINCT ON` is a different feature — see
ST08.)

  bad:  SELECT DISTINCT a FROM t GROUP BY a
  good: SELECT a FROM t GROUP BY a",
    },
    RuleDoc {
        code: "AM02",
        fixable: false,
        summary: "Set operators (UNION/EXCEPT/INTERSECT) should state ALL or DISTINCT",
        explanation: "\
`UNION` defaults to `UNION DISTINCT`, which silently deduplicates. Stating
`ALL` or `DISTINCT` makes the intent — and the cost — explicit. Set
`prefer = all|distinct` to require one specific modifier.",
    },
    RuleDoc {
        code: "AM09",
        fixable: false,
        summary: "LIMIT/OFFSET without ORDER BY is non-deterministic",
        explanation: "\
Without `ORDER BY`, `LIMIT`/`OFFSET` return an arbitrary subset of rows that can
change between runs. Add an `ORDER BY` to make the result deterministic.",
    },
    RuleDoc {
        code: "AM03",
        fixable: false,
        summary: "ORDER BY mixes explicit and implicit sort directions",
        explanation: "\
Once one `ORDER BY` term states `ASC`/`DESC`, omitting it on the others is
ambiguous — readers cannot tell whether the default was intended. State a
direction on every term or on none.

  bad:  ORDER BY a, b DESC
  good: ORDER BY a ASC, b DESC",
    },
    RuleDoc {
        code: "ST01",
        fixable: true,
        summary: "Redundant ELSE NULL in CASE",
        explanation: "\
A `CASE` expression already yields NULL when no branch matches, so `ELSE NULL`
is redundant. Removed automatically.

  bad:  CASE WHEN x THEN 1 ELSE NULL END
  good: CASE WHEN x THEN 1 END",
    },
    RuleDoc {
        code: "ST05",
        fixable: false,
        summary: "Subquery in FROM/JOIN; prefer a CTE",
        explanation: "\
A subquery embedded in `FROM` or `JOIN` is harder to read and reuse than the
same query factored into a `WITH` clause. Extract it into a CTE.",
    },
    RuleDoc {
        code: "AM07",
        fixable: false,
        summary: "Set-operation branches select different column counts",
        explanation: "\
`UNION`/`EXCEPT`/`INTERSECT` require every branch to project the same number of
columns; otherwise Postgres rejects the query. Branches containing `*` are
skipped because their width is not known without the schema.

  bad:  SELECT a, b FROM t UNION SELECT c FROM u
  good: SELECT a, b FROM t UNION SELECT c, d FROM u",
    },
    RuleDoc {
        code: "ST07",
        fixable: false,
        summary: "Avoid NATURAL JOIN",
        explanation: "\
`NATURAL JOIN` joins on every column the two tables happen to share by name, so
adding or renaming a column silently changes the join. State the columns with
`ON` or `USING`.

  bad:  FROM a NATURAL JOIN b
  good: FROM a JOIN b ON a.id = b.a_id",
    },
    RuleDoc {
        code: "ST08",
        fixable: false,
        summary: "DISTINCT ON without ORDER BY is non-deterministic",
        explanation: "\
`DISTINCT ON (...)` keeps one row per group, but which row survives is arbitrary
unless an `ORDER BY` pins it down. Add an `ORDER BY` covering the `DISTINCT ON`
expressions.

  bad:  SELECT DISTINCT ON (user_id) * FROM events
  good: SELECT DISTINCT ON (user_id) * FROM events ORDER BY user_id, ts DESC",
    },
    RuleDoc {
        code: "AL01",
        fixable: true,
        summary: "Table alias should be introduced with AS",
        explanation: "\
An implicit table alias (`users u`) is easy to misread as two separate tables.
Spelling it `users AS u` is unambiguous. Inserted automatically.

  bad:  SELECT u.id FROM users u
  good: SELECT u.id FROM users AS u",
    },
    RuleDoc {
        code: "AL02",
        fixable: true,
        summary: "Column alias should be introduced with AS",
        explanation: "\
An implicit column alias (`a b`) reads like a typo or a missing comma. Spelling
it `a AS b` is unambiguous. Inserted automatically.

  bad:  SELECT total t FROM s
  good: SELECT total AS t FROM s",
    },
    RuleDoc {
        code: "AL03",
        fixable: false,
        summary: "Complex select expression should be aliased",
        explanation: "\
A select target that is a function call, arithmetic expression, CASE, etc. has
no inherent name, so the resulting column gets an opaque auto-generated label.
Name it with `AS`.

  bad:  SELECT count(*) FROM t
  good: SELECT count(*) AS total FROM t",
    },
    RuleDoc {
        code: "AL04",
        fixable: false,
        summary: "Duplicate table alias in one FROM",
        explanation: "\
Two tables in the same `FROM` share an alias, so any qualified reference to it
is ambiguous. Give each table a distinct alias.

  bad:  FROM orders o JOIN order_items o ON ...
  good: FROM orders o JOIN order_items i ON ...",
    },
    RuleDoc {
        code: "AL05",
        fixable: false,
        summary: "Table alias declared but never used",
        explanation: "\
The query introduces a table alias that no column reference uses to qualify a
name. Drop the alias, or use it.",
    },
    RuleDoc {
        code: "AL08",
        fixable: false,
        summary: "Duplicate column alias in a SELECT list",
        explanation: "\
Two select items resolve to the same output name, so any consumer addressing
the column by name gets an ambiguous or surprising result. Give each a distinct
alias.

  bad:  SELECT a AS x, b AS x FROM t
  good: SELECT a AS x, b AS y FROM t",
    },
    RuleDoc {
        code: "RF03",
        fixable: false,
        summary: "Inconsistent column qualification in a single-table query",
        explanation: "\
Within a single-table query, either qualify every column with the table/alias
or qualify none. Mixing the two makes the code noisy and inconsistent.",
    },
    RuleDoc {
        code: "ST03",
        fixable: false,
        summary: "CTE is defined but never used",
        explanation: "\
A `WITH` clause declares a CTE that no later query references. It is dead code;
remove it or use it.",
    },
    RuleDoc {
        code: "SF01",
        fixable: false,
        summary: "UPDATE without WHERE affects all rows",
        explanation: "\
An UPDATE with no WHERE clause rewrites every row in the table. This is
occasionally intended but far more often a mistake; add a WHERE clause to scope
the change.

  risky: UPDATE patient SET active = false;
  safe:  UPDATE patient SET active = false WHERE id = 42;",
    },
    RuleDoc {
        code: "SF02",
        fixable: false,
        summary: "DELETE without WHERE affects all rows",
        explanation: "\
A DELETE with no WHERE clause empties the table. Add a WHERE clause unless you
truly intend to remove every row (in which case TRUNCATE is usually clearer).

  risky: DELETE FROM sessions;
  safe:  DELETE FROM sessions WHERE expires_at < now();",
    },
    RuleDoc {
        code: "SF03",
        fixable: false,
        summary: "INSERT without an explicit column list",
        explanation: "\
`INSERT INTO t VALUES (...)` binds values to columns positionally, so adding,
dropping or reordering a column silently corrupts the insert. List the target
columns. `INSERT ... DEFAULT VALUES` is exempt.

  risky: INSERT INTO patient VALUES (1, 'Ann');
  safe:  INSERT INTO patient (id, name) VALUES (1, 'Ann');",
    },
    RuleDoc {
        code: "JB01",
        fixable: true,
        summary: "Use ->> when comparing a JSONB value to text",
        explanation: "\
The -> operator returns `jsonb`, so comparing it to a text literal never
matches. Use ->> to extract the value as `text` for the comparison.

  bad:  WHERE resource->'name' = 'Ann'
  good: WHERE resource->>'name' = 'Ann'",
    },
    RuleDoc {
        code: "CV01",
        fixable: true,
        summary: "Use <> instead of != for inequality",
        explanation: "\
Both `<>` and `!=` mean inequality in Postgres; `<>` is the SQL standard
spelling. This rule rewrites `!=` to `<>` for consistency.",
    },
    RuleDoc {
        code: "CV04",
        fixable: true,
        summary: "Use count(*) instead of count(1)/count(0)",
        explanation: "\
`count(1)` and `count(0)` count rows exactly like `count(*)`, which states the
intent directly and is the idiomatic spelling. Rewrites the literal to `*`.

  bad:  SELECT count(1) FROM t
  good: SELECT count(*) FROM t",
    },
    RuleDoc {
        code: "CV05",
        fixable: true,
        summary: "Compare with NULL using IS NULL / IS NOT NULL",
        explanation: "\
`x = NULL` and `x <> NULL` are never true — NULL comparisons via `=`/`<>` always
yield NULL. Use `IS NULL` / `IS NOT NULL`. Fixed automatically when NULL is on
the right-hand side.

  bad:  WHERE deleted_at = NULL
  good: WHERE deleted_at IS NULL",
    },
    RuleDoc {
        code: "CV06",
        fixable: true,
        summary: "Statements should end with a semicolon",
        explanation: "\
A trailing semicolon terminates each statement, avoiding ambiguity when
statements are concatenated. Added automatically.",
    },
    RuleDoc {
        code: "CV08",
        fixable: false,
        summary: "Prefer LEFT JOIN over RIGHT JOIN",
        explanation: "\
A `RIGHT JOIN` can always be rewritten as a `LEFT JOIN` by swapping the joined
tables, which keeps the reading order (left-to-right) aligned with the join
direction and is easier to follow.

  bad:  FROM a RIGHT JOIN b ON a.id = b.a_id
  good: FROM b LEFT JOIN a ON a.id = b.a_id",
    },
    RuleDoc {
        code: "CV10",
        fixable: true,
        summary: "LIKE without a wildcard is just =",
        explanation: "\
A `LIKE` pattern with no `%` or `_` matches exactly one string, so it is an
equality test written the slow way. `=` is clearer and index-friendly. Only
plain `LIKE` is rewritten (`ILIKE`/`NOT LIKE` differ from `=`).

  bad:  WHERE status LIKE 'active'
  good: WHERE status = 'active'",
    },
    RuleDoc {
        code: "CV09",
        fixable: false,
        summary: "Use of a configured blocked word",
        explanation: "\
Flags identifiers or keywords listed in the rule's `blocked` option — useful to
ban deprecated columns, reserved names, or risky functions. Off until
configured:

  [lint.rules.CV09]
  blocked = [\"old_status\", \"sysdate\"]",
    },
    RuleDoc {
        code: "CV11",
        fixable: false,
        summary: "Inconsistent cast style within a statement",
        explanation: "\
A statement that mixes the `x::type` shorthand with `CAST(x AS type)` is noisy.
The first cast's style is taken as canonical and the others are flagged; pick
one and stick to it.

  bad:  SELECT a::int, CAST(b AS text) FROM t
  good: SELECT a::int, b::text FROM t",
    },
    RuleDoc {
        code: "CV13",
        fixable: true,
        summary: "IN with a single value is just =",
        explanation: "\
`x IN (v)` tests one value, so `x = v` (or `x <> v` for `NOT IN`) is clearer and
lets the planner use an index. Lists and subqueries are left alone. The fix
swaps the operator and drops the parentheses.

  bad:  WHERE status IN ('active')
  good: WHERE status = 'active'",
    },
    RuleDoc {
        code: "CP01",
        fixable: true,
        summary: "Keywords should be upper case",
        explanation: "\
Upper-casing keywords visually separates SQL structure from identifiers and
literals. This rule is auto-fixable: run `mold fix` to apply. Set
`policy = \"lower\"` to require lower case instead (default `upper`).

  bad:  select id from patient
  good: SELECT id FROM patient",
    },
    RuleDoc {
        code: "CP02",
        fixable: true,
        summary: "Unquoted identifiers should be lower case",
        explanation: "\
Postgres folds unquoted identifiers to lower case, so a mixed- or upper-case
unquoted name is misleading. This rule lower-cases it (auto-fixable). Quoted
identifiers are left untouched. Set `policy = \"upper\"` to require upper case
instead (default `lower`).

  bad:  SELECT Id FROM Patient
  good: SELECT id FROM patient",
    },
    RuleDoc {
        code: "RF01",
        fixable: false,
        summary: "Reference to an unknown table, column or alias (needs schema)",
        explanation: "\
The reference does not resolve against the connected schema. This catches typos
and stale queries at author time. Requires a [database] connection so the live
schema can be introspected; without one these checks are skipped.

A 'did you mean …?' hint is offered when a similarly named object exists.",
    },
    RuleDoc {
        code: "RF02",
        fixable: false,
        summary: "Ambiguous column; qualify with a table name (needs schema)",
        explanation: "\
The column exists in more than one table in scope, so the reference is
ambiguous. Qualify it with the table name or alias.

  bad:  SELECT id FROM patient JOIN orders ON ...
  good: SELECT patient.id FROM patient JOIN orders ON ...",
    },
    RuleDoc {
        code: "RF06",
        fixable: true,
        summary: "Identifier quoted unnecessarily",
        explanation: "\
Double quotes only matter when the identifier would otherwise be folded or
rejected — mixed case, special characters, or a reserved word. A plain
lower-case name does not need them, and the quotes just add noise. Removed
automatically.

  bad:  SELECT \"id\" FROM \"patient\"
  good: SELECT id FROM patient",
    },
];

/// Looks up a rule by code (case-insensitive).
pub(crate) fn find(code: &str) -> Option<&'static RuleDoc> {
    RULES.iter().find(|r| r.code.eq_ignore_ascii_case(code))
}

/// Output format for `mold rules`.
#[derive(Clone, Copy, Debug, Default, clap::ValueEnum)]
pub enum RulesFormat {
    /// Aligned table for terminals.
    #[default]
    Human,
    /// JSON array for tooling.
    Json,
}

/// `mold rules` arguments.
#[derive(clap::Args, Debug)]
pub struct RulesArgs {
    /// Output format.
    #[arg(long, value_enum, default_value_t = RulesFormat::Human)]
    pub format: RulesFormat,
    /// Only list rules in this category (e.g. `convention`) or prefix (`CV`).
    #[arg(long)]
    pub group: Option<String>,
}

/// The category a rule code belongs to, derived from its two-letter prefix.
pub(crate) fn category(code: &str) -> &'static str {
    match &code[..2] {
        "AL" => "aliasing",
        "AM" => "ambiguity",
        "ST" => "structure",
        "SF" => "safety",
        "JB" => "jsonb",
        "CV" => "convention",
        "CP" => "capitalisation",
        "RF" => "references",
        _ => "other",
    }
}

/// True when `code` matches a `--group` filter (category name or code prefix,
/// case-insensitive).
fn matches_group(code: &str, group: &str) -> bool {
    category(code).eq_ignore_ascii_case(group) || code[..2].eq_ignore_ascii_case(group)
}

pub fn run(args: &RulesArgs) -> Result<u8> {
    let selected: Vec<&RuleDoc> = RULES
        .iter()
        .filter(|r| {
            args.group
                .as_deref()
                .is_none_or(|g| matches_group(r.code, g))
        })
        .collect();

    match args.format {
        RulesFormat::Json => {
            let items: Vec<String> = selected
                .iter()
                .map(|r| {
                    format!(
                        "{{\"code\":\"{}\",\"group\":\"{}\",\"fixable\":{},\"summary\":\"{}\"}}",
                        r.code,
                        category(r.code),
                        r.fixable,
                        json_escape(r.summary),
                    )
                })
                .collect();
            println!("[{}]", items.join(","));
        }
        RulesFormat::Human => {
            println!(
                "Prefixes: AL = aliasing, AM = ambiguity, ST = structure, SF = safety, JB = JSONB, CV = convention, CP = capitalisation, RF = references\n"
            );
            println!("{:<6} {:<8} DESCRIPTION", "CODE", "FIXABLE");
            for r in &selected {
                println!(
                    "{:<6} {:<8} {}",
                    r.code,
                    if r.fixable { "yes" } else { "no" },
                    r.summary
                );
            }
        }
    }
    Ok(exit::OK)
}

/// Minimal JSON string escaping for the summary text.
fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            _ => out.push(c),
        }
    }
    out
}
