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
        code: "AM02",
        fixable: false,
        summary: "Set operators (UNION/EXCEPT/INTERSECT) should state ALL or DISTINCT",
        explanation: "\
`UNION` defaults to `UNION DISTINCT`, which silently deduplicates. Stating
`ALL` or `DISTINCT` makes the intent — and the cost — explicit.",
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
        code: "AL05",
        fixable: false,
        summary: "Table alias declared but never used",
        explanation: "\
The query introduces a table alias that no column reference uses to qualify a
name. Drop the alias, or use it.",
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
        code: "CP01",
        fixable: true,
        summary: "Keywords should be upper case",
        explanation: "\
Upper-casing keywords visually separates SQL structure from identifiers and
literals. This rule is auto-fixable: run `mold fix` to apply.

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
identifiers are left untouched.

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
];

/// Looks up a rule by code (case-insensitive).
pub(crate) fn find(code: &str) -> Option<&'static RuleDoc> {
    RULES.iter().find(|r| r.code.eq_ignore_ascii_case(code))
}

pub fn run() -> Result<u8> {
    println!(
        "Prefixes: AL = aliasing, AM = ambiguity, ST = structure, SF = safety, JB = JSONB, CV = convention, CP = capitalisation, RF = references\n"
    );
    println!("{:<6} {:<8} DESCRIPTION", "CODE", "FIXABLE");
    for r in RULES {
        println!(
            "{:<6} {:<8} {}",
            r.code,
            if r.fixable { "yes" } else { "no" },
            r.summary
        );
    }
    Ok(exit::OK)
}
