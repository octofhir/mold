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
        fixable: false,
        summary: "Avoid SELECT *; list columns explicitly",
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
        "Prefixes: AM = ambiguity, SF = safety, JB = JSONB, CP = capitalisation, RF = references\n"
    );
    println!("{:<6} {:<8} {}", "CODE", "FIXABLE", "DESCRIPTION");
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
