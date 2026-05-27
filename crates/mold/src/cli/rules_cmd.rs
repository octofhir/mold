//! `mold rules` — list the built-in lint rules.

use anyhow::Result;

use super::exit;

/// A built-in rule's catalog entry: code, fixability, one-line summary.
struct RuleDoc {
    code: &'static str,
    fixable: bool,
    summary: &'static str,
}

const RULES: &[RuleDoc] = &[
    RuleDoc {
        code: "AM04",
        fixable: false,
        summary: "Avoid SELECT *; list columns explicitly",
    },
    RuleDoc {
        code: "AM05",
        fixable: false,
        summary: "Implicit cross join; use an explicit JOIN clause",
    },
    RuleDoc {
        code: "SF01",
        fixable: false,
        summary: "UPDATE without WHERE affects all rows",
    },
    RuleDoc {
        code: "SF02",
        fixable: false,
        summary: "DELETE without WHERE affects all rows",
    },
    RuleDoc {
        code: "JB01",
        fixable: false,
        summary: "Use ->> when comparing a JSONB value to text",
    },
    RuleDoc {
        code: "CP01",
        fixable: true,
        summary: "Keywords should be upper case",
    },
];

pub fn run() -> Result<u8> {
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
