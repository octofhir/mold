//! Declarative rule fixtures.
//!
//! Each `tests/rule_cases/*.yaml` file is a sequence of cases. A case names a
//! rule code and provides exactly one SQL sample under `pass`, `fail`, or
//! `fail` + `fix`:
//!
//! ```yaml
//! - name: select_star_flagged
//!   rule: AM04
//!   fail: "SELECT * FROM users;"
//!
//! - name: explicit_columns_ok
//!   rule: AM04
//!   pass: "SELECT id FROM users;"
//!
//! - name: star_expands_to_columns
//!   rule: AM04
//!   schema: true            # analyse against the test schema (users/orders)
//!   fail: "SELECT * FROM users;"
//!   fix: "SELECT id, name, active FROM users;"
//! ```
//!
//! Semantics:
//! - `pass`  — the rule must NOT fire on this SQL.
//! - `fail`  — the rule MUST fire at least once.
//! - `fix`   — applying that rule's fixes to `fail` must produce this text.
//!
//! Add a rule's coverage by editing YAML; no Rust changes needed.

use std::fs;
use std::path::Path;

use mold_hir::{
    AnalysisOptions, BuiltinLintPack, ColumnInfo, DataType, NullSchemaProvider, SchemaProvider,
    TableInfo, TableType, analyze_query_with_options,
};
use mold_parser::parse;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Case {
    name: String,
    rule: String,
    #[serde(default)]
    schema: bool,
    #[serde(default)]
    pass: Option<String>,
    #[serde(default)]
    fail: Option<String>,
    #[serde(default)]
    fix: Option<String>,
    /// Per-rule options: `code -> (key -> value)`, fed into the analyzer.
    #[serde(default)]
    options: std::collections::BTreeMap<String, std::collections::BTreeMap<String, String>>,
}

/// The fixed schema fixtures resolve against when `schema: true`.
struct TestProvider;

impl SchemaProvider for TestProvider {
    fn lookup_table(&self, schema: Option<&str>, name: &str) -> Option<TableInfo> {
        match (schema, name) {
            (Some("public") | None, "users") => Some(TableInfo {
                schema: Some("public".to_string()),
                name: "users".to_string(),
                table_type: TableType::Table,
            }),
            (Some("public") | None, "orders") => Some(TableInfo {
                schema: Some("public".to_string()),
                name: "orders".to_string(),
                table_type: TableType::Table,
            }),
            _ => None,
        }
    }

    fn lookup_columns(&self, schema: Option<&str>, table: &str) -> Vec<ColumnInfo> {
        let col = |name: &str, ordinal: usize, data_type: DataType| ColumnInfo {
            name: name.to_string(),
            data_type,
            nullable: true,
            ordinal,
        };
        match (schema, table) {
            (Some("public") | None, "users") => vec![
                col("id", 0, DataType::Integer),
                col("name", 1, DataType::Text),
                col("active", 2, DataType::Boolean),
            ],
            (Some("public") | None, "orders") => vec![
                col("id", 0, DataType::Integer),
                col("user_id", 1, DataType::Integer),
                col("amount", 2, DataType::Integer),
            ],
            _ => Vec::new(),
        }
    }

    fn all_table_names(&self) -> Vec<String> {
        vec!["users".to_string(), "orders".to_string()]
    }

    fn all_schema_names(&self) -> Vec<String> {
        vec!["public".to_string()]
    }
}

/// Every built-in pack, so any rule code can be exercised from a fixture.
fn all_packs() -> AnalysisOptions {
    AnalysisOptions::new().with_builtin_lint_packs([
        BuiltinLintPack::Core,
        BuiltinLintPack::Jsonb,
        BuiltinLintPack::Capitalisation,
        BuiltinLintPack::Convention,
        BuiltinLintPack::Migration,
    ])
}

/// Diagnostics carrying `rule`, in source order, with their fix edits.
fn fires(case: &Case, sql: &str) -> Vec<mold_hir::Diagnostic> {
    let (rule, schema) = (case.rule.as_str(), case.schema);
    let parse = parse(sql);
    let mut options = all_packs();
    options.rule_options = case.options.clone();
    let analysis = if schema {
        analyze_query_with_options(&parse, &TestProvider, &options)
    } else {
        analyze_query_with_options(&parse, &NullSchemaProvider, &options)
    };
    analysis
        .diagnostics
        .into_iter()
        .filter(|d| d.code.map(|c| c.as_str()) == Some(rule))
        .collect()
}

/// Applies a rule's fix edits to `sql` (right-to-left, so offsets stay valid).
fn apply_fixes(sql: &str, diags: &[mold_hir::Diagnostic]) -> String {
    let mut edits: Vec<_> = diags
        .iter()
        .flat_map(|d| d.fixes.iter())
        .flat_map(|f| f.edits.iter())
        .collect();
    edits.sort_by_key(|e| std::cmp::Reverse(u32::from(e.range.start())));
    let mut out = sql.to_string();
    for edit in edits {
        let start = u32::from(edit.range.start()) as usize;
        let end = u32::from(edit.range.end()) as usize;
        out.replace_range(start..end, &edit.new_text);
    }
    out
}

fn run_case(case: &Case) {
    let label = format!("{} [{}]", case.name, case.rule);

    if let Some(sql) = &case.pass {
        let diags = fires(case, sql);
        assert!(
            diags.is_empty(),
            "{label}: expected no {} on `pass` SQL, got {} diagnostic(s)",
            case.rule,
            diags.len()
        );
    }

    if let Some(sql) = &case.fail {
        let diags = fires(case, sql);
        assert!(
            !diags.is_empty(),
            "{label}: expected {} to fire on `fail` SQL, but it did not",
            case.rule
        );

        if let Some(expected) = &case.fix {
            let fixed = apply_fixes(sql, &diags);
            assert_eq!(
                &fixed, expected,
                "{label}: fix output mismatch\n  got:      {fixed:?}\n  expected: {expected:?}"
            );
        }
    }

    assert!(
        case.pass.is_some() || case.fail.is_some(),
        "{label}: case has neither `pass` nor `fail`"
    );
}

#[test]
fn rule_fixtures() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/rule_cases");
    let mut files: Vec<_> = fs::read_dir(&dir)
        .expect("rule_cases dir exists")
        .filter_map(Result::ok)
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|x| x == "yaml"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "no fixture files found in {dir:?}");

    let mut total = 0;
    for file in &files {
        let text = fs::read_to_string(file).expect("read fixture");
        let cases: Vec<Case> =
            noyalib::from_str(&text).unwrap_or_else(|e| panic!("parse {}: {e}", file.display()));
        for case in &cases {
            run_case(case);
            total += 1;
        }
    }
    eprintln!("rule fixtures: {total} cases across {} files", files.len());
}
