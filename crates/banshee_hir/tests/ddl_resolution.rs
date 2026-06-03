use banshee_hir::{NullSchemaProvider, analyze_query};
use banshee_parser::parse;

fn diags(sql: &str) -> Vec<String> {
    let parse = parse(sql);
    let a = analyze_query(&parse, &NullSchemaProvider);
    a.diagnostics
        .iter()
        .map(|d| format!("[{:?}] {}", d.code, d.message))
        .collect()
}

#[test]
fn ddl_names_not_flagged() {
    for sql in [
        "CREATE TABLE new_t (id int);",
        "ALTER TABLE other ADD COLUMN c int;",
        "DROP TABLE gone;",
        "CREATE INDEX i ON tbl (a);",
        "TRUNCATE foo CASCADE;",
        "CREATE TABLE t (id bigint, name text, CHECK (id > 0));",
    ] {
        let d = diags(sql);
        eprintln!("SQL: {sql} -> {d:?}");
        assert!(d.is_empty(), "unexpected diagnostics for `{sql}`: {d:?}");
    }
}

#[test]
fn create_then_select_resolves() {
    let sql = "CREATE TABLE app_users (id bigint, email text); SELECT id, email FROM app_users;";
    let d = diags(sql);
    eprintln!("{d:?}");
    assert!(d.is_empty(), "expected clean, got {d:?}");
}

#[test]
fn select_unknown_column_in_ddl_table_flags() {
    let sql = "CREATE TABLE app_users (id bigint); SELECT missing FROM app_users;";
    let d = diags(sql);
    eprintln!("{d:?}");
    assert!(
        d.iter().any(|m| m.contains("missing")),
        "expected unknown-column diagnostic, got {d:?}"
    );
}
