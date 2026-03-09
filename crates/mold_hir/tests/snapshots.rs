use insta::assert_snapshot;
use mold_hir::{
    Analysis, ColumnInfo, DataType, NullSchemaProvider, SchemaProvider, TableInfo, TableType,
    analyze_query,
};
use mold_parser::parse;
use text_size::TextRange;

struct TestProvider;

impl SchemaProvider for TestProvider {
    fn lookup_table(&self, schema: Option<&str>, name: &str) -> Option<TableInfo> {
        match (schema, name) {
            (Some("public"), "users") | (None, "users") => Some(TableInfo {
                schema: Some("public".to_string()),
                name: "users".to_string(),
                table_type: TableType::Table,
            }),
            (Some("public"), "orders") | (None, "orders") => Some(TableInfo {
                schema: Some("public".to_string()),
                name: "orders".to_string(),
                table_type: TableType::Table,
            }),
            _ => None,
        }
    }

    fn lookup_columns(&self, schema: Option<&str>, table: &str) -> Vec<ColumnInfo> {
        match (schema, table) {
            (Some("public"), "users") | (None, "users") => vec![
                ColumnInfo {
                    name: "id".to_string(),
                    data_type: DataType::Integer,
                    nullable: false,
                    ordinal: 0,
                },
                ColumnInfo {
                    name: "name".to_string(),
                    data_type: DataType::Text,
                    nullable: true,
                    ordinal: 1,
                },
                ColumnInfo {
                    name: "active".to_string(),
                    data_type: DataType::Boolean,
                    nullable: true,
                    ordinal: 2,
                },
            ],
            (Some("public"), "orders") | (None, "orders") => vec![
                ColumnInfo {
                    name: "id".to_string(),
                    data_type: DataType::Integer,
                    nullable: false,
                    ordinal: 0,
                },
                ColumnInfo {
                    name: "user_id".to_string(),
                    data_type: DataType::Integer,
                    nullable: false,
                    ordinal: 1,
                },
                ColumnInfo {
                    name: "amount".to_string(),
                    data_type: DataType::Integer,
                    nullable: false,
                    ordinal: 2,
                },
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

fn format_range(range: Option<TextRange>) -> String {
    match range {
        Some(range) => format!("{}..{}", u32::from(range.start()), u32::from(range.end())),
        None => "-".to_string(),
    }
}

fn render_analysis(analysis: &Analysis) -> String {
    let mut lines = Vec::new();

    lines.push("diagnostics:".to_string());
    for diagnostic in &analysis.diagnostics {
        lines.push(format!(
            "- {:?} | {} | {}",
            diagnostic.severity,
            diagnostic.message,
            format_range(diagnostic.range)
        ));
    }
    if analysis.diagnostics.is_empty() {
        lines.push("- <none>".to_string());
    }

    lines.push("tables:".to_string());
    for table in &analysis.tables {
        lines.push(format!(
            "- {} | original={} | alias={} | source={:?} | range={}",
            table.binding.name,
            table.binding.original_name,
            table.binding.alias.as_deref().unwrap_or("-"),
            table.binding.source,
            format_range(Some(table.range))
        ));
    }
    if analysis.tables.is_empty() {
        lines.push("- <none>".to_string());
    }

    lines.push("columns:".to_string());
    for column in &analysis.columns {
        lines.push(format!(
            "- {}.{} | type={:?} | ambiguous={} | range={}",
            column.table.name,
            column.column.name,
            column.column.data_type,
            column.was_ambiguous,
            format_range(Some(column.range))
        ));
    }
    if analysis.columns.is_empty() {
        lines.push("- <none>".to_string());
    }

    lines.push("output_columns:".to_string());
    for column in &analysis.output_columns {
        lines.push(format!(
            "- {} | table={} | type={:?}",
            column.name,
            column.table.as_deref().unwrap_or("-"),
            column.data_type
        ));
    }
    if analysis.output_columns.is_empty() {
        lines.push("- <none>".to_string());
    }

    lines.join("\n")
}

#[test]
fn snapshot_unknown_column_analysis() {
    let provider = TestProvider;
    let parse = parse("SELECT missing_column FROM users");
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("unknown_column_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_update_without_where_analysis() {
    let provider = TestProvider;
    let parse = parse("UPDATE users SET active = true");
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("update_without_where_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_unknown_table_analysis() {
    let provider = NullSchemaProvider;
    let parse = parse("SELECT * FROM missing_table");
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("unknown_table_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_cte_resolution_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH recent AS (SELECT user_id, amount FROM orders) SELECT recent.user_id FROM recent",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("cte_resolution_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_update_from_alias_analysis() {
    let provider = TestProvider;
    let parse = parse("UPDATE users u SET active = true FROM orders o WHERE u.id = o.user_id");
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("update_from_alias_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_delete_using_alias_analysis() {
    let provider = TestProvider;
    let parse = parse("DELETE FROM users u USING orders o WHERE u.id = o.user_id");
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("delete_using_alias_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_with_update_cte_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH recent AS (SELECT user_id, amount FROM orders) UPDATE users u SET active = true FROM recent WHERE u.id = recent.user_id",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("with_update_cte_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_with_delete_cte_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH recent AS (SELECT user_id, amount FROM orders) DELETE FROM users u USING recent WHERE u.id = recent.user_id",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("with_delete_cte_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_with_insert_source_cte_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH recent AS (SELECT user_id, amount FROM orders) INSERT INTO users (id, name) SELECT recent.user_id, recent.amount FROM recent",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!(
        "with_insert_source_cte_analysis",
        render_analysis(&analysis)
    );
}

#[test]
fn snapshot_dml_cte_resolution_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH changed AS (UPDATE orders SET amount = amount + 1 RETURNING user_id, amount) SELECT changed.user_id FROM changed",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("dml_cte_resolution_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_dml_cte_local_scope_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH changed AS (UPDATE orders o SET amount = o.amount + 1 RETURNING o.user_id, o.amount) SELECT changed.user_id FROM changed",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("dml_cte_local_scope_analysis", render_analysis(&analysis));
}

#[test]
fn snapshot_cte_forward_reference_analysis() {
    let provider = TestProvider;
    let parse = parse(
        "WITH first_cte AS (SELECT later_cte.user_id FROM later_cte), later_cte AS (SELECT user_id FROM orders) SELECT first_cte.user_id FROM first_cte",
    );
    let analysis = analyze_query(&parse, &provider);

    assert_snapshot!("cte_forward_reference_analysis", render_analysis(&analysis));
}
