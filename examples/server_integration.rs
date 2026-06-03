//! How an application (e.g. a FHIR SQL console) drives banshee's analysis with its
//! own schema source.
//!
//! It implements `banshee_hir::SchemaProvider`, runs the analyzer with the lint
//! packs it wants, and turns the resulting diagnostics — code, message, help,
//! and fixes — into whatever its surface needs (here, plain stdout). This is
//! the same shape an LSP server maps onto `publishDiagnostics` + code actions.
//!
//! Run with: `cargo run -p banshee --example server_integration`

use banshee_hir::{
    AnalysisOptions, BuiltinLintPack, ColumnInfo, DataType, SchemaProvider, TableInfo, TableType,
    analyze_query_with_options,
};

/// A provider backed by a hard-coded schema. A real application would source
/// this from a catalog, a cached snapshot, or a live database.
struct AppSchema;

impl SchemaProvider for AppSchema {
    fn lookup_table(&self, _schema: Option<&str>, name: &str) -> Option<TableInfo> {
        (name.eq_ignore_ascii_case("patient")).then(|| TableInfo {
            schema: Some("public".to_string()),
            name: "patient".to_string(),
            table_type: TableType::Table,
        })
    }

    fn lookup_columns(&self, _schema: Option<&str>, table: &str) -> Vec<ColumnInfo> {
        if !table.eq_ignore_ascii_case("patient") {
            return Vec::new();
        }
        vec![
            ColumnInfo {
                name: "id".to_string(),
                data_type: DataType::Integer,
                nullable: false,
                ordinal: 0,
            },
            ColumnInfo {
                name: "resource".to_string(),
                data_type: DataType::Jsonb,
                nullable: true,
                ordinal: 1,
            },
        ]
    }
}

fn main() {
    let sql = "select id, naem from patient where resource->'active' = 'true'";
    let parse = banshee_parser::parse(sql);

    let options = AnalysisOptions::new().with_builtin_lint_packs([
        BuiltinLintPack::Core,
        BuiltinLintPack::Jsonb,
        BuiltinLintPack::Convention,
    ]);
    let analysis = analyze_query_with_options(&parse, &AppSchema, &options);

    println!("source: {sql}\n");
    for d in &analysis.diagnostics {
        let code = d.code.map(|c| c.as_str()).unwrap_or("--");
        println!("[{code}] {:?}: {}", d.severity, d.message);
        if let Some(help) = &d.help {
            println!("    help: {help}");
        }
        for fix in &d.fixes {
            println!("    fix: {} ({} edit(s))", fix.title, fix.edits.len());
        }
    }
}
