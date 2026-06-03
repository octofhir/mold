//! Node-shape coverage for the DDL/command typed AST accessors.
//!
//! Each test parses real PostgreSQL, projects the typed AST, and asserts the
//! accessors observe the structure the grammar produced.

use banshee_parser::parse;
use banshee_syntax::ast::{
    AlterActionKind, AstNode, Constraint, CreateIndexStmt, DropStmt, SourceFile, Stmt, TruncateStmt,
};

fn first_stmt(sql: &str) -> Stmt {
    let parse = parse(sql);
    let source = SourceFile::cast(parse.syntax()).expect("source file");
    source.statements().next().expect("at least one statement")
}

#[test]
fn create_table_columns_and_constraints() {
    let sql = "CREATE TABLE public.users (\
        id bigint PRIMARY KEY, \
        name text NOT NULL, \
        email varchar(255) DEFAULT '', \
        CONSTRAINT uq_email UNIQUE (email)\
    );";
    let Stmt::CreateTable(ct) = first_stmt(sql) else {
        panic!("expected CREATE TABLE");
    };

    assert_eq!(ct.name().and_then(|n| n.name()).unwrap().text(), "users");
    assert_eq!(ct.name().and_then(|n| n.schema()).unwrap().text(), "public");
    assert!(!ct.is_temporary());
    assert!(!ct.if_not_exists());

    let cols = ct.columns();
    assert_eq!(cols.len(), 3);
    assert_eq!(cols[0].name().unwrap().text(), "id");
    assert!(cols[0].is_primary_key());
    assert_eq!(cols[1].name().unwrap().text(), "name");
    assert!(cols[1].is_not_null());
    assert_eq!(cols[2].name().unwrap().text(), "email");
    assert!(cols[2].default_expr().is_some());
    assert_eq!(cols[2].type_name().unwrap().text(), "varchar(255)");
    assert!(cols[2].type_name().unwrap().has_modifier());

    // One table-level UNIQUE constraint.
    let table_constraints: Vec<_> = ct
        .constraints()
        .into_iter()
        .filter(|c| matches!(c, Constraint::Unique(_)))
        .collect();
    assert_eq!(table_constraints.len(), 1);
}

#[test]
fn create_table_if_not_exists_temp() {
    let sql = "CREATE TEMP TABLE IF NOT EXISTS t (id int);";
    let Stmt::CreateTable(ct) = first_stmt(sql) else {
        panic!("expected CREATE TABLE");
    };
    assert!(ct.is_temporary());
    assert!(ct.if_not_exists());
}

#[test]
fn create_index_flags() {
    let sql = "CREATE UNIQUE INDEX CONCURRENTLY idx ON public.t USING btree (a, b) WHERE a > 0;";
    let Stmt::CreateIndex(ci) = first_stmt(sql) else {
        panic!("expected CREATE INDEX");
    };
    let ci: CreateIndexStmt = ci;
    assert!(ci.is_unique());
    assert!(ci.is_concurrent());
    assert_eq!(ci.table().and_then(|t| t.name()).unwrap().text(), "t");
    assert_eq!(ci.using_method().unwrap().text(), "btree");
    assert!(ci.where_clause().is_some());
}

#[test]
fn create_index_plain_not_concurrent() {
    let sql = "CREATE INDEX ON t (a);";
    let Stmt::CreateIndex(ci) = first_stmt(sql) else {
        panic!("expected CREATE INDEX");
    };
    assert!(!ci.is_unique());
    assert!(!ci.is_concurrent());
}

#[test]
fn alter_table_add_column() {
    let sql = "ALTER TABLE t ADD COLUMN c text NOT NULL;";
    let Stmt::Alter(alter) = first_stmt(sql) else {
        panic!("expected ALTER");
    };
    assert!(!alter.is_rename());
    let actions: Vec<_> = alter.actions().collect();
    assert_eq!(actions.len(), 1);
    assert_eq!(actions[0].kind(), AlterActionKind::AddColumn);
    let col = actions[0].added_column().unwrap();
    assert_eq!(col.name().unwrap().text(), "c");
    assert!(col.is_not_null());
}

#[test]
fn alter_table_add_constraint_not_valid() {
    let sql = "ALTER TABLE t ADD CONSTRAINT fk FOREIGN KEY (a) REFERENCES o (id) NOT VALID;";
    let Stmt::Alter(alter) = first_stmt(sql) else {
        panic!("expected ALTER");
    };
    let actions: Vec<_> = alter.actions().collect();
    assert_eq!(actions[0].kind(), AlterActionKind::AddConstraint);
    let constraint = actions[0].added_constraint().unwrap();
    assert!(matches!(constraint, Constraint::ForeignKey(_)));
    assert!(constraint.is_not_valid());
}

#[test]
fn alter_table_alter_column_type() {
    let sql = "ALTER TABLE t ALTER COLUMN c TYPE bigint;";
    let Stmt::Alter(alter) = first_stmt(sql) else {
        panic!("expected ALTER");
    };
    let actions: Vec<_> = alter.actions().collect();
    assert_eq!(actions[0].kind(), AlterActionKind::AlterColumn);
    assert!(actions[0].changes_type());
    assert_eq!(actions[0].new_type().unwrap().text(), "bigint");
}

#[test]
fn alter_table_drop_column() {
    let sql = "ALTER TABLE t DROP COLUMN c;";
    let Stmt::Alter(alter) = first_stmt(sql) else {
        panic!("expected ALTER");
    };
    let actions: Vec<_> = alter.actions().collect();
    assert_eq!(actions[0].kind(), AlterActionKind::DropColumn);
    assert_eq!(actions[0].target_name().unwrap().text(), "c");
}

#[test]
fn alter_table_rename() {
    let sql = "ALTER TABLE t RENAME COLUMN a TO b;";
    let Stmt::Alter(alter) = first_stmt(sql) else {
        panic!("expected ALTER");
    };
    assert!(alter.is_rename());
    assert!(alter.renames_subobject());
    assert_eq!(alter.actions().count(), 0);
}

#[test]
fn drop_statement() {
    let sql = "DROP INDEX CONCURRENTLY IF EXISTS a, b CASCADE;";
    let Stmt::Drop(drop) = first_stmt(sql) else {
        panic!("expected DROP");
    };
    let drop: DropStmt = drop;
    assert_eq!(
        drop.object_kind(),
        Some(banshee_syntax::SyntaxKind::INDEX_KW)
    );
    assert!(drop.is_concurrent());
    assert!(drop.if_exists());
    assert!(drop.is_cascade());
    assert_eq!(drop.names().count(), 2);
}

#[test]
fn truncate_statement() {
    let sql = "TRUNCATE TABLE a, b RESTART IDENTITY CASCADE;";
    let Stmt::Truncate(trunc) = first_stmt(sql) else {
        panic!("expected TRUNCATE");
    };
    let trunc: TruncateStmt = trunc;
    assert_eq!(trunc.names().count(), 2);
    assert!(trunc.is_cascade());
    assert!(trunc.restart_identity());
}

#[test]
fn create_table_as_select_has_query() {
    let sql = "CREATE TABLE t AS SELECT 1 AS x;";
    let Stmt::CreateTable(ct) = first_stmt(sql) else {
        panic!("expected CREATE TABLE");
    };
    assert!(ct.column_def_list().is_none());
    assert!(ct.as_query().is_some());
}

#[test]
fn statements_iter_yields_ddl_variants() {
    let sql = "CREATE TABLE t (id int); ALTER TABLE t ADD COLUMN c int; DROP TABLE t;";
    let parse = parse(sql);
    let source = SourceFile::cast(parse.syntax()).unwrap();
    let kinds: Vec<_> = source.statements().map(|s| s.syntax().kind()).collect();
    assert_eq!(
        kinds,
        vec![
            banshee_syntax::SyntaxKind::CREATE_TABLE_STMT,
            banshee_syntax::SyntaxKind::ALTER_STMT,
            banshee_syntax::SyntaxKind::DROP_STMT,
        ]
    );
}
