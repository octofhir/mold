//! Migration-safety rules (Migration pack): squawk-class checks for DDL that
//! locks tables, destroys data, or picks a type that will need migrating later.
//!
//! Codes: `MG01` concurrent index · `MG02` constraint `NOT VALID` ·
//! `MG03` volatile column default · `MG04` `NOT NULL` without default ·
//! `MG05` drop column · `MG06` alter column type · `MG07` rename ·
//! `MG08` `TRUNCATE … CASCADE` · `MG09` prefer `text` · `MG10` prefer
//! `timestamptz` · `MG11` prefer `bigint` primary key · `MG12` concurrent
//! index drop.

use mold_syntax::SyntaxKind;
use mold_syntax::SyntaxToken;
use mold_syntax::ast::{
    AlterActionKind, AlterStmt, AlterTableAction, AstNode, ColumnDef, Constraint, CreateIndexStmt,
    CreateTableStmt, DropStmt, FuncCall, TruncateStmt, TypeName,
};
use text_size::TextRange;

use super::Rule;
use crate::analyze::{Analyzer, BuiltinLintPack, Diagnostic, Fix, RuleCode, TextEdit};

/// End offset of the first direct child token of `kind`, for inserting text.
fn token_end(node: &mold_syntax::SyntaxNode, kind: SyntaxKind) -> Option<text_size::TextSize> {
    node.children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == kind)
        .map(|t| t.text_range().end())
}

/// End offset of the last non-trivia token anywhere under `node`.
fn last_token_end(node: &mold_syntax::SyntaxNode) -> Option<text_size::TextSize> {
    node.descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| !t.kind().is_trivia())
        .map(|t| t.text_range().end())
        .max()
}

/// The base type word(s) of a column type, lowercased and without any modifier
/// (`varchar(255)` → `"varchar"`, `double precision` → `"double precision"`).
fn base_type(ty: &TypeName) -> String {
    let text = ty.text().to_ascii_lowercase();
    text.split(['(', '['])
        .next()
        .unwrap_or("")
        .trim()
        .to_string()
}

// ===========================================================================
// MG01 / MG12 — concurrent index operations
// ===========================================================================

/// MG01 — `CREATE INDEX` without `CONCURRENTLY` takes an exclusive lock that
/// blocks writes for the whole build. `CONCURRENTLY` cannot run inside an
/// explicit transaction block.
pub(super) struct IndexConcurrently;

impl Rule for IndexConcurrently {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg01]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(stmt) = CreateIndexStmt::cast(node.clone()) else {
                continue;
            };
            if stmt.is_concurrent() {
                continue;
            }
            let mut diag = Diagnostic::warning(
                "CREATE INDEX without CONCURRENTLY locks the table against writes for the \
                 duration of the build",
            )
            .with_code(RuleCode::Mg01)
            .with_range(stmt.syntax().text_range());
            if let Some(at) = token_end(stmt.syntax(), SyntaxKind::INDEX_KW) {
                diag = diag.with_fix(Fix::new(
                    "Add CONCURRENTLY",
                    vec![TextEdit::replace(TextRange::empty(at), " CONCURRENTLY")],
                ));
            }
            analyzer.emit(diag);
        }
    }
}

/// MG12 — `DROP INDEX` without `CONCURRENTLY` takes an exclusive lock on the
/// table while the index is removed.
pub(super) struct DropIndexConcurrently;

impl Rule for DropIndexConcurrently {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg12]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(stmt) = DropStmt::cast(node.clone()) else {
                continue;
            };
            if stmt.object_kind() != Some(SyntaxKind::INDEX_KW) || stmt.is_concurrent() {
                continue;
            }
            let mut diag = Diagnostic::warning("DROP INDEX without CONCURRENTLY locks the table")
                .with_code(RuleCode::Mg12)
                .with_range(stmt.syntax().text_range());
            if let Some(at) = token_end(stmt.syntax(), SyntaxKind::INDEX_KW) {
                diag = diag.with_fix(Fix::new(
                    "Add CONCURRENTLY",
                    vec![TextEdit::replace(TextRange::empty(at), " CONCURRENTLY")],
                ));
            }
            analyzer.emit(diag);
        }
    }
}

// ===========================================================================
// MG02 — ADD CONSTRAINT without NOT VALID
// ===========================================================================

/// MG02 — adding a `FOREIGN KEY` or `CHECK` constraint without `NOT VALID`
/// scans and validates every existing row while holding a lock. Add the
/// constraint `NOT VALID`, then `VALIDATE CONSTRAINT` in a separate step.
pub(super) struct ConstraintNotValid;

impl Rule for ConstraintNotValid {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg02]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(action) = AlterTableAction::cast(node.clone()) else {
                continue;
            };
            if action.kind() != AlterActionKind::AddConstraint {
                continue;
            }
            let Some(constraint) = action.added_constraint() else {
                continue;
            };
            let validatable =
                matches!(constraint, Constraint::ForeignKey(_) | Constraint::Check(_));
            if !validatable || constraint.is_not_valid() {
                continue;
            }
            let mut diag = Diagnostic::warning(
                "ADD CONSTRAINT without NOT VALID validates every existing row under a lock; \
                 add it NOT VALID and VALIDATE CONSTRAINT separately",
            )
            .with_code(RuleCode::Mg02)
            .with_range(action.syntax().text_range());
            if let Some(at) = last_token_end(constraint.syntax()) {
                diag = diag.with_fix(Fix::new(
                    "Add NOT VALID",
                    vec![TextEdit::replace(TextRange::empty(at), " NOT VALID")],
                ));
            }
            analyzer.emit(diag);
        }
    }
}

// ===========================================================================
// MG03 / MG04 — ADD COLUMN hazards
// ===========================================================================

const VOLATILE_DEFAULT_FUNCS: &[&str] = &[
    "now",
    "clock_timestamp",
    "statement_timestamp",
    "transaction_timestamp",
    "timeofday",
    "random",
    "gen_random_uuid",
    "uuid_generate_v1",
    "uuid_generate_v1mc",
    "uuid_generate_v4",
    "nextval",
];

/// MG03 — `ADD COLUMN … DEFAULT <volatile>` evaluates the default for every
/// existing row, rewriting the whole table under an exclusive lock.
pub(super) struct AddColumnVolatileDefault;

impl Rule for AddColumnVolatileDefault {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg03]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for action in added_columns(root) {
            let Some(col) = action.added_column() else {
                continue;
            };
            let Some(default) = col.default_expr() else {
                continue;
            };
            let has_volatile = default.syntax().descendants().any(|n| {
                FuncCall::cast(n.clone())
                    .and_then(|f| f.name())
                    .map(|name| {
                        VOLATILE_DEFAULT_FUNCS
                            .iter()
                            .any(|v| name.text().eq_ignore_ascii_case(v))
                    })
                    .unwrap_or(false)
            });
            if has_volatile {
                analyzer.emit(
                    Diagnostic::warning(
                        "ADD COLUMN with a volatile DEFAULT rewrites the whole table; add the \
                         column without a default, then backfill",
                    )
                    .with_code(RuleCode::Mg03)
                    .with_range(action.syntax().text_range()),
                );
            }
        }
    }
}

/// MG04 — `ADD COLUMN … NOT NULL` without a `DEFAULT` fails immediately on a
/// table that already has rows.
pub(super) struct AddColumnNotNullNoDefault;

impl Rule for AddColumnNotNullNoDefault {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg04]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for action in added_columns(root) {
            let Some(col) = action.added_column() else {
                continue;
            };
            if col.is_not_null() && col.default_expr().is_none() {
                analyzer.emit(
                    Diagnostic::warning(
                        "ADD COLUMN NOT NULL without a DEFAULT fails on a table that already \
                         has rows",
                    )
                    .with_code(RuleCode::Mg04)
                    .with_range(action.syntax().text_range()),
                );
            }
        }
    }
}

/// `ALTER TABLE` actions that add a column.
fn added_columns(root: &mold_syntax::SyntaxNode) -> impl Iterator<Item = AlterTableAction> + '_ {
    root.descendants().filter_map(|n| {
        let action = AlterTableAction::cast(n.clone())?;
        (action.kind() == AlterActionKind::AddColumn).then_some(action)
    })
}

// ===========================================================================
// MG05 / MG06 / MG07 — destructive ALTER TABLE actions
// ===========================================================================

/// MG05 — `DROP COLUMN` permanently destroys the column's data and breaks any
/// view, index, or application code that depends on it.
pub(super) struct DropColumn;

impl Rule for DropColumn {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg05]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(action) = AlterTableAction::cast(node.clone()) else {
                continue;
            };
            if action.kind() == AlterActionKind::DropColumn {
                analyzer.emit(
                    Diagnostic::warning(
                        "DROP COLUMN destroys the column's data and breaks dependent objects",
                    )
                    .with_code(RuleCode::Mg05)
                    .with_range(action.syntax().text_range()),
                );
            }
        }
    }
}

/// MG06 — `ALTER COLUMN … TYPE` rewrites the table under an exclusive lock and
/// can fail or lose data on an incompatible conversion.
pub(super) struct AlterColumnType;

impl Rule for AlterColumnType {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg06]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(action) = AlterTableAction::cast(node.clone()) else {
                continue;
            };
            if action.changes_type() {
                analyzer.emit(
                    Diagnostic::warning(
                        "ALTER COLUMN TYPE rewrites the table under an exclusive lock",
                    )
                    .with_code(RuleCode::Mg06)
                    .with_range(action.syntax().text_range()),
                );
            }
        }
    }
}

/// MG07 — renaming a table or column breaks every query, view, and client that
/// still refers to the old name.
pub(super) struct Rename;

impl Rule for Rename {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg07]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(alter) = AlterStmt::cast(node.clone()) else {
                continue;
            };
            if alter.is_rename() {
                let what = if alter.renames_subobject() {
                    "a column or constraint"
                } else {
                    "a table"
                };
                analyzer.emit(
                    Diagnostic::warning(format!(
                        "RENAME of {what} breaks code that still refers to the old name"
                    ))
                    .with_code(RuleCode::Mg07)
                    .with_range(alter.syntax().text_range()),
                );
            }
        }
    }
}

// ===========================================================================
// MG08 — TRUNCATE ... CASCADE
// ===========================================================================

/// MG08 — `TRUNCATE … CASCADE` empties not just the named tables but every
/// table with a foreign key into them, often far more than intended.
pub(super) struct TruncateCascade;

impl Rule for TruncateCascade {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg08]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            let Some(trunc) = TruncateStmt::cast(node.clone()) else {
                continue;
            };
            if trunc.is_cascade() {
                analyzer.emit(
                    Diagnostic::warning(
                        "TRUNCATE ... CASCADE also empties every table with a foreign key into \
                         the targets",
                    )
                    .with_code(RuleCode::Mg08)
                    .with_range(trunc.syntax().text_range()),
                );
            }
        }
    }
}

// ===========================================================================
// MG09 / MG10 / MG11 — type preferences
// ===========================================================================

/// MG09/MG10/MG11 — prefer durable column types: `text` over `char(n)` and
/// `varchar(n)`, `timestamptz` over `timestamp`, and `bigint` over a narrower
/// integer for a primary key.
pub(super) struct TypePreference;

impl Rule for TypePreference {
    fn codes(&self) -> &'static [RuleCode] {
        &[RuleCode::Mg09, RuleCode::Mg10, RuleCode::Mg11]
    }
    fn group(&self) -> BuiltinLintPack {
        BuiltinLintPack::Migration
    }
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>) {
        for node in root.descendants() {
            if let Some(col) = ColumnDef::cast(node.clone()) {
                if let Some(ty) = col.type_name() {
                    check_char_varchar(&ty, analyzer);
                    check_timestamp(&ty, analyzer);
                }
                continue;
            }
            // `ALTER COLUMN … TYPE <type>` carries a bare type, not a ColumnDef.
            if let Some(action) = AlterTableAction::cast(node.clone())
                && action.changes_type()
                && let Some(ty) = action.new_type()
            {
                check_char_varchar(&ty, analyzer);
                check_timestamp(&ty, analyzer);
            }
        }

        // MG11 — primary-key columns typed with a narrow integer.
        for node in root.descendants() {
            let Some(table) = CreateTableStmt::cast(node.clone()) else {
                continue;
            };
            let pk_columns = primary_key_columns(&table);
            for col in table.columns() {
                let is_pk = col.is_primary_key()
                    || col
                        .name()
                        .map(|n| pk_columns.iter().any(|p| p.eq_ignore_ascii_case(n.text())))
                        .unwrap_or(false);
                if !is_pk {
                    continue;
                }
                if let Some(ty) = col.type_name()
                    && matches!(
                        base_type(&ty).as_str(),
                        "int" | "integer" | "int4" | "smallint" | "int2" | "serial"
                    )
                {
                    analyzer.emit(
                        Diagnostic::warning(
                            "prefer bigint for a primary key; a narrower integer can run out of \
                             values and is costly to widen later",
                        )
                        .with_code(RuleCode::Mg11)
                        .with_range(ty.syntax().text_range()),
                    );
                }
            }
        }
    }
}

/// MG09 — flag `char(n)`/`character(n)`/`varchar(n)`/`character varying`.
fn check_char_varchar(ty: &TypeName, analyzer: &mut Analyzer<'_>) {
    let base = base_type(ty);
    let flagged = matches!(
        base.as_str(),
        "char" | "character" | "varchar" | "character varying"
    );
    if !flagged {
        return;
    }
    analyzer.emit(
        Diagnostic::warning(
            "prefer text to char(n)/varchar(n); the length limit gives no storage benefit and \
             changing it later rewrites the table",
        )
        .with_code(RuleCode::Mg09)
        .with_range(ty.syntax().text_range())
        .with_fix(Fix::new(
            "Use text",
            vec![TextEdit::replace(ty.syntax().text_range(), "text")],
        )),
    );
}

/// MG10 — flag plain `timestamp` (no time-zone qualifier).
fn check_timestamp(ty: &TypeName, analyzer: &mut Analyzer<'_>) {
    if base_type(ty) != "timestamp" {
        return;
    }
    let text = ty.text().to_ascii_lowercase();
    if text.contains("time zone") {
        return; // `timestamp with/without time zone` written explicitly
    }
    let mut diag = Diagnostic::warning(
        "prefer timestamptz to timestamp; timestamp without time zone ignores the session \
         time zone and is a frequent source of bugs",
    )
    .with_code(RuleCode::Mg10)
    .with_range(ty.syntax().text_range());
    // Replace only the leading `timestamp` keyword so any precision is kept.
    if let Some(kw) = timestamp_keyword(ty) {
        diag = diag.with_fix(Fix::new(
            "Use timestamptz",
            vec![TextEdit::replace(kw.text_range(), "timestamptz")],
        ));
    }
    analyzer.emit(diag);
}

fn timestamp_keyword(ty: &TypeName) -> Option<SyntaxToken> {
    ty.syntax()
        .children_with_tokens()
        .filter_map(|e| e.into_token())
        .find(|t| t.kind() == SyntaxKind::TIMESTAMP_KW)
        .cloned()
}

/// Column names named by a single-column table-level `PRIMARY KEY`.
fn primary_key_columns(table: &CreateTableStmt) -> Vec<String> {
    let mut names = Vec::new();
    for constraint in table.constraints() {
        if let Constraint::PrimaryKey(pk) = constraint {
            for ident in pk
                .syntax()
                .descendants_with_tokens()
                .filter_map(|e| e.into_token())
                .filter(|t| matches!(t.kind(), SyntaxKind::IDENT | SyntaxKind::QUOTED_IDENT))
            {
                names.push(ident.text().to_string());
            }
        }
    }
    names
}
