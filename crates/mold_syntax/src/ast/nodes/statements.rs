//! Statement AST nodes.
//!
//! This module provides typed wrappers for SQL statement nodes in the CST.

use crate::{SyntaxKind, SyntaxNode};

use super::super::support;
use super::super::traits::AstNode;
use super::clauses::*;
use super::ddl::*;
use super::table_refs::TableRef;

/// Root node of a parsed SQL source file containing zero or more statements.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceFile(SyntaxNode);

impl AstNode for SourceFile {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SOURCE_FILE
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SourceFile {
    /// Returns an iterator over all statements in this source file.
    pub fn statements(&self) -> impl Iterator<Item = Stmt> {
        support::children(&self.0)
    }
}

/// Enum representing all SQL statement types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stmt {
    Select(SelectStmt),
    Insert(InsertStmt),
    Update(UpdateStmt),
    Delete(DeleteStmt),
    CreateTable(CreateTableStmt),
    CreateIndex(CreateIndexStmt),
    CreateView(CreateViewStmt),
    CreateSequence(CreateSequenceStmt),
    CreateSchema(CreateSchemaStmt),
    CreateExtension(CreateExtensionStmt),
    CreateType(CreateTypeStmt),
    CreateFunction(CreateFunctionStmt),
    CreateTrigger(CreateTriggerStmt),
    Alter(AlterStmt),
    Drop(DropStmt),
    Truncate(TruncateStmt),
    Comment(CommentStmt),
    Transaction(TransactionStmt),
    Set(SetStmt),
    Show(ShowStmt),
    Reset(ResetStmt),
    Explain(ExplainStmt),
    Call(CallStmt),
    Do(DoStmt),
    Vacuum(VacuumStmt),
    Analyze(AnalyzeStmt),
    Copy(CopyStmt),
    Grant(GrantStmt),
    Revoke(RevokeStmt),
    Merge(MergeStmt),
}

impl AstNode for Stmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::SELECT_STMT
                | SyntaxKind::INSERT_STMT
                | SyntaxKind::UPDATE_STMT
                | SyntaxKind::DELETE_STMT
                | SyntaxKind::CREATE_TABLE_STMT
                | SyntaxKind::CREATE_INDEX_STMT
                | SyntaxKind::CREATE_VIEW_STMT
                | SyntaxKind::CREATE_SEQUENCE_STMT
                | SyntaxKind::CREATE_SCHEMA_STMT
                | SyntaxKind::CREATE_EXTENSION_STMT
                | SyntaxKind::CREATE_TYPE_STMT
                | SyntaxKind::CREATE_FUNCTION_STMT
                | SyntaxKind::CREATE_TRIGGER_STMT
                | SyntaxKind::ALTER_STMT
                | SyntaxKind::DROP_STMT
                | SyntaxKind::TRUNCATE_STMT
                | SyntaxKind::COMMENT_STMT
                | SyntaxKind::TRANSACTION_STMT
                | SyntaxKind::SET_STMT
                | SyntaxKind::SHOW_STMT
                | SyntaxKind::RESET_STMT
                | SyntaxKind::EXPLAIN_STMT
                | SyntaxKind::CALL_STMT
                | SyntaxKind::DO_STMT
                | SyntaxKind::VACUUM_STMT
                | SyntaxKind::ANALYZE_STMT
                | SyntaxKind::COPY_STMT
                | SyntaxKind::GRANT_STMT
                | SyntaxKind::REVOKE_STMT
                | SyntaxKind::MERGE_STMT
        )
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::SELECT_STMT => Some(Stmt::Select(SelectStmt(node))),
            SyntaxKind::INSERT_STMT => Some(Stmt::Insert(InsertStmt(node))),
            SyntaxKind::UPDATE_STMT => Some(Stmt::Update(UpdateStmt(node))),
            SyntaxKind::DELETE_STMT => Some(Stmt::Delete(DeleteStmt(node))),
            SyntaxKind::CREATE_TABLE_STMT => Some(Stmt::CreateTable(CreateTableStmt(node))),
            SyntaxKind::CREATE_INDEX_STMT => Some(Stmt::CreateIndex(CreateIndexStmt(node))),
            SyntaxKind::CREATE_VIEW_STMT => Some(Stmt::CreateView(CreateViewStmt(node))),
            SyntaxKind::CREATE_SEQUENCE_STMT => {
                Some(Stmt::CreateSequence(CreateSequenceStmt(node)))
            }
            SyntaxKind::CREATE_SCHEMA_STMT => Some(Stmt::CreateSchema(CreateSchemaStmt(node))),
            SyntaxKind::CREATE_EXTENSION_STMT => {
                Some(Stmt::CreateExtension(CreateExtensionStmt(node)))
            }
            SyntaxKind::CREATE_TYPE_STMT => Some(Stmt::CreateType(CreateTypeStmt(node))),
            SyntaxKind::CREATE_FUNCTION_STMT => {
                Some(Stmt::CreateFunction(CreateFunctionStmt(node)))
            }
            SyntaxKind::CREATE_TRIGGER_STMT => Some(Stmt::CreateTrigger(CreateTriggerStmt(node))),
            SyntaxKind::ALTER_STMT => Some(Stmt::Alter(AlterStmt(node))),
            SyntaxKind::DROP_STMT => Some(Stmt::Drop(DropStmt(node))),
            SyntaxKind::TRUNCATE_STMT => Some(Stmt::Truncate(TruncateStmt(node))),
            SyntaxKind::COMMENT_STMT => Some(Stmt::Comment(CommentStmt(node))),
            SyntaxKind::TRANSACTION_STMT => Some(Stmt::Transaction(TransactionStmt(node))),
            SyntaxKind::SET_STMT => Some(Stmt::Set(SetStmt(node))),
            SyntaxKind::SHOW_STMT => Some(Stmt::Show(ShowStmt(node))),
            SyntaxKind::RESET_STMT => Some(Stmt::Reset(ResetStmt(node))),
            SyntaxKind::EXPLAIN_STMT => Some(Stmt::Explain(ExplainStmt(node))),
            SyntaxKind::CALL_STMT => Some(Stmt::Call(CallStmt(node))),
            SyntaxKind::DO_STMT => Some(Stmt::Do(DoStmt(node))),
            SyntaxKind::VACUUM_STMT => Some(Stmt::Vacuum(VacuumStmt(node))),
            SyntaxKind::ANALYZE_STMT => Some(Stmt::Analyze(AnalyzeStmt(node))),
            SyntaxKind::COPY_STMT => Some(Stmt::Copy(CopyStmt(node))),
            SyntaxKind::GRANT_STMT => Some(Stmt::Grant(GrantStmt(node))),
            SyntaxKind::REVOKE_STMT => Some(Stmt::Revoke(RevokeStmt(node))),
            SyntaxKind::MERGE_STMT => Some(Stmt::Merge(MergeStmt(node))),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Stmt::Select(it) => it.syntax(),
            Stmt::Insert(it) => it.syntax(),
            Stmt::Update(it) => it.syntax(),
            Stmt::Delete(it) => it.syntax(),
            Stmt::CreateTable(it) => it.syntax(),
            Stmt::CreateIndex(it) => it.syntax(),
            Stmt::CreateView(it) => it.syntax(),
            Stmt::CreateSequence(it) => it.syntax(),
            Stmt::CreateSchema(it) => it.syntax(),
            Stmt::CreateExtension(it) => it.syntax(),
            Stmt::CreateType(it) => it.syntax(),
            Stmt::CreateFunction(it) => it.syntax(),
            Stmt::CreateTrigger(it) => it.syntax(),
            Stmt::Alter(it) => it.syntax(),
            Stmt::Drop(it) => it.syntax(),
            Stmt::Truncate(it) => it.syntax(),
            Stmt::Comment(it) => it.syntax(),
            Stmt::Transaction(it) => it.syntax(),
            Stmt::Set(it) => it.syntax(),
            Stmt::Show(it) => it.syntax(),
            Stmt::Reset(it) => it.syntax(),
            Stmt::Explain(it) => it.syntax(),
            Stmt::Call(it) => it.syntax(),
            Stmt::Do(it) => it.syntax(),
            Stmt::Vacuum(it) => it.syntax(),
            Stmt::Analyze(it) => it.syntax(),
            Stmt::Copy(it) => it.syntax(),
            Stmt::Grant(it) => it.syntax(),
            Stmt::Revoke(it) => it.syntax(),
            Stmt::Merge(it) => it.syntax(),
        }
    }
}

/// A SELECT statement.
///
/// Structure: `[WITH ...] SELECT ... [FROM ...] [WHERE ...] [GROUP BY ...] [HAVING ...] [ORDER BY ...] [LIMIT ...] [OFFSET ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SelectStmt(pub(crate) SyntaxNode);

impl AstNode for SelectStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::SELECT_STMT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl SelectStmt {
    /// Returns the WITH clause if present.
    pub fn with_clause(&self) -> Option<WithClause> {
        support::child(&self.0)
    }

    /// Returns the SELECT clause.
    pub fn select_clause(&self) -> Option<SelectClause> {
        support::child(&self.0)
    }

    /// Returns the FROM clause if present.
    pub fn from_clause(&self) -> Option<FromClause> {
        support::child(&self.0)
    }

    /// Returns the WHERE clause if present.
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.0)
    }

    /// Returns the GROUP BY clause if present.
    pub fn group_by_clause(&self) -> Option<GroupByClause> {
        support::child(&self.0)
    }

    /// Returns the HAVING clause if present.
    pub fn having_clause(&self) -> Option<HavingClause> {
        support::child(&self.0)
    }

    /// Returns the ORDER BY clause if present.
    pub fn order_by_clause(&self) -> Option<OrderByClause> {
        support::child(&self.0)
    }

    /// Returns the LIMIT clause if present.
    pub fn limit_clause(&self) -> Option<LimitClause> {
        support::child(&self.0)
    }

    /// Returns the OFFSET clause if present.
    pub fn offset_clause(&self) -> Option<OffsetClause> {
        support::child(&self.0)
    }
}

/// An INSERT statement.
///
/// Structure: `[WITH ...] INSERT INTO table [(columns)] VALUES ... | SELECT ... [ON CONFLICT ...] [RETURNING ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InsertStmt(pub(crate) SyntaxNode);

impl AstNode for InsertStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::INSERT_STMT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl InsertStmt {
    /// Returns the WITH clause if present.
    pub fn with_clause(&self) -> Option<WithClause> {
        support::child(&self.0)
    }

    /// Returns the target table name.
    pub fn table(&self) -> Option<TableRef> {
        support::child(&self.0)
    }

    /// Returns the insert columns clause if present.
    pub fn columns(&self) -> Option<InsertColumns> {
        support::child(&self.0)
    }

    /// Returns the VALUES clause if present.
    pub fn values_clause(&self) -> Option<ValuesClause> {
        support::child(&self.0)
    }

    /// Returns the source SELECT statement if present (INSERT ... SELECT).
    pub fn source_select(&self) -> Option<SelectStmt> {
        support::child(&self.0)
    }

    /// Returns the ON CONFLICT clause if present.
    pub fn on_conflict_clause(&self) -> Option<OnConflictClause> {
        support::child(&self.0)
    }

    /// Returns the RETURNING clause if present.
    pub fn returning_clause(&self) -> Option<ReturningClause> {
        support::child(&self.0)
    }
}

/// An UPDATE statement.
///
/// Structure: `[WITH ...] UPDATE table SET ... [FROM ...] [WHERE ...] [RETURNING ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct UpdateStmt(pub(crate) SyntaxNode);

impl AstNode for UpdateStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::UPDATE_STMT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl UpdateStmt {
    /// Returns the WITH clause if present.
    pub fn with_clause(&self) -> Option<WithClause> {
        support::child(&self.0)
    }

    /// Returns the target table.
    pub fn table(&self) -> Option<TableRef> {
        support::child(&self.0)
    }

    /// Returns the SET clause.
    pub fn set_clause(&self) -> Option<SetClause> {
        support::child(&self.0)
    }

    /// Returns the FROM clause if present.
    pub fn from_clause(&self) -> Option<FromClause> {
        support::child(&self.0)
    }

    /// Returns the WHERE clause if present.
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.0)
    }

    /// Returns the RETURNING clause if present.
    pub fn returning_clause(&self) -> Option<ReturningClause> {
        support::child(&self.0)
    }
}

/// A DELETE statement.
///
/// Structure: `[WITH ...] DELETE FROM table [USING ...] [WHERE ...] [RETURNING ...]`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct DeleteStmt(pub(crate) SyntaxNode);

impl AstNode for DeleteStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::DELETE_STMT
    }

    fn cast(node: SyntaxNode) -> Option<Self> {
        Self::can_cast(node.kind()).then(|| Self(node))
    }

    fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
}

impl DeleteStmt {
    /// Returns the WITH clause if present.
    pub fn with_clause(&self) -> Option<WithClause> {
        support::child(&self.0)
    }

    /// Returns the target table.
    pub fn table(&self) -> Option<TableRef> {
        support::child(&self.0)
    }

    /// Returns the USING clause if present.
    pub fn using_clause(&self) -> Option<UsingClause> {
        support::child(&self.0)
    }

    /// Returns the WHERE clause if present.
    pub fn where_clause(&self) -> Option<WhereClause> {
        support::child(&self.0)
    }

    /// Returns the RETURNING clause if present.
    pub fn returning_clause(&self) -> Option<ReturningClause> {
        support::child(&self.0)
    }
}
