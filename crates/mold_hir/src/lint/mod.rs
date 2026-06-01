//! Built-in lint rules.
//!
//! Each rule is a zero-sized type implementing [`Rule`]. A rule walks the CST
//! (and, where useful, consults the analyzer's resolved scope) and emits
//! [`Diagnostic`]s tagged with a stable [`RuleCode`]. Rules that can
//! mechanically repair the source attach a [`Fix`].
//!
//! Rules are grouped into [`BuiltinLintPack`]s; [`apply_lints`] runs every rule
//! whose pack is enabled. New rules are added by implementing [`Rule`] and
//! listing the value in [`builtin_rules`] — no central dispatch to edit.
//!
//! Rule code prefixes: `AM` ambiguity · `AL` aliasing · `ST` structure ·
//! `SF` safety · `RF` references · `CV` convention · `CP` capitalisation ·
//! `JB` JSONB.

use mold_syntax::SyntaxKind;

use crate::analyze::{AnalysisOptions, Analyzer, BuiltinLintPack, RuleCode};

mod aliasing;
mod ambiguity;
mod capitalisation;
mod convention;
mod jsonb;
mod safety;
mod structure;

/// A built-in lint rule.
pub trait Rule: Send + Sync {
    /// The rule codes this rule may emit (most emit exactly one).
    fn codes(&self) -> &'static [RuleCode];

    /// The pack this rule belongs to; it runs only when the pack is enabled.
    fn group(&self) -> BuiltinLintPack;

    /// Walks `root` and emits diagnostics through `analyzer`.
    fn run(&self, root: &mold_syntax::SyntaxNode, analyzer: &mut Analyzer<'_>);
}

/// The full registry of built-in rules.
pub(crate) fn builtin_rules() -> &'static [&'static dyn Rule] {
    &[
        // Ambiguity (Core)
        &ambiguity::SelectStar,
        &ambiguity::ImplicitCrossJoin,
        &ambiguity::SetOpModifier,
        &ambiguity::LimitWithoutOrder,
        // Aliasing / references (Core)
        &aliasing::UnaliasedSelectItem,
        &aliasing::AliasingAndQualification,
        // Structure (Core)
        &structure::RedundantElseNull,
        &structure::SubqueryAsTable,
        &structure::UnusedCtes,
        // Safety (Core)
        &safety::UpdateWithoutWhere,
        &safety::DeleteWithoutWhere,
        // JSONB
        &jsonb::JsonbTextComparison,
        // Capitalisation
        &capitalisation::Capitalisation,
        // Convention
        &convention::NeSpelling,
        &convention::NullComparison,
        &convention::MissingSemicolons,
    ]
}

/// Runs every enabled built-in rule, then any external packs.
pub(crate) fn apply_lints(
    root: &mold_syntax::SyntaxNode,
    analyzer: &mut Analyzer<'_>,
    options: &AnalysisOptions,
) {
    for rule in builtin_rules() {
        if options.has_builtin_pack(rule.group()) {
            rule.run(root, analyzer);
        }
    }
    for pack in &options.external_lint_packs {
        pack.apply(root, analyzer);
    }
}

/// Whether `kind` is a top-level statement node. Shared by rules that need to
/// attribute nodes to their enclosing statement.
pub(super) fn is_statement(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::SELECT_STMT
            | SyntaxKind::INSERT_STMT
            | SyntaxKind::UPDATE_STMT
            | SyntaxKind::DELETE_STMT
    )
}
