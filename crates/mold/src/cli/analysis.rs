//! Shared analysis pipeline used by `lint` and `fix`.
//!
//! Translates [`MoldConfig`] lint settings into [`AnalysisOptions`], runs the
//! analyzer with a schema-less provider, and filters diagnostics by the
//! configured rule selection.

use mold_config::{MoldConfig, SeverityLevel};
use mold_hir::{
    AnalysisOptions, BuiltinLintPack, Diagnostic, NullSchemaProvider, SchemaProvider, Severity,
    analyze_query_with_options,
};

/// The result of analyzing one source file.
pub struct Analyzed {
    /// Lint findings and syntax errors surviving rule selection.
    pub diagnostics: Vec<Diagnostic>,
}

/// Parses and analyzes `text` under the given configuration.
///
/// Without a live schema (no `[database]` connection), reference-resolution
/// diagnostics (unknown table/column) are false positives, so only keyed lint
/// findings are kept. Once a schema provider is wired in, semantic diagnostics
/// can be surfaced too.
pub fn analyze(
    text: &str,
    config: &MoldConfig,
    provider: Option<&dyn SchemaProvider>,
) -> Analyzed {
    let parse = mold_parser::parse(text);

    let options = build_options(config);
    let null = NullSchemaProvider;
    let active: &dyn SchemaProvider = provider.unwrap_or(&null);
    let analysis = analyze_query_with_options(&parse, active, &options);

    let schema_aware = provider.is_some();

    let mut diagnostics: Vec<Diagnostic> = analysis
        .diagnostics
        .into_iter()
        .filter(|d| match d.code {
            // Unkeyed semantic diagnostics are only trustworthy with a schema.
            None => schema_aware,
            // Reference checks (RF*) need a schema; suppress them without one.
            Some(code) if code.is_reference() => {
                schema_aware && config.lint.is_rule_enabled(code.as_str())
            }
            Some(code) => config.lint.is_rule_enabled(code.as_str()),
        })
        .map(|mut d| {
            if let Some(code) = d.code
                && let Some(level) = config.lint.severity_for(code.as_str())
            {
                d.severity = map_severity(level).unwrap_or(d.severity);
            }
            d
        })
        .collect();

    // Surface syntax errors alongside lint findings so they render too.
    for err in parse.errors() {
        diagnostics.push(Diagnostic::error(err.message.clone()).with_range(err.range));
    }

    Analyzed { diagnostics }
}

/// Builds analyzer options, enabling the capitalisation pack only when at least
/// one of its rules is active (it is off by default).
fn build_options(config: &MoldConfig) -> AnalysisOptions {
    let mut packs = vec![BuiltinLintPack::Core, BuiltinLintPack::Jsonb];
    if config.lint.is_rule_enabled("CP01") {
        packs.push(BuiltinLintPack::Capitalisation);
    }
    AnalysisOptions::new().with_builtin_lint_packs(packs)
}

fn map_severity(level: SeverityLevel) -> Option<Severity> {
    match level {
        SeverityLevel::Error => Some(Severity::Error),
        SeverityLevel::Warning => Some(Severity::Warning),
        SeverityLevel::Info => Some(Severity::Info),
        SeverityLevel::Off => None,
    }
}
