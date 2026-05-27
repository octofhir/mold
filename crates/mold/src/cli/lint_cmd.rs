//! `mold lint` — analyze SQL and report findings.

use std::path::PathBuf;

use anyhow::Result;
use clap::Args;
use mold_hir::{Diagnostic, Severity};

use super::io::{self, InputFile, gather_inputs, line_col};
use super::{Cli, ReportFormat, exit};

#[derive(Args, Debug)]
pub struct LintArgs {
    /// Files or directories to lint. Use `-` or omit to read stdin.
    paths: Vec<PathBuf>,

    /// Output format.
    #[arg(long, value_enum, default_value_t = ReportFormat::Human)]
    format: ReportFormat,
}

pub fn run(args: &LintArgs, cli: &Cli) -> Result<u8> {
    let config = cli.load_config(&io::discovery_anchor(&args.paths))?;
    let inputs = gather_inputs(&args.paths)?;
    let provider = super::schema::resolve(&config)?;
    let provider_ref = provider
        .as_ref()
        .map(|p| p as &dyn mold_hir::SchemaProvider);

    let mut total = 0usize;
    for input in &inputs {
        let analyzed = super::analysis::analyze(&input.text, &config, provider_ref);
        total += analyzed.diagnostics.len() + analyzed.parse_errors;
        report(args.format, input, &analyzed.diagnostics);
    }

    if matches!(args.format, ReportFormat::Human) {
        if total == 0 {
            eprintln!("No issues found.");
        } else {
            eprintln!("{total} issue(s) found.");
        }
    }

    Ok(if total > 0 { exit::FINDINGS } else { exit::OK })
}

fn report(format: ReportFormat, input: &InputFile, diags: &[Diagnostic]) {
    match format {
        ReportFormat::Human => report_human(input, diags),
        ReportFormat::Json => report_json(input, diags),
        ReportFormat::Github => report_github(input, diags),
    }
}

fn severity_label(s: Severity) -> &'static str {
    match s {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
        Severity::Hint => "hint",
        _ => "warning",
    }
}

fn report_human(input: &InputFile, diags: &[Diagnostic]) {
    for d in diags {
        let (line, col) = d
            .range
            .map(|r| line_col(&input.text, u32::from(r.start())))
            .unwrap_or((1, 1));
        let code = d.code.as_deref().unwrap_or("--");
        let fixable = if d.fixes.is_empty() { "" } else { " [fixable]" };
        println!(
            "{}:{}:{}: {} {}: {}{}",
            input.label,
            line,
            col,
            severity_label(d.severity),
            code,
            d.message,
            fixable
        );
    }
}

fn report_github(input: &InputFile, diags: &[Diagnostic]) {
    for d in diags {
        let (line, col) = d
            .range
            .map(|r| line_col(&input.text, u32::from(r.start())))
            .unwrap_or((1, 1));
        let level = match d.severity {
            Severity::Error => "error",
            _ => "warning",
        };
        let code = d.code.as_deref().unwrap_or("");
        println!(
            "::{level} file={},line={},col={}::{} {}",
            input.label, line, col, code, d.message
        );
    }
}

fn report_json(input: &InputFile, diags: &[Diagnostic]) {
    // Hand-rolled JSON to avoid imposing serde on the diagnostic types.
    let items: Vec<String> = diags
        .iter()
        .map(|d| {
            let (line, col) = d
                .range
                .map(|r| line_col(&input.text, u32::from(r.start())))
                .unwrap_or((1, 1));
            format!(
                r#"{{"code":{},"severity":"{}","line":{},"col":{},"message":{},"fixable":{}}}"#,
                json_str(d.code.as_deref()),
                severity_label(d.severity),
                line,
                col,
                io::json_escape(&d.message),
                !d.fixes.is_empty()
            )
        })
        .collect();
    println!(
        r#"{{"file":{},"diagnostics":[{}]}}"#,
        io::json_escape(&input.label),
        items.join(",")
    );
}

fn json_str(s: Option<&str>) -> String {
    match s {
        Some(s) => io::json_escape(s),
        None => "null".to_string(),
    }
}
