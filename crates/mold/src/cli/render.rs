//! Rich, rustc-style diagnostic rendering for human output.
//!
//! Uses `annotate-snippets` (the renderer the Rust compiler itself uses) to
//! print each finding with a `severity[CODE]:` header, the offending source
//! line, and a caret underline.

use std::io::IsTerminal;

use annotate_snippets::{Level, Renderer, Snippet};
use mold_hir::{Diagnostic, Severity};

fn level_of(severity: Severity) -> Level {
    match severity {
        Severity::Error => Level::Error,
        Severity::Warning => Level::Warning,
        Severity::Info => Level::Info,
        Severity::Hint => Level::Note,
        _ => Level::Warning,
    }
}

/// Renders all diagnostics for one source file as a single string.
#[must_use]
pub fn render(origin: &str, source: &str, diagnostics: &[Diagnostic]) -> String {
    let renderer = if std::io::stdout().is_terminal() {
        Renderer::styled()
    } else {
        Renderer::plain()
    };

    let mut out = String::new();
    for diag in diagnostics {
        let level = level_of(diag.severity);
        let (start, end) = diag
            .range
            .map(|r| (u32::from(r.start()) as usize, u32::from(r.end()) as usize))
            .unwrap_or((0, 0));
        // Clamp to the source bounds so a stale range never panics the renderer.
        let start = start.min(source.len());
        let end = end.min(source.len()).max(start);

        let snippet = Snippet::source(source)
            .origin(origin)
            .fold(true)
            .annotation(level.span(start..end).label(&diag.message));

        let mut message = level.title(&diag.message).snippet(snippet);
        if let Some(code) = &diag.code {
            message = message.id(code);
        }

        let help_note;
        if let Some(help) = &diag.help {
            help_note = Level::Help.title(help.as_str());
            message = message.footer(help_note);
        }

        let fix_note;
        if !diag.fixes.is_empty() {
            fix_note = Level::Note.title("a fix is available; run `mold fix`");
            message = message.footer(fix_note);
        }

        out.push_str(&renderer.render(message).to_string());
        out.push('\n');
    }
    out
}
