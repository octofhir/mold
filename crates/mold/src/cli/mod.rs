//! Command-line interface for the mold SQL toolchain.
//!
//! Dispatches the `format`, `lint`, `fix`, `parse`, `rules` and `lsp`
//! subcommands. Each subcommand lives in its own module; shared input handling
//! and edit application live in [`io`].

mod analysis;
mod complete_cmd;
mod explain_cmd;
mod fix_cmd;
mod format_cmd;
mod init_cmd;
mod io;
mod lint_cmd;
mod parse_cmd;
mod render;
mod rules_cmd;
mod schema;

use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Parser, Subcommand, ValueEnum};
use mold_config::MoldConfig;

/// Process exit codes, forming the CI contract.
pub mod exit {
    /// Success: no findings, nothing to fix.
    pub const OK: u8 = 0;
    /// Findings present, or `--check` detected unformatted input.
    pub const FINDINGS: u8 = 1;
    /// A hard error (bad arguments, unreadable file, parse failure where fatal).
    pub const ERROR: u8 = 2;
}

/// PostgreSQL SQL parser, formatter and linter.
#[derive(Parser, Debug)]
#[command(name = "mold", version, about, long_about = None)]
pub struct Cli {
    /// Path to a `mold.toml`. When omitted, the nearest one is discovered.
    #[arg(long, global = true, value_name = "FILE")]
    config: Option<PathBuf>,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Format SQL.
    Format(format_cmd::FormatArgs),
    /// Lint SQL and report findings.
    Lint(lint_cmd::LintArgs),
    /// Apply autofixes to SQL.
    Fix(fix_cmd::FixArgs),
    /// Print the concrete syntax tree.
    Parse(parse_cmd::ParseArgs),
    /// List available lint rules.
    Rules,
    /// Scaffold a mold.toml in the current directory.
    Init(init_cmd::InitArgs),
    /// Explain a lint rule in detail.
    Explain(explain_cmd::ExplainArgs),
    /// Print completion items at a byte offset (debug helper).
    Complete(complete_cmd::CompleteArgs),
    /// Run the language server over stdio.
    Lsp,
}

/// Machine- or human-oriented output format for diagnostics.
#[derive(ValueEnum, Clone, Copy, Debug, PartialEq, Eq)]
pub enum ReportFormat {
    /// Colored, human-readable output.
    Human,
    /// One JSON object per run.
    Json,
    /// GitHub Actions workflow annotations.
    Github,
    /// SARIF 2.1.0 (for CI / code-scanning dashboards).
    Sarif,
}

impl Cli {
    /// Parses arguments and runs the selected command, returning an exit code.
    pub fn run() -> ExitCode {
        let cli = Cli::parse();
        let result = match &cli.command {
            Command::Format(args) => format_cmd::run(args, &cli),
            Command::Lint(args) => lint_cmd::run(args, &cli),
            Command::Fix(args) => fix_cmd::run(args, &cli),
            Command::Parse(args) => parse_cmd::run(args, &cli),
            Command::Rules => rules_cmd::run(),
            Command::Init(args) => init_cmd::run(args),
            Command::Explain(args) => explain_cmd::run(args),
            Command::Complete(args) => complete_cmd::run(args, &cli),
            Command::Lsp => run_lsp(&cli),
        };
        match result {
            Ok(code) => ExitCode::from(code),
            Err(err) => {
                eprintln!("error: {err:#}");
                ExitCode::from(exit::ERROR)
            }
        }
    }

    /// Resolves the effective configuration for this invocation.
    ///
    /// An explicit `--config` wins; otherwise discovery walks up from `near`
    /// (typically the first input file or the current directory).
    fn load_config(&self, near: &std::path::Path) -> anyhow::Result<MoldConfig> {
        match &self.config {
            Some(path) => Ok(MoldConfig::load(path)?),
            None => Ok(MoldConfig::discover(near)?),
        }
    }
}

fn run_lsp(cli: &Cli) -> anyhow::Result<u8> {
    let config = cli.load_config(&PathBuf::from("."))?;
    let schema = schema::resolve(&config)?;
    mold_lsp::run_stdio(config, schema)?;
    Ok(exit::OK)
}
