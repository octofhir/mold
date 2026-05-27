//! mold — PostgreSQL SQL parser, formatter and linter CLI.

mod cli;

use std::process::ExitCode;

fn main() -> ExitCode {
    cli::Cli::run()
}
