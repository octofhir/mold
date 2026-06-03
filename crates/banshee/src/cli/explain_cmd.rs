//! `banshee explain <CODE>` — print the long-form description of a lint rule.

use anyhow::Result;
use clap::Args;

use super::exit;
use super::rules_cmd;

#[derive(Args, Debug)]
pub struct ExplainArgs {
    /// Rule code to explain (e.g. `AM05`). Case-insensitive.
    code: String,
}

pub fn run(args: &ExplainArgs) -> Result<u8> {
    let Some(rule) = rules_cmd::find(&args.code) else {
        eprintln!(
            "unknown rule '{}'. Run `banshee rules` to list available rules.",
            args.code
        );
        return Ok(exit::ERROR);
    };

    let fixable = if rule.fixable { " (auto-fixable)" } else { "" };
    println!("{}: {}{}\n", rule.code, rule.summary, fixable);
    println!("{}", rule.explanation);
    Ok(exit::OK)
}
