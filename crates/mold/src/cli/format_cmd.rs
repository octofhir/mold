//! `mold format` — format SQL to stdout, in place, or check formatting.

use std::path::PathBuf;

use anyhow::{Result, bail};
use clap::Args;

use super::io::{self, gather_inputs};
use super::{Cli, exit};

#[derive(Args, Debug)]
pub struct FormatArgs {
    /// Files or directories to format. Use `-` or omit to read stdin.
    paths: Vec<PathBuf>,

    /// Rewrite files in place instead of printing to stdout.
    #[arg(long, conflicts_with = "check")]
    write: bool,

    /// Exit non-zero if any input is not already formatted (no output).
    #[arg(long)]
    check: bool,
}

pub fn run(args: &FormatArgs, cli: &Cli) -> Result<u8> {
    let config = cli.load_config(&io::discovery_anchor(&args.paths))?;
    let fmt_config = config.format_config();
    let inputs = gather_inputs(&args.paths)?;

    let mut unformatted = 0usize;
    for input in &inputs {
        let formatted = mold_format::format(&input.text, &fmt_config);

        if args.check {
            if formatted != input.text {
                unformatted += 1;
                eprintln!("would reformat: {}", input.label);
            }
            continue;
        }

        if args.write {
            let Some(path) = &input.path else {
                bail!("--write requires file inputs, not stdin");
            };
            if formatted != input.text {
                std::fs::write(path, &formatted)?;
            }
        } else {
            print!("{formatted}");
        }
    }

    if args.check && unformatted > 0 {
        eprintln!("{unformatted} file(s) need formatting");
        return Ok(exit::FINDINGS);
    }
    Ok(exit::OK)
}
