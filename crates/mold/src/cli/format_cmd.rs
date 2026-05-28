//! `mold format` — format SQL to stdout, in place, or check formatting.

use std::path::PathBuf;

use anyhow::{Result, bail};
use clap::Args;
use rayon::prelude::*;

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

    /// In --check mode, do not print the per-file "would reformat" lines.
    #[arg(long)]
    quiet: bool,
}

pub fn run(args: &FormatArgs, cli: &Cli) -> Result<u8> {
    let config = cli.load_config(&io::discovery_anchor(&args.paths))?;
    let inputs = gather_inputs(&args.paths)?;

    // Format every input in parallel; results stay aligned with `inputs`.
    let formatted: Vec<String> = inputs.par_iter().map(|i| config.format(&i.text)).collect();

    let mut unformatted = 0usize;
    for (input, formatted) in inputs.iter().zip(&formatted) {
        if args.check {
            if *formatted != input.text {
                unformatted += 1;
                if !args.quiet {
                    eprintln!("would reformat: {}", input.label);
                }
            }
            continue;
        }

        if args.write {
            let Some(path) = &input.path else {
                bail!("--write requires file inputs, not stdin");
            };
            if *formatted != input.text {
                std::fs::write(path, formatted)?;
            }
        } else {
            print!("{formatted}");
        }
    }

    if args.check && unformatted > 0 {
        if !args.quiet {
            eprintln!("{unformatted} file(s) need formatting");
        }
        return Ok(exit::FINDINGS);
    }
    Ok(exit::OK)
}
