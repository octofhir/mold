//! `banshee complete` — debug helper that prints completion items at an offset.

use std::path::PathBuf;

use anyhow::{Result, bail};
use banshee_completion::CompletionRequest;
use clap::Args;
use text_size::TextSize;

use super::io::{self, gather_inputs};
use super::{Cli, exit};

#[derive(Args, Debug)]
pub struct CompleteArgs {
    /// File to complete in. Use `-` or omit to read stdin.
    paths: Vec<PathBuf>,

    /// Byte offset of the cursor.
    #[arg(long)]
    offset: u32,

    /// Maximum number of items to print.
    #[arg(long, default_value_t = 25)]
    limit: usize,
}

pub fn run(args: &CompleteArgs, cli: &Cli) -> Result<u8> {
    let config = cli.load_config(&io::discovery_anchor(&args.paths))?;
    let inputs = gather_inputs(&args.paths)?;
    let Some(input) = inputs.into_iter().next() else {
        bail!("no input provided");
    };

    let provider = super::schema::resolve(&config)?;

    let mut request = CompletionRequest::new(&input.text, TextSize::from(args.offset));
    if let Some(p) = &provider {
        request = request.with_schema_provider(p).with_function_provider(p);
    }
    let result = banshee_completion::complete(request);

    if result.items.is_empty() {
        println!("(no completions at offset {})", args.offset);
        return Ok(exit::OK);
    }

    for item in result.items.iter().take(args.limit) {
        let detail = item.detail.as_deref().unwrap_or("");
        println!("{:<24} {:?}  {}", item.label, item.kind, detail);
    }
    Ok(exit::OK)
}
