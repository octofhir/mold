//! `banshee parse` — dump the concrete syntax tree for debugging.

use std::path::PathBuf;

use anyhow::Result;
use banshee_syntax::SyntaxNode;
use clap::{Args, ValueEnum};

use super::io::{self, gather_inputs, line_col};
use super::{Cli, exit};

#[derive(ValueEnum, Clone, Copy, Debug, PartialEq, Eq)]
pub enum TreeFormat {
    /// Indented, human-readable tree (the parser's debug form).
    Tree,
    /// Nested JSON of nodes and tokens.
    Json,
}

#[derive(Args, Debug)]
pub struct ParseArgs {
    /// Files to parse. Use `-` or omit to read stdin.
    paths: Vec<PathBuf>,

    /// Output format.
    #[arg(long, value_enum, default_value_t = TreeFormat::Tree)]
    format: TreeFormat,
}

pub fn run(args: &ParseArgs, _cli: &Cli) -> Result<u8> {
    let inputs = gather_inputs(&args.paths)?;

    let mut had_errors = false;
    for input in &inputs {
        let parse = banshee_parser::parse(&input.text);
        let root = parse.syntax();

        match args.format {
            TreeFormat::Tree => print!("{root:#?}"),
            TreeFormat::Json => {
                let mut out = String::new();
                write_node_json(&root, &mut out);
                println!("{out}");
            }
        }

        for err in parse.errors() {
            had_errors = true;
            let (line, col) = line_col(&input.text, u32::from(err.range.start()));
            eprintln!(
                "{}:{}:{}: parse error: {}",
                input.label, line, col, err.message
            );
        }
    }

    Ok(if had_errors { exit::FINDINGS } else { exit::OK })
}

fn write_node_json(node: &SyntaxNode, out: &mut String) {
    let range = node.text_range();
    out.push_str(&format!(
        r#"{{"kind":"{:?}","start":{},"end":{},"children":["#,
        node.kind(),
        u32::from(range.start()),
        u32::from(range.end())
    ));
    let mut first = true;
    for child in node.children_with_tokens() {
        if !first {
            out.push(',');
        }
        first = false;
        if let Some(t) = child.as_token() {
            let r = t.text_range();
            out.push_str(&format!(
                r#"{{"kind":"{:?}","start":{},"end":{},"text":{}}}"#,
                t.kind(),
                u32::from(r.start()),
                u32::from(r.end()),
                io::json_escape(t.text())
            ));
        } else if let Some(n) = child.as_node() {
            write_node_json(n, out);
        }
    }
    out.push_str("]}");
}
