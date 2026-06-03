//! `banshee init` — scaffold a `banshee.toml` in the current directory.

use std::path::Path;

use anyhow::{Result, bail};
use banshee_config::CONFIG_FILE_NAME;
use clap::Args;

use super::exit;

/// Starter config written by `banshee init`.
const TEMPLATE: &str = r#"# banshee configuration. See `banshee rules` for lint codes.

[format]
# Base preset: "sqlstyle", "pgformatter", or "compact". Individual knobs
# (keyword-case, indent-width, comma-style, …) may override it.
style = "sqlstyle"

[lint]
enabled = true
# exclude = ["AM04"]

# [lint.rules.CP01]
# severity = "warning"

# Uncomment to enable schema-aware checks and JSONB completion against a live
# database. The connection string is read from the named environment variable.
# [database]
# url-env = "DATABASE_URL"
# schema = "public"
"#;

#[derive(Args, Debug)]
pub struct InitArgs {
    /// Overwrite an existing banshee.toml.
    #[arg(long)]
    force: bool,
}

pub fn run(args: &InitArgs) -> Result<u8> {
    let path = Path::new(CONFIG_FILE_NAME);
    if path.exists() && !args.force {
        bail!("{CONFIG_FILE_NAME} already exists; pass --force to overwrite");
    }
    std::fs::write(path, TEMPLATE)?;
    println!("wrote {CONFIG_FILE_NAME}");
    Ok(exit::OK)
}
