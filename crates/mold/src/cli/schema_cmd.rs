//! `mold schema` — inspect and manage the `.mold/` schema cache.

use anyhow::Result;
use clap::{Args, Subcommand};
use mold_schema::cache;

use super::schema;
use super::{Cli, exit};

#[derive(Args, Debug)]
pub struct SchemaArgs {
    #[command(subcommand)]
    command: SchemaCommand,
}

#[derive(Subcommand, Debug)]
enum SchemaCommand {
    /// Re-introspect the database and rewrite the cache (needs `db` build).
    Refresh,
    /// Print the cached schema (tables, columns, JSONB shapes).
    Show {
        /// Only show tables whose name contains this substring.
        #[arg(long)]
        table: Option<String>,
    },
    /// Delete the `.mold/` cache directory.
    Clear,
    /// Print the cache path, fingerprint and freshness.
    Where,
}

pub fn run(args: &SchemaArgs, cli: &Cli) -> Result<u8> {
    let config = cli.load_config(&schema::base_dir())?;
    match &args.command {
        SchemaCommand::Refresh => refresh(&config),
        SchemaCommand::Show { table } => show(table.as_deref()),
        SchemaCommand::Clear => clear(),
        SchemaCommand::Where => locate(&config),
    }
}

fn refresh(config: &mold_config::MoldConfig) -> Result<u8> {
    match schema::force_refresh(config)? {
        Some(snap) => {
            println!(
                "refreshed: {} tables from schema '{}'",
                snap.tables.len(),
                snap.default_schema
            );
            Ok(exit::OK)
        }
        None => {
            eprintln!("no [database] connection configured; nothing to refresh");
            Ok(exit::ERROR)
        }
    }
}

fn show(table_filter: Option<&str>) -> Result<u8> {
    let Some(snap) = cache::load(&schema::base_dir()) else {
        eprintln!("no schema cache; run `mold schema refresh` first");
        return Ok(exit::ERROR);
    };

    let mut shown = 0usize;
    for table in &snap.tables {
        if let Some(f) = table_filter
            && !table.name.contains(f)
        {
            continue;
        }
        shown += 1;
        println!("{}.{}", table.schema, table.name);
        for col in &table.columns {
            let pk = if col.is_primary_key { " PK" } else { "" };
            let null = if col.nullable { "" } else { " NOT NULL" };
            println!("    {} {}{}{}", col.name, col.data_type, null, pk);
            if let Some(shape) = &col.jsonb {
                for field in &shape.fields {
                    println!("        ->'{}' : {:?}", field.name, field.ty);
                }
            }
        }
    }
    if shown == 0 {
        eprintln!("no matching tables in cache");
    }
    Ok(exit::OK)
}

fn clear() -> Result<u8> {
    let dir = schema::base_dir().join(cache::CACHE_DIR);
    if dir.exists() {
        std::fs::remove_dir_all(&dir)?;
        println!("removed {}", dir.display());
    } else {
        println!("no cache to remove");
    }
    Ok(exit::OK)
}

fn locate(config: &mold_config::MoldConfig) -> Result<u8> {
    let path = cache::cache_path(&schema::base_dir());
    println!("cache path: {}", path.display());
    match schema::config_fingerprint(config) {
        Some(fp) => println!("config fingerprint: {fp}"),
        None => println!("config fingerprint: <no database configured>"),
    }
    match cache::load(&schema::base_dir()) {
        Some(snap) => println!(
            "cached: fingerprint {}, {} tables",
            snap.fingerprint,
            snap.tables.len()
        ),
        None => println!("cached: <none>"),
    }
    Ok(exit::OK)
}
