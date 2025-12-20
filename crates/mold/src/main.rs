//! SQL Formatter CLI
//!
//! A simple command-line tool to format SQL using mold_format.

use std::io::{self, Read, Write};

// ANSI color codes
const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Parse command-line options
    let mut compact = false;
    let mut sql_arg: Option<String> = None;
    let mut no_color = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_help();
                return;
            }
            "-c" | "--compact" => {
                compact = true;
            }
            "--no-color" => {
                no_color = true;
            }
            "-" => {
                // Read from stdin
                sql_arg = Some(read_stdin());
            }
            arg if arg.starts_with('-') => {
                eprintln!("Unknown option: {}", arg);
                eprintln!("Use --help for usage information");
                std::process::exit(1);
            }
            _ => {
                sql_arg = Some(args[i].clone());
            }
        }
        i += 1;
    }

    // Get SQL input
    let sql = match sql_arg {
        Some(s) => s,
        None => {
            // If no argument provided, read from stdin
            if atty::is(atty::Stream::Stdin) {
                // Interactive mode - show help
                print_help();
                return;
            }
            read_stdin()
        }
    };

    // Check if we should use colors (TTY and not disabled)
    let use_color = !no_color && atty::is(atty::Stream::Stdout);

    // Parse the SQL to check for errors
    let parse = mold_parser::parse(&sql);
    let errors = parse.errors();

    if !errors.is_empty() {
        // Print errors in red
        if use_color {
            eprint!("{RED}{BOLD}Error:{RESET} ");
        } else {
            eprint!("Error: ");
        }
        eprintln!("Failed to parse SQL");

        for error in errors {
            if use_color {
                eprintln!("  {RED}•{RESET} {} at offset {}", error.message, u32::from(error.range.start()));
            } else {
                eprintln!("  • {} at offset {}", error.message, u32::from(error.range.start()));
            }
        }

        // Still output the best-effort formatted result
        let formatted = if compact {
            mold_format::format_compact(&sql)
        } else {
            mold_format::format_sqlstyle(&sql)
        };
        eprintln!();
        print!("{}", formatted);
        std::process::exit(1);
    }

    // Format the SQL
    let formatted = if compact {
        mold_format::format_compact(&sql)
    } else {
        mold_format::format_sqlstyle(&sql)
    };

    // Print result with optional green label
    if use_color {
        println!("{GREEN}{BOLD}Result:{RESET}");
    }
    print!("{}", formatted);
    let _ = io::stdout().flush();
}

fn read_stdin() -> String {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read from stdin");
    input
}

fn print_help() {
    eprintln!(
        r#"mold - SQL Formatter

USAGE:
    mold [OPTIONS] [SQL]
    echo "SELECT * FROM users" | mold

OPTIONS:
    -h, --help      Print this help message
    -c, --compact   Use compact formatting (default: sqlstyle.guide)
    --no-color      Disable colored output
    -               Read SQL from stdin

EXAMPLES:
    # Format SQL from argument
    mold "select id,name from users where active=true"

    # Format SQL from stdin
    echo "select * from users" | mold

    # Format SQL file
    cat query.sql | mold

    # Use compact formatting
    mold -c "select id, name from users"
"#
    );
}
