//! SQL Formatter CLI
//!
//! A simple command-line tool to format SQL using mold_format.

use std::io::{self, Read, Write};

use mold_format::{CaseOption, PgFormatterConfig};

// ANSI color codes
const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const BOLD: &str = "\x1b[1m";
const RESET: &str = "\x1b[0m";

/// Formatting style options
#[derive(Debug, Clone, Copy, Default)]
enum Style {
    #[default]
    SqlStyle,
    PgFormatter,
    Compact,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    // Parse command-line options
    let mut style = Style::default();
    let mut sql_arg: Option<String> = None;
    let mut no_color = false;
    let mut pg_config = PgFormatterConfig::default();

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_help();
                return;
            }
            "-c" | "--compact" => {
                style = Style::Compact;
            }
            "-s" | "--style" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --style requires a value (sqlstyle, pgformatter, compact)");
                    std::process::exit(1);
                }
                style = match args[i].to_lowercase().as_str() {
                    "sqlstyle" | "sql" => Style::SqlStyle,
                    "pgformatter" | "pg" => Style::PgFormatter,
                    "compact" => Style::Compact,
                    _ => {
                        eprintln!("Unknown style: {}. Use sqlstyle, pgformatter, or compact", args[i]);
                        std::process::exit(1);
                    }
                };
            }
            "--keyword-case" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --keyword-case requires a value (upper, lower, unchanged, capitalize)");
                    std::process::exit(1);
                }
                pg_config.keyword_case = parse_case_option(&args[i]);
            }
            "--function-case" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --function-case requires a value");
                    std::process::exit(1);
                }
                pg_config.function_case = parse_case_option(&args[i]);
            }
            "--type-case" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --type-case requires a value");
                    std::process::exit(1);
                }
                pg_config.type_case = parse_case_option(&args[i]);
            }
            "--spaces" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --spaces requires a number");
                    std::process::exit(1);
                }
                pg_config.spaces = args[i].parse().unwrap_or(4);
            }
            "--tabs" => {
                pg_config.use_tabs = true;
            }
            "--comma-start" => {
                pg_config.comma_start = true;
                pg_config.comma_end = false;
            }
            "--comma-end" => {
                pg_config.comma_start = false;
                pg_config.comma_end = true;
            }
            "--no-comment" => {
                pg_config.no_comment = true;
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
                eprintln!(
                    "  {RED}•{RESET} {} at offset {}",
                    error.message,
                    u32::from(error.range.start())
                );
            } else {
                eprintln!(
                    "  • {} at offset {}",
                    error.message,
                    u32::from(error.range.start())
                );
            }
        }

        // Still output the best-effort formatted result
        let formatted = format_sql(&sql, style, &pg_config);
        eprintln!();
        print!("{}", formatted);
        std::process::exit(1);
    }

    // Format the SQL
    let formatted = format_sql(&sql, style, &pg_config);

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

/// Formats SQL using the specified style.
fn format_sql(sql: &str, style: Style, pg_config: &PgFormatterConfig) -> String {
    match style {
        Style::SqlStyle => mold_format::format_sqlstyle(sql),
        Style::Compact => mold_format::format_compact(sql),
        Style::PgFormatter => mold_format::format_with_pgformatter(sql, pg_config),
    }
}

/// Parses a case option from a string.
fn parse_case_option(s: &str) -> CaseOption {
    match s.to_lowercase().as_str() {
        "upper" | "2" => CaseOption::Upper,
        "lower" | "1" => CaseOption::Lower,
        "unchanged" | "0" => CaseOption::Unchanged,
        "capitalize" | "3" => CaseOption::Capitalize,
        _ => {
            eprintln!("Unknown case option: {}. Using 'upper'", s);
            CaseOption::Upper
        }
    }
}

fn print_help() {
    eprintln!(
        r#"mold - SQL Formatter

USAGE:
    mold [OPTIONS] [SQL]
    echo "SELECT * FROM users" | mold

OPTIONS:
    -h, --help              Print this help message
    -s, --style <STYLE>     Formatting style: sqlstyle, pgformatter, compact
                            (default: sqlstyle)
    -c, --compact           Shorthand for --style compact
    --no-color              Disable colored output
    -                       Read SQL from stdin

PGFORMATTER OPTIONS (used with --style pgformatter):
    --keyword-case <CASE>   Keyword case: upper, lower, unchanged, capitalize
    --function-case <CASE>  Function name case
    --type-case <CASE>      Data type case
    --spaces <N>            Spaces per indent level (default: 4)
    --tabs                  Use tabs instead of spaces
    --comma-start           Place comma at start of line
    --comma-end             Place comma at end of line (default)
    --no-comment            Remove comments from output

EXAMPLES:
    # Format SQL with sqlstyle.guide (default)
    mold "select id,name from users where active=true"

    # Format SQL with pgFormatter style
    mold -s pg "select id, name from users"

    # Format with lowercase keywords
    mold -s pg --keyword-case lower "SELECT * FROM users"

    # Format SQL from stdin
    echo "select * from users" | mold

    # Format SQL file with pgFormatter
    cat query.sql | mold -s pgformatter

    # Use compact formatting
    mold -c "select id, name from users"
"#
    );
}
