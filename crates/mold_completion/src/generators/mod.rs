//! Completion generators.
//!
//! Each generator produces completion items for a specific context.

pub mod columns;
pub mod functions;
pub mod jsonb;
pub mod keywords;
pub mod tables;

pub use columns::complete_columns;
pub use functions::{complete_functions, get_jsonb_arg_completion, JsonbArgCompletion};
pub use jsonb::{complete_jsonb_paths, complete_jsonpath};
pub use keywords::complete_keywords;
pub use tables::complete_tables;
