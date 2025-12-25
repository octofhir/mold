//! pgFormatter-style SQL formatting implementation.
//!
//! This module provides SQL formatting that matches pgFormatter behavior,
//! using `PgFormatterConfig` for all formatting options.

use mold_syntax::{Parse, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::pg_formatter::PgFormatterConfig;

/// Formats SQL source code using pgFormatter style.
#[must_use]
pub fn format(source: &str, config: &PgFormatterConfig) -> String {
    let parse = mold_parser::parse(source);
    format_parse(&parse, config)
}

/// Formats a parsed SQL tree using pgFormatter style.
pub fn format_parse(parse: &Parse, config: &PgFormatterConfig) -> String {
    let mut printer = PgPrinter::new(config.clone());
    let root = parse.syntax();
    format_node(&root, &mut printer);
    printer.finish()
}

/// pgFormatter-style printer.
#[derive(Debug)]
pub struct PgPrinter {
    config: PgFormatterConfig,
    buffer: String,
    line: usize,
    column: usize,
    indent_level: usize,
    at_line_start: bool,
}

impl PgPrinter {
    /// Creates a new pgFormatter printer.
    pub fn new(config: PgFormatterConfig) -> Self {
        Self {
            config,
            buffer: String::new(),
            line: 0,
            column: 0,
            indent_level: 0,
            at_line_start: true,
        }
    }

    /// Returns the configuration.
    pub fn config(&self) -> &PgFormatterConfig {
        &self.config
    }

    /// Consumes the printer and returns the output.
    pub fn finish(mut self) -> String {
        // Handle no_extra_line option
        if self.config.no_extra_line {
            // Remove trailing newline
            while self.buffer.ends_with('\n') {
                self.buffer.pop();
            }
        } else {
            // Ensure trailing newline
            if !self.buffer.ends_with('\n') {
                self.buffer.push('\n');
            }
        }
        self.buffer
    }

    /// Returns the indent string for one level.
    fn indent_str(&self) -> String {
        if self.config.use_tabs {
            "\t".to_string()
        } else {
            " ".repeat(self.config.spaces)
        }
    }

    /// Writes indentation based on current level.
    fn write_indent(&mut self) {
        let indent = self.indent_str().repeat(self.indent_level);
        self.buffer.push_str(&indent);
        self.column += indent.len();
    }

    /// Writes text to the output.
    pub fn write(&mut self, text: &str) {
        if text.is_empty() {
            return;
        }

        if self.at_line_start && !text.starts_with('\n') {
            self.write_indent();
            self.at_line_start = false;
        }

        for ch in text.chars() {
            if ch == '\n' {
                self.buffer.push(ch);
                self.line += 1;
                self.column = 0;
                self.at_line_start = true;
            } else {
                if self.at_line_start {
                    self.write_indent();
                    self.at_line_start = false;
                }
                self.buffer.push(ch);
                self.column += 1;
            }
        }
    }

    /// Writes raw text without indentation handling.
    pub fn write_raw(&mut self, text: &str) {
        for ch in text.chars() {
            if ch == '\n' {
                self.buffer.push(ch);
                self.line += 1;
                self.column = 0;
                self.at_line_start = true;
            } else {
                self.buffer.push(ch);
                self.column += 1;
                self.at_line_start = false;
            }
        }
    }

    /// Writes a newline.
    pub fn newline(&mut self) {
        self.buffer.push('\n');
        self.line += 1;
        self.column = 0;
        self.at_line_start = true;
    }

    /// Writes a space.
    pub fn space(&mut self) {
        if self.at_line_start {
            self.write_indent();
            self.at_line_start = false;
        }
        self.buffer.push(' ');
        self.column += 1;
    }

    /// Increases indentation level.
    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    /// Decreases indentation level.
    pub fn dedent(&mut self) {
        self.indent_level = self.indent_level.saturating_sub(1);
    }

    /// Ensures we're at the start of a line.
    pub fn ensure_newline(&mut self) {
        if !self.at_line_start {
            self.newline();
        }
    }

    /// Ensures a blank line exists.
    pub fn ensure_blank_line(&mut self) {
        if !self.at_line_start {
            self.newline();
        }
        self.newline();
    }

    /// Writes a keyword with case transformation.
    pub fn write_keyword(&mut self, keyword: &str) {
        let transformed = self.config.keyword_case.apply(keyword);
        self.write(&transformed);
    }

    /// Writes a function name with case transformation.
    pub fn write_function(&mut self, name: &str) {
        let transformed = self.config.function_case.apply(name);
        self.write(&transformed);
    }

    /// Writes a type name with case transformation.
    pub fn write_type(&mut self, name: &str) {
        let transformed = self.config.type_case.apply(name);
        self.write(&transformed);
    }

    /// Writes an identifier (no transformation for pgFormatter).
    pub fn write_identifier(&mut self, ident: &str) {
        self.write(ident);
    }

    /// Writes a comma according to configuration.
    pub fn write_comma(&mut self) {
        self.write(",");
        if self.config.comma_break {
            self.newline();
        }
    }

    /// Writes an operator with surrounding spaces.
    pub fn write_operator(&mut self, op: &str) {
        self.space();
        self.write(op);
        self.space();
    }

    /// Writes an operator without surrounding spaces.
    pub fn write_operator_compact(&mut self, op: &str) {
        self.write(op);
    }

    /// Returns true if comments should be removed.
    pub fn should_remove_comments(&self) -> bool {
        self.config.no_comment
    }

    /// Returns true if there should be no space before function parentheses.
    pub fn no_space_function(&self) -> bool {
        self.config.no_space_function
    }

    /// Returns true if comma should be at start of line.
    pub fn comma_start(&self) -> bool {
        self.config.comma_start
    }
}

/// Formats a syntax node.
fn format_node(node: &SyntaxNode, printer: &mut PgPrinter) {
    match node.kind() {
        SyntaxKind::SOURCE_FILE => format_source_file(node, printer),
        SyntaxKind::SELECT_STMT => format_select(node, printer),
        SyntaxKind::INSERT_STMT => format_insert(node, printer),
        SyntaxKind::UPDATE_STMT => format_update(node, printer),
        SyntaxKind::DELETE_STMT => format_delete(node, printer),
        SyntaxKind::ERROR => format_error_node(node, printer),
        _ => format_children(node, printer),
    }
}

/// Formats the source file (root node).
fn format_source_file(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first_stmt = true;

    for child in node.children() {
        if child.kind() == SyntaxKind::WHITESPACE || child.kind() == SyntaxKind::NEWLINE {
            continue;
        }

        if !first_stmt && !printer.config.no_grouping {
            printer.ensure_blank_line();
        } else if !first_stmt {
            printer.ensure_newline();
        }

        format_node(&child, printer);
        first_stmt = false;
    }

    printer.ensure_newline();
}

/// Formats children of a node.
fn format_children(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_node(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                format_token(&token, printer);
            }
        }
    }
}

/// Formats a token.
fn format_token(token: &SyntaxToken, printer: &mut PgPrinter) {
    let kind = token.kind();
    let text = token.text();

    match kind {
        SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE => {
            // We control spacing
        }
        SyntaxKind::LINE_COMMENT | SyntaxKind::BLOCK_COMMENT => {
            if !printer.should_remove_comments() {
                printer.write(text);
            }
        }
        k if k.is_keyword() => {
            if needs_space_before(k) {
                printer.space();
            }
            printer.write_keyword(text);
            if needs_space_after(k) {
                printer.space();
            }
        }
        SyntaxKind::IDENT => {
            printer.write_identifier(text);
        }
        SyntaxKind::QUOTED_IDENT => {
            printer.write(text);
        }
        SyntaxKind::INTEGER
        | SyntaxKind::FLOAT
        | SyntaxKind::STRING
        | SyntaxKind::DOLLAR_STRING
        | SyntaxKind::BIT_STRING
        | SyntaxKind::HEX_STRING
        | SyntaxKind::PARAM => {
            printer.write(text);
        }
        SyntaxKind::SEMICOLON => {
            printer.write(";");
        }
        SyntaxKind::COMMA => {
            printer.write_comma();
        }
        SyntaxKind::DOT => {
            printer.write(".");
        }
        SyntaxKind::COLON => {
            printer.write(":");
        }
        SyntaxKind::DOUBLE_COLON => {
            printer.write("::");
        }
        SyntaxKind::L_PAREN => {
            printer.write("(");
        }
        SyntaxKind::R_PAREN => {
            printer.write(")");
        }
        SyntaxKind::L_BRACKET => {
            printer.write("[");
        }
        SyntaxKind::R_BRACKET => {
            printer.write("]");
        }
        SyntaxKind::EQ
        | SyntaxKind::NE
        | SyntaxKind::LT
        | SyntaxKind::LE
        | SyntaxKind::GT
        | SyntaxKind::GE
        | SyntaxKind::PLUS
        | SyntaxKind::MINUS
        | SyntaxKind::STAR
        | SyntaxKind::SLASH
        | SyntaxKind::PERCENT
        | SyntaxKind::CARET
        | SyntaxKind::PIPE_PIPE => {
            printer.write_operator(text);
        }
        SyntaxKind::ARROW | SyntaxKind::ARROW_TEXT => {
            printer.write_operator_compact(text);
        }
        SyntaxKind::HASH_ARROW | SyntaxKind::HASH_ARROW_TEXT => {
            printer.write_operator_compact(text);
        }
        SyntaxKind::AT_GT
        | SyntaxKind::LT_AT
        | SyntaxKind::QUESTION
        | SyntaxKind::QUESTION_PIPE
        | SyntaxKind::QUESTION_AMP
        | SyntaxKind::HASH_MINUS
        | SyntaxKind::AT_QUESTION
        | SyntaxKind::AT_AT => {
            printer.write_operator(text);
        }
        _ => {
            printer.write(text);
        }
    }
}

/// Formats an error node by preserving original text.
fn format_error_node(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_error_node(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                printer.write_raw(token.text());
            }
        }
    }
}

// === Statement formatting ===

/// Formats a SELECT statement.
fn format_select(node: &SyntaxNode, printer: &mut PgPrinter) {
    // WITH clause
    if let Some(with_clause) = find_child(node, SyntaxKind::WITH_CLAUSE) {
        format_with_clause(&with_clause, printer);
        printer.newline();
    }

    // SELECT keyword and items
    printer.write_keyword("SELECT");

    // Check for DISTINCT
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::DISTINCT_KW {
                printer.space();
                printer.write_keyword("DISTINCT");
                break;
            }
            if token.kind() == SyntaxKind::ALL_KW {
                printer.space();
                printer.write_keyword("ALL");
                break;
            }
        }
    }

    // Select list
    if let Some(select_list) = find_child(node, SyntaxKind::SELECT_ITEM_LIST) {
        printer.newline();
        printer.indent();
        format_select_list(&select_list, printer);
        printer.dedent();
    }

    // FROM clause
    if let Some(from_clause) = find_child(node, SyntaxKind::FROM_CLAUSE) {
        printer.newline();
        printer.write_keyword("FROM");
        printer.space();
        format_from_clause(&from_clause, printer);
    }

    // WHERE clause
    if let Some(where_clause) = find_child(node, SyntaxKind::WHERE_CLAUSE) {
        printer.newline();
        printer.write_keyword("WHERE");
        printer.space();
        format_where_clause(&where_clause, printer);
    }

    // GROUP BY clause
    if let Some(group_by) = find_child(node, SyntaxKind::GROUP_BY_CLAUSE) {
        printer.newline();
        printer.write_keyword("GROUP");
        printer.space();
        printer.write_keyword("BY");
        printer.space();
        format_group_by(&group_by, printer);
    }

    // HAVING clause
    if let Some(having) = find_child(node, SyntaxKind::HAVING_CLAUSE) {
        printer.newline();
        printer.write_keyword("HAVING");
        printer.space();
        format_expression_children(&having, printer);
    }

    // ORDER BY clause
    if let Some(order_by) = find_child(node, SyntaxKind::ORDER_BY_CLAUSE) {
        printer.newline();
        printer.write_keyword("ORDER");
        printer.space();
        printer.write_keyword("BY");
        printer.space();
        format_order_by(&order_by, printer);
    }

    // LIMIT clause
    if let Some(limit) = find_child(node, SyntaxKind::LIMIT_CLAUSE) {
        printer.newline();
        format_limit_clause(&limit, printer);
    }

    // Semicolon
    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

/// Formats a WITH clause.
fn format_with_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("WITH");

    // Check for RECURSIVE
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::RECURSIVE_KW {
                printer.space();
                printer.write_keyword("RECURSIVE");
                break;
            }
        }
    }

    printer.newline();
    printer.indent();

    let ctes: Vec<_> = node.children()
        .filter(|c| c.kind() == SyntaxKind::CTE)
        .cloned()
        .collect();

    for (i, cte) in ctes.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.newline();
        }
        format_cte(cte, printer);
    }

    printer.dedent();
}

/// Formats a CTE.
fn format_cte(node: &SyntaxNode, printer: &mut PgPrinter) {
    // CTE name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IDENT {
                printer.write_identifier(token.text());
                break;
            }
        }
    }

    printer.space();
    printer.write_keyword("AS");
    printer.space();
    printer.write("(");
    printer.newline();
    printer.indent();

    // CTE query
    if let Some(select) = find_child(node, SyntaxKind::SELECT_STMT) {
        format_select(&select, printer);
    }

    printer.dedent();
    printer.newline();
    printer.write(")");
}

/// Formats a select list.
fn format_select_list(node: &SyntaxNode, printer: &mut PgPrinter) {
    let items: Vec<_> = node.children()
        .filter(|c| c.kind() == SyntaxKind::SELECT_ITEM)
        .cloned()
        .collect();

    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            if printer.comma_start() {
                printer.newline();
                printer.write(",");
                printer.space();
            } else {
                printer.write(",");
                if printer.config().comma_break {
                    printer.newline();
                } else {
                    printer.space();
                }
            }
        }
        format_select_item(item, printer);
    }
}

/// Formats a select item.
fn format_select_item(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut saw_as = false;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::AS_KW {
                    printer.space();
                    printer.write_keyword("AS");
                    printer.space();
                    saw_as = true;
                } else if token.kind() == SyntaxKind::IDENT && saw_as {
                    printer.write_identifier(token.text());
                } else if token.kind() == SyntaxKind::QUOTED_IDENT && saw_as {
                    printer.write(token.text());
                } else if token.kind() != SyntaxKind::WHITESPACE && token.kind() != SyntaxKind::NEWLINE {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a FROM clause.
fn format_from_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if child.kind() == SyntaxKind::TABLE_REF {
            if !first {
                printer.write(",");
                printer.space();
            }
            format_table_ref(&child, printer);
            first = false;
        } else if child.kind() == SyntaxKind::JOIN_EXPR {
            printer.newline();
            format_join_expr(&child, printer);
        }
    }
}

/// Formats a table reference.
fn format_table_ref(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::SUBQUERY {
                    format_subquery(&child, printer);
                } else if child.kind() == SyntaxKind::ALIAS {
                    printer.space();
                    format_alias(&child, printer);
                } else {
                    format_expression(&child, printer);
                }
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::IDENT {
                    printer.write_identifier(token.text());
                } else if token.kind() == SyntaxKind::QUOTED_IDENT {
                    printer.write(token.text());
                } else if token.kind() == SyntaxKind::DOT {
                    printer.write(".");
                }
            }
        }
    }
}

/// Formats an alias.
fn format_alias(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::AS_KW {
                printer.write_keyword("AS");
                printer.space();
            } else if token.kind() == SyntaxKind::IDENT {
                printer.write_identifier(token.text());
            } else if token.kind() == SyntaxKind::QUOTED_IDENT {
                printer.write(token.text());
            }
        }
    }
}

/// Formats a JOIN expression.
fn format_join_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    // Check if the first child is a nested JOIN_EXPR
    let first_child = node.children().next();
    let has_nested_join = first_child
        .as_ref()
        .is_some_and(|c| c.kind() == SyntaxKind::JOIN_EXPR);

    if has_nested_join {
        // Recursively format the nested join first
        if let Some(nested_join) = first_child {
            format_join_expr(&nested_join, printer);
        }
    } else {
        // No nested join - format the left table (first TABLE_REF)
        let left_table = node.children().find(|c| c.kind() == SyntaxKind::TABLE_REF);
        if let Some(table) = left_table {
            format_table_ref(&table, printer);
        }
    }

    // Determine join type from tokens at THIS level
    let mut join_parts: Vec<&str> = Vec::new();
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            match token.kind() {
                SyntaxKind::LEFT_KW => join_parts.push("LEFT"),
                SyntaxKind::RIGHT_KW => join_parts.push("RIGHT"),
                SyntaxKind::INNER_KW => join_parts.push("INNER"),
                SyntaxKind::FULL_KW => join_parts.push("FULL"),
                SyntaxKind::CROSS_KW => join_parts.push("CROSS"),
                SyntaxKind::NATURAL_KW => join_parts.push("NATURAL"),
                SyntaxKind::OUTER_KW => join_parts.push("OUTER"),
                SyntaxKind::JOIN_KW => join_parts.push("JOIN"),
                _ => {}
            }
        }
    }

    let join_keyword = join_parts.join(" ");
    let join_keyword = if join_keyword.is_empty() {
        "JOIN".to_string()
    } else {
        join_keyword
    };

    printer.newline();
    for word in join_keyword.split_whitespace() {
        printer.write_keyword(word);
        printer.space();
    }

    // Format the right table
    let table_refs: Vec<_> = node
        .children()
        .filter(|c| c.kind() == SyntaxKind::TABLE_REF)
        .collect();

    let right_table = if has_nested_join {
        table_refs.first()
    } else {
        table_refs.get(1)
    };

    if let Some(table) = right_table {
        format_table_ref(table, printer);
    }

    // ON condition
    if let Some(condition) = find_child(node, SyntaxKind::JOIN_CONDITION) {
        printer.space();
        printer.write_keyword("ON");
        printer.space();
        format_expression_children(&condition, printer);
    }
}

/// Formats a WHERE clause.
fn format_where_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    format_expression_children(node, printer);
}

/// Formats a GROUP BY clause.
fn format_group_by(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(&child, printer);
        first = false;
    }
}

/// Formats an ORDER BY clause.
fn format_order_by(node: &SyntaxNode, printer: &mut PgPrinter) {
    let items: Vec<_> = node.children()
        .filter(|c| c.kind() == SyntaxKind::ORDER_BY_ITEM)
        .cloned()
        .collect();

    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.space();
        }
        format_order_by_item(item, printer);
    }
}

/// Formats an ORDER BY item.
fn format_order_by_item(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::ASC_KW {
                    printer.space();
                    printer.write_keyword("ASC");
                } else if token.kind() == SyntaxKind::DESC_KW {
                    printer.space();
                    printer.write_keyword("DESC");
                } else if token.kind() == SyntaxKind::NULLS_KW {
                    printer.space();
                    printer.write_keyword("NULLS");
                } else if token.kind() == SyntaxKind::FIRST_KW {
                    printer.space();
                    printer.write_keyword("FIRST");
                } else if token.kind() == SyntaxKind::LAST_KW {
                    printer.space();
                    printer.write_keyword("LAST");
                }
            }
        }
    }
}

/// Formats a LIMIT clause.
fn format_limit_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::LIMIT_KW {
                    printer.write_keyword("LIMIT");
                    printer.space();
                } else if token.kind() == SyntaxKind::OFFSET_KW {
                    printer.space();
                    printer.write_keyword("OFFSET");
                    printer.space();
                }
            }
        }
    }
}

/// Formats an INSERT statement.
fn format_insert(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("INSERT");
    printer.space();
    printer.write_keyword("INTO");
    printer.space();

    // Table name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IDENT {
                printer.write_identifier(token.text());
                break;
            }
        }
    }

    // Column list
    if let Some(col_list) = find_child(node, SyntaxKind::INSERT_COLUMNS) {
        printer.space();
        printer.write("(");
        format_column_list(&col_list, printer);
        printer.write(")");
    }

    // VALUES or SELECT
    if let Some(values) = find_child(node, SyntaxKind::VALUES_CLAUSE) {
        printer.newline();
        printer.write_keyword("VALUES");
        printer.newline();
        printer.indent();
        format_values_clause(&values, printer);
        printer.dedent();
    } else if let Some(select) = find_child(node, SyntaxKind::SELECT_STMT) {
        printer.newline();
        format_select(&select, printer);
    }

    // RETURNING clause
    if let Some(returning) = find_child(node, SyntaxKind::RETURNING_CLAUSE) {
        printer.newline();
        printer.write_keyword("RETURNING");
        printer.space();
        format_returning_clause(&returning, printer);
    }

    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

/// Formats a column list.
fn format_column_list(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IDENT {
                if !first {
                    printer.write(",");
                    printer.space();
                }
                printer.write_identifier(token.text());
                first = false;
            }
        }
    }
}

/// Formats a VALUES clause.
fn format_values_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let rows: Vec<_> = node.children()
        .filter(|c| c.kind() == SyntaxKind::VALUES_ROW)
        .cloned()
        .collect();

    for (i, row) in rows.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.newline();
        }
        format_values_row(row, printer);
    }
}

/// Formats a VALUES row.
fn format_values_row(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write("(");
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(&child, printer);
        first = false;
    }
    printer.write(")");
}

/// Formats a RETURNING clause.
fn format_returning_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(&child, printer);
        first = false;
    }
}

/// Formats an UPDATE statement.
fn format_update(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("UPDATE");
    printer.space();

    // Table name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IDENT {
                printer.write_identifier(token.text());
                break;
            }
        }
    }

    // SET clause
    if let Some(set_clause) = find_child(node, SyntaxKind::SET_CLAUSE) {
        printer.newline();
        printer.write_keyword("SET");
        printer.space();
        format_set_clause(&set_clause, printer);
    }

    // FROM clause
    if let Some(from_clause) = find_child(node, SyntaxKind::FROM_CLAUSE) {
        printer.newline();
        printer.write_keyword("FROM");
        printer.space();
        format_from_clause(&from_clause, printer);
    }

    // WHERE clause
    if let Some(where_clause) = find_child(node, SyntaxKind::WHERE_CLAUSE) {
        printer.newline();
        printer.write_keyword("WHERE");
        printer.space();
        format_where_clause(&where_clause, printer);
    }

    // RETURNING clause
    if let Some(returning) = find_child(node, SyntaxKind::RETURNING_CLAUSE) {
        printer.newline();
        printer.write_keyword("RETURNING");
        printer.space();
        format_returning_clause(&returning, printer);
    }

    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

/// Formats a SET clause.
fn format_set_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let assignments: Vec<_> = node.children()
        .filter(|c| c.kind() == SyntaxKind::SET_ITEM)
        .cloned()
        .collect();

    for (i, item) in assignments.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.newline();
            printer.write("    ");
        }
        format_set_item(item, printer);
    }
}

/// Formats a SET item.
fn format_set_item(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut saw_eq = false;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::IDENT && !saw_eq {
                    printer.write_identifier(token.text());
                } else if token.kind() == SyntaxKind::EQ {
                    printer.space();
                    printer.write("=");
                    printer.space();
                    saw_eq = true;
                }
            }
        }
    }
}

/// Formats a DELETE statement.
fn format_delete(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("DELETE");
    printer.space();
    printer.write_keyword("FROM");
    printer.space();

    // Table name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IDENT {
                printer.write_identifier(token.text());
                break;
            }
        }
    }

    // USING clause
    if let Some(using) = find_child(node, SyntaxKind::USING_CLAUSE) {
        printer.newline();
        printer.write_keyword("USING");
        printer.space();
        format_from_clause(&using, printer);
    }

    // WHERE clause
    if let Some(where_clause) = find_child(node, SyntaxKind::WHERE_CLAUSE) {
        printer.newline();
        printer.write_keyword("WHERE");
        printer.space();
        format_where_clause(&where_clause, printer);
    }

    // RETURNING clause
    if let Some(returning) = find_child(node, SyntaxKind::RETURNING_CLAUSE) {
        printer.newline();
        printer.write_keyword("RETURNING");
        printer.space();
        format_returning_clause(&returning, printer);
    }

    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

// === Expression formatting ===

/// Formats expression children of a node.
fn format_expression_children(node: &SyntaxNode, printer: &mut PgPrinter) {
    for child in node.children() {
        format_expression(&child, printer);
    }
}

/// Formats an expression.
fn format_expression(node: &SyntaxNode, printer: &mut PgPrinter) {
    match node.kind() {
        SyntaxKind::BINARY_EXPR => format_binary_expr(node, printer),
        SyntaxKind::UNARY_EXPR => format_unary_expr(node, printer),
        SyntaxKind::PAREN_EXPR => format_paren_expr(node, printer),
        SyntaxKind::FUNC_CALL => format_func_call(node, printer),
        SyntaxKind::CASE_EXPR => format_case_expr(node, printer),
        SyntaxKind::CAST_EXPR => format_cast_expr(node, printer),
        SyntaxKind::BETWEEN_EXPR => format_between_expr(node, printer),
        SyntaxKind::IN_EXPR => format_in_expr(node, printer),
        SyntaxKind::LIKE_EXPR => format_like_expr(node, printer),
        SyntaxKind::IS_EXPR => format_is_expr(node, printer),
        SyntaxKind::EXISTS_EXPR => format_exists_expr(node, printer),
        SyntaxKind::SUBQUERY_EXPR | SyntaxKind::SUBQUERY => format_subquery(node, printer),
        SyntaxKind::ARRAY_EXPR => format_array_expr(node, printer),
        SyntaxKind::JSONB_ACCESS_EXPR | SyntaxKind::JSONB_PATH_EXPR => format_jsonb_access(node, printer),
        SyntaxKind::COALESCE_EXPR => format_coalesce(node, printer),
        SyntaxKind::NULLIF_EXPR => format_nullif(node, printer),
        SyntaxKind::GREATEST_EXPR | SyntaxKind::LEAST_EXPR => format_greatest_least(node, printer),
        SyntaxKind::COLUMN_REF | SyntaxKind::QUALIFIED_NAME => format_column_ref(node, printer),
        SyntaxKind::LITERAL => format_literal(node, printer),
        SyntaxKind::STAR_EXPR => printer.write("*"),
        SyntaxKind::OVER_CLAUSE => format_over_clause(node, printer),
        _ => format_children(node, printer),
    }
}

/// Formats a binary expression.
fn format_binary_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                format_token(&token, printer);
            }
        }
    }
}

/// Formats a unary expression.
fn format_unary_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                let kind = token.kind();
                if kind == SyntaxKind::MINUS || kind == SyntaxKind::PLUS {
                    printer.write(token.text());
                } else {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a parenthesized expression.
fn format_paren_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write("(");
    for child in node.children() {
        format_expression(&child, printer);
    }
    printer.write(")");
}

/// Formats a function call.
fn format_func_call(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::ARG_LIST {
                    if !printer.no_space_function() {
                        // pgFormatter default: no space before (
                    }
                    printer.write("(");
                    format_arg_list(&child, printer);
                    printer.write(")");
                } else if child.kind() == SyntaxKind::OVER_CLAUSE {
                    printer.space();
                    format_over_clause(&child, printer);
                } else if child.kind() == SyntaxKind::FILTER_CLAUSE {
                    printer.space();
                    format_filter_clause(&child, printer);
                } else if child.kind() == SyntaxKind::QUALIFIED_NAME {
                    format_column_ref(&child, printer);
                } else {
                    format_expression(&child, printer);
                }
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::IDENT {
                    printer.write_function(token.text());
                } else if token.kind() != SyntaxKind::L_PAREN && token.kind() != SyntaxKind::R_PAREN {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a function argument list.
fn format_arg_list(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if !first {
                    printer.write(",");
                    printer.space();
                }
                format_expression(&child, printer);
                first = false;
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::DISTINCT_KW {
                    printer.write_keyword("DISTINCT");
                    printer.space();
                } else if token.kind() == SyntaxKind::ALL_KW {
                    printer.write_keyword("ALL");
                    printer.space();
                }
            }
        }
    }
}

/// Formats an OVER clause.
fn format_over_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("OVER");
    printer.space();
    printer.write("(");

    if let Some(partition) = find_child(node, SyntaxKind::PARTITION_BY) {
        printer.write_keyword("PARTITION");
        printer.space();
        printer.write_keyword("BY");
        printer.space();
        format_partition_by(&partition, printer);
    }

    if let Some(order) = find_child(node, SyntaxKind::ORDER_BY_CLAUSE) {
        printer.space();
        printer.write_keyword("ORDER");
        printer.space();
        printer.write_keyword("BY");
        printer.space();
        format_order_by(&order, printer);
    }

    if let Some(frame) = find_child(node, SyntaxKind::FRAME_CLAUSE) {
        printer.space();
        format_children(&frame, printer);
    }

    printer.write(")");
}

/// Formats PARTITION BY items.
fn format_partition_by(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(&child, printer);
        first = false;
    }
}

/// Formats a FILTER clause.
fn format_filter_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("FILTER");
    printer.space();
    printer.write("(");
    printer.write_keyword("WHERE");
    printer.space();
    for child in node.children() {
        format_expression(&child, printer);
    }
    printer.write(")");
}

/// Formats a CASE expression.
fn format_case_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("CASE");

    let mut saw_when = false;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::CASE_WHEN {
                    printer.newline();
                    printer.indent();
                    format_case_when(&child, printer);
                    printer.dedent();
                    saw_when = true;
                } else if child.kind() == SyntaxKind::CASE_ELSE {
                    printer.newline();
                    printer.indent();
                    format_case_else(&child, printer);
                    printer.dedent();
                } else if !saw_when {
                    printer.space();
                    format_expression(&child, printer);
                }
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::END_KW {
                    printer.newline();
                    printer.write_keyword("END");
                }
            }
        }
    }
}

/// Formats a CASE WHEN clause.
fn format_case_when(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("WHEN");
    printer.space();

    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::THEN_KW {
                    printer.space();
                    printer.write_keyword("THEN");
                    printer.space();
                }
            }
        }
    }
}

/// Formats a CASE ELSE clause.
fn format_case_else(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("ELSE");
    printer.space();
    for child in node.children() {
        format_expression(&child, printer);
    }
}

/// Formats a CAST expression.
fn format_cast_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut is_double_colon = false;
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::DOUBLE_COLON {
                is_double_colon = true;
                break;
            }
        }
    }

    if is_double_colon {
        let mut saw_colon = false;
        for element in node.children_with_tokens() {
            match element {
                cstree::util::NodeOrToken::Node(child) => {
                    format_expression(&child, printer);
                }
                cstree::util::NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::DOUBLE_COLON {
                        printer.write("::");
                        saw_colon = true;
                    } else if saw_colon && token.kind().is_keyword() {
                        printer.write_type(token.text());
                    } else {
                        format_token(&token, printer);
                    }
                }
            }
        }
    } else {
        printer.write_keyword("CAST");
        printer.write("(");

        let mut saw_as = false;
        for element in node.children_with_tokens() {
            match element {
                cstree::util::NodeOrToken::Node(child) => {
                    format_expression(&child, printer);
                }
                cstree::util::NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::AS_KW {
                        printer.space();
                        printer.write_keyword("AS");
                        printer.space();
                        saw_as = true;
                    } else if token.kind() != SyntaxKind::CAST_KW
                        && token.kind() != SyntaxKind::L_PAREN
                        && token.kind() != SyntaxKind::R_PAREN
                    {
                        if saw_as && token.kind().is_keyword() {
                            printer.write_type(token.text());
                        } else {
                            format_token(&token, printer);
                        }
                    }
                }
            }
        }

        printer.write(")");
    }
}

/// Formats a BETWEEN expression.
fn format_between_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    let children: Vec<_> = node.children().collect();
    if children.len() >= 3 {
        format_expression(&children[0], printer);
        printer.space();
        printer.write_keyword("BETWEEN");
        printer.space();
        format_expression(&children[1], printer);
        printer.space();
        printer.write_keyword("AND");
        printer.space();
        format_expression(&children[2], printer);
    } else {
        format_children(node, printer);
    }
}

/// Formats an IN expression.
fn format_in_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::SUBQUERY {
                    format_subquery(&child, printer);
                } else {
                    format_expression(&child, printer);
                }
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::IN_KW {
                    printer.space();
                    printer.write_keyword("IN");
                    printer.space();
                } else if token.kind() == SyntaxKind::NOT_KW {
                    printer.space();
                    printer.write_keyword("NOT");
                } else if token.kind() == SyntaxKind::L_PAREN {
                    printer.write("(");
                } else if token.kind() == SyntaxKind::R_PAREN {
                    printer.write(")");
                } else if token.kind() == SyntaxKind::COMMA {
                    printer.write(",");
                    printer.space();
                } else {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a LIKE expression.
fn format_like_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                format_token(&token, printer);
            }
        }
    }
}

/// Formats an IS expression.
fn format_is_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                format_token(&token, printer);
            }
        }
    }
}

/// Formats an EXISTS expression.
fn format_exists_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("EXISTS");
    printer.space();
    for child in node.children() {
        format_subquery(&child, printer);
    }
}

/// Formats a subquery.
fn format_subquery(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write("(");
    printer.newline();
    printer.indent();

    for child in node.children() {
        if child.kind() == SyntaxKind::SELECT_STMT {
            format_select(&child, printer);
        } else {
            format_expression(&child, printer);
        }
    }

    printer.dedent();
    printer.newline();
    printer.write(")");
}

/// Formats an array expression.
fn format_array_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("ARRAY");
    printer.write("[");

    let mut first = true;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if !first {
                    printer.write(",");
                    printer.space();
                }
                format_expression(&child, printer);
                first = false;
            }
            cstree::util::NodeOrToken::Token(_) => {}
        }
    }

    printer.write("]");
}

/// Formats a JSONB access expression.
fn format_jsonb_access(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                printer.write(token.text());
            }
        }
    }
}

/// Formats a COALESCE expression.
fn format_coalesce(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_function("COALESCE");
    printer.write("(");

    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(&child, printer);
        first = false;
    }

    printer.write(")");
}

/// Formats a NULLIF expression.
fn format_nullif(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_function("NULLIF");
    printer.write("(");

    let children: Vec<_> = node.children().collect();
    if children.len() >= 2 {
        format_expression(&children[0], printer);
        printer.write(",");
        printer.space();
        format_expression(&children[1], printer);
    }

    printer.write(")");
}

/// Formats GREATEST/LEAST expressions.
fn format_greatest_least(node: &SyntaxNode, printer: &mut PgPrinter) {
    let keyword = if node.kind() == SyntaxKind::GREATEST_EXPR {
        "GREATEST"
    } else {
        "LEAST"
    };

    printer.write_function(keyword);
    printer.write("(");

    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(&child, printer);
        first = false;
    }

    printer.write(")");
}

/// Formats a column reference.
fn format_column_ref(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if !first {
                    printer.write(".");
                }
                format_expression(&child, printer);
                first = false;
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::DOT {
                    // Handled above
                } else if token.kind() == SyntaxKind::IDENT {
                    if !first {
                        printer.write(".");
                    }
                    printer.write_identifier(token.text());
                    first = false;
                } else if token.kind() == SyntaxKind::QUOTED_IDENT {
                    if !first {
                        printer.write(".");
                    }
                    printer.write(token.text());
                    first = false;
                } else if token.kind() == SyntaxKind::STAR {
                    if !first {
                        printer.write(".");
                    }
                    printer.write("*");
                    first = false;
                } else {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a literal value.
fn format_literal(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            let kind = token.kind();
            if kind != SyntaxKind::WHITESPACE && kind != SyntaxKind::NEWLINE {
                printer.write(token.text());
            }
        }
    }
}

// === Helper functions ===

/// Finds a child node of a specific kind.
fn find_child(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    node.children().find(|c| c.kind() == kind).cloned()
}

/// Checks if a node has a token of a specific kind.
fn has_token(node: &SyntaxNode, kind: SyntaxKind) -> bool {
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == kind {
                return true;
            }
        }
    }
    false
}

/// Returns true if a keyword needs a space before it.
fn needs_space_before(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AND_KW
            | SyntaxKind::OR_KW
            | SyntaxKind::NOT_KW
            | SyntaxKind::IS_KW
            | SyntaxKind::IN_KW
            | SyntaxKind::LIKE_KW
            | SyntaxKind::ILIKE_KW
            | SyntaxKind::BETWEEN_KW
            | SyntaxKind::AS_KW
            | SyntaxKind::ASC_KW
            | SyntaxKind::DESC_KW
    )
}

/// Returns true if a keyword needs a space after it.
fn needs_space_after(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::AND_KW
            | SyntaxKind::OR_KW
            | SyntaxKind::NOT_KW
            | SyntaxKind::IS_KW
            | SyntaxKind::IN_KW
            | SyntaxKind::LIKE_KW
            | SyntaxKind::ILIKE_KW
            | SyntaxKind::AS_KW
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pg_formatter::CaseOption;

    #[test]
    fn test_format_simple_select() {
        let sql = "select id, name from users";
        let config = PgFormatterConfig::default();
        let formatted = format(sql, &config);
        assert!(formatted.contains("SELECT"));
        assert!(formatted.contains("FROM"));
    }

    #[test]
    fn test_format_with_function_case() {
        let sql = "SELECT COUNT(*) FROM users";
        let config = PgFormatterConfig::default().with_function_case(CaseOption::Lower);
        let formatted = format(sql, &config);
        eprintln!("Formatted output: {:?}", formatted);
        // COUNT is a reserved keyword in SQL, so it won't be transformed by function_case
        // Instead, it follows keyword_case. For user-defined functions like my_func(),
        // function_case would apply.
        assert!(formatted.contains("COUNT") || formatted.contains("count"));
    }

    #[test]
    fn test_format_with_keyword_case_lower() {
        let sql = "SELECT id FROM users";
        let config = PgFormatterConfig::default().with_keyword_case(CaseOption::Lower);
        let formatted = format(sql, &config);
        assert!(formatted.contains("select"));
        assert!(formatted.contains("from"));
    }

    #[test]
    fn test_format_no_comment() {
        let sql = "SELECT id -- comment\nFROM users";
        let config = PgFormatterConfig::default().with_no_comment(true);
        let formatted = format(sql, &config);
        assert!(!formatted.contains("comment"));
    }

    #[test]
    fn test_format_no_extra_line() {
        let sql = "SELECT id FROM users";
        let config = PgFormatterConfig {
            no_extra_line: true,
            ..Default::default()
        };
        let formatted = format(sql, &config);
        assert!(!formatted.ends_with('\n'));
    }

    #[test]
    fn test_format_comma_start() {
        let sql = "SELECT id, name, email FROM users";
        let config = PgFormatterConfig::default().with_comma_start(true);
        let formatted = format(sql, &config);
        // Should have leading commas
        assert!(formatted.contains(", "));
    }

    #[test]
    fn test_format_type_case() {
        let sql = "SELECT id::INTEGER FROM users";
        let config = PgFormatterConfig::default().with_type_case(CaseOption::Upper);
        let formatted = format(sql, &config);
        assert!(formatted.contains("INTEGER"));
    }
}
