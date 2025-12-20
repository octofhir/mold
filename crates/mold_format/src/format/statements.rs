//! Statement formatting.

use mold_syntax::{SyntaxKind, SyntaxNode};

use crate::config::CommaStyle;
use crate::printer::Printer;

use super::{children_of_kind, find_child, format_children, format_token};

/// Formats a SELECT statement.
pub fn format_select(node: &SyntaxNode, printer: &mut Printer) {
    // Handle WITH clause if present
    if let Some(with_clause) = find_child(node, SyntaxKind::WITH_CLAUSE) {
        format_with_clause(&with_clause, printer);
    }

    // The SELECT keyword is a direct child token of SELECT_STMT
    // Look for SELECT_KW token and SELECT_ITEM_LIST node
    let has_select = node.children_with_tokens().any(
        |e| matches!(e, cstree::util::NodeOrToken::Token(t) if t.kind() == SyntaxKind::SELECT_KW),
    );

    if has_select {
        // Write SELECT keyword
        if printer.config().river_alignment {
            printer.write_keyword_river("SELECT");
        } else {
            printer.write_keyword("SELECT");
        }
        printer.space();

        // Check for DISTINCT/ALL
        for element in node.children_with_tokens() {
            if let cstree::util::NodeOrToken::Token(token) = element {
                if token.kind() == SyntaxKind::DISTINCT_KW {
                    printer.write_keyword("DISTINCT");
                    printer.space();
                } else if token.kind() == SyntaxKind::ALL_KW {
                    printer.write_keyword("ALL");
                    printer.space();
                }
            }
        }

        // Format select items from SELECT_ITEM_LIST
        if let Some(items_list) = find_child(node, SyntaxKind::SELECT_ITEM_LIST) {
            let items = children_of_kind(&items_list, SyntaxKind::SELECT_ITEM);
            format_select_items(&items, printer);
        }
    }

    // Also check for SELECT_CLAUSE (alternative structure)
    if let Some(select_clause) = find_child(node, SyntaxKind::SELECT_CLAUSE) {
        format_select_clause(&select_clause, printer);
    }

    // Format FROM clause
    if let Some(from_clause) = find_child(node, SyntaxKind::FROM_CLAUSE) {
        format_from_clause(&from_clause, printer);
    }

    // Format WHERE clause
    if let Some(where_clause) = find_child(node, SyntaxKind::WHERE_CLAUSE) {
        format_where_clause(&where_clause, printer);
    }

    // Format GROUP BY clause
    if let Some(group_clause) = find_child(node, SyntaxKind::GROUP_BY_CLAUSE) {
        format_group_by_clause(&group_clause, printer);
    }

    // Format HAVING clause
    if let Some(having_clause) = find_child(node, SyntaxKind::HAVING_CLAUSE) {
        format_having_clause(&having_clause, printer);
    }

    // Format ORDER BY clause
    if let Some(order_clause) = find_child(node, SyntaxKind::ORDER_BY_CLAUSE) {
        format_order_by_clause(&order_clause, printer);
    }

    // Format LIMIT clause
    if let Some(limit_clause) = find_child(node, SyntaxKind::LIMIT_CLAUSE) {
        format_limit_clause(&limit_clause, printer);
    }

    // Format OFFSET clause
    if let Some(offset_clause) = find_child(node, SyntaxKind::OFFSET_CLAUSE) {
        format_offset_clause(&offset_clause, printer);
    }

    // Handle set operations (UNION, INTERSECT, EXCEPT)
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            match token.kind() {
                SyntaxKind::UNION_KW | SyntaxKind::INTERSECT_KW | SyntaxKind::EXCEPT_KW => {
                    printer.newline();
                    printer.write_keyword(token.text());
                    printer.space();
                }
                _ => {}
            }
        }
    }
}

/// Formats a WITH clause (CTEs).
fn format_with_clause(node: &SyntaxNode, printer: &mut Printer) {
    if printer.config().river_alignment {
        printer.write_keyword_river("WITH");
    } else {
        printer.write_keyword("WITH");
    }
    printer.space();

    // Check for RECURSIVE
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::RECURSIVE_KW {
                printer.write_keyword("RECURSIVE");
                printer.space();
            }
        }
    }

    // Format CTEs
    let ctes = children_of_kind(node, SyntaxKind::CTE);
    for (i, cte) in ctes.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.newline();
            if printer.config().river_alignment {
                printer.spaces(printer.river_width() - 4); // Align with first CTE
            }
        }
        format_cte(cte, printer);
    }

    printer.newline();
}

/// Formats a single CTE.
fn format_cte(node: &SyntaxNode, printer: &mut Printer) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::SUBQUERY || child.kind() == SyntaxKind::SELECT_STMT {
                    printer.write("(");
                    printer.newline();
                    printer.indent();
                    format_select(&child, printer);
                    printer.dedent();
                    printer.newline();
                    printer.write(")");
                } else {
                    format_children(&child, printer);
                }
            }
            cstree::util::NodeOrToken::Token(token) => {
                format_token(&token, printer);
            }
        }
    }
}

/// Formats a SELECT clause.
fn format_select_clause(node: &SyntaxNode, printer: &mut Printer) {
    if printer.config().river_alignment {
        printer.write_keyword_river("SELECT");
    } else {
        printer.write_keyword("SELECT");
    }
    printer.space();

    // Check for DISTINCT/ALL
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::DISTINCT_KW {
                printer.write_keyword("DISTINCT");
                printer.space();
            } else if token.kind() == SyntaxKind::ALL_KW {
                printer.write_keyword("ALL");
                printer.space();
            }
        }
    }

    // Format select items
    let items = children_of_kind(node, SyntaxKind::SELECT_ITEM);
    format_select_items(&items, printer);
}

/// Formats a list of select items.
fn format_select_items(items: &[SyntaxNode], printer: &mut Printer) {
    let comma_style = printer.config().comma_style;
    let river_alignment = printer.config().river_alignment;
    let river_width = printer.river_width();

    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            match comma_style {
                CommaStyle::Trailing => {
                    printer.write(",");
                    printer.newline();
                    if river_alignment {
                        // Align with first column (after SELECT)
                        printer.spaces(river_width + 1);
                    }
                }
                CommaStyle::Leading => {
                    printer.newline();
                    if river_alignment {
                        // Right-align comma
                        printer.spaces(river_width - 1);
                        printer.write(",");
                        printer.space();
                    } else {
                        printer.write(",");
                        printer.space();
                    }
                }
            }
        }

        format_select_item(item, printer);
    }
}

/// Formats a single select item.
fn format_select_item(node: &SyntaxNode, printer: &mut Printer) {
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

/// Formats an expression node.
fn format_expression(node: &SyntaxNode, printer: &mut Printer) {
    super::expressions::format_expression(node, printer);
}

/// Formats a FROM clause.
fn format_from_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("FROM");
    } else {
        printer.write_keyword("FROM");
    }
    printer.space();

    // Format table references - JOIN_EXPR contains the left table as first child
    let mut first = true;
    for child in node.children() {
        match child.kind() {
            SyntaxKind::TABLE_REF => {
                if !first {
                    printer.write(",");
                    printer.space();
                }
                format_table_ref(&child, printer);
                first = false;
            }
            SyntaxKind::JOIN_EXPR => {
                format_join_expr(&child, printer, first);
                first = false;
            }
            _ => {}
        }
    }
}

/// Formats a table reference.
fn format_table_ref(node: &SyntaxNode, printer: &mut Printer) {
    let mut first = true;
    let mut after_as = false;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::SUBQUERY {
                    printer.write("(");
                    printer.newline();
                    printer.indent();
                    if let Some(select) = find_child(&child, SyntaxKind::SELECT_STMT) {
                        format_select(&select, printer);
                    }
                    printer.dedent();
                    printer.newline();
                    printer.write(")");
                } else if child.kind() == SyntaxKind::ALIAS {
                    format_alias(&child, printer);
                } else if child.kind() == SyntaxKind::TABLE_NAME
                    || child.kind() == SyntaxKind::QUALIFIED_NAME
                {
                    format_table_name(&child, printer);
                } else {
                    format_children(&child, printer);
                }
                first = false;
            }
            cstree::util::NodeOrToken::Token(token) => {
                let kind = token.kind();
                // Skip whitespace and handle identifiers
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE {
                    continue;
                }
                if kind == SyntaxKind::AS_KW {
                    printer.space();
                    printer.write_keyword("AS");
                    printer.space();
                    after_as = true;
                    first = false;
                } else if kind == SyntaxKind::IDENT || kind == SyntaxKind::QUOTED_IDENT {
                    // Don't add space if we just wrote AS
                    if !first && !after_as {
                        printer.space();
                    }
                    printer.write_identifier(token.text());
                    first = false;
                    after_as = false;
                } else {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a table name.
fn format_table_name(node: &SyntaxNode, printer: &mut Printer) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_children(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                let kind = token.kind();
                if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE {
                    continue;
                }
                if kind == SyntaxKind::DOT {
                    printer.write(".");
                } else if kind == SyntaxKind::IDENT || kind == SyntaxKind::QUOTED_IDENT {
                    printer.write_identifier(token.text());
                } else {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats an alias.
fn format_alias(node: &SyntaxNode, printer: &mut Printer) {
    // Check if there's an AS keyword
    let has_as = node
        .children_with_tokens()
        .any(|e| matches!(e, cstree::util::NodeOrToken::Token(t) if t.kind() == SyntaxKind::AS_KW));

    printer.space();

    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            let kind = token.kind();
            if kind == SyntaxKind::WHITESPACE || kind == SyntaxKind::NEWLINE {
                continue;
            }
            if kind == SyntaxKind::AS_KW {
                printer.write_keyword("AS");
                printer.space();
            } else if kind == SyntaxKind::IDENT || kind == SyntaxKind::QUOTED_IDENT {
                // If no AS keyword, this is a shorthand alias
                if !has_as {
                    // Just write the alias directly (already added space above)
                }
                printer.write_identifier(token.text());
            }
        }
    }
}

/// Formats a JOIN expression.
/// The JOIN_EXPR structure is left-associative:
/// - For multiple JOINs: JOIN_EXPR(JOIN_EXPR(table1, join, table2), join, table3)
/// - The inner JOIN_EXPR is the left side, the outer adds another join
fn format_join_expr(node: &SyntaxNode, printer: &mut Printer, is_first: bool) {
    // Check if the first child is a nested JOIN_EXPR
    let first_child = node.children().next();
    let has_nested_join = first_child
        .as_ref()
        .is_some_and(|c| c.kind() == SyntaxKind::JOIN_EXPR);

    if has_nested_join {
        // Recursively format the nested join first
        if let Some(nested_join) = first_child {
            format_join_expr(&nested_join, printer, is_first);
        }
    } else {
        // No nested join - format the left table (first TABLE_REF)
        let left_table = node.children().find(|c| c.kind() == SyntaxKind::TABLE_REF);
        if let Some(table) = left_table {
            format_table_ref(&table, printer);
        }
    }

    // Determine join type from tokens at THIS level (not nested)
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

    // Add blank line between JOINs (not before the first one)
    if has_nested_join {
        printer.newline();
    }

    printer.newline();
    if printer.config().river_alignment {
        // Indent JOIN to align with table name after FROM
        printer.spaces(printer.river_width() + 1);
    }
    printer.write_keyword(&join_keyword);
    printer.space();

    // Format the right table (TABLE_REF that's not the first/nested one)
    let table_refs: Vec<_> = node
        .children()
        .filter(|c| c.kind() == SyntaxKind::TABLE_REF)
        .collect();

    // If we have nested join, the first TABLE_REF is the right table
    // If no nested join, the second TABLE_REF is the right table
    let right_table = if has_nested_join {
        table_refs.first()
    } else {
        table_refs.get(1)
    };

    if let Some(table) = right_table {
        format_table_ref(table, printer);
    }

    // Format ON condition
    if let Some(condition) = find_child(node, SyntaxKind::JOIN_CONDITION) {
        printer.newline();
        if printer.config().river_alignment {
            // Align ON with JOIN
            printer.spaces(printer.river_width() + 1);
        }
        printer.write_keyword("ON");
        printer.space();

        // Format condition with special handling for AND
        format_join_condition(&condition, printer);
    }
}

/// Formats a JOIN condition, handling AND with proper indentation.
fn format_join_condition(node: &SyntaxNode, printer: &mut Printer) {
    for child in node.children() {
        format_condition_expr(&child, printer, true);
    }
}

/// Formats a condition expression in JOIN context.
fn format_condition_expr(node: &SyntaxNode, printer: &mut Printer, is_top_level: bool) {
    // Check if this is a binary expression with AND/OR
    let has_logical_op = node.children_with_tokens().any(|e| {
        matches!(e, cstree::util::NodeOrToken::Token(t)
            if t.kind() == SyntaxKind::AND_KW || t.kind() == SyntaxKind::OR_KW)
    });

    if has_logical_op && is_top_level {
        // Split on AND/OR for proper formatting
        let mut first = true;
        for element in node.children_with_tokens() {
            match element {
                cstree::util::NodeOrToken::Node(child) => {
                    if child.kind() == SyntaxKind::BINARY_EXPR {
                        format_condition_expr(&child, printer, false);
                    } else {
                        format_expression(&child, printer);
                    }
                    first = false;
                }
                cstree::util::NodeOrToken::Token(token) => {
                    if token.kind() == SyntaxKind::AND_KW || token.kind() == SyntaxKind::OR_KW {
                        printer.newline();
                        if printer.config().river_alignment {
                            // Indent AND/OR more than ON
                            printer.spaces(printer.river_width() + 4);
                        }
                        printer.write_keyword(token.text());
                        printer.space();
                    } else if !first {
                        format_token(&token, printer);
                    }
                }
            }
        }
    } else {
        format_expression(node, printer);
    }
}

/// Formats a WHERE clause.
fn format_where_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("WHERE");
    } else {
        printer.write_keyword("WHERE");
    }
    printer.space();

    // Format the condition
    for child in node.children() {
        format_where_condition(&child, printer);
    }
}

/// Formats a WHERE condition, handling AND/OR with newlines.
fn format_where_condition(node: &SyntaxNode, printer: &mut Printer) {
    let newline_before_logical = printer.config().newline_before_logical;
    let river_alignment = printer.config().river_alignment;
    let river_width = printer.river_width();

    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::BINARY_EXPR {
                    format_where_condition(&child, printer);
                } else {
                    format_expression(&child, printer);
                }
            }
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::AND_KW || token.kind() == SyntaxKind::OR_KW {
                    if newline_before_logical {
                        printer.newline();
                        if river_alignment {
                            // Right-align AND/OR
                            let kw = if token.kind() == SyntaxKind::AND_KW {
                                "AND"
                            } else {
                                "OR"
                            };
                            let padding = river_width.saturating_sub(kw.len());
                            printer.spaces(padding);
                            printer.write_keyword(kw);
                        } else {
                            printer.write_keyword(token.text());
                        }
                        printer.space();
                    } else {
                        printer.space();
                        printer.write_keyword(token.text());
                        printer.space();
                    }
                } else {
                    format_token(&token, printer);
                }
            }
        }
    }
}

/// Formats a GROUP BY clause.
fn format_group_by_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("GROUP BY");
    } else {
        printer.write_keyword("GROUP");
        printer.space();
        printer.write_keyword("BY");
    }
    printer.space();

    let mut first = true;
    for child in node.children() {
        if child.kind() != SyntaxKind::GROUP_KW && child.kind() != SyntaxKind::BY_KW {
            if !first {
                printer.write(",");
                printer.space();
            }
            format_expression(&child, printer);
            first = false;
        }
    }
}

/// Formats a HAVING clause.
fn format_having_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("HAVING");
    } else {
        printer.write_keyword("HAVING");
    }
    printer.space();

    for child in node.children() {
        format_expression(&child, printer);
    }
}

/// Formats an ORDER BY clause.
fn format_order_by_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("ORDER BY");
    } else {
        printer.write_keyword("ORDER");
        printer.space();
        printer.write_keyword("BY");
    }
    printer.space();

    let items = children_of_kind(node, SyntaxKind::ORDER_BY_ITEM);
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.space();
        }
        format_order_by_item(item, printer);
    }
}

/// Formats an ORDER BY item.
fn format_order_by_item(node: &SyntaxNode, printer: &mut Printer) {
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

/// Formats a LIMIT clause.
fn format_limit_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("LIMIT");
    } else {
        printer.write_keyword("LIMIT");
    }
    printer.space();

    for child in node.children() {
        format_expression(&child, printer);
    }
}

/// Formats an OFFSET clause.
fn format_offset_clause(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("OFFSET");
    } else {
        printer.write_keyword("OFFSET");
    }
    printer.space();

    for child in node.children() {
        format_expression(&child, printer);
    }
}

/// Formats an INSERT statement.
pub fn format_insert(node: &SyntaxNode, printer: &mut Printer) {
    // Handle WITH clause if present
    if let Some(with_clause) = find_child(node, SyntaxKind::WITH_CLAUSE) {
        format_with_clause(&with_clause, printer);
    }

    if printer.config().river_alignment {
        printer.write_keyword_river("INSERT INTO");
    } else {
        printer.write_keyword("INSERT");
        printer.space();
        printer.write_keyword("INTO");
    }
    printer.space();

    // Format table name
    if let Some(table_name) = find_child(node, SyntaxKind::TABLE_NAME) {
        format_children(&table_name, printer);
    }

    // Format column list
    if let Some(columns) = find_child(node, SyntaxKind::INSERT_COLUMNS) {
        printer.space();
        format_children(&columns, printer);
    }

    // Format VALUES or SELECT
    if let Some(values) = find_child(node, SyntaxKind::VALUES_CLAUSE) {
        printer.newline();
        if printer.config().river_alignment {
            printer.write_keyword_river("VALUES");
        } else {
            printer.write_keyword("VALUES");
        }
        printer.space();
        format_values_clause(&values, printer);
    } else if let Some(select) = find_child(node, SyntaxKind::SELECT_STMT) {
        printer.newline();
        format_select(&select, printer);
    }

    // Format ON CONFLICT
    if let Some(conflict) = find_child(node, SyntaxKind::ON_CONFLICT_CLAUSE) {
        format_on_conflict(&conflict, printer);
    }

    // Format RETURNING
    if let Some(returning) = find_child(node, SyntaxKind::RETURNING_CLAUSE) {
        format_returning(&returning, printer);
    }
}

/// Formats a VALUES clause.
fn format_values_clause(node: &SyntaxNode, printer: &mut Printer) {
    let rows = children_of_kind(node, SyntaxKind::VALUES_ROW);
    for (i, row) in rows.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.newline();
            if printer.config().river_alignment {
                printer.spaces(printer.river_width() + 1);
            }
        }
        format_children(row, printer);
    }
}

/// Formats an ON CONFLICT clause.
fn format_on_conflict(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    printer.write_keyword("ON");
    printer.space();
    printer.write_keyword("CONFLICT");

    // Format conflict target
    if let Some(target) = find_child(node, SyntaxKind::CONFLICT_TARGET) {
        printer.space();
        format_children(&target, printer);
    }

    // Format DO action
    if let Some(action) = find_child(node, SyntaxKind::CONFLICT_ACTION) {
        printer.space();
        format_children(&action, printer);
    }
}

/// Formats a RETURNING clause.
fn format_returning(node: &SyntaxNode, printer: &mut Printer) {
    printer.newline();
    if printer.config().river_alignment {
        printer.write_keyword_river("RETURNING");
    } else {
        printer.write_keyword("RETURNING");
    }
    printer.space();

    let items = children_of_kind(node, SyntaxKind::SELECT_ITEM);
    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            printer.write(",");
            printer.space();
        }
        format_select_item(item, printer);
    }
}

/// Formats an UPDATE statement.
pub fn format_update(node: &SyntaxNode, printer: &mut Printer) {
    // Handle WITH clause if present
    if let Some(with_clause) = find_child(node, SyntaxKind::WITH_CLAUSE) {
        format_with_clause(&with_clause, printer);
    }

    if printer.config().river_alignment {
        printer.write_keyword_river("UPDATE");
    } else {
        printer.write_keyword("UPDATE");
    }
    printer.space();

    // Format table name
    if let Some(table_name) = find_child(node, SyntaxKind::TABLE_NAME) {
        format_children(&table_name, printer);
    }

    // Format SET clause
    if let Some(set_clause) = find_child(node, SyntaxKind::SET_CLAUSE) {
        printer.newline();
        if printer.config().river_alignment {
            printer.write_keyword_river("SET");
        } else {
            printer.write_keyword("SET");
        }
        printer.space();
        format_set_clause(&set_clause, printer);
    }

    // Format FROM clause (PostgreSQL extension)
    if let Some(from_clause) = find_child(node, SyntaxKind::FROM_CLAUSE) {
        format_from_clause(&from_clause, printer);
    }

    // Format WHERE clause
    if let Some(where_clause) = find_child(node, SyntaxKind::WHERE_CLAUSE) {
        format_where_clause(&where_clause, printer);
    }

    // Format RETURNING
    if let Some(returning) = find_child(node, SyntaxKind::RETURNING_CLAUSE) {
        format_returning(&returning, printer);
    }
}

/// Formats a SET clause.
fn format_set_clause(node: &SyntaxNode, printer: &mut Printer) {
    let items = children_of_kind(node, SyntaxKind::SET_ITEM);
    let comma_style = printer.config().comma_style;
    let river_alignment = printer.config().river_alignment;
    let river_width = printer.river_width();

    for (i, item) in items.iter().enumerate() {
        if i > 0 {
            match comma_style {
                CommaStyle::Trailing => {
                    printer.write(",");
                    printer.newline();
                    if river_alignment {
                        printer.spaces(river_width + 1);
                    }
                }
                CommaStyle::Leading => {
                    printer.newline();
                    if river_alignment {
                        printer.spaces(river_width - 1);
                        printer.write(",");
                        printer.space();
                    } else {
                        printer.write(",");
                        printer.space();
                    }
                }
            }
        }
        format_children(item, printer);
    }
}

/// Formats a DELETE statement.
pub fn format_delete(node: &SyntaxNode, printer: &mut Printer) {
    // Handle WITH clause if present
    if let Some(with_clause) = find_child(node, SyntaxKind::WITH_CLAUSE) {
        format_with_clause(&with_clause, printer);
    }

    if printer.config().river_alignment {
        printer.write_keyword_river("DELETE FROM");
    } else {
        printer.write_keyword("DELETE");
        printer.space();
        printer.write_keyword("FROM");
    }
    printer.space();

    // Format table name
    if let Some(table_name) = find_child(node, SyntaxKind::TABLE_NAME) {
        format_children(&table_name, printer);
    }

    // Format USING clause (PostgreSQL extension)
    if let Some(using_clause) = find_child(node, SyntaxKind::USING_CLAUSE) {
        printer.newline();
        if printer.config().river_alignment {
            printer.write_keyword_river("USING");
        } else {
            printer.write_keyword("USING");
        }
        printer.space();
        format_children(&using_clause, printer);
    }

    // Format WHERE clause
    if let Some(where_clause) = find_child(node, SyntaxKind::WHERE_CLAUSE) {
        format_where_clause(&where_clause, printer);
    }

    // Format RETURNING
    if let Some(returning) = find_child(node, SyntaxKind::RETURNING_CLAUSE) {
        format_returning(&returning, printer);
    }
}

/// Formats a CREATE TABLE statement.
pub fn format_create_table(node: &SyntaxNode, printer: &mut Printer) {
    printer.write_keyword("CREATE");
    printer.space();

    // Check for TEMP/TEMPORARY
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::TEMP_KW || token.kind() == SyntaxKind::TEMPORARY_KW {
                printer.write_keyword(token.text());
                printer.space();
            }
        }
    }

    printer.write_keyword("TABLE");
    printer.space();

    // Check for IF NOT EXISTS
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element {
            if token.kind() == SyntaxKind::IF_KW {
                printer.write_keyword("IF");
                printer.space();
                printer.write_keyword("NOT");
                printer.space();
                printer.write_keyword("EXISTS");
                printer.space();
            }
        }
    }

    // Format table name
    if let Some(table_name) = find_child(node, SyntaxKind::TABLE_NAME) {
        format_children(&table_name, printer);
    }

    // Format column definitions
    if let Some(columns) = find_child(node, SyntaxKind::COLUMN_DEF_LIST) {
        printer.space();
        printer.write("(");
        printer.newline();
        printer.indent();

        let defs = children_of_kind(&columns, SyntaxKind::COLUMN_DEF);
        for (i, def) in defs.iter().enumerate() {
            if i > 0 {
                printer.write(",");
                printer.newline();
            }
            format_column_def(def, printer);
        }

        // Format table constraints
        let constraints = children_of_kind(&columns, SyntaxKind::CONSTRAINT);
        for constraint in &constraints {
            printer.write(",");
            printer.newline();
            format_children(constraint, printer);
        }

        printer.dedent();
        printer.newline();
        printer.write(")");
    }
}

/// Formats a column definition.
fn format_column_def(node: &SyntaxNode, printer: &mut Printer) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::TYPE_NAME {
                    printer.space();
                }
                format_children(&child, printer);
            }
            cstree::util::NodeOrToken::Token(token) => {
                format_token(&token, printer);
            }
        }
    }
}
