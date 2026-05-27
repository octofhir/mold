//! Statement formatting for the pgFormatter style. Split from the module
//! root for maintainability; see `super` for the printer and dispatch.

use mold_syntax::{SyntaxKind, SyntaxNode};

use super::*;

pub(crate) fn format_select(node: &SyntaxNode, printer: &mut PgPrinter) {
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
pub(crate) fn format_with_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("WITH");

    // Check for RECURSIVE
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && token.kind() == SyntaxKind::RECURSIVE_KW
        {
            printer.space();
            printer.write_keyword("RECURSIVE");
            break;
        }
    }

    printer.newline();
    printer.indent();

    let ctes: Vec<_> = node
        .children()
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
pub(crate) fn format_cte(node: &SyntaxNode, printer: &mut PgPrinter) {
    // CTE name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && token.kind() == SyntaxKind::IDENT
        {
            printer.write_identifier(token.text());
            break;
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
pub(crate) fn format_select_list(node: &SyntaxNode, printer: &mut PgPrinter) {
    let items: Vec<_> = node
        .children()
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
pub(crate) fn format_select_item(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut saw_as = false;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(child, printer);
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
                } else if token.kind() != SyntaxKind::WHITESPACE
                    && token.kind() != SyntaxKind::NEWLINE
                {
                    format_token(token, printer);
                }
            }
        }
    }
}

/// Formats a FROM clause.
pub(crate) fn format_from_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if child.kind() == SyntaxKind::TABLE_REF {
            if !first {
                printer.write(",");
                printer.space();
            }
            format_table_ref(child, printer);
            first = false;
        } else if child.kind() == SyntaxKind::JOIN_EXPR {
            printer.newline();
            format_join_expr(child, printer);
        }
    }
}

/// Formats a table reference.
pub(crate) fn format_table_ref(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::SUBQUERY {
                    format_subquery(child, printer);
                } else if child.kind() == SyntaxKind::ALIAS {
                    printer.space();
                    format_alias(child, printer);
                } else {
                    format_expression(child, printer);
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
pub(crate) fn format_alias(node: &SyntaxNode, printer: &mut PgPrinter) {
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
pub(crate) fn format_join_expr(node: &SyntaxNode, printer: &mut PgPrinter) {
    // Check if the first child is a nested JOIN_EXPR
    let first_child = node.children().next();
    let has_nested_join = first_child
        .as_ref()
        .is_some_and(|c| c.kind() == SyntaxKind::JOIN_EXPR);

    if has_nested_join {
        // Recursively format the nested join first
        if let Some(nested_join) = first_child {
            format_join_expr(nested_join, printer);
        }
    } else {
        // No nested join - format the left table (first TABLE_REF)
        let left_table = node.children().find(|c| c.kind() == SyntaxKind::TABLE_REF);
        if let Some(table) = left_table {
            format_table_ref(table, printer);
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
pub(crate) fn format_where_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    format_expression_children(node, printer);
}

/// Formats a GROUP BY clause.
pub(crate) fn format_group_by(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(child, printer);
        first = false;
    }
}

/// Formats an ORDER BY clause.
pub(crate) fn format_order_by(node: &SyntaxNode, printer: &mut PgPrinter) {
    let items: Vec<_> = node
        .children()
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
pub(crate) fn format_order_by_item(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(child, printer);
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
pub(crate) fn format_limit_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(child, printer);
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
pub(crate) fn format_insert(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("INSERT");
    printer.space();
    printer.write_keyword("INTO");
    printer.space();

    // Table name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && token.kind() == SyntaxKind::IDENT
        {
            printer.write_identifier(token.text());
            break;
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
pub(crate) fn format_column_list(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && token.kind() == SyntaxKind::IDENT
        {
            if !first {
                printer.write(",");
                printer.space();
            }
            printer.write_identifier(token.text());
            first = false;
        }
    }
}

/// Formats a VALUES clause.
pub(crate) fn format_values_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let rows: Vec<_> = node
        .children()
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
pub(crate) fn format_values_row(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write("(");
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(child, printer);
        first = false;
    }
    printer.write(")");
}

/// Formats a RETURNING clause.
pub(crate) fn format_returning_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut first = true;
    for child in node.children() {
        if !first {
            printer.write(",");
            printer.space();
        }
        format_expression(child, printer);
        first = false;
    }
}

/// Formats an UPDATE statement.
pub(crate) fn format_update(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("UPDATE");
    printer.space();

    // Table name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && token.kind() == SyntaxKind::IDENT
        {
            printer.write_identifier(token.text());
            break;
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
pub(crate) fn format_set_clause(node: &SyntaxNode, printer: &mut PgPrinter) {
    let assignments: Vec<_> = node
        .children()
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
pub(crate) fn format_set_item(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut saw_eq = false;
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => {
                format_expression(child, printer);
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
pub(crate) fn format_delete(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("DELETE");
    printer.space();
    printer.write_keyword("FROM");
    printer.space();

    // Table name
    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && token.kind() == SyntaxKind::IDENT
        {
            printer.write_identifier(token.text());
            break;
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
