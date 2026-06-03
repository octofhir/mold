//! Statement formatting for the pgFormatter style. Split from the module
//! root for maintainability; see `super` for the printer and dispatch.

use mold_syntax::{SyntaxKind, SyntaxNode, SyntaxToken};

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
    // Tracks whether the previous token was an identifier, so a bare second
    // identifier (an implicit alias, `users a`) is separated by a space while a
    // dotted qualifier (`schema.table`) is not.
    let mut prev_ident = false;
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
                prev_ident = false;
            }
            cstree::util::NodeOrToken::Token(token) => match token.kind() {
                SyntaxKind::IDENT => {
                    if prev_ident {
                        printer.space();
                    }
                    printer.write_identifier(token.text());
                    prev_ident = true;
                }
                SyntaxKind::QUOTED_IDENT => {
                    if prev_ident {
                        printer.space();
                    }
                    printer.write(token.text());
                    prev_ident = true;
                }
                SyntaxKind::DOT => {
                    printer.write(".");
                    prev_ident = false;
                }
                _ => {}
            },
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

/// Constraint node kinds that may appear in a column definition list.
const CONSTRAINT_KINDS: &[SyntaxKind] = &[
    SyntaxKind::PRIMARY_KEY_CONSTRAINT,
    SyntaxKind::FOREIGN_KEY_CONSTRAINT,
    SyntaxKind::UNIQUE_CONSTRAINT,
    SyntaxKind::CHECK_CONSTRAINT,
    SyntaxKind::NOT_NULL_CONSTRAINT,
    SyntaxKind::DEFAULT_EXPR,
    SyntaxKind::CONSTRAINT,
];

fn is_constraint_kind(kind: SyntaxKind) -> bool {
    CONSTRAINT_KINDS.contains(&kind)
}

pub(crate) fn format_create_table(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("CREATE");
    printer.space();

    for element in node.children_with_tokens() {
        if let cstree::util::NodeOrToken::Token(token) = element
            && matches!(
                token.kind(),
                SyntaxKind::TEMP_KW | SyntaxKind::TEMPORARY_KW | SyntaxKind::UNLOGGED_KW
            )
        {
            printer.write_keyword(token.text());
            printer.space();
        }
    }

    printer.write_keyword("TABLE");
    printer.space();

    if has_token(node, SyntaxKind::IF_KW) {
        printer.write_keyword("IF");
        printer.space();
        printer.write_keyword("NOT");
        printer.space();
        printer.write_keyword("EXISTS");
        printer.space();
    }

    if let Some(table_name) = find_child(node, SyntaxKind::TABLE_REF) {
        format_inline(&table_name, printer, &mut Inline::default());
    }

    if let Some(query) = find_child(node, SyntaxKind::SELECT_STMT) {
        printer.space();
        printer.write_keyword("AS");
        printer.newline();
        format_select(&query, printer);
        return;
    }

    if let Some(columns) = find_child(node, SyntaxKind::COLUMN_DEF_LIST) {
        printer.space();
        printer.write("(");
        printer.newline();
        printer.indent();

        let elements: Vec<SyntaxNode> = columns
            .children()
            .filter(|c| c.kind() == SyntaxKind::COLUMN_DEF || is_constraint_kind(c.kind()))
            .cloned()
            .collect();
        for (i, element) in elements.iter().enumerate() {
            if i > 0 {
                printer.write(",");
                printer.newline();
            }
            format_inline(element, printer, &mut Inline::default());
        }

        printer.dedent();
        printer.newline();
        printer.write(")");
    }

    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

/// A `CREATE INDEX` statement: header inline, partial `WHERE` on its own line.
pub(crate) fn format_create_index(node: &SyntaxNode, printer: &mut PgPrinter) {
    let mut state = Inline::default();
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Token(token) => {
                if matches!(token.kind(), SyntaxKind::SEMICOLON | SyntaxKind::WHERE_KW) {
                    continue;
                }
                emit_inline_token(token, printer, &mut state);
            }
            cstree::util::NodeOrToken::Node(child) => {
                if child.kind() == SyntaxKind::WHERE_CLAUSE {
                    printer.newline();
                    printer.write_keyword("WHERE");
                    let mut where_state = Inline {
                        prev_word: true,
                        prev_kind: Some(SyntaxKind::WHERE_KW),
                    };
                    format_inline(child, printer, &mut where_state);
                } else {
                    format_inline(child, printer, &mut state);
                }
            }
        }
    }
    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

/// An `ALTER TABLE` statement: header inline, one action per line.
pub(crate) fn format_alter(node: &SyntaxNode, printer: &mut PgPrinter) {
    printer.write_keyword("ALTER");
    printer.space();
    printer.write_keyword("TABLE");
    printer.space();

    if has_token(node, SyntaxKind::IF_KW) {
        printer.write_keyword("IF");
        printer.space();
        printer.write_keyword("EXISTS");
        printer.space();
    }
    if has_token(node, SyntaxKind::ONLY_KW) {
        printer.write_keyword("ONLY");
        printer.space();
    }

    if let Some(table_name) = find_child(node, SyntaxKind::TABLE_REF) {
        format_inline(&table_name, printer, &mut Inline::default());
    }

    let actions: Vec<SyntaxNode> = node
        .children()
        .filter(|c| c.kind() == SyntaxKind::ALTER_TABLE_ACTION)
        .cloned()
        .collect();
    if actions.is_empty() {
        let mut state = Inline {
            prev_word: true,
            prev_kind: Some(SyntaxKind::TABLE_KW),
        };
        for element in node.children_with_tokens() {
            if let cstree::util::NodeOrToken::Token(token) = element
                && !matches!(
                    token.kind(),
                    SyntaxKind::ALTER_KW
                        | SyntaxKind::TABLE_KW
                        | SyntaxKind::ONLY_KW
                        | SyntaxKind::SEMICOLON
                )
                && !token.kind().is_trivia()
            {
                emit_inline_token(token, printer, &mut state);
            }
        }
        if has_token(node, SyntaxKind::SEMICOLON) {
            printer.write(";");
        }
        return;
    }

    printer.indent();
    for (i, action) in actions.iter().enumerate() {
        if i > 0 {
            printer.write(",");
        }
        printer.newline();
        format_inline(action, printer, &mut Inline::default());
    }
    printer.dedent();

    if has_token(node, SyntaxKind::SEMICOLON) {
        printer.write(";");
    }
}

/// Spacing state for [`format_inline`].
#[derive(Default)]
struct Inline {
    prev_word: bool,
    prev_kind: Option<SyntaxKind>,
}

fn space_before_paren(prev_kind: Option<SyntaxKind>) -> bool {
    matches!(
        prev_kind,
        Some(SyntaxKind::KEY_KW | SyntaxKind::UNIQUE_KW | SyntaxKind::CHECK_KW | SyntaxKind::IN_KW)
    )
}

/// Walks a node's tokens with single spaces between word tokens and none around
/// punctuation — the spacing DDL fragments want.
fn format_inline(node: &SyntaxNode, printer: &mut PgPrinter, state: &mut Inline) {
    for element in node.children_with_tokens() {
        match element {
            cstree::util::NodeOrToken::Node(child) => format_inline(child, printer, state),
            cstree::util::NodeOrToken::Token(token) => {
                if token.kind() == SyntaxKind::SEMICOLON {
                    continue;
                }
                emit_inline_token(token, printer, state);
            }
        }
    }
}

fn emit_inline_token(token: &SyntaxToken, printer: &mut PgPrinter, state: &mut Inline) {
    let kind = token.kind();
    if kind.is_trivia() {
        return;
    }
    match kind {
        SyntaxKind::COMMA => {
            printer.write(",");
            printer.space();
            state.prev_word = false;
        }
        SyntaxKind::L_PAREN => {
            if space_before_paren(state.prev_kind) {
                printer.space();
            }
            printer.write("(");
            state.prev_word = false;
        }
        SyntaxKind::R_PAREN => {
            printer.write(")");
            state.prev_word = true;
        }
        SyntaxKind::L_BRACKET => {
            printer.write("[");
            state.prev_word = false;
        }
        SyntaxKind::R_BRACKET => {
            printer.write("]");
            state.prev_word = true;
        }
        SyntaxKind::DOT => {
            printer.write(".");
            state.prev_word = false;
        }
        k if k.is_keyword() => {
            if state.prev_word {
                printer.space();
            }
            printer.write_keyword(token.text());
            state.prev_word = true;
        }
        SyntaxKind::IDENT => {
            if state.prev_word {
                printer.space();
            }
            printer.write_identifier(token.text());
            state.prev_word = true;
        }
        _ => {
            if state.prev_word {
                printer.space();
            }
            printer.write(token.text());
            state.prev_word = true;
        }
    }
    state.prev_kind = Some(kind);
}
