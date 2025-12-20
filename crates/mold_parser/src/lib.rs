mod event;
mod grammar;
mod parser;
mod sink;
mod token_set;

pub use mold_syntax::SyntaxKind;
pub use token_set::TokenSet;

use mold_lexer::tokenize;
use mold_syntax::Parse;

pub fn parse(source: &str) -> Parse {
    let tokens = tokenize(source);
    let mut parser = parser::Parser::new(&tokens, source);
    grammar::root(&mut parser);
    let events = parser.finish();
    let (green, errors, interner) = sink::Sink::new(&tokens, source, events).finish();
    Parse::new(green, errors, interner)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn format_tree(source: &str) -> String {
        let parse = parse(source);
        let node = parse.syntax();
        format_node(&node, 0)
    }

    fn format_node(node: &mold_syntax::SyntaxNode, indent: usize) -> String {
        use cstree::util::NodeOrToken;
        let mut result = String::new();
        let prefix = "  ".repeat(indent);

        result.push_str(&format!(
            "{}{}@{:?}\n",
            prefix,
            format!("{:?}", node.kind()),
            node.text_range()
        ));

        for child in node.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    result.push_str(&format_node(&n, indent + 1));
                }
                NodeOrToken::Token(t) => {
                    result.push_str(&format!(
                        "{}  {:?}@{:?} {:?}\n",
                        prefix,
                        t.kind(),
                        t.text_range(),
                        t.text()
                    ));
                }
            }
        }

        result
    }

    #[test]
    fn test_simple_select() {
        let sql = "SELECT id, name FROM users";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_select_where() {
        let sql = "SELECT * FROM users WHERE active = true";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_select_join() {
        let sql = "SELECT u.id, p.title FROM users u JOIN posts p ON u.id = p.user_id";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_jsonb_access() {
        let sql = "SELECT data->'name'->>'first' FROM users";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_complex_expression() {
        let sql = "SELECT (a + b) * c, CASE WHEN x THEN y ELSE z END FROM t";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_aggregate_functions() {
        let sql = "SELECT count(*), sum(amount) FROM orders";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_aggregate_order_by() {
        let sql = "SELECT array_agg(name ORDER BY id) FROM users";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_window_function() {
        let sql = "SELECT row_number() OVER (PARTITION BY dept ORDER BY salary DESC) FROM emp";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_window_frame() {
        let sql = "SELECT SUM(x) OVER (ORDER BY id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_table_function() {
        let sql = "SELECT * FROM generate_series(1, 10) AS s(n)";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_cte() {
        let sql = "WITH cte AS (SELECT id FROM users) SELECT * FROM cte";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_subquery() {
        let sql = "SELECT * FROM (SELECT id FROM users) sub WHERE id > 10";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_jsonb_contains() {
        let sql = "SELECT * FROM users WHERE data @> '{\"active\": true}'";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_cast_expression() {
        let sql = "SELECT id::text, CAST(amount AS numeric(10, 2)) FROM t";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_coalesce_nullif() {
        let sql = "SELECT COALESCE(a, b, c), NULLIF(x, y) FROM t";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_filter_clause() {
        let sql = "SELECT count(*) FILTER (WHERE active) FROM users";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_jsonpath_exists() {
        let sql = "SELECT * FROM users WHERE data @? '$.name'";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_jsonpath_match() {
        let sql = "SELECT * FROM users WHERE data @@ '$.active == true'";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    // =========================================================================
    // INSERT statement tests
    // =========================================================================

    #[test]
    fn test_insert_values() {
        let sql = "INSERT INTO users (id, name) VALUES (1, 'Alice')";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_insert_multiple_rows() {
        let sql = "INSERT INTO users (id, name) VALUES (1, 'Alice'), (2, 'Bob')";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_insert_select() {
        let sql = "INSERT INTO archive SELECT * FROM users WHERE deleted = true";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_insert_default_values() {
        let sql = "INSERT INTO counters DEFAULT VALUES";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_insert_on_conflict_nothing() {
        let sql = "INSERT INTO users (id, name) VALUES (1, 'Alice') ON CONFLICT DO NOTHING";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_insert_on_conflict_update() {
        let sql = "INSERT INTO users (id, name) VALUES (1, 'Alice') ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_insert_returning() {
        let sql = "INSERT INTO users (name) VALUES ('Alice') RETURNING id";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    // =========================================================================
    // UPDATE statement tests
    // =========================================================================

    #[test]
    fn test_update_simple() {
        let sql = "UPDATE users SET active = true WHERE id = 1";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_update_multiple_columns() {
        let sql = "UPDATE users SET name = 'Bob', email = 'bob@example.com' WHERE id = 1";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_update_from() {
        let sql = "UPDATE users SET email = c.email FROM contacts c WHERE users.id = c.user_id";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_update_returning() {
        let sql = "UPDATE users SET active = true WHERE id = 1 RETURNING *";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    // =========================================================================
    // DELETE statement tests
    // =========================================================================

    #[test]
    fn test_delete_simple() {
        let sql = "DELETE FROM users WHERE id = 1";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_delete_using() {
        let sql = "DELETE FROM users USING inactive_list WHERE users.id = inactive_list.user_id";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_delete_returning() {
        let sql = "DELETE FROM users WHERE id = 1 RETURNING id, name";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    // =========================================================================
    // CTE advanced features
    // =========================================================================

    #[test]
    fn test_cte_materialized() {
        let sql = "WITH cached AS MATERIALIZED (SELECT * FROM big_table) SELECT * FROM cached";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_cte_not_materialized() {
        let sql = "WITH inline AS NOT MATERIALIZED (SELECT id FROM t) SELECT * FROM inline";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_cte_data_modifying_insert() {
        let sql = "WITH inserted AS (INSERT INTO logs (msg) VALUES ('test') RETURNING id) SELECT * FROM inserted";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_cte_data_modifying_delete() {
        let sql = "WITH deleted AS (DELETE FROM old_data WHERE expired = true RETURNING id) SELECT count(*) FROM deleted";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    // =========================================================================
    // ANY/ALL/SOME operators
    // =========================================================================

    #[test]
    fn test_any_array() {
        let sql = "SELECT * FROM users WHERE id = ANY(ARRAY[1, 2, 3])";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_any_subquery() {
        let sql = "SELECT * FROM products WHERE price > ANY(SELECT avg_price FROM categories)";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_all_subquery() {
        let sql = "SELECT * FROM products WHERE price > ALL(SELECT min_price FROM categories)";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }

    #[test]
    fn test_some_array() {
        let sql = "SELECT * FROM users WHERE status = SOME(ARRAY['active', 'pending'])";
        let parse = parse(sql);
        assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
        insta::assert_snapshot!(format_tree(sql));
    }
}
