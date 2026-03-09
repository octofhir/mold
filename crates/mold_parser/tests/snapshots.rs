use cstree::util::NodeOrToken;
use insta::assert_snapshot;
use mold_parser::parse;
use mold_syntax::SyntaxNode;

fn read_fixture(name: &str) -> String {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src")
        .join("fixtures")
        .join("valid")
        .join(name);

    std::fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read fixture {}: {err}", path.display()))
}

fn format_tree(source: &str) -> String {
    let parse = parse(source);
    format_node(&parse.syntax(), 0)
}

fn format_node(node: &SyntaxNode, indent: usize) -> String {
    let mut result = String::new();
    let prefix = "  ".repeat(indent);

    result.push_str(&format!(
        "{}{:?}@{:?}\n",
        prefix,
        node.kind(),
        node.text_range()
    ));

    for child in node.children_with_tokens() {
        match child {
            NodeOrToken::Node(n) => result.push_str(&format_node(n, indent + 1)),
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

fn render_parse(source: &str) -> String {
    let parse = parse(source);
    let mut lines = Vec::new();

    lines.push("errors:".to_string());
    if parse.errors().is_empty() {
        lines.push("- <none>".to_string());
    } else {
        for error in parse.errors() {
            lines.push(format!(
                "- {}..{} | {}",
                u32::from(error.range.start()),
                u32::from(error.range.end()),
                error.message
            ));
        }
    }

    lines.push("tree:".to_string());
    lines.push(format_tree(source));

    lines.join("\n")
}

#[test]
fn snapshot_join_alias_and_schema() {
    let sql = "SELECT u.id, o.total FROM public.users AS u JOIN orders o ON u.id = o.user_id";
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("join_alias_and_schema", render_parse(sql));
}

#[test]
fn snapshot_quoted_aliases() {
    let sql = r#"SELECT "o".id FROM "patient" "o""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("quoted_aliases", render_parse(sql));
}

#[test]
fn snapshot_quoted_qualified_identifiers() {
    let sql = r#"SELECT "User"."id" FROM "User" AS "AccountUser""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("quoted_qualified_identifiers", render_parse(sql));
}

#[test]
fn snapshot_quoted_cte_identifiers() {
    let sql = r#"WITH "RecentUsers"("UserId") AS (SELECT "User"."id" FROM "User") SELECT "RecentUsers"."UserId" FROM "RecentUsers""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("quoted_cte_identifiers", render_parse(sql));
}

#[test]
fn snapshot_quoted_on_conflict_constraint() {
    let sql = r#"INSERT INTO "User" ("id", "name") VALUES (1, 'Alice') ON CONFLICT ON CONSTRAINT "User_pkey" DO UPDATE SET "name" = EXCLUDED."name""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("quoted_on_conflict_constraint", render_parse(sql));
}

#[test]
fn snapshot_quoted_window_and_for_update() {
    let sql = r#"SELECT "User"."id", row_number() OVER "UserWindow" FROM "User" WINDOW "UserWindow" AS (PARTITION BY "User"."org_id" ORDER BY "User"."id") FOR UPDATE OF "User""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("quoted_window_and_for_update", render_parse(sql));
}

#[test]
fn snapshot_window_base_and_frame_clause() {
    let sql = r#"SELECT sum("amount") OVER "DerivedWindow" FROM "payments" WINDOW "BaseWindow" AS (PARTITION BY "account_id"), "DerivedWindow" AS ("BaseWindow" ORDER BY "created_at" ROWS BETWEEN 1 PRECEDING AND CURRENT ROW)"#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("window_base_and_frame_clause", render_parse(sql));
}

#[test]
fn snapshot_join_using_quoted_identifiers() {
    let sql = r#"SELECT * FROM "users" JOIN "orders" USING ("user_id", "tenant_id")"#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("join_using_quoted_identifiers", render_parse(sql));
}

#[test]
fn snapshot_for_no_key_update_skip_locked() {
    let sql = r#"SELECT * FROM "User" "u" FOR NO KEY UPDATE OF "u" SKIP LOCKED"#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("for_no_key_update_skip_locked", render_parse(sql));
}

#[test]
fn snapshot_for_key_share_nowait_multiple_relations() {
    let sql = r#"SELECT * FROM public.orders o FOR KEY SHARE OF o, public."users" NOWAIT"#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("for_key_share_nowait_multiple_relations", render_parse(sql));
}

#[test]
fn snapshot_top_level_with_insert() {
    let sql = r#"WITH "incoming" AS (SELECT 1 AS "id") INSERT INTO "users" ("id") SELECT "id" FROM "incoming""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("top_level_with_insert", render_parse(sql));
}

#[test]
fn snapshot_top_level_with_update() {
    let sql = r#"WITH "flagged" AS (SELECT "id" FROM "users") UPDATE "users" u SET "active" = true FROM "flagged" WHERE u."id" = "flagged"."id""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("top_level_with_update", render_parse(sql));
}

#[test]
fn snapshot_top_level_with_delete() {
    let sql = r#"WITH "stale" AS (SELECT "id" FROM "users") DELETE FROM "users" USING "stale" WHERE "users"."id" = "stale"."id""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("top_level_with_delete", render_parse(sql));
}

#[test]
fn snapshot_data_modifying_cte_returning() {
    let sql = r#"WITH changed AS (UPDATE orders SET amount = amount + 1 RETURNING user_id, amount) SELECT changed.user_id, changed.amount FROM changed"#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("data_modifying_cte_returning", render_parse(sql));
}

#[test]
fn snapshot_quoted_returning_aliases() {
    let sql = r#"INSERT INTO "User" ("name") VALUES ('Alice') RETURNING "id" AS "user_id", "name""#;
    let parse = parse(sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("quoted_returning_aliases", render_parse(sql));
}

#[test]
fn snapshot_fixture_with_recursive() {
    let sql = read_fixture("with_recursive.sql");
    let parse = parse(&sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("fixture_with_recursive", render_parse(&sql));
}

#[test]
fn snapshot_fixture_cte_data_modifying() {
    let sql = read_fixture("cte_data_modifying.sql");
    let parse = parse(&sql);

    assert!(parse.errors().is_empty(), "errors: {:?}", parse.errors());
    assert_snapshot!("fixture_cte_data_modifying", render_parse(&sql));
}

#[test]
fn snapshot_recovery_case_in_subquery() {
    let sql = "SELECT * FROM t WHERE x IN (SELECT CASE WHEN y THEN z FROM inner_t)";
    let parse = parse(sql);

    assert!(!parse.errors().is_empty(), "expected recovery errors");
    assert_snapshot!("recovery_case_in_subquery", render_parse(sql));
}

#[test]
fn snapshot_recovery_broken_with_clause() {
    let sql = "WITH cte AS (SELECT id FROM t SELECT * FROM cte";
    let parse = parse(sql);

    assert!(!parse.errors().is_empty(), "expected recovery errors");
    assert_snapshot!("recovery_broken_with_clause", render_parse(sql));
}
