//! DDL formatting under the pgFormatter style engine.

use banshee_format::{PgFormatterConfig, format_with_pgformatter};

fn pg(sql: &str) -> String {
    format_with_pgformatter(sql, &PgFormatterConfig::default())
}

#[test]
fn create_table() {
    let out = pg(
        "create table t (id bigint primary key, name varchar(255), constraint u unique (name));",
    );
    insta::assert_snapshot!(out);
}

#[test]
fn create_index() {
    let out = pg("create unique index concurrently i on public.t using btree (a, b) where a > 0;");
    insta::assert_snapshot!(out);
}

#[test]
fn alter_table() {
    let out = pg(
        "alter table t add column c int not null default 0, drop column d, alter column e type bigint;",
    );
    insta::assert_snapshot!(out);
}

#[test]
fn idempotent() {
    for sql in [
        "create table t (id bigint primary key, name varchar(255));",
        "create index concurrently i on t using btree (a) where a > 0;",
        "alter table t add column c int, drop column d;",
        "alter table t rename to t2;",
    ] {
        let once = pg(sql);
        let twice = pg(&once);
        assert_eq!(once, twice, "not idempotent for `{sql}`:\n{once}");
    }
}
