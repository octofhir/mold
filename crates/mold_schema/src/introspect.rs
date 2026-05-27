//! Live Postgres schema introspection (requires the `db` feature).
//!
//! Queries `information_schema` for tables, columns, primary keys and
//! functions in a target schema and assembles a [`SchemaSnapshot`]. The result
//! is plain data and can be cached and reused without a database connection.

use std::collections::{HashMap, HashSet};
use std::time::Duration;

use sqlx::Row;
use sqlx::postgres::PgPoolOptions;

use crate::snapshot::{
    ColumnEntry, FunctionEntry, SchemaSnapshot, TableEntry, TableKind, fingerprint,
};

/// Connects to `url`, introspects `schema`, and returns a snapshot.
///
/// `pool_size` bounds connections; `connect_timeout` caps the initial connect.
pub async fn introspect(
    url: &str,
    schema: &str,
    pool_size: u32,
    connect_timeout: Duration,
) -> Result<SchemaSnapshot, sqlx::Error> {
    let pool = PgPoolOptions::new()
        .max_connections(pool_size.max(1))
        .acquire_timeout(connect_timeout)
        .connect(url)
        .await?;

    let mut snapshot = SchemaSnapshot::new(fingerprint(url, schema), schema);

    let primary_keys = fetch_primary_keys(&pool, schema).await?;
    let mut tables = fetch_tables(&pool, schema).await?;
    attach_columns(&pool, schema, &mut tables, &primary_keys).await?;
    snapshot.tables = tables.into_values().collect();
    snapshot.tables.sort_by(|a, b| a.name.cmp(&b.name));
    snapshot.functions = fetch_functions(&pool, schema).await?;

    pool.close().await;
    Ok(snapshot)
}

fn table_kind(raw: &str) -> TableKind {
    match raw {
        "VIEW" => TableKind::View,
        "FOREIGN TABLE" | "FOREIGN" => TableKind::ForeignTable,
        _ => TableKind::Table,
    }
}

async fn fetch_tables(
    pool: &sqlx::PgPool,
    schema: &str,
) -> Result<HashMap<String, TableEntry>, sqlx::Error> {
    let rows = sqlx::query(
        "SELECT table_name, table_type \
         FROM information_schema.tables \
         WHERE table_schema = $1",
    )
    .bind(schema)
    .fetch_all(pool)
    .await?;

    let mut tables = HashMap::new();
    for row in rows {
        let name: String = row.get("table_name");
        let kind: String = row.get("table_type");
        tables.insert(
            name.clone(),
            TableEntry {
                schema: schema.to_string(),
                name,
                kind: table_kind(&kind),
                columns: Vec::new(),
            },
        );
    }
    Ok(tables)
}

async fn attach_columns(
    pool: &sqlx::PgPool,
    schema: &str,
    tables: &mut HashMap<String, TableEntry>,
    primary_keys: &HashMap<String, HashSet<String>>,
) -> Result<(), sqlx::Error> {
    let rows = sqlx::query(
        "SELECT table_name, column_name, data_type, is_nullable, ordinal_position::int AS ord \
         FROM information_schema.columns \
         WHERE table_schema = $1 \
         ORDER BY table_name, ordinal_position",
    )
    .bind(schema)
    .fetch_all(pool)
    .await?;

    for row in rows {
        let table_name: String = row.get("table_name");
        let Some(table) = tables.get_mut(&table_name) else {
            continue;
        };
        let name: String = row.get("column_name");
        let data_type: String = row.get("data_type");
        let is_nullable: String = row.get("is_nullable");
        let ord: i32 = row.get("ord");
        let is_pk = primary_keys
            .get(&table_name)
            .is_some_and(|cols| cols.contains(&name));

        table.columns.push(ColumnEntry {
            name,
            data_type,
            nullable: is_nullable.eq_ignore_ascii_case("YES"),
            is_primary_key: is_pk,
            ordinal: ord.saturating_sub(1).max(0) as usize,
        });
    }
    Ok(())
}

async fn fetch_primary_keys(
    pool: &sqlx::PgPool,
    schema: &str,
) -> Result<HashMap<String, HashSet<String>>, sqlx::Error> {
    let rows = sqlx::query(
        "SELECT tc.table_name, kcu.column_name \
         FROM information_schema.table_constraints tc \
         JOIN information_schema.key_column_usage kcu \
           ON tc.constraint_name = kcu.constraint_name \
          AND tc.table_schema = kcu.table_schema \
         WHERE tc.constraint_type = 'PRIMARY KEY' AND tc.table_schema = $1",
    )
    .bind(schema)
    .fetch_all(pool)
    .await?;

    let mut map: HashMap<String, HashSet<String>> = HashMap::new();
    for row in rows {
        let table: String = row.get("table_name");
        let column: String = row.get("column_name");
        map.entry(table).or_default().insert(column);
    }
    Ok(map)
}

async fn fetch_functions(
    pool: &sqlx::PgPool,
    schema: &str,
) -> Result<Vec<FunctionEntry>, sqlx::Error> {
    let rows = sqlx::query(
        "SELECT routine_name, data_type \
         FROM information_schema.routines \
         WHERE routine_schema = $1 AND routine_type = 'FUNCTION'",
    )
    .bind(schema)
    .fetch_all(pool)
    .await?;

    Ok(rows
        .into_iter()
        .map(|row| FunctionEntry {
            schema: schema.to_string(),
            name: row.get("routine_name"),
            return_type: row
                .try_get::<String, _>("data_type")
                .unwrap_or_else(|_| "void".to_string()),
        })
        .collect())
}
