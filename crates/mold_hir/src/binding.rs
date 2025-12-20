//! Binding types for semantic analysis.
//!
//! Bindings represent resolved references to tables, columns, and CTEs
//! within a query's scope.

use std::sync::Arc;
use text_size::TextRange;

/// A binding to a table in a query scope.
///
/// Tables can come from various sources: physical tables, CTEs, subqueries,
/// or table-valued functions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TableBinding {
    /// The name used to reference this table in the query.
    /// This is the alias if one exists, otherwise the table name.
    pub name: String,

    /// The original table name (before aliasing).
    pub original_name: String,

    /// The alias assigned to this table, if any.
    pub alias: Option<String>,

    /// The source of this table.
    pub source: TableSource,

    /// Columns available from this table.
    /// Empty if schema information is not available.
    pub columns: Vec<ColumnBinding>,

    /// The text range in the source where this binding was introduced.
    pub range: Option<TextRange>,
}

impl TableBinding {
    /// Creates a new table binding from a physical table.
    pub fn table(schema: Option<String>, name: String) -> Self {
        let reference_name = name.clone();
        Self {
            name: reference_name,
            original_name: name.clone(),
            alias: None,
            source: TableSource::Table { schema, name },
            columns: Vec::new(),
            range: None,
        }
    }

    /// Creates a new table binding from a CTE reference.
    pub fn cte(name: String) -> Self {
        Self {
            name: name.clone(),
            original_name: name.clone(),
            alias: None,
            source: TableSource::Cte { name },
            columns: Vec::new(),
            range: None,
        }
    }

    /// Creates a new table binding from a subquery.
    pub fn subquery(alias: String) -> Self {
        Self {
            name: alias.clone(),
            original_name: alias.clone(),
            alias: Some(alias.clone()),
            source: TableSource::Subquery,
            columns: Vec::new(),
            range: None,
        }
    }

    /// Creates a new table binding from a function call.
    pub fn function(name: String, returns_table: bool) -> Self {
        Self {
            name: name.clone(),
            original_name: name.clone(),
            alias: None,
            source: TableSource::Function { name, returns_table },
            columns: Vec::new(),
            range: None,
        }
    }

    /// Creates a new table binding from a VALUES clause.
    pub fn values(alias: String) -> Self {
        Self {
            name: alias.clone(),
            original_name: alias.clone(),
            alias: Some(alias),
            source: TableSource::Values,
            columns: Vec::new(),
            range: None,
        }
    }

    /// Sets the alias for this table binding.
    pub fn with_alias(mut self, alias: String) -> Self {
        self.name = alias.clone();
        self.alias = Some(alias);
        self
    }

    /// Sets the columns for this table binding.
    pub fn with_columns(mut self, columns: Vec<ColumnBinding>) -> Self {
        self.columns = columns;
        self
    }

    /// Sets the text range for this binding.
    pub fn with_range(mut self, range: TextRange) -> Self {
        self.range = Some(range);
        self
    }

    /// Returns the effective name used to reference this table.
    pub fn reference_name(&self) -> &str {
        &self.name
    }

    /// Returns true if this table has an alias.
    pub fn has_alias(&self) -> bool {
        self.alias.is_some()
    }

    /// Finds a column by name (case-insensitive).
    pub fn find_column(&self, name: &str) -> Option<&ColumnBinding> {
        let name_lower = name.to_lowercase();
        self.columns.iter().find(|c| c.name.to_lowercase() == name_lower)
    }
}

/// The source of a table binding.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TableSource {
    /// A physical table in the database.
    Table {
        /// The schema name, if specified.
        schema: Option<String>,
        /// The table name.
        name: String,
    },

    /// A Common Table Expression (CTE).
    Cte {
        /// The CTE name.
        name: String,
    },

    /// A subquery used as a table source.
    Subquery,

    /// A table-valued function.
    Function {
        /// The function name.
        name: String,
        /// Whether this function returns a table (SETOF).
        returns_table: bool,
    },

    /// A VALUES clause used as a table source.
    Values,
}

impl TableSource {
    /// Returns true if this is a physical table.
    pub fn is_table(&self) -> bool {
        matches!(self, TableSource::Table { .. })
    }

    /// Returns true if this is a CTE.
    pub fn is_cte(&self) -> bool {
        matches!(self, TableSource::Cte { .. })
    }

    /// Returns true if this is a subquery.
    pub fn is_subquery(&self) -> bool {
        matches!(self, TableSource::Subquery)
    }

    /// Returns true if this is a function.
    pub fn is_function(&self) -> bool {
        matches!(self, TableSource::Function { .. })
    }
}

/// A binding to a column in a query scope.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnBinding {
    /// The column name.
    pub name: String,

    /// The table this column belongs to, if known.
    pub table: Option<String>,

    /// The data type of this column, if known.
    pub data_type: Option<DataType>,

    /// The ordinal position of this column (0-indexed).
    pub ordinal: usize,

    /// The text range in the source where this binding was introduced.
    pub range: Option<TextRange>,

    /// Whether this column is nullable.
    pub nullable: Option<bool>,
}

impl ColumnBinding {
    /// Creates a new column binding.
    pub fn new(name: String, ordinal: usize) -> Self {
        Self {
            name,
            table: None,
            data_type: None,
            ordinal,
            range: None,
            nullable: None,
        }
    }

    /// Sets the table for this column binding.
    pub fn with_table(mut self, table: String) -> Self {
        self.table = Some(table);
        self
    }

    /// Sets the data type for this column binding.
    pub fn with_type(mut self, data_type: DataType) -> Self {
        self.data_type = Some(data_type);
        self
    }

    /// Sets the nullability for this column binding.
    pub fn with_nullable(mut self, nullable: bool) -> Self {
        self.nullable = Some(nullable);
        self
    }

    /// Sets the text range for this binding.
    pub fn with_range(mut self, range: TextRange) -> Self {
        self.range = Some(range);
        self
    }

    /// Returns the fully qualified name (table.column) if table is known.
    pub fn qualified_name(&self) -> String {
        match &self.table {
            Some(table) => format!("{}.{}", table, self.name),
            None => self.name.clone(),
        }
    }
}

/// A binding to a Common Table Expression (CTE).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CteBinding {
    /// The CTE name.
    pub name: String,

    /// The column names defined for this CTE.
    pub columns: Vec<String>,

    /// Whether this is a recursive CTE.
    pub is_recursive: bool,

    /// Whether this CTE has been materialized.
    pub is_materialized: Option<bool>,

    /// The text range of the CTE definition.
    pub range: Option<TextRange>,
}

impl CteBinding {
    /// Creates a new CTE binding.
    pub fn new(name: String) -> Self {
        Self {
            name,
            columns: Vec::new(),
            is_recursive: false,
            is_materialized: None,
            range: None,
        }
    }

    /// Sets the columns for this CTE.
    pub fn with_columns(mut self, columns: Vec<String>) -> Self {
        self.columns = columns;
        self
    }

    /// Marks this CTE as recursive.
    pub fn recursive(mut self) -> Self {
        self.is_recursive = true;
        self
    }

    /// Sets the materialization hint.
    pub fn with_materialized(mut self, materialized: bool) -> Self {
        self.is_materialized = Some(materialized);
        self
    }

    /// Sets the text range for this binding.
    pub fn with_range(mut self, range: TextRange) -> Self {
        self.range = Some(range);
        self
    }
}

/// SQL data types for type tracking.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataType {
    // Numeric types
    SmallInt,
    Integer,
    BigInt,
    Real,
    DoublePrecision,
    Numeric { precision: Option<u32>, scale: Option<u32> },

    // Character types
    Char { length: Option<u32> },
    VarChar { length: Option<u32> },
    Text,

    // Binary types
    ByteA,

    // Date/time types
    Date,
    Time { with_timezone: bool },
    Timestamp { with_timezone: bool },
    Interval,

    // Boolean
    Boolean,

    // UUID
    Uuid,

    // JSON types
    Json,
    Jsonb,

    // Array type
    Array(Box<DataType>),

    // User-defined or unknown type
    Custom(String),

    // Type is unknown/unresolved
    Unknown,
}

impl DataType {
    /// Returns true if this is a numeric type.
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            DataType::SmallInt
                | DataType::Integer
                | DataType::BigInt
                | DataType::Real
                | DataType::DoublePrecision
                | DataType::Numeric { .. }
        )
    }

    /// Returns true if this is a text/string type.
    pub fn is_text(&self) -> bool {
        matches!(
            self,
            DataType::Char { .. } | DataType::VarChar { .. } | DataType::Text
        )
    }

    /// Returns true if this is a JSON type.
    pub fn is_json(&self) -> bool {
        matches!(self, DataType::Json | DataType::Jsonb)
    }

    /// Returns true if this is a JSONB type.
    pub fn is_jsonb(&self) -> bool {
        matches!(self, DataType::Jsonb)
    }

    /// Returns true if this is a date/time type.
    pub fn is_temporal(&self) -> bool {
        matches!(
            self,
            DataType::Date
                | DataType::Time { .. }
                | DataType::Timestamp { .. }
                | DataType::Interval
        )
    }

    /// Returns true if this is an array type.
    pub fn is_array(&self) -> bool {
        matches!(self, DataType::Array(_))
    }

    /// Returns the element type if this is an array.
    pub fn element_type(&self) -> Option<&DataType> {
        match self {
            DataType::Array(elem) => Some(elem),
            _ => None,
        }
    }
}

impl std::fmt::Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::SmallInt => write!(f, "smallint"),
            DataType::Integer => write!(f, "integer"),
            DataType::BigInt => write!(f, "bigint"),
            DataType::Real => write!(f, "real"),
            DataType::DoublePrecision => write!(f, "double precision"),
            DataType::Numeric { precision, scale } => match (precision, scale) {
                (Some(p), Some(s)) => write!(f, "numeric({}, {})", p, s),
                (Some(p), None) => write!(f, "numeric({})", p),
                _ => write!(f, "numeric"),
            },
            DataType::Char { length } => match length {
                Some(len) => write!(f, "char({})", len),
                None => write!(f, "char"),
            },
            DataType::VarChar { length } => match length {
                Some(len) => write!(f, "varchar({})", len),
                None => write!(f, "varchar"),
            },
            DataType::Text => write!(f, "text"),
            DataType::ByteA => write!(f, "bytea"),
            DataType::Date => write!(f, "date"),
            DataType::Time { with_timezone } => {
                if *with_timezone {
                    write!(f, "time with time zone")
                } else {
                    write!(f, "time")
                }
            }
            DataType::Timestamp { with_timezone } => {
                if *with_timezone {
                    write!(f, "timestamp with time zone")
                } else {
                    write!(f, "timestamp")
                }
            }
            DataType::Interval => write!(f, "interval"),
            DataType::Boolean => write!(f, "boolean"),
            DataType::Uuid => write!(f, "uuid"),
            DataType::Json => write!(f, "json"),
            DataType::Jsonb => write!(f, "jsonb"),
            DataType::Array(elem) => write!(f, "{}[]", elem),
            DataType::Custom(name) => write!(f, "{}", name),
            DataType::Unknown => write!(f, "unknown"),
        }
    }
}

/// A resolved reference to a column.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ResolvedColumn {
    /// The table binding this column belongs to.
    pub table: Arc<TableBinding>,

    /// The column binding.
    pub column: ColumnBinding,
}

/// A resolved reference that may be ambiguous.
#[derive(Clone, Debug)]
pub enum Resolution<T> {
    /// Successfully resolved to a single binding.
    Resolved(T),

    /// Reference is ambiguous - multiple bindings match.
    Ambiguous(Vec<T>),

    /// Reference could not be resolved.
    Unresolved,
}

impl<T> Resolution<T> {
    /// Returns the resolved value if unambiguous.
    pub fn ok(self) -> Option<T> {
        match self {
            Resolution::Resolved(v) => Some(v),
            _ => None,
        }
    }

    /// Returns true if the resolution is ambiguous.
    pub fn is_ambiguous(&self) -> bool {
        matches!(self, Resolution::Ambiguous(_))
    }

    /// Returns true if the resolution failed.
    pub fn is_unresolved(&self) -> bool {
        matches!(self, Resolution::Unresolved)
    }
}
