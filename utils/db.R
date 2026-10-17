# Connection helpers for the project's DuckDB database.
# The database is a single file in data/, rebuilt from the raw files by the pipeline
# and never committed. Tables are grouped into schemas:
#   staging   one table per source file, as read by utils/read_data.R
#   analysis  tables built from staging with the SQL in queries/ (later chapters)

db_path <- file.path("data", "load_recovery.duckdb")

db_connect <- function(path = db_path, read_only = FALSE) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
}

db_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

# Opens a connection, runs fn(con) and always closes the connection afterwards,
# even if fn fails. Returns whatever fn returns.
with_db <- function(fn, path = db_path, read_only = FALSE) {
  con <- db_connect(path, read_only = read_only)
  on.exit(db_disconnect(con), add = TRUE)
  fn(con)
}

# Replaces schema.table with df, creating the schema if it doesn't exist yet
db_write_table <- function(con, schema, table, df) {
  DBI::dbExecute(con, paste("CREATE SCHEMA IF NOT EXISTS", DBI::dbQuoteIdentifier(con, schema)))
  DBI::dbWriteTable(con, DBI::Id(schema = schema, table = table), as.data.frame(df), overwrite = TRUE)
  invisible(nrow(df))
}

# Row count of every table in a schema, for checking a load
db_table_counts <- function(con, schema) {
  tables <- DBI::dbGetQuery(con, "
    SELECT table_name FROM information_schema.tables
    WHERE table_schema = ? ORDER BY table_name", params = list(schema))$table_name
  rows <- vapply(tables, function(t) {
    DBI::dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ",
                                DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = t))))$n
  }, numeric(1))
  data.frame(table = paste0(schema, ".", tables), rows = rows, row.names = NULL)
}
