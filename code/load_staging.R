# Pipeline section: load the four raw files into the staging schema.
# Each table is read with its reader from utils/read_data.R and written unchanged.
# All four are written in one transaction, so a failure leaves the previous load intact.

staging_readers <- list(
  gps        = read_gps,
  recovery   = read_recovery,
  capability = read_capability,
  priorities = read_priorities
)

load_staging <- function(con, input_dir = "input") {
  # Read everything first, so a file that fails to parse stops the load before any write
  tables <- lapply(staging_readers, function(reader) reader(input_dir))

  DBI::dbWithTransaction(con, {
    for (name in names(tables)) db_write_table(con, "staging", name, tables[[name]])
  })

  db_table_counts(con, "staging")
}
