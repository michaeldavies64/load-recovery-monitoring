# Pipeline section: load the raw files into the staging schema, and the hand-written
# reference tables (glossaries, known events, player profile) into the reference schema.
# Each table is read with its reader from utils/read_data.R and written unchanged.
# Everything is written in one transaction, so a failure leaves the previous load intact.

staging_readers <- list(
  gps        = read_gps,
  recovery   = read_recovery,
  capability = read_capability,
  priorities = read_priorities
)

reference_readers <- list(
  gps_glossary      = read_gps_glossary,
  recovery_glossary = read_recovery_glossary,
  events            = read_events,
  player_profile    = read_player_profile
)

load_staging <- function(con, input_dir = here::here("input")) {
  # Read everything first, so a file that fails to parse stops the load before any write
  tables <- lapply(staging_readers, function(reader) reader(input_dir))
  reference <- lapply(reference_readers, function(reader) reader())

  DBI::dbWithTransaction(con, {
    for (name in names(tables)) db_write_table(con, "staging", name, tables[[name]])
    for (name in names(reference)) db_write_table(con, "reference", name, reference[[name]])
  })

  rbind(db_table_counts(con, "staging"), db_table_counts(con, "reference"))
}
