# Pipeline section: build the features schema from staging with the SQL in queries/.
# Features are derived inputs (labels, flags, reshaping, rolling loads), not model outputs.
# Tables are built in the order listed, so later queries can use earlier tables.
# The reasoning behind each choice is in docs/feature_engineering.qmd.

feature_tables <- c(
  daily_load        = "daily_load.sql",
  load_long         = "load_long.sql",
  match_reference   = "match_reference.sql",
  load_by_match_day = "load_by_match_day.sql"
)

build_features <- function(con) {
  DBI::dbWithTransaction(con, {
    for (name in names(feature_tables)) db_build_table(con, "features", name, feature_tables[[name]])
  })
  db_table_counts(con, "features")
}
