# Getting the app's data.
# The app reads the project database read-only. If the database doesn't exist (for example
# on a fresh deployment, where data/ is not in the repository), it is built first by
# running the whole pipeline: download, staging, features and monitoring. The raw
# Vizathon files are fetched from Chelsea's public repository, never stored here.

ensure_database <- function(path = db_path) {
  if (file.exists(path)) return(invisible(path))
  message("No database found: building it from the raw files")
  for (f in c("load_staging.R", "build_features.R", "build_monitoring.R")) {
    source(here::here("code", f), encoding = "UTF-8")
  }
  result <- download_vizathon_data()
  if (any(result$status == "failed")) stop("Could not download the Vizathon data", call. = FALSE)
  with_db(path = path, function(con) {
    load_staging(con)
    build_features(con)
    build_monitoring(con)
  })
  invisible(path)
}

# The app's data, built or read the first time a visitor arrives and then kept for every
# later visitor. Called from the server, not at start-up, so the home page can be sent
# to the browser (and read) while a fresh deployment is still building the database.
get_app_data <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      ensure_database()
      cache <<- load_app_data()
    }
    cache
  }
})

# Everything the app shows, read once. The data is small (a few thousand
# rows), so holding it in memory keeps the app fast and the database connection short.
load_app_data <- function(path = db_path) {
  with_db(path = path, read_only = TRUE, function(con) {
    tables <- list(
      profile         = db_read(con, "reference", "player_profile"),
      gps_glossary    = db_read(con, "reference", "gps_glossary"),
      rec_glossary    = db_read(con, "reference", "recovery_glossary"),
      status          = db_read(con, "monitoring", "player_status"),
      load_ranges     = db_read(con, "monitoring", "load_ranges"),
      recovery_ranges = db_read(con, "monitoring", "recovery_ranges"),
      daily           = db_read(con, "features", "daily_load"),
      load_long       = db_read(con, "features", "load_long"),
      load_by_type    = db_read(con, "features", "load_by_match_day"),
      match_reference = db_read(con, "features", "match_reference")
    )
    tables$app_date <- tables$status$app_date[1]
    tables
  })
}
