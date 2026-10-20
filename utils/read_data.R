# Readers for the four Vizathon files.
# Each one does only what is needed to read its file correctly, and nothing analytical:
#   - fixes the file's quirks (encoding, byte-order mark, day/month/year dates)
#   - gives every column an explicit type, so a change in the source file fails loudly
#   - uses snake_case column names and adds a player_id
# The cleaned tables are then loaded into DuckDB unchanged; all analysis happens in SQL.
# Known quirks are documented in docs/data_quality.qmd.
# All paths are built with here::here(), so they resolve from the project root.

# The data describes one player. Every table carries an ID so a squad can be added later.
default_player_id <- "P01"

read_gps <- function(dir = here::here("input"), player_id = default_player_id) {
  read_vizathon(
    dir, "gps",
    locale = readr::locale(encoding = "latin1"),     # opposition names such as "Kobenhavn" (with o-slash)
    col_types = readr::cols(
      date = readr::col_date("%d/%m/%Y"),
      opposition_code = readr::col_character(),
      opposition_full = readr::col_character(),
      md_plus_code = readr::col_integer(),
      md_minus_code = readr::col_integer(),
      season = readr::col_character(),
      hr_zone_1_hms = readr::col_time(),
      hr_zone_2_hms = readr::col_time(),
      hr_zone_3_hms = readr::col_time(),
      hr_zone_4_hms = readr::col_time(),
      hr_zone_5_hms = readr::col_time(),
      .default = readr::col_double()
    )
  ) |>
    # Heart-rate zones arrive as hh:mm:ss; store them as minutes
    dplyr::mutate(dplyr::across(dplyr::starts_with("hr_zone_"), ~ as.numeric(.x) / 60)) |>
    dplyr::rename_with(~ sub("_hms$", "_min", .x)) |>
    add_player_id(player_id) |>
    dplyr::arrange(date)
}

read_capability <- function(dir = here::here("input"), player_id = default_player_id) {
  read_vizathon(
    dir, "capability",
    locale = readr::locale(encoding = "UTF-8"),      # strips the byte-order mark on testDate
    col_types = readr::cols(
      testDate = readr::col_date("%d/%m/%Y"),
      benchmarkPct = readr::col_double(),
      .default = readr::col_character()
    )
  ) |>
    dplyr::rename(test_date = testDate, benchmark_pct = benchmarkPct) |>
    add_player_id(player_id) |>
    dplyr::arrange(movement, quality, expression, test_date)
}

read_recovery <- function(dir = here::here("input"), player_id = default_player_id) {
  read_vizathon(
    dir, "recovery",
    col_types = readr::cols(
      sessionDate = readr::col_date("%d/%m/%Y"),
      value = readr::col_double(),
      .default = readr::col_character()
    )
  ) |>
    dplyr::rename(date = sessionDate, season = seasonName) |>
    add_player_id(player_id) |>
    dplyr::arrange(date, category, metric)
}

read_priorities <- function(dir = here::here("input"), player_id = default_player_id) {
  read_vizathon(
    dir, "priorities",
    col_types = readr::cols(
      Priority = readr::col_integer(),
      `Target set` = readr::col_date("%d/%m/%Y"),
      `Review Date` = readr::col_date("%d/%m/%Y"),
      .default = readr::col_character()
    )
  ) |>
    snake_names() |>
    add_player_id(player_id)
}

# Helpers ---------------------------------------------------------------------------

# Reads one file and stops if any value failed to parse, rather than silently returning NA
read_vizathon <- function(dir, key, ...) {
  path <- file.path(dir, vizathon_files[[key]])
  if (!file.exists(path)) stop("Missing ", path, ". Run scr/01_download_data.R first.", call. = FALSE)
  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE, ...)
  issues <- readr::problems(df)
  if (nrow(issues)) {
    stop(vizathon_files[[key]], ": ", nrow(issues), if (nrow(issues) == 1) " value" else " values", " failed to parse, first in row ",
         issues$row[1], ", column ", issues$col[1], " (expected ", issues$expected[1],
         ", got '", issues$actual[1], "')", call. = FALSE)
  }
  df
}

add_player_id <- function(df, player_id) {
  dplyr::mutate(df, player_id = player_id, .before = 1)
}

# Minimal snake_case for column names, to avoid a janitor dependency
snake_names <- function(df) {
  names(df) <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(names(df))))
  df
}

# Reference tables ------------------------------------------------------------------
# Hand-written glossaries in input/reference/, committed to the repository.
# They hold display labels, units and the definitions from the Vizathon data guide,
# for the app's selectors, axes and tooltips.

read_gps_glossary <- function(dir = here::here("input", "reference")) {
  read_glossary(file.path(dir, "gps_glossary.csv"), is_metric = readr::col_logical(),
                log_offset = readr::col_double())
}

read_recovery_glossary <- function(dir = here::here("input", "reference")) {
  read_glossary(file.path(dir, "recovery_glossary.csv"), is_score = readr::col_logical())
}

# Known changes that reset how much past sessions count in the adaptive ranges:
# season starts and head-coach changes, each with the weight (in sessions) to cut to
read_events <- function(dir = here::here("input", "reference")) {
  readr::read_csv(
    file.path(dir, "events.csv"),
    col_types = readr::cols(
      date = readr::col_date(),
      reset_weight = readr::col_double(),
      .default = readr::col_character()
    ),
    show_col_types = FALSE, progress = FALSE
  )
}

# Invented profile for the simulated player, for the app's profile page
read_player_profile <- function(dir = here::here("input", "reference")) {
  readr::read_csv(
    file.path(dir, "player_profile.csv"),
    col_types = readr::cols(
      date_of_birth = readr::col_date(),
      height_cm = readr::col_double(),
      weight_kg = readr::col_double(),
      body_fat_pct = readr::col_double(),
      .default = readr::col_character()
    ),
    show_col_types = FALSE, progress = FALSE
  )
}

read_glossary <- function(path, ...) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      display_order = readr::col_integer(),
      decimals = readr::col_integer(),
      ...,
      .default = readr::col_character()
    ),
    show_col_types = FALSE, progress = FALSE
  )
}
