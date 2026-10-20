# One function per page, preparing exactly what that page shows.
# The current status leaves out the per-minute metrics for now: they stay in the data and
# the other pages, but the status shows totals and recovery only.
# Plain R with no Shiny, so each can be run and checked in the console:
#   app_data <- load_app_data(); str(overview_data(app_data))

status_metrics <- function(status) dplyr::filter(status, !grepl("_per_min$", metric))

overview_data <- function(data) {
  app_date <- data$app_date
  status <- status_metrics(data$status)

  # Latest session: every metric, with its range or share of a full match
  latest_date <- max(data$load_ranges$date[data$load_ranges$date <= app_date])
  latest <- dplyr::filter(data$load_ranges, date == latest_date) |>
    dplyr::left_join(dplyr::select(data$gps_glossary, metric = column_name, short_label, unit, decimals),
                     by = "metric")
  session_day <- dplyr::filter(data$daily, date == latest_date)

  # Last 28 days of total distance, and the season's typical 28-day weekly average
  last28 <- dplyr::filter(data$load_long, metric == "distance", date > app_date - 28, date <= app_date)
  season_now <- last28$season[last28$date == app_date]
  season_28 <- dplyr::filter(data$load_long, metric == "distance", season == season_now,
                             rolling_28d_complete, date <= app_date)

  list(
    profile = data$profile[1, ],
    app_date = app_date,
    status = status,
    flagged = dplyr::filter(status, is_flag(status)) |> dplyr::arrange(display_order),
    n_flag = sum(is_flag(status$status)),
    n_assessed = sum(is_assessed(status$status)),
    latest_date = latest_date,
    latest = latest,
    session_day = session_day,
    last28 = last28,
    distance_week = last28$rolling_28d[last28$date == app_date],
    distance_week_typical = stats::median(season_28$rolling_28d)
  )
}

profile_data <- function(data) {
  status <- status_metrics(data$status) |>
    dplyr::left_join(dplyr::select(data$gps_glossary, metric = column_name, gps_group = group),
                     by = "metric") |>
    dplyr::mutate(group = dplyr::if_else(source == "GPS", gps_group, "Recovery")) |>
    dplyr::arrange(display_order)
  list(
    profile = data$profile[1, ],
    app_date = data$app_date,
    status = status,
    by_type = data$load_by_type |>
      dplyr::left_join(dplyr::select(data$gps_glossary, metric = column_name, label, short_label, unit, decimals),
                       by = "metric"),
    seasons = c("All seasons", sort(unique(data$daily$season), decreasing = TRUE)),
    metric_choices = stats::setNames(status$metric, status$label),
    status_choices = stats::setNames(status$metric, status$short_label),
    type_choices = data$gps_glossary |>
      dplyr::filter(is_metric, grepl("^(distance|accel_decel)", column_name)) |>
      dplyr::arrange(display_order) |>
      (\(g) stats::setNames(g$column_name, g$label))()
  )
}

glossary_data <- function(data) {
  gps <- data$gps_glossary |>
    dplyr::arrange(display_order) |>
    dplyr::transmute(
      Metric = label, Short = short_label, Group = group,
      Unit = dplyr::if_else(unit %in% c("text", "date"), "", unit),
      `What it measures` = dplyr::if_else(guide_definition == "" | is.na(guide_definition), "Derived for this app", guide_definition),
      Notes = dplyr::coalesce(notes, ""),
      Source = dplyr::if_else(guide_definition == "" | is.na(guide_definition), "Derived", "Vizathon data guide")
    )
  recovery <- data$rec_glossary |>
    dplyr::arrange(display_order) |>
    dplyr::transmute(
      Metric = label, Short = short_label, Category = category_label, Measure = measure,
      Collected = dplyr::coalesce(dplyr::na_if(collection, ""), "Not stated in the guide"),
      `What it measures` = paste(dplyr::coalesce(category_definition, ""), guide_definition),
      Notes = dplyr::coalesce(notes, "")
    )
  list(gps = gps, recovery = recovery)
}

home_data <- function(data) {
  status <- status_metrics(data$status)
  last_session <- max(data$daily$date[data$daily$has_session & data$daily$date <= data$app_date])
  day <- data$daily[data$daily$date == last_session, ]
  list(
    profile = data$profile,
    app_date = data$app_date,
    n_flag = sum(is_flag(status$status)),
    n_assessed = sum(is_assessed(status$status)),
    last_session = last_session,
    last_session_label = paste0(day$match_day, if (isTRUE(day$is_match_day)) paste0(" v ", day$opposition_full) else ""),
    seasons = paste(range(substr(data$daily$season, 1, 4)), collapse = " to ")
  )
}
