# One function per page, preparing exactly what that page shows.
# Plain R with no Shiny, so each can be run and checked in the console:
#   app_data <- load_app_data(); str(overview_data(app_data))

overview_data <- function(data) {
  app_date <- data$app_date
  status <- data$status

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
