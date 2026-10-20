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
    # Shown to start with: heart rate as the yellow and red bands only; the five zones
    # stay available in the metric chooser
    status_default = status$metric[!grepl("^hr_zone_", status$metric)],
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

# Longitudinal page: every load metric by day, with its rolling 7- and 28-day values, its
# normal range on days with a session, and the season's full-match median for "% of a full
# match". Days are kept up to the app date.
load_time_data <- function(data) {
  glossary <- data$gps_glossary |>
    dplyr::filter(is_metric) |>
    dplyr::arrange(display_order)
  groups <- unique(glossary$group)
  long <- data$load_long |>
    dplyr::filter(date <= data$app_date) |>
    dplyr::left_join(dplyr::select(data$load_ranges, date, metric, range_typical = typical, lower, upper, status),
                     by = c("date", "metric")) |>
    dplyr::left_join(dplyr::select(data$match_reference, season, metric, match_median), by = c("season", "metric")) |>
    dplyr::left_join(dplyr::select(data$daily, date, opposition_full), by = "date")
  season_now <- long$season[long$date == data$app_date][1]
  list(
    app_date = data$app_date,
    first_date = min(long$date),
    season_start = min(long$date[long$season == season_now]),
    glossary = glossary,
    # Grouped as in the glossary, for the metric selectors
    metric_choices = lapply(split(glossary, factor(glossary$group, levels = groups)),
                            function(g) stats::setNames(g$column_name, g$label)),
    # The six cards and the metric each starts on
    cards = c("distance", "distance_over_21", "distance_over_27", "accel_decel_over_2_5", "hr_yellow_min", "hr_red_min"),
    long = long
  )
}

# How a metric adds up over several days: amounts are summed, peak speed is the highest,
# and per-minute rates are averaged over the sessions (as in features.load_long)
metric_kind <- function(metric) {
  if (metric == "peak_speed") "max" else if (grepl("_per_min$", metric)) "rate" else "amount"
}

# What the line or bar shows, in words, for a metric and view
view_label <- function(metric, view) {
  switch(paste(metric_kind(metric), view),
    "amount 7d" = "7-day total", "amount 28d" = "Weekly average over 28 days",
    "max 7d" = "Highest in the last 7 days", "max 28d" = "Highest in the last 28 days",
    "rate 7d" = "Average per session, last 7 days", "rate 28d" = "Average per session, last 28 days",
    if (view == "range") "Position in the normal range" else "Each session")
}

# Where a value sits in its normal range: 0 is typical, -1 the bottom of the range and +1
# the top. The range is not symmetric (it is built on the log scale), so each side is
# scaled separately. Values beyond the range are beyond +/-1.
range_position <- function(value, typical, lower, upper) {
  dplyr::if_else(value >= typical, (value - typical) / (upper - typical), (value - typical) / (typical - lower))
}

# One row per calendar day in [start, end] for one metric. Days with no GPS row (the summer
# break) are kept as gaps. rolling: the value for a rolling view; position: for the range view.
load_time_days <- function(d, metric, start, end, view) {
  x <- dplyr::filter(d$long, metric == !!metric, date >= start, date <= end)
  cal <- tibble::tibble(date = seq(start, end, by = "day"))
  x <- dplyr::left_join(cal, x, by = "date") |>
    dplyr::mutate(
      in_data = !is.na(season),
      has_session = dplyr::coalesce(has_session, FALSE),
      is_match_day = dplyr::coalesce(is_match_day, FALSE),
      rolling = dplyr::case_when(view == "7d" & rolling_7d_complete ~ rolling_7d,
                                 view == "28d" & rolling_28d_complete ~ rolling_28d),
      position = dplyr::if_else(has_session & !is.na(lower), range_position(value, range_typical, lower, upper), NA_real_)
    )
  # Rolling views: the sessions outside their normal range within each day's window, so a
  # week with several unusual sessions stands out. Counted from the whole history, so a
  # window that starts before the period shown is not undercounted.
  if (view %in% c("7d", "28d")) {
    span <- if (view == "7d") 6 else 27
    flags <- dplyr::filter(d$long, metric == !!metric, has_session, is_flag(status))
    in_window <- lapply(x$date, function(day) flags[flags$date >= day - span & flags$date <= day, ])
    x$window_flags <- vapply(in_window, nrow, integer(1))
    x$window_flag_list <- vapply(in_window, function(f) {
      if (!nrow(f)) return("")
      paste(paste(as.integer(format(f$date, "%d")), format(f$date, "%b"), ifelse(f$status == "Above range", "above", "below")), collapse = ", ")
    }, character(1))
  }

  # Typical for the season: the median of complete rolling values across the whole season
  if (view %in% c("7d", "28d")) {
    col <- if (view == "7d") "rolling_7d" else "rolling_28d"
    ok <- if (view == "7d") "rolling_7d_complete" else "rolling_28d_complete"
    typical <- d$long |>
      dplyr::filter(metric == !!metric, .data[[ok]]) |>
      dplyr::group_by(season) |>
      dplyr::summarise(typical = stats::median(.data[[col]]), .groups = "drop")
    x <- dplyr::left_join(x, typical, by = "season")
  } else {
    x$typical <- NA_real_
  }
  x
}

# Totals for the period shown, in the metric's own terms
load_time_summary <- function(days, metric) {
  s <- dplyr::filter(days, has_session)
  kind <- metric_kind(metric)
  n_days <- sum(days$in_data)
  list(
    sessions = nrow(s),
    matches = sum(s$is_match_day),
    flagged = sum(is_flag(s$status)),
    assessed = sum(is_assessed(s$status)),
    kind = kind,
    total = switch(kind, amount = sum(s$value), max = if (nrow(s)) max(s$value) else NA, rate = if (nrow(s)) mean(s$value) else NA),
    weekly = if (kind == "amount" && n_days) sum(s$value) / n_days * 7 else NA
  )
}
