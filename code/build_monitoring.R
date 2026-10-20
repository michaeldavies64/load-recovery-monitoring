# Pipeline section: build the monitoring schema, the model outputs the app reads.
#   load_ranges  one row per session per load metric: the value, the player's typical
#                value for that kind of day, the adaptive range, a standardised score and
#                a status. Every session is included, so the app can show all of them;
#                sessions without a range say why. On match days only matches lasting
#                85 to 125 minutes get a range (see queries/daily_load.sql). Every session
#                also has pct_of_match: its value as a share of a full match that season.
#   recovery_ranges  the same for the overall recovery score and category composites,
#                one row per day a score was recorded.
#   player_status  the latest value of every metric, for the profile page (SQL, built
#                from the two range tables)

build_load_ranges <- function(con) {
  sessions <- db_read(con, "features", "load_long") |>
    dplyr::filter(has_session) |>
    dplyr::left_join(
      db_read(con, "features", "daily_load") |>
        dplyr::select(player_id, date, is_baseline_season, has_range, has_load_range),
      by = c("player_id", "date")
    )
  offsets <- db_read(con, "reference", "gps_glossary") |>
    dplyr::filter(is_metric) |>
    dplyr::select(metric = column_name, log_offset)
  events <- db_read(con, "reference", "events")
  # Every session can also be compared with a full match in the same season, whether or
  # not it has an adaptive range (for example, a short substitute appearance)
  match_ref <- db_read(con, "features", "match_reference") |>
    dplyr::select(player_id, season, metric, match_median)

  sessions |>
    dplyr::inner_join(offsets, by = "metric") |>
    dplyr::arrange(player_id, metric, match_day, date) |>
    dplyr::group_by(player_id, metric, match_day) |>
    dplyr::group_modify(function(d, key) {
      # Only sessions with a load range are modelled: on match days, that means matches
      # lasting 85 to 125 minutes. Other sessions in the group keep an empty range.
      r <- empty_range(nrow(d))
      use <- d$has_load_range
      if (any(use)) {
        offset <- d$log_offset[1]
        fit <- adaptive_range(to_model_scale(d$value[use], offset), d$is_baseline_season[use], d$date[use], events)
        r[use, ] <- dplyr::mutate(fit, dplyr::across(c(typical, lower, upper), ~ from_model_scale(.x, offset)))
      }
      dplyr::bind_cols(d, r)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      no_range_reason = dplyr::case_when(
        !has_range         ~ "No range: extended gap",
        !has_load_range    ~ "No range: match outside 85-125 min",
        is_baseline_season ~ "Baseline season",
        is.na(lower)       ~ "Building baseline",
        TRUE               ~ NA_character_
      ),
      status = range_status(value, lower, upper, no_range_reason)
    ) |>
    dplyr::left_join(match_ref, by = c("player_id", "season", "metric")) |>
    # Share of a full match: for distance, speed bands and efforts (totals and per minute)
    # only. Not for duration, peak speed or heart-rate zones, where it isn't meaningful.
    dplyr::mutate(pct_of_match = dplyr::if_else(grepl("^(distance|accel_decel)", metric),
                                                value / match_median, NA_real_)) |>
    dplyr::select(player_id, date, season, match_day, match_day_order, is_match_day, metric,
                  value, typical, lower, upper, score, status, history_weight, pct_of_match) |>
    dplyr::arrange(player_id, metric, date)
}

# Recovery ranges: the same method for the overall recovery score and the six category
# composites, with three differences:
#   - the baseline is July to December 2023, because recovery data starts in July 2023
#   - values are modelled as recorded (they are already relative to the player's norm)
#   - a unit needs at least 5 baseline values. If it has fewer, its first values after the
#     baseline period are added to the starting point until it has 5, and show
#     "Building baseline" in the meantime.
# Recovery is measured on some days without a GPS row (June and July), which have no
# match-day label and so no range.
build_recovery_ranges <- function(con) {
  scores <- db_read(con, "reference", "recovery_glossary") |>
    dplyr::filter(is_score) |>
    dplyr::select(metric)
  days <- db_read(con, "features", "daily_load") |>
    dplyr::select(player_id, date, match_day, match_day_order, is_match_day, has_range)
  events <- db_read(con, "reference", "events")
  min_n <- range_settings$recovery_min_baseline

  db_read(con, "staging", "recovery") |>
    dplyr::semi_join(scores, by = "metric") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::left_join(days, by = c("player_id", "date")) |>
    dplyr::mutate(has_range = dplyr::coalesce(has_range, FALSE)) |>
    dplyr::arrange(player_id, metric, match_day, date) |>
    dplyr::group_by(player_id, metric, match_day) |>
    dplyr::group_modify(function(d, key) {
      # Starting point: the baseline period, topped up to the minimum if it falls short
      d$is_baseline <- d$date <= range_settings$recovery_baseline_end | seq_len(nrow(d)) <= min_n
      d$short_baseline <- sum(d$date <= range_settings$recovery_baseline_end) < min_n
      r <- if (d$has_range[1] && sum(d$is_baseline) >= min_n) {
        adaptive_range(d$value, d$is_baseline, d$date, events)
      } else {
        empty_range(nrow(d))
      }
      dplyr::bind_cols(d, r)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      no_range_reason = dplyr::case_when(
        is.na(match_day)                                       ~ "No range: off-season",
        !has_range                                             ~ "No range: extended gap",
        date <= range_settings$recovery_baseline_end           ~ "Baseline period",
        is_baseline | is.na(lower)                             ~ "Building baseline",
        TRUE                                                   ~ NA_character_
      ),
      status = range_status(value, lower, upper, no_range_reason)
    ) |>
    dplyr::select(player_id, date, season, match_day, match_day_order, is_match_day, category, metric,
                  value, typical, lower, upper, score, status, history_weight) |>
    dplyr::arrange(player_id, metric, date)
}

build_monitoring <- function(con) {
  load_ranges <- build_load_ranges(con)
  recovery_ranges <- build_recovery_ranges(con)
  DBI::dbWithTransaction(con, {
    db_write_table(con, "monitoring", "load_ranges", load_ranges)
    db_write_table(con, "monitoring", "recovery_ranges", recovery_ranges)
    db_build_table(con, "monitoring", "player_status", "player_status.sql")
  })
  db_table_counts(con, "monitoring")
}
