# Data quality checks for the Vizathon staging tables.
# Each check_*_quality() returns one row per check: what was tested, how many rows it affects, a status and a note.
# Status is one of "Pass", "Note" (worth knowing, no action needed) or "Issue" (needs handling in the pipeline).

check <- function(area, check, n, total, status, note) {
  tibble::tibble(area = area, check = check, n = n, total = total, status = status, note = note)
}

pass_if_zero <- function(n, fail = "Issue") if (n == 0) "Pass" else fail

# "7 March 2025": portable on Windows, macOS and Linux (unlike %-d)
fmt_day <- function(d, month = "%B") paste(as.integer(format(d, "%d")), format(d, paste(month, "%Y")))

check_gps_quality <- function(gps) {
  sessions <- dplyr::filter(gps, distance > 0)
  hr_min <- rowSums(dplyr::select(sessions, dplyr::starts_with("hr_zone_")))
  n_zero <- sum(gps$distance == 0)
  n_zero_match <- sum(gps$distance == 0 & gps$md_plus_code == 0)
  n_hr <- sum(hr_min > sessions$day_duration * 1.1)
  n_frac <- sum(sessions$accel_decel_over_2_5 %% 1 != 0)
  # Thresholds should be nested: distance over 21 km/h includes the distance over 24 and 27
  n_speed <- sum(sessions$distance_over_21 < sessions$distance_over_24 | sessions$distance_over_24 < sessions$distance_over_27)
  n_effort <- sum(sessions$accel_decel_over_2_5 < sessions$accel_decel_over_3_5 | sessions$accel_decel_over_3_5 < sessions$accel_decel_over_4_5)
  gaps <- as.numeric(diff(gps$date))

  dplyr::bind_rows(
    check("GPS", "One row per date", sum(duplicated(gps$date)), nrow(gps), pass_if_zero(sum(duplicated(gps$date))), "Each date appears once, so date is a safe join key."),
    check("GPS", "Dates parse as day/month/year", sum(is.na(gps$date)), nrow(gps), pass_if_zero(sum(is.na(gps$date))), "All dates valid."),
    check("GPS", "Days with no session recorded", n_zero, nrow(gps), "Note", "Every calendar day has a row; days off are rows of zeros, not missing rows. Exclude them from averages."),
    check("GPS", "Match days with no data", n_zero_match, sum(gps$md_plus_code == 0), pass_if_zero(n_zero_match, "Note"), "All three fall in a 29-day block of zeros from 14 March 2025, which suggests a period of unavailability rather than errors."),
    check("GPS", "Effort counts are whole numbers", n_frac, nrow(sessions), pass_if_zero(n_frac, "Note"), "Counts are fractional, a sign of simulation. Round for display only."),
    check("GPS", "Speed distances are nested (>21 includes >24 and >27 km/h)", n_speed, nrow(sessions), pass_if_zero(n_speed, "Note"), "Each threshold should include the faster ones, but in these sessions a faster band is larger than a slower one, most likely a sign of simulation. Use each band as supplied, and don't work out distance between thresholds (such as 21-24 km/h) by subtraction."),
    check("GPS", "Effort counts are nested (>2.5 includes >3.5 and >4.5 m/s\u00b2)", n_effort, nrow(sessions), pass_if_zero(n_effort, "Note"), "The >2.5 count already includes the harder efforts, so it is the total; adding the three bands would count the hardest efforts more than once."),
    check("GPS", "Heart rate time fits within session duration", n_hr, nrow(sessions), pass_if_zero(n_hr, "Note"), "Time in HR zones exceeds session duration by more than 10% in a few sessions, most likely because the GPS session was clipped to the training period while heart rate kept recording. Use duration from GPS and treat HR zone time in these sessions with caution."),
    check("GPS", "Continuous daily coverage in season", sum(gaps > 1), length(gaps), "Note", "Daily from 1 August to 31 May each season; June and July are absent, so there is no GPS pre-season data. Pre-season appears only in the capability and recovery files.")
  )
}

# Marks capability rows that are genuine new results rather than values carried forward:
# a row is new when it has a score that differs from the previous day's score for that test
flag_capability_tests <- function(cap) {
  cap |>
    dplyr::group_by(player_id, movement, quality, expression) |>
    dplyr::arrange(test_date, .by_group = TRUE) |>
    dplyr::mutate(
      is_new_result = !is.na(benchmark_pct) &
        (is.na(dplyr::lag(benchmark_pct)) | benchmark_pct != dplyr::lag(benchmark_pct))
    ) |>
    dplyr::ungroup()
}

# cap: capability rows with is_new_result, from flag_capability_tests()
check_capability_quality <- function(cap) {
  n_dup <- sum(duplicated(dplyr::select(cap, test_date, movement, quality, expression)))
  n_carried <- sum(!is.na(cap$benchmark_pct) & !cap$is_new_result)
  n_na <- sum(is.na(cap$benchmark_pct))
  n_out <- sum(cap$benchmark_pct > 1 | cap$benchmark_pct < 0, na.rm = TRUE)

  dplyr::bind_rows(
    check("Capability", "One row per date and test combination", n_dup, nrow(cap), pass_if_zero(n_dup), "20 movement, quality and expression combinations every day."),
    check("Capability", "Rows that are carried forward, not new results", n_carried, nrow(cap), "Issue", "Values repeat until a new test, as the guide says. Use only new results for trends and test counts, or trends will look smoother and more frequent than they are."),
    check("Capability", "Missing benchmark scores", n_na, nrow(cap), "Note", "Missing where there is not yet enough data for a benchmark, mostly isometric tests early on."),
    check("Capability", "Scores outside 0\u2013100%", n_out, sum(!is.na(cap$benchmark_pct)), "Note", "Benchmark scores can exceed 100% or fall below 0, so they are relative scores rather than percentiles. Do not cap them.")
  )
}

check_recovery_quality <- function(rec) {
  n_dup <- sum(duplicated(dplyr::select(rec, date, metric)))
  wide <- rec |>
    dplyr::filter(category != "total") |>
    dplyr::mutate(
      domain = sub("_baseline_(completeness|composite)$", "", metric),
      kind = sub(".*_baseline_", "", metric)
    ) |>
    dplyr::select(date, domain, kind, value) |>
    tidyr::pivot_wider(names_from = kind, values_from = value)
  n_incons <- sum((wide$completeness == 0) != is.na(wide$composite), na.rm = TRUE)
  total <- dplyr::filter(rec, metric == "emboss_baseline_score")
  mean_comp <- wide |> dplyr::group_by(date) |> dplyr::summarise(mean_comp = mean(composite, na.rm = TRUE), .groups = "drop")
  cmp <- dplyr::inner_join(total, mean_comp, by = "date") |> dplyr::filter(!is.na(value), is.finite(mean_comp))
  n_differs <- sum(abs(cmp$value - cmp$mean_comp) > 0.01)

  dplyr::bind_rows(
    check("Recovery", "One row per date and metric", n_dup, nrow(rec), pass_if_zero(n_dup), "13 metrics every day: 6 categories with 2 metrics each, plus the overall score."),
    check("Recovery", "Completeness of 0 always means no composite score", n_incons, nrow(wide), pass_if_zero(n_incons), "Consistent: a composite exists only on days its tests were completed."),
    check("Recovery", "Days with an overall recovery score", sum(!is.na(total$value)), nrow(total), "Note", "The overall score exists on 60% of days. Missing means not collected, not zero."),
    check("Recovery", "Overall score equals the mean of the category composites", n_differs, nrow(cmp), "Note", "The overall score tracks the category mean but is not equal to it, so its weighting is unknown. Use it as supplied rather than recomputing it.")
  )
}

# Cross-file coverage: the window in which all three daily sources have data
check_coverage_quality <- function(gps, cap, rec) {
  overlap <- c(max(min(gps$date), min(cap$test_date), min(rec$date)),
               min(max(gps$date), max(cap$test_date), max(rec$date)))
  n_days <- as.numeric(diff(overlap)) + 1
  gps_span <- range(gps$date)
  check("All files", "Daily sources cover the same period", n_days, as.numeric(diff(gps_span)) + 1, "Note",
        sprintf("GPS covers three seasons (%s to %s); capability and recovery cover two. All three overlap for %d days, from %s to %s, so analysis that combines sources should use that window.",
                fmt_day(gps_span[1]), fmt_day(gps_span[2]), n_days, fmt_day(overlap[1]), fmt_day(overlap[2])))
}
