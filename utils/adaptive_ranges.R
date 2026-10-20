# Adaptive ranges: is a value normal for this player, on this kind of day?
# Method after Roshan et al. (2021), adapted for one player. Each unit (one metric on one
# match-day label) gets a Bayesian predictive range from its own history:
#   - a starting point (mean, spread and weight) from a baseline period
#   - a 90% Student-t predictive range for each new value, from past values only
#   - a normal-inverse-gamma update after each value
#   - at each known change (season start, head-coach change) the history's weight is
#     cut back, so the range widens and the new period's values soon take over
# The reasoning for every setting is in docs/adaptive_ranges.qmd.

range_settings <- list(
  reset_weight = 10,                              # what past values count for after a known change
  interval = 0.90,                                # width of the normal range
  recovery_baseline_end = as.Date("2023-12-31"),  # recovery data starts in July 2023
  recovery_min_baseline = 5                       # values needed before a recovery range is given
)

# y: one unit's values in date order, already on the modelling scale
# baseline: TRUE for values in the baseline period (they set the starting point, no range)
# date: date of each value; events: data frame with date and reset_weight
adaptive_range <- function(y, baseline, date, events,
                           start_weight = range_settings$reset_weight,
                           level = range_settings$interval) {
  n <- length(y)
  out <- empty_range(n)
  b <- y[baseline]
  n_b <- length(b)
  if (n_b < 2 || all(baseline)) return(out)

  # Starting point from the baseline: typical value, spread, and how much each counts
  mu <- mean(b); s2 <- var(b)
  kappa <- min(n_b, start_weight); nu <- min(n_b - 1, start_weight)
  last_date <- max(date[baseline])

  for (i in which(!baseline)) {
    # Known changes since this unit's previous value: cut the history's weight
    hit <- events$reset_weight[events$date > last_date & events$date <= date[i]]
    if (length(hit)) { kappa <- min(kappa, min(hit)); nu <- min(nu, min(hit)) }
    last_date <- date[i]

    # Predictive range for this value, from past values only
    spread <- sqrt(s2 * (1 + 1 / kappa))
    half <- stats::qt(1 - (1 - level) / 2, df = nu) * spread
    out[i, ] <- c(mu, mu - half, mu + half, (y[i] - mu) / spread, kappa)

    # Normal-inverse-gamma update with this value
    kappa_new <- kappa + 1; nu_new <- nu + 1
    s2 <- (nu * s2 + kappa / kappa_new * (y[i] - mu)^2) / nu_new
    mu <- (kappa * mu + y[i]) / kappa_new
    kappa <- kappa_new; nu <- nu_new
  }
  out
}

# A range table with no values, for units that get no range
empty_range <- function(n) {
  data.frame(typical = rep(NA_real_, n), lower = rep(NA_real_, n), upper = rep(NA_real_, n),
             score = rep(NA_real_, n), history_weight = rep(NA_real_, n))
}

# Converts between real units and the modelling scale: log(value + offset), or as recorded
# when offset is NA. Offsets come from the glossary (log_offset).
to_model_scale   <- function(x, offset) if (is.na(offset)) x else log(x + offset)
from_model_scale <- function(y, offset) if (is.na(offset)) y else exp(y) - offset

# Labels each value against its range
range_status <- function(value, lower, upper, no_range_reason) {
  dplyr::case_when(
    !is.na(no_range_reason) ~ no_range_reason,
    value > upper           ~ "Above range",
    value < lower           ~ "Below range",
    TRUE                    ~ "In range"
  )
}
