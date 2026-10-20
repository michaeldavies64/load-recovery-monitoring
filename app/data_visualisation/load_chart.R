# Longitudinal chart: one column per calendar day.
#   daily  a bar per session (teal on match days, amber outside the normal range) over a
#          light band showing that day's normal range
#   range  each session's position in its own normal range, as a bar from typical (0):
#          the shaded band is the normal range (-1 to +1), so sessions on different kinds
#          of day can be compared on one scale
#   7d/28d a line, a dashed step line for the season's typical value, match days marked
#          along the bottom, and a dot on the line for each session outside its normal
#          range (filled above, hollow below), so weeks with several stand out
# Summer breaks with no GPS data are shaded. Drawn as SVG; a transparent column per day
# carries the tooltip and the click, so it matches the app's other charts.
# days: from load_time_days(); tip: function(row) returning tooltip text;
# onclick: function(date) returning JavaScript; height: chart height in pixels

load_chart <- function(days, view, tip, onclick = NULL, height = 300) {
  n <- nrow(days)
  num <- function(x) format(x, scientific = FALSE, trim = TRUE)
  w <- if (n > 120) 0.8 else 0.64
  svg <- character(0)

  # Vertical scale. Bars start at 0. Lines don't need to, so the rolling views fit the
  # axis to the data (otherwise peak speed, around 30 km/h, would be a flat line). The
  # range view is centred on typical and shows at least the whole range.
  if (view == "range") {
    p <- days$position[!is.na(days$position)]
    lim <- max(2, ceiling(max(abs(c(p, 0)))))
    lim <- min(lim, 4)                       # very unusual sessions are drawn at the edge
    ticks <- seq(-lim, lim, by = if (lim > 2) 2 else 1)
    ticks <- sort(unique(c(ticks, -1, 0, 1)))
  } else {
    shown <- if (view == "daily") days$value[days$has_session] else days$rolling
    top_val <- max(c(shown, if (view == "daily") days$upper else days$typical, 0), na.rm = TRUE)
    low_val <- if (view == "daily") 0 else min(c(shown, days$typical), na.rm = TRUE)
    ticks <- pretty(c(low_val, top_val), n = 4)
  }
  lo <- min(ticks)
  top <- max(ticks)
  if (top <= lo) top <- lo + 1
  y <- function(v) round(100 - 100 * (pmin(pmax(v, lo), top) - lo) / (top - lo), 3)

  bar_class <- function(i) ifelse(is_flag(days$status[i]), "md-lt-bar md-lt-flag",
                                  ifelse(days$is_match_day[i], "md-lt-bar md-lt-match", "md-lt-bar"))

  if (view == "daily") {
    band <- which(!is.na(days$lower))
    bw <- min(1, w + 0.2)   # a little wider than the bar, so neighbouring days stay apart
    svg <- c(svg, sprintf('<rect class="md-lt-band" x="%s" y="%s" width="%s" height="%s"/>',
                          num(band - 1 + (1 - bw) / 2), y(days$upper[band]), bw, num(y(days$lower[band]) - y(days$upper[band]))))
    s <- which(days$has_session)
    svg <- c(svg, sprintf('<rect class="%s" x="%s" y="%s" width="%s" height="%s"/>',
                          bar_class(s), num(s - 1 + (1 - w) / 2), y(days$value[s]), w, num(100 - y(days$value[s]))))
  } else if (view == "range") {
    svg <- c(svg, sprintf('<rect class="md-lt-band" x="0" y="%s" width="%s" height="%s"/>', y(1), n, num(y(-1) - y(1))))
    svg <- c(svg, sprintf('<path class="md-lt-typical" d="M0 %s H%s"/>', y(0), n))
    s <- which(!is.na(days$position))
    top_y <- pmin(y(0), y(days$position[s]))
    svg <- c(svg, sprintf('<rect class="%s" x="%s" y="%s" width="%s" height="%s"/>',
                          bar_class(s), num(s - 1 + (1 - w) / 2), top_y, w, num(pmax(abs(y(days$position[s]) - y(0)), 0.6))))
  } else {
    # Typical for the season: one dashed segment per season
    season <- dplyr::coalesce(days$season, "")   # summer-gap days have no season
    for (seg in split(seq_len(n), cumsum(c(TRUE, diff(is.na(days$typical)) != 0 | season[-1] != season[-n])))) {
      if (all(is.na(days$typical[seg]))) next
      svg <- c(svg, sprintf('<path class="md-lt-typical" d="M%s %s H%s"/>', seg[1] - 1, y(days$typical[seg[1]]), max(seg)))
    }
    # The rolling line, broken wherever a value is missing (window not yet full, summer break)
    ok <- which(!is.na(days$rolling))
    for (r in split(ok, cumsum(c(TRUE, diff(ok) != 1)))) {
      if (!length(r)) next
      svg <- c(svg, sprintf('<path class="md-lt-line" d="M%s"/>', paste0(r - 0.5, " ", y(days$rolling[r]), collapse = " L")))
    }
    m <- which(days$is_match_day)
    svg <- c(svg, sprintf('<rect class="md-lt-match" x="%s" y="96" width="%s" height="4"/>',
                          num(m - 1 + 0.1), if (n > 120) 1 else 0.8))
  }

  # Sessions outside their range, on the rolling line. HTML dots rather than SVG circles,
  # which would stretch with the chart.
  dots <- NULL
  if (view %in% c("7d", "28d")) {
    f <- which(days$has_session & is_flag(days$status) & !is.na(days$rolling))
    dots <- lapply(f, function(i) htmltools::div(
      class = paste("md-lt-dot", if (days$status[i] == "Below range") "md-lt-dot-below"),
      style = sprintf("left:%s%%; top:%s%%", round(100 * (i - 0.5) / n, 3), y(days$rolling[i]))))
  }

  gaps <- which(!days$in_data)
  for (g in split(gaps, cumsum(c(TRUE, diff(gaps) != 1)))) {
    if (length(g)) svg <- c(svg, sprintf('<rect class="md-lt-gap" x="%s" y="0" width="%s" height="100"/>', g[1] - 1, length(g)))
  }

  tick_label <- function(t) {
    if (view != "range") return(format(t, big.mark = ",", scientific = FALSE))
    switch(as.character(t), "-1" = "Low", "0" = "Typical", "1" = "High", sprintf("%+d", t))
  }

  hits <- lapply(seq_len(n), function(i) {
    r <- days[i, ]
    clickable <- r$has_session && !is.null(onclick)
    edge <- if (i > n * 0.6) "md-edge-r" else NULL   # tooltips open to the left near the right edge
    htmltools::div(class = paste("md-lt-hit", edge, if (clickable) "md-clickable"),
                   `data-tip` = tip(r), onclick = if (clickable) onclick(r$date))
  })

  htmltools::div(
    class = "md-lt-chart", style = sprintf("grid-template-rows: %spx auto;", height),
    htmltools::div(class = "md-lt-yaxis",
      lapply(ticks, function(t) htmltools::span(style = sprintf("top:%s%%", y(t)), tick_label(t)))),
    htmltools::div(
      class = "md-lt-plot",
      lapply(ticks, function(t) htmltools::div(class = "md-lt-grid", style = sprintf("top:%s%%", y(t)))),
      htmltools::HTML(sprintf('<svg class="md-lt-svg" viewBox="0 0 %s 100" preserveAspectRatio="none" aria-hidden="true">%s</svg>',
                              n, paste(svg, collapse = ""))),
      dots,
      htmltools::div(class = "md-lt-hits", hits)
    ),
    htmltools::div(),
    load_chart_xaxis(days$date)
  )
}

# Date labels along the bottom. Up to five weeks: every day, with the month on the first
# label and on the 1st (on a phone only Mondays and month starts show, via styles.css).
# Up to four months: Mondays. Longer: month starts, every second month beyond 14 months.
load_chart_xaxis <- function(dates) {
  n <- length(dates)
  day <- as.integer(format(dates, "%d"))
  monday <- format(dates, "%u") == "1"
  if (n <= 35) {
    at <- seq_len(n)
    lab <- ifelse(at == 1 | day == 1, paste(day, format(dates, "%b")), day)
    minor <- !(monday | day == 1 | at == 1)
  } else if (n <= 120) {
    at <- which(monday)
    lab <- paste(day[at], format(dates[at], "%b"))
    minor <- rep(FALSE, length(at))
  } else {
    first <- which(day == 1)
    step <- if (n <= 430) 1 else 2
    at <- first[as.integer(format(dates[first], "%m")) %% step == 0 | step == 1]
    lab <- ifelse(format(dates[at], "%m") == "01" | n > 430, format(dates[at], "%b %Y"), format(dates[at], "%b"))
    minor <- rep(FALSE, length(at))
  }
  if (n <= 35) lab <- lab[at]
  if (n <= 35) minor <- minor[at]
  htmltools::div(class = "md-lt-xaxis md-muted small",
    lapply(seq_along(at), function(k) htmltools::span(
      class = if (minor[k]) "md-lt-x-minor",
      style = sprintf("left:%s%%", round(100 * (at[k] - 0.5) / n, 2)), lab[k])))
}

# One legend for all the cards, with only the keys for the view shown.
# gap: whether the period includes a summer break with no GPS data
load_chart_legend <- function(view, gap = TRUE) {
  key <- function(cls, text) htmltools::span(class = "md-lt-key", htmltools::span(class = paste("md-key", cls)), text)
  htmltools::div(class = "md-lt-legend md-muted small",
    switch(view,
      daily = htmltools::tagList(key("md-lt-key-bar", "Training session"), key("md-key-match", "Match day"),
                                 key("md-lt-key-flag", "Outside normal range"), key("md-lt-key-band", "Normal range for that day")),
      range = htmltools::tagList(key("md-lt-key-bar", "Training session"), key("md-key-match", "Match day"),
                                 key("md-lt-key-flag", "Outside normal range"), key("md-lt-key-band", "Normal range (Low to High)"),
                                 key("md-lt-key-typical", "Typical for that kind of day")),
      htmltools::tagList(key("md-lt-key-line", "Rolling value"), key("md-lt-key-typical", "Typical for the season"),
                         key("md-lt-key-dot", "Session above its normal range"),
                         key("md-lt-key-dot md-lt-dot-below", "Session below its normal range"),
                         key("md-key-match", "Match day"))),
    if (gap) key("md-lt-key-gap", "No GPS data (summer break)"))
}
