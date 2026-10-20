# Recent sessions strip: one row per metric, with the recent sessions of the same type as
# grey dots along a line and this session as a larger dot (amber when outside its range).
# A tick marks the typical value for this kind of day. Built in HTML, with tooltips.
# rows: the recent sessions for one metric (date, value, status, typical); this_date: the session

session_strip <- function(label, rows, this_date, decimals, unit, rank_text, onclick = NULL) {
  rows <- rows[order(rows$date), ]
  lo <- min(rows$value, rows$lower, na.rm = TRUE)
  hi <- max(rows$value, rows$upper, na.rm = TRUE)
  if (hi <= lo) hi <- lo + 1
  pad <- (hi - lo) * 0.06
  x <- function(v) round(100 * (v - lo + pad) / (hi - lo + 2 * pad), 2)
  this <- rows[rows$date == this_date, ]
  typical <- this$typical
  dots <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    is_this <- r$date == this_date
    cls <- paste("md-ss-dot", if (is_this) "md-ss-this", if (is_this && is_flag(r$status)) "md-ss-flag")
    htmltools::div(class = paste(cls, if (!is_this && !is.null(onclick)) "md-clickable"),
                   style = sprintf("left:%s%%", x(r$value)),
                   `data-tip` = paste0(format(r$date, "%a "), fmt_date_short(r$date), "\n", fmt_value(r$value, decimals, unit),
                                       if (is_this) "\nThis session" else if (!is.null(onclick)) "\nClick to open this session" else ""),
                   onclick = if (!is_this && !is.null(onclick)) onclick(r$date))
  })
  htmltools::div(
    class = "md-ss-row",
    htmltools::div(class = "md-ss-label",
      htmltools::span(class = "md-ss-name", label),
      htmltools::span(class = "md-ss-value", if (nrow(this)) fmt_value(this$value, decimals, unit) else "\u2013")),
    htmltools::div(class = "md-ss-track",
      if (length(typical) && !is.na(typical)) htmltools::div(class = "md-ss-typical", style = sprintf("left:%s%%", x(typical)),
                                                             `data-tip` = paste("Typical:", fmt_value(typical, decimals, unit))),
      dots),
    htmltools::div(class = "md-ss-rank md-muted small", rank_text)
  )
}

# Recovery around a session: one row per score, one column per day (the session day and
# the next days with recovery data), each cell showing the score coloured by its status
recovery_grid <- function(recovery, days, labels, metrics) {
  head <- htmltools::div(class = "md-rg-row md-rg-head",
    htmltools::span(),
    lapply(seq_along(days), function(k) {
      gap <- as.numeric(days[k] - days[1])
      htmltools::span(
        if (k == 1) "Day of the session" else paste(gap, if (gap == 1) "day later" else "days later"), htmltools::br(),
        htmltools::span(class = "md-muted", paste0(fmt_date_short(days[k]), if (!is.na(labels[k])) paste0(" \u00b7 ", labels[k]) else "")))
    }))
  rows <- lapply(seq_len(nrow(metrics)), function(i) {
    m <- metrics[i, ]
    htmltools::div(class = "md-rg-row",
      htmltools::span(class = "md-rg-name", m$label),
      lapply(days, function(dd) {
        r <- recovery[recovery$metric == m$metric & recovery$date == dd, ]
        if (!nrow(r)) return(htmltools::span(class = "md-rg-cell md-rg-none", "Not collected"))
        cls <- if (is_flag(r$status)) "md-rg-flag" else if (is_assessed(r$status)) "md-rg-ok" else "md-rg-plain"
        tip <- paste0(r$status, if (!is.na(r$lower)) paste0("\nNormal ", round(100 * r$lower), "% to ", round(100 * r$upper), "%, typical ", round(100 * r$typical), "%") else "")
        htmltools::span(class = paste("md-rg-cell", cls), `data-tip` = tip,
                        sprintf("%+.0f%%", round(100 * r$value) + 0),
                        if (is_flag(r$status)) htmltools::span(class = "md-rg-dir", if (r$status == "Above range") "above" else "below"))
      }))
  })
  htmltools::div(class = "md-rg", head, rows)
}
