# Daily bar chart: one bar per calendar day, match days in teal, days without a session
# as a dotted baseline. Each bar has a tooltip; days with a session can be clicked.
# days: data frame with date, match_day, value, has_session, is_match_day
# value_label: function turning a value into tooltip text
# onclick: function(date) returning JavaScript to run when that day is clicked

daily_bars <- function(days, value_label, onclick = NULL) {
  top <- max(days$value) * 1.05
  bars <- lapply(seq_len(nrow(days)), function(i) {
    d <- days[i, ]
    cls <- if (d$is_match_day) "md-bar md-bar-match" else if (!d$has_session) "md-bar md-bar-off" else "md-bar"
    clickable <- d$has_session && !is.null(onclick)
    tip <- paste0(format(d$date, "%a "), fmt_date_short(d$date), "\n", d$match_day,
                  if (d$has_session) paste0("\n", value_label(d$value), if (clickable) "\nClick to check this session")
                  else "\nNo session")
    htmltools::div(class = paste(cls, if (clickable) "md-clickable"),
                   style = paste0("height:", max(1.5, round(100 * d$value / top, 1)), "%"),
                   `data-tip` = tip,
                   onclick = if (clickable) onclick(d$date))
  })
  htmltools::tagList(
    htmltools::div(class = "md-bars", bars),
    htmltools::div(class = "md-bars-axis md-muted small",
      htmltools::span(fmt_date_short(min(days$date))),
      htmltools::span(htmltools::span(class = "md-key md-key-match"), "match day"),
      htmltools::span(fmt_date_short(max(days$date))))
  )
}
