# Range bar: the player's normal range as a band, the typical value as a tick and the
# value as a dot, amber when outside the range. Built in HTML so it stays crisp at any size.
# When a value has no adaptive range but can be compared with a full match, the bar shows
# its share of a full match instead, with a tick at 100%.
# onclick: optional JavaScript (see js_set_input()) to run when the bar is clicked.

range_bar <- function(label, value, lower, upper, typical, status, decimals = 0, unit = "", note = NULL,
                      pct_of_match = NA, onclick = NULL) {
  wrap <- function(tip, ...) htmltools::div(class = paste("md-rb", if (!is.null(onclick)) "md-clickable"),
                                            `data-tip` = tip, onclick = onclick, ...)
  if (identical(unit, "proportion")) {
    # Recovery composites are relative to the player's norm: show them as percentages
    value <- 100 * value; lower <- 100 * lower; upper <- 100 * upper; typical <- 100 * typical
    decimals <- 0; unit <- "%"
  }
  fmt <- function(x) if (unit == "%") paste0(formatC(x, format = "f", digits = 0), "%") else fmt_value(x, decimals)
  value_text <- if (unit == "%") fmt(value) else fmt_value(value, decimals, unit)

  if (is.na(lower)) {
    head <- htmltools::div(class = "md-rb-head", htmltools::span(label), htmltools::span(class = "md-rb-val", value_text))
    if (is.na(pct_of_match)) {
      return(wrap(paste(label, value_text, status, sep = "\n"),
                  head, htmltools::div(class = "md-rb-foot md-muted", sub("No range: ", "No range, ", status))))
    }
    width <- paste0(round(100 * min(pct_of_match, 1.5) / 1.5, 1), "%")
    return(wrap(
      paste0(label, ": ", value_text, "\n", round(100 * pct_of_match),
             "% of a typical full match this season\nNo adaptive range: ", tolower(sub("No range: ", "", status))),
      head,
      htmltools::div(class = "md-rb-track",
        htmltools::div(class = "md-pm-fill", style = paste0("width:", width)),
        htmltools::div(class = "md-rb-typical", style = paste0("left:", round(100 / 1.5, 1), "%"))),
      htmltools::div(class = "md-rb-foot md-muted", paste0(round(100 * pct_of_match), "% of a full match"))
    ))
  }

  lo <- min(lower, value); hi <- max(upper, value); pad <- (hi - lo) * 0.08
  lo <- lo - pad; hi <- hi + pad
  pos <- function(x) paste0(round(100 * (x - lo) / (hi - lo), 1), "%")
  flag <- is_flag(status)
  wrap(
    paste0(label, ": ", fmt(value), " (", tolower(status), ")\nTypical ", fmt(typical),
           "\nNormal range ", fmt(lower), " to ", fmt(upper),
           if (!is.null(note)) paste0("\n", note) else ""),
    htmltools::div(class = "md-rb-head",
      htmltools::span(label),
      htmltools::span(class = if (flag) "md-rb-val md-warn" else "md-rb-val", value_text)),
    htmltools::div(class = "md-rb-track",
      htmltools::div(class = "md-rb-band", style = paste0("left:", pos(lower), ";width:calc(", pos(upper), " - ", pos(lower), ")")),
      htmltools::div(class = "md-rb-typical", style = paste0("left:", pos(typical))),
      htmltools::div(class = if (flag) "md-rb-dot md-flag" else "md-rb-dot", style = paste0("left:", pos(value)))),
    htmltools::div(class = "md-rb-foot md-muted",
      paste0(if (!is.null(note)) paste0(note, " \u00b7 ") else "", "Typical ", fmt(typical),
             " \u00b7 normal range ", fmt(lower), " to ", fmt(upper)))
  )
}
