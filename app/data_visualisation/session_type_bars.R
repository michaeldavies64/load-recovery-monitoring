# Typical load by session type: one row per match-day label, a bar to the median and a
# whisker across the 10th to 90th percentile, with a dashed line marking a full match.
# Built in HTML, with a tooltip on each row.
# rows: data frame with match_day, sessions, median, p10, p90, pct_of_match
# reference: typical full-match value (NA to leave the line out)

session_type_bars <- function(rows, reference, decimals = 0, unit = "") {
  top <- max(c(rows$p90, reference), na.rm = TRUE) * 1.05
  pos <- function(x) paste0(round(100 * x / top, 1), "%")
  items <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    pct <- if (!is.na(r$pct_of_match)) paste0(round(100 * r$pct_of_match), "% of a full match") else ""
    htmltools::div(
      class = paste("md-st-row", if (r$match_day == "Extended gap") "md-st-muted"),
      `data-tip` = paste0(r$match_day, " \u00b7 ", r$sessions, " sessions\nTypical ", fmt_value(r$median, decimals, unit),
                          "\nUsual spread ", fmt_value(r$p10, decimals), " to ", fmt_value(r$p90, decimals),
                          if (nzchar(pct)) paste0("\n", pct) else ""),
      htmltools::span(class = "md-st-label", r$match_day),
      htmltools::div(class = "md-st-track",
        htmltools::div(class = "md-st-bar", style = paste0("width:", pos(r$median))),
        htmltools::div(class = "md-st-whisker", style = paste0("left:", pos(r$p10), ";width:calc(", pos(r$p90), " - ", pos(r$p10), ")")),
        if (!is.na(reference)) htmltools::div(class = "md-st-ref", style = paste0("left:", pos(reference)))),
      htmltools::span(class = "md-st-value", fmt_value(r$median, decimals),
                      htmltools::span(class = "md-muted small", if (nzchar(pct)) paste0(" \u00b7 ", round(100 * r$pct_of_match), "%")))
    )
  })
  htmltools::tagList(
    htmltools::div(class = "md-st", items),
    htmltools::div(class = "md-muted small mt-2",
      htmltools::span(class = "md-key md-key-bar"), "typical session  ",
      htmltools::span(class = "md-key md-key-whisker"), "usual spread (10th to 90th percentile)  ",
      if (!is.na(reference)) htmltools::tagList(htmltools::span(class = "md-key md-key-ref"), "full match"))
  )
}
