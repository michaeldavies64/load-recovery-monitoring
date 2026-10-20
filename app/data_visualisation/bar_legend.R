# Legend for range bars and share-of-a-full-match bars.
# Shows only the parts present: pass ranges = TRUE if any bar has an adaptive range, and
# match = TRUE if any bar is compared with a full match instead.

bar_legend <- function(ranges = TRUE, match = FALSE) {
  item <- function(swatch, text) htmltools::span(class = "md-legend-item", swatch, text)
  line <- function(title, ...) htmltools::div(class = "md-legend-line", htmltools::span(class = "md-legend-title", title), ...)
  htmltools::div(
    class = "md-legend",
    if (ranges) line("Range bars",
      item(htmltools::span(class = "md-lg-band"), "normal range for this kind of day"),
      item(htmltools::span(class = "md-lg-tick"), "typical value"),
      item(htmltools::span(class = "md-lg-dot"), "this session"),
      item(htmltools::span(class = "md-lg-dot md-lg-flag"), "outside the normal range")
    ),
    if (match) line("Full-match bars",
      item(htmltools::span(class = "md-lg-fill"), "this session, as a share of a full match"),
      item(htmltools::span(class = "md-lg-tick"), "a full match (100%)")
    )
  )
}
