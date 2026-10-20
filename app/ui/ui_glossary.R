# Glossary page: what every metric means, where it comes from, and how it is used.
# Searchable tables, filled by server_glossary().

ui_glossary <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::card(
      fill = FALSE,
      bslib::card_header(class = "md-card-header", htmltools::span("GPS metrics")),
      bslib::card_body(
        htmltools::p(class = "md-muted small", "Definitions from the CFC Insights Vizathon 2025 data guide, with notes on how this app uses each metric. Per-minute values are derived here: the metric divided by session duration."),
        DT::DTOutput(ns("gps"), height = "auto", fill = FALSE)
      )
    ),
    bslib::card(
      fill = FALSE,
      bslib::card_header(class = "md-card-header", htmltools::span("Recovery scores")),
      bslib::card_body(
        htmltools::p(class = "md-muted small", "Composite scores are relative to the player's own norm: 0% is normal and -50% is half the usual value. Missing means not collected; 0 is a real 0."),
        DT::DTOutput(ns("recovery"), height = "auto", fill = FALSE)
      )
    )
  )
}
