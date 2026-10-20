# Longitudinal page: a controls card that sets the view and period for every chart,
# then seven cards, each starting on a key metric that the user can change.
# The controls start empty; server_load() fills them once the data is ready.

ui_load <- function(id, n_cards = 7) {
  ns <- shiny::NS(id)
  preset <- function(inputId, label) shiny::actionButton(ns(inputId), label, class = "btn-sm btn-outline-secondary")
  htmltools::tagList(
    bslib::card(
      class = "md-lt-control-card",
      bslib::card_body(
        htmltools::div(
          class = "md-lt-controls",
          shiny::radioButtons(ns("view"), "View", inline = TRUE,
                              choices = c("Daily" = "daily", "Adaptive range" = "range",
                                          "Rolling 7 days" = "7d", "Rolling 28 days" = "28d")),
          htmltools::div(
            class = "md-lt-period",
            htmltools::tags$label(class = "control-label", "Period"),
            htmltools::div(class = "md-lt-presets",
              preset("p28", "Last 28 days"), preset("p3m", "Last 3 months"),
              preset("season", "This season"), preset("all", "All")),
            shiny::dateRangeInput(ns("dates"), NULL, format = "d M yyyy", separator = "to", weekstart = 1)
          )
        ),
        htmltools::div(class = "md-lt-legend-row",
          shiny::uiOutput(ns("legend")),
          shiny::uiOutput(ns("period_counts"), class = "md-lt-period-counts"))
      )
    ),
    htmltools::div(class = "md-lt-cards",
      lapply(seq_len(n_cards), function(i) ui_load_card(ns(paste0("card", i)))))
  )
}

# One chart card: its own metric chooser in the header, then the period total and chart
ui_load_card <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    class = "md-lt-card",
    bslib::card_header(
      class = "md-card-header md-lt-card-header",
      shiny::selectizeInput(ns("metric"), NULL, choices = character(0), width = "100%",
                            options = list(dropdownParent = "body")),
      shiny::uiOutput(ns("note"), class = "md-muted small")
    ),
    bslib::card_body(shiny::uiOutput(ns("summary")), shiny::uiOutput(ns("chart")))
  )
}
