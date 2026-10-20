# Session check page: one session against its normal ranges, against recent sessions of
# the same type, and the recovery scores around it. The content is rendered by
# server_session(); the session is the shared selected_date.

ui_session <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::card(
      class = "md-sc-header-card",
      bslib::card_body(
        htmltools::div(class = "md-sc-nav",
          shiny::actionButton(ns("prev"), htmltools::tagList(shiny::icon("chevron-left"), "Previous session"), class = "btn-sm btn-outline-secondary"),
          shiny::dateInput(ns("date"), NULL, format = "D d M yyyy", weekstart = 1, width = "190px"),
          shiny::actionButton(ns("next_s"), htmltools::tagList("Next session", shiny::icon("chevron-right")), class = "btn-sm btn-outline-secondary"),
          shiny::actionButton(ns("latest"), "Latest", class = "btn-sm btn-outline-secondary")),
        shiny::uiOutput(ns("facts"))
      )
    ),
    bslib::card(
      bslib::card_header(class = "md-card-header", shiny::uiOutput(ns("range_title"), inline = TRUE)),
      bslib::card_body(shiny::uiOutput(ns("ranges")))
    ),
    bslib::card(
      bslib::card_header(class = "md-card-header", shiny::uiOutput(ns("recent_title"), inline = TRUE)),
      bslib::card_body(shiny::uiOutput(ns("recent")))
    ),
    bslib::card(
      bslib::card_header(class = "md-card-header", htmltools::span("Recovery around this session")),
      bslib::card_body(shiny::uiOutput(ns("recovery")))
    )
  )
}
