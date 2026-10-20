# Profile page: who the player is, the current status of every metric, and what each
# type of session typically looks like. The content is rendered by server_profile().
# The metric chooser sits under the profile card, collapsed by default. The controls start
# empty; server_profile() fills their choices once the data is ready.

ui_profile <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(3, 9),
      htmltools::div(
        shiny::uiOutput(ns("card")),
        bslib::accordion(
          id = ns("chooser"), open = FALSE, class = "md-chooser",
          bslib::accordion_panel(
            title = shiny::uiOutput(ns("chooser_title"), inline = TRUE), value = "metrics", icon = shiny::icon("sliders"),
            htmltools::div(class = "md-chooser-actions",
              shiny::actionLink(ns("all"), "Show all"), shiny::actionLink(ns("none"), "Hide all")),
            shiny::checkboxGroupInput(ns("metrics"), NULL, choices = character(0))
          )
        )
      ),
      htmltools::div(
        bslib::card(
          bslib::card_header(class = "md-card-header", shiny::uiOutput(ns("status_title"), inline = TRUE)),
          bslib::card_body(shiny::uiOutput(ns("status")))
        ),
        bslib::card(
          bslib::card_header(
            class = "md-card-header",
            htmltools::span("Typical load by session type"),
            htmltools::div(class = "md-controls",
              shiny::selectInput(ns("type_metric"), NULL, choices = character(0), width = "300px"),
              shiny::radioButtons(ns("season"), NULL, choices = character(0), selected = character(0), inline = TRUE))
          ),
          bslib::card_body(shiny::uiOutput(ns("by_type")))
        )
      )
    )
  )
}
