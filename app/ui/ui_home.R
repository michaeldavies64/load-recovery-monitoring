# Home page: a welcome, what the tool does, and the choice of player.
# Everything here except the player list is fixed text, so the page shows straight away,
# even while a fresh deployment is still building the database. Until then the player
# list holds a placeholder and a disabled button; server_home() replaces it when ready.

ui_home <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "md-home",
    htmltools::div(
      class = "md-home-hero",
      htmltools::p(class = "md-home-kicker", "Load and recovery monitoring"),
      htmltools::h1(class = "md-home-title", "Welcome to Michael's load and recovery monitoring tool"),
      htmltools::p(class = "md-home-lead",
        "How much load is a player carrying, and how is their recovery responding? Each session and recovery score is compared with that player's own normal range for that kind of day, so unusual days stand out at a glance."),
      htmltools::div(class = "md-home-facts",
        htmltools::span(shiny::icon("location-crosshairs"), "GPS and recovery data"),
        htmltools::span(shiny::icon("chart-line"), "Adaptive normal ranges"),
        htmltools::span(shiny::icon("flask"), "Simulated data"))
    ),
    htmltools::h2(class = "md-home-h2", "Choose a player"),
    # What uiOutput() makes, but holding a placeholder until the server renders the player list
    htmltools::div(id = ns("players"), class = "shiny-html-output", home_loading()),
    htmltools::div(
      class = "md-home-foot",
      htmltools::p(class = "md-muted mb-1",
        "The player and their data are simulated: CFC Insights Vizathon 2025, Chelsea FC Performance Insights."),
      htmltools::div(class = "md-home-links",
        shiny::actionLink(ns("go_glossary"), htmltools::tagList(shiny::icon("book"), "Glossary: what every metric means")),
        htmltools::span(class = "md-muted", "Built by Michael Davies in R: DuckDB, Quarto and Shiny"))
    )
  )
}

# Shown while the data is being prepared
home_loading <- function() {
  htmltools::tagList(
    htmltools::div(class = "md-player-card md-player-card-loading",
      htmltools::div(class = "md-headshot md-skeleton"),
      htmltools::div(class = "md-player-card-text",
        htmltools::span(class = "md-skeleton md-skeleton-line", style = "width: 40%"),
        htmltools::span(class = "md-skeleton md-skeleton-line", style = "width: 65%"))),
    htmltools::div(class = "md-home-continue",
      htmltools::tags$button(type = "button", class = "btn btn-primary", disabled = NA,
        htmltools::span(class = "spinner-border spinner-border-sm me-2", role = "status", `aria-hidden` = "true"),
        "Preparing the data"),
      htmltools::span(class = "md-muted small", "The first visit after a quiet spell takes about 10 seconds."))
  )
}
