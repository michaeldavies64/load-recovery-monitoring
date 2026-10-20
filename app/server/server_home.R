# Home page: fills the player list and the continue button, and opens the chosen
# player's overview. selected_player is shared with the other pages: there is one player
# in this data, but the pages read the choice from here so more players can be added later.

server_home <- function(id, data, nav_session, selected_player) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    h <- home_data(data)

    output$players <- shiny::renderUI({
      cards <- lapply(seq_len(nrow(h$profile)), function(i) {
        p <- h$profile[i, ]
        player_card(p, h$app_date, h$n_flag, h$n_assessed, h$last_session, h$last_session_label,
                    onclick = js_set_input(ns("open_player"), p$player_id))
      })
      htmltools::tagList(
        htmltools::div(class = "md-player-list", cards),
        htmltools::div(class = "md-home-continue",
          shiny::actionButton(ns("continue"), htmltools::tagList("Continue", shiny::icon("arrow-right")), class = "btn-primary"),
          htmltools::span(class = "md-muted small",
            paste0("Viewing as of ", fmt_date_short(h$app_date), ", the last day of data.")))
      )
    })

    go <- function(page) bslib::nav_select("main_nav", page, session = nav_session)
    shiny::observeEvent(input$open_player, { selected_player(input$open_player); go("overview") })
    shiny::observeEvent(input$continue, { selected_player(h$profile$player_id[1]); go("overview") })
    shiny::observeEvent(input$go_glossary, go("glossary"))
  })
}
