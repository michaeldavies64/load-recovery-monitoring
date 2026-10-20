# A player to choose on the home page: headshot, details and a one-line status.
# onclick: JavaScript to run when the card is clicked (see js_set_input()).

player_card <- function(profile, app_date, n_flag, n_assessed, last_session, last_session_label, onclick) {
  htmltools::tags$button(
    type = "button", class = "md-player-card", onclick = onclick,
    `aria-label` = paste("Open", profile$display_name),
    htmltools::tags$img(src = profile$headshot_file, class = "md-headshot", alt = ""),
    htmltools::div(
      class = "md-player-card-text",
      htmltools::span(class = "md-player-name", profile$display_name),
      htmltools::span(class = "md-muted", paste(profile$position, "\u00b7 Age", age_on(profile$date_of_birth, app_date),
                                               "\u00b7", profile$height_cm, "cm \u00b7", profile$weight_kg, "kg")),
      htmltools::span(class = "md-muted small", paste0("Last session ", fmt_date_short(last_session), " \u00b7 ", last_session_label))
    ),
    htmltools::div(
      class = "md-player-card-status",
      htmltools::span(class = if (n_flag) "md-stat-value md-warn" else "md-stat-value", n_flag),
      htmltools::span(class = "md-muted small", paste("of", n_assessed, "metrics outside"), htmltools::br(), "their normal range")
    ),
    shiny::icon("arrow-right", class = "md-player-card-go")
  )
}
