# Building blocks for cards: tiles, headline counts and the player banner

# A landing-page tile: label, body and a link to another page
md_tile <- function(icon, label, body, link_id, link_text) {
  bslib::card(
    class = "md-tile",
    bslib::card_body(
      htmltools::p(class = "md-tile-label", shiny::icon(icon), label),
      htmltools::div(class = "md-tile-body", body),
      shiny::actionLink(link_id, class = "md-tile-link",
                        label = htmltools::tagList(link_text, shiny::icon("arrow-right")))
    )
  )
}

# A headline number with a label underneath; denominator optional
md_count <- function(n, label, of = NULL, muted = FALSE) {
  htmltools::div(class = "md-count",
    htmltools::span(class = paste("md-count-n", if (muted) "md-muted"), n,
                    if (!is.null(of)) htmltools::span(class = "md-muted", paste0("/", of))),
    htmltools::span(class = "md-muted small", label))
}

# The player banner: headshot, name and details, and how many metrics are outside range
player_banner <- function(profile, app_date, n_flag, n_assessed) {
  bslib::card(
    class = "md-banner",
    bslib::card_body(
      class = "md-banner-grid",
      htmltools::tags$img(src = profile$headshot_file, class = "md-headshot", alt = "Illustrated headshot"),
      htmltools::div(
        htmltools::h1(class = "md-banner-title", profile$display_name),
        htmltools::p(class = "md-banner-meta",
          paste(profile$position, paste("Age", age_on(profile$date_of_birth, app_date)),
                paste(profile$height_cm, "cm"), paste(profile$weight_kg, "kg"), sep = "  \u00b7  ")),
        htmltools::p(class = "md-muted mb-0 small", paste0("Status as of ", fmt_date_short(app_date), " \u00b7 simulated player"))
      ),
      htmltools::div(
        class = "md-banner-stat",
        htmltools::span(class = if (n_flag) "md-stat-value md-warn" else "md-stat-value", n_flag),
        htmltools::span(class = "md-muted", paste("of", n_assessed, "metrics with a range"), htmltools::br(), "are outside it")
      )
    )
  )
}
