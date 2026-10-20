# Profile page: fills the profile card, the status of the selected metrics, and the
# typical load by session type for the selected metric and season.

server_profile <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    d <- profile_data(data)

    # Fill the controls' choices (the page is built before the data is ready)
    shiny::updateCheckboxGroupInput(session, "metrics", choices = d$status_choices, selected = d$status_default)
    shiny::updateSelectInput(session, "type_metric", choices = d$type_choices, selected = "distance")
    shiny::updateRadioButtons(session, "season", choices = d$seasons, selected = d$seasons[1], inline = TRUE)

    output$card <- shiny::renderUI(profile_card(d$profile, d$app_date))

    output$chooser_title <- shiny::renderUI(
      paste0("Metrics shown (", length(input$metrics), " of ", length(d$status_choices), ")"))
    shiny::observeEvent(input$all, shiny::updateCheckboxGroupInput(session, "metrics", selected = d$status_choices))
    shiny::observeEvent(input$none, shiny::updateCheckboxGroupInput(session, "metrics", selected = character(0)))

    # The latest session date and day type go in the card title once; a metric measured on
    # a different day (such as a recovery test two days earlier) says so on its own bar
    latest <- d$status[d$status$date == max(d$status$date), ][1, ]
    output$status_title <- shiny::renderUI(htmltools::span(
      "Current status",
      htmltools::span(class = "md-muted md-title-note",
        paste0(fmt_date_short(latest$date), " \u00b7 ", latest$match_day))))

    output$status <- shiny::renderUI({
      shown <- dplyr::filter(d$status, metric %in% input$metrics)
      if (!nrow(shown)) return(htmltools::p(class = "md-muted", "Choose at least one metric to show."))
      # Groups longer than 5 continue in the next column
      blocks <- list()
      for (g in unique(shown$group)) {
        rows <- dplyr::filter(shown, group == g)
        chunks <- split(seq_len(nrow(rows)), ceiling(seq_len(nrow(rows)) / 5))
        for (k in seq_along(chunks)) blocks[[length(blocks) + 1]] <- list(title = if (k == 1) g else paste(g, "(continued)"), rows = rows[chunks[[k]], ])
      }
      legend <- bar_legend(ranges = any(!is.na(shown$lower)),
                           match = any(is.na(shown$lower) & !is.na(shown$pct_of_match)))
      htmltools::tagList(legend, htmltools::div(class = "md-status-grid", lapply(blocks, function(b) {
        htmltools::div(class = "md-status-group",
          htmltools::p(class = "md-subhead mt-0", b$title),
          lapply(seq_len(nrow(b$rows)), function(i) {
            r <- b$rows[i, ]
            note <- if (r$date != latest$date) paste(r$match_day, "\u00b7", r$days_ago, if (r$days_ago == 1) "day ago" else "days ago") else NULL
            range_bar(r$short_label, r$value, r$lower, r$upper, r$typical, r$status, r$decimals, r$unit,
                      note = note, pct_of_match = r$pct_of_match)
          }))
      })))
    })

    output$by_type <- shiny::renderUI({
      shiny::req(input$type_metric, input$season)
      rows <- dplyr::filter(d$by_type, metric == input$type_metric, season == input$season) |>
        dplyr::arrange(match_day_order)
      if (!nrow(rows)) return(htmltools::p(class = "md-muted", "No sessions for this selection."))
      reference <- rows$median[rows$match_day == "MD"] / rows$pct_of_match[rows$match_day == "MD"]
      htmltools::tagList(
        htmltools::p(class = "md-muted small",
          paste0(rows$label[1], ", ", tolower(input$season), ". Each row is one type of day; the percentage compares its typical session with a full match (85 to 125 minutes).")),
        session_type_bars(rows, if (length(reference)) reference else NA, rows$decimals[1], rows$unit[1])
      )
    })
  })
}
