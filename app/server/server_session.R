# Session check page: follows the shared selected_date, which the overview and the
# longitudinal page set when a session is clicked, and moves between sessions.

server_session <- function(id, data, selected_date) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sd <- session_data(data)
    all_days <- seq(min(sd$session_dates), sd$app_date, by = "day")

    # The date picker only offers days with a session
    shiny::updateDateInput(session, "date", value = shiny::isolate(selected_date()), min = min(sd$session_dates), max = sd$app_date)
    session$sendInputMessage("date", list(datesdisabled = format(setdiff(all_days, sd$session_dates) |> as.Date(origin = "1970-01-01"))))

    # Keep the picker and the shared date in step, whichever changes
    shiny::observeEvent(selected_date(), {
      if (!identical(input$date, selected_date())) shiny::updateDateInput(session, "date", value = selected_date())
    })
    shiny::observeEvent(input$date, {
      shiny::req(input$date %in% sd$session_dates)
      if (!identical(input$date, selected_date())) selected_date(input$date)
    })
    step <- function(k) {
      i <- findInterval(selected_date(), sd$session_dates)
      i <- if (k < 0 && sd$session_dates[i] == selected_date()) i - 1 else if (k > 0) i + 1 else i
      if (i >= 1 && i <= length(sd$session_dates)) selected_date(sd$session_dates[i])
    }
    shiny::observeEvent(input$prev, step(-1))
    shiny::observeEvent(input$next_s, step(1))
    shiny::observeEvent(input$latest, selected_date(max(sd$session_dates)))
    shiny::observeEvent(input$pick_date, selected_date(as.Date(input$pick_date)))

    v <- shiny::reactive({
      shiny::validate(shiny::need(selected_date() %in% sd$session_dates, "There was no session on this day. Choose another date."))
      session_view(sd, selected_date())
    })

    output$facts <- shiny::renderUI({
      s <- v(); day <- s$day
      what <- paste0(day$match_day, if (isTRUE(day$is_match_day) && !is.na(day$opposition_full)) paste0(" v ", day$opposition_full) else "")
      kind <- if (isTRUE(day$is_match_day)) { if (isTRUE(day$is_full_match)) "Full match" else "Match day, not a full match (85 to 125 minutes)" } else "Training"
      htmltools::div(class = "md-sc-facts",
        htmltools::div(
          htmltools::h2(class = "md-sc-title", paste0(format(s$date, "%A "), fmt_date_short(s$date))),
          htmltools::p(class = "md-muted mb-0", paste(what, kind, paste(round(day$day_duration), "minutes"), sep = " \u00b7 "))),
        if (s$n_assessed) htmltools::div(class = "md-counts",
          md_count(s$n_flag, paste("of", s$n_assessed, "metrics outside their normal range")))
        else htmltools::p(class = "md-muted small mb-0 md-sc-norange",
          if (isTRUE(day$is_match_day)) "No normal ranges: only full matches (85 to 125 minutes) have them, so this session is compared with a full match instead."
          else "No normal ranges for this session: see each metric for why."))
    })

    output$range_title <- shiny::renderUI(htmltools::span("Against the normal range",
      htmltools::span(class = "md-muted md-title-note", paste("for", v()$day$match_day))))

    output$ranges <- shiny::renderUI({
      s <- v()
      if (!nrow(s$load)) return(htmltools::p(class = "md-muted", "No load data for this session."))
      groups <- split(s$load, factor(s$load$group, levels = unique(s$load$group)))
      htmltools::tagList(
        bar_legend(ranges = any(!is.na(s$load$lower)), match = any(is.na(s$load$lower) & !is.na(s$load$pct_of_match))),
        htmltools::div(class = "md-status-grid", lapply(names(groups), function(g) {
          rows <- groups[[g]]
          htmltools::div(class = "md-status-group",
            htmltools::p(class = "md-subhead mt-0", g),
            lapply(seq_len(nrow(rows)), function(i) {
              r <- rows[i, ]
              range_bar(r$short_label, r$value, r$lower, r$upper, r$typical, r$status, r$decimals, r$unit,
                        pct_of_match = r$pct_of_match)
            }))
        }))
      )
    })

    output$recent_title <- shiny::renderUI({
      s <- v()
      htmltools::span("Compared with recent sessions of the same type",
        htmltools::span(class = "md-muted md-title-note", paste0("the last ", length(unique(s$recent$date)), " ", s$recent_label)))
    })

    output$recent <- shiny::renderUI({
      s <- v()
      if (length(unique(s$recent$date)) < 2) return(htmltools::p(class = "md-muted", "This is the first session of its type, so there is nothing to compare it with yet."))
      rows <- lapply(sd$key_metrics, function(m) {
        x <- s$recent[s$recent$metric == m, ]
        x <- x[order(x$date), ]
        if (!nrow(x)) return(NULL)
        i <- which(x$date == s$date)
        session_strip(x$label[1], x, s$date, x$decimals[1], x$unit[1],
                      paste(rank_phrase(x$value, i), "of", nrow(x)),
                      onclick = function(date) js_set_input(ns("pick_date"), format(date)))
      })
      htmltools::tagList(
        htmltools::p(class = "md-muted small", "Each dot is one session; this one is the large dot, amber if it is outside its normal range. The tick is the typical value for this kind of day. Click a dot to open that session."),
        htmltools::div(class = "md-ss", rows))
    })

    output$recovery <- shiny::renderUI({
      s <- v()
      metrics <- dplyr::distinct(sd$recovery, metric, label, display_order) |> dplyr::arrange(display_order)
      htmltools::tagList(
        htmltools::p(class = "md-muted small", "The session day and the next two days with recovery data, within a week (recovery is often not collected on match days or the day after). Each score is relative to the player's norm (0%); amber is outside the normal range for that kind of day. Hover for the range."),
        recovery_grid(s$recovery, s$recovery_days, s$recovery_labels, metrics),
        if (length(s$recovery_days) < 3) htmltools::p(class = "md-muted small mt-2 mb-0",
          if (s$date + 7 > sd$app_date) "The data ends before the next recovery days." else "Fewer than two later days with recovery data in the following week."))
    })
  })
}
