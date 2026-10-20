# Longitudinal page: the shared controls (view and period), and the six chart cards.
# Clicking a session on any card opens it on the session check page.

server_load <- function(id, data, nav_session, selected_date) {
  shiny::moduleServer(id, function(input, output, session) {
    d <- load_time_data(data)

    # Fill the controls (the page is built before the data is ready). Default: the last 28 days
    shiny::updateDateRangeInput(session, "dates", start = d$app_date - 27, end = d$app_date,
                                min = d$first_date, max = d$app_date)
    set_period <- function(start) shiny::updateDateRangeInput(session, "dates", start = max(start, d$first_date), end = d$app_date)
    shiny::observeEvent(input$p28, set_period(d$app_date - 27))
    shiny::observeEvent(input$p3m, set_period(seq(d$app_date, by = "-3 months", length.out = 2)[2] + 1))
    shiny::observeEvent(input$season, set_period(d$season_start))
    shiny::observeEvent(input$all, set_period(d$first_date))

    view <- shiny::reactive(shiny::req(input$view))
    dates <- shiny::reactive({
      shiny::req(input$dates)
      shiny::validate(shiny::need(input$dates[1] <= input$dates[2], "The start date must be before the end date."))
      input$dates
    })

    # Sessions and matches are the same for every metric, so they sit with the controls
    output$period_counts <- shiny::renderUI({
      s <- load_time_summary(load_time_days(d, "distance", dates()[1], dates()[2], "daily"), "distance")
      htmltools::span(class = "md-muted small", paste0(s$sessions, " sessions \u00b7 ", s$matches, " matches in this period"))
    })
    output$legend <- shiny::renderUI({
      gap <- !all(seq(dates()[1], dates()[2], by = "day") %in% d$long$date)
      load_chart_legend(view(), gap)
    })

    open_session <- function(date) {
      selected_date(as.Date(date))
      bslib::nav_select("main_nav", "session", session = nav_session)
    }
    for (i in seq_along(d$cards)) {
      server_load_card(paste0("card", i), d, d$cards[i], view, dates, open_session)
    }
  })
}

# One chart card. metric: the metric it starts on; view and dates: the shared controls
server_load_card <- function(id, d, metric, view, dates, open_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::updateSelectizeInput(session, "metric", choices = d$metric_choices, selected = metric)

    meta <- shiny::reactive({
      shiny::req(input$metric)
      d$glossary[d$glossary$column_name == input$metric, ]
    })
    days <- shiny::reactive({
      shiny::req(input$metric)
      load_time_days(d, input$metric, dates()[1], dates()[2], view())
    })
    fmt <- function(v) fmt_value(v, meta()$decimals, meta()$unit)

    output$note <- shiny::renderUI(view_label(input$metric, view()))

    output$summary <- shiny::renderUI({
      s <- load_time_summary(days(), input$metric)
      m <- meta()
      parts <- c(
        if (!is.na(s$total)) paste(fmt(s$total), switch(s$kind, amount = "in total", max = "highest", rate = "average per session")),
        if (!is.na(s$weekly)) paste(fmt(s$weekly), "a week on average"),
        if (s$assessed) paste(s$flagged, "of", s$assessed, if (s$assessed == 1) "session" else "sessions", "outside the normal range")
      )
      htmltools::p(class = "md-lt-card-summary", paste(parts, collapse = " \u00b7 "))
    })

    output$chart <- shiny::renderUI({
      x <- days()
      m <- meta()
      v <- view()
      tip <- function(r) {
        head <- paste0(format(r$date, "%a "), fmt_date_short(r$date))
        if (!r$in_data) return(paste0(head, "\nNo GPS data"))
        lines <- c(head, paste0(r$match_day, if (isTRUE(r$is_match_day) && !is.na(r$opposition_full)) paste0(" v ", r$opposition_full) else ""))
        if (!r$has_session) {
          lines <- c(lines, "No session")
        } else {
          lines <- c(lines, paste("Session:", fmt(r$value)))
          # Share of a full match (the season's median for matches of 85 to 125 minutes)
          if (input$metric != "day_duration" && !is.na(r$match_median) && r$match_median > 0) {
            lines <- c(lines, paste0(round(100 * r$value / r$match_median), "% of a full match (", fmt(r$match_median), ")"))
          }
          if (!is.na(r$lower)) {
            lines <- c(lines, paste0(r$status, ": normal ", fmt_value(r$lower, m$decimals), " to ", fmt(r$upper),
                                     ", typical ", fmt(r$range_typical)))
          } else if (!is.na(r$status)) {
            lines <- c(lines, r$status)
          }
        }
        if (v %in% c("7d", "28d")) {
          lines <- c(lines, paste0(view_label(input$metric, v), ": ", if (is.na(r$rolling)) "not yet a full window" else fmt(r$rolling)))
          lines <- c(lines, if (r$window_flags == 0) "No sessions outside the normal range in this window"
                            else paste0("Sessions outside the normal range in this window: ", r$window_flags, " (", r$window_flag_list, ")"))
        }
        if (r$has_session) lines <- c(lines, "Click to check this session")
        paste(lines, collapse = "\n")
      }
      if (!any(x$in_data)) return(htmltools::p(class = "md-muted", "No GPS data in this period."))
      load_chart(x, v, tip, onclick = function(date) js_set_input(ns("pick_date"), format(date)), height = 220)
    })

    shiny::observeEvent(input$pick_date, open_session(input$pick_date))
  })
}
