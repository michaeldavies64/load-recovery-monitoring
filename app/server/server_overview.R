# Overview page: fills the banner and tiles, and handles the links and clicks.
# nav_session: the top-level session, so links can switch pages.
# selected_date: a reactiveVal shared with the session check page, so clicking a day here
# opens that day there.

server_overview <- function(id, data, nav_session, selected_date) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    d <- overview_data(data)

    output$banner <- shiny::renderUI(player_banner(d$profile, d$app_date, d$n_flag, d$n_assessed))

    output$tile_status <- shiny::renderUI({
      counts <- function(src) {
        s <- d$status[d$status$source == src & is_assessed(d$status$status), ]
        if (!nrow(s)) return(md_count("\u2013", paste(src, "has no range today"), muted = TRUE))
        md_count(sum(is_flag(s$status)), paste(src, "outside range"), of = nrow(s))
      }
      bars <- lapply(seq_len(nrow(d$flagged)), function(i) {
        f <- d$flagged[i, ]
        when <- if (f$days_ago == 0) "today" else paste(f$days_ago, "days ago")
        range_bar(f$short_label, f$value, f$lower, f$upper, f$typical, f$status, f$decimals, f$unit,
                  note = paste(f$match_day, when), onclick = js_set_input(ns("go_profile_bar"), f$metric))
      })
      md_tile("heart-pulse", "Current status",
              htmltools::tagList(htmltools::div(class = "md-counts", counts("GPS"), counts("Recovery")),
                                 if (nrow(d$flagged)) bar_legend(ranges = TRUE), bars),
              ns("go_profile"), "Open profile")
    })

    output$tile_session <- shiny::renderUI({
      key <- c("day_duration", "distance", "distance_over_21", "accel_decel_over_3_5")
      shown <- d$latest[d$latest$metric %in% key, ]
      bars <- lapply(key, function(m) {
        r <- d$latest[d$latest$metric == m, ]
        range_bar(r$short_label, r$value, r$lower, r$upper, r$typical, r$status, r$decimals, r$unit,
                  pct_of_match = r$pct_of_match, onclick = js_set_input(ns("pick_date"), format(d$latest_date)))
      })
      day <- d$session_day
      what <- paste0(fmt_date_short(d$latest_date), " \u00b7 ", day$match_day,
                     if (day$is_match_day) paste0(" v ", day$opposition_full) else "")
      intro <- if (all(is.na(d$latest$lower[d$latest$metric %in% key]))) {
        paste0("A ", round(day$day_duration), "-minute match, outside the 85-125 minute band used for normal ranges, so compared with a full match instead")
      } else "Against the player's normal range for this kind of day"
      md_tile("calendar", "Latest session",
              htmltools::tagList(htmltools::p(class = "md-tile-value", what),
                                 htmltools::p(class = "md-muted small", intro),
                                 bar_legend(ranges = any(!is.na(shown$lower)),
                                            match = any(is.na(shown$lower) & !is.na(shown$pct_of_match))),
                                 bars),
              ns("go_session"), "Check this session")
    })

    output$tile_28d <- shiny::renderUI({
      md_tile("chart-column", "Last 28 days",
        htmltools::tagList(
          htmltools::div(class = "md-counts",
            md_count(sum(d$last28$has_session), "sessions"),
            md_count(sum(d$last28$is_match_day), "matches"),
            md_count(fmt_value(d$distance_week / 1000, 1),
                     paste0("km a week (typical ", fmt_value(d$distance_week_typical / 1000, 1), ")"))),
          htmltools::p(class = "md-subhead", "Total distance each day"),
          daily_bars(d$last28, value_label = function(v) fmt_value(v / 1000, 1, "km"),
                     onclick = function(date) js_set_input(ns("pick_date"), format(date)))),
        ns("go_load"), "See load over time")
    })

    go <- function(page) bslib::nav_select("main_nav", page, session = nav_session)
    shiny::observeEvent(input$go_profile, go("profile"))
    shiny::observeEvent(input$go_profile_bar, go("profile"))
    shiny::observeEvent(input$go_load, go("load"))
    shiny::observeEvent(input$go_session, { selected_date(d$latest_date); go("session") })
    shiny::observeEvent(input$pick_date, { selected_date(as.Date(input$pick_date)); go("session") })
  })
}
