# Load and recovery monitor
# A Shiny app on the CFC Insights Vizathon 2025 data (simulated data for one player).
# Run from the project root: shiny::runApp("app")
#
# app/
#   ui/                  layout of each page
#   server/              reactivity: inputs, links and shared state
#   data_visualisation/  reusable visual components (no data access)
#   data_management/     loading the data and preparing it for each page (no Shiny)
#   www/                 styles and images

suppressPackageStartupMessages(library(dplyr))

# Project functions (database connection, readers, adaptive ranges), then the app's own
for (f in list.files(here::here("utils"), pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
for (dir in c("data_management", "data_visualisation", "ui", "server")) {
  for (f in list.files(here::here("app", dir), pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
}

ensure_database()
app_data <- load_app_data()

theme <- bslib::bs_theme(
  version = 5,
  bg = "#FFFFFF", fg = "#1F2A37",
  primary = "#007A7A", secondary = "#4E5866",
  base_font = bslib::font_google("Inter", local = FALSE),
  heading_font = bslib::font_google("Inter", local = FALSE),
  "border-radius" = "10px"
)

ui <- bslib::page_navbar(
  id = "main_nav",
  fillable = FALSE,
  title = "Load and recovery monitor",
  theme = theme,
  header = htmltools::tags$link(rel = "stylesheet", href = "styles.css"),
  bslib::nav_panel("Overview", value = "overview", ui_overview("overview")),
  bslib::nav_panel("Profile", value = "profile", ui_placeholder("Player profile: next")),
  bslib::nav_panel("Load over time", value = "load", ui_placeholder("Longitudinal overview: to follow")),
  bslib::nav_panel("Session check", value = "session", shiny::uiOutput("session_placeholder")),
  bslib::nav_spacer(),
  bslib::nav_item(htmltools::span(
    class = "md-asof",
    shiny::icon("calendar"),
    paste("Viewing as of", fmt_date_short(app_data$app_date)),
    htmltools::span(class = "md-muted", "\u00b7 the last day of data \u00b7 simulated")
  ))
)

server <- function(input, output, session) {
  # The day being looked at on the session check page, shared between pages
  selected_date <- shiny::reactiveVal(app_data$app_date)
  server_overview("overview", app_data, nav_session = session, selected_date = selected_date)
  output$session_placeholder <- shiny::renderUI(
    ui_placeholder(paste("Session check for", fmt_date_short(selected_date()), "(page to follow)"))
  )
}

shiny::shinyApp(ui, server)
