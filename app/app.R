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
  header = htmltools::tagList(
    htmltools::tags$link(rel = "stylesheet", href = "styles.css"),
    # Until the data is ready, only the home page can be opened (see styles.css)
    htmltools::tags$script(htmltools::HTML(
      "document.documentElement.classList.add('md-loading');
       $(document).on('shiny:connected', function() {
         Shiny.addCustomMessageHandler('md-ready', function(x) { document.documentElement.classList.remove('md-loading'); });
       });"))
  ),
  bslib::nav_panel("Home", value = "home", icon = shiny::icon("house"), ui_home("home")),
  bslib::nav_panel("Overview", value = "overview", ui_overview("overview")),
  bslib::nav_panel("Profile", value = "profile",
                   ui_profile("profile")),
  bslib::nav_panel("Longitudinal", value = "load", ui_load("load")),
  bslib::nav_panel("Session check", value = "session", ui_session("session")),
  bslib::nav_panel("Glossary", value = "glossary", ui_glossary("glossary")),
  bslib::nav_spacer(),
  bslib::nav_item(shiny::uiOutput("asof", inline = TRUE))
)

server <- function(input, output, session) {
  # The page is already in the browser. Get the data: about 10 seconds the first time on a
  # fresh deployment (the database is built), instant after that.
  app_data <- get_app_data()

  output$asof <- shiny::renderUI(htmltools::span(
    class = "md-asof",
    shiny::icon("calendar"),
    paste("Viewing as of", fmt_date_short(app_data$app_date)),
    htmltools::span(class = "md-muted", "\u00b7 the last day of data \u00b7 simulated")
  ))
  # The day being looked at on the session check page, shared between pages
  selected_date <- shiny::reactiveVal(app_data$app_date)
  # The chosen player, set on the home page (one player in this data)
  selected_player <- shiny::reactiveVal(app_data$profile$player_id[1])
  server_home("home", app_data, nav_session = session, selected_player = selected_player)
  server_overview("overview", app_data, nav_session = session, selected_date = selected_date)
  server_profile("profile", app_data)
  server_load("load", app_data, nav_session = session, selected_date = selected_date)
  server_session("session", app_data, selected_date = selected_date)
  server_glossary("glossary", app_data)
  session$sendCustomMessage("md-ready", TRUE)
}

shiny::shinyApp(ui, server)
