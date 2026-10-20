# Entry point for deployment (Posit Connect Cloud looks for app.R at the project root).
# The app itself lives in app/: see app/app.R for its structure.
# Locally, either source this file or run shiny::runApp("app").
shiny::shinyAppDir(here::here("app"))
