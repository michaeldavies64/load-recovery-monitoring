# Overview page (landing page): a banner, then three tiles that each summarise a page
# and link to it. The content is rendered by server_overview().

ui_overview <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "md-overview",
    shiny::uiOutput(ns("banner")),
    bslib::layout_columns(
      col_widths = c(4, 4, 4), class = "md-overview-tiles",
      shiny::uiOutput(ns("tile_status"), class = "md-tile-wrap"),
      shiny::uiOutput(ns("tile_session"), class = "md-tile-wrap"),
      shiny::uiOutput(ns("tile_28d"), class = "md-tile-wrap")
    )
  )
}
