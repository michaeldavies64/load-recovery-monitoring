# Glossary page: renders the glossary tables

server_glossary <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    g <- glossary_data(data)
    table <- function(df) DT::datatable(
      df, rownames = FALSE, class = "compact stripe", fillContainer = FALSE,
      options = list(pageLength = 30, dom = "ft", autoWidth = FALSE,
                     language = list(search = "", searchPlaceholder = "Search the glossary"))
    )
    output$gps <- DT::renderDT(table(g$gps))
    output$recovery <- DT::renderDT(table(g$recovery))
  })
}
