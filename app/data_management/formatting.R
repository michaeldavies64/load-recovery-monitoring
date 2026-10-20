# Formatting helpers shared by every page

fmt_value <- function(x, decimals = 0, unit = NULL) {
  out <- formatC(x, format = "f", digits = decimals, big.mark = ",")
  if (!is.null(unit) && !unit %in% c("count", "proportion", "text", "date", "days")) out <- paste(out, unit)
  out
}

fmt_date_short <- function(d) paste(as.integer(format(d, "%d")), format(d, "%b %Y"))

age_on <- function(dob, on) {
  as.integer(format(on, "%Y")) - as.integer(format(dob, "%Y")) -
    (format(on, "%m%d") < format(dob, "%m%d"))
}

is_flag <- function(status) status %in% c("Above range", "Below range")
is_assessed <- function(status) status %in% c("In range", "Above range", "Below range")

# JavaScript that sends a value to a Shiny input when an element is clicked
js_set_input <- function(input_id, value) {
  sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, value)
}
