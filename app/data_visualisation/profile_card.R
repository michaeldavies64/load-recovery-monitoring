# Player profile card: headshot and the invented profile details

profile_card <- function(profile, app_date) {
  row <- function(label, value) htmltools::div(class = "md-kv", htmltools::span(class = "md-muted", label), htmltools::span(value))
  bslib::card(
    class = "md-profile-card",
    bslib::card_body(
      htmltools::tags$img(src = profile$headshot_file, class = "md-headshot-lg", alt = "Illustrated headshot"),
      htmltools::h1(class = "md-banner-title text-center", profile$display_name),
      htmltools::p(class = "md-muted text-center", profile$position),
      row("Age", age_on(profile$date_of_birth, app_date)),
      row("Date of birth", fmt_date_short(profile$date_of_birth)),
      row("Height", paste(profile$height_cm, "cm")),
      row("Weight", paste(profile$weight_kg, "kg")),
      row("Body fat", paste0(profile$body_fat_pct, "%")),
      htmltools::p(class = "md-muted small mt-3 mb-0", "Invented profile for a simulated player.")
    )
  )
}
