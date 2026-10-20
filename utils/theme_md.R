# Slate and Signal: ggplot2 theme for Michael Davies
# Usage:
#   source("R/theme_md.R")
#   ggplot(df, aes(x, y, colour = player)) + geom_line() +
#     md_highlight("Player 7") + theme_md()
#
# Fonts: install Inter from Google Fonts. With {systemfonts}/{ragg} installed,
# ggplot picks it up automatically (use ragg::agg_png or device = ragg::agg_png).

md_colours <- list(
  # brand and text
  surface        = "#FFFFFF",
  surface_raised = "#F5F7F9",
  line           = "#D9DEE4",
  gridline       = "#E8EBEF",
  baseline       = "#B8C0C9",
  slate          = "#1F2A37",
  ink            = "#1F2A37",
  ink_secondary  = "#4E5866",
  ink_muted      = "#636D7A",
  signal         = "#009999",
  signal_text    = "#007A7A",
  signal_soft    = "#E0F4F4",
  context        = "#B8C0C9",
  # status (always pair with an icon or word)
  good           = "#2E9E5B",
  warning        = "#B37A00",
  alert          = "#D64545"
)

md_categorical <- c("#009999", "#4C5FD5", "#D8702E", "#B8478A", "#8A5A3B", "#3F8FCB")
md_sequential  <- c("#E0F4F4", "#A8E0E0", "#5FC4C4", "#009999", "#007A7A", "#004F4F")
md_diverging   <- c("#A8531F", "#D8702E", "#F0B48A", "#E5E8EC", "#8ED3D3", "#009999", "#006B6B")

theme_md <- function(base_size = 12, base_family = "Inter") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = md_colours$surface, colour = NA),
      panel.background   = ggplot2::element_rect(fill = md_colours$surface, colour = NA),
      panel.grid.major.y = ggplot2::element_line(colour = md_colours$gridline, linewidth = 0.4),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_line(colour = md_colours$baseline, linewidth = 0.4),
      axis.ticks         = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(colour = md_colours$ink_muted, size = base_size * 0.8),
      axis.title         = ggplot2::element_text(colour = md_colours$ink_secondary, size = base_size * 0.85),
      plot.title         = ggplot2::element_text(colour = md_colours$ink, face = "bold", size = base_size * 1.4,
                                                 margin = ggplot2::margin(b = 4)),
      plot.subtitle      = ggplot2::element_text(colour = md_colours$ink_secondary, size = base_size * 0.95,
                                                 margin = ggplot2::margin(b = 12)),
      plot.caption       = ggplot2::element_text(colour = md_colours$ink_muted, size = base_size * 0.75,
                                                 hjust = 0, margin = ggplot2::margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position    = "top",
      legend.justification = "left",
      legend.title       = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(colour = md_colours$ink_secondary, size = base_size * 0.85),
      strip.text         = ggplot2::element_text(colour = md_colours$ink, face = "bold", hjust = 0),
      plot.margin        = ggplot2::margin(16, 16, 12, 16)
    )
}

scale_colour_md <- function(...) ggplot2::scale_colour_manual(values = md_categorical, ...)
scale_fill_md   <- function(...) ggplot2::scale_fill_manual(values = md_categorical, ...)
scale_fill_md_seq <- function(...) ggplot2::scale_fill_gradientn(colours = md_sequential, ...)
# Diverging fill centred on a baseline (teal above, orange below, grey at the midpoint).
scale_fill_md_div <- function(midpoint = 0, ...) {
  ggplot2::scale_fill_gradientn(
    colours = md_diverging,
    rescaler = function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to = to, from = from, mid = midpoint)
    },
    ...
  )
}

# Emphasis: the named level(s) in signal, every other level in context grey.
# Tip: order the data so the focus is drawn last, e.g.
#   dplyr::arrange(df, player %in% focus)
md_highlight <- function(focus, aesthetics = c("colour", "fill")) {
  ggplot2::scale_discrete_manual(
    aesthetics,
    values   = stats::setNames(rep(md_colours$signal, length(focus)), focus),
    na.value = md_colours$context,
    guide    = "none"
  )
}
