# Rearview's own colours (rearview-driver.vercel.app, light theme), so every post's charts read like the site.
library(ggplot2)

rv <- c(
  blue  = "#2a78d6",  # the figure a chart is about
  amber = "#a0660f",  # slower, or longer, as on the site
  grey  = "#d3d1cb",  # everything else, kept quiet
  ink   = "#0b0b0b",
  muted = "#6e6d68"
)

theme_set(theme_minimal())
theme_update(
  plot.background  = element_rect(fill = "#fcfcfb", colour = NA),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(colour = "#eceae5"),
  axis.text        = element_text(colour = rv[["muted"]]),
  axis.title       = element_text(colour = rv[["muted"]])
)
