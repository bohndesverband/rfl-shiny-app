plotDefaults <- theme(
  plot.margin = margin(15, 15, 15, 15),
  text = element_text(color = var.colorBlue, family = var.fontText),

  plot.background = element_rect(fill = var.colorBlue),
  plot.title = element_text(size = 18, hjust = 0.5, family = var.fontHeadline, color = var.colorAccent, lineheight = 0.8),
  plot.subtitle = element_text(size = 14, hjust = 0.5, color = var.colorAccent, family = var.fontTextBold),

  axis.title = element_text(size = 10, color = var.colorAccent, family = var.fontText),
  axis.title.x = element_text(vjust = -1),
  axis.title.y = element_text(vjust = 2.5),
  axis.text = element_text(size = 9, color = var.colorAccent, family = var.fontTextBold),
  axis.line = element_line(color = var.colorAccent, size = 0.5),
  axis.ticks = element_line(color = var.colorAccent, size = 0.5),

  strip.background = element_rect(fill = var.colorAccent),
  strip.text = element_text(size = 10, color = var.colorBlue, family = var.fontTextBold),

  legend.background = element_blank(),
  legend.title = element_text(size = 10, color = var.colorAccent, family = var.fontTextBold),
  legend.key = element_blank(),
  legend.key.size = unit(4, "pt"),
  legend.text = element_text(size = 10, color = var.colorAccent),

  panel.background = element_rect(fill = "#002459"),
  panel.grid.major = element_line(color = var.colorBlue, size = 0.35),
  panel.grid.minor = element_line(color = var.colorBlue, size = 0.25)
)
