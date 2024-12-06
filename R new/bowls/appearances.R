# data ----
data <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/refs/heads/main/reports/season-review/output/plots/playoff-appearances/data.csv")
super_bowl <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/refs/heads/main/reports/season-review/output/plots/playoff-appearances/super_bowl.csv")

### helper ----
# render geom_curve with dynamic curvature

# function for a single geom_curve()
curve_geom <- function(franchise_name, xstart, xend, curvature, color) {
  ggplot2::geom_curve(
    # since I use a facet plot, I need the geom only visible in a specific panel
    data = data.frame(franchise_name = {{franchise_name}}),
    aes(x = {{xstart}}, xend = {{xend}}, color = {{color}}),
    y = 0.1, curvature = {{curvature}}, yend = 0.1, linewidth = 0.3)
}

# function to apply on the ggplot
add_curves <- function(franchise_name) {
  franchise_info <- super_bowl %>%
    dplyr::filter(franchise_name == {{franchise_name}})

  # use helper function to create a geom_curve() for each entry in the franchise_info df
  mapply(
    curve_geom,
    franchise_name = {{franchise_name}},
    xstart = franchise_info$season,
    xend = franchise_info$next_appearance,
    curvature = franchise_info$curvature,
    color = franchise_info$result_color
  )
}

### base grid ----
right <- ggplot2::ggplot(super_bowl, aes(x = season, color = result_color, y = 0.1)) +
  ggplot2::facet_wrap(~franchise_name, scales = "free_y", ncol = 2, strip.position = "left", labeller = ggplot2::label_wrap_gen(width = 15))

### add curves ----
for (franchise in unique(super_bowl$franchise_name)) {
  right <- right +
    add_curves(franchise)
}

right +
  ggplot2::geom_point(aes(size = po_finish, color = result_color))

    #ggplot2::scale_color_manual(values = colors) +
    #ggplot2::scale_size_continuous(range = c(1.5, 0.5), guide = "none") +
    #ggplot2::scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0)) +

    #ggplot2::labs(
    #  title = "Superbowl Finishes",
    #  subtitle = "All teams with at least two appearances in the RFL Super Bowl",
    #)

    #ggplot2::theme(
    #  plot.margin = ggplot2::margin(l = 5, unit = "mm"),
    #  plot.title = ggplot2::element_text(family = "accent", hjust = 0.5, size = 42, margin = ggplot2::margin(c(0, 0, 0, 0))),
    #  plot.subtitle = ggplot2::element_text(family = "base", hjust = 0.5, size = 20, margin = ggplot2::margin(t = 2, b = 4, unit = "mm")),
    #  strip.text.y.left = ggplot2::element_text(angle = 90, family = "accent", size = 14, lineheight = 0.35),
    #  panel.spacing.x = ggplot2::unit(8, "mm"),
    #  panel.spacing.y = ggplot2::unit(5, "mm"),
    #)


## left ----
### other bowls ----
ggplot(data = subset(data, !(franchise_name %in% unique(super_bowl$franchise_name))), aes(x = season, y = franchise_name, size = po_finish, color = factor(bowl, levels = c("SB", "PB", "TB")))) +
  ggplot2::geom_rect(xmin = 2015, xmax = 2016.8, ymin = 0, ymax = 36, fill = color_bg, color = NA) +

  ggplot2::geom_curve(data = subset(data, season == 2021 & bowl == "PB" & po_finish == min(po_finish)), aes(xend = season - 0.5, y = 16.5, yend = 12), curvature = 0.5, size = 0.3) +
  ggplot2::geom_point(position = ggplot2::position_nudge(y = 0.5)) +
  ggplot2::geom_text(
    data = subset(data, season == 2021 & bowl == "PB" & po_finish == min(po_finish)),
    aes(x = season - 0.5), y = 10,
    label = "Size of points\nstands for\nplacement in Bowl",
    size = 6, lineheight = 0.4, angle = 10, show.legend = FALSE) +

  ggplot2::scale_color_manual(labels = c("Super Bowl", "Pro Bowl", "Toilet Bowl"), values = c("#ff9f43", "#2e86de", "#ee5253")) +
  ggplot2::scale_size_continuous(range = c(2, 0.5), guide = "none") +
  ggplot2::scale_x_continuous(limits = c(2015.8, 2022), expand = c(0, 0.05), breaks = 2017:2022)

  #ggplot2::labs(
  #  caption = caption
  #)

  #plot_default +
  #ggplot2::theme(
  #  axis.text.y = ggplot2::element_text(color = color_light, size = 14, hjust = 1, vjust = -1.3, margin = ggplot2::margin(r = -1.9, unit = "cm"), family = "accent"),
  #  panel.grid.major.y = ggplot2::element_line(color = "#425B78", linewidth = 0.1),
  #  plot.caption = ggtext::element_markdown(
  #    size = 30,
  #    vjust = 0,
  #    hjust = 0,
  #    color = color_light,
  #    family = "base",
  #    margin = ggplot2::margin(t = 10, unit = "mm")
  #  ),
  #  plot.caption.position = "plot"
  #)
