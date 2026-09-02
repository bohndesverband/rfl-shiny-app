# rfl_postseason_data

ggplot(data = rfl_postseason_data, aes(x = season, y = franchise_name, size = po_finish, color = factor(bowl, levels = c("SB", "PB", "TB")))) +
  ggplot2::geom_point(position = ggplot2::position_nudge(y = 0.5)) +

  ggplot2::scale_color_manual(labels = c("Super Bowl", "Pro Bowl", "Toilet Bowl"), values = c(color_orange, color_blue, color_red)) +
  ggplot2::scale_size_continuous(range = c(4, 1), guide = "none") +
  ggplot2::scale_x_continuous(limits = c(2016.8, max(rfl_postseason_data$season)), expand = c(0, 0.05), breaks = 2017:max(rfl_postseason_data$season)) +
  plot_defaults +
  ggplot2::labs(
    title = "RFL Postseason Appearances",
    subtitle = "Size indicates how far a team advanced in the postseason. Color indicates which bowl game a team appeared in.",
    x = "",
    y = "",
    color = ""
  )

superbowl_data <- rfl_postseason_data %>%
  #filter(franchise_id == "0007" & bowl == "PB") %>%
  dplyr::group_by(franchise_id, season, bowl) %>%
  dplyr::arrange(dplyr::desc(week)) %>%
  dplyr::slice(1) %>%
  dplyr::group_by(franchise_id, bowl) %>%
  dplyr::arrange(season) %>%
  dplyr::mutate(
    total_appearances = n(),
    next_appearance = dplyr::lead(season),
    distance = next_appearance - season,
    curvature = (total_appearances - po_finish) / max(abs(total_appearances - po_finish)),
    result_color = dplyr::case_when(
      po_finish == 1 ~ "1st",
      po_finish == 2 ~ "2nd",
      po_finish == 3 ~ "3rd",
      TRUE ~ "4th+"
    )
  ) %>%
  filter(bowl == "SB" & total_appearances >= 2)

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
  franchise_info <- superbowl_data %>%
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

superbowl_chart <- ggplot2::ggplot(rfl_postseason_data, aes(x = season, color = result_color, y = 0.1)) +
  ggplot2::facet_wrap(~franchise_name, scales = "free_y", ncol = 2, strip.position = "left", labeller = ggplot2::label_wrap_gen(width = 15))

### add curves ----
for (franchise in unique(superbowl_data$franchise_name)) {
  superbowl_chart <- superbowl_chart +
    add_curves(franchise)
}

superbowl_chart
  ggplot2::geom_point(aes(size = po_finish, color = result_color))

    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_size_continuous(range = c(1.5, 0.5), guide = "none") +
    ggplot2::scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0))

    ggplot2::labs(
      title = "Superbowl Finishes",
      subtitle = "All teams with at least two appearances in the RFL Super Bowl",
    ) +

    plot_default +
    plot_reset +
    ggplot2::theme(
      plot.margin = ggplot2::margin(l = 5, unit = "mm"),
      plot.title = ggplot2::element_text(color = color_accent, family = "accent", hjust = 0.5, size = 42, margin = ggplot2::margin(c(0, 0, 0, 0))),
      plot.subtitle = ggplot2::element_text(color = color_light, family = "base", hjust = 0.5, size = 20, margin = ggplot2::margin(t = 2, b = 4, unit = "mm")),
      strip.text.y.left = ggplot2::element_text(angle = 90, color = color_light, family = "accent", size = 14, lineheight = 0.35),
      panel.spacing.x = ggplot2::unit(8, "mm"),
      panel.spacing.y = ggplot2::unit(5, "mm"),
    )
