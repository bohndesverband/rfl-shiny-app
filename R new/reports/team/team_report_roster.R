# depth chart ----
## load data ----
rfl_depth_chart_data <- read_data_table("rfl_depth_chart_data")

## filter data ----
depth_chart_team <- shiny::reactive({
  depth_chart_team <- rfl_depth_chart_data %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeam) %>%
    #dplyr::filter(season == 2026 & franchise_id == "0032") %>%
    dplyr::filter(!is.na(coord_v) | !is.na(coord_h))
})

## output ----
output$team_depth_chart <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(depth_chart_team(), ggplot2::aes(x = coord_h, y = coord_v)) +
    ggplot2::geom_hline(yintercept = 0, color = color_yellow, size = 1) + # LOS
    ggplot2::scale_x_continuous(limits = c(1, 15), expand = c(0, 0), breaks = seq(1, 15, by = 1)) +
    ggplot2::scale_y_continuous(limits = c(-6, 6)) +

    ggplot2::geom_label(ggplot2::aes(label = depth_chart), fill = color_grey_light, hjust = 0.5, vjust = -0.5, linewidth = 0, size = 3.5, label.padding = ggplot2::unit(2, "mm"), label.r = ggplot2::unit(0, "mm"), na.rm = TRUE) +
    ggiraph::geom_label_interactive(ggplot2::aes(label = display_name, fill = pos_grouped, tooltip = pos_players), hjust = 0.5, linewidth = 0, size = 4.5, label.padding = ggplot2::unit(2, "mm"), label.r = ggplot2::unit(0, "mm"), na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = colors_positions_grouped, guide = "none") +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_label(ggplot2::aes(label = paste0("#", war_rank_league, " (", war, " WAR)"), fill = war_rank_league), hjust = 0.5, vjust = 1.52, linewidth = 0, size = 3.5, label.padding = ggplot2::unit(2, "mm"), label.r = ggplot2::unit(0, "mm"), na.rm = TRUE) +
    ggplot2::scale_fill_gradientn(colors = c(color_blue, color_green, color_yellow, color_orange, color_red), limits = c(1, 12), na.value = color_red, guide = "none") +

    plot_defaults +
    ggplot2::theme(
      #panel.background = ggplot2::element_rect(fill = color_grey_dark),
      axis.text = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste(selected_team_name(), "Depth Chart", input$selectYear),
      subtitle = "Angezeigt werden die Top-Spieler des Rosters auf ihren Positionen nach Wins above Replacement (WAR).",
      x = "",
      y = ""
    )

  girafe_default_output(plot)
})
