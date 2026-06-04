draftorder_history <- reactive({
  rfl_standing_data %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(
      div_id == selected_team()$division
    ) %>%
    dplyr::arrange(week) %>%
    dplyr::select(franchise_id, div_id, week, pick, league_rank) %>%
    tidyr::gather(key, value, c(pick, league_rank)) %>%
    dplyr::arrange(key) %>%
    dplyr::group_by(key) %>%
    dplyr::mutate(key = ifelse(key == "pick", "Draft Pick", "Tabellenplatz"))
})

output$pickHistory <- shiny::renderPlot({
  ggplot2::ggplot(subset(draftorder_history(), franchise_id == input$selectRflTeam), ggplot2::aes(x = week, y = value, color = key)) +
    ggplot2::geom_hline(yintercept = 1, color = color_grey_light) +
    ggplot2::geom_hline(yintercept = 12, color = color_grey_light) +
    ggplot2::geom_hline(yintercept = 24, color = color_grey_light) +
    ggplot2::geom_hline(yintercept = 36, color = color_grey_light) +

    #ggalt::geom_xspline(spline_shape = -0.2) +
    #ggplot2::geom_line(data = subset(draftorder_history(), key == "Tabellenplatz"), linewidth = 1, color = "#ccc") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(data = subset(draftorder_history(), franchise_id == input$selectRflTeam & week == max(week)), size = 5) +
    ggrepel::geom_label_repel(data = subset(draftorder_history(), franchise_id == input$selectRflTeam & week == max(week)), ggplot2::aes(label = value), point.padding = 10, label.size = 0.5, show.legend = FALSE) +

    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_x_continuous(limits = c(1, 13), labels = c(1:13), breaks = c(1:13)) +
    ggplot2::scale_y_reverse(limits = c(36, 1), labels = c(36, 24, 12, 1), breaks = c(36, 24, 12, 1)) +

    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste(selected_team()$franchise_name, "Saisonverlauf"),
      x = "Woche",
      y = "",
      color = ""
    ) +
    plot_defaults +
    plot_clean
}, width = 800, height = 800)
