# filter data ----
rfl_drafts_rookies_filtered <- shiny::reactive({
  rfl_drafts_rookies %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2] &
        pos %in% c(input$selectPositions)
    )
})

output$draft_basics_average <- plotly::renderPlotly({
  plot <- rfl_drafts_rookies_filtered() %>%
    ggplot2::ggplot(ggplot2::aes(x = overall, y = avg_pick)) +
      geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', color = color_grey_mid) +
      ggplot2::geom_point(color = color_grey_light, size = 2) +



    #ggplot2::facet_wrap(~factor(pos, levels = positions_full), ncol = 3) +

    #ggplot2::geom_segment(ggplot2::aes(x = first_pick, xend = third_pick, yend = pos_rank), color = color_grey_mid) +
    #ggplot2::geom_point(ggplot2::aes(x = first_pick), color = color_grey_mid, size = 2) +
    #ggplot2::geom_point(ggplot2::aes(x = second_pick), color = color_grey_mid, size = 2) +
    #ggplot2::geom_point(ggplot2::aes(x = third_pick), color = color_grey_mid, size = 2) +

    ggplot2::geom_point(data = subset(rfl_drafts_rookies_filtered(), franchise_id == input$selectRflTeam),
                        ggplot2::aes(
                          x = overall,
                          color = franchise_name,
                          text = paste0(season, " Pick ", overall, " ", pos, " ", player_name, "\nMin: ", min_pick, "\nMax: ", max_pick, "\nAvg: ", avg_pick)
                        ),
                        size = 4) +
    ggplot2::scale_color_discrete(type = colors) +

    #ggplot2::scale_x_continuous(
    #  limits = c(min(rfl_drafts_rookies_filtered()$overall), max(rfl_drafts_rookies_filtered()$overall)),
    #  breaks = seq(1, 260, 36),
    #) +
    #ggplot2::scale_y_reverse(
    #  breaks = seq(min(rfl_drafts_rookies_filtered()$pos_rank), max(rfl_drafts_rookies_filtered()$pos_rank), 10),
    #) +

    ggplot2::labs(
      title = "Durchschnittliche RFL Draftpicks",
      x = "Overall",
      y = "Avg",
      color = "RFL Team"
    ) +
    plot_defaults

  plotly::ggplotly(plot, tooltip = "text")
})
