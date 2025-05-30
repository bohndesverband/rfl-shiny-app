# filter data ----
rfl_drafts_rookies_filtered <- shiny::reactive({
  rfl_drafts_rookies %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2] &
        pos %in% c(input$selectPositions)
    )
})

#output$draft_basics_average <- plotly::renderPlotly({
#  plot <- rfl_drafts_rookies_filtered() %>%
#    ggplot2::ggplot(ggplot2::aes(x = overall, y = avg_pick)) +
#      geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', color = color_grey_mid) +
#      ggplot2::geom_point(color = color_grey_light, size = 2) +



    #ggplot2::facet_wrap(~factor(pos, levels = positions_full), ncol = 3) +

    #ggplot2::geom_segment(ggplot2::aes(x = first_pick, xend = third_pick, yend = pos_rank), color = color_grey_mid) +
    #ggplot2::geom_point(ggplot2::aes(x = first_pick), color = color_grey_mid, size = 2) +
    #ggplot2::geom_point(ggplot2::aes(x = second_pick), color = color_grey_mid, size = 2) +
    #ggplot2::geom_point(ggplot2::aes(x = third_pick), color = color_grey_mid, size = 2) +

#    ggplot2::geom_point(data = subset(rfl_drafts_rookies_filtered(), franchise_id == input$selectRflTeam),
#                        ggplot2::aes(
#                          x = overall,
#                          color = franchise_name,
#                          text = paste0(season, " Pick ", overall, " ", pos, " ", player_name, "\nMin: ", min_pick, "\nMax: ", max_pick, "\nAvg: ", avg_pick)
#                        ),
#                        size = 4) +
#    ggplot2::scale_color_discrete(type = colors) +

    #ggplot2::scale_x_continuous(
    #  limits = c(min(rfl_drafts_rookies_filtered()$overall), max(rfl_drafts_rookies_filtered()$overall)),
    #  breaks = seq(1, 260, 36),
    #) +
    #ggplot2::scale_y_reverse(
    #  breaks = seq(min(rfl_drafts_rookies_filtered()$pos_rank), max(rfl_drafts_rookies_filtered()$pos_rank), 10),
    #) +

#    ggplot2::labs(
#      title = "Durchschnittliche RFL Draftpicks",
#      x = "Overall",
#      y = "Avg",
#      color = "RFL Team"
#    ) +
#    plot_defaults

#  plotly::ggplotly(plot, tooltip = "text")
#})

# nach nfl rounds ----
output$rfl_picks_over_nfl_rounds <- shiny::renderPlot({
  rfl_drafts_rookies %>%
    dplyr::rename(rfl_round = round) %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2], pos != "PK") %>%
    dplyr::left_join(
      nfl_drafts_data %>%
        dplyr::filter(!is.na(gsis_id)),
      by = "gsis_id"
    ) %>%
    # anpassung draftrounds
    dplyr::mutate(
      nfl_round = dplyr::case_when(
        is.na(gsis_id) & player_name == "Carlos Henderson" ~ 3,
        is.na(gsis_id) & player_name == "Logan Hall" ~ 2,
        is.na(gsis_id) & player_name == "Jartavius Martin" ~ 2,
        is.na(nfl_round) ~ 8,
        TRUE ~ nfl_round
      )
    ) %>%
    dplyr::group_by(pos, rfl_round, nfl_round) %>%
    dplyr::summarise(
      count = n(),
      .groups = "drop"
    ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = nfl_round + 0.5, y = rfl_round + 0.5, fill = factor(pos, levels = positions_full))) +
    ggplot2::facet_wrap(~factor(pos, levels = positions_full), ncol = 3) +
    ggplot2::geom_tile(mapping = ggplot2::aes(alpha = count)) +

    ggplot2::scale_x_continuous(limits = c(1, 9), breaks = seq(1, 8, by = 1), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(8, 1), breaks = seq(7, 1, by = -1), expand = c(0, 0)) +
    ggplot2::scale_alpha(range = c(0.1, 1), guide = "none") +
    ggplot2::scale_fill_manual(values = colors_position, guide = "none") +
    plot_defaults +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(hjust = -3),
      axis.text.y = ggplot2::element_text(vjust = 2),
    ) +
    ggplot2::labs(
      title = "RFL Draftpicks nach NFL Draft-Runde",
      subtitle = "In welcher Runde wurden die im RFL Draft gepickten Spieler im NFL Draft gewählt?",
      x = "NFL Draft Runde",
      y = "RFL Draft Runde"
    )
}, height = 900)
