rookie_impact <- rfl_drafts_data %>%
  dplyr::left_join(
    player_ppg,
    by = c("season", "mfl_id")
  ) %>%
  dplyr::filter(
    !is.na(fpts)
  )

ggplot2::ggplot(rookie_impact, ggplot2::aes(x = overall, y = ppg)) +
  ggplot2::geom_hex(bins = 70) +
  ggplot2::aes(lwd = 1.2) +
  ggplot2::scale_linewidth_identity() +
  ggplot2::scale_fill_continuous(type = "gradient") +
  ggplot2::scale_fill_gradientn(colors = c("#f1f4f6", color_grey_mid), guide = "none") +

  ggplot2::geom_point(data = subset(rookie_impact, franchise_id == "0007"), ggplot2::aes(color = franchise_name, size = games)) +

  ggplot2::scale_color_discrete(type = colors) +
  plot_defaults +
  plot_clean

  ggplot2::labs(
    y = "ELO",
    x = "",
    color = ""
  )
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.9),
    axis.text.x = ggplot2::element_blank()
  )




ggplot2::ggplot(running_elo, ggplot2::aes(x = game, y = franchise_elo_postgame)) +
  ggplot2::geom_hex(bins = 70) +

  ggplot2::geom_vline(data = subset(running_elo_vlines, vline == 1), ggplot2::aes(xintercept = game), color = color_grey_light, linewidth = 0.5) +
  ggplot2::geom_text(data = subset(running_elo_vlines, vline == 1), ggplot2::aes(label = season, x = game), nudge_x = 3, y = 1300, color = color_grey_dark, size = 4) +

  ggalt::geom_xspline(data = subset(running_elo, franchise_id %in% c(input$selectRflTeams) | division %in% c(input$selectRflDivisions)), ggplot2::aes(color = franchise_name), spline_shape = -0.5) +
  ggplot2::aes(lwd = 1.2) +
  ggplot2::scale_linewidth_identity() +

  ggplot2::geom_point(data = subset(running_elo, (franchise_id %in% c(input$selectRflTeams) | division %in% c(input$selectRflDivisions)) & (game == min(game) | game == max(game))), ggplot2::aes(color = franchise_name), size = 5) +

  plot_elo_defaults +

  ggplot2::labs(
    title = paste("RFL ELO Rating"),
    #subtitle = paste("Total Avg Opp Win % - maximale Total Avg Opp Win % der Liga.\nJe niedriger der Wert, desto leichter ist der SOS im Vergleich zum Rest der Liga."),
  )
