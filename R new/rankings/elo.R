source("R new/rankings/standing.R", local = TRUE)
source("R new/rankings/ranking_tables.R", local = TRUE)

running_elo <- team_elo %>%
  dplyr::select(season, week, franchise_id, franchise_name, division, division_name, conference_id, conference_name, franchise_elo_pregame, franchise_elo_postgame) %>%
  dplyr::distinct() %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(game = dplyr::row_number()) %>%
  dplyr::ungroup()

running_elo_vlines <- running_elo %>%
  dplyr::select(season, game) %>%
  dplyr::distinct() %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(game) %>%
  dplyr::mutate(vline = ifelse(dplyr::row_number() == 1 & season != 2016, 1, 0))

elo_change <- running_elo %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::filter(week == min(week) | week == max(week)) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
  #  xmax = max(game),
  #  xmin = min(game),
      elo_start = ifelse(week == min(week), franchise_elo_pregame, dplyr::lag(franchise_elo_postgame)),
      elo_end = ifelse(week == max(week), franchise_elo_postgame, dplyr::lead(franchise_elo_postgame)),
      elo_shift = elo_end - elo_start,
      elo_shift_label = sprintf("%+d", elo_shift)
  ) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup()

output$running_elo <- shiny::renderPlot({
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
}, height = 800)


output$elo_change <- shiny::renderPlot({
  ggplot2::ggplot(elo_change, ggplot2::aes(y = reorder(franchise_name, elo_shift), color = franchise_id %in% c(input$selectRflTeams) | division %in% c(input$selectRflDivisions))) +
    ggforce::geom_link(aes(x = elo_start, xend = elo_end, yend = franchise_name, linewidth = ggplot2::after_stat(index))) +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::geom_point(ggplot2::aes(x = elo_end), size = 4.7) +

    ggplot2::geom_text(ggplot2::aes(label = ifelse(elo_shift > 0, paste(elo_shift_label, franchise_name), paste(franchise_name, elo_shift_label)), x = ifelse(elo_shift > 0, elo_end + 10, elo_end - 10), hjust = ifelse(elo_shift > 0, 0, 1))) +

    ggplot2::scale_color_discrete(type = c(color_grey_mid, color_red), guide = "none") +

    ggplot2::scale_x_continuous(limits = c(min(elo_change$elo_end) - 100, max(elo_change$elo_end) + 100)) +

    plot_defaults +
    ggplot2::labs(
      title = "ELO Veränderung zum Saisonbeginn",
      subtitle = "Der Schweif symbolisiert die ELO Veränderung vom Saisonbeginn zum aktuellen Stand,\nwobei das große Ende des Schweifs den Ist-Stand darstellt.",
      x = "ELO"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )
}, height = 1200)
