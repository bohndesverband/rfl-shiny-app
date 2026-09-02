recent_mvps <- rfl_awards %>%
  dplyr::filter(award == "MVP" & rank == 1) %>%
  dplyr::select(season, gsis_id, display_name, pos, war) %>%
  dplyr::distinct()

output$rflMVP <- shiny::renderPlot({
  ggplot2::ggplot(recent_mvps, aes(x = season, y = war, fill = pos)) +
    ggplot2::geom_col() +
    geom_text(mapping = ggplot2::aes(label = war), vjust = 1.7, size = 6, color = color_bg) +
    geom_text(aes(label = sapply(display_name, function(x) paste(strwrap(x, width = 5), collapse = "\n"))), vjust = -0.5, hjust = 0.5, size = 3, family = "base", color = color_black, lineheight = 0.8) +
    geom_hline(yintercept = 0, color = color_black, size = 0.3) +
    nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), y = 0, height = 0.1, vjust = 0) +

    ggplot2::scale_x_continuous(breaks = c(2016:new_season_sept)) +
    ggplot2::scale_y_continuous(limits = c(0, max(recent_mvps$war) + 0.5)) +
    ggplot2::scale_fill_manual(values = colors_position, guide = "none") +

    ggplot2::labs(
      title = paste0("RFL MVPs 2016-", new_season_sept),
      subtitle = "Die Spieler mit den meisten Wins Above Replacement (WAR) pro Saison",
      y = ""
    ) +

    plot_defaults +
    plot_clean +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank()
    )
})
