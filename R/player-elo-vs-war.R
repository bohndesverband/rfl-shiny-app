
elo_shift_season <- player_elo %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::summarise(elo_shift_season = sum(elo_shift)) %>%
  dplyr::left_join(
    war %>%
      dplyr::filter(season == max(season)) %>%
      dplyr::mutate(player_id = as.character(player_id)) %>%
      dplyr::select(player_id, war, player_name, pos),
    by = c("mfl_id" = "player_id")
  ) %>%
  dplyr::filter(!is.na(war))

ggplot2::ggplot(elo_shift_season, ggplot2::aes(x = elo_shift_season, y = war, color = player_name)) +
  plotDefaults +
  geom_hline(yintercept = 0, color = var.colorRed, size = 0.5, alpha = 0.75) +
  geom_vline(xintercept = 0, color = var.colorRed, size = 0.5, alpha = 0.75) +

  ggplot2::geom_smooth(method = "lm", se = FALSE, color = var.colorAccent, alpha = 0.3, size = 0.5) +
  ggplot2::geom_point(color = var.colorAccent, alpha = 0.3) +
  ggplot2::geom_point(data = subset(elo_shift_season, mfl_id == "13130"), size = 4) +

  paletteer::scale_color_paletteer_d("awtools::ppalette") +

  labs(
    title = paste("RFL ELO Rating"),
    y = "ELO",
    x = "Game",
    color = ""
  ) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12)
  )

