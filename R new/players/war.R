# war ----
player_war <- shiny::reactive({
  war_data %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      dplyr::across(c(player_name, pos), last),
      dplyr::across(c(points, war, games_played, games_missed), sum),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      mfl_players %>%
        dplyr::select(player_id, team),
      by = "player_id"
    ) %>%
    dplyr::filter(!is.na(player_name)) %>%
    dplyr::mutate(
      war = round(war, 2),
      war_pct = war / max(war),
      display_name = nflreadr::clean_player_names(player_name)
    ) %>%
    dplyr::group_by(pos) %>%
    dplyr::mutate(war_pct_pos = war / max(war)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(position = pos) %>%
    dplyr::select(player_id, display_name, position, team, dplyr::starts_with("war"), dplyr::starts_with("games_"))
})


output$player_war <- gt::render_gt({
  player_war() %>%
    dplyr::arrange(dplyr::desc(war)) %>%
    gt::gt() %>%

    gt::tab_header(
      title = paste("Spieler Wins above Replacement"),
      subtitle = paste(input$selectYears[1], input$selectYears[2], sep = "-")
    ) %>%

    gt::tab_spanner(
      label = "WAR",
      columns = dplyr::starts_with("war")
    ) %>%

    gt::tab_spanner(
      label = "Spiele",
      columns = dplyr::starts_with("games")
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%

    gt::cols_align(
      align = "left",
      columns = c(display_name)
    ) %>%

    gt::fmt_percent(
      dplyr::starts_with("war_pct"),
      decimals = 0
    ) %>%

    gt::data_color(
      c(war_pct:games_missed),
      palette = c(color_bg, color_grey_mid)
    ) %>%

    gt::data_color(
      war,
      palette = c(color_red, color_yellow, color_green, color_blue)
    ) %>%

    gt::cols_hide(player_id) %>%

    gt::cols_label(
      war = "Tot",
      war_pct = "Pct",
      war_pct_pos = "Pos Pct",
      games_played = "Gespielt",
      games_missed = "Verpasst"
    ) %>%

    gtDefaults() %>%

    gt_player()
})

# best players per position ----
best_position_players <- shiny::reactive({
  player_war() %>%
    dplyr::group_by(position) %>%
    dplyr::arrange(dplyr::desc(war)) %>%
    dplyr::filter(dplyr::row_number() <= 3) %>%
    dplyr::mutate(
      pos_rank = dplyr::row_number(),
      col_height = war / max(war)
    ) %>%
    dplyr::left_join(
      nflreadr::load_ff_playerids() %>%
        dplyr::select(mfl_id, gsis_id),
      by = c("player_id" = "mfl_id")
    )
})

output$war_best_players <- shiny::renderPlot({
  ggplot2::ggplot(best_position_players(), aes(x = factor(pos_rank, levels = c(2,1,3)), y = col_height)) +
    ggplot2::facet_wrap(~factor(position, c("QB", "RB", "WR", "TE", "PK", "DL", "LB", "DB")), ncol = 1, strip.position = "bottom") +
    ggplot2::geom_col(aes(fill = position)) +

    ggplot2::geom_text(mapping = ggplot2::aes(label = war), vjust = 2, size = 4, color = color_bg, fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(label = sapply(display_name, function(x) paste(strwrap(x, width = 14), collapse = "\n"))), nudge_y = 0.15, vjust = 0, hjust = 0.5, size = 4, color = color_grey_mid, lineheight = 0.8) +
    ggplot2::scale_fill_manual(values = colors_position, guide = "none") +

    ggplot2::coord_cartesian(ylim = c(0.1, 1.5)) +

    plot_defaults +
    plot_clean +

    ggplot2::labs(
      title = "Die wertvollsten Spieler",
      x = "",
      y = ""
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
    )

}, height = 1000)
