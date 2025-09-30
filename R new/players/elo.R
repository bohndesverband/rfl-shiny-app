# running elo ----
running_player_elo <- player_elo %>%
  dplyr::select(season, week, mfl_id, display_name, position, team, player_elo_post) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    game = dplyr::row_number(),
    active = ifelse(max(season) == season_before_wk_1, 1, 0)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    mfl_players %>%
      dplyr::select(player_id, season) %>%
      dplyr::rename(last_season = season),
    by = c("mfl_id" = "player_id")
  ) %>%
  dplyr::filter(last_season >= new_season_sept - 2)

## output ----
output$running_player_elo <- renderPlot({
  ggplot2::ggplot(running_player_elo, ggplot2::aes(x = game, y = player_elo_post)) +
    ggplot2::geom_hex(data = subset(running_player_elo, active == 1 & position == input$selectPosition), bins = 70) +

    ggalt::geom_xspline(data = subset(running_player_elo, mfl_id %in% c(input$selectPlayers)), ggplot2::aes(color = display_name), spline_shape = -0.5) +
    ggplot2::coord_cartesian(xlim = c(0, max(running_player_elo$game)), ylim = c(min(running_player_elo$player_elo_post) - 10, max(running_player_elo$player_elo_post)), expand = FALSE) +

    plot_elo_defaults +

    ggplot2::labs(
      title = paste("RFL", input$selectPosition, " ELO")
    )

}, height = 650)

# elo standing ----
player_elo_standing <- shiny::reactive({
  running_player_elo %>%
    dplyr::filter(position == input$selectPosition) %>%
    dplyr::group_by(mfl_id) %>%
    dplyr::arrange(game) %>%
    dplyr::mutate(
      elo_shift = ifelse(
        dplyr::row_number() == 1,
        player_elo_post - 1500,
        player_elo_post- dplyr::lag(player_elo_post)
      )
    ) %>%
    dplyr::filter(game == max(game)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(player_elo_post))
})

## output ----
output$player_elo_ranking_table <- gt::render_gt({
  player_elo_standing() %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL", input$selectPosition, "ELO Ranking"),
      subtitle = paste("Woche", player_elo_standing()$week[1], player_elo_standing()$season[1])
    ) %>%
    gt::cols_hide(c(season:mfl_id, active)) %>%
    gt::cols_move(elo_shift, player_elo_post) %>%

    gt::data_color(
      player_elo_post,
      palette = c(color_red, color_yellow, color_green, color_blue)
    ) %>%

    gt::data_color(
      game,
      palette = c(color_grey_mid, color_bg)
    ) %>%

    gt::data_color(
      elo_shift,
      palette = c(color_red, color_blue)
    ) %>%

    gt::cols_label(
      player_elo_post = paste("WK", player_elo_standing()$week[1]),
      game = "Spiele",
      elo_shift = "+/-"
    ) %>%

    gt::tab_footnote(
      footnote = "ELO Veränderung zur Vorwoche",
      locations = gt::cells_column_labels(
        columns = elo_shift
      ),
      placement = "left"
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%

    gt::cols_align(
      align = "left",
      columns = c(display_name)
    ) %>%

    gt_player() %>%

    gt::tab_options(
      ihtml.use_filters = FALSE
    ) %>%
    gt::opt_interactive(
      page_size_default = 24,
      use_compact_mode = TRUE,
      selection_mode = "multiple"
    )
})

observeEvent(input$player_elo_ranking_table, {
  selected_players <- player_elo_standing()$mfl_id[input$player_elo_ranking_table]
  updateSelectizeInput(session, "selectPlayers", selected = selected_players)
})

# elo peaks ----
player_elo_peaks <- running_player_elo %>%
  dplyr::filter(active == 1) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::filter(player_elo_post == max(player_elo_post)) %>%
  dplyr::filter(game == max(game)) %>% # falls es mehrere wochen mit selber elo gibt
  dplyr::ungroup() %>%
  dplyr::select(display_name, position, team, player_elo_post, season, week, game) %>%
  dplyr::arrange(dplyr::desc(player_elo_post))

output$player_elo_peaks_table <- gt::render_gt({
  player_elo_peaks %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste0("RFL ELO Peaks 2016-", season_before_wk_1)
    ) %>%

    gt::data_color(
      season,
      palette = c(color_bg, color_grey_light)
    ) %>%

    gt::data_color(
      player_elo_post,
      palette = c(color_red, color_yellow, color_green, color_blue)
    ) %>%

    gt::cols_label(
      season = "Saison",
      week = "Woche",
      game = "Spiele",
      player_elo_post = "ELO Peak"
    ) %>%
    gt_player()
})
