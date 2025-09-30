nfl_player_stats <- nflreadr::load_player_stats(summary_level = "reg") %>%
  dplyr::mutate(
    fpts_big_plays = dplyr::case_when(
      position %in% c("QB", "RB", "WR", "TE") ~ (passing_tds * 4) + ((rushing_tds + receiving_tds) * 6),
      position_group %in% c("DL", "LB", "DB") ~ ((def_sacks + def_interceptions) * 4) + ((special_teams_tds + def_tds) * 6)
    )
  ) %>%
  dplyr::select(player_id, fpts_big_plays)

big_play_points_data <- rfl_fantasy_finishes %>%
  dplyr::filter(season == new_season_sept) %>%
  dplyr::select(season:pos_rank) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::left_join(
    nfl_player_stats,
    by = c("gsis_id" = "player_id")
  ) %>%
  dplyr::select(-gsis_id) %>%
  dplyr::mutate(
    fpts_big_plays = ifelse(is.na(fpts_big_plays), 0, fpts_big_plays),
    fpts_big_plays_pct = fpts_big_plays / points,
    subline = paste(pos, team, sep = ", ")
  ) %>%
  dplyr::filter(!is.na(fpts_big_plays_pct)) %>%
  dplyr::arrange(player_name)


output$bigPlayPoints <- gt::render_gt({
  big_play_points_data %>%
    dplyr::arrange(desc(fpts_big_plays_pct)) %>%
    dplyr::select(pos_rank, player_id, pos:points, fpts_big_plays:subline) %>%

    dplyr::filter(player_id %in% input$selectPlayers) %>%

    dplyr::arrange(dplyr::desc(points)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Big Play Fantasy Punkte",
      subtitle = "Die Tabelle zeigt, wie sehr Spieler von Big Plays abhängig sind.\nBig Plays in der Offense sind Touchdowns, in der Defense Sacks und Interceptions."
    ) %>%
    gt::cols_hide(c(pos, team, player_id)) %>%
    gtExtras::gt_merge_stack(
      player_name,
      subline,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%
    gtExtras::gt_plt_bullet(
      points,
      target = fpts_big_plays,
      width = 100,
      palette = c(color_grey_light, color_green)
    ) %>%
    gt::tab_spanner(
      label = "Big Plays",
      columns = c(points, fpts_big_plays_pct)
    ) %>%
    gt::fmt_percent(
      columns = fpts_big_plays_pct,
      decimals = 0
    ) %>%
    gt::data_color(
      c(fpts_big_plays_pct, pos_rank),
      palette = c(color_blue, color_red)
    ) %>%
    gt::cols_label(
      player_name = "Spieler",
      points = "FPts",
      pos_rank = "Pos Rank",
      fpts_big_plays_pct = "Pct",
    ) %>%
    gtDefaults()
})

selection_data <- big_play_points_data %>%
  dplyr::select(player_id, player_name, pos, pos_rank) %>%
  dplyr::left_join(
    rfl_roster_data %>%
      dplyr::filter(season == max(season)) %>%
      dplyr::filter(week == max(week)),
    by = "player_id"
  )

selection_r <- shiny::reactive({
  selection_data %>%
    dplyr::filter(
      if (isTruthy(input$selectRflTeam))
        franchise_id == input$selectRflTeam
      else TRUE
    ) %>%
    dplyr::filter(
      if (isTruthy(input$selectPositions))
        pos %in% input$selectPositions
      else TRUE
    )
})

# Wenn Team oder Position geändert werden -> Auswahl aktualisieren
shiny::observeEvent(list(input$selectRflTeam, input$selectPositions, input$selectPositionRank), {
  req(active_tab() == "#section-big-play-punkte")

  if(input$selectRflTeam == "") {
    selected_positions <- big_play_points_data %>%
      dplyr::filter(
        if (isTruthy(input$selectPositions))
          pos %in% input$selectPositions
        else TRUE
      )

    selected_ids <- selection_r() %>%
      dplyr::select(-franchise_id) %>%
      dplyr::distinct() %>%
      dplyr::filter(pos_rank <= input$selectPositionRank) %>%
      dplyr::pull(player_id)
  } else {
    selected_positions <- big_play_points_data

    selected_ids <- selection_r() %>%
      dplyr::pull(player_id)
  }

  shinyWidgets::updatePickerInput(
    session,
    "selectPlayers",
    choices = split(
      setNames(selected_positions$player_id,
               selected_positions$player_name),
      selected_positions$pos
    ),
    selected = selected_ids,
    options = list("max-options" = 50),
  )
})
