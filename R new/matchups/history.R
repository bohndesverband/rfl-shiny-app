matchup_table <- function(df) {
  df %>%
    gt::gt() %>%
    gt::cols_move(elo_shift, franchise_elo_pregame) %>%

    gt::tab_spanner(
      "Gegner",
      dplyr::starts_with("opponent"),
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_label(
      season = "Saison",
      week = "WK",
      franchise_name = "Name",
      franchise_elo_pregame = "Pregame ELO",
      franchise_score = "Punkte",
      elo_shift = "ELO +/-",
      elo_diff = "ELO Diff",
      opponent_score = "Punkte",
      opponent_elo_pregame = "Pregame ELO",
      opponent_name = "Name",
    ) %>%
    gt::cols_hide(c(upset, franchise_id, opponent_id)) %>%
    gtDefaults() %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = TRUE,
      ihtml.page_size_default = 24,
      ihtml.use_highlight = TRUE,
      ihtml.use_page_size_select = TRUE,
      ihtml.page_size_values = c(24, 50, 100),
      ihtml.pagination_type = "jump",
      ihtml.use_filters = TRUE,
    ) %>%
    gt::cols_width(
      c(franchise_name, opponent_name) ~ px(180),
      c(season, franchise_elo_pregame, franchise_score, opponent_score, opponent_elo_pregame, elo_diff) ~ px(100),
      c(week, elo_shift) ~ px(70),
    )
}

output$rfl_matchup_history_table <- gt::render_gt({
  rfl_matchups_history %>%
    dplyr::left_join(
      rfl_standing_data %>%
        dplyr::select(franchise_id, pp, season, week, all_play_wins),
      by = c("franchise_id", "season", "week")
    ) %>%
    dplyr::mutate(eff = round(franchise_score / pp, 3) * 100) %>%
    dplyr::arrange(dplyr::desc(season), dplyr::desc(week)) %>%
    dplyr::select(-label, -pp) %>%
    matchup_table() %>%
    gt::cols_move(eff, franchise_score) %>%
    gt::cols_move(win, franchise_name) %>%
    gt::cols_move(all_play_wins, win) %>%
    gt::tab_spanner(
      "Team",
      c(dplyr::starts_with("franchise"), win, all_play_wins, elo_shift, total_elo_shift),
    ) %>%
    gt::tab_spanner(
      "Match Totals",
      c(total_points, score_diff, score_diff_pct, total_elo, elo_diff),
    ) %>%
    gt::cols_label(
      win = "Win",
      all_play_wins = "All-Play Wins",
      eff  = "Eff %",
      total_points = "Total FPts",
      total_elo = "Total ELO",
      score_diff = "Score Diff",
      score_diff_pct = "Score Diff %",
      total_elo_shift = "Total ELO +/-"
    ) %>%
    gt::cols_width(
      #c(franchise_name, opponent_name) ~ px(200),
      c(all_play_wins, total_points, total_elo, score_diff, score_diff_pct, total_elo_shift) ~ px(100),
      c(win, eff) ~ px(70),
    )
})

output$rfl_matchup_upsets_table <- gt::render_gt({
  rfl_matchups_history %>%
    dplyr::select(-win, -total_elo_shift, -total_points, -total_elo, -score_diff, -score_diff_pct, -label) %>%
    dplyr::filter(upset == 1 & elo_diff < -100) %>%
    dplyr::arrange(elo_diff) %>%
    matchup_table() %>%
    gt::tab_spanner(
      "Team",
      c(dplyr::starts_with("franchise"), elo_shift),
    ) %>%
    gt::data_color(
      dplyr::ends_with("elo_pregame"),
      palette = c(color_red, color_blue)
    ) %>%
    gt::data_color(
      elo_diff,
      palette = c(color_red, color_blue)
    )
})
