rfl_matchups_history <- rfl_team_elo %>%
  mutate(
    total_points = franchise_score + opponent_score,
    total_elo = franchise_elo_pregame + opponent_elo_pregame,
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_diff > 0, 1, 0),
    result = ifelse(score_diff > 0, " W über ", " L gegen "),
    win = ifelse(score_diff > 0, 1, 0),
    label = paste0(franchise_name, " (", franchise_elo_pregame, ")", result, opponent_name, " (", opponent_elo_pregame, ") - WK", week, " ", season),
    score_diff = round(franchise_score - opponent_score, 2),
    score_diff_pct = round(score_diff / opponent_score, 4) * 100
  ) %>%
  dplyr::group_by(season, week, franchise_id) %>%
  dplyr::mutate(
    total_elo_shift = sum(elo_shift)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, week, franchise_id, opponent_id, franchise_name, win, elo_shift, total_elo_shift, franchise_elo_pregame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, total_points, total_elo, score_diff, score_diff_pct, upset, label) %>%
  dplyr::left_join(
    rfl_starter_ppg_fpts_diff %>%
      dplyr::select(season, week, franchise_id, franchise_ppg_score, franchise_points_ppg_diff),
    by = c("season", "week", "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_starter_ppg_fpts_diff %>%
      dplyr::select(season, week, franchise_id, opponent_ppg_score = franchise_ppg_score, opponent_points_ppg_diff = franchise_points_ppg_diff),
    by = c("season", "week", "opponent_id" = "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_standing_data %>%
      dplyr::select(season, week, franchise_id, pp, all_play_wins),
    by = c("season", "week", "franchise_id")
  ) %>%
  # pctl berechnung
  dplyr::mutate(
    eff = franchise_score / pp,
    dplyr::across(
      c(franchise_score, pp, eff, elo_shift, all_play_wins, franchise_elo_pregame, elo_shift, franchise_ppg_score, franchise_points_ppg_diff, total_points, total_elo, score_diff, score_diff_pct),
      ~ dplyr::percent_rank(.x),
      .names = "{.col}_pctl"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(season), dplyr::desc(week))

rfl_matchups_with_standing <- rfl_matchups_history %>%
    dplyr::arrange(dplyr::desc(season), dplyr::desc(week))
