# teams ----
rfl_team_elo <- feather::read_feather("data/rfl_team_elo.feather")

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
  dplyr::select(season, week, franchise_id, opponent_id, franchise_name, win, elo_shift, total_elo_shift, franchise_elo_pregame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, total_points, total_elo, score_diff, score_diff_pct, upset, label)

# spieler ----
player_elo <- feather::read_feather("data/rfl_player_elo.feather")
