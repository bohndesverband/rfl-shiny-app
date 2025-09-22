# teams ----
team_elo <- feather::read_feather("data/rfl_team_elo.feather")

rfl_matchups_history <- team_elo %>%
  mutate(
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_diff > 0, 1, 0),
    result = ifelse(score_diff > 0, " W über ", " L gegen "),
    label = paste0(franchise_name, " (", franchise_elo_pregame, ")", result, opponent_name, " (", opponent_elo_pregame, ") - WK", week, " ", season)
  ) %>%
  dplyr::select(season, week, franchise_id, opponent_id, franchise_name, elo_shift, franchise_elo_pregame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, upset, label)

# spieler ----
player_elo <- feather::read_feather("data/rfl_player_elo.feather")
