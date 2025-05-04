# teams ----
team_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_team-elo_{x}.csv"),
    col_types = "iiccnnnnnnn"
  )
}) %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name, division, division_name, conference_id, conference_name), by = "franchise_id") %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

rfl_matchups_history <- team_elo %>%
  mutate(
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_diff > 0, 1, 0),
    label = paste0(franchise_name, " (", franchise_elo_pregame, ") W über ", opponent_name, " (", opponent_elo_pregame, ") - WK", week, " ", season)
  ) %>%
  dplyr::select(season, week, franchise_id, opponent_id, franchise_name, elo_shift, franchise_elo_pregame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, upset, label)

# spieler ----
player_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_player-elo_{x}.csv"),
    col_types = "iicccccnniiiii"
  )
}) %>%
  dplyr::left_join(nflreadr::load_players() %>% select(display_name, gsis_id), by = "gsis_id")
