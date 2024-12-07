team_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_team-elo_{x}.csv"),
    col_types = "iiccnnnnnnn"
  )
}) %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name, division, division_name, conference_id, conference_name), by = "franchise_id") %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

player_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_player-elo_{x}.csv"),
    col_types = "iicccccnniiiii"
  )
}) %>%
  dplyr::left_join(nflreadr::load_players() %>% select(display_name, gsis_id), by = "gsis_id")
