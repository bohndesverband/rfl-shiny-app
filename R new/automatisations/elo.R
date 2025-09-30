#after franchises
library(tidyverse)
library(nflreadr)
library(feather)

new_season_sept <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

season_before_wk_1 <- new_season_sept
season_before_wk_2 <- new_season_sept

if (current_week == 1) {
  season_before_wk_1 <- nflreadr::get_current_season() - 1
}

if (nflreadr::get_current_week(TRUE) < 2) {
  season_before_wk_2 <- nflreadr::get_current_season() - 1
}

# teams ----
rfl_team_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_team-elo_{x}.csv"),
    col_types = "iiccnnnnnnn"
  )
}) %>%
  dplyr::left_join(feather::read_feather("data/rfl_franchises.feather") %>% select(franchise_id, franchise_name, division, division_name, conference_id, conference_name), by = "franchise_id") %>%
  dplyr::left_join(feather::read_feather("data/rfl_franchises.feather") %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

feather::write_feather(rfl_team_elo, "data/rfl_team_elo.feather")

# spieler ----
player_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_player-elo_{x}.csv"),
    col_types = "iicccccnniiiii"
  )
}) %>%
  dplyr::left_join(nflreadr::load_players() %>% select(display_name, gsis_id), by = "gsis_id")

feather::write_feather(player_elo, "data/rfl_player_elo.feather")
