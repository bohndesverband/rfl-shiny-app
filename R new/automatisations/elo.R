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
  dplyr::mutate(
    franchise_elo_pregame_pctl = round(dplyr::percent_rank(franchise_elo_pregame), 2),
    franchise_elo_postgame_pctl = round(dplyr::percent_rank(franchise_elo_postgame), 2),
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    franchise_elo_pregame_pctl_week = round(dplyr::percent_rank(franchise_elo_pregame), 2),
    franchise_elo_postgame_pctl_week = round(dplyr::percent_rank(franchise_elo_postgame), 2),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(rfl_franchise_data %>% select(franchise_id, franchise_name, division, division_name, conference_id, conference_name), by = "franchise_id") %>%
  dplyr::left_join(rfl_franchise_data %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

DBI::dbWriteTable(con, "rfl_team_elo", rfl_team_elo, overwrite = TRUE)
#DBI::dbWriteTable(con, "rfl_team_elo", rfl_team_elo, overwrite = TRUE)

# spieler ----
rfl_player_elo <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_player-elo_{x}.csv"),
    col_types = "iicccccnniiiii"
  )
}) %>%
  dplyr::group_by(position) %>%
  dplyr::mutate(
    player_elo_pre_pctl = round(dplyr::percent_rank(player_elo_pre), 2),
    player_elo_post_pctl = round(dplyr::percent_rank(player_elo_post), 2),
  ) %>%
  dplyr::group_by(season, week, position) %>%
  dplyr::mutate(
    player_elo_post_pctl_week = round(dplyr::percent_rank(player_elo_post), 2),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(nflreadr::load_players() %>% select(display_name, gsis_id), by = "gsis_id") %>%
  dplyr::group_by(mfl_id, season) %>%
  dplyr::mutate(elo_season_end = player_elo_post[which.max(week)]) %>%

  # add pctl
  dplyr::group_by(position) %>%
  dplyr::mutate(
    player_elo_pre_pctl = round(dplyr::percent_rank(player_elo_pre), 2),
    player_elo_post_pctl = round(dplyr::percent_rank(player_elo_post), 2),
  ) %>%
  dplyr::group_by(season, week, position) %>%
  dplyr::mutate(
    player_elo_post_pctl_week = round(dplyr::percent_rank(player_elo_post), 2),
  ) %>%
  dplyr::ungroup()
  #dplyr::group_by(mfl_id, season) %>%
  #dplyr::mutate(elo_season_end = player_elo_post[which.max(week)]) %>%
  #dplyr::ungroup()

# DBI::dbWriteTable(con, "rfl_player_elo", rfl_player_elo, overwrite = TRUE)
