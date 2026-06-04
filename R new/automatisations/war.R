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

player_to_tv <- function(rating, beta = 2.5) {
  1000 * (rating / 10)^beta
}

# teams ----
rfl_war_data <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/war_data/rfl_war_{x}.csv"),
    col_types = "icccddii"
  )
}) %>%
  #filter(player_id == "16646") %>%
  # add player value
  #dplyr::left_join(
  #  nflreadr::load_ff_playerids() %>%
  #    dplyr::select(gsis_id, mfl_id),
  #  by = c("player_id" = "mfl_id"),
  #  relationship = "many-to-many"
  #) %>%
  dplyr::left_join(
    rfl_fantasy_finishes_season %>%
      dplyr::select(player_id, top5_season, top12_season, top24_season, season),
    by = c("player_id", "season")
  #  relationship = "many-to-many"
  ) %>%
  dplyr::group_by(player_id) %>%
  dplyr::mutate(
    war_mean = mean(war),
    dplyr::across(c(top5_season, top12_season, top24_season), sum),
    games_played_sum = mean(games_played)
    #value_pctl = dplyr::percent_rank(value)
  ) %>%
  ungroup() %>%
  dplyr::mutate(
    bonus_top5 = top5_season,
    bonus_top12 = (top12_season - top5_season) / 2,
    bonus_top24 = dplyr::case_when(
      !(pos %in% c("QB", "PK")) ~ (top24_season - top12_season) / 4,
      TRUE ~ 0
    ),
    bonus_finishes = bonus_top5 + bonus_top12 + bonus_top24,
    pvar = war_mean + ((games_played_sum + bonus_finishes) / 10),
    # pvar ins positive rücken
    #pvar = pvar + abs(min(pvar, na.rm = TRUE)),
    # zwischen 1 und 10 normalisieren
    pvar = round(scales::rescale(pvar, c(1, 10)), 1),
    #trade_value = player_to_tv(pvar)
  ) %>%
  dplyr::select(-war_mean, -games_played_sum, -dplyr::starts_with("top"), -dplyr::starts_with("bonus"))

feather::write_feather(rfl_war_data, "data/rfl_war_data.feather")
