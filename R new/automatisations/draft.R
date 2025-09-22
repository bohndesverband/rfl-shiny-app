# after franchises, player elo & fantasy finishes
library(tidyverse)
library(feather)

var_season <- 2025

rfl_drafts_data <- purrr::map_df(2016:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft_{x}.csv"),
    col_types = "iTiiiccccccci"
  )
}) %>%
  dplyr::filter(!is.na(timestamp)) %>%
  dplyr::mutate(
    pos_grouped = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::select(-franchise_name) %>%
  dplyr::left_join(
    feather::read_feather("data/rfl_franchises.feather") %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::group_by(season, mfl_id) %>%
  dplyr::arrange(overall) %>%
  dplyr::mutate(
    first_pick = overall[1],
    second_pick = overall[2],
    third_pick = overall[3],
    min_pick = min(overall),
    max_pick = max(overall),
    avg_pick = round(mean(overall)),
    pick_value = overall - second_pick,
  ) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::arrange(first_pick) %>%
  # number groups
  dplyr::mutate(pos_rank = dplyr::dense_rank(first_pick)) %>%
  dplyr::ungroup() %>%

  # add player elo
  dplyr::left_join(
    feather::read_feather("data/rfl_player_elo.feather") %>%
      dplyr::group_by(season, mfl_id) %>%
      dplyr::mutate(ppg = round(mean(score, na.rm = TRUE), 2)) %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::mutate(elo_peak = max(player_elo_post)) %>%
      dplyr::arrange(season, week) %>%
      dplyr::filter(row_number() == max(row_number())) %>%
      dplyr::mutate(elo_shift = player_elo_post - 1500) %>%
      dplyr::rename(current_player_elo = player_elo_post) %>%
      dplyr::ungroup() %>%
      dplyr::select(mfl_id, current_player_elo, elo_peak, elo_shift, ppg),
    by = "mfl_id"
  ) %>%

  # add fantasy finish
  dplyr::left_join(
    feather::read_feather("data/rfl_fantasy_finishes.feather") %>%
      dplyr::select(player_id, dplyr::starts_with("top")),
    by = c("mfl_id" = "player_id")
  )

feather::write_feather(rfl_drafts_data, "data/rfl_drafts_data.feather")

rfl_draft_orders <- purrr::map_df(2017:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft-order_{x}.csv"),
    col_types = "ici"
  )
})

feather::write_feather(rfl_draft_orders, "data/rfl_draft_orders.feather")

mfl_adp_data <- vroom::vroom("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/adp_data.csv", col_types = "icidii") %>%
  dplyr::mutate(adp_range = paste(rfl_min, rfl_max, sep = "-"))

feather::write_feather(mfl_adp_data, "data/mfl_adp_data.feather")

nfl_drafts <- nflreadr::load_draft_picks(2017:var_season) %>%
  dplyr::select(gsis_id, round) %>%
  dplyr::rename(nfl_round = round)

feather::write_feather(nfl_drafts, "data/nfl_drafts_data.feather")
