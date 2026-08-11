# after franchises, player elo & fantasy finishes
library(tidyverse)
library(feather)

var_season <- 2026

mfl_adp_data <- vroom::vroom("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/adp_data.csv", col_types = "icidii") %>%
  dplyr::mutate(adp_range = paste(rfl_min, rfl_max, sep = "-"))

feather::write_feather(mfl_adp_data, "data/mfl_adp_data.feather")

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
    pos_team = paste(pos, team, sep = ", "),
    round_pick = paste(round, sprintf("%02d", pick), sep = "."),
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
      dplyr::mutate(
        ppg = round(mean(score, na.rm = TRUE), 2)
      ) %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::mutate(
        elo_peak = max(player_elo_post),
        peak_season = season[which.max(player_elo_post)]
      ) %>%
      dplyr::arrange(season, week) %>%
      dplyr::filter(row_number() == max(row_number())) %>%
      dplyr::mutate(elo_shift = player_elo_post - 1500) %>%
      dplyr::rename(current_player_elo = player_elo_post) %>%
      dplyr::ungroup() %>%
      dplyr::select(mfl_id, current_player_elo, elo_peak, elo_season_end, elo_shift, ppg, peak_season),
    by = "mfl_id"
  ) %>%

  # add fantasy finish
  dplyr::left_join(
    feather::read_feather("data/rfl_fantasy_finishes_season.feather") %>%
      dplyr::select(player_id, dplyr::starts_with("top")) %>%
      dplyr::rename_with(~ gsub("_season", "", .x)) %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(across(dplyr::starts_with("top"), ~ sum(.x, na.rm = TRUE)), .groups = "drop"),
    by = c("mfl_id" = "player_id")
  ) %>%

  dplyr::group_by(mfl_id) %>%
  dplyr::mutate(
    last_season = ifelse(season == max(season), 1, 0),
    class = paste(season, franchise_name)
  ) %>%
  dplyr::ungroup() %>%

  # adp data
  dplyr::left_join(
    mfl_adp_data %>%
      dplyr::mutate(adp = (rfl_min + rfl_max) / 2) %>%
      dplyr::select(season, mfl_id, adp),
    by = c("season", "mfl_id")
  ) %>%

  # additional info
  dplyr::rowwise() %>%
  dplyr::mutate(
    side = "received",
    asset_id_new = paste("DP", as.integer(round) - 1, pick - 1, season, sep = "_"),
    first_pick_new = first_pick,
    second_pick_new = second_pick,
    third_pick_new = third_pick,
    across(
      c(first_pick_new, second_pick_new, third_pick_new),
      ~ ifelse(.x == overall, paste0("<strong>", .x, "</strong>"), as.character(.x))
    ),
    asset_name = paste(season, paste0(as.integer(round), ".", sprintf("%02d", pick)), player_name, paste0("(", pos, ", ", team, ")")),
    text_rfl =  paste("RFL:", paste(na.omit(c(first_pick_new, second_pick_new, third_pick_new)), collapse = ", ")),
    text_adp = paste0("ADP: ", adp),
    draft_range_subline = ifelse(
      !is.na(adp),
      paste(text_rfl, text_adp, sep = " - "),
      text_rfl
    ),
    pick_id = paste(round, sprintf("%02d", pick), sep = "_")
  ) %>%
  dplyr::rename(date = timestamp) %>%
  dplyr::select(-dplyr::ends_with("pick_new"), -text_rfl, -text_adp)

feather::write_feather(rfl_drafts_data, "data/rfl_drafts_data.feather")

rfl_draft_orders <- purrr::map_df(2017:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft-order_{x}.csv"),
    col_types = "ici"
  )
})

feather::write_feather(rfl_draft_orders, "data/rfl_draft_orders.feather")

nfl_drafts <- nflreadr::load_draft_picks(2017:var_season) %>%
  dplyr::select(gsis_id, round) %>%
  dplyr::rename(nfl_round = round)

feather::write_feather(nfl_drafts, "data/nfl_drafts_data.feather")
