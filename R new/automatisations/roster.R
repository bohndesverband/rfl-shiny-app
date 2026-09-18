library(tidyverse)
library(nflreadr)

rfl_roster_data <- purrr::map_df(2024:season_before_wk_1, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_{x}.csv"),
    col_types = "dicccccc"
  )
}) %>%
  # add birthday
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, birthdate),
    by = c("player_id" = "mfl_id")
  ) %>%
  # add schedule data for age calculation
  dplyr::left_join(
    nflreadr::load_schedules(2016:new_season_sept) %>%
      dplyr::select(season, week, gameday) %>%
      dplyr::arrange(gameday) %>%
      dplyr::group_by(season, week) %>%
      dplyr::slice(1),
    by = c("season", "week")
  ) %>%
  # calculate age
  dplyr::mutate(
    age = round(as.numeric(as.Date(gameday) - as.Date(birthdate)) / 365.25, 1)
  ) %>%
  dplyr::select(-birthdate, -gameday) %>%

  dplyr::left_join(
    rfl_war_data %>%
      dplyr::select(player_id, season, war),
    by = c("player_id", "season")
  ) %>%
  dplyr::left_join(
    rfl_starter_data %>%
      dplyr::mutate(player_id = as.character(player_id)) %>%
      dplyr::select(season:player_id, player_score),
    by = c("season", "week", "player_id", "franchise_id")
  ) %>%
  dplyr::mutate(
    pos_grouped = dplyr::case_when(
      pos %in% c("DE", "DT") ~ "DL",
      pos %in% c("S", "CB") ~ "DB",
      TRUE ~ pos
    ),
    player_name_with_info = paste0("<div>", nflreadr::clean_player_names(player_name), "</div><div><small>", pos_grouped, ", ", team, "</small></div>")
  ) %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::select(season, week, mfl_id, player_elo_post),
    by = c("season", "week", "player_id" = "mfl_id")
  ) %>%
  dplyr::mutate(started = ifelse(starter_status == "starter", 1, 0)) %>%
  dplyr::group_by(season, franchise_id, player_id) %>%
  dplyr::mutate(games_started = sum(started, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-started) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  )

DBI::dbWriteTable(con, "rfl_roster_data", rfl_roster_data, overwrite = TRUE)

# IR ----
rfl_ir_data <- rfl_roster_data %>%
  # add gsis id
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%
  # add nfl IR designation
  dplyr::left_join(
    nflreadr::load_rosters_weekly(2016:season_before_wk_2) %>%
      dplyr::filter(status == "RES") %>%
      dplyr::select(gsis_id, season, week, status) %>%
      dplyr::filter(!is.na(gsis_id)),
    by = c("gsis_id", "season", "week")
  ) %>%
  dplyr::filter(!is.na(status)) %>%
  # add latest PPG data
  dplyr::left_join(
    rfl_player_scores %>%
      dplyr::select(player_id, season, games, ppg) %>%
      dplyr::distinct() %>%
      dplyr::group_by(player_id) %>%
      dplyr::filter(games > 3 & season >= season - 1) %>% # min 3 games in dieser oder der letzten saison gespielt
      dplyr::filter(season == max(season)) %>%
      dplyr::ungroup(),
    by = c("player_id", "season")
  ) %>%
  dplyr::mutate(ppg = ifelse(is.na(ppg), 0, ppg)) %>%
  # add franchise data
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::select(-roster_status, -age, -gsis_id, -status)

DBI::dbWriteTable(con, "rfl_ir_data", rfl_ir_data, overwrite = TRUE)
