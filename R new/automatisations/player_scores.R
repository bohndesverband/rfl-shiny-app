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

rfl_player_scores <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/playerscores_data/rfl_playerscores_{x}.csv"), col_types = "iiccccn"
  )
})

feather::write_feather(rfl_player_scores, "data/rfl_player_scores.feather")

# fantay finishes ----

create_ranks <- function(df, group, colname) {
  group_syms <- rlang::syms(group)
  df %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::arrange(dplyr::desc(points)) %>%
    dplyr::mutate(
      pos_rank = row_number(),
      !!paste0("top3_", colname) := ifelse(pos_rank <= 3, 1, 0),
      !!paste0("top5_", colname) := ifelse(pos_rank <= 5, 1, 0),
      !!paste0("top8_", colname) := ifelse(pos_rank <= 8, 1, 0),
      !!paste0("top12_", colname) := ifelse(pos_rank <= 12, 1, 0),
      !!paste0("top24_", colname) := ifelse(pos_rank <= 24, 1, 0),
      !!paste0("top36_", colname) := ifelse(pos_rank <= 36, 1, 0),
      !!paste0("top48_", colname) := ifelse(pos_rank <= 48, 1, 0),
      !!paste0("top60_", colname) := ifelse(pos_rank <= 60, 1, 0)
    ) %>%
    dplyr::ungroup()
}

rfl_fantasy_finishes_weekly <- feather::read_feather("data/rfl_player_scores.feather") %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  create_ranks(c("season", "pos", "week"), "weekly")

feather::write_feather(rfl_fantasy_finishes_weekly, "data/rfl_fantasy_finishes_weekly.feather")

rfl_fantasy_finishes_season <- feather::read_feather("data/rfl_player_scores.feather") %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(season, player_id) %>%
  dplyr::summarise(
    player_id = dplyr::first(player_id),
    pos = dplyr::last(pos),
    player_name = dplyr::last(player_name),
    team = dplyr::last(team),
    points = sum(as.numeric(points), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  create_ranks(c("season", "pos"), "season")

feather::write_feather(rfl_fantasy_finishes_season, "data/rfl_fantasy_finishes_season.feather")
