library(tidyverse)
library(feather)

var_season <- 2025

# preseason

sos <- rfl_drafts_data <- purrr::map_df(2024:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/schedule/rfl-sos-{x}.csv"),
    col_types = "cdddddcc"
  ) %>%
    dplyr::mutate(season = x)
})

feather::write_feather(sos, "data/rfl_sos.feather")

rfl_schedule_data <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/rfl-schedules.csv", col_types = "dicc")

feather::write_feather(rfl_schedule_data, "data/rfl_schedule_data.feather")

# inseason
schedule <- rfl_schedule_data %>%
  #filter(season == 2025) %>%
  dplyr::left_join(
    rfl_standing_data %>%
      dplyr::group_by(season) %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::select(season, franchise_id, all_play_wins_total),
    by = c("opponent_id" = "franchise_id", "season")
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(
    type = dplyr::case_when(
      season >= var_season & week >= current_week ~ "sos_upcoming",
      TRUE ~ "sos_previous"
    ),
    max_week = ifelse(season == var_season, current_week - 1, max(week)),
    all_play_win_pct = all_play_wins_total / (max_week * 35)
  ) %>%
  dplyr::group_by(season, franchise_id) %>%
  dplyr::mutate(
    sos_total = mean(all_play_win_pct, na.rm = TRUE)
  ) %>%
  dplyr::group_by(season, franchise_id, type) %>%
  dplyr::summarise(
    sos_total = dplyr::first(sos_total),
    sos = mean(all_play_win_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = type, values_from = c(sos))

if (current_week <= 13) {
  sos_inseason <- schedule %>%
    dplyr::left_join(
      sos %>%
        dplyr::select(season, franchise_id, sos_preseason = Total, sos_score_preseason = SOS),
      by = c("franchise_id", "season")
    ) %>%
    filter(season == 2025) %>%
    dplyr::mutate(
      sos_change = sos_total - sos_preseason,
      sos_diff = sos_previous - sos_upcoming,
      sos_score_total = sos_total / max(sos_total, na.rm = TRUE),
      sos_score_previous = sos_previous / max(sos_previous, na.rm = TRUE),
      sos_score_upcoming = sos_upcoming / max(sos_upcoming, na.rm = TRUE)
    ) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name, division),
      by = "franchise_id"
    )

  feather::write_feather(sos_inseason, "data/rfl_sos_inseason.feather")
}
