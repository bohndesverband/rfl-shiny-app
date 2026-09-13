library(tidyverse)
library(feather)

var_season <- 2026

# matchups
rfl_matchups_history <- rfl_team_elo %>%
  mutate(
    total_points = franchise_score + opponent_score,
    total_elo = franchise_elo_pregame + opponent_elo_pregame,
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_diff > 0, 1, 0),
    result = ifelse(score_diff > 0, " W über ", " L gegen "),
    win = ifelse(score_diff > 0, 1, 0),
    label = paste0(franchise_name, " (", franchise_elo_pregame, ")", result, opponent_name, " (", opponent_elo_pregame, ") - WK", week, " ", season),
    score_diff = round(franchise_score - opponent_score, 2),
    score_diff_pct = round(score_diff / opponent_score, 4) * 100
  ) %>%
  dplyr::group_by(season, week, franchise_id) %>%
  dplyr::mutate(
    total_elo_shift = sum(elo_shift)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, week, franchise_id, opponent_id, franchise_name, win, elo_shift, total_elo_shift, franchise_elo_pregame, franchise_elo_postgame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, total_points, total_elo, score_diff, score_diff_pct, upset, label, division, conference_id) %>%
  dplyr::left_join(
    rfl_starter_ppg_fpts_diff %>%
      dplyr::select(season, week, franchise_id, franchise_ppg_score, franchise_points_ppg_diff),
    by = c("season", "week", "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_starter_ppg_fpts_diff %>%
      dplyr::select(season, week, franchise_id, opponent_ppg_score = franchise_ppg_score, opponent_points_ppg_diff = franchise_points_ppg_diff),
    by = c("season", "week", "opponent_id" = "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_standing_data %>%
      dplyr::select(season, week, franchise_id, pp, all_play_wins),
    by = c("season", "week", "franchise_id")
  ) %>%
  # pctl berechnung
  dplyr::mutate(
    eff = franchise_score / pp,
    dplyr::across(
      c(franchise_score, pp, eff, elo_shift, all_play_wins, franchise_elo_pregame, elo_shift, franchise_ppg_score, franchise_points_ppg_diff, total_points, total_elo, score_diff, score_diff_pct),
      ~ dplyr::percent_rank(.x),
      .names = "{.col}_pctl"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(season), dplyr::desc(week))

feather::write_feather(rfl_matchups_history, "data/rfl_matchups_data.feather")

# schedule ----
rfl_schedule_data <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/rfl-schedules.csv", col_types = "dicc") %>%

  # add elo data
  dplyr::left_join(
    rfl_matchups_history %>%
      dplyr::select(season, week, franchise_id, opponent_id, dplyr::ends_with("elo_pregame"), dplyr::ends_with("elo_postgame"), win, dplyr::ends_with("_score"), division, conference_id),
    by = c("season", "week", "franchise_id", "opponent_id")
  ) %>%
  dplyr::left_join(
    rfl_matchups_history %>%
      dplyr::select(season, week, franchise_id = opponent_id, opponent_id = franchise_id, opponent_elo_postgame = franchise_elo_postgame) %>%
      dplyr::distinct(),
    by = c("season", "week", "franchise_id", "opponent_id")
  ) %>%

  # daten für neue saison wenn kein schedule vorhanden ist
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    franchise_elo_pregame = ifelse(is.na(franchise_elo_pregame), dplyr::lag(franchise_elo_postgame), franchise_elo_pregame),
  ) %>%
  tidyr::fill(
    franchise_elo_pregame,
    .direction = "down"
  ) %>%
  dplyr::group_by(opponent_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    opponent_elo_pregame = ifelse(is.na(opponent_elo_pregame), dplyr::lag(opponent_elo_postgame), opponent_elo_pregame)
  ) %>%
  tidyr::fill(
    opponent_elo_pregame,
    .direction = "down"
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    avg_elo = round(mean(franchise_elo_pregame), 0),
    franchise_elo_diff = franchise_elo_pregame - avg_elo,
    opponent_elo_diff = opponent_elo_pregame - avg_elo
  ) %>%
  dplyr::select(-dplyr::ends_with("postgame"), -avg_elo) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name, abbrev),
    by = c("franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, opponent_name = franchise_name, opponent_abbrev = abbrev, opponent_div = division, opponent_conf = conference_id),
    by = c("opponent_id" = "franchise_id")
  ) %>%
  dplyr::mutate(
    matchup = dplyr::case_when(
      division == opponent_div ~ "Div",
      conference_id == opponent_conf ~ "Conf",
      TRUE ~ "Zufällig"
    )
  ) %>%
  dplyr::select(-opponent_div, -division, -opponent_conf, -conference_id)

feather::write_feather(rfl_schedule_data, "data/rfl_schedule_data.feather")

# preseason
sos <- rfl_drafts_data <- purrr::map_df(2024:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/schedule/rfl-sos-{x}.csv"),
    col_types = "cdddddcc"
  ) %>%
    dplyr::mutate(season = x)
})

feather::write_feather(sos, "data/rfl_sos.feather")

# inseason
schedule <- rfl_schedule_data %>%
  #filter(season == 2025) %>%
  # add standing
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
  dplyr::mutate(
    sos_total = ifelse(!is.na(sos_total), sos_total, NA),
    sos = ifelse(!is.na(sos), sos, NA),
  ) %>%
  tidyr::pivot_wider(names_from = type, values_from = c(sos))

if (current_week <= 13) {
  sos_inseason <- schedule %>%
    dplyr::left_join(
      sos %>%
        dplyr::select(season, franchise_id, sos_preseason = Total, sos_score_preseason = SOS),
      by = c("franchise_id", "season")
    ) %>%
    #filter(season == 2026) %>%
    dplyr::mutate(
      sos_change = ifelse(!is.na(sos_total), sos_total - sos_preseason, NA),
      sos_diff = ifelse(!is.na(sos_previous), sos_previous - sos_upcoming, NA),
      sos_score_total = ifelse(!is.na(sos_total), sos_total / max(sos_total, na.rm = TRUE), sos_preseason / max(sos_preseason, na.rm = TRUE)),
      sos_score_previous = ifelse(!is.na(sos_previous), sos_previous / max(sos_previous, na.rm = TRUE), NA),
      sos_score_upcoming = ifelse(!is.na(sos_upcoming), sos_upcoming / max(sos_upcoming, na.rm = TRUE), NA)
    ) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name, division),
      by = "franchise_id"
    )

  feather::write_feather(sos_inseason, "data/rfl_sos_inseason.feather")
}
