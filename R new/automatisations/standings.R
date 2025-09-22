# nach elo

library(tidyverse)
library(feather)

new_season_sept <- nflreadr::get_current_season()

# load base data ----
rfl_standing_data <- vroom::vroom(paste0("https://github.com/bohndesverband/rfl-data/releases/download/standing_data/rfl_standing_", new_season_sept, ".csv"), col_types = "icccdddiiiddiiiiiiiiicii")

## write to feather ----
feather::write_feather(rfl_standing_data, "data/rfl_standing_data.feather")

# create weekly rankings ----
rfl_weekly_standing <- feather::read_feather("data/rfl_team_elo.feather") %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::mutate(
    winloss = ifelse(score_diff > 0, 1, 0)
  ) %>%
  dplyr::group_by(franchise_id, week) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    winloss = paste(winloss, collapse = ","),
    dplyr::across(c(season, dplyr::starts_with("franchise"), division, division_name, conference_id, conference_name), last),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    rfl_standing_data %>%
      dplyr::select(week, franchise_id, dplyr::ends_with("_rank"), dplyr::ends_with("_total"), bowl, seed, pick),
    by = c("franchise_id", "week")
  ) %>%
  dplyr::group_by(week) %>%
  dplyr::mutate(
    elo_shift = franchise_elo_postgame - franchise_elo_pregame,
    elo_rank = dplyr::min_rank(dplyr::desc(franchise_elo_postgame)),
    true_standing = ((pf_rank + pp_rank * elo_rank) * 2) + record_rank + all_play_rank + (eff_rank / 4),
  ) %>%
  dplyr::arrange(true_standing, dplyr::desc(wins_total), dplyr::desc(pf_total)) %>%
  dplyr::mutate(
    power_rank = dplyr::row_number(),
    seed_emoji = dplyr::case_when(
      bowl == "SB" & seed <= 2 ~ emoji::emoji("zzz"),
      bowl %in% c("PB", "TB") & seed <= 4 ~ emoji::emoji("zzz"),
    ),
    bowl_emoji = dplyr::case_when(
      bowl == "SB" ~ emoji::emoji("trophy"),
      bowl == "PB" ~ emoji::emoji("sports medal"),
      bowl == "TB" ~ emoji::emoji("pile of poo")
    ),
    losses_total = (week * 2) - wins_total,
  ) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    power_rank_change = dplyr::lag(power_rank) - power_rank
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    power_rank_emoji = dplyr::case_when(
      power_rank_change == 0 ~ "⭤",
      power_rank_change > 3 ~ "⮅",
      power_rank_change > 0 ~ "⭡",
      power_rank_change < -3 ~ "⮇",
      power_rank_change < 0 ~ "⭣",
    ),
    power_rank_change = paste(power_rank, power_rank_emoji),
    seed_emoji = ifelse(is.na(seed_emoji), seed, seed_emoji)
  )

## write to feather ----
feather::write_feather(rfl_weekly_standing, "data/rfl_weekly_standing.feather")

# create current standing ----
rfl_current_standing <- rfl_weekly_standing %>%
  dplyr::group_by(franchise_id) %>%
  #filter(week == 12) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    winloss = paste(winloss, collapse = ","),
    pf_sparkline = paste(unique(franchise_score), collapse = ","),
    elo_sparkline = paste(unique(franchise_elo_postgame), collapse = ","),
    dplyr::across(c(franchise_name, division, division_name, conference_name, season, franchise_elo_pregame), first),
    dplyr::across(c(season, week, dplyr::ends_with("_total"), franchise_elo_postgame, elo_shift, dplyr::ends_with("_rank"), power_rank_change, bowl, bowl_emoji, seed, seed_emoji, power_rank_emoji), last),
    .groups = "drop"
  ) %>%
  dplyr::group_by(bowl, conference_name) %>%
  dplyr::arrange(seed) %>%
  dplyr::mutate(
    divider = ifelse(dplyr::row_number() == 6, 1, 0),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, week, franchise_id, division, franchise_name, conference_name, division_name, wins_total, losses_total, winloss, pf_sparkline, pp_total, pf_total, dplyr::starts_with("franchise_elo"), elo_sparkline, elo_shift, dplyr::ends_with("_rank"), power_rank_emoji, bowl, bowl_emoji, seed, seed_emoji, divider) %>%
  dplyr::arrange(league_rank)

## write to feather ----
feather::write_feather(rfl_current_standing, "data/rfl_current_standing.feather")
