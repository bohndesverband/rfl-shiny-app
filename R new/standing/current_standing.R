current_standing <- team_elo %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::mutate(
    winloss = ifelse(score_diff > 0, 1, 0)
  ) %>%
  dplyr::group_by(franchise_id, week) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    winloss = list(winloss),
    dplyr::across(c(season, dplyr::starts_with("franchise"), division_name, conference_name), last),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    standing_data %>%
      dplyr::select(week, franchise_id, dplyr::ends_with("_rank"), dplyr::ends_with("_total"), bowl, seed, pick),
    by = c("franchise_id", "week")
  ) %>%
  dplyr::group_by(week) %>%
  dplyr::mutate(
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
    bowl = dplyr::case_when(
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
  dplyr::summarise(
    winloss = list(unlist(winloss)),
    pf_sparkline = list(unique(franchise_score)),
    elo_sparkline = list(unique(franchise_elo_postgame)),
    dplyr::across(c(franchise_name, division_name, conference_name, season, franchise_elo_pregame), first),
    dplyr::across(c(season, week, dplyr::ends_with("_total"), franchise_elo_postgame, dplyr::ends_with("_rank"), power_rank_change, bowl, seed, seed_emoji), last),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    power_rank_emoji = dplyr::case_when(
      power_rank_change == 0 ~ "⭤",
      power_rank_change > 3 ~ "⮅",
      power_rank_change > 0 ~ "⭡",
      power_rank_change < -3 ~ "⮇",
      power_rank_change < 0 ~ "⭣",
    ),
    power_rank_change = paste(power_rank, power_rank_emoji)
  ) %>%
  dplyr::group_by(bowl, conference_name) %>%
  dplyr::arrange(seed) %>%
  dplyr::mutate(
    divider = ifelse(dplyr::row_number() == 6, 1, 0),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    seed = ifelse(is.na(seed_emoji), seed, seed_emoji)
  ) %>%
  dplyr::select(season, week, franchise_name, conference_name, division_name, wins_total, losses_total, winloss, pf_sparkline, pp_total, pf_total, dplyr::starts_with("franchise_elo"), elo_sparkline, dplyr::ends_with("_rank"), power_rank_emoji, bowl, seed, divider) %>%
  dplyr::arrange(league_rank)
