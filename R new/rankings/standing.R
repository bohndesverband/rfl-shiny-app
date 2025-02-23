weekly_standing <- team_elo %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::mutate(
    winloss = ifelse(score_diff > 0, 1, 0)
  ) %>%
  dplyr::group_by(franchise_id, week) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    winloss = list(winloss),
    dplyr::across(c(season, dplyr::starts_with("franchise"), division, division_name, conference_id, conference_name), last),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    standing_data %>%
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

current_standing <- weekly_standing %>%
  dplyr::group_by(franchise_id) %>%
  #filter(week == 12) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    winloss = list(unlist(winloss)),
    pf_sparkline = list(unique(franchise_score)),
    elo_sparkline = list(unique(franchise_elo_postgame)),
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

# draft order----
draft_order <- weekly_standing %>%
  dplyr::select(week, franchise_id, franchise_name, pick)

if (current_week > 13) {
  postseason_teams <- draft_order %>%
    dplyr::filter(week == max(week) & pick > 24) %>%
    dplyr::select(-week)

  postseason_results <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/postseason_data/rfl_postseason_", new_season_sept, ".csv"), col_types = "icciccdcii") %>%
    dplyr::filter(bowl == "SB" & (match_result == "L" | title == 1) & bracket != "Spiel um Platz drei") %>%
    dplyr::left_join(postseason_teams, by = "franchise_id") %>%
    dplyr::group_by(week) %>%
    dplyr::arrange(pick) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(week) %>%
    dplyr::mutate(
      pick = dplyr::case_when(
        week == 17 & title == 0 ~ 35,
        week == 17 & title == 1 ~ 36,
        TRUE ~ row_number() + 24
      ),
      week = 14
    ) %>%
    dplyr::arrange(pick) %>%
    dplyr::select(week, franchise_id, pick) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::select(week, franchise_id, franchise_name, pick)

  draft_order_postseason <- draft_order %>%
    dplyr::filter(week == 13) %>%
    dplyr::arrange(pick) %>%
    dplyr::filter(pick <= 24) %>%
    dplyr::mutate(week = 14) %>%
    dplyr::bind_rows(postseason_results)

  draft_order <- rbind(draft_order, draft_order_postseason) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, division),
      by = "franchise_id"
    )
}
