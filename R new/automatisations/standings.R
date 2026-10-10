# nach elo

library(tidyverse)
library(feather)

new_season_sept <- nflreadr::get_current_season()

# load base data ----
rfl_standing_data <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/standing_data/rfl_standing_{x}.csv"),
    #col_types = "dicccddddididdidddiddddiiiiiiiiiiiiiiiiiiiiicii"
  )
}) %>%
  dplyr::left_join(
    rfl_team_elo %>%
      dplyr::group_by(season, franchise_id, week) %>%
      dplyr::mutate(
        winloss = ifelse(score_diff > 0, 1, 0)
      ) %>%
      dplyr::summarise(
        winloss = paste(winloss, collapse = ","),
        dplyr::across(c(franchise_name, franchise_elo_pregame, franchise_elo_postgame), ~ last(.x)),
        .groups = "drop"
      ),
    by = c("franchise_id", "season", "week")
  ) %>%
  dplyr::left_join(
    rfl_roster_data %>%
      dplyr::filter(starter_status == "starter") %>%
      dplyr::group_by(season, franchise_id, week) %>%
      dplyr::summarise(war_starter = sum(war, na.rm = TRUE), .groups = "drop") %>%
      dplyr::group_by(franchise_id, season) %>%
      dplyr::arrange(week) %>%
      dplyr::mutate(war_starter_season = cumsum(war_starter)) %>%
      dplyr::group_by(season, week) %>%
      dplyr::mutate(
        war_starter_rank = dplyr::min_rank(dplyr::desc(war_starter)),
        war_starter_season_rank = dplyr::min_rank(dplyr::desc(war_starter_season))
      ) %>%
      dplyr::ungroup(),
    by = c("season", "franchise_id", "week")
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    elo_shift = franchise_elo_postgame - franchise_elo_pregame,
    elo_season_rank = dplyr::min_rank(dplyr::desc(franchise_elo_postgame)),
    true_standing = ((pf_season_rank + elo_season_rank + (wins_expected_end_of_season_rank * pp_season_rank * war_starter_season_rank)) * 2) + wins_season_rank + all_play_wins_season_rank + quality_season_rank + eff_season_rank + ((luck_season_rank + pa_season_rank) / 4),
  ) %>%
  dplyr::arrange(true_standing, dplyr::desc(wins_season), dplyr::desc(pf_season)) %>%
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
    losses_season = (week * 2) - wins_season,
  ) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    power_rank_change = dplyr::lag(power_rank) - power_rank
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    wins_over_exp = wins_season - wins_expected_season,
    wins_over_exp_pct = round((wins_season / wins_expected_end_of_season), 2),
    wins_over_exp_pct = ifelse(is.na(wins_over_exp_pct), 0, wins_over_exp_pct),
    power_rank_emoji = dplyr::case_when(
      power_rank_change == 0 ~ "⭤",
      power_rank_change > 3 ~ "⮅",
      power_rank_change > 0 ~ "⭡",
      power_rank_change < -3 ~ "⮇",
      power_rank_change < 0 ~ "⭣",
    ),
    power_rank_change = paste(power_rank, power_rank_emoji),
    seed_emoji = ifelse(is.na(seed_emoji), seed, seed_emoji)
  ) %>%
  dplyr::group_by(week) %>%
  dplyr::mutate(
    dplyr::across(
      c(war_starter_season, pf_season, pp_season, wins_season, all_play_wins_season, wins_expected_end_of_season, eff_season, luck_season, quality_season, franchise_elo_pregame),
      ~ round(dplyr::percent_rank(.x), 2),
      .names = "{.col}_pctl"
    ),
    pa_season_pctl = round(dplyr::percent_rank(dplyr::desc(pa_season)), 2),
    elo_season_pctl = round(dplyr::percent_rank(franchise_elo_postgame), 2),
    true_standing_pctl = round(dplyr::percent_rank(dplyr::desc(true_standing)), 2),
  ) %>%
  dplyr::ungroup() %>%
  #filter(franchise_id == "0007" & season == 2026) %>%
  dplyr::group_by(season, franchise_id) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    winloss_season = purrr::accumulate(winloss, ~ paste(.x, .y, sep = ",")),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, division_name, conference_name),
    by = "franchise_id"
  ) %>%

  # create seeding
  #filter(division_name == "Barry Sanders Division") %>%
  dplyr::group_by(season, week, division_name) %>%
  dplyr::arrange(div_rank) %>%
  dplyr::mutate(
    status_div = dplyr::case_when(
      wins_season - dplyr::nth(wins_season, 2) > 26 - week * 2 ~ "D",
      dplyr::nth(wins_season, 1) - wins_season > 26 - week * 2 ~ "<s>D</s>",
      TRUE ~ ""
    )
  ) %>%
  #filter(conference_name == "RFC") %>%
  dplyr::group_by(season, week, conference_name) %>%
  dplyr::arrange(
    factor(bowl, levels = c("SB", "PB", "TB")),
    seed
  ) %>%
  dplyr::group_by(season, week, bowl, conference_name) %>%
  dplyr::arrange(seed) %>%
  dplyr::mutate(
    divider = ifelse(dplyr::row_number() == 6, 1, 0),
  ) %>%
  dplyr::group_by(season, week, conference_name) %>%
  dplyr::arrange(
    factor(bowl, levels = c("SB", "PB", "TB")),
    seed
  ) %>%
  # create status
  dplyr::mutate(
    seed_total = dplyr::row_number(),
    # check if wins_season is less than wins_season in nth row of group
    status_pb = dplyr::case_when(
      dplyr::nth(wins_season, 12) - wins_season > 26 - week * 2 ~ "<s>PB</s>",
      wins_season - dplyr::nth(wins_season, 13) > 26 - week * 2 ~ "PB",
      TRUE ~ ""
    ),
    status_sb = dplyr::case_when(
      dplyr::nth(wins_season, 12) - wins_season > 26 - week * 2 ~ "<s>SB</s>",
      wins_season - dplyr::nth(wins_season, 7) > 26 - week * 2 ~ "SB",
      TRUE ~ ""
    ),
    status_sb_bye = dplyr::case_when(
      dplyr::nth(wins_season, 2) - wins_season > 26 - week * 2 | status_div == "<s>D</s>" ~ "<s>zZ</s>",
      status_div == "D" & wins_season - dplyr::nth(wins_season, 3) > 26 - week * 2 ~ "zZ",
      TRUE ~ ""
    ),
    status_bowl = ifelse(status_sb == "SB", status_sb, status_pb),
    franchise_name_status = ifelse(status_pb != "" & status_sb != "" & status_sb_bye != "", paste(franchise_name, "<sup>", status_div, status_bowl, status_sb_bye, "</sup>"), franchise_name)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, week, franchise_id, franchise_name = franchise_name_status, division_name, conference_name, conf_id, div_id, pf:quality, winloss, wins_over_exp, wins_over_exp_pct, war_starter, dplyr::ends_with("_season"), elo_pre = franchise_elo_pregame, elo_pre_pctl = franchise_elo_pregame_pctl, elo_post = franchise_elo_postgame, elo_shift, bowl, seed, pick, power_rank, power_rank_change, dplyr::ends_with("_emoji"), dplyr::ends_with("_rank"), true_standing, dplyr::ends_with("_pctl"), seed_total)
  #dplyr::select(season, week, franchise_name, true_standing, true_standing_season_pctl)

## write to db ----
DBI::dbWriteTable(con, "rfl_standing_data", rfl_standing_data, overwrite = TRUE)
