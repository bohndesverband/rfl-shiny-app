running_elo <- elo %>%
  dplyr::mutate(
    week = stringr::str_pad(week, 2, pad = "0")
  ) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::mutate(game = paste0(season, week)) %>%
  dplyr::arrange(game) %>%
  dplyr::select(game, franchise_id, franchise_elo_postgame) %>%
  dplyr::distinct() %>%
  dplyr::mutate(game = dplyr::row_number()) %>%
  dplyr::left_join(franchises %>% dplyr::select(franchise_id, franchise_name, division_name), by = "franchise_id") %>%
  dplyr::ungroup()

elo_matchups_prev <- elo %>%
  mutate(
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_diff > 0, 1, 0)
  ) %>%
  dplyr::select(season, week, franchise_name, franchise_elo_pregame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, upset, elo_shift) %>%
  dplyr::rename(
    Season = season,
    WK = week,
    Home = franchise_name,
    Away = opponent_name,
    "Home ELO" = franchise_elo_pregame,
    "Away ELO" = opponent_elo_pregame,
    "ELO Diff" = elo_diff,
    "Home Score" = franchise_score,
    "Away Score" = opponent_score,
    "ELO Shift" = elo_shift
  )

if (nflreadr::get_current_week() <= 13) {
  elo_matchups_next <- jsonlite::read_json(paste0(var.mflApiBase, "/export?TYPE=schedule&L=63018&APIKEY=&W=", nflreadr::get_current_week(), "&JSON=1")) %>%
    purrr::pluck("schedule", "weeklySchedule", "matchup") %>%
    tibble::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(1, names_sep = "_") %>%
    tidyr::unnest_wider(1) %>%
    dplyr::rename(franchise_id = id) %>%
    dplyr::select(-spread, -isHome, -result) %>%
    tidyr::unnest_wider(2) %>%
    dplyr::rename(opponent_id = id) %>%
    dplyr::select(franchise_id, opponent_id) %>%
    dplyr::left_join(
      elo %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::filter(week == max(week)) %>%
        dplyr::select(franchise_id, franchise_elo_pregame) %>% rename(franchise_elo = franchise_elo_pregame) %>%
        dplyr::distinct(),
      by = "franchise_id",
      multiple = "all"
    ) %>%
    dplyr::left_join(
      elo %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::filter(week == max(week)) %>%
        dplyr::select(franchise_id, franchise_elo_pregame) %>% rename(opponent_elo = franchise_elo_pregame) %>%
        dplyr::distinct(),
      by = c("opponent_id" = "franchise_id"),
      multiple = "all",
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(win_pct = 1 / (10^(-(franchise_elo - opponent_elo) / 400 ) + 1)) %>%
    dplyr::left_join(franchises %>% select(franchise_id, franchise_name), by = "franchise_id") %>%
    dplyr::left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id")) %>%
    dplyr::mutate("ELO Adv" = scales::percent(win_pct, 1)) %>%
    dplyr::select(franchise_name, franchise_elo, "ELO Adv", opponent_elo, opponent_name) %>%
    dplyr::rename(Home = franchise_name, "Home ELO" = franchise_elo, "Away ELO" = opponent_elo, Away = opponent_name)
}
