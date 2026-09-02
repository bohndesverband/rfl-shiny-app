nfl_pool_pick_data <- jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=pool&L=63018&POOLTYPE=NFL&JSON=1"))$poolPicks$franchise %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::unnest_longer(week) %>%
  tidyr::unnest_wider(week) %>%
  tidyr::unnest_longer(game) %>%
  tidyr::unnest_wider(game, names_sep = "_") %>%
  dplyr::select(week, franchise_id = id, matchup = game_matchup, pick = game_pick) %>%
  tidyr::separate(matchup, into = c("away_team", "home_team")) %>%
  dplyr::filter(!is.na(pick)) %>%
  dplyr::mutate(
    week = as.numeric(week),
    away_team = nflreadr::clean_team_abbrs(away_team),
    home_team = nflreadr::clean_team_abbrs(home_team),
    pick = nflreadr::clean_team_abbrs(pick),
    matchup = paste0(away_team, "@", home_team)
  ) %>%
  dplyr::left_join(
    nflreadr::load_schedules(seasons = new_season_sept) %>%
      dplyr::mutate(matchup = paste0(away_team, "@", home_team)) %>%
      dplyr::select(week, matchup, spread_line),
    by = c("week", "matchup")
  ) %>%
  dplyr::mutate(
    picked_on_favorite = dplyr::case_when(
      (spread_line < 0 & pick == away_team) | (spread_line > 0 & pick == home_team) ~ 1,
      TRUE ~ 0
    )
  )

unique_picks <- nfl_pool_pick_data %>%
  filter(franchise_id %in% c("0017", "0026", "0022", "0009", "0001")) %>%
  dplyr::group_by(matchup, pick) %>%
  dplyr::mutate(
    unique_matchup_picks = n(),
    unique_pick = ifelse(unique_matchup_picks == 1, 1, 0)
  )

  dplyr::group_by(franchise_id) %>%
  dplyr::summarise(
    total_picks = n(),
    picks_on_favorite = sum(picked_on_favorite, na.rm = TRUE),
    pct_picks_on_favorite = round(picks_on_favorite / total_picks * 100, 2)
  ) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  )

