# rfl players ----
rfl_player_data <- rfl_player_scores %>%
  #filter(season == 2026) %>%
  dplyr::mutate(
    display_name = nflreadr::clean_player_names(player_name)
  ) %>%
  dplyr::select(season:player_name, display_name, pos, pos_grouped, team) %>%

  # add fantasy finishes
  # add fantasy points
  dplyr::left_join(
    rfl_fantasy_finishes_weekly %>%
      dplyr::select(-player_name:-team, -pos_grouped),
    by = c("season", "week", "player_id")
  ) %>%
  dplyr::group_by(player_id, season) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    fpts_running = cumsum(points),
    ppg_running = round(cummean(points), 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(fpts = points) %>%
  dplyr::select(season, week, reg_season, player_id:fpts, fpts_running, ppg_running, pos_rank:top60_weekly) %>%

  dplyr::left_join(
    rfl_fantasy_finishes_season %>%
      dplyr::select(season, player_id, ppg, pos_rank_season = pos_rank, dplyr::ends_with("_season"), -points_season),
    by = c("player_id", "season"),
    relationship = "many-to-many"
  ) %>%
  dplyr::select(season:fpts, ppg, fpts_running, ppg_running, pos_rank:top60_season) %>%

  # add war
  ## weekly
  dplyr::left_join(
    rfl_war_data %>%
      dplyr::select(-pos, -points),
    by = c("season", "week", "player_id")
  ) %>%
  ## current war
  dplyr::left_join(
    rfl_war_data %>%
      dplyr::group_by(season, player_id) %>%
      dplyr::arrange(week) %>%
      dplyr::summarise(war_end_of_season = last(war), .groups = "drop"),
    by = c("season", "player_id")
  ) %>%
  ## running career war

  dplyr::left_join(
    rfl_war_data %>%
      dplyr::arrange(player_id, season, week) %>%
      dplyr::group_by(player_id, season) %>%
      dplyr::mutate(first_week = ifelse(week == min(week), 1, 0)) %>%
      dplyr::mutate(
        war_week = ifelse(first_week == 1, war, war - dplyr::lag(war))
      ) %>%
      dplyr::group_by(player_id) %>%
      dplyr::mutate(war_career = round(cumsum(war_week), 2)) %>%
      dplyr::group_by(season, week, pos) %>%
      dplyr::mutate(war_career_pctl = round(dplyr::percent_rank(war_career), 2)) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, player_id, war_week, war_career, war_career_pctl),
    by = c("season", "week", "player_id")
  ) %>%

  # add elo
  ## add weekly elo
  dplyr::left_join(
    player_elo %>%
      dplyr::select(-position, -team, -score_diff, -dplyr::starts_with("opponent"), -gsis_id, -score),
    by = c("season", "week", "player_id" = "mfl_id")
  ) %>%
  dplyr::select(season:team, games_played, games_missed, everything()) %>%

  ## add current elo
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::summarise(
        #position_last = last(position),
        player_elo_current = last(player_elo_post),
        player_elo_current_pctl = last(player_elo_post_pctl),
        player_elo_max = max(player_elo_post)
      ),
    by = c("player_id" = "mfl_id")
  )

DBI::dbWriteTable(con, "rfl_player_data", rfl_player_data, overwrite = TRUE)

# mfl players ----
## player data ----
mfl_players <- jsonlite::read_json(paste0(mfl_api_base_march, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1")) %>%
  purrr::pluck("players", "player") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    player_name = name,
    pos = position,
    player_id = id
  ) %>%
  dplyr::filter(!grepl("TM", pos), !pos %in% c("Def", "ST", "Off", "Coach", "PN")) %>%
  dplyr::mutate(
    grouped_pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    ),
    display_name = nflreadr::clean_player_names(player_name)
  )

DBI::dbWriteTable(con, "mfl_players", mfl_players, overwrite = TRUE)

