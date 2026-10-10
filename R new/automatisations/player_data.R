# rfl players ----
rfl_player_data <- rfl_player_scores %>%
  dplyr::distinct() %>%
  dplyr::select(-team) %>%
  #filter(player_id == "11192") %>%
  group_by(player_id, season) %>%
  tidyr::complete(week = 1:17) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::left_join(
    nflreadr::load_rosters_weekly(2016:new_season_sept) %>%
      dplyr::select(season, week, gsis_id, team, status),
    by = c("season", "week", "gsis_id"),
    multiple = "last"
  ) %>%
  dplyr::group_by(season, player_id) %>%
  dplyr::arrange(week) %>%
  tidyr::fill(team, status) %>%
  dplyr::ungroup() %>%

  ## end of season war
  dplyr::left_join(
    rfl_war_data %>%
      dplyr::group_by(season, player_id) %>%
      dplyr::arrange(week) %>%
      dplyr::summarise(war_end_of_season = last(war), .groups = "drop"),
    by = c("season", "player_id"),
  ) %>%

  ## running career war
  dplyr::left_join(
    rfl_war_data %>%
      dplyr::group_by(player_id, season) %>%
      dplyr::arrange(player_id, week) %>%
      dplyr::mutate(first_week = ifelse(week == min(week), 1, 0)) %>%
      dplyr::mutate(
        war_week = ifelse(first_week == 1, war, war - dplyr::lag(war))
      ) %>%
      dplyr::group_by(player_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::mutate(war_career = round(cumsum(war_week), 2)) %>%
      dplyr::group_by(season, week, pos) %>%
      dplyr::mutate(war_career_pctl = round(dplyr::percent_rank(war_career), 2)) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, player_id, war, war_pctl, war_week, war_career, war_career_pctl, games_missed, pvar),
    by = c("season", "week", "player_id")
  ) %>%

  # add date from week
  dplyr::left_join(
    nflreadr::load_schedules(2016:season_before_wk_2) %>%
      dplyr::group_by(season, week) %>%
      dplyr::arrange(gameday) %>%
      dplyr::mutate(date = last(gameday)) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, game_id, date, away_team, home_team) %>%
      tidyr::uncount(2) %>%
      dplyr::group_by(game_id) %>%
      dplyr::mutate(
        team = ifelse(dplyr::row_number() == 1, away_team, home_team),
        opponent = ifelse(dplyr::row_number() == 1, home_team, away_team),
        opponent_side = ifelse(dplyr::row_number() == 1, paste0("@", opponent), opponent)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-game_id, -away_team, -home_team),
    by = c("season", "week", "team"),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    dplyr::across(c(opponent, opponent_side), ~ ifelse(is.na(.x), "BYE", .x))
  ) %>%

  # add running ppg
  dplyr::left_join(
    rfl_player_scores %>%
      dplyr::mutate(team = nflreadr::clean_team_abbrs(team)) %>%
      dplyr::group_by(player_id, season) %>%
      dplyr::arrange(week) %>%
      dplyr::mutate(ppg_running = round(cummean(points), 2)) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, player_id, team, ppg_running),
    by = c("season", "week", "player_id", "team")
  ) %>%

  # add fantasy finishes
  # add fantasy points
  dplyr::left_join(
    rfl_fantasy_finishes_weekly %>%
      dplyr::select(-player_name:-team, -pos_grouped, -points:-points_ppg_diff),
    by = c("season", "week", "player_id")
  ) %>%

  dplyr::group_by(player_id, season) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    points = as.numeric(points),
    fpts_running = cumsum(dplyr::coalesce(points, 0)),
    ppg_running_diff = round(points - ppg_running, 2)
  ) %>%
  dplyr::ungroup() %>%

  dplyr::left_join(
    rfl_fantasy_finishes_season %>%
      dplyr::select(season, player_id, pos_rank_season = pos_rank, dplyr::ends_with("_season"), -points_season),
    by = c("player_id", "season"),
    relationship = "many-to-many"
  ) %>%

  # add elo
  ## add weekly elo
  dplyr::left_join(
    rfl_player_elo %>%
      dplyr::select(-position, -team, -score_diff, -dplyr::starts_with("opponent"), -gsis_id, -score, -display_name),
    by = c("season", "week", "player_id" = "mfl_id")
  ) %>%

  ## add current elo
  dplyr::left_join(
    rfl_player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::summarise(
        #position_last = last(position),
        player_elo_current = last(player_elo_post),
        player_elo_current_pctl = last(player_elo_post_pctl),
        player_elo_max = max(player_elo_post)
      ),
    by = c("player_id" = "mfl_id")
  ) %>%
  #dplyr::filter(season != max(season) | week < current_week) %>%

  # fülle zeilen mit fehlenden daten
  dplyr::group_by(player_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    game = dplyr::row_number(),
    display_name = nflreadr::clean_player_names(player_name)
  ) %>%
  dplyr::select(season, week, date, reg_season, player_id, player_name, display_name, pos, pos_grouped, team, game, opponent, opponent_side, status, fpts = points, ppg_running, pos_rank, fpts_running, ppg_running_diff, war, war_pctl, war_week, war_end_of_season, war_career, war_career_pctl, pvar, pos_rank_season, dplyr::starts_with("top"), player_elo_pre, player_elo_pre_pctl, player_elo_post, player_elo_post_pctl, player_elo_post_pctl_week, elo_shift, player_elo_season_end = elo_season_end, player_elo_current, player_elo_current_pctl) %>%
  dplyr::group_by(player_id, season) %>%
  dplyr::arrange(week) %>%
  tidyr::fill(ppg_running, war, war_pctl) %>%
  tidyr::fill(player_name, display_name, pos, pos_grouped, team, ppg_running, pvar, player_elo_pre:player_elo_season_end, .direction = "downup") %>%
  dplyr::ungroup()

DBI::dbWriteTable(con, "rfl_player_data", rfl_player_data, overwrite = TRUE)

# mfl players ----
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
  ) %>%
  dplyr::left_join(
    rfl_player_data %>%
      dplyr::group_by(player_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::select(player_id, player_elo_post),
    by = "player_id"
  )

DBI::dbWriteTable(con, "mfl_players", mfl_players, overwrite = TRUE)
