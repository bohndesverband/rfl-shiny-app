# stats for calculating fantasy points
offense_stats <- nflreadr::load_player_stats((var.season - 20):var.season, "offense") %>%
  dplyr::filter(
    position %in% c("QB", "RB", "WR", "TE"),
    season_type == "REG"
  ) %>%
  dplyr::mutate_if(is.numeric , replace_na, replace = 0) %>%
  dplyr::mutate(
    fpts = (passing_yards * 0.04) + (passing_tds * 4) - interceptions + (rushing_yards * 0.1) + (rushing_tds * 6) + (rushing_2pt_conversions * 2) + receptions + (receiving_yards * 0.1) + (receiving_tds * 6) + (receiving_2pt_conversions * 2) - (sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost)
  ) %>%
  dplyr::select(season, week, player_id, position, fpts)

defense_stats <- nflreadr::load_player_stats((var.season - 20):var.season, "defense") %>%
  dplyr::mutate(
    position = dplyr::case_when(
      position %in% c("DT", "DE", "OLB", "NT", "DL") ~ "DL",
      position %in% c("ILB", "LB", "MLB") ~ "LB",
      position %in% c("CB", "DB", "FS", "SS", "SAF", "S") ~ "DB"
    )
  ) %>%
  dplyr::filter(position %in% c("DL", "LB", "DB"), season_type == "REG") %>%
  dplyr::mutate_if(is.numeric , replace_na, replace = 0) %>%
  dplyr::mutate(
    fpts = (def_tackles_solo * 2) + def_tackle_assists + (def_fumbles_forced * 3) + (def_sacks * 4) + (def_interceptions * 4) + (def_pass_defended * 2) + (def_tds * 6) - def_fumbles + (def_fumble_recovery_opp * 3) + (def_safety * 2)
  ) %>%
  dplyr::select(season, week, player_id, position, fpts)

kicking_stats <- nflreadr::load_player_stats((var.season - 20):var.season, "kicking") %>%
  dplyr::filter(position == "K", season_type == "REG") %>%
  dplyr::mutate_if(is.numeric , replace_na, replace = 0) %>%
  dplyr::mutate(
    fpts = (fg_made_distance) * 0.1 + ((fg_missed_0_19 + fg_missed_20_29) * -2) + (fg_missed_30_39 * -1) + (fg_missed_40_49 * -0.5) + ((fg_missed_50_59 + fg_missed_60_) * -0.25) + pat_made - pat_missed
  ) %>%
  dplyr::select(season, week, player_id, position, fpts)

# combine fantasy points
fpts <- rbind(offense_stats, defense_stats, kicking_stats) %>%
  dplyr::left_join(
    nflreadr::load_players() %>%
      dplyr::select(gsis_id, entry_year),
    by = c("player_id" = "gsis_id")
  ) %>%
  dplyr::filter(!is.na(entry_year))
#week <= 16

# calculating ppg
ppg <- fpts %>%
  dplyr::left_join(
    nflreadr::load_draft_picks() %>%
      dplyr::select(gsis_id, round),
    by = c("player_id" = "gsis_id")
  ) %>%
  dplyr::mutate(round = ifelse(is.na(round), 8, round)) %>%
  dplyr::group_by(season, player_id, position, entry_year, round) %>%
  dplyr::summarise(
    games = n(),
    fpts = sum(fpts, na.rm = TRUE),
    ppg = sum(fpts, na.rm = TRUE) / games,
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    multi_games = games / max(games),
    experience = season - entry_year + 1
  ) %>%
  dplyr::select(-entry_year, -games, -fpts)

experience <- ppg %>%
  dplyr::group_by(position, round, experience) %>%
  dplyr::summarise(
    players_w_same_experience = n(),
    .groups = "drop"
  ) %>%
  dplyr::group_by(position) %>%
  dplyr::arrange(round, experience) %>%
  dplyr::mutate(
    multi_experience = players_w_same_experience / max(players_w_same_experience),
    multi_experience_n5 = round((dplyr::lead(multi_experience, 1, default = 0) + dplyr::lead(multi_experience, 2, default = 0) + dplyr::lead(multi_experience, 3, default = 0) + dplyr::lead(multi_experience, 4, default = 0) + dplyr::lead(multi_experience, 5, default = 0)) / 5, 4),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-players_w_same_experience, -multi_experience)

aging_curves <- ppg %>%
  dplyr::group_by(position, experience, round) %>%
  dplyr::summarise(ppg = mean(ppg), .groups = "drop") %>%
  dplyr::group_by(position, round) %>%
  dplyr::mutate(
    career_ppg = mean(ppg),
    career_ppg_pct = ppg / career_ppg
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(position, round) %>%
  dplyr::arrange(experience) %>%
  dplyr::mutate(
    multi_n5 = round((dplyr::lead(career_ppg_pct, 1, default = 0) + dplyr::lead(career_ppg_pct, 2, default = 0) + dplyr::lead(career_ppg_pct, 3, default = 0) + dplyr::lead(career_ppg_pct, 4, default = 0) + dplyr::lead(career_ppg_pct, 5, default = 0)) / 5, 4),
    multi_n5 = ifelse(multi_n5 == 0, lag(multi_n5, 1), multi_n5) # wenn maximum erreicht ist, nehme letzten wert anstatt 0
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(position, experience, round, multi_n5)

# positional value
positional_value <- starter %>%
  dplyr::filter(starter_status == "starter") %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(season, week, franchise_id, player_id) %>%
  dplyr::filter(dplyr::row_number() == 1) %>% # entferne doppelte spieler von double header matchups
  dplyr::group_by(season, week, franchise_id) %>%
  dplyr::mutate(franchise_points = sum(player_score, na.rm = TRUE)) %>%
  dplyr::group_by(season, week, franchise_id, pos) %>%
  dplyr::mutate(
    position_count = n(),
    position_score = sum(player_score, na.rm = TRUE),
    position_value = position_score / franchise_points
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::summarise(
    dplyr::across(position_count:position_value, \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  )

player_values <- rbind(
  roster %>%
    dplyr::select(player_id),
  rfl_drafts_data %>%
    select(mfl_id) %>%
    dplyr::rename(player_id = mfl_id)
  ) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::filter(!is.na(gsis_id)) %>%
  dplyr::left_join(
    nflreadr::load_players() %>%
      dplyr::select(gsis_id, display_name, entry_year),
    by = "gsis_id"
  ) %>%
  dplyr::left_join(
    nflreadr::load_draft_picks() %>%
      dplyr::select(gsis_id, round),
    by = "gsis_id"
  ) %>%
  dplyr::mutate(
    round = ifelse(is.na(round), 8, round),
    experience = var.season - entry_year + 1
  ) %>%
  dplyr::left_join(
    ppg %>%
      dplyr::group_by(player_id, position) %>%
      dplyr::summarise(
        multi_games = mean(multi_games, na.rm = TRUE),
        career_ppg = mean(ppg, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("gsis_id" = "player_id")
  ) %>%
  dplyr::left_join(
    ppg %>%
      dplyr::filter(season == var.season) %>%
      dplyr::select(player_id, ppg) %>%
      dplyr::rename(ppg_last_season = ppg),
    by = c("gsis_id" = "player_id")
  ) %>%
  dplyr::select(-entry_year, -gsis_id) %>%
  dplyr::left_join(
    aging_curves,
    by = c("position", "experience", "round")
  ) %>%
  dplyr::left_join(
    experience,
    by = c("position", "experience", "round")
  ) %>%
  dplyr::filter(!is.na(multi_n5)) %>%
  dplyr::left_join(
    positional_value %>%
      dplyr::select(pos, position_value),
    by = c("position" = "pos")
  ) %>%
  dplyr::mutate(
    proj_ppg = (((ppg_last_season * 2) + career_ppg) / 3) * multi_n5,
    proj_fpts = proj_ppg * 16 * 5 * position_value * (multi_experience_n5 + multi_games)
    # TODO: Vertragslänge
    # TODO: avg ausfallzeit einzelner positionen als multiplier hinzufügen
    # TODO: avg karriere dauer. derzeit reißen wenige einzelspieler die aging curve spät in der karriere nach oben
  )

pick_values <- rfl_drafts_data %>%
  dplyr::left_join(
    player_values %>%
      dplyr::select(player_id, career_ppg),
    by = c("mfl_id" = "player_id")
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(dplyr::desc(career_ppg), mfl_id) %>%
  dplyr::mutate(new_order = row_number()) %>%
  dplyr::group_by(new_order) %>%
  dplyr::reframe(pick_value = mean(career_ppg, na.rm = TRUE) * 10) %>%
  dplyr::mutate(pick_value = ifelse(is.na(pick_value) | pick_value < 0, 0, pick_value)) %>%
  dplyr::arrange(dplyr::desc(pick_value)) %>%
  dplyr::mutate(pick = row_number()) %>%
  dplyr::select(-new_order)

trade_values <- rbind(
  player_values %>%
    dplyr::mutate(
      asset = paste0(display_name, " (", position, ")"),
    ) %>%
    dplyr::rename(value = proj_fpts) %>%
    dplyr::select(asset, value),
  pick_values %>%
    dplyr::rename(asset = pick, value = pick_value) %>%
    select(asset, value)
)
