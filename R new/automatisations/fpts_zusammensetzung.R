nfl_player_stats <- nflreadr::load_player_stats(summary_level = "reg") %>%
  #filter(player_id == "00-0037839") %>%
  dplyr::left_join(
    nflreadr::load_pbp() %>%
      dplyr::filter(special_teams_play == "1") %>%
      dplyr::mutate(play_id = paste(game_id, play_id, sep = "_")) %>%
      dplyr::select(play_id, solo_tackle, assist_tackle, solo_tackle_1_player_id, solo_tackle_2_player_id, assist_tackle_1_player_id, assist_tackle_2_player_id, assist_tackle_3_player_id, assist_tackle_4_player_id, tackle_with_assist_1_player_id, tackle_with_assist_2_player_id) %>%
      # Alle Tackles in einer Zeile zusammenfassen
      tidyr::pivot_longer(cols = c(solo_tackle_1_player_id, solo_tackle_2_player_id, assist_tackle_1_player_id, assist_tackle_2_player_id, assist_tackle_3_player_id, assist_tackle_4_player_id, tackle_with_assist_1_player_id, tackle_with_assist_2_player_id), names_to = "tackle_type", values_to = "player_id") %>%
      dplyr::filter(!is.na(player_id)) %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(
        st_solo_tackles = sum(solo_tackle, na.rm = TRUE),
        st_assist_tackles = sum(assist_tackle, na.rm = TRUE),
      ) %>%
      dplyr::ungroup(),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    st_solo_tackles = ifelse(is.na(st_solo_tackles), 0, st_solo_tackles),
    st_assist_tackles = ifelse(is.na(st_assist_tackles), 0, st_assist_tackles),
    fpts_tds = (passing_tds * 4) + ((rushing_tds + receiving_tds + special_teams_tds + def_tds) * 6) + ((passing_2pt_conversions + rushing_2pt_conversions + receiving_2pt_conversions + def_safeties) * 2),
    fpts_yds = (passing_yards * 0.04) + (rushing_yards * 0.1) + (receiving_yards * 0.1) + (kickoff_return_yards * 0.03) + (punt_return_yards * 0.05),
    fpts_receptions = receptions,
    fpts_fmbl_recovery = ifelse(position %in% c("QB", "RB", "WR", "TE"), fumble_recovery_opp, fumble_recovery_opp * 3),
    fpts_negative = (passing_interceptions + sack_fumbles_lost + rushing_fumbles_lost + receiving_fumbles_lost) * -1,
    fpts_turnover = (def_interceptions * 4) + (def_fumbles_forced * 3) + (fg_blocked * 2) + pat_blocked,
    fpts_pass_breakups = def_pass_defended * 2,
    fpts_tackles = ((def_tackles_solo + def_tackles_with_assist) * 2) + def_tackle_assists - (st_solo_tackles * 2) - st_assist_tackles,
    fpts_sacks = def_sacks * 4,
    fpts = fpts_tds + fpts_yds + fpts_receptions + fpts_fmbl_recovery - fpts_negative + fpts_turnover + fpts_pass_breakups + fpts_tackles + fpts_sacks,
    fpts = ifelse(position %in% c("QB", "RB", "WR", "TE"), fpts - fpts_tackles, fpts)
  ) %>%
  dplyr::select(player_id, dplyr::starts_with("fpts"))

fpts_zusammensetzung <- rfl_fantasy_finishes_season %>%
  dplyr::filter(pos != "PK") %>%
  dplyr::filter(season == new_season_sept) %>%
  dplyr::select(season:pos_rank) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::left_join(
    nfl_player_stats,
    by = c("gsis_id" = "player_id")
  ) %>%
  dplyr::select(-gsis_id) %>%
  dplyr::mutate(
    # Erstelle eine Liste mit den fpts_type und fpts_value für gt_plt_bar_stack
    fpts_breakdown = purrr::pmap(
      list(
        round((fpts_tds + fpts_sacks + fpts_turnover + fpts_fmbl_recovery) / fpts, 1),
        round((fpts_yds + fpts_tackles) / fpts, 1),
        round((fpts_receptions + fpts_pass_breakups) / fpts, 1)
      ),
      ~ c(`TDs/Sacks` = ..1, `Yds` = ..2, `Rec/Tkl+PB` = ..3)
    ),
    subline = paste(pos, team, sep = ", ")
  ) %>%
  dplyr::filter(!is.na(points))

saveRDS(fpts_zusammensetzung, "data/fpts_zusammensetzung.rds")
