pbp <- nflreadr::load_pbp(seasons = 2026) %>%
  dplyr::filter(week == 2 & penalty == 0)

test <- pbp %>%
  #filter(play_id == 137) %>%
  dplyr::mutate(
    player_id = apply(
      dplyr::pick(dplyr::ends_with("player_id")),
      1,
      \(x) paste(unique(stats::na.omit(x)), collapse = ", ")
    )
  ) %>%
  tidyr::separate_rows(player_id, sep = ", ") %>%
  dplyr::mutate(
    fpts_kick = dplyr::case_when(
      field_goal_attempt == 1 & field_goal_result == "made" ~ kick_distance * 0.1,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 25 ~ -2,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 35 ~ -1,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 50 ~ -0.5,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 60 ~ -0.25,
      extra_point_result == "good" ~ 1,
      extra_point_result == "failed" ~ -1
    ),
    fpts_2pt = dplyr::if_else(!is.na(two_point_conv_result) & two_point_conv_result == "success", 2, 0),
    fpts_blocked = dplyr::case_when(
      field_goal_attempt == 1 & field_goal_result == "blocked" ~ 2,
      punt_blocked == 1 ~ 2,
      extra_point_result == "blocked" ~ 1
    ),
    fpts = dplyr::case_when(
      player_id == passer_player_id ~ passing_yards * 0.04 + (touchdown * 4) + (interception * -1) + fpts_2pt,
      player_id == receiver_player_id ~ receiving_yards * 0.1 + (touchdown * 6) + complete_pass + fpts_2pt,
      player_id == rusher_player_id ~ rushing_yards * 0.1 + (touchdown * 6) + fpts_2pt,
      player_id == kicker_player_id ~ fpts_kick,
      player_id == punt_returner_player_id ~ return_yards * 0.05 + (touchdown * 6),
      player_id == kickoff_returner_player_id ~ return_yards * 0.03 + (touchdown * 6),
      player_id == blocked_player_id ~ fpts_blocked,
      player_id == forced_fumble_player_1_player_id ~ fumble_forced * 3,
      player_id == solo_tackle_1_player_id | player_id == solo_tackle_2_player_id ~ solo_tackle * 2,
      player_id == assist_tackle_1_player_id | player_id == assist_tackle_2_player_id | player_id == assist_tackle_3_player_id | player_id == assist_tackle_4_player_id ~ assist_tackle,
      player_id == interception_player_id ~ 4,
      player_id == pass_defense_1_player_id | player_id == pass_defense_2_player_id ~ 2,
      (player_id == fumbled_1_player_id | player_id == fumbled_2_player_id) & fumble_lost == 1 ~ -1,
      (player_id == fumble_recovery_1_player_id & fumbled_1_team != fumble_recovery_1_team) | (player_id == fumble_recovery_2_player_id & fumbled_2_team != fumble_recovery_2_team) ~ 3,
      player_id == sack_player_id ~ 4,
      player_id == half_sack_1_player_id | player_id == half_sack_2_player_id ~ 2,
      player_id == safety_player_id ~ 2
    )
  ) %>%
  dplyr::select(gsis_id = player_id, fpts, play_id, desc) %>%
  dplyr::filter(!is.na(fpts)) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(gsis_id, mfl_id, name),
    by = "gsis_id"
  )

# TODO: fumble rules prüfen nachdem pos der spieler bekannt ist
# TODO: tackle numbers checken
# fumbles lost checken

