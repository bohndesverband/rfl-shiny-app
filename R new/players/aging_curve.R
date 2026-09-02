current_season <- nflreadr::most_recent_season()

# offense stats
player_stats <- nflreadr::load_player_stats(current_season:(current_season - 20), summary_level = "reg")

fpts <- player_stats %>%
  dplyr::mutate(
    position = dplyr::case_when(
      position %in% c("DT", "DE", "OLB", "NT", "DL") ~ "DL",
      position %in% c("ILB", "LB", "MLB") ~ "LB",
      position %in% c("CB", "DB", "FS", "SS", "SAF", "S") ~ "DB",
      position == "K" ~ "PK",
      TRUE ~ position
    )
  ) %>%
  dplyr::filter(
    position %in% c("QB", "RB", "WR", "TE", "DL", "LB", "DB", "PK")
  ) %>%
  dplyr::mutate_if(is.numeric , replace_na, replace = 0) %>%
  dplyr::mutate(
    fpts = (passing_yards * 0.04) +
      (passing_tds * 4) -
      passing_interceptions +
      (rushing_yards * 0.1) +
      (rushing_tds * 6) +
      (rushing_2pt_conversions * 2) +
      receptions +
      (receiving_yards * 0.1) +
      (receiving_tds * 6) +
      (receiving_2pt_conversions * 2) -
      (sack_fumbles_lost +
         rushing_fumbles_lost +
         receiving_fumbles_lost) +
      (def_tackles_solo * 2) +
      def_tackle_assists +
      (def_fumbles_forced * 3) +
      (def_sacks * 4) +
      (def_interceptions * 4) +
      (def_pass_defended * 2) +
      (def_tds * 6) -
      def_fumbles +
      (fumble_recovery_opp * 3) +
      (def_safeties * 2) +
      (special_teams_tds * 6) +
      (punt_return_yards * 0.05) +
      (kickoff_return_yards * 0.03) +
      (fg_made_distance * 0.1) +
      pat_made -
      pat_missed -
      ((fg_missed_0_19 + fg_missed_20_29) * -2) -
      fg_missed_30_39 -
      (fg_missed_40_49 * -0.5) -
      ((fg_missed_50_59 + fg_missed_60_) * -0.25)
  ) %>%
  dplyr::select(season, player_id, position, games, fpts) %>%
  dplyr::left_join(
    nflreadr::load_players() %>%
      dplyr::select(gsis_id, years_of_experience, draft_round),
    by = c("player_id" = "gsis_id")
  ) %>%
  dplyr::filter(
    (season - years_of_experience) > current_season - 20 & years_of_experience > 0 & !is.na(draft_round)
  ) %>%
  dplyr::mutate(
    #draft_round = ifelse(is.na(draft_round), 8, draft_round),
    ppg = fpts / games,
  )

avg <- fpts %>%
  dplyr::group_by(position, years_of_experience) %>%
  dplyr::summarise(
    #fpts = sum(fpts, na.rm = TRUE),
    avg_ppg = mean(ppg, na.rm = TRUE),
    .groups = "drop"
  )
  #dplyr::group_by(position) %>%
  #dplyr::mutate(
    #career_ppg = mean(ppg),
    #avg_ppg_pct = ppg / career_ppg
  #)

players <- player_elo %>%
  filter(mfl_id == "9431") %>%
  dplyr::group_by(mfl_id, gsis_id, season) %>%
  dplyr::summarise(
    position = last(position),
    season_ppg = mean(score, na.rm = TRUE),
    elo_peak = max(player_elo_post, na.rm = TRUE),
    current_elo = last(player_elo_post),
    .groups = "drop"
  ) %>%
  dplyr::arrange(mfl_id, gsis_id) %>%
  dplyr::mutate(
    career_ppg = round(mean(season_ppg, na.rm = TRUE), 2),
    season_peak = season[which.max(elo_peak)],
    elo_peak = max(elo_peak, na.rm = TRUE),
    current_elo = current_elo[which.max(season)]
  ) %>%
  dplyr::left_join(
    fpts %>%
      dplyr::select(player_id, years_of_experience) %>%
      dplyr::distinct(),
    by = c("gsis_id" = "player_id")
  ) %>%
  dplyr::group_by(mfl_id) %>%
  tidyr::complete(season = (min(season)):(max(season))) %>%
  dplyr::mutate(
    #career_ppg_pct = ppg / career_ppg,
    years_of_experience = ifelse(season == max(season), years_of_experience, years_of_experience - (max(season) - season))
  ) %>%
  dplyr::left_join(
    avg,
    by = c("position", "years_of_experience")
  )

# Vorhersage für zukünftige Saisons basierend auf Aging Curve
predict_future_seasons <- function(players_df, years_ahead = 5) {

  # Aktuelles Jahr und maximale years_of_experience ermitteln
  current_year <- max(players_df$season, na.rm = TRUE)
  max_experience <- max(players_df$years_of_experience, na.rm = TRUE)

  # Zukünftige Saisons erstellen
  future_seasons <- data.frame(
    mfl_id = unique(players_df$mfl_id),
    season = (current_year + 1):(current_year + years_ahead),
    gsis_id = unique(players_df$gsis_id)[1],
    position = unique(players_df$position)[1],
    career_ppg = unique(players_df$career_ppg)[1]
    #elo_peak = unique(players_df$elo_peak)[1],
    #current_elo = unique(players_df$current_elo)[1],
    #season_peak = unique(players_df$season_peak)[1]
  ) %>%
    dplyr::mutate(
      years_of_experience = max_experience + (season - current_year)
    ) %>%
    dplyr::left_join(
      avg,
      by = c("position", "years_of_experience")
    )

  return(future_seasons)
}

# Vorhersage für den aktuellen Spieler erstellen
future_predictions <- predict_future_seasons(players, years_ahead = 5)

# Kombiniere historische Daten mit Vorhersagen
complete_player_data <- dplyr::bind_rows(
  players,
  future_predictions
) %>%
  dplyr::arrange(season) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::mutate(
    ppg_diff = mean(season_ppg, na.rm = TRUE) - mean(avg_ppg, na.rm = TRUE),
    season_ppg = ifelse(is.na(season_ppg), avg_ppg + ppg_diff, season_ppg)
  ) %>%
  dplyr::ungroup()


ggplot(avg, aes(x = years_of_experience, y = avg_ppg)) +
  facet_wrap(~ position) +
  ggalt::geom_xspline(spline_shape = -0.5) +
  ggalt::geom_xspline(data = subset(complete_player_data, season <= current_season), aes(y = season_ppg), color = "red", spline_shape = -0.5) +
  ggalt::geom_xspline(data = subset(complete_player_data, season >= current_season), aes(y = season_ppg), linetype = "dashed", color = "red", spline_shape = -0.5)

