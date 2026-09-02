player_ppg <- player_elo %>%
  dplyr::group_by(season, mfl_id) %>%
  dplyr::summarise(
    fpts = sum(score),
    games = n(),
    ppg = round(fpts / games, 2),
    .groups = "drop"
  )
