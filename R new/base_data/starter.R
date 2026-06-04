rfl_starter_data <- feather::read_feather("data/rfl_starter_data.feather")

rfl_starter_ppg_fpts_diff <- rfl_starter_data %>%
  dplyr::filter(starter_status == "starter") %>%
  dplyr::group_by(season, week, franchise_id) %>%
  dplyr::summarise(
    franchise_score = sum(player_score, na.rm = TRUE),
    franchise_ppg_score = sum(ppg, na.rm = TRUE),
    franchise_points_ppg_diff = round(sum(points_ppg_diff, na.rm = TRUE), 2),
    .groups = "drop"
  )
