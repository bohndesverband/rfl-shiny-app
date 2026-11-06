rfl_standing_data <- read_data_table("rfl_standing_data")

rfl_current_standing <- rfl_standing_data %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::filter(week == max(week))

rfl_latest_standings <- rfl_standing_data %>%
  dplyr::group_by(season) %>%
  dplyr::filter(week == max(week)) %>%
  dplyr::ungroup()
