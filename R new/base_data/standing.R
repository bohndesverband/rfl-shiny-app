rfl_standing_data <- read_data_table("rfl_standing_data")
rfl_schedule_sim_summary <- read_data_table("rfl_po_prob")

rfl_current_standing <- rfl_standing_data %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::filter(week == max(week)) %>%
  dplyr::left_join(
    rfl_schedule_sim_summary,
    by = c("season", "week", "franchise_id")
  )

rfl_latest_standings <- rfl_standing_data %>%
  dplyr::group_by(season) %>%
  dplyr::filter(week == max(week)) %>%
  dplyr::ungroup()
