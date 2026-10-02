rfl_roster_data <- read_data_table("rfl_roster_data")

rfl_current_roster <- rfl_roster_data %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::filter(week == max(week))

rfl_ir_data <- read_data_table("rfl_ir_data")
