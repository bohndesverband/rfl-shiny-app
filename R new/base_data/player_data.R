if (!rfl_player_data_loaded()) {
  rfl_player_data <- read_data_table("rfl_player_data")

  rfl_player_data_latest <- rfl_player_data %>%
    dplyr::group_by(player_id) %>%
    dplyr::arrange(season, week) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  #rfl_player_data_loaded(TRUE)
}
