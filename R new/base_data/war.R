rfl_war_data <- feather::read_feather("data/rfl_war_data.feather")

roster_war <- rfl_roster_data %>%
  dplyr::filter(week == max(week)) %>%
  dplyr::left_join(
    rfl_war_data %>%
      dplyr::filter(season == max(season)),
    by = c("player_id", "season")
  ) %>%
  dplyr::left_join(
    rfl_starter_data %>%
      dplyr::filter(starter_status == "starter" & season == max(season)) %>%
      dplyr::mutate(player_id = as.character(player_id)) %>%
      dplyr::group_by(franchise_id, player_id) %>%
      dplyr::summarise(starts = dplyr::n(), .groups = "drop"),
    by = c("player_id", "franchise_id")
  )
