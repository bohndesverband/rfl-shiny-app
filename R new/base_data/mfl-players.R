## player data ----
mfl_players <- feather::read_feather("data/mfl_players.feather") %>%
  dplyr::left_join(
    rfl_current_roster %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(franchise_ids = paste(franchise_id, collapse = ", "), .groups = "drop"),
    by = "player_id"
  )
