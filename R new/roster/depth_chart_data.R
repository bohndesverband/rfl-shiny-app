rfl_transactions_history <- rfl_transactions_data %>%
  dplyr::mutate(player_id = as.character(player_id)) %>%
  dplyr::bind_rows(
    rfl_trades_data %>%
      dplyr::mutate(
        type = "TRADED",
        type_desc = "traded",
      ) %>%
      dplyr::rename(player_id = asset_id) %>%
      dplyr::select(season, timestamp, type, type_desc, franchise_id, player_id, trade_partner)
  ) %>%
  dplyr::arrange(timestamp)
