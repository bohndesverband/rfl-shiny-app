rfl_trades_data <- feather::read_feather("data/rfl_trades_data.feather")

rfl_trade_history <- feather::read_feather("data/rfl_trade_history_data.feather")

#rfl_transactions_draft <- feather::read_feather("data/rfl_transactions_draft_data.feather")

# nötig für trades in draftklassen
#rfl_trade_assets_per_side <- rfl_transactions_draft %>%
  #filter(trade_id == "2025025") %>%
#  dplyr::group_by(trade_id, side, franchise_id) %>%
#  dplyr::summarise(
#    date = first(date),
#    assets = paste(asset_name, collapse = "\n"),
#    .groups = "drop"
#  )

rfl_transactions_data <- feather::read_feather("data/rfl_transactions_data.feather")
