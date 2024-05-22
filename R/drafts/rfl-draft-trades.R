draft_start <- as.numeric(as.POSIXct("2024-05-10 20:00:00 MEZ"))
#draft_end

rfl_draft_trades <- trades %>%
  dplyr::filter(timestamp >= draft_start) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
    dplyr::mutate(
      asset_type = ifelse(grepl("DP_", asset_id), "pick", "player"),
    ) %>%
    dplyr::group_by(trade_id) %>%
    dplyr::mutate(
      asset_types = paste(asset_type, collapse = ","),
      asset_ids = paste(asset_id, collapse = ","),
      franchise_ids = paste(franchise_id, collapse = ",")
    ) %>%
  dplyr::group_by(trade_id, trade_side, asset_types) %>%
  dplyr::summarise(
    date = dplyr::first(date),
    asset_ids = dplyr::first(asset_ids),
    asset_types = dplyr::first(asset_types),
    franchise_ids = dplyr::first(franchise_ids),
    asset_names = paste(asset_name, collapse = "--"),
    franchise_name = dplyr::first(franchise_name),
    .groups = "drop"
  ) %>%
  dplyr::select(trade_id, date, trade_side, asset_names, franchise_name) %>%
  tidyr::spread(trade_side, asset_names) %>%
  dplyr::group_by(trade_id) %>%
  dplyr::arrange(franchise_1) %>%
  dplyr::mutate(
    franchise_gets = ifelse(is.na(franchise_2), lead(franchise_2), lag(franchise_1)),
    franchise_sends = ifelse(is.na(franchise_1), franchise_2, franchise_1),
  ) %>%
  dplyr::select(-franchise_1, -franchise_2) %>%
  dplyr::ungroup()
  #filter(franchise_name == "Jena Dragons")

draft_trades_get <- rfl_draft_trades %>%
  dplyr::select(franchise_name, franchise_gets) %>%
  tidyr::separate_rows(franchise_gets, sep = "--") %>%
  dplyr::group_by(franchise_name) %>%
  dplyr::arrange(franchise_gets) %>%
  dplyr::summarise(
    franchise_gets = paste(franchise_gets, collapse = "\n"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    franchise_gets = paste0(franchise_name, " gets\n", franchise_gets),
  )

draft_trades_send <- rfl_draft_trades %>%
  dplyr::select(franchise_name, franchise_sends) %>%
  tidyr::separate_rows(franchise_sends, sep = "--") %>%
  dplyr::group_by(franchise_name) %>%
  dplyr::arrange(franchise_sends) %>%
  dplyr::summarise(
    franchise_sends = paste(franchise_sends, collapse = "\n"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    franchise_sends = paste0(franchise_name, " sends\n", franchise_sends),
  )

draft_trades_combined <- draft_trades_get %>%
  dplyr::left_join(draft_trades_send, by = "franchise_name")
