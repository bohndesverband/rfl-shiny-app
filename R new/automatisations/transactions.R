rfl_trades_data <- purrr::map_df(2016:2026, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_{x}.csv"),
    col_types = "dddTdcccc"
  )
}) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pick_round = dplyr::case_when(
      grepl("FP_", asset_id) ~ stringr::str_split(asset_id, "_")[[1]][4],
      grepl("DP_", asset_id) ~ stringr::str_split(asset_id, "_")[[1]][2]
    ),
    team = dplyr::case_when(
      grepl("FP_", asset_id) ~ stringr::str_split(asset_id, "_")[[1]][2],
      grepl("DP_", asset_id) ~ franchise_id
    )
  ) %>%
  dplyr::left_join(
    feather::read_feather("data/rfl_franchises.feather") %>%
      dplyr::select(franchise_id, franchise_name),
    by = c("team" = "franchise_id")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    pick_round = ifelse(grepl("DP_", asset_id), as.numeric(pick_round) + 1, as.numeric(pick_round)),
    trade_asset_name = ifelse(grepl("FP_", asset_id), paste(asset_name, franchise_name), asset_name),
    trade_asset_id = ifelse(grepl("DP_", asset_id) | grepl("FP_", asset_id), paste0("DP_", pick_round), asset_id)
  ) %>%
  dplyr::select(-franchise_name, -team) %>%
  dplyr::group_by(trade_id) %>%
  dplyr::arrange(trade_side) %>%
  dplyr::mutate(
    trade_partner = ifelse(trade_side == "franchise_2", first(franchise_id), last(franchise_id)),
  ) %>%
  dplyr::arrange(trade_id) %>%
  dplyr::ungroup() %>%
  dplyr::distinct() %>%

  # füge draftorder zu trades mit picks hinzu
  dplyr::mutate(
    pick_owner = dplyr::case_when(
      grepl("FP", asset_id) ~ stringr::word(asset_id, 2, sep = "_")
    ),
    pick_year = dplyr::case_when(
      grepl("FP", asset_id) ~ as.double(stringr::word(asset_id, 3, sep = "_")),
      grepl("DP", asset_id) ~ season
    )
  ) %>%
  dplyr::left_join(
    rfl_draft_orders %>%
      dplyr::select(season, franchise_id, pick),
    by = c("pick_year" = "season", "pick_owner" = "franchise_id")
  ) %>%
  dplyr::mutate(
    draft_pick = dplyr::case_when(
      is.na(pick_owner) ~ as.double(stringr::word(asset_id, 3, sep = "_")) + 1,
      TRUE ~ as.double(stringr::str_pad(pick, 2, pad = "0"))
    )
  ) %>%
  dplyr::select(-pick) %>%

  # füge getätigte draftpicks an
  dplyr::left_join(
    rfl_drafts_data %>%
      dplyr::select(pick_year = season, pick_round = round, draft_pick = pick, player_name_with_info, pick_cat_badge),
    by = c("pick_year", "pick_round", "draft_pick")
  ) %>%
  dplyr::select(season:asset_name, trade_asset_name:pick_year, pick_round, everything())

feather::write_feather(rfl_trades_data, "data/rfl_trades_data.feather")

rfl_trade_history <- rfl_trades_data %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::mutate(
    asset_type = ifelse(grepl("DP_", asset_id), "pick", "player"),
    trade_asset_id = dplyr::case_when(
      pick_year <= new_season_march ~ paste("DP", pick_round, stringr::str_pad(draft_pick, 2, pad = "0"), pick_year, sep = "_"),
      TRUE ~ asset_id
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pos = stringr::str_split(gsub(".*\\(([^)]+)\\).*", "\\1", asset_name), ",")[[1]][1],
    asset_name_with_draft_info = ifelse(!is.na(player_name_with_info), paste(asset_name, player_name_with_info, sep = " - "), trade_asset_name),
    asset_name_with_draft_info_badge= ifelse(!is.na(pick_cat_badge), paste0(asset_name_with_draft_info, pick_cat_badge), asset_name_with_draft_info)
  ) %>%
  dplyr::group_by(trade_id) %>%
  dplyr::mutate(
    asset_types = paste(unique(asset_type), collapse = ","),
    asset_ids = paste(unique(trade_asset_id), collapse = ","),
    trade_asset_ids = paste(asset_id, collapse = ","),
    asset_positions = paste(unique(pos), collapse = ", "),
    franchise_ids = paste(unique(franchise_id), collapse = ",")
  ) %>%
  dplyr::group_by(season, trade_id, trade_side, asset_types) %>%
  dplyr::summarise(
    date = dplyr::first(date),
    asset_ids = dplyr::first(asset_ids),
    trade_asset_ids = dplyr::first(trade_asset_ids),
    #asset_types = dplyr::first(asset_types),
    asset_positions = dplyr::first(asset_positions),
    franchise_ids = dplyr::first(franchise_ids),
    asset_names = paste(trade_asset_name, collapse = "\n"),
    franchise_id = dplyr::first(franchise_id),
    franchise_name = dplyr::first(franchise_name),
    asset_names_with_draft_info = paste(asset_name_with_draft_info, collapse = "\n"),
    asset_names_with_draft_info_badge = paste(asset_name_with_draft_info_badge, collapse = "\n"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    trade_assets = asset_names,
    description = paste0(franchise_name, " sends\n", asset_names),
  ) %>%
  dplyr::select(-asset_names)

feather::write_feather(rfl_trade_history, "data/rfl_trade_history_data.feather")

# transactions
rfl_transactions_data <- purrr::map_df(2016:2026, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/transactions_data/rfl_transactions_{x}.csv"),
    col_types = "ddTdcccc"
  )
})

feather::write_feather(rfl_transactions_data, "data/rfl_transactions_data.feather")

# draft transactions
rfl_transactions_draft <- readr::read_csv("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_draftclass-trades.csv", col_types = "ciTccccccc")

feather::write_feather(rfl_transactions_draft, "data/rfl_transactions_draft_data.feather")
