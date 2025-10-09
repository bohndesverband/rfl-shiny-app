rfl_trades_data <- purrr::map_df(2016:2025, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_{x}.csv"),
    col_types = "dddTdcccc"
  )
}) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    draft_pick = ifelse(grepl("DP_", asset_id), stringr::str_pad(as.numeric(stringr::str_split(asset_id, "_")[[1]][3]) + 1, 2, pad = "0"), NA),
    draft_round = dplyr::case_when(
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
    draft_round = ifelse(grepl("DP_", asset_id), as.numeric(draft_round) + 1, draft_round),
    trade_asset_name = ifelse(grepl("FP_", asset_id), paste(asset_name, franchise_name), asset_name),
    trade_asset_id = ifelse(grepl("DP_", asset_id) | grepl("FP_", asset_id), paste0("DP_", draft_round), asset_id)
  ) %>%
  dplyr::select(-franchise_name, -team) %>%
  dplyr::group_by(trade_id) %>%
  dplyr::arrange(trade_side) %>%
  dplyr::mutate(
    trade_partner = ifelse(trade_side == "franchise_2", first(franchise_id), last(franchise_id)),
  ) %>%
  dplyr::arrange(trade_id) %>%
  dplyr::ungroup()

feather::write_feather(rfl_trades_data, "data/rfl_trades_data.feather")

# transactions
rfl_transactions_data <- purrr::map_df(2017:2025, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/transactions_data/rfl_transactions_{x}.csv"),
    col_types = "dicccc"
  )
})

problems(rfl_transactions_data)

feather::write_feather(rfl_transactions_data, "data/rfl_transactions_data.feather")
