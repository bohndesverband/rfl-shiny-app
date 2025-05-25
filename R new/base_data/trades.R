rfl_trades_data <- purrr::map_df(2016:2025, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_{x}.csv"),
    col_types = "dddTdcccc"
  )
})

rfl_trades <- rfl_trades_data %>%
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
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = c("team" = "franchise_id")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    draft_round = ifelse(grepl("DP_", asset_id), as.numeric(draft_round) + 1, draft_round),
    trade_asset_name = ifelse(grepl("FP_", asset_id), paste(asset_name, franchise_name), asset_name),
    trade_asset_id = ifelse(grepl("DP_", asset_id) | grepl("FP_", asset_id), paste0("DP_", draft_round), asset_id)
  ) %>%
  dplyr::select(-franchise_name, -team)
