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
    rfl_franchise_data %>%
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
      dplyr::select(pick_year = season, pick_round = round, draft_pick = pick, asset_name_with_info = asset_name),
    by = c("pick_year", "pick_round", "draft_pick")
  ) %>%
  dplyr::mutate(
    asset_name = ifelse(!is.na(asset_name_with_info), asset_name_with_info, asset_name)
  ) %>%
  dplyr::select(season:asset_name, trade_asset_name:pick_year, pick_round, everything(), -asset_name_with_info)

DBI::dbWriteTable(con, "rfl_trades_data", rfl_trades_data, overwrite = TRUE)

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
    #asset_name_with_draft_info = ifelse(!is.na(player_name_with_info), paste(asset_name, player_name_with_info, sep = " - "), trade_asset_name),
    #asset_name_with_draft_info_badge= ifelse(!is.na(pick_cat_badge), paste0(asset_name_with_draft_info, pick_cat_badge), asset_name_with_draft_info)
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
    #asset_names_with_draft_info = paste(asset_name_with_draft_info, collapse = "\n"),
    #asset_names_with_draft_info_badge = paste(asset_name_with_draft_info_badge, collapse = "\n"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    trade_assets = asset_names,
    description = paste0(franchise_name, " sends\n", asset_names),
  ) %>%
  dplyr::select(-asset_names)

DBI::dbWriteTable(con, "rfl_trade_history", rfl_trade_history, overwrite = TRUE)

# transactions
rfl_transactions_data <- purrr::map_df(2026:2026, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/transactions_data/rfl_transactions_{x}.csv"),
    col_types = "ddTdcccc"
  )
}) %>%
  #dplyr::mutate(player_id = as.character(player_id))
  # helper um zu joinen
  dplyr::mutate(
    season_join = dplyr::case_when(
      week == 0 & season > 2016 ~ season - 1,
      TRUE ~ season
    ),
    week_join = dplyr::case_when(
      (week == 0 | week == 22) & season < 2020 ~ 12,
      (week == 0 | week == 22) & season >= 2020 ~ 13,
      TRUE ~ week
    ),
    join_id = paste0(season_join, week_join)
  ) %>%



  # füge trades an
  #dplyr::bind_rows(
  #  rfl_trades_data %>%
  #    dplyr::mutate(
  #      type = "TRADED",
  #      type_desc = "traded",
  #    ) %>%
  #    dplyr::rename(player_id = asset_id) %>%
  #    dplyr::select(season, timestamp, date, type, type_desc, franchise_id, player_id, trade_partner)
  #) %>%

  dplyr::left_join(
    rfl_player_data %>%
      dplyr::mutate(join_id_new = paste0(season, week)) %>%
      dplyr::select(join_id_new, player_id, display_name, pos_grouped, team, games_played, games_missed, fpts_running, ppg_running, pos_rank, war, war_pctl, war_end_of_season, player_elo_pre, player_elo_pre_pctl, player_elo_current, player_elo_current),
    by = dplyr::join_by(
      player_id,
      dplyr::closest(join_id >= join_id_new)
    )
  ) %>%

  # add player info für nicht vorhandene namen
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::mutate(
        position = dplyr::case_when(
          position %in% c("DT", "DE") ~ "DL",
          position %in% c("CB", "S") ~ "DB",
          TRUE ~ position
        )
      ) %>%
      dplyr::select(player_id = mfl_id, display_name_new = name, team_new = team, position_new = position),
    by = "player_id"
  ) %>%
  dplyr::mutate(
    display_name = dplyr::coalesce(display_name, display_name_new),
    pos_grouped = dplyr::coalesce(pos_grouped, position_new),
    team = dplyr::coalesce(team, team_new)
  ) %>%
  dplyr::select(-dplyr::ends_with("_new"), -dplyr::ends_with("_join"), -join_id) %>%

  # add franchise info
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%

  # data cleaning
  dplyr::mutate(
    player_elo_pre = ifelse(is.na(player_elo_pre), 1500, player_elo_pre),
    elo_shift = player_elo_current - player_elo_pre,
    war_shift = war_end_of_season - war
  ) %>%
  dplyr::arrange(dplyr::desc(timestamp))

DBI::dbWriteTable(con, "rfl_transactions_data", rfl_transactions_data, overwrite = TRUE)

# draft transactions
rfl_transactions_draft <- readr::read_csv("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_draftclass-trades.csv", col_types = "ciTccccccc")

DBI::dbWriteTable(con, "rfl_transactions_draft", rfl_transactions_draft, overwrite = TRUE)
