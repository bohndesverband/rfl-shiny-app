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

rfl_depth_chart_data <- rfl_current_roster %>%
  #filter(player_id == "14123") %>%
  #filter(franchise_id == "0007") %>%
  dplyr::left_join(
    rfl_war_data %>%
      dplyr::filter(season == max(season)) %>%
      dplyr::select(player_id, war),
    by = "player_id"
  ) %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::arrange(dplyr::desc(season)) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(mfl_id, player_elo_post),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::left_join(
    rfl_drafts_data %>%
      dplyr::select(franchise_id, mfl_id) %>%
      dplyr::mutate(drafted = "drafted"),
    by = c(player_id = "mfl_id", "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_transactions_history %>%
      dplyr::mutate(franchise_id = ifelse(!is.na(trade_partner), trade_partner, franchise_id)) %>%
      dplyr::group_by(franchise_id, player_id) %>%
      dplyr::arrange(dplyr::desc(timestamp)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(franchise_id, player_id, type_desc),
    by = c("franchise_id", "player_id")
  ) %>%
  dplyr::mutate(
    player_name = nflreadr::clean_player_names(player_name),
    subline = paste(pos, team, sep = ", "),
    transaction = dplyr::case_when(
      player_id == "12801" & (franchise_id %in% c("0011", "0017")) ~ "drafted", # tyreek hill 2016 gedraftet
      player_id == "11938" & franchise_id == "0019" ~ "added", # adam thielen 2016 geadded
      is.na(type_desc) ~ drafted,
      TRUE ~ type_desc
    ),
    emoji = dplyr::case_when(
      transaction == "drafted" ~ emoji::emoji("ballot_box_with_check"),
      transaction == "traded" ~ emoji::emoji("arrows_counterclockwise"),
      TRUE ~ emoji::emoji("heavy_plus_sign")
    ),
  ) %>%
  dplyr::select(franchise_id, franchise_name, player_name, pos, subline, age, war, player_elo_post, transaction, emoji)
