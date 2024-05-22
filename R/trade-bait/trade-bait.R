# helper ----
create_pos_rank <- function(df) {
  df %>%
    dplyr::select(player_id, pos, war) %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(war = round(mean(war), 2)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(pos) %>%
    dplyr::arrange(dplyr::desc(war)) %>%
    dplyr::mutate(pos_rank = paste0(pos, " #", dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_id, war, pos_rank)
}

trade_baits <- jsonlite::read_json(paste0(var.mflApiBaseEarlyNewYear, "/export?TYPE=tradeBait&L=63018&APIKEY=aRNp3s%2BWvuWsx12mPlrBYDoeErox&INCLUDE_DRAFT_PICKS=0&JSON=1"))$tradeBaits$tradeBait %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%

  # split col willGiveUp on , and create a new row for each player
  tidyr::separate_rows(willGiveUp, sep = ",") %>%
  dplyr::mutate(willGiveUp = as.numeric(willGiveUp)) %>%

  # war last season
  dplyr::left_join(
    war %>%
      dplyr::filter(season == var.season) %>%
      create_pos_rank() %>%
      dplyr::rename(last_season_war = war, last_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id")
  ) %>%

  # war last 2 seasons
  dplyr::left_join(
    war %>%
      dplyr::filter(season <= var.season & season >= var.season - 1) %>%
      create_pos_rank() %>%
      dplyr::rename(two_season_war = war, two_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id"),
    multiple = "first"
  ) %>%

  # war last 3 seasons
  dplyr::left_join(
    war %>%
      dplyr::filter(season <= var.season & season >= var.season - 2) %>%
      create_pos_rank() %>%
      dplyr::rename(three_season_war = war, three_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id"),
    multiple = "first"
  ) %>%

  # war since 2016 seasons
  dplyr::left_join(
    war %>%
      create_pos_rank() %>%
      dplyr::rename(all_season_war = war, all_season_pos_rank = pos_rank),
    by = c("willGiveUp" = "player_id"),
    multiple = "first"
  ) %>%
  dplyr::left_join(war %>% dplyr::select(player_id, player_name, pos), by = c("willGiveUp" = "player_id"), multiple = "first") %>%
  dplyr::left_join(franchises %>% dplyr::select(franchise_id, franchise_name), by = "franchise_id") %>%
  dplyr::select(-timestamp, -willGiveUp, -franchise_id)

# gt ----
create_gt <- function(df) {
  df %>%
    dplyr::arrange(desc(last_season_war)) %>%
    gt::gt() %>%
    gt::cols_align(columns = last_season_war:all_season_pos_rank, align = "center") %>%
    #gt::cols_hide(inExchangeFor) %>%
    gt::sub_missing(missing_text = "---") %>%
    gtExtras::gt_merge_stack(col1 = last_season_war, col2 = last_season_pos_rank, palette = c("black", "black")) %>%
    gtExtras::gt_merge_stack(col1 = two_season_war, col2 = two_season_pos_rank, palette = c("black", "black")) %>%
    gtExtras::gt_merge_stack(col1 = three_season_war, col2 = three_season_pos_rank, palette = c("black", "black")) %>%
    gtExtras::gt_merge_stack(col1 = all_season_war, col2 = all_season_pos_rank, palette = c("black", "black")) %>%
    gt::cols_label(
      pos = "",
      player_name = "",
      last_season_war = var.season,
      two_season_war = paste(var.season - 1, var.season, sep = "-"),
      three_season_war = paste(var.season - 2, var.season, sep = "-"),
      all_season_war = paste(2016, var.season, sep = "-")
    ) %>%
    gt::cols_move_to_start(columns = player_name) %>%
    gt::data_color(
      columns = c(last_season_war, two_season_war, three_season_war, all_season_war),
      colors = c("white", "#c8d6e5")
    ) %>%
    gtDefaults()
}
