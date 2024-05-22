most_tradet_players <- trades %>%
  dplyr::filter(!grepl("Pick", asset_name) & !is.na(asset_name)) %>%
  dplyr::group_by(asset_id) %>%
  dplyr::summarise(Trades = n(), .groups = "drop") %>%
  dplyr::left_join(trades %>% dplyr::select(asset_id, asset_name), by = "asset_id", multiple = "last") %>%
  dplyr::select(asset_name, Trades) %>%
  dplyr::rename(Spieler = asset_name)

most_trades_by_franchise <- trades %>%
  dplyr::mutate(season = lubridate::year(date)) %>%
  dplyr::select(trade_id, franchise_id, season) %>%
  dplyr::distinct() %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::mutate(total_trades = n()) %>%
  dplyr::group_by(franchise_id, season) %>%
  dplyr::summarise(
    total_trades = dplyr::first(total_trades),
    Trades = n(),
    .groups = "drop"
  ) %>%
  tidyr::spread(season, Trades, fill = 0) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::select(franchise_name, total_trades, dplyr::starts_with("20")) %>%
  dplyr::rename(Franchise = franchise_name, Summe = total_trades)

most_trades_between_teams <- trades %>%
  dplyr::select(trade_id, franchise_id) %>%
  dplyr::distinct() %>%
  dplyr::group_by(trade_id) %>%
  dplyr::mutate(
    partner_1 = lag(franchise_id),
    partner_2 = lead(franchise_id),
    partner = ifelse(is.na(partner_1), partner_2, partner_1)
  ) %>%
  dplyr::select(-partner_1, -partner_2) %>%
  group_by(franchise_id, partner) %>%
  dplyr::summarise(
    trade_id = dplyr::first(trade_id),
    Trades = n(),
    .groups = "drop"
  ) %>%
  dplyr::group_by(trade_id) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name) %>%
      dplyr::rename("Tradepartner 2" = franchise_name),
    by = c("partner" = "franchise_id")
  ) %>%
  dplyr::rename("Tradepartner 1" = franchise_name) %>%
  dplyr::select("Tradepartner 1", Trades, "Tradepartner 2")
