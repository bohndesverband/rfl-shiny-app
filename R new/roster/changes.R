rfl_transactions <- purrr::map_df(2016:new_season_march, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/transactions_data/rfl_transactions_{x}.csv"),
    col_types = ""
  )
})

rfl_transactions %>%
  dplyr::filter(!is.na(player_name)) %>% # nur spieler, keine picks
  # entferne mehrere transactions pro spieler (nur letzte)
  dplyr::group_by(season, franchise_id, player_id) %>%
  dplyr::arrange(dplyr::desc(timestamp)) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%

  # draft data hinzufügen
  dplyr::bind_rows(
    rfl_drafts_data %>%
      dplyr::rename(player_id = mfl_id) %>%
      dplyr::mutate(type_desc = "drafted") %>%
      dplyr::select(season, timestamp, franchise_id, player_id, player_name, pos, team, type_desc)
  ) %>%
  filter(franchise_id == "0007") %>%
  filter(season == 2024) %>%

  # ursprüngliche reihenfolge wiederherstellen
  dplyr::arrange(timestamp) %>%


  #dplyr::select(season, franchise_id, player_id, player_name, pos, team, type_desc) %>%
  dplyr::group_by(season, franchise_id, type_desc) %>%
  dplyr::summarise(
    players = paste(paste(player_name, paste0("(", pos, ", ", team, ")")), collapse = "\n"),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  tidyr::spread(type_desc, players) %>%
  dplyr::group_by(franchise_name, season) %>%
  gt::gt() %>%
  gt::cols_hide(franchise_id) %>%
  gt::tab_header(
    title = paste("RFL Draft Hit Rates"),
    subtitle = "Keine Waiver im Moment"
  ) %>%
  gt::tab_spanner(
    "Zugänge",
    columns = c(traded_for, added, drafted)
  ) %>%

  gt::tab_spanner(
    "Abgänge",
    columns = c(traded_away, dropped)
  )
