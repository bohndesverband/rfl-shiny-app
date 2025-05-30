trade_assets <- shiny::reactive({
  rfl_trades_data %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= (input$selectYears[2])
    ) %>%
    dplyr::select(trade_asset_id, asset_name) %>%
    dplyr::mutate(
      asset_name = ifelse(grepl("DP_", trade_asset_id), gsub("\\s*\\([^\\)]+\\)","", asset_name), asset_name),
      asset_name = ifelse(grepl("DP_", trade_asset_id), gsub("\\d+$","", asset_name), asset_name),
      asset_type = ifelse(grepl("DP_", trade_asset_id), "Picks", "Spieler")
    ) %>%
    dplyr::distinct() %>%
    dplyr::filter(!(asset_type == "Picks" & grepl("\\.", asset_name))) %>%
    dplyr::arrange(asset_name)
})

shiny::observeEvent(input$selectYears, {
  shinyWidgets::updatePickerInput(
    session,
    "selectTradeAsset",
    choices = split(setNames(trade_assets()$trade_asset_id, trade_assets()$asset_name), trade_assets()$asset_type)
  )
})

# trade history ----
trade_history <- reactive({
  rfl_trades_data %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::mutate(
      asset_type = ifelse(grepl("DP_", asset_id), "pick", "player")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pos = stringr::str_split(gsub(".*\\(([^)]+)\\).*", "\\1", asset_name), ",")[[1]][1]
    ) %>%
    dplyr::group_by(trade_id) %>%
    dplyr::mutate(
      asset_types = paste(asset_type, collapse = ","),
      asset_ids = paste(trade_asset_id, collapse = ","),
      trade_asset_ids = paste(asset_id, collapse = ","),
      asset_positions = paste(pos, collapse = ", "),
      franchise_ids = paste(franchise_id, collapse = ",")
    ) %>%
    dplyr::group_by(season, trade_id, trade_side, asset_types) %>%
    dplyr::summarise(
      date = dplyr::first(date),
      asset_ids = dplyr::first(asset_ids),
      trade_asset_ids = dplyr::first(trade_asset_ids),
      asset_types = dplyr::first(asset_types),
      asset_positions = dplyr::first(asset_positions),
      franchise_ids = dplyr::first(franchise_ids),
      asset_names = paste(trade_asset_name, collapse = "\n"),
      franchise_id = dplyr::first(franchise_id),
      franchise_name = dplyr::first(franchise_name),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      trade_assets = asset_names,
      asset_names = paste0(franchise_name, " sends\n", asset_names),
    ) %>%

    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
    ) %>%

    # strsplit(franchise_ids[i], ",") teilt die kommagetrennte Liste auf.
    # trimws() entfernt Leerzeichen rund um die Werte.
    # any(input$selectRflTeams %in% ...) prüft, ob mindestens einer der ausgewählten Werte in der Liste vorhanden ist.
    # sapply() wendet das Ganze auf jeden Eintrag in franchise_ids an.

    dplyr::filter(
      if(isTruthy(input$selectTradeAsset))
        sapply(seq_along(asset_ids), function(i) {
          any(input$selectTradeAsset %in% trimws(strsplit(asset_ids[i], ",")[[1]]))
        })
      else
        TRUE
    ) %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        sapply(seq_along(franchise_ids), function(i) {
          any(input$selectRflTeams %in% trimws(strsplit(franchise_ids[i], ",")[[1]]))
        })
      else
        TRUE
    ) %>%

    #dplyr::filter(
    #  if(isTruthy(input$selectPositions))
    #    sapply(seq_along(asset_positions), function(i) {
    #      any(input$selectPositions %in% trimws(strsplit(asset_positions[i], ",")[[1]]))
    #    })
    #  else
    #    TRUE
    #) %>%
    #dplyr::filter(
    #  if(input$tradeHistoryType == "Picks")
    #    !grepl("player", as.character(asset_types))
    #  else
    #    TRUE
    #) %>%
    #dplyr::filter(
    #  if(input$tradeHistoryType == "Spieler")
    #    !grepl("pick", as.character(asset_types))
    #  else
    #    TRUE
    #) %>%

    dplyr::select(trade_id, date, trade_side, asset_names) %>%
    tidyr::spread(trade_side, asset_names) %>%
    dplyr::arrange(dplyr::desc(trade_id)) %>%
    dplyr::select(-trade_id) %>%
    dplyr::mutate(
      date = format(date, "%d.%m.%Y")
    ) %>%
    dplyr::rename(Datum = date, "Franchise 1" = franchise_1, "Franchise 2" = franchise_2)
})

output$trade_history <- DT::renderDataTable({
  DT::datatable(
    trade_history(),
    rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 8, scrollY = "600px")
  )
})

# most traded players ----
most_tradet_players <- rfl_trades_data %>%
  dplyr::filter(!grepl("DP_", trade_asset_id) & !is.na(asset_name)) %>%
  dplyr::group_by(asset_id) %>%
  dplyr::summarise(Trades = n(), .groups = "drop") %>%
  dplyr::left_join(rfl_trades_data %>% dplyr::select(asset_id, asset_name), by = "asset_id", multiple = "last") %>%
  dplyr::select(asset_name, Trades) %>%
  dplyr::rename(Spieler = asset_name) %>%
  dplyr::mutate(
    Spieler = ifelse(grepl("Pick", Spieler), substr(Spieler, 1, nchar(Spieler) - 5), Spieler) # remove last 5 characters (year) if its a pick
  )

output$most_tradet_players <- DT::renderDataTable({
  formattable::formattable(
    most_tradet_players,
    list(
      Trades = formattable::color_tile(color_red, color_blue)
    )
  ) %>%
    formattable::as.datatable(escape = FALSE, rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 12, order = list(1, 'asc')))
})

# trades between franchises ----
trades_between_teams <- rfl_trades_data %>%
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
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name) %>%
      dplyr::rename(Accepted = franchise_name),
    by = c("partner" = "franchise_id")
  ) %>%
  dplyr::rename(Offered = franchise_name) %>%
  dplyr::select(Offered, Trades, Accepted)

output$trades_between_teams <- DT::renderDataTable({
  formattable::formattable(
    trades_between_teams,
    list(
      Trades = formattable::color_tile(color_red, color_blue)
    )
  ) %>%
    formattable::as.datatable(escape = FALSE, rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 12, order = list(1, 'asc')))
})

# trades per franchise ----
trades_by_franchise <- rfl_trades_data %>%
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
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::select(franchise_name, total_trades, dplyr::starts_with("20")) %>%
  dplyr::rename(Franchise = franchise_name, Summe = total_trades)

output$trades_by_franchise <- DT::renderDataTable({
  numeric_cols <- names(trades_by_franchise)[sapply(trades_by_franchise, is.numeric)]

  formattable::formattable(
    trades_by_franchise,
    lapply(setNames(numeric_cols, numeric_cols), function(col) {
      formattable::color_tile(color_red, color_blue)
    })
  ) %>%
    formattable::as.datatable(escape = FALSE, rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 12, order = list(1, 'asc')))
})
