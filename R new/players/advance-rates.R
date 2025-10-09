advance_rates <- shiny::reactive({
  advance_rates <- rfl_fantasy_finishes_season %>%
    dplyr::select(-dplyr::starts_with("top")) %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::left_join(
      rfl_war_data %>%
        dplyr::select(season, player_id, war),
      by = c("season", "player_id")
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      dplyr::across(c(points, war), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(c(player_name, pos, team), ~ dplyr::last(.x)),
      .groups = "drop"
    ) %>%
    dplyr::group_by(pos) %>%
    dplyr::arrange(dplyr::desc(points)) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::left_join(
      rfl_roster_data %>%
        dplyr::filter(week == max(week)) %>%
        dplyr::select(player_id, franchise_id),
      by = "player_id"
    ) %>%
    dplyr::left_join(
      rfl_current_standing %>%
        dplyr::select(franchise_name, franchise_id, seed, bowl),
      by = "franchise_id"
    ) %>%
    dplyr::filter(!is.na(bowl)) %>%
    dplyr::group_by(player_id, bowl) %>%
    dplyr::mutate(advance_rate = round(n() / 3, 2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_name, pos, points, pos_rank, war, advance_rate, bowl) %>%
    dplyr::distinct() %>%
    tidyr::spread(bowl, advance_rate) %>%
    tidyr::replace_na(list(PB = 0, SB = 0, TB = 0))
})

output$advanceRates <- gt::render_gt({
  advance_rates() %>%
    dplyr::arrange(dplyr::desc(points)) %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        sapply(seq_along(pos), function(i) {
          any(input$selectPositions %in% trimws(strsplit(pos[i], ",")[[1]]))
        })
      else
        TRUE
    ) %>%
    #filter(pos == "QB") %>%

    gt::gt() %>%
    gt::tab_header(
      title = paste("Advance Rates", paste0(input$selectYears[1])),
      #subtitle = "Nur RFL Regular Season"
    ) %>%
    #gt::cols_hide(c(player_id)) %>%
    gt::tab_spanner(
      label = "Fantasy",
      columns = c(points, pos_rank, war)
    ) %>%

    gt::tab_spanner(
      label = "Bowls",
      columns = c(SB, PB, TB)
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_align(
      align = "left",
      columns = c(player_name)
    ) %>%

    gt::fmt_percent(
      c(SB, PB, TB),
      decimals = 0
    ) %>%

    gt::data_color(
      c(points, war),
      palette = c(color_red, color_yellow, color_green, color_blue)
    ) %>%
    gt::data_color(
      c(SB, PB, TB),
      palette = c(color_red, color_blue)
    ) %>%
    gt::data_color(
      c(pos_rank),
      palette = c(color_blue, color_green, color_yellow, color_red)
    ) %>%
    gt::cols_label(
      player_name = "Spieler",
      pos = "Pos",
      points = "Pts",
      war = "WAR",
      pos_rank = "Rang",
    ) %>%

    gtDefaults() %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_filters = TRUE,
      ihtml.use_search = TRUE,
      ihtml.use_pagination = TRUE,
      ihtml.use_page_size_select = TRUE,
      ihtml.page_size_default = 12,
      ihtml.page_size_values = c(12, 25, 50, 100),
      ihtml.use_highlight = TRUE
    )
})
