source("R new/roster/depth_chart_data.R", local = TRUE)

# TODO: in reactable tabellen umwandeln

roster_war_filtered <- shiny::reactive({
  roster_war_filtered <- roster_war %>%
    dplyr::filter(
      starts >= input$selectRflGames[1]
    #  starts >= 2
    ) %>%
    dplyr::group_by(franchise_id, pos) %>%
    dplyr::summarise(
      war = round(sum(war, na.rm = T), 2),
      .groups = "drop") %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::mutate(total = round(sum(war, na.rm = T), 2)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(pos, war) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::select(franchise_id, franchise_name, total, "QB", "RB", "WR", "TE", "DL", "LB", "DB", "PK") %>%
    dplyr::arrange(dplyr::desc(total))
})

output$roster_war <- gt::render_gt({
  roster_war_filtered() %>%
    dplyr::select(-franchise_id) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Team WAR nach Positionen"),
      subtitle = paste0("Woche ", current_week, " ", season_before_wk_2, "; Nur Spieler mit mind. ", input$selectRflGames, " Starts für das entsprechende Team")
    ) %>%

    gt::data_color(
      is.numeric,
      palette = c(color_red, color_yellow, color_green, color_blue),
      na_color = color_bg
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%

    gt::cols_align(
      align = "left",
      columns = c(franchise_name)
    ) %>%

    gtDefaults() %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = FALSE,
      ihtml.use_highlight = TRUE,
      ihtml.use_sorting = TRUE,
      ihtml.use_filters = TRUE
    ) %>%
    gt::cols_label(
      franchise_name = "Team",
      total = "WAR"
    )
})

output$roster_war_transactions <- gt::render_gt({
  rfl_depth_chart_data %>%
    dplyr::group_by(franchise_name, transaction) %>%
    dplyr::summarise(war = round(sum(war, na.rm = TRUE), 2), .groups = "drop") %>%
    dplyr::filter(!is.na(transaction)) %>%
    tidyr::pivot_wider(names_from = transaction, values_from = war) %>%
    dplyr::select(franchise_name, added, traded, drafted) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Team WAR nach Transaktionen"),
    ) %>%

    gt::data_color(
      is.numeric,
      palette = c(color_red, color_yellow, color_green, color_blue),
      na_color = color_bg
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%

    gt::cols_align(
      align = "left",
      columns = c(franchise_name)
    ) %>%

    gt::cols_label(
      franchise_name = "Team",
      added = "WW/FA",
      drafted = "Drafts",
      traded = "Trades"
    ) %>%

    gtDefaults() %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = FALSE,
      ihtml.use_highlight = TRUE,
      ihtml.use_sorting = TRUE,
      ihtml.use_filters = TRUE
    )
})





