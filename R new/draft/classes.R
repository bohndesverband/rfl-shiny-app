rfl_draft_classes <- shiny::reactive({
  rfl_drafts_with_elo %>%
    #filter(franchise_id == "0034") %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::filter(round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2]) %>%
    dplyr::mutate(
      #label = paste(season, paste0(round, ".", pick, " (", overall, ")"), player_name, paste0("(", team, ", ", pos, ")"))
      elo_peak = elo_peak - 1500,
    ) %>%
    dplyr::group_by(season, franchise_id) %>%
    dplyr::arrange(overall) %>%
    dplyr::summarise(
      franchise_name = first(franchise_name),
      picks = n(),
      #label = paste(label, collapse = "\n"),
      elo_shift = sum(elo_shift, na.rm = TRUE),
      elo_peak = sum(elo_peak, na.rm = TRUE),
      .groups = "drop"
    )
})

output$draft_classes <- plotly::renderPlotly({
  draft_classes_plot <- ggplot2::ggplot(rfl_draft_classes(), ggplot2::aes(x = elo_shift, y = elo_peak)) +
    geom_point(ggplot2::aes(size = picks, alpha = season, text = paste(franchise_name, season)), color = color_grey_mid) +

    geom_point(data = subset(rfl_draft_classes(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name, size = picks, alpha = season)) +

    scale_color_discrete(type = colors) +
    plot_defaults +
    ggplot2::labs(
      title = paste("RFL Draftklassen"),
      x = "Derzeitige ELO im Vergleich zum Draftjahr",
      y = "Höchste ELO",
      color = "RFL Team"
    ) +
    plot_clean

  team_draft_classes <- rfl_drafts_with_elo %>%
    dplyr::filter(
      franchise_id %in% input$selectRflTeams & (season >= input$selectYears[1] & season <= input$selectYears[2]) & (round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2])
    )

  if (nrow(team_draft_classes) > 0) {
    draft_class_plot <- ggplot2::ggplot(team_draft_classes, ggplot2::aes(x = overall, y = elo_peak, color = pos_grouped)) +
      ggplot2::facet_wrap(~season) +
      ggplot2::geom_point(ggplot2::aes(text = paste0(player_name, " (", franchise_name, ")")), size = 5) +
      ggplot2::scale_color_discrete(type = colors_position) +
      ggplot2::scale_y_continuous(limits = c(min(team_draft_classes$elo_peak - 100), max(team_draft_classes$elo_peak) + 100)) +

      plot_defaults +
      ggplot2::labs(
        color = "Position"
      )

    fig2 <- plotly::ggplotly(draft_class_plot, height = 1200)

    fig1 <- plotly::ggplotly(draft_classes_plot, source = "A")
    plotly::subplot(fig1, fig2, nrows = 2, heights = c(0.3, 0.7), margin = c(0, 0, 0.12, 0)) %>%
      plotly::layout(
        annotations = list(
          list(
            x = 0,
            y = 1,
            text = "Gegenübergestellt wird, wie sich die Draftklassen im Vergleich zum Draftjahr verändert haben (Aktuelle Spieler ELO, X-Achse) und der maximale ELO Wert aller Spieler (Y-Achse).",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0,
            y = 0.62,
            text = "Es werden alle Spieler nach ihrem Overall Pick im RFL Draft (X-Achse) und ihrer höchsten ELO (Y-Achse) dargestellt.",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      )
  } else {
    plotly::ggplotly(draft_classes_plot, source = "A", height = 600) %>%
      plotly::layout(
        annotations = list(
          list(
            x = 0,
            y = 1,
            text = "Gegenübergestellt wird, wie sich die Draftklassen im Vergleich zum Draftjahr verändert haben (Aktuelle Spieler ELO, X-Achse) und der maximale ELO Wert aller Spieler (Y-Achse).",
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      )
  }
})

output$dynamic_draft_plot_ui <- shiny::renderUI({
  plotly::plotlyOutput("draft_classes", height = NULL)
})

observeEvent(event_data("plotly_click", source = "A"), {
  click <- event_data("plotly_click", source = "A")
  clicked_data <- rfl_draft_classes()[click$pointNumber + 1, ]

  shinyWidgets::updatePickerInput(
    session,
    "selectRflTeams",
    selected = setNames(clicked_data$franchise_id, clicked_data$franchise_name)
  )

  shiny::updateSliderInput(
    session,
    "selectYear",
    value = clicked_data$season
  )
})

draft_class_trades <- shiny::reactive({
  draft_class_trades <- rfl_trades %>%
    filter(season <= input$selectYear) %>%
    #filter(season <= 2024) %>%
    dplyr::group_by(trade_id) %>%
    dplyr::mutate(
      franchise_ids = paste(franchise_id, collapse = ","),
      asset_ids = paste(asset_id, collapse = ","),
      asset_names = paste(trade_asset_name, collapse = "\n")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(grepl(input$selectRflTeams, franchise_ids) & (grepl(paste0(input$selectRflTeams, "_", input$selectYear), asset_ids) | grepl(input$selectYear, asset_names))) %>%
    #dplyr::filter(grepl("0034", franchise_ids) & (grepl(paste0("0034_", 2024), asset_ids) | grepl(2024, asset_names))) %>%
    dplyr::mutate(
      side = ifelse(franchise_id == input$selectRflTeams, "sent", "received"),
      #side = ifelse(franchise_id == "0034", "sent", "received")
    ) %>%
    dplyr::select(season, date, trade_id, side, trade_asset_name, asset_id) %>%
    dplyr::rename(asset_name = trade_asset_name) %>%
    tidyr::separate(asset_id, into = c("prefix", "pick_owner", "pick_year", "pick_round"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(
      pick_round = dplyr::case_when(
        prefix == "FP" ~ as.integer(pick_round),
        prefix == "DP" ~ as.integer(pick_owner) + 1
      ),
      pick_year = as.integer(pick_year)
    ) %>%
    dplyr::left_join(
      rfl_draft_orders %>%
        dplyr::mutate(season = season + 1),
      by = c("pick_year" = "season", "pick_owner" = "franchise_id")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      asset_id_new = ifelse(prefix == "FP" & !is.na(pick), paste("DP", pick_round - 1, pick - 1, pick_year, sep = "_"), asset_id), # für future picks
      asset_name = ifelse(grepl("DP_", asset_id_new) & !is.na(pick), paste0(pick_round, ".", pick, " ", pick_year), asset_name),
      pick = ifelse(prefix == "DP", as.integer(pick_year) + 1, pick),
      pick_year = ifelse(prefix == "DP", as.integer(stringr::str_split(asset_name, " ")[[1]][2]), pick_year),
      asset_id_new = ifelse(prefix == "DP", paste(asset_id, pick_year, sep = "_"), asset_id_new), # für draft picks
      asset_id = ifelse(pick_year == input$selectYear & prefix == "FP", asset_id_new, asset_id)
      #asset_id = ifelse(pick_year == 2024 & prefix == "FP", asset_id_new, asset_id)
    ) %>%
    dplyr::left_join(
      rfl_drafts_data %>%
        dplyr::filter(season <= input$selectYear) %>%
        #dplyr::filter(season <= 2024) %>%
        dplyr::mutate(
          round = as.integer(round),
          player_name = paste0(player_name, " (", pos, ", ", team, ")"),
        ) %>%
        dplyr::select(season, round, pick, player_name),
      by = c("pick_year" = "season", "pick_round" = "round", "pick")
    ) %>%
    dplyr::mutate(asset_name = ifelse(!is.na(player_name), paste(asset_name, player_name), asset_name)) %>%
    dplyr::select(-prefix, -pick_owner:-pick, -player_name)
})

output$single_draft_class <- gt::render_gt({
  req(input$selectRflTeams)
  req(input$selectYear)

  shiny::validate(
    shiny::need(length(input$selectRflTeams) <= 1, "Bitte wähle  nur ein Team aus. Die Daten werden sonst nicht korrekt dargestellt.")
  )

  draft <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeams) %>%
    #dplyr::filter(season == 2024 & franchise_id == "0034") %>%
    dplyr::mutate(
      side = "received",
      asset_id_new = paste("DP", as.integer(round) - 1, pick - 1, season, sep = "_"),
      asset_name = paste(paste0(as.integer(round), ".", sprintf("%02d", pick), " (", overall, ") ", player_name, " (", pos, ")"))
    ) %>%
    dplyr::select(season, timestamp, side, asset_name, asset_id_new) %>%
    dplyr::rename(date = timestamp)

  dplyr::bind_rows(
    lapply(
      list(draft, draft_class_trades()),
      function(x) {
        if (is.data.frame(x) && nrow(x) > 0) x else NULL
      }
    )
  ) %>%
    # filter alle erhaltene picks, mit denen dann gepickt wurde, raus
    dplyr::group_by(asset_id_new) %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%

    dplyr::mutate(
      side = ifelse(side == "sent", "Abgänge", "Zugänge"),
      type = dplyr::case_when(
        #grepl("-", asset_name) ~ "2_past_pick",
        #!is.na(trade_id) & grepl("DP_", asset_id) ~ "2_past_pick",
        grepl("FP_", asset_id) ~ "3_future_pick",
        grepl("DP_", asset_id_new) ~ "2_pick",
        #is.na(asset_id_new) ~ "4_picked_player",
        TRUE ~ "1_Player"
      ),
      date = dplyr::first(format(date, "%d.%m.%Y"))
    ) %>%
    dplyr::group_by(side, type) %>%
    dplyr::arrange(dplyr::desc(side), type, asset_name) %>%
    dplyr::group_by(side) %>%
    dplyr::select(-season, -type, -asset_id_new, -asset_id, -trade_id) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste(franchises$franchise_name[franchises$franchise_id == input$selectRflTeams], "Draftklasse", input$selectYear),
    ) %>%
    gtDefaults() %>%
    gt::cols_label(
      date = "Datum",
      asset_name = "Asset"
    )

  # TODO: add UDFAs
})

output$single_draft_class_trades <- DT::renderDataTable({
  req(input$selectRflTeams)
  req(input$selectYear)

  DT::datatable(
    draft_class_trades() %>%
      dplyr::group_by(trade_id, side) %>%
      dplyr::arrange(asset_name) %>%
      dplyr::summarise(
        asset_name = paste(asset_name, collapse = "\n"),
        date = dplyr::first(format(date, "%d.%m.%Y")),
        .groups = "drop"
      ) %>%
      tidyr::spread(side, asset_name) %>%
      dplyr::select(-trade_id) %>%
      dplyr::rename(
        "Geholt" = received,
        "Abgegeben" = sent,
        Datum = date
      ),
    rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 12, scrollY = "600px")
  )
})
