rfl_draft_classes <- shiny::reactive({
  rfl_draft_classes <- rfl_drafts_data %>%
    #filter(franchise_id == "0027") %>%
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
    ggplot2::geom_point(ggplot2::aes(size = picks, alpha = season, text = paste(franchise_name, season)), color = color_grey_mid) +

    ggplot2::geom_point(data = subset(rfl_draft_classes(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name, size = picks, alpha = season)) +

    ggplot2::scale_color_discrete(type = colors) +
    plot_defaults +
    ggplot2::labs(
      title = paste("RFL Draftklassen"),
      x = "Derzeitige ELO im Vergleich zum Draftjahr",
      y = "Höchste ELO",
      color = "RFL Team"
    ) +
    plot_clean

  team_draft_classes <- rfl_drafts_data %>%
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
  draft_class_trades <- rfl_trades_data %>%
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
    #dplyr::filter(grepl("0027", franchise_ids) & (grepl(paste0("0027_", 2024), asset_ids) | grepl(2024, asset_names))) %>%
    dplyr::mutate(
      side = ifelse(franchise_id == input$selectRflTeams, "sent", "received"),
      #side = ifelse(franchise_id == "0027", "sent", "received")
    ) %>%
    dplyr::select(season, date, trade_id, side, trade_asset_name, asset_id) %>%
    dplyr::rename(asset_name = trade_asset_name) %>%
    tidyr::separate(asset_id, into = c("prefix", "pick_owner", "pick_year", "pick_round"), sep = "_", remove = FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pick_year = dplyr::case_when(
        prefix == "DP" ~ stringr::str_split(asset_name, " ")[[1]][2],
        TRUE ~ pick_year
      ),
      pick_round = dplyr::case_when(
        prefix == "FP" ~ as.integer(pick_round),
        prefix == "DP" ~ as.integer(pick_owner) + 1
      )
    ) %>%

    # add draft order um pick für getradete future picks zu erhalten
    dplyr::left_join(
      rfl_draft_orders %>%
        dplyr::mutate(pick_year = as.character(season)) %>%
        dplyr::select(-season),
      by = c("pick_year", "pick_owner" = "franchise_id")
    ) %>%

    dplyr::mutate(
      asset_id_new = dplyr::case_when(
        # alle ehemaligen future picks, die jetzt in der gegenwart sind, erhalten eine ID für den aktuellen draft
        prefix == "FP" & pick_year <= input$selectYear ~ paste("DP", pick_round - 1, pick - 1, pick_year, sep = "_"),
        prefix == "DP" ~ paste0(asset_id, "_", pick_year),
        TRUE ~ asset_id
      ),
      #asset_id_new = dplyr::case_when(
      #  # alle ehemaligen future picks, die jetzt in der gegenwart sind, erhalten eine ID für den aktuellen draft
      #  prefix == "FP" & pick_year <= 2024 ~ paste("DP", pick_round - 1, pick - 1, pick_year, sep = "_"),
      #  prefix == "DP" ~ paste0(asset_id, "_", pick_year),
      #  TRUE ~ asset_id
      #),
      pick = dplyr::case_when(
        prefix == "DP" ~ as.integer(stringr::str_split(asset_id, "_")[[1]][3]) + 1,
        TRUE ~ as.integer(pick)
      )
    ) %>%
    arrange(side) %>%
    dplyr::left_join(
      rfl_drafts_data %>%
        dplyr::mutate(
          round = as.integer(round),
          player_name = paste0(player_name, " (", pos, ", ", team, ")"),
          pick_team_id = franchise_id,
          pick_year = as.character(season)
        ) %>%
        dplyr::select(pick_year, round, pick, player_name, pick_team_id),
      by = c("pick_year", "pick_round" = "round", "pick")
    ) %>%
    dplyr::select(-prefix, -pick_owner)
})

output$single_draft_class <- gt::render_gt({
  req(input$selectRflTeams)
  req(input$selectYear)

  shiny::validate(
    shiny::need(length(input$selectRflTeams) <= 1, "Bitte wähle  nur ein Team aus. Die Daten werden sonst nicht korrekt dargestellt.")
  )

  draft <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeams) %>%
    #dplyr::filter(season == 2024 & franchise_id == "0027") %>%
    dplyr::left_join(
      mfl_adp_data %>%
        dplyr::mutate(adp = (rfl_min + rfl_max) / 2) %>%
        dplyr::select(season, mfl_id, adp),
      by = c("season", "mfl_id")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      side = "received",
      asset_id_new = paste("DP", as.integer(round) - 1, pick - 1, season, sep = "_"),
      across(
        c(first_pick, second_pick, third_pick),
        ~ ifelse(.x == overall, paste0("**", .x, "**"), as.character(.x))
      ),
      asset_name = paste(season, paste0(as.integer(round), ".", sprintf("%02d", pick)), player_name, paste0("(", pos, ", ", team, ")")),
      text_rfl =  paste("RFL:", paste(na.omit(c(first_pick, second_pick, third_pick)), collapse = ", ")),
      text_adp = paste0("ADP: ", adp),
      subline = ifelse(
        !is.na(adp),
        paste(text_rfl, text_adp, sep = " - "),
        text_rfl
      )
    ) %>%
    dplyr::select(season, timestamp, side, asset_id_new, asset_name, subline) %>%
    dplyr::rename(date = timestamp)

  combined_data <- dplyr::bind_rows(
    lapply(
      list(draft, draft_class_trades()),
      #list(draft, draft_class_trades),
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
        "asset_id_new" %in% names(.) && grepl("FP_", asset_id_new) ~ "3_future_pick",
        "asset_id_new" %in% names(.) && grepl("DP_", asset_id_new) ~ "2_pick",
        TRUE ~ "1_Player"
      ),
      date = dplyr::first(format(date, "%d.%m.%Y")),
      player_info = paste(paste0(as.integer(pick_round), ".", sprintf("%02d", as.integer(pick))), player_name),
      asset_name = dplyr::case_when(
        # für draftpicks
        !is.na(subline) ~ paste(asset_name, subline, sep = "\n"),
        !is.na(player_name) & pick_year <= input$selectYear ~ player_info,
        !is.na(player_name) & new_season_march > input$selectYear + 2 ~ paste(asset_name, player_info, sep = "\n"), # zeige gepickte spieler erst 2 jahre nach der gewählten draftklasse
        pick_year > input$selectYear & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
        #!is.na(player_name) & pick_year <= 2024 ~ player_info,
        #!is.na(player_name) & new_season_march > 2024 + 2 ~ paste(asset_name, player_info, sep = "\n"),
        #pick_year > 2024 & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
        TRUE ~ asset_name
      )
    ) %>%
    dplyr::group_by(side, type) %>%
    dplyr::arrange(dplyr::desc(side), type, !!!if ("pick_year" %in% names(.)) rlang::syms("pick_year") else NULL, asset_name) # !!! und rlang::syms() erlauben es, Spaltennamen programmatisch einzufügen.

  row_index_line_through <- which(combined_data$pick_team_id != input$selectRflTeams)[1]
  #row_index_line_through <- which(combined_data$pick_team_id != "0027")[1]
  row_index_first_pick <- which(is.na(combined_data$trade_id))[1]
  row_index_future_picks <- which(combined_data$pick_year > input$selectYear)[1]
  #row_index_future_picks <- which(combined_data$pick_year > 2024)[1]

  table <- combined_data %>%
    dplyr::group_by(side) %>%
    dplyr::select(date, side, asset_name, pick_team_id) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste(rfl_franchise_data$franchise_name[rfl_franchise_data$franchise_id == input$selectRflTeams], "Draftklasse", input$selectYear)
    ) %>%
    gtDefaults() %>%
    gt::tab_style(
      style = gt::cell_text(decorate = "line-through"),
      locations = cells_body(
        columns = asset_name,
        rows = side == "Zugänge" & pick_team_id != input$selectRflTeams
        #rows = side == "Zugänge" & pick_team_id != "0027"
      )
    ) %>%
    gt::fmt_markdown(columns = asset_name) %>%
    gt::cols_hide(c(side, pick_team_id)) %>%
    gt::cols_label(
      date = "Datum",
      asset_name = "Asset"
    )

  if (!is.na(row_index_line_through)) {
    table <- table %>%
      gt::tab_footnote(
        footnote = "Durchgestrichenen Picks wurden nicht vom dem ausgewählten Team getätigt, sondern wurden weiter getradet.",
        locations = cells_body(
          columns = asset_name,
          rows = row_index_line_through
        )
      )
  }

  if (!is.na(row_index_first_pick)) {
    table <- table %>%
      gt::tab_footnote(
        footnote = "Der hervorgehobene RFL Pick ist der Spot, an dem das ausgewählte Team den Spieler gewählt hat.",
        locations = cells_body(
          columns = asset_name,
          rows = row_index_first_pick
        )
      )
  }

  if (!is.na(row_index_future_picks)) {
    table <- table %>%
      gt::tab_footnote(
        footnote = "Für Future Picks, die weniger als 2 Jahre in der Zukunft der Draftklasse sind, werden keine Spieler angezeigt.",
        locations = cells_body(
          columns = asset_name,
          rows = row_index_future_picks
        )
      )
  }

  table

  # TODO: add UDFAs
})

output$single_draft_class_trades <- DT::renderDataTable({
  req(input$selectRflTeams)
  req(input$selectYear)

  DT::datatable(
    draft_class_trades() %>%
      dplyr::mutate(
        player_info = paste(paste0(as.integer(pick_round), ".", sprintf("%02d", as.integer(pick))), player_name),
        asset_name = dplyr::case_when(
          !is.na(player_name) & pick_year <= input$selectYear ~ player_info,
          !is.na(player_name) & new_season_march > input$selectYear + 2 ~ paste(asset_name, player_info, sep = "\n"), # zeige gepickte spieler erst 2 jahre nach der gewählten draftklasse
          pick_year > input$selectYear & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
          #!is.na(player_name) & pick_year <= 2024 ~ player_info,
          #!is.na(player_name) & new_season_march > 2024 + 2 ~ paste(asset_name, player_info, sep = "\n"),
          #pick_year > 2024 & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
          TRUE ~ asset_name
        )
      ) %>%
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
