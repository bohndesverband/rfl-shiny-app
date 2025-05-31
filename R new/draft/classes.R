# überblick über alle draftklassen ----
## daten ----
rfl_draft_classes <- shiny::reactive({
  rfl_draft_classes <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & season < new_season_march) %>%
    #filter(franchise_id == "0016") %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::filter(round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2]) %>%
    #dplyr::mutate(
    #label = paste(season, paste0(round, ".", pick, " (", overall, ")"), player_name, paste0("(", team, ", ", pos, ")"))
    #  elo_peak = elo_peak - 1500,
    #  elo_shift = elo_shift - 1500,
    #) %>%
    dplyr::group_by(season, franchise_id) %>%
    dplyr::arrange(overall) %>%
    dplyr::summarise(
      franchise_name = first(franchise_name),
      picks = n(),
      #label = paste(label, collapse = "\n"),
      elo_shift = sum(elo_shift, na.rm = TRUE) / picks,
      elo_peak = sum(elo_peak, na.rm = TRUE) / picks,
      .groups = "drop"
    )
})

## output ----
output$draft_classes_overview <- shiny::renderPlot({
  ggplot2::ggplot(subset(rfl_draft_classes(), !franchise_id %in% input$selectRflTeams), ggplot2::aes(x = elo_shift, y = elo_peak)) +

    plot_quadrants(
      xmin = min(rfl_draft_classes()$elo_shift),
      xmean = mean(rfl_draft_classes()$elo_shift),
      xmax = max(rfl_draft_classes()$elo_shift),
      ymin = min(rfl_draft_classes()$elo_peak),
      ymean = mean(rfl_draft_classes()$elo_peak),
      ymax = max(rfl_draft_classes()$elo_peak),
      ltl = "Höherer Einfluss, schlechtere Breite",
      ltr = "Höherere Einfluss, bessere Breite",
      lbr = "Weniger Einfluss, bessere Breite",
      lbl = "Weniger Einfluss, schlechtere Breite"
    ) +

    ggplot2::geom_hline(yintercept = 1500, color = color_red, alpha = 0.5) +
    ggplot2::geom_text(ggplot2::aes(x = max(rfl_draft_classes()$elo_shift)), y = 1490, color = color_red, vjust = 1, hjust = 1, label = "Default ELO", show.legend = FALSE) +

    ggplot2::geom_point(ggplot2::aes(size = picks, alpha = season), color = color_grey_mid) +

    ggplot2::geom_point(data = subset(rfl_draft_classes(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name, size = picks, alpha = season)) +

    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(min(rfl_draft_classes()$picks), max(rfl_draft_classes()$picks)), guide = "none") +
    ggplot2::scale_alpha_continuous(guide = "none") +
    plot_defaults +
    ggplot2::labs(
      title = paste("RFL Draftklassen"),
      subtitle = paste("Angezeigt werden alle", paste(input$selectPositions, collapse = ", "), "Draftpicks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "von", input$selectYears[1], "bis", input$selectYears[2], "\nDie Größe der Punkte stellt die Anzahl der Spieler in der Draftklasse dar, die Transparenz die Aktualität (aktuellest = dunkelste)."),
      x = "Derzeitige Gesamt-ELO pro Pick im Vergleich zum Draftjahr",
      y = "Höchste Gesamt-ELO pro Pick",
      color = "RFL Team",
    ) +
    plot_clean

  #TODO: girafe(ggobj = plot)
}, height = 800)

# überblick über alle draftklassen eines teams ----
team_draft_class_elo_height <- shiny::reactiveVal(800)

shiny::observeEvent(input$selectYears, {
  if (input$selectYears[2] - input$selectYears[1] == 0) {
    # wenn nur eine klasse ausgewählt ist
    team_draft_class_elo_height(800)
  } else {
    height_val <- ceiling((input$selectYears[2] - input$selectYears[1] + 1) / 2) * 450
    team_draft_class_elo_height(height_val)
  }
})

## output ----
output$team_draft_class_elo <- shiny::renderPlot({
  req(input$selectRflTeams)

  team_draft_classes <- player_elo %>%
    dplyr::group_by(mfl_id, season) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::select(season, mfl_id, week, player_elo_post) %>%
    dplyr::left_join(
      rfl_drafts_data %>%
        dplyr::filter(season > 2016) %>%
        dplyr::mutate(draft_class = season) %>%
        dplyr::select(draft_class, mfl_id, overall, franchise_id, franchise_name, player_name, pos_grouped),
      by = "mfl_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(overall)) %>%
    dplyr::group_by(mfl_id) %>%
    dplyr::mutate(
      max_season = ifelse(season == max(season), 1, 0),
      elo_peak = max(player_elo_post)
    ) %>%
    dplyr::ungroup() %>%
    #dplyr::filter(draft_class >= 2024 & draft_class <= 2024)
    dplyr::filter(draft_class >= input$selectYears[1] & draft_class <= input$selectYears[2])

  ggplot2::ggplot(subset(team_draft_classes, franchise_id == input$selectRflTeams & max_season == 1), ggplot2::aes(x = overall, y = player_elo_post, color = factor(pos_grouped, positions), alpha = season)) +
    ggplot2::facet_wrap(~draft_class, ncol = 2) +
    ggplot2::geom_hline(yintercept = 1500, color = color_red, alpha = 0.5) +
    ggplot2::geom_point(
      data = team_draft_classes %>%
        dplyr::group_by(mfl_id) %>%
        dplyr::filter(player_elo_post == max(player_elo_post)) %>%
        dplyr::filter(franchise_id != input$selectRflTeams),
      fill = color_grey_light,
      color = color_grey_light,
      alpha = 1,
      size = 3
    ) +
    ggplot2::geom_point(size = 10) +
    ggplot2::geom_point(data = subset(team_draft_classes, franchise_id == input$selectRflTeams & max_season == 0), size = 5) +
    ggplot2::scale_alpha_continuous(guide = "none") +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = paste0(player_name)),
      size = 4,
      alpha = 1,
      show.legend = FALSE
    ) +

    # genutzte positionen werden aus palette gefiltert, damit nur die nötigen farben genutzt werden und kein grau
    ggplot2::scale_color_manual(values = colors_position[names(colors_position) %in% unique(team_draft_classes$pos_grouped)], guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +

    plot_defaults +
    ggplot2::labs(
      title = paste(rfl_franchise_data$franchise_name[rfl_franchise_data$franchise_id == input$selectRflTeams], "RFL Draftklassen"),
      subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem höchsten Karriere ELO-Wert am Ende einer Saison.\nDie Picks des gewählten Teams werden farbig hervorgehoben und zeigen die ELO-Werte am Ende jeder RFL Regular Season.\nJe heller der Punkt, desto länger ist die Saison her. Der große Punkt ist immer die letzte Saison des Spielers."),
      x = "Overall Pick im RFL Draft",
      y = "Maximale ELO am Ende jeder RFL Regular Season",
      color = "Position"
    )
}, height = function() { team_draft_class_elo_height() })

#observeEvent(event_data("plotly_click", source = "A"), {
#  click <- event_data("plotly_click", source = "A")
#  clicked_data <- rfl_draft_classes()[click$pointNumber + 1, ]

#  shinyWidgets::updatePickerInput(
#    session,
#    "selectRflTeams",
#    selected = setNames(clicked_data$franchise_id, clicked_data$franchise_name)
#  )

#  shiny::updateSliderInput(
#    session,
#    "selectYear",
#    value = clicked_data$season
#  )
#})

# einzelne draft klasse eines teams ----
## trades ----
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
    #dplyr::filter(grepl("0016", franchise_ids) & (grepl(paste0("0016_", 2024), asset_ids) | grepl(2024, asset_names))) %>%
    dplyr::mutate(
      side = ifelse(franchise_id == input$selectRflTeams, "sent", "received"),
      #side = ifelse(franchise_id == "0016", "sent", "received")
    ) %>%
    dplyr::select(season, date, trade_id, side, trade_asset_name, asset_id) %>%
    dplyr::rename(asset_name = trade_asset_name) %>%
    tidyr::separate(asset_id, into = c("prefix", "pick_owner", "pick_year", "pick_round"), sep = "_", remove = FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pick_year = dplyr::case_when(
        prefix == "DP" ~ stringr::word(asset_name, 2),
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
        # alle ehemaligen future picks, die jetzt in der gegenwart sind, erhalten eine ID für den aktuellen draft
      #  prefix == "FP" & pick_year <= 2024 ~ paste("DP", pick_round - 1, pick - 1, pick_year, sep = "_"),
      #  prefix == "DP" ~ paste0(asset_id, "_", pick_year),
      #  TRUE ~ asset_id
      #),
      pick = dplyr::case_when(
        prefix == "DP" ~ as.integer(stringr::word(asset_id, 3, sep = "_")) + 1,
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
    dplyr::bind_rows(as_tibble(lapply(df, \(x) NA), .name_repair = "unique")) %>%  # neue zeile notwendig, damit beim mergen die colnames existieren. sonst kommt es bei teams ohen trades zu fehlern
    dplyr::select(season:pick_team_id, -prefix, -pick_owner)
})

## output übersicht ----
output$single_draft_class <- gt::render_gt({
  req(input$selectRflTeams)

  draft <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeams) %>%
    #dplyr::filter(season == 2024 & franchise_id == "0016") %>%
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
    dplyr::filter(dplyr::row_number() == 1 & !is.na(date)) %>%

    dplyr::mutate(
      side = ifelse(side == "sent", "Abgänge", "Zugänge"),
      type = dplyr::case_when(
        "asset_id_new" %in% names(.) && grepl("FP_", asset_id_new) ~ "3_future_pick",
        "asset_id_new" %in% names(.) && grepl("DP_", asset_id_new) ~ "2_pick",
        TRUE ~ "1_Player"
      ),
      date = dplyr::first(format(date, "%d.%m.%Y")),
      player_info = ifelse(!is.na(pick_round) & !is.na(pick), paste0(as.integer(pick_round), ".", sprintf("%02d", as.integer(pick)), " ", player_name), ""),
      asset_name = dplyr::case_when(
        # für draftpicks
        !is.na(subline) ~ paste(asset_name, subline, sep = "\n"),
        !is.na(player_name) & pick_year <= input$selectYear ~ paste(pick_year, player_info),
        !is.na(player_name) & new_season_march > input$selectYear + 2 ~ paste(asset_name, player_info, sep = "\n"), # zeige gepickte spieler erst 2 jahre nach der gewählten draftklasse
        pick_year > input$selectYear & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
        #!is.na(player_name) & pick_year <= 2024 ~ paste(pick_year, player_info),
        #!is.na(player_name) & new_season_march > 2024 + 2 ~ paste(asset_name, player_info, sep = "\n"),
        #pick_year > 2024 & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
        TRUE ~ asset_name
      )
    ) %>%
    dplyr::group_by(side, type) %>%
    dplyr::arrange(dplyr::desc(side), type, !!!if ("pick_year" %in% names(.)) rlang::syms("pick_year") else NULL, asset_name) # !!! und rlang::syms() erlauben es, Spaltennamen programmatisch einzufügen.

  row_index_line_through <- which(combined_data$side == "Zugänge" & combined_data$pick_team_id != input$selectRflTeams)[1]
  #row_index_line_through <- which(combined_data$pick_team_id != "0016")[1]
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
        #rows = side == "Zugänge" & pick_team_id != "0016"
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

## output trades ----
output$single_draft_class_trades <- DT::renderDataTable({
  req(input$selectRflTeams)
  req(input$selectYear)

  DT::datatable(
    draft_class_trades() %>%
      dplyr::filter(!is.na(trade_id)) %>%
      dplyr::mutate(
        player_info = paste(paste0(as.integer(pick_round), ".", sprintf("%02d", as.integer(pick))), player_name),
        asset_name = dplyr::case_when(
          !is.na(player_name) & pick_year <= input$selectYear ~ paste(pick_year, player_info),
          !is.na(player_name) & new_season_march > input$selectYear + 2 ~ paste(asset_name, player_info, sep = "\n"), # zeige gepickte spieler erst 2 jahre nach der gewählten draftklasse
          pick_year > input$selectYear & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
          #!is.na(player_name) & pick_year <= 2024 ~ paste(pick_year, player_info),
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

# ui output ----
output$team_draft_classes <- shiny::renderUI({
  shiny::validate(
    shiny::need(length(input$selectRflTeams) == 1, "Bitte wähle exakt ein Team, um dir dessen Draftklassen anzuschauen.")
  )

  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(shiny::plotOutput("team_draft_class_elo")),
      width = 12
    )
  )
})

output$team_draft_class <- shiny::renderUI({
  shiny::validate(
    shiny::need(input$selectYears[2] - input$selectYears[1] == 0, "Bitte wähle exakt ein Jahr, um dir eine einzelne Draftklasse anzuschauen. Das machst du, indem du beide Saison-Regler auf das selbe Jahr stellst.")
  )

  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(gt::gt_output("single_draft_class")),
      width = 5
    ),
    shiny::column(
      tags$h4("Alle Trades mit Picks dieses Drafts"),
      shinycssloaders::withSpinner(DT::DTOutput("single_draft_class_trades")),
      # TODO: meldung, wenn keine trades gefunden wurden
      width = 7
    )
  )
})
