rfl_draft_classes <- shiny::reactive({
  rfl_drafts_with_elo %>%
    #filter(franchise_id == "0007") %>%
    dplyr::filter(season > 2016) %>%
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
