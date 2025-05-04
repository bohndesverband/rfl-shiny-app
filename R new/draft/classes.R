rfl_draft_classes <- rfl_drafts_with_elo %>%
  #filter(franchise_id == "0007") %>%
  dplyr::filter(season > 2016) %>%
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

output$draft_classes <- plotly::renderPlotly({
  draft_classes_plot <- ggplot2::ggplot(rfl_draft_classes, ggplot2::aes(x = elo_shift, y = elo_peak)) +
    geom_point(ggplot2::aes(size = picks, alpha = season, text = paste(franchise_name, season)), color = color_grey_mid) +

    geom_point(data = subset(rfl_draft_classes, franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(color = franchise_name, size = picks)) +

    scale_color_discrete(type = colors) +
    plot_defaults +
    ggplot2::labs(
      title = paste("RFL Draftklassen"),
      subtitle = "Gegenübergestellt werden, wie sich die Draftklasse im Vergleich zum Draftjahr verändert hat (X-Achse) und der maximale ELO Wert aller Spieler (Y-Achse).",
      x = "Derzeitige ELO",
      y = "Höchste ELO"
    ) +
    plot_clean

  team_draft_classes <- rfl_drafts_with_elo %>%
    dplyr::filter(
      franchise_id %in% input$selectRflTeams & season >= input$selectYears[1] & season <= input$selectYears[2]
    )

  if (nrow(team_draft_classes) > 0) {
    draft_class_plot <- ggplot2::ggplot(team_draft_classes, ggplot2::aes(x = overall, y = elo_peak, color = pos)) +
      facet_wrap(~season) +
      geom_point(aes(text = player_name)) +
      scale_color_discrete(type = colors_position) +
      plot_defaults

    fig2 <- plotly::ggplotly(draft_class_plot)

    fig1 <- plotly::ggplotly(draft_classes_plot, source = "A")
    plotly::subplot(fig1, fig2, nrows = 2, heights = c(0.2, 0.8))
  } else {
    plotly::ggplotly(draft_classes_plot, source = "A")
  }
})

output$dynamic_draft_plot_ui <- shiny::renderUI({
  plotly::plotlyOutput("draft_classes", height = NULL)
})

observeEvent(event_data("plotly_click", source = "A"), {
  click <- event_data("plotly_click", source = "A")
  clicked_data <- rfl_draft_classes[click$pointNumber + 1, ]

  shinyWidgets::updatePickerInput(
    session,
    "selectRflTeams",
    selected = setNames(clicked_data$franchise_id, clicked_data$franchise_name)
  )

  shiny::updateSliderInput(
    session,
    "selectYears",
    value = c(clicked_data$season, clicked_data$season)
  )
})

output$draft_class <- plotly::renderPlotly({
  draft_class_plot <- ggplot2::ggplot(subset(rfl_drafts_with_elo, franchise_id %in% c("0007") & season == 2017), ggplot2::aes(x = overall, y = elo_peak, color = pos)) +
    facet_wrap(~season) +
    geom_point(aes(text = player_name)) +
    scale_color_discrete(type = colors_position) +
    plot_defaults

  plotly::ggplotly(draft_class_plot)
})
