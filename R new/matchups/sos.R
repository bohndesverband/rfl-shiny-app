rfl_sos <- feather::read_feather("data/rfl_sos.feather") %>%
  dplyr::mutate(
    above_avg = round(Total - mean(Total), 3),
    SOS = round(SOS, 3),
  ) %>%
  dplyr::left_join(rfl_franchise_data %>% dplyr::rename(div_id = division) %>% select(franchise_id, franchise_name, div_id), by = "franchise_id")

rfl_sos_filtered <- shiny::reactive({
  rfl_sos_filtered <- rfl_sos %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
    ) %>%
    #dplyr::filter(
    #  season >= 2024 & season <= 2025
    #) %>%
    dplyr::group_by(franchise_id, div_id) %>%
    dplyr::summarise(
      franchise_name = dplyr::last(franchise_name),
      div_name = dplyr::last(div_name),
      dplyr::across(
        c("above_avg", "Total", "Division", "Conference", "Random", "SOS"),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
 })

# Sos abv. avg ----
output$rfl_sos_total <- shiny::renderPlot({
    ggplot2::ggplot(rfl_sos_filtered(), ggplot2::aes(x = above_avg, y = reorder(franchise_name, above_avg))) +
    plot_defaults +
    ggplot2::geom_col(fill = color_grey_dark) +
    ggplot2::geom_col(data = subset(rfl_sos_filtered(), div_id %in% input$selectRflDivisions | franchise_id %in% input$selectRflTeams), ggplot2::aes(fill = franchise_name)) +
    ggplot2::scale_fill_discrete(type = colors) +
    ggplot2::labs(
      title = paste(paste0("RFL Strength of Schedule (SoS) im Vergleich zum Durchschnitt (", input$selectYears[1], "-", input$selectYears[2], ")")),
      subtitle = paste("Je niedriger der Wert, desto leichter ist der SoS im Vergleich zum Rest der Liga."),
      y = "",
      x = "Abweichung des SoS vom Durchschnitt",
      fill = ""
    ) +
    ggplot2::theme(
      legend.position = "top"
    )
}, height = 800)

# SoS nach matchups ----
chartSosByMatchupData <- shiny::reactive({
  rfl_sos_filtered() %>%
    gather(matchup, sos, c(Total, Division, Conference, Random))
})

output$rfl_sos_matchups <- shiny::renderPlot({
    ggplot2::ggplot(chartSosByMatchupData(), aes(x = matchup, y = sos, group = franchise_name, color = franchise_name)) +
    plot_defaults +

    ggplot2::geom_point(size = 3, color = color_grey_dark, alpha = 0.5) +
    ggbump::geom_bump(data = subset(chartSosByMatchupData(), div_id %in% input$selectRflDivisions | franchise_id %in% input$selectRflTeams), size = 1) +
    ggplot2::geom_point(data = subset(chartSosByMatchupData(), div_id %in% input$selectRflDivisions | franchise_id %in% input$selectRflTeams), size = 5) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::labs(
      title = "RFL SoS nach Matchups",
      x = "Matchup Typ",
      y = "Durchschnittliche Win % der Gegner",
      color = ""
    ) +
    ggplot2::theme(
      legend.position = "top"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent)

}, height = 800)
