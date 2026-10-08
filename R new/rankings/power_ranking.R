source("R new/rankings/standing_output.R", local = TRUE)

power_ranking_plot <- shiny::reactive({
  data <- rfl_standing_data %>%
    dplyr::filter(season == max(season))


  ggplot2::ggplot(data, ggplot2::aes(x = week, y = power_rank , group = franchise_id, color = franchise_id)) +

    ggbump::geom_bump(color = color_grey_light, linewidth = 1) +
    geom_point(data = subset(data, week == 1 | week == max(week)), size = 3, color = color_grey_light) +

    ggplot2::geom_text(data = subset(data, week == 1), ggplot2::aes(label = franchise_name), x = 0.9, hjust = 1, vjust = 0.35, color = color_text) +
    ggplot2::geom_text(data = subset(data, week == max(week)), ggplot2::aes(label = franchise_name, x = max(week) + 0.1), hjust = 0, vjust = 0.35, color = color_text) +

    ggbump::geom_bump(data = subset(data, franchise_id %in% input$selectRflTeams), linewidth = 1.8) +
    ggplot2::geom_point(data = subset(data, franchise_id %in% input$selectRflTeams), size = 5) +

    ggplot2::scale_x_continuous(limits = c(-0.4, max(data$week) + 1.3), labels = c(1:max(data$week)), breaks = c(1:max(data$week))) +
    ggplot2::scale_y_reverse(limits = c(36, 1), labels = c(36:1), breaks = c(36:1), sec.axis = ggplot2::sec_axis(transform = ~., name="Power Rank", labels = c(36:1), breaks = c(36:1))) +

    ggplot2::scale_color_discrete(type = colors) +
    plot_defaults +
    plot_clean +
    labs(
      title = paste0("RFL Power Ranking Wochen ", min(data$week), "-", max(data$week), " ", max(data$season)),
      x = "Woche",
      y = "Power Rank",
      color = ""
    ) +
    theme(
      legend.position = "none"
    )
})

# outputs ----

## rankings ----
output$power_ranking <- shiny::renderPlot({
  power_ranking_plot()
}, height = 1000)

## team report ----
output$team_report_power_ranking <- shiny::renderPlot({
  power_ranking_plot()
}, height = 800)
