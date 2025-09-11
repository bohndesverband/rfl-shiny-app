source("R new/rankings/standing.R", local = TRUE)

#plot_bump_default <- function(df, period) {
#  list <- list(
#      ggbump::geom_bump(color = color_grey_light, linewidth = 1),
#      geom_point(data = df[{period} == min(period) | {period} == max(period)], size = 3, color = color_grey_light),
#      ggplot2::geom_text(data = df[{period} == min(period)], ggplot2::aes(label = franchise_name), x = 0.9, hjust = 1, vjust = 0.35, color = color_text),
#      ggplot2::geom_text(data = df[{period} == max(period)], ggplot2::aes(label = franchise_name, x = as.numeric(max(period)) + 0.1), hjust = 0, vjust = 0.35, color = color_text)
      #ggbump::geom_bump(data = subset(draft_order, division %in% c(input$selectRflDivisions) | franchise_id %in% c(input$selectRflTeams) | franchise_id == input$selectRflTeam), linewidth = 1.8),
      #ggplot2::geom_point(data = subset(draft_order, division %in% c(input$selectRflDivisions) | franchise_id %in% c(input$selectRflTeams) | franchise_id == input$selectRflTeam), size = 5)
      #ggplot2::scale_x_continuous(limits = c(-0.4, df[{period} == max(period)] + 1.3), labels = c(1:df[{period} == max(period)]), breaks = c(1:df[{period} == max(period)])),
      #ggplot2::scale_y_reverse(limits = c(36, 1), labels = c(36:1), breaks = c(36:1), sec.axis = ggplot2::sec_axis(transform = ~., name="Draft Pick", labels = c(36:1), breaks = c(36:1)))
#  )

#  list
#}

# TODO: create function to remove duplicate code

# outputs ----

## rankings ----
output$draft_order <- shiny::renderPlot({
  ggplot2::ggplot(draft_order, ggplot2::aes(x = week, y = pick , group = franchise_id, color = franchise_id)) +

    ggbump::geom_bump(color = color_grey_light, linewidth = 1) +
    geom_point(data = subset(draft_order, week == 1 | week == max(week)), size = 3, color = color_grey_light) +

    ggplot2::geom_text(data = subset(draft_order, week == 1), ggplot2::aes(label = franchise_name), x = 0.9, hjust = 1, vjust = 0.35, color = color_text) +
    ggplot2::geom_text(data = subset(draft_order, week == max(week)), ggplot2::aes(label = franchise_name, x = max(week) + 0.1), hjust = 0, vjust = 0.35, color = color_text) +

    ggbump::geom_bump(data = subset(draft_order, division %in% c(input$selectRflDivisions) | franchise_id %in% c(input$selectRflTeams) | franchise_id == input$selectRflTeam), linewidth = 1.8) +
    ggplot2::geom_point(data = subset(draft_order, division %in% c(input$selectRflDivisions) | franchise_id %in% c(input$selectRflTeams) | franchise_id == input$selectRflTeam), size = 5) +

    ggplot2::scale_x_continuous(limits = c(-0.4, max(draft_order$week) + 1.3), labels = c(1:max(draft_order$week)), breaks = c(1:max(draft_order$week))) +
    ggplot2::scale_y_reverse(limits = c(36, 1), labels = c(36:1), breaks = c(36:1), sec.axis = ggplot2::sec_axis(transform = ~., name="Draft Pick", labels = c(36:1), breaks = c(36:1))) +

    ggplot2::scale_color_discrete(type = colors) +
    plot_defaults +
    plot_clean +
    labs(
      title = paste0("RFL Draft Reihenfolge Wochen ", min(draft_order$week), "-", max(draft_order$week), " ", new_season_sept),
      subtitle = "Woche 14 fast alle Postseason Ergebnisse zusammen",
      x = "Woche",
      y = "Draft Pick",
      color = ""
    ) +
    theme(
      legend.position = "none"
    )
}, height = 1000)

output$draft_order_history <- shiny::renderPlot({
  draft_order_history_plot <- rfl_draft_orders %>%
    ggplot2::ggplot(ggplot2::aes(x = season, y = pick , group = franchise_id, color = franchise_id)) +

    ggbump::geom_bump(color = color_grey_light, linewidth = 1) +
    geom_point(data = subset(rfl_draft_orders, season == 1 | season == max(season)), size = 3, color = color_grey_light) +

    #ggplot2::geom_text(data = subset(rfl_draft_orders, season == min(season)), ggplot2::aes(label = franchise_name, x = min(season) - 0.1), hjust = 1, vjust = 0.35, color = color_text) +
    ggplot2::geom_text(data = subset(rfl_draft_orders, season == max(season)), ggplot2::aes(label = franchise_name, x = max(season) + 0.1), hjust = 0, vjust = 0.35, color = color_text) +

    ggbump::geom_bump(data = subset(rfl_draft_orders, division %in% c(input$selectRflDivisions) | franchise_id %in% c(input$selectRflTeams) | franchise_id == input$selectRflTeam), linewidth = 1.8) +
    ggplot2::geom_point(data = subset(rfl_draft_orders, division %in% c(input$selectRflDivisions) | franchise_id %in% c(input$selectRflTeams) | franchise_id == input$selectRflTeam), size = 5) +

    ggplot2::scale_x_continuous(limits = c(min(rfl_draft_orders$season), max(rfl_draft_orders$season) + 1.3), expand = c(0, 0), labels = c(min(rfl_draft_orders$season):max(rfl_draft_orders$season)), breaks = c(min(rfl_draft_orders$season):max(rfl_draft_orders$season))) +
    ggplot2::scale_y_reverse(limits = c(36, 1), labels = c(36:1), breaks = c(36:1), sec.axis = ggplot2::sec_axis(transform = ~., name="Draft Pick", labels = c(36:1), breaks = c(36:1))) +

    ggplot2::scale_color_discrete(type = colors) +
    plot_defaults +
    plot_clean +
    labs(
      title = paste0("RFL Draft Reihenfolgen ", min(rfl_draft_orders$season), "-", max(rfl_draft_orders$season)),
      x = "Draftjahr",
      y = "Draft Pick",
      color = ""
    ) +
    theme(
      legend.position = "none"
    )

  if(input$showHistoricTeamNames) {
    draft_order_history_plot +
      ggrepel::geom_label_repel(data = subset(rfl_draft_orders, franchise_id %in% input$selectRflTeams & new_name == 1), ggplot2::aes(label = historic_name))
  } else {
    draft_order_history_plot
  }
}, height = 1000)
