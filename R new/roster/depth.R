roster_depth_weekly <- shiny::reactive({
  roster_depth_weekly <- rfl_starter_data %>%
    dplyr::filter(season == season_before_wk_2) %>%
    #dplyr::filter(season == 2024) %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        pos_grouped %in% input$selectPositions
      else
        TRUE
    ) %>%
    dplyr::group_by(franchise_id, week, starter_status) %>%
    dplyr::mutate(pf = round(sum(player_score, na.rm = TRUE), 2)) %>%
    dplyr::group_by(franchise_id, week, should_start) %>%
    dplyr::mutate(pp = round(sum(player_score, na.rm = TRUE), 2)) %>%
    dplyr::filter(starter_status == "starter" & should_start == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(week, franchise_id, pf, pp) %>%
    dplyr::distinct() %>%
    dplyr::rename(pppg = pp) %>%
    dplyr::mutate(points_back = pf - pppg) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    )
})

roster_depth_season <- shiny::reactive({
  roster_depth_season <- roster_depth_weekly() %>%
  dplyr::group_by(franchise_id, franchise_name) %>%
  dplyr::summarise(
    pppg = round(mean(pppg, na.rm = TRUE), 2),
    points_back = round(mean(points_back, na.rm = TRUE), 2),
    .groups = "drop"
  )
})

roster_depth_plot <- function(df) {
  list(
    ggplot2::annotate(
      "rect",
      xmin = min({{df}}$points_back) - 1,
      xmax = mean({{df}}$points_back),
      ymin = mean({{df}}$pppg),
      ymax = max({{df}}$pppg) + 5,
      fill = color_yellow,
      alpha = 0.2
    ),

    ggplot2::annotate(
      "text",
      label = "Besseres Roster,\nschlechtere Starter",
      x = min({{df}}$points_back),
      y = max({{df}}$pppg),
      hjust = 0,
      vjust = 0.5,
      lineheight = 0.9,
    ),

    ggplot2::annotate(
      "rect",
      xmin = mean({{df}}$points_back),
      xmax = max({{df}}$points_back) + 1,
      ymin = mean({{df}}$pppg),
      ymax = max({{df}}$pppg) + 5,
      fill = color_blue,
      alpha = 0.15
    ),

    ggplot2::annotate(
      "text",
      label = "Besseres Roster,\nbessere Starter",
      x = max({{df}}$points_back),
      y = max({{df}}$pppg),
      hjust = 1,
      vjust = 0.5,
      lineheight = 0.9,
    ),

    ggplot2::annotate(
      "rect",
      xmin = min({{df}}$points_back) - 1,
      xmax = mean({{df}}$points_back),
      ymin = min({{df}}$pppg) - 5,
      ymax = mean({{df}}$pppg),
      fill = color_red,
      alpha = 0.15
    ),

    ggplot2::annotate(
      "text",
      label = "Schlechteres Roster,\nschlechtere Starter",
      x = min({{df}}$points_back),
      y = min({{df}}$pppg),
      hjust = 0,
      lineheight = 0.9,
    ),

    ggplot2::annotate(
      "rect",
      xmin = mean({{df}}$points_back),
      xmax = max({{df}}$points_back) + 1,
      ymin = min({{df}}$pppg) - 5,
      ymax = mean({{df}}$pppg),
      fill = color_green,
      alpha = 0.15
    ),

    ggplot2::annotate(
      "text",
      label = "Schlechteres Roster,\nbessere Starter",
      x = max({{df}}$points_back),
      y = min({{df}}$pppg),
      hjust = 1,
      lineheight = 0.9,
    ),

    ggplot2::geom_hline(yintercept = mean({{df}}$pppg), color = color_grey_mid, linetype = "dashed", linewidth = 0.5) ,
    ggplot2::geom_vline(xintercept = mean({{df}}$points_back), color = color_grey_mid, linetype = "dashed", linewidth = 0.5),
    plot_defaults,
    plot_clean,
    ggplot2::theme(
      legend.position = "top"
    ),
    ggplot2::labs(
      subtitle = paste("Alle", paste(input$selectPositions, collapse = ", "), "im Roster")
    )
  )
}

output$roster_depth_season <- shiny::renderPlot({
  ggplot2::ggplot(roster_depth_season(), ggplot2::aes(x = points_back, y = pppg)) +
    roster_depth_plot(roster_depth_season()) +

    ggplot2::geom_point(size = 5, alpha = 0.5, color = color_grey_mid) +
    ggplot2::geom_point(data = subset(roster_depth_season(), franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(color = franchise_name), size = 7) +
    ggplot2::scale_color_discrete(type = colors) +

    ggplot2::labs(
      title = paste("RFL Rosterstärke Woche", max(rfl_standing_data$week), season_before_wk_2),
      x = "PF - PP pro Spiel",
      y = "PP pro Spiel",
      color = "RFL Teams"
    )
}, height = 600)

output$roster_depth_weekly <- shiny::renderPlot({
  shiny::validate(
    shiny::need(input$selectRflTeams != "", "Wähle mindestens ein Team, um diese Grafik anzuzeigen.")
  )

  ggplot2::ggplot(roster_depth_weekly(), ggplot2::aes(x = points_back, y = pppg)) +
    roster_depth_plot(roster_depth_weekly()) +

    ggplot2::geom_point(data = subset(roster_depth_weekly(), franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(color = franchise_name, alpha = week), size = 5) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_alpha_continuous(range = c(0.5, 1), guide = "none") +

    ggplot2::labs(
      title = paste0("Rosterstärke Wochen ", min(roster_depth_weekly()$week), "-", max(roster_depth_weekly()$week), " ", season_before_wk_2),
      x = "PF - PP",
      y = "PP",
      color = "RFL Teams"
    )

}, height = 600)



