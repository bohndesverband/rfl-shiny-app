roster_depth_weekly <- shiny::reactive({
  roster_depth_weekly <- rfl_starter_data %>%
    dplyr::filter(season == season_before_wk_2) %>%
    #dplyr::filter(season == 2025) %>%
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
      xmin = min({{df}}$points_back) - 5,
      xmax = mean({{df}}$points_back),
      ymin = mean({{df}}$pppg),
      ymax = max({{df}}$pppg) + 5,
      fill = color_blue,
      alpha = 0.2,
    ),

    ggplot2::annotate(
      "text",
      label = "Besseres Roster,\nbessere Starter",
      x = min({{df}}$points_back) + 3,
      y = max({{df}}$pppg),
      lineheight = 0.9,
    ),

    ggplot2::annotate(
      "rect",
      xmin = mean({{df}}$points_back),
      xmax = max({{df}}$points_back) + 5,
      ymin = mean({{df}}$pppg),
      ymax = max({{df}}$pppg) + 5,
      fill = color_green,
      alpha = 0.15
    ),

    ggplot2::annotate(
      "text",
      label = "Schlechteres Roster,\nbessere Starter",
      x = max({{df}}$points_back) - 5,
      y = max({{df}}$pppg),
      lineheight = 0.9,
    ),

    ggplot2::annotate(
      "rect",
      xmin = min({{df}}$points_back) - 5,
      xmax = mean({{df}}$points_back),
      ymin = min({{df}}$pppg) - 5,
      ymax = mean({{df}}$pppg),
      fill = color_yellow,
      alpha = 0.15
    ),

    ggplot2::annotate(
      "text",
      label = "Besseres Roster,\nschlechtere Starter",
      x = min({{df}}$points_back) + 5,
      y = min({{df}}$pppg),
      lineheight = 0.9,
    ),

    ggplot2::annotate(
      "rect",
      xmin = mean({{df}}$points_back),
      xmax = max({{df}}$points_back) + 5,
      ymin = min({{df}}$pppg) - 5,
      ymax = mean({{df}}$pppg),
      fill = color_red,
      alpha = 0.15
    ),

    ggplot2::annotate(
      "text",
      label = "Schlechteres Roster,\nschlechtere Starter",
      x = max({{df}}$points_back) - 5,
      y = min({{df}}$pppg),
      lineheight = 0.9
    ),

    ggplot2::geom_hline(yintercept = mean({{df}}$pppg), color = color_grey_mid, linetype = "dashed", linewidth = 0.5) ,
    ggplot2::geom_vline(xintercept = mean({{df}}$points_back), color = color_grey_mid, linetype = "dashed", linewidth = 0.5),
    ggplot2::scale_x_continuous(limits = c(min({{df}}$points_back) - 5, max({{df}}$points_back) + 5), expand = c(0, 0)),
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

output$roster_depth_season <- plotly::renderPlotly({
  plot <- ggplot2::ggplot(roster_depth_season(), ggplot2::aes(x = points_back, y = pppg)) +
    roster_depth_plot(roster_depth_season()) +

    ggplot2::geom_point(ggplot2::aes(text = franchise_name), size = 5, alpha = 0.5, color = color_grey_mid) +
    ggplot2::geom_point(data = subset(roster_depth_season(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name), size = 7) +
    ggplot2::scale_color_discrete(type = colors) +

    ggplot2::labs(
      title = paste("RFL Rosterstärke Woche", max(rfl_standing_data$week), season_before_wk_2),
      x = "PF - PP pro Spiel",
      y = "PP pro Spiel",
      color = ""
    )

  plotly::ggplotly(plot, tooltip = c("text"), source = "roster_depth_season") %>%
    plotly::style(hoverinfo = "none", traces = c(12)) %>%
    plotly::layout(
      legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", font = list(size = 11))
    )


  #p_json <- plotly::plotly_json(test)

  #print(paste0(jsonlite::fromJSON(p_json$x$data)$data$type, ": ",
   #            jsonlite::fromJSON(p_json$x$data)$data$name))
})


# input bei klick setzen
observeEvent(event_data("plotly_click", source = "roster_depth_season"), {
  click_data <- event_data("plotly_click", source = "roster_depth_season")

  print(click_data)

  if (!is.null(click_data)) {
    selected_team <- roster_depth_season()$franchise_id[click_data$pointNumber + 1]

    updateSelectInput(session, "selectRflTeams", selected = unique(c(input$selectRflTeams, selected_team)))
  }
})


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
      color = ""
    )

}, height = 600)

# punkte nach alter ----
fpts_by_age <- rfl_roster_data %>%
  dplyr::left_join(
    rfl_player_scores %>%
      dplyr::filter(season == new_season_sept) %>%
      dplyr::select(week, player_id, pos, points),
    by = c("player_id", "week")
  ) %>%
  dplyr::mutate(
    age = round(age),
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(pos, age) %>%
  dplyr::mutate(league_avg = round(mean(points, na.rm = TRUE), 2)) %>%
  dplyr::group_by(franchise_id, pos, age) %>%
  dplyr::mutate(
    team_avg = round(mean(points, na.rm = TRUE), 2),
    player_count = n()
  ) %>%
  dplyr::arrange(age) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(points)) %>%
  dplyr::select(franchise_id, pos, age, player_count, league_avg, team_avg) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::mutate(pos = factor(pos, positions_grouped))

fpts_by_age_avg <- fpts_by_age %>%
  dplyr::group_by(pos, age) %>%
  dplyr::summarise(
    league_avg = mean(league_avg, na.rm = TRUE),
    .groups = "drop"
  )

output$fptsByAge <- shiny::renderPlot({
  ggplot2::ggplot(data = subset(fpts_by_age, pos %in% input$selectPositions), ggplot2::aes(x = age, y = team_avg)) +
    ggplot2::facet_wrap(~ pos, ncol = 2) +
    ggalt::geom_xspline(data = subset(fpts_by_age_avg, pos %in% input$selectPositions), ggplot2::aes(y = league_avg), spline_shape = -0.5, color = color_grey_dark) +
    ggplot2::geom_jitter(ggplot2::aes(size = player_count, alpha = player_count), width = 0.25, color = color_grey_mid) +

    ggalt::geom_xspline(data = subset(fpts_by_age, pos %in% input$selectPositions & franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name), spline_shape = -0.5) +
    ggplot2::aes(lwd = 1.2) +
    ggplot2::scale_linewidth_identity() +

    ggplot2::geom_point(data = subset(fpts_by_age, pos %in% input$selectPositions & franchise_id %in% input$selectRflTeams), ggplot2::aes(size = player_count, color = franchise_name)) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1,8), guide = "none") +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = "Durchschnittliche FPts pro Position nach Alter",
      subtitle = "Größe der Punkte zeigt die Anzahl der Spieler. Die schwarze Kurve ist der Ligadurchschnitt.",
      x = "Alter",
      y = "Durchschnittliche FPts",
      color = ""
    )
}, height = 1200)

