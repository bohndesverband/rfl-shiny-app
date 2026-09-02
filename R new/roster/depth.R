source("R new/roster/war.R", local = TRUE)
source("R new/roster/depth_chart_data.R", local = TRUE)

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
      title = paste("RFL Rosterstärke Woche", current_week_thu, season_before_wk_2),
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

  if (!is.null(click_data)) {
    selected_team <- roster_depth_season()$franchise_id[click_data$pointNumber + 1]

    print(selected_team)

    shinyWidgets::updatePickerInput(
      session, "selectRflTeams", selected = unique(c(input$selectRflTeams, selected_team)),
      options = list("max-options" = 6)
    )
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
  dplyr::filter(week == max(week)) %>%
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

output$rosterByAge <- gt::render_gt({
  roster_by_age <- fpts_by_age %>%
    dplyr::group_by(franchise_id, franchise_name, pos) %>%
    dplyr::summarise(
      age_avg = round(mean(age, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::group_by(franchise_id, franchise_name) %>%
    dplyr::mutate(avg = round(mean(age_avg, na.rm = TRUE), 1)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(pos, age_avg) %>%
    dplyr::arrange(avg)

  roster_by_age %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        franchise_id %in% input$selectRflTeams
      else
        TRUE
    ) %>%
    dplyr::select(-franchise_id) %>%
    gt::gt() %>%
    gt::data_color(
      avg,
      palette = c(color_blue, color_green, color_yellow, color_red),
      domain = c(min(roster_by_age$avg, na.rm = TRUE), max(roster_by_age$avg, na.rm = TRUE))
    ) %>%
    purrr::reduce(
      c("QB", "RB", "WR", "TE", "PK", "DL", "LB", "DB"),
      function(gt_tbl, pos) {
      gt::data_color(
        gt_tbl,
        columns = pos,
        palette = c(color_blue, color_green, color_yellow, color_red),
        domain = c(min(roster_by_age[[pos]], na.rm = TRUE), max(roster_by_age[[pos]], na.rm = TRUE))
      )
      },
      .init = .
    ) %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = FALSE,
      ihtml.use_highlight = TRUE
    ) %>%
    gt::cols_width(
      franchise_name ~ px(250),
      c(avg:DB) ~ px(70),
    ) %>%
    gt::cols_label(
      franchise_name = "Team",
      avg = "Avg"
    ) %>%
    gtDefaults()
})


# depth ----
war_max <- max(rfl_depth_chart_data$war, na.rm = TRUE)
war_min <- min(rfl_depth_chart_data$war, na.rm = TRUE)
elo_max <- max(rfl_depth_chart_data$player_elo_post, na.rm = TRUE)
elo_min <- min(rfl_depth_chart_data$player_elo_post, na.rm = TRUE)

output$depthChart <- gt::render_gt({
  shiny::validate(
    shiny::need(input$selectRflTeams != "", "Wähle ein Team, um diese Grafik anzuzeigen.")
    #shiny::need(length(input$selectRflTeams) == 1, "Wähle genau ein Team, um diese Grafik anzuzeigen.")
  )

  selected_team <- rfl_franchise_data %>%
    dplyr::filter(franchise_id %in% input$selectRflTeams) %>%
    dplyr::pull(franchise_name)

  rfl_depth_chart_data %>%
    dplyr::filter(franchise_id %in% input$selectRflTeams) %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        pos %in% input$selectPositions
      else
        TRUE
    ) %>%
    dplyr::group_by(franchise_name, pos) %>%
    dplyr::arrange(factor(pos, levels = positions_grouped), dplyr::desc(war)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste(paste(selected_team, collapse = ", "), "Depth Chart")
    ) %>%
    gt::cols_hide(c(franchise_id, transaction)) %>%
    gtExtras::gt_merge_stack(
      player_name,
      subline,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%
    gt::data_color(
      war,
      palette = c(color_red, color_yellow, color_green, color_blue),
      domain = c(war_min, war_max)
    ) %>%
    gt::data_color(
      player_elo_post,
      palette = c(color_red, color_yellow, color_green, color_blue),
      domain = c(elo_min, elo_max)
    ) %>%
    gt::data_color(
      age,
      palette = c(color_bg, color_red),
      domain = c(20, 40)
    ) %>%
    gt::cols_label(
      player_name = "Spieler",
      age = "Alter",
      war = "WAR",
      player_elo_post = "ELO",
      emoji = ""
    ) %>%
    gtDefaults() %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_align(
      align = "left",
      columns = c(player_name)
    )
})

elo_vs_war <- shiny::reactive({
  rfl_team_elo %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::select(franchise_id, franchise_elo_postgame) %>%
    dplyr::left_join(
      roster_war_filtered() %>%
        dplyr::select(franchise_id, franchise_name, total),
      by = "franchise_id"
    )
})

output$elo_vs_war <- shiny::renderPlot({
  ggplot2::ggplot(elo_vs_war(), ggplot2::aes(x = franchise_elo_postgame, y = total)) +
    plot_quadrants(
      min(elo_vs_war()$franchise_elo_postgame),
      mean(elo_vs_war()$franchise_elo_postgame),
      max(elo_vs_war()$franchise_elo_postgame),
      min(elo_vs_war()$total),
      mean(elo_vs_war()$total),
      max(elo_vs_war()$total),
      "Auf dem Weg nach oben",
      "Sieht langfristig gut aus",
      "Auf dem Weg nach unten",
      "Rebuild"
    ) +

    # trendline
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = color_grey_dark, linetype = "dashed", se = FALSE, linewidth = 0.5) +

    # alle punkte
    ggplot2::geom_point(size = 8, alpha = 0.25, color = color_grey_mid) +

    # ausgewählte punkte
    ggplot2::geom_point(data = subset(elo_vs_war(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name), size = 8) +
    ggplot2::scale_color_discrete(type = colors) +

    ggplot2::scale_x_continuous(limits = c(min(elo_vs_war()$franchise_elo_postgame) - 25, max(elo_vs_war()$franchise_elo_postgame) + 25), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(min(elo_vs_war()$total) - 1, max(elo_vs_war()$total) + 1), expand = c(0, 0)) +

    plot_defaults +

    ggplot2::labs(
      title = c("RFL ELO vs WAR Woche", current_week_thu),
      subtitle = "Die Grafik zeigt den aktuellen ELO-Wert (langfristiger Trend) eines Teams im Vergleich zu dessen Starter-WAR (kurzfristiger Trend).",
      x = "ELO",
      y = "WAR",
      color = ""
    )
}, height = 600)
