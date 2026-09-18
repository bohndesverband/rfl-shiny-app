# war nach positionen ----
roster_war_sum <- shiny::reactive({
  roster_war_sum <- rfl_roster_data %>%
    dplyr::filter(
      week < 14 &
      #season == 2025
      season == input$selectYear & games_started >= input$selectRflGames
    ) %>%
    dplyr::group_by(franchise_id, pos_grouped) %>%
    dplyr::summarise(
      war = round(sum(war, na.rm = TRUE), 2),
      .groups = "drop") %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::mutate(total = round(sum(war, na.rm = TRUE), 2)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(pos_grouped, war) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::select(franchise_id, franchise_name, total, "QB", "RB", "WR", "TE", "DL", "LB", "DB", "PK") %>%
    dplyr::arrange(dplyr::desc(total))
})

player_war_reactable <- function(selected_franchise_id, selected_position) {
  data <- rfl_roster_data %>%
    dplyr::filter(season == input$selectYear & franchise_id == selected_franchise_id & pos_grouped == selected_position) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::select(player_name_with_info, age, games_started, war, player_elo_post)

  reactable_default(
    data,
    columns = list(
      player_name_with_info = reactable::colDef(
        name = "Spieler",
        html = TRUE,
        cell = function(value, index) {
          as.character(htmltools::div(htmltools::HTML(value)))
        },
        minWidth = 350
      ),
      age = reactable_coldef_bg(name = "Alter", width = 100, palette_fun = scale_red_white(c(18:45), reverse = TRUE)),
      games_started = reactable_coldef_bg(name = "Gestartet", width = 100, palette_fun = scale_blue_white(c(0:14))),
      war = reactable_coldef_bg(name = "WAR", width = 100, palette_fun = scale_rainbow(rfl_roster_data$war)),
      player_elo_post = reactable_coldef_bg(name = "ELO", width = 100, palette_fun = scale_rainbow(rfl_roster_data$player_elo_post))
    ),
    defaultSorted = "war",
    defaultSortOrder = "desc",
    defaultPageSize = 5,
    compact = TRUE,
    fullWidth = FALSE
  )
}

position_coldef <- function(position) {
  reactable_coldef_bg(
    name = position,
    details = function(index) {
      selected_franchise_id <- roster_war_sum()$franchise_id[index]
      selected_franchise_name <- rfl_franchise_data %>%
        dplyr::filter(franchise_id == selected_franchise_id) %>%
        dplyr::pull(franchise_name)

      htmltools::tagList(
        htmltools::div(
          htmltools::strong(paste0(selected_franchise_name, " ", position, "s"))
        ),
        player_war_reactable(selected_franchise_id, position)
      )
    },
    palette_fun = scale_rainbow(roster_war_sum()[[position]])
  )
}

## output ----
output$team_war_by_position <- reactable::renderReactable({
  reactable_default(
    roster_war_sum(),
    columns = c(
      list(
      franchise_id = reactable::colDef(show = FALSE),
      franchise_name = reactable::colDef(name = "Team", width = 200),
      total = reactable_coldef_bg(name = "Total", width = 100, palette_fun = scale_red_blue(roster_war_sum()$total))
      ),
      stats::setNames(lapply(positions_grouped, position_coldef), positions_grouped)
    ),
    defaultSorted = "total",
    defaultSortOrder = "desc",
    defaultPageSize = 12,
    selection = "multiple",
    onClick = "select"
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# roster stärke ----
## data ----
roster_depth_weekly <- shiny::reactive({
  roster_depth_weekly <- rfl_starter_data %>%
    dplyr::filter(season == input$selectYear) %>%
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
      pf = round(sum(pf, 2)),
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

## plot output ----
### season ----
output$roster_depth_season <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(roster_depth_season(), ggplot2::aes(x = points_back, y = pppg)) +
    roster_depth_plot(roster_depth_season()) +

    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = paste0(franchise_name, "\nPP: ", pppg, "\nPF: ", pf, "\nDiff: ", points_back), data_id = franchise_id),
      size = 7, alpha = 0.5, color = color_grey_mid
    ) +
    ggiraph::geom_point_interactive(
      data = subset(roster_depth_season(), franchise_id %in% input$selectRflTeams),
      ggplot2::aes(tooltip = paste0(franchise_name, "\nPP: ", pppg, "\nPF: ", pf, "\nDiff: ", points_back), color = franchise_name, data_id = franchise_id),
      size = 10
    ) +
    ggplot2::scale_color_discrete(type = colors) +

    ggplot2::labs(
      title = paste("RFL Rosterstärke Woche", max(roster_depth_weekly()$week), input$selectYear),
      x = "PF - PP pro Spiel",
      y = "PP pro Spiel",
      color = ""
    )

  girafe_default_output(plot, height = 12)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

### weekly ----
output$roster_depth_weekly <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(roster_depth_weekly(), ggplot2::aes(x = points_back, y = pppg)) +
    roster_depth_plot(roster_depth_weekly()) +

    ggiraph::geom_point_interactive(
      data = subset(roster_depth_weekly(), !franchise_id %in% c(input$selectRflTeams)),
      ggplot2::aes(tooltip = paste(franchise_name, "Woche", week), alpha = week, size = week, data_id = franchise_id), color = color_grey_mid
    ) +
    ggiraph::geom_point_interactive(
      data = subset(roster_depth_weekly(), franchise_id %in% c(input$selectRflTeams)),
      ggplot2::aes(tooltip = paste(franchise_name, "Woche", week), color = franchise_name, alpha = week, size = week * 1.5, data_id = franchise_id)
    ) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_alpha_continuous(range = c(0.5, 1), guide = "none") +
    ggplot2::scale_size_continuous(range = c(2, 7), guide = "none") +

    ggplot2::labs(
      title = paste0("RFL Rosterstärke Wochen ", min(roster_depth_weekly()$week), "-", max(roster_depth_weekly()$week), " ", season_before_wk_2),
      x = "PF - PP",
      y = "PP",
      color = ""
    )

  girafe_default_output(plot, height = 12)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ELO vs WAR ----
## data ----
elo_vs_war <- shiny::reactive({
  elo_vs_war <- rfl_team_elo %>%
    dplyr::filter(season == input$selectYear) %>%
    #dplyr::filter(season == 2025) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::select(franchise_id, franchise_name, franchise_elo_postgame) %>%
    dplyr::left_join(
      roster_war_sum() %>%
        dplyr::select(franchise_id, total),
      by = "franchise_id"
    )
})

output$elo_vs_war <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(elo_vs_war(), ggplot2::aes(x = franchise_elo_postgame, y = total)) +
    plot_quadrants(
      xmin = min(elo_vs_war()$franchise_elo_postgame, na.rm = TRUE),
      xmean = mean(elo_vs_war()$franchise_elo_postgame, na.rm = TRUE),
      xmax = max(elo_vs_war()$franchise_elo_postgame, na.rm = TRUE),
      ymin = min(elo_vs_war()$total, na.rm = TRUE),
      ymean = mean(elo_vs_war()$total, na.rm = TRUE),
      ymax = max(elo_vs_war()$total, na.rm = TRUE),
      ltl = "Auf dem Weg nach oben",
      ltr = "Sieht langfristig gut aus",
      lbr = "Auf dem Weg nach unten",
      lbl = "Rebuild"
    ) +

    # trendline
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = color_grey_dark, linetype = "dashed", se = FALSE, linewidth = 0.5) +

    # alle punkte
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = paste0(franchise_name, "\nELO: ", franchise_elo_postgame, "\nWAR: ", total), data_id = franchise_id),
      size = 7, alpha = 0.25, color = color_grey_mid
    ) +

    # ausgewählte punkte
    ggiraph::geom_point_interactive(
      data = subset(elo_vs_war(), franchise_id %in% input$selectRflTeams),
      ggplot2::aes(tooltip = paste0(franchise_name, "\nELO: ", franchise_elo_postgame, "\nWAR: ", total), color = franchise_name, data_id = franchise_id), size = 10
    ) +
    ggplot2::scale_color_discrete(type = colors) +

    ggplot2::scale_x_continuous(limits = c(min(elo_vs_war()$franchise_elo_postgame, na.rm = TRUE) - 25, max(elo_vs_war()$franchise_elo_postgame, na.rm = TRUE) + 25), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(min(elo_vs_war()$total, na.rm = TRUE) - 1, max(elo_vs_war()$total, na.rm = TRUE) + 1), expand = c(0, 0)) +

    plot_defaults +

    ggplot2::labs(
      title = paste("RFL ELO vs WAR", input$selectYear),
      subtitle = paste0("Die Grafik zeigt den aktuellen ELO-Wert (langfristiger Trend) eines Teams im Vergleich zu dessen Starter-WAR (kurzfristiger Trend).\nAlle Spieler mit mind.", input$selectRflGames, " Starts"),
      x = "ELO",
      y = "WAR",
      color = ""
    )

  girafe_default_output(plot, height = 13)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# TODO: plot_quadrants bg nach filtern nicht sichtbar

# Punkte nach Alter ----
## data ----
fpts_by_age <- shiny::reactive({
  fpts_by_age <- rfl_roster_data %>%
    dplyr::filter(season == input$selectYear & games_started >= input$selectRflGames) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::group_by(franchise_id, pos_grouped) %>%
    dplyr::summarize(
      franchise_name = last(franchise_name),
      age_avg = round(mean(age, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::mutate(total = round(mean(age_avg), 1)) %>%
    tidyr::pivot_wider(names_from = "pos_grouped", values_from = "age_avg") %>%
    dplyr::select(franchise_id, franchise_name, total, QB, RB, WR, TE, DL, LB, DB, PK)
})

## output ----
roster_by_age_position_coldef <- function(position) {
  reactable_coldef_bg(
    name = position,
    palette_fun = scale_rainbow(c(24:35), reverse = TRUE),
    width = 50
  )
}

output$roster_by_age <- reactable::renderReactable({
  reactable_default(
    fpts_by_age(),
    columns = c(
      list(
        franchise_id = reactable::colDef(show = FALSE),
        franchise_name = reactable::colDef(name = "Team", width = 200),
        total = reactable_coldef_bg(name = "Total", width = 75, palette_fun = scale_red_blue(fpts_by_age()$total, reverse = TRUE))
      ),
      stats::setNames(lapply(positions_grouped, roster_by_age_position_coldef), positions_grouped)
    ),
    defaultSorted = "total",
    defaultPageSize = 12,
    selection = "multiple",
    onClick = "select"
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ui output ----
output$roster_depth <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Roster Depth", input$selectYear)),
    shiny::fluidRow(
      shiny::column(
        htmltools::h2("WAR nach Positionen"),
        htmltools::HTML(paste0("Die Summe der Wins Above Replacement (WAR) aller Spieler mit mind. <i>", input$selectRflGames, " Starts</i> für das Team, aufgeteilt nach Position. Mit Klick in eine Zelle können die Spieler im Roster angezeigt werden.")),
        shinycssloaders::withSpinner(reactable::reactableOutput("team_war_by_position")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2(paste("Rosterstärke", input$selectYear)),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("roster_depth_season")),
        width = 6
      ),
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("roster_depth_weekly")),
        width = 6
      )
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("elo_vs_war")),
        width = 6
      ),
      shiny::column(
        htmltools::h3(paste("FPts nach Alter")),
        shinycssloaders::withSpinner(reactable::reactableOutput("roster_by_age")),
        width = 6
      )
    )
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)




#shiny::fluidPage(
#  shiny::fluidRow(
#    shiny::column(
#      shinycssloaders::withSpinner(plotly::plotlyOutput("roster_depth_season", height = "600px")),
#      width = 6
#    ),
#    shiny::column(
#      shinycssloaders::withSpinner(shiny::plotOutput("roster_depth_weekly")),
#      width = 6
#    ),
#    style = "height: 600px"
#  ),
#  shiny::fluidRow(
#    shiny::column(
#      shinycssloaders::withSpinner(gt::gt_output("depthChart")),
#      width = 3
#    ),
#    shiny::column(
#      shiny::fluidRow(
#        shiny::column(
#          shinycssloaders::withSpinner(shiny::plotOutput("elo_vs_war")),
#          width = 12
#        ),
#        style = "height: 600px"
#      ),
#      shiny::fluidRow(
#        shiny::column(
#          shinycssloaders::withSpinner(shiny::plotOutput("fptsByAge")),
#          width = 12
#        ),
#        style = "height: 1200px"
#      ),
#      shiny::fluidRow(
#        shiny::column(
#          tags$h4("Roster nach Alter"),
#          shinycssloaders::withSpinner(gt::gt_output("rosterByAge")),
#          width = 12
#        )
#      ),
#      width = 9
#    )
#  )
#)
