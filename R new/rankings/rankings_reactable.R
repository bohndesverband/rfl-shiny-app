standing_data_filtered <- shiny::reactive({
  standing_data_filtered <- rfl_standing_data %>%
    dplyr::filter(season == input$selectYear & week == input$selectWeek) %>%
    #dplyr::filter(season == 2026 & week == 3) %>%
    dplyr::mutate(eff_season = scales::percent(eff_season)) %>%
    dplyr::select(franchise_id:conference_name, elo_post, dplyr::ends_with("_season"), wins_over_exp, wins_over_exp_pct,
                  wins_expected_end_of_season_pctl, all_play_wins_season_pctl, wins_season_pctl, pf_season_pctl, pp_season_pctl, eff_season_pctl, war_starter_season_pctl, elo_season_pctl, pa_season_pctl, true_standing_pctl,
                  dplyr::ends_with("_rank"), dplyr::ends_with("emoji"), -elo_pre_pctl, seed_total)
})

team_data <- function(selected_team) {
  current_standing_by_season %>%
    dplyr::filter(franchise_id == selected_team)
}

team_id_at <- function(data, index) {
  data$franchise_id[[index]]
}

fpts_bar_charts <- function(selected_team) {
  chart <- team_data(selected_team) %>%
    echarts4r::e_charts(x = week, height = "40px") %>%
    echarts4r::e_bar(serie = pp, stack = "grp", itemStyle = list(color = color_grey_light)) %>%
    echarts4r::e_bar(serie = pf, stack = "grp", itemStyle = list(color = color_green)) %>%
    finish_sparkline(
      y_min = min(current_standing_by_season$pf, na.rm = TRUE) - 30,
      y_max = max(current_standing_by_season$pp, na.rm = TRUE) + 10
      #tooltip = "
      #  function(params){
      #    return(
      #      '<strong>Woche ' + params[0].value[0] +
      #      '</strong><br />PP: ' + params[0].value[1] +
      #      '<br />PF: ' + params[1].value[1]
      #    );
      #  }
      #"
    )
}



pctl_color <- function(pctl) {
  grDevices::colorRampPalette(
    c(color_red, color_orange, color_yellow, color_green, color_blue)
  )(101)[round(pctl * 100) + 1]
}

team_chart_cell <- function(renderer) {
  function(value, index) {
    renderer(team_id_at(rfl_current_standing, index))
  }
}

output$current_standing <- reactable::renderReactable({
  input$showPctl

  data <- standing_data_filtered()

  reactable_standing(
    data,
    hide_cols = names(data)[
      stringr::str_detect(names(data), "(_season|_rank)$") |
        names(data) %in% c("franchise_id", "division_name", "wins_over_exp", "wins_season", "losses_season", "winloss_season", "elo_post", "seed_emoji", "seed_total")
    ],
    groupBy = "conference_name",
    defaultSorted = c("seed_total"),
    height = 800
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ui output ----
output$league_ranking <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Ranking", "Woche", input$selectWeek, input$selectYear)),
      shiny::fluidRow(
        shiny::column(
          #shinyWidgets::prettySwitch("showPctl", "Zeige Perzentile statt Werte", value = FALSE, fill = TRUE, status = "primary"),
          shinycssloaders::withSpinner(reactable::reactableOutput("current_standing")),
          width = 12
      )
    )
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)
