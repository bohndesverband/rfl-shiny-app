standing_data_filtered <- shiny::reactive({
  standing_data_filtered <- rfl_standing_data %>%
    dplyr::filter(season == input$selectYear & week == input$selectWeek) %>%
    #dplyr::filter(season == 2026 & week == 8) %>%
    dplyr::mutate(
      eff_season = scales::percent(eff_season),
      dplyr::across(c(pf_season, pp_season, pa_season), ~ round(.x, 0))
    ) %>%
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

  data <- standing_data_filtered()

  if (input$selectYear == new_season_sept & input$selectWeek == current_week - 1) {
    data <- data %>%
      dplyr::left_join(
        rfl_schedule_sim_summary %>%
          dplyr::select(franchise_id, p_sb, p_sb_bye),
        by = "franchise_id"
      ) %>%
      dplyr::select(franchise_id:elo_post, dplyr::ends_with("_season"), dplyr::ends_with("_pctl"), dplyr::starts_with("p_"), -true_standing_pctl, true_standing_pctl, everything())
  }

  reactable_standing(
    data,
    columns = list(
      p_sb = reactable_coldef_bg_bar(
        header = reactable_header_with_tooltip("PO", "Schedule basierte Wahrscheinlichkeit in den Superbowl zu kommen"),
        pctl = "p_sb",
        data = data,
        format = "pct",
        width = 50,
      ),
      p_sb_bye = reactable_coldef_bg_bar(
        header = reactable_header_with_tooltip("BYE", "Schedule basierte Wahrscheinlichkeit eine Bye Week im Superbowl zu haben"),
        pctl = "p_sb_bye",
        data = data,
        format = "pct",
        width = 50
      )
      #p_pb = reactable_coldef_bg_bar(
      #  header = reactable_header_with_tooltip("PB", "Schedule basierte Wahrscheinlichkeit in den Pro Bowl zu kommen"),
      #  pctl = "p_pb",
      #  data = data,
      #  format = "pct",
      #  width = 50
      #),
      #p_pb_bye = reactable_coldef_bg_bar(
      #  header = reactable_header_with_tooltip("PB BYE", "Schedule basierte Wahrscheinlichkeit Bye Week im Pro Bowl zu haben"),
      #  pctl = "p_pb_bye",
      #  data = data,
      #  format = "pct",
      #  width = 50
      #)
    ),
    columnGroups = list(
      reactable::colGroup(
        name = "Chancen",
        columns = c("p_sb", "p_sb_bye")
      )
    ),
    hide_cols = names(data)[
      stringr::str_detect(names(data), "(_season|_rank)$") |
        names(data) %in% c("season", "week", "franchise_id", "division_name", "wins_over_exp", "wins_season", "losses_season", "winloss_season", "elo_post", "seed_emoji", "seed_total")
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
