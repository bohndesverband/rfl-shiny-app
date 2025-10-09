source("R new/rankings/standing.R", local = TRUE)
source("R new/rankings/ranking_tables.R", local = TRUE)

total_games <- 27

magic_number_data <- expand.grid(
    franchise_id = rfl_current_standing$franchise_id,
    franchise_id_b = rfl_current_standing$franchise_id
  ) %>%
  dplyr::filter(franchise_id != franchise_id_b) %>%  # Ausschließen von Paaren mit derselben Franchise
  dplyr::left_join(
    rfl_current_standing,
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    rfl_current_standing %>%
      dplyr::select(franchise_id, franchise_name, conference_name, wins_total, losses_total) %>%
      dplyr::rename(franchise_name_b = franchise_name, wins_b = wins_total, losses_b = losses_total, conference_b = conference_name),
    by = c("franchise_id_b" = "franchise_id")
  ) %>%
  dplyr::filter(conference_name == conference_b) # Nur paare aus gleicher conf

magic_number_conf <- shiny::reactive({
  magic_number_conf <- magic_number_data %>%
    dplyr::filter(conference_name == input$selectRflConference) %>%
    dplyr::mutate(
      magic_number = total_games - wins_total - losses_b,
      magic_number = ifelse(magic_number <= 0 | magic_number > 26 - (week * 2), NA, magic_number)
    ) %>%
    dplyr::filter(magic_number != "") %>%
    dplyr::select(-franchise_id_b, -franchise_elo_pregame:-elo_shift, -pf_rank:-power_rank_emoji, -conference_b:-losses_b) %>%
    tidyr::spread(franchise_name_b, magic_number, fill = "") %>%
    dplyr::arrange(league_rank)
})

output$magic_number_table <- gt::render_gt({
  #magic_number_conf <- magic_number_conf()[, !(colnames(df) %in% input$selectRflTeams)]


  selected_teams <- magic_number_conf() %>%
    dplyr::filter(conference_name == input$selectRflConference) %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        franchise_id %in% input$selectRflTeams
      else
        TRUE
    ) %>%
    dplyr::pull(franchise_name)

  last_team <- tail(selected_teams, n = 1)

  magic_number_conf() %>%
  dplyr::select(
    1:21,
    dplyr::any_of(selected_teams)
  ) %>%
  gt::gt() %>%
  gt::tab_header(
    title = paste("Magic Numbers RFL", magic_number_conf()$conference_name[1], "Conference"),
    subtitle = paste("Woche", magic_number_conf()$week[1], magic_number_conf()$season[1])
  ) %>%
  ranking_table_base %>%
  ranking_table_standing %>%
  ranking_table_bowl %>%
  gtDefaults() %>%
  gt::data_color(
    22:last_team,
    palette = c(color_bg, color_grey_mid, color_grey_light)
  ) %>%
  gt::cols_hide(conference_name)
})

shiny::observeEvent(input$selectRflConference, {
  req(input$selectRflConference)

  conf_teams <- rfl_franchise_data %>%
    dplyr::filter(conference_name == input$selectRflConference)

  shinyWidgets::updatePickerInput(
    session,
    inputId = "selectRflDivisions",
    choices = setNames(unique(conf_teams$division[order(conf_teams$division_name)]), unique(conf_teams$division_name[order(conf_teams$division_name)]))
  )
})

shiny::observeEvent(input$selectRflDivisions, {
  req(input$selectRflDivisions)

  conf_teams <- rfl_franchise_data %>%
    dplyr::filter(conference_name == input$selectRflConference)

  selected_teams <- conf_teams %>%
    dplyr::filter(division %in% input$selectRflDivisions) %>%
    dplyr::pull(franchise_id)

  shinyWidgets::updatePickerInput(
    session,
    inputId = "selectRflTeams",
    choices = setNames(conf_teams$franchise_id[order(conf_teams$franchise_name)], conf_teams$franchise_name[order(conf_teams$franchise_name)]),
    selected = selected_teams
  )
})
