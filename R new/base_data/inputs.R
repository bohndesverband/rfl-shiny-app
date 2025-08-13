find_top_elo_player <- function(df) {
  df %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::arrange(dplyr::desc(player_elo_post)) %>%
    head(3) %>%
    dplyr::pull(player_id)
}

# set reactive to active tab ----
active_tab <- shiny::reactive({
  input$active_tab
})

## debug ----
output$active_tab <- renderText({
  paste("Aktive Tab:", active_tab())
})

# set inputs based on active tab ----
shiny::observeEvent(active_tab(), {

  if (active_tab() == "#section-draftklassen" || active_tab() == "#section-hit-rates") {
    # draft klasse
    shiny::updateSliderInput(session, "selectYears", min = 2017, max = 2025, value = c(2017, new_season_march - 3))
  } else if (active_tab() == "#section-hit-rates") {
    # draft hit rates
    shiny::updateSliderInput(session, "selectYears", min = 2017, value = c(2017, new_season_march - 3))
  } else if(active_tab() == "#section-basics") {
    # draft basics
    shiny::updateSliderInput(session, "selectYears", min = 2017, max = season_before_wk_2, value = c(2017, season_before_wk_2))
    # season_before_wk_2 weil nach gsis_id gesynct wird und die erst nach den ersten spielen vorhanden ist
  } else if(active_tab() == "#section-trade-history") {
    shiny::updateSliderInput(session, "selectYears", min = 2016, max = 2025, value = c(2025, 2025))
  } else if(active_tab() == "#section-fantasy-finishes") {
    shiny::updateSliderInput(session, "selectYears", min = 2016, max = new_season_march - 1, value = c(new_season_march - 3, new_season_march - 1))
  } else if (active_tab() == "#section-strength-of-schedule") {
    # sos
    shiny::updateSliderInput(session, "selectYears", min = 2024, max = 2025, value = c(2025, 2025))
  } else {
    shiny::updateSliderInput(session, "selectYears", min = 2016, max = new_season_sept, value = c(new_season_sept, new_season_sept))
  }

  if (active_tab() == "#section-elo") {
    # player elo

    req(mfl_players_preselection())
    req(nrow(mfl_players_preselection()) > 0)

    top_player_id <- find_top_elo_player(mfl_players_preselection())

    shinyWidgets::updatePickerInput(session, "selectPosition", selected = "QB")
    shinyWidgets::updatePickerInput(session, "selectPlayers", choices = setNames(mfl_players_preselection()$player_id, mfl_players_preselection()$player_name), selected = top_player_id)
  }

  if (active_tab() == "#section-trade-history") {
    # trade history
    shinyWidgets::updatePickerInput(
      session,
      "selectPositions",
      selected = ""
    )
  } else if(active_tab() == "#section-fantasy-finishes" | active_tab() == "#section-roster-tiefe") {
    shinyWidgets::updatePickerInput(
      session,
      "selectPositions",
      choices = positions_grouped,
      selected = positions_grouped,
    )
  } else {
    shinyWidgets::updatePickerInput(
      session,
      "selectPositions",
      selected = list("QB", "RB", "WR", "TE", "DT", "DE", "LB", "CB", "S"),
    )
  }
})

# create preselection of mfl players ----
mfl_players_preselection <- shiny::reactive({
  req(input$selectPosition)

  # Grundfilter basierend auf der Position
  filtered_data <- mfl_players %>%
    dplyr::filter(grouped_pos %in% input$selectPosition) %>%
    dplyr::arrange(player_name)

  # Optionaler Filter basierend auf dem ausgewählten Team
  if (!is.null(input$selectRflTeam) && input$selectRflTeam != "") {
    filtered_data <- filtered_data %>%
      dplyr::filter(grepl(paste0("\\b", input$selectRflTeam, "\\b"), franchise_ids)) %>%
      dplyr::arrange(dplyr::desc(player_elo_post))
  }

  filtered_data
})

# set inputs based on other inputs ----
shiny::observe({
  req(mfl_players_preselection())
  req(nrow(mfl_players_preselection()) > 0)

  top_player_id <- find_top_elo_player(mfl_players_preselection())

  shinyWidgets::updatePickerInput(session, "selectPlayers", choices = setNames(mfl_players_preselection()$player_id, mfl_players_preselection()$player_name), selected = top_player_id)
})

shiny::observe({
  req(input$selectRflDivisions)

  div_teams <- rfl_franchise_data %>%
    dplyr::filter(division %in% input$selectRflDivisions)

  shinyWidgets::updatePickerInput(
    session,
    "selectRflTeams",
    selected = setNames(div_teams$franchise_id, div_teams$franchise_name)
  )
})

# zeige alte teamnamen in auswahl an, wenn checkbox ausgewählt
observeEvent(input$showHistoricTeamNames, {
  # Wähle Datenquelle basierend auf Checkbox
  if (input$showHistoricTeamNames) {
    historic_rfl_names <- rfl_draft_orders %>%
      dplyr::mutate(
        type = ifelse(season == max(season), "Aktuelle Teamnamen", "Ehemalige Teamnamen")
      ) %>%
      dplyr::select(franchise_id, historic_name, type) %>%
      dplyr::distinct() %>%
      dplyr::group_by(historic_name) %>%
      # filter aktuelle namen aus ehemaligen heraus
      dplyr::arrange(type) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()

    team_choices <- setNames(
      historic_rfl_names$franchise_id[order(historic_rfl_names$historic_name)],
      historic_rfl_names$historic_name[order(historic_rfl_names$historic_name)]
    )

    # TODO: in kategorien einteilen
  } else {
    team_choices <- setNames(
      rfl_franchise_data$franchise_id[order(rfl_franchise_data$franchise_name)],
      rfl_franchise_data$franchise_name[order(rfl_franchise_data$franchise_name)]
    )
  }

  # Update Picker Input
  shinyWidgets::updatePickerInput(
    session,
    inputId = "selectRflTeams",
    choices = team_choices,
    selected = NULL  # Optional: leere Auswahl setzen
  )
})
