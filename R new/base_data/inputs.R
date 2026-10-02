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

  # years
  if (active_tab() == "#section-hit-rates") {
    # draft klasse
    shiny::updateSliderInput(session, "selectYears", min = 2017, max = 2025, value = c(2017, new_season_march - 3))
  } else if (active_tab() == "#section-hit-rates") {
    # draft hit rates
    shiny::updateSliderInput(session, "selectYears", min = 2017, value = c(2017, new_season_march - 3))
  } else if(active_tab() == "#section-trade-history") {
    shiny::updateSliderInput(session, "selectYears", min = 2016, max = 2026, value = c(2026, 2026))
  } else if(active_tab() == "#section-fantasy-finishes") {
    shiny::updateSliderInput(session, "selectYears", min = 2016, max = new_season_sept, value = c(new_season_sept, new_season_sept))
  } else if (active_tab() == "#section-strength-of-schedule") {
    # sos
    shiny::updateSliderInput(session, "selectYears", min = 2024, max = 2026, value = c(2026, 2026))
  } else if (active_tab() == "#section-wochenbericht") {
    shiny::updateSliderInput(session, "selectWeek", value = current_week - 1)
  } else {
    shiny::updateSliderInput(session, "selectYears", min = 2016, max = new_season_sept, value = c(new_season_sept, new_season_sept))
  }

  # year
  if (active_tab() == "#section-draftklassen") {
    shiny::updateSliderInput(session, "selectYear", min = 2017, max = max(rfl_drafts_data$season), value = new_season_sept - 2)
  } else if (active_tab() == "#section-report") {
    shiny::updateSliderInput(session, "selectYear", min = 2017, max = max(rfl_drafts_data$season), value = max(rfl_drafts_data$season))
  } else if (active_tab() == "#section-awards") {
    shiny::updateSliderInput(session, "selectYear", min = 2016, max = 2025, value = max(rfl_drafts_data$season))
  } else {
    #shiny::updateSliderInput(session, "selectYear", min = 2016, max = new_season_sept, value = new_season_sept)
  }

  if (active_tab() == "#section-elo") {
    # player elo
    req(mfl_players_preselection())
    req(nrow(mfl_players_preselection()) > 0)

    top_player_id <- find_top_elo_player(mfl_players_preselection())

    shinyWidgets::updatePickerInput(session, "selectPosition", selected = "QB")
    shinyWidgets::updatePickerInput(session, "selectPlayers", choices = setNames(mfl_players_preselection()$player_id, mfl_players_preselection()$player_name), selected = top_player_id)
  }

  # positions
  if (active_tab() == "#section-trade-history") {
    # trade history
    shinyWidgets::updatePickerInput(
      session,
      "selectPositions",
      selected = ""
    )
  } else if(active_tab() == "#section-fantasy-finishes" | active_tab() == "#section-roster-tiefe" | active_tab() == "#section-draftboards" | active_tab() == "#section-draftklassen" | active_tab() == "#section-big-play-punkte") {
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

  # rfl teams
  if (active_tab() == "#section-magic-number") {
    #conf_teams <- rfl_franchise_data %>%
    #  dplyr::filter(conference_name == input$selectRflConference)

    #shinyWidgets::updatePickerInput(
    #  session,
    #  "selectRflTeams",
    #  choices = setNames(conf_teams$franchise_id[order(conf_teams$franchise_name)], conf_teams$franchise_name[order(conf_teams$franchise_name)]),
    #  selected = NULL
    #)
  } else {
    shinyWidgets::updatePickerInput(
      session,
      "selectRflTeams",
      choices = setNames(rfl_franchise_data$franchise_id[order(rfl_franchise_data$franchise_name)], rfl_franchise_data$franchise_name[order(rfl_franchise_data$franchise_name)]),
      selected = NULL,
      options = list("actions-box" = TRUE, "none-selected-text" = "RFL Team wählen")
    )
  }
})

shiny::observeEvent(
  list(active_tab(), input$selectYear),
  {
    tab <- active_tab()
    year <- input$selectYear

    if (length(tab) == 0 || length(year) == 0 || is.null(year) || is.na(year)) {
      return()
    }

    if (tab == "#section-roster-tiefe" && year < season_before_wk_2) {
      shiny::updateSliderInput(
        session,
        "selectRflGames",
        "Anzahl Spiele als Starter",
        max = 13,
        value = 7
      )
    } else if (tab == "#section-roster-tiefe" && year == season_before_wk_2) {
      shiny::updateSliderInput(
        session,
        "selectRflGames",
        "Anzahl Spiele als Starter",
        max = current_week - 1,
        value = ifelse(current_week >= 13, 8, floor(current_week * 0.5))
      )
    }
  }
)

# create preselection of mfl players ----
mfl_players_preselection <- shiny::reactive({
  req(input$selectPosition)

  # Grundfilter basierend auf der Position
  mfl_players_preselection <- mfl_players %>%
    dplyr::mutate(season = new_season_march) %>%
    dplyr::filter(grouped_pos %in% input$selectPosition) %>%
    dplyr::arrange(player_name)

  # Optionaler Filter basierend auf dem ausgewählten Team
  if (!is.null(input$selectRflTeams) &&
      length(input$selectRflTeams) > 0 &&
      all(input$selectRflTeams != "")) {

    mfl_players_preselection <- mfl_players_preselection %>%
      dplyr::filter(grepl(paste0("\\b", input$selectRflTeams, "\\b"), franchise_ids)) %>%
      dplyr::arrange(dplyr::desc(player_elo_post))
  }

  mfl_players_preselection
})

# set inputs based on other inputs ----
shiny::observe({
  req(mfl_players_preselection())
  req(nrow(mfl_players_preselection()) > 0)

  top_player_id <- find_top_elo_player(mfl_players_preselection())

  shinyWidgets::updatePickerInput(session, "selectPlayers", choices = setNames(mfl_players_preselection()$player_id, mfl_players_preselection()$player_name), selected = top_player_id)
})

#shiny::observe({
#  req(input$selectRflDivisions)

#  div_teams <- rfl_franchise_data %>%
#    dplyr::filter(division %in% input$selectRflDivisions)

#  shinyWidgets::updatePickerInput(
#    session,
#    "selectRflTeams",
#    selected = setNames(div_teams$franchise_id, div_teams$franchise_name)
#  )
#})

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
