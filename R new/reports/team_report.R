source("R new/reports/team/team_report_draft.R", local = TRUE)

# team mvp ----
#rfl_roster_data %>%
#  dplyr::filter(season == max(season)) %>%
#  dplyr::filter(week == max(week)) %>%
#  dplyr::left_join(
#    rfl_war_data %>%
#      dplyr::group_by(player_id) %>%
#      dplyr::arrange(dplyr::desc(season)) %>%
#      dplyr::slice(1) %>%
#      dplyr::select(-season),
#    by = "player_id"
#  ) %>%
#  dplyr::group_by(franchise_id) %>%
#  dplyr::filter(war == max(war, na.rm = TRUE)) %>%
#  dplyr::ungroup() %>%
#  filter(franchise_id == "0007")


#selected_team <- shiny::reactive({
#  rfl_franchise_data %>%
#    dplyr::filter(franchise_id == input$selectRflTeam)
#})

## Draftorder Verlauf ----

#source("R new/reports/team/draftorder_history.R", local = TRUE)
#source("R new/roster/ir.R", local = TRUE)
#source("R new/rankings/power_ranking.R", local = TRUE)

# Output ----
output$team_report <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1("Teambericht", paste(selected_team_name())),
    htmltools::h2("Draft"),
    shiny::fluidRow(
      shiny::column(
        htmltools::h3("Alle Draftklassen"),
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_team")),
        width = 6
      ),
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_team_chart")),
        width = 6
      )
    ),
    shiny::fluidRow(
      shiny::column(
        htmltools::h3("Alle Zu- und Abgänge"),
        #shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_team")),
        width = 6
      ),
      shiny::column(
        #shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_team_chart")),
        width = 6
      )
    )
  )
})

