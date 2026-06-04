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
#output$teamReport <- shiny::renderUI({
#  shiny::fluidPage(
    #tags$h1(paste(selected_team()$franchise_name, "Team Report", new_season_sept)),
    #shiny::plotOutput("pickHistory"),

#    shiny::fluidRow(
#      shiny::column(
      #tags$h2("Rankings"),
#        shiny::plotOutput("team_report_power_ranking"),
#        width = 8
#      ),
 #     shiny::column(
#        shiny::renderText("Text"),
#        width = 4
#      )
#    )
#  )
#})
