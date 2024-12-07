selected_team <- shiny::reactive({
  franchises %>%
    dplyr::filter(franchise_id == input$selectRflTeam)
})

## Draftorder Verlauf ----

source("R new/reports/team/draftorder_history.R", local = TRUE)
source("R new/roster/ir.R", local = TRUE)
source("R new/rankings/power_ranking.R", local = TRUE)

# Output ----
output$teamReport <- shiny::renderUI({
  shiny::fluidPage(
    #tags$h1(paste(selected_team()$franchise_name, "Team Report", new_season_sept)),
    #shiny::plotOutput("pickHistory"),

    shiny::fluidRow(
      shiny::column(
      #tags$h2("Rankings"),
        shiny::plotOutput("team_report_power_ranking"),
        width = 8
      ),
      shiny::column(
        shiny::renderText("Text"),
        width = 4
      )
    )
  )
})
