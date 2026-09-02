#source("R new/base_data/awards.R", local = TRUE)
#source("R new/reports/league/mvp.R", local = TRUE)

#source("R new/rankings/standing.R", local = TRUE)
#source("R new/rankings/ranking_tables.R", local = TRUE)


selected_team <- "0007"

# Output ----
output$leagueReport <- shiny::renderUI({
  shiny::fluidPage(
    #tags$h1(paste(selected_team()$franchise_name, "Team Report", new_season_sept)),
    #shiny::plotOutput("pickHistory"),

    shiny::fluidRow(
      shiny::column(
        #id = "standing",
        #gt::gt_output("conf_standing_table_report"),
        width = 12
      ),
    ),

    shiny::fluidRow(
      shiny::column(
        #shiny::plotOutput("rflMVP"),
        width = 8
      ),
      shiny::column(
        #shiny::renderText("Text"),
        width = 4
      )
    )
  )
})
