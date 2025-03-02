#source("R new/base_data/awards.R", local = TRUE)
source("R new/rankings/standing.R", local = TRUE)

output$player_awards <- shiny::renderUI({
  tags$img(src = paste0("https://github.com/bohndesverband/rfl-data/releases/download/awards_data/rfl_player-awards_plot-", input$selectYear, ".jpg"))
})

output$best_players <- shiny::renderUI({
  tags$img(src = paste0("https://github.com/bohndesverband/rfl-data/releases/download/awards_data/rfl_best-players_plot-", input$selectYear, ".jpg"))
})

output$league_mvps <- shiny::renderUI({
  tags$img(src = "https://github.com/bohndesverband/rfl-data/releases/download/awards_data/rfl_plot-mvps.jpg")
})
