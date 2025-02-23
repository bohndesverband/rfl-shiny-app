source("R new/base_data/awards.R", local = TRUE)

output$player_awards <- shiny::renderUI({
  tags$img(src = paste0("https://github.com/bohndesverband/rfl-data/releases/download/awards_data/rfl-player-awards-plot-", input$selectYear, ".jpg"))
})
