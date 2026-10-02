# Output ----
output$player_report <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1("Spielerprofil", paste(selected_team_name(), input$selectYear))
  )
})
