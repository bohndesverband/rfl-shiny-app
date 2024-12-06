output$test <- shiny::renderUI({
  shiny::selectizeInput(
    "selectRflTeam",
    "Filter nach Team",
    choices = setNames(franchises$franchise_id, franchises$franchise_name),
    multiple= FALSE,
    selected = "Jena Dragons"
    #options = list(
    #  placeholder = "Team wählen",
    #  onInitialize = I('function() { this.setValue(""); }')
    #)
  )
})

shiny::uiOutput("test")
