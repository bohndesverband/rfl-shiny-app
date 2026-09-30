transactions_filtered <- shiny::reactive({
  transactions_filtered <- rfl_transactions_data %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::select(-season, -timestamp, -type, -player_id, -franchise_id, -player_elo_current, -player_elo_max)
})

with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
            title = tooltip, value)
}

output$players_transactions_table <- reactable::renderReactable({
  reactable_default(
    transactions_filtered(),
    columns = list(
      date = reactable::colDef("Datum", format = reactable::colFormat(datetime = TRUE), width = 175),
      week = reactable::colDef("WK", width = 50),
      display_name = reactable::colDef("Spieler", width = 150),
      position = reactable::colDef("Pos", width = 50),
      team = reactable::colDef("Team", width = 75, sticky = "left"),
      fpts = reactable_coldef_bg("FPts", palette_fun = scale_rainbow(min(transactions_filtered()$fpts, na.rm = TRUE):max(transactions_filtered()$fpts, na.rm = TRUE)), minWidth = 75),
      ppg = reactable_coldef_bg("PPG", palette_fun = scale_rainbow(min(transactions_filtered()$ppg, na.rm = TRUE):max(transactions_filtered()$ppg, na.rm = TRUE)), minWidth = 75),
      war = reactable_coldef_bg("WAR", palette_fun = scale_rainbow(min(transactions_filtered()$war, na.rm = TRUE):max(transactions_filtered()$war, na.rm = TRUE)), minWidth = 75, class = "border-right"),
      player_elo = reactable::colDef(
        header = with_tooltip("ELO", "ELO zum Zeitpunkt der Transaktion. Balken zeigt Perzentil innerhalb der Positionsgruppe."), width = 75,
        style = function(value, index) {
          pctl <- transactions_filtered()$player_elo_pctl[index]

          reactable_coldef_bar_bg(width = pctl)
        }
      ),
      elo_shift = reactable_coldef_color(
        header = with_tooltip("+/-", "ELO-Veränderung seit der Transaktion"),
        palette_fun = scale_red_blue(min(transactions_filtered()$elo_shift, na.rm = TRUE):max(transactions_filtered()$elo_shift, na.rm = TRUE)), minWidth = 50,
        cell = function(value) {
          content <- value

          if (!is.na(value) & value > 0) {
            content <- paste0("+", value)
          }

          content
        }
      ),
      franchise_name = reactable::colDef("Team"),
      type_desc = reactable::colDef(
        "Transaktion",
        html = TRUE,
        cell = function(value) {
          color <- "green"

          if (value == "dropped") {
            color <- "red"
          }

          htmltools::span(value, class = paste("badge", color))
        }
      ),
      player_elo_pctl = reactable::colDef(show = FALSE),
      player_elo_current_pctl = reactable::colDef(show = FALSE)
    ),
    columnGroups = list(
      reactable::colGroup(
        "Spieler",
        columns = c("display_name", "position", "team")
      ),
      reactable::colGroup(
        "Fantasy Pts",
        columns = c("fpts", "ppg", "war")
      ),
      reactable::colGroup(
        "ELO",
        columns = c("player_elo", "elo_shift")
      )
    ),
    filterable = TRUE,
    defaultPageSize = 15
  )
})


 # free agents ----
 #rfl_transactions_data %>%
 #  dplyr::arrange(dplyr::desc(timestamp)) %>%
#   dplyr::
#
# rfl_fantasy_finishes_season

 ## alle transactions ----

 # ui output ----
 output$players_transactions <- shiny::renderUI({
   shiny::fluidPage(
     htmltools::h1("Transaktionen"),
     shiny::tabsetPanel(
       shiny::tabPanel(
         "Transaktionen",
         shiny::fluidPage(
           htmltools::h2("Historie"),
           shiny::fluidRow(
             shiny::column(
               shinycssloaders::withSpinner(reactable::reactableOutput("players_transactions_table")),
               width = 12
             )
           ),
           #shiny::fluidRow(
           #  shiny::column(
          #     shinycssloaders::withSpinner(gt::gt_output("team_roster")),
          #     width = 12
          #   )
          # )
         )
       ),
       shiny::tabPanel(
         "Trades",
         shiny::fluidPage(
           htmltools::h2("Saison"),
           shiny::fluidRow(
             htmltools::h3("Schedule"),
             shiny::column(
               #shinycssloaders::withSpinner(ggiraph::girafeOutput("team_schedule")),
               width = 12
             )
           )
         )
       ),
       shiny::tabPanel(
         "Offseason",
         shiny::fluidPage(
           htmltools::h2("Offseason"),

           shiny::fluidRow(
             shiny::column(
               htmltools::h3("Alle Draftklassen"),
               #shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_team")),
               width = 7
             ),
             shiny::column(
               #shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_team_chart")),
               width = 5
             )
           ),

           htmltools::h3("Einzelne Draftklasse"),
           shiny::fluidRow(
             shiny::column(
               #shinycssloaders::withSpinner(shiny::plotOutput("draft_class_capital")),
               width = 6
             ),
             shiny::column(
               #shinycssloaders::withSpinner(shiny::plotOutput("draft_class_capital_positions")),
               width = 6
             )
           ),
           shiny::fluidRow(
             shiny::column(
               htmltools::div(
                 class = "flex",
                 shinyWidgets::radioGroupButtons(
                   "selectDraftClassCharts",
                   choices = c("ADP", "Bewertung", "VOE", "WAR", "ELO")
                 ),
                 #shinyWidgets::prettySwitch("showLeagueComparison", "Zeige Picks im Vergleich zur Klasse", value = FALSE, fill = TRUE, status = "primary")
                 # TODO: liga vergleich charts
               ),
               #shinycssloaders::withSpinner(ggiraph::girafeOutput("team_draft_class")),
               width = 12
             )
           ),
           shiny::fluidRow(
             htmltools::h4("Bewertung"),
             shiny::column(
               #shinycssloaders::withSpinner(reactable::reactableOutput("team_draft_class_grades")),
               width = 12
             )
           )

         )
       )
     )
   )
 })
