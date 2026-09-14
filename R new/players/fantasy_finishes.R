fantasy_finishes_sum <- shiny::reactive({
  fantasy_finishes_sum <- rfl_fantasy_finishes_season %>%
    dplyr::group_by(player_id) %>%
    dplyr::arrange(season) %>%
    dplyr::summarise(
      dplyr::across(c(player_name, pos, team), ~ last(.x)),
      dplyr::across(c(games, points, dplyr::ends_with("_season")), ~ round(sum(.x, na.rm = TRUE), 2)),
      years = n()
    ) %>%
    dplyr::mutate(
      ppg = round(points / games, 2)
    ) %>%
    dplyr::group_by(pos) %>%
    dplyr::arrange(dplyr::desc(ppg)) %>%
    dplyr::mutate(
      ppg_rank = dplyr::row_number()
    ) %>%
    dplyr::arrange(dplyr::desc(points)) %>%
    dplyr::mutate(
      fpts_rank = dplyr::row_number()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      across(c(dplyr::ends_with("_season")), ~ round(.x / years, 2))
    )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

output$fantasy_finishes_yearly <- reactable::renderReactable({
  row_details <- function(index) {
    selected_player <- fantasy_finishes_sum[index, ]$player_id

    player_data <- rfl_fantasy_finishes_weekly %>%
      #filter(player_id == "15281")
      dplyr::filter(player_id == selected_player)

    ggplot2::ggplot(player_data, ggplot2::aes(x = points)) +
      ggplot2::geom_density(data = subset(rfl_fantasy_finishes_weekly, pos == player_data$pos & points > 0), fill = color_grey_light, color = NA) +
      ggplot2::geom_density(ggplot2::aes(color = pos), size = 1) +
      ggplot2::scale_x_continuous(limits = c(-10, 70), breaks = c(-10, 0, 10, 20, 30, 40, 50, 60), expand = c(0, 0)) +
      ggplot2::scale_color_discrete(guide = "none") +
      plot_defaults +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        y = ""
      )
  }

  reactable_default(
    fantasy_finishes_sum,
    columns = list(
      player_id = reactable::colDef(show = FALSE),
      player_name = reactable::colDef(name = "Spieler", width = 175),
      pos = reactable::colDef(name = "Pos", width = 50),
      team = reactable::colDef(name = "Team", width = 75),
      points = reactable::colDef(name = "FPts", width = 100),
      fpts_rank = reactable::colDef(name = "Rank", width = 75),
      years = reactable::colDef(name = "Jahre", width = 75),
      games = reactable::colDef(name = "Spiele", width = 75),
      ppg = reactable::colDef(name = "FPts", width = 75),
      ppg_rank = reactable::colDef(name = "Rank", width = 75),
      top3_season = reactable::colDef(name = "Top 3", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top5_season = reactable::colDef(name = "Top 5", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top8_season = reactable::colDef(name = "Top 8", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top12_season = reactable::colDef(name = "Top 12", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top24_season = reactable::colDef(name = "Top 24", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top36_season = reactable::colDef(name = "Top 36", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top48_season = reactable::colDef(name = "Top 48", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE)),
      top60_season = reactable::colDef(name = "Top 60", width = 75, format = reactable::colFormat(digits = 0, percent = TRUE))
    ),
    columnGroups = list(
      colGroup(name = "Total", columns = c("years", "games", "points", "fpts_rank")),
      colGroup(name = "Per Game", columns = c("ppg", "ppg_rank")),
      colGroup(name = "Elite", columns = c("top3_season", "top5_season")),
      colGroup(name = "Stud", columns = c("top8_season", "top12_season")),
      colGroup(name = "Starter", columns = c("top24_season", "top36_season")),
      colGroup(name = "Backup", columns = c("top48_season", "top60_season"))
    ),
    details = row_details,
    defaultSorted = c("points"),
    defaultSortOrder = "desc",
    filterable = TRUE,
    pagination = TRUE,
    defaultPageSize = 12
  )
})





reactable(
  columnGroups =
)














rfl_fantasy_finishes_weekly_filtered <- shiny::reactive({
  rfl_fantasy_finishes_weekly %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
      #season >= 2025 & season <= 2025
    ) %>%
    dplyr::select(-dplyr::ends_with("_season")) %>%
    dplyr::rename_with(~ gsub("_weekly", "", .x)) %>%

    summarize_fantasy_finishes() %>%

    dplyr::filter(
      if(isTruthy(input$selectPositions))
        sapply(seq_along(pos), function(i) {
          any(input$selectPositions %in% trimws(strsplit(pos[i], ",")[[1]]))
        })
      else
        TRUE
    )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)














# ui output ----
output$fantasy_finishes <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Draft", input$selectYear)),
    shiny::fluidRow(
      shiny::column(
        htmltools::h2("Alle Picks des RFL Drafts"),
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_picks")),
        width = 6
      ),
      shiny::column(
        htmltools::h2("Alle Picks der RFL Draftklassen"),
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_teams")),
        width = 6
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("Welche Draftklasse ist am wertvollsten?"),
    #htmltools::div("Abgebildet sind alle Draftpicks mit ihrem derzeitigen Wert nach pVAR. Die Rote kurve zeigt den pVARexp des entsprechenden Picks."),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_impact")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("Welche Picks sind am wertvollsten gewesen?"),
    htmltools::div("Abgebildet sind alle Draftpicks mit ihrem derzeitigen Wert nach pVAR. Die Rote kurve zeigt den pVARexp des entsprechenden Picks."),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_voe")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("RFL Draftboard"),
    shinyWidgets::prettySwitch("sortForFpts", "Draftboard nach FPts sortieren", value = FALSE, fill = TRUE, status = "primary"),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(shiny::plotOutput("draft_board", height = "2600px")),
        width = 12
      )
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_reaches")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("Wie viel haben die Teams in die verschiedenen Positionsgruppen investiert?"),
    htmltools::div("Summe der Werte, die für eine bestimmte Positionsgruppe investiert wurden. pVARexp zeigt dabei, wie viel ein Team investiert hat. Schaust du dir VOE an, siehst du, wie erfolgreich es dabei war. Um die Effzizenz statt die Summe zu sehen ändere die Ansicht zu \"per Pick\". Mit Klick auf die Pfeile kannst du dir die konkreten Picks anschauen."),
    htmltools::div(
      shinyWidgets::prettySwitch("draft_class_voe_exp_toggle", label = "VOE statt pVARexp zeigen", status = "primary", fill = TRUE),
      shinyWidgets::prettySwitch("draft_class_voe_exp_toggle_per_pick", label = "per Pick Daten zeigen", status = "primary", fill = TRUE),
      style = "display: flex; align-items: center; flex-wrap: wrap; gap: 1rem; margin-top: 2rem;"
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_class_voe_exp")),
        width = 12
      )
    )
  )
})
