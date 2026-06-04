gt_fantasy_finishes <- function(df) {
  df %>%
    dplyr::arrange(dplyr::desc(top5), dplyr::desc(points)) %>%
    #dplyr::select(-last_season) %>%
    dplyr::rename(display_name = player_name, position = pos) %>%
    gt::gt() %>%
    gt::cols_hide(c(player_id, last_season)) %>%
    gt::tab_spanner(
      label = "Elite",
      columns = c(top3, top5)
    ) %>%
    gt::tab_spanner(
      label = "Stud",
      columns = c(top8, top12)
    ) %>%
    gt::tab_spanner(
      label = "Starter",
      columns = c(top24, top36)
    ) %>%
    gt::tab_spanner(
      label = "Backup",
      columns = c(top48, top60)
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_align(
      align = "left",
      columns = c(display_name)
    ) %>%

    #gt::fmt_percent(
    #  dplyr::starts_with("top"),
    #  decimals = 0
    #) %>%

    gt::data_color(
      dplyr::starts_with("top"),
      palette = c(color_red, color_yellow, color_green, color_blue)
    ) %>%
    gt::cols_label(
      seasons = "Saisons",
      points = "FPts",
      top3 = "Top 3",
      top5 = "Top 5",
      top8 = "Top 8",
      top12 = "Top 12",
      top24 = "Top 24",
      top36 = "Top 36",
      top48 = "Top 48",
      top60 = "Top 60"
    ) %>%

    gtDefaults() %>%

    gt_player() %>%
    gt::cols_width(
      display_name ~ px(150),
      c(position, team) ~ px(70),
      c(seasons, points) ~ px(100)
    )
}

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

output$fantasy_finishes_weekly <- reactable::renderReactable({
  data <- rfl_fantasy_finishes_weekly %>%
    dplyr::select(player_id, player_name, pos, team, season, week, points, pos_rank, dplyr::ends_with("_weekly")) %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
      #season >= 2025 & season <= 2025
    )

  reactable::reactable(
    data,
    columns = list(
      player_id = reactable::colDef(show = FALSE),
      player_name = reactable::colDef(name = "Spieler", aggregate = "unique"),
      pos = reactable::colDef(name = "Pos", aggregate = "unique"),
      team = reactable::colDef(name = "Team", aggregate = "unique"),
      season = reactable::colDef(name = "Saison"),
      week = reactable::colDef(name = "WK"),
      points = reactable::colDef(name = "FPts", aggregate = "sum", format = reactable::colFormat(separators = TRUE, digits = 2)),
      pos_rank = reactable::colDef(name = "Pos #", format = reactable::colFormat(digits = 0)),
      top3_weekly = colDef(name = "Top 3", aggregate = "sum"),
      top5_weekly = colDef(name = "Top 5", aggregate = "sum"),
      top8_weekly = colDef(name = "Top 8", aggregate = "sum"),
      top12_weekly = colDef(name = "Top 12", aggregate = "sum"),
      top24_weekly = colDef(name = "Top 24", aggregate = "sum"),
      top36_weekly = colDef(name = "Top 36", aggregate = "sum"),
      top48_weekly = colDef(name = "Top 48", aggregate = "sum"),
      top60_weekly = colDef(name = "Top 60", aggregate = "sum")
    ),
    columnGroups = list(
      colGroup(name = "Elite", columns = c("top3_weekly", "top5_weekly")),
      colGroup(name = "Stud", columns = c("top8_weekly", "top12_weekly")),
      colGroup(name = "Starter", columns = c("top24_weekly", "top36_weekly")),
      colGroup(name = "Backup", columns = c("top48_weekly", "top60_weekly"))
    ),
    groupBy = c("player_id", "season"),
    defaultSorted = c("top5_weekly", "season", "week"),
    defaultSortOrder = "desc",
    filterable = TRUE
  )


  #rfl_fantasy_finishes_weekly_filtered() %>%
  #  #filter(pos == "QB") %>%
  #  gt_fantasy_finishes() %>%
  #  gt::tab_header(
  #    title = paste(
  #      "Wöchentliche Fantasy Finishes",
  #      if (input$selectYears[1] == input$selectYears[2]) {
  #        input$selectYears[1]
  #      } else {
  #        paste0(input$selectYears[1], "-", input$selectYears[2])
  #      }
  #    ),
  #    subtitle = "Nur RFL Regular Season"
  #  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

rfl_fantasy_finishes_season_filtered <- shiny::reactive({
  rfl_fantasy_finishes_season_filtered <- rfl_fantasy_finishes_season %>%
    #dplyr::filter(
      # TODO: filter einbauen
    #  if (isTruthy(input$onlyRflRegSeason))
    #    season == 2016 & week <= 13 |
    #    season > 2016 & week <= 12 |
    #    season > 2021 & week <= 14
    #  else
    #    TRUE
    #) %>%
    #dplyr::filter(
    #  season >= input$selectYears[1] & season <= input$selectYears[2]
      #season >= 2025 & season <= 2025
    #) %>%
    dplyr::select(-dplyr::ends_with("_weekly")) %>%
    dplyr::rename_with(~ gsub("_season", "", .x)) %>%

    summarize_fantasy_finishes()

    #dplyr::filter(
    #  season >= 2021 & season <= 2024
    #) %>%

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

output$fantasy_finishes_yearly <- reactable::renderReactable({
  data <- rfl_fantasy_finishes_season %>%
    dplyr::mutate(ppg = round(points / games, 2)) %>%
    dplyr::group_by(season, pos) %>%
    dplyr::arrange(dplyr::desc(ppg)) %>%
    dplyr::mutate(
      ppg_rank = dplyr::row_number()
    ) %>%
    #dplyr::filter(
    #  season >= input$selectYears[1] & season <= input$selectYears[2]
      #season >= 2025 & season <= 2025
    #) %>%
    dplyr::rename_with(~ gsub("_season", "", .x)) %>%
    dplyr::select(player_id, player_name, pos, team, season, games, ppg, ppg_rank, points, pos_rank, dplyr::starts_with("top"))

  reactable::reactable(
    data,
    columns = list(
      player_id = reactable::colDef(),
      player_name = reactable::colDef(name = "Spieler", aggregate = "unique"),
      pos = reactable::colDef(name = "Pos", aggregate = "unique"),
      team = reactable::colDef(name = "Team", aggregate = "unique"),
      season = reactable::colDef(name = "Saison"),
      points = reactable::colDef(name = "FPts", aggregate = "sum", format = colFormat(separators = TRUE, digits = 2)),
      top3 = colDef(name = "Top 3", aggregate = "sum"),
      top5 = colDef(name = "Top 5", aggregate = "sum"),
      top8 = colDef(name = "Top 8", aggregate = "sum"),
      top12 = colDef(name = "Top 12", aggregate = "sum"),
      top24 = colDef(name = "Top 24", aggregate = "sum"),
      top36 = colDef(name = "Top 36", aggregate = "sum"),
      top48 = colDef(name = "Top 48", aggregate = "sum"),
      top60 = colDef(name = "Top 60", aggregate = "sum")
    ),
    columnGroups = list(
      colGroup(name = "Per Game", columns = c("games", "ppg", "ppg_rank")),
      colGroup(name = "Elite", columns = c("top3", "top5")),
      colGroup(name = "Stud", columns = c("top8", "top12")),
      colGroup(name = "Starter", columns = c("top24", "top36")),
      colGroup(name = "Backup", columns = c("top48", "top60"))
    ),
    groupBy = c("player_id"),
    defaultSorted = c("top5", "season"),
    defaultSortOrder = "desc",
    filterable = TRUE
  )


  #rfl_fantasy_finishes_season_filtered() %>%
    #filter(pos == "QB") %>%
  #  gt_fantasy_finishes() %>%
  #  gt::tab_header(
  #    title = paste(
  #      "Fantasy Finishes",
  #      if (input$selectYears[1] == input$selectYears[2]) {
  #        input$selectYears[1]
  #      } else {
  #        paste0(input$selectYears[1], "-", input$selectYears[2])
  #      }
  #    ),
  #    subtitle = "Nur RFL Regular Season"
  #  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# TODO: schick machen
