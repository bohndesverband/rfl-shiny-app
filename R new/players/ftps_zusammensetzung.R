fpts_zusammensetzung <- readRDS("data/fpts_zusammensetzung.rds")

output$bigPlayPoints <- gt::render_gt({
  fpts_zusammensetzung %>%
    dplyr::arrange(desc(points)) %>%
    #slice_head(n = 10) %>%
    dplyr::select(pos_rank, player_id, pos:points, fpts_breakdown, subline, fpts_negative) %>%
    dplyr::filter(player_id %in% input$selectPlayers) %>%
    dplyr::arrange(dplyr::desc(points)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Zusammensetzung der Fantasy Punkte",
      subtitle = "Die Tabelle zeigt, wie sich die FPts der Spieler prozentual zusammensetzen. Die Werte sind nicht 100%ig genau und gelten für die Offense/Defense"
    ) %>%
    gt::cols_hide(c(pos, team, player_id)) %>%
    gtExtras::gt_merge_stack(
      player_name,
      subline,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%
    gtExtras::gt_plt_bar_stack(
      fpts_breakdown,
      width = 100,
      labels = c("TDs/Sacks+TO", "Yds/Tckls", "Rec/PB"),
      palette = c(color_red, color_blue, color_orange)
    ) %>%
    gt::data_color(
      points,
      palette = c(color_red, color_yellow, color_green, color_blue),
    ) %>%
    #gt::data_color(
    #  fpts_negative,
    #  palette = c(color_red, color_blue),
    #) %>%
    gt::cols_label(
      player_name = "Spieler",
      points = "FPts",
      pos_rank = "Pos Rank",
      fpts_negative = "Neg"
      #fpts_breakdown = "Breakdown"
    ) %>%
    gtDefaults() %>%
    gt::tab_footnote(
      footnote = "Negative Punkte durch Interceptions, Fumbles, etc.",
      locations = gt::cells_column_labels(
        columns = fpts_negative
      ),
      placement = "left"
    )
})

selection_data <- fpts_zusammensetzung %>%
  dplyr::select(player_id, player_name, pos, pos_rank) %>%
  dplyr::left_join(
    rfl_roster_data %>%
      dplyr::filter(season == max(season)) %>%
      dplyr::filter(week == max(week)),
    by = "player_id"
  )

selection_r <- shiny::reactive({
  selection_data %>%
    dplyr::filter(
      if (isTruthy(input$selectRflTeam))
        franchise_id == input$selectRflTeam
      else TRUE
    ) %>%
    dplyr::filter(
      if (isTruthy(input$selectPositions))
        pos %in% input$selectPositions
      else TRUE
    )
})

# Wenn Team oder Position geändert werden -> Auswahl aktualisieren
shiny::observeEvent(list(input$selectRflTeam, input$selectPositions, input$selectPositionRank), {
  req(active_tab() == "#section-fpts-zusammensetzung")

  if(input$selectRflTeam == "") {
    selected_positions <- fpts_zusammensetzung %>%
      dplyr::filter(
        if (isTruthy(input$selectPositions))
          pos %in% input$selectPositions
        else TRUE
      )

    selected_ids <- selection_r() %>%
      dplyr::select(-franchise_id) %>%
      dplyr::distinct() %>%
      dplyr::filter(pos_rank <= input$selectPositionRank) %>%
      dplyr::pull(player_id)
  } else {
    selected_positions <- fpts_zusammensetzung

    selected_ids <- selection_r() %>%
      dplyr::pull(player_id)
  }

  shinyWidgets::updatePickerInput(
    session,
    "selectPlayers",
    choices = split(
      setNames(selected_positions$player_id,
               selected_positions$player_name),
      selected_positions$pos
    ),
    selected = selected_ids,
    options = list("max-options" = 50),
  )
})
