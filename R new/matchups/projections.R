source("R new/matchups/matchup-data.R", local = TRUE)

shiny::observe({
  req(rfl_matchups())
  req(nrow(rfl_matchups()) > 0) # Prüfen, ob Daten vorhanden sind

  shinyWidgets::updatePickerInput(
    session,
    "selectRflMatchup",
    choices = rfl_matchups()$matchup,
    selected = rfl_matchups()$matchup[1],
    options = list("max-options" = 1),
  )
})

matchup_projections_home <- reactive({
  req(rfl_matchups())
  rfl_matchups() %>%
    dplyr::filter(matchup == input$selectRflMatchup) %>%
    dplyr::pull(home_name)
})

matchup_projections_away <- reactive({
  req(rfl_matchups())
  rfl_matchups() %>%
    dplyr::filter(matchup == input$selectRflMatchup) %>%
    dplyr::pull(away_name)
})

gtMatchupProjections <- function(df, name) {
  df %>%
    gt::gt() %>%

    gt::tab_header(
      title = name,
      subtitle = paste("Week", input$selectWeek, season_before_wk_1)
    ) %>%

    gt::cols_hide(c(franchise_name)) %>%
    gt::cols_move(ppg, player_name) %>%

    gtExtras::gt_merge_stack(
      player_name,
      subline,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      projected,
      time_remaining,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_plt_bullet(
      column = live_1,
      target = projected_1,
      width = 30,
      palette = c(color_green, color_grey_mid)
    ) %>%

    gt::grand_summary_rows(
      columns = c(projected, diff, live_1, ppg),
      fns = list(
        sum ~ sum(.)
      ),
    ) %>%

    gtExtras::gt_merge_stack(
      live_1,
      live,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      ppg,
      sd,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%

    gt::data_color(
      columns = diff,
      method = "numeric",
      palette = "viridis",
    ) %>%

    gt::data_color(
      columns = c(projected, ppg),
      method = "numeric",
      palette = c(color_bg, color_grey_light),
    ) %>%

    gt::cols_label(
      player_name = "Player",
      projected = "Proj",
      live_1 = "Points",
      diff = "+/-",
      ppg = "FPts/G",
    ) %>%

    gt::tab_footnote(
      footnote = "FPts/G und in klein Standardabweichung (um wie viele Punkte weichen die PPG im Schnitt ab)",
      locations = gt::cells_column_labels(
        columns = ppg
      ),
      placement = "left"
    ) %>%

    gt::tab_footnote(
      footnote = "fantasysharks.com Projections und in klein die verbleibende Spielzei tin Sekunden",
      locations = gt::cells_column_labels(
        columns = projected
      ),
      placement = "left"
    ) %>%

    gtDefaults()
}

output$matchup_projections_home_table <- gt::render_gt({
  req(matchup_projection_table_data())
  matchup_projection_table_data() %>%
    dplyr::filter(franchise_name == matchup_projections_home()) %>%
    gtMatchupProjections(name = matchup_projections_home())
})

output$matchup_projections_away_table <- gt::render_gt({
  req(matchup_projection_table_data())
  matchup_projection_table_data() %>%
    dplyr::filter(franchise_name == matchup_projections_away()) %>%
    gtMatchupProjections(name = matchup_projections_away())
})
