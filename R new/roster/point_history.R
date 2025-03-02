rfl_point_history_raw <- starter_data %>%
  dplyr::mutate(
    pos = case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    ),
    unit = ifelse(pos %in% c("QB", "RB", "WR", "TE", "PK"), "offense", "defense")
  )

rfl_point_history_starter <- rfl_point_history_raw %>%
  dplyr::filter(starter_status == "starter") %>%

  # position values
  dplyr::group_by(season, franchise_id, pos) %>%
  dplyr::summarise(
    pos_total = round(sum(player_score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::arrange(dplyr::desc(pos_total)) %>%
  dplyr::mutate(
    rank = row_number()
  ) %>%
  dplyr::select(season, franchise_id, pos, rank) %>%
  tidyr::spread(season, rank) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pos = factor(pos, levels = position_order)) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name, division),
    by = "franchise_id"
  ) %>%
  dplyr::group_by(franchise_name) %>%
  dplyr::arrange(franchise_name, pos)

output$roster_point_history <- gt::render_gt({
  rfl_point_history_starter %>%
    dplyr::filter(
      is.null(input$selectRflTeams) | franchise_id %in% c(input$selectRflTeams)
    ) %>%
    dplyr::filter(
      is.null(input$selectRflDivisions) | division %in% c(input$selectRflDivisions)
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Positions Ranking nach Total Points"),
      subtitle = paste("Abschneiden der Teams nach erzielten Total Points je Positionsgruppen")
    ) %>%
    gt::cols_hide(c(franchise_id, division)) %>%
    gtDefaults() %>%
    gt::data_color(
      where(is.numeric),
      palette = c(color_blue, color_red),
      domain = c(1, 36)
    ) %>%
    gt::data_color(
      pos,
      palette = colors_position
    ) %>%
    gt::cols_label(
      pos = "Pos"
    ) %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    )
})
# ToDo: filter nach Positionen


