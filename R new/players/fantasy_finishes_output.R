output$fantasy_finishes <- gt::render_gt({
  rfl_fantasy_finishes_summarised %>%
    dplyr::filter(
      last_season >= new_season_march - 1
    ) %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        sapply(seq_along(pos), function(i) {
          any(input$selectPositions %in% trimws(strsplit(pos[i], ",")[[1]]))
        })
      else
        TRUE
    ) %>%
    #filter(pos == "QB") %>%
    dplyr::arrange(dplyr::desc(top5)) %>%
    dplyr::select(-seasons, -last_season) %>%
    dplyr::rename(display_name = player_name, position = pos) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Fantasy Finishes seit 2016"),
    ) %>%
    gt::cols_hide(c(player_id)) %>%
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

    gt_player()
})
