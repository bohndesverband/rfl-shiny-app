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

output$fantasy_finishes_weekly <- gt::render_gt({
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
    ) %>%
    #filter(pos == "QB") %>%
    gt_fantasy_finishes() %>%
    gt::tab_header(
      title = paste("Wöchentliche Fantasy Finishes", paste0(input$selectYears[1], "-", input$selectYears[2])),
      subtitle = "Nur RFL Regular Season"
    )
})

output$fantasy_finishes_yearly <- gt::render_gt({
  rfl_fantasy_finishes_season %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
      #season >= 2025 & season <= 2025
    ) %>%
    dplyr::select(-dplyr::ends_with("_weekly")) %>%
    dplyr::rename_with(~ gsub("_season", "", .x)) %>%

    summarize_fantasy_finishes() %>%

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
    ) %>%
    #filter(pos == "QB") %>%
    gt_fantasy_finishes() %>%
    gt::tab_header(
      title = paste("Fantasy Finishes", paste0(input$selectYears[1], "-", input$selectYears[2])),
      subtitle = "Nur RFL Regular Season"
    )
})
