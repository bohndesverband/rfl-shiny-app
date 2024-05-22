## WAR berechnung nach filter ----
player_war <- reactive({
  war %>%
    dplyr::filter(
      season >= input$selectSeason[1] & season <= input$selectSeason[2]
    ) %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      points = sum(points),
      war = sum(war),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      players %>%
        dplyr::mutate(player_id = as.numeric(player_id)),
      by = "player_id"
    ) %>%
    dplyr::filter(!is.na(player_name)) %>%
    dplyr::mutate(
      war = round(war, 2),
      "WAR %" = scales::percent(war / max(war))
    ) %>%
    dplyr::group_by(pos) %>%
    dplyr::mutate("WAR % Pos" = scales::percent(war / max(war))) %>%
    dplyr::select(player_name, pos, points, war, "WAR %", "WAR % Pos")
})
