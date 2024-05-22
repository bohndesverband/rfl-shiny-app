## WAR berechnung nach filter ----
roster_data <- roster %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id",
      multiple = "all"
    ) %>%
    dplyr::left_join(
      war %>%
        dplyr::filter(season >= var.season - 3) %>%
        dplyr::group_by(player_id) %>%
        dplyr::mutate(
          war_last_three_seasons = round(mean(war, na.rm = TRUE), 2),
          player_id = as.character(player_id)
        ) %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::select(player_id, points, war, war_last_three_seasons) %>%
        dplyr::select(-status, -player_id, -week),
      by = "player_id",
      multiple ="all"
    )
