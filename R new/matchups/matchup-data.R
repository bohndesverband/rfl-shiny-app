rfl_matchups_raw <- shiny::reactive({
  rfl_matchups_raw <-
    ##jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=schedule&L=", league_id, "&W=", current_week - 1, "&JSON=1"))$schedule$weeklySchedule$matchup %>%
    jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=schedule&L=", league_id, "&W=", input$selectWeek, "&JSON=1"))$schedule$weeklySchedule$matchup %>%
    dplyr::tibble()
})

rfl_matchups <- shiny::reactive({
  req(rfl_matchups_raw())
  req(nrow(rfl_matchups_raw()) > 0)

  rfl_matchups <- rfl_matchups_raw() %>%
    tidyr::unnest(1) %>%
    tidyr::unnest_wider(1, names_sep = "_") %>%
    tidyr::unnest_wider("._1") %>%
    dplyr::rename(home = id) %>%
    tidyr::unnest_wider("._2", names_sep = "_") %>%
    dplyr::rename(away = "._2_id") %>%
    dplyr::select(home, away) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name) %>%
        dplyr::rename(home_name = franchise_name),
      by = c("home" = "franchise_id")
    ) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name) %>%
        dplyr::rename(away_name = franchise_name),
      by = c("away" = "franchise_id")
    ) %>%
    dplyr::mutate(matchup = paste(home_name, "vs", away_name))
})

starter_data <- shiny::reactive({
  req(input$selectWeek)
  starter_data <-
    #jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=weeklyResults&L=", league_id, "&W=", current_week - 1, "&JSON=1"))$weeklyResults$matchup %>%
    jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=weeklyResults&L=", league_id, "&W=", input$selectWeek, "&JSON=1"))$weeklyResults$matchup %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(franchise, names_sep = "_") %>%
    dplyr::select(-regularSeason) %>%
    tidyr::unnest_wider(franchise_1, names_sep = "_") %>%
    tidyr::unnest_wider(franchise_2, names_sep = "_") %>%
    dplyr::select(dplyr::ends_with("id"), dplyr::ends_with("_starters"))
})

projected_points <- shiny::reactive({
  req(input$selectWeek)
  projected_points <-
    #jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=projectedScores&L=", league_id, "&W=", current_week - 1, "&JSON=1"))$projectedScores$playerScore %>%
    jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=projectedScores&L=", league_id, "&W=", input$selectWeek, "&JSON=1"))$projectedScores$playerScore %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::rename(projected = score)
})

live_data <- shiny::reactive({
  req(input$selectWeek)
  live_data <-
    #jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=liveScoring&L=", league_id, "&W=", current_week - 1, "&JSON=1"))$liveScoring$matchup %>%
    jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=liveScoring&L=", league_id, "&W=", input$selectWeek, "&JSON=1"))$liveScoring$matchup %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(franchise, names_sep = "_") %>%
    tidyr::unnest_wider(franchise_1, names_sep = "_") %>%
    tidyr::unnest_wider(franchise_2, names_sep = "_") %>%
    dplyr::select(-dplyr::ends_with("isHome"), -dplyr::ends_with("score"))
})

live_points_data <- shiny::reactive({
  req(live_data())
  live_points_data <- c(live_data()$franchise_1_players, live_data()$franchise_2_players) %>%
    dplyr::tibble() %>%
    tidyr::hoist(1, "players" = "player") %>%
    tidyr::unnest(players) %>%
    tidyr::unnest_wider(players) %>%
    dplyr::distinct() %>%
    dplyr::rename(
      live = score,
      time_remaining = gameSecondsRemaining
    ) %>%
    dplyr::select(id, live, time_remaining)
})

starter <- shiny::reactive({
  req(starter_data())
  req(projected_points())
  req(live_points_data())

  starter <- data.frame(
    franchise_id = c(starter_data()$franchise_1_id, starter_data()$franchise_2_id),
    id = c(starter_data()$franchise_1_starters, starter_data()$franchise_2_starters)
  ) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows(id, sep = ",") %>%
    dplyr::filter(id != "") %>%
    dplyr::left_join(
      projected_points(),
      by = "id"
    ) %>%
    dplyr::left_join(
      live_points_data(),
      by = "id"
    )
})

matchup_projection_table_data <- shiny::reactive({
  req(rfl_matchups())
  req(starter())

  matchup_projection_table_data <- data.frame(
    franchise_id = c(rfl_matchups()$home, rfl_matchups()$away),
    franchise_name = c(rfl_matchups()$home_name, rfl_matchups()$away_name),
    matchup = rfl_matchups()$matchup
  ) %>%
    dplyr::left_join(
      starter(),
      by = "franchise_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      mfl_players %>%
        dplyr::select(-status),
      by = c("id" = "player_id")
    ) %>%
    dplyr::mutate(
      live = as.numeric(live),
      projected = ifelse(is.na(projected), 0, as.numeric(projected)),
      time_remaining = as.numeric(time_remaining),
      pts = ifelse(time_remaining < 3600, live, projected),
      diff = pts - projected,
      subline = paste(pos, team, sep = ", "),
      #pts_per_second = live / (3600 - time_remaining),
      #calc = live + (pts_per_second * time_remaining * 0.75)
    ) %>%
    dplyr::mutate(
      live_1 = live,
      projected_1 = projected
    ) %>%
    dplyr::left_join(
      rfl_player_scores %>%
        dplyr::group_by(player_id) %>%
        dplyr::summarise(
          ppg = round(mean(points, na.rm = TRUE), 2),
          sd = round(sd(points, na.rm = TRUE), 1),
          .groups = "drop"
        ),
      by = c("id" = "player_id")
    ) %>%
    dplyr::select(franchise_name, player_name, subline, projected, time_remaining, live, live_1, projected_1, diff, ppg, sd) %>%
    dplyr::distinct()
})
