rfl_matchups <- jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=schedule&L=", league_id, "&W=", current_week, "&JSON=1"))$schedule$weeklySchedule$matchup %>%
    dplyr::tibble()

if(nrow(rfl_matchups > 0)) {
  rfl_matchups <- rfl_matchups %>%
    tidyr::unnest(1) %>%
    tidyr::unnest_wider(1, names_sep = "_") %>%
    tidyr::unnest_wider("._1") %>%
    dplyr::rename(home = id) %>%
    tidyr::unnest_wider("._2", names_sep = "_") %>%
    dplyr::rename(away = "._2_id") %>%
    dplyr::select(home, away) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name) %>%
        dplyr::rename(home_name = franchise_name),
      by = c("home" = "franchise_id")
    ) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name) %>%
        dplyr::rename(away_name = franchise_name),
      by = c("away" = "franchise_id")
    ) %>%
    dplyr::mutate(matchup = paste(home_name, "vs", away_name))

  starter_data <- jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=weeklyResults&L=", league_id, "&W=", current_week, "&JSON=1"))$weeklyResults$matchup %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(franchise, names_sep = "_") %>%
    dplyr::select(-regularSeason) %>%
    tidyr::unnest_wider(franchise_1, names_sep = "_") %>%
    tidyr::unnest_wider(franchise_2, names_sep = "_") %>%
    dplyr::select(dplyr::ends_with("id"), dplyr::ends_with("_starters"))

  projected_points <- jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=projectedScores&L=", league_id, "&W=", current_week, "&JSON=1"))$projectedScores$playerScore %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::rename(projected = score)

  live_data <- jsonlite::read_json(paste0(mfl_api_base_sept, "/export?TYPE=liveScoring&L=", league_id, "&W=", current_week, "&JSON=1"))$liveScoring$matchup %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    tidyr::unnest_wider(franchise, names_sep = "_") %>%
    tidyr::unnest_wider(franchise_1, names_sep = "_") %>%
    tidyr::unnest_wider(franchise_2, names_sep = "_") %>%
    dplyr::select(-dplyr::ends_with("isHome"), -dplyr::ends_with("score"))

  live_points_data <- c(live_data$franchise_1_players, live_data$franchise_2_players) %>%
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

  starter <- data.frame(
    franchise_id = c(starter_data$franchise_1_id, starter_data$franchise_2_id),
    id = c(starter_data$franchise_1_starters, starter_data$franchise_2_starters)
  ) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows(id, sep = ",") %>%
    dplyr::filter(id != "") %>%
    dplyr::left_join(
      projected_points,
      by = "id"
    ) %>%
    dplyr::left_join(
      live_points_data,
      by = "id"
    )

  # ToDo: remaining seconds etc hinzufügen

  matchup_projection_table_data <-reactive({
    data.frame(
      franchise_id = c(rfl_matchups$home, rfl_matchups$away),
      franchise_name = c(rfl_matchups$home_name, rfl_matchups$away_name),
      matchup = rfl_matchups$matchup
    ) %>%
      dplyr::left_join(
        starter,
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
      dplyr::select(franchise_name, player_name, subline, projected, time_remaining, live, live_1, projected_1, diff) %>%
      dplyr::distinct()
  })
}

shiny::observe({
  req(rfl_matchups)
  req(nrow(rfl_matchups > 0)) # Prüfen, ob Daten vorhanden sind

  shinyWidgets::updatePickerInput(
    session,
    "selectRflMatchup",
    choices = rfl_matchups$matchup,
    selected = rfl_matchups$matchup[1],
  )
})

matchup_projections_home <- reactive({
  rfl_matchups %>%
    dplyr::filter(matchup == input$selectRflMatchup) %>%
    dplyr::pull(home_name)
})

matchup_projections_away <- reactive({
  rfl_matchups %>%
    dplyr::filter(matchup == input$selectRflMatchup) %>%
    dplyr::pull(away_name)
})

gtMatchupProjections <- function(df) {
  df %>%
    gt::gt() %>%

    gt::tab_header(
      title = home_team(),
      subtitle = paste("Week", current_week, season_before_wk_1)
    ) %>%

    gt::cols_hide(franchise_name) %>%

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
      columns = c(projected, diff),
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

    gt::data_color(
      columns = diff,
      method = "numeric",
      palette = "viridis",
    ) %>%

    gt::cols_label(
      player_name = "Player",
      projected = "Proj",
      live_1 = "Points",
      diff = "+/-"
    ) %>%

    gtDefaults()
}

output$matchup_projections_home_table <- gt::render_gt({
  matchup_projection_table_data() %>%
    dplyr::filter(franchise_name == matchup_projections_home()) %>%
    gtMatchupProjections()

})

output$matchup_projections_away_table <- gt::render_gt({
  matchup_projection_table_data() %>%
    dplyr::filter(franchise_name == matchup_projections_away()) %>%
    gtMatchupProjections()
})
