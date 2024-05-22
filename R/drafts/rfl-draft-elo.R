rfl_draft_elo <- reactive({
  rfl_drafts_data %>%
    dplyr::left_join(
      player_elo %>%
        dplyr::group_by(mfl_id) %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::filter(week == max(week)) %>%
        dplyr::ungroup() %>%
        dplyr::select(mfl_id, player_elo_post),
      by = "mfl_id"
    ) %>%
    dplyr::left_join(
      player_elo %>%
        dplyr::group_by(mfl_id) %>%
        dplyr::filter(player_elo_post == max(player_elo_post)) %>%
        dplyr::ungroup() %>%
        dplyr::select(mfl_id, player_elo_post) %>%
        dplyr::rename(elo_peak = player_elo_post) %>%
        dplyr::distinct(),
      by = "mfl_id",
    ) %>%
    dplyr::left_join(
      nflreadr::load_draft_picks() %>%
        dplyr::select(gsis_id, season, round, pick) %>%
        dplyr::rename(
          nfl_dy = season,
          nfl_dr = round,
          nfl_dp = pick
        ),
      by = "gsis_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(round = as.numeric(round)) %>%
    dplyr::filter(
      !is.na(gsis_id),
      season >= input$draftEloSelectSeason[1] & season <= input$draftEloSelectSeason[2] &
        round >= input$draftEloSelectRounds[1] & round <= input$draftEloSelectRounds[2]
    ) %>%
    dplyr::filter(
      if(isTruthy(input$draftEloSelectPosition))
        position %in% input$draftEloSelectPosition
      else
        TRUE
    ) %>%
    dplyr::filter(
      if(isTruthy(input$draftEloSelectTeams))
        franchise %in% input$draftEloSelectTeams
      else
        TRUE
    ) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name),
      by = c("franchise" = "franchise_id")
    ) %>%
    dplyr::mutate_at(c("player_elo_post", "elo_peak"), .funs = ~ tidyr::replace_na(as.numeric(.x), 1200)) %>%
    dplyr::select(franchise_name, season, round, pick, overall, player_name, position, team, player_elo_post, elo_peak, dplyr::starts_with("nfl_")) %>%
    dplyr::rename(
      "RFL Team" = franchise_name,
      DY = season,
      DR = round,
      DP = pick,
      OVRL = overall,
      Spieler = player_name,
      Pos = position,
      Team = team,
      ELO = player_elo_post,
      "ELO Peak" = elo_peak,
      "NFL DY" = nfl_dy,
      "NFL DR" = nfl_dr,
      "NFL OVRL" = nfl_dp
    )
})
