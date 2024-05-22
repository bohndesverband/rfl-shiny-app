rfl_drafts <- reactive({
  rfl_drafts <- rfl_drafts_data %>%
    dplyr::filter(
      !is.na(position),
      overall >= input$selectPicks[1] & overall <= input$selectPicks[2]
    ) %>%
    dplyr::group_by(season, mfl_id) %>%
    dplyr::arrange(overall) %>%
    dplyr::mutate(
      first_pick = overall[1],
      second_pick = overall[2],
      third_pick = overall[3]
    ) %>%
    dplyr::group_by(mfl_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::select(season, overall, round, pick, mfl_id, position, first_pick, second_pick, third_pick) %>%
    dplyr::group_by(season, position) %>%
    dplyr::arrange(first_pick) %>%
    dplyr::mutate(pos_rank = dplyr::row_number()) %>%
    dplyr::ungroup()
})

nfl_draft_rounds <- reactive({
  nfl_draft_rounds <- rfl_drafts_data %>%
    dplyr::filter(
      !is.na(gsis_id),
      overall >= input$selectPicks[1] & overall <= input$selectPicks[2]
    ) %>%
    dplyr::left_join(
      nflreadr::load_draft_picks() %>%
        dplyr::filter(!is.na(gsis_id), season >= 2016) %>%
        dplyr::select(gsis_id, season, round) %>%
        dplyr::rename(round_nfl = round),
      by = c("gsis_id", "season"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      round_nfl = ifelse(is.na(gsis_id), 8, round_nfl)
    ) %>%
    dplyr::group_by(position, round, round_nfl) %>%
    dplyr::summarise(count = n(), .groups = "drop")
})
