# draft order----
draft_order <- rfl_weekly_standing %>%
  dplyr::select(week, franchise_id, franchise_name, pick, division)

if (current_week > 13) {
  postseason_teams <- draft_order %>%
    dplyr::filter(week == max(week) & pick > 24) %>%
    dplyr::select(-week)

  postseason_results <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/postseason_data/rfl_postseason_", new_season_sept, ".csv"), col_types = "icciccdcii") %>%
    dplyr::filter(bowl == "SB" & (week < 16 | week == 17) & (match_result == "L" | title == 1) | bracket == "Spiel um Platz drei") %>%
    dplyr::left_join(postseason_teams, by = "franchise_id") %>%
    dplyr::group_by(week) %>%
    dplyr::arrange(pick) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(week) %>%
    dplyr::mutate(
      pick = ifelse(week == 17, 37 - po_finish, row_number() + 24),
      week = 14
    ) %>%
    dplyr::arrange(pick) %>%
    dplyr::select(week, franchise_id, pick) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::select(week, franchise_id, franchise_name, pick)

  draft_order_postseason <- draft_order %>%
    dplyr::filter(week == 13) %>%
    dplyr::arrange(pick) %>%
    dplyr::filter(pick <= 24) %>%
    dplyr::mutate(week = 14) %>%
    dplyr::bind_rows(postseason_results)

  draft_order <- rbind(draft_order, draft_order_postseason) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, division),
      by = "franchise_id"
    )
}
