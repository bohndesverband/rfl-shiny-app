rfl_drafts_data <- purrr::map_df(2016:2024, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft_{x}.csv"),
    col_types = "iTiciccccccci"
  )
}) %>%
  dplyr::mutate(
    pos_grouped = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::select(-franchise_name) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::group_by(season, mfl_id) %>%
  dplyr::arrange(overall) %>%
  dplyr::mutate(
    first_pick = overall[1],
    second_pick = overall[2],
    third_pick = overall[3],
    min_pick = min(overall),
    max_pick = max(overall),
    avg_pick = round(mean(overall)),
    value = avg_pick - overall,
  ) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::arrange(first_pick) %>%
  # number groups
  dplyr::mutate(pos_rank = dplyr::dense_rank(first_pick)) %>%
  dplyr::ungroup()

rfl_drafts_rookies <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & is_rookie) # nur rookie drafts & rookies

rfl_drafts_with_elo <- rfl_drafts_data %>%
  dplyr::mutate(round = as.numeric(round)) %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::mutate(elo_peak = max(player_elo_post)) %>%
      dplyr::arrange(season, week) %>%
      dplyr::filter(row_number() == max(row_number())) %>%
      dplyr::select(mfl_id, player_elo_post, elo_peak) %>%
      dplyr::mutate(elo_shift = player_elo_post - 1500) %>%
      dplyr::rename(current_player_elo = player_elo_post),
    by = "mfl_id"
  ) %>%
  dplyr::left_join(
    rfl_fantasy_finishes_summarised %>%
      dplyr::select(player_id, dplyr::starts_with("top")),
    by = c("mfl_id" = "player_id")
  )
