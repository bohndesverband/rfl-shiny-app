running_player_elo <- player_elo %>%
  dplyr::mutate(
    week = stringr::str_pad(week, 2, pad = "0")
  ) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::mutate(game = paste0(season, week)) %>%
  dplyr::arrange(game) %>%
  dplyr::select(game, mfl_id, display_name, position, player_elo_post) %>%
  dplyr::distinct() %>%
  dplyr::mutate(game = dplyr::row_number()) %>%
  dplyr::ungroup()
