## player data ----
mfl_players <- jsonlite::read_json(paste0(mfl_api_base_march, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1")) %>%
  purrr::pluck("players", "player") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    player_name = name,
    pos = position,
    player_id = id
  ) %>%
  dplyr::filter(!grepl("TM", pos), !pos %in% c("Def", "ST", "Off", "Coach", "PN")) %>%
  dplyr::mutate(
    grouped_pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    ),
  ) %>%
  dplyr::left_join(
    feather::read_feather("data/rfl_player_elo.feather") %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::select(mfl_id, player_elo_post, season, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::filter(!is.na(player_elo_post)) %>%
  dplyr::left_join(
    nflreadr::load_players() %>%
      dplyr::select(gsis_id, headshot),
    by = "gsis_id"
  )

feather::write_feather(mfl_players, "data/mfl_players.feather")
