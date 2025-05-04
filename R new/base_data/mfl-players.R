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
    )
  )

mfl_players_with_elo <- mfl_players %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::select(mfl_id, player_elo_post, season),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::filter(!is.na(player_elo_post)) %>%
  dplyr::left_join(
    rfl_current_roster %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(franchise_ids = paste(franchise_id, collapse = ", "), .groups = "drop"),
    by = "player_id"
  )
