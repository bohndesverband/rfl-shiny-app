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
      dplyr::filter(season == season_before_wk_1 & week == max(week)) %>%
      dplyr::select(mfl_id, player_elo_post, season),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::left_join(
    roster_data %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(franchise_ids = paste(franchise_id, collapse = ", "), .groups = "drop"),
    by = "player_id"
  )
