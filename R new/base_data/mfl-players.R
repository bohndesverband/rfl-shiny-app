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
