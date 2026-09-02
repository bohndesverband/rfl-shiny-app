roster_data <- starter %>%
  dplyr::distinct() %>%
  filter(franchise_id == "0007") %>%
  dplyr::filter(starter_status == "starter") %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DE", "DT") ~ "DL",
      pos %in% c("S", "CB") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::left_join(
    roster %>%
      dplyr::select(franchise_id, player_id, status) %>%
      dplyr::mutate(player_id = as.double(player_id)),
    by = c("franchise_id", "player_id")
  ) %>%
  dplyr::mutate(
    player = nflreadr::clean_player_names(player_name),
    player = ifelse(!is.na(status), paste0("<strong>", player, "</strong>"), player),
  )

roster_over_time <- roster_data %>%
  dplyr::group_by(season, franchise_id) %>%
  dplyr::filter(week == min(week) | week == max(week)) %>%
  dplyr::group_by(season, week, franchise_id, pos) %>%
  dplyr::arrange(week, dplyr::desc(player_score)) %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("RB", "WR") & dplyr::row_number() > 2 ~ "FLX",
      pos == "TE" & dplyr::row_number() > 1 ~ "FLX",
      pos %in% c("LB", "DB") & dplyr::row_number() > 2 ~ "IDP",
      TRUE ~ pos
    ),
    week = ifelse(week == 1, "start", "end")
  ) %>%
  dplyr::arrange(factor(pos, var.posOrder)) %>%
  dplyr::mutate(
    join_pos = paste(pos, dplyr::row_number())
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(franchise_id, season, week, player, pos, join_pos)

roster_over_time %>%
  dplyr::filter(week == "start") %>%
  dplyr::select(-week) %>%
  dplyr::rename(start = player) %>%
  dplyr::left_join(
    roster_over_time %>%
      dplyr::filter(week == "end") %>%
      dplyr::select(-week, -pos) %>%
      dplyr::rename(end = player),
    by = c("franchise_id", "season", "join_pos"),
    multiple = "first"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-franchise_id, -join_pos) %>%
  dplyr::arrange(dplyr::desc(season)) %>%
  gt::gt(groupname_col = "season") %>%
  gt::fmt_markdown(c(start, end))

#starts_by_player <-
roster_data %>%
  dplyr::distinct() %>%
  filter(franchise_id == "0007") %>%
  dplyr::filter(starter_status == "starter") %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(
    player = dplyr::first(player),
    starts = n(),
    seasons = paste(min(season), max(season), sep = "-")
  ) %>%
  dplyr::arrange(dplyr::desc(starts)) %>%
  dplyr::select(-player_id) %>%
  gt::gt() %>%
  gt::fmt_markdown(player)
