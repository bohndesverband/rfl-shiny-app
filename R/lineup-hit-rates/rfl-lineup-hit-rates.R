lineup_hit_rates <- starter %>%
  dplyr::mutate(
    hit = dplyr::case_when(
      starter_status == "starter" & should_start == 1 ~ 1,
      starter_status == "nonstarter" & should_start == 0 ~ 1,
      TRUE ~ 0
    ),
    started = ifelse(starter_status == "starter", 1, 0)
  ) %>%
  dplyr::group_by(franchise_id, player_id, player_name) %>%
  dplyr::distinct() %>%
  dplyr::arrange(dplyr::desc(season), dplyr::desc(week)) %>% # important for displaying winloss correctly (the last 10 games)
  dplyr::summarise(
    weeks = n(),
    hits = sum(hit),
    starts = sum(started),
    started = list(started),
    hit = list(hit),
    team =  dplyr::first(team),
    pos = dplyr::first(pos),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    hit_rate = hits / weeks,
    subline = paste(pos, team, sep = ", ")
  ) %>%
  dplyr::arrange(dplyr::desc(starts), dplyr::desc(hit_rate)) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::select(-weeks, -hits, -team, -pos, -franchise_id)
