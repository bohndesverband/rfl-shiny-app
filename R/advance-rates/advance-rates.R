advanced_rosters <- player_ranks_avg %>% 
  dplyr::left_join(
    roster %>% 
      dplyr::select(-pos),
    by = c("id" = "player_id")
  ) %>% 
  dplyr::left_join(
    franchises %>% 
      dplyr::select(franchise_name, franchise_id),
    by = "franchise_id"
  ) %>% 
  dplyr::left_join(
    current_standing %>% 
      dplyr::select(franchise_name, seed),
    by = "franchise_name"
  ) %>% 
  dplyr::mutate(
    advanced = ifelse(seed <= 6, 1, 0)
  ) %>% 
  dplyr::group_by(id, player_name) %>% 
  dplyr::mutate(
    count = sum(advanced),
    advance_rate = count / 3
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    score = round(as.numeric(score), 2),
    count = ifelse(is.na(count), 0, count),
    advance_rate = ifelse(is.na(advance_rate), 0, round(advance_rate, 2))
  ) %>% 
  dplyr::select(player_name, score, pos, rank, advance_rate) %>% 
  dplyr::distinct()
