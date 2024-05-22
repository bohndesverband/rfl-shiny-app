power_ranking <- true_standing %>%
  dplyr::group_by(franchise_id, franchise_name) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    running_pf = cumsum(pf),
    running_pp = cumsum(pp),
    running_coach = cumsum(coach),
    running_wins = cumsum(win),
    running_all_play_wins = cumsum(all_play_wins)
  ) %>%
  dplyr::group_by(week) %>%
  dplyr::mutate(
    pf_rank = dplyr::dense_rank(dplyr::desc(running_pf)),
    pp_rank = dplyr::dense_rank(dplyr::desc(running_pp)),
    record_rank = dplyr::dense_rank(dplyr::desc(running_wins)),
    all_play_rank = dplyr::dense_rank(dplyr::desc(running_all_play_wins)),
    coach_rank = dplyr::dense_rank(dplyr::desc(running_coach)),
    true_standing = pf_rank + pp_rank + record_rank + all_play_rank + coach_rank
  ) %>%
  dplyr::arrange(true_standing, running_wins, dplyr::desc(running_pf)) %>%
  dplyr::mutate(true_rank = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(week, franchise_id, franchise_name, true_rank, division_name)
