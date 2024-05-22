true_standing <- readr::read_csv(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/true-standing/rfl-true-standing-", var.season, ".csv"), col_types = "ncinnnnnnnnn") %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name, division_name),
    by = "franchise_id"
  ) %>%
  dplyr::arrange(week)

current_standing <- elo %>%
  dplyr::filter(season == max(season)) %>%
  dplyr::mutate(
    winloss = ifelse(score_diff > 0, 1, 0)
  ) %>%
  dplyr::group_by(franchise_id, week) %>%
  dplyr::mutate(elo_shift = sum(elo_shift)) %>%
  dplyr::group_by(franchise_id, franchise_name, division_name) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    season = dplyr::first(season),
    week = dplyr::last(week),
    winloss = list(winloss),
    pf_sparkline = list(unique(franchise_score)),
    franchise_elo_postgame = dplyr::last(franchise_elo_postgame),
    elo_shift = dplyr::last(elo_shift),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    true_standing %>%
      dplyr::group_by(franchise_id) %>%
      dplyr::arrange(week) %>%
      dplyr::summarise(
        pp_dist = list(unique(pp / 2) / week),
        dplyr::across(win:all_play_wins, \(x) sum(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        pf_rank = dplyr::dense_rank(dplyr::desc(pf)),
        pp_rank = dplyr::dense_rank(dplyr::desc(pp)),
        record_rank = dplyr::dense_rank(dplyr::desc(win)),
        all_play_rank = dplyr::dense_rank(dplyr::desc(all_play_wins)),
        coach_rank = dplyr::dense_rank(dplyr::desc(coach)),
        true_standing = pf_rank + pp_rank + record_rank + all_play_rank + coach_rank
      ) %>%
      dplyr::arrange(true_standing, win, pf_rank) %>%
      dplyr::mutate(true_rank = dplyr::row_number()),
    by = "franchise_id"
  ) %>%
  dplyr::arrange(desc(win), desc(pf)) %>%
  dplyr::mutate(
    place = row_number(),
    loss = (2 * week) - win,
    pf = pf,
    pp = pp,
    #dplyr::across(pf_rank:coach_rank, ~ 37 - .x),
    elo_shift_norm = elo_shift - min(elo_shift)
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, conference_name),
    by = "franchise_id"
  ) %>%
  dplyr::mutate(
    subline = paste(division_name, conference_name, sep = ", ")
  ) %>%
  dplyr::group_by(division_name) %>%
  dplyr::arrange(desc(win), desc(pf)) %>%
  dplyr::mutate(rank_div = row_number()) %>%
  dplyr::group_by(conference_name, rank_div) %>%
  dplyr::arrange(desc(win), desc(pf)) %>%
  dplyr::mutate(
    seed = case_when(
      rank_div == 1 ~ row_number()
    )
  ) %>%
  dplyr::group_by(conference_name) %>%
  dplyr::arrange(seed, desc(win), desc(pf)) %>%
  dplyr::mutate(
    seed = ifelse(is.na(seed), row_number(), seed),
    bowl = case_when(
      seed <= 2 ~ emoji::emoji("zzz"),
      seed >= 3 & seed <= 6 ~ emoji::emoji("trophy"),
      seed >= 7 & seed <= 12 ~ emoji::emoji("sports medal"),
      seed >= 13 ~ emoji::emoji("pile of poo")
    ),
    subline = paste(conference_name, division_name, sep = ", ")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(conference_name) %>%
  dplyr::select(season, week, franchise_name, win, loss, winloss, pf_sparkline, pp_dist, pf_rank:true_rank, elo_shift, franchise_elo_postgame, subline, seed, bowl, conference_name, -true_standing)

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
