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

current_standing_table <- current_standing %>%
  dplyr::select(-season, -week) %>%
  gt::gt() %>%
  gt::tab_header(
    title = paste("RFL Standings"),
    subtitle = paste("Woche", current_standing$week[1], current_standing$season[1])
  ) %>%

  gtExtras::gt_merge_stack(
    franchise_name,
    subline,
    small_cap = F,
    palette = c("#222f3e", "#8395a7"),
    #font_size = c("14px", "10px"),
    font_weight = c("normal", "normal")
  ) %>%

  gt::tab_spanner(
    label = "Standings",
    columns = c(win, loss, winloss, pf_sparkline, pp_dist)
  ) %>%

  gtExtras::gt_plt_winloss(
    winloss,
    max_wins = 26,
    palette = c("#10ac84", "#ee5253", "#c8d6e5"),
    type = "pill"
  ) %>%

  gtExtras::gt_plt_sparkline(
    pf_sparkline,
    type = "ref_iqr",
    fig_dim = c(5, 35),
    palette = c("#222f3e", "#222f3e", "#ee5253", "#10ac84", "#c8d6e5"),
    label = F,
    same_limit = F
  ) %>%
  gtExtras::gt_plt_dist(
    pp_dist,
    type = "density",
    fig_dim = c(5, 20),
    line_color = "#222f3e",
    fill_color = "#c8d6e5",
    same_limit = TRUE
  ) %>%

  gt::tab_spanner(
    label = "Power Rank",
    columns = dplyr::ends_with("_rank")
  ) %>%

  gt::tab_spanner(
    label = "Post-Season",
    columns = c(bowl, seed)
  ) %>%

  gt::data_color(
    dplyr::ends_with("_rank"),
    colors = scales::col_numeric(
      palette = c("#c8d6e5", "white"),
      domain = c(1,36)
    )
  ) %>%

  gt::tab_style(
    style = list(
      cell_text(weight = "bolder")
    ),
    locations = cells_body(
      columns = true_rank
    )
  ) %>%

  cols_merge(
    columns = c(franchise_elo_postgame, elo_shift),
    pattern = "{1} ({2})"
  ) %>%

  gtExtras::gt_add_divider(c(franchise_name, pp_dist, true_rank), color = "#c8d6e5", include_labels = F) %>%

  gt::cols_label(
    franchise_name = "Team",
    franchise_elo_postgame = "ELO",
    elo_shift = "+/-",
    win = "W",
    loss = "L",
    winloss = "Ergebnisse",
    pf_sparkline = "Points For",
    pp_dist = "PP",
    pf_rank = "PF",
    pp_rank = "PP",
    record_rank = "Record",
    all_play_rank = "All-Play",
    coach_rank = "Coach",
    true_rank = "Ovrl",
    bowl = "Bowl",
    seed = "Seed"
  ) %>%

  gt::cols_align(
    align = "center",
    columns = gt::everything()
  ) %>%

  gt::cols_align(
    align = "left",
    columns = c(franchise_name)
  ) %>%

  gt::tab_footnote(
    footnote = "Effizienz: PF - PP",
    locations = cells_column_labels(
      columns = coach_rank
    ),
    placement = "left"
  ) %>%

  gt::tab_footnote(
    footnote = "In Klammern = ELO Veränderung zur Vorwoche",
    locations = cells_column_labels(
      columns = franchise_elo_postgame
    ),
    placement = "left"
  ) %>%

  gtDefaults()
