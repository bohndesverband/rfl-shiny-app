fantasy_finishes_raw <- purrr::map_df(2016:var.season, function(x) {
  readr::read_rds(
    glue::glue("https://github.com/jak3sch/rfl/raw/main/data/playerscores/rfl-playerscores-{x}.rds"),
  )
})

rfl_fantasy_finishes <- fantasy_finishes_raw %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(season, player_id) %>%
  dplyr::summarise(
    pos = dplyr::last(pos),
    points = sum(as.numeric(points), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    rfl_drafts_data %>%
      dplyr::select(season, mfl_id) %>%
      dplyr::distinct() %>%
      dplyr::rename(draft_year = season),
    by = c("player_id" = "mfl_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::arrange(dplyr::desc(points)) %>%
  dplyr::mutate(
    pos_rank = row_number(),
    top3 = ifelse(pos_rank <= 3, 1, 0),
    top5 = ifelse(pos_rank <= 5, 1, 0),
    top8 = ifelse(pos_rank <= 8, 1, 0),
    top12 = ifelse(pos_rank <= 12, 1, 0),
    top24 = ifelse(pos_rank <= 24, 1, 0),
    top36 = ifelse(pos_rank <= 36, 1, 0),
    top48 = ifelse(pos_rank <= 48, 1, 0),
    top60 = ifelse(pos_rank <= 60, 1, 0),
  )

draft_with_elo <- rfl_drafts_data %>%
  dplyr::mutate(round = as.numeric(round)) %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::filter(row_number() == max(row_number())) %>%
      dplyr::select(mfl_id, player_elo_post) %>%
      dplyr::mutate(elo_shift = player_elo_post - 1500),
    by = "mfl_id"
  )

# https://www.rotoballer.com/dynasty-primer-1-how-to-value-dynasty-draft-picks/1343067
draft_with_elo_and_hit_rates <- reactive({
  draft_with_elo %>%
    dplyr::left_join(
      rfl_fantasy_finishes %>%
        dplyr::group_by(player_id) %>%
        dplyr::summarise_at(c("top3", "top5", "top8", "top12", "top24", "top36", "top48", "top60"), sum, na.rm = TRUE),
      by = c("mfl_id" = "player_id")
    ) %>%
    dplyr::mutate(
      hr = dplyr::case_when(
        is_rookie == 1 & top5 > 2 ~ 1, # top 5 finish
        is_rookie == 1 & season > (max(season) - 3) & season < max(season) & top5 > 1 ~ 1, # Top5 für junge spieler
        is_rookie == 1 & position %in% c("QB", "TE", "PK") & top3 > 3 ~ 1, # top 3 finish
        is_rookie == 1 & position %in% c("RB", "WR", "DL", "LB", "DB") & top12 > 3 ~ 1, # top 12 finish
        TRUE ~ 0
      ),
      hit = dplyr::case_when(
        is_rookie == 1 & position %in% c("QB", "TE", "PK") & top8 > 3 ~ 1, # top 8 finish
        is_rookie == 1 & season > (max(season) - 3) & position %in% c("QB", "TE", "PK") & top12 > 1 ~ 1, # top 12 für junge spieler
        is_rookie == 1 & position %in% c("RB", "WR", "DL", "LB", "DB") & top36 > 3 ~ 1, # top 36 finish
        is_rookie == 1 & season > (max(season) - 3) & position %in% c("RB", "WR", "DL", "LB", "DB") & top24 > 1 ~ 1, # top 24 für junge spieler
        TRUE ~ 0
      ),
      miss = dplyr::case_when(
        is_rookie == 1 & season < (max(season) - 2) & position %in% c("QB", "TE", "PK") & top24 == 0 ~ 1, # kein top 24 finish
        is_rookie == 1 & season < (max(season) - 2) & position %in% c("RB", "DL", "DB") & top48 == 0 ~ 1, # kein top 48 finish
        is_rookie == 1 & season < (max(season) - 2) & position %in% c("WR", "LB") & top60 == 0 ~ 1, # kein top 48 finish
        TRUE ~ 0
      )
    ) %>%
    #dplyr::filter(
    #  season >= 2017 & season <= 2021 &
    #    round >= 1 & round <= 7
    #)
    dplyr::filter(
      season >= input$draftHitRatesSelectSeason[1] & season <= input$draftHitRatesSelectSeason[2] &
        round >= input$draftHitRatesSelectRounds[1] & round <= input$draftHitRatesSelectRounds[2]
    )
})

draft_hit_rates <- reactive({
  draft_with_elo_and_hit_rates() %>%
    dplyr::group_by(round) %>%
    dplyr::summarise(
      count = n(),
      dplyr::across(c("hr", "hit", "miss"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate_at(
      c("hr", "hit", "miss"), ~ round(.x / count, 2)
    ) %>%
    tidyr::gather(category, pct, c("hr", "hit", "miss")) %>%
    dplyr::mutate(
      category = ifelse(category == "hr", "Home Run", stringr::str_to_title(category)),
    )
})

team_hit_rates <- reactive({
draft_with_elo_and_hit_rates() %>%
    dplyr::group_by(franchise, round) %>%
    dplyr::summarise(
      total_picks_in_round = n(),
      dplyr::across(c("hr", "hit", "miss"), ~ sum(.x, na.rm = TRUE)),
      hr_picks = hr,
      hit_picks = hit,
      miss_picks = miss,
      .groups = "drop"
    ) %>%
    dplyr::mutate_at(
      c("hr", "hit", "miss"), ~ round(.x / total_picks_in_round, 2)
    ) %>%
    tidyr::gather(category, pct, c("hr", "hit", "miss")) %>%
    dplyr::mutate(
      category = ifelse(category == "hr", "Home Run", stringr::str_to_title(category)),
      picks = dplyr::case_when(
        category == "Home Run" ~ hr_picks,
        category == "Hit" ~ hit_picks,
        category == "Miss" ~ miss_picks,
      )
    ) %>%
    dplyr::select(!dplyr::ends_with("_picks"))
})

team_hit_rates_table <- reactive({
  team_hit_rates() %>%
    dplyr::mutate(
      category = ifelse(category == "Home Run", "hr", tolower(category))
    ) %>%
    dplyr::group_by(franchise, category) %>%
    dplyr::summarise(
      total_picks = sum(total_picks_in_round, na.rm = TRUE),
      picks = sum(picks, na.rm = TRUE),
      pct = mean(pct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = category, values_from = c(total_picks, picks, pct)) %>%
    dplyr::left_join(
      franchises %>%
        dplyr::select(franchise_id, franchise_name),
      by = c("franchise" = "franchise_id")
    ) %>%
    dplyr::select(franchise_name, total_picks_hit, dplyr::starts_with("pct_"),  dplyr::starts_with("picks_")) %>%
    dplyr::arrange(dplyr::desc(pct_hit)) %>%

    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Draft Hit Rates"),
      #subtitle = paste(current_standing$week[1], current_standing$season[1])
    ) %>%

    gt::fmt_percent(
      columns = dplyr::starts_with("pct_"),
      decimals = 0
    ) %>%

    gtExtras::gt_merge_stack(
      pct_hit,
      picks_hit,
      palette = c("#222f3e", "#8395a7"),
    ) %>%

    gtExtras::gt_merge_stack(
      pct_hr,
      picks_hr,
      palette = c("#222f3e", "#8395a7"),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      pct_miss,
      picks_miss,
      palette = c("#222f3e", "#8395a7"),
      font_weight = c("normal", "normal")
    ) %>%

    gt::data_color(
      pct_hit,
      fn = scales::col_numeric(
        palette = c("white", "#c8d6e5"),
        domain = c(0,0.3)
      )
    ) %>%

    gt::data_color(
      pct_hr,
      fn = scales::col_numeric(
        palette = c("white", "#c8d6e5"),
        domain = c(0,0.2)
      )
    ) %>%

    gt::data_color(
      pct_miss,
      fn = scales::col_numeric(
        palette = c("#c8d6e5", "white"),
        domain = c(0.1,0.6)
      )
    ) %>%

    gt::data_color(
      dplyr::starts_with("total_"),
      fn = scales::col_numeric(
        palette = c("white", "#c8d6e5"),
        domain = c(0,70)
      )
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%

    gt::cols_align(
      align = "left",
      columns = c(franchise_name)
    ) %>%

    gt::cols_label(
      franchise_name = "Team",
      total_picks_hit = "Picks",
      pct_hit = "Hits",
      pct_hr = "Home Runs",
      pct_miss = "Misses",
    ) %>%

    gtDefaults()
})
