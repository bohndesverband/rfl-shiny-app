# manipulate & filter data ----
# TODO: https://www.rotoballer.com/dynasty-primer-1-how-to-value-dynasty-draft-picks/1343067
rfl_hit_rates <- shiny::reactive({
  #rfl_hit_rates <-

  rfl_drafts_data %>%
    #filter(mfl_id == "15329") %>%
    #filter(pos_grouped == "TE") %>%
    dplyr::filter((season >= input$selectYears[1] & season <= input$selectYears[2]) & (round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2])) %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        pos %in% input$selectPositions
      else
        TRUE
    ) %>%
    dplyr::mutate(
      hr = dplyr::case_when(
        top3 >= 2 ~ 1, # mind. 2 Top3
        season > (new_season_march - 3) & season < new_season_march & top8 >= 1 ~ 1, # 1 Top8 für junge spieler
        pos_grouped %in% c("QB", "TE", "PK") & top5 >= 3 ~ 1, # mind. 3 Top5 für single starter
        pos_grouped %in% c("QB", "TE", "PK") & top12 >= 6 ~ 1, # mind. 6 Top12 für single starter
        pos_grouped %in% c("RB", "WR", "DL", "LB", "DB") & top12 >= 3 ~ 1, # mind. 3 Top12 für multi starter
        pos_grouped %in% c("RB", "WR", "DL", "LB", "DB") & top24 >= 6 ~ 1, # mind. 6 Top24 für multi starter
        TRUE ~ 0
      ),
      hit = dplyr::case_when(
        pos_grouped %in% c("QB", "TE", "PK") & top8 >= 2 ~ 1, # mind 2 Top8 für single starter
        pos_grouped %in% c("QB", "TE", "PK") & top12 >= 4 ~ 1, # mind 4 Top12 für single starter
        season > (new_season_march - 3) & pos_grouped %in% c("QB", "TE", "PK") & top12 >= 1 ~ 1, # Top12 für junge spieler
        pos_grouped %in% c("RB", "WR", "DL", "LB", "DB") & top24 >= 2 ~ 1, # mind 2 Top24 für multi starter
        pos_grouped %in% c("RB", "WR", "DL", "LB", "DB") & top36 >= 4 ~ 1, # mind 3 Top36 für multi starter
        season > (new_season_march - 3) & pos_grouped %in% c("RB", "WR", "DL", "LB", "DB") & top24 >= 1 ~ 1, # 1 Top24 für junge spieler
        TRUE ~ 0
      ),
      miss = dplyr::case_when(
        season < (new_season_march - 3) & pos_grouped %in% c("QB", "TE", "PK") & top24 == 0 ~ 1, # kein top 24 finish
        season < (new_season_march - 3) & pos_grouped %in% c("RB", "DL", "DB") & top48 == 0 ~ 1, # kein top 48 finish
        season < (new_season_march - 3) & pos_grouped %in% c("WR", "LB") & top60 == 0 ~ 1, # kein top 48 finish
        TRUE ~ 0
      )
    )
})

hit_rates_average <- shiny::reactive({
  #hit_rates_average <-

  rfl_hit_rates() %>%
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

hit_rates_per_team <- shiny::reactive({
  #hit_rates_per_team <-

  rfl_hit_rates() %>%
    dplyr::group_by(franchise_id, franchise_name, round) %>%
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

output$draft_hit_rates_plot <- shiny::renderPlot({
  ggplot2::ggplot(hit_rates_per_team(), ggplot2::aes(x = round, y = pct, group = category, color = franchise_name)) +
    ggplot2::facet_wrap(~ category, ncol = 1) +

    ggplot2::geom_boxplot(ggplot2::aes(group = round), fill = color_grey_light, color = color_grey_mid, linewidth = 0.15, outliers = FALSE) +
    ggplot2::geom_jitter(ggplot2::aes(size = total_picks_in_round, alpha = total_picks_in_round), width = 0.25, color = color_grey_mid) +
    ggplot2::geom_text(data = hit_rates_average(), ggplot2::aes(label = paste("Avg:", scales::percent(pct, 1))), y = 1, show.legend = FALSE, color = color_grey_dark, size = 4) +

    # TODO: mehrere teams ausgewählt führt zu fehlern
    #ggalt::geom_xspline(data = subset(hit_rates_per_team(), franchise_id %in% c(input$selectRflTeams)), spline_shape = -0.5) +
    #ggplot2::aes(lwd = 1) +
    #ggplot2::scale_linewidth_identity() +

    ggplot2::geom_point(data = subset(hit_rates_per_team(), franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(size = total_picks_in_round)) +
    ggrepel::geom_label_repel(data = subset(hit_rates_per_team(), franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(label = scales::percent(pct, 1))) +

    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1, 8)) +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean +
    ggplot2::scale_x_continuous(limits = c(0.6, 7.4), breaks = seq(1, 7, 1)) +
    ggplot2::scale_y_continuous(limits = c(-0.1, 1.1), labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(
      title = paste0("RFL Draft Hit Rates ", input$selectYears[1], "-", input$selectYears[2]),
      subtitle = paste0("Wie gut haben die RFL Teams gepickt?\nAlle ", paste(input$selectPositions, collapse = ", "), ";\nRunden ", input$selectDraftRounds[1], "-", input$selectDraftRounds[2]),
      x = "Runde",
      y = "Prozentualer Anteil",
      color = "RFL Team",
      size = "Anzahl Picks in Runde",
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1)
    )
}, height = 900)

output$team_hit_rates_table <- gt::render_gt({
  formatted_players <- rfl_hit_rates() %>%
      dplyr::mutate(
        label = paste(season, paste0(round, ".", pick, " (", overall, ")"), player_name, paste0("(", team, ", ", pos, ")"))
      ) %>%
      tidyr::pivot_longer(cols = c(hit, hr, miss), names_to = "category", values_to = "cat_value") %>%
      group_by(franchise_id, category) %>%
      dplyr::filter(cat_value == 1) %>%
      dplyr::arrange(season, overall) %>%
      dplyr::summarise(
        label = paste(label, collapse = "\n"),
        .groups = "drop"
      ) %>%
      tidyr::spread(category, label)

  formatted_players %>%
    dplyr::left_join(
      hit_rates_per_team() %>%
        dplyr::group_by(franchise_id, franchise_name, category) %>%
        dplyr::summarise(
          total_picks = sum(total_picks_in_round, na.rm = TRUE),
          picks = sum(picks, na.rm = TRUE),
          pct = round(mean(pct, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(names_from = category, values_from = c(total_picks, picks, pct)),
      by = "franchise_id"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::starts_with("total_")) %>%
    dplyr::arrange(dplyr::desc(pct_Hit)) %>%

    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Draft Hit Rates"),
      #subtitle = paste0(input$selectYears[1], "-", input$selectYears[2], " Runden ", input$selectDraftRounds[1], "-", input$selectDraftRounds[2])
    ) %>%

    gt::tab_spanner(
      "Hits",
      columns = c(pct_Hit, picks_Hit, hit)
    ) %>%

    gt::tab_spanner(
      "Home Runs",
      columns = c("pct_Home Run", "picks_Home Run", hr)
    ) %>%

    gt::tab_spanner(
      "Misses",
      columns = c(pct_Miss, picks_Miss, miss)
    ) %>%

    gt::fmt_percent(
      columns = dplyr::starts_with("pct_"),
      decimals = 0
    ) %>%

    gt::data_color(
      pct_Hit,
      palette = c(color_red, color_blue)
    ) %>%

    gt::data_color(
      picks_Hit,
      palette = c(color_bg, color_grey_light)
    ) %>%

    gt::data_color(
      "pct_Home Run",
      palette = c(color_red, color_blue)
    ) %>%

    gt::data_color(
      "picks_Home Run",
      palette = c(color_bg, color_grey_light)
    ) %>%

    gt::data_color(
      pct_Miss,
      palette = c(color_blue, color_red)
    ) %>%

    gt::data_color(
      picks_Miss,
      palette = c(color_grey_light, color_bg)
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
      pct_Hit = "Pct",
      picks_Hit = "Picks",
      hit = "Spieler",
      "pct_Home Run" = "Pct",
      "picks_Home Run" = "Picks",
      hr = "Spieler",
      pct_Miss = "Pct",
      picks_Miss = "Picks",
      miss = "Spieler",
    ) %>%

    gtDefaults() %>%

    gtExtras::gt_highlight_rows(
      rows = franchise_id %in% input$selectRflTeams,
      columns = franchise_name,
      fill = color_grey_mid
    ) %>%

    gt::cols_hide(franchise_id) %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_filters = FALSE,
      ihtml.use_search = TRUE,
      ihtml.use_pagination = TRUE,
      ihtml.use_page_size_select = TRUE,
      ihtml.page_size_default = 6,
      ihtml.page_size_values = c(12, 24, 36),
      ihtml.use_highlight = TRUE
    )
})

reframe_hit_rate_data <- function(df) {
  df %>%
    filter(pos != "PK") %>%
    dplyr::group_by(pos, round) %>%
    dplyr::mutate(pos_picks = n()) %>%
    tidyr::gather(key, value, c(hr, hit, miss)) %>%
    dplyr::filter(value == 1) %>%
    dplyr::group_by(pos, round, key) %>%
    dplyr::reframe(
      value = sum(value, na.rm = TRUE) / pos_picks
    ) %>%
    dplyr::distinct()
}

nfl_drafts_with_hit_rates <- shiny::reactive({
  #nfl_drafts_with_hit_rates <-
  rfl_hit_rates() %>%
    filter(season > 2016) %>%
    dplyr::select(gsis_id, pos, hr, hit, miss) %>%
    dplyr::left_join(
      nfl_drafts_data,
      by = "gsis_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      nfl_round = dplyr::case_when(
        gsis_id == "00-0030383" ~ 3,
        is.na(nfl_round) ~ 8,
        TRUE ~ nfl_round
      )
    ) %>%
    dplyr::distinct()
})

output$draft_hit_rates_by_round <- shiny::renderPlot({
  nfl_draft_data <- nfl_drafts_with_hit_rates() %>%
    dplyr::rename(round = nfl_round) %>%
    reframe_hit_rate_data()

  rfl_hit_rates() %>%
    filter(season > 2016) %>%
    reframe_hit_rate_data() %>%
    ggplot2::ggplot(ggplot2::aes(x = round, y = value, group = key, color = key)) +
    ggplot2::facet_wrap(~ factor(pos, levels = positions_full), ncol = 3) +
    ggplot2::geom_col(data = nfl_draft_data, ggplot2::aes(fill = key), alpha = 0.3, position = "dodge", linetype = 0, show.legend = FALSE) +
    ggplot2::geom_point(size = 3) +
    ggalt::geom_xspline(spline_shape = -0.5) +
    ggplot2::aes(lwd = 1) +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_discrete(type = c(color_green, color_blue, color_red), labels = c("Hits", "Home Runs", "Misses")) +
    ggplot2::scale_fill_discrete(type = c(color_green, color_blue, color_red), labels = c("Hits", "Home Runs", "Misses")) +
    ggplot2::scale_x_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-0.1, 1.1), labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, by = 0.2), expand = c(0, 0)) +
    plot_defaults +
    ggplot2::theme(
      panel.grid.minor.x = ggplot2::element_blank(),
    ) +
    ggplot2::labs(
      title = "Hit Rates nach Draftrunde",
      subtitle = paste("Abgebildet wird der Durchschnitt aller Draftpicks", paste0(input$selectYears[1], "-", input$selectYears[2]), "\nDie durchgezogenen Linien und Punkte sind Daten des RFL Drafts, die Balken die des NFL Drafts."),
      x = "Draft Runde",
      y = "Prozentualer Anteil aller Picks der selben Positionsgruppe in der Draftrunde",
      color = ""
    )
}, height = 900)

# fantasy finishes + hit rates ----

source("R new/players/fantasy_finishes_output.R", local = TRUE)

output$hit_rates_fantasy_finishes <- gt::render_gt({
  rfl_fantasy_finishes %>%
    dplyr::left_join(
      nflreadr::load_ff_playerids() %>%
        dplyr::select(mfl_id, gsis_id),
      by = c("player_id" = "mfl_id")
    ) %>%
    dplyr::left_join(
      rfl_drafts_rookies %>%
        dplyr::group_by(mfl_id) %>%
        dplyr::summarise(
          rfl_rounds = paste(as.numeric(round), collapse = ", "),
          draft_year = dplyr::first(season)
        ),
      by = c("player_id" = "mfl_id")
    ) %>%
    dplyr::filter(!is.na(draft_year)) %>%
    dplyr::left_join(
      nfl_drafts_with_hit_rates() %>%
        dplyr::select(gsis_id, nfl_round),
      by = "gsis_id",
      na_matches = "never"
    ) %>%
    dplyr::select(-gsis_id) %>%
    gt_fantasy_finishes() %>%
    gt::tab_header(
      title = paste("Fantasy Finishes aller in der RFL gedrafteten Spieler seit 2017")
    ) %>%
    gt::cols_move(
      c(nfl_round, rfl_rounds, draft_year),
      position
    ) %>%
    gt::tab_spanner(
      label = "Draft",
      columns = c(nfl_round, rfl_rounds, draft_year)
    ) %>%
    gt::cols_label(
      nfl_round = "NFL Runde",
      rfl_rounds = "RFL Runden",
      draft_year = "Jahr"
    )
})
