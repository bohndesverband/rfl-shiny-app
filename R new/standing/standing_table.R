output$current_standing_table <- gt::render_gt({
  current_standing %>%
    group_by(conference_name) %>%
    dplyr::select(-season, -week, -div_rank, -conf_rank, -league_rank) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Standing"),
      subtitle = paste("Woche", current_standing$week[1], current_standing$season[1])
    ) %>%

    gtExtras::gt_merge_stack(
      franchise_name,
      division_name,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      #font_size = c("14px", "10px"),
      font_weight = c("normal", "normal")
    ) %>%

    gt::tab_spanner(
      label = "Standing",
      columns = c(wins_total, losses_total, winloss, pf_sparkline, pp_total, pf_total)
    ) %>%

    gtExtras::gt_plt_winloss(
      winloss,
      max_wins = 26,
      palette = c(color_green, color_red, color_yellow),
      type = "pill"
    ) %>%

    gtExtras::gt_plt_sparkline(
      pf_sparkline,
      type = "default",
      fig_dim = c(8, 35),
      palette = c(color_grey_dark, color_grey_dark, color_red, color_green, color_grey_light),
      label = TRUE,
      same_limit = FALSE
    ) %>%

    gtExtras::gt_plt_bullet(
      pp_total,
      target = pf_total,
      width = 30,
      palette = c(color_grey_light, color_green)
    ) %>%

    gt::tab_spanner(
      label = "ELO",
      columns = c(franchise_elo_postgame, elo_sparkline)
    ) %>%

    gtExtras::gt_plt_sparkline(
      elo_sparkline,
      type = "default",
      fig_dim = c(5, 35),
      palette = c(color_grey_dark, color_grey_dark, color_red, color_green, color_grey_light),
      label = FALSE,
      same_limit = FALSE
    ) %>%

    gtExtras::gt_merge_stack(
      franchise_elo_postgame,
      franchise_elo_pregame,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      #font_size = c("14px", "10px"),
      font_weight = c("normal", "normal")
    ) %>%

    gt::tab_spanner(
      label = "Power Rank",
      columns = c(dplyr::ends_with("_rank"))
    ) %>%

    gt::cols_merge(
      c(power_rank, power_rank_emoji)
    ) %>%

    gt::data_color(
      c(pf_rank:elo_rank),
      palette = c(color_grey_light, color_bg),
      domain = c(1,36)
    ) %>%

    gt::data_color(
      power_rank,
      palette = c(color_blue, color_red),
      domain = c(1,36)
    ) %>%

    gtExtras::gt_merge_stack(
      bowl,
      seed,
      small_cap = FALSE,
      palette = c(color_text, color_text),
      font_weight = c("normal", "normal")
    ) %>%

    gt::tab_style(
      style = list(
        cell_text(weight = "bolder")
      ),
      locations = gt::cells_body(
        columns = power_rank
      )
    ) %>%

    gt::tab_style(
      style = gt::cell_borders(sides = c("bottom"), weight = px(1), color = color_grey_mid),
      locations = cells_body(rows = divider == 1)
    ) %>%

    gt::cols_move(franchise_elo_postgame, elo_sparkline) %>%
    gt::cols_move(elo_rank, franchise_elo_postgame) %>%

    gtExtras::gt_add_divider(c(franchise_name, pp_total, franchise_elo_postgame), color = color_grey_light, include_labels = FALSE) %>%
    gtExtras::gt_add_divider(c(eff_rank, power_rank), color = color_bg, weight = "1px", include_labels = FALSE) %>%

    gt::cols_label(
      franchise_name = "Team",
      wins_total = "W",
      losses_total = "L",
      winloss = "Ergebnisse",
      pf_sparkline = "Points For",
      pp_total = "Potential Points",
      elo_sparkline = "Saisonverlauf",
      franchise_elo_postgame = paste("WK", current_standing$week[1]),
      elo_rank = "ELO",
      pf_rank = "PF",
      pp_rank = "PP",
      record_rank = "Record",
      all_play_rank = "All-Play",
      eff_rank = "Eff",
      power_rank = "Ovrl",
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

    gt::cols_hide(divider) %>%

    gtDefaults()
})
