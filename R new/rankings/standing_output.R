source("R new/rankings/ranking_tables.R", local = TRUE)

# conference standing ----
output$conf_standing_table <- gt::render_gt({
  rfl_current_standing %>%
    dplyr::mutate(
      franchise_name = franchise_name_status
    ) %>%
    dplyr::group_by(conference_name) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Standing"),
      subtitle = paste("Woche", rfl_current_standing$week[1], rfl_current_standing$season[1])
    ) %>%

    ranking_table_base() %>%
    ranking_table_standing() %>%
    ranking_table_elo() %>%
    ranking_table_power_rank() %>%
    ranking_table_bowl() %>%

    gt::fmt_markdown(
      columns = franchise_name
    ) %>%

    gt::cols_move(elo_rank, franchise_elo_postgame) %>%

    gt::cols_hide(elo_shift) %>%

    gt::tab_style(
      style = gt::cell_borders(sides = c("bottom"), weight = px(1), color = color_grey_dark),
      locations = cells_body(rows = divider == 1)
    ) %>%

    gtExtras::gt_add_divider(c(franchise_name, pp_total, franchise_elo_postgame), color = color_grey_light, include_labels = FALSE) %>%
    gtExtras::gt_add_divider(c(eff_rank, power_rank), color = color_bg, weight = "1px", include_labels = FALSE) %>%
    gtDefaults()
})

# power ranking ----
output$power_ranking_table <- gt::render_gt({
  rfl_current_standing %>%
    dplyr::arrange(power_rank) %>%
    #dplyr::select(-season, -week, -div_rank, -conf_rank, -league_rank) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Power Ranking"),
      subtitle = paste("Woche", rfl_current_standing$week[1], rfl_current_standing$season[1])
    ) %>%

    ranking_table_base() %>%
    ranking_table_power_rank() %>%
    ranking_table_bowl() %>%

    gt::cols_move(power_rank, franchise_name) %>%
    gt::cols_move(elo_rank, power_rank) %>%

    gt::cols_hide(c(conference_name:elo_sparkline)) %>%

    gtDefaults() %>%
    ranking_table_interactive
})

# elo ----
output$elo_ranking_table <- gt::render_gt({
  rfl_current_standing %>%
    dplyr::arrange(dplyr::desc(franchise_elo_postgame)) %>%
    #dplyr::select(-season, -week, -div_rank, -conf_rank, -league_rank) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL ELO Ranking"),
      subtitle = paste("Woche", rfl_current_standing$week[1], rfl_current_standing$season[1])
    ) %>%

    ranking_table_base() %>%
    ranking_table_elo() %>%

    gt::cols_hide(c(conference_name:pf_sparkline, pp_total:pf_total, pf_rank:seed_emoji)) %>%

    gtDefaults()
})

# draft order ----
current_draft_order_week <- rfl_current_standing$week[1]

if (current_week > 13 & current_week <= 17) {
  current_draft_order_week <- current_week
}

if (current_week > 17) {
  current_draft_order_week <- 17
}

output$draft_order_table <- gt::render_gt({
  draft_order %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::left_join(
      rfl_current_standing %>%
        dplyr::filter(week == max(week)) %>%
        dplyr::select(franchise_id, season, div_rank, league_rank, divider, division_name, pp_total, power_rank, seed, bowl, bowl_emoji, seed_emoji),
      by = "franchise_id"
    ) %>%
    dplyr::arrange(pick) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Draft Reihenfolge"),
      subtitle = paste("Woche", current_draft_order_week, rfl_current_standing$season[1])
    ) %>%
    ranking_table_base() %>%
    ranking_table_bowl() %>%
    gtDefaults() %>%
    gt::cols_hide(week) %>%
    #gtExtras::gt_highlight_rows(rows = highlight_rows, fill = color_grey_light, font_weight = NULL) %>%
    gtExtras::gt_highlight_rows(
      rows = franchise_id %in% c(input$selectRflTeams) | division %in% c(input$selectRflDivisions),
      fill = color_grey_light
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = c("bottom"), weight = px(1), color = color_grey_dark),
      locations = cells_body(
        rows = seq(3, 36, 3)
      )
    ) %>%
    gt::data_color(
      pp_total,
      palette = c(color_red, color_blue)
    ) %>%
    gt::data_color(
      power_rank,
      palette = c(color_blue, color_red),
      domain = c(1,36)
    ) %>%
    gt::data_color(
      pick,
      palette = c(color_red, color_blue),
      domain = c(1,36)
    ) %>%
    gt::cols_label(
      pick = "Pick",
      pp_total = "Potential Points",
      power_rank = "Power Rank"
    ) %>%
    gtExtras::gt_add_divider(c(franchise_name, pick, pp_total, power_rank), color = color_grey_light, include_labels = FALSE)
})

