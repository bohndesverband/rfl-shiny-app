matchup_table <- function(df) {
  df %>%
    gt::gt() %>%
    gt::tab_spanner(
      "Team",
      c(dplyr::starts_with("franchise"), elo_shift),
    ) %>%

    gt::cols_move(elo_shift, franchise_elo_pregame) %>%

    gt::tab_spanner(
      "Gegner",
      dplyr::starts_with("opponent"),
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%

    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = TRUE,
      ihtml.page_size_default = 24,
      ihtml.use_highlight = TRUE,
      ihtml.page_size_values = c(24, 50, 100),
      ihtml.use_filters = TRUE
    ) %>%
    gt::cols_label(
      season = "Saison",
      week = "Woche",
      franchise_name = "Name",
      franchise_elo_pregame = "Pregame ELO",
      franchise_score = "Punkte",
      elo_shift = "ELO Veränderung",
      elo_diff = "ELO Diff",
      opponent_score = "Punkte",
      opponent_elo_pregame = "Pregame ELO",
      opponent_name = "Name",
    ) %>%
    gt::cols_hide(c(upset, franchise_id, opponent_id)) %>%
    gtDefaults()
}

output$rfl_matchup_history_table <- gt::render_gt({
  rfl_matchups_history %>%
    dplyr::arrange(dplyr::desc(season), dplyr::desc(week)) %>%
    matchup_table()
})

# upsets ----

#plotly::ggplotly(
#  ggplot2::ggplot(data = subset(rfl_matchups_history, upset == 1 & elo_diff < -100 & franchise_elo_pregame < 1600), ggplot2::aes(x = opponent_elo_pregame, y = franchise_elo_pregame, text = label, alpha = season)) +
#    geom_point(color = color_grey_light, size = 3) +
#    geom_point(data = subset(rfl_matchups_history, upset == 1 & elo_diff < -100 & franchise_elo_pregame < 1600 & (franchise_id %in% c("0007") | opponent_id %in% c("0007"))), color = color_red, size = 5, alpha = 1) +
#
#    plot_defaults +
#    ggplot2::labs(
#      x = "Gegner Pregame ELO",
#      y = "Team Pregame ELO"
#    ),
#  tooltip = c("text")
#)

# TODO: Add plotly version

output$rfl_matchup_upsets_table <- gt::render_gt({
  rfl_matchups_history %>%
    dplyr::filter(upset == 1 & elo_diff < -100) %>%
    dplyr::arrange(elo_diff) %>%
    matchup_table() %>%
    gt::data_color(
      dplyr::ends_with("elo_pregame"),
      palette = c(color_red, color_blue)
    ) %>%
    gt::data_color(
      elo_diff,
      palette = c(color_red, color_blue)
    )
})
