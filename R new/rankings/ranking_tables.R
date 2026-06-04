# modulat table ----
## base ----
ranking_table_base <- function(df) {
  df %>%
    gtExtras::gt_merge_stack(
      franchise_name,
      division_name,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      #font_size = c("14px", "10px"),
      font_weight = c("normal", "normal")
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
      franchise_name = "Team"
    ) %>%
    gt::cols_hide(c(franchise_id, season:division, div_rank:league_rank, seed, bowl, divider, seed_total, franchise_name_status)) %>%

    gtExtras::gt_highlight_rows(
      rows = franchise_id %in% c(input$selectRflTeams) | division %in% c(input$selectRflDivisions),
      fill = color_grey_light
    ) %>%
    gt::cols_width(
      franchise_name ~ gt::px(210)
    ) %>%
    gt::tab_footnote(
      "Meilensteine: D = Division; PB = Pro Bowl; SB = Super Bowl; zZ = Super Bowl Bye. Durchgestrichen = nicht mehr erreichbar",
    )
}

## standing ----
ranking_table_standing <- function(df) {
  df %>%
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
      palette = c(color_grey_mid, color_grey_dark)
    ) %>%

    gt::cols_label(
      wins_total = "W",
      losses_total = "L",
      winloss = "Ergebnisse",
      pf_sparkline = "Points For",
      pp_total = "Potential Points"
    )
}

## elo ----
ranking_table_elo <- function(df) {
  df %>%
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
      label = "ELO",
      columns = c(franchise_elo_postgame, elo_sparkline)
    ) %>%

    gt::cols_move(franchise_elo_postgame, elo_sparkline) %>%

    gt::data_color(
      elo_shift,
      palette = c(color_red, color_blue)
    ) %>%

    gt::cols_label(
      elo_sparkline = "Saisonverlauf",
      franchise_elo_postgame = paste("WK", rfl_current_standing$week[1]),
      elo_shift = "+/-"
    ) %>%

    gt::tab_footnote(
      footnote = "in klein ELO zum Start der Saison",
      locations = gt::cells_column_labels(
        columns = franchise_elo_postgame
      ),
      placement = "left"
    ) %>%
    gt::tab_footnote(
      footnote = "ELO Veränderung zur Vorwoche",
      locations = gt::cells_column_labels(
        columns = elo_shift
      ),
      placement = "left"
    )
}

## power rank ----
ranking_table_power_rank <- function(df) {
  df %>%
    gt::tab_spanner(
      label = "Power Rank",
      columns = c(dplyr::ends_with("_rank"))
    ) %>%

    gt::cols_merge(
      c(power_rank, power_rank_emoji)
    ) %>%

    gt::data_color(
      c(pf_rank:elo_rank),
      palette = c(color_blue, color_green, color_yellow, color_orange, color_red),
      domain = c(1,36)
    ) %>%

    gt::data_color(
      power_rank,
      palette = c(color_blue, color_red),
      domain = c(1,36)
    ) %>%

    gt::tab_style(
      style = list(
        cell_text(weight = "bolder")
      ),
      locations = gt::cells_body(
        columns = power_rank
      )
    ) %>%

    gt::cols_hide(elo_shift) %>%

    gt::cols_label(
      elo_rank = "ELO",
      pf_rank = "PF",
      pp_rank = "PP",
      record_rank = "Record",
      all_play_rank = "All-Play",
      eff_rank = "Eff",
      power_rank = "Ovrl",
    )
}

## bowl ----
ranking_table_bowl <- function(df) {
  df %>%
    gtExtras::gt_merge_stack(
      bowl_emoji,
      seed_emoji,
      small_cap = FALSE,
      palette = c(color_text, color_text),
      font_weight = c("normal", "normal")
    ) %>%

    gt::cols_label(
      bowl_emoji = "Bowl"
    )
}

## interactive ----
ranking_table_interactive <- function(df) {
  df %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = FALSE,
      ihtml.use_highlight = TRUE
    )
}
