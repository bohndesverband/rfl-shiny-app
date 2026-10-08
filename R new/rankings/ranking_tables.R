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
    gt::cols_hide(c(franchise_id, season, div_rank:league_rank, seed, bowl, divider, seed_total, franchise_name_status)) %>%

    gtExtras::gt_highlight_rows(
      rows = franchise_id %in% c(input$selectRflTeams),
      fill = color_grey_light
    ) %>%
    gt::cols_width(
      franchise_name ~ gt::px(175)
    ) %>%
    gt::tab_footnote(
      "Meilensteine: D = Division; PB = Pro Bowl; SB = Super Bowl; zZ = Super Bowl Bye. Durchgestrichen = nicht mehr erreichbar",
    )
}

## standing ----
ranking_table_standing <- function(df) {
  df <- df %>%
    gt::tab_spanner(
      label = "Standing",
      columns = c(wins_season, losses_season, winloss, pf_sparkline, pp_season, pf_season)
    ) %>%

    gtExtras::gt_plt_winloss(
      winloss,
      max_wins = 26,
      palette = c(color_green, color_red, color_yellow),
      type = "pill"
    )

  if (current_week > 2) {
    df <- df %>%
      gtExtras::gt_plt_sparkline(
        pf_sparkline,
        type = "default",
        fig_dim = c(8, 35),
        palette = c(color_grey_dark, color_grey_dark, color_red, color_green, color_grey_light),
        label = TRUE,
        same_limit = FALSE
      ) %>%
      gt::cols_label(
        pf_sparkline = "Points For"
      )
  } else {
    df <- df %>%
      gt::cols_hide(pf_sparkline)
  }

  df <- df %>%
    gtExtras::gt_plt_bullet(
      pp_season,
      target = pf_season,
      width = 30,
      palette = c(color_grey_mid, color_grey_dark)
    ) %>%

    gt::cols_width(
      wins_season ~ gt::px(50),
      losses_season ~ gt::px(50)
    ) %>%

    gt::cols_label(
      wins_season = "W",
      losses_season = "L",
      winloss = "Ergebnisse",
      pp_season = "Potential Points"
    )

  df
}

## elo ----
ranking_table_elo <- function(df) {
  df <- df %>%
    gtExtras::gt_plt_sparkline(
      elo_sparkline,
      type = "default",
      fig_dim = c(5, 35),
      palette = c(color_grey_dark, color_grey_dark, color_red, color_green, color_grey_light),
      label = FALSE,
      same_limit = FALSE
    )

    #gt_pctl_bar("franchise_elo_pregame", "franchise_elo_pregame_pctl") %>%
    #gt_pctl_bar("franchise_elo_postgame", "franchise_elo_postgame_pctl")

    if (current_week > 2) {
      df <- df %>%
        gt::tab_spanner(
          label = "ELO",
          columns = c(elo_pre, elo_sparkline, elo_post, elo_shift)
        ) %>%
        gt::cols_label(
          elo_pre = "WK 1",
          elo_sparkline = paste0("WK 1-", rfl_current_standing$week[1]),
          elo_post = paste("WK", rfl_current_standing$week[1]),
          elo_shift = "+/-"
        )
    } else {
      df <- df %>%
        gt::cols_hide(elo_sparkline) %>%
        gt::cols_label(
          elo_post = "ELO",
        )
    }

    df <- df %>%
      gt::cols_move(elo_post, elo_sparkline) %>%
      #gtExtras::gt_merge_stack(
      #  franchise_elo_pregame,
      #  franchise_elo_pregame_pctl,
      #  small_cap = FALSE,
      #  palette = c(color_text, color_text),
      #  font_weight = c("normal", "normal")
      #) %>%

      #gtExtras::gt_merge_stack(
      ##  franchise_elo_postgame,
      #  franchise_elo_postgame_pctl,
      #  small_cap = FALSE,
      #  palette = c(color_text, color_text),
      #  font_weight = c("normal", "normal")
      #) %>%

      gt::data_color(
        elo_shift,
        palette = c(color_red, color_blue)
      ) %>%

      gt::cols_width(
        elo_pre ~ gt::px(75),
        elo_post ~ gt::px(75),
        elo_shift ~ gt::px(50)
      ) %>%

      gt::tab_footnote(
        footnote = "ELO Veränderung zur Vorwoche",
        locations = gt::cells_column_labels(
          columns = elo_shift
        ),
        placement = "left"
      )

    df
}

## power ranking ----
ranking_table_power_rank <- function(df) {
  df <- df %>%
    gt::tab_spanner(
      label = "Power Rank",
      columns = c(dplyr::ends_with("_rank"))
    )

  if (current_week > 2) {
    df <- df %>%
      gt::cols_merge(
        c(power_rank, power_rank_emoji)
      )
  } else {
    df <- df %>%
      gt::cols_hide(power_rank_emoji)
  }

  df <- df %>%
    gtExtras::gt_merge_stack(
      elo_season_rank,
      elo_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      pf_rank,
      pf_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      pp_rank,
      pp_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      record_rank,
      wins_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      all_play_rank,
      all_play_wins_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      eff_rank,
      eff_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      war_starter_season_rank,
      war_starter_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    gtExtras::gt_merge_stack(
      power_rank,
      true_standing_season_pctl,
      small_cap = FALSE,
      palette = c(color_bg, color_bg),
      font_weight = c("normal", "normal")
    ) %>%

    #gt_plt_bar_pct(column = elo_season_pctl, scaled = TRUE) %>%
    #gt_pctl_bar("elo_season_rank", "elo_season_pctl") %>%
    #gt_pctl_bar("pf_rank", "pf_season_pctl") %>%
    #gt_pctl_bar("pp_rank", "pp_season_pctl") %>%
    #gt_pctl_bar("record_rank", "wins_season_pctl") %>%
    #gt_pctl_bar("all_play_rank", "all_play_wins_season_pctl") %>%
    #gt_pctl_bar("eff_rank", "eff_season_pctl") %>%
    #gt_pctl_bar("war_starter_season_rank", "war_starter_season_pctl") %>%
    #gt_pctl_bar("power_rank", "true_standing_season_pctl") %>%

    gt::data_color(
      c(pf_rank:elo_season_rank),
      palette = c(color_blue, color_green, color_yellow, color_orange, color_red),
      domain = c(1,36)
    ) %>%

    gt::data_color(
      power_rank,
      palette = c(color_blue, color_red),
      domain = c(1,36)
    ) %>%

    gt::cols_hide(elo_shift) %>%

    gt::cols_width(
      elo_season_rank ~ gt::px(50),
      pf_rank ~ gt::px(50),
      pp_rank ~ gt::px(50),
      eff_rank ~ gt::px(50),
      war_starter_season_rank ~ gt::px(70),
      record_rank ~ gt::px(70),
      all_play_rank ~ gt::px(70),
      power_rank ~ gt::px(70)
    ) %>%

    gt::cols_label(
      elo_season_rank = "ELO",
      pf_rank = "PF",
      pp_rank = "PP",
      record_rank = "Record",
      all_play_rank = "All-Play",
      eff_rank = "Eff",
      war_starter_season_rank = "WAR",
      power_rank = "Ovrl",
    )

  df
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

    gt::cols_width(
      bowl_emoji ~ gt::px(70)
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
