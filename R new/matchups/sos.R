rfl_sos <- feather::read_feather("data/rfl_sos.feather") %>%
  dplyr::mutate(
    above_avg = round(Total - mean(Total), 3),
    SOS = round(SOS, 3),
  ) %>%
  dplyr::left_join(rfl_franchise_data %>% dplyr::rename(div_id = division) %>% select(franchise_id, franchise_name, div_id), by = "franchise_id")

rfl_sos_filtered <- shiny::reactive({
  req(input$selectYears[1] >= 2024)

  rfl_sos_filtered <- rfl_sos %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
      #season == 2025
    ) %>%
    #dplyr::filter(
    #  season >= 2024 & season <= 2025
    #) %>%
    dplyr::group_by(franchise_id, div_id) %>%
    dplyr::summarise(
      franchise_name = dplyr::last(franchise_name),
      div_name = dplyr::last(div_name),
      dplyr::across(
        c("above_avg", "Total", "Division", "Conference", "Random", "SOS"),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
 })

# Sos abv. avg ----
output$rfl_sos_total <- shiny::renderPlot({
  shiny::validate(
    shiny::need(input$selectYears[1] >= 2024, "Die Daten gibt es erst seit 2024")
  )

  ggplot2::ggplot(rfl_sos_filtered(), ggplot2::aes(x = above_avg, y = reorder(franchise_name, above_avg))) +
    plot_defaults +
    ggplot2::geom_col(fill = color_grey_dark) +
    ggplot2::geom_col(data = subset(rfl_sos_filtered(), div_id %in% input$selectRflDivisions | franchise_id %in% input$selectRflTeams), ggplot2::aes(fill = franchise_name)) +
    ggplot2::scale_fill_discrete(type = colors) +
    ggplot2::labs(
      title = paste(paste0("RFL Preseason Strength of Schedule (SoS) im Vergleich zum Durchschnitt (", input$selectYears[1], "-", input$selectYears[2], ")")),
      subtitle = paste("Je niedriger der Wert, desto leichter ist der SoS im Vergleich zum Rest der Liga."),
      y = "",
      x = "Abweichung des SoS vom Durchschnitt",
      fill = ""
    ) +
    ggplot2::theme(
      legend.position = "top"
    )
}, height = 800)

# SoS nach matchups ----
chartSosByMatchupData <- shiny::reactive({
  req(input$selectYears[1] >= 2024)

  chartSosByMatchupData <- rfl_sos_filtered() %>%
    gather(matchup, sos, c(Total, Division, Conference, Random))
})

output$rfl_sos_matchups <- shiny::renderPlot({
  shiny::validate(
    shiny::need(input$selectYears[1] >= 2024, "Die Daten gibt es erst seit 2024")
  )

  ggplot2::ggplot(chartSosByMatchupData(), aes(x = matchup, y = sos, group = franchise_name, color = franchise_name)) +
    plot_defaults +

    ggplot2::geom_point(size = 3, color = color_grey_dark, alpha = 0.5) +
    ggbump::geom_bump(data = subset(chartSosByMatchupData(), div_id %in% input$selectRflDivisions | franchise_id %in% input$selectRflTeams), size = 1) +
    ggplot2::geom_point(data = subset(chartSosByMatchupData(), div_id %in% input$selectRflDivisions | franchise_id %in% input$selectRflTeams), size = 5) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::labs(
      title = "RFL Preseason SoS nach Matchups",
      x = "Matchup Typ",
      y = "Durchschnittliche Win % der Gegner",
      color = ""
    ) +
    ggplot2::theme(
      legend.position = "top"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent)

}, height = 800)

# inseason sos ----
rfl_sos_data_filtered <- shiny::reactive({
  rfl_sos_data_filtered <- rfl_sos_data %>%
    dplyr::filter(
      #season >= input$selectYears[1] & season <= input$selectYears[2]
      season == 2025
    ) %>%
    #dplyr::filter(
    #  if(isTruthy(input$selectRflTeams))
    #    franchise_id %in% input$selectRflTeams
    #  else
    #    TRUE
    #) %>%
    #dplyr::filter(
    #  if(isTruthy(input$selectRflDivisions))
    #    division %in% input$selectRflDivisions
    #  else
    #    TRUE
    #) %>%
    dplyr::group_by(franchise_id, franchise_name, division) %>%
    dplyr::summarise(
      dplyr::across(
        c(
          sos_preseason,
          sos_previous,
          sos_upcoming,
          sos_total,
          sos_score_preseason,
          sos_score_previous,
          sos_score_upcoming,
          sos_score_total
        ),
        ~ mean(.x, na.rm = TRUE)
      ),
      sos_change = sos_total - sos_preseason,
      sos_diff = sos_score_previous - sos_score_upcoming,
      .groups = "drop"
    )
})%>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

output$rfl_sos_insesason <- gt::render_gt({
  rfl_sos_data_filtered() %>%
    tidyr::replace_na(
      list(
        sos_preseason = 0,
        sos_previous = 0,
        sos_upcoming = 0,
        sos_total = 0,
        sos_score_preseason = 0,
        sos_score_previous = 0,
        sos_score_upcoming = 0,
        sos_score_total = 0,
        sos_change = 0,
        sos_diff = 0
      )
    ) %>%
    dplyr::arrange(dplyr::desc(sos_score_total)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "RFL Strength of Schedule Überblick",
      subtitle = "Ranking des SOS basierend auf der aktuellen All-Play Win % aller Gegner eines Teams"
    ) %>%
    gt::cols_move(
      c(sos_total, sos_change),
      franchise_name
    ) %>%
    gt::cols_move(
      c(sos_preseason, sos_score_preseason),
      sos_change
    ) %>%
    gt::cols_move(
      c(sos_previous, sos_upcoming),
      sos_score_preseason
    ) %>%

    gt::cols_move_to_end(
      sos_diff
    ) %>%

    gt::tab_spanner(
      "Preseason",
      columns = c(sos_preseason, sos_score_preseason, sos_change)
    ) %>%

    gt::tab_spanner(
      "Total",
      columns = c(sos_total, sos_score_total)
    ) %>%

    gt::tab_spanner(
      "Bisher",
      columns = c(sos_previous, sos_score_previous)
    ) %>%

    gt::tab_spanner(
      "Kommend",
      columns = c(sos_upcoming, sos_score_upcoming)
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

    gt::fmt_percent(
      columns = c(sos_preseason, sos_previous, sos_upcoming, sos_total, sos_change),
      decimals = 1
    ) %>%

    gt::fmt_number(
      columns = c(sos_score_preseason, sos_score_total, sos_score_previous, sos_score_upcoming),
      decimals = 1,
      scale_by = 100
    ) %>%

    gt::fmt_number(
      columns = sos_diff,
      decimals = 3,
    ) %>%

    gt::data_color(
      columns = gtExtras::starts_with("sos_score_"),
      palette = c(color_grey_light, color_bg),
    ) %>%

    gt::data_color(
      columns = c(sos_change),
      palette = c(color_blue, color_red),
    ) %>%

    gt::data_color(
      columns = c(sos_diff),
      palette = c(color_red, color_bg, color_blue),
    ) %>%

    gt::data_color(
      columns = c(sos_preseason, sos_previous, sos_upcoming, sos_total),
      palette = c(color_blue, color_green, color_yellow, color_red),
    ) %>%

    #gtExtras::gt_highlight_rows(
    #  rows = franchise_id %in% c(input$selectRflTeams) | division %in% c(input$selectRflDivisions),
    #  fill = color_grey_light
    #)
    gtDefaults() %>%
    gt::tab_footnote(
      "SOS basierend auf der All-Play Win % der vergangenen Saison",
      cells_column_labels(columns = sos_preseason)
    ) %>%
    gt::tab_footnote(
      "SOS bisher - SOS Kommend. Je höher der Wert, desto einfacher ist der kommende Schedule im Vergleich zum bisherigen",
      cells_column_labels(columns = sos_diff)
    ) %>%
    gt::tab_footnote(
      "Win % Veränderung im Vergleich zur Preseason. Niedrige Werte = Schedule ist einfacher geworden",
      cells_column_labels(columns = sos_change)
    ) %>%
    gt::cols_hide(c(franchise_id, division)) %>%
    gt::cols_label(
      sos_preseason = "Win %",
      sos_score_preseason = "SOS",
      sos_previous = "Win %",
      sos_score_previous = "SOS",
      sos_upcoming = "Win %",
      sos_score_upcoming = "SOS",
      sos_total = "Win %",
      sos_score_total = "SOS",
      sos_change = "+/-",
      sos_diff = "Diff"
    ) %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_pagination = FALSE,
      ihtml.use_highlight = TRUE
    )

  # todo: highlight rows
  # todo: click auf zeile wählt team aus (wie bei player elo)
  # todo: spalten breiten
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

output$sos_history <- shiny::renderPlot({
  ggplot2::ggplot(rfl_sos_data, ggplot2::aes(x = season, y = sos_total, color = franchise_name)) +
    ggplot2::geom_boxplot(ggplot2::aes(group = season), fill = color_grey_light, color = color_grey_mid, linewidth = 0.15, outliers = FALSE) +
    ggplot2::geom_jitter(ggplot2::aes(size = sos_score_total, alpha = sos_score_total), width = 0.25, color = color_grey_mid) +

    ggalt::geom_xspline(data = subset(rfl_sos_data, franchise_id %in% c(input$selectRflTeams)), spline_shape = -0.5) +
    ggplot2::aes(lwd = 1.2) +
    ggplot2::scale_linewidth_identity() +

    ggplot2::geom_point(data = subset(rfl_sos_data, franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(size = sos_score_total)) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1,8), guide = "none") +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean +
    scale_y_continuous(labels = scales::percent, limits = c(min(rfl_sos_data$sos_total), max(rfl_sos_data$sos_total))) +
    ggplot2::scale_x_continuous(limits = c(2016 - 0.4, max(rfl_sos_data$season) + 0.4), labels = c(2016:max(rfl_sos_data$season)), breaks = c(2016:max(rfl_sos_data$season))) +
    ggplot2::labs(
      title = "Historischer RFL Strength of Schedule",
      x = "Saison",
      y = "Ø All-Play Win % aller Gegner",
      color = "RFL Teams",
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1)
    )
}, height = 600)

color_cells <- function(x) {
  dplyr::case_when(
    x == "kommend" ~ color_red,
    x == "bisher" ~ color_blue,
    grepl(",", x) ~ color_orange,
    TRUE ~ color_black
  )
}

add_data_color <- function(df, color, data) {
  df %>%
    gt::data_color(
      columns = color,
      target_columns = data,
      palette = c(color_blue, color_bg, color_red),
      domain = c(0, 0.5, 1)
    )
}

output$rfl_schedule <- gt::render_gt({
  #req(new_season_march != new_season_sept)

  latest_week <- ifelse(input$selectYears[2] == new_season_sept, current_week - 1, max(data$week))

  #latest_week <- 12

  data <- rfl_schedule_data %>%
    dplyr::filter(
      season == input$selectYears[2]
      #season == 2025
    ) %>%
    dplyr::mutate(
      type = ifelse(week <= latest_week, "bisher", "kommend")
    ) %>%
    dplyr::left_join(
      rfl_team_elo %>%
        dplyr::select(season, week, franchise_id, opponent_id, franchise_elo_pregame, opponent_elo_pregame),
      by = c("season", "week", "franchise_id", "opponent_id")
    ) %>%
    # carry last known pregame elo forward for teams and opponents within each season
    dplyr::group_by(season, franchise_id) %>%
    dplyr::arrange(week) %>%
    tidyr::fill(franchise_elo_pregame, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season, opponent_id) %>%
    dplyr::arrange(week) %>%
    tidyr::fill(opponent_elo_pregame, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      p_win = 1 / (1 + 10^((opponent_elo_pregame - franchise_elo_pregame) / 400)),
      difficulty_raw = 1 - p_win,
      win_difficulty = (difficulty_raw - min(difficulty_raw, na.rm = TRUE)) / (max(difficulty_raw, na.rm = TRUE) - min(difficulty_raw, na.rm = TRUE))
    ) %>%
    #filter(franchise_id == "0007") %>%
    dplyr::group_by(franchise_id, type) %>%
    dplyr::mutate(
      win_difficulty_avg = round(mean(win_difficulty, na.rm = TRUE), 3)
    ) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::mutate(
      win_difficulty_delta = (dplyr::last(win_difficulty_avg) - dplyr::first(win_difficulty_avg)) * 100,
    ) %>%
    dplyr::group_by(franchise_id, week) %>%
    dplyr::mutate(
      matchup = paste(type, paste0("WK", week), paste0("m_", dplyr::row_number()), sep = ".")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name, abbrev),
      by = c("franchise_id")
    ) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, opponent_abbrev = abbrev),
      by = c("opponent_id" = "franchise_id")
    ) %>%
    dplyr::arrange(win_difficulty_delta, dplyr::desc(win_difficulty_avg)) %>%
    dplyr::select(-week, -p_win, -difficulty_raw, -dplyr::ends_with("_elo_pregame"), -type, -opponent_id, -win_difficulty_avg) %>%
    tidyr::pivot_wider(names_from = matchup, values_from = c(opponent_abbrev, win_difficulty), names_glue = "{matchup}_{.value}")

  #latest_week <- 7

  data %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        franchise_id %in% input$selectRflTeams
      else
        TRUE
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::html(paste0("RFL Schedule Difficulty ", input$selectYears[2], ": Welche Teams treffen auf <span style=\"background-color: ", color_blue, "; color: white;\">einfachere</span> oder <span style=\"background-color: ", color_red, "; color: white;\">schwerere</span> Gegner in den verbleibendenen Wochen?")),
      subtitle = "Anhand der Pregame-ELO wird von jedem Matchup die Sieg-Wahrscheinlichkeit berechnet.\nDie Differenz aus dem Durchschnitt der kommenden und dem Durchschnitt der bisherigen Matchups ergibt das Schedule Delta (Δ). Hier wird die Stärke des eigenen Teams und die relative Stärke des Gegners zum eigenen Team berücksichtigt. Je kleiner der Wert, desto einfacher ist der kommende Schedule gemessen am bisherigen."
    ) %>%

    gt::tab_spanner_delim(delim = ".") %>%
    gt::cols_move(franchise_name, paste0("bisher.WK", latest_week, ".m_2_opponent_abbrev")) %>%
    gt::cols_move(win_difficulty_delta, franchise_name) %>%
    gt::cols_hide(c(season, franchise_id, dplyr::ends_with("_win_difficulty"))) %>%

    gt::cols_label(
      dplyr::ends_with("1_opponent_abbrev") ~ "M1",
      dplyr::ends_with("2_opponent_abbrev") ~ "M2",
      franchise_name = "",
      win_difficulty_delta = "Schedule Δ"
    ) %>%
    gtDefaults() %>%
    gtExtras::gt_merge_stack(
      franchise_name,
      abbrev,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%

    add_data_color(dplyr::contains("WK1.m_1_win_difficulty"), dplyr::contains("WK1.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK1.m_2_win_difficulty"), dplyr::contains("WK1.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK2.m_1_win_difficulty"), dplyr::contains("WK2.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK2.m_2_win_difficulty"), dplyr::contains("WK2.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK3.m_1_win_difficulty"), dplyr::contains("WK3.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK3.m_2_win_difficulty"), dplyr::contains("WK3.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK4.m_1_win_difficulty"), dplyr::contains("WK4.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK4.m_2_win_difficulty"), dplyr::contains("WK4.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK5.m_1_win_difficulty"), dplyr::contains("WK5.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK5.m_2_win_difficulty"), dplyr::contains("WK5.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK6.m_1_win_difficulty"), dplyr::contains("WK6.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK6.m_2_win_difficulty"), dplyr::contains("WK6.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK7.m_1_win_difficulty"), dplyr::contains("WK7.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK7.m_2_win_difficulty"), dplyr::contains("WK7.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK8.m_1_win_difficulty"), dplyr::contains("WK8.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK8.m_2_win_difficulty"), dplyr::contains("WK8.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK9.m_1_win_difficulty"), dplyr::contains("WK9.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK9.m_2_win_difficulty"), dplyr::contains("WK9.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK10.m_1_win_difficulty"), dplyr::contains("WK10.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK10.m_2_win_difficulty"), dplyr::contains("WK10.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK11.m_1_win_difficulty"), dplyr::contains("WK11.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK11.m_2_win_difficulty"), dplyr::contains("WK11.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK12.m_1_win_difficulty"), dplyr::contains("WK12.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK12.m_2_win_difficulty"), dplyr::contains("WK12.m_2_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK13.m_1_win_difficulty"), dplyr::contains("WK13.m_1_opponent_abbrev")) %>%
    add_data_color(dplyr::contains("WK13.m_2_win_difficulty"), dplyr::contains("WK13.m_2_opponent_abbrev")) %>%

    #gtExtras::gt_highlight_rows(
    #  rows = franchise_id %in% input$selectRflTeams,
    #  columns = c(franchise_name),
    #  fill = color_grey_light
    #) %>%

    gt::data_color(
      columns = win_difficulty_delta,
      palette = c(color_blue, color_green, color_yellow, color_orange, color_red)
    ) %>%

    gt::cols_width(
      franchise_name ~ gt::px(210),
      win_difficulty_delta ~ gt::px(80),
      everything() ~ gt::px(40),
    ) %>%

    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_align(
      align = "left",
      columns = c(franchise_name)
    ) %>%

    gt::tab_options(
      table.font.size = gt::px(12),
      column_labels.padding = gt::px(5),
      row_group.padding.horizontal = gt::px(5),
      data_row.padding.horizontal = gt::px(5),
      column_labels.padding.horizontal = gt::px(5)
    )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# matchup over/underachievments ----
output$rfl_fpts_abv_avg <- plotly::renderPlotly({
  plot_data <- rfl_matchups_history %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    #dplyr::filter(season == 2025) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::summarise(
      franchise_name = dplyr::last(franchise_name),
      avg_franchise_fpts_diff = sum(franchise_points_ppg_diff, na.rm = TRUE) / 2,
      avg_opponent_fpts_diff = sum(opponent_points_ppg_diff, na.rm = TRUE),
      .groups = "drop"
    )

  plot <- ggplot2::ggplot(plot_data, aes(x = avg_franchise_fpts_diff, y = avg_opponent_fpts_diff)) +
    plot_quadrants(
      xmin = min(plot_data$avg_franchise_fpts_diff),
      xmax = max(plot_data$avg_franchise_fpts_diff),
      xmean = mean(plot_data$avg_franchise_fpts_diff),
      ymin = min(plot_data$avg_opponent_fpts_diff),
      ymax = max(plot_data$avg_opponent_fpts_diff),
      ymean = mean(plot_data$avg_opponent_fpts_diff),
      ltl = "Eigene Starter unterperformen,\nGegnerische Starter überperformen",
      ltr = "Eigene Starter überperformen,\nGegnerische Starter überperformen",
      lbr = "Eigene Starter überperformen,\nGegnerische Starter unterperformen",
      lbl = "Eigene Starter unterperformen,\nGegnerische Starter unterperformen",
      col_tl = color_red,
      col_tr = color_yellow,
      col_br = color_blue,
      col_bl = color_yellow
    ) +
    ggplot2::geom_point(ggplot2::aes(text = franchise_name), color = color_grey_mid, alpha = 0.8, size = 4) +
    ggplot2::geom_point(data = subset(plot_data, franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name), size = 6) +
    ggplot2::scale_color_discrete(type = colors) +
    plot_defaults +
    ggplot2::labs(
      title = "Fantasy Points Above Average: Team vs. Gegner",
      x = "Team FPts - PPG",
      y = "Gegner FPts - PPG",
      color = ""
    ) +
    ggplot2::scale_x_continuous(limits = c(min(plot_data$avg_franchise_fpts_diff) - 20, max(plot_data$avg_franchise_fpts_diff) + 20), expand = c(0, 0))

  plotly::ggplotly(plot, tooltip = c("text")) %>%
    plotly::layout(
      legend = list(orientation = "h", x = 0.5, y = 1.05, xanchor = "center", font = list(size = 11))
    )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)
