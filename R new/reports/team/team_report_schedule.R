# ranks ----
output$team_pctl <- ggiraph::renderGirafe({
  order <- c("elo", "war_starter", "pf", "pp", "pa", "eff", "wins", "all_play_wins", "wins_expected", "luck", "quality", "true_standing")
  names <- c("ELO", "WAR", "Points For", "Potential Points", "Points Against", "Effizienz", "H2H Siege", "All-Play Siege", "Erwartete Siege", "Glück", "Qualität", "Power Rank")

  plot_data <- rfl_standing_data %>%
    dplyr::group_by(season) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(elo_season = elo_post, true_standing_season = true_standing) %>%
    # TODO: team avg zu data hinzufügen
    dplyr::group_by(franchise_id) %>%
    dplyr::mutate(
      dplyr::across(
        c(dplyr::ends_with("_season")),
        ~ round(mean(.x, na.rm = TRUE), 2),
        .names = "{.col}_avg"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dplyr::across(
        c(dplyr::ends_with("_avg")),
        ~ round(dplyr::percent_rank(.x), 2),
        .names = "{.col}_pctl"
      ),
      true_standing_season_avg_pctl = round(dplyr::percent_rank(dplyr::desc(true_standing_season_avg)), 2),
    ) %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeam) %>%
    #dplyr::filter(season == 2026 & franchise_id == "0007") %>%
    dplyr::select(dplyr::ends_with("season_pctl"), dplyr::ends_with("avg_pctl"), dplyr::ends_with("avg"), dplyr::ends_with("_season_rank"), power_rank) %>%
    tidyr::pivot_longer(-power_rank, values_to = "season") %>%
    dplyr::mutate(
      time = dplyr::case_when(
        stringr::str_detect(name, "avg_pctl") ~ "avg",
        stringr::str_detect(name, "avg") ~ "value",
        stringr::str_detect(name, "rank") ~ "rank",
        stringr::str_detect(name, "season") ~ "season",
      ),
      name = stringr::str_remove(name, "_season.*$")
    ) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(
      avg = dplyr::if_else(
        time == "season",
        season[time == "avg"][1],
        NA_real_
      ),
      avg_value = dplyr::if_else(
        time == "season",
        season[time == "value"][1],
        NA_real_
      ),
      rank_season = dplyr::if_else(
        time == "season",
        season[time == "rank"][1],
        NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(time == "season") %>%
    dplyr::mutate(
      rank_season = ifelse(name == "true_standing", power_rank, rank_season),
      name = factor(
        name,
        levels = order,
        labels = names
      )
    )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = name, y = season, color = season)) +
    ggplot2::geom_hline(yintercept = 0.25, color = color_red, alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0.50, color = color_yellow, alpha = 0.4) +
    ggplot2::geom_hline(yintercept = 0.75, color = color_green, alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 1, color = color_blue, alpha = 0.2) +

    ggforce::geom_link(data = subset(plot_data, time == "season"), aes(x = name, xend = name, y = avg, yend = season, linewidth = ggplot2::after_stat(index))) +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::scale_linewidth_continuous(guide = "none") +
    ggiraph::geom_point_interactive(
      data = subset(plot_data, time == "season"),
      ggplot2::aes(
        tooltip = paste0(
          "<strong>", name, "</strong>\n",
          "Platz ", rank_season
        )
      ),
      size = 4.7
    ) +
    plot_defaults +
    ggplot2::scale_color_gradientn(colors = colors_rainbow, guide = "none") +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = function(x) {ifelse(x == 0, "", scales::percent(x, accuracy = 1))}) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
    ) +
    ggplot2::labs(
      x = "",
      y = ""
    )

  girafe_default_output(
    plot,
  )
})




output$team_schedule <- ggiraph::renderGirafe({
  data <- rfl_schedule_data %>%
    dplyr::group_by(season, week, franchise_id) %>%
    dplyr::mutate(
      nudge_x = ifelse(dplyr::row_number() == 1, 0.2, -0.2),
      result = dplyr::case_when(
        win == 1 ~ "W",
        win == 0 ~ "L",
        TRUE ~ NA
      ),
      label = paste0("WK ", week, " vs. ", opponent_name),
      label = ifelse(!is.na(result), paste0(label, "\n", result, " ", franchise_score, " vs. ", opponent_score), label)
    ) %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeam)
    #dplyr::filter(season == 2025 & franchise_id == "0007")

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = week + nudge_x, y = opponent_elo_diff)) +
    ggplot2::geom_line(ggplot2::aes(y = franchise_elo_diff), color = color_grey_mid, linewidth = 1) +
    ggplot2::geom_col(
      ggplot2::aes(
        fill = result
      ),
      width = 0.2
    ) +
    ggplot2::geom_hline(yintercept = 0, color = color_grey_light, size = 2) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = label,
        color = result,
        shape = matchup
      ),
      size = 12,
      fill = color_bg,
      stroke = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = opponent_abbrev),
      hjust = 0.5,
      size = 3
    ) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(data$week) + 0.5), breaks = seq(1, max(data$week), by = 1)) +
    ggplot2::scale_color_manual(values = c("W" = color_green, "L" = color_red), na.value = color_grey_mid, guide = "none") +
    ggplot2::scale_fill_manual(values = c("W" = color_green, "L" = color_red), na.value = color_grey_mid, guide = "none") +
    ggplot2::scale_shape_manual(values = c("Div" = 23, "Conf" = 21, "Zufällig" = 22), labels = c("Div" = "Division", "Conf" = "Conference", "Zufällig")) +
    plot_defaults +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste(selected_team_name(), "schedule", input$selectYear),
      subtitle = "Dargestellt werden alle Matchups der regular Season und die Differenz der Gegner-ELO zum Durchschnitt der Liga (ELO Δ).\nDie Linie ist die eigene ELO Δ.",
      x = "Woche",
      y = "ELO Δ",
      shape = ""
    )

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 9)
})

# TODO: Postseason Matchups ergänzen
