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
