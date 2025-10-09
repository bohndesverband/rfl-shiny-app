source("R new/matchups/matchup-data.R", local = TRUE)
source("R new/rankings/ranking_tables.R", local = TRUE)

shiny::observe({
  req(rfl_matchups())
  req(nrow(rfl_matchups()) > 0) # Prüfen, ob Daten vorhanden sind

  shinyWidgets::updatePickerInput(
    session,
    "selectRflMatchup",
    choices = rfl_matchups()$matchup,
    selected = NULL,
    options = list("max-options" = 36)
  )
})

rfl_matchup_overview <- shiny::reactive({
  req(rfl_matchups())
  req(matchup_projection_table_data())

  rfl_matchup_overview <- rfl_matchups() %>%
    #dplyr::select(-matchup) %>%
    dplyr::rename(franchise_id = home, franchise_name = home_name, opponent_id = away, opponent_name = away_name) %>%

    # dupliziere jede zeile und tausche team und opponent
    dplyr::bind_rows(
      rfl_matchups() %>%
        dplyr::select(-matchup) %>%
        dplyr::rename(franchise_id = away, franchise_name = away_name, opponent_id = home, opponent_name = home_name)
    ) %>%

    # füge team infos hinzu
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, division, conference_id, division_name, conference_name),
      by = "franchise_id"
    ) %>%

    # füge franchise scoring hinzu
    dplyr::left_join(
      matchup_projection_table_data() %>%
        dplyr::mutate(
          franchise_variance = ifelse(time_remaining > 0, sd, 0)
        ) %>%
        dplyr::group_by(franchise_name) %>%
        dplyr::summarise(
          franchise_points_left = sum(ifelse(time_remaining > 0, projected, 0), na.rm = TRUE),
          dplyr::across(c(projected, live, franchise_variance), \(x) sum(x, na.rm = TRUE)),
          franchise_left_to_play = sum(time_remaining > 0, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        #dplyr::mutate(
        #  franchise_variance = sqrt(franchise_points_left) * 4
        #) %>%
        dplyr::rename(franchise_projected = projected, franchise_live = live),
      by = "franchise_name"
    ) %>%

    # füge opponent scoring hinzu
    dplyr::left_join(
      matchup_projection_table_data() %>%
        dplyr::mutate(
          opponent_variance = ifelse(time_remaining > 0, sd, 0)
        ) %>%
        dplyr::group_by(franchise_name) %>%
        dplyr::summarise(
          opponent_points_left = sum(ifelse(time_remaining > 0, projected, 0), na.rm = TRUE),
          dplyr::across(c(projected, live, opponent_variance), \(x) sum(x, na.rm = TRUE)),
          opponent_left_to_play = sum(time_remaining > 0, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        #dplyr::mutate(
        #  opponent_variance = sqrt(opponent_left_to_play) * 4
        #) %>%
        dplyr::rename(opponent_projected = projected, opponent_live = live),
      by = c("opponent_name" = "franchise_name")
    ) %>%

    # berechnung
    dplyr::mutate(
      franchise_projected = franchise_live + franchise_points_left,
      franchise_rest_score = franchise_projected - franchise_live,
      opponent_projected = opponent_live + opponent_points_left,
      opponent_rest_score = opponent_projected - opponent_live,
      difference = (franchise_live + franchise_rest_score) - (opponent_live + opponent_rest_score),
      franchise_variance = ifelse(franchise_left_to_play == 0, 0, franchise_variance),
      opponent_variance = ifelse(opponent_left_to_play == 0, 0, opponent_variance),
      total_variance = franchise_variance + opponent_variance,
      win_prob = round(pnorm(difference / total_variance), 2),
      win_prob_end = 0.5 + (win_prob * (1.5 - 0.5))
    ) %>%
    dplyr::left_join(
      rfl_standing_data %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::filter(
          week == ifelse(input$selectWeek > max(week), max(week), input$selectWeek)
        ) %>%
        dplyr::mutate(loss_total = (week * 2) - wins_total) %>%
        dplyr::select(franchise_id, div_rank, wins_total, loss_total),
      by = "franchise_id"
    ) %>%
    #select(-division:-conference_name) %>%
    dplyr::group_by(division) %>%
    dplyr::arrange(dplyr::desc(div_rank)) %>%
    dplyr::mutate(matchup_rank = dplyr::row_number()) %>%
    dplyr::ungroup()

    #dplyr::select(franchise_id:conference_name, franchise_projected, opponent_projected, franchise_left_to_play, opponent_left_to_play, franchise_live, opponent_live, franchise_points_left, opponent_points_left, win_prob) %>%
    #filter(division == "00")
})

output$matchupOverview <- shiny::renderPlot({
  req(rfl_matchup_overview())

  ggplot2::ggplot(rfl_matchup_overview(), ggplot2::aes(x = 1, y = matchup_rank)) +
    ggplot2::facet_wrap(~ division_name, ncol = 2) +
    ggplot2::geom_tile(fill = color_grey_light, color = color_bg, linewidth = 0.5, width = 1) +

    # balken füllung
    ggplot2::geom_rect(
      ggplot2::aes(xmin = 0.5, xmax = win_prob_end,
                   ymin = matchup_rank - 0.5, ymax = matchup_rank + 0.5),
      fill = color_green,
      color = color_bg,
      linewidth = 0.5
    ) +

    # franchise name
    ggplot2::geom_text(
      ggplot2::aes(label = paste(franchise_name, paste0("(", wins_total, "-", loss_total, ")"))),
      x = 0.51,
      size = 6, hjust = 0, nudge_y = 0.15, fontface = "bold", color = color_black
    ) +

    # franchise players left
    ggplot2::geom_text(
      ggplot2::aes(label = paste(franchise_left_to_play, "Spieler übrig")),
      x = 0.51,
      size = 5, hjust = 0, nudge_y = -0.2, color = color_black
    ) +

    # franchise score
    ggplot2::geom_text(
      ggplot2::aes(label = franchise_live),
      x = 0.99,
      size = 6, hjust = 1, nudge_y = 0.15, fontface = "bold", color = color_black
    ) +

    # franchise projeted
    ggplot2::geom_text(
      ggplot2::aes(label = franchise_projected),
      x = 0.99,
      size = 5, hjust = 1, nudge_y = -0.2, color = color_black
    ) +

    # opponent name
    ggplot2::geom_text(
      ggplot2::aes(label = opponent_name),
      x = 1.49,
      size = 6, hjust = 1, nudge_y = 0.15, fontface = "bold", color = color_black
    ) +

    # opponent players left
    ggplot2::geom_text(
      ggplot2::aes(label = paste(opponent_left_to_play, "Spieler übrig")),
      x = 1.49,
      size = 5, hjust = 1, nudge_y = -0.2, color = color_black
    ) +

    # opponent score
    ggplot2::geom_text(
      ggplot2::aes(label = opponent_live),
      x = 1.01,
      size = 6, hjust = 0, nudge_y = 0.15, fontface = "bold", color = color_black
    ) +

    # opponent projeted
    ggplot2::geom_text(
      ggplot2::aes(label = opponent_projected),
      x = 1.01,
      size = 5, hjust = 0, nudge_y = -0.2, color = color_black
    ) +

    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +

    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = paste("RFL Matchup Übersicht - Week", current_week_thu),
      subtitle = "Angezeigt werden die Teams mit ihren derzeitigen Punkten (fett) und den Projected Points. Der grüne Balken zeigt die Gewinnwahrscheinlichkeit des linken Teams an.\nDie Berechnung erfolgt dabei auf Grundlage der Projected Points und der tatsächlich erzielten Punkte der noch spielenden NFL Spieler in der Vergangenheit.",
      x = "",
      y = ""
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank()
    )
}, height = 2600)

# preview ----
output$matchupPreview <- gt::render_gt({
  dplyr::bind_rows(
    rfl_matchups(),
    rfl_matchups() %>%
      dplyr::rename(
        home = away,
        home_name = away_name,
        away = home,
        away_name = home_name
      )
  ) %>%
    dplyr::filter(
      if (isTruthy(input$selectRflMatchup))
        matchup %in% input$selectRflMatchup
      else
        TRUE
    ) %>%
    dplyr::select(-dplyr::starts_with("away")) %>%
    dplyr::rename(franchise_id = home, franchise_name = home_name) %>%
    dplyr::left_join(
      rfl_current_standing %>%
        dplyr::select(-week, -franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::mutate(division_name = paste(conference_name, division_name, sep = " - ")) %>%
    dplyr::select(-conference_name) %>%
    dplyr::group_by(matchup) %>%
    dplyr::mutate(
      elo_sum = paste("Total ELO:", sum(franchise_elo_postgame))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(elo_sum)) %>%
    dplyr::group_by(matchup, elo_sum) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("RFL Matchups Woche", current_week_thu)
    ) %>%
    ranking_table_base() %>%
    ranking_table_standing() %>%
    ranking_table_elo() %>%
    ranking_table_power_rank() %>%
    ranking_table_bowl() %>%
    gtDefaults()
})

