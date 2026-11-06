source("R new/matchups/matchup-data.R", local = TRUE)
source("R new/rankings/ranking_tables.R", local = TRUE)

shiny::observe({
  req(rfl_matchups())
  req(nrow(rfl_matchups()) > 0) # Prüfen, ob Daten vorhanden sind

  shinyWidgets::updatePickerInput(
    session,
    "selectRflMatchup",
    choices = unique(rfl_matchups()$matchup),
    selected = NULL,
    options = list("max-options" = 36)
  )
})

rfl_matchup_overview <- shiny::reactive({
  req(rfl_matchups())
  req(matchup_projection_table_data())

  rfl_matchup_overview <- rfl_matchups() %>%
    #dplyr::select(-matchup) %>%

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
        dplyr::mutate(loss_total = (week * 2) - wins_season) %>%
        dplyr::select(franchise_id, div_rank, wins_season, loss_total),
      by = "franchise_id"
    ) %>%
    #select(-division:-conference_name) %>%
    dplyr::group_by(division) %>%
    dplyr::arrange(dplyr::desc(div_rank)) %>%
    dplyr::mutate(matchup_rank = dplyr::row_number()) %>%
    dplyr::ungroup()

  # TODO: DB Tabelle erstellen bzw. zu daten hinzufügen

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
      ggplot2::aes(label = paste(franchise_name, paste0("(", wins_season, "-", loss_total, ")"))),
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
      title = paste("RFL Matchup Übersicht - Week", input$selectWeek),
      subtitle = "Angezeigt werden die Teams mit ihren derzeitigen Punkten (fett) und den Projected Points. Der grüne Balken zeigt die Gewinnwahrscheinlichkeit des linken Teams an.\nDie Berechnung erfolgt dabei auf Grundlage der Projected Points und der tatsächlich erzielten Punkte der noch spielenden NFL Spieler in der Vergangenheit.",
      x = "",
      y = ""
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank()
    )
}, height = 2600)

# preview ----
output$matchupTable <- reactable::renderReactable({
  data_raw <- rfl_matchups() %>%
    dplyr::filter(
      if (isTruthy(input$selectRflMatchup))
        matchup %in% input$selectRflMatchup
      else
        TRUE
    ) %>%
    dplyr::left_join(
      rfl_current_standing %>%
        dplyr::select(-franchise_name),
      by = "franchise_id"
    ) %>%
    dplyr::mutate(division_name = paste(conference_name, division_name, sep = " - ")) %>%
    dplyr::select(-conference_name)

  if (input$selectWeek == current_week) {
    data <- data_raw %>%
      dplyr::group_by(matchup) %>%
      dplyr::mutate(
        elo_sum = paste("Total ELO:", sum(elo_post)),
        matchup = paste(matchup, elo_sum, sep = " - ")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(elo_sum)) %>%
      dplyr::select(matchup, franchise_name, division_name, dplyr::ends_with("_season"), wins_over_exp, wins_over_exp_pct, dplyr::ends_with("season_rank"), dplyr::ends_with("season_pctl"), elo_post, true_standing_pctl, power_rank, power_rank_emoji, bowl_emoji, seed_emoji)

    reactable_standing(
      data,
      hide_cols = names(data)[
        stringr::str_detect(names(data), "(_season|_rank)$") |
          names(data) %in% c("division_name", "wins_over_exp", "luck_season_pctl", "quality_season_pctl", "seed_emoji", "elo_post")
      ],
      groupBy = "matchup",
      height = 1000,
      virtual = TRUE,
      pagination = FALSE
    )
  } else {
    data <- data_raw %>%
      dplyr::select(matchup, franchise_id, opponent_id, division_name) %>%
      dplyr::left_join(
        rfl_matchups_history %>%
          dplyr::filter(season == input$selectYear & week == input$selectWeek),
        by = c("franchise_id", "opponent_id")
      ) %>%
      dplyr::select(matchup, franchise_name, division_name, franchise_score, pp, eff, all_play_wins, franchise_ppg_score, franchise_points_ppg_diff, franchise_elo_pregame, elo_shift, total_points, score_diff, score_diff_pct, total_elo, quality, luck, dplyr::ends_with("_pctl")) %>%
      dplyr::arrange(abs(score_diff)) %>%
      dplyr::group_by(matchup)

    hide_cols <- names(data)[
        names(data) %in% c("franchise_id", "division_name", "franchise_score", "pp", "eff", "franchise_ppg_score", "franchise_points_ppg_diff", "all_play_wins", "franchise_elo_pregame", "elo_shift", "total_points", "total_elo", "score_diff", "score_diff_pct", "quality", "luck")
    ]

    reactable_default(
      data,
        columns = c(
          list(
          matchup = reactable::colDef(name = "Matchup"),
          franchise_name = reactable_col_franchise_name(
            data = data,
            sticky = "left",
            style = list(borderRight = paste("2px solid", color_grey_light)),
            width = 250
          ),
          franchise_score_pctl = reactable_coldef_bg_pctl(
            name = "FPts",
            value_col = "franchise_score",
            rank = "pctl",
            data = data
          ),
          pp_pctl = reactable_coldef_bg_pctl(
            name = "PP",
            value_col = "pp",
            rank = "pctl",
            data = data
          ),
          eff_pctl = reactable_coldef_bg_pctl(
            name = "Eff",
            value_col = "eff",
            rank = "pctl",
            data = data
          ),
          franchise_ppg_score_pctl = reactable_coldef_bg_pctl(
            header = reactable_header_with_tooltip("PPG", "Summe aller PPG der gesatarten Spieler im jeweiligen Matchup"),
            value_col = "franchise_ppg_score",
            rank = "pctl",
            data = data
          ),
          franchise_points_ppg_diff_pctl = reactable_coldef_bg_pctl(
            header = reactable_header_with_tooltip("FPtsOAvg", "PPG - FPts, zeigt, ob das Team im Matchup Over- (positiv) oder Unterachieved (negativ) hat im Vergleich zu den PPG der gestarteten Spieler"),
            value_col = "franchise_points_ppg_diff",
            rank = "pctl",
            data = data
          ),
          all_play_wins_pctl = reactable_coldef_bg_pctl(
            name = "All-Play Wins",
            value_col = "all_play_wins",
            rank = "pctl",
            data = data,
          ),
          franchise_elo_pregame_pctl = reactable_coldef_bg_pctl(
            name = "Pregame",
            value_col = "franchise_elo_pregame",
            rank = "pctl",
            data = data
          ),
          elo_shift_pctl = reactable_coldef_bg_pctl(
            name = "+/-",
            value_col = "elo_shift",
            rank = "pctl",
            data = data
          ),
          total_points_pctl = reactable_coldef_bg_pctl(
            name = "FPts",
            value_col = "total_points",
            rank = "pctl",
            data = data
          ),
          total_elo_pctl = reactable_coldef_bg_pctl(
            name = "ELO",
            value_col = "total_elo",
            rank = "pctl",
            data = data
          ),
          score_diff_pct_pctl = reactable_coldef_bg_pctl(
            name = "Diff",
            value_col = "score_diff",
            rank = "pctl",
            data = data
          ),
          score_diff_pctl = reactable_coldef_bg_pctl(
            name = "Diff %",
            value_col = "score_diff_pct",
            rank = "pctl",
            data = data
          ),
          quality_pctl = reactable_coldef_bg_pctl(
            name = "Qualität",
            value_col = "quality",
            rank = "pctl",
            data = data
          ),
          luck_pctl = reactable_coldef_bg_pctl(
            name = "Glück",
            value_col = "luck",
            rank = "pctl",
            data = data
          )
        ),
        purrr::set_names(
          purrr::map(
            hide_cols,
            ~ reactable::colDef(show = FALSE)
          ),
          hide_cols
        )
      ),
      columnGroups = list(
        colGroup(name = "Punkte", columns = c("franchise_score_pctl", "pp_pctl", "eff_pctl", "franchise_ppg_score_pctl", "franchise_points_ppg_diff_pctl")),
        colGroup(name = "ELO", columns = c("franchise_elo_pregame_pctl", "elo_shift_pctl")),
        colGroup(name = "Matchup Totals", columns = c("total_points_pctl", "total_elo_pctl", "score_diff_pctl", "score_diff_pct_pctl", "quality_pctl", "luck_pctl"))
      ),
      defaultSorted = c("score_diff"),
      groupBy = "matchup",
      defaultExpanded = TRUE,
      borderless = TRUE,
      height = 1000,
      virtual = TRUE,
      pagination = FALSE
    )
  }
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ui output ----
output$matchup_overview <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Matchups Woche", input$selectWeek, input$selectYear)),
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          shinycssloaders::withSpinner(reactable::reactableOutput("matchupTable")),
          width = 12
        )
      ),
      shiny::fluidRow(
        shiny::column(
          shinycssloaders::withSpinner(shiny::plotOutput("matchupOverview")),
          width = 12
        ),
        style = "height: 2600px"
      )
    )
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)
