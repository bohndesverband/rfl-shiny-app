rfl_draft_boards <- shiny::reactive({
  rfl_draft_boards <- rfl_drafts_data %>%
    dplyr::left_join(
      rfl_player_scores %>%
        dplyr::group_by(player_id) %>%
        dplyr::summarise(
          latest_player_name = dplyr::last(player_name),
          fpts = sum(points, na.rm = TRUE),
          .groups = "drop"
        ),
      by = c("mfl_id" = "player_id")
    ) %>%
    dplyr::filter(
      season == input$selectYear
      #season == 2018
    ) %>%
    dplyr::filter(
      if(isTruthy(input$showOnlyRookies))
        is_rookie == 1
      else
        TRUE
    ) %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        pos_grouped %in% input$selectPositions
      else
        TRUE
    ) %>%
    dplyr::mutate(fpts_max = max(fpts, na.rm = TRUE)) %>%
    dplyr::group_by(pos_grouped) %>%
    dplyr::mutate(fpts = ifelse(is.na(fpts), 0, fpts)) %>%
    dplyr::arrange(dplyr::desc(fpts)) %>%
    dplyr::mutate(
      fpts_max_pos = max(fpts, na.rm = TRUE),
      max_bar_legth = fpts_max_pos / fpts_max,
      fpts_pct = fpts / fpts_max_pos,
      fpts_pct = ifelse(fpts_pct > 0, fpts_pct, 0),
      fpts_rank = dplyr::dense_rank(dplyr::desc(fpts)),
      latest_player_name = ifelse(is.na(latest_player_name), player_name, latest_player_name)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      #normalize bar_length to max 0.6 and min 0.2
      max_bar_legth = 0.2 + (max_bar_legth * 0.4)
    )
    #dplyr::filter(
    #  if(isTruthy(input$selectRflTeams))
    #    franchise_id %in% input$selectRflTeams
    #  else
    #    TRUE
    #)

  # nach punkten sortieren
  if (input$sortForFpts == TRUE) {
    rfl_draft_boards <- rfl_draft_boards %>%
      dplyr::arrange(dplyr::desc(fpts)) %>%
      dplyr::mutate(
        overall = dplyr::row_number(),
        round = ceiling(overall / 36)
      ) %>%
      dplyr::group_by(round) %>%
      dplyr::mutate(
        pick = dplyr::row_number()
      )
  }

  rfl_draft_boards
})

# TODO: alte teamnamen

output$draft_board <- shiny::renderPlot({
  ggplot2::ggplot(rfl_draft_boards(), ggplot2::aes(x = round, y = pick)) +
    ggplot2::geom_tile(ggplot2::aes(fill = pos_grouped), color = color_bg, size = 0.5, alpha = 0.2) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("#", overall, ") ", latest_player_name)),
      size = 4.5, fontface = "bold", nudge_y = 0.2
    ) +

    ggplot2::geom_text(
      ggplot2::aes(label = franchise_name),
      size = 3, fontface = "bold", nudge_y = -0.3
    ) +

    ggchicklet:::geom_rrect(
      ggplot2::aes(xmin = round - 0.4, xmax = round - 0.4 + 0.6,
                   ymin = pick + 0.15, ymax = pick),
      fill = NA, color = color_black, linetype = "dashed", size = 0.3, r = unit(0.5, "npc")
    ) +

    ggchicklet:::geom_rrect(
      ggplot2::aes(xmin = round - 0.4, xmax = round - 0.4 + max_bar_legth,
                   ymin = pick + 0.15, ymax = pick),
      fill = color_bg, color = color_black, size = 0.5, r = unit(0.5, "npc")
    ) +

    ggchicklet:::geom_rrect(
      ggplot2::aes(fill = pos_grouped, xmin = round - 0.4, xmax = round - 0.4 + max_bar_legth * fpts_pct,
                  ymin = pick + 0.14, ymax = pick + 0.01),
      r = unit(0.5, "npc")
    ) +

    ggplot2::geom_text(
      ggplot2::aes(label = paste(pos_grouped, paste0("#", fpts_rank, "\n", round(fpts, 0), " FPts"))),
      size = 3, fontface = "bold", hjust = 0, nudge_x = 0.25, nudge_y = -0.06, lineheight = 0.9
    ) +

    ggplot2::geom_tile(data = subset(rfl_draft_boards(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name), fill = NA, size = 2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors_positions_grouped, guide = "none") +
    ggplot2::scale_y_reverse(limits = c(36.5, 0.5), breaks = c(1:36), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(rfl_draft_boards()$round) + 0.5), breaks = c(1:max(rfl_draft_boards()$round)), expand = c(0, 0)) +
    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = paste("RFL Draft Board", input$selectYear),
      subtitle = "Angezeigt werden alle Picks des Drafts mit ihren Total Fantasy Points (FPts) seit dem Draft.\nDie länge der weißen Balken zeigt die FPts im Verhältnis zum besten Spieler der Klasse, die bunten das Verhältnis zur Positionsgruppe.",
      x = "Runde",
      y = "Pick",
      color = ""
    )
}, height = 2600)

