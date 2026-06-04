# filter data ----
rfl_drafts_rookies_filtered <- shiny::reactive({
  rfl_drafts_rookies %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
    )
})

# nach nfl rounds ----
output$rfl_picks_over_nfl_rounds <- shiny::renderPlot({
  rfl_drafts_rookies %>%
    dplyr::rename(rfl_round = round) %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2], pos != "PK") %>%
    dplyr::left_join(
      nfl_drafts_data %>%
        dplyr::filter(!is.na(gsis_id)),
      by = "gsis_id"
    ) %>%
    # anpassung draftrounds
    dplyr::mutate(
      nfl_round = dplyr::case_when(
        is.na(gsis_id) & player_name == "Carlos Henderson" ~ 3,
        is.na(gsis_id) & player_name == "Logan Hall" ~ 2,
        is.na(gsis_id) & player_name == "Jartavius Martin" ~ 2,
        is.na(nfl_round) ~ 8,
        TRUE ~ nfl_round
      )
    ) %>%
    dplyr::group_by(pos, rfl_round, nfl_round) %>%
    dplyr::summarise(
      count = n(),
      .groups = "drop"
    ) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = nfl_round + 0.5, y = rfl_round + 0.5, fill = factor(pos, levels = positions_full))) +
    ggplot2::facet_wrap(~factor(pos, levels = positions_full), ncol = 3) +
    ggplot2::geom_tile(mapping = ggplot2::aes(alpha = count)) +

    ggplot2::scale_x_continuous(limits = c(1, 9), breaks = seq(1, 8, by = 1), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(8, 1), breaks = seq(7, 1, by = -1), expand = c(0, 0)) +
    ggplot2::scale_alpha(range = c(0.1, 1), guide = "none") +
    ggplot2::scale_fill_manual(values = colors_position, guide = "none") +
    plot_defaults +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(hjust = -3),
      axis.text.y = ggplot2::element_text(vjust = 2),
    ) +
    ggplot2::labs(
      title = "RFL Draftpicks nach NFL Draft-Runde",
      subtitle = "In welcher Runde wurden die im RFL Draft gepickten Spieler im NFL Draft gewählt?",
      x = "NFL Draft Runde",
      y = "RFL Draft Runde"
    )
}, height = 700)

# VOE ----
output$draft_pos_voe <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(rfl_drafts_rookies_filtered(), ggplot2::aes(x = voe, y = factor(pos_grouped, levels = positions_grouped))) +
    ggplot2::facet_wrap(~ round, ncol = 2) +
    ggplot2::geom_vline(xintercept = 0, color = color_grey_mid) +
    ggiraph::geom_jitter_interactive(
      ggplot2::aes(
        tooltip = paste(
          player_name, paste0("(", pos_grouped, ", ", team, ")"),
          "\n", franchise_name,
          "\nPick: ", paste0(round, ".", pick, " (#", overall, ")"),
          "\nVOE:", voe),
        data_id = mfl_id,
        size = pvar_exp,
        color = voe
      ),
      alpha = 0.8
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::scale_colour_gradientn(colors = c(color_red, color_orange, color_green, color_blue), rescaler = ~ scales::rescale_mid(.x, mid = 0), guide = FALSE) +
    ggplot2::scale_size(guide = FALSE) +
    plot_defaults +
    ggplot2::labs(
      title = "Draft Investment Returns nach Runde",
      subtitle = "Jeder Punkt ist ein Draftpick. Die Größe spiegelt den pVARexp wieder (je größer der Punkt, desto wertvoller war der eingesetzte Pick).",
      x = "VOE"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank()
    )

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 14) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})
