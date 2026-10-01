# depth chart ----
## load data ----
rfl_depth_chart_data <- read_data_table("rfl_depth_chart_data")

## filter data ----
depth_chart_team <- shiny::reactive({
  depth_chart_team <- rfl_depth_chart_data %>%
    #dplyr::filter(season == 2026 & franchise_id == "0036")
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeam)
})

## output ----
output$team_depth_chart <- ggiraph::renderGirafe({
  data <- depth_chart_team() %>%
    dplyr::filter(!is.na(coord_v) | !is.na(coord_h))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = coord_h, y = coord_v)) +
    ggplot2::geom_hline(yintercept = 0, color = color_yellow, linewidth = 1) + # LOS
    ggplot2::scale_x_continuous(limits = c(1, 15), expand = c(0, 0), breaks = seq(1, 15, by = 1)) +
    ggplot2::scale_y_continuous(limits = c(-6, 6)) +

    ggplot2::geom_label(ggplot2::aes(label = depth_chart), fill = color_grey_light, hjust = 0.5, vjust = -0.5, linewidth = 0, size = 3.5, label.padding = ggplot2::unit(2, "mm"), label.r = ggplot2::unit(0, "mm"), na.rm = TRUE) +
    ggiraph::geom_label_interactive(ggplot2::aes(label = display_name, fill = pos_grouped, tooltip = pos_players), hjust = 0.5, linewidth = 0, size = 4.5, label.padding = ggplot2::unit(2, "mm"), label.r = ggplot2::unit(0, "mm"), na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = colors_positions_grouped, guide = "none") +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_label(ggplot2::aes(label = paste0("#", war_rank_league, " (", war, " WAR)"), fill = war_rank_league), hjust = 0.5, vjust = 1.52, linewidth = 0, size = 3.5, label.padding = ggplot2::unit(2, "mm"), label.r = ggplot2::unit(0, "mm"), na.rm = TRUE) +
    ggplot2::scale_fill_gradientn(colors = c(color_blue, color_green, color_yellow, color_orange, color_red), limits = c(1, 12), na.value = color_red, guide = "none") +

    plot_defaults +
    ggplot2::theme(
      #panel.background = ggplot2::element_rect(fill = color_grey_dark),
      axis.text = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste(selected_team_name(), "Depth Chart", input$selectYear),
      subtitle = "Angezeigt werden die Top-Spieler des Rosters auf ihren Positionen nach Wins above Replacement (WAR).",
      x = "",
      y = ""
    )

  girafe_default_output(plot)
})

# roster depth ----
# depth ----
output$team_roster <- gt::render_gt({
  data <- rfl_roster_data %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeam) %>%
    #dplyr::filter(franchise_id == "0007" & season == 2026) %>%
    dplyr::filter(week == max(week)) %>%
    #filter(player_id == "14221") %>%
    dplyr::select(franchise_name, pos_grouped, display_name, player_pos_team, war, player_elo_post, age, transaction_emoji, war_pctl_season, player_elo_post_pctl) %>%
    dplyr::mutate(
      dplyr::across(
        c(war_pctl_season, player_elo_post_pctl),
        ~ dplyr::coalesce(.x, 0)
      )
    ) %>%
    dplyr::group_by(franchise_name, pos_grouped) %>%
    dplyr::arrange(factor(pos_grouped, levels = positions_grouped), dplyr::desc(war), dplyr::desc(player_elo_post)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste(paste(selected_team_name(), collapse = ", "), "Depth Chart", input$selectYear)
    ) %>%
    gtExtras::gt_merge_stack(
      display_name,
      player_pos_team,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      font_weight = c("normal", "normal")
    ) %>%
    gt_pctl_bar("war", "war_pctl_season") %>%
    gt_pctl_bar("player_elo_post", "player_elo_post_pctl") %>%
    gt::data_color(
      age,
      palette = c(color_bg, color_red),
      domain = c(20, 40)
    ) %>%
    gt::cols_label(
      display_name = "Spieler",
      age = "Alter",
      war = "WAR",
      player_elo_post = "ELO",
      transaction_emoji = ""
    ) %>%
    gtDefaults() %>%
    gt::cols_align(
      align = "center",
      columns = gt::everything()
    ) %>%
    gt::cols_align(
      align = "left",
      columns = c(display_name)
    )
})
