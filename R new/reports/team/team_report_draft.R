rfl_rookie_drafts <- rfl_drafts_data %>%
  dplyr::filter(season > 2016)

selected_draft_year <- shiny::reactive({
  selected_draft_year <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear)
    #dplyr::filter(season == 2025)
})

selected_draft_team <- shiny::reactive({
  selected_draft_team <- selected_draft_year() %>%
    dplyr::filter(franchise_id == input$selectRflTeam)
    #dplyr::filter(franchise_id == "0007")
})

selected_draft_players <- shiny::reactive({
  selected_draft_players <- selected_draft_team() %>%
    dplyr::pull(mfl_id)
})

selected_draft_grades <- shiny::reactive({
  selected_draft_grades <- rfl_draft_grades %>%
    dplyr::select(-asset_name, -asset_id_new) %>%
    dplyr::filter(draft_class == input$selectYear & team_id == input$selectRflTeam) %>%
    #dplyr::filter(draft_class == 2025 & team_id == "0007") %>%
    dplyr::left_join(
      selected_draft_team() %>%
        dplyr::mutate(pick_id = paste(round, pick, sep = "_")),
      by = c("pick" = "pick_id")
    )
})

rfl_draft_classes_sum_filtered <- shiny::reactive({
  rfl_draft_classes_sum_filtered <- rfl_draft_classes_sum %>%
    dplyr::filter(franchise_id == input$selectRflTeam) %>%
    #dplyr::filter(franchise_id == "0007") %>%
    dplyr::select(franchise_name, season, picks, rank_season, pvar, voe, rank, voe_pctl)
})


# alle Draftklassen ----
## tabelle ----
output$draft_classes_team <- reactable::renderReactable({
  draft_classes_teams_reactable(
    rfl_draft_classes_sum_filtered(),
    rfl_rookie_drafts,
    columns = list(
      season = reactable::colDef(
        name = "Saison",
        minWidth = 70
      ),
      rank_season = reactable_coldef_color(
        name = "#",
        palette_fun = scale_green_red(c(1, 36)),
        footer_fun = NULL,
        minWidth = 50
      )
    ),
    column_groups = list(
      reactable::colGroup(name = paste0("2017-", new_season_march - 1), columns = c("rank", "voe_pctl")),
      reactable::colGroup(name = "VOE", columns = c("rank_season", "voe"))
    )
  )
})

## plot ----
output$draft_classes_team_chart <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(rfl_draft_classes_sum, ggplot2::aes(y = season, x = voe, color = franchise_name)) +
    plot_geom_vline(0) +
    ggplot2::geom_boxplot(ggplot2::aes(group = season), fill = color_grey_light, color = color_grey_mid, linewidth = 0.15, outliers = FALSE) +

    ggiraph::geom_jitter_interactive(
      data = subset(rfl_draft_classes_sum, !franchise_id %in% c(input$selectRflTeam)),
      ggplot2::aes(
        tooltip = paste(franchise_name, "\nVOE:", voe),
        size = picks,
        alpha = picks
      ),
      width = 0.25, color = color_grey_mid
    ) +

    ggplot2::geom_point(data = subset(rfl_draft_classes_sum, franchise_id %in% c(input$selectRflTeam)), ggplot2::aes(size = picks)) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1, 6), guide = "none") +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean+
    ggplot2::scale_y_continuous(labels = c(2017:new_season_march - 1), breaks = c(2017:new_season_march - 1)) +
    ggplot2::labs(
      title = paste0("Total VOE aller RFL Draftklassen von 2017-", new_season_march - 1),
      subtitle = "Jeder Punkt ist eine RFL Draftklasse.\nJe größer der Punkt, desto mehr Picks hatte die Draftklasse.",
      x = "Total VOE",
      y = "Draft",
      color = ""
    )

  ggiraph::girafe(ggobj = plot, width_svg = 10, height_svg = 11) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})

# einzelne Draftklasse ----
output$team_draft_class <- ggiraph::renderGirafe({
  type <- input$selectDraftClassCharts

  draft_class_plot_defaults <- list(
    ggplot2::scale_y_discrete(
      limits = rev
    ),
    plot_defaults
  )

  if (type == "ADP") {
  ## ADP ----
    data <- selected_draft_team() %>%
      dplyr::select(season, asset_name, first_pick, second_pick, third_pick, mfl_id) %>%
      tidyr::pivot_longer(c(first_pick, second_pick, third_pick), names_to = "pick", values_to = "overall") %>%
      dplyr::left_join(
        rfl_drafts_data %>%
          dplyr::select(season, overall, pick_info, pick_value, adp),
        by = c("season", "overall")
      )

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = overall, y = asset_name)) +
      draft_class_plot_defaults +
      plot_draft_defaults() +

      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste0(pick_info,
                           "\nOverall: ", overall,
                           "\nValue: ", pick_value,
                           "\nADP: ", adp
          ),
          data_id = mfl_id
        ),
        size = 5,
        color = color_grey_light
      ) +
      ggplot2::geom_point(
        data = selected_draft_team(),
        ggplot2::aes(x = adp),
        color = color_grey_mid,
        fill = color_grey_mid,
        shape = 25,
        size = 3
      ) +
      ggiraph::geom_point_interactive(
        data = selected_draft_team(),
        ggplot2::aes(
          tooltip = paste0(pick_info,
                           "\nOverall:", overall,
                           "\nValue: ", pick_value,
                           "\nADP: ", adp
          ),
          color = factor(pos_grouped, positions_grouped),
          data_id = mfl_id
        ),
        size = 8
      ) +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse mit ihren 3 Copies und dem MFL ADP (Dreieck).",
      ) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(linewidth = 1, color = color_grey_light)
      )
  } else if (type == "WAR") {
    ## WAR ----
    shiny::validate(
      shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
    )

    data <- rfl_war_data %>%
      dplyr::filter(player_id %in% selected_draft_players()) %>%
      dplyr::left_join(selected_draft_team() %>% dplyr::select(asset_name, mfl_id), by = c("player_id" = "mfl_id"))

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = war, y = asset_name, color = factor(pos, positions_grouped), alpha = season, size = season)) +
      draft_class_plot_defaults +
      plot_geom_vline(0) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(season, "\nWAR: ", war),
          data_id = player_id
        )
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 1), limits = c(input$selectYear, new_season_march - 1), guide = "none") +
      ggplot2::scale_size_continuous(range = c(4, 8), limits = c(input$selectYear, new_season_march - 1), guide = "none") +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse mit ihren Wins Above Replacement (WAR) seit dem Draft.",
        x = "WAR"
      )
  } else if (type == "ELO") {
    ## ELO ----
    shiny::validate(
      shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
    )

    data <- player_elo %>%
      dplyr::filter(mfl_id %in% selected_draft_players()) %>%
      dplyr::group_by(mfl_id, season) %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(selected_draft_team() %>% dplyr::select(asset_name, mfl_id), by = "mfl_id")

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = player_elo_post, y = asset_name, color = factor(position, positions_grouped), alpha = season, size = season)) +
      draft_class_plot_defaults +
      plot_geom_vline(1500) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(season, "\nELO: ", player_elo_post),
          data_id = mfl_id
        )
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 1), limits = c(input$selectYear, new_season_march - 1), guide = "none") +
      ggplot2::scale_size_continuous(range = c(4, 8), limits = c(input$selectYear, new_season_march - 1), guide = "none") +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse mit ihrer ELO seit dem Draft.",
        x = "ELO am Ende der Saison"
      )
  } else if (type == "VOE") {
    ## VOE ----
    shiny::validate(
      shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
    )

    plot <- ggplot2::ggplot(selected_draft_team(), ggplot2::aes(x = voe, y = asset_name, color = factor(pos_grouped, positions_grouped))) +
      draft_class_plot_defaults +
      plot_geom_vline(0) +

      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(season, "\nVOE: ", voe),
          data_id = mfl_id
        ),
        size = 8
      ) +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse und ihr Value over Expected (VOE).",
        x = "VOE"
      )

    # Liga vergleich
    #plot <- ggplot2::ggplot(selected_draft_team(), ggplot2::aes(x = overall, y = pvar, color = factor(pos_grouped, positions_grouped))) +
    #  draft_class_plot_defaults +
    #  plot_draft_defaults()

    #  ggplot2::geom_smooth(
    #    data = rfl_draft_pvar_exp,
    #    ggplot2::aes(y = pvar_exp),
    #    se = FALSE,
    #    color = color_red,
    #    size = 0.5
    #  ) +
    #  ggiraph::geom_point_interactive(
    #    data = selected_draft_year(),
    #    ggplot2::aes(
    #      tooltip = paste(
    #        player_name, paste0("(", pos_grouped, ", ", team, ")"),
    #        "\n", franchise_name,
    #        "\nPick: ", paste0(round, ".", pick, " (#", overall, ")"),
    #        "\nVOE:", voe),
    #      data_id = mfl_id
    #    ),
    #    color = color_grey_light,
    #    alpha = 0.6,
    #    size = 5,
    #    hover_nearest = TRUE
    #  ) +
    #  ggiraph::geom_point_interactive(
    #    ggplot2::aes(
    #      tooltip = paste(
    #        player_name, paste0("(", pos_grouped, ", ", team, ")"),
    #        "\n", franchise_name,
    #        "\nPick: ", paste0(round, ".", pick, " (#", overall, ")"),
    #        "\nVOE:", voe),
    #      data_id = mfl_id
    #    ),
    #    size = 7,
    #    hover_nearest = TRUE
    #  ) +
    #  ggplot2::labs(
    #    y = "pVAR"
    #  )
  } else if (type == "Bewertung") {
    ## Bewertung ----

    breaks <- seq(1, 20/3, by = 1/3)

    labels <- c(
      "1+", "1", "1-",
      "2+", "2", "2-",
      "3+", "3", "3-",
      "4+", "4", "4-",
      "5+", "5", "5-",
      "6+", "6", "6-"
    )

    plot <- ggplot2::ggplot(data = subset(selected_draft_grades(), !is.na(asset_name)), ggplot2::aes(x = grade, y = asset_name, color = user, alpha = year, size = year)) +
      draft_class_plot_defaults +
      ggplot2::geom_vline(
        data = selected_draft_grades() %>%
          dplyr::filter(
            pick == "klasse",
            year == max(year, na.rm = TRUE)
          ),
        ggplot2::aes(xintercept = grade, color = user),
        linewidth = 0.5, linetype = "dashed"
      ) +

      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(user, year, "\nNote: ", grade),
          data_id = user
        ),
      ) +
      ggplot2::scale_x_continuous(
        limits = c(1, 6.666),
        breaks = breaks,
        minor_breaks = NULL,
        labels = labels
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 1), limits = c(min(selected_draft_grades()$year), max(selected_draft_grades()$year)), guide = "none") +
      ggplot2::scale_size_continuous(range = c(4, 8), limits = c(min(selected_draft_grades()$year), max(selected_draft_grades()$year)), guide = "none") +
      ggplot2::scale_color_discrete(palette = colors) +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse und ihre Bewertungen nach dem Draft, einem, drei und fünf Jahren. Die gestrichelten Linien sind die letzten Bewertungen der gesamten Klasse.",
        x = "Bewertung",
        color = ""
      )
  }

  if (type != "Bewertung") {
    plot <- plot +
      ggplot2::scale_color_manual(values = colors_position, guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +
      ggplot2::labs(
        color = "Position",
      )
  }

  plot <- plot +
    ggplot2::labs(
      title = paste("Draftpicks", selected_team_name(), input$selectYear, "-", input$selectDraftClassCharts),
      y = ""
    ) +
    theme(
      plot.margin = ggplot2::margin(rem_to_pt(2), rem_to_pt(2), rem_to_pt(2), 0, unit = "pt"),
    )

  player_count <- nrow(selected_draft_team())

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 5 + (as.integer(player_count) * 0.3)) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})

# grades ----
#rfl_draft_class_grades <- rfl_draft_grades %>%
#  dplyr::filter(pick == "klasse") %>%
#  dplyr::group_by(team_id, franchise_name, draft_class) %>%
#  dplyr::filter(year == max(year)) %>%
#  dplyr::summarise(
#    grade = mean(grade),
#    .groups = "drop"
#  )

output$draft_grades_overview <- reactable::renderReactable({
  #shiny::validate(
  #  shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
  #)

  #reactable_default(
  #  rfl_draft_class_grades,
  #  columns = list(
  #    team_id = reactable::colDef(show = FALSE),
  #    franchise_name = reactable::colDef(
  #      name = "Team",
  #      minWidth = 170
  #    ),
  #    draft_class = reactable::colDef(
  #      name = "Draft",
  #      style = function(value) {
  #        list(
  #          textAlign = "center"
  #        )
  #      },
  #      minWidth = 50
  #    ),
  #    grade = reactable_coldef_bg(
  #      name = "Note",
  #      palette_fun = scale_rainbow(range(data$grade, na.rm = TRUE)),
  #      style_fun = function(value) {
  #        list(
  #          textAlign = "center"
  #        )
  #      }
  #    )
  #  ),
  #  sortable = TRUE,
  #  filterable = TRUE,
  #  defaultSorted = c("grade"),
  #  pagination = TRUE,
  #  defaultPageSize = 10
    # height = 772
    #height = 745
  #)
})

output$draft_grades_overview_plot <- ggiraph::renderGirafe({
  #plot <- ggplot2::ggplot(rfl_draft_class_grades, ggplot2::aes(x = draft_class, y = grade)) +
  #  ggiraph::geom_point_interactive(
  #    ggplot2::aes(
  #      tooltip = paste(draft_class, franchise_name),
  #      data_id = team_id,
  #    ),
  #    size = 5
  #  ) +
  #  plot_defaults +
  #  ggplot2::labs(
  #    x = "Draft-Jahr",
  #    y = "Aktuellste Ø Gesamtbewertung"
  #  )

  #ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 9) %>%
  #  ggiraph::girafe_options(
  #    ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
  #    ggiraph::opts_hover_inv(css = "opacity:0.4")
  #  )
})
# TODO: Charts von Draftklassen seite (Investment Returns), Draftkapital
