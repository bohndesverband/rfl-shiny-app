rfl_draft_classes_filtered <- shiny::reactive({
  rfl_draft_classes_filtered <- rfl_drafts_data %>%
    dplyr::filter(season > 2016)
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
    rfl_draft_classes_filtered,
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

# zu- und abgänge ----





rfl_draft_class_grades <- rfl_draft_grades %>%
  dplyr::filter(pick == "klasse") %>%
  dplyr::group_by(team_id, franchise_name, draft_class) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::summarise(
    grade = mean(grade),
    .groups = "drop"
  )

output$draft_grades_overview <- reactable::renderReactable({
  #shiny::validate(
  #  shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
  #)

  reactable_default(
    rfl_draft_class_grades,
    columns = list(
      team_id = reactable::colDef(show = FALSE),
      franchise_name = reactable::colDef(
        name = "Team",
        minWidth = 170
      ),
      draft_class = reactable::colDef(
        name = "Draft",
        style = function(value) {
          list(
            textAlign = "center"
          )
        },
        minWidth = 50
      ),
      grade = reactable_coldef_bg(
        name = "Note",
        palette_fun = scale_rainbow(range(data$grade, na.rm = TRUE)),
        style_fun = function(value) {
          list(
            textAlign = "center"
          )
        }
      )
    ),
    sortable = TRUE,
    filterable = TRUE,
    defaultSorted = c("grade"),
    pagination = TRUE,
    defaultPageSize = 10
    # height = 772
    #height = 745
  )
})

output$draft_grades_overview_plot <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(rfl_draft_class_grades, ggplot2::aes(x = draft_class, y = grade)) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = paste(draft_class, franchise_name),
        data_id = team_id,
      ),
      size = 5
    ) +
    plot_defaults +
    ggplot2::labs(
      x = "Draft-Jahr",
      y = "Aktuellste Ø Gesamtbewertung"
    )

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 9) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})
