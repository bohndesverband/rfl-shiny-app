# create base data ----
filtered_ir_data <- shiny::reactive({
  filtered_ir_data <- rfl_ir_data %>%
    dplyr::filter(
      season >= input$selectYears[1] & season <= input$selectYears[2]
    )
})

# weekly IR analysis ----
## plot data ----
output$ir_weekly <- ggiraph::renderGirafe({
  team_ir <- filtered_ir_data() %>%
    dplyr::group_by(franchise_id, season, week) %>%
    dplyr::summarise(
      player_count = n(),
      franchise_name = first(franchise_name),
      ppg_median = median(ppg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::arrange(season, week) %>%
    dplyr::mutate(new_week = dplyr::row_number())

  plot <- ggplot2::ggplot(team_ir, ggplot2::aes(x = new_week, y = ppg_median, color = franchise_name)) +
    ggplot2::geom_boxplot(ggplot2::aes(group = new_week), fill = color_grey_light, color = color_grey_mid, linewidth = 0.15, outliers = FALSE) +
    ggiraph::geom_jitter_interactive(
      data = subset(team_ir, !franchise_id %in% c(input$selectRflTeams)),
      ggplot2::aes(tooltip = paste0(franchise_name, "\n WK ", week, " ", season), size = player_count, alpha = player_count, data_id = franchise_id),
      width = 0.25, color = color_grey_mid
    ) +

    ggplot2::aes(lwd = 1.2) +
    ggplot2::scale_linewidth_identity() +

    ggiraph::geom_point_interactive(
      data = subset(team_ir, franchise_id %in% c(input$selectRflTeams)),
      ggplot2::aes(tooltip = paste0(franchise_name, "\n WK ", week, " ", season), size = player_count, data_id = franchise_id)
    ) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1,8)) +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean +
    ggplot2::scale_x_continuous(limits = c(0.4, max(team_ir$new_week) + 0.4), labels = c(1:max(team_ir$new_week)), breaks = c(1:max(team_ir$new_week))) +
    ggplot2::labs(
      title = "Median FPts/G aller Spieler auf der NFL IR",
      x = "Woche",
      y = "IR Median FPts/G",
      color = "RFL Teams",
      size = "Anzahl Spieler auf IR"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.09, 0.8)
    )

  if (max(team_ir$new_week) > 1) {
    plot <- plot +
      ggalt::geom_xspline(data = subset(team_ir, franchise_id %in% c(input$selectRflTeams)), spline_shape = -0.5)
  }

  girafe_default_output(plot)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# lost FPTS ----
output$lost_fpts <- reactable::renderReactable({
  data <- filtered_ir_data() %>%
    dplyr::group_by(franchise_id, player_id, season) %>%
    dplyr::mutate(
      games_on_ir = n(),
      points = round(ppg * games_on_ir, 2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(franchise_name, player_name_with_info, season, games_on_ir, ppg, points) %>%
    dplyr::distinct()

  reactable_default(
    data,
    columns = list(
      franchise_name = reactable::colDef(name = "Team", width = 350),
      player_name_with_info = reactable::colDef(
        name = "Spieler",
        width = 250,
        html = TRUE
      ),
      season = reactable::colDef(
        name = "Saison",
        aggregate = reactable::JS("
          function(values) {
            values = values
              .map(Number)
              .filter(function(x) { return !isNaN(x); });

            if (values.length === 0) return '';

            var min = Math.min.apply(null, values);
            var max = Math.max.apply(null, values);

            return min === max ? String(min) : min + '-' + max;
          }
        ")
      ),
      games_on_ir = reactable::colDef(name = "Spiele", width = 100, aggregate = "sum"),
      ppg = reactable::colDef(name = "PPG", width = 100, aggregate = "sum", format = colFormat(digits = 2)),
      points = reactable::colDef(name = "FPts", width = 100, aggregate = "sum", format = colFormat(digits = 2))
    ),
    columnGroups = list(
      reactable::colGroup(name = "Verpasst", columns = c("games_on_ir", "ppg", "points"))
    ),
    groupBy = "franchise_name",
    defaultSorted = list(points = "desc", ppg = "desc"),
    defaultPageSize = 36,
    showPagination = FALSE
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ui output ----
output$injured_reserve <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Injured Reserve", input$selectYear)),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("ir_weekly")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    shiny::fluidRow(
      shiny::column(
        htmltools::h2(paste("Durch IR verlorene Fantasy Punkte", input$selectYear)),
        htmltools::div("Für die Berechnung werden die durchschnittlichen Fantasy Punkte pro Spiel (FPts/G) aus der\naktuellsten Saison mit mind. 3 Spielen genommen. Diese werden mit den dieses Jahr verpassten\nSpielen multipliziert."),
        shinycssloaders::withSpinner(reactable::reactableOutput("lost_fpts")),
        width = 12
      )
    )
  )
})

# TODO: Durch IR verlorene Punkte als reactable
# TODO: alle IR Spieler als Tabelle anzeigen
# TODO: filterung nach Jahr ermöglichen
# TODO: filter zu nur einem Team ändern
