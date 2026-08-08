# TODO: seite für neueste draftklasse aufräumen (Dinge ausblenden)
# TODO: plot für die density der gedrafteten position, ähnlich wie auf history seite ganz unten
# daten ----
## fitere draft daten ----
rfl_draft_classes_filtered <- shiny::reactive({
  rfl_draft_classes_filtered <- rfl_drafts_data %>%
    dplyr::filter(season > 2016) %>%

    #filter(franchise_id == "0001") %>%
    #dplyr::filter(season == 2024)
    dplyr::filter(season == input$selectYear) %>%
    dplyr::filter(round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2]) %>%
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
    )
})

## filtere summierte daten für klasse ----
rfl_draft_classes_sum_filtered <- shiny::reactive({
  rfl_draft_classes_sum_filtered <- rfl_draft_classes_sum %>%
    #dplyr::filter(season == 2021)
    dplyr::filter(season == input$selectYear)

  rfl_draft_classes_sum_filtered
})

# TODO: filter nach position und draftrunden einbauen und daten dahingehend berechnen


# TODO: filterung bei button klick
# shiny::bindEvent(input$filterData, ignoreNULL = FALSE)
# draftpicks ----
output$draft_classes_picks <- reactable::renderReactable({
  shiny::validate(
    shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
  )

  data <- rfl_draft_classes_filtered() %>%
    dplyr::left_join(
      player_elo %>%
        dplyr::group_by(mfl_id) %>%
        dplyr::arrange(season, week) %>%
        dplyr::summarise(player_elo = list(player_elo_post), .groups = "drop"),
      by = "mfl_id"
    ) %>%
    dplyr::arrange(overall) %>%
    dplyr::mutate(
      pick = paste(round, formatC(pick, width = 2, flag = "0"), sep = "."),
      player_info = paste(pos_grouped, team, sep = ", ")
    ) %>%
    dplyr::select(pick, player_name_with_badge, player_info, franchise_name, subline, pick_cat, pvar, voe)

  reactable_default(
    data,
    columns = list(
      player_name_with_badge = reactable::colDef(
        name = "Spieler",
        html = TRUE,
        cell = function(value, index) {
          content <- shiny::tagList(
            htmltools::div(htmltools::HTML(value)),
            htmltools::div(htmltools::HTML(paste0("<small>", data$player_info[index], "</small>")))
          )

          as.character(content)
        },
        minWidth = 170
      ),
      franchise_name = reactable::colDef(
        name = "Team",
        html = TRUE,
        cell = function(value, index) {
          content <- shiny::tagList(
            htmltools::div(value),
            htmltools::div(htmltools::HTML(paste0("<small>", data$subline[index], "</small>")))
          )

          as.character(content)
        },
        minWidth = 170
      ),
      pick = reactable::colDef(
        name = "Pick",
        style = function(value) {
          list(
            textAlign = "center"
          )
        },
        minWidth = 50
      ),
      pvar = coldef_pvar(),
      voe = coldef_voe(),
      player_info = reactable::colDef(show = FALSE),
      subline = reactable::colDef(show = FALSE),
      pick_cat = reactable::colDef(show = FALSE)
    ),
    columnGroups = list(
      colGroup(name = "Value", columns = c("pvar", "voe"))
    ),
    sortable = TRUE,
    filterable = TRUE,
    defaultSorted = c("pick"),
    # height = 772
    height = 745
  )
})

# impact ----
output$draft_classes_impact <- ggiraph::renderGirafe({
  shiny::validate(
    shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
  )

  plot <- rfl_draft_classes_sum_filtered() %>%
    plot_pVARexp_voe()

  girafe_default_output(plot)
})

# draftklassen ----
output$draft_classes_teams <- reactable::renderReactable({
  shiny::validate(
    shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
  )

  data <- rfl_draft_classes_sum_filtered() %>%
    dplyr::select(franchise_name, season, picks, rank_season, voe, pvar, rank, voe_pctl)

  draft_classes_teams_reactable(
    data,
    rfl_draft_classes_filtered,
    columns = list(
      season = reactable::colDef(show = FALSE)
    ),
    column_groups = list(
      colGroup(name = paste0("2017-", new_season_march - 1), columns = c("rank", "voe_pctl")),
      colGroup(name = paste0(input$selectYear), columns = c("rank_season", "voe", "pvar"))
    )
  )
})

# TODO: bei team filter hervorheben

# VOE ----
rfl_draft_voe <- shiny::reactive({
  rfl_draft_voe <- rfl_drafts_data %>%
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
    filter(!is.na(player_name))
})

output$draft_classes_voe <- ggiraph::renderGirafe({
  shiny::validate(
    shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
  )

  plot <- ggplot2::ggplot(rfl_draft_classes_filtered(), ggplot2::aes(x = overall, y = pvar, color = factor(pos_grouped, positions_grouped))) +
    ggplot2::geom_vline(xintercept = 36, color = color_grey_light) +
    ggplot2::geom_vline(xintercept = 72, color = color_grey_light) +
    ggplot2::geom_vline(xintercept = 108, color = color_grey_light) +
    ggplot2::geom_vline(xintercept = 144, color = color_grey_light) +
    ggplot2::geom_vline(xintercept = 180, color = color_grey_light) +
    ggplot2::geom_vline(xintercept = 216, color = color_grey_light) +
    ggplot2::geom_vline(xintercept = 252, color = color_grey_light) +
    ggplot2::geom_smooth(
      data = rfl_draft_pvar_exp,
      ggplot2::aes(y = pvar_exp),
      se = FALSE,
      color = color_red,
      size = 0.5
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = paste(
          player_name, paste0("(", pos_grouped, ", ", team, ")"),
          "\n", franchise_name,
          "\nPick: ", paste0(round, ".", pick, " (#", overall, ")"),
          "\nVOE:", voe),
        data_id = mfl_id
      ),
      alpha = 0.8,
      size = 7,
      hover_nearest = TRUE
    ) +
    ggplot2::scale_color_manual(values = colors_position[names(colors_position) %in% unique(rfl_draft_voe()$pos_grouped)], guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +
    plot_defaults +
    ggplot2::labs(
      title = paste("RFL Draftklasse", input$selectYear, "Value over Expected"),
      #subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem derzeitigen Wert."),
      x = "Overall Pick im RFL Draft",
      y = "pVAR",
      color = "Position"
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 9) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})

# draftboard ----
rfl_draft_boards <- shiny::reactive({
  rfl_draft_boards <- rfl_draft_classes_filtered() %>%
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

# reaches ----
output$draft_reaches <- ggiraph::renderGirafe({
  plot_data <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & !is.na(player_name)) %>%
    dplyr::select(season, mfl_id, player_name_with_info, pick_info, min_pick, max_pick, voe) %>%
    dplyr::mutate(
      pick_diff = min_pick - max_pick,
      pick_diff_pctl = 1 - dplyr::percent_rank(pick_diff)
    ) %>%
    dplyr::group_by(season, mfl_id) %>%
    dplyr::arrange(pick_info) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = min_pick, y = max_pick)) +
    ggplot2::geom_point(
      color = color_grey_light,
      size = 3
    ) +
    ggiraph::geom_point_interactive(
      data = subset(plot_data, season == input$selectYear),
      ggplot2::aes(
        tooltip = paste(
          player_name_with_info,
          "\n", pick_info
        ),
        data_id = mfl_id,
        color = pick_diff_pctl,
      ),
      size = 5
    ) +
    ggplot2::scale_colour_gradientn(colors = c(color_blue, color_green, color_orange, color_red), rescaler = ~ scales::rescale_mid(.x, mid = 0.5), guide = "none") +
    plot_defaults +
    ggplot2::labs(
      title = "Reaches anhand der RFL Draftposition",
      subtitle = "Jeder Punkt ist ein Spieler im RFL Draft. Die farbig hervorgehobenen gehören zur ausgwählten Klasse.\nJe weiter oben links er in der Ecke steht, desto mehr wurde beim ersten Pick für ihn gereacht.",
      x = "1. Copy",
      y = "3. Copy"
    )

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 9) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg))
    )
})

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

    ggplot2::geom_tile(data = subset(rfl_draft_boards(), franchise_id %in% input$selectRflTeams), ggplot2::aes(color = franchise_name), fill = NA, linewidth = 2) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors_positions_grouped, guide = "none") +
    ggplot2::scale_y_reverse(limits = c(36.5, 0.5), breaks = c(1:36), expand = c(0, 0)) +
    ggplot2::scale_x_continuous(limits = c(0.5, max(rfl_draft_boards()$round) + 0.5), breaks = c(1:max(rfl_draft_boards()$round)), expand = c(0, 0)) +
    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = paste("RFL Draft Board", input$selectYear),
      subtitle = "Angezeigt werden alle Picks des Drafts mit ihren Total Fantasy Points (FPts) seit dem Draft.\nDer gepunktete Balken visualisiert die FPts des besten Spielers der Draftklasse. Die länge des weißen Balkens zeigt die relativen FPts des besten Spielers der Positionsgruppe.",
      x = "Runde",
      y = "Pick",
      color = ""
    )
}, height = 2600)

# pVARexp ----
output$draft_class_voe_exp <- reactable::renderReactable({
  drafts_data <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear & !is.na(player_name) & pos_grouped != "PK") %>%
    dplyr::select(franchise_id, franchise_name, pos_grouped, voe, pvar_exp, asset_name_with_subline, pick_cat_badge)

  render_draft_history_table(
    data_source = drafts_data,
    use_voe = input$draft_class_voe_exp_toggle,
    use_mean = input$draft_class_voe_exp_toggle_per_pick
  )
})

# bewertung ----


#TODO: vergleich Kapital vor/nach dem draft (wer hat am meisten getradet)

# ui output ----
output$draft_class <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Draft", input$selectYear)),
    shiny::fluidRow(
      shiny::column(
        htmltools::h2("Alle Picks des RFL Drafts"),
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_picks")),
        width = 6
      ),
      shiny::column(
        htmltools::h2("Alle Picks der RFL Draftklassen"),
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_classes_teams")),
        width = 6
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("Welche Draftklasse ist am wertvollsten?"),
    #htmltools::div("Abgebildet sind alle Draftpicks mit ihrem derzeitigen Wert nach pVAR. Die Rote kurve zeigt den pVARexp des entsprechenden Picks."),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_impact")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("Welche Picks sind am wertvollsten gewesen?"),
    htmltools::div("Abgebildet sind alle Draftpicks mit ihrem derzeitigen Wert nach pVAR. Die Rote kurve zeigt den pVARexp des entsprechenden Picks."),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_classes_voe")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("RFL Draftboard"),
    shinyWidgets::prettySwitch("sortForFpts", "Draftboard nach FPts sortieren", value = FALSE, fill = TRUE, status = "primary"),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(shiny::plotOutput("draft_board", height = "2600px")),
        width = 12
      )
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_reaches")),
        width = 12
      )
    ),
    htmltools::hr(style = "margin-block: 2rem"),
    htmltools::h2("Wie viel haben die Teams in die verschiedenen Positionsgruppen investiert?"),
    htmltools::div("Summe der Werte, die für eine bestimmte Positionsgruppe investiert wurden. pVARexp zeigt dabei, wie viel ein Team investiert hat. Schaust du dir VOE an, siehst du, wie erfolgreich es dabei war. Um die Effzizenz statt die Summe zu sehen ändere die Ansicht zu \"per Pick\". Mit Klick auf die Pfeile kannst du dir die konkreten Picks anschauen."),
    htmltools::div(
      shinyWidgets::prettySwitch("draft_class_voe_exp_toggle", label = "VOE statt pVARexp zeigen", status = "primary", fill = TRUE),
      shinyWidgets::prettySwitch("draft_class_voe_exp_toggle_per_pick", label = "per Pick Daten zeigen", status = "primary", fill = TRUE),
      style = "display: flex; align-items: center; flex-wrap: wrap; gap: 1rem; margin-top: 2rem;"
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_class_voe_exp")),
        width = 12
      )
    )
  )
})
