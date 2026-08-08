# https://perthirtysix.com/nfl/draft/season

## helper ----
draft_history_best_picks_helper <- function(df, group) {
  df %>%
    dplyr::filter(season > 2016 & season < new_season_march) %>%
    dplyr::group_by({{group}}, franchise_id) %>%
    dplyr::summarise(
      franchise_name = dplyr::last(franchise_name),
      value =  {
        use_voe <- input$draft_history_best_picks_by_year_toggle
        #use_mean <- TRUE

        result <- if (use_voe) {
          sum(voe, na.rm = TRUE)
        } else {
          mean(voe, na.rm = TRUE)
        }

        round(result, 1)
      },
      .groups = "drop"
    ) %>%
    dplyr::group_by({{group}}) %>%
    dplyr::mutate(
      cat = dplyr::case_when(
        value == min(value) ~ "worst",
        value == max(value) ~ "best",
        TRUE ~ NA
      )
    ) %>%
    dplyr::filter(!is.na(cat)) %>%
    dplyr::group_by({{group}}, cat) %>%
    dplyr::summarise(
      franchise_name = paste0(franchise_name, collapse = "<br>"),
      value = dplyr::first(value),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      opacity = round(scales::rescale(abs(value) / max(value), c(0.1, 0.5)), 1),
      value_new = ifelse(value > 0, paste0("+", value), value),
      franchise_name = paste0("<span>", franchise_name, "</span>"),
      value = paste0("<span class=\"badge\" style=\"--_opacity:", opacity, "\">", value_new, "</small>"),
      franchise_name = paste0(franchise_name, value),
    ) %>%
    dplyr::select(-value, -value_new, -opacity) %>%
    tidyr::pivot_wider(names_from = cat, values_from = franchise_name)
}

highlight_selected_teams <- function(tbl_data, tbl, selected_teams) {

  if (is.null(selected_teams) || length(selected_teams) == 0) {
    return(tbl)
  }

  # escape regex specials + combine to single pattern
  pattern <- paste(
    stringr::str_replace_all(
      selected_teams,
      "([\\.^$|()\\[\\]{}*+?\\\\-])",
      "\\\\\\1"
    ),
    collapse = "|"
  )

  cols <- names(tbl_data)

  for (col in cols) {

    if (!is.character(tbl_data[[col]]) && !is.factor(tbl_data[[col]])) next

    tbl <- tbl %>%
      gt::tab_style(
        style = gt::cell_fill(color = color_grey_light),
        locations = gt::cells_body(
          columns = all_of(col),
          rows = grepl(pattern, tbl_data[[col]], perl = TRUE)
        )
      )
  }

  tbl
}

# best picks ----
output$draft_history_top_picks <- gt::render_gt({
  tbl_data <- rfl_drafts_rookies %>%
    dplyr::filter(season < new_season_march) %>%
    #dplyr::filter(pos_grouped %in% c("QB")) %>%
    dplyr::group_by(season, pos_grouped) %>%
    dplyr::mutate(
      value =  {
        use_pvar <- input$draft_history_top_picks_toggle
        #use_pvar <- TRUE

        result <- if (use_pvar) {
          pvar
        } else {
          voe
        }

        result
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value_new = ifelse(value > 0, paste0("+", value), value),
      value_new = paste0("<span class=\"badge\" style=\"--_opacity:0.1;\">", value, "</small>"),
      player_and_pick_info = paste0(player_and_pick_info, value_new),
    ) %>%
    #select(season, player_and_pick_info, pos_grouped, pvar, voe, value, value_new, overall) %>%
    dplyr::group_by(season, pos_grouped) %>%
    #dplyr::arrange(dplyr::desc(value)) %>%
    #filter(season == 2020) %>%
    dplyr::slice_max(value, n = 1) %>%
    dplyr::arrange(dplyr::desc(overall)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, pos_grouped, player_and_pick_info) %>%
    tidyr::spread(pos_grouped, player_and_pick_info) %>%
    dplyr::select(season, QB, RB, WR, TE, DL, LB, DB) %>%
    dplyr::arrange(dplyr::desc(season))

  tbl <- tbl_data %>%
    gt::gt() %>%
    gtDefaults %>%
    gt::fmt_markdown() %>%
    gt::tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = season
      )
    ) %>%
    gt::cols_label(season = "Saison")

  highlight_selected_teams(tbl_data, tbl, selected_team_names())
})

### by position ----
output$draft_history_best_picks_by_teams <- gt::render_gt({
  tbl_data <- rfl_drafts_data %>%
    dplyr::filter(pos_grouped != "PK") %>%
    draft_history_best_picks_helper(pos_grouped) %>%
    dplyr::arrange(factor(pos_grouped, levels = positions_grouped))

  tbl <- tbl_data %>%
    gt::gt() %>%
    gtDefaults %>%
    gt::fmt_markdown() %>%
    gt::cols_label(
      pos_grouped = "Pos",
      best = "Beste",
      worst = "Schlechteste"
    ) %>%
    gt::tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = pos_grouped
      )
    )

  highlight_selected_teams(tbl_data, tbl, selected_team_names())
})

### by range ----
output$draft_history_best_picks_by_range <- gt::render_gt({
  tbl_data <- rfl_drafts_data %>%
    draft_history_best_picks_helper(range) %>%
    dplyr::arrange(factor(range, levels = c("Top 12", "Rest of 1st", "Round 2", "Round 3", "Late Rounds")))

  tbl <- tbl_data %>%
    gt::gt() %>%
    gtDefaults %>%
    gt::fmt_markdown() %>%
    gt::cols_label(
      range = "Draft Range",
      best = "Beste",
      worst = "Schlechteste"
    ) %>%
    gt::tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = range
      )
    )

  highlight_selected_teams(tbl_data, tbl, selected_team_names())
})

### by year ----
output$draft_history_best_picks_by_year <- gt::render_gt({
  tbl_data <- rfl_drafts_data %>%
    draft_history_best_picks_helper(season) %>%
    dplyr::arrange(dplyr::desc(season))

  tbl <- tbl_data %>%
    gt::gt() %>%
    gtDefaults %>%
    gt::fmt_markdown() %>%
    gt::cols_label(
      season = "Saison",
      best = "Beste",
      worst = "Schlechteste"
    ) %>%
    gt::tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = season
      )
    )

  highlight_selected_teams(tbl_data, tbl, selected_team_names())
})

# VOE ----
### picks ----
output$draft_history_voe <- ggiraph::renderGirafe({
  data <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & season < new_season_march & pos != "PK" & !is.na(player_name))

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = overall, y = voe, size = pvar_exp)) +
    plot_voe_defaults(
      title = "Draft Investment Returns nach Positionen",
      subtitle = "Jeder Punkt ist ein Draftpick. Die Größe spiegelt den pVARexp wieder (je größer der Punkt, desto wertvoller war der eingesetzte Pick).",
      x = "Overall Pick im RFL Draft"
    ) +
    ggplot2::facet_wrap(~ factor(pos_grouped, levels = positions_grouped), ncol = 3) +
    ggplot2::geom_hline(yintercept = 0, color = color_grey_mid)

  if (length(input$selectRflTeams) > 0) {
    plot <- plot +
      ggplot2::geom_point(
        size = 2,
        color = color_grey_light
      ) +
      ggiraph::geom_point_interactive(
        data = subset(data, franchise_id %in% input$selectRflTeams),
        ggplot2::aes(
          tooltip = paste(
            player_name, paste0("(", pos_grouped, ", ", team, ")"),
            "\n", franchise_name,
            "\nPick: ", paste0(round, ".", pick, season, " (#", overall, ")"),
            "\npVAR:", pvar,
            "\nVOE:", voe
          ),
          data_id = mfl_id,
          color = franchise_name
        ),
        size = 5,
        alpha = 1
      )+
      ggplot2::scale_color_discrete(palette = colors) +
      ggplot2::labs(
        color = "RFL Team"
      )
  } else {
    plot <- plot +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(
            player_name, paste0("(", pos_grouped, ", ", team, ")"),
            "\n", franchise_name,
            "\nPick: ", paste0(round, ".", pick, " ", season, " (#", overall, ")"),
            "\npVAR:", pvar,
            "\nVOE:", voe
          ),
          data_id = mfl_id,
          color = voe
        ),
        alpha = 0.5
      )
  }

  plot

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 14) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})

### classes ----
output$draft_history_voe_classes <- ggiraph::renderGirafe({
  plot <- rfl_draft_classes_sum %>%
    plot_pVARexp_voe(selectedTeamNames = input$selectRflTeams)

  girafe_default_output(plot)
})

# pVARexp ----
output$draft_history_voe_exp <- reactable::renderReactable({
  drafts_data <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & season < new_season_march & !is.na(player_name) & pos_grouped != "PK") %>%
    dplyr::select(franchise_id, franchise_name, pos_grouped, voe, pvar_exp, asset_name_with_subline, pick_cat_badge)

  render_draft_history_table(
    data_source = drafts_data,
    use_voe = input$draft_history_voe_exp_toggle,
    use_mean = input$draft_history_voe_exp_toggle_per_pick
  )
})

# reaches / steals nach RFL Picks
output$draft_reaches <- ggiraph::renderGirafe({
  plot <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & season < new_season_march & !is.na(player_name)) %>%
    #dplyr::filter(pick_value >= input$selectDraftStealTreshold) %>%
    dplyr::select(season, mfl_id, player_name_with_info, min_pick, max_pick, voe) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      pick_diff = min_pick - max_pick,
      pick_diff_pctl = 1 - dplyr::percent_rank(pick_diff)
    ) %>%
    dplyr::filter(pick_diff_pctl > 0.9) %>%
    ggplot2::ggplot(ggplot2::aes(x = min_pick, y = max_pick)) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(
        tooltip = paste(
          player_name_with_info,
          "\n1. Copy:", min_pick,
          "\n3. Copy:", max_pick
        ),
        data_id = mfl_id,
        color = pick_diff_pctl,
        size = voe
      )
    ) +
    ggplot2::scale_colour_gradientn(colors = c(color_blue, color_green, color_orange, color_red), rescaler = ~ scales::rescale_mid(.x, mid = 0.5), guide = "none") +
    ggplot2::scale_size(range = c(1, 8), guide = "none") +
    plot_defaults +
    ggplot2::labs(
      title = "Reaches anhand der RFL Draftposition",
      subtitle = "Jeder Punkt ist ein Spieler im RFL Draft. Je weiter oben links er in der Ecke steht, desto mehr wurde beim ersten Pick für ihn gereacht.\nDie Größe des Punktes beschreibt den VOE des Spielers.",
      x = "1. Pick im RFL Draft",
      y = "3. Pick im RFL Draft"
    )

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 9) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg))
    )
})

# nach nfl rounds ----
output$draft_history_picks_by_round <- shiny::renderPlot({
  rfl_drafts_rookies %>%
    dplyr::filter(pos != "PK") %>%
    dplyr::rename(rfl_round = round) %>%
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
    ggplot2::facet_wrap(~factor(pos, levels = positions_full), ncol = 5) +
    ggplot2::geom_tile(mapping = ggplot2::aes(alpha = count)) +

    ggplot2::scale_x_continuous(limits = c(1, 9), breaks = seq(1, 8, by = 1), expand = c(0, 0)) +
    ggplot2::scale_y_reverse(limits = c(8, 1), breaks = seq(7, 1, by = -1), expand = c(0, 0)) +
    ggplot2::scale_alpha(range = c(0.1, 1), guide = "none") +
    ggplot2::scale_fill_manual(values = colors_position, guide = "none") +
    plot_defaults +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(hjust = -3),
      axis.text.y = ggplot2::element_text(vjust = 2.7),
    ) +
    ggplot2::labs(
      title = "RFL Draftpicks nach NFL Draft-Runde",
      subtitle = "In welcher Runde wurden die im RFL Draft gepickten Spieler im NFL Draft gewählt?",
      x = "NFL Draft Runde",
      y = "RFL Draft Runde"
    )
}, height = 800)

## grades ----
rfl_draft_class_grades <- rfl_draft_grades %>%
  dplyr::filter(pick == "klasse") %>%
  dplyr::group_by(team_id, franchise_name, draft_class) %>%
  dplyr::filter(year == max(year)) %>%
  dplyr::summarise(
    grade = mean(grade),
    .groups = "drop"
  )

output$draft_grades_overview <- reactable::renderReactable({
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
        name = "Ø Note",
        palette_fun = scale_rainbow_reverse(range(rfl_draft_class_grades$grade, na.rm = TRUE))
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

### plot ----
output$draft_grades_overview_plot <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(rfl_draft_class_grades, ggplot2::aes(x = draft_class, y = grade)) +
    plot_defaults +
    ggplot2::scale_x_continuous(
      limits = c(min(rfl_draft_class_grades$draft_class), max(rfl_draft_class_grades$draft_class)),
      breaks = seq(min(rfl_draft_class_grades$draft_class), max(rfl_draft_class_grades$draft_class), by = 1),
      minor_breaks = NULL
    ) +
    ggplot2::scale_y_reverse(
      limits = c(7, 1),
      breaks = seq(6, 1, by = -1),
      minor_breaks = seq(6.666, 1, by = -0.333),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = "Draft-Jahr",
      y = "Aktuellste Ø Gesamtbewertung"
    ) +
    ggplot2::theme(

    )

  if (length(input$selectRflTeams) > 0) {
    plot <- plot +
      ggplot2::geom_point(
        size = 5,
        color = color_grey_light
      ) +
      ggiraph::geom_point_interactive(
        data = subset(rfl_draft_class_grades, team_id %in% input$selectRflTeams),
        ggplot2::aes(
          tooltip = paste(draft_class, franchise_name),
          data_id = team_id,
          color = franchise_name
        ),
        size = 10,
        alpha = 1
      ) +
      ggplot2::scale_color_discrete(palette = colors) +
      ggplot2::labs(
        color = "RFL Team"
      )
  } else {
    plot <- plot +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(draft_class, franchise_name),
          data_id = team_id,
          color = grade
        ),
        size = 10,
        alpha = 0.5
      ) +
      ggplot2::scale_color_continuous(palette = c(color_blue, color_green, color_yellow, color_orange, color_red), guide = "none")
  }

  girafe_default_output(plot)
})

# ui output ----
output$draft_history <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste0("RFL Draft History 2017-", new_season_march - 1)),
    shiny::fluidRow(
      shiny::column(
        # TODO: link zu pVAR
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_history_voe_classes")),
        width = 12
      )
    ),
    htmltools::hr(),
    shiny::fluidRow(
      shiny::column(
        htmltools::h2("Welche Spieler waren die wertvollsten Picks ihres Drafts?"),
        htmltools::div("Dieser Spieler haben den Wert ihre Draftposition am meisten übertroffen (VOE) oder sind die wertvollsten (pVAR)."),
        htmltools::div(
          shinyWidgets::prettySwitch("draft_history_top_picks_toggle", label = "pVAR statt VOE zeigen", status = "primary", fill = TRUE),
          style = "margin-top: 2rem;"
        ),
        shinycssloaders::withSpinner(gt::gt_output("draft_history_top_picks")),
        width = 12
      )
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_reaches")),
        width = 12
      )
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_history_voe")),
        width = 12
      )
    ),
    htmltools::hr(),
    htmltools::h2("Was sind die Stärken und Schwächen der RFL Teams im Draft?"),
    htmltools::div("Diese Teams haben nach Jahr, Position und Draft Range am besten und schlechtesten abgeschnitten. Um die unterschiedlichen Größen der Draftklassen zu berücksichtigen, wird der durchschnittliche VOE angezeigt."),
    htmltools::div(
      shinyWidgets::prettySwitch("draft_history_best_picks_by_year_toggle", label = "VOE stat VOE/Pick anzeigen", status = "primary", fill = TRUE),
      style = "margin-top: 2rem;"
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(gt::gt_output("draft_history_best_picks_by_year")),
        width = 4
      ),
      shiny::column(
        shinycssloaders::withSpinner(gt::gt_output("draft_history_best_picks_by_teams")),
        width = 4
      ),
      shiny::column(
        shinycssloaders::withSpinner(gt::gt_output("draft_history_best_picks_by_range")),
        width = 4
      )
    ),
    htmltools::hr(),
    htmltools::h2("Wie viel haben die Teams in die verschiedenen Positionsgruppen investiert?"),
    htmltools::div("Summe der Werte, die für eine bestimmte Positionsgruppe investiert wurden. pVARexp zeigt dabei, wie viel ein Team investiert hat. Schaust du dir VOE an siehst du, wie erfolgreich es dabei war. Um die Effzizenz statt die Summe zu sehen ändere die Ansicht zu \"per Pick\". Mit Klick auf die Pfeile kannst du dir die konkreten Picks anschauen."),
    htmltools::div(
      shinyWidgets::prettySwitch("draft_history_voe_exp_toggle", label = "VOE statt pVARexp zeigen", status = "primary", fill = TRUE),
      shinyWidgets::prettySwitch("draft_history_voe_exp_toggle_per_pick", label = "per Pick Daten zeigen", status = "primary", fill = TRUE),
      style = "display: flex; align-items: center; flex-wrap: wrap; gap: 1rem; margin-top: 2rem;"
    ),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_history_voe_exp")),
        width = 12
      )
    ),
    htmltools::hr(),
    htmltools::h2("Bewertungen der RFL Draftklassen"),
    htmltools::div("Adrian und Jakob bewerten die RFL Draftklassen seit 2024 nach dem Draft, nach einem, drei und fünf Jahren. Hier abgebildet werden die durchschnittlichen Gesamtnoten der aktuellsten Bewertung."),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(reactable::reactableOutput("draft_grades_overview")),
        width = 4
      ),
      shiny::column(
        shinycssloaders::withSpinner(ggiraph::girafeOutput("draft_grades_overview_plot")),
        width = 8
      ),
      style = "padding-inline: 2rem;"
    ),
    htmltools::hr(),
    htmltools::h2("Wann wurden die NFL Draftpicks in den RFL Drafts gepickt?"),
    shiny::fluidRow(
      shiny::column(
        shinycssloaders::withSpinner(shiny::plotOutput("draft_history_picks_by_round")),
        width = 12
      )
    )
  )
})
