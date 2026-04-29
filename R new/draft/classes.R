# daten ----
rfl_draft_classes <- shiny::reactive({
  rfl_draft_classes <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & season < new_season_march) %>%

    #filter(franchise_id == "0001") %>%
    #dplyr::filter(season == 2025) %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::filter(round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2])
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

rfl_draft_classes_sum <- rfl_drafts_data %>%
  dplyr::filter(season > 2016) %>%
  group_by(season, franchise_id, franchise_name, class) %>%
  dplyr::summarise(
    picks = n(),
    value = sum(value),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    value_pctl = dplyr::percent_rank(value)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(dplyr::desc(value)) %>%
  dplyr::mutate(
    rank = dplyr::row_number(),
    season_date = as.Date(paste0(season, "-01-01"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, season_date, franchise_id, franchise_name, class, picks, rank, value, value_pctl)

rfl_draft_classes_sum_filtered <- shiny::reactive({
  rfl_draft_classes_sum %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        franchise_id %in% input$selectRflTeams
      else
        TRUE
    ) %>%
    head(2)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ranking aller draftklassen ----
## overview ----
output$draft_classes_overview <- shiny::renderPlot({
  ggplot2::ggplot(rfl_draft_classes_sum, ggplot2::aes(x = season_date, y = reorder(franchise_name, dplyr::desc(franchise_name)), fill = rank)) +
    ggplot2::geom_tile(color = color_bg) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = rank,
        fontface = "bold",
        size = 10
      )
    ) +
    ggplot2::scale_fill_gradient2(high = color_red, mid = color_yellow, low = color_green, midpoint = 18, guide = "none") +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "'%y", expand = c(0, 0)) +
    plot_defaults +
    ggplot2::labs(
      title = "RFL Draftklassen Überblick",
      x = "Saison"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

}, height = 800)

# überblick über alle draftklassen eines teams ----
#output$team_draft_classes <- reactable::renderReactable({
#  selected_teams <- rfl_draft_classes_sum_filtered() %>%
#    dplyr::filter(franchise_id %in% input$selectRflTeams)

#  reactable::reactable(
#    selected_teams
#  )
#})





## value ----
#draft_classes_value_height <- shiny::reactiveVal(250)

#shiny::observeEvent(input$selectRflTeams, {
#  n_selected <- if (is.null(input$selectRflTeams)) 0 else length(input$selectRflTeams)

#  if (n_selected != 0) {
#    draft_classes_value_height(n_selected * 250)
#  } else {
#    draft_classes_value_height(250)
#  }
#})

#output$draft_classes_value <- shiny::renderPlot({
#  ggplot2::ggplot(data = subset(rfl_draft_classes_sum, franchise_id %in% input$selectRflTeams), ggplot2::aes(x = as.Date(paste0(season, "-01-01")), y = value)) +
#    ggplot2::facet_wrap(~ franchise_name, ncol = 1) +
#    ggplot2::geom_col(ggplot2::aes(fill = rank)) +
#    ggplot2::geom_text(
#      ggplot2::aes(
#        y = min(value) - 0.7,
#        label = paste0("#", rank),
#        color = rank
#      ),
#      fontface = "bold",
#      size = 5
#    ) +
#    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "'%y", expand = c(0, 0)) +
#    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.1))) +
#    ggplot2::scale_fill_gradient2(high = color_red, mid = color_yellow, low = color_green, midpoint = 16, guide = "none") +
#    ggplot2::scale_color_gradient2(high = color_red, mid = color_yellow, low = color_green, midpoint = 16, guide = "none") +
#    ggplot2::scale_size_continuous(guide = "none") +
#    plot_defaults +
#    ggplot2::labs(
#      title = "Ranking der Draftklassen"
#    ) +
#    ggplot2::theme(
#      axis.title.x = ggplot2::element_blank(),
#      axis.title.y = ggplot2::element_blank(),
#      axis.text.y = ggplot2::element_blank(),
#      panel.grid.major = ggplot2::element_blank(),
#      panel.grid.minor = ggplot2::element_blank()
#    )
#}, height = function() { draft_classes_value_height() })



team_draft_class_elo_height <- shiny::reactiveVal(800)

shiny::observeEvent(input$selectYears, {
  if (input$selectYears[2] - input$selectYears[1] == 0) {
    # wenn nur eine klasse ausgewählt ist
    team_draft_class_elo_height(800)
  } else {
    height_val <- ceiling((input$selectYears[2] - input$selectYears[1] + 1) / 2) * 450
    team_draft_class_elo_height(height_val)
  }
})

## output ----


#observeEvent(event_data("plotly_click", source = "A"), {
#  click <- event_data("plotly_click", source = "A")
#  clicked_data <- rfl_draft_classes()[click$pointNumber + 1, ]

#  shinyWidgets::updatePickerInput(
#    session,
#    "selectRflTeams",
#    selected = setNames(clicked_data$franchise_id, clicked_data$franchise_name)
#  )

#  shiny::updateSliderInput(
#    session,
#    "selectYear",
#    value = clicked_data$season
#  )
#})

# einzelne draft klasse eines teams ----
## reactable ----
output$team_draft_class <- reactable::renderReactable({
  data <- rfl_drafts_data %>%
    dplyr::select(-elo_season_end) %>%
    dplyr::left_join(
      player_elo %>%
        dplyr::select(mfl_id, elo_season = season, elo_season_end) %>%
        dplyr::distinct(),
      by = c("mfl_id"),
      relationship = "many-to-many"
    ) %>%
    dplyr::group_by(mfl_id) %>%
    dplyr::mutate(last_season = ifelse(elo_season == max(elo_season), 1, 0)) %>%
    dplyr::ungroup()

  row_details <- function(index) {
    selected_class <- rfl_draft_classes_sum_filtered()[index, ]$class

    data_filtered <- data %>%
      dplyr::filter(.data$class == selected_class)

    ### picks ----
    output[[paste0("team_draft_class_picks_", index)]] <- reactable::renderReactable({
      draft_class_data <- data_filtered %>%
        dplyr::mutate(
          info = paste0(round, ".", pick, " (#", overall, ")"),
          player = paste0(player_name, " (", team, ", ", pos_grouped, ")")
        ) %>%
        dplyr::select(info, player, subline, pos_rank) %>%
        dplyr::distinct()

      reactable::reactable(
        draft_class_data,
        columns = list(
          player = reactable::colDef(
            name = "Spieler",
            html = TRUE,
            cell = function(value, index) {
              tagList <- tagList(
                div(
                  div(draft_class_data$player[index]),
                  div(htmltools::HTML(draft_class_data$subline[index]), style = list(fontSize = "0.6rem"))
                )
              )

              as.character(tagList)
            }
          ),
          subline = reactable::colDef(show = FALSE)
        ),
        defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom"),
        sortable = FALSE,
        searchable = TRUE,
        striped = TRUE,
        outlined = TRUE,
        theme = reactableTheme(
          borderColor = color_grey_dark,
          stripedColor = color_bg
        )
      )
    })

    ### value ----
    output[[paste0("team_draft_class_value_", index)]] <- shiny::renderPlot({
      ggplot2::ggplot(subset(data_filtered, last_season == 1), ggplot2::aes(x = overall, y = value, color = factor(pos_grouped, positions_grouped))) +
        ggplot2::geom_hline(yintercept = 0, color = color_red, alpha = 0.5) +
        ggplot2::geom_point(
          data = rfl_drafts_data %>%
            dplyr::group_by(mfl_id) %>%
            dplyr::filter(season == data_filtered$season[1]) %>%
            dplyr::filter(franchise_id != data_filtered$franchise_id[1]),
          fill = color_grey_light,
          color = color_grey_light,
          alpha = 1,
          size = 3
        ) +
        ggplot2::geom_point(size = 5, alpha = 1) +
        ggrepel::geom_label_repel(
          ggplot2::aes(label = paste(player_name, paste0("(", pos_grouped, ", ", team, ")"))),
          size = 4,
          alpha = 1,
          show.legend = FALSE
        ) +
        ggplot2::scale_color_manual(values = colors_position[names(colors_position) %in% unique(data$pos_grouped)], guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +
        plot_defaults +
        ggplot2::labs(
          title = paste(selected_class, "Draftvalue"),
          subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem derzeitigen Wert."),
          x = "Overall Pick im RFL Draft",
          y = "⌀WAR"
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(size = 16),
          plot.subtitle = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 11)
        )

    }, height = 700)

    ### elo ----
    output[[paste0("team_draft_class_elo", index)]] <- shiny::renderPlot({
      shiny::validate(
        shiny::need(input$selectYears[2] < new_season_march, "Für die aktuelle Draftklasse gibt es noch keine ELO-Daten. Wähle mit dem zweiten Saison-Regler eine frühere Saison.")
      )

      ggplot2::ggplot(subset(data_filtered, last_season == 1), ggplot2::aes(x = overall, y = elo_season_end, color = factor(pos_grouped, positions_grouped), alpha = season)) +
        ggplot2::geom_hline(yintercept = 1500, color = color_red, alpha = 0.5) +
        ggplot2::geom_point(
          data = rfl_drafts_data %>%
            dplyr::group_by(mfl_id) %>%
            dplyr::filter(season == data_filtered$season[1]) %>%
            dplyr::filter(elo_season_end == max(elo_season_end)) %>%
            dplyr::filter(franchise_id != data_filtered$franchise_id[1]),
          fill = color_grey_light,
          color = color_grey_light,
          alpha = 1,
          size = 3
        ) +
        ggplot2::geom_point(size = 10, alpha = 1) +
        ggplot2::geom_point(data = subset(data_filtered, last_season == 0), size = 5) +
        ggplot2::scale_alpha_continuous(guide = "none") +
        ggrepel::geom_label_repel(
          ggplot2::aes(label = paste(player_name, paste0("(", pos_grouped, ", ", team, ")"))),
          size = 4,
          alpha = 1,
          show.legend = FALSE
        ) +

        ggplot2::scale_color_manual(values = colors_position[names(colors_position) %in% unique(data$pos_grouped)], guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +

        plot_defaults +
        ggplot2::labs(
          title = paste(selected_class, "Draft - ELO Entwicklung"),
          subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem ELO-Wert am Ende einer Saison.\nDie Picks des Teams werden farbig hervorgehoben und zeigen die ELO-Werte am Ende jeder RFL Regular Season.\nJe heller der Punkt, desto länger ist die Saison her. Der große Punkt ist immer die letzte Saison des Spielers."),
          x = "Overall Pick im RFL Draft",
          y = "ELO am Ende jeder RFL Regular Season"
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(size = 16),
          plot.subtitle = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 11)
        )
    }, height = 700)

    ### output ----
    detail <- shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          shinycssloaders::withSpinner(reactable::reactableOutput(paste0("team_draft_class_picks_", index))),
          width = 12
        ),
        shiny::column(
          shinycssloaders::withSpinner(shiny::plotOutput(paste0("team_draft_class_value_", index))),
          width = 6
        ),
        shiny::column(
          shinycssloaders::withSpinner(shiny::plotOutput(paste0("team_draft_class_elo", index))),
          width = 6
        )
      )
    )

    detail
  }

  reactable::reactable(
    rfl_draft_classes_sum_filtered(),
    columns = list(
      class = reactable::colDef(name = "", sticky = "left"),
      picks = reactable::colDef(
        name = "Picks",
      ),
      rank = reactable::colDef(
        name = "Platzierung im Draftjahr",
        style = function(value) {
          value <- as.numeric(value)

          if (value > 6) {
            color <- color_cyan
          } else if (value > 12) {
            color <- color_green
          } else if (value > 18) {
            color <- color_yellow
          } else if (value > 24) {
            color <- color_orange
          } else if (value > 30) {
            color <- color_red
          } else {
            color <- color_blue
          }

          list(color = color)
        }
      ),
      value = reactable::colDef(
        name = "Value",
        sortable = TRUE
      ),
      value_pctl = reactable::colDef(
        name = "Perzentil seit 2017",
        cell = JS('function(cellInfo) {
          // Format as percentage
          const pct = (cellInfo.value * 100).toFixed(1) + "%"
          // Pad single-digit numbers
          let value = pct.padStart(5)
          // Show % on first row only
          if (cellInfo.viewIndex > 0) {
            value = value.replace("%", " ")
          }
          // Render bar chart
          return `
            <div class="bar-cell">
              <span class="number">${value}</span>
              <div class="bar-chart" style="background-color: #e1e1e1">
                <div class="bar" style="width: ${pct}; background-color: #fc5185"></div>
              </div>
            </div>
          `
        }'),
        html = TRUE
      ),
      season = reactable::colDef(show = FALSE),
      season_date = reactable::colDef(show = FALSE),
      franchise_id = reactable::colDef(show = FALSE),
      franchise_name = reactable::colDef(show = FALSE)
    ),
    defaultSorted = "value",
    defaultSortOrder = "desc",
    sortable = FALSE,
    showSortable = TRUE,
    defaultPageSize = 12,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(25, 50, 100),
    onClick = "expand",
    resizable = TRUE,
    details = row_details,
    wrap = FALSE,
    compact = TRUE,
    highlight = TRUE,
    theme = reactableTheme(
      borderColor = color_grey_light,
    ),
    #virtual = TRUE,
    height = 800,
    searchable = TRUE
  )
})


## trades ----
draft_class_trades <- shiny::reactive({
  draft_class_trades <- rfl_trades_data %>%
    filter(season <= input$selectYears[1]) %>%
    #filter(season <= 2025) %>%
    dplyr::group_by(trade_id) %>%
    dplyr::mutate(
      franchise_ids = paste(franchise_id, collapse = ","),
      asset_ids = paste(asset_id, collapse = ","),
      asset_names = paste(asset_name, collapse = "\n")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(grepl(input$selectRflTeams, franchise_ids) & (grepl(paste0(input$selectRflTeams, "_", input$selectYears[1]), asset_ids) | grepl(input$selectYears[1], asset_names))) %>%
    #dplyr::filter(grepl("0001", franchise_ids) & (grepl(paste0("0001_", 2025), asset_ids) | grepl(2025, asset_names))) %>%
    dplyr::mutate(
      side = ifelse(franchise_id == input$selectRflTeams, "sent", "received"),
      #side = ifelse(franchise_id == "0001", "sent", "received")
    ) %>%
    dplyr::select(season, date, trade_id, side, asset_name, asset_id) %>%
    tidyr::separate(asset_id, into = c("prefix", "pick_owner", "pick_year", "pick_round"), sep = "_", remove = FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pick_year = dplyr::case_when(
        prefix == "DP" ~ stringr::word(asset_name, 2),
        TRUE ~ pick_year
      ),
      pick_round = dplyr::case_when(
        prefix == "FP" ~ as.integer(pick_round),
        prefix == "DP" ~ as.integer(pick_owner) + 1
      )
    ) %>%

    # add draft order um pick für getradete future picks zu erhalten
    dplyr::left_join(
      rfl_draft_orders %>%
        dplyr::mutate(pick_year = as.character(season)) %>%
        dplyr::select(-season),
      by = c("pick_year", "pick_owner" = "franchise_id")
    ) %>%

    # add franchise name to picks where no future draft order is available
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name) %>%
        dplyr::rename(current_franchise_name = franchise_name),
      by = c("pick_owner" = "franchise_id")
    ) %>%

    dplyr::mutate(
      franchise_name = ifelse(is.na(franchise_name) & !is.na(current_franchise_name), current_franchise_name, franchise_name),
      asset_id_new = dplyr::case_when(
        # alle ehemaligen future picks, die jetzt in der gegenwart sind, erhalten eine ID für den aktuellen draft
        prefix == "FP" & pick_year <= input$selectYears[1] ~ paste("DP", pick_round - 1, pick - 1, pick_year, sep = "_"),
        prefix == "DP" ~ paste0(asset_id, "_", pick_year),
      TRUE ~ asset_id
      ),
      #asset_id_new = dplyr::case_when(
      # alle ehemaligen future picks, die jetzt in der gegenwart sind, erhalten eine ID für den aktuellen draft
      #  prefix == "FP" & pick_year <= 2025 ~ paste("DP", pick_round - 1, pick - 1, pick_year, sep = "_"),
      #  prefix == "DP" ~ paste0(asset_id, "_", pick_year),
      #  TRUE ~ asset_id
      #),
      pick = dplyr::case_when(
        prefix == "DP" ~ as.integer(stringr::word(asset_id, 3, sep = "_")) + 1,
        TRUE ~ as.integer(pick)
      )
    ) %>%
    arrange(side) %>%
    dplyr::left_join(
      rfl_drafts_data %>%
        dplyr::mutate(
          round = as.integer(round),
          player_name = paste0(player_name, " (", pos, ", ", team, ")"),
          pick_team_id = franchise_id,
          pick_year = as.character(season)
        ) %>%
        dplyr::select(pick_year, round, pick, player_name, pick_team_id),
      by = c("pick_year", "pick_round" = "round", "pick")
    ) %>%
    dplyr::mutate(
      asset_name = ifelse(
        prefix == "FP",
        paste(asset_name, franchise_name),
        asset_name
      )
    ) %>%
    dplyr::bind_rows(as_tibble(lapply(df, \(x) NA), .name_repair = "unique")) %>%  # neue zeile notwendig, damit beim mergen die colnames existieren. sonst kommt es bei teams ohen trades zu fehlern
    dplyr::select(season:pick_team_id, -prefix, -pick_owner, -current_franchise_name)
})

draft <- shiny::reactive({
  draft <- rfl_drafts_data %>%
    #dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    #dplyr::filter(season == 2025) %>%
    dplyr::left_join(
      mfl_adp_data %>%
        dplyr::mutate(adp = (rfl_min + rfl_max) / 2) %>%
        dplyr::select(season, mfl_id, adp),
      by = c("season", "mfl_id")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      side = "received",
      asset_id_new = paste("DP", as.integer(round) - 1, pick - 1, season, sep = "_"),
      first_pick_new = first_pick,
      second_pick_new = second_pick,
      third_pick_new = third_pick,
      across(
        c(first_pick_new, second_pick_new, third_pick_new),
        ~ ifelse(.x == overall, paste0("**", .x, "**"), as.character(.x))
      ),
      asset_name = paste(season, paste0(as.integer(round), ".", sprintf("%02d", pick)), player_name, paste0("(", pos, ", ", team, ")")),
      text_rfl =  paste("RFL:", paste(na.omit(c(first_pick_new, second_pick_new, third_pick_new)), collapse = ", ")),
      text_adp = paste0("ADP: ", adp),
      subline = ifelse(
        !is.na(adp),
        paste(text_rfl, text_adp, sep = " - "),
        text_rfl
      )
    ) %>%
    dplyr::rename(date = timestamp)
})

draft_team <- shiny::reactive({
  draft_team <- draft() %>%
    dplyr::filter(franchise_id %in% input$selectRflTeams) %>%
    #dplyr::filter(franchise_id == "0001") %>%
    dplyr::mutate(
      xstart = dplyr::case_when(
        overall == second_pick ~ first_pick,
        is.na(second_pick) | is.na(third_pick) ~ max(draft()$overall),
        TRUE ~ second_pick
      )
    )
})

## atkuelle draft klasse ----
output$team_current_draft_class <- shiny::renderPlot({
  #shiny::validate(
  #  shiny::need(input$selectYears[2] - input$selectYears[1] == 0, "Bitte wähle exakt ein Jahr, um dir die ADP Daten einer einzelnen Draftklasse anzuschauen. Das machst du, indem du beide Saison-Regler auf das selbe Jahr stellst.")
  #)

  shiny::validate(
    shiny::need(input$selectYears[1] >= 2020, "ADP Daten gibt es erst seit dem Draft 2020. Stelle den Linken Saison-Regler auf mindestens 2020, um dir die ADP Daten anzeigen zu lassen.")
  )

  ggplot2::ggplot(draft(), ggplot2::aes(x = overall, y = adp, color = factor(pos_grouped, positions_grouped))) +
    ggplot2::geom_ribbon(
      formula = 'y ~ x',
      stat = "smooth",
      method = "loess",
      se = TRUE,
      fill = color_grey_light,
      color = color_grey_light,
      alpha = 0.3,
      linetype = 0
    ) +
    ggplot2::geom_smooth(formula = 'y ~ x', method = 'loess', se = FALSE, linewidth = 0.5, color = color_grey_mid, linetype = "dashed", show.legend = FALSE) +

    ggplot2::geom_point(color = color_grey_light, alpha = 1, size = 3) +

    ggforce::geom_link(data = draft_team(), ggplot2::aes(x = xstart, xend = overall, yend = adp, linewidth = ggplot2::after_stat(index))) +
    ggplot2::scale_linewidth_continuous(guide = "none") +
    ggplot2::geom_point(data = draft_team(), size = 5) +

    ggplot2::annotate("segment", x = max(draft()$overall) * 0.21, xend = max(draft()$overall) * 0.13, y = max(draft()$adp) * 0.6, yend = max(draft()$adp) * 0.6, arrow = ggplot2::arrow(), color = color_grey_dark) +
    ggplot2::annotate("segment", x = max(draft()$overall) * 0.2, xend = max(draft()$overall) * 0.2, y = max(draft()$adp) * 0.58, yend = max(draft()$adp) * 0.75, arrow = ggplot2::arrow(), color = color_grey_dark) +
    ggplot2::annotate("text", x = max(draft()$overall) * 0.18, y = max(draft()$adp) * 0.6, label = "Reaches", hjust = 0.5, vjust = -1) +

    ggplot2::annotate("segment", x = max(draft()$overall) * 0.8, xend = max(draft()$overall) * 0.88, y = max(draft()$adp) * 0.1, yend = max(draft()$adp) * 0.1, arrow = ggplot2::arrow(), color = color_grey_dark) +
    ggplot2::annotate("segment", x = max(draft()$overall) * 0.81, xend = max(draft()$overall) * 0.81, y = max(draft()$adp) * 0.12, yend = max(draft()$adp) * -0.05, arrow = ggplot2::arrow(), color = color_grey_dark) +
    ggplot2::annotate("text", x = max(draft()$overall) * 0.83, y = max(draft()$adp) * 0.1, label = "Steals", hjust = 0.5, vjust = 2) +

    ggrepel::geom_label_repel(
      data = draft_team(),
      ggplot2::aes(label = paste(player_name, paste0("(", pos, ", ", team, ")"))),
      point.padding = 10,
      size = 6,
      alpha = 1,
      show.legend = FALSE,
      nudge_y = 12,
    ) +

    # genutzte positionen werden aus palette gefiltert, damit nur die nötigen farben genutzt werden und kein grau
    ggplot2::scale_color_manual(values = colors_position[names(colors_position) %in% unique(draft()$pos_grouped)], guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +
    ggplot2::scale_x_continuous(breaks = seq(1, max(draft()$overall, na.rm = TRUE), by = 36), labels = seq(1, max(draft()$overall, na.rm = TRUE), by = 36), minor_breaks = seq(1, max(draft()$overall, na.rm = TRUE), by = 36/3), limits = c(0, max(draft()$overall, na.rm = TRUE) + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq(1, max(draft()$adp, na.rm = TRUE), by = 36), labels = seq(1, max(draft()$adp, na.rm = TRUE), by = 36)) +
    plot_defaults +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_line(color = color_grey_mid, linewidth = 0.35),
      panel.grid.minor.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste(rfl_franchise_data$franchise_name[rfl_franchise_data$franchise_id == input$selectRflTeams], "RFL Draftklasse", input$selectYears[1], "nach ADP"),
      subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem Overall Pick und ihrer zugehörigen ADP.\nDer Schweif zeigt den Abstand zur 2. Copy des Spielers an."),
      x = "Overall Pick im RFL Draft",
      y = "Average Draft Position",
      color = "Position"
    )
}, height = 800)


## output übersicht ----
output$single_draft_class <- gt::render_gt({
  req(input$selectRflTeams)

  combined_data <- dplyr::bind_rows(
    lapply(
      list(draft_team() %>% dplyr::select(season, date, side, asset_id_new, asset_name, subline), draft_class_trades()),
      #list(draft_team %>% dplyr::select(season, date, side, asset_id_new, asset_name, subline), draft_class_trades),
      function(x) {
        if (is.data.frame(x) && nrow(x) > 0) x else NULL
      }
    )
  ) %>%
    # filter alle erhaltene picks, mit denen dann gepickt wurde, raus
    dplyr::group_by(asset_id_new) %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::filter(dplyr::row_number() == 1 & !is.na(date)) %>%

    dplyr::mutate(
      side = ifelse(side == "sent", "Abgänge", "Zugänge"),
      type = dplyr::case_when(
        "asset_id_new" %in% names(.) && grepl("FP_", asset_id_new) ~ "3_future_pick",
        "asset_id_new" %in% names(.) && grepl("DP_", asset_id_new) ~ "2_pick",
        TRUE ~ "1_Player"
      ),
      date = dplyr::first(format(date, "%d.%m.%Y")),
      player_info = ifelse(!is.na(pick_round) & !is.na(pick), paste0(as.integer(pick_round), ".", sprintf("%02d", as.integer(pick)), " ", player_name), ""),
      asset_name = dplyr::case_when(
        # für draftpicks
        !is.na(subline) ~ paste(asset_name, subline, sep = "\n"),
        !is.na(player_name) & pick_year <= input$selectYears[1] ~ paste(pick_year, player_info),
        !is.na(player_name) & new_season_march > input$selectYears[1] + 2 ~ paste(asset_name, player_info, sep = "\n"), # zeige gepickte spieler erst 2 jahre nach der gewählten draftklasse
        pick_year > input$selectYears[1] & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
        #!is.na(player_name) & pick_year <= 2025 ~ paste(pick_year, player_info),
        #!is.na(player_name) & new_season_march > 2025 + 2 ~ paste(asset_name, player_info, sep = "\n"),
        #pick_year > 2025 & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
        TRUE ~ asset_name
      )
    ) %>%
    dplyr::group_by(side, type) %>%
    dplyr::arrange(dplyr::desc(side), type, !!!if ("pick_year" %in% names(.)) rlang::syms("pick_year") else NULL, asset_name) # !!! und rlang::syms() erlauben es, Spaltennamen programmatisch einzufügen.

  row_index_line_through <- which(combined_data$side == "Zugänge" & combined_data$pick_team_id != input$selectRflTeams)[1]
  #row_index_line_through <- which(combined_data$side == "Zugänge" & combined_data$pick_team_id != "0001")[1]
  row_index_first_pick <- which(is.na(combined_data$trade_id))[1]
  row_index_future_picks <- which(combined_data$pick_year > input$selectYears[1])[1]
  #row_index_future_picks <- which(combined_data$pick_year > 2025)[1]

  table <- combined_data %>%
    dplyr::group_by(side) %>%
    dplyr::select(date, side, asset_name, pick_team_id) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste(rfl_franchise_data$franchise_name[rfl_franchise_data$franchise_id == input$selectRflTeams], "Draftklasse", input$selectYears[1])
    ) %>%
    gtDefaults() %>%
    gt::tab_style(
      style = gt::cell_text(decorate = "line-through"),
      locations = cells_body(
        columns = asset_name,
        rows = side == "Zugänge" & pick_team_id != input$selectRflTeams
        #rows = side == "Zugänge" & pick_team_id != "0001"
      )
    ) %>%
    gt::fmt_markdown(columns = asset_name) %>%
    gt::cols_hide(c(side, pick_team_id)) %>%
    gt::cols_label(
      date = "Datum",
      asset_name = "Asset"
    )

  if (!is.na(row_index_line_through)) {
    table <- table %>%
      gt::tab_footnote(
        footnote = "Durchgestrichenen Picks wurden nicht vom dem ausgewählten Team getätigt, sondern wurden weiter getradet.",
        locations = cells_body(
          columns = asset_name,
          rows = row_index_line_through
        )
      )
  }

  if (!is.na(row_index_first_pick)) {
    table <- table %>%
      gt::tab_footnote(
        footnote = "Der hervorgehobene RFL Pick ist der Spot, an dem das ausgewählte Team den Spieler gewählt hat.",
        locations = cells_body(
          columns = asset_name,
          rows = row_index_first_pick
        )
      )
  }

  if (!is.na(row_index_future_picks)) {
    table <- table %>%
      gt::tab_footnote(
        footnote = "Für Future Picks, die weniger als 2 Jahre in der Zukunft der Draftklasse sind, werden keine Spieler angezeigt.",
        locations = cells_body(
          columns = asset_name,
          rows = row_index_future_picks
        )
      )
  }

  table

  # TODO: add UDFAs
})

## output trades ----
output$single_draft_class_trades <- DT::renderDataTable({
  req(input$selectRflTeams)

  DT::datatable(
    draft_class_trades() %>%
      dplyr::filter(!is.na(trade_id)) %>%
      dplyr::mutate(
        player_info = paste(paste0(as.integer(pick_round), ".", sprintf("%02d", as.integer(pick))), player_name),
        asset_name = dplyr::case_when(
          !is.na(player_name) & pick_year <= input$selectYears[1] ~ paste(pick_year, player_info),
          !is.na(player_name) & new_season_march > input$selectYears[1] + 2 ~ paste(asset_name, player_info, sep = "\n"), # zeige gepickte spieler erst 2 jahre nach der gewählten draftklasse
          pick_year > input$selectYears[1] & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
          #!is.na(player_name) & pick_year <= 2025 ~ paste(pick_year, player_info),
          #!is.na(player_name) & new_season_march > 2025 + 2 ~ paste(asset_name, player_info, sep = "\n"),
          #pick_year > 2025 & !is.na(pick) ~ paste(asset_name, paste0("(", pick_round, ".", sprintf("%02d", as.integer(pick)), ")")),
          TRUE ~ asset_name
        )
      ) %>%
      dplyr::group_by(trade_id, side) %>%
      dplyr::arrange(asset_name) %>%
      dplyr::summarise(
        asset_name = paste(asset_name, collapse = "\n"),
        date = dplyr::first(format(date, "%d.%m.%Y")),
        .groups = "drop"
      ) %>%
      tidyr::spread(side, asset_name) %>%
      dplyr::select(-trade_id) %>%
      dplyr::rename(
        "Geholt" = received,
        "Abgegeben" = sent,
        Datum = date
      ),
    rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 20, scrollY = "600px")
  )
})

draft_elo <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & is_rookie == 1 & !is.na(elo_peak)) %>%
  dplyr::group_by(season, franchise_id) %>%
  dplyr::summarise(elo_total = sum(elo_peak), .groups = "drop") %>%
  dplyr::mutate(pctl_total = round(dplyr::percent_rank(elo_total), 2)) %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(season_pctl = round(dplyr::percent_rank(elo_total), 2))

# ui output ----
output$draft_classes_selected_team <- shiny::renderUI({
  req(input$selectRflTeams)

  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(reactable::reactableOutput("team_draft_classes")),
      width = 12
    ),
    style = "height: 800px"
  )
})


output$draft_class_selected_team <- shiny::renderUI({
  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(reactable::reactableOutput("team_draft_class")),
      width = 12
    ),
    style = "height: 800px"
  )
})

output$draft_overview <- shiny::renderUI({
  req(input$selectYears[1] < new_season_march)
  #req(!input$selectRflTeams)

  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(shiny::plotOutput("draft_classes_overview")),
      width = 12
    ),
    style = "height: 800px"
  )
})

output$team_draft_classes <- shiny::renderUI({
  shiny::validate(
    shiny::need(length(input$selectRflTeams) == 1, "Bitte wähle exakt ein Team, um dir dessen Draftklassen anzuschauen.")
  )

  shiny::fluidRow(
    shiny::column(
      width = 12,
      shinycssloaders::withSpinner(shiny::plotOutput("team_current_draft_class")),
      shinycssloaders::withSpinner(shiny::plotOutput("team_draft_class_elo"))
    )
  )
})

output$team_draft_class_ui <- shiny::renderUI({
  shiny::validate(
    shiny::need(input$selectYears[2] - input$selectYears[1] == 0, "Bitte wähle exakt ein Jahr, um dir eine einzelne Draftklasse anzuschauen. Das machst du, indem du beide Saison-Regler auf das selbe Jahr stellst.")
  )

  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(gt::gt_output("single_draft_class")),
      width = 5
    ),
    shiny::column(
      tags$h4("Alle Trades mit Picks dieses Drafts"),
      shinycssloaders::withSpinner(DT::DTOutput("single_draft_class_trades")),
      # TODO: meldung, wenn keine trades gefunden wurden
      width = 7
    )
  )
})
