# daten ----
pal_pvar <- scale_rainbow(range(rfl_war_data$pvar, na.rm = TRUE))
pal_pvar_text <- scale_rainbow_text(range(rfl_war_data$pvar, na.rm = TRUE))
pal_voe <- scale_red_green(range(rfl_drafts_data$voe, na.rm = TRUE))

# fitere draft daten
rfl_draft_classes <- shiny::reactive({
  rfl_draft_classes <- rfl_drafts_data %>%
    dplyr::filter(season > 2016 & season < new_season_march) %>%

    #filter(franchise_id == "0001") %>%
    #dplyr::filter(season == 2025) %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::filter(round >= input$selectDraftRounds[1] & round <= input$selectDraftRounds[2])
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# berechne werte für gesamte klasse
rfl_draft_classes_sum <- rfl_drafts_data %>%
  dplyr::filter(season > 2016) %>%
  group_by(season, franchise_id, franchise_name, class) %>%
  dplyr::summarise(
    picks = n(),
    pvar = sum(pvar),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    id = paste(season, franchise_id, sep = "_"),
    voe_pctl = dplyr::percent_rank(pvar)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(dplyr::desc(pvar)) %>%
  dplyr::mutate(
    rank = dplyr::row_number(),
    season_date = as.Date(paste0(season, "-01-01"))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(id, season, season_date, franchise_id, franchise_name, class, picks, rank, pvar, voe_pctl)

# filtere gesamte klasse
rfl_draft_classes_sum_filtered <- shiny::reactive({
  rfl_draft_classes_sum_filtered <- rfl_draft_classes_sum %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        franchise_id %in% input$selectRflTeams
      else
        TRUE
    )
  #head(2)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ranking aller draftklassen ----
## overview ----
output$draft_classes_overview <- shiny::renderPlot({
  ggplot2::ggplot(rfl_draft_classes_sum, ggplot2::aes(x = season_date, y = reorder(franchise_name, dplyr::desc(franchise_name)), fill = rank)) +
    ggplot2::geom_tile(ggplot2::aes(key = id), color = color_bg) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = rank,
        fontface = "bold",
        size = 8
      ),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient2(high = color_red, mid = color_yellow, low = color_green, midpoint = 18, guide = "none") +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "'%y", expand = c(0, 0)) +
    plot_defaults +
    ggplot2::labs(
      title = "RFL Draftklassen Überblick",
      subtitle = "Dargestellt werden alle RFL Draftklassen seit 2017 mit ihrem Ranking nach kumulierten pVAR der Draftpicks",
      x = "Saison"
    ) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  #plotly::ggplotly(plot, source = "draft_classes_overview", height = 800) %>%
  #  plotly::event_register("plotly_click") %>%
  #  plotly::layout(
  #    font = list(family = font),
  #    title = list(font = list(family = var.fontHeadline, size = 24)),
  #    yaxis = list(font = list(size = 6))
  #  )
  # TODO: interaktiv machen
}, height = 800)

# input bei klick setzen
#observeEvent(event_data("plotly_click", source = "draft_classes_overview"), {

#  click_data <- event_data("plotly_click", source = "draft_classes_overview")
#  print(click_data)
#  req(click_data)

#  selected_class <- rfl_draft_classes_sum[
#    rfl_draft_classes_sum$id == click_data$key,
#  ]

#  print(selected_class)

#  updateSliderInput(
#    session,
#    "selectYears",
#    value = c(selected_class$season, selected_class$season)
#  )

#  updatePickerInput(
#    session,
#    "selectRflTeams",
#    selected = selected_class$franchise_id
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
    dplyr::arrange(season) %>%
    dplyr::mutate(
      year = dplyr::row_number(), # für alpha in plots besser als jahreszahl
      last_season = ifelse(elo_season == max(elo_season), 1, 0)
    ) %>%
    dplyr::ungroup()

  row_details <- function(index) {
    #selected_class <- rfl_draft_classes_sum_filtered()[index, ]$class
    selected_class <- "2019 Dresden Jaguars"

    data_filtered <- data %>%
      dplyr::filter(.data$class == selected_class)

    data_year <- data_filtered %>%
      dplyr::first() %>%
      #mutate(season = 2019) %>%
      dplyr::pull(season)

    data_franchise_id <- data_filtered %>%
      dplyr::first() %>%
      #mutate(franchise_id = "0019") %>%
      dplyr::pull(franchise_id)

    ### picks ----
    output[[paste0("team_draft_class_picks_", index)]] <- reactable::renderReactable({
      draft_transactions <- rfl_transactions_draft %>%
        dplyr::filter(franchise_id == data_franchise_id | receiving_franchise_id == data_franchise_id) %>%
        #mutate(month = lubridate::month(date)) %>%
        dplyr::filter(
          (season == data_year & grepl("^DP_", asset_ids)) |
            (season == data_year & grepl("^DP_", asset_id_new)) |
            grepl(paste0("FP_.*_", data_year), asset_ids)
        ) %>%
        dplyr::mutate(
          side = case_when(
            receiving_franchise_id == data_franchise_id ~ "received",
            TRUE ~ side
          ),
          side = ifelse(side == "sent", "Abgänge", "Zugänge")
        ) %>%
        #dplyr::filter(side == "Zugänge") %>%
        dplyr::group_by(asset_id_new) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(
          # filtere gruppen, wo das gleiche asset zuerst ein zugang und dann ein abgang war (Zugang bleibt, Abgang wird gelöscht)
          !(
            dplyr::row_number() == 1 &
              dplyr::first(side) == "Zugänge" &
              dplyr::last(side)  == "Abgänge"
          ),
          # wenn eine gruppe mehr als eine zeil ehat, enferne die ohne trade_id
          !(
            dplyr::n() > 1 &
              is.na(trade_id)
          )
        ) %>%
        dplyr::mutate(
          # füge pick_cat hinzu
          asset_name = dplyr::case_when(
            !is.na(pick_cat) & (franchise_id == data_franchise_id | pick_team_id == data_franchise_id) & side == "Zugänge" ~
              paste0(asset_name, "<span class=\"badge ", pick_cat, "\">", pick_cat, "</span>"),
            TRUE ~ asset_name
          ),

          asset_name = dplyr::case_when(
            !is.na(subline) & side == "Zugänge" &
              ((pick_team_id == data_franchise_id) |
                 (franchise_id == data_franchise_id)) ~ paste(asset_name, paste0("<small>", subline, "</small>"), sep = "<br/>"),
            TRUE ~ asset_name
          ),
          # FP die nicht vom Team gepickt werden durchstreichen
          asset_name = ifelse(!is.na(pick_team_id) & pick_team_id != data_franchise_id & side == "Zugänge", paste0("<s>", asset_name, "</s>"), asset_name),
        ) %>%
        dplyr::ungroup() %>%
        #filter(asset_id == "FP_0018_2025_4")
        dplyr::select(side, date, asset_name, type, pick_year, pick_round, pick, pvar, voe)

      reactable::reactable(
        draft_transactions,
        columns = list(
          side = reactable::colDef(name = "", defaultSortOrder = "desc"),
          date = reactable::colDef(name = "Datum", format = reactable::colFormat(date = TRUE, locales = "de-DE"), minWidth = 150),
          asset_name = reactable::colDef(
            name = "Asset",
            html = TRUE,
            cell = function(value, index) {
              tagList <- shiny::tagList(
                htmltools::div(htmltools::HTML(draft_transactions$asset_name[index]))
              )

              as.character(tagList)
            },
            minWidth = 350
          ),
          pick_round = reactable::colDef(name = "RD", minWidth = 50),
          pvar = reactable::colDef(
            name = "pVAR",
            aggregate = "sum",
            format = reactable::colFormat(digits = 2),
            style = function(value) {
              list(
                background = pal_pvar(value),
                color = pal_pvar_text(value),
                textAlign = "center"
              )
            },
            minWidth = 50
          ),
          voe = reactable::colDef(
            name = "VOE",
            aggregate = "sum",
            cell = function(value) {
              if (is.na(value)) return(NA)

              if (value > 0) {
                paste0("+", value)
              } else {
                value
              }
            },
            style = function(value) {
              list(color = pal_voe(value), textAlign = "center", fontWeight = "bold")
            },
            minWidth = 50
          ),
          type = reactable::colDef(show = FALSE),
          pick_year = reactable::colDef(show = FALSE),
          pick = reactable::colDef(show = FALSE)
        ),
        defaultColDef = colDef(
          headerStyle = list(background = color_bg),
          vAlign = "center",
          headerVAlign = "bottom"
        ),
        defaultSorted = c("side", "type", "pick_year", "pick_round", "pick", "date"),
        groupBy = "side",
        sortable = FALSE,
        searchable = TRUE,
        striped = TRUE,
        pagination = FALSE,
        highlight = TRUE,
        defaultExpanded = TRUE,
        theme = reactableTheme(
          borderColor = color_grey_light,
          stripedColor = color_bg
        )
      )
    })

    ### value ----
    output[[paste0("team_draft_class_value_", index)]] <- shiny::renderPlot({
      ggplot2::ggplot(subset(data_filtered, last_season == 1), ggplot2::aes(x = overall, y = pvar, color = factor(pos_grouped, positions_grouped))) +
        ggplot2::geom_smooth(
          data = rfl_draft_pvar_exp,
          ggplot2::aes(y = pvar_exp),
          se = FALSE,
          color = color_red,
          size = 0.5
        ) +
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
          title = paste(selected_class, "Value over Expected"),
          #subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem derzeitigen Wert."),
          x = "Overall Pick im RFL Draft",
          y = "pVAR"
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(size = 16),
          plot.subtitle = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 11)
        )

    }, height = 700)
    # TODO: höhe dynamisch nach anzahl der spieler

    ### elo ----
    output[[paste0("team_draft_class_elo", index)]] <- shiny::renderPlot({
      shiny::validate(
        shiny::need(input$selectYears[2] < new_season_march, "Für die aktuelle Draftklasse gibt es noch keine ELO-Daten. Wähle mit dem zweiten Saison-Regler eine frühere Saison.")
      )

      ggplot2::ggplot(data = subset(data_filtered, last_season == 1), ggplot2::aes(y = reorder(player_name, overall, decreasing = TRUE), x = elo_season_end, color = factor(pos_grouped, positions_grouped), alpha = year)) +
        ggplot2::geom_vline(xintercept = 1500, color = color_red, alpha = 0.5) +
        ggplot2::geom_point(size = 10, alpha = 1) +
        ggplot2::geom_point(data = subset(data_filtered, last_season == 0), size = 5) +
        ggplot2::scale_alpha_continuous(range = c(0.2, 0.9), guide = "none") +

        ggplot2::scale_color_manual(values = colors_position[names(colors_position) %in% unique(data$pos_grouped)], guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +

        plot_defaults +
        ggplot2::labs(
          title = paste(selected_class, "Draft - ELO Entwicklung"),
          #subtitle = paste("Angezeigt werden alle",  paste(input$selectPositions, collapse = ", "), "Picks aus den Runden", paste0(input$selectDraftRounds[1], "-", input$selectDraftRounds[2]), "mit ihrem ELO-Wert am Ende einer Saison.\nDie Picks des Teams werden farbig hervorgehoben und zeigen die ELO-Werte am Ende jeder RFL Regular Season.\nJe heller der Punkt, desto länger ist die Saison her. Der große Punkt ist immer die letzte Saison des Spielers."),
          x = "ELO am Ende jeder RFL Regular Season",
        ) +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(size = 16),
          plot.subtitle = ggplot2::element_text(size = 11),
          axis.title = ggplot2::element_text(size = 11),
          axis.title.y = ggplot2::element_blank()
        )
    }, height = 700)

    ### bewertung ----
    output[[paste0("team_draft_class_grades", index)]] <- reactable::renderReactable({
      selected_grades <- rfl_draft_grades %>%
        dplyr::filter(team_id == data_franchise_id) %>%
        #filter(team_id == "0001") %>%
        dplyr::select(-team_id, -pick)

      asset_groups <- selected_grades %>%
        dplyr::group_by(asset_name, user) %>%
        dplyr::filter(year == max(year)) %>%
        dplyr::group_by(asset_name, asset_id_new) %>%
        dplyr::summarise(grade = round(mean(grade), 1), .groups = "drop")

      selected_transactions <- rfl_transactions_draft %>%
        dplyr::filter(grepl(data_franchise_id, franchise_ids) & (grepl(paste0(data_franchise_id, "_", data_year), asset_ids) | grepl(data_year, asset_names)))

      reactable::reactable(
        asset_groups,
        columns = list(
          asset_name = reactable::colDef(
            name = "",
            html = TRUE
          ),
          grade = reactable::colDef(
            name = "⌀Bewertung",
            #  aggregate = "mean",
            #  format = reactable::colFormat(digifts = 1)
          ),
          asset_id_new = reactable::colDef(show = FALSE)
        ),
        details = function(index) {
          selection <- asset_groups[index, ]
          #selection <- asset_groups[3, ]

          selected_analysis <- selected_grades %>%
            dplyr::filter(asset_name == selection$asset_name)

          #### trades ----
          asset_trade_ids <- rfl_transactions_draft %>%
            dplyr::filter(asset_id_new == selection$asset_id_new) %>%
            dplyr::filter(!is.na(trade_id)) %>%
            dplyr::pull(trade_id)

          asset_trades <- rfl_trade_assets_per_side %>%
            dplyr::filter(trade_id %in% asset_trade_ids) %>%
            dplyr::group_by(trade_id) %>%
            dplyr::filter(any(franchise_id == data_franchise_id)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              trade_side = ifelse(franchise_id == data_franchise_id, "Abgegeben", "Geholt")
            ) %>%
            dplyr::select(-franchise_id) %>%
            tidyr::spread(trade_side, assets) %>%
            dplyr::arrange(date)

          output[[paste0("trades_", index)]] <- DT::renderDT({
            DT::datatable(
              asset_trades %>%
                dplyr::mutate(date = dplyr::first(format(date, "%d.%m.%Y"))) %>%
                dplyr::select(Datum = date, Geholt, Abgegeben),
              rownames = FALSE, options = list(dom = "Bfrtip", pageLength = 20, scrollY = "600px")
            )
          })

          output[[paste0("analysis_", index)]] <- reactable::renderReactable({
            reactable::reactable(
              selected_analysis,
              columns = list(
                user = reactable::colDef(name = "Analyst", minWidth = 100),
                year = reactable::colDef(name = "Analyse von", defaultSortOrder = "desc", minWidth = 100),
                text = reactable::colDef(name = "Analyse", style = list(whiteSpace = "pre-wrap"), minWidth = 400),
                grade = reactable::colDef(name = "Bewertung", aggregate = "mean", format = reactable::colFormat(digits = 1), minWidth = 80),
                asset_name = reactable::colDef(show = FALSE),
                order = reactable::colDef(show = FALSE),
                asset_id_new = reactable::colDef(show = FALSE)
              ),
              groupBy = c("user"),
              defaultSorted = c("user", "year"),
              wrap = TRUE,
              sortable = FALSE,
              defaultExpanded = TRUE,
              theme = reactableTheme(
                borderColor = color_grey_light,
                stripedColor = color_bg
              )
            )
          })

          trades_ui <- if (nrow(asset_trades) > 0) {
            htmltools::tagList(
              htmltools::h4("Trades"),
              shinycssloaders::withSpinner(
                DT::DTOutput(paste0("trades_", index))
              )
            )
          } else NULL

          htmltools::tagList(
            trades_ui,
            htmltools::h4("Analyse"),
            shinycssloaders::withSpinner(reactable::reactableOutput(paste0("analysis_", index)))
          )
        },
        #defaultSorted = c("order", "year"),
        striped = TRUE,
        wrap = TRUE,
        sortable = FALSE,
        pagination = FALSE,
        theme = reactableTheme(
          borderColor = color_grey_light,
          stripedColor = color_bg
        )
      )
    })

    ### output ----
    overview <- shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          htmltools::h4("Zu- & Abgänge der Draftklasse"),
          htmltools::div(
            htmltools::div("Durchgestrichenen Picks wurden nicht von dem Team getätigt, sondern wurden weiter getradet."),
            htmltools::div("Der hervorgehobene RFL Pick ist der Spot, an dem das Team den Spieler gewählt hat."),
            htmltools::div("Für Future Picks, die weniger als 2 Jahre in der Zukunft der Draftklasse sind, werden keine Spieler angezeigt."),
            if (data_year < 2020) {
              htmltools::div("Für Draftklassen vor 2020 gibt es keine ADP Daten.")
            }
          ),
          shinycssloaders::withSpinner(reactable::reactableOutput(paste0("team_draft_class_picks_", index))),
          htmltools::h4("Wertentwicklung der Picks"),
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

    grades <- shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          htmltools::h4("Bewertung der Draftklasse"),
          shinycssloaders::withSpinner(reactable::reactableOutput(paste0("team_draft_class_grades", index))),
          #htmltools::h4("Wertentwicklung der Picks"),
          width = 12
        ),
      )
    )

    # TODO: überschrift
    #htmltools::h3(paste(selected_class, "Draftanalyse")),
    shiny::tabsetPanel(
      shiny::tabPanel("Überblick", overview),
      if(data_year >= 2024) {
        shiny::tabPanel("Bewertung", grades)
      }
    )
  }

  reactable::reactable(
    rfl_draft_classes_sum_filtered,
    columns = list(
      id = reactable::colDef(show = FALSE),
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
      pvar = reactable::colDef(
        name = "pVAR",
        sortable = TRUE
      ),
      voe_pctl = reactable::colDef(
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
    defaultColDef = colDef(
      headerStyle = list(background = color_bg),
    ),
    defaultSorted = "pvar",
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

# überblick über alle draftklassen eines teams ----
output$team_draft_classes <- reactable::renderReactable({
  selected_teams <- rfl_draft_classes() %>%
    #dplyr::filter(franchise_id == "0007") %>%
    dplyr::filter(franchise_id %in% input$selectRflTeams & !is.na(player_name)) %>%
    dplyr::mutate(
      pos_order = dplyr::case_when(
        pos_grouped == "QB" ~ 1,
        pos_grouped == "RB" ~ 2,
        pos_grouped == "WR" ~ 3,
        pos_grouped == "TE" ~ 4,
        pos_grouped == "PK" ~ 5,
        pos_grouped == "DL" ~ 6,
        pos_grouped == "LB" ~ 7,
        pos_grouped == "DB" ~ 8,
      ),
      pos_grouped = factor(pos_grouped, levels = positions_grouped)
    ) %>%
    dplyr::arrange(pos_grouped) %>%
    dplyr::select(pos_order, franchise_name, season, player_name, pos_grouped, team, subline, round, overall, pvar)

  reactable::reactable(
    selected_teams,
    columns = list (
      #franchise_name = reactable::colDef(name = "Team"),
      pos_grouped = reactable::colDef(name = "Pos"),
      player_name = reactable::colDef(
        name = "Spieler",
        html = TRUE,
        cell = function(value, index) {
          tagList <- shiny::tagList(
            htmltools::div(htmltools::HTML(paste0(selected_teams$player_name[index], " (", selected_teams$pos_grouped[index], ", ", selected_teams$team[index], ")"))),
            htmltools::div(htmltools::HTML(paste0("<small>", selected_teams$subline[index], "</small>")))
          )

          as.character(tagList)
        },
        minWidth = 350
      ),
      season = reactable::colDef(name = "Saison"),
      round = reactable::colDef(name = "Runde", aggregate = "mean", format = reactable::colFormat(digits = 1)),
      overall = reactable::colDef(name = "Overall", aggregate = "mean", format = reactable::colFormat(digits = 0)),
      pvar = reactable::colDef(name = "pVAR", aggregate = "sum", format = reactable::colFormat(digits = 2)),
      pos_order = reactable::colDef(show = FALSE, aggregate = "mean"),
      subline = reactable::colDef(show = FALSE),
      team = reactable::colDef(show = FALSE)
    ),
    groupBy = c("pos_grouped"),
    defaultSorted = c("pos_order", "value"),
    sortable = FALSE,
    striped = TRUE,
    highlight = TRUE,
    wrap = FALSE,
    theme = reactableTheme(
      borderColor = color_grey_light,
    ),
    searchable = TRUE
  )

  # TODO: mehrere franchises ermöglichen
})

# TODO: pctl für draftklasse einfügen
# TODO: details als child table

# ui output ----
output$draft_overview <- shiny::renderUI({
  req(input$selectYears[1] < new_season_march)

  shiny::fluidRow(
    shiny::column(
      shinycssloaders::withSpinner(shiny::plotOutput("draft_classes_overview")),
      width = 12
    ),
    style = "height: 800px"
  )
})

output$draft_class_selected_team <- shiny::renderUI({
  shiny::fluidRow(
    shiny::column(
      htmltools::h2("Alle RFL Draftklassen"),
      shinycssloaders::withSpinner(reactable::reactableOutput("team_draft_class")),
      width = 12
    ),
    style = "height: 800px"
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

output$draft_classes_selected_team <- shiny::renderUI({
  req(input$selectRflTeams)
  req(length(input$selectRflTeams) == 1)

  shiny::fluidRow(
    shiny::column(
      htmltools::h2(paste0("Draftanalse")),
      shinycssloaders::withSpinner(reactable::reactableOutput("team_draft_classes")),
      width = 12
    ),
    style = "height: 800px"
  )
})
