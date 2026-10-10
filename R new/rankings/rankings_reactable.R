reactable_bar <- function(...) {
  reactablefmtr::data_bars(
    ..., text_position = "above", fill_color = c(color_red, color_orange, color_yellow, color_green, color_blue), background = color_grey_light
  )
}

standing_data_filtered <- shiny::reactive({
  standing_data_filtered <- rfl_standing_data %>%
    dplyr::filter(season == input$selectYear & week == input$selectWeek) %>%
    #dplyr::filter(season == 2026 & week == 3) %>%
    dplyr::select(franchise_id:conference_name, elo_post, dplyr::ends_with("_season"), wins_over_exp, wins_over_exp_pct, dplyr::ends_with("_rank"), dplyr::ends_with("_pctl"), dplyr::ends_with("emoji"), -elo_pre_pctl, seed_total)
})

team_data <- function(selected_team) {
  current_standing_by_season %>%
    dplyr::filter(franchise_id == selected_team)
}

team_id_at <- function(data, index) {
  data$franchise_id[[index]]
}

finish_sparkline <- function(chart, y_min, y_max, tooltip = NA) {
  chart <- chart %>%
    echarts4r::e_y_axis(min = y_min, max = y_max, show = FALSE) %>%
    echarts4r::e_x_axis(min = 1, max = 14, interval = 1, show = FALSE) %>%
    echarts4r::e_legend(FALSE)

  if (!is.na(tooltip)) {
    chart <- chart %>%
      echarts4r::e_tooltip(formatter = htmlwidgets::JS(tooltip), trigger = "axis") %>%
      htmlwidgets::onRender(
        htmlwidgets::JS(
          "function(el, x) { el.parentElement.style.overflow = 'visible'; }"
        )
      )
  }

  chart
}

fpts_bar_charts <- function(selected_team) {
  chart <- team_data(selected_team) %>%
    echarts4r::e_charts(x = week, height = "40px") %>%
    echarts4r::e_bar(serie = pp, stack = "grp", itemStyle = list(color = color_grey_light)) %>%
    echarts4r::e_bar(serie = pf, stack = "grp", itemStyle = list(color = color_green)) %>%
    finish_sparkline(
      y_min = min(current_standing_by_season$pf, na.rm = TRUE) - 30,
      y_max = max(current_standing_by_season$pp, na.rm = TRUE) + 10
      #tooltip = "
      #  function(params){
      #    return(
      #      '<strong>Woche ' + params[0].value[0] +
      #      '</strong><br />PP: ' + params[0].value[1] +
      #      '<br />PF: ' + params[1].value[1]
      #    );
      #  }
      #"
    )
}

elo_sparkline <- function(selected_team) {
  chart <- team_data(selected_team) %>%
    echarts4r::e_charts(x = week, height = "40px") %>%
    echarts4r::e_line(serie = elo_post, itemStyle = list(color = color_green)) %>%
    finish_sparkline(
      y_min = min(current_standing_by_season$elo_post, na.rm = TRUE) - 20,
      y_max = max(current_standing_by_season$elo_post, na.rm = TRUE) + 20,
      tooltip = "
        function(params){
          return(
            '<strong>Woche ' + params[0].value[0] +
            '</strong><br />ELO: ' + params[0].value[1]
          );
        }
      "
    )
}

pctl_color <- function(pctl) {
  grDevices::colorRampPalette(
    c(color_red, color_orange, color_yellow, color_green, color_blue)
  )(101)[round(pctl * 100) + 1]
}

team_chart_cell <- function(renderer) {
  function(value, index) {
    renderer(team_id_at(rfl_current_standing, index))
  }
}

reactable_header_with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help", title = tooltip, value)
}

reactable_bar_bg <- function(pctl_column, height = "3px", align = c("left", "right")) {
  align <- match.arg(align)
  palette <- grDevices::colorRampPalette(
    c(color_red, color_orange, color_yellow, color_green, color_blue)
  )(101)

  reactable::JS(sprintf(
    "function(rowInfo) {
      const rawWidth = rowInfo.row[%s];
      if (rawWidth == null || !Number.isFinite(Number(rawWidth))) return {};

      const width = Math.max(0, Math.min(1, Number(rawWidth)));
      const fill = %s[Math.round(width * 100)];
      const position = (width * 100) + '%%';
      const backgroundImage = %s === 'left'
        ? `linear-gradient(90deg, ${fill} ${position}, transparent ${position})`
        : `linear-gradient(90deg, transparent ${100 - width * 100}%%, ${fill} ${100 - width * 100}%%)`;

      return {
        backgroundImage: backgroundImage,
        backgroundSize: `100%% ${%s}`,
        backgroundRepeat: 'no-repeat',
        backgroundPosition: 'center bottom',
        fontSize: '0.85rem',
        borderRight: %s
      };
    }",
    jsonlite::toJSON(pctl_column, auto_unbox = TRUE),
    jsonlite::toJSON(unname(palette)),
    jsonlite::toJSON(align, auto_unbox = TRUE),
    jsonlite::toJSON(height, auto_unbox = TRUE),
    jsonlite::toJSON(paste("1px solid", color_grey_light), auto_unbox = TRUE)
  ))
}

reactable_cell_pctl <- function(value, index, value_col, rank, data) {
  shown_value <- data[[value_col]][index]

  # TODO: switch für pctl
  #if (isTRUE(show_pctl)) {
  #  pctl_col <- stringr::str_replace(value_col, "_rank", "_pctl")
  #  pctl <- data[[pctl_col]][index] * 100
#
#    last_one <- pctl %% 10
#    last_two <- pctl %% 100
#
#    ending <- dplyr::case_when(
#      last_two %in% 11:13 ~ "th",
#      last_one == 1       ~ "st",
#      last_one == 2       ~ "nd",
#      last_one == 3       ~ "rd",
#      TRUE                ~ "th"
#    )

  #  shown_value <- paste0(pctl, ending)
  #}

  htmltools::HTML(
    htmltools::HTML(paste0("<div><strong>", shown_value, "</strong></div>")),
    htmltools::HTML(paste0("<div><small>#", data[[rank]][index], "</small></div>"))
  )
}

reactable_coldef_bg_bar <- function(column, data, width = 75, ...) {
  reactable::colDef(
    ...,
    cell = function(value, index) {
      reactable_cell_pctl(value, index, rank, pctl, data)
    },
    html = TRUE,
    width = width,
    style = reactable_bar_bg(column),
  )
}

reactable_coldef_bg_pctl <- function(value_col, rank = NULL, data = NULL, width = 75, ...) {
  if (missing(rank) || is.null(rank)) {
    rank <- paste0({{value_col}}, "_rank")
  }

  reactable_coldef_bg(
    ...,
    cell = function(value, index) {
      reactable_cell_pctl(value, index, value_col, rank, data)
    },
    palette_fun = scale_rainbow(0:1),
    html = TRUE,
    width = width
  )
}

columns_to_hide <- names(rfl_standing_data)[
  stringr::str_detect(names(rfl_standing_data), "(_season|_rank)$") &
    !names(rfl_standing_data) %in% c("wins_season", "losses_season", "winloss_season")
]

output$current_standing <- reactable::renderReactable({
  input$showPctl

  data <- standing_data_filtered()

  reactable_default(
    data,
    columns = c(
      list(
        conference_name = reactable::colDef(name = "Conf"),
        franchise_name = reactable::colDef(
          name = "Team",
          html = TRUE,
          cell = function(value, index) {
            content <- shiny::tagList(
              htmltools::div(htmltools::HTML(value)),
              htmltools::div(htmltools::HTML(paste0("<small>", data$division_name[index], "</small>")))
            )

            as.character(content)
          },
          width = 250,
          sticky = "left",
          align = "left",
          style = list(borderRight = paste("2px solid", color_grey_light)),
        ),
        wins_season = reactable::colDef(
          name = "W",
          width = 50
        ),
        losses_season = reactable::colDef(
          name = "L",
          width = 50
        ),
        wins_over_exp_pct = reactable::colDef(
          header = reactable_header_with_tooltip("WoExp", "Wins over Expectation. Der Balken zeigt an, wie viel % von den erwarteten Siegen bereits erreicht wurden."),
          align = "left",
          cell = function(value, index) {
            woe <- round(data$wins_over_exp[index], 1)

            if (woe > 0) {
              woe <- paste0("+", woe)
            }

            width <- paste0(value * 100, "%")
            bar_chart(woe, width = width, height = "0.5rem")
          },
          minWidth = 100
        ),
        pf_sparkline = reactable::colDef(
          name = "PF & PP",
          cell = team_chart_cell(fpts_bar_charts),
          width = 150,
          style = list(borderRight = paste("2px solid", color_grey_light)),
        ),
        elo_season_pctl = reactable_coldef_bg_pctl(
          name = "ELO",
          "elo_post",
          "elo_season_rank",
          data
        ),
        pf_season_pctl = reactable_coldef_bg_pctl(
          header = reactable_header_with_tooltip("PF", "Points For bis zur ausgewählten Woche"),
          "pf_season",
          data = data
        ),
        pp_season_pctl = reactable_coldef_bg_pctl(
          header = reactable_header_with_tooltip("PP", "Potential Points bis zur ausgewählten Woche"),
          "pp_season",
          data = data
        ),
        pa_season_pctl = reactable_coldef_bg_pctl(
          header = reactable_header_with_tooltip("PA", "Points Against bis zur ausgewählten Woche"),
          "pa_season",
          data = data
        ),
        war_starter_season_pctl = reactable_coldef_bg_pctl(
          header = reactable_header_with_tooltip("WAR", "Wins above Replacement der gestarteten Spieler"),
          "war_starter_season",
          data = data
        ),
        wins_season_pctl = reactable_coldef_bg_pctl(
          name = "Wins",
          "wins_season",
          data = data
        ),
        all_play_wins_season_pctl = reactable_coldef_bg_pctl(
          name = "All-Play",
          "all_play_wins_season",
          data = data
        ),
        eff_season_pctl = reactable_coldef_bg_pctl(
          header = reactable_header_with_tooltip("Eff", "Effizienz (PF / PP)"),
          "eff_season",
          data = data
        ),
        luck_season_pctl = reactable_coldef_bg_pctl(
          name = "Glück",
          "luck_season",
          data = data
        ),
        quality_season_pctl = reactable_coldef_bg_pctl(
          name = "Qualität",
          "quality_season",
          data = data
        ),
        wins_expected_end_of_season_pctl = reactable_coldef_bg_pctl(
          header = reactable_header_with_tooltip("ExpW", "Expected Wins (Erwartete Siege)"),
          "wins_expected_end_of_season",
          data = data
        ),
        power_rank_emoji = reactable::colDef(
          "",
          width = 50
        ),
        true_standing_pctl = reactable_coldef_bg_pctl(
          name = "Ovrl",
          "power_rank",
          "power_rank",
          data
        ),
        bowl_emoji = reactable::colDef(
          name = "Bowl",
          html = TRUE,
          cell = function(value, index) {
            content <- shiny::tagList(
              htmltools::div(value),
              htmltools::div(htmltools::HTML(data$seed_emoji[index]))
            )
            as.character(content)
          },
          align = "center",
          width = 75,
          sticky = "right"
        ),
        franchise_id = reactable::colDef(show = FALSE),
        winloss_season = reactable::colDef(show = FALSE),
        division_name = reactable::colDef(show = FALSE),
        elo_post = reactable::colDef(show = FALSE),
        seed_emoji = reactable::colDef(show = FALSE),
        wins_over_exp = reactable::colDef(show = FALSE),
        seed_total = reactable::colDef(show = FALSE)
      ),
      purrr::set_names(
        purrr::map(
          columns_to_hide,
          ~ reactable::colDef(show = FALSE)
        ),
        columns_to_hide
      )
    ),
    columnGroups = list(
      colGroup(
        name = "Standing",
        columns = c("wins_season", "losses_season", "wins_over_exp_pct", "winloss_season")
      ),
      colGroup(
        name = "Power Ranking",
        columns = c("elo_season_pctl", "pf_season_pctl", "pp_season_pctl", "pa_season_pctl", "war_starter_season_pctl", "wins_season_pctl", "all_play_wins_season_pctl", "eff_season_pctl", "luck_season_pctl", "quality_season_pctl", "wins_expected_end_of_season_pctl")
      )
    ),
    groupBy = "conference_name",
    defaultSorted = c("seed_total"),
    defaultExpanded = TRUE,
    height = 800,
    borderless = TRUE
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

# ui output ----
output$league_ranking <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1(paste("RFL Ranking", "Woche", input$selectWeek, input$selectYear)),
      shiny::fluidRow(
        shiny::column(
          #shinyWidgets::prettySwitch("showPctl", "Zeige Perzentile statt Werte", value = FALSE, fill = TRUE, status = "primary"),
          shinycssloaders::withSpinner(reactable::reactableOutput("current_standing")),
          width = 12
      )
    )
  )
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)
