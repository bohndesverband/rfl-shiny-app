reactable_bar <- function(...) {
  reactablefmtr::data_bars(
    ..., text_position = "above", fill_color = c(color_red, color_orange, color_yellow, color_green, color_blue), background = color_grey_light
  )
}

current_standing_by_season <- rfl_weekly_standing %>%
  dplyr::filter(season == 2026)

current_week <- max(current_standing_by_season$week, na.rm = TRUE)

team_standing <- function(selected_team) {
  current_standing_by_season %>%
    dplyr::filter(franchise_id == selected_team)
}

team_id_at <- function(data, index) {
  data$franchise_id[[index]]
}

finish_sparkline <- function(chart, y_min, y_max, tooltip) {
  chart %>%
    echarts4r::e_y_axis(min = y_min, max = y_max, show = FALSE) %>%
    echarts4r::e_x_axis(min = 1, max = 14, interval = 1, show = FALSE) %>%
    echarts4r::e_legend(FALSE) %>%
    echarts4r::e_tooltip(formatter = htmlwidgets::JS(tooltip), trigger = "axis") %>%
    htmlwidgets::onRender(
      htmlwidgets::JS(
        "function(el, x) { el.parentElement.style.overflow = 'visible'; }"
      )
    )
}

fpts_bar_charts <- function(selected_team) {
  chart <- team_standing(selected_team) %>%
    echarts4r::e_charts(x = week, height = "40px") %>%
    echarts4r::e_bar(serie = pp, stack = "grp", itemStyle = list(color = color_grey_light)) %>%
    echarts4r::e_bar(serie = pf, stack = "grp", itemStyle = list(color = color_green)) %>%
    finish_sparkline(
      y_min = min(current_standing_by_season$pf, na.rm = TRUE) - 30,
      y_max = max(current_standing_by_season$pp, na.rm = TRUE) + 10,
      tooltip = "
        function(params){
          return(
            '<strong>Woche ' + params[0].value[0] +
            '</strong><br />PP: ' + params[0].value[1] +
            '<br />PF: ' + params[1].value[1]
          );
        }
      "
    )
}

elo_sparkline <- function(selected_team) {
  chart <- team_standing(selected_team) %>%
    echarts4r::e_charts(x = week, height = "40px") %>%
    echarts4r::e_line(serie = franchise_elo_postgame, itemStyle = list(color = color_green)) %>%
    finish_sparkline(
      y_min = min(current_standing_by_season$franchise_elo_postgame, na.rm = TRUE) - 20,
      y_max = max(current_standing_by_season$franchise_elo_postgame, na.rm = TRUE) + 20,
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

pctl_bar <- function(
  selected_team,
  pctl_column,
  value
) {
  #selected_team <- "0007"
  #pctl_column <- "franchise_elo_postgame_pctl"
  #value_column <- "franchise_elo_postgame"

  data <- rfl_current_standing %>%
    dplyr::filter(franchise_id == selected_team) %>%
    dplyr::mutate(
      category = "PCTL",
      background = 1,
      pctl_value = .data[[pctl_column]]
    )

  data %>%
    echarts4r::e_charts(category, height = "40px") %>%
    echarts4r::e_bar(
      background,
      barWidth = "100%",
      itemStyle = list(
        color = color_grey_light
      ),
      animation = FALSE
    ) %>%
    echarts4r::e_bar(
      pctl_value,
      barWidth = "100%",
      barGap = "-100%",
      itemStyle = list(
        color = pctl_color(data[[pctl_column]])
      )
    ) %>%
    echarts4r::e_flip_coords() %>%
    echarts4r::e_x_axis(
      min = 0,
      max = 1,
      show = FALSE
    ) %>%
    echarts4r::e_y_axis(
      min = 0,
      max = 8,
      show = FALSE
    ) %>%
    echarts4r::e_legend(FALSE) %>%
    #echarts4r::e_tooltip(
    #  formatter = htmlwidgets::JS("
    #    function(params){
    #      return(
    #        params[1].value[0] * 100 + '% Pctl'
    #      );
    #    }
    #  "),
    #  trigger = "axis"
    #) %>%
    echarts4r::e_annotations(
      default_color = color_black,
      annotations = list(
        list(
          x = 0.5,
          y = 0,
          offsetY = -15,
          text = value,
          lineStyle = "none",
          rectStyle = "none",
          arrowStyle = "none",
          textStyle = list(
            "text-anchor" = "middle",
            "font-size" = 14
          )
        )
      )
    ) %>%
    htmlwidgets::onRender(
      htmlwidgets::JS("
        function(el, x) {
          el.parentElement.style.overflow = 'visible';
        }
      ")
    )
}

team_chart_cell <- function(renderer) {
  function(value, index) {
    renderer(team_id_at(rfl_current_standing, index))
  }
}

pctl_cell <- function(pctl_column) {
  function(value, index) {
    pctl_bar(team_id_at(rfl_current_standing, index), pctl_column, value)
  }
}

pctl_columns <- names(rfl_current_standing)[stringr::str_ends(names(rfl_current_standing), "pctl")]

output$current_standing <- reactable::renderReactable({
  data <- rfl_current_standing %>%
    dplyr::arrange(power_rank) %>%
    dplyr::select(-season, -week, -division, -bowl, -seed, -franchise_name_status, -div_rank:-league_rank)

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
              htmltools::div(value),
              htmltools::div(htmltools::HTML(paste0("<small>", data$division_name[index], "</small>")))
            )

            as.character(content)
          },
          width = 200,
          sticky = "left"
        ),
        wins_total = reactable::colDef(
          name = "W",
          width = 50
        ),
        losses_total = reactable::colDef(
          name = "L",
          width = 50
        ),
        pp_total = reactable::colDef(
          name = "PP",
          cell = pctl_cell("pp_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        pf_total = reactable::colDef(
          name = "PF",
          cell = pctl_cell("pf_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        franchise_elo_pregame = reactable::colDef(
          name = paste("WK", current_week),
          cell = pctl_cell("franchise_elo_pregame_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        pf_sparkline = reactable::colDef(
          name = "PF & PP",
          cell = team_chart_cell(fpts_bar_charts),
          width = 150,
          style = "overflow: visible;"
        ),
        elo_sparkline = reactable::colDef(
          name = paste0("WK 1-", current_week),
          cell = team_chart_cell(elo_sparkline),
          width = 150,
          style = "overflow: visible;"
        ),
        elo_shift = reactable::colDef(
          name = "+/-",
          cell = function(value) {
            content <- value

            if (value > 0) {
              content <- paste0("+", value)
            }

            content
          },
          width = 50
        ),
        franchise_elo_postgame = reactable::colDef(
          name = paste("WK", current_week),
          cell = pctl_cell("franchise_elo_postgame_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        elo_rank = reactable::colDef(
          name = "ELO",
          cell = pctl_cell("elo_rank_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        pf_rank = reactable::colDef(
          name = "PF",
          cell = pctl_cell("pf_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        pp_rank = reactable::colDef(
          name = "PP",
          cell = pctl_cell("pp_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        record_rank = reactable::colDef(
          name = "Record",
          cell = pctl_cell("wins_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        all_play_rank = reactable::colDef(
          name = "All-Play",
          cell = pctl_cell("all_play_wins_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        eff_rank = reactable::colDef(
          name = "Eff",
          cell = pctl_cell("eff_total_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        war_rank = reactable::colDef(
          name = "WAR",
          cell = pctl_cell("war_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        power_rank = reactable::colDef(
          name = "Ovrl",
          cell = pctl_cell("true_standing_pctl"),
          width = 100,
          style = "overflow: visible;"
        ),
        seed_total = reactable::colDef(
          name = "Bowl",
          html = TRUE,
          cell = function(value, index) {
            content <- shiny::tagList(
              htmltools::div(htmltools::HTML(data$bowl_emoji[index])),
              htmltools::div(htmltools::HTML(data$seed_emoji[index]))
            )

            as.character(content)
          },
          style = function(value) {
            list(
              textAlign = "center"
            )
          },
        ),
        franchise_id = reactable::colDef(show = FALSE),
        division_name = reactable::colDef(show = FALSE),
        bowl_emoji = reactable::colDef(show = FALSE),
        seed_emoji = reactable::colDef(show = FALSE)
      ),
      purrr::set_names(
        purrr::map(
          pctl_columns,
          ~ reactable::colDef(show = FALSE)
        ),
        pctl_columns
      )
    ),
    columnGroups = list(
      colGroup(
        name = "Standing",
        columns = c("wins_total", "losses_total", "winloss", "pf_sparkline", "pf_total", "pp_total")
      ),
      colGroup(
        name = "ELO",
        columns = c("franchise_elo_pregame", "elo_sparkline", "franchise_elo_postgame", "elo_shift")
      ),
      colGroup(
        name = "Power Ranking",
        columns = c("elo_rank", "pf_rank", "pp_rank", "record_rank", "all_play_rank", "eff_rank", "war_rank", "power_rank")
      )
    ),
    groupBy = "conference_name",
    defaultSorted = c("seed_total"),
    defaultExpanded = TRUE,
    height = 600
  )
})


