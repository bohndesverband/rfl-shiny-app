source("R new/base_data/player_data.R", local = TRUE)

player_report_data <- shiny::reactive({
  player_report_data <- rfl_player_data %>%
    dplyr::filter(player_id == "15290") %>%
    dplyr::select(date, season, week, player_id, pos_grouped, game, status, opponent_side, fpts, war, war_career, player_elo_pre, player_elo_post, elo_shift, pos_rank, pos_rank_season, dplyr::starts_with("top"))
})

# FPts ----
player_report_data %>%
  dplyr::mutate(
    season = factor(season, levels = rev(sort(unique(season)))),
    color = dplyr::case_when(
      pos_rank <= 12 ~ "#3AD17C",
      (pos_grouped %in% c("RB", "WR", "LB") & pos_rank > 36) | (pos_grouped %in% c("QB", "TE", "PK") & pos_rank > 16) | (pos_grouped %in% c("DL", "DB") & pos_rank > 24) ~ "#F0587A",
      (pos_grouped %in% c("RB", "WR", "LB") & pos_rank > 24) | (pos_grouped %in% c("QB", "TE", "PK") & pos_rank > 14) | (pos_grouped %in% c("DL", "DB") & pos_rank > 16) ~ "#E8B23E",
      TRUE ~ "#7A7979"
    ), # add to data
    pos_rank = ifelse(is.na(pos_rank), status, paste(pos_grouped, pos_rank, sep = " #"))
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(week) %>%
  echarts4r::e_chart(x = week, timeline = TRUE) %>%
  echarts4r::e_mark_line(
    "war"
  ) %>%
  echarts4r::e_bar(
    fpts, name = "FPts",
    label = list(
      show = TRUE,
      formatter = htmlwidgets::JS(
        "function(params) {
          if (params.data.label.pos_rank) {
            return params.data.label.pos_rank;
          } else {
            return ''
          }
        }"
      ),
      position = "top",
      color = "#505052"
    ),
    itemStyle = list(
      color = htmlwidgets::JS(
      "function(params) {
      console.log(params)
        return params.data.color.color;
      }"
      )
    )
  ) %>%
  e_mark_line(data = list(type = "average")) %>%
  echarts4r::e_add_nested("label", pos_rank) %>%
  echarts4r::e_add_nested("color", color) %>%
  echarts4r::e_add_nested("opponent", opponent_side) %>%
  echarts4r::e_area(war, name = "WAR") %>%
  echarts4r::e_x_axis(min = 0, max = 18, interval = 1) %>%
  echarts4r::e_legend(top = 0, left = "center") %>%
  echarts4r::e_tooltip(
    trigger ="axis",
    formatter = htmlwidgets::JS("
      function(params){
        return ('<strong>WK #' + params[0].data.value[0] + '</strong><br />' +
        params[0].data.opponent.opponent_side + '<br />' +
        params[0].data.label.pos_rank +
        '<br />FPts: ' + params[0].value[1] +
        '<br />WAR: ' + params[1].value[1])
      }
    ")
  ) %>%
  e_x_axis(
    splitLine = list(show = FALSE),
    axisLine = list(lineStyle = list(color = "#1B1E25", width = 1)),
    axisLabel = list(
      color = "#f4f1ea47",
      fontSize = 11,
      letterSpacing = "0.08em"
    ),
    axisLabel = list(
      formatter = htmlwidgets::JS(
        "function(value) {
            if (Number.isInteger(value) && value >= 1 && value <= 17) {
              return value
            }
            return '';
        }"
      )
    )
  ) %>%
  #e_format_x_axis(prefix = "WK") %>%
  e_y_axis(
    splitLine = list(lineStyle = list(color = "#1B1E25")),
    axisLine = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLabel = list(
      color = "#f4f1ea47",
      fontSize = 11,
      letterSpacing = "0.08em"
    ),
  ) %>%
  e_timeline_opts(
    axisType = "category",
    controlStyle = list(
      showPlayBtn = FALSE,
      showPrevBtn = FALSE,
      showNextBtn = FALSE
    ),
    lineStyle = list(
      color = "#1B1E25",
      width = 1
    ),
    itemStyle = list(
      color = "#1B1E25"
    ),
    checkpointStyle = list(
      color = "#505052"
    ),
    label = list(
      color = "#f4f1ea47",
      fontSize = 12
    )
  ) %>%
  e_grid(height = "75%", bottom = "15%") %>%
  e_theme_custom('{"backgroundColor":["#04060B"], "color":["#7A7979"]}')

# fantasy finish ----
## by week ----
player_report_data %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(dplyr::across(dplyr::ends_with("_weekly"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  tidyr::pivot_longer(dplyr::ends_with("_weekly")) %>%
  # TODO: je nach position platzierungen kürzen
  echarts4r::e_chart(x = name) %>%
  echarts4r::e_pie(value, roseType = "radius")

## by season ----
player_report_data %>%
  dplyr::filter(season != max(season)) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(week) %>%
  dplyr::summarise(
    min_length = min(pos_rank, na.rm = TRUE),
    max_length = max(pos_rank.rm = TRUE),
    pos_rank_season = last(pos_rank_season),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    max_rank = max(c(max_length, pos_rank_season)) + 1,
    dplyr::across(c(min_length, max_length, pos_rank_season), ~ max_rank - .x)
  ) %>%
  echarts4r::e_chart(x = season) %>%
  echarts4r::e_line(pos_rank_season) %>%
  e_barRange(
    lower = min_length,
    upper = max_length,
    textSymbol = ''
  ) %>%
  e_x_axis(
    splitLine = list(show = FALSE),
    min = min(player_report_data$season),
    max = max(player_report_data$season),
    interval = 1
  ) %>%
  e_y_axis(
    min = 1,
    max = 100,
    interval = 13,
    axisLabel = list(
      formatter = htmlwidgets::JS(
        "function(value) {
        return 100 - value;
      }"
      )
    )
  )


# WAR und ELO ----
plot_elo <- player_report_data %>%
  echarts4r::e_chart(x = date) %>%
  echarts4r::e_candle(player_elo_post, player_elo_pre, player_elo_post, player_elo_pre) %>%
  echarts4r::e_group("player_value") %>%
  e_y_axis(
    index = 0,
    min = min(player_report_data$player_elo_post) - 200,
    max = max(player_report_data$player_elo_post) + 100
  ) %>%
  e_mark_point(data = list(type = "max")) %>%

  e_y_axis(index = 1, splitLine = list(show = FALSE)) %>%
  e_x_axis(
    axisLabel = list(
      formatter = htmlwidgets::JS(
        "function(value) {
          var d = new Date(value);
          return String(d.getDate()).padStart(2, '0') + '.' +
                 String(d.getMonth() + 1).padStart(2, '0') + '.' +
                 String(d.getFullYear()).slice(-2);
        }"
      )
    )
  ) %>%
  echarts4r::e_tooltip(
    trigger ="axis"
  )

plot_war_career <- player_report_data %>%
  echarts4r::e_chart(x = date) %>%
  echarts4r::e_line(war_career, smooth = TRUE) %>%
  echarts4r::e_group("player_value") %>%
  e_connect_group("player_value")

plot_war_season <- player_report_data %>%
  dplyr::group_by(season) %>%
  echarts4r::e_chart(x = date) %>%
  echarts4r::e_line(war, smooth = TRUE) %>%
  echarts4r::e_group("player_value")


e_arrange(plot_elo, plot_war_career, plot_war_season, title = "Linked datazoom")








plot %>%
  echarts4r::e_datazoom(x_index = 0)


e1 <- cars |>
  e_charts(
    speed,
    height = 200
  ) |>
  e_scatter(dist) |>
  e_datazoom(show = FALSE) |>
  e_group("grp") # assign group

e2 <- cars |>
  e_charts(
    dist,
    height = 200
  ) |>
  e_scatter(speed) |>
  e_datazoom() |>
  e_group("grp") |> # assign group
  e_connect_group("grp") # connect

e_arrange(e1, e2, title = "Linked datazoom")
