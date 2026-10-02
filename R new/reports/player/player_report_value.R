source("R new/base_data/player_data.R", local = TRUE)

player_report_data <- shiny::reactive({
  player_report_data <- rfl_player_data %>%
    dplyr::filter(player_id == "14136" & reg_season == 1)
})

# WAR und ELO ----
player_report_data %>%
  dplyr::select(game, war, war_career, player_elo_post) %>%
  echarts4r::e_chart(x = game) %>%
  echarts4r::e_line(player_elo_post) %>%
  echarts4r::e_line(war, y_index = 1) %>%
  echarts4r::e_line(war_career, y_index = 1) %>%
  echarts4r::e_datazoom(x_index = 0)

