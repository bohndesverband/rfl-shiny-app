source("R new/base_data/player_data.R", local = TRUE)

player_report_data <- shiny::reactive({
  player_report_data <- rfl_player_data %>%
    dplyr::filter(player_id == "14136")
})

# WAR und ELO ----
player_report_data %>%
  dplyr::select(war_career, player_elo_post) %>%
  echarts4r::e_chart(x = )
