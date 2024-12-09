source("R new/rankings/standing.R", local = TRUE)
source("R new/rankings/ranking_tables.R", local = TRUE)

total_games <- 27

magic_number_data <- expand.grid(
    franchise_id = current_standing$franchise_id,
    franchise_id_b = current_standing$franchise_id
  ) %>%
  dplyr::filter(franchise_id != franchise_id_b) %>%  # Ausschließen von Paaren mit derselben Franchise
  dplyr::left_join(
    current_standing,
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    current_standing %>%
      dplyr::select(franchise_id, franchise_name, conference_name, wins_total, losses_total) %>%
      dplyr::rename(franchise_name_b = franchise_name, wins_b = wins_total, losses_b = losses_total, conference_b = conference_name),
    by = c("franchise_id_b" = "franchise_id")
  ) %>%
  dplyr::filter(conference_name == conference_b) # Nur paare aus gleicher conf

magic_number_conf <- shiny::reactive({
  test <- magic_number_data %>%
    dplyr::filter(conference_name == input$selectRflConference) %>%
    dplyr::mutate(
      magic_number = total_games - wins_total - losses_b,
      magic_number = ifelse(magic_number <= 0 | magic_number > 26 - (week * 2), NA, magic_number)
    ) %>%
    dplyr::filter(magic_number != "") %>%
    dplyr::select(-franchise_id_b, -franchise_elo_pregame:-elo_shift, -pf_rank:-power_rank_emoji, -conference_b:-losses_b) %>%
    #filter(franchise_id %in% c(input$selectRflConferenceTeams)) %>%
    tidyr::spread(franchise_name_b, magic_number, fill = "") %>%
    dplyr::arrange(league_rank)
})

output$magic_number_table <- gt::render_gt({
  #magic_number_conf <- magic_number_conf()[, !(colnames(df) %in% input$selectRflConferenceTeams)]

  last_col <- colnames(magic_number_conf()[ncol(magic_number_conf())])

  magic_number_conf() %>%
    dplyr::select(1:21, any_of(input$selectRflConferenceTeams)) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Magic Numbers RFL", magic_number_conf()$conference_name[1], "Conference"),
      subtitle = paste("Woche", magic_number_conf()$week[1], magic_number_conf()$season[1])
    ) %>%
    ranking_table_base %>%
    ranking_table_standing %>%
    ranking_table_bowl %>%
    gtDefaults() %>%
    #gt::data_color(
    #  22:last_col,
    #  palette = c(color_bg, color_grey_mid, color_grey_light)
    #) %>%
    gt::cols_hide(conference_name)
})
