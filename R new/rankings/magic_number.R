source("R new/rankings/standing.R", local = TRUE)
source("R new/rankings/ranking_tables.R", local = TRUE)

total_games <- 26
pf_per_game <- 20

magic_number_data <- expand.grid(
    franchise_id = rfl_current_standing$franchise_id,
    franchise_id_b = rfl_current_standing$franchise_id
  ) %>%
  dplyr::filter(franchise_id != franchise_id_b) %>%  # Ausschließen von Paaren mit derselben Franchise
  dplyr::left_join(
    rfl_current_standing,
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    rfl_current_standing %>%
      dplyr::select(franchise_id, franchise_name_b = franchise_name, conference_b = conference_name, division_b = division_name, league_rank_b = league_rank, conf_rank_b = conf_rank, bowl_b = bowl, seed_b = seed_total, wins_b = wins_total, losses_b = losses_total, pf_total_b = pf_total),
    by = c("franchise_id_b" = "franchise_id")
  ) %>%
  dplyr::filter(conference_name == conference_b) # Nur paare aus gleicher conf

magic_number_conf <- shiny::reactive({
  magic_number_conf <- magic_number_data %>%
    dplyr::filter(conference_name == input$selectRflConference) %>%
    #dplyr::filter(conference_name == "BFC") %>%
    dplyr::mutate(
      tie_breaker = ifelse(pf_total - pf_total_b > pf_per_game * 13 - week, 0, 1), # Tiebreaker: 0 wenn führendes Team mehr als 20 Punkte pro verbleibender Woche Vorsprung hat
      magic_number = dplyr::case_when(
        wins_total == wins_b ~ total_games - wins_total - losses_b, # wenn gleiche siege, ignoriere tiebreaker
        TRUE ~ total_games + tie_breaker - wins_total - losses_b
      ),
      magic_number = dplyr::case_when(
        magic_number <= 0 ~ 0,
        (magic_number > total_games - (week * 2)) | # magic number nur, wenn mit verfügbaren spielen noch erreichbar
          (bowl == bowl_b & seed > seed_b) | # magic number nur für teams, die hinter anderen stehen
          (conf_rank > conf_rank_b) |
          (division_name != division_b & bowl == "SB" & seed == 3 & seed_b > 3) ~ NA, # keine magic number auf 3. div sieger wennn nicht gleiche div
        TRUE ~ magic_number
      )
    ) %>%
    #filter(franchise_id_b == "0022")
    #dplyr::filter(magic_number != "") %>%
    dplyr::arrange(league_rank) %>%
    dplyr::select(-franchise_id_b, -franchise_elo_pregame:-elo_shift, -pf_rank:-power_rank_emoji, -conference_b:-pf_total_b, -tie_breaker) %>%
    tidyr::spread(franchise_name_b, magic_number, fill = "") %>%
    dplyr::arrange(league_rank)
})

output$magic_number_table <- gt::render_gt({
  #magic_number_conf <- magic_number_conf()[, !(colnames(df) %in% input$selectRflTeams)]

  selected_teams <- magic_number_conf() %>%
    dplyr::filter(conference_name == input$selectRflConference) %>%
    dplyr::filter(
      if(isTruthy(input$selectRflTeams))
        franchise_id %in% input$selectRflTeams
      else
        TRUE
    ) %>%
    dplyr::pull(franchise_name)

  last_team <- tail(selected_teams, n = 1)

  magic_number_conf() %>%
    dplyr::select(
      1:23,
      dplyr::any_of(selected_teams)
    ) %>%
    dplyr::mutate(
      across(23:last_team, as.numeric)
    ) %>%
    dplyr::mutate(
      franchise_name = franchise_name_status
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Magic Numbers RFL", magic_number_conf()$conference_name[1], "Conference", "Woche", magic_number_conf()$week[1], magic_number_conf()$season[1]),
      subtitle = paste("Die “Magic Number” ist die Summe an Siegen des führenden Teams (in der Tabelle links) und den Niederlagen des Verfolgerteams (in der Tabelle oben) in den verbleibenden Spielen", paste0("(", 26 - (magic_number_conf()$week[1] * 2), "),"), "die das Verfolgerteam aus dem entsprechendem Platzierungskampf eliminiert. Faustregel: je kleiner der Wert, desto unwahrscheinlicher ist es für das obere Team auf das an der Seite aufzuholen. Bei mehr als", pf_per_game, "PF pro verbleibender Woche Rückstand wird davon ausgegangen, einen extra Sieg zu benötigen, um am führenden Team vorbeizuziehen.")
    ) %>%
    ranking_table_base %>%
    ranking_table_standing %>%
    ranking_table_bowl %>%
    gtDefaults() %>%
    gt::data_color(
      22:last_team,
      palette = c(color_red, color_bg),
      domain = c(0, (26 - magic_number_conf()$week[1] * 2)),
      na_color = color_grey_mid
    ) %>%
    gt::fmt_markdown(columns = franchise_name) %>%
    gt::fmt_missing(columns = everything(), missing_text = "") %>%
    gt::cols_hide(conference_name)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

shiny::observeEvent(input$selectRflConference, {
  req(input$selectRflConference)

  conf_teams <- rfl_franchise_data %>%
    dplyr::filter(conference_name == input$selectRflConference)

  shinyWidgets::updatePickerInput(
    session,
    inputId = "selectRflDivisions",
    choices = setNames(unique(conf_teams$division[order(conf_teams$division_name)]), unique(conf_teams$division_name[order(conf_teams$division_name)]))
  )
})

shiny::observeEvent(input$selectRflConference, {
  req(input$selectRflConference)

  conf_teams <- rfl_franchise_data %>%
    dplyr::left_join(
      rfl_current_standing %>%
        dplyr::select(franchise_id, conf_rank, bowl),
      by = "franchise_id"
    ) %>%
    dplyr::filter(conference_name == input$selectRflConference)

  selected_teams <- conf_teams %>%
    dplyr::filter(conference_name %in% input$selectRflConference) %>%
    dplyr::pull(franchise_id)

  shinyWidgets::updatePickerInput(
    session,
    inputId = "selectRflTeams",
    choices = split(
      setNames(conf_teams$franchise_id[order(conf_teams$franchise_name)], conf_teams$franchise_name[order(conf_teams$franchise_name)]),
      factor(conf_teams$bowl[order(conf_teams$franchise_name)],
             levels = c("SB", "PB", "TB"))
    ),
    selected = selected_teams,
    options = list("max-options" = 18),
  )
})
