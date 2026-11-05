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
      dplyr::select(franchise_id, franchise_name_b = franchise_name, conference_b = conference_name, division_b = division_name, div_id_b = div_id, power_rank_b = power_rank, conf_rank_b = conf_rank, bowl_b = bowl, seed_b = seed_total, wins_b = wins_season, losses_b = losses_season, pf_season_b = pf_season),
    by = c("franchise_id_b" = "franchise_id")
  ) %>%
  dplyr::filter(conference_name == conference_b) %>% # Nur paare aus gleicher conf
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, team_abbrev = abbrev),
    by = c("franchise_id" = "franchise_id")
  )

magic_number_conf <- shiny::reactive({
  magic_number_table <- magic_number_data %>%
    dplyr::select(week, team_abbrev, pf_season, pp_season, wins_season, conference_name, division_name, div_id, bowl, seed, seed_total, conf_rank, dplyr::ends_with("_b")) %>%
    dplyr::filter(conference_name == input$selectRflConference) %>%
    #dplyr::filter(conference_name == "BFC") %>%
    dplyr::mutate(
      tie_breaker = ifelse(pf_season - pf_season_b > pp_season * 13 - week, 0, 1), # Tiebreaker: 0 wenn führendes Team mehr als 20 Punkte pro verbleibender Woche Vorsprung hat
      magic_number = dplyr::case_when(
        wins_season == wins_b ~ total_games - wins_season - losses_b, # wenn gleiche siege, ignoriere tiebreaker
        TRUE ~ total_games + tie_breaker - wins_season - losses_b
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
    dplyr::filter(
      if(isTruthy(input$selectRflDivisions))
        div_id_b %in% input$selectRflDivisions & div_id == input$selectRflDivisions
      else
        TRUE
    ) %>%
    #filter(franchise_id_b == "0022")
    #dplyr::filter(division_b == "Larry Fitzgerald Division") %>%
    dplyr::arrange(seed_total) %>%
    dplyr::select(team_abbrev, franchise_id = franchise_id_b, franchise_name = franchise_name_b, magic_number) %>%
    tidyr::pivot_wider(names_from = team_abbrev, values_from = magic_number)

  magic_number_conf <- magic_number_table %>%
    dplyr::left_join(
      magic_number_data %>%
        dplyr::select(franchise_id, team_abbrev, division_name, wins_season, losses_season, seed_total, bowl_emoji, seed_emoji) %>%
        dplyr::distinct(),
      by = "franchise_id"
    ) %>%
    dplyr::select(franchise_id, franchise_name, team_abbrev:seed_emoji, everything())

  magic_number_conf
})

output$magic_number_table <- reactable::renderReactable({
  team_columns <- names(magic_number_conf())[
    (match("seed_emoji", names(magic_number_conf())) + 1):ncol(magic_number_conf())
  ]

  data <- magic_number_conf()

  reactable_default(
    data,
    columns = c(
      list(
        franchise_name = reactable_col_franchise_name(
          data = data,
          width = 200,
          sticky = "left",
          style = list(borderRight = paste("2px solid", color_grey_light)),
        ),
        wins_season = reactable::colDef("W", width = 50),
        losses_season = reactable::colDef("L", width = 50),
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
          style = list(borderRight = paste("2px solid", color_grey_light)),
        ),
        franchise_id = reactable::colDef(show = FALSE),
        team_abbrev = reactable::colDef(show = FALSE),
        division_name = reactable::colDef(show = FALSE),
        conference_name = reactable::colDef(show = FALSE),
        seed_emoji = reactable::colDef(show = FALSE),
        seed_total = reactable::colDef(show = FALSE)
      ),
      rlang::set_names(
        purrr::map(
          team_columns,
           ~ reactable_coldef_bg(
             header = function(value) {
               franchise_name <- data %>%
                 dplyr::filter(team_abbrev == value) %>%
                 dplyr::pull(franchise_name)

               reactable_header_with_tooltip(value, franchise_name)
             },
             palette_fun = scale_colors(1:10, c(color_red, color_blue), na_color = color_black),
             minWidth = 50
          )
        ),
        team_columns
      )
    ),
    defaultSorted = c("seed_total"),
    height = 900,
    pagination = FALSE
  )

  #magic_number_conf() %>%
    #dplyr::select(
    #  1:23,
    #  dplyr::any_of(selected_teams)
    #) %>%
    #dplyr::mutate(
    #  dplyr::across(23:dplyr::all_of(last_team), as.numeric)
    #) %>%
    #gt::gt() %>%
    #gt::tab_header(
    #  title = paste("Magic Numbers RFL", magic_number_conf()$conference_name[1], "Conference", "Woche", magic_number_conf()$week[1], magic_number_conf()$season[1]),
    #  subtitle = paste("Die “Magic Number” ist die Summe an Siegen des führenden Teams (in der Tabelle links) und den Niederlagen des Verfolgerteams (in der Tabelle oben) in den verbleibenden Spielen", paste0("(", 26 - (magic_number_conf()$week[1] * 2), "),"), "die das Verfolgerteam aus dem entsprechendem Platzierungskampf eliminiert. Faustregel: je kleiner der Wert, desto unwahrscheinlicher ist es für das obere Team auf das an der Seite aufzuholen. Bei mehr als", pf_per_game, "PF pro verbleibender Woche Rückstand wird davon ausgegangen, einen extra Sieg zu benötigen, um am führenden Team vorbeizuziehen.")
    #) %>%
    #ranking_table_base %>%
    #ranking_table_standing %>%
    #ranking_table_bowl %>%
    #gtDefaults() %>%
    #gt::data_color(
    #  22:last_team,
    #  palette = c(color_red, color_bg),
    #  domain = c(0, (26 - magic_number_conf()$week[1] * 2)),
    #  na_color = color_grey_mid
    #) %>%
    #gt::fmt_markdown(columns = franchise_name) %>%
    #gt::sub_missing(columns = everything(), missing_text = "")
    #gt::cols_hide(conference_name)
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

# ui output ----
output$magic_number <- shiny::renderUI({
    shiny::fluidPage(
      htmltools::h1(paste("Magic Numbers RFL", input$selectRflConference, "Conference", "Woche", current_week, new_season_sept)),
      htmltools::div(
        paste("Die “Magic Number” ist die Summe an Siegen des führenden Teams (in der Tabelle links) und den Niederlagen des Verfolgerteams (in der Tabelle oben) in den verbleibenden Spielen", paste0("(", total_games - (current_week * 2), "),"), "die das Verfolgerteam aus dem entsprechendem Platzierungskampf eliminiert. Faustregel: je größer der Wert, desto unwahrscheinlicher ist es für das obere Team auf das an der Seite aufzuholen. Bei mehr als", pf_per_game, "PF pro verbleibender Woche Rückstand wird davon ausgegangen, einen extra Sieg zu benötigen, um am führenden Team vorbeizuziehen."),
        style = "margin-bottom: 1rem;"
      ),
      shiny::fluidRow(
        shiny::column(
          shinycssloaders::withSpinner(reactable::reactableOutput("magic_number_table")),
          width = 12
          #if (current_week < 13) {
          #} else {
          #  htmltools::div(
          #    style = "text-align: center; font-size: 18px; color: #f53b57; margin-top: 20px; padding-left: 30px;",
          #    "Die Saison ist vorbei, hier gibt es nichts mehr zu sehen. Schau dir stattdessen das Conference Standing an."
          #  )
          #}
        )
      )
    )
})
