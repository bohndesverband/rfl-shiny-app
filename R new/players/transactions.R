source("R new/base_data/player_data.R", local = TRUE)

# data ----
transactions_filtered <- shiny::reactive({
  transactions_filtered <- rfl_transactions_data %>%
    #dplyr::filter(season == 2026) %>%
    dplyr::filter(season >= input$selectYears[1] & season <= input$selectYears[2]) %>%
    dplyr::select(date, week, type_desc, display_name, pos_grouped, team, fpts_running, ppg_running, war = war_career, war_pctl = war_career_pctl, war_shift, player_elo, player_elo_pctl, elo_shift, franchise_name)
}) %>%
  shiny::bindEvent(input$filterData, ignoreNULL = FALSE)

with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help", title = tooltip, value)
}

reactable_cell_shift <- function(value) {
  if(is.na(value)) {
    NULL
  } else if (value > 0) {
    paste0("+", value)
  } else {
    value
  }
}

reactable_coldef_bar_bg <- function(pctl_column, height = "3px", align = c("left", "right")) {
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
        borderLeft: %s,
        borderRight: %s
      };
    }",
    jsonlite::toJSON(pctl_column, auto_unbox = TRUE),
    jsonlite::toJSON(unname(palette)),
    jsonlite::toJSON(align, auto_unbox = TRUE),
    jsonlite::toJSON(height, auto_unbox = TRUE),
    jsonlite::toJSON(paste("1px solid", color_grey_light), auto_unbox = TRUE),
    jsonlite::toJSON(paste("1px solid", color_grey_light), auto_unbox = TRUE)
  ))
}

# history ----
output$players_transactions_table <- reactable::renderReactable({
  data <- transactions_filtered() %>%
    dplyr::arrange(dplyr::desc(date), franchise_name)

  reactable_default(
    data,
    columns = list(
      date = reactable::colDef("Datum", format = reactable::colFormat(datetime = TRUE), width = 175, align = "left"),
      week = reactable::colDef("WK", width = 50),
      type_desc = reactable::colDef(
        "Typ",
        html = TRUE,
        width = 75,
        cell = function(value) {
          color <- "green"

          if (value == "dropped") {
            color <- "red"
          }

          htmltools::span(value, class = paste("badge", color))
          # TODO: zu daten hinzufügen
        }
      ),
      display_name = reactable::colDef("Spieler", width = 200, align = "left"),
      pos_grouped = reactable::colDef("Pos", width = 50),
      team = reactable::colDef("Team", width = 75),
      fpts_running = reactable_coldef_color(
        name = "FPts", width = 75,
        palette_fun = scale_rainbow(range(data$fpts_running, na.rm = TRUE), text = TRUE)
      ),
      ppg_running = reactable_coldef_color(
        name = "PPG", width = 75,
        palette_fun = scale_rainbow(range(data$ppg_running, na.rm = TRUE), text = TRUE)
      ),
      war = reactable::colDef(
        header = with_tooltip("WAR", "Career Wins above Replacement zum Zeitpunkt der Transaktion. Balken zeigt Perzentil innerhalb der Positionsgruppe."),
        width = 125,
        style = reactable_coldef_bar_bg("war_pctl")
      ),
      war_shift = reactable_coldef_bg(
        header = with_tooltip("+/-", "WAR-Veränderung seit der Transaktion."), width = 100,
        palette_fun = scale_colors(range(data$war_shift, na.rm = TRUE), c(color_red, color_blue)),
        cell = reactable_cell_shift
      ),
      player_elo = reactable::colDef(
        header = with_tooltip("ELO", "ELO zum Zeitpunkt der Transaktion. Balken zeigt Perzentil innerhalb der Positionsgruppe."),
        width = 125,
        style = reactable_coldef_bar_bg("player_elo_pctl")
      ),
      elo_shift = reactable_coldef_bg(
        header = with_tooltip("+/-", "ELO-Veränderung seit der Transaktion"), width = 100,
        palette_fun = scale_colors(range(data$elo_shift, na.rm = TRUE), c(color_red, color_blue)),
        cell = reactable_cell_shift
      ),
      franchise_name = reactable::colDef("Team", width = 250, align = "left"),
      war_pctl = reactable::colDef(show = FALSE),
      player_elo_pctl = reactable::colDef(show = FALSE),
      player_elo_current_pctl = reactable::colDef(show = FALSE)
    ),
    columnGroups = list(
      reactable::colGroup(
        "Fantasy Pts",
        columns = c("fpts_running", "ppg_running")
      ),
      reactable::colGroup(
        "WAR",
        columns = c("war", "war_shift")
      ),
      reactable::colGroup(
        "ELO",
        columns = c("player_elo", "elo_shift")
      )
    ),
    filterable = TRUE,
    defaultPageSize = 15
  )
})

# TODO: horizontal scolling
# TODO: check 13.2.2018, 11:30:47 22 added Saquon Barkley
# TODO: check WAR bei Wentz (ohne war aufgenommen, dann aber gespielt)

# free agents ----
#rfl_transactions_data %>%
#  dplyr::arrange(dplyr::desc(timestamp)) %>%
#   dplyr::
#
# rfl_fantasy_finishes_season

## alle transactions ----

# trade bait ----
## data ----
rfl_trade_baits <- jsonlite::read_json(paste0(mfl_api_base_march, "/export?TYPE=tradeBait&L=63018&APIKEY=aRNp3s%2BWvuWsx12mPlrBYDoeErox&INCLUDE_DRAFT_PICKS=0&JSON=1"))$tradeBaits$tradeBait %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::separate_rows(willGiveUp, sep = ",") %>%
  dplyr::mutate(willGiveUp = willGiveUp) %>%
  dplyr::left_join(rfl_franchise_data %>% dplyr::select(franchise_id, franchise_name), by = "franchise_id") %>%
  dplyr::left_join(
    rfl_player_data_latest %>%
      dplyr::select(player_id, display_name, pos_grouped, team, fpts_running, ppg_running, pos_rank_season, war, war_pctl, player_elo = player_elo_current, player_elo_pctl = player_elo_current_pctl),
    by = c("willGiveUp" = "player_id")
  ) %>%

  #dplyr::left_join(mfl_players %>% dplyr::select(player_id, display_name_new = display_name, pos = grouped_pos, team_new = team), by = c("willGiveUp" = "player_id")) %>%

  dplyr::mutate(
    #display_name = dplyr::coalesce(display_name, display_name_new),
    #pos_grouped = dplyr::coalesce(pos_grouped, pos),
    #team = dplyr::coalesce(team, team_new),
    player_elo = ifelse(is.na(player_elo), 1500, player_elo)
  ) %>%

  dplyr::arrange(dplyr::desc(player_elo)) %>%
  dplyr::rename(player_id = willGiveUp) %>%
  dplyr::select(franchise_name, display_name, pos_grouped, team, pos_rank_season, fpts_running, ppg_running, war, player_elo, war_pctl, player_elo_pctl, inExchangeFor)

## output ----
output$trade_bait <- reactable::renderReactable({
  reactable_default(
    rfl_trade_baits,
    columns = list(
      player_id = reactable::colDef(show = FALSE),
      team = reactable::colDef("Team", width = 75, align = "left"),
      display_name = reactable::colDef("Spieler", width = 200, align = "left"),
      pos_grouped = reactable::colDef("Pos", width = 75),
      pos_rank_season = reactable_coldef_color(name = "Rank", palette_fun = scale_colors(8:30, c(color_blue, color_red)), width = 75),
      fpts_running = reactable_coldef_color(
        name = "FPts", width = 75,
        palette_fun = scale_rainbow((5 * current_week - 1):(20 * current_week - 1), text = TRUE)
      ),
      ppg_running = reactable_coldef_color("PPG", palette_fun = scale_rainbow(5:20, text = TRUE), width = 75),
      war = reactable::colDef(
        "WAR", width = 100,
        style = reactable_coldef_bar_bg("war_pctl"),
      ),
      war_pctl = reactable::colDef(show = FALSE),
      player_elo = reactable::colDef(
        "ELO", width = 100,
        style = reactable_coldef_bar_bg("player_elo_pctl")
      ),
      player_elo_pctl = reactable::colDef(show = FALSE),
      franchise_name = reactable::colDef("Team", width = 200),
      inExchangeFor = reactable::colDef("Gesucht wird", align = "left")
    ),
    columnGroups = list(
      reactable::colGroup("Spieler", columns = c("display_name", "pos_grouped", "team")),
      reactable::colGroup("FPts", columns = c("pos_rank_season", "fpts_running", "ppg_running"))
    ),
    filterable = TRUE,
    defaultSorted = list(war = "desc"),
    defaultPageSize = 15
  )
})

# ui output ----
output$players_transactions <- shiny::renderUI({
  shiny::fluidPage(
    htmltools::h1("Transaktionen"),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Transaktionen",
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              shinycssloaders::withSpinner(reactable::reactableOutput("players_transactions_table")),
              width = 12
            )
          ),
          #shiny::fluidRow(
          #  shiny::column(
          #     shinycssloaders::withSpinner(gt::gt_output("team_roster")),
          #     width = 12
          #   )
          # )
        )
      ),
      shiny::tabPanel(
        "Trades",
        shiny::fluidPage(
          htmltools::h2("Saison"),
          shiny::fluidRow(
            htmltools::h3("Schedule"),
            shiny::column(
              #shinycssloaders::withSpinner(ggiraph::girafeOutput("team_schedule")),
              width = 12
            )
          )
        )
      ),
      shiny::tabPanel(
        "Trade Bait",
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(
              shinycssloaders::withSpinner(reactable::reactableOutput("trade_bait")),
              width = 12
            )
          )
        )
      )
    )
  )
})
