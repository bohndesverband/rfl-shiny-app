# helper ----
reactable_col_franchise_name <- function(data, name = "Team", ...) {
  reactable::colDef(
    name = name,
    html = TRUE,
    cell = function(value, index) {
      content <- shiny::tagList(
        htmltools::div(htmltools::HTML(value)),
        htmltools::div(htmltools::HTML(paste0("<small>", data$division_name[index], "</small>")))
      )

      as.character(content)
    },
    align = "left",
    ...
  )
}

# bar chart  ----
bar_chart <- function(text, width = "100%", height = "1rem", fill = color_grey_mid, background = color_grey_light) {
  bar <- htmltools::div(class="bar-chart--bar", style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(class="bar-chart--chart", style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  label <- htmltools::span(class="bar-chart--label", text, style = list(fontSize = "0.8em", width = "25%", textAlign = "right"))

  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}

## pctl als bg ----
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

# pctö und bar chart ----
reactable_cell_pctl <- function(value, index, value_col = NULL, rank = NULL, data) {
  if (!is.null(value_col)) {
    shown_value <- data[[value_col]][index]
  } else {
    shown_value <- value
  }

pctl_value <- NULL

  if (identical(rank, "pctl")) {
    #pctl_col <- stringr::str_replace(value_col, "_rank", "_pctl")
    pctl <- value * 100

    last_one <- pctl %% 10
    last_two <- pctl %% 100

    ending <- dplyr::case_when(
      last_two %in% 11:13 ~ "th",
      last_one == 1       ~ "st",
      last_one == 2       ~ "nd",
      last_one == 3       ~ "rd",
      TRUE                ~ "th"
    )

    pctl_value <- paste0(pctl, ending)
  }

  if (!is.null(rank) & is.null(pctl_value)) {
    htmltools::HTML(
      htmltools::HTML(paste0("<div><strong>", format(shown_value, big.mark = ".", decimal.mark = ","), "</strong></div>")),
      htmltools::HTML(paste0("<div><small>#", data[[rank]][index], "</small></div>"))
    )
  } else {
    htmltools::HTML(
      htmltools::HTML(paste0("<div><strong>", format(shown_value, big.mark = ".", decimal.mark = ","), "</strong></div>")),
      htmltools::HTML(paste0("<div><small>", pctl_value, "</small></div>"))
    )
  }
}

reactable_coldef_bg_bar <- function(pctl, data, width = 75, format = "pctl", ...) {
  reactable::colDef(
    ...,
    cell = function(value, index) {
      if (format == "pct") {
        scales::percent(value, accuracy = 1)
      } else {
        reactable_cell_pctl(value, index, value_col = NULL, data = data, rank = NULL)
      }
    },
    html = TRUE,
    width = width,
    style = reactable_bar_bg(pctl),
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

## shift ----
reactable_cell_shift <- function(value) {
  if(is.na(value)) {
    NULL
  } else if (value > 0) {
    paste0("+", value)
  } else {
    value
  }
}

## tooltip im header ----
reactable_header_with_tooltip <- function(value, tooltip) {
  tags$abbr(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help", title = tooltip, value)
}

# Wie viel haben die Teams in die verschiedenen Positionsgruppen investiert? ----
render_draft_history_table <- function(
    data_source,
    positions = c("QB", "RB", "WR", "TE", "DL", "LB", "DB"),
    use_voe = TRUE,
    use_mean = FALSE,
    table_height = 650
) {

  # Aggregation
  data <- data_source %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::group_by(franchise_id, pos_grouped) %>%
    dplyr::summarise(
      franchise_name = dplyr::last(franchise_name),
      value = {
        x <- if (use_voe) voe else pvar_exp

        result <- if (use_mean) {
          mean(x, na.rm = TRUE)
        } else {
          sum(x, na.rm = TRUE)
        }

        round(result, 1)
      },
      .groups = "drop"
    )

  pal_pvar_exp_sum <- scale_rainbow(range(data$value, na.rm = TRUE))

  # Wide Format
  data <- data %>%
    tidyr::spread(pos_grouped, value) %>%
    dplyr::select(
      franchise_id,
      franchise_name,
      dplyr::any_of(positions)
    )

  # TOTAL-Spalte ergänzen
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total = {
        values <- c_across(dplyr::any_of(positions))

        if (use_mean) {
          round(mean(values, na.rm = TRUE), 1)
        } else {
          round(sum(values, na.rm = TRUE), 1)
        }
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(franchise_id, franchise_name, total, QB:DB)

  pal_pvar_exp_total <- scale_red_blue(range(data$total, na.rm = TRUE))

  # Detailtabelle für aufgeklappte Positionen
  position_details <- function(index, pos) {

    pos_data <- data_source %>%
      dplyr::filter(
        franchise_id == data$franchise_id[index],
        pos_grouped == pos
      ) %>%
      dplyr::select(
        asset_name_with_subline,
        pvar_exp,
        voe
      )

    reactable_default(
      pos_data,
      columns = list(
        asset_name_with_subline = reactable::colDef(
          name = paste0(data$franchise_name[index], " ", pos, "s"),
          html = TRUE,
          cell = function(value, index) {
            as.character(
              htmltools::div(
                htmltools::HTML(
                  pos_data$asset_name_with_subline[index]
                )
              )
            )
          },
          footer = "Total",
          footerStyle = list(
            fontWeight = "bold",
            textAlign = "right"
          ),
          minWidth = 350
        ),
        pvar_exp = coldef_pvar_exp(footer = function(values) { sum(values, na.rm = TRUE) }),
        voe = coldef_voe(footer = function(values) { sum(values, na.rm = TRUE) })
      ),
      fullWidth = FALSE,
      defaultSorted = "pvar_exp",
      defaultSortOrder = "desc",
      pagination = TRUE,
      defaultPageSize = 5
    )
  }

  # Positions-Spalten erzeugen
  create_position_columns <- function(positions) {

    lapply(positions, function(pos) {

      reactable_coldef_bg(
        name = pos,
        palette_fun = pal_pvar_exp_sum,
        details= function(index) {

          row <- data[index, ]
          value <- row[[pos]]

          if (is.null(value) || is.na(value) || value == "") {
            return(NULL)
          }

          position_details(index, pos)
        },
        footer = function(values) { sum(values, na.rm = TRUE) }
      )
    })
  }

  position_cols <- setNames(
    create_position_columns(positions),
    positions
  )

  # Finales Columns-Objekt
  columns <- c(
    list(
      total = reactable_coldef_bg(
        name = "Total",
        palette_fun = pal_pvar_exp_total,
        minWidth = 80
      ),
      franchise_name = reactable::colDef(
        name = "Team",
        minWidth = 150,
        align = "left"
      ),
      franchise_id = reactable::colDef(show = FALSE)
    ),
    position_cols
  )

  # Finale Tabelle
  reactable_default(
    data,
    columns = columns,
    defaultSorted = "total",
    defaultSortOrder = "desc",
    searchable = TRUE,
    height = table_height,
    defaultPageSize = 12,
    pagination = TRUE
  )
}

# Draftklassen Tabelle ----
## reusable reactable for draft class player ----
draft_class_players_reactable <- function(data, col_names = NULL, col_groups = NULL, ...) {

  default_column_groups <- NULL

  if (all(c("pvar", "voe") %in% names(data))) {
    default_column_groups <- list(
      reactable::colGroup(
        name = "Value",
        columns = c("pvar", "voe")
      )
    )
  }

  reactable_default(
    data,
    columns = c(
      list(
        player_name_with_badge = reactable::colDef(
          name = "Spieler",
          html = TRUE,
          cell = function(value, index) {
            content <- shiny::tagList(
              htmltools::div(htmltools::HTML(value)),
              htmltools::div(
                htmltools::HTML(
                  paste0("<small>", data$pos_team[index], "</small>")
                )
              )
            )

            as.character(content)
          },
          minWidth = 170,
          align = "left"
        ),
        franchise_name = reactable::colDef(
          name = "Team",
          html = TRUE,
          cell = function(value, index) {
            content <- shiny::tagList(
              htmltools::div(value),
              htmltools::div(
                htmltools::HTML(
                  paste0("<small>", data$draft_range_subline[index], "</small>")
                )
              )
            )

            as.character(content)
          },
          minWidth = 170,
          align = "left"
        ),
        round_pick = reactable::colDef(
          name = "Pick",
          style = function(value) {
            list(textAlign = "center")
          },
          minWidth = 50
        ),
        pvar = coldef_pvar(),
        voe = coldef_voe(),
        pos_team = reactable::colDef(show = FALSE),
        draft_range_subline = reactable::colDef(show = FALSE)
      ),
      col_names
    ),
    columnGroups = c(
      default_column_groups,
      col_groups
    ),
    defaultSorted = "round_pick",
    ...
  )
}

## draft grades ----

## reusable reactable for draft class teams ----
draft_classes_teams_reactable <- function(data, picks_reactive, column_groups = NULL, columns = NULL, ...) {
  # define default columns
  default_columns <- list(
    franchise_name = reactable::colDef(
      name = "Team",
      html = TRUE,
      align = "left",
      cell = function(value, index) {
        content <- shiny::tagList(
          htmltools::div(value),
          htmltools::div(htmltools::HTML(paste0("<small>", data$picks[index], " Picks", "</small>")))
        )

        as.character(content)
      },
      minWidth = 170
    ),
    rank_season = reactable::colDef(name = "VOE #", minWidth = 70),
    pvar = coldef_pvar_text(palette_fun = pal_pvar_class_sum),
    voe = coldef_voe_bg(palette_fun = pal_voe_class_sum_bg),
    rank = reactable::colDef(
      name = "Platz",
      html = TRUE,
      cell = function(value) {
        content <- shiny::tagList(
          htmltools::div(value),
          htmltools::div(htmltools::HTML(paste0("<small>", "aus ", (season_before_wk_2 + 1 - 2017) * 36, "</small>")))
        )

        as.character(content)
      },
      style = function(value) {
        list(
          textAlign = "center"
        )
      },
      minWidth = 70
    ),
    voe_pctl = colDef(
      name = "VOE Pctl",
      align = "left",
      cell = function(value) {
        width <- paste0(value * 100, "%")
        bar_chart(width, width = width)
      },
      minWidth = 200
    ),
    picks = reactable::colDef(show = FALSE)
  )

  # allow overriding/merging of columns
  if (!is.null(columns)) {
    default_columns <- utils::modifyList(default_columns, columns)
  }

  reactable_default(
    data,
    columns = default_columns,
    columnGroups = column_groups,
    details = function(index) {
      row <- data[index, ]

      picks <- picks_reactive %>%
        dplyr::filter(franchise_name == row$franchise_name & season == row$season) %>%
        dplyr::select(asset_name_with_badge, voe)

      reactable_default(
        picks,
        columns = list(
          asset_name_with_badge = reactable::colDef(
            name = "Spieler",
            html = TRUE,
            cell = function(value, index) {
              as.character(htmltools::div(htmltools::HTML(value)))
            },
            minWidth = 350,
            align = "left"
          ),
          voe = coldef_voe(),
          compact = TRUE,
          fullWidth = FALSE
        ),
        defaultSorted = c("asset_name_with_badge")
      )
    },
    filterable = TRUE,
    sortable = TRUE,
    defaultSorted = "voe",
    defaultSortOrder = "desc",
    defaultPageSize = 12,
    height = 775,
    ...
  )
}

## draft grades defaults -----
draft_grades_reactable <- function(
    data,
    writers,
    col_names = NULL,
    col_groups = NULL,
    ...
) {

  writers_col_names <- list()
  writers_col_groups <- list()

  for (name in writers) {

    text_col <- paste0("text_", name)
    grade_col <- paste0("grade_", name)

    writers_col_names[[text_col]] <- reactable::colDef(
      name = "Analyse",
      minWidth = 300
    )

    writers_col_names[[grade_col]] <- reactable_coldef_bg(
      name = "Note",
      palette_fun = scale_rainbow_reverse(range(1:7, na.rm = TRUE)),
      minWidth = 70
    )

    writers_col_groups[[length(writers_col_groups) + 1]] <-
      reactable::colGroup(
        name = name,
        columns = c(text_col, grade_col)
      )
  }

  reactable::reactable(
    data,
    columns = c(
      col_names,
      writers_col_names
    ),
    columnGroups = c(
      col_groups,
      writers_col_groups
    ),
    striped = TRUE,
    ...
  )
}

# standing Tabelle ----
reactable_standing <- function(data, columns = NULL, hide_cols = NULL, columnGroups = NULL, ...) {
  default_columns <- list(
    conference_name = reactable::colDef(name = "Conf"),
    matchup = reactable::colDef(name = "Matchup"),
    franchise_name = reactable_col_franchise_name(
      data = data,
      sticky = "left",
      style = list(borderRight = paste("2px solid", color_grey_light)),
      width = 250
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
    #luck_season_pctl = reactable_coldef_bg_pctl(
    #  name = "Glück",
    #  "luck_season",
    #  data = data
    #),
    #quality_season_pctl = reactable_coldef_bg_pctl(
    #  name = "Qualität",
    #  "quality_season",
    #  data = data
    #),
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
    )
  )

  # allow overriding/merging of columns
  if (!is.null(columns)) {
    default_columns <- utils::modifyList(default_columns, columns)
  }

  column_groups <- list(
    reactable::colGroup(
      name = "Standing",
      columns = c("wins_season", "losses_season", "wins_over_exp_pct")
    ),
    reactable::colGroup(
      name = "Power Ranking",
      columns = c(
        "elo_season_pctl", "pf_season_pctl", "pp_season_pctl",
        "pa_season_pctl", "war_starter_season_pctl",
        "wins_season_pctl", "all_play_wins_season_pctl",
        "eff_season_pctl", "wins_expected_end_of_season_pctl"
      )
    )
  )

  if (!is.null(columnGroups)) {
    column_groups <- append(
      column_groups,
      columnGroups
    )
  }

  reactable_default(
    data,
    columns = c(
      default_columns,
      purrr::set_names(
        purrr::map(
          hide_cols,
          ~ reactable::colDef(show = FALSE)
        ),
        hide_cols
      )
    ),
    columnGroups = column_groups,
    defaultExpanded = TRUE,
    borderless = TRUE,
    ...
  )
}
