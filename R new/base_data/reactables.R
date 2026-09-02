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
        pvar_exp = coldef_pvar_exp(footer_fun = sum),
        voe = coldef_voe(footer = sum)
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
        details_fun = function(index) {

          row <- data[index, ]
          value <- row[[pos]]

          if (is.null(value) || is.na(value) || value == "") {
            return(NULL)
          }

          position_details(index, pos)
        },
        footer_fun = sum
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
        minWidth = 150
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
          minWidth = 170
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
          minWidth = 170
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



## bar chart helper ----
bar_chart <- function(text, width = "100%", height = "1rem", fill = color_grey_mid, background = color_grey_light) {
  bar <- htmltools::div(class="bar-chart--bar", style = list(background = fill, width = width, height = height))
  chart <- htmltools::div(class="bar-chart--chart", style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  label <- htmltools::span(class="bar-chart--label", text, style = list(fontSize = "0.8em"))

  htmltools::div(style = list(display = "flex", alignItems = "center"), label, chart)
}

## draft grades ----


## reusable reactable for draft class teams ----
draft_classes_teams_reactable <- function(data, picks_reactive, column_groups = NULL, columns = NULL, ...) {
  # define default columns
  default_columns <- list(
    franchise_name = reactable::colDef(
      name = "Team",
      html = TRUE,
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
          htmltools::div(htmltools::HTML(paste0("<small>", "aus ", (new_season_march - 2017) * 36, "</small>")))
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
            minWidth = 350
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
    height = 745,
    ...
  )
}

## draft grades defauls -----
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
    ...
  )
}

