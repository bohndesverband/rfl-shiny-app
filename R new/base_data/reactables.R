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
