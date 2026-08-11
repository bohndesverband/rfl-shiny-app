league_id <- 63018
new_season_sept <- nflreadr::get_current_season()
new_season_march <- nflreadr::get_current_season(TRUE)
current_week <- nflreadr::get_current_week()
current_week_thu <- nflreadr::get_current_week(TRUE)

season_before_wk_1 <- new_season_sept
season_before_wk_2 <- new_season_sept

if (current_week == 1) {
  season_before_wk_1 <- nflreadr::get_current_season() - 1
}

if (nflreadr::get_current_week(TRUE) < 2) {
  season_before_wk_2 <- nflreadr::get_current_season() - 1
}

# mfl api ----
mfl_api_base_sept <- paste0("https://www45.myfantasyleague.com/", new_season_sept)
mfl_api_base_march <- paste0("https://www45.myfantasyleague.com/", new_season_march)
mfl_connection <- ffscrapr::mfl_connect(season = new_season_march, league_id = league_id)

# base data----
position_order <- c("QB", "RB", "WR", "TE", "FLX", "PK", "DL", "LB", "DB", "IDP")
positions_grouped <- c("QB", "RB", "WR", "TE", "PK", "DL", "LB", "DB")
positions_full <- c("QB", "RB", "WR", "TE", "PK", "DT", "DE", "DL", "LB", "CB", "S", "DB")

# color ----
# https://flatuicolors.com/palette/se
color_grey_light <- "#d2dae2"
color_grey_mid <- "#808e9b"
color_grey_dark <- "#485460"
color_black <- "#1e272e"
color_red <- "#f53b57"
color_blue <- "#3c40c6"
color_cyan <- "#0fbcf9"
color_petrol <- "#00d8d6"
color_green <- "#05c46b"
color_orange <- "#ffa801"
color_yellow <- "#ffd32a"
color_bg <- "white"

colors <- c(color_red, color_blue, color_green, color_orange, color_cyan, color_yellow)

colors_position <- c(
  "QB" = "#feca57",
  "RB" = "#1dd1a1",
  "WR" = "#54a0ff",
  "TE" = "#ff6b6b",
  "PK" = "#c8d6e5",
  "DL" = "#48dbfb",
  "DT" = "#48dbfb",
  "DE" = "#ffa801",
  "LB" = "#ff9ff3",
  "DB" = "#00c0c1",
  "CB" = "#00c0c1",
  "S" = "#3c40c6"
)

colors_positions_grouped <- c(
  "QB" = "#feca57",
  "RB" = "#1dd1a1",
  "WR" = "#54a0ff",
  "TE" = "#ff6b6b",
  "PK" = "#c8d6e5",
  "DL" = "#48dbfb",
  "LB" = "#ff9ff3",
  "DB" = "#00c0c1"
)

color_text <- color_black

#("Poppins")
gdtools::register_gfont("Open Sans")
gdtools::register_gfont("Poppins")

#systemfonts::register_font("Poppins")

font <- "Poppins"

# dark: https://iibawards-prod.s3.amazonaws.com/projects/images/000/006/283/page.png?1693573818
rem_to_pt <- function(rem, base_px = 16) {
  rem * base_px * 0.75
}

# gt ----
gtDefaults <- function(df) {
  df %>%
    gt::tab_options(
      table.font.size = gt::px(14),
      table.font.color = color_text,
      table.border.top.color = color_bg,
      table.border.bottom.color = color_bg,
      table_body.border.bottom.color = color_bg,
      table_body.border.bottom.width = gt::px(5),

      heading.border.bottom.color = color_bg,
      heading.title.font.size = gt::px(22),
      heading.title.font.weight = "bold",
      heading.subtitle.font.size = gt::px(16),

      row_group.font.weight = "bold",
      row_group.padding.horizontal = gt::px(15),

      data_row.padding.horizontal = gt::px(15),

      stub.border.color = color_grey_light,

      column_labels.font.weight = "bold",
      column_labels.padding = gt::px(10),
      column_labels.padding.horizontal = gt::px(15),
      column_labels.border.top.color = color_bg,
      column_labels.border.bottom.color = color_grey_dark,

      footnotes.background.color = color_grey_light,
      footnotes.padding = gt::px(7),
      footnotes.padding.horizontal = gt::px(15)
    )
}


gt_stack_cols <- function(df, col1, col2) {
  df %>%
    gtExtras::gt_merge_stack(
      col1,
      col2,
      small_cap = FALSE,
      palette = c(color_text, color_grey_mid),
      #font_size = c("14px", "10px"),
      font_weight = c("normal", "normal")
    )
}

## player tables ----
gt_player <- function(df) {
  df %>%
    gt::cols_label(
      display_name = "Spieler",
      position = "Pos",
      team = "Team"
    ) %>%
    gt::tab_options(
      ihtml.active = TRUE,
      ihtml.use_filters = TRUE,
      ihtml.use_search = TRUE,
      ihtml.use_pagination = TRUE,
      ihtml.use_page_size_select = TRUE,
      ihtml.page_size_default = 12,
      ihtml.page_size_values = c(12, 25, 50, 100),
      ihtml.use_highlight = TRUE
    ) %>%
    gtDefaults
}

# gt_pct_bar
gt_pctl_bar <- function(df, value, pctl) {
  df %>%
    nflplotR::gt_pct_bar(
      value,
      pctl,
      value_scale = 100,
      hide_col_pct = TRUE,
      value_position = "above",
      background_fill.height = "8px",
      background_fill.color = color_grey_light,
      fill_palette = c(color_red, color_orange, color_yellow, color_green, color_blue)
    )
}

# reactable ----
reactable_default <- function(data, ..., columns = NULL, pagination = FALSE) {
  defaults <- list(
    data = data,
    columns = columns,
    defaultColDef = colDef(
      headerStyle = list(background = color_bg),
      vAlign = "center",
      headerVAlign = "bottom",
      footerStyle = list(fontWeight = "bold", textAlign = "center")
    ),
    theme = reactableTheme(
      borderColor = color_grey_light,
      stripedColor = color_bg
    ),
    striped = TRUE,
    wrap = FALSE,
    pagination = pagination
  )

  args <- c(defaults, list(...))

  do.call(reactable::reactable, args)
}

## columns ----
text_color <- function(bg) {
  if (is.na(bg) || bg == "transparent") {
    return(NULL)
  }

  rgb <- grDevices::col2rgb(bg)

  lum <- (0.2126 * rgb[1] +
            0.7152 * rgb[2] +
            0.0722 * rgb[3]) / 255

  if (lum > 0.5) "#000000" else "#FFFFFF"
}

reactable_coldef_bg <- function(
    name,
    cell_fun = NULL,
    palette_fun,
    details_fun = NULL,
    minWidth = 100,
    footer_fun = NULL,
    ...
) {
  reactable::colDef(
    name = name,
    minWidth = minWidth,
    ...,
    cell = if (!is.null(cell_fun)) {
      function(index) {
        cell_fun(index)
      }
    } else NULL,

    style = function(value) {
      bg <- palette_fun(value)

      list(
        background = bg,
        color = text_color(bg),
        textAlign = "center"
      )
    },

    details = if (!is.null(details_fun)) {
      function(index) {
        details_fun(index)
      }
    } else NULL,

    footer = if (!is.null(footer_fun)) {
      function(values) footer_fun(values, na.rm = TRUE)
    } else NULL
  )
}

reactable_coldef_color <- function(
    palette_fun,
    footer_fun,
    ...
) {
  reactable::colDef(
    ...,
    style = function(value) {
      list(
        color = palette_fun(value),
        textAlign = "center",
        fontWeight = "bold"
      )
    },

    footer = if (!is.null(footer_fun)) {
      function(values) footer_fun(values, na.rm = TRUE)
    } else NULL
  )
}

## rainbow scale ----
scale_rainbow <- function(domain_values) {
  scales::col_numeric(
    palette = c(color_red, color_orange, color_yellow, color_green, color_blue),
    domain = domain_values,
    na.color = "transparent"
  )
}

scale_rainbow_reverse <- function(domain_values) {
  scales::col_numeric(
    palette = c(color_blue, color_green, color_yellow, color_orange, color_red),
    domain = domain_values,
    na.color = "transparent"
  )
}

scale_rainbow_text <- function(domain_values) {
  scales::col_numeric(
    palette = c(color_bg, color_black, color_bg),
    domain = domain_values,
    na.color = "transparent"
  )
}

scale_red_green <- function(domain_values) {
  scales::col_numeric(
    palette = c(color_red, color_green),
    domain = domain_values,
    na.color = "transparent"
  )
}

scale_green_red <- function(domain_values) {
  scales::col_numeric(
    palette = c(color_green, color_red),
    domain = domain_values,
    na.color = "transparent"
  )
}

scale_red_blue <- function(domain_values) {
  scales::col_numeric(
    palette = c(color_red, color_blue),
    domain = domain_values,
    na.color = "transparent"
  )
}
