league_id <- 63018
new_season_sept <- nflreadr::get_current_season()
new_season_march <- nflreadr::get_current_season(TRUE)
current_week <- nflreadr::get_current_week()

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
  "LB" = "#ff9ff3",
  "DB" = "#00d2d3"
)

color_text <- color_black

font <- "Poppins"

var.fontTextBold <- "Open Sans Semibold"
var.fontHeadline <- "Open Sans Semibold"

# dark: https://iibawards-prod.s3.amazonaws.com/projects/images/000/006/283/page.png?1693573818
# plot defaults ----
plot_defaults <- theme(
  plot.margin = ggplot2::margin(25, 25, 25, 25),
  text = ggplot2::element_text(color = color_text, family = font, lineheight = 1.2),

  plot.title = ggplot2::element_text(size = 24, face = "bold", lineheight = 0.8, margin = ggplot2::margin(b = 15)),
  plot.title.position = "plot",
  plot.subtitle = ggplot2::element_text(size = 16, margin = ggplot2::margin(t = -5, b = 15)),

  axis.title = ggplot2::element_text(size = 14, face = "bold"),
  axis.title.x = ggplot2::element_text(vjust = -5),
  axis.title.y = ggplot2::element_text(vjust = 2.5),
  #axis.title.y.right = ggplot2::element_text(vjust = 2.5, hjust = 1),
  axis.text = ggplot2::element_text(size = 12),
  #axis.line = element_line(color = var.colorAccent, linewidth = 0.5),
  #axis.ticks = element_line(color = var.colorAccent, linewidth = 0.5),

  strip.background = ggplot2::element_rect(fill = color_grey_dark),
  strip.text = ggplot2::element_text(size = 12, color = color_bg, face = "bold"),

  legend.background = ggplot2::element_blank(),
  legend.title = ggplot2::element_text(size = 14, face = "bold"),
  #legend.key = element_blank(),
  legend.key.size = ggplot2::unit(6, "pt"),
  legend.text = ggplot2::element_text(size = 12),
  legend.position = "top",
  legend.margin = ggplot2::margin(t = 25),

  panel.background = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_line(color = color_grey_light, linewidth = 0.35),
  panel.grid.minor = element_line(color = color_grey_light, linewidth = 0.25)
)

plot_clean <- theme(
  plot.background = ggplot2::element_blank(),
  panel.grid.major = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_blank()
)

## geoms ----

plot_geom_point <- function(...) {
  ggplot2::geom_point(size = 5, ...)
}

plot_geom_xspline <- function(...) {
  df %>%
    ggalt::geom_xspline(spline_shape = -0.2, ...) +
    ggplot2::aes(lwd = 1) +
    ggplot2::scale_linewidth_identity()
}

#plot_geom_large_text <- function(color = c_light, ...) {
#  geom_text(color = color, size = 6, family = "accent", ...)
#}

#plot_geom_xsmall_text <- function(...) {
#  geom_text(size = 2, family = "accent", ...)
#}

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
