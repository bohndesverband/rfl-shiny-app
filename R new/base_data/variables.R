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
#gdtools::register_gfont("Open Sans")

#systemfonts::register_font("Poppins")

font <- "Poppins"

var.fontTextBold <- "Open Sans Semibold"
var.fontHeadline <- "Open Sans Semibold"

# dark: https://iibawards-prod.s3.amazonaws.com/projects/images/000/006/283/page.png?1693573818
# ggplot defaults ----
plot_defaults <- list(
  ggplot2::labs(
    caption = paste("RFL Tools, Stand", format(Sys.Date(), "%d.%m.%Y"))
  ),
  ggplot2::theme(
    plot.margin = ggplot2::margin(25, 25, 25, 25),
    text = ggplot2::element_text(color = color_text, family = font, lineheight = 1.2),


    plot.title = ggplot2::element_text(size = 24, face = "bold", lineheight = 0.8, margin = ggplot2::margin(b = 15)),
    plot.title.position = "plot",
    plot.subtitle = ggplot2::element_text(size = 16, margin = ggplot2::margin(t = -5, b = 15)),
    plot.caption = ggplot2::element_text(size = 12),

    axis.title = ggplot2::element_text(size = 14, face = "bold"),
    axis.title.x = ggplot2::element_text(vjust = -5),
    axis.title.y = ggplot2::element_text(vjust = 2.5),
    #axis.title.y.right = ggplot2::element_text(vjust = 2.5, hjust = 1),
    axis.text = ggplot2::element_text(size = 12),
    #axis.line = element_line(color = var.colorAccent, linewidth = 0.5),
    axis.ticks = ggplot2::element_blank(),

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
)

plot_clean <- list(
  ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  )
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

plot_geom_vline <- function(xintercept, ...) {
  ggplot2::geom_vline(xintercept = xintercept, color = color_grey_mid, linewidth = 0.5, linetype = "dashed", ...)
}

plot_geom_hline <- function(yintercept, ...) {
  ggplot2::geom_hline(yintercept = yintercept, color = color_grey_mid, linewidth = 0.5, linetype = "dashed", ...)
}

plot_quadrants <- function(xmin, xmean, xmax, ymin, ymean, ymax, ltl, ltr, lbr, lbl) { # ltp = label top left etc, bottom right etc.
  abs_x = (abs(xmax - xmin) / 100) * 3
  abs_y = (abs(ymax - ymin) / 100) * 5

  list <- list(
    # oben links fläche
    ggplot2::annotate(
      "rect",
      xmin = xmin - abs_x,
      xmax = xmean,
      ymin = ymean,
      ymax = ymax + abs_y,
      fill = color_green,
      alpha = 0.10
    ),
    # oben links text
    ggplot2::annotate(
      "text",
      label = ltl,
      x = xmin,
      y = ymax,
      hjust = 0,
      vjust = 1,
      lineheight = 0.9,
    ),
    # oben rechts fläche
    ggplot2::annotate(
      "rect",
      xmin = xmean,
      xmax = xmax + abs_x,
      ymin = ymean,
      ymax = ymax + abs_y,
      fill = color_blue,
      alpha = 0.15
    ),
    # oben rechts text
    ggplot2::annotate(
      "text",
      label = ltr,
      x = xmax,
      y = ymax,
      hjust = 1,
      vjust = 1,
      lineheight = 0.9,
    ),
    # unten rechts fläche
    ggplot2::annotate(
      "rect",
      xmin = xmean,
      xmax = xmax + abs_x,
      ymin = ymin - abs_y,
      ymax = ymean,
      fill = color_yellow,
      alpha = 0.2
    ),
    # unten rechts text
    ggplot2::annotate(
      "text",
      label = lbr,
      x = xmax,
      y = ymin,
      hjust = 1,
      vjust = 0,
      lineheight = 0.9,
    ),
    # unten links fläche
    ggplot2::annotate(
      "rect",
      xmin = xmin - abs_x,
      xmax = xmean,
      ymin = ymin - abs_y,
      ymax = ymean,
      fill = color_red,
      alpha = 0.15
    ),
    # unten links text
    ggplot2::annotate(
      "text",
      label = lbl,
      x = xmin,
      y = ymin,
      hjust = 0,
      vjust = 0,
      lineheight = 0.9,
    ),
    plot_geom_vline(xmean),
    plot_geom_hline(ymean),
    ggplot2::scale_x_continuous(expand = c(0, 0)),
    ggplot2::scale_y_continuous(expand = c(0, 0))
  )

  list
}



#plot_geom_large_text <- function(color = c_light, ...) {
#  geom_text(color = color, size = 6, family = "accent", ...)
#}

#plot_geom_xsmall_text <- function(...) {
#  geom_text(size = 2, family = "accent", ...)
#}

## elo ----
plot_elo_defaults <- list(
  ggplot2::aes(lwd = 1.2),
  ggplot2::scale_linewidth_identity(),
  ggplot2::scale_fill_continuous(type = "gradient"),
  ggplot2::scale_fill_gradientn(colors = c("#f1f4f6", color_grey_mid), guide = "none"),
  geom_hline(yintercept = 1500, color = color_grey_light, linewidth = 0.5, alpha = 0.75), # default elo
  ggplot2::scale_color_discrete(type = colors),
  plot_defaults,
  plot_clean,
  ggplot2::labs(
    y = "ELO",
    x = "",
    color = ""
  ),
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = c(0.08, 0.9),
    axis.text.x = ggplot2::element_blank()
  )
)

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
