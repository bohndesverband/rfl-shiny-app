# ggplot defaults ----
plot_defaults <- list(
  ggplot2::labs(
    caption = paste("RFL Tools, Stand", format(Sys.Date(), "%d.%m.%Y"))
  ),
  ggplot2::theme(
    plot.margin = ggplot2::margin(rem_to_pt(2), rem_to_pt(2), rem_to_pt(2), rem_to_pt(2), unit = "pt"),
    text = ggplot2::element_text(color = color_text, family = font, lineheight = 1.2),

    plot.title = ggplot2::element_text(size = rem_to_pt(2.074), face = "bold", lineheight = 0.8, margin = ggplot2::margin(b = rem_to_pt(2.074 * 0.25))),
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

plot_quadrants <- function(xmin, xmean, xmax, ymin, ymean, ymax, ltl, ltr, lbr, lbl, ctl = color_green, ctr = color_blue, cbr = color_yellow, cbl = color_red) { # ltp = label top left etc, bottom right etc.
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
      fill = ctl,
      alpha = 0.1
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
      fill = ctr,
      alpha = 0.1
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
      fill = cbr,
      alpha = 0.1
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
      fill = cbl,
      alpha = 0.1
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

## VOE ----
plot_voe_defaults <- function(title, subtitle, x, y = "VOE") {
  list(
    ggplot2::scale_colour_gradientn(colors = c(color_red, color_orange, color_green, color_blue), rescaler = ~ scales::rescale_mid(.x, mid = 0), guide = "none"),
    ggplot2::scale_size(guide = "none"),
    plot_defaults,
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x,
      y = y,
    ),
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  )
}

## pvar exp & voe ----
plot_pVARexp_voe <- function(df, selectedTeamNames = FALSE) {
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = pvar_exp, y = voe)) +
    plot_quadrants(
      xmin = min(df$pvar_exp),
      xmax = max(df$pvar_exp),
      ymin = min(df$voe),
      ymax = max(df$voe),
      xmean = mean(df$pvar_exp),
      ymean = mean(df$voe),
      ltl = "wenig Kapital, viel Value",
      ltr = "viel Kapital, viel Value",
      lbr = "viel Kapital, wenig Value",
      lbl = "wenig Kapital, wenig Value",
      ctl = color_blue,
      ctr = color_green,
      cbr = color_red,
      cbl = color_yellow
    ) +
    plot_voe_defaults(
      title = "Investment Returns der RFL Draftklassen",
      subtitle = "Jeder Punkt ist eine Draftklasse, die nach eingesetztem Kapital (pVARexp) und erhaltenem Wert (VOE) abgebildet wird.\nDie Größe spiegelt das Draftjahr wieder (je größer der Punkt, desto aktueller ist die Draftklasse).",
      x = "Investiertes Kapital (pVARexp)",
      y = "Value over Expected (VOE)"
    )

  if (length(selectedTeamNames) > 0) {
    p <- p +
      ggplot2::geom_point(
        size = 3,
        color = color_grey_light
      ) +
      ggiraph::geom_point_interactive(
        data = subset(df, franchise_id %in% selectedTeamNames),
        ggplot2::aes(
          tooltip = paste(
            franchise_name, season,
            "\n", picks, "Picks",
            "\npVAR:", pvar,
            "\nVOE:", voe),
          size = season,
          data_id = franchise_id,
          color = franchise_name
        ),
        alpha = 1
      ) +
      ggplot2::scale_color_discrete(palette = colors) +
      ggplot2::labs(
        color = "RFL Team"
      )
  } else {
    p <- p +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(
            franchise_name, season,
            "\n", picks, "Picks",
            "\npVAR:", pvar,
            "\nVOE:", voe),
          size = season,
          data_id = franchise_id,
          color = voe
        ),
        alpha = 0.5
      )
  }

  p +
    ggplot2::scale_size(range = c(3, 10), guide = "none")
}

# draft plot defauls ----
plot_draft_defaults <- function(df) {
  list <- list(
    ggplot2::geom_vline(xintercept = 36, color = color_grey_light),
    ggplot2::geom_vline(xintercept = 72, color = color_grey_light),
    ggplot2::geom_vline(xintercept = 108, color = color_grey_light),
    ggplot2::geom_vline(xintercept = 144, color = color_grey_light),
    ggplot2::geom_vline(xintercept = 180, color = color_grey_light),
    ggplot2::geom_vline(xintercept = 216, color = color_grey_light),
    ggplot2::geom_vline(xintercept = 252, color = color_grey_light),
    ggplot2::scale_color_manual(values = colors_position, guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)),
    plot_defaults,
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ),
    ggplot2::labs(
      x = "Overall Pick im Draft",
      color = "Position"
    )
  )

  list
}

# giraf defaults ----
girafe_default_output <- function(plot, width = 16, height = 9) {
  ggiraph::girafe(ggobj = plot, width_svg = width, height_svg = height) %>%
    ggiraph::girafe_options(
      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg, ";")),
      ggiraph::opts_hover_inv(css = "opacity:0.4"),
      ggiraph::opts_hover_key(css = "opacity:1")
    )
}
