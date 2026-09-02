rfl_rookie_drafts <- rfl_drafts_data %>%
  dplyr::filter(season > 2016)

selected_draft_year <- shiny::reactive({
  selected_draft_year <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear)
    #dplyr::filter(season == 2025)
})

selected_draft_team <- shiny::reactive({
  selected_draft_team <- selected_draft_year() %>%
    dplyr::filter(franchise_id == input$selectRflTeam)
    #dplyr::filter(franchise_id == "0027")
})

selected_draft_players <- shiny::reactive({
  selected_draft_players <- selected_draft_team() %>%
    dplyr::pull(mfl_id)
})

selected_draft_grades <- shiny::reactive({
  selected_draft_grades <- rfl_draft_grades %>%
    dplyr::select(-asset_id_new) %>%
    dplyr::filter(draft_class == input$selectYear & team_id == input$selectRflTeam)
    #dplyr::filter(draft_class == 2025 & team_id == "0027")
})

selected_draft_trades <- shiny::reactive({
  selected_draft_trades <- rfl_trade_history %>%
    dplyr::filter(
      #grepl(paste("FP", input$selectRflTeam, input$selectYear, sep = "_"), trade_asset_ids)
      #grepl(paste("FP", "0016", "2025", sep = "_"), trade_asset_ids)
      grepl(input$selectRflTeam, franchise_ids)
      #grepl("0027", franchise_ids)

    )
})

rfl_draft_classes_sum_filtered <- shiny::reactive({
  rfl_draft_classes_sum_filtered <- rfl_draft_classes_sum %>%
    dplyr::filter(franchise_id == input$selectRflTeam) %>%
    #dplyr::filter(franchise_id == "0027") %>%
    dplyr::select(franchise_name, season, picks, rank_season, pvar, voe, rank, voe_pctl)
})


# alle Draftklassen ----
## tabelle ----
output$draft_classes_team <- reactable::renderReactable({
  draft_classes_teams_reactable(
    rfl_draft_classes_sum_filtered(),
    rfl_rookie_drafts,
    columns = list(
      season = reactable::colDef(
        name = "Saison",
        minWidth = 70
      ),
      rank_season = reactable_coldef_color(
        name = "#",
        palette_fun = scale_green_red(c(1, 36)),
        footer_fun = NULL,
        minWidth = 50
      )
    ),
    column_groups = list(
      reactable::colGroup(name = paste0("2017-", new_season_march - 1), columns = c("rank", "voe_pctl")),
      reactable::colGroup(name = "VOE", columns = c("rank_season", "voe"))
    ),
    rowStyle = function(index) {
      if (rfl_draft_classes_sum_filtered()$season[index] == input$selectYear) {
        list(
          background = color_grey_light
        )
      }
    }
  )
})

## plot ----
output$draft_classes_team_chart <- ggiraph::renderGirafe({
  plot <- ggplot2::ggplot(rfl_draft_classes_sum, ggplot2::aes(y = season, x = voe, color = franchise_name)) +
    plot_geom_vline(0) +
    ggplot2::geom_boxplot(ggplot2::aes(group = season), fill = color_grey_light, color = color_grey_mid, linewidth = 0.15, outliers = FALSE) +

    ggiraph::geom_jitter_interactive(
      data = subset(rfl_draft_classes_sum, !franchise_id %in% c(input$selectRflTeam)),
      ggplot2::aes(
        tooltip = paste(franchise_name, "\nVOE:", voe),
        size = picks,
        alpha = picks
      ),
      width = 0.25, color = color_grey_mid
    ) +

    ggplot2::geom_point(data = subset(rfl_draft_classes_sum, franchise_id %in% c(input$selectRflTeam)), ggplot2::aes(size = picks)) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1, 8), guide = "none") +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean+
    ggplot2::scale_y_continuous(labels = c(2017:new_season_march - 1), breaks = c(2017:new_season_march - 1)) +
    ggplot2::labs(
      title = paste0("Total VOE aller RFL Draftklassen von 2017-", new_season_march - 1),
      subtitle = "Jeder Punkt ist eine RFL Draftklasse.\nJe größer der Punkt, desto mehr Picks hatte die Draftklasse.",
      x = "Total VOE",
      y = "Draft",
      color = ""
    )

  ggiraph::girafe(ggobj = plot, width_svg = 10, height_svg = 13) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.4")
    )
})

# einzelne Draftklasse ----
## kapital ----

# TODO: maximum für pvar aus daten ermitteln

output$draft_class_capital <- shiny::renderPlot({
  team_pick <- rfl_draft_orders %>%
    dplyr::filter(season == input$selectYear & franchise_id == input$selectRflTeam) %>%
    dplyr::pull(pick)

  team_draft_capital_theory <- rfl_drafts_data %>%
    dplyr::filter(season == input$selectYear & pick == team_pick) %>%
    dplyr::group_by(round) %>%
    dplyr::summarise(pvar_exp = round(sum(pvar_exp), 2))

  team_draft_capital_actual <- selected_draft_team() %>%
    dplyr::group_by(round) %>%
    dplyr::summarise(dplyr::across(c(pvar, pvar_exp), ~ round(sum(.x), 2))) %>%
    dplyr::mutate(direction = ifelse(pvar >= pvar_exp, "up", "down"))

  plot <- ggplot2::ggplot(team_draft_capital_theory, ggplot2::aes(x = round, y = pvar_exp)) +
    ggplot2::geom_smooth(color = color_grey_mid, se = FALSE) +
    ggplot2::scale_x_continuous(breaks = c(1:6)) +
    ggplot2::scale_y_continuous(limits = c(0, 20), breaks = , expand = 0) +
    ggplot2::scale_color_manual(values = c("down" = color_red, "up" = color_blue), guide = "none") +
    plot_defaults +
    ggplot2::labs(
      title = paste("Draftkapital vs. Ertrag", selected_team_name(), input$selectYear),
      subtitle = "Der Schweif symbolisiert die Differenz zwischen dem eingesetzten Draftkapital\n(pVARexp) und dem Ertrag (pVAR). Ist das große Ende des Schweifs über dem\nkleinen, haben die Picks das eingesetzte Kapital übertroffen.\nDie Linie zeigt die pVARexp aller originalen Draftpicks des Teams an.\nLiegt der Schweif über der Linie hat der Owner mehr Kapital angesammelt.",
      x = "Draft Runde",
      y = "pVAR",
      color = ""
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  if (input$selectYear == new_season_march) {
    plot <- plot +
      ggplot2::geom_point(data = team_draft_capital_actual, color = color_blue, size = 4.7)
  } else {
    plot <- plot +
      ggforce::geom_link(data = team_draft_capital_actual, aes(xend = round, yend = pvar, linewidth = ggplot2::after_stat(index), color = direction)) +
      ggplot2::scale_size_continuous(guide = "none") +
      ggplot2::scale_linewidth_continuous(guide = "none") +
      ggplot2::geom_point(data = team_draft_capital_actual, ggplot2::aes(y = pvar, color = direction), shape = 21, size = 4.7, fill = color_bg, stroke = 1)
  }

  plot
}, height = 500)

## kapital pro positionsgruppe ----
output$draft_class_capital_positions <- shiny::renderPlot({
  team_draft_capital_positions <- selected_draft_team() %>%
    dplyr::group_by(pos_grouped) %>%
    dplyr::summarise(
      dplyr::across(c(pvar_exp, pvar), ~ round(sum(.x), 2)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(color = ifelse(pvar > pvar_exp, "less", pos_grouped))

  max_value <- max(
    team_draft_capital_positions$pvar,
    team_draft_capital_positions$pvar_exp,
    na.rm = TRUE
  )

  plot <- ggplot2::ggplot(team_draft_capital_positions, ggplot2::aes(x = factor(pos_grouped, levels = positions_grouped), y = pvar_exp, fill = factor(pos_grouped, levels = positions_grouped))) +
    ggplot2::coord_polar() +
    ggplot2::scale_fill_manual(values = colors_positions_grouped, guide = "none") +
    ggplot2::scale_color_manual(values = c(colors_positions_grouped, "less" = color_bg), guide = "none") +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, ceiling(max_value)), breaks = c(0, ceiling(max_value))) +
    plot_defaults +
    ggplot2::labs(
      title = paste0("Eingesetztes Draftkapital pro Position\n", paste(selected_team_name(), input$selectYear)),
      subtitle = paste("Die Segmente zeigen den pVAR, die Linien den pVARexp.\nÜberragt das Segment die Linie, haben die Picks das eingesetzte Kapital übertroffen."),
      x = "",
      y = ""
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      #axis.text = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )

  if (input$selectYear < new_season_march) {
    plot <- plot +
      ggplot2::geom_col(ggplot2::aes(y = pvar), width = 1) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = pvar_exp, ymax = pvar_exp, color = factor(color, levels = c(positions_grouped, "less"))),
        width = 1, linewidth = 0.5
      )
  } else {
    plot <- plot +
      ggplot2::geom_col(width = 1, alpha = 1)
  }

  plot
}, height = 600)

## picks --scale_fill_discrete()## picks ----
output$team_draft_class <- ggiraph::renderGirafe({
  type <- input$selectDraftClassCharts

  draft_class_plot_defaults <- list(
    ggplot2::scale_y_discrete(
      limits = rev
    ),
    plot_defaults
  )

  if (type == "ADP") {
  ## ADP ----
    data <- selected_draft_team() %>%
      dplyr::select(season, asset_name, first_pick, second_pick, third_pick, mfl_id) %>%
      tidyr::pivot_longer(c(first_pick, second_pick, third_pick), names_to = "pick", values_to = "overall") %>%
      dplyr::left_join(
        rfl_drafts_data %>%
          dplyr::select(season, overall, pick_info, pick_value, adp),
        by = c("season", "overall")
      )

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = overall, y = asset_name)) +
      draft_class_plot_defaults +
      plot_draft_defaults() +

      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste0(pick_info,
                           "\nOverall: ", overall,
                           "\nValue: ", pick_value,
                           "\nADP: ", adp
          ),
          data_id = mfl_id
        ),
        size = 5,
        color = color_grey_light
      ) +
      ggplot2::geom_point(
        data = selected_draft_team(),
        ggplot2::aes(x = adp),
        color = color_grey_mid,
        fill = color_grey_mid,
        shape = 25,
        size = 3
      ) +
      ggiraph::geom_point_interactive(
        data = selected_draft_team(),
        ggplot2::aes(
          tooltip = paste0(pick_info,
                           "\nOverall:", overall,
                           "\nValue: ", pick_value,
                           "\nADP: ", adp
          ),
          color = factor(pos_grouped, positions_grouped),
          data_id = mfl_id
        ),
        size = 8
      ) +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse mit ihren 3 Copies und dem MFL ADP (Dreieck).\nDer Value im Tooltip ist die Differenz des Picks zur 2. Copy des Spielers.",
      ) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(linewidth = 1, color = color_grey_light)
      )
  } else if (type == "WAR") {
    ## WAR ----
    shiny::validate(
      shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
    )

    data <- rfl_war_data %>%
      dplyr::filter(player_id %in% selected_draft_players()) %>%
      dplyr::left_join(selected_draft_team() %>% dplyr::select(asset_name, mfl_id), by = c("player_id" = "mfl_id"))

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = war, y = asset_name, color = factor(pos, positions_grouped), alpha = season, size = season)) +
      draft_class_plot_defaults +
      plot_geom_vline(0) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(season, "\nWAR: ", war),
          data_id = player_id
        )
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 1), limits = c(min(data$season), max(data$season)), guide = "none") +
      ggplot2::scale_size_continuous(range = c(2, 8), limits = c(min(data$season), max(data$season)), guide = "none") +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse mit ihren Wins Above Replacement (WAR) seit dem Draft.",
        x = "WAR"
      )
  } else if (type == "ELO") {
    ## ELO ----
    shiny::validate(
      shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
    )

    data <- player_elo %>%
      dplyr::filter(mfl_id %in% selected_draft_players()) %>%
      dplyr::group_by(mfl_id, season) %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(selected_draft_team() %>% dplyr::select(asset_name, mfl_id), by = "mfl_id")

    plot <- ggplot2::ggplot(data, ggplot2::aes(x = player_elo_post, y = asset_name, color = factor(position, positions_grouped), alpha = season, size = season)) +
      draft_class_plot_defaults +
      plot_geom_vline(1500) +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(season, "\nELO: ", player_elo_post),
          data_id = mfl_id
        )
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 1), limits = c(min(data$season), max(data$season)), guide = "none") +
      ggplot2::scale_size_continuous(range = c(2, 8), limits = c(min(data$season), max(data$season)), guide = "none") +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse mit ihrer ELO seit dem Draft.",
        x = "ELO am Ende der Saison"
      )
  } else if (type == "VOE") {
    ## VOE ----
    shiny::validate(
      shiny::need(input$selectYear < new_season_march, "Noch keine Daten vorhanden")
    )

    plot <- ggplot2::ggplot(selected_draft_team(), ggplot2::aes(x = voe, y = asset_name, color = factor(pos_grouped, positions_grouped))) +
      draft_class_plot_defaults +
      plot_geom_vline(0) +

      ggiraph::geom_point_interactive(
        ggplot2::aes(
          tooltip = paste(season, "\nVOE: ", voe),
          data_id = mfl_id
        ),
        size = 8
      ) +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse und ihr Value over Expected (VOE).",
        x = "VOE"
      )

    # Liga vergleich
    #plot <- ggplot2::ggplot(selected_draft_team(), ggplot2::aes(x = overall, y = pvar, color = factor(pos_grouped, positions_grouped))) +
    #  draft_class_plot_defaults +
    #  plot_draft_defaults()

    #  ggplot2::geom_smooth(
    #    data = rfl_draft_pvar_exp,
    #    ggplot2::aes(y = pvar_exp),
    #    se = FALSE,
    #    color = color_red,
    #    size = 0.5
    #  ) +
    #  ggiraph::geom_point_interactive(
    #    data = selected_draft_year(),
    #    ggplot2::aes(
    #      tooltip = paste(
    #        player_name, paste0("(", pos_grouped, ", ", team, ")"),
    #        "\n", franchise_name,
    #        "\nPick: ", paste0(round, ".", pick, " (#", overall, ")"),
    #        "\nVOE:", voe),
    #      data_id = mfl_id
    #    ),
    #    color = color_grey_light,
    #    alpha = 0.6,
    #    size = 5,
    #    hover_nearest = TRUE
    #  ) +
    #  ggiraph::geom_point_interactive(
    #    ggplot2::aes(
    #      tooltip = paste(
    #        player_name, paste0("(", pos_grouped, ", ", team, ")"),
    #        "\n", franchise_name,
    #        "\nPick: ", paste0(round, ".", pick, " (#", overall, ")"),
    #        "\nVOE:", voe),
    #      data_id = mfl_id
    #    ),
    #    size = 7,
    #    hover_nearest = TRUE
    #  ) +
    #  ggplot2::labs(
    #    y = "pVAR"
    #  )
  } else if (type == "Bewertung") {
    ## Bewertung ----

    shiny::validate(
      shiny::need(nrow(selected_draft_grades()) > 0, "Noch keine Daten vorhanden")
    )

    breaks <- seq(1, 20/3, by = 1/3)

    labels <- c(
      "1+", "1", "1-",
      "2+", "2", "2-",
      "3+", "3", "3-",
      "4+", "4", "4-",
      "5+", "5", "5-",
      "6+", "6", "6-"
    )

    plot <- ggplot2::ggplot(data = subset(selected_draft_grades(), !is.na(asset_name)), ggplot2::aes(x = grade, y = asset_name, color = user, alpha = year, size = year)) +
      draft_class_plot_defaults +
      ggplot2::geom_vline(
        data = selected_draft_grades() %>%
          dplyr::filter(
            pick == "klasse",
            year == max(year, na.rm = TRUE)
          ),
        ggplot2::aes(xintercept = grade, color = user),
        linewidth = 0.5, linetype = "dashed"
      ) +

      ggiraph::geom_jitter_interactive(
        ggplot2::aes(
          tooltip = paste(user, year, "\nNote: ", grade),
          data_id = user
        ),
        width = 0,
        height = 0.3
      ) +
      ggplot2::scale_x_continuous(
        limits = c(1, 6.666),
        breaks = breaks,
        minor_breaks = NULL,
        labels = labels
      ) +
      ggplot2::scale_alpha_continuous(range = c(0.2, 1), limits = c(min(selected_draft_grades()$year), max(selected_draft_grades()$year)), guide = "none") +
      ggplot2::scale_size_continuous(range = c(4, 8), limits = c(min(selected_draft_grades()$year), max(selected_draft_grades()$year)), guide = "none") +
      ggplot2::scale_color_discrete(palette = colors) +
      ggplot2::labs(
        subtitle = "Abgebildet werden alle Draftpicks der Klasse und ihre Bewertungen nach dem Draft, einem, drei und fünf Jahren.\nDie gestrichelten Linien sind die letzten Bewertungen der gesamten Klasse.",
        x = "Bewertung",
        color = ""
      )
  }

  if (type != "Bewertung") {
    plot <- plot +
      ggplot2::scale_color_manual(values = colors_position, guide = ggplot2::guide_legend(direction = "horizontal", nrow = 1)) +
      ggplot2::labs(
        color = "Position",
      )
  }

  plot <- plot +
    ggplot2::labs(
      title = paste("Draftpicks", selected_team_name(), input$selectYear, "-", input$selectDraftClassCharts),
      y = ""
    ) +
    theme(
      plot.margin = ggplot2::margin(rem_to_pt(2), rem_to_pt(2), rem_to_pt(2), 0, unit = "pt"),
    )

  player_count <- nrow(selected_draft_team())

  ggiraph::girafe(ggobj = plot, width_svg = 16, height_svg = 5 + (as.integer(player_count) * 0.3)) %>%
    ggiraph::girafe_options(
      ggiraph::opts_hover(css = paste0("fill:", color_grey_dark, ";stroke:", color_bg)),
      ggiraph::opts_hover_inv(css = "opacity:0.2")
    )
})

# grades ----
output$team_draft_class_grades <- reactable::renderReactable({
  shiny::validate(
    shiny::need(nrow(selected_draft_grades()) > 0, "Noch keine Daten vorhanden")
  )

  assets <- selected_draft_grades() %>%
    dplyr::filter(!is.na(grade)) %>%
    dplyr::select(pick, text, grade, user, asset_name_output, year) %>%
    tidyr::pivot_wider(names_from = c(user), values_from = c(text, grade))

  assets_latest <- assets %>%
    dplyr::group_by(pick) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::distinct() %>%
    dplyr::select(-year)

  writers <- sort(unique(selected_draft_grades()$user))

  row_details <- function(index) {
    selected_asset <- assets_latest[index, ]$pick
    #selected_asset <- "trade_2023012"

    selected_grades <- assets %>%
      dplyr::filter(pick == selected_asset)

    trade_details <- NULL
    player_details <- NULL
    data <- NULL

    ## Spielertabellen ----
    if (selected_asset != "klasse" || startsWith(selected_asset, "trade_")) {
      data <- selected_draft_team()

      if (selected_asset == "laterounds") {
        data <- data %>%
          dplyr::filter(round > 3)
      } else {
        data <- data %>%
          dplyr::filter(pick_id == selected_asset)
      }

      data <- data %>%
        dplyr::arrange(overall) %>%
        dplyr::select(pick_id, season, round_pick, player_name_with_badge, franchise_name, pvar, voe, pos_team, draft_range_subline, player_count, ppg, best_pos_rank, war_career, war_career_rank, current_player_elo, current_player_elo_rank) %>%
        dplyr::left_join(
          selected_draft_grades() %>%
            dplyr::select(pick, grade_avg, year) %>%
            dplyr::distinct() %>%
            dplyr::filter(year == max(year)) %>%
            dplyr::select(-year),
          by = c("pick_id" = "pick")
        ) %>%
        dplyr::select(-pick_id)

      if (input$selectYear == new_season_march) {
        data <- data %>%
          dplyr::select(-pvar, -voe)
      }

      if (nrow(data) > 0) {
        player_details <- htmltools::tagList(
          htmltools::h5("Pickanalyse"),
          draft_class_players_reactable(
            data,
            col_names = list(
              ppg = reactable_coldef_bg(
                name = "PPG",
                palette_fun = scale_rainbow(5:20),
                minWidth = 80
              ),
              best_pos_rank = reactable_coldef_color(
                name = "Top Finish",
                palette_fun = scale_rainbow(48:8, reverse = TRUE),
                minWidth = 80
              ),
              war_career = reactable_coldef_bg(
                name = "Career",
                palette_fun = scale_rainbow((-0.5 * (new_season_sept - data$season + 1)):(1.5 * (new_season_sept - data$season + 1))),
                minWidth = 80
              ),
              war_career_rank = reactable_coldef_color(
                name = "Rank*",
                palette_fun = scale_red_green(1:data$player_count, reverse = TRUE),
                minWidth = 50
              ),
              current_player_elo = reactable_coldef_bg(
                name = "Current",
                palette_fun = scale_rainbow((1500 - (200 * (new_season_sept - data$season + 1))):(1500 + (200 * (new_season_sept - data$season + 1)))),
                minWidth = 80
              ),
              current_player_elo_rank = reactable_coldef_color(
                name = "Rank*",
                palette_fun = scale_red_green(1:data$player_count, reverse = TRUE),
                minWidth = 50
              ),
              grade_avg = reactable_coldef_bg(
                name = "Ø Note",
                palette_fun = scale_rainbow(1:6, reverse = TRUE),
                minWidth = 80
              ),
              season = reactable::colDef(show = FALSE),
              player_count = reactable::colDef(show = FALSE)
            ),
            col_groups = list(
              reactable::colGroup(
                name = "ELO",
                columns = c("current_player_elo", "current_player_elo_rank")
              ),
              reactable::colGroup(
                name = "FPts",
                columns = c("ppg", "best_pos_rank")
              ),
              reactable::colGroup(
                name = "WAR",
                columns = c("war_career", "war_career_rank")
              )
            )
          ),
          htmltools::div(class="small", "*Rank = Platzierung innerhalb der Positionsgruppe dieser Drafklasse", style="margin-top: 0.5rem; margin-bottom: 1rem;")
        )
      }
    }

    ## Trades ----
    if (grepl("_", selected_asset)) {
      data <- selected_draft_trades()

      if (grepl("trade", selected_asset)) {
        data <- data %>%
          dplyr::filter(trade_id == stringr::str_split_i(selected_asset, "_", 2))
      } else {
        #asset_id <- paste0("DP_", stringr::str_replace(selected_asset, "_", "."))
        #selected_asset <- "2_01"

        data <- data %>%
          dplyr::filter(grepl(paste("DP", selected_asset, input$selectYear, sep = "_"), asset_ids))
      }

      data <- data %>%
        dplyr::mutate(
          date = format(date, format = "%d.%m.%Y"),
          trade_side = ifelse(franchise_name == selected_team_name(), "Abgegeben", "Geholt")
        ) %>%
        dplyr::arrange(dplyr::desc(trade_side)) %>%
        dplyr::select(date, trade_side, asset_names_with_draft_info_badge) %>%
        tidyr::pivot_wider(names_from = trade_side, values_from = asset_names_with_draft_info_badge)

      if (nrow(data) > 0) {
        trade_details <- htmltools::tagList(
          htmltools::h5("Tradeübersicht"),
          reactable::reactable(
            data,
            columns = list(
              date = reactable::colDef(name = "Datum"),
              Geholt = reactable::colDef(
                html = TRUE
              ),
              Abgegeben = reactable::colDef(
                html = TRUE
              )
            )
          )
        )
      }
    }

    htmltools::tagList(
      trade_details,
      player_details,
      draft_grades_reactable(
        data = selected_grades,
        writers = writers,
        col_names = list(
          pick = reactable::colDef(
            show = FALSE
          ),
          asset_name_output = reactable::colDef(
            show = FALSE
          ),
          year = reactable::colDef(
            name = "Jahr",
            minWidth = 80,
            defaultSortOrder = "desc"
          )
        ),
        defaultSorted = "year"
      )
    )
  }

  draft_grades_reactable(
    data = assets_latest,
    writers = writers,
    col_names = list(
      pick = reactable::colDef(
        show = FALSE
      ),
      asset_name_output = reactable::colDef(
        name = "Asset",
        html = TRUE,
        minWidth = 250
      )
    ),
    details = row_details
  )
})

# TODO: tabelle um FPTs (wie bei draftboard mit %) avg grade in tabelle erweitern (auch mit ranks als vergleich zur bewertung)

# TODO: Charts von Draftklassen seite (Investment Returns), Draftkapital
# TODO: bei trades von spielern, die kein Pick waren, auch spieler infos zeigen
# TODO: html widgets für ELO (sparkline) und WAR (bar): https://glin.github.io/reactable/articles/examples.html#grouped-cell-rendering
