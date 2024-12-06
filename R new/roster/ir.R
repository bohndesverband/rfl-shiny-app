# load neccesary data ----
source("R new/players/fpts.R", local = TRUE)

# create base data ----
ir_players <- roster_data %>%
  dplyr::select(player_id) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = c("player_id" = "mfl_id")
  ) %>%

  # add nfl IR status
  dplyr::left_join(
    nflreadr::load_rosters_weekly() %>%
      dplyr::filter(status == "RES" & week <= 13) %>%
      dplyr::group_by(gsis_id) %>%
      dplyr::arrange(week) %>%
      dplyr::summarise(
        games_on_ir = n(),
        first_week_on_ir = first(week),
        last_week_on_ir = last(week),
        .groups = "drop"
      ),
    by = "gsis_id"
  ) %>%
  dplyr::filter(!is.na(games_on_ir) & first_week_on_ir <= 13) %>%
  dplyr::select(-dplyr::ends_with("week_on_ir"))

# create data for weekly IR analysis ----
team_ir_weekly <- roster_data %>%
  dplyr::left_join(
    ir_players,
    by = "player_id"
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    nflreadr::load_rosters_weekly() %>%
      dplyr::filter(status == "RES") %>%
      dplyr::select(gsis_id, week, status),
    by = c("gsis_id", "week")
  ) %>%
  dplyr::mutate(
    status = ifelse(is.na(status), "ACTIVE", status)
  ) %>%

  # add elo
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::summarise(
        player_name = last(display_name),
        player_elo_post = last(player_elo_post),
        .groups = "drop"
      ),
    by = c("player_id" = "mfl_id")
  ) %>%

  dplyr::mutate(player_elo_post = ifelse(is.na(player_elo_post), 1400, player_elo_post)) %>%
  dplyr::group_by(franchise_id, week) %>%
  dplyr::arrange(dplyr::desc(player_elo_post)) %>%
  dplyr::ungroup()

team_ir <- team_ir_weekly %>%
  dplyr::filter(status == "RES") %>%
  dplyr::group_by(franchise_id, week) %>%
  dplyr::summarise(
    player_count = n(),
    franchise_name = first(franchise_name),
    elo_high = max(player_elo_post),
    elo_low = min(player_elo_post),
    elo_avg = round(mean(player_elo_post), 0),
    elo_sum = sum(player_elo_post),
    elo_median = median(player_elo_post),
    .groups = "drop"
  )

## plot data ----
output$ir <- shiny::renderPlot({
  ggplot2::ggplot(team_ir, ggplot2::aes(x = week, y = elo_median, color = franchise_name)) +
    ggplot2::geom_boxplot(aes(group = week), fill = color_grey_light, color = color_grey_mid, linewidth = 0.15, outliers = FALSE) +
    ggplot2::geom_jitter(ggplot2::aes(size = player_count, alpha = player_count), width = 0.25, color = color_grey_mid) +

    ggalt::geom_xspline(data = subset(team_ir, franchise_id %in% c(input$selectRflTeams)), spline_shape = -0.5) +
    ggplot2::aes(lwd = 1.2) +
    ggplot2::scale_linewidth_identity() +

    ggplot2::geom_point(data = subset(team_ir, franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(size = player_count)) +
    ggplot2::scale_color_discrete(type = colors) +
    ggplot2::scale_size_continuous(range = c(1,8)) +
    ggplot2::scale_alpha(guide = "none") +

    plot_defaults +
    plot_clean +
    ggplot2::scale_x_continuous(limits = c(0.4, max(team_ir$week) + 0.4), labels = c(1:max(team_ir$week)), breaks = c(1:max(team_ir$week))) +
    ggplot2::labs(
      title = "Median ELO aller Spieler auf der NFL IR",
      x = "Woche",
      y = "IR Median ELO",
      color = "RFL Teams",
      size = "Anzahl Spieler auf IR"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::theme(
      legend.position.inside = c(0.09, 0.8)
    )
}, height = 800)

# create data for IR players by team ----
team_ir_players <- team_ir_weekly %>%
  dplyr::filter(status == "RES") %>%
  dplyr::select(franchise_id, franchise_name, player_id, player_name, games_on_ir, player_elo_post) %>%
  dplyr::distinct()

all_ir_players <- team_ir_players %>%
  dplyr::select(-franchise_id, -franchise_name) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(player_name))

## plot data ----
output$ir_player <- shiny::renderPlot({
  ggplot2::ggplot(all_ir_players, ggplot2::aes(x = games_on_ir, y = reorder(player_name, player_elo_post))) +
    ggplot2::geom_col(fill = color_grey_light) +
    ggplot2::geom_col(data = subset(team_ir_players, franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(fill = franchise_name)) +

    ggplot2::scale_fill_discrete(type = colors) +
    ggplot2::scale_x_continuous(limits = c(0, max(team_ir_players$games_on_ir)), labels = c(0:max(team_ir_players$games_on_ir)), breaks = c(0:max(team_ir_players$games_on_ir))) +
    ggplot2::labs(
      title = "Ausgefallene Spieler",
      subtitle = "Angezeigt werden alle Spieler, die mind. 1 Spiel verpasst haben, weil sie in der NFL auf IR waren.\nSortiert absteigend nach ihrer aktuellen ELO.",
      x = "Missed Games",
      y = "",
      fill = ""
    ) +
    plot_defaults +
    plot_clean
}, height = 2500)

# create data for lost FPTS ----
team_ir_ppg <- team_ir_players %>%
  dplyr::left_join(
    player_ppg %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::filter(games >= 3) %>%  # min 3 games played
      dplyr::filter(season == max(season)) %>%
      dplyr::select(mfl_id, ppg),
    by = c("player_id" = "mfl_id")
  ) %>%
  dplyr::mutate(
    ppg = ifelse(is.na(ppg), 0, ppg),
    missed_fpts = games_on_ir * ppg
  ) %>%
  dplyr::group_by(franchise_id, franchise_name) %>%
  dplyr::summarise(
    missed_fpts = round(sum(missed_fpts, na.rm = TRUE), 2),
    missed_ppg = round(sum(missed_fpts, na.rm = TRUE) / sum(games_on_ir, na.rm = TRUE), 2),
    .groups = "drop"
  )

## plot data ----
output$ir_fpts <- shiny::renderPlot({
  ggplot2::ggplot(team_ir_ppg, ggplot2::aes(x = missed_fpts, y = reorder(franchise_name, missed_fpts))) +
    ggplot2::geom_col(fill = color_grey_light) +
    ggplot2::geom_col(data = subset(team_ir_ppg, franchise_id %in% c(input$selectRflTeams)), ggplot2::aes(fill = franchise_name)) +
    ggplot2::scale_fill_discrete(type = colors, guide = "none") +
    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = "Durch IR verlorene Fantasy Punkte",
      subtitle = "Für die Berechnung werden die durchschnittlichen Fantasy Punkte pro Spiel (FPts/G) aus der\naktuellsten Saison mit mind. 3 Spielen genommen. Diese werden mit den dieses Jahr verpassten\nSpielen multipliziert.",
      x = "FPts/G",
      y = "",
      fill = ""
    )
}, height = 1000)

# daten für team report ----

#ggplot2::ggplot(data = subset(team_ir_weekly, franchise_id %in% c("0007")), ggplot2::aes(values = 1, fill = status)) +
#  waffle::geom_waffle(color = "white", linewidth = 1.125, n_rows = 10) +
#  facet_wrap(~week, ncol = 3) +
#  plot_defaults +
#  plot_clean
