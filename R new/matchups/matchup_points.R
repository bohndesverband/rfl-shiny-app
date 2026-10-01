pbp_week <- nflreadr::load_pbp(seasons = 2026) %>%
  dplyr::filter(week == 2)

fpts_data <- pbp_week %>%
  #filter(play_id == 2941) %>%
  dplyr::mutate(
    player_id = apply(
      dplyr::pick(dplyr::ends_with("player_id")),
      1,
      \(x) paste(unique(stats::na.omit(x)), collapse = ", ")
    )
  ) %>%
  tidyr::separate_rows(player_id, sep = ", ") %>%
  dplyr::mutate(
    fpts_kick = dplyr::case_when(
      field_goal_attempt == 1 & field_goal_result == "made" ~ kick_distance * 0.1,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 25 ~ -2,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 35 ~ -1,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 50 ~ -0.5,
      field_goal_attempt == 1 & field_goal_result %in% c("missed", "blocked") & kick_distance <= 60 ~ -0.25,
      extra_point_result == "good" ~ 1,
      extra_point_result == "failed" ~ -1
    ),
    fpts_2pt = dplyr::if_else(!is.na(two_point_conv_result) & two_point_conv_result == "success", 2, 0),
    fpts_blocked = dplyr::case_when(
      field_goal_attempt == 1 & field_goal_result == "blocked" ~ 2,
      punt_blocked == 1 ~ 2,
      extra_point_result == "blocked" ~ 1
    ),
    fpts_fumble = dplyr::case_when(
      player_id == forced_fumble_player_1_player_id ~ fumble_forced * 3,
      TRUE ~ 0
    ),

    fpts = dplyr::case_when(
      player_id == passer_player_id & complete_pass == 1 ~ passing_yards * 0.04 + (touchdown * 4) + fpts_2pt + (fumble_lost * -1),
      player_id == passer_player_id & interception == 1 ~ -1,
      player_id == receiver_player_id & complete_pass == 1 ~ receiving_yards * 0.1 + (touchdown * 6) + complete_pass + fpts_2pt + (fumble_lost * -1),
      player_id == rusher_player_id ~ rushing_yards * 0.1 + (touchdown * 6) + fpts_2pt + (fumble_lost * -1),
      player_id == kicker_player_id ~ fpts_kick,
      player_id == punt_returner_player_id ~ return_yards * 0.05 + (touchdown * 6),
      player_id == kickoff_returner_player_id ~ return_yards * 0.03 + (touchdown * 6),
      player_id == blocked_player_id ~ fpts_blocked,
      solo_tackle_1_team != posteam & (player_id == solo_tackle_1_player_id | player_id == solo_tackle_2_player_id) ~ solo_tackle * 2 + fpts_fumble,
      assist_tackle_1_team != posteam & (player_id == assist_tackle_1_player_id | player_id == assist_tackle_2_player_id | player_id == assist_tackle_3_player_id | player_id == assist_tackle_4_player_id) ~ assist_tackle + fpts_fumble,
      player_id == interception_player_id ~ 4,
      player_id == pass_defense_1_player_id | player_id == pass_defense_2_player_id ~ 2,
      (player_id == fumble_recovery_1_player_id & fumbled_1_team != fumble_recovery_1_team) | (player_id == fumble_recovery_2_player_id & fumbled_2_team != fumble_recovery_2_team) ~ 3,
      player_id == sack_player_id ~ 4,
      player_id == half_sack_1_player_id | player_id == half_sack_2_player_id ~ 2,
      player_id == safety_player_id ~ 2
    ),
    start_time = lubridate::mdy_hms(start_time),
    time_sec = lubridate::minute(lubridate::ms(time)) * 60 +
      lubridate::second(lubridate::ms(time)),
    play_timestamp = start_time +
      ((qtr - 1) * 15 * 60 + (15 * 60 - time_sec)),
  ) %>%
  dplyr::select(play_id, play_timestamp, qtr, time, gsis_id = player_id, fpts, desc) %>%
  dplyr::filter(!is.na(fpts)) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(gsis_id, mfl_id),
    by = "gsis_id"
  )

roster_players <- rfl_roster_data %>%
  dplyr::filter(season == 2026 & week == 2 & franchise_id %in% c("0005", "0020") & starter_status == "starter")

roster_player_ids <- roster_players %>%
  dplyr::pull(player_id)

# TODO: fumble rules prüfen nachdem pos der spieler bekannt ist
# TODO: tackle numbers checken
# fumbles lost checken

plot_data <- roster_players %>%
  dplyr::left_join(
    fpts_data %>%
      dplyr::filter(mfl_id %in% roster_player_ids),
    by = c("player_id" = "mfl_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::arrange(play_timestamp) %>%
  dplyr::mutate(
    x = dplyr::row_number(),
  ) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::mutate(
    fpts_sum = cumsum(fpts)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(franchise_id, franchise_name, display_name, fpts, x, play_timestamp, fpts_sum, desc) %>%
  dplyr::arrange(x) %>%
  tidyr::pivot_wider(
    names_from = franchise_id,
    values_from = fpts_sum,
    names_prefix = "score_"
  ) %>%
  tidyr::fill(
    score_0005,
    score_0020,
    .direction = "down"
  ) %>%
  dplyr::mutate(
    score_0005 = tidyr::replace_na(score_0005, 0),
    score_0020 = tidyr::replace_na(score_0020, 0),
    score_diff = score_0005 - score_0020
  )

breaks <- seq(
  1,
  nrow(plot_data),
  length.out = 10
)

js <- "
function(el) {

  const svg = el.querySelector('svg');

  const lines = svg.querySelectorAll(
    'line[data-id^=\"hover-line-\"]'
  );

  const points = svg.querySelectorAll(
    '[data-id^=\"point-\"]'
  );

  points.forEach(function(point) {

    const id = point.getAttribute('data-id');
    const x = id.replace('point-', '');

    const line = svg.querySelector(
      'line[data-id=\"hover-line-' + x + '\"]'
    );

    if (!line) return;

    point.addEventListener('mouseenter', function() {
      line.style.opacity = 1;
    });

    point.addEventListener('mouseleave', function() {
      line.style.opacity = 0;
    });

  });

}
"

plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = score_diff)) +
  ggplot2::geom_area(ggplot2::aes(y = ifelse(score_diff < 0, score_diff, 0)), fill = color_orange) +
  ggplot2::geom_area(ggplot2::aes(y = ifelse(score_diff > 0, score_diff, 0)), fill = color_blue) +
  #ggiraph::geom_point_interactive(
  #  ggplot2::aes(tooltip = paste0(": ", " FPts\n", desc)), fill = "green"
  #) +
  ggplot2::geom_hline(yintercept = 0, color = color_grey_light, size = 0.5, linetype = "dashed") +
  ggplot2::geom_line(size = 1, color = color_grey_light) +

  ggiraph::geom_vline_interactive(
    ggplot2::aes(
      xintercept = x,
      data_id = paste0("hover-line-", x),
      tooltip = paste0(paste(display_name, fpts, "FPts für", franchise_name), "\n", desc)
    ),
    alpha = 0
  ) +
  ggplot2::scale_x_continuous(
    breaks = breaks,
    labels = format(plot_data$play_timestamp[breaks], "%d.%m. %H:%M"),
  ) +
  ggplot2::scale_y_continuous(limits = c(min(plot_data$score_diff), max(plot_data$score_diff)), breaks = seq(round(min(plot_data$score_diff), 0), round(max(plot_data$score_diff), 0), by = 25)) +
  plot_defaults +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = color_grey_dark),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_line(color = color_grey_light),
    axis.text = ggplot2::element_text(color = color_grey_light)
  ) +
  ggplot2::labs(
    x = "",
    y = "Punktedifferenz"
  )

girafe_default_output(plot)



