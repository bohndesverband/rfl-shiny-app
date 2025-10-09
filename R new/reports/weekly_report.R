source("R new/matchups/matchup-data.R", local = TRUE)

# report wie bei sleeper

# players ----
report_players <- shiny::reactive({
  report_players <- rfl_starter_data %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::mutate(
      player_name = nflreadr::clean_player_names(player_name),
      player_id = as.character(player_id)
    ) %>%
    dplyr::left_join(
      mfl_players %>%
        dplyr::select(player_id, headshot),
      by = "player_id"
    ) %>%
    dplyr::left_join(
      rfl_war_data %>%
        dplyr::filter(season == max(season)) %>%
        dplyr::mutate(ppg = points / games_played) %>%
        dplyr::select(player_id, ppg),
      by = "player_id"
    ) %>%
    dplyr::left_join(
      projected_points(),
      by = c("player_id" = "id")
    ) %>%
    dplyr::select(-ppg) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name),
      by = "franchise_id"
    )
})

## highest scoring players ----
high_scorers <- shiny::reactive({
  report_players() %>%
    dplyr::filter(starter_status == "starter") %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      franchises = paste(unique(franchise_name), collapse = ",\n") %>% sub(",\n([^,]*)$", " &\n\\1", .)
    ) %>%
    dplyr::group_by(pos_grouped) %>%
    dplyr::arrange(desc(player_score)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = player_score,
      title = paste(pos, "der Woche"),
      subtitle = paste(player_name, "\nhat diese Woche\ndie meisten", pos, "Punkte für die\n", franchises, "erzielt."),
      image = headshot
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## benchwarmers ----
benchwarmers <- shiny::reactive({
  report_players() %>%
    dplyr::filter(starter_status == "nonstarter") %>%
    dplyr::group_by(player_id) %>%
    dplyr::mutate(
      franchises = paste(unique(franchise_name), collapse = ",\n") %>% sub(",\n([^,]*)$", " &\n\\1", .)
    ) %>%
    dplyr::group_by(pos_grouped) %>%
    dplyr::arrange(desc(player_score)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = player_score,
      title = paste(pos, "Bankwärmer der Woche"),
      subtitle = paste(player_name, "\nhat diese Woche\ndie meisten", pos, "Punkte\nauf der Bank der\n", franchises, "erzielt."),
      image = headshot
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## biggest player elo risers ----
biggest_player_elo_risers <- shiny::reactive({
  player_elo %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::group_by(position) %>%
    dplyr::arrange(dplyr::desc(elo_shift)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      mfl_players %>%
        dplyr::select(player_id, headshot),
      by = c("mfl_id" = "player_id")
    ) %>%
    dplyr::mutate(
      value = elo_shift,
      title = paste("Größter", position, "ELO Gewinn"),
      subtitle = paste(display_name, "\nhat diese Woche\ndie meiste ELO\nunter", paste0(position, "s"), "gewonnen."),
      image = headshot
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## biggest player elo drops ----
biggest_player_elo_drops <- shiny::reactive({
  player_elo %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::group_by(position) %>%
    dplyr::arrange(elo_shift) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      mfl_players %>%
        dplyr::select(player_id, headshot),
      by = c("mfl_id" = "player_id")
    ) %>%
    dplyr::mutate(
      value = elo_shift,
      title = paste("Größter", position, "ELO Verlust"),
      subtitle = paste(display_name, "\nhat diese Woche\ndie meiste ELO\nunter", paste0(position, "s"), "verloren"),
      image = headshot
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

weekly_report_players <- shiny::reactive({
  rbind(high_scorers(), benchwarmers())
})

# manager ----
report_manager <- shiny::reactive({
  rfl_standing_data %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(
      week == max(week),
    ) %>%
    dplyr::mutate(
      weekly_eff = pf / pp,
    ) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, franchise_name, icon),
      by = "franchise_id"
    ) %>%
    dplyr::left_join(
      report_players() %>%
        dplyr::filter(starter_status == "starter") %>%
        dplyr::group_by(franchise_id) %>%
        dplyr::summarise(
          projected_points = round(sum(as.numeric(projected), na.rm = TRUE), 2),
          .groups = "drop"
        ),
      by = "franchise_id"
    ) %>%
    dplyr::mutate(
      projected_diff = pf - projected_points,
      projected_diff_pct = ifelse(projected_points > 0, scales::percent(projected_diff / projected_points, 0.01), NA)
    )
})

## best manager ----
best_manager <- shiny::reactive({
  report_manager() %>%
    dplyr::filter(weekly_eff == max(weekly_eff)) %>%
    dplyr::mutate(
      value = scales::percent(weekly_eff),
      title = "Bester Manager",
      subtitle = paste("Das Lineup vom Team\n", franchise_name, "\nhat diese Woche satte \n", pf, "der möglichen", pp, "FPts\n erzielt."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## worst manager ----
worst_manager <- shiny::reactive({
  report_manager() %>%
    dplyr::filter(weekly_eff == min(weekly_eff)) %>%
    dplyr::mutate(
      value = scales::percent(weekly_eff),
      title = "Schlechtester Manager",
      subtitle = paste("Das Lineup vom Team\n", franchise_name, "\nhat diese Woche nur\n", pf, "der möglichen", pp, "FPts\n erzielt."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## luckiest manager ----
luckiest_manager <- shiny::reactive({
  report_manager() %>%
    dplyr::filter(wins > 0) %>%
    dplyr::filter(all_play_wins == min(all_play_wins)) %>%
    dplyr::mutate(
      wins_text = ifelse(wins == 1, "Sieg", "Siege"),
      value = paste0(all_play_wins, "-", 35 - all_play_wins),
      title = "Glücklichstes Team",
      subtitle = paste("Der Owner vom Team\n", franchise_name, "\nhätte mit seinen", pf, "FPts\ndiese Woche nur", all_play_wins, "Teams\ngeschlagen, hat aber trotzdem\n", wins, wins_text, "geholt."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## unluckiest manager ----
unluckiest_manager <- shiny::reactive({
  report_manager() %>%
    dplyr::filter(wins == 0) %>%
    dplyr::filter(all_play_wins == max(all_play_wins)) %>%
    dplyr::mutate(
      value = paste0(all_play_wins, "-", 35 - all_play_wins),
      title = "Unglücklichstes Team",
      subtitle = paste("Der Owner vom Team\n", franchise_name, "\nhätte mit seinen", pf, "FPts\ndiese Woche", all_play_wins, "Teams\ngeschlagen, hat aber keines\nseiner Matches gewonnen."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## biggest overachiever ----
biggest_overachiever <- shiny::reactive({
  report_manager() %>%
    dplyr::filter(projected_diff == max(projected_diff)) %>%
    dplyr::mutate(
      value = projected_diff_pct,
      title = "Größter Overachiever",
      subtitle = paste("Der Owner vom Team\n", franchise_name, "\nhat seine vorhergesagten\n", projected_points, "FPts um\n", projected_diff, paste0("(=", pf, ")"), "\nübertroffen."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## biggest underachiever ----
biggest_underachiever <- shiny::reactive({
  report_manager() %>%
    dplyr::filter(projected_diff == min(projected_diff)) %>%
    dplyr::mutate(
      value = projected_diff_pct,
      title = "Größter Underachiever",
      subtitle = paste("Der Owner vom Team\n", franchise_name, "\nhat seine vorhergesagten\n", projected_points, "FPts um\n", abs(projected_diff), paste0("(=", pf, ")"), "\nuntertroffen."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

# matches ----
report_matchups <- shiny::reactive({
  rfl_matchups_history %>%
    dplyr::filter(season == max(season)) %>%
    dplyr::filter(week == max(week)) %>%
    dplyr::mutate(
      score_diff = franchise_score - opponent_score,
      total_points = franchise_score + opponent_score,
      pct = ifelse(score_diff > 0, scales::percent(score_diff / opponent_score, 0.01), 0)
    ) %>%
    dplyr::left_join(
      rfl_franchise_data %>%
        dplyr::select(franchise_id, icon),
      by = "franchise_id"
    )
})

## biggest blowout ----
biggest_blowout <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(score_diff == max(score_diff)) %>%
    dplyr::mutate(
      value = paste(franchise_score, "vs", opponent_score),
      title = "Deutlichster Sieg",
      subtitle = paste("Das Team\n", franchise_name, "\nhat seinen Gegner\n", opponent_name, "\nmit einem Abstand von\n", pct, "geschlagen."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## narrowest win ----
narrowest_win <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(score_diff > 0) %>%
    dplyr::filter(score_diff == min(score_diff)) %>%
    dplyr::mutate(
      value = paste(franchise_score, "vs", opponent_score),
      title = "Knappster Sieg",
      subtitle = paste("Das Team\n", franchise_name, "\nhat seinen Gegner\n", opponent_name, "\nmit einem Abstand\nvon", pct, "geschlagen."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## biggest upset ----
biggest_upset <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(upset == 1) %>%
    dplyr::filter(elo_diff == min(elo_diff)) %>%
    dplyr::mutate(
      value = paste(elo_diff, "ELO"),
      title = "Größte Überraschung",
      subtitle = paste("Das Team\n", franchise_name, paste0("(", franchise_elo_pregame, ")"), "\nhat das Team\n", opponent_name, paste0("(", opponent_elo_pregame, ")"), "\ngeschlagen."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## highest scoring game ----
highest_scoring_game <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(total_points == max(total_points)) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      value = total_points,
      title = "Punktreichstes Spiel",
      subtitle = paste("Im Spiel zwischen\n", franchise_name, paste0("(", franchise_score, ")"), "\nund\n", opponent_name, paste0("(", opponent_score, ")"), "\nsind die meisten Punkte\nerzielt worden."),
      image = "https://www45.myfantasyleague.com/fflnetdynamic2024/63018_award_1735820644.png"
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## lowest scoring game ----
lowest_scoring_game <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(total_points == min(total_points)) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      value = total_points,
      title = "Punktärmstes Spiel",
      subtitle = paste("Im Spiel zwischen\n", franchise_name, paste0("(", franchise_score, ")"), "\nund\n", opponent_name, paste0("(", opponent_score, ")"), "\nsind die wenigsten Punkte\nerzielt worden."),
      image = "https://www45.myfantasyleague.com/fflnetdynamic2024/63018_award_1735820644.png"
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## highest combined ELO ----
top_game <- shiny::reactive({
  report_matchups() %>%
    dplyr::mutate(combined_elo = franchise_elo_pregame + opponent_elo_pregame) %>%
    dplyr::filter(combined_elo == max(combined_elo)) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      value = combined_elo,
      title = "Top Spiel",
      subtitle = paste("Das Spiel zwischen\n", franchise_name, paste0("(", franchise_elo_pregame, ")"), "\nund\n", opponent_name, paste0("(", opponent_elo_pregame, ")"), "\nhatte die höchste gesamt-ELO."),
      image = "https://www45.myfantasyleague.com/fflnetdynamic2024/63018_award_1735820644.png"
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## high scorer ----
high_scorer <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(franchise_score == max(franchise_score)) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      value = franchise_score,
      title = "Meiste Punkte",
      subtitle = paste("Das Team\n", franchise_name, "\nhat diese Woche die\nmeisten Punkte erzielt."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## low scorer ----
low_scorer <- shiny::reactive({
  report_matchups() %>%
    dplyr::filter(franchise_score == min(franchise_score)) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(
      value = franchise_score,
      title = "Wenigste Punkte",
      subtitle = paste("Das Team\n", franchise_name, "\nhat diese Woche die\nwenigsten Punkte erzielt."),
      image = icon
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## highest ELO jump ----
highest_elo_jump <- shiny::reactive({
  rfl_current_standing %>%
    dplyr::filter(elo_shift == max(elo_shift)) %>%
    dplyr::mutate(
      value = elo_shift,
      title = "Größter ELO Gewinn",
      subtitle = paste("Das Team\n", franchise_name, "\nhat diese Woche die\nmeiste ELO gewonnen."),
      image = "https://www45.myfantasyleague.com/fflnetdynamic2024/63018_award_1735820644.png"
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## biggest ELO drop ----
biggest_elo_drop <- shiny::reactive({
  rfl_current_standing %>%
    dplyr::filter(elo_shift == min(elo_shift)) %>%
    dplyr::mutate(
      value = elo_shift,
      title = "Größter ELO Verlust",
      subtitle = paste("Das Team\n", franchise_name, "\nhat diese Woche die\nmeiste ELO verloren."),
      image = "https://www45.myfantasyleague.com/fflnetdynamic2024/63018_award_1735820644.png"
    ) %>%
    dplyr::select(title, subtitle, value, image)
})

## output ----
weekly_report_teams <- shiny::reactive({
  rbind(best_manager(), worst_manager(), high_scorer(), low_scorer(), top_game(), highest_scoring_game(), lowest_scoring_game(), biggest_blowout(), narrowest_win(), biggest_upset(), luckiest_manager(), unluckiest_manager(), biggest_overachiever(), biggest_underachiever(), highest_elo_jump(), biggest_elo_drop())
})

# output ----
output$weeklyReportPlayers <- shiny::renderPlot({
  ggplot2::ggplot(transform(weekly_report_players(), title = factor(title, levels = weekly_report_players()$title)), ggplot2::aes(x = 1, y = 1)) +
    ggplot2::facet_wrap(. ~ title, ncol = 4) +
    ggplot2::geom_tile(fill = color_grey_light) +
    ggplot2::geom_text(
      ggplot2::aes(label = subtitle),
      size = 7, hjust = 0.5, lineheight = 0.9, nudge_y = 0.1, color = color_black
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      size = 12, hjust = 0.5, nudge_y = -0.35, color = color_blue
    ) +
    ggplot2::scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = paste("RFL Wochenrückblick Spieler Woche", report_manager()$week[1]),
      x = "",
      y = ""
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = color_grey_dark, color = color_bg),
      strip.text = ggplot2::element_text(size = 20, color = color_bg, face = "bold", margin = ggplot2::margin(t = 20, b = 20)),
    )
}, height = 1200)

output$weeklyReportPlayersTable <- DT::renderDataTable({
  DT::datatable(
    rbind(biggest_player_elo_risers(), biggest_player_elo_drops()) %>%
      dplyr::select(-image),
    options = list(dom = "Bfrti", pageLength = 32)
  )
})

output$weeklyReportTeams <- shiny::renderPlot({
  ggplot2::ggplot(transform(weekly_report_teams(), title = factor(title, levels = weekly_report_teams()$title)), ggplot2::aes(x = 1, y = 1)) +
    ggplot2::facet_wrap(. ~ title, ncol = 4) +
    ggplot2::geom_tile(fill = color_grey_light) +
    ggplot2::geom_text(
      ggplot2::aes(label = subtitle),
      size = 7, hjust = 0.5, lineheight = 0.9, nudge_y = 0.1, color = color_black
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = value),
      size = 12, hjust = 0.5, nudge_y = -0.35, color = color_blue
    ) +
    ggplot2::scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
    plot_defaults +
    plot_clean +
    ggplot2::labs(
      title = paste("RFL Wochenrückblick Teams Woche", report_manager()$week[1]),
      x = "",
      y = ""
    ) +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = color_grey_dark, color = color_bg),
      strip.text = ggplot2::element_text(size = 20, color = color_bg, face = "bold", margin = ggplot2::margin(t = 20, b = 20)),
    )
}, height = 1200)
