matchups <- jsonlite::read_json(paste0(var.mflApiBase, "/export?TYPE=schedule&L=", var.mflLeagueID, "&W=", input$matchupProjectionsSelectWeek, "&JSON=1"))$schedule$weeklySchedule$matchup %>% 
  dplyr::tibble() %>% 
  tidyr::unnest(1) %>% 
  tidyr::unnest_wider(1, names_sep = "_") %>% 
  tidyr::unnest_wider("._1") %>% 
  dplyr::rename(home = id) %>% 
  tidyr::unnest_wider("._2", names_sep = "_") %>% 
  dplyr::rename(away = "._2_id") %>% 
  dplyr::select(home, away) %>% 
  dplyr::left_join(
    franchises %>% 
      dplyr::select(franchise_id, franchise_name) %>% 
      dplyr::rename(home_name = franchise_name),
    by = c("home" = "franchise_id")
  ) %>% 
  dplyr::left_join(
    franchises %>% 
      dplyr::select(franchise_id, franchise_name) %>% 
      dplyr::rename(away_name = franchise_name),
    by = c("away" = "franchise_id")
  ) %>% 
  dplyr::mutate(matchup = paste(home_name, "vs", away_name))

starter_data <- jsonlite::read_json(paste0(var.mflApiBase, "/export?TYPE=weeklyResults&L=", var.mflLeagueID, "&W=", nflreadr::get_current_week(), "&JSON=1"))$weeklyResults$matchup %>% 
  dplyr::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest_wider(franchise, names_sep = "_") %>% 
  dplyr::select(-regularSeason) %>% 
  tidyr::unnest_wider(franchise_1, names_sep = "_") %>% 
  tidyr::unnest_wider(franchise_2, names_sep = "_") %>% 
  dplyr::select(dplyr::ends_with("id"), dplyr::ends_with("_starters"))

projected_points <- jsonlite::read_json(paste0(var.mflApiBase, "/export?TYPE=projectedScores&L=", var.mflLeagueID, "&W=", nflreadr::get_current_week(), "&JSON=1"))$projectedScores$playerScore %>%
  dplyr::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  dplyr::rename(projected = score)

live_data <- jsonlite::read_json(paste0(var.mflApiBase, "/export?TYPE=liveScoring&L=", var.mflLeagueID, "&W=", nflreadr::get_current_week(), "&JSON=1"))$liveScoring$matchup %>% 
  dplyr::tibble() %>% 
  tidyr::unnest_wider(1) %>% 
  tidyr::unnest_wider(franchise, names_sep = "_") %>% 
  tidyr::unnest_wider(franchise_1, names_sep = "_") %>% 
  tidyr::unnest_wider(franchise_2, names_sep = "_") %>% 
  dplyr::select(-dplyr::ends_with("isHome"), -dplyr::ends_with("score"))

live_points_data <- live_data %>% 
  dplyr::select(dplyr::ends_with("players")) %>% 
  tidyr::unnest_wider(c(franchise_1_players, franchise_2_players), names_sep = "_") %>% 
  tidyr::unnest(c(1, 2)) %>% 
  tidyr::unnest_wider(c(1, 2), names_sep = "_")

starter <- data.frame(
  franchise_id = c(starter_data$franchise_1_id, starter_data$franchise_2_id),
  id = c(starter_data$franchise_1_starters, starter_data$franchise_2_starters)
) %>% 
  tidyr::separate_rows(id, sep = ",") %>% 
  dplyr::filter(id != "") %>% 
  dplyr::left_join(
    projected_points,
    by = "id"
  ) %>% 
  dplyr::left_join(
    data.frame(
      player_id = c(live_points_data$franchise_1_players_player_id, live_points_data$franchise_2_players_player_id),
      live = c(live_points_data$franchise_1_players_player_score, live_points_data$franchise_2_players_player_score),
      time_remaining = c(live_points_data$franchise_1_players_player_gameSecondsRemaining, live_points_data$franchise_2_players_player_gameSecondsRemaining)
    ) %>% 
      dplyr::distinct(),
    by = c("id"  = "player_id")
  )

# ToDo: remaining seconds etc hinzufügen

matchup_projection_table_data <- data.frame(
  franchise_id = c(matchups$home, matchups$away),
  franchise_name = c(matchups$home_name, matchups$away_name),
  matchup = matchups$matchup
) %>% 
  
  dplyr::left_join(
    starter,
    by = "franchise_id"
  ) %>% 
  dplyr::left_join(
    players %>% 
      dplyr::select(-status),
    by = c("id" = "player_id")
  ) %>% 
  dplyr::mutate(
    live = as.numeric(live),
    projected = ifelse(is.na(projected), 0, as.numeric(projected)),
    time_remaining = as.numeric(time_remaining),
    pts = ifelse(time_remaining < 3600, live, projected),
    diff = pts - projected,
    subline = paste(pos, team, sep = ", "),
    #pts_per_second = live / (3600 - time_remaining),
    #calc = live + (pts_per_second * time_remaining * 0.75)
  ) %>% 
  dplyr::mutate(
    live_1 = live,
    projected_1 = projected
  ) %>%
  dplyr::select(franchise_name, player_name, subline, projected, time_remaining, live, live_1, projected_1, diff)

gtMatchupProjections <- function(df) {
  df %>% 
    gt::gt() %>% 
    
    gt::cols_hide(franchise_name) %>% 
    
    gtExtras::gt_merge_stack(
      player_name,
      subline,
      small_cap = FALSE,
      palette = c("#222f3e", "#8395a7"),
      font_weight = c("normal", "normal")
    ) %>%
    
    gtExtras::gt_merge_stack(
      projected,
      time_remaining,
      small_cap = FALSE,
      palette = c("#222f3e", "#8395a7"),
      font_weight = c("normal", "normal")
    ) %>%
    
    gtExtras::gt_plt_bullet(
      column = live_1,
      target = projected_1,
      width = 30,
      palette = c("#10ac84", "#222f3e")
    ) %>% 
    
    gt::grand_summary_rows(
      columns = c(projected, diff),
      fns = list(
        sum ~ sum(.)
      ),
    ) %>% 
    
    gtExtras::gt_merge_stack(
      live_1,
      live,
      small_cap = FALSE,
      palette = c("#222f3e", "#8395a7"),
      font_weight = c("normal", "normal")
    ) %>% 
    
    gt::data_color(
      columns = diff,
      method = "numeric",
      palette = "viridis",
    ) %>% 
    
    gt::cols_label(
      player_name = "Player",
      projected = "Proj",
      live_1 = "Points",
      diff = "+/-"
    ) %>% 
    
    gtDefaults()
}
