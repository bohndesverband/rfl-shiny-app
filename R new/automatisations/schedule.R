library(tidyverse)
library(lme4)

var_season <- 2026

# matchups ----
rfl_matchups_history <- rfl_team_elo %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    pf_avg = mean(franchise_score, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  mutate(
    luck = round((2 * pf_avg) / opponent_score, 2),
    quality = round(franchise_score / pf_avg, 2),
    total_points = franchise_score + opponent_score,
    total_elo = franchise_elo_pregame + opponent_elo_pregame,
    elo_diff = franchise_elo_pregame - opponent_elo_pregame,
    upset = ifelse(elo_diff < 0 & score_diff > 0, 1, 0),
    result = ifelse(score_diff > 0, " W über ", " L gegen "),
    win = ifelse(score_diff > 0, 1, 0),
    label = paste0(franchise_name, " (", franchise_elo_pregame, ")", result, opponent_name, " (", opponent_elo_pregame, ") - WK", week, " ", season),
    score_diff = round(franchise_score - opponent_score, 2),
    score_diff_pct = round(score_diff / opponent_score, 4) * 100
  ) %>%
  dplyr::group_by(season, week, franchise_id) %>%
  dplyr::mutate(
    total_elo_shift = sum(elo_shift)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(season, week, franchise_id, opponent_id, franchise_name, win, elo_shift, total_elo_shift, franchise_elo_pregame, franchise_elo_postgame, franchise_score, elo_diff, opponent_score, opponent_elo_pregame, opponent_name, luck, quality, total_points, total_elo, score_diff, score_diff_pct, upset, label, division, conference_id) %>%
  dplyr::left_join(
    rfl_starter_data %>%
      dplyr::filter(starter_status == "starter") %>%
      dplyr::group_by(season, week, franchise_id) %>%
      dplyr::summarise(
        franchise_score = sum(player_score, na.rm = TRUE),
        franchise_ppg_score = sum(ppg, na.rm = TRUE),
        franchise_points_ppg_diff = round(sum(ppg_diff, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      dplyr::select(season, week, franchise_id, franchise_ppg_score, franchise_points_ppg_diff),
    by = c("season", "week", "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_roster_data %>%
      dplyr::filter(starter_status == "starter") %>%
      dplyr::group_by(season, week, franchise_id) %>%
      dplyr::summarise(war_franchise_starter = round(sum(war), 2), .groups = "drop"),
    by = c("season", "week", "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_starter_data %>%
      dplyr::filter(starter_status == "starter") %>%
      dplyr::group_by(season, week, franchise_id) %>%
      dplyr::summarise(
        franchise_score = sum(player_score, na.rm = TRUE),
        franchise_ppg_score = sum(ppg, na.rm = TRUE),
        franchise_points_ppg_diff = round(sum(ppg_diff, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      dplyr::select(season, week, franchise_id, opponent_ppg_score = franchise_ppg_score, opponent_points_ppg_diff = franchise_points_ppg_diff),
    by = c("season", "week", "opponent_id" = "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_standing_data %>%
      dplyr::select(season, week, franchise_id, pp, all_play_wins),
    by = c("season", "week", "franchise_id")
  ) %>%

  # pctl berechnung
  dplyr::mutate(
    eff = round(franchise_score / pp, 2),
    dplyr::across(
      c(franchise_score, pp, luck, quality, eff, elo_shift, all_play_wins, franchise_elo_pregame, elo_shift, franchise_ppg_score, franchise_points_ppg_diff, total_points, total_elo, score_diff, score_diff_pct),
      ~ round(dplyr::percent_rank(.x), 2),
      .names = "{.col}_pctl"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(season), dplyr::desc(week))

DBI::dbWriteTable(con, "rfl_matchups_history", rfl_matchups_history, overwrite = TRUE)

# schedule ----
rfl_schedule_data <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/rfl-schedules.csv", col_types = "dicc") %>%
  # add elo data
  dplyr::left_join(
    rfl_matchups_history %>%
      dplyr::select(season, week, franchise_id, opponent_id, dplyr::ends_with("elo_pregame"), dplyr::ends_with("elo_postgame"), win, dplyr::ends_with("_score"), division, conference_id),
    by = c("season", "week", "franchise_id", "opponent_id")
  ) %>%
  dplyr::left_join(
    rfl_matchups_history %>%
      dplyr::select(season, week, franchise_id = opponent_id, opponent_id = franchise_id, opponent_elo_postgame = franchise_elo_postgame) %>%
      dplyr::distinct(),
    by = c("season", "week", "franchise_id", "opponent_id")
  ) %>%

  # daten für neue saison wenn kein schedule vorhanden ist
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    franchise_elo_pregame = ifelse(is.na(franchise_elo_pregame), dplyr::lag(franchise_elo_postgame), franchise_elo_pregame),
  ) %>%
  tidyr::fill(
    franchise_elo_pregame,
    .direction = "down"
  ) %>%
  dplyr::group_by(opponent_id) %>%
  dplyr::arrange(season, week) %>%
  dplyr::mutate(
    opponent_elo_pregame = ifelse(is.na(opponent_elo_pregame), dplyr::lag(opponent_elo_postgame), opponent_elo_pregame)
  ) %>%
  tidyr::fill(
    opponent_elo_pregame,
    .direction = "down"
  ) %>%
  dplyr::group_by(season, week) %>%
  dplyr::mutate(
    avg_elo = round(mean(franchise_elo_pregame), 0),
    franchise_elo_diff = franchise_elo_pregame - avg_elo,
    opponent_elo_diff = opponent_elo_pregame - avg_elo
  ) %>%
  dplyr::select(-dplyr::ends_with("postgame"), -avg_elo) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name, abbrev, division_new = division, conference_id_new = conference_id),
    by = c("franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, opponent_name = franchise_name, opponent_abbrev = abbrev, opponent_div = division, opponent_conf = conference_id),
    by = c("opponent_id" = "franchise_id")
  ) %>%

  # war
  dplyr::left_join(
    rfl_roster_war %>%
      dplyr::rename(war_franchise = war) %>%
      dplyr::select(-prediction),
    by = c("season", "week", "franchise_id")
  ) %>%
  ## opponent
  dplyr::left_join(
    rfl_roster_war %>%
      dplyr::rename(war_opponent = war) %>%
      dplyr::select(-war_lost_bye),
    by = c("season", "week", "opponent_id" = "franchise_id")
  ) %>%

  dplyr::mutate(
    dplyr::across(c(war_franchise, war_opponent, prediction), ~ ifelse(is.na(.x), 0, .x)),
    division = ifelse(is.na(division), division_new, division),
    conference_id = ifelse(is.na(conference_id), conference_id_new, conference_id),
    matchup = dplyr::case_when(
      division == opponent_div ~ "Div",
      conference_id == opponent_conf ~ "Conf",
      TRUE ~ "Zufällig"
    )
  ) %>%
  dplyr::select(-opponent_div, -division, -opponent_conf, -conference_id, -division_new, -conference_id_new) %>%

  # schedule delta
  dplyr::mutate(
    type = ifelse(season == season_before_wk_2 & week > current_week_thu, "kommend", "bisher")
  ) %>%
  dplyr::mutate(
    #p_win = 1 / (1 + 10^((opponent_elo_pregame - franchise_elo_pregame) / 400)),
    #difficulty_raw = 1 - p_win,
    #win_difficulty = (difficulty_raw - min(difficulty_raw, na.rm = TRUE)) / (max(difficulty_raw, na.rm = TRUE) - min(difficulty_raw, na.rm = TRUE)),
    #win_difficulty = ifelse(is.na(win_difficulty), 0.5, round(win_difficulty, 2)),

    p_win = 1 / (1 + 10^((war_opponent - war_franchise) / 400)),
    difficulty_raw = 1 - p_win,
    win_prob = round((p_win - min(p_win, na.rm = TRUE)) / (max(p_win, na.rm = TRUE) - min(p_win, na.rm = TRUE)), 2),
    win_difficulty = round((difficulty_raw - min(difficulty_raw, na.rm = TRUE)) / (max(difficulty_raw, na.rm = TRUE) - min(difficulty_raw, na.rm = TRUE)), 2),
  ) %>%
  dplyr::group_by(franchise_id, season, type) %>%
  dplyr::mutate(
    win_difficulty_avg = round(mean(win_difficulty, na.rm = TRUE), 2)
  ) %>%
  dplyr::group_by(franchise_id, season) %>%
  dplyr::mutate(
    win_difficulty_delta = (dplyr::last(win_difficulty_avg) - dplyr::first(win_difficulty_avg)) * 100,
  ) %>%
  dplyr::select(-p_win, -difficulty_raw)

DBI::dbWriteTable(con, "rfl_schedule_data", rfl_schedule_data, overwrite = TRUE)

# playoff probability ----
sigma_week <- sqrt(197.2889)
sigma_team <- sqrt(745.2835)

model_data <- rfl_matchups_history %>%
  dplyr::select(season, week, franchise_id, franchise_name, franchise_score, war_starter = war_franchise_starter) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    rfl_roster_war %>%
      dplyr::select(season, week, franchise_id, war_team = war_bye) %>%
      dplyr::distinct(),
    by = c("season", "week", "franchise_id")
  )

model_score <- lm(
  franchise_score ~ war_starter,
  data = model_data
)

## wöchentliche varianz
test_seasons <- 2017:2025

validation_results <- lapply(test_seasons, function(test_season) {

  train_data <- model_data %>%
    filter(season < test_season)

  test_data <- model_data %>%
    filter(season == test_season)

  # Nur vollständige Beobachtungen
  train_data <- train_data %>%
    filter(
      is.finite(franchise_score),
      is.finite(war_starter)
    )

  test_data <- test_data %>%
    filter(
      is.finite(franchise_score),
      is.finite(war_starter)
    )

  # Leere Trainings- oder Testdaten überspringen
  if (nrow(train_data) == 0 || nrow(test_data) == 0) {
    warning(paste("Keine Daten für Saison", test_season))
    return(NULL)
  }

  model <- lm(
    franchise_score ~ war_starter,
    data = train_data
  )

  predictions <- predict(model, newdata = test_data)

  test_data %>%
    transmute(
      season,
      week,
      franchise_id,
      actual = franchise_score,
      predicted = predictions,
      residual = actual - predicted
    )

}) %>%
  bind_rows() %>%
  mutate(
    season_week = interaction(season, week, drop = TRUE)
  )

bias_correction <- mean(validation_results$residual)

### liga
model_random_effect <- lmer(
  residual ~ 1 + (1 | season_week),
  data = validation_results,
  REML = TRUE
)

weekly_effects <- ranef(model_random_effect)$season_week %>%
  tibble::rownames_to_column("season_week") %>%
  rename(weekly_error = `(Intercept)`)

weekly_errors_boot <- weekly_effects$weekly_error
weekly_errors_boot <- weekly_errors_boot -
  mean(weekly_errors_boot)

weekly_errors_boot <- weekly_errors_boot *
  (sigma_week / sd(weekly_errors_boot))

### team
team_effects <- validation_results %>%
  mutate(
    weekly_error = ranef(model_random_effect)$season_week[
      as.character(season_week),
      "(Intercept)"
    ],
    team_error = residual -
      fixef(model_random_effect)[["(Intercept)"]] -
      weekly_error
  )

team_errors_boot <- team_effects$team_error
team_errors_boot <- team_errors_boot -
  mean(team_errors_boot)

team_errors_boot <- team_errors_boot *
  (sigma_team / sd(team_errors_boot))

future_schedule <- rfl_schedule_data %>%
  dplyr::filter(season == new_season_sept & week >= current_week) %>%
  dplyr::select(season, week, franchise_id, franchise_name, opponent_id, opponent_name, war_franchise, war_opponent, win_prob) %>%
  rename(war_starter = war_franchise)

predicted_scores <- future_schedule %>%
  mutate(
    predicted_score = predict(
      model_score,
      newdata = tibble(war_starter = war_starter)
    )
  )

simulate_season <- function(
    predicted_scores,
    matchups
) {

  # 1. Scores für alle Teams und Wochen simulieren
  simulated_scores <- predicted_scores %>%
    dplyr::select(season, week, franchise_id, predicted_score) %>%
    dplyr::distinct() %>%
    group_by(season, week) %>%
    mutate(
      weekly_error = sample(
        weekly_errors_boot,
        size = 1
      ),
      team_error = sample(
        team_errors_boot,
        size = n(),
        replace = TRUE
      ),
      simulated_score = predicted_score +
        bias_correction +
        weekly_error +
        team_error
    ) %>%
    ungroup() %>%
    select(
      season,
      week,
      franchise_id,
      simulated_score
    )

  # 2. Scores den Matchups zuordnen
  matchup_results <- future_schedule %>%
    left_join(
      simulated_scores,
      by = c("season", "week", "franchise_id")
    ) %>%
    rename(score = simulated_score) %>%
    left_join(
      simulated_scores %>%
        rename(
          opponent_id = franchise_id,
          opponent_score = simulated_score
        ),
      by = c("season", "week", "opponent_id")
    ) %>%
    mutate(
      result = case_when(
        score > opponent_score ~ 1,
        score < opponent_score ~ 0,
        TRUE ~ 0.5
      )
    )

  # 3. Ergebnisse je Team aggregieren
  team_results <- matchup_results %>%
    group_by(franchise_id) %>%
    summarise(
      wins = sum(result),
      losses = sum(result == 0),
      ties = sum(result == 0.5),
      score = sum(score) / 2,
      .groups = "drop"
    )

  team_results
}

n_simulations <- 10000

simulation_results <- map_dfr(
  seq_len(n_simulations),
  function(sim_id) {

    simulate_season(
      predicted_scores = predicted_scores,
      matchups = matchups
    ) %>%
      mutate(simulation = sim_id)
  }
)

rfl_schedule_sim_summary <- simulation_results %>%
  dplyr::left_join(
    rfl_standing_data %>%
      dplyr::filter(season == max(season)) %>%
      dplyr::filter(week == max(week)) %>%
      dplyr::select(franchise_id, franchise_name, conf_id, div_id, wins_current = wins_season, score_current = pf_season),
    by = "franchise_id"
  ) %>%
  dplyr::mutate(
    wins_sim = wins + wins_current,
    pf_sim = round(score + score_current, 2)
  ) %>%

  # create divison ranking
  dplyr::group_by(simulation, div_id) %>%
  dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
  dplyr::mutate(div_rank = dplyr::row_number()) %>%

  # create conf ranking
  dplyr::group_by(simulation, conf_id) %>%
  dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
  dplyr::mutate(conf_rank = dplyr::row_number()) %>%

  # add seeding
  dplyr::group_by(simulation, conf_id, div_rank) %>%
  dplyr::arrange(desc(wins_sim), desc(pf_sim)) %>%
  dplyr::mutate(
    seed = ifelse(div_rank == 1, dplyr::row_number(), NA),
  ) %>%
  dplyr::group_by(simulation, conf_id) %>%
  dplyr::arrange(seed, desc(wins_sim), desc(pf_sim)) %>%
  dplyr::mutate(
    seed = ifelse(is.na(seed), row_number(), seed),
    bowl = dplyr::case_when(
      seed <= 6 ~ 1, # 1 == SB
      seed <= 12 ~ 2, # 2 == PB
      TRUE ~ 3 # 3 == TB
    ),
  ) %>%
  dplyr::group_by(simulation, bowl) %>%
  dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
  dplyr::mutate(
    seed = ifelse(bowl > 1, dplyr::row_number(), seed)
  ) %>%

  # create league ranking
  dplyr::group_by(simulation) %>%
  dplyr::mutate(
    po_helper = ifelse(bowl == 1, 1, 0)
  ) %>%
  dplyr::arrange(bowl, seed) %>%
  dplyr::mutate(
    league_rank = dplyr::row_number(),
    sb = bowl == 1,
    pb = bowl <= 2,
    sb_bye = bowl == 1 & conf_rank <= 2,
    pb_bye = (bowl == 2 & seed <= 4) | bowl == 1
  ) %>%
  #filter(simulation == 1)
  group_by(franchise_id) %>%
  summarise(
    #dplyr::across(c(franchise_name), ~ first(.x)),
    #dplyr::across(c(wins_sim, pf_sim), ~ mean(.x)),
    dplyr::across(c(sb, sb_bye, pb, pb_bye), ~ round(mean(.x), 2), .names = "{.col}_prob"),
    #dplyr::across(c(sb, sb_bye, pb, pb_bye), ~ sum(.x)),
    .groups = "drop"
  ) %>%
  arrange(desc(sb_prob), desc(sb_bye_prob), desc(pb_prob), desc(pb_bye_prob))

rm(sigma_week, sigma_team, model_data, test_seasons, validation_results, bias_correction, model_random_effect, weekly_effects, weekly_errors_boot, team_effects, team_errors_boot, future_schedule, predicted_scores, simulate_season, n_simulations, simulation_results)

DBI::dbWriteTable(con, "rfl_po_prob", rfl_schedule_sim_summary, overwrite = TRUE)

# preseason
sos <- purrr::map_df(2024:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/schedule/rfl-sos-{x}.csv"),
    col_types = "cdddddcc"
  ) %>%
    dplyr::mutate(season = x)
}) %>%
  dplyr::mutate(
    above_avg = round(Total - mean(Total), 3),
    SOS = round(SOS, 3),
  ) %>%
  dplyr::left_join(rfl_franchise_data %>% dplyr::rename(div_id = division) %>% select(franchise_id, franchise_name, div_id), by = "franchise_id")

DBI::dbWriteTable(con, "sos", sos, overwrite = TRUE)

# inseason
#schedule <- rfl_schedule_data %>%
  #filter(season == 2026) %>%
  # add standing
#  dplyr::left_join(
#    rfl_standing_data %>%
#      dplyr::group_by(season) %>%
#      dplyr::filter(week == max(week)) %>%
#      dplyr::select(season, franchise_id, all_play_wins_total),
#    by = c("opponent_id" = "franchise_id", "season")
#  ) %>%
#  dplyr::group_by(season) %>%
#  dplyr::mutate(
#    type = dplyr::case_when(
#      season >= var_season & week >= current_week ~ "sos_upcoming",
#      TRUE ~ "sos_previous"
#    ),
#    max_week = ifelse(season == var_season, current_week - 1, max(week)),
#    all_play_win_pct = all_play_wins_total / (max_week * 35)
#  ) %>%
#  dplyr::group_by(season, franchise_id) %>%
#  dplyr::mutate(
#    sos_total = mean(all_play_win_pct, na.rm = TRUE)
#  ) %>%
#  dplyr::group_by(season, franchise_id, type) %>%
#  dplyr::summarise(
#    sos_total = dplyr::first(sos_total),
#    sos = mean(all_play_win_pct, na.rm = TRUE),
#    .groups = "drop"
#  ) %>%
#  dplyr::mutate(
#    sos_total = ifelse(!is.na(sos_total), sos_total, NA),
#    sos = ifelse(!is.na(sos), sos, NA),
#  ) %>%
#  tidyr::pivot_wider(names_from = type, values_from = c(sos))

#if (current_week <= 13) {
#  sos_inseason <- schedule %>%
#    dplyr::left_join(
#      sos %>%
#        dplyr::select(season, franchise_id, sos_preseason = Total, sos_score_preseason = SOS),
#      by = c("franchise_id", "season")
#    ) %>%
#    #filter(season == 2026) %>%
#    dplyr::mutate(
#      sos_change = ifelse(!is.na(sos_total), sos_total - sos_preseason, NA),
#      sos_diff = ifelse(!is.na(sos_previous), sos_previous - sos_upcoming, NA),
#      sos_score_total = ifelse(!is.na(sos_total), sos_total / max(sos_total, na.rm = TRUE), sos_preseason / max(sos_preseason, na.rm = TRUE)),
#      sos_score_previous = ifelse(!is.na(sos_previous), sos_previous / max(sos_previous, na.rm = TRUE), NA),
#      sos_score_upcoming = ifelse(!is.na(sos_upcoming), sos_upcoming / max(sos_upcoming, na.rm = TRUE), NA)
#    ) %>%
#    dplyr::left_join(
#      rfl_franchise_data %>%
#        dplyr::select(franchise_id, franchise_name, division),
#      by = "franchise_id"
#    )

#  DBI::dbWriteTable(con, "sos_inseason", sos_inseason, overwrite = TRUE)
#}
