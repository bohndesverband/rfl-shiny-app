# ============================================================================
# RFL: Vollständige Playoff-/Seeding-Simulation und Counterfactual-Key-Matchups
# ============================================================================
# Dieses Skript baut Score-Modell, Out-of-Sample-Fehlerverteilungen,
# Monte-Carlo-Simulationen, Seeding und Counterfactuals selbst auf.
#
# Vor dem Start müssen die RFL-Projektdaten und Saisonvariablen geladen sein:
#   - rfl_matchups_history
#   - rfl_roster_war
#   - rfl_schedule_data
#   - rfl_standing_data
#   - new_season_sept
#   - current_week
#
# Diese Objekte stammen aus deinem RFL-Projekt. Das Skript setzt sie als
# Datenquellen voraus, erzeugt aber alle Modell- und Simulationsobjekte selbst.
# ============================================================================

# ---------------------------------------------------------------------------
# 0. Pakete, Einstellungen und Hilfsfunktionen
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(piggyback)
})

required_packages <- c("lme4", "future", "furrr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Bitte installiere zunächst: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# ab welcher woche soll simuliert werden? default current_weel
sim_week <- 3

# Anzahl der Monte-Carlo-Simulationen.
n_simulations <- 10000L

# Anzahl paralleler Worker für die Counterfactual-Auswertung.
workers <- 2L

# Reproduzierbarkeit.
random_seed <- 20261108L

# NULL = alle verbleibenden Wochen simulieren.
# Zum gezielten Testen kann hier z.B. 9L eingetragen werden.
weeks_to_simulate <- NULL

# Streuungen aus deinem ursprünglichen Modellskript.
sigma_week <- sqrt(197.2889)
sigma_team <- sqrt(745.2835)

# IDs einheitlich als Character mit mindestens vier Stellen behandeln.
normalize_franchise_id <- function(x) {
  x <- trimws(as.character(x))
  is_numeric_id <- !is.na(x) & grepl("^[0-9]+$", x)
  id_length <- nchar(x[is_numeric_id])
  missing_zeroes <- pmax(0L, 4L - id_length)
  x[is_numeric_id] <- paste0(
    vapply(
      missing_zeroes,
      function(n) paste0(rep("0", n), collapse = ""),
      character(1)
    ),
    x[is_numeric_id]
  )
  x
}

# Kleine Hilfsfunktion für verständliche Prüfungen.
assert_no_rows <- function(x, message) {
  if (nrow(x) > 0L) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

# Prüfen, ob die benötigten RFL-Datenobjekte geladen sind.
required_objects <- c(
  "rfl_matchups_history",
  "rfl_roster_war",
  "rfl_schedule_data",
  "rfl_standing_data",
  "new_season_sept",
  "current_week"
)
missing_objects <- required_objects[
  !vapply(
    required_objects,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = FALSE
  )
]
if (length(missing_objects) > 0L) {
  stop(
    "Diese RFL-Datenobjekte/Variablen müssen vor dem Skript geladen werden: ",
    paste(missing_objects, collapse = ", "),
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# 1. Score-Modell und Fehlerverteilungen vollständig aufbauen
# ---------------------------------------------------------------------------

# Historische Datenbasis wie in deinem ursprünglichen simulation.R.
model_data <- rfl_matchups_history %>%
  dplyr::select(
    season,
    week,
    franchise_id,
    franchise_name,
    franchise_score,
    war_starter = war_franchise_starter
  ) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    rfl_roster_war %>%
      dplyr::select(
        season,
        week,
        franchise_id,
        war_team = war_bye
      ) %>%
      dplyr::distinct(),
    by = c("season", "week", "franchise_id")
  )

# Finales Modell für die Prognose zukünftiger Fantasy-Scores.
model_score <- stats::lm(
  franchise_score ~ war_starter,
  data = model_data
)

# Out-of-sample-Validierung: jede Testsaison wird nur mit früheren Saisons
# trainiert. Das entspricht deinem ursprünglichen Modellskript.
test_seasons <- 2017:season_before_wk_2

validation_results <- lapply(test_seasons, function(test_season) {
  train_data <- model_data %>%
    dplyr::filter(season < test_season) %>%
    dplyr::filter(
      is.finite(franchise_score),
      is.finite(war_starter)
    )

  test_data <- model_data %>%
    dplyr::filter(season == test_season) %>%
    dplyr::filter(
      is.finite(franchise_score),
      is.finite(war_starter)
    )

  if (nrow(train_data) == 0L || nrow(test_data) == 0L) {
    warning("Keine vollständigen Trainings-/Testdaten für Saison ", test_season)
    return(NULL)
  }

  validation_model <- stats::lm(
    franchise_score ~ war_starter,
    data = train_data
  )

  predictions <- stats::predict(
    validation_model,
    newdata = test_data
  )

  test_data %>%
    dplyr::transmute(
      season,
      week,
      franchise_id,
      actual = franchise_score,
      predicted = predictions,
      residual = actual - predicted
    )

}) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    season_week = interaction(season, week, drop = TRUE)
  )

if (nrow(validation_results) == 0L) {
  stop("Die Out-of-sample-Validierung hat keine Beobachtungen geliefert.")
}
if (any(!is.finite(validation_results$residual))) {
  stop("Die Validierungsresiduen enthalten fehlende/nicht endliche Werte.")
}

# Mittlerer historischer Prognosefehler.
bias_correction <- mean(validation_results$residual)

# Wochenkomponente: gemeinsamer Random Effect je Saison/Woche.
model_random_effect <- lme4::lmer(
  residual ~ 1 + (1 | season_week),
  data = validation_results,
  REML = TRUE
)

weekly_effects <- lme4::ranef(model_random_effect)$season_week %>%
  tibble::rownames_to_column("season_week") %>%
  dplyr::rename(weekly_error = `(Intercept)`)

weekly_errors_boot <- weekly_effects$weekly_error
weekly_errors_boot <- weekly_errors_boot - mean(weekly_errors_boot)
weekly_errors_boot <- weekly_errors_boot *
  (sigma_week / stats::sd(weekly_errors_boot))

# Teamkomponente: Residuum nach Abzug von Intercept und Wochen-Random-Effect.
team_effects <- validation_results %>%
  dplyr::mutate(
    weekly_error = lme4::ranef(model_random_effect)$season_week[
      as.character(season_week),
      "(Intercept)"
    ],
    team_error = residual -
      lme4::fixef(model_random_effect)[["(Intercept)"]] -
      weekly_error
  )

team_errors_boot <- team_effects$team_error
team_errors_boot <- team_errors_boot - mean(team_errors_boot)
team_errors_boot <- team_errors_boot *
  (sigma_team / stats::sd(team_errors_boot))

# Vor der Simulation sicherstellen, dass beide Verteilungen verwendbar sind.
if (
  length(weekly_errors_boot) < 2L ||
  any(!is.finite(weekly_errors_boot)) ||
  !is.finite(stats::sd(weekly_errors_boot)) ||
  stats::sd(weekly_errors_boot) == 0
) {
  stop("weekly_errors_boot konnte nicht als gültige Fehlerverteilung erzeugt werden.")
}

if (
  length(team_errors_boot) < 2L ||
  any(!is.finite(team_errors_boot)) ||
  !is.finite(stats::sd(team_errors_boot)) ||
  stats::sd(team_errors_boot) == 0
) {
  stop("team_errors_boot konnte nicht als gültige Fehlerverteilung erzeugt werden.")
}

if (!is.finite(bias_correction)) {
  stop("bias_correction ist nicht endlich.")
}

message(
  "Modell und Fehlerverteilungen erstellt. ",
  "Validierungsfälle: ", nrow(validation_results),
  "; Wochenfehler: ", length(weekly_errors_boot),
  "; Teamfehler: ", length(team_errors_boot)
)

# ---------------------------------------------------------------------------
# 2. Spielplan und Score-Prognosen vorbereiten
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 1. Spielplan und Score-Prognosen vorbereiten
# ---------------------------------------------------------------------------

# Der Spielplan ist in future_schedule enthalten. Jede reale Partie steht
# zweimal darin: einmal aus Sicht jedes Teams.
schedule <- rfl_schedule_data %>%
  dplyr::filter(season == new_season_sept & week >= sim_week) %>%
  dplyr::select(season, week, franchise_id, franchise_name, opponent_id, opponent_name, war_franchise, war_opponent, win_prob) %>%
  rename(war_starter = war_franchise) %>%
  dplyr::mutate(
    franchise_id = normalize_franchise_id(franchise_id),
    opponent_id = normalize_franchise_id(opponent_id)
  ) %>%
  dplyr::select(
    season, week, franchise_id, opponent_id,
    dplyr::any_of(c("franchise_name", "opponent_name", "war_starter"))
  )

# Optionaler Wochenfilter: standardmäßig alle verbleibenden Wochen.
if (!is.null(weeks_to_simulate)) {
  schedule <- schedule %>%
    dplyr::filter(week %in% weeks_to_simulate)
}

required_schedule_cols <- c(
  "season", "week", "franchise_id", "opponent_id", "war_starter"
)
missing_schedule_cols <- setdiff(required_schedule_cols, names(schedule))
if (length(missing_schedule_cols) > 0L) {
  stop(
    "In future_schedule fehlen Spalten: ",
    paste(missing_schedule_cols, collapse = ", "),
    call. = FALSE
  )
}

# Jede gerichtete Begegnung darf nur einmal vorhanden sein.
duplicate_schedule_rows <- schedule %>%
  dplyr::count(season, week, franchise_id, opponent_id) %>%
  dplyr::filter(n > 1)

assert_no_rows(
  duplicate_schedule_rows,
  "future_schedule enthält doppelte gerichtete Matchup-Zeilen."
)

# Prüfen, ob jedes Matchup auch in Gegenrichtung vorhanden ist.
reverse_schedule <- schedule %>%
  rename(
    franchise_id_new = franchise_id,
    opponent_id_new = opponent_id
  ) %>%
  transmute(
    season,
    week,
    franchise_id = opponent_id_new,
    opponent_id = franchise_id_new
  )

missing_reverse_rows <- schedule %>%
  dplyr::anti_join(
    reverse_schedule,
    by = c("season", "week", "franchise_id", "opponent_id")
  )

assert_no_rows(
  missing_reverse_rows,
  "Mindestens ein Matchup fehlt in future_schedule in Gegenrichtung."
)

# WAR muss je Franchise und Woche eindeutig sein, auch wenn ein Team in einer
# Woche zwei Matchups hat.
war_duplicates <- schedule %>%
  dplyr::group_by(season, week, franchise_id) %>%
  dplyr::summarise(
    n_war_values = dplyr::n_distinct(war_starter),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_war_values != 1L)

assert_no_rows(
  war_duplicates,
  "war_starter ist nicht eindeutig je Franchise und Woche."
)

# Erzeuge genau eine Prognose je Franchise und Woche.
predicted_scores <- schedule %>%
  dplyr::distinct(season, week, franchise_id, war_starter) %>%
  dplyr::mutate(
    predicted_score = stats::predict(
      model_score,
      newdata = tibble::tibble(war_starter = war_starter)
    )
  )

invalid_predictions <- predicted_scores %>%
  dplyr::filter(!is.finite(predicted_score))
assert_no_rows(
  invalid_predictions,
  "Es gibt fehlende oder nicht endliche Score-Prognosen. Prüfe war_starter."
)

# Es muss exakt eine Score-Prognose je Team und Woche geben.
score_duplicates <- predicted_scores %>%
  dplyr::count(season, week, franchise_id) %>%
  dplyr::filter(n != 1L)

assert_no_rows(
  score_duplicates,
  "predicted_scores ist nicht eindeutig je Saison/Woche/Franchise."
)

# ---------------------------------------------------------------------------
# 3. Eine vollständige Saison-Simulation
# ---------------------------------------------------------------------------

simulate_season <- function(predicted_scores, schedule) {

  # 2a. Pro Franchise/Woche genau einen Fantasy-Score simulieren.
  #     Der weekly_error ist innerhalb einer Woche für alle Teams identisch.
  #     Der team_error wird für jedes Team separat gezogen.
  simulated_scores <- predicted_scores %>%
    dplyr::select(season, week, franchise_id, predicted_score) %>%
    dplyr::group_by(season, week) %>%
    dplyr::mutate(
      weekly_error = sample(weekly_errors_boot, size = 1L),
      team_error = sample(
        team_errors_boot,
        size = dplyr::n(),
        replace = TRUE
      ),
      simulated_score = predicted_score +
        bias_correction + weekly_error + team_error
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, week, franchise_id, simulated_score)

  # 2b. Die Team-Wochen-Scores den einzelnen Matchups zuordnen.
  #     Da schedule beide Richtungen enthält, erhält jedes Team seinen Score
  #     in jeder seiner beiden Partien dieser Woche.
  matchup_results <- schedule %>%
    dplyr::left_join(
      simulated_scores,
      by = c("season", "week", "franchise_id"),
      relationship = "many-to-one"
    ) %>%
    dplyr::rename(score = simulated_score) %>%
    dplyr::left_join(
      simulated_scores %>%
        dplyr::rename(
          opponent_id = franchise_id,
          opponent_score = simulated_score
        ),
      by = c("season", "week", "opponent_id"),
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      result = dplyr::case_when(
        score > opponent_score ~ 1,
        score < opponent_score ~ 0,
        TRUE ~ 0.5
      )
    )

  # Fehlende Scores dürfen nicht unbemerkt als Niederlagen/Ties weiterlaufen.
  missing_matchup_scores <- matchup_results %>%
    dplyr::filter(
      !is.finite(score) | !is.finite(opponent_score) | is.na(result)
    )
  assert_no_rows(
    missing_matchup_scores,
    "Für mindestens ein Matchup fehlt ein simulierter Score."
  )

  # 2c. Ergebnisse je Franchise aggregieren.
  #     wins enthält bei einem Tie wie bisher 0.5.
  team_records <- matchup_results %>%
    dplyr::group_by(season, franchise_id) %>%
    dplyr::summarise(
      wins = sum(result),
      losses = sum(result == 0),
      ties = sum(result == 0.5),
      .groups = "drop"
    )

  # 2d. PF nur einmal je Franchise/Woche aufsummieren, nicht je Matchup.
  team_points <- simulated_scores %>%
    dplyr::group_by(season, franchise_id) %>%
    dplyr::summarise(
      score = sum(simulated_score),
      .groups = "drop"
    )

  team_results <- team_records %>%
    dplyr::left_join(
      team_points,
      by = c("season", "franchise_id"),
      relationship = "one-to-one"
    )

  list(
    team_results = team_results,
    matchup_results = matchup_results
  )
}

# ---------------------------------------------------------------------------
# 4. Monte-Carlo-Simulationen erzeugen
# ---------------------------------------------------------------------------

set.seed(random_seed)

# Wir speichern nur:
#   - Team-Summen je Simulation (36 Zeilen je Simulation)
#   - gerichtete Matchups in genau einer Richtung (468 Zeilen je Simulation)
# Dadurch vermeiden wir, unnötige Score- und Namensspalten millionenfach
# aufzubewahren.
team_results_list <- vector("list", n_simulations)
matchup_results_list <- vector("list", n_simulations)

for (sim_id in seq_len(n_simulations)) {

  sim <- simulate_season(
    predicted_scores = predicted_scores,
    schedule = schedule
  )

  team_results_list[[sim_id]] <- sim$team_results %>%
    dplyr::mutate(simulation = sim_id)

  matchup_results_list[[sim_id]] <- sim$matchup_results %>%
    # Eine Partie nur aus Sicht des Teams mit der kleineren Franchise-ID
    # speichern. Das Ergebnis bezieht sich danach immer auf team_a.
    dplyr::filter(franchise_id < opponent_id) %>%
    dplyr::transmute(
      simulation = sim_id,
      season,
      week,
      team_a = franchise_id,
      team_b = opponent_id,
      result_a = result
    )

  if (sim_id %% 500L == 0L) {
    message("Simulationen abgeschlossen: ", sim_id, " / ", n_simulations)
  }
}

simulation_results <- dplyr::bind_rows(team_results_list)
simulation_matchups <- dplyr::bind_rows(matchup_results_list)

rm(team_results_list, matchup_results_list)
gc()

# Prüfen, dass pro Simulation alle Teams einmal enthalten sind.
expected_franchises <- sort(unique(schedule$franchise_id))
team_count_check <- simulation_results %>%
  dplyr::group_by(simulation) %>%
  dplyr::summarise(
    n_teams = dplyr::n_distinct(franchise_id),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_teams != length(expected_franchises))
assert_no_rows(
  team_count_check,
  "Mindestens eine Simulation enthält nicht alle erwarteten Teams."
)

# ---------------------------------------------------------------------------
# 5. Seeding-Funktion
# ---------------------------------------------------------------------------

# Diese Funktion bildet deine bisherige Seeding-Logik ab:
#   - Division-Ranking
#   - Conference-Ranking
#   - Division-Sieger zuerst beim Conference-Seeding
#   - Bowl-Zuordnung
#   - globale Seeds innerhalb der Bowls
#   - Playoff-/Bye-Flags
create_seeding <- function(df) {
  df %>%
    # Division-Ranking
    dplyr::group_by(simulation, div_id) %>%
    dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
    dplyr::mutate(div_rank = dplyr::row_number()) %>%

    # Conference-Ranking
    dplyr::group_by(simulation, conf_id) %>%
    dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
    dplyr::mutate(conf_rank = dplyr::row_number()) %>%

    # Division-Sieger innerhalb der Conference zuerst seeden
    dplyr::group_by(simulation, conf_id, div_rank) %>%
    dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
    dplyr::mutate(
      seed = ifelse(div_rank == 1L, dplyr::row_number(), NA_real_)
    ) %>%

    # Restliche Teams nach Wins und PF dahinter einsortieren
    dplyr::group_by(simulation, conf_id) %>%
    dplyr::arrange(seed, dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
    dplyr::mutate(
      seed = ifelse(is.na(seed), dplyr::row_number(), seed),
      bowl = dplyr::case_when(
        seed <= 6 ~ 1L,
        seed <= 12 ~ 2L,
        TRUE ~ 3L
      )
    ) %>%

    # Globale Reihenfolge innerhalb der Bowls
    dplyr::group_by(simulation, bowl) %>%
    dplyr::arrange(dplyr::desc(wins_sim), dplyr::desc(pf_sim)) %>%
    dplyr::mutate(
      seed = ifelse(bowl > 1L, dplyr::row_number(), seed)
    ) %>%

    # Finale Liga-Rangfolge und Qualifikations-Flags
    dplyr::group_by(simulation) %>%
    dplyr::mutate(po_helper = ifelse(bowl == 1L, 1L, 0L)) %>%
    dplyr::arrange(bowl, seed) %>%
    dplyr::mutate(
      league_rank = dplyr::row_number(),
      sb = bowl == 1L,
      pb = bowl <= 2L,
      sb_bye = bowl == 1L & seed <= 2L,
      pb_bye = (bowl == 2L & seed <= 4L) | bowl == 1L
    ) %>%
    dplyr::ungroup()
}

# ---------------------------------------------------------------------------
# 6. Aktuellen Tabellenstand sauber mit den Simulationen verbinden
# ---------------------------------------------------------------------------

create_simulation_standings <- function(simulation_results,
                                        add_seeding = FALSE) {

  # Die Saison aus dem zukünftigen Spielplan verwenden, damit nicht aus
  # Versehen eine andere Saison aus rfl_standing_data gezogen wird.
  target_season <- max(schedule$season, na.rm = TRUE)

  current_season_data <- rfl_standing_data %>%
    dplyr::filter(season == target_season)

  if (nrow(current_season_data) == 0L) {
    stop(
      "Keine aktuellen Tabellenstände für Saison ", target_season,
      " in rfl_standing_data gefunden.",
      call. = FALSE
    )
  }

  # Letzte vorhandene Tabellenwoche dieser Saison.
  latest_week <- max(current_season_data$week, na.rm = TRUE)

  current_standings <- current_season_data %>%
    dplyr::filter(week == latest_week) %>%
    dplyr::mutate(
      franchise_id = normalize_franchise_id(franchise_id)
    ) %>%
    dplyr::select(
      franchise_id,
      franchise_name,
      conf_id,
      div_id,
      wins_current = wins_season,
      score_current = pf_season
    )

  # Ein Franchise darf im aktuellen Tabellenstand nur einmal vorkommen.
  duplicate_franchises <- current_standings %>%
    dplyr::count(franchise_id) %>%
    dplyr::filter(n != 1L)
  assert_no_rows(
    duplicate_franchises,
    "Der aktuelle Tabellenstand enthält Franchise-Duplikate."
  )

  # Franchise-IDs aus den Simulationen ebenfalls normalisieren.
  simulation_results <- simulation_results %>%
    dplyr::mutate(
      franchise_id = normalize_franchise_id(franchise_id)
    )

  # Prüfen, ob alle simulierten Teams in den aktuellen Tabellenständen
  # gefunden werden. Das verhindert die zuvor aufgetretenen NA bei conf_id,
  # div_id und franchise_name.
  missing_franchises <- simulation_results %>%
    dplyr::distinct(franchise_id) %>%
    dplyr::anti_join(
      current_standings %>% dplyr::distinct(franchise_id),
      by = "franchise_id"
    )

  if (nrow(missing_franchises) > 0L) {
    stop(
      "Für folgende Franchise-IDs fehlt ein aktueller Tabellenstand: ",
      paste(missing_franchises$franchise_id, collapse = ", "),
      call. = FALSE
    )
  }

  # Metadaten und aktuelle Siege/Punkte an die Simulationsergebnisse hängen.
  standings <- simulation_results %>%
    dplyr::left_join(
      current_standings,
      by = "franchise_id",
      relationship = "many-to-one"
    ) %>%
    dplyr::mutate(
      wins_sim = wins + wins_current,
      pf_sim = round(score + score_current, 2)
    )

  # Nach dem Join dürfen keine zentralen Ranking-Informationen fehlen.
  invalid_standings <- standings %>%
    dplyr::filter(
      is.na(franchise_name) |
        is.na(conf_id) |
        is.na(div_id) |
        !is.finite(wins_sim) |
        !is.finite(pf_sim)
    )
  assert_no_rows(
    invalid_standings,
    "Nach dem Join fehlen franchise_name, conf_id, div_id, wins_sim oder pf_sim."
  )

  # Jede Simulation muss je Franchise genau eine Zeile haben.
  duplicate_simulation_teams <- standings %>%
    dplyr::count(simulation, franchise_id) %>%
    dplyr::filter(n != 1L)
  assert_no_rows(
    duplicate_simulation_teams,
    "Es gibt doppelte Zeilen je Simulation und Franchise."
  )

  if (add_seeding) {
    standings %>% create_seeding()
  } else {
    standings
  }
}

# Rohdaten-Basis ohne Seeding: Diese Tabelle wird für die Counterfactuals
# wiederverwendet. Das Seeding wird pro Szenario neu berechnet.
base_standings <- create_simulation_standings(
  simulation_results,
  add_seeding = FALSE
) %>%
  dplyr::select(
    simulation,
    franchise_id,
    conf_id,
    div_id,
    wins_sim,
    pf_sim
  ) %>%
  dplyr::arrange(simulation, franchise_id)

# Optional: normales Seeding der ursprünglichen Monte-Carlo-Simulationen.
# Dient als Referenz für die Playoffwahrscheinlichkeiten ohne Eingriff.
baseline_seeding <- create_seeding(base_standings)

# Pro Simulation einen kompakten Integer-Index anlegen. Dieser Index erlaubt
# später das Zuordnen des jeweiligen Matchup-Ergebnisses ohne wiederholten Join.
simulation_levels <- sort(unique(base_standings$simulation))
base_standings <- base_standings %>%
  dplyr::mutate(
    sim_index = match(simulation, simulation_levels)
  )

# Es muss je Simulation die gleiche Anzahl Teams geben.
team_count_per_simulation <- base_standings %>%
  dplyr::count(simulation)
if (any(team_count_per_simulation$n != length(expected_franchises))) {
  stop(
    "base_standings enthält nicht in jeder Simulation gleich viele Teams.",
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# 7. Eindeutige Matchups und Ergebnisvektoren vorbereiten
# ---------------------------------------------------------------------------

# Jede reale Partie nur einmal. team_a ist immer die kleinere Franchise-ID.
unique_matchups <- schedule %>%
  dplyr::mutate(
    team_a = pmin(franchise_id, opponent_id),
    team_b = pmax(franchise_id, opponent_id)
  ) %>%
  dplyr::distinct(season, week, team_a, team_b) %>%
  dplyr::arrange(season, week, team_a, team_b) %>%
  dplyr::mutate(matchup_id = dplyr::row_number())

# Die bereits gespeicherten Simulationsergebnisse auf diese Matchups abbilden.
# result_a ist immer das Ergebnis aus Sicht von team_a.
simulation_matchups <- simulation_matchups %>%
  dplyr::left_join(
    unique_matchups,
    by = c("season", "week", "team_a", "team_b"),
    relationship = "many-to-one"
  )

if (anyNA(simulation_matchups$matchup_id)) {
  stop(
    "Nicht alle simulierten Matchups konnten einem matchup_id zugeordnet werden.",
    call. = FALSE
  )
}

# Nach matchup_id und simulation sortieren. Danach enthält jeder Vektor
# result_a in der Reihenfolge simulation_levels (1, 2, ..., n_simulations).
simulation_matchups <- simulation_matchups %>%
  dplyr::arrange(matchup_id, simulation)

matchup_counts <- simulation_matchups %>%
  dplyr::count(matchup_id) %>%
  dplyr::filter(n != n_simulations)
assert_no_rows(
  matchup_counts,
  "Mindestens ein Matchup besitzt nicht genau ein Ergebnis je Simulation."
)

# Nur die Ergebnisvektoren behalten. Das spart Speicher gegenüber einer Liste
# von Dataframes mit vielen wiederholten ID-Spalten.
matchup_result_vectors <- split(
  simulation_matchups$result_a,
  factor(
    simulation_matchups$matchup_id,
    levels = unique_matchups$matchup_id
  )
)

rm(simulation_matchups)
gc()

# ---------------------------------------------------------------------------
# 8. Counterfactual: ein Matchup erzwingen und Seeding neu berechnen
# ---------------------------------------------------------------------------

# Berechnet für ein Szenario die Playoff-/Bye-Wahrscheinlichkeit beider Teams.
# new_result_a = 1: team_a gewinnt, team_b verliert.
# new_result_a = 0: team_a verliert, team_b gewinnt.
calculate_one_counterfactual <- function(game,
                                         original_result_vector,
                                         new_result_a,
                                         scenario_name) {

  team_a <- game$team_a[[1]]
  team_b <- game$team_b[[1]]

  # Das ursprüngliche Ergebnis der jeweiligen Simulation an jede Teamzeile
  # hängen. Über sim_index statt left_join: deutlich weniger Join-Aufwand.
  original_result_by_row <- original_result_vector[base_standings$sim_index]

  # Nur die beiden beteiligten Teams erhalten eine Änderung bei wins_sim.
  # Alle übrigen Teams bleiben exakt so, wie sie in dieser Simulation waren.
  win_delta <- numeric(nrow(base_standings))
  is_team_a <- base_standings$franchise_id == team_a
  is_team_b <- base_standings$franchise_id == team_b

  win_delta[is_team_a] <- new_result_a - original_result_by_row[is_team_a]
  win_delta[is_team_b] <- original_result_by_row[is_team_b] - new_result_a

  # Die PF-Werte bleiben absichtlich unverändert. Nur das Ergebnis dieses
  # einen Spiels wird ausgetauscht.
  counterfactual_standings <- base_standings %>%
    dplyr::mutate(
      wins_sim = wins_sim + win_delta
    ) %>%
    dplyr::select(-sim_index) %>%
    create_seeding()

  # Für die Leverage-Kennzahl benötigen wir nur die beiden Teams des Spiels.
  # Deshalb werden die 36 Teams je Simulation nicht dauerhaft gespeichert.
  counterfactual_standings %>%
    dplyr::filter(franchise_id %in% c(team_a, team_b)) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::summarise(
      p_sb = mean(sb),
      p_pb = mean(pb),
      p_sb_bye = mean(sb_bye),
      p_pb_bye = mean(pb_bye),
      n_simulations = dplyr::n_distinct(simulation),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      season = game$season[[1]],
      week = game$week[[1]],
      matchup_id = game$matchup_id[[1]],
      team_a = team_a,
      team_b = team_b,
      scenario = scenario_name
    )
}

# Ein Matchup wird in beiden Szenarien ausgewertet. Die Funktion gibt nur
# vier kleine Ergebniszeilen zurück (2 Teams x 2 Szenarien), nicht die gesamte
# 360.000-Zeilen-Seeding-Tabelle.
calculate_matchup_counterfactuals <- function(i) {

  game <- unique_matchups[i, , drop = FALSE]
  result_vector <- matchup_result_vectors[[as.character(game$matchup_id[[1]])]]

  if (length(result_vector) != length(simulation_levels)) {
    stop(
      "Ergebnisvektor hat nicht die erwartete Zahl an Simulationen für matchup_id ",
      game$matchup_id[[1]],
      call. = FALSE
    )
  }

  dplyr::bind_rows(
    calculate_one_counterfactual(
      game = game,
      original_result_vector = result_vector,
      new_result_a = 1,
      scenario_name = "team_a_win"
    ),
    calculate_one_counterfactual(
      game = game,
      original_result_vector = result_vector,
      new_result_a = 0,
      scenario_name = "team_a_loss"
    )
  )
}

# ---------------------------------------------------------------------------
# 9. Counterfactuals parallel berechnen
# ---------------------------------------------------------------------------

# Die Counterfactuals enthalten keine Zufallsziehungen mehr. Deshalb ist
# seed = FALSE korrekt und vermeidet unnötige RNG-Verwaltung in den Workern.
# Auf Windows wird multisession verwendet; zwei Worker sind ein vorsichtiger
# Startwert, da jeder Worker base_standings im Speicher benötigt.
old_future_plan <- future::plan()

counterfactual_runtime <- system.time({
  counterfactual_results <- tryCatch(
    {
      future::plan(
        future::multisession,
        workers = workers
      )

      furrr::future_map_dfr(
        seq_len(nrow(unique_matchups)),
        calculate_matchup_counterfactuals,
        .options = furrr::furrr_options(seed = FALSE),
        .progress = TRUE
      )
    },
    finally = {
      # Den vorherigen Future-Plan auch dann wiederherstellen, wenn ein Worker
      # einen Fehler meldet.
      future::plan(old_future_plan)
    }
  )
})

print(counterfactual_runtime)

# Plausibilitätsprüfung: vier Zeilen je Matchup/Szenario-Paar-Set.
expected_counterfactual_rows <- nrow(unique_matchups) * 2L * 2L
if (nrow(counterfactual_results) != expected_counterfactual_rows) {
  stop(
    "Unerwartete Anzahl Counterfactual-Zeilen: ",
    nrow(counterfactual_results),
    " statt ", expected_counterfactual_rows,
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# 10. Leverage je Team und Matchup berechnen
# ---------------------------------------------------------------------------

# Die Szenarien beziehen sich zunächst immer auf team_a:
#   team_a_win  = A gewinnt und B verliert
#   team_a_loss = A verliert und B gewinnt
# Für team_b ist die Interpretation entsprechend umgekehrt.
key_matchup_by_team <- counterfactual_results %>%
  tidyr::pivot_wider(
    id_cols = c(
      matchup_id, season, week, team_a, team_b, franchise_id
    ),
    names_from = scenario,
    values_from = c(p_sb, p_pb, p_sb_bye, p_pb_bye)
  ) %>%
  dplyr::mutate(
    # Wahrscheinlichkeit, wenn das jeweilige franchise_id-Team gewinnt.
    p_sb_if_win = dplyr::if_else(
      franchise_id == team_a,
      p_sb_team_a_win,
      p_sb_team_a_loss
    ),
    p_sb_if_loss = dplyr::if_else(
      franchise_id == team_a,
      p_sb_team_a_loss,
      p_sb_team_a_win
    ),
    p_pb_if_win = dplyr::if_else(
      franchise_id == team_a,
      p_pb_team_a_win,
      p_pb_team_a_loss
    ),
    p_pb_if_loss = dplyr::if_else(
      franchise_id == team_a,
      p_pb_team_a_loss,
      p_pb_team_a_win
    ),
    p_sb_bye_if_win = dplyr::if_else(
      franchise_id == team_a,
      p_sb_bye_team_a_win,
      p_sb_bye_team_a_loss
    ),
    p_sb_bye_if_loss = dplyr::if_else(
      franchise_id == team_a,
      p_sb_bye_team_a_loss,
      p_sb_bye_team_a_win
    ),
    p_pb_bye_if_win = dplyr::if_else(
      franchise_id == team_a,
      p_pb_bye_team_a_win,
      p_pb_bye_team_a_loss
    ),
    p_pb_bye_if_loss = dplyr::if_else(
      franchise_id == team_a,
      p_pb_bye_team_a_loss,
      p_pb_bye_team_a_win
    ),

    # Leverage = Wahrscheinlichkeit bei Sieg minus Wahrscheinlichkeit
    # bei Niederlage. Ein positiver Wert zeigt den Vorteil eines Sieges.
    leverage_sb = round(p_sb_if_win - p_sb_if_loss, 3),
    leverage_pb = round(p_pb_if_win - p_pb_if_loss, 3),
    leverage_sb_bye = round(p_sb_bye_if_win - p_sb_bye_if_loss, 3),
    leverage_pb_bye = round(p_pb_bye_if_win - p_pb_bye_if_loss)
  )

# Teamnamen und Division-/Conference-Zugehörigkeit ergänzen.
team_lookup <- rfl_standing_data %>%
  dplyr::filter(season == max(schedule$season, na.rm = TRUE)) %>%
  dplyr::mutate(franchise_id = normalize_franchise_id(franchise_id)) %>%
  dplyr::filter(week == max(week, na.rm = TRUE)) %>%
  dplyr::select(franchise_id, franchise_name, conf_id, div_id) %>%
  dplyr::distinct()

team_a_lookup <- team_lookup %>%
  dplyr::rename(
    team_a = franchise_id,
    franchise_name_a = franchise_name,
    conf_id_a = conf_id,
    div_id_a = div_id
  )

team_b_lookup <- team_lookup %>%
  dplyr::rename(
    team_b = franchise_id,
    franchise_name_b = franchise_name,
    conf_id_b = conf_id,
    div_id_b = div_id
  )

key_matchups <- key_matchup_by_team %>%
  dplyr::left_join(
    team_a_lookup,
    by = "team_a",
    relationship = "many-to-one"
  ) %>%
  dplyr::left_join(
    team_b_lookup,
    by = "team_b",
    relationship = "many-to-one"
  ) %>%
  dplyr::mutate(
    team_name = dplyr::if_else(
      franchise_id == team_a,
      franchise_name_a,
      franchise_name_b
    ),
    same_conference = conf_id_a == conf_id_b,
    same_division = same_conference & div_id_a == div_id_b
  ) %>%
  dplyr::arrange(dplyr::desc(abs(leverage_sb))) %>%
  dplyr::mutate(
    sim_season = new_season_sept,
    sim_week = sim_week,
    opponent_id = ifelse(franchise_id == team_a, team_b, team_a)
  ) %>%
  dplyr::select(sim_season, sim_week, matchup_season = season, matchup_week = week, franchise_id, opponent_id, dplyr::starts_with("same_"), dplyr::starts_with("leverage_")) %>%

  # quantifizierng

#key_matchups <- tmp_data %>%
 # dplyr::select(-dplyr::ends_with("_key")) %>%
  dplyr::group_by(sim_season, sim_week) %>%
  dplyr::mutate(
    across(starts_with("leverage"), ~ round(.x, 3)),
    dplyr::across(
      c(leverage_sb, leverage_pb, leverage_sb_bye, leverage_pb_bye),
      ~ round(dplyr::percent_rank(.x), 2),
      .names="{.col}_week_pctl"
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(sim_season) %>%
  dplyr::mutate(
    dplyr::across(
      c(leverage_sb, leverage_pb, leverage_sb_bye, leverage_pb_bye),
      ~ round(dplyr::percent_rank(.x), 2),
      .names="{.col}_season_pctl"
    )
  ) %>%

  # Perzentil aus Sicht des eigenen Teams
  group_by(sim_season, sim_week, franchise_id) %>%
  mutate(
    leverage_sb_team_pctl =
      dplyr::percent_rank(leverage_sb_season_pctl)
  ) %>%
  ungroup() %>%

  # Perzentil aus Sicht des Gegners
  group_by(sim_season, sim_week, opponent_id) %>%
  mutate(
    opponent_team_pctl =
      dplyr::percent_rank(leverage_sb_season_pctl)
  ) %>%
  ungroup() %>%

  # Beide Perspektiven kombinieren
  mutate(
    leverage_sb_key_score =
      (leverage_sb_team_pctl + opponent_team_pctl) / 2
  ) %>%

  group_by(sim_season, sim_week) %>%
  mutate(
    leverage_sb_threshold =
      median(leverage_sb_key_score, na.rm = TRUE) +
      0.7 * (
        quantile(leverage_sb_key_score, 0.90, na.rm = TRUE) -
          median(leverage_sb_key_score, na.rm = TRUE)
      ),

    leverage_sb_key =
      leverage_sb_key_score > leverage_sb_threshold
  ) %>%
  ungroup()




#tmp_data <- readr::read_csv(paste0("rfl_key_matchups_", new_season_sept, "_", 9, ".csv"))

qs2::qs_save(key_matchups, paste0("rfl_key_matchups_", new_season_sept, "_", sim_week, ".qs2"))

# ---------------------------------------------------------------------------
# 11. Ergebnisse prüfen und anzeigen
# ---------------------------------------------------------------------------

# Baseline-Playoffwahrscheinlichkeiten aus den ursprünglichen Simulationen.
baseline_playoff_probabilities <- baseline_seeding %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::summarise(
    p_sb = mean(sb),
    p_pb = mean(pb),
    p_sb_bye = mean(sb_bye),
    p_pb_bye = mean(pb_bye),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(p_sb)) %>%
  dplyr::mutate(
    season = new_season_sept,
    week = sim_week
  )

old_data <- readr::read_csv(
  paste0("https://github.com/bohndesverband/rfl-data/releases/download/standing_data/rfl_baseline_playoff_probabilities_", new_season_sept, ".csv"),
  col_types = "ddddddd"
) %>%
  dplyr::filter(week < sim_week)

readr::write_csv(rbind(old_data, baseline_playoff_probabilities), paste0("data/rfl_baseline_playoff_probabilities_", new_season_sept, ".csv"))

piggyback::pb_upload(paste0("data/rfl_baseline_playoff_probabilities_", new_season_sept, ".csv"), "bohndesverband/rfl-data", "standing_data", overwrite = TRUE)

#DBI::dbWriteTable(con, "rfl_po_prob", rbind(old_data, baseline_playoff_probabilities), overwrite = TRUE)

#rm(rfl_standing_data_with_prob)

# Die wichtigsten Objekte für weitere Analysen:
#   simulation_results               Team-Ergebnisse je Simulation
#   simulation_matchups              wurde aus Speichergründen entfernt
#   baseline_seeding                 normales Seeding aller Simulationen
#   counterfactual_results           kompakte Wahrscheinlichkeiten je Szenario
#   key_matchup_by_team              Leverage je Team und Matchup
#   key_matchup_overview             Gesamt-Leverage je Matchup
#   baseline_playoff_probabilities   normale SB/PB-/Bye-Wahrscheinlichkeiten
# ============================================================================
