# league data ----
league <- jsonlite::read_json(paste0(var.mflApiBaseEarlyNewYear, "/export?TYPE=league&L=63018&APIKEY=&JSON=1")) %>%
  purrr::pluck("league")

## franchise data ----
franchises <- league %>%
  purrr::pluck("franchises", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    franchise_name = name,
    franchise_id = id
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("divisions", "division") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        division_name = name,
        division_id = id,
        conference_id = conference
      ),
    by = c("division" = "division_id")
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("conferences", "conference") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        conference_name = name
      ),
    by = c("conference_id" = "id")
  )

## starter data ----
starter <- purrr::map_df(2016:var.seasonBeforeWeek1, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/starter_data/rfl_starter_{x}.csv"),
    col_types = "iiccdcccni"
  )
})

## roster data ----
roster <- jsonlite::read_json(paste0(var.mflApiBaseEarlyNewYear, "/export?TYPE=rosters&L=63018&APIKEY=&FRANCHISE=&W=&JSON=1")) %>%
  purrr::pluck("rosters", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::unnest(player) %>%
  dplyr::rename(franchise_id = id) %>%
  tidyr::unnest_wider(player) %>%
  dplyr::rename(player_id = id) %>%
  dplyr::left_join(
    players %>%
      select(player_id, player_name, pos),
    by = "player_id"
  )

# war data ----
war <- purrr::map_df(2016:var.seasonBeforeWeek2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/war_data/rfl_war_{x}.csv"),
    col_types = "idccdd"
  ) %>%
    dplyr::mutate(season = x)
})

elo <- purrr::map_df(2016:var.seasonBeforeWeek2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_team-elo_{x}.csv"),
    col_types = "iiccnnnnnnn"
  )
}) %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name, division_name), by = "franchise_id") %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

player_elo <- purrr::map_df(2016:var.seasonBeforeWeek2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_player-elo_{x}.csv"),
    col_types = "iicccccnniiiii"
  )
}) %>%
  dplyr::left_join(nflreadr::load_players() %>% select(display_name, gsis_id), by = "gsis_id")

player_ranks_avg <- jsonlite::read_json(paste0(var.mflApiBase, "/export?TYPE=playerScores&L=63018&W=AVG&JSON=1"))$playerScores$playerScore %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::select(id, score) %>%
  dplyr::left_join(
    players %>%
      dplyr::select(player_id, pos),
    by = c("id" = "player_id")
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::arrange(dplyr::desc(as.numeric(score))) %>%
  dplyr::mutate(
    rank = row_number()
  )

trades <- rfl_drafts_data <- purrr::map_df(2016:nflreadr::get_current_season(TRUE), function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_{x}.csv"),
    col_types = "dddTdcccc"
  )
})

rfl_drafts_data <- purrr::map_df(2017:nflreadr::get_current_season(TRUE), function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft_{x}.csv"),
    col_types = "ddddccccccd"
  )
})

true_standing <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/standing_data/rfl_true-standing_", var.seasonBeforeWeek2, ".csv"), col_types = "ncinnnnnnnnn") %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name, division_name),
    by = "franchise_id"
  ) %>%
  dplyr::arrange(week)
