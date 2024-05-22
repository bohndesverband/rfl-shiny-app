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
starter <- purrr::map_df(2016:nflreadr::get_current_season(), function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/starter/rfl-starter-{x}.csv"),
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
war <- purrr::map_df(2016:nflreadr::get_current_season(), function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/war/rfl-war-{x}.csv"),
    col_types = "idccdd"
  ) %>%
    dplyr::mutate(season = x)
})

elo <- purrr::map_df(2016:nflreadr::get_current_season(), function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-elo-{x}.csv"),
    col_types = "ciiccnnnnnnn"
  )
}) %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name, division_name), by = "franchise_id") %>%
  dplyr::left_join(franchises %>% select(franchise_id, franchise_name) %>% rename(opponent_name = franchise_name), by = c("opponent_id" = "franchise_id"))

player_elo <- purrr::map_df(2016:nflreadr::get_current_season(), function(x) {
  readr::read_csv(
    glue::glue("https://raw.githubusercontent.com/jak3sch/rfl/main/data/elo/rfl-player-elo-{x}.csv"),
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

trades <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/trades/rfl-trades.csv", col_types = "ddTdcccc")

rfl_drafts_data <- readr::read_csv("https://raw.githubusercontent.com/jak3sch/rfl/main/data/drafts/rfl-draft.csv", col_types = "ddddccccccd")

true_standing <- readr::read_csv(paste0("https://raw.githubusercontent.com/jak3sch/rfl/main/data/true-standing/rfl-true-standing-", var.season, ".csv"), col_types = "ncinnnnnnnnn") %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name, division_name),
    by = "franchise_id"
  ) %>%
  dplyr::arrange(week)
