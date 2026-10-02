new_season_sept <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

season_before_wk_1 <- new_season_sept

if (current_week == 1) {
  season_before_wk_1 <- nflreadr::get_current_season() - 1
}

rfl_starter_data <- purrr::map_df(2016:season_before_wk_1, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/starter_data/rfl_starter_{x}.csv"),
    col_types = "iiccccccni"
  )
}) %>%
  dplyr::left_join(
    rfl_player_data %>%
      dplyr::select(season, week, player_id, pos_grouped, fpts_running, ppg = ppg_running, ppg_diff = ppg_running_diff),
    by = c("season", "week", "player_id")
  )

# TODO: mit roster data direkt zusammenführen

DBI::dbWriteTable(con, "rfl_starter_data", rfl_starter_data, overwrite = TRUE)
