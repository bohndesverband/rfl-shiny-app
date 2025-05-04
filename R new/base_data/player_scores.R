player_scores <- purrr::map_df(2016:season_before_wk_2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/playerscores_data/rfl_playerscores_{x}.csv"), col_types = "iiccccn"
  )
})
