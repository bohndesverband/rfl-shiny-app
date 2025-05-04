starter_data <- purrr::map_df(2016:season_before_wk_1, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/starter_data/rfl_starter_{x}.csv"),
    col_types = "iiccdcccni"
  )
})
