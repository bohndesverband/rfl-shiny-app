trades <- purrr::map_df(2016:2025, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_{x}.csv"),
    col_types = "dddTdcccc"
  )
})
