war_data <- purrr::map_df(2016:season_before_wk_2, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/war_data/rfl_war_{x}.csv"),
    col_types = "icccddii"
  ) %>%
    dplyr::mutate(season = x)
})
