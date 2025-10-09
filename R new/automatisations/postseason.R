var_season <- 2024

rfl_postseason_data <- purrr::map_df(2017:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/postseason_data/rfl_postseason_{x}.csv"),
    col_types = "dcciccdcii"
  )
}) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  )

feather::write_feather(rfl_postseason_data, "data/rfl_postseason_data.feather")
