new_season_sept <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

season_before_wk_1 <- new_season_sept

if (current_week == 1) {
  season_before_wk_1 <- nflreadr::get_current_season() - 1
}

rfl_starter_data <- purrr::map_df(2016:season_before_wk_1, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/starter_data/rfl_starter_{x}.csv"),
    col_types = "iiccdcccni"
  )
}) %>%
  dplyr::mutate(
    pos_grouped = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  )

feather::write_feather(rfl_starter_data, "data/rfl_starter_data.feather")
