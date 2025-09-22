library(tidyverse)
library(nflreadr)
library(feather)

new_season_sept <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

season_before_wk_1 <- new_season_sept
season_before_wk_2 <- new_season_sept

if (current_week == 1) {
  season_before_wk_1 <- nflreadr::get_current_season() - 1
}

if (nflreadr::get_current_week(TRUE) < 2) {
  season_before_wk_2 <- nflreadr::get_current_season() - 1
}

# teams ----
rfl_war_data <- purrr::map_df(2016:season_before_wk_2, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/war_data/rfl_war_{x}.csv"),
    col_types = "icccddii"
  )
})

feather::write_feather(rfl_war_data, "data/rfl_war_data.feather")
