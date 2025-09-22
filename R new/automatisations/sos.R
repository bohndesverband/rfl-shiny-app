library(tidyverse)
library(feather)

var_season <- 2025

sos <- rfl_drafts_data <- purrr::map_df(2024:var_season, function(x) {
  vroom::vroom(
    glue::glue("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/schedule/rfl-sos-{x}.csv"),
    col_types = "cdddddcc"
  ) %>%
    dplyr::mutate(season = x)
})

feather::write_feather(sos, "data/rfl_sos.feather")
