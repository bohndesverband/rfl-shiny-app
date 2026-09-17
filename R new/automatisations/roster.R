library(tidyverse)
library(nflreadr)

rfl_roster_data <- purrr::map_df(2024:season_before_wk_1, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_{x}.csv"),
    col_types = "diccc"
  )
}) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, age),
    by = c("player_id" = "mfl_id")
  )

DBI::dbWriteTable(con, "rfl_roster_data", rfl_roster_data, overwrite = TRUE)
