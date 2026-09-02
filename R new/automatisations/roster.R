library(tidyverse)
library(feather)

new_season_sept <- nflreadr::get_current_season()

rfl_roster_data <- vroom::vroom(paste0("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_", new_season_sept, ".csv"), col_types = "diccc") %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, age),
    by = c("player_id" = "mfl_id")
  )

feather::write_feather(rfl_roster_data, "data/rfl_roster_data.feather")
