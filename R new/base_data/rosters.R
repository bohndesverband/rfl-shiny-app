roster_data <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_", new_season_sept, ".csv"), col_types = "diccc")

rfl_current_roster <- ffscrapr::ff_rosters(mfl_connection) %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  )
