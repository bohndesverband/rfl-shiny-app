rfl_roster_data <- feather::read_feather("data/rfl_roster_data.feather")

rfl_current_roster <- ffscrapr::ff_rosters(mfl_connection) %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  )
