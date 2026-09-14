rfl_roster_data <- read_data_table("rfl_roster_data")

rfl_current_roster <- ffscrapr::ff_rosters(mfl_connection) %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  )
