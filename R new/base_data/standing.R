rfl_standing_data <- read_data_table("rfl_standing_data")

rfl_weekly_standing <- read_data_table("rfl_weekly_standing") %>%
  dplyr::mutate(winloss = as.list(stringr::str_split(winloss, ",")))

rfl_current_standing <- read_data_table("rfl_current_standing") %>%
  dplyr::mutate(
    across(c(winloss, pf_sparkline, elo_sparkline), ~ map(str_split(.x, ","), ~ as.numeric(.x)))
  )
