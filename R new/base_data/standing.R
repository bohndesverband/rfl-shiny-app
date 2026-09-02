rfl_standing_data <- feather::read_feather("data/rfl_standing_data.feather")

rfl_weekly_standing <- feather::read_feather("data/rfl_weekly_standing.feather") %>%
  dplyr::mutate(winloss = as.list(stringr::str_split(winloss, ",")))

rfl_current_standing <- feather::read_feather("data/rfl_current_standing.feather") %>%
  dplyr::mutate(
    across(c(winloss, pf_sparkline, elo_sparkline), ~ map(str_split(.x, ","), ~ as.numeric(.x)))
  )
