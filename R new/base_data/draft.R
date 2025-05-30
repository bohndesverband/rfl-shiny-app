rfl_drafts_data <- feather::read_feather("rfl_drafts_data.feather")

rfl_drafts_rookies <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & is_rookie) # nur rookie drafts & rookies

rfl_draft_orders <- feather::read_feather("rfl_draft_orders.feather")

mfl_adp_data <- feather::read_feather("mfl_adp_data.feather")

nfl_drafts_data <- feather::read_feather("nfl_drafts_data.feather")
