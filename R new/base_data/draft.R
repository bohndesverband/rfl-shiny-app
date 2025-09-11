rfl_drafts_data <- feather::read_feather("data/rfl_drafts_data.feather")

rfl_drafts_rookies <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & is_rookie) # nur rookie drafts & rookies

rfl_draft_orders <- feather::read_feather("data/rfl_draft_orders.feather") %>%
  #dplyr::mutate(
    #season = as.numeric(season),
    #pick = as.numeric(pick)
  #) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name, division),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    feather::read_feather("data/rfl_franchises_history.feather") %>%
      dplyr::select(season, franchise_id, franchise_name) %>%
      dplyr::group_by(franchise_id) %>%
      dplyr::arrange(season) %>%
      dplyr::mutate(
        new_name = ifelse(franchise_name != lag(franchise_name) | season == 2017, 1, 0)
      ) %>%
      dplyr::rename(historic_name = franchise_name),
    by = c("season", "franchise_id")
  )

mfl_adp_data <- feather::read_feather("data/mfl_adp_data.feather")

nfl_drafts_data <- feather::read_feather("data/nfl_drafts_data.feather")
