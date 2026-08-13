rfl_draft_grades <- readr::read_csv("../rfl-data/data/rfl-draft-grades.csv") %>%
  #filter(team_id == "0001")
  #filter(merge_id == "2025_0027_1_1_0") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    grade = as.double(grade),
    draft_class = as.double(draft_class),
    pick_split = stringr::str_split(pick, "_"),
    round = as.double(pick_split[1]),
    trade_id = as.double(pick_split[2])
  ) %>%
  dplyr::left_join(
    rfl_drafts_data %>%
      dplyr::select(season, round, pick, asset_name, draft_range_subline, asset_id_new),
    by = c("draft_class" = "season", "round", "trade_id" = "pick")
  ) %>%
  dplyr::group_by(team_id, draft_class) %>%
  dplyr::arrange(round, trade_id) %>%
  dplyr::mutate(
    order = dplyr::case_when(
      pick == "klasse" ~ 0,
      pick == "laterounds" ~ 20,
      grepl("trade", pick) ~ 30,
      TRUE ~ dplyr::row_number()
    ),
    year = draft_class + as.double(year)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(order) %>%
  dplyr::select(-merge_id, -pick_split:-trade_id, -draft_range_subline) %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = c("team_id" = "franchise_id")
  ) %>%
  dplyr::left_join(
    rfl_trade_history %>%
      dplyr::mutate(trade_id = paste0("trade_", trade_id)) %>%
      dplyr::group_by(trade_id) %>%
      dplyr::arrange(trade_side) %>%
      dplyr::mutate(trade_partner = ifelse(trade_side == "franchise_1", dplyr::last(franchise_id), dplyr::first(franchise_id))) %>%
      dplyr::select(trade_id, trade_partner, trade_assets),
    by = c("pick" = "trade_id", "team_id" = "trade_partner")
  ) %>%
  plyr::mutate(
    asset_name_output = dplyr::case_when(
      pick == "klasse" ~ "Draftklasse",
      pick == "laterounds" ~ "Lateround Picks",
      !is.na(trade_assets) ~ paste("Getradet für", trade_assets, sep = "\n"),
      TRUE ~ asset_name
    )
  ) %>%
  dplyr::select(-trade_assets) %>%

  # avg grade
  dplyr::group_by(team_id, draft_class, year, pick) %>%
  dplyr::mutate(
    grade_avg = round(mean(grade, na.rm = TRUE), 1)
  ) %>%
  dplyr::ungroup()

feather::write_feather(rfl_draft_grades, "data/rfl_draft_grades_data.feather")
