rfl_draft_production <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & is_rookie == 1) %>%
  dplyr::left_join(
    player_scores %>%
      dplyr::group_by(player_id) %>%
      dplyr::summarise(
        ppg = round(mean(points, na.rm = TRUE), 2),
        .groups = "drop"
      ),
    by = c("mfl_id" = "player_id")
  ) %>%
  dplyr::filter(!is.na(ppg) & !is.na(pos)) %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(pos) %>%
  dplyr::mutate(pos_count = n()) %>%
  dplyr::filter(pos_count > 1) %>%
  dplyr::ungroup()

test <- rfl_draft_production %>%
  filter(franchise_id == "0007") %>%
  dplyr::select(season, overall, ppg)

ggplot2::ggplot(rfl_draft_production, ggplot2::aes(x = overall, y = ppg, group = pos, color = pos)) +
  ggplot2::geom_smooth(se = FALSE) +
  ggplot2::scale_color_discrete(type = colors_position)

ggplot2::ggplot(test, ggplot2::aes(x = overall, y = ppg, group = season, color = season)) +
  ggplot2::geom_smooth(se = FALSE)
