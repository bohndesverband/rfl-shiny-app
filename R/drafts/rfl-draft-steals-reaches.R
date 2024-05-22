# base data ----
draft_steals_reaches <- rfl_drafts_data %>%
  dplyr::filter(is_rookie == 1) %>%
  dplyr::group_by(mfl_id) %>%
  dplyr::mutate(
    count = n(),
    min_pick = min(overall),
    max_pick = dplyr::case_when(
      season < 2025 & count == 3 ~ max(overall),
      season < 2025 & count < 3 ~ 253,
      season >= 2025 & count < 3 ~ 217
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    pick_diff = max_pick - min_pick,
    diff_min = overall - min_pick,
    diff_max = overall - max_pick,
    score_raw = dplyr::case_when(
      diff_min == 0 ~ diff_max,
      diff_max == 0 ~ diff_min,
      TRUE ~ diff_min + diff_max,
    ),
    score = score_raw / (1 + log(overall))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(franchise_id, franchise_name),
    by = c("franchise" = "franchise_id")
  ) %>%
  dplyr::mutate(
    label = paste0(player_name, "\n(", franchise_name, ")")
  )

# steals ----
biggest_draft_steals <- draft_steals_reaches %>%
  dplyr::arrange(dplyr::desc(score)) %>%
  head(10)

# reaches ----
biggest_draft_reaches <- draft_steals_reaches %>%
  dplyr::arrange(score) %>%
  head(10)

# pick differenzen ----
biggest_draft_diff <- draft_steals_reaches %>%
  dplyr::select(mfl_id, player_name, min_pick, max_pick, pick_diff) %>%
  dplyr::distinct()

biggest_draft_diff_plot <- ggplot(biggest_draft_diff, aes(x = max_pick, y = min_pick)) +
  geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', color = var.colorYellow) +
  geom_point(data = subset(biggest_draft_diff, pick_diff < 80), color = var.colorAccent, alpha = 0.3, aes(size = pick_diff, text = player_name)) +
  geom_point(data = subset(biggest_draft_diff, pick_diff >= 80), aes(size = pick_diff, text = player_name), color = var.colorRed) +

  labs(
    title = "Größte Pick Differenzen",
    x = "Letzter Pick im Draft",
    y = "Erster Pick im Draft",
    size = "Differenz zwischen 1. und 3. Pick",
  ) +
  plotDefaults

