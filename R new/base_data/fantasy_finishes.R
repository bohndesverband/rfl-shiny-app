rfl_fantasy_finishes_weekly <- read_data_table("rfl_fantasy_finishes_weekly")

rfl_fantasy_finishes_season <- read_data_table("rfl_fantasy_finishes_season")

summarize_fantasy_finishes <- function(df) {
  df %>%
    dplyr::group_by(player_id) %>%
    dplyr::reframe(
      last_season = max(season),
      seasons = max(season) - min(season) + 1,
      dplyr::across(c(player_name, team, pos, pos_rank), ~ dplyr::last(.x)),
      dplyr::across(c(points, top3, top5, top8, top12, top24, top36, top48, top60), \(x) sum(x, na.rm = TRUE)),
      ppg = points / games,
      .groups = "drop"
    )
}
