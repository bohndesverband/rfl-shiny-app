rfl_fantasy_finishes_weekly <- feather::read_feather("data/rfl_fantasy_finishes_weekly.feather")

rfl_fantasy_finishes_season <- feather::read_feather("data/rfl_fantasy_finishes_season.feather")

summarize_fantasy_finishes <- function(df) {
  df %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      last_season = max(season),
      seasons = max(season) - min(season) + 1,
      dplyr::across(c(player_name, team, pos), ~ dplyr::last(.x)),
      dplyr::across(c(points, top3, top5, top8, top12, top24, top36, top48, top60), \(x) sum(x, na.rm = TRUE)),
      .groups = "drop"
    )
}
