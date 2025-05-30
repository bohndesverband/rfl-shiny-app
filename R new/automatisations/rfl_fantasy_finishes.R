# nach player_scores
library(tidyverse)
library(feather)

rfl_fantasy_finishes <- feather::read_feather("data/player_scores.feather") %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  ) %>%
  dplyr::group_by(season, player_id) %>%
  dplyr::summarise(
    pos = dplyr::last(pos),
    player_name = dplyr::last(player_name),
    team = dplyr::last(team),
    points = sum(as.numeric(points), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(season, pos) %>%
  dplyr::arrange(dplyr::desc(points)) %>%
  dplyr::mutate(
    pos_rank = row_number(),
    top3 = ifelse(pos_rank <= 3, 1, 0),
    top5 = ifelse(pos_rank <= 5, 1, 0),
    top8 = ifelse(pos_rank <= 8, 1, 0),
    top12 = ifelse(pos_rank <= 12, 1, 0),
    top24 = ifelse(pos_rank <= 24, 1, 0),
    top36 = ifelse(pos_rank <= 36, 1, 0),
    top48 = ifelse(pos_rank <= 48, 1, 0),
    top60 = ifelse(pos_rank <= 60, 1, 0),
  )

rfl_fantasy_finishes_summarised <- rfl_fantasy_finishes %>%
  #filter(player_id == "11244") %>%
  #dplyr::filter(
  #  season >= input$selectYears[1] & season <= input$selectYears[2]
  #) %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarise(
    last_season = max(season),
    seasons = max(season) - min(season) + 1,
    dplyr::across(c(player_name, team, pos), ~ dplyr::last(.x)),
    dplyr::across(c(top3, top5, top8, top12, top24, top36, top48, top60), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  )
#  dplyr::filter(top60 > 0)
#dplyr::filter(
#  last_season >= input$selectYears[2] - 1
#) %>%

#dplyr::filter(
#  if(isTruthy(input$selectPosition))
#    grepl(input$selectPosition, as.character(pos))
#  else
#    TRUE
#) %>%

#dplyr::mutate(
#  dplyr::across(c(top3:top60), ~ .x / seasons)
#)

feather::write_feather(rfl_fantasy_finishes_summarised, "data/rfl_fantasy_finishes.feather")
