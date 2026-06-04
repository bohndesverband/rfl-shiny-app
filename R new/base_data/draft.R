rfl_drafts_data <- feather::read_feather("data/rfl_drafts_data.feather")
  #dplyr::left_join(
  #  rfl_war_data %>%
  #    dplyr::group_by(player_id) %>%
  #    dplyr::summarise(
  #      war_list = list(war),
  #      .groups = "drop"
  #    ),
  #  by = c("mfl_id" = "player_id")
  #)

rfl_drafts_rookies <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & is_rookie) # nur rookie drafts & rookies

rfl_draft_pvar_exp <- rfl_drafts_data %>%
  dplyr::filter(season > 2016) %>%
  dplyr::select(overall, pvar_exp) %>%
  dplyr::distinct()

## berechne werte für gesamte klasse ----
rfl_draft_classes_sum <- rfl_drafts_data %>%
  dplyr::filter(season > 2016 & season < new_season_march) %>%
  #filter(season == 2025) %>%
  dplyr::group_by(season, franchise_id, franchise_name, class) %>%
  dplyr::summarise(
    picks = n(),
    pvar = round(sum(pvar), 1),
    pvar_exp = round(sum(pvar_exp), 1),
    voe = round(sum(voe), 1),
    .groups = "drop"
  ) %>%
  dplyr::arrange(dplyr::desc(pvar)) %>%
  dplyr::mutate(
    id = paste(season, franchise_id, sep = "_"),
    rank = dplyr::row_number(),
    pvar_pctl = round(dplyr::percent_rank(pvar), 2)
  ) %>%
  dplyr::group_by(season) %>%
  dplyr::arrange(dplyr::desc(pvar)) %>%
  dplyr::mutate(
    pvar_per_pick = round(pvar / picks, 2),
    pvar_pctl_season = round(dplyr::percent_rank(pvar), 2),
    rank_season = dplyr::row_number(),
    season_date = as.Date(paste0(season, "-01-01")),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(id, season, season_date, franchise_id, franchise_name, class, picks, rank, pvar, pvar_exp, voe, pvar_per_pick, pvar_pctl, rank_season, pvar_pctl_season)

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

rfl_draft_grades <- feather::read_feather("data/rfl_draft_grates_data.feather")

# reactable paletten
pal_pvar <- scale_rainbow(range(rfl_drafts_rookies$pvar, na.rm = TRUE))
pal_pvar_class_sum <- scale_rainbow(range(rfl_draft_classes_sum$pvar, na.rm = TRUE))
pal_pvar_exp <- scale_rainbow(range(rfl_drafts_rookies$pvar_exp, na.rm = TRUE))
pal_voe <- scale_red_green(range(rfl_drafts_rookies$voe, na.rm = TRUE))
pal_voe_class_sum <- scale_red_green(range(rfl_draft_classes_sum$voe, na.rm = TRUE))

coldef_voe <- function(palette_fun = pal_voe, footer_fun = NULL, ...) {
  reactable_coldef_color(
    name = "VOE",
    palette_fun = palette_fun,
    cell = function(value) {
      if (is.na(value)) return(NA)

      if (value > 0) {
        paste0("+", value)
      } else {
        value
      }
    },
    minWidth = 70,
    footer_fun = footer_fun,
    ...
  )
}

coldef_pvar <- function(palette_fun = pal_pvar, footer_fun = NULL, ...) {
  reactable_coldef_bg(
    name = "pVAR",
    palette_fun = palette_fun,
    footer_fun = footer_fun,
    minWidth = 100,
    ...
  )
}

coldef_pvar_exp <- function(palette_fun = pal_pvar_exp, footer_fun = NULL, ...) {
  reactable_coldef_bg(
    name = "pVARexp",
    palette_fun = palette_fun,
    footer_fun = footer_fun,
    minWidth = 100,
    ...
  )
}
