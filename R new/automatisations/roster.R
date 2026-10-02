library(tidyverse)
library(nflreadr)

rfl_roster_data <- purrr::map_df(2016:season_before_wk_1, function(x) {
  vroom::vroom(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_{x}.csv"),
    col_types = "dicccccc"
  )
}) %>%
  #filter(franchise_id == "0017" & season == 2026) %>%
  # add birthday
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, birthdate),
    by = c("player_id" = "mfl_id")
  ) %>%
  # add schedule data for age calculation
  dplyr::left_join(
    nflreadr::load_schedules(2016:new_season_sept) %>%
      dplyr::select(season, week, gameday) %>%
      dplyr::arrange(gameday) %>%
      dplyr::group_by(season, week) %>%
      dplyr::slice(1),
    by = c("season", "week")
  ) %>%
  # calculate age
  dplyr::mutate(
    display_name = nflreadr::clean_player_names(player_name),
    age = round(as.numeric(as.Date(gameday) - as.Date(birthdate)) / 365.25, 1)
  ) %>%
  dplyr::select(-birthdate, -gameday) %>%

  # add player data
  dplyr::mutate(week_join = ifelse(week > 17, 17, week)) %>% # um weekly stats zu mergen

  dplyr::left_join(
    rfl_player_data %>%
      dplyr::select(player_id, season, week, pos_grouped, fpts, war, war_pctl = war_pctl, player_elo = player_elo_post, player_elo_pctl = player_elo_post_pctl),
    by = c("player_id", "season", "week_join" = "week"),
    relationship = "many-to-many"
  ) %>%

  dplyr::group_by(player_id) %>%
  dplyr::arrange(season, week) %>%
  tidyr::fill(c(pos_grouped, player_elo, player_elo_pctl)) %>%
  dplyr::group_by(player_id, season) %>%
  dplyr::arrange(season, week) %>%
  tidyr::fill(c(war, war_pctl)) %>%
  dplyr::ungroup() %>%

  # add starter status
  dplyr::left_join(
    rfl_starter_data %>%
      dplyr::select(season, week, franchise_id, player_id, starter_status),
    by = c("season", "week", "franchise_id", "player_id")
  ) %>%

  dplyr::mutate(started = ifelse(starter_status == "starter", 1, 0)) %>%
  dplyr::group_by(season, franchise_id, player_id) %>%
  dplyr::mutate(games_started = sum(started, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-started) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    rfl_franchise_data %>%
      dplyr::select(franchise_id, franchise_name),
    by = "franchise_id"
  ) %>%
  dplyr::left_join(
    rfl_drafts_data %>%
      dplyr::select(season_drafted = season, franchise_id, mfl_id) %>%
      dplyr::mutate(drafted = 1),
    by = c(player_id = "mfl_id", "franchise_id")
  ) %>%
  #filter(season == 2026 & franchise_id == "0007" & week == 2) %>%
  dplyr::left_join(
    rfl_transactions_data %>%
      dplyr::filter(type_desc == "added") %>%
      dplyr::mutate(added = 1) %>%
      dplyr::select(season_added = season, franchise_id, player_id, added),
    by = dplyr::join_by(
      franchise_id,
      player_id,
      season >= season_added
    ),
    multiple = "last"
  ) %>%
  dplyr::left_join(
    rfl_trade_history %>%
      dplyr::select(season_traded = season, franchise_ids, asset_ids) %>%
      dplyr::mutate(traded = 1) %>%
      tidyr::separate_rows(asset_ids, sep = ",") %>%
      tidyr::separate_rows(franchise_ids, sep = ","),
    by = dplyr::join_by(
      franchise_id == franchise_ids,
      player_id == asset_ids,
      season >= season_traded
    ),
    multiple = "last"
  ) %>%
  dplyr::mutate(
    transaction = dplyr::case_when(
      traded == 1 | season_traded > season_drafted ~ "traded",
      drafted == 1 ~ "drafted",
      TRUE ~ "added"
    ),
    transaction_emoji = dplyr::case_when(
      transaction == "drafted" ~ emoji::emoji("ballot_box_with_check"),
      transaction == "traded" ~ emoji::emoji("arrows_counterclockwise"),
      TRUE ~ emoji::emoji("heavy_plus_sign")
    ),
    pos_grouped = ifelse(is.na(pos_grouped), pos, pos_grouped),
    pos_grouped = dplyr::case_when(
      pos_grouped %in% c("DT", "DE") ~ "DL",
      pos_grouped %in% c("CB", "S") ~ "DB",
      TRUE ~ pos_grouped
    ),
    player_pos_team = paste(pos_grouped, team, sep = ", "),
    player_name_with_info = paste0("<div>", display_name, "</div><div><small>", pos_grouped, ", ", team, "</small></div>")
  ) %>%
  dplyr::select(-season_drafted:-traded, -week_join)

DBI::dbWriteTable(con, "rfl_roster_data", rfl_roster_data, overwrite = TRUE)

# IR ----
rfl_ir_data <- rfl_roster_data %>%
  dplyr::filter(roster_status == "INJURED_RESERVE") %>%
  # filter nur reg season
  dplyr::filter(
    ((season > 2020 | season == 2016) & week <= 13) |
    (season > 2016 & season < 2021 & week <= 12)
  ) %>%
  # add latest PPG data
  dplyr::left_join(
    rfl_fantasy_finishes_season %>%
      dplyr::group_by(player_id) %>%
      dplyr::filter(games > 3 & season >= season - 1) %>% # min 3 games in dieser oder der letzten saison gespielt
      dplyr::filter(season == max(season)) %>%
      dplyr::ungroup() %>%
      dplyr::select(player_id, ppg),
    by = c("player_id")
  ) %>%
  dplyr::select(-roster_status)

DBI::dbWriteTable(con, "rfl_ir_data", rfl_ir_data, overwrite = TRUE)

# depth charts ----
rfl_depth_chart_data <- rfl_roster_data %>%
  #filter(franchise_id == "0017" & season == "2026") %>%
  dplyr::group_by(season) %>%
  dplyr::filter(week == max(week)) %>%
  dplyr::group_by(season, franchise_id) %>%
  dplyr::mutate(war_rank_team = dplyr::dense_rank(dplyr::desc(war))) %>%
  dplyr::group_by(season, franchise_id, pos_grouped) %>%
  dplyr::arrange(dplyr::desc(war)) %>%
  dplyr::mutate(
    depth_chart_rank_pos = dplyr::row_number(),
    depth_chart = dplyr::case_when(
      pos_grouped %in% c("QB", "TE", "PK") & depth_chart_rank_pos == 1 ~ paste0(pos_grouped, depth_chart_rank_pos),
      pos_grouped %in% c("RB", "WR", "DL", "LB", "DB") & depth_chart_rank_pos <= 2 ~ paste0(pos_grouped, depth_chart_rank_pos),
      pos_grouped %in% c("RB", "WR", "TE") ~ "FLEX",
      pos_grouped %in% c("DL", "LB", "DB") ~ "IDP"
    ),
    coord_h = dplyr::case_when(
      depth_chart == "QB1" ~ 8,
      depth_chart == "RB1" ~ 6.5,
      depth_chart == "RB2" ~ 9.5,
      depth_chart == "WR1" ~ 3,
      depth_chart == "WR2" ~ 13,
      depth_chart == "TE1" ~ 5.5,
      depth_chart == "PK1" ~ 13,
      depth_chart == "DL1" ~ 6.5,
      depth_chart == "DL2" ~ 9.5,
      depth_chart == "LB1" ~ 8,
      depth_chart == "LB2" ~ 5.5,
      depth_chart == "DB1" ~ 5.5,
      depth_chart == "DB2" ~ 10.5,
    ),
    coord_v = dplyr::case_when(
      depth_chart == "QB1" ~ -3,
      depth_chart == "PK1" ~ -5,
      depth_chart %in% c("RB1", "RB2") ~ -5,
      depth_chart %in% c("WR1", "WR2", "TE1") ~ -1,
      depth_chart %in% c("DL1", "DL2") ~ 1,
      depth_chart %in% c("LB1", "LB2") ~ 3,
      depth_chart %in% c("DB1", "DB2") ~ 5
    )
  ) %>%
  # flex spieler berechnen
  dplyr::group_by(season, franchise_id, depth_chart) %>%
  dplyr::mutate(
    depth_chart_rank = dplyr::dense_rank(dplyr::desc(war)),
    depth_chart = dplyr::case_when(
      is.na(depth_chart_rank_pos) | depth_chart == "FLEX" & depth_chart_rank > 2 | depth_chart == "IDP" & depth_chart_rank > 3 ~ NA,
      TRUE ~ depth_chart
    )
  ) %>%
  dplyr::group_by(season, franchise_id, depth_chart) %>%
  dplyr::arrange(dplyr::desc(war)) %>%
  dplyr::mutate(
    depth_chart_new = ifelse(depth_chart %in% c("FLEX", "IDP"), paste0(depth_chart, dplyr::row_number()), depth_chart),
  ) %>%
  dplyr::group_by(season, franchise_id, depth_chart, pos_grouped) %>%
  dplyr::mutate(
    depth_chart_rank = dplyr::dense_rank(dplyr::desc(war)),
    depth_chart_pos = dplyr::case_when(
      depth_chart_new %in% c("FLEX1", "FLEX2", "IDP1", "IDP2", "IDP3") ~ paste0(depth_chart, pos_grouped, depth_chart_rank),
      TRUE ~ depth_chart
    ),
    coord_h = dplyr::case_when(
      depth_chart_pos %in% c("FLEXRB1") ~ 5.5,
      depth_chart_pos %in% c("FLEXWR1", "FLEXTE1", "IDPLB1") ~ 10.5,
      depth_chart_pos %in% c("FLEXRB2") ~ 10.5,
      depth_chart_pos %in% c("FLEXTE2", "FLEXWR2", "IDPDB1") ~ 8,
      depth_chart_pos %in% c("IDPDL1", "IDPLB3", "IDPDB2") ~ 3,
      depth_chart_pos %in% c("IDPDL2", "IDPLB2", "IDPDB3") ~ 13,
      TRUE ~ coord_h
    ),
    coord_v = dplyr::case_when(
      depth_chart_pos %in% c("FLEXWR1", "FLEXTE1", "FLEXWR2", "FLEXTE2") ~ -1,
      depth_chart_pos %in% c("FLEXRB1", "FLEXRB2") ~ -3,
      depth_chart_pos %in% c("IDPDL1", "IDPDL2", "IDPDL3") ~ 1,
      depth_chart_pos %in% c("IDPLB1", "IDPLB2", "IDPLB3") ~ 3,
      depth_chart_pos %in% c("IDPDB1", "IDPDB2", "IDPDB3") ~ 5,
      TRUE ~ coord_v
    )
  ) %>%
  dplyr::group_by(season, franchise_id, pos_grouped) %>%
  dplyr::arrange(dplyr::desc(war)) %>%
  dplyr::mutate(
    player = ifelse(!is.na(war), paste0(display_name, " (", war, " WAR)"), display_name),
    pos_players = paste0(player, collapse = "\n")
  ) %>%
  dplyr::group_by(season, depth_chart) %>%
  dplyr::mutate(
    war_rank_league = dplyr::dense_rank(dplyr::desc(war))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(depth_chart)) %>%
  dplyr::select(-war_rank_team, -depth_chart_rank_pos, -depth_chart_rank, -depth_chart_pos, -player, -depth_chart, depth_chart = depth_chart_new)

DBI::dbWriteTable(con, "rfl_depth_chart_data", rfl_depth_chart_data, overwrite = TRUE)
