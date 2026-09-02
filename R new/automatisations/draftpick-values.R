library(tidyverse)
library(isotone)
library(feather)

# data ----
war_data <- rfl_war_data %>%
  dplyr::select(player_id, pvar) %>%
  dplyr::distinct()

draft_data <- rfl_drafts_data %>%
  #dplyr::select(-pvar, -pvar_exp, -voe) %>%
  dplyr::filter(season >= 2017 & season < new_season_march & !is.na(player_name)) %>%
  dplyr::left_join(
    war_data,
    by = c("mfl_id" = "player_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    pvar = ifelse(is.na(pvar), 1, pvar),
  )

# functions ----
C_def <- 2.5
L_def <- 5
S_def <- 2.5

sigma_fun <- function(i, j, C = C_def, L = L_def, S = S_def) {
  C * (((i + j)/2) + L)^(1 / S)
}

weight_function <- function(i, j, C = C_def, L = L_def, S = S_def) {
  sigma <- sigma_fun(i, j, C, L, S)

  exp(-((i - j)^2) / (2 * sigma^2))
}

estimate_pick_value <- function(i, picks, values, C = C_def, L = L_def, S = S_def) {
  weights <- sapply(picks, function(j)
    weight_function(i, j, C, L, S)
  )

  sum(weights * values, na.rm = TRUE) /
    sum(weights, na.rm = TRUE)
}

# berechnung ----
curve_raw <- data.frame(
  pick = 1:max(draft_data$overall)
)

curve_raw$ev <- sapply(curve_raw$pick, function(i)
  estimate_pick_value(
    i,
    picks = draft_data$overall,
    values = draft_data$pvar
  )
)

ggplot(curve_raw, aes(pick, ev)) +
  geom_line(color="red", linewidth=1.4) +
  theme_minimal()

## glättung ----
iso <- isotone::gpava(curve_raw$pick, -curve_raw$ev)

curve_clean <- curve_raw

curve_clean$final_ev <- -iso$x

eps <- 0.007

for(i in 2:nrow(curve_clean)){
  curve_clean$final_ev[i] <-
    min(curve_clean$final_ev[i],
        curve_clean$final_ev[i-1] - eps)
}

gamma <- 13

curve_clean$trade_value <- (
  curve_clean$final_ev / max(curve_clean$final_ev)
)^gamma * 1000

curve_clean$trade_value <- curve_clean$trade_value / 100

### check ----
ggplot(curve_clean, aes(pick, final_ev)) +
  geom_line(color="red", linewidth=1.4) +
  geom_line(aes(y = trade_value), color="blue", linewidth=1.4) +
  theme_minimal()

# zusammenführen ----
rfl_drafts_data <- feather::read_feather("data/rfl_drafts_data.feather") %>%
  #dplyr::select(-pvar, -pvar_exp, -voe) %>%
  #dplyr::select(-first_pick:-side) %>%

  dplyr::left_join(
    war_data,
    by = c("mfl_id" = "player_id"),
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    curve_clean %>% dplyr::select(pick, pvar_exp = final_ev),
    by = c("overall" = "pick")
  ) %>%
  dplyr::mutate(
    pvar_exp = ifelse(season != 2016, round(pvar_exp, 1), NA),
    pvar = ifelse(!is.na(pvar), pvar, 1),
    voe = ifelse(season != 2016, round(pvar - pvar_exp, 1), NA),
    threshold_top_steal = quantile(voe, 0.95, na.rm = TRUE),
    threshold_steal = quantile(voe, 0.9, na.rm = TRUE),
    threshold_reach = quantile(voe, 0.5, na.rm = TRUE),
    threshold_bust = quantile(voe, 0.3, na.rm = TRUE),
    #voe_pctl = dplyr::percent_rank(voe),
    pick_cat = dplyr::case_when(
      season + 2 == new_season_march & voe >= threshold_steal ~ "Rising Star", # top spieler von vor 2 drafts
      season + 3 > new_season_march ~ "Early", # letzten 2 drafts nicht bewerten
      voe >= threshold_top_steal ~ "Top-Steal",
      voe >= threshold_steal ~ "Steal",
      voe <= threshold_bust ~ "Bust",
      voe <= threshold_reach ~ "Reach",
    ),
    pick_cat_badge = ifelse(!is.na(pick_cat), paste0("<span class=\"badge ", pick_cat, "\">", pick_cat, "</span>"), NA),
    range = dplyr::case_when(
      overall <= 12 ~ "Top 12",
      overall <= 36 ~ "Rest of 1st",
      round == 2 ~ "Round 2",
      round == 3 ~ "Round 3",
      round >= 4 ~ "Late Rounds"
    ),
    pick_info = paste(paste0(round, ".", stringr::str_pad(pick, 2, pad = "0")), franchise_name),
    player_info = paste0("(", pos_grouped, ", ", team, ")"),
    player_name_with_info = paste(player_name, player_info),
    player_and_pick_info = paste0("<div>", player_name_with_info, "</div>", "<div>", "<small>", pick_info, "</small>", "</div>"),
    player_name_with_badge = ifelse(!is.na(pick_cat), paste0(player_name, pick_cat_badge), player_name),
    asset_name_with_badge = ifelse(!is.na(pick_cat), paste0(asset_name, pick_cat_badge), asset_name),
    player_name_with_subline = paste0("<div>", player_name_with_badge, "</div>", "<div>", "<small>", draft_range_subline, "</small>", "</div>"),
    asset_name_with_subline = paste0("<div>", asset_name_with_badge, "</div>", "<div>", "<small>", draft_range_subline, "</small>", "</div>")
  ) %>%
  #filter(season == 2024 & franchise_id == "0007")
  dplyr::select(-dplyr::starts_with("threshold"))

feather::write_feather(rfl_drafts_data, "data/rfl_drafts_data.feather")

# historische trades ----
# TODO: Tradevalues
#gamma <- 13

#calc_trade_values <- function(ev, gamma){
#  (ev / max(ev))^gamma * 1000
#}

#player_to_tv <- function(rating, beta = 2.5) {
#  1000 * (rating / 10)^beta
#}

#trades <- rfl_trades_data %>%
#  dplyr::filter(season > 2016 & trade_id != "2024064") %>%
#  dplyr::group_by(trade_id) %>%
#  dplyr::filter(any(grepl("DP", asset_id))) %>%
#  dplyr::filter(!any(grepl("FP", asset_id))) %>%
#  dplyr::filter(!any(is.na(draft_pick))) %>%
#  dplyr::mutate(overall = ((as.integer(draft_round) - 1) * 36) + as.integer(draft_pick)) %>%
#  dplyr::select(trade_id, trade_side, overall)

#trades_with_value <- trades %>%
#  dplyr::left_join(
#  curve_clean %>%
#    dplyr::select(pick, trade_value),
#  by = c("overall" = "pick")
#) %>%
#dplyr::group_by(trade_id, trade_side) %>%
#dplyr::summarise(trade_value = sum(trade_value), .groups = "drop") %>%
#dplyr::group_by(trade_id)
#dplyr::mutate(diff = trade_value - lag(trade_value)) %>%
#dplyr::ungroup()

rm(war_data, draft_data, C_def, L_def, S_def, sigma_fun, weight_function, estimate_pick_value, curve_raw, iso, curve_clean, eps)
