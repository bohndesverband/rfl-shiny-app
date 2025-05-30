# create data ----
rfl_trade_baits <- jsonlite::read_json(paste0(mfl_api_base_march, "/export?TYPE=tradeBait&L=63018&APIKEY=aRNp3s%2BWvuWsx12mPlrBYDoeErox&INCLUDE_DRAFT_PICKS=0&JSON=1"))$tradeBaits$tradeBait %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::separate_rows(willGiveUp, sep = ",") %>%
  dplyr::mutate(willGiveUp = willGiveUp) %>%
  dplyr::left_join(mfl_players %>% dplyr::select(player_id, player_name, pos, team), by = c("willGiveUp" = "player_id")) %>%
  dplyr::left_join(rfl_franchise_data %>% dplyr::select(franchise_id, franchise_name), by = "franchise_id") %>%
  dplyr::left_join(
    player_elo %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::select(mfl_id, player_elo_post), by = c("willGiveUp" = "mfl_id")) %>%
  dplyr::select(-timestamp, -franchise_id) %>%
  dplyr::mutate(
    player_elo_post = ifelse(is.na(player_elo_post), 1500, player_elo_post)
  ) %>%
  dplyr::arrange(dplyr::desc(player_elo_post)) %>%
  dplyr::rename(display_name = player_name, position = pos, player_id = willGiveUp) %>%
  dplyr::select(player_id, display_name:team, player_elo_post, franchise_name)

# filter data ----
rfl_trade_bait_filtered <- shiny::reactive({
  rfl_trade_baits %>%
    dplyr::filter(
      if(isTruthy(input$selectPositions))
        sapply(seq_along(position), function(i) {
          any(input$selectPositions %in% trimws(strsplit(position[i], ",")[[1]]))
        })
      else
        TRUE
    )
})

# set inputs ----
#shiny::observe({
#  req(rfl_trade_bait_filtered())
#  req(nrow(rfl_trade_bait_filtered()) > 0) # Prüfen, ob Daten vorhanden sind

#  top_player_ids <- rfl_trade_bait_filtered() %>%
#    head(5)

#  shinyWidgets::updatePickerInput(
#    session,
#    "selectPosition",
#    selected = "",
#  )

#  shinyWidgets::updatePickerInput(
    #session,
    #"selectPlayers",
    #choices = setNames(rfl_trade_bait_filtered()$player_id, rfl_trade_bait_filtered()$display_name),
    #selected = setNames(top_player_ids$player_id, top_player_ids$display_name),
  #)
#})

# output ----
output$trade_bait <- gt::render_gt({
  rfl_trade_bait_filtered() %>%
    gt::gt() %>%
    gt::tab_header(
      title = "RFL Trade Bait"
    ) %>%
    gt_player() %>%
    gt::data_color(
      player_elo_post,
      palette = c(color_red, color_blue)
    ) %>%
    gt::cols_label(
      player_elo_post = "ELO",
      franchise_name = "RFL Team"
    ) %>%

    gt::cols_hide(
      player_id
    ) %>%

    gt::tab_options(
      ihtml.use_filters = FALSE,
    )
})
