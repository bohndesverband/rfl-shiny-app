# league data ----
league <- jsonlite::read_json(paste0(mfl_api_base_march, "/export?TYPE=league&L=63018&APIKEY=&JSON=1")) %>%
  purrr::pluck("league")

## franchise data ----
franchises <- league %>%
  purrr::pluck("franchises", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    franchise_name = name,
    franchise_id = id
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("divisions", "division") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        division_name = name,
        division_id = id,
        conference_id = conference
      ),
    by = c("division" = "division_id")
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("conferences", "conference") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        conference_name = name
      ),
    by = c("conference_id" = "id")
  )
