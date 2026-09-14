if (!exists("con", mode = "any") || !DBI::dbIsValid(con)) {
  if (!dir.exists("data")) {
    dir.create("data", recursive = TRUE, showWarnings = FALSE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path("data", "data.duckdb"), read_only = TRUE)
}

read_data_table <- function(table_name) {
  if (DBI::dbExistsTable(con, table_name)) {
    return(DBI::dbReadTable(con, table_name))
  }

  stop(sprintf("Table '%s' not found in DuckDB.", table_name))
}

source("R new/base_data/variables.R", local = TRUE)
source("R new/base_data/plots.R", local = TRUE)
source("R new/base_data/reactables.R", local = TRUE)

rfl_franchise_data <- read_data_table("rfl_franchise_data")
rfl_matchups_history <- read_data_table("rfl_matchups_history")

source("R new/base_data/elo.R", local = TRUE)

rfl_player_scores <- read_data_table("rfl_player_scores")

source("R new/base_data/fantasy_finishes.R", local = TRUE)

source("R new/base_data/rosters.R", local = TRUE)

source("R new/base_data/standing.R", local = TRUE)

source("R new/base_data/starter.R", local = TRUE)

source("R new/base_data/war.R", local = TRUE)

source("R new/base_data/draft.R", local = TRUE) # nach fantasy_finishes, elo und war

source("R new/base_data/transactions.R", local = TRUE) # nach Draft und WAR

source("R new/base_data/mfl-players.R", local = TRUE) # nach ELO & rosters

rfl_postseason_data <- read_data_table("rfl_postseason_data")

rfl_sos_data <- read_data_table("sos_inseason")
rfl_schedule_data <- read_data_table("rfl_schedule_data")
