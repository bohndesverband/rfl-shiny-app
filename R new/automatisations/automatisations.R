library(DBI)
library(duckdb)

if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE, showWarnings = FALSE)
}

if (!exists("con", mode = "any") || !DBI::dbIsValid(con)) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path("data", "data.duckdb"))
}

on.exit(DBI::dbDisconnect(con, shutdown = FALSE), add = TRUE)

source("R new/automatisations/elo.R", local = TRUE)
source("R new/automatisations/league.R", local = TRUE)
source("R new/automatisations/mfl_players.R", local = TRUE)
source("R new/automatisations/player_scores.R", local = TRUE)
source("R new/automatisations/roster.R", local = TRUE)
source("R new/automatisations/standings.R", local = TRUE)
source("R new/automatisations/starter.R", local = TRUE) # nach player_scores
source("R new/automatisations/schedule.R", local = TRUE) # nach standings, nach ELO, nach League, nach Starter, nach standing
source("R new/automatisations/war.R", local = TRUE) # nach player_scores
source("R new/automatisations/draft.R", local = TRUE) # nach elo, war
source("R new/automatisations/draftpick-values.R", local = TRUE) # nach draft und war
source("R new/automatisations/postseason.R", local = TRUE)
source("R new/automatisations/fpts_zusammensetzung.R", local = TRUE)
source("R new/automatisations/transactions.R", local = TRUE) # nach draft, war und draftpick values
source("R new/automatisations/draft-grades.R", local = TRUE) # nach transactions
