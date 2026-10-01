library(DBI)
library(duckdb)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path("data", "data.duckdb"), read_only = FALSE)

source("R new/automatisations/league.R", local = TRUE)
source("R new/automatisations/elo.R", local = TRUE)
source("R new/automatisations/player_scores.R", local = TRUE) # nach elo
source("R new/automatisations/starter.R", local = TRUE) # nach player_scores
source("R new/automatisations/schedule.R", local = TRUE) # nach standings, nach ELO, nach League, nach Starter, nach standing
source("R new/automatisations/war.R", local = TRUE) # nach player_scores
source("R new/automatisations/roster.R", local = TRUE) # nach war, starter
source("R new/automatisations/standings.R", local = TRUE) # nach roster
source("R new/automatisations/postseason.R", local = TRUE)
source("R new/automatisations/fpts_zusammensetzung.R", local = TRUE)
#source("R new/automatisations/player_data.R", local = TRUE) # nach player_scores, elo, war
source("R new/automatisations/draft.R", local = TRUE) # nach elo, war, player scores, player_data
source("R new/automatisations/draftpick-values.R", local = TRUE) # nach draft und war
source("R new/automatisations/draft-grades.R", local = TRUE) # nach transactions
source("R new/automatisations/transactions.R", local = TRUE) # nach draft, war, fantasy finishes und draftpick values, player_data

DBI::dbDisconnect(con, shutdown = TRUE)
