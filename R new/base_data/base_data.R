source("R new/base_data/variables.R", local = TRUE)
source("R new/base_data/plots.R", local = TRUE)
source("R new/base_data/reactables.R", local = TRUE)

rfl_franchise_data <- feather::read_feather("data/rfl_franchises.feather")

source("R new/base_data/elo.R", local = TRUE)

rfl_player_scores <- feather::read_feather("data/rfl_player_scores.feather")

source("R new/base_data/fantasy_finishes.R", local = TRUE)

source("R new/base_data/rosters.R", local = TRUE)

source("R new/base_data/standing.R", local = TRUE)

source("R new/base_data/starter.R", local = TRUE)

source("R new/base_data/war.R", local = TRUE)

source("R new/base_data/draft.R", local = TRUE) # nach fantasy_finishes, elo und war

source("R new/base_data/transactions.R", local = TRUE) # nach Draft und WAR

source("R new/base_data/mfl-players.R", local = TRUE) # nach ELO & rosters

source("R new/base_data/matchups.R", local = TRUE) # nach ELO & standing & starter

rfl_postseason_data <- feather::read_feather("data/rfl_postseason_data.feather")

rfl_sos_data <- feather::read_feather("data/rfl_sos_inseason.feather")
rfl_schedule_data <- feather::read_feather("data/rfl_schedule_data.feather")
