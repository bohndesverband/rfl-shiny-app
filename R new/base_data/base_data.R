source("R new/base_data/variables.R", local = TRUE)

rfl_franchise_data <- feather::read_feather("data/rfl_franchises.feather")

source("R new/base_data/elo.R", local = TRUE)

rfl_player_scores <- feather::read_feather("data/rfl_player_scores.feather")

source("R new/base_data/fantasy_finishes.R", local = TRUE)

source("R new/base_data/draft.R", local = TRUE) # nach fantasy_finishes

source("R new/base_data/rosters.R", local = TRUE)

source("R new/base_data/standing.R", local = TRUE)

rfl_starter_data <- feather::read_feather("data/rfl_starter_data.feather")

source("R new/base_data/transactions.R", local = TRUE)

rfl_war_data <- feather::read_feather("data/rfl_war_data.feather")

source("R new/base_data/mfl-players.R", local = TRUE) # nach ELO & rosters
