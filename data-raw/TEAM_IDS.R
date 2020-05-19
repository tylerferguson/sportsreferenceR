TEAM_IDS <- teams.team_index(include_total_history = TRUE) %>%
  select(Franchise, TeamId)

usethis::use_data(TEAM_IDS, overwrite = TRUE)
