TEAM_IDS <- teams.team_index() %>%
  select(Franchise, TeamId)

usethis::use_data(TEAM_IDS, overwrite = TRUE)
