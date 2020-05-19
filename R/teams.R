#' @import dplyr
#' @import magrittr
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "Franchise", "MadePlayoffs", "Team", "TeamId", "TEAM_IDS"))


team_index_url <- "https://www.basketball-reference.com/teams/"

#' Index of all active NBA franchises.
#'
#' Returns the "Active Franchises" table as shown at
#' \url{https://www.basketball-reference.com/teams/}.
#'
#' A TeamId variable is added that represents the Basketball Reference
#' unique identifier for that team as shown in the team URL e.g. Atlanta - ATL
#' (\url{https://www.basketball-reference.com/teams/ATL/}).
#' @param include_total_history If TRUE, return data frame with all historical
#'   counterparts of each franchise listed as separate rows.
#' @export
teams.team_index <- function(include_total_history = FALSE) {
  active_table <- xml2::read_html(team_index_url) %>%
    rvest::html_nodes("#teams_active")

  team_links <- active_table %>%
    rvest::html_nodes("a")

  matches <- team_links %>%
    rvest::html_attr("href") %>%
    stringr::str_match("/teams/([A-Z]{3})/")

  team_ids <- matches[,2]

  current_team_names <- team_links %>%
    rvest::html_text()

  current_teams <- tibble(TeamId = team_ids, Franchise = current_team_names)

  if (include_total_history) {
    all_team_names <- active_table %>%
      rvest::html_nodes("th") %>%
      rvest::html_text()

    all_teams <- tibble(Franchise = all_team_names) %>%
      left_join(current_teams, by = "Franchise") %>%
      tidyr::fill(TeamId)

    team_index <- active_table %>%
      first() %>%
      rvest::html_table() %>%
      left_join(all_teams, by = "Franchise") %>%
      # remove first instance of each franchise (the row representing total
      # franchise history)
      arrange(-row_number()) %>%
      distinct(Franchise, .keep_all = TRUE) %>%
      arrange(-row_number())
  } else {
    team_index <- active_table %>%
      first() %>%
      rvest::html_table() %>%
      right_join(current_teams, by = c("Franchise")) %>%
      # keep first instance of each franchise
      distinct(Franchise, .keep_all = TRUE)
  }

  team_index %>%
    select(Franchise, TeamId, everything())
}

get_seasons_table <- function(team_url) {
  team_url %>%
    xml2::read_html() %>%
    rvest::html_node(".stats_table") %>%
    rvest::html_table()
}

not_all_na <- function(x) {
  !(all(is.na(x)) | all(x == ""))
}

parse_team <- function(seasons_df) {
  seasons_df %>%
    tidyr::separate(Team, into = c("Team", "MadePlayoffs"), fill = "right", sep="(?=\\*)") %>%
    mutate(MadePlayoffs = !is.na(MadePlayoffs))
}

#' Index of all seasons for given NBA franchises.
#'
#' Returns the row binded "Seasons" tables as shown on individual team pages
#' e.g. \url{https://www.basketball-reference.com/teams/ATL/}.
#'
#' \code{teams.franchise_index} adds the convenience variable MadePlayoffs,
#' representing whether the team made the playoffs in the corresponding season.
#' This is determined by parsing the "*" symbol off the end of the Team variable
#' such that there are no asterisks in the resulting Team name.
#' @param team_ids Optional vector of Basketball Reference team IDs (see \code{\link{TEAM_IDS}}).
#'   Defaults to NULL which scrapes the franchise index of every active NBA team.
#'
#' @export
teams.franchise_index <- function(team_ids = NULL) {
  if (team_ids %>% is.null()) {
    utils::data(TEAM_IDS, package = "sportsreferenceR")
    team_ids <- TEAM_IDS$TeamId
  }

  team_urls <- paste0(team_index_url, team_ids)

  team_urls %>%
    purrr::map_dfr(get_seasons_table) %>%
    stats::setNames(make.names(names(.), unique = TRUE)) %>%
    select_if(not_all_na) %>%
    parse_team()
}