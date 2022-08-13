
#' Get a team's game results
#'
#'This function scrapes a team's game results and computes some useful info for each game.
#'
#' @param team The name of the team you want to scrape data for.
#' @export
#' @examples
#' load_team_results_men("Virginia")

load_team_results_men = function(team) {

  site = rvest::read_html("https://www.frisbee-rankings.com/usau/college/men")
  links = site %>% rvest::html_nodes("td:nth-child(3) a") %>% rvest::html_attr('href')
  teams = site %>% rvest::html_nodes("td:nth-child(3) a") %>% rvest::html_text()
  df = data.frame(Teams = teams,URLs = links)

  df.for.pull = df[which(df$Teams == team),]

  scraper.url = paste0("https://www.frisbee-rankings.com",df.for.pull$URLs)

  team.site = rvest::read_html(scraper.url)
  team.data = (team.site %>% rvest::html_table())[[1]]

  opponent.rankings = (team.site %>% rvest::html_nodes("td:nth-child(2) a") %>% rvest::html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  postgame.rating = (team.site %>% rvest::html_nodes("td:nth-child(4) span") %>% rvest::html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  game.scores = (team.site %>% rvest::html_nodes("td:nth-child(3) span") %>% rvest::html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  team.data.clean = team.data %>%
    dplyr::rename(OppRk = `#`,PctOfRanking = `% of Ranking`) %>%
    dplyr::select(OppRk, Opponent, Result, Effect, Status, PctOfRanking, Date, Event) %>%
    dplyr::mutate(OppRk = as.numeric(OppRk)) %>%
    dplyr::mutate(Status = substr(Status,1,7)) %>%
    dplyr::mutate(Status = factor(stringr::str_trim(Status))) %>%
    dplyr::mutate(Opponent = gsub("\\*","",Opponent)) %>%
    dplyr::mutate(PctOfRanking = as.numeric(gsub("\\%","",PctOfRanking))/100) %>%
    dplyr::mutate(Win = ifelse(substr(Result,1,1) == "W",1,0)) %>%
    dplyr::mutate(Date = as.Date(Date,format = "%h %d"))

  results = gsub("[^0-9.-]","",team.data.clean$Result) %>%
    stringr::str_split("-") %>%
    unlist()

  team.data.clean = team.data.clean %>%
    dplyr::mutate(Pts = as.numeric(results[seq(1,length(results),by = 2)]),
                  OppPts = as.numeric(results[seq(2,length(results),by = 2)]),
                  GameScore = game.scores) %>%
    dplyr::mutate(PtDiff = Pts-OppPts,
                  OpponentRating = opponent.rankings,
                  TeamRatingPostgame = postgame.rating,
                  GameScore = ifelse(Win == 1, GameScore,-GameScore)) %>%
    dplyr::mutate(GameValue = GameScore + OpponentRating,
                  TeamRatingPregame = TeamRatingPostgame - Effect) %>%
    dplyr::mutate(GameValueUsed = ifelse(Status == "Counts",GameValue,0),
                  RatingsImpact = GameValue*PctOfRanking) %>%
    dplyr::mutate(Team = team,
                  GameNum = seq(1:nrow(team.data.clean)))

  team.data.clean
}

#' Get a team's game results
#'
#'This function scrapes a team's game results and computes some useful info for each game.
#'
#' @param team The name of the team you want to scrape data for.
#' @export
#' @examples
#' load_team_results_women("Virginia")

load_team_results_women = function(team) {

  site = rvest::read_html("https://www.frisbee-rankings.com/usau/college/women")
  links = site %>% rvest::html_nodes("td:nth-child(3) a") %>% rvest::html_attr('href')
  teams = site %>% rvest::html_nodes("td:nth-child(3) a") %>% rvest::html_text()
  df = data.frame(Teams = teams,URLs = links)

  df.for.pull = df[which(df$Teams == team),]

  scraper.url = paste0("https://www.frisbee-rankings.com",df.for.pull$URLs)

  team.site = rvest::read_html(scraper.url)
  team.data = (team.site %>% rvest::html_table())[[1]]

  opponent.rankings = (team.site %>% rvest::html_nodes("td:nth-child(2) a") %>% rvest::html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  postgame.rating = (team.site %>% rvest::html_nodes("td:nth-child(4) span") %>% rvest::html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  game.scores = (team.site %>% rvest::html_nodes("td:nth-child(3) span") %>% rvest::html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  team.data.clean = team.data %>%
    dplyr::rename(OppRk = `#`,PctOfRanking = `% of Ranking`) %>%
    dplyr::select(OppRk, Opponent, Result, Effect, Status, PctOfRanking, Date, Event) %>%
    dplyr::mutate(OppRk = as.numeric(OppRk)) %>%
    dplyr::mutate(Status = substr(Status,1,7)) %>%
    dplyr::mutate(Status = factor(stringr::str_trim(Status))) %>%
    dplyr::mutate(Opponent = gsub("\\*","",Opponent)) %>%
    dplyr::mutate(PctOfRanking = as.numeric(gsub("\\%","",PctOfRanking))/100) %>%
    dplyr::mutate(Win = ifelse(substr(Result,1,1) == "W",1,0)) %>%
    dplyr::mutate(Date = as.Date(Date,format = "%h %d"))

  results = gsub("[^0-9.-]","",team.data.clean$Result) %>%
    stringr::str_split("-") %>%
    unlist()

  team.data.clean = team.data.clean %>%
    dplyr::mutate(Pts = as.numeric(results[seq(1,length(results),by = 2)]),
                  OppPts = as.numeric(results[seq(2,length(results),by = 2)]),
                  GameScore = game.scores) %>%
    dplyr::mutate(PtDiff = Pts-OppPts,
                  OpponentRating = opponent.rankings,
                  TeamRatingPostgame = postgame.rating,
                  GameScore = ifelse(Win == 1, GameScore,-GameScore)) %>%
    dplyr::mutate(GameValue = GameScore + OpponentRating,
                  TeamRatingPregame = TeamRatingPostgame - Effect) %>%
    dplyr::mutate(GameValueUsed = ifelse(Status == "Counts",GameValue,0),
                  RatingsImpact = GameValue*PctOfRanking) %>%
    dplyr::mutate(Team = team,
                  GameNum = seq(1:nrow(team.data.clean)))

  team.data.clean
}
