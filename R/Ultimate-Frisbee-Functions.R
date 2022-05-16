#' Get updated college men's frisbee rankings
#'
#' This function returns up-to-date ultimate frisbee rankings from frisbee-rankings.com
#' @param DivisionIOnly Returns just D-I rankings if TRUE. Defaults to FALSE.
#' @param SimpleTable Returns just a table with team info & rating if TRUE. Defaults to FALSE.
#' @keywords
#' @export
#' @examples
#' GetFrisbeeRankings()

GetFrisbeeRankings = function(DivisionIOnly = F,SimpleTable = F){

  site = rvest::read_html("https://www.frisbee-rankings.com/usau/college/men")
  data = (site %>% rvest::html_table())[[1]][,-c(2,6)] %>%
    dplyr::mutate(Ranked = ifelse(is.na(as.numeric(substr(Team,nchar(Team)-1,nchar(Team)))), F, T)) %>%
    dplyr::mutate(RegionRank = ifelse(Ranked,substr(Team,nchar(Team) - 3, nchar(Team)),NA)) %>%
    dplyr::mutate(Team = ifelse(Ranked,substr(Team,1, nchar(Team) - 5),Team)) %>%
    dplyr::mutate(Rank = as.numeric(Rank))

  wins.losses = unlist(str_split(data$Record,"-"))

  data = data %>%
    dplyr::mutate(Wins = as.numeric(wins.losses[seq(from = 1,to = length(wins.losses),by = 2)]),
           Losses = as.numeric(wins.losses[seq(from = 2,to = length(wins.losses),by = 2)])) %>%
    dplyr::mutate(WinPct = Wins/(Wins + Losses)) %>%
    dplyr::rename(ContributionPct = `%`) %>%
    dplyr::mutate(ContributionPct = as.numeric(ContributionPct)) %>%
    dplyr::mutate(Division = factor(Div,levels = c("D-I","D-III","Dev","?")),
           Region = factor(Region, levels = unique(data$Region))) %>%
    dplyr::select(Rank,RegionRank,Team,Record,WinPct,Rating,Region,Conference,Division,SoS,PDC)

  if (DivisionIOnly == T) {
    data = data %>% dplyr::filter(Division == "D-I")
  }

  if (SimpleTable == T) {
    data = data %>% dplyr::select(Rank, RegionRank, Team, Record, Rating, Region, Conference, Division)
  }

  return(data)

}

#' Calculate game score from a game result
#'
#' This function calculates game score using the USA Ultimate Rankings Algorithm.
#' It returns the game score for the winning team.
#' @param winner_score The score of the winning team.
#' @param loser_score The score of the losing team.
#' @keywords
#' @export
#' @examples
#' GameScoreCalculator(13,12)
#' GameScoreCalculator(13,6)

GameScoreCalculator = function(winner_score,loser_score) {
  r = loser_score/(winner_score - 1)

  score = 125 + 475*(sin(min(1,(1 - r)/0.5)*0.4*pi)/sin(0.4*pi))
  return(score)
}

#' Calculate team rating change from a game result
#'
#' This function calculates game score using the USA Ultimate Rankings Algorithm.
#' Then, it determines the ratings changes as a result of that game outcome.
#' Returns a dataframe with information.
#' @param winner_rating The rating of the winning team.
#' @param loser_rating The rating of the losing team.
#' @param winner_score The score of the winning team.
#' @param loser_score The score of the losing team.
#' @keywords
#' @export
#' @examples
#' RatingAdjustedGameScoreCalculator(1500,1500,13,6)

RatingAdjustedGameScoreCalculator = function(winner_rating,loser_rating,winner_score,loser_score) {

  gamescore = GameScoreCalculator(winner_score = winner_score, loser_score = loser_score)

  winner_game_rating = loser_rating + gamescore
  loser_game_rating = winner_rating - gamescore

  output = data.frame(Team = c("winner","loser"),
                      Initial = c(winner_rating,loser_rating),
                      GameScore = c(winner_game_rating,loser_game_rating)) %>%
    dplyr::mutate(Difference = GameScore - Initial) %>%
    dplyr::mutate(Increased = ifelse(Difference > 0, T, F))

  print(output)
}

#' Calculate team rating change from a game result
#'
#' This function calculates game score using the USA Ultimate Rankings Algorithm.
#' Then, it determines the ratings changes as a result of that game outcome.
#' Unlike its counterpart, RatingAdjustedGameScoreCalculator, it takes team names — not ratings — as inputs.
#' Then, it pulls that team's most updated rating from the rankings site and calculates the results.
#' Returns a dataframe with information.
#' @param winner_team The name of the winning team.
#' @param loser_team The name of the losing team.
#' @param winner_score The score of the winning team.
#' @param loser_score The score of the losing team.
#' @keywords
#' @export
#' @examples
#' RatingAdjustedGameScoreCalculatorTeam("Virginia","Virginia Tech",13,6)

RatingAdjustedGameScoreCalculatorTeam = function(winner_team,loser_team,winner_score,loser_score,input_data = suppressWarnings(GetFrisbeeRankings())) {

  winner_rating = dplyr::pull(input_data[input_data$Team == winner_team,"Rating"])
  loser_rating = dplyr::pull(input_data[input_data$Team == loser_team,"Rating"])

  gamescore = GameScoreCalculator(winner_score = winner_score, loser_score = loser_score)

  winner_game_rating = loser_rating + gamescore
  loser_game_rating = winner_rating - gamescore

  output = data.frame(Name = c(winner_team,loser_team),
                      Team = c("winner","loser"),
                      Initial = c(winner_rating,loser_rating),
                      GameScore = c(winner_game_rating,loser_game_rating)) %>%
    dplyr::mutate(Difference = GameScore - Initial) %>%
    dplyr::mutate(Increased = ifelse(Difference > 0, T, F))

  print(output)
}

#' Get a team's game results
#'
#'This function scrapes a team's game results and computes some useful info for each game.
#'
#' @param team The name of the team you want to scrape data for.
#' @keywords
#' @export
#' @examples
#' GetTeamResults("Virginia")

GetTeamResults = function(team) {

  site = rvest::read_html("https://www.frisbee-rankings.com/usau/college/men")
  links = site %>% rvest::html_nodes("td:nth-child(3) a") %>% html_attr('href')
  teams = site %>% rvest::html_nodes("td:nth-child(3) a") %>% html_text()
  df = data.frame(Teams = teams,URLs = links)

  df.for.pull = df[which(df$Teams == team),]

  scraper.url = paste0("https://www.frisbee-rankings.com",df.for.pull$URLs)

  team.site = rvest::read_html(scraper.url)
  team.data = (team.site %>% rvest::html_table())[[1]]

  opponent.rankings = (team.site %>% html_nodes("td:nth-child(2) a") %>% html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  postgame.rating = (team.site %>% html_nodes("td:nth-child(4) span") %>% html_attr("title")) %>%
    gsub("[^0-9.]","",.) %>%
    as.numeric()

  game.scores = (team.site %>% html_nodes("td:nth-child(3) span") %>% html_attr("title")) %>%
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

