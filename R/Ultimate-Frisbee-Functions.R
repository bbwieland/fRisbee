library(tidyverse)
library(rvest)

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

GameScoreCalculator = function(winner_score,loser_score) {
  r = loser_score/(winner_score - 1)
  
  score = 125 + 475*(sin(min(1,(1 - r)/0.5)*0.4*pi)/sin(0.4*pi))
  return(score)
}

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

RatingAdjustedGameScoreCalculatorTeam = function(winner_team,loser_team,winner_score,loser_score,input_data = suppressWarnings(GetFrisbeeRankings())) {
  
  winner_rating = pull(input_data[input_data$Team == winner_team,"Rating"])
  loser_rating = pull(input_data[input_data$Team == loser_team,"Rating"])
  
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

RatingAdjustedGameScoreCalculatorTeam("Virginia","Georgetown",13,11)
RatingAdjustedGameScoreCalculatorTeam("North Carolina State","Virginia",12,8)
