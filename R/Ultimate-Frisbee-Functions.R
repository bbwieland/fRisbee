#' Get updated college men's frisbee rankings
#'
#' This function returns up-to-date ultimate frisbee rankings from frisbee-rankings.com
#' @param DivisionIOnly Returns just D-I rankings if TRUE. Defaults to FALSE.
#' @param SimpleTable Returns just a table with team info & rating if TRUE. Defaults to FALSE.
#' @keywords
#' @export
#' @examples
#' load_rankings_men()

load_rankings_men = function(DivisionIOnly = F,SimpleTable = F){

  site = rvest::read_html("https://www.frisbee-rankings.com/usau/college/men")
  data = (site %>% rvest::html_table())[[1]][,-c(2,6)] %>%
    dplyr::mutate(Ranked = ifelse(is.na(as.numeric(substr(Team,nchar(Team)-1,nchar(Team)))), F, T)) %>%
    dplyr::mutate(RegionRank = ifelse(Ranked,substr(Team,nchar(Team) - 3, nchar(Team)),NA)) %>%
    dplyr::mutate(Team = ifelse(Ranked,substr(Team,1, nchar(Team) - 5),Team)) %>%
    dplyr::mutate(Rank = as.numeric(Rank))

  wins.losses = unlist(stringr::str_split(data$Record,"-"))

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

#' Get updated college women's frisbee rankings
#'
#' This function returns up-to-date ultimate frisbee rankings from frisbee-rankings.com
#' @param DivisionIOnly Returns just D-I rankings if TRUE. Defaults to FALSE.
#' @param SimpleTable Returns just a table with team info & rating if TRUE. Defaults to FALSE.
#' @keywords
#' @export
#' @examples
#' load_rankings_women()

load_rankings_women = function(DivisionIOnly = F,SimpleTable = F){

  site = rvest::read_html("https://www.frisbee-rankings.com/usau/college/women")
  data = (site %>% rvest::html_table())[[1]][,-c(2,6)] %>%
    dplyr::mutate(Ranked = ifelse(is.na(as.numeric(substr(Team,nchar(Team)-1,nchar(Team)))), F, T)) %>%
    dplyr::mutate(RegionRank = ifelse(Ranked,substr(Team,nchar(Team) - 3, nchar(Team)),NA)) %>%
    dplyr::mutate(Team = ifelse(Ranked,substr(Team,1, nchar(Team) - 5),Team)) %>%
    dplyr::mutate(Rank = as.numeric(Rank))

  wins.losses = unlist(stringr::str_split(data$Record,"-"))

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
#' calculate_game_score(13,12)
#' calculate_game_score(13,6)

calculate_game_score = function(winner_score,loser_score) {
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
#' calculate_game_score_adjusted(1500,1500,13,6)

calculate_game_score_adjusted = function(winner_rating,loser_rating,winner_score,loser_score) {

  gamescore = calculate_game_score(winner_score = winner_score, loser_score = loser_score)

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
#' Unlike its counterpart, calculate_game_score_adjusted, it takes team names — not ratings — as inputs.
#' Then, it pulls that team's most updated rating from the rankings site and calculates the results.
#' Returns a dataframe with information.
#' @param winner_team The name of the winning team.
#' @param loser_team The name of the losing team.
#' @param winner_score The score of the winning team.
#' @param loser_score The score of the losing team.
#' @param league_type The league type of the game. Should be equal to "mens" for men's games and "womens" for women's games. Other values will return an error.
#' @keywords
#' @export
#' @examples
#' calculate_game_score_adjusted_team("Virginia","Virginia Tech",13,6,league_type = "mens")

calculate_game_score_adjusted_team = function(winner_team,loser_team,winner_score,loser_score,league_type) {

  if(league_type != "mens" & league_type != "womens") {
    stop("Invalid league type. League type should equal 'mens' for men's games and 'womens' for women's games.")
  }

  if (league_type == "mens") {
    input_data = suppressWarnings(load_rankings_men())
  }

  if (league_type == "womens") {
    input_data = suppressWarnings(load_rankings_women())
  }

  winner_rating = dplyr::pull(input_data[input_data$Team == winner_team,"Rating"])
  loser_rating = dplyr::pull(input_data[input_data$Team == loser_team,"Rating"])

  gamescore = calculate_game_score(winner_score = winner_score, loser_score = loser_score)

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
#' @keywords
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

#' Get pregame win probability
#'
#' This function returns the pregame win probability for a pair of teams, given each team's rating and the league type.
#'
#' @param TeamRating The rating of the first team playing in the game. The order of TeamRating and OpponentRating is interchangeable.
#' @param OpponentRating The rating of the second team playing in the game. The order of TeamRating and OpponentRating is interchangeable.
#' @param LeagueType The league type of the game. Should be equal to "mens" for men's games and "womens" for women's games. Other values will return an error.
#' @importFrom stats predict
#' @keywords
#' @export
#' @examples
#' calculate_win_probability(1500,1400,"mens")
#' calculate_win_probability(1500,1400,"womens")
calculate_win_probability = function(TeamRating,OpponentRating,LeagueType) {

  if(LeagueType != "mens" & LeagueType != "womens") {
    stop("Invalid league type. League type should equal 'mens' for men's games and 'womens' for women's games.")
  }

  predict_df = data.frame(TeamRatingPregame = TeamRating, OpponentRating = OpponentRating)

  if (LeagueType == "mens") {
    pred_value = as.numeric(predict(logitmodelM,predict_df,type = "response"))
  }

  if (LeagueType == "womens") {
    pred_value = as.numeric(predict(logitmodelW,predict_df,type = "response"))
  }

  return(pred_value)
}

#' Load AUDL player statistics
#'
#' Loads AUDL player statistics from the specified season into memory, scraped from the AUDL website.
#'
#' @param season The season for which data is requested. All seasons since 2012 are documented, not including 2020 when the AUDL season was cancelled due to the COVID-19 pandemic.
#' @param stat_type The desired statistic type. Set to "game" by default, for per-game statistics. Can also be set to "total" for total statistics, "10 points" for per-10-point statistics, or "10 possessions" for per-10-possession statistics.
#' @keywords
#' @export
#' @examples
#' load_audl_player_stats(2022)
#' load_audl_player_stats(2021, stat_type = "10 points")

load_audl_player_stats = function(season, stat_type = "game") {
  valid_seasons = c(2012:2019,2021,2022)

  if (season %in% valid_seasons == F) {
    stop("Please enter a valid season: these include all years since 2012, with the exception of 2020.")
  }

  valid_stat_types = c("game","total","10 points","10 possessions")

  if (stat_type %in% valid_stat_types == F) {
    stop("Please enter a valid stat type: 'total' for total stats, 'game' for per-game stats, '10 points' for per-10-point stats, and '10 possessions' for per-10-possession stats.")
  }

  if (stat_type == "total") {
    page = 1
    is_done = F

    base_url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&year=",season)

    df = jsonlite::fromJSON(base_url)$stats

    page = page + 1

    while (is_done == F) {
      url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&year=",season)
      new_df = jsonlite::fromJSON(url)$stats

      if (length(new_df) == 0) {
        is_done = T
      }

      page = page + 1

      df = rbind(df,new_df)
    }
  }

  if (stat_type == "game") {
    page = 1
    is_done = F

    base_url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&per=game&year=",season)

    df = jsonlite::fromJSON(base_url)$stats

    page = page + 1

    while (is_done == F) {
      url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&per=game&year=",season)
      new_df = jsonlite::fromJSON(url)$stats

      if (length(new_df) == 0) {
        is_done = T
      }

      page = page + 1

      df = rbind(df,new_df)
    }
  }

  if (stat_type == "10 points") {
    page = 1
    is_done = F

    base_url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&per=points&year=",season)

    df = jsonlite::fromJSON(base_url)$stats

    page = page + 1

    while (is_done == F) {
      url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&per=points&year=",season)
      new_df = jsonlite::fromJSON(url)$stats

      if (length(new_df) == 0) {
        is_done = T
      }

      page = page + 1

      df = rbind(df,new_df)
    }
  }

  if (stat_type == "10 possessions") {
    page = 1
    is_done = F

    base_url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&per=possessions&year=",season)

    df = jsonlite::fromJSON(base_url)$stats

    page = page + 1

    while (is_done == F) {
      url = paste0("https://www.backend.audlstats.com/web-api/player-stats?limit=20&page=",page,"&per=possessions&year=",season)
      new_df = jsonlite::fromJSON(url)$stats

      if (length(new_df) == 0) {
        is_done = T
      }

      page = page + 1

      df = rbind(df,new_df)
    }
  }

  df = df %>%
    dplyr::mutate(across(c(oEfficiency, huckPercentage, completionPercentage),as.numeric))

  return(df)
}

#' Load AUDL team statistics
#'
#' Loads AUDL team statistics from the specified season into memory, scraped from the AUDL website.
#'
#' @param season The season for which data is requested. All seasons since 2012 are documented, not including 2020 when the AUDL season was cancelled due to the COVID-19 pandemic.
#' @param stat_type The desired statistic type. Set to "game" by default, for per-game statistics. Can also be set to "total" for total statistics.
#' @param team_type Which team the returned statistics should describe. "team" is the default, and will return the statistics for each team. "opponent" will return statistics for opponent performance against each team.
#' @keywords
#' @export
#' @examples
#' load_audl_team_stats(2022)
#' load_audl_team_stats(2022, stat_type = "total", team_type = "opponent")
load_audl_team_stats = function(season, stat_type = "game", team_type = "team") {

  valid_seasons = c(2012:2019,2021,2022)

  if (season %in% valid_seasons == F) {
    stop("Please enter a valid season: these include all years since 2012, with the exception of 2020.")
  }

  valid_stat_types = c("game","total")

  if (stat_type %in% valid_stat_types == F) {
    stop("Please enter a valid stat type: 'game' for per game stats, 'total' for total stats.")
  }

  valid_team_types = c("team","opponent")

  if (team_type %in% valid_team_types == F) {
    stop("Please enter a valid team type: 'team' for team stats, 'opponent' for opponent stats.")
  }

  if (stat_type == "total") {
    if (team_type == "team") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season)
      df = jsonlite::fromJSON(url)$stats
    }

    if (team_type == "opponent") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season,"&opponent=true")
      df = jsonlite::fromJSON(url)$stats
    }

  }

  if (stat_type == "game") {
    if (team_type == "team") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season,"&perGame=true")
      df = jsonlite::fromJSON(url)$stats
    }

    if (team_type == "opponent") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season,"&perGame=true&opponent=true")
      df = jsonlite::fromJSON(url)$stats
    }

  }



  return(df)

}

#' Load AUDL games
#'
#' Loads information on all AUDL games from the specified season into memory, scraped from the AUDL website.
#'
#' @param seasons The seasons for which data is requested. All seasons since 2012 are documented, not including 2020 when the AUDL season was cancelled due to the COVID-19 pandemic.
#' @keywords
#' @export
#' @examples
#' load_audl_games(2022)
#' load_audl_games(c(2019,2021,2022))
#'
load_audl_games = function(seasons) {

  seasons_string = paste(seasons,collapse=",")
  page = 1
  is_done = F

  base_url = paste0("https://www.backend.audlstats.com/web-api/games?limit=10&page=",page,"&years=",seasons_string)

  df = jsonlite::fromJSON(base_url)$games

  page = page + 1

  while (is_done == F) {
    url = paste0("https://www.backend.audlstats.com/web-api/games?limit=10&page=",page,"&years=",seasons_string)
    new_df = jsonlite::fromJSON(url)$games

    if (length(new_df) == 0) {
      is_done = T
    }

    page = page + 1

    df = rbind(df,new_df)
  }

  df = df %>%
    dplyr::mutate(gameDate = as.Date(substr(startTimestamp,1,10), format = "%Y-%m-%d"))

  return(df)
}


