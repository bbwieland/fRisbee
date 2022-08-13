
#' Load AUDL player statistics
#'
#' Loads AUDL player statistics from the specified season into memory, scraped from the AUDL website.
#'
#' @param season The season for which data is requested. All seasons since 2012 are documented, not including 2020 when the AUDL season was cancelled due to the COVID-19 pandemic.
#' @param stat_type The desired statistic type. Set to "game" by default, for per-game statistics. Can also be set to "total" for total statistics, "10 points" for per-10-point statistics, or "10 possessions" for per-10-possession statistics.
#' @export
#' @examples
#' load_audl_player_stats(2022)
#' load_audl_player_stats(2021, stat_type = "10 possessions")

load_audl_player_stats = function(season, stat_type = "game") {

  scores = assists = goals = plusMinus = completions = completionPercentage = hockeyAssists = throwaways = stalls = drops = blocks = callahans = pulls = yardsTotal = yardsThrown = yardsReceived = hucksCompleted = huckPercentage = minutesPlayed = possessions = oEfficiency = pointsPlayed = oPointsPlayed = dPointsPlayed = NULL

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
    df = df %>%
      dplyr::mutate(dplyr::across(c(scores, assists, goals,
                                    plusMinus, completions,
                                    completionPercentage, hockeyAssists,
                                    throwaways, stalls, drops, blocks, callahans,
                                    pulls, yardsTotal, yardsThrown, yardsReceived,
                                    hucksCompleted, huckPercentage,
                                    minutesPlayed, possessions, oEfficiency,
                                    pointsPlayed, oPointsPlayed, dPointsPlayed), as.numeric))
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

    df = df %>%
      dplyr::mutate(dplyr::across(c(scores, assists, goals,
                                    plusMinus, completions,
                                    completionPercentage, hockeyAssists,
                                    throwaways, stalls, drops, blocks, callahans,
                                    pulls, yardsTotal, yardsThrown, yardsReceived,
                                    hucksCompleted, huckPercentage,
                                    minutesPlayed, possessions, oEfficiency,
                                    pointsPlayed, oPointsPlayed, dPointsPlayed), as.numeric))
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
    df = df %>%
      dplyr::mutate(dplyr::across(c(scores, assists, goals,
                                    plusMinus, completions,
                                    completionPercentage, hockeyAssists,
                                    throwaways, stalls, drops, blocks, callahans,
                                    pulls, yardsTotal, yardsThrown, yardsReceived,
                                    hucksCompleted, huckPercentage,
                                    minutesPlayed, possessions, oEfficiency,
                                    pointsPlayed, oPointsPlayed, dPointsPlayed), as.numeric))
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
    df = df %>%
      dplyr::mutate(dplyr::across(c(scores, assists, goals,
                                    plusMinus, completions,
                                    completionPercentage, hockeyAssists,
                                    throwaways, stalls, drops,
                                    yardsTotal, yardsThrown, yardsReceived,
                                    hucksCompleted, huckPercentage,
                                    minutesPlayed, possessions, oEfficiency,
                                    pointsPlayed, oPointsPlayed, dPointsPlayed), as.numeric))
  }

  return(df)

}

#' Load AUDL team statistics
#'
#' Loads AUDL team statistics from the specified season into memory, scraped from the AUDL website.
#'
#' @param season The season for which data is requested. All seasons since 2012 are documented, not including 2020 when the AUDL season was cancelled due to the COVID-19 pandemic.
#' @param stat_type The desired statistic type. Set to "game" by default, for per-game statistics. Can also be set to "total" for total statistics.
#' @param team_type Which team the returned statistics should describe. "team" is the default, and will return the statistics for each team. "opponent" will return statistics for opponent performance against each team.
#' @export
#' @examples
#' load_audl_team_stats(2022)
#' load_audl_team_stats(2022, stat_type = "total", team_type = "opponent")
load_audl_team_stats = function(season, stat_type = "game", team_type = "team") {

  wins = losses = scoresFor = scoresAgainst = completions = turnovers = blocks = completionPercentage = holdPercentage = huckCompletions = huckPercentage = oLineConversionPercentage = dLineConversionPercentage = breakPercentage = redZoneConversionPercentage = opponentCompletions = opponentTurnovers = opponentBlocks = opponentCompletionPercentage = opponentHoldPercentage = opponentOLineConversionPercentage = opponentBreakPercentage = opponentDLineConversionPercentage = opponentHuckCompletions = opponentHuckPercentage = opponentRedZoneConversionPercentage = holds = breaks = huckTurnovers = opponentHolds = opponentBreaks = opponentHuckTurnovers = NULL

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

      df = df %>%
        dplyr::mutate(dplyr::across(c(wins, losses,
                                      scoresFor, scoresAgainst,
                                      completions, turnovers,
                                      blocks, completionPercentage, holdPercentage,
                                      huckCompletions, huckPercentage,
                                      oLineConversionPercentage, dLineConversionPercentage,
                                      breakPercentage, redZoneConversionPercentage),
                                    as.numeric))
    }

    if (team_type == "opponent") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season,"&opponent=true")
      df = jsonlite::fromJSON(url)$stats

      df = df %>%
        dplyr::mutate(dplyr::across(c(wins, losses,
                                      scoresFor, scoresAgainst,
                                      opponentCompletions, opponentTurnovers,
                                      opponentBlocks, opponentCompletionPercentage, opponentHoldPercentage,
                                      opponentOLineConversionPercentage, opponentBreakPercentage,
                                      opponentDLineConversionPercentage, opponentHuckCompletions,
                                      opponentHuckPercentage, opponentRedZoneConversionPercentage),
                                    as.numeric))
    }

  }

  if (stat_type == "game") {
    if (team_type == "team") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season,"&perGame=true")
      df = jsonlite::fromJSON(url)$stats

      df = df %>%
        dplyr::mutate(dplyr::across(c(wins, losses,
                                      scoresFor, scoresAgainst,
                                      completions, turnovers,
                                      blocks, holds, breaks,
                                      huckCompletions, huckTurnovers),
                                    as.numeric))
    }

    if (team_type == "opponent") {
      url = paste0("https://www.backend.audlstats.com/web-api/team-stats?limit=50&year=",season,"&perGame=true&opponent=true")
      df = jsonlite::fromJSON(url)$stats

      df = df %>%
        dplyr::mutate(dplyr::across(c(wins, losses,
                                      scoresFor, scoresAgainst,
                                      opponentCompletions, opponentTurnovers,
                                      opponentBlocks, opponentHolds, opponentBreaks,
                                      opponentHuckCompletions, opponentHuckTurnovers),
                                    as.numeric))
    }

  }



  return(df)

}

#' Load AUDL games
#'
#' Loads information on all AUDL games from the specified season into memory, scraped from the AUDL website.
#'
#' @param seasons The seasons for which data is requested. All seasons since 2012 are documented, not including 2020 when the AUDL season was cancelled due to the COVID-19 pandemic.
#' @export
#' @examples
#' load_audl_games(2022)
#' load_audl_games(c(2019,2021,2022))
#'
load_audl_games = function(seasons) {

  startTimestamp = NULL

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


