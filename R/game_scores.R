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
