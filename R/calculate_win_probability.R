
#' Get pregame win probability
#'
#' This function returns the pregame win probability for a pair of teams, given each team's rating and the league type.
#'
#' @param TeamRating The rating of the first team playing in the game. The order of TeamRating and OpponentRating is interchangeable.
#' @param OpponentRating The rating of the second team playing in the game. The order of TeamRating and OpponentRating is interchangeable.
#' @param LeagueType The league type of the game. Should be equal to "mens" for men's games and "womens" for women's games. Other values will return an error.
#' @importFrom stats predict
#' @export
#' @examples
#' calculate_win_probability(1500,1400,"mens")
#' calculate_win_probability(1500,1400,"womens")
calculate_win_probability = function(TeamRating,OpponentRating,LeagueType) {

  logitmodelM = logitmodelM
  logitmodelW = logitmodelW

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
