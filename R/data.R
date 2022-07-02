#' Game-level data for the college men's 2021-22 season.
#'
#' A dataset containing information on each of the 6,752 documented men's ultimate frisbee games played at the collegiate level in the 2021-22 season.
#'
#' @format A data frame with 6752 rows and 21 variables:
#' \describe{
#'   \item{OppRk}{Current national ranking of opponent, per Frisbee Rankings.}
#'   \item{Opponent}{Name of opposing team.}
#'   \item{Result}{Whether the team won or lost the game, as well as the final score, in character format.}
#'   \item{Effect}{Effect of the game on the team's Frisbee Rankings rank.}
#'   \item{Status}{Factor representing whether a game was included or excluded in ranking calculation, with 2 levels: Counts and Ignored.}
#'   \item{PctOfRanking}{The percentage of a team's total ranking that is accounted for by the given game's outcome.}
#'   \item{Date}{The date on which the game was played.}
#'   \item{Event}{The event at which the game was played.}
#'   \item{Win}{A binary indicator of whether the team won or lost: 1 if win, 0 if loss. Useful for modeling.}
#'   \item{Pts}{The number of points scored by the team.}
#'   \item{OppPts}{The number of points scored by their opponent.}
#'   \item{GameScore}{The game score for the team, as calculated by the fRisbee::GameScoreCalculator function.}
#'   \item{PtDiff}{The number of points scored by the team minus the number of points scored by their opponent}
#'   \item{OpponentRating}{The opponent team's Elo rating on Frisbee Rankings.}
#'   \item{TeamRatingPostgame}{The team's Elo rating after playing the game.}
#'   \item{GameValue}{The implied Elo rating that the team played at in the game.}
#'   \item{TeamRatingPregame}{The team's Elo rating before playing the game.}
#'   \item{GaneValueUsed}{Equal to GameValue where Status is Counts. 0 where Status is Ignored.}
#'   \item{RatingsImpact}{The impact of the game result on a team's Elo rating.}
#'   \item{Team}{The name of the team.}
#'   \item{GameNum}{The game number in the season; for example, GameNum = 1 for a team's first game of the year.}
#' }
#' @source \url{https://www.frisbee-rankings.com/}
"gamesM"

#' Game-level data for the college women's 2021-22 season.
#'
#'A dataset containing information on each of the 4,220 documented women's ultimate frisbee games played at the collegiate level in the 2021-22 season.
#'
#' @format A data frame with 4220 rows and 21 variables:
#' \describe{
#'   \item{OppRk}{Current national ranking of opponent, per Frisbee Rankings.}
#'   \item{Opponent}{Name of opposing team.}
#'   \item{Result}{Whether the team won or lost the game, as well as the final score, in character format.}
#'   \item{Effect}{Effect of the game on the team's Frisbee Rankings rank.}
#'   \item{Status}{Factor representing whether a game was included or excluded in ranking calculation, with 2 levels: Counts and Ignored.}
#'   \item{PctOfRanking}{The percentage of a team's total ranking that is accounted for by the given game's outcome.}
#'   \item{Date}{The date on which the game was played.}
#'   \item{Event}{The event at which the game was played.}
#'   \item{Win}{A binary indicator of whether the team won or lost: 1 if win, 0 if loss. Useful for modeling.}
#'   \item{Pts}{The number of points scored by the team.}
#'   \item{OppPts}{The number of points scored by their opponent.}
#'   \item{GameScore}{The game score for the team, as calculated by the fRisbee::GameScoreCalculator function.}
#'   \item{PtDiff}{The number of points scored by the team minus the number of points scored by their opponent}
#'   \item{OpponentRating}{The opponent team's Elo rating on Frisbee Rankings.}
#'   \item{TeamRatingPostgame}{The team's Elo rating after playing the game.}
#'   \item{GameValue}{The implied Elo rating that the team played at in the game.}
#'   \item{TeamRatingPregame}{The team's Elo rating before playing the game.}
#'   \item{GaneValueUsed}{Equal to GameValue where Status is Counts. 0 where Status is Ignored.}
#'   \item{RatingsImpact}{The impact of the game result on a team's Elo rating.}
#'   \item{Team}{The name of the team.}
#'   \item{GameNum}{The game number in the season; for example, GameNum = 1 for a team's first game of the year.}
#' }
#' @source \url{https://www.frisbee-rankings.com/}
"gamesW"

#' Win probability model for men's college Ultimate Frisbee
#'
#' A win probability model fit in base R using logistic regression to the modelM dataset with one feature: Elo difference.
#'
#' @format An R generalized linear model object.
#' @source \url{https://www.frisbee-rankings.com/}
"logitmodelM"

#' Win probability model for women's college Ultimate Frisbee
#'
#' A win probability model fit in base R using logistic regression to the modelW dataset with one feature: Elo difference.
#'
#' @format An R generalized linear model object.
#' @source \url{https://www.frisbee-rankings.com/}
"logitmodelW"
