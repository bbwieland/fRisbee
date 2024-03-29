% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/game_scores.R
\name{calculate_game_score_adjusted}
\alias{calculate_game_score_adjusted}
\title{Calculate team rating change from a game result}
\usage{
calculate_game_score_adjusted(
  winner_rating,
  loser_rating,
  winner_score,
  loser_score
)
}
\arguments{
\item{winner_rating}{The rating of the winning team.}

\item{loser_rating}{The rating of the losing team.}

\item{winner_score}{The score of the winning team.}

\item{loser_score}{The score of the losing team.}
}
\description{
This function calculates game score using the USA Ultimate Rankings Algorithm.
Then, it determines the ratings changes as a result of that game outcome.
Returns a dataframe with information.
}
\examples{
calculate_game_score_adjusted(1500,1500,13,6)
}
