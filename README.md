
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fRisbee

<!-- badges: start -->
<!-- badges: end -->

The goal of fRisbee is to …

## Installation

You can install the released version of fRisbee from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fRisbee")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bbwieland/fRisbee")
```

## Example: Evaluating AUDL team performance

This is a basic example which shows you how to solve a common problem:

``` r
library(fRisbee)

team_stats = fRisbee::load_audl_team_stats(season = 2022, stat_type = "game")
str(team_stats)
#> 'data.frame':    25 obs. of  14 variables:
#>  $ teamID         : chr  "empire" "flyers" "summit" "union" ...
#>  $ teamName       : chr  "Empire" "Flyers" "Summit" "Union" ...
#>  $ gamesPlayed    : int  12 12 12 12 12 12 12 12 12 12 ...
#>  $ wins           : num  1 0.92 0.92 0.92 0.83 0.83 0.75 0.75 0.75 0.67 ...
#>  $ losses         : num  0 0.08 0.08 0.08 0.17 0.17 0.25 0.25 0.25 0.33 ...
#>  $ scoresFor      : num  25.1 22.1 24.5 24.3 23.9 ...
#>  $ scoresAgainst  : num  16.8 17.1 19.2 19.1 20 ...
#>  $ completions    : num  262 259 246 246 268 ...
#>  $ turnovers      : num  13.5 13.7 15.2 15.8 14.2 ...
#>  $ blocks         : num  11.5 9.33 11.17 9.58 10.25 ...
#>  $ holds          : num  15 15.5 16 16.3 16.4 ...
#>  $ breaks         : num  9.92 6.42 8.5 8 7.33 8.75 5.08 8.08 9.33 6.92 ...
#>  $ huckCompletions: num  7.42 8.58 10.17 7.33 6.58 ...
#>  $ huckTurnovers  : num  3.25 4.75 5 3.42 3.83 4.42 2.92 4.83 4.83 2.83 ...
```

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# creating a 'net rating' variable:

team_stats = team_stats %>%
  dplyr::mutate(netRating = scoresFor - scoresAgainst)

# attaching the dataframe of team logos, glossary_AUDL_teams

team_stats = team_stats %>%
  dplyr::left_join(fRisbee::glossary_AUDL_teams, by = "teamName")
```

``` r
library(ggplot2)

ggplot2::ggplot(team_stats, aes(x = netRating, y= reorder(teamName, netRating))) +
  geom_col() +
  labs(x = "Net Rating", y = "Team",
       title = "AUDL Net Ratings - 2022 Season") +
  theme_bw()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

``` r
library(ggimage)
#> Bioconductor version '3.12' is out-of-date; the current release version '3.15'
#>   is available with R version '4.2'; see https://bioconductor.org/install

ggplot2::ggplot(team_stats, aes(x = scoresFor, y = scoresAgainst)) +
  geom_image(aes(image = logoURL), asp = 1)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
