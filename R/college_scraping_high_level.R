#' Get updated college men's frisbee rankings
#'
#' This function returns up-to-date ultimate frisbee rankings from frisbee-rankings.com
#' @param DivisionIOnly Returns just D-I rankings if TRUE. Defaults to FALSE.
#' @param SimpleTable Returns just a table with team info & rating if TRUE. Defaults to FALSE.
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
