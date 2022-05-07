# fRisbee: For college ultimate frisbee

fRisbee is an R package designed to make interfacting with college
frisbee team rankings straightforward & easy.

Install the package using the install github function.

    devtools::install_github("https://github.com/bbwieland/fRisbee",upgrade = "ask",force = T)
    library(fRisbee)

    head(fRisbee::GetFrisbeeRankings())

    ## Warning in ifelse(is.na(as.numeric(substr(Team, nchar(Team) - 1,
    ## nchar(Team)))), : NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## # A tibble: 6 × 11
    ##    Rank RegionRank Team    Record WinPct Rating Region Conference Division   SoS
    ##   <dbl> <chr>      <chr>   <chr>   <dbl>  <dbl> <fct>  <chr>      <fct>    <dbl>
    ## 1     1 NE 1       Brown   27-1    0.964  2224. New E… Greater N… D-I      1843.
    ## 2     2 AC 1       North … 30-3    0.909  2173. Atlan… Carolina … D-I      1789.
    ## 3     3 SC 1       Colora… 25-1    0.962  2138. South… Rocky Mou… D-I      1725.
    ## 4     4 NW 1       Brigha… 22-1    0.957  2064. North… Big Sky DI D-I      1582.
    ## 5     5 NE 2       Massac… 18-2    0.9    2053. New E… Greater N… D-I      1753.
    ## 6     6 NW 2       Washin… 11-0    1      2027. North… Cascadia … D-I      1634.
    ## # … with 1 more variable: PDC <dbl>
