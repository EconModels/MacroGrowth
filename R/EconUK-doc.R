#' UK Economic Data
#'
#' Macroeconomic data for the UK from 1960 through 2010.
#'
#' @docType data
#' @format
#' A data frame with the following variables
#'
#' * `Year` Year
#' * `iYear` Indexed year (starting from 0)
#' * `iGDP`  Indexed GDP (scaled so value is 1 in 1960)
#' * `iK`    Indexed capital stock (scaled so value is 1 in 1960)
#' * `iL`    Indexed labor (scaled so value is 1 in 1960)
#' * `iXp`   Indexed primary exergy (scaled so value is 1 in 1960)
#' * `iXu`   Indexed useful exergy (scaled so value is 1 in 1960)
#'
#' @source This is a subset of the data use in
#' Heun, M.K.; Santos, J.; Brockway, P.E.; Pruim, R.; Domingos, T.; Sakai, M.
#' "From Theory to Econometrics to Energy Policy: Cautionary Tales for Policymaking Using Aggregate Production Functions".
#' *Energies* 2017, 10, 203.
#' @references
#' <doi://10.3390/en10020203>

"EconUK"
