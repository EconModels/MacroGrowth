# List of countries in our data set.
#' @export
countryAbbrevs <- c(US="US", UK="UK", JP="JP", AT="AT", CN="CN", ZA="ZA", SA="SA", IR="IR", TZ="TZ", ZM="ZM")
# Energy types and factors
#' @export
# energyTypes <- c(Q="iQ", X="iX", U="iU") 
energyTypes <- c(Qp="iQp", Xp="iXp", Qf="iQf", Xf="iXf, U="iU") 
#' @export
energyLevels <- c(none="none", energyTypes)
#' @export
factors <- c(K="iK", L="iL", energyTypes)

# Preferred order for nestStr
#' @export
# nestStrLevels <- c("iK", "iL", "iQ", "iX", "iU", 
#                    "iK+iL", 
#                    "iK+iL+iQ", "iK+iL+iX", "iK+iL+iU",
#                    "iL+iQ+iK", "iL+iX+iK", "iL+iU+iK",
#                    "iQ+iK+iL", "iX+iK+iL", "iU+iK+iL")
nestStrLevels <- c("iK", "iL", "iQp", "iXp", "iQf", "iXf", "iU", 
                   "iK+iL", 
                   "iK+iL+iQp", "iK+iL+iXp", "iK+iL+iQf", "iK+iL+iXf", "iK+iL+iU",
                   "iL+iQp+iK", "iL+iXp+iK", "iL+iQf+iK", "iL+iXF+iK", "iL+iU+iK",
                   "iQp+iK+iL", "iXp+iK+iL", "iQf+iK+iL", "iXf+iK+iL", "iU+iK+iL")

# Preferred order for nestStrParen
#' @export
# nestStrParenLevels <- c("(iK) + ()", "(iL) + ()", "(iQ) + ()", "(iX) + ()" , "(iU) + ()",
#                         "(iK + iL) + ()", 
#                         "(iK + iL) + (iQ)", "(iK + iL) + (iX)", "(iK + iL) + (iU)",
#                         "(iL + iQ) + (iK)", "(iL + iX) + (iK)", "(iL + iU) + (iK)",
#                         "(iQ + iK) + (iL)", "(iX + iK) + (iL)", "(iU + iK) + (iL)")
nestStrParenLevels <- c("(iK) + ()", 
                        "(iL) + ()", 
                        "(iQp) + ()", 
                        "(iXp) + ()", 
                        "(iQf) + ()", 
                        "(iXf) + ()", 
                        "(iU) + ()",
                        "(iK + iL) + ()", 
                        "(iK + iL) + (iQp)", "(iK + iL) + (iXp)", 
                        "(iK + iL) + (iQf)", "(iK + iL) + (iXf)", "(iK + iL) + (iU)",
                        "(iL + iQp) + (iK)", "(iL + iXp) + (iK)", 
                        "(iL + iQf) + (iK)", "(iL + iXf) + (iK)", "(iL + iU) + (iK)",
                        "(iQp + iK) + (iL)", "(iXp + iK) + (iL)", 
                        "(iQf + iK) + (iL)", "(iXf + iK) + (iL)", "(iU + iK) + (iL)")

#' Relevels a factor based on given levels
#' 
#' @param x a factor to be releveled
#' @param levs desired order for the levels of factor \code{x}.
#' @details The length of \code{levs} may be longer or shorter than the number of levels in \code{x}.
#' If lengths are different, only \code{levs} that are also factors of \code{x}
#' are applied. 
#' Levels of factor \code{x} that do not appear in \code{levs} are ignored;
#' they will appear \emph{after} \code{levs} in the levels of the object returned from this function.
#' If none of \code{levs} are in the levels of \code{x}, an unmodified version of \code{x}
#' will be returned.
#' @return an object containing same data as \code{x} whose levels have been reordered according to \code{levs}.
#' @export
relevelFactor <- function(x, levs){
  if (! is.factor(x)){
    stop("x must be a factor.")
  }
  availableLevels <- levs[levs %in% levels(x)]
  for (lev in rev(availableLevels)) { 
    x <- relevel(x, ref=lev) 
  }
  return(x)
}