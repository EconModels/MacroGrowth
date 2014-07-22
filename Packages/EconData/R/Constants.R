# List of countries in our data set.
#' @export
countryAbbrevs <- c(US="US", UK="UK", JP="JP", AT="AT", CN="CN", ZA="ZA", SA="SA", IR="IR", TZ="TZ", ZM="ZM")
# Energy types and factors
#' @export
energyTypes <- c(Q="iQ", X="iX", U="iU") 
#' @export
energyLevels <- c(none="none", energyTypes)
#' @export
factors <- c(K="iK", L="iL", energyTypes)

# Preferred order for nestStr
#' @export
nestStrLevels <- c("iK", "iL", "iQ", "iX", "iU", 
                   "iK+iL", 
                   "iK+iL+iQ", "iK+iL+iX", "iK+iL+iU",
                   "iL+iQ+iK", "iL+iX+iK", "iL+iU+iK",
                   "iQ+iK+iL", "iX+iK+iL", "iU+iK+iL")

# Preferred order for nestStrParen
#' @export
nestStrParenLevels <- c("(iK) + ()", "(iL) + ()", "(iQ) + ()", "(iX) + ()" , "(iU) + ()",
                        "(iK + iL) + ()", 
                        "(iK + iL) + (iQ)", "(iK + iL) + (iX)", "(iK + iL) + (iU)",
                        "(iL + iQ) + (iK)", "(iL + iX) + (iK)", "(iL + iU) + (iK)",
                        "(iQ + iK) + (iL)", "(iX + iK) + (iL)", "(iU + iK) + (iL)")
