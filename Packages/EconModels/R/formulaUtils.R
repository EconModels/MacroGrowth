#' Utilities for working with formulas
#' 
#' Some functions to make it easier to build and minipulate formulas

#' @rdname formula.utils
#' @param lhs,rhs list or vector of character strings containing summands for left and right hand sides
#'   of a formula.  \code{NULL}'s are ignored.
#' @param formula a formula
#' @param n a vector of integer indices.  Negatives can be used to deselect.
#' @param env an environment to attach to the formula
#' @param term a character string containing a new term
#' @param op The operator to use for splitting into terms.
#' @param right a logical indicating whether to operate on the right side (else the left) of 
#'   the formula.
#' @export
#' @examples
#' a <- build_formula(c("a", "b"), c("x", "y", "log(x*y)")); a
#' b <- build_formula("response", list("x", "y", NULL, "log(x)"), op="*"); b
#' f <- build_formula( "iGDP", list("iK", "iL", "iQp", "iYear")); f
#' g <- build_formula( "iGDP", c("iK", "iL", "iQp", "iYear")); g
#' identical(f,g)
#' summands(a)
#' summands(a, right=FALSE)
#' summands(b, op="*")
#' keep_summands(f, 3)
#' keep_summands(f, -3)
#' keep_summands(f, 2:4)
#' h <- replace_summand( f, 3, paste(summands(f, 3), lhs(f), sep="/")); h
 
build_formula <- function( lhs, rhs, env=parent.frame(), op="+" ) {
  lhs <- lhs[sapply(lhs, function(x) !is.null(x))]
  rhs <- rhs[sapply(rhs, function(x) !is.null(x))]
  res <- as.formula(
          paste(
            paste(lhs,  collapse = op), 
            paste(rhs, collapse = op), 
            sep="~")
    )
  environment(res) <- env
  res
}

# drop term n from the RHS of a formula
# this isn't very robust if "terms" have + in them.  But for simple things it will work.

#' @rdname formula.utils
#' @export
#' 
keep_summands <- function(formula, n, right=TRUE, env=environment(formula), op="+") {
  formulaString <- deparse(formula)
  formulaSplit <- strsplit(formulaString, " ~ ")[[1]]
  l <- strsplit( formulaSplit[1], paste0(" ", op, " "), fixed=TRUE)[[1]]
  r <- strsplit( formulaSplit[2], paste0(" ", op, " "), fixed=TRUE)[[1]]
  if (right) {
    r <- r[n] 
  } else { 
    l <- l[n]
  }
  build_formula( l, r, env=env)
}

#' @rdname formula.utils
#' @export
#' 
summands <- function(formula, n, right=TRUE, op="+") {
  formulaString <- deparse(formula)
  formulaSplit <- strsplit(formulaString, " ~ ")[[1]]
  strsplit( formulaSplit[1 + as.numeric(right)], paste0(" ", op, " "), fixed=TRUE)[[1]][n]
  }

#' @rdname formula.utils
#' @export
#' 
replace_summand <- function(formula, n, term, right=TRUE, op="+", env=environment(formula)) {
  formulaString <- deparse(formula)
  formulaSplit <- strsplit(formulaString, " ~ ")[[1]]
  l <- strsplit( formulaSplit[1], paste0(" ", op, " "), fixed=TRUE)[[1]]
  r <- strsplit( formulaSplit[2], paste0(" ", op, " "), fixed=TRUE)[[1]]
  if (right) {
    r[n] <- term
  } else {
    l[n] <- term
  }
  build_formula( l, r, env=env)
}



# CDformulas <- 
#   list( 
#     PT = c(unadjusted = iGDP ~ iKstkS.L        + iL     + iXpMP + iYear,
#            adjusted = iGDP ~ iKservS.L       + ihLest + iUMP  + iYear),
#     UK = c(unadjusted = iGDP ~ iKserv0.WwithRD + iL     + iXp   + iYear,
#            adjusted = iGDP ~ iKserv0.WwithRD + ihLest + iU    + iYear)
#   )
# 
# foo3 <- function(country=NULL, 
#                  flavor = c("unadjusted", "adjusted"), 
#                  include.energy = c(FALSE, TRUE),
#                  formulas = CDformulas){
#   
#   country <- numeric_choices(country, names(formulas))
#   
#   if (length(country) < 1) stop("No valid countries specified.")
#   
#   res <- list()  
#   for (c in names(country)) { 
#     for (f in flavor) {
#       for (ie in include.energy) {
#         new <- formulas[[c]][[f]]
#         if (!ie) new <- keep_rhs(new, -4)
#         res[[paste(c,f, if (ie) "energy" else "noenergy", sep=":")]] <- new
#       }
#     }
#   }
#   res
# }

# foo3(country=1, include.energy=FALSE, flavor="adjusted")
# foo3()
# f <- iGDP ~ iK + iL + iQp + iYear; f
# g <- replace_rhs_summand( f, 3, paste(rhs_summands(f, 3), lhs(f), sep="/")); g
# cdModel(g, data=subset(Calvin, Country=="UK"))
# 
# 

numeric_choices <- function(names_or_numbers, choices) {
  if (is.null(names_or_numbers)) { names_or_numbers <- choices }
  if (is.numeric(names_or_numbers)) {
    res <- names_or_numbers[names_or_numbers <= length(choices)]
  } else {
    res <- which(choices %in% names_or_numbers)
  }
  names(res) <- choices[res]
  res
}
