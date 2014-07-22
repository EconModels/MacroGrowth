
#' Creates an string representation of the factors of production
#' 
#' @param formula the formula used for fitting. 
#' The energy variable is extracted from the formula.
#' @param nest an integer vector containing 2 or 3 values. 
#' For 2-value vectors, integers must be 1 or 2.
#' For 3-value vectors, integers must be 1, 2, or 3.
#' @param sep the separator for the factorString
#' @param showNestParens tells whether to show parentheses in the factor string if a nest is present
#' @details Factors of production are ordered according to the nest, if present.
#' In the nest argument, the integer 1 indicates the capital stock variable ("iK"),
#' the integer 2 indicates the labor variable ("iL"), and
#' the integer 3 indicates the energy variable (one of "iQ", "iX", or "iU")
#' Nesting positions are given by location in the \code{nest} vector, if present.
#' c(3,1,2) is interpreted as energy and capital stock nested together. 
#' For example, (iX + iK) + (iL).
#' If the \code{nest} vector is not present, factors of production 
#' are returned in the order they appear in the \code{formula}.
#' If showNestParens=FALSE, parentheses are not included in the factor string, e.g., \code{iX+iK+iL}.
#' If showNestParens=TRUE and a nest argument is provided, 
#' parentheses are include in the factor string, e.g., \code{(iX + iK) + (iL)} or \code{(iL) + ()}.
#' @return a string representing the nesting of the form \code{iQ+iK+iL} or \code{(iQ + iK) + (iL)}, etc.
#' An empty string if a nest is not involved.
#' @export
factorString <- function( formula, nest, sep="+", showNestParens=FALSE ) {
  if (is.character(formula)){
    formula <- eval(parse(text=formula))
  }
  matches <- na.omit(match(x=all.vars(formula), table=factors))
  factorsPresent <- factors[matches]
  noNest <- missing(nest) || is.null(nest) || is.na(nest) || (nest=="") || ( is.list(nest) && (length(nest)==0)) 
  if (noNest){
    # Simply return the factors of production in the order they appear in the formula.
    out <- paste(factorsPresent, collapse=sep)
  } else {
    # We have a nest.
    if (is.list(nest)){
      # nest is a list. Grab the "nest" item and see if it is of integer type
      if ( is.integer(nest[["nest"]]) ){
        nest <- nest[["nest"]]
      } else {
        # give up
        stop(paste("Unknown nest =", nest, "in factorString"))
      }
    }
    if (length(nest) != length(factorsPresent)){
      # This is a problem. Need to have as many nest items as factors of production.
      stop(paste("length(nest) =", length(nest), 
                 "and length(factorsPresent) =", length(factorsPresent), 
                 ". They should be equal."))
    }
    if (length(nest) == 1){
      # Only one factor of production. Return it.
      out <- factorsPresent[[1]]
    } else {
      # Rearrange the factors in the nested order.
      orderedFactors <- factorsPresent[nest]
      out <- paste(orderedFactors, collapse=sep)
    }
  }
  if (showNestParens){
    if (noNest==TRUE){
      # Set the nest to be the order of variables supplied
      nest <- c(1:length(factorsPresent))
    }
    # We have a nest and we want to show parentheses.
    pieces <- strsplit(out, split=sep, fixed=TRUE)[[1]] # fixed=TRUE is needed as the default sep is "+"
    out <- paste0(
      "(", 
      paste(head(pieces, 2), collapse=" + "),  
      ") + (", 
      paste(tail(pieces, -2), collapse=" + "),
      ")"
    )
  }
  return(out)
}

#' Parse a factor string
#' 
#' @param factorString a string of the form \code{"iK+iL+iU"}. 
#' @param sep the separator in \code{factorString}
#' @param rVar the name of the response variable
#' @param kVar the name of the capital stock variable
#' @param lVar the name of the labor variable
#' @param tVar the name of the time variable
#' @return a list with several elements. See details.
#' @details White space is removed from \code{factorString} before parsing.
#' In the return value, \code{formula} is a formula object with factors of production
#' in the correct order. 
#' \code{nest} is an integer vector whose elements indicate nesting of the 
#' factors of production. For example, \code{c(3,1,2)} means that the 3rd and 1st factors of production 
#' in the formula are nested together and the 2nd factor of production 
#' is independent. 
#' \code{factor} is NA if there is more than one factor of production. 
#' If only one factor of production, factor is set to the single factor.
#' \code{energyType} is the energy type in this factorString, if present.
#' NA otherwise.
#' \code{nestStr} a nest string without nesting parentheses.
#' \code{nestStrParen} a nest string with nesting parentheses, useful for pretty-printing.
#' @export
parseFactorString <- function(factorString, sep="+", rVar="iGDP", kVar=factors[["K"]], lVar=factors[["L"]], tVar="iYear"){
  factorString <- gsub(pattern=" ", replacement="", x=factorString) # Remove spaces
  factorString <- gsub(pattern="\\(", replacement="", x=factorString) # Remove parens
  factorString <- gsub(pattern="\\)", replacement="", x=factorString) # Remove parens
  factorString <- gsub(pattern="\\+$", replacement="", x=factorString) # Remove trailing "+" if present
  energyType <- extractEnergyType(factorString)
  if (! grepl(pattern=sep, x=factorString, fixed=TRUE)){
    # The factorString doesn't contain sep. Assume there is only one variable.
    # We have a single-factor model
    formula <- paste(rVar, "~", factorString, "+", tVar)
    nest <- c(1)
    factor <- factorString
  } else {
    vars <- unlist(strsplit(x=factorString, split=sep, fixed=TRUE))
    nVars <- length(vars)
    if (nVars == 2) {
      # We have a 2-factor model with K and L
      formula <- paste(rVar, "~", kVar, "+", lVar, "+", tVar)
      nest <- match(vars, c(kVar, lVar))
      factor <- NA
    } else if (nVars == 3){
      # We have an energy variable and three factors
      eVar <- energyTypes[[na.omit(match(x=vars, table=energyTypes))]]
      formula <- paste(rVar, "~", kVar, "+", lVar, "+", eVar, "+", tVar)
      nest <- match(vars, c(kVar, lVar, eVar) )
      factor <- NA
    } else {
      stop("Don't know what to do with 4 or more factors of production in parseFactorString")
    }
  }
  formula <- eval(parse(text=formula))
  # Create string representations
  nestStr <- factorString(formula, nest=nest, sep=sep, showNestParens=FALSE)
  nestStrParen <- factorString(formula, nest=nest, sep=sep, showNestParens=TRUE)
  # Create a list and return.
  return(list(formula=formula, nest=nest, factor=factor, energyType=energyType, nestStr=nestStr, nestStrParen=nestStrParen))
}

#' Creates an id for this run of resampling
#' 
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A 2- or 3-vector of integers.
#' @param nestStr if used, the string for the nesting
#' @param n the number of resamples being attempted
#' @param sep the separator used to create the id string. Default is " : ".
#' @return a string to be used as the id for this resample
#' @export
fittingID <- function(fun, countryAbbrev, formula, nest=NULL, n, sep=" : "){
  id <- paste(countryAbbrev, fun, formula, factorString(formula=formula, nest=nest), n, sep=sep)
  return(id)
}

#' File name for resample coefficients for the given parameters
#' 
#' @param prefix a prefix for the file name
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A vector of 2 or 3 integers.
#' @param sep the separator used to create the id string. Default is "_".
#' @return a string representing the file name for these resample coefficients.
resampleFileName <- function(prefix, fun, countryAbbrev, formula, nest=NULL, sep="_"){
  # Strip "Model" out of fun, if present
  modelType <- sub(pattern="Model", replacement="", x=fun)
  f <- paste(prefix, countryAbbrev, modelType, factorString(formula=formula, nest=nest), sep=sep)
  f <- paste0(f, ".Rdata")
  return(f)
}

#' Path to resample coefficients for the given parameters
#' 
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A vector of 2 or 3 integers.
#' @param sep the separator used to create the id string. Default is "_".
#' @param prefix a prefix for the file name. Should not include the separator.
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return a string representing the file name for these resample coefficients.
#' @export
resampleFilePath <- function(fun, countryAbbrev, formula, nest=NULL, prefix, sep="_", baseResample){
  f <- resampleFileName(prefix=prefix, fun=fun, countryAbbrev=countryAbbrev, formula=formula, nest=nest, sep=sep)
  path <- file.path(baseResample, f)
  return(path)
}

#' Extracts an energyType from a formula or a factorString.
#' 
#' @param x a formula or factorString from which you want to extract an energyType
#' @param energyTypes the types of energy available
#' @param sep the separator for the formula or factorString
#' @return a string representing the energyType that was used for the fitting. NA if no energy type was found.
extractEnergyType <- function(x, eTypes=energyTypes, sep="+"){
  if (is.character(x)){
    # Try to coerce to a formula
    if (grepl(pattern="~", x=x, fixed=TRUE)){
      x <- eval(parse(text=x))
    }
  }
  if (inherits(x ,"formula")) {
    # Split into a bunch of strings
    vars <- all.vars(x)
  } else if (is.character(x)) {
    # Split at the separator
    vars <- unlist(strsplit(x=x, split=sep, fixed=TRUE))
  }
  # Find the energyType
  matches <- na.omit(match(x=vars, table=eTypes))
  if (length(matches) <= 0){
    eType <- NA
  } else {
    eType <- energyTypes[[matches]]
  }
  return(eType)
}