

#' Fitting CES models
#' 
#' This function fits a CES model
#' @param formula a formula of the form \code{response ~ a + b + c + d + time}.  
#' \code{c} and \code{d} are optional.
#' @param data a data frame, in which to evaluate the formula.
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param a instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param b instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param c instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param d instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually
#' @param nest a permutation (a,b,c,d) of the integers 1 through 4.
#' For models with 3 factors, the nesting
#' is (a + b) + c.  For 4 factors, the nesting is (a + b) + (c + d)
#' @param prevModel a model used to start a gradient search.
#' \code{prevModel} will be used as a starting point for a gradient search after
#' (a) grid search in \code{rho} and \code{rho1} and 
#' (b) a gradient search starting from the best grid search 
#' are complete.
#' #' Use \code{NULL} (the default value) 
#' if you want to skip the gradient search from a previous model.
#' @param multErr logical, tells whether errors are to be assessed in a multiplicative manner
#' @param rho,rho1 Default values for \code{rho} and \code{rho1} are a grid upon which 
#' searches will be made.
#' Note that \code{rho = 0.25} and \code{rho1 = 0.25} are included. These are the default 
#' starting values for \code{rho} and \code{rho1}, so that we don't need to do a fit from 
#' the default values.
#' \code{rho = 0.25} corresponds to \code{sigma = 0.8}.
#' @param digits the number of sse digits that is to be considered significant 
#' when comparing one fit against another.
#' @param save.data a logical indicating whether data is to be saved with the model.
#' Be sure to set \code{TRUE} if resampling is needed later. Default is \code{TRUE}.
#' @param constrained a logical indicating whether the parameters should be constrained in the fitting process.
#' Default is \code{TRUE}.
#' @param fitBoundaries a logical indicating whether fits should be performed along boundaries.
#' Default is \code{TRUE}.
#' @note For now the components in \code{f} (or the arguments \code{response}, 
#' \code{a}, \code{b}, \code{c}, \code{d}, and \code{time}) must correspond to variables in \code{data} and may
#' not be other kinds of expressions.
#' @note For now, this function works for only 2 or 3 factors of production.
#' Setting the value of \code{d} or using \code{f} of the form \code{y ~ a + b + c + d + time}
#' will not work.
#' @return a cesEst model with additional information attached as attributes.
#' @examples
#' if (require("EconData") & require("dplyr")) {
#'   cesModel(iGDP ~ iK + iL + iQp + iYear, data = filter(Calvin, Country=="US"), nest = c(1,2,3))
#'   cesModel(iGDP ~ iK + iL + iQp + iYear, data = filter(Calvin, Country=="US"), nest = c(2,3,1))
#'   cesModel(iGDP ~ iK + iL + iQp + iYear, data = filter(Calvin, Country=="US"), nest = c(3,1,2))
#'   cesModel(iGDP ~ iK + iL + iQp + iYear, data = filter(Calvin, Country=="ZM"), nest = c(3,1,2))
#' }
#' @export
cesModel <- function(formula, data,
                      response,
                      a,
                      b,
                      c=NULL,
                      d=NULL,
                      time,
                      nest=1:3,        # change to 1:4 if we support 4-factor models
                      prevModel=NULL,
                      algorithms=c("PORT","L-BFGS-B"), 
                      multErr=TRUE,
                      rho =c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      rho1=c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      digits=6,
                      save.data=TRUE,
                      constrained=TRUE,
                      fitBoundaries=TRUE,
                      ...){
  if ( missing(formula) ) { 
    substitutionList <-  list( response = substitute(response),
                               capital = substitute(a),
                               labor = substitute(b),
                               energy = substitute(c),
                               other = substitute(d),
                               time = substitute(time)
    )
    if (is.null(c) & is.null(d)) {
      formula <- substitute( response ~ capital + labor + time, substitutionList )
    } else if (is.null(d)) {
      formula <- substitute( response ~ capital + labor + energy + time, substitutionList )
    } else {
      formula <- substitute( response ~ capital + labor + energy + other + time, substitutionList )
    }
  }
    
  fNames <- cesParseFormula(formula, nest)
  # Extract names for convenience.
  cesNames <- fNames$cesNames
  yName <- fNames$yName
  xNames <- fNames$xNames
  tName <- fNames$tName
  numFactors <- fNames$numFactors
  nest <- head(nest, numFactors)
  
  if ( ! numFactors %in% 2L:3L) {
    stop("cesModel() only handles models with 2 or 3 variables.")
  }

  sdata <- data[ , cesNames ]
  if ( ! any( complete.cases(sdata) ) ) {
    stop("No valid rows of data for your model.")
  }
  # This ensures that response is the first column.  This is assumed in downstream code
  # when we save the first column as $response.  This simplifies retrieving it later.
  data <- data[ complete.cases(sdata), c(cesNames, setdiff(names(data), cesNames)) ]
  
  boundary.models <- 
    if (constrained && fitBoundaries){
      cesBoundaryModels(formula, data=data, nest=nest)
    } else {
      list()
    }
  
  # Define lower and upper based on whether constrained fitting is desired.
  if (constrained){
    # Setting lower and upper to NULL invokes the default constraints, 
    # namely 0 <= gamma <= Inf, 0 <= delta <= 1, and -1 <= rho <= Inf
    lower <- NULL
    upper <- NULL
  } else {
    # Setting lower and upper to -Inf and Inf, respectively, removes the constraints.
    lower <- -Inf
    upper <- Inf
  }
  
  cesEst.models <- list()
  
  for (algorithm in algorithms) {
    #
    # Try grid search.
    #
    if (numFactors == 2) {
      # We want a model with only 2 factors. No need for a rho1 argument.
      model <- tryCatch( {
        eval(substitute(
          cesEst(data=data, yName=yNAME, xNames=xNAMES, tName=tNAME, method=ALGORITHM, rho=rho,
                 control = CONTROL, multErr = multErr, 
                 lower = lower, upper = upper, ...), # Always fit unconstrained. Constraints applied later.
          list( yNAME = yName, xNAMES=xNames, tNAME = tName, 
                ALGORITHM=algorithm, CONTROL=chooseCESControl(algorithm) )
        ))
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(2):\n ", as.character(e)))
        return(NULL)
      }
      )
    } else {
      # We want a model with 3 factors. Need a rho1 argument, because we are using a nesting.
      model <- tryCatch( {
        eval(substitute(
          cesEst(data=data, yName=yNAME, xNames=xNAMES, tName=tNAME, method=ALGORITHM, rho=rho, rho1=rho1, 
                 control=CONTROL, multErr=multErr, 
                 lower = lower, upper = upper, ...),
          list( yNAME = yName, xNAMES = xNames, tNAME = tName, 
                ALGORITHM=algorithm, CONTROL=chooseCESControl(algorithm) )   
        ))
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(3):\n ", as.character(e)))
        return(NULL)
      }
      )
    }
    if (! is.null(model) ) {
      model$history <- paste(algorithm, "(grid)", sep="", collapse="|")  
      cesEst.models[length(cesEst.models)+1] <- list(model)
    }
  }
  #
  # Now try gradient search starting from the best place found by the grid searches above.
  #
  bestMod <- bestModel(cesEst.models, digits=digits)
  start <- coef(bestMod)
  
  for (algorithm in algorithms) {
    model <- tryCatch( {
      eval(substitute(
        cesEst(data=data, yName=yNAME, xNames=xNAMES, tName=tNAME, method=ALGORITHM, 
               control=CONTROL, start=start, multErr=multErr, 
               lower = lower, upper = upper, ...),
        list( yNAME = yName, xNAMES = xNames, tNAME = tName, 
              ALGORITHM=algorithm, CONTROL=chooseCESControl(algorithm) ) 
      ))
    },
    error = function(e) { 
      warning(paste("cesEst() failed with", algorithm, "(4):\n ", as.character(e)))
      return(NULL)
    }
    )
    if (! is.null( model ) ) {
      model$history <- paste(algorithm, "[", getHistory(bestMod), "]", collapse="|", sep="")
      cesEst.models[length(cesEst.models)+1] <- list(model)
    }
  }
  #
  # Now try gradient search starting from prevModel (if it is present in the argument list).
  #
  if (! is.null(prevModel)){
    start <- coef(prevModel)
    for (algorithm in algorithms) {
      tryCatch( {
        model <- 
          eval(substitute(
            cesEst(data=data, yName=yNAME, xNames=xNAMES, tName=tNAME, method=ALGORITHM, 
                   control=CONTROL, start=start, multErr=multErr, 
                   lower = lower, upper = upper, ...), 
            list( yNAME = yName, xNAMES = xNames, tNAME = tName, 
                  ALGORITHM=algorithm, CONTROL=chooseCESControl(algorithm) ) 
          ))
        # If there's a problem during fitting, we avoid adding model to models.
        model$history <- paste(algorithm, "[", getHistory(prevModel), ".prev]", sep="", collapse="|")
        cesEst.models[length(cesEst.models)+1] <- list(model)
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(1):\n ", as.character(e)))
        return(NULL)
      }
      )
    }
  }
 
  boundary.models <- Map(function(mod, nm) {mod$bname <- nm; mod}, boundary.models, names(boundary.models)) 
  models <- c(boundary.models, cesEst.models)
  res <- bestModel(models, digits=digits, constrained = constrained)
  
  if ( is.null( res ) ) {
    warning("cesModel() produced a NULL model.")
  } else {
    attr(res, "model.attempts") <- models
    res$formula <- formula
    if (save.data) { res$data <- data }
    res$response <- sdata[,1] # eval( formula[[2]], sdata, parent.frame() )
    res$nest <- nest
    class(res) <- unique(c("cesModel", class(res)))
  }
  return(res)
}

#' Extract information from a CES formula
#' 
#' This function sets up names of parameters for a CES model fit in various formats.
#' In particular, it handles nesting and returns nest strings.
#' @param f the CES fitting formula. 
#' @param nest the nesting for the factors of production.
#' @note \code{f} is assumed to be in the form \code{response ~ a + b + c + d + time}
#' where \code{a}, \code{b}, \code{c}, and \code{d} are factors of production,
#' \code{response} is the response variable (typically economic output), and
#' \code{time} is the time variable.
#' @note \code{nest} is assumed to be integers in the form of \code{c(1,2,3,4)}. 
#' Nest indicates the left-to-right order of parameters in the CES function.
#' @return a list of information extracted from the \code{f} and \code{nest}, including
#' \code{numFactors} (the number of factors of production), 
#' \code{xNames} (a list containing the variable names of the factors of production, 
#' \code{a}, \code{b}, \code{c}, and \code{d}),
#' \code{tName} (the variable name for time),
#' \code{yName} (the variable name for response),
#' \code{nestStr} (a string representing the nest, in the form of \code{k+l+e}),
#' \code{nestStrParen} (a string representing the nest, in the form of \code{(k + l) + (e)}), and
#' \code{cesNames} (a list of variable names in the order they appear in \code{f}, 
#' \code{response}, \code{xNames}, \code{tName}).
#' @export
cesParseFormula <- function(f, nest){
 
  # Set up *Names 
  fNames <- rownames( attr(terms(f), "factors") )
  numFactors <- length(fNames) - 2  # not response, not time
  xNames <- switch( as.character(numFactors),
                    "2" = fNames[1 + nest[1:2]],      # add 1 here to avoid response
                    "3" = fNames[1 + nest[1:3]],
                    "4" = fNames[1 + nest[1:4]]
  )
  yName <- head(fNames, 1)
  tName <- tail(fNames, 1)

  nest <- nest[1:numFactors]
  group1 <- paste(head(xNames, 2), collapse="+")
  group2 <- paste(tail(xNames, -2), collapse="+")
  if (nchar(group2) == 0){
    # Only 2 parameters
    nestStr <- paste0(paste(head(xNames, 2), collapse="+"))
  } else {
    # 3 or more parameters
    nestStr <- paste(paste(head(xNames, 2), collapse="+"), paste(tail(xNames, -2), collapse="+"), sep="+")
  }
  nestStrParen <- paste0(
    "(", 
    paste(head(xNames,2), collapse=" + "),  
    ") + (", 
    paste(tail(xNames, -2), collapse=" + "),
    ")"
  )
  
  cesNames <- c(yName, xNames, tName)
  
  out <- list(numFactors = numFactors, 
              xNames = xNames,
              tName = tName,
              yName = yName,
              nestStr = nestStr,
              nestStrParen = nestStrParen,
              cesNames = cesNames)
  return(out)  
}

#' Tells whether a CES model is within constraints
#' 
#' This function evaluates a fitted CES model for economic meaningfullness as expressed in constraints on 
#' \code{delta_1}, \code{sigma_1} and, if present, \code{delta_2}, \code{sigma_2}, \code{delta}, and \code{sigma}.
#' Specifically, this function checks whether \code{0 <= delta <= 1} and \code{sigma >= 0}
#' @param model the CES model to be evaluated
#' @note Missing, \code{NULL}, and \code{NA} parameters are interpreted as meeting constraints.
#' @note If none of \code{delta_1}, \code{sigma_1}, \code{delta_2}, \code{sigma_2}, \code{delta}, and \code{sigma}
#' is present, \code{FALSE} is returned.
#' @note This function relies upon the presence of the \code{naturalCoeffs} attribute to model.
#' If \code{naturalCoeffs} is \code{NULL}, \code{FALSE} is returned.
#' @return \code{TRUE} if the fitted model is within the constraints, \code{FALSE} if not.
#' @export
withinConstraints <- function(model){
  withinConstraints <- TRUE
  naturalCoeffs <- naturalCoef(model)
  if (is.null(naturalCoeffs)){
    return(FALSE)
  }
  delta_1 <- naturalCoeffs$delta_1
  delta_2 <- naturalCoeffs$delta_2
  delta   <- naturalCoeffs$delta
  sigma_1 <- naturalCoeffs$sigma_1
  sigma_2 <- naturalCoeffs$sigma_2
  sigma   <- naturalCoeffs$sigma
  if ((is.null(delta_1) || is.na(delta_1)) 
      && (is.null(delta_2) || is.na(delta_2))
      && (is.null(delta) || is.na(delta)) 
      && (is.null(sigma_1) || is.na(sigma_1))
      && (is.null(sigma_2) || is.na(sigma_2))
      && (is.null(sigma) || is.na(sigma))){
    # We have no valid fitted parameters
    return(FALSE)
  }
  
  if (!is.null(delta_1) && !is.na(delta_1) && (delta_1 < 0 || delta_1 > 1)){
    return(FALSE)
  }
  if (!is.null(delta_2) && !is.na(delta_2) && (delta_2 < 0 || delta_2 > 1)){
    return(FALSE)
  }
  if (!is.null(delta) && !is.na(delta) && (delta < 0 || delta > 1)){
    return(FALSE)
  }
  
  if (!is.null(sigma_1) && !is.na(sigma_1) && sigma_1 < 0){
    return(FALSE)
  }
  if (!is.null(sigma_2) && !is.na(sigma_2) && sigma_2 < 0){
    return(FALSE)
  }
  if (!is.null(sigma) && !is.na(sigma) && sigma < 0){
    return(FALSE)
  }
  
  # If we get here, we meet constraints for the economically meaningful region of the model
  return(TRUE)
}


chooseCESControl <- function(algorithm){
  ####################
  # This function chooses the CES control parameter
  # based on whether we want PORT or L-BRGS-B.
  ##
  control <- switch(algorithm,
                    "PORT" = list(iter.max=2000, eval.max=2000),
                    "L-BFGS-B" = list(maxit=5000),
                    "LM" = nls.lm.control( maxiter = 1000 ),
                    list()
  )
  return(control)
}