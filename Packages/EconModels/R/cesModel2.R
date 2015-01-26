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
#' because \code{origModel} will be used to obtain the starting point for a gradient search.
#' @param prevModel a model used to start gradient searches.
#' Use \code{NULL} if you want to use the default start locations AND do a grid search 
#' in sigma.
#' @param rho,rho1 Default values for \code{rho} and \code{rho1} are a grid upon which 
#' searches will be made.
#' Note that \code{rho = 0.25} and \code{rho1 = 0.25} are included. These are the default 
#' starting values for \code{rho} and \code{rho1}, so that we don't need to do a fit from 
#' the default values.
#' \code{rho = 0.25} corresponds to \code{sigma = 0.8}.
#' @param digits the number of sse digits that is to be considered significant.
#' @param constrained a logical indicating whether the parameters should be constrained in the fitting process.
#' @param save.data a logical indicating whether data is to be saved with the model.
#' Be sure to set TRUE if resampling is needed later.
#' @note For now the components in \code{formula} (or the arguments \code{response}, 
#' \code{a}, \code{b}, etc. ) must correspond to variables in \code{data} and may
#' not be other kinds of expressions.
#' @return a cesEst model with additional information attached as attributes.
#' @export
cesModel2 <- function(formula, data,
                      response,
                      a,
                      b,
                      c=NULL,
                      d=NULL,
                      time,
                      nest=1:4,
                      prevModel=NULL,
                      algorithms=c("PORT","L-BFGS-B", "LM"), 
                      multErr=TRUE,
                      rho =c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      rho1=c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      digits=6,
                      save.data=TRUE,
                      constrained=TRUE,
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
      numComponents <- 2
    } else if (is.null(d)) {
      formula <- substitute( response ~ capital + labor + energy + time, substitutionList )
      numComponents <- 3
    } else {
      formula <- substitute( response ~ capital + labor + energy + other + time, substitutionList )
      numComponents <- 4
    }
  } else {
    numComponents <- ncol(attr(terms(formula),"factors")) - 1 # subtract off time
  }
  
  # Note: we don't have to verify algorithms. 
  # If constrained fitting is desired, we fit along boundaries with special equations.
  # cesEst will be used in its unconstrained mode always.
 
  # Set up *Names 
  fNames <- rownames( attr(terms(formula), "factors") )
  numFactors <- length(fNames) - 2  # not response, not time
  xNames <- switch( as.character(numFactors),
                    "2" = fNames[1 + (1:2)],      # add 1 here to avoid response
                    "3" = fNames[1 + nest[1:3]],
                    "4" = fNames[1 + nest[1:4]]
  )
  tName <- tail(fNames, 1)
  yName <- head(fNames, 1)
  
  nest <- nest[1:numComponents]
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
  
  # remove incomplete cases since cesEst() fails with incomplete cases.
  cesNames <- c(yName, xNames, tName)
  sdata <- data[ , cesNames ]
  if ( ! any( complete.cases(sdata) ) ) {
    stop("No valid rows of data for your model.")
  }
  # This ensures that response is the first column.  This is assumed in downstream code.
  data <- data[ complete.cases(sdata), c(cesNames, setdiff(names(data), cesNames)) ]
  
  #  sdata <- subset(data, select = all.vars(formula))
  #  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  models <- list()
  for (algorithm in algorithms) {
    #
    # Try grid search.
    #
    if (numFactors == 2) {
      # We want a model with only 2 factors. No need for a rho1 argument.
      model <- tryCatch( {
        eval(substitute(
          cesEst(data = DATA, yName = yNAME, xNames = xNAMES, 
                 tName = tNAME, method=ALGORITHM, rho = RHO,
                 control = CONTROL, multErr = MULTERR, 
                 lower = LOWER, upper = UPPER, ...),
          list(DATA = data, yNAME = yName, xNAMES=xNames,
               tNAME = tName, ALGORITHM = algorithm, RHO = rho,
               CONTROL = chooseCESControl(algorithm), MULTERR = multErr,
               LOWER = -Inf, UPPER = Inf) # Always fit unconstrained. Constraints applied later.
        ))
        # cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
        #       rho=rho, control=chooseCESControl(algorithm), multErr=multErr, ...)
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
          cesEst(data=DATA, yName=yNAME, xNames=xNAMES, 
                 tName=tNAME, method=ALGORITHM, rho = RHO, rho1 = RHO1, 
                 control=CONTROL, multErr=MULTERR, 
                 lower = LOWER, upper = UPPER, ...),
          list( DATA = data, yNAME = yName, xNAMES = xNames, 
                tNAME = tName, ALGORITHM = algorithm, RHO = rho, RHO1 = rho1, 
                CONTROL = chooseCESControl(algorithm), MULTERR=multErr,
                LOWER = -Inf, UPPER = Inf) # Always fit unconstrained. Constraints applied later.
        ))
        #cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
        #       rho=rho, rho1=rho1, control=chooseCESControl(algorithm), multErr=multErr, ...)
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(3):\n ", as.character(e)))
        return(NULL)
      }
      )
    }
    if (! is.null(model) ) {
      hist <- paste(algorithm, "(grid)", sep="", collapse="|")  
      model <- addMetaData(model, nest=nest, nestStr=nestStr, nestStrParen=nestStrParen, history=hist)
      models[length(models)+1] <- list(model)
    }
  }
  #
  # Now try gradient search starting from the best place found by the grid searches above.
  #
  bestMod <- bestModel(models, digits=digits)
  start <- coef(bestMod)
  
  for (algorithm in algorithms) {
    model <- tryCatch( {
      eval(substitute(
        cesEst(data=data, yName=yNAME, xNames=xNAMES, tName=tNAME, method=ALGORITHM, 
               control=CONTROL, start=START, multErr=MULTERR, 
               lower = LOWER, upper = UPPER, ...),
        list( yNAME = yName, xNAMES = xNames, tNAME = tName, ALGORITHM = algorithm,
              CONTROL = chooseCESControl(algorithm), START=start, MULTERR=multErr, 
              LOWER = -Inf, UPPER = Inf) # Always fit unconstrained. Constraints applied later.
      ))
      #      cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
      #             control=chooseCESControl(algorithm), start=start, multErr=multErr, ...)
    },
    error = function(e) { 
      warning(paste("cesEst() failed with", algorithm, "(4):\n ", as.character(e)))
      return(NULL)
    }
    )
    if (! is.null( model ) ) {
      hist <- paste(algorithm, "[", getHistory(bestMod), "]", collapse="|", sep="")
      model <- addMetaData(model, nest=nest, nestStr=nestStr, nestStrParen=nestStrParen, history=hist)
      models[length(models)+1] <- list(model)
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
                   control=CONTROL, start=START, multErr=MULTERR, 
                   lower = LOWER, upper = UPPER, ...),
            list( yNAME = yName, xNAMES = xNames, tNAME = tName, ALGORITHM = algorithm,
                  CONTROL =  chooseCESControl(algorithm), START=start, MULTERR=multErr, 
                  LOWER = -Inf, UPPER = Inf) # Always fit unconstrained. Constraints applied later.
          ))
        # If there's a problem during fitting, we avoid adding model to models.
        hist <- paste(algorithm, "[", getHistory(prevModel), ".prev]", sep="", collapse="|")
        model <- addMetaData(model, nest=nest, nestStr=nestStr, nestStrParen=nestStrParen, history=hist)
        models[length(models)+1] <- list(model)
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(1):\n ", as.character(e)))
        return(NULL)
      }
      )
    }
  }
  
  res <- bestModel(models, digits=digits)
  if ( is.null( res ) ) {
    warning("cesModel() produced a NULL model.")
  } else {
    attr(res, "model.attempts") <- models
    attr(res, "formula") <- formula
    if (save.data) { attr(res, "data") <- data }
    attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  }
  
  return(res)
}