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
#' @param rho,rho1 Default values for \code{rho} and \code{rho1} are a grid upon which 
#' searches will be made.
#' Note that \code{rho = 0.25} and \code{rho1 = 0.25} are included. These are the default 
#' starting values for \code{rho} and \code{rho1}, so that we don't need to do a fit from 
#' the default values.
#' \code{rho = 0.25} corresponds to \code{sigma = 0.8}.
#' @param digits the number of sse digits that is to be considered significant 
#' when comparing one fit against another.
#' @param constrained a logical indicating whether the parameters should be constrained in the fitting process.
#' @param save.data a logical indicating whether data is to be saved with the model.
#' Be sure to set \code{TRUE} if resampling is needed later.
#' @note For now the components in \code{formula} (or the arguments \code{response}, 
#' \code{a}, \code{b}, \code{c}, \code{d}, and \code{time}) must correspond to variables in \code{data} and may
#' not be other kinds of expressions.
#' @note For now, this function works for only 2 or 3 factors of production.
#' Setting the value of \code{d} or using a formula of the form \code{y ~ a + b + c + d + time}
#' will not work.
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
    } else if (is.null(d)) {
      formula <- substitute( response ~ capital + labor + energy + time, substitutionList )
    } else {
      formula <- substitute( response ~ capital + labor + energy + other + time, substitutionList )
    }
  }
    
  fNames <- cesFormulaNames(formula, nest)
  # Extract names for convenience.
  cesNames <- fNames$cesNames
  yName <- fNames$yName
  xNames <- fNames$xNames
  tName <- fNames$tName

  sdata <- data[ , cesNames ]
  if ( ! any( complete.cases(sdata) ) ) {
    stop("No valid rows of data for your model.")
  }
  # This ensures that response is the first column.  This is assumed in downstream code.
  data <- data[ complete.cases(sdata), c(cesNames, setdiff(names(data), cesNames)) ]
  
  #
  # If we are fitting constrained, do fits along all constraints.
  #
  if (constrained){
    # Calculate new combinations of variables.
    # Note that xNames contains the variable names for the factors of production 
    # in the left-to-right order that they appear in the CES function.
    x1Name <- xNames[[1]]
    x1 <- eval(substitute(data$colx1, list(colx1 = x1Name)))
    x2Name <- xNames[[2]]
    x2 <- eval(substitute(data$colx2, list(colx2 = x2Name)))
    minx1x2 <- pmin(x1, x2)
    if (numFactors >= 3){
      x3Name <- xNames[[3]]
      x3 <- eval(substitute(data$colx3, list(colx3 = x3Name)))
      minx1x3 <- pmin(x1, x3)
      minx2x3 <- pmin(x2, x3)
      minx1x2x3 <- pmin(x1, x2, x3)
    }
    if (numFactors == 4){
      # Full support for 4 factors of production has not been included in this function.
      x4Name <- xNames[[4]]
      x4 <- eval(substitute(data$colx4, list(colx4 = x4Name)))
    }
    y <- eval(substitute(data$y, list(y = yName)))
    time <- eval(substitute(data$time, list(time = tName)))
  }  
  # Fit model 1: y = gamma * A * x1 as ln(y/x1) = ln(gamma) + lambda*time
  # For model 1, the constraints are delta_1 = 1, and delta = 1.
  # rho_1 (sigma_1) and rho (sigma) are unknowable.
  mod1 <- lm(log(y/x1) ~ time)
  mod1 <- addMetaData(mod1, nest, )
  naturalCoeffs <- data.frame(
    gamma_coef = as.vector(exp(mod1$coefficients[[1]])),
    lambda = as.vector(mod1$coefficients[[2]]),
    delta_1 = as.vector(1),
    delta = as.vector(1),
    sigma_1 = NA,
    rho_1 = NA, 
    sigma = NA,
    rho = NA,
    sse = as.vector(sum(resid(mod1)^2))
  )
  attr(mod1, "naturalCoeffs") <- naturalCoeffs
  
  
  models <- list()
  for (algorithm in algorithms) {
    #
    # Try grid search.
    #
    if (numFactors == 2) {
      # We want a model with only 2 factors. No need for a rho1 argument.
      model <- tryCatch( {
        eval(substitute(
          cesEst(data = data, yName = yNAME, xNames = xNAMES, 
                 tName = tNAME, method=ALGORITHM, rho = RHO,
                 control = CONTROL, multErr = MULTERR, 
                 lower = -Inf, upper = Inf, ...), # Always fit unconstrained. Constraints applied later.
          list(yNAME = yName, xNAMES=xNames,
               tNAME = tName, ALGORITHM = algorithm, RHO = rho,
               CONTROL = chooseCESControl(algorithm), MULTERR = multErr) 
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
          cesEst(data=data, yName=yNAME, xNames=xNAMES, 
                 tName=tNAME, method=ALGORITHM, rho = RHO, rho1 = RHO1, 
                 control=CONTROL, multErr=MULTERR, 
                 lower = -Inf, upper = Inf, ...), # Always fit unconstrained. Constraints applied later.
          list( yNAME = yName, xNAMES = xNames, 
                tNAME = tName, ALGORITHM = algorithm, RHO = rho, RHO1 = rho1, 
                CONTROL = chooseCESControl(algorithm), MULTERR=multErr) 
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
               lower = -Inf, upper = Inf, ...), # Always fit unconstrained. Constraints applied later.
        list( yNAME = yName, xNAMES = xNames, tNAME = tName, ALGORITHM = algorithm,
              CONTROL = chooseCESControl(algorithm), START=start, MULTERR=multErr) 
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
                   lower = -Inf, upper = Inf, ...), # Always fit unconstrained. Constraints applied later.
            list( yNAME = yName, xNAMES = xNames, tNAME = tName, ALGORITHM = algorithm,
                  CONTROL =  chooseCESControl(algorithm), START=start, MULTERR=multErr) 
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

#' Extract CES variable names from a CES formula
#' 
#' This function sets up names of parameters for a CES model fit in various formats.
#' In particular, it handles nesting and returns nest strings.
#' @param formula the CES fitting formula. 
#' @param nest the nesting for the factors of production.
#' @note \code{formula} is assumed to be in the form \code{response ~ a + b + c + d + time}
#' where \code{a}, \code{b}, \code{c}, and \code{d} are factors of production,
#' \code{response} is the response variable (typically economic output), and
#' \code{time} is the time variable.
#' @note \code{nest} is assumed to be integers in the form of \code{c(1,2,3,4)}. 
#' Nest indicates the left-to-right order of parameters in the CES function.
#' @return a list of information extracted from the \code{formula} and \code{nest}, including
#' \code{numFactors} (the number of factors of production), 
#' \code{xNames} (a list containing the variable names of the factors of production, 
#' \code{a}, \code{b}, \code{c}, and \code{d}),
#' \code{tName} (the variable name for time),
#' \code{yName} (the variable name for response),
#' \code{nestStr} (a string representing the nest, in the form of \code{k+l+e}),
#' \code{nestStrParen} (a string representing the nest, in the form of \code{(k + l) + (e)}), and
#' \code{cesNames} (a list of variable names in the order they appear in the formula, 
#' \code{response}, \code{xNames}, \code{tName}).
#' @export
cesFormulaNames <- function(formula, nest){
  
  # Set up *Names 
  fNames <- rownames( attr(terms(formula), "factors") )
  numFactors <- length(fNames) - 2  # not response, not time
#   if (numFactors >= 4){
#     stop("4 factors of prodcution not supported in cesModel at this time.")
#   }
  xNames <- switch( as.character(numFactors),
                    "2" = fNames[1 + nest[1:2]],      # add 1 here to avoid response
                    "3" = fNames[1 + nest[1:3]],
                    "4" = fNames[1 + nest[1:4]]
  )
  tName <- tail(fNames, 1)
  yName <- head(fNames, 1)
  
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
  
  # remove incomplete cases because cesEst() fails with incomplete cases.
  cesNames <- c(yName, xNames, tName)
  
  out <- list(numFactors = numFactors, 
              xNames = xNames,
              tName = tName,
              yName = yName,
              nestStr = nestStr,
              nestStrParen = nestStrParen,
              cesNames)
  return(out)  
}