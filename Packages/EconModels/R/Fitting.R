

nestMatch <- function( n1, n2 ) {
  (length(n1) == length(n2)) && all(n1==n2)
}

#' @export
naturalCoef <- function(object) {
  if (! "naturalCoeffs" %in% names(attributes(object)) ) return(as.data.frame(matrix(nrow=1, ncol=0)))
  return( attr(object, "naturalCoeffs") )
}

#' @export
metaData <- function(object) {
  if (! "meta" %in% names(attributes(object)) ) return(as.data.frame(matrix(nrow=1, ncol=0)))
  return( attr(object, "meta") )
}

#' @export
bestModel <- function(models, digits=6, orderOnly=FALSE) {
  ###################
  # Extracts the best model (least sse) from a list of models
  ##
  # Note that the order function below preserves the original order in the event of ties.
  o <- order(sapply( models, function(model) { round(sum(resid(model)^2), digits=digits) } ) )
  if (orderOnly) return(o)
  out  <- models[[ o[1] ]] 
  return(out) 
}

#' Compute fitted values on natural scale
#' 
#' This is similar to \code{fitted}, but will invert logarithmic 
#' transformations of the response variable for certain models (e.g., LINEX, and Cobb-Douglas 
#' models fit in this package).
#' 
#' @param object An object returned by a fitting function.
#' @export
yhat <- function(object, ...) {
  UseMethod('yhat')
}

# This works for cesEst objects because fitted() retunrs fits on the natural scale.
#' @export
yhat.default <- function(object,...) {
  fitted(object,...)
}

#' @export
yhat.CDEmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
  exp( fitted(object,...) + lx0[!is.na(lx0)] )
}

#' @export
yhat.LINEXmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
  exp( fitted(object, ...) + lx0[!is.na(lx0)] )
}

#' @export
yhat.sfModel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
  exp( fitted(object, ...) + lx0[!is.na(lx0)] )
}

#' Return response values from original data used to fit a model
#' 
#' This function returns the values of the original response variable
#' The values are calculated from fits and residuals.
#' For models of class \code{"LINEXmodel"}
#' \code{"CDEmodel"} or \code{"cesEst"}, the logarthmic transformation, if it 
#' was used, will be undone, returning the values to their natural scale.
#' @param object a model object from one of the model fitting functions.
#' @param ... additional arguments
#' @return a numeric vector
#' @export

response <- function(object, ...) {
  UseMethod('response') 
}

#' @export
response.default <- function(object, ...) {
  if (inherits( object,  "LINEXmodel" ) ||
        inherits( object, "CDEmodel" ) ) {
    return( attr(object, "response") )
  }
  return( fitted(object) + resid(object) )
}

#' @export
predict.CDEmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
  exp( NextMethod() + lx0[!is.na(lx0)] )
}

#' @export
predict.LINEXmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
  exp( NextMethod() + lx0[!is.na(lx0)] )
}


#' Fitting single factor models
#' 
#' @param formula a formula of the form \code{ y ~ factor + time }
#' @param data a data frame in which \code{formula} is evaluated
#' @param constrained a logical indicating whether the model parameters are constrained
#' @return an lm object with some additional attributes
#' @export
sfModel <- function(formula, data, response, factor, time, constrained=FALSE,
                              save.data=TRUE) {
  
  if ( missing(formula) ) {
    formula <- substitute( response ~ factor + time,
                           list( response = substitute(response),
                                 factor = substitute(factor),
                                 time = substitute(time)
                           )
    ) 
  }
  formulas <- list( log(y) ~ x + time,
                    log(y) - x ~ time)
  formulas <- lapply( formulas,
                      function(x) do.call(substitute, 
                                          list(x,
                                               list( y = formula[[2]],
                                                     x = formula[[3]][[2]],
                                                     time = formula[[3]][[3]]
                                               )
                                          )
                      )
  )
  # Build the additional object to add as an atrribute to the output
  if (constrained){
    res <- lm( formulas[[2]], data=data )
    m <- 1.0
    logscale <- coef(res)[1]
    lambda <- coef(res)[2]
  } else {
    res <- lm( formulas[[1]], data=data )
    logscale <- coef(res)[1]
    m <- coef(res)[2]
    lambda <- as.vector(coef(res)[3])
  }
  naturalCoeffs <- data.frame(
    logscale=as.vector(logscale),
    scale=exp(as.vector(logscale)),
    lambda = as.vector(lambda),
    m = as.vector(m),
    sse = sum(resid(res)^2),
    isConv = TRUE
  )
  
  attr(res, "naturalCoeffs") <- naturalCoeffs
  
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  if (save.data) { attr(res, "data") <- sdata }
  attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  attr(res, "formula") <- formula
  
  class(res) <- c("sfModel", class(res))
  return(res)
  
}

#' Fitting Cobb-Douglas models
#' 
#' @param formala a formula of the form 
#' \code{response ~ capital + labor + time} or 
#' \code{response ~ capital + labor + energy + time}
#' @param data a data frame in which \code{formula} is evaluated
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param capital instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param labor instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param energy instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param constrained a logical indicating whether the parameters are constrained
#' @return a CDEmodel object, which is an lm object with some additioanl attributes.
#' @export
cdModel <- function(formula, data, response, capital, labor, energy, time, 
                             constrained=FALSE, save.data=TRUE, ...) {
  if (missing(formula)) {
    if (missing(energy)) {
      return( cdwoeModel( data=data, responses=response, capital=capital, labor=labor,
                        time=time, constrained = constrained, save.data=save.data, ...) )
    } else {
      return( cdeModel( data=data, responses=response, capital=capital, labor=labor,
                        energy=energy,
                        time=time, constrained = constrained, save.data=save.data, ...) )
    }
  }
  
  if (ncol( attr(terms(formula),"factors") ) == 3 ) {
    # formula contains response, capital, and labor
    cdwoeModel( formula=formula, data=data, constrained = constrained, save.data=save.data, ... )
  } else {
    # formula contains response, capital, labor, and energy
    cdeModel( formula=formula, data=data, constrained = constrained, save.data=save.data, ... )
  }
}

#' Fitting Cobb-Douglas Models
#' 
#' @param formala a formual of the form \code{response ~ capital + labor + time}
#' @param data a data fram in which \code{formula} is evaluated
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param capital instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param labor instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param constrained a logical indicating whether the parameters are constrained
#' @return a CDEmodel object, which is an lm object with some additional attributes.
cdwoeModel <- function(formula, data, response, capital, labor, time, constrained=FALSE, 
                    save.data=TRUE, ...) {
  if ( missing(formula) ) {
    formula <- substitute( response ~ capital + labor + time,
                           list( response = substitute(response),
                                 capital = substitute(capital),
                                 labor = substitute(labor),
                                 time = substitute(time)
                           )
    )
  }
  
  sdata <- subset(data, select= all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  
  formulas <- list(
    log(y) - log(labor) ~ I(log(capital) - log(labor)) + time,
    log(y) - log(capital) ~ time,  
    log(y) - log(labor) ~ time
    )
  
  formulas <- lapply( formulas,
                      function(x) do.call(substitute, 
                                          list(x,
                                               list( y = formula[[2]],
                                                     capital = formula[[3]][[2]][[2]],
                                                     labor = formula[[3]][[2]][[3]],
                                                     time = formula[[3]][[3]]
                                               )
                                          )
                      )
  )
  res <- lm(formulas[[1]], data=sdata)
  # Build the additional object to add as an atrribute to the output
  # could try this, but it is a hack.
  # model$coefficients <- c(m$cofficients, 1 - m$coeffcients[3]) 
  names(res$coefficients) <- c( "logscale", "lambda", "alpha")
  alpha <- coef(res)["alpha"]
  if (constrained){
    if (alpha < 0.0 || alpha > 1.0){
      # Need to adjust alpha, because we are beyond 0.0 or 1.0
      if (alpha < 0.0){
        alpha <- 0.0
        res <- lm( formulas[[2]], data=sdata )
      } else {
        alpha <- 1.0
        res <- lm( formulas[[3]], data=sdata )
      }
      # Refit for lambda only
      names(res$coefficients) <- c("logscale", "lambda")
    }
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(res)["lambda"]),
                              logscale = as.vector(coef(res)["logscale"]),
                              scale = exp(as.vector(coef(res)["logscale"])),
                              alpha = as.vector(alpha),
                              beta = as.vector(1.0 - alpha),
                              gamma = 0.0, # Energy is not a factor for this model.
                              sse = sum(resid(res)^2),
                              isConv = TRUE  # res$convInfo$isConv
  )
  attr(res, "naturalCoeffs") <- naturalCoeffs

  if (save.data) {
    attr(res, "data") <- sdata
  }
  attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  attr(res, "formula") <- formula
  
  class(res) <- c("CDEmodel", class(res))
  return(res)
}

respectsConstraints <- function( model ) {
  # Tells whether we're fitting within the constraints for a Cobb-Douglas model.
  ##
  cf <- coef(model)
  if( length(cf) >=2 ) cf <- cf[-c(1,2)] 
  all( cf >= 0 ) & ( sum(cf) <=1 )
}



#' Fitting Cobb-Douglas models with Energy
#' 
#' @param formula a formula of the form \code{response ~ capital + labor + energy + time}
#' @param data a data frame in which \code{formala} is evaluated
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param capital instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param labor instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param energy instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param constrained a logical indicated whether the coefficents are constrained. See details
#' @param \dots additional arguments; currently unused.
#' @details More about contranints TBA.

# y ~ capital + labor + energy + time
cdeModel <- function( formula, data, response, capital, labor, energy, time, 
                      constrained=FALSE, save.data=TRUE, ...){
  
  if ( missing(formula) ) {
    formula <- substitute( response ~ capital + labor + energy + time,
                           list( response = substitute(response),
                                 capital = substitute(capital),
                                 labor = substitute(labor),
                                 energy = substitute(energy),
                                 time = substitute(time)
                           )
    )
  }
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  formulas <- list( 
    log(y) - log(energy) ~ 
      I(log(capital) - log(energy)) + I(log(labor) - log(energy)) + time,  
    
    log(y) - log(energy) ~ I(log(capital) - log(energy)) + time,
    log(y) - log(energy) ~ I(log(labor)  - log(energy)) + time,
    log(y) - log(labor)  ~ I(log(capital) - log(labor)) + time,
    
    log(y) - log(capital) ~ time,
    log(y) - log(labor)  ~ time,
    log(y) - log(energy) ~ time 
  )
  
  formulas <- lapply(formulas, function(x) do.call( substitute, list( x,  list(
    time = formula[[3]][[3]],
    energy = formula[[3]][[2]][[3]],
    labor = formula[[3]][[2]][[2]][[3]],
    capital = formula[[3]][[2]][[2]][[2]],
    y = formula[[2]]  
  ) 
  ) ) )
  
  coefNames <- list( 
    c("logscale", "lambda", "alpha", "beta"),
    
    c("logscale", "lambda", "alpha"),
    c("logscale", "lambda", "beta"),
    c("logscale", "lambda", "alpha"),
    
    c("logscale", "lambda"),
    c("logscale", "lambda"),
    c("logscale", "lambda")
  )
  models <- lapply( formulas, function(form)  lm( form, data=sdata )  )
  sse <- sapply( models, function(m) sum( resid(m)^2 ) )
  if ( constrained ) {
    good <- sapply( models, respectsConstraints)
    good[ !good ] <- NA
  } else { 
    good <- TRUE
  }
  winner <- which.min( sse * good )
  res <- models[[winner]]
  names( res$coefficients ) <- coefNames[[winner]]
  
  # Build the additional object to add as an atrribute to the output
  cf <- coef(res)
  naturalCoeffs <- data.frame(
    logscale= cf["logscale"],
    scale = exp( cf["logscale"] ),
    sse = sum(resid(res)^2),
    isConv = TRUE,
    winner = winner,
    lambda = cf["lambda"]
  )
  naturalCoeffs$alpha <- switch( as.character(winner), 
                                 "1" = cf['alpha'],
                                 "2" = cf['alpha'],
                                 "4" = cf['alpha'],
                                 "5" = 1,
                                 0
  ) 
  
  naturalCoeffs$beta <- switch( as.character(winner),
                                "1" = cf['beta'],
                                "3" = cf['beta'],
                                "4" = 1 - cf['alpha'],
                                "6" = 1,
                                0 )
  
  
  naturalCoeffs$gamma <- switch( as.character(winner),
                                 "1" = 1 - cf['alpha'] - cf['beta'],
                                 "2" = 1 - cf['alpha'],
                                 "3" = 1 - cf['beta'],
                                 "7" = 1,
                                 0 )
  
  attr(res, "naturalCoeffs") <- naturalCoeffs
  attr(res, "good") <-  sapply( models, respectsConstraints )
  attr(res, "sse") <-  sse
  attr(res, "winner") <-  winner
  attr(res, "formula") <-  formula
  if (save.data) { attr(res, "data") <- sdata }
  attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  
  class(res) <- c( "CDEmodel", class(res) )
  return(res)
}

#' Fitting CES models
#' 
#' This function fits a CES model
#' @param formula a formula of the form \code{response ~ a + b + c + d + time}.  
#' \code{c} and \code{d} are optional.
#' @param data a data frame, in which to evaluate the formula.
#' #' @param formula a formula of the form \code{response ~ captial + labor + energy + time}
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
#' @note For now the components in \code{formula} (or the arguments \code{response}, 
#' \code{a}, \code{b}, etc. ) must correspond to variables in \code{data} and may
#' not be other kinds of expressions.
#' @return a cesEst model with additional information attached as attributes.
#' @export
cesModel <- function(formula, data,
                     response,
                     a,
                     b,
                     c=NULL,
                     d=NULL,
                     time,
                     nest=1:4,
                     prevModel=NULL,
                     algorithms=c("PORT","L-BFGS-B"), 
                     multErr=TRUE,
                     rho =c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                     rho1=c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                     digits=6,
                     save.data=TRUE,
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
  
  # Verify algorithm
  # These are the only valid algs that respect constraints
  cesAlgorithms <- c("PORT", "L-BFGS-B") 
  algorithms <- toupper(algorithms)
  badAlgorithms <- setdiff(algorithms, cesAlgorithms)
  algorithms <- intersect(algorithms, cesAlgorithms)
  for (m in badAlgorithms) {
    stop(paste("Unrecognized algorithm:", m))
  }  
  
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
  nestString <- paste0(
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
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               rho=rho, control=chooseCESControl(algorithm), multErr=multErr, ...)
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(2):\n ", as.character(e)))
        return(NULL)
      }
      )
    } else {
      # We want a model with 3 factors. Need a rho1 argument, because we are using a nesting.
      model <- tryCatch( {
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               rho=rho, rho1=rho1, control=chooseCESControl(algorithm), multErr=multErr, ...)
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(3):\n ", as.character(e)))
        return(NULL)
      }
      )
    }
    if (! is.null(model) ) {
      hist <- paste(algorithm, "(grid)", sep="", collapse="|")  
      model <- addMetaData(model, nest=nest, nestString=nestString, history=hist)
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
      cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
             control=chooseCESControl(algorithm), start=start, multErr=multErr, ...)
    },
    error = function(e) { 
      warning(paste("cesEst() failed with", algorithm, "(4):\n ", as.character(e)))
      return(NULL)
    }
    )
  }
  if (! is.null( model ) ) {
    hist <- paste(algorithm, "[", getHistory(bestMod), "]", collapse="|", sep="")
    model <- addMetaData(model, nest=nest, nestString=nestString, history=hist)
    models[length(models)+1] <- list(model)
  }
  #
  # Now try gradient search starting from prevModel (if it is present in the argument list).
  #
  if (! is.null(prevModel)){
    start <- coef(prevModel)
    for (algorithm in algorithms) {
      tryCatch( {
        model <- cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
                        control=chooseCESControl(algorithm), start=start, multErr=multErr, ...)
        # If there's a problem during fitting, we avoid adding model to models.
        hist <- paste(algorithm, "[", getHistory(prevModel), ".prev]", sep="", collapse="|")
        model <- addMetaData(model, nest=nest, nestString=nestString, history=hist)
        models[length(models)+1] <- list(model)
      },
      error = function(e) {  
        warning(paste("cesEst() failed with ", algorithm, "(1):\n ", as.character(e)))
        return(NULL)
      }
      )
    }
  }
  
  res <- bestModel(models)
  if ( is.null( res ) ) {
    warning("cesModel() produced a NULL model.")
  } else {
    attr(res, "model.attempts") <- models
    attr(res, "formula") <- formula
    if (save.data) { attr(res, "data") <- data }
    attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
#    res <- addMetaData(res, nest=nest, nestString=nestString, history=hist)
  }
  
  return(res)
}

#' Add meta data to CES model object
#'
#'  
#' This function adds metadata to a model.  Currently this is only designed to
#' work with CES models. Metadata is attached as attributes (naturalCoeffs and meta)
#' to the object and the new object is returned from the function.
#' 
addMetaData <- function(model, nest, nestString, history=""){
  if (is.null(model)){
    return(model) 
  }
  
  if ( ! as.character(model$call[[1]]) == "cesEst" ){
    stop("Unsupported model type.  Must be NULL or the result of calling cesEst()")
  }
  
  grid <- length( intersect(c("rho", "rho1"), names(model$call) ) ) > 0
  
  # We may be arriving here with a model that was estimated wihthout energy.
  # If that is the case, we will have only rho and sigma parameters, not
  # rho_1 and delta_1 parameters. 
  # Test for the without energy model.
  withoutEnergy <- is.na(coef(model)["rho_1"]) || is.na(coef(model)["delta_1"])
  
  if (withoutEnergy){
    # The coefficient representing the split between k and l 
    # is given by delta in the model. But, in our calculations, 
    # we're defining the split between k and l and delta_1.
    # So, reassign here.
    delta_1 <- coef(model)["delta"]
    # The without-energy model has delta <- 1
    delta <- 1
    # The coefficient representing the substitutability between k and l
    # is given by rho in the model argument. But, in our calculations,
    # we're defining that substitutability as rho_1.
    # So, reassign here.
    rho_1 <- coef(model)["rho"]
    # The no-energy situation is tantamount to saying that there is
    # infinite substitutability between (kl) and e. 
    # So, assign the value of rho to be -1 (sigma = Inf).
    rho <- -1
  } else {
    # This is the no-energy situation. Things are more straightforward.
    delta_1 <- coef(model)["delta_1"]
    delta <- coef(model)["delta"]
    rho_1 <- coef(model)["rho_1"]
    rho <- coef(model)["rho"]
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(model)["lambda"]),
                              delta_1 = as.vector(delta_1),
                              rho_1 = as.vector(rho_1),
                              sigma_1 = as.vector(1 / (1 + rho_1)),
                              # Variable name collision alert: there is a gamma coefficient
                              # in the CES model (gamma_coef) and a gamma calculated 
                              # from the delta values.
                              # gamma_coef is the coefficient in the CES model. It should be near 1.0.
                              # gamma is calculated from the delta values in the model.
                              # gamma is analogous to the gamma exponent on energy in the Cobb-Douglas model.
                              # And, gamma is the required name of the variable to be plotted with the ternary 
                              # plot function standardTriPlot.  (standardTriPlot assumes that one variable 
                              # is named "gamma", and it plots that variable.)  
                              # gamma_coef is in the naturalCoeffs attribute.
                              # gamma is in the meta attribute.
                              gamma_coef = as.vector(coef(model)["gamma"]),
                              delta = as.vector(delta),
                              rho = as.vector(rho),
                              sigma = as.vector(1 / (1 + rho)),
                              sse = sum(resid(model)^2)
  )
  # Calculate some metadata, including gamma. See comments above.
  if (missing(nest) || is.na(nest) || nestMatch( nest, 1:2) ) {
    alpha <- delta_1
    beta <- 1.0 - delta_1
    gamma <- 0.0
  } else if ( nestMatch(nest, 1:3) ) { # (nest == "(kl)e"){
    alpha <- delta * delta_1
    beta  <- delta * (1.0 - delta_1)
    gamma <- 1.0 - delta
  } else if ( nestMatch(nest, c(2,3,1) ) ) { # (nest == "(le)k"){
    beta <- delta * delta_1
    gamma <- delta * (1.0 - delta_1)
    alpha <- 1.0 - delta
  } else if ( nestMatch(nest, c(1,3,2) ) ) { # (nest == "(ke)l"){
    alpha <- delta * delta_1
    gamma <- delta * (1.0 - delta_1)
    beta <- 1.0 - delta
  } else if ( nestMatch(nest, c(2,1,3) ) ) { # (nest == "(lk)e"){
    beta <- delta * delta_1
    alpha <- delta * (1.0 - delta_1)
    gamma <- 1.0 - delta
  } else if ( nestMatch(nest, c(3,2,1) ) ) { # (nest == "(el)k"){
    gamma <- delta * delta_1
    beta <- delta * (1.0 - delta_1)
    alpha <- 1.0 - delta
  } else if ( nestMatch(nest, c(3,1,2) ) ) { # (nest == "(ek)l"){
    gamma <- delta * delta_1
    alpha <- delta * (1.0 - delta_1)
    beta <- 1.0 - delta
  } else {
    stop(paste("Unknown nest:", nestString, "in addMetaData."))
  }
  metaData <- data.frame( isConv = model$convergence,
                          algorithm = model$method,
                          #                          iter = as.vector(model["iter"]),
                          grid = grid,
                          alpha = as.vector(alpha),
                          beta = as.vector(beta),
                          gamma = as.vector(gamma),
                          start.lambda = as.vector(model$start["lambda"]),
                          start.delta_1 = as.vector(model$start["delta_1"]),
                          start.rho_1 = as.vector(model$start["rho_1"]),
                          start.gamma_coef = as.vector(model$start["gamma"]),
                          start.delta = as.vector(model$start["delta"]),
                          start.rho = as.vector(model$start["rho"]),
                          history=history,
                          nest = nestString
  )
  
  metaList <- list(  isConv = model$convergence,
                     algorithm = model$method,
                     iter = as.vector(model$iter),
                     grid = grid,
                     alpha = as.vector(alpha),
                     beta = as.vector(beta),
                     gamma = as.vector(gamma),
                     start.lambda = as.vector(model$start["lambda"]),
                     start.delta_1 = as.vector(model$start["delta_1"]),
                     start.rho_1 = as.vector(model$start["rho_1"]),
                     start.gamma_coef = as.vector(model$start["gamma"]),
                     start.delta = as.vector(model$start["delta"]),
                     start.rho = as.vector(model$start["rho"]),
                     history=history,
                     nest=nestString
  )
  
  if ( nrow(metaData) > 1 ) {
    warning( paste0("\nmeta data has ", nrow(metaData), " rows: ", paste(nest,history, sep="|")) )
    for (item in metaList) { 
      if ( length(item) > 1 ) {
        warning(paste0("\t", toString(item)))
      }
    }
  }
  attr(model, "naturalCoeffs") <- naturalCoeffs[1,]
  metaData$metaDataRows <- nrow(metaData)
  attr(model, "meta") <- metaData[1,] 
  attr(model, "metaList") <- metaList 
  
  return(model)
}


#' Fitting LINEX models
#' 
#' @param formula a formula of the form \code{response ~ captial + labor + energy + time}
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param capital instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param labor instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param energy instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param data a data frame in which \code{formula} is evaluated
#' @export
#' 
linexModel <- function(formula, data, response, capital, labor, energy, time, save.data=TRUE) {
  ####################
  # Returns an nls linex model for the country and energyType specified.
  # 
  
  if ( missing(formula) ) {
    formula <- substitute( response ~ capital + labor + energy + time,
                           list( response = substitute(response),
                                 capital = substitute(capital),
                                 labor = substitute(labor),
                                 energy = substitute(energy),
                                 time = substitute(time)
                           )
    )
  }
  formulas <- list( log(y) - log(energy) ~  
                      I(2 * (1 - 1/(capital / ( .5 * (energy + labor) ) )) )  + 
                      I( labor/energy - 1 )
  )
  
  
  formulas <- lapply(formulas, function(x) do.call( substitute, list( x,  list(
    time = formula[[3]][[3]],
    energy = formula[[3]][[2]][[3]],
    labor = formula[[3]][[2]][[2]][[3]],
    capital = formula[[3]][[2]][[2]][[2]],
    y = formula[[2]]  
  ) 
  ) ) )
  
  res <- lm( formulas[[1]], data=data )
  
  # Build the additional object to add as an atrribute to the output
  a_0 <- coef(res)[2]
  a_1 <- coef(res)[3]
  c_t <- a_1 / a_0
  naturalCoeffs <- data.frame(
    logscale = as.vector(coef(res)[1]),
    scale = exp(as.vector(coef(res)[1])),
    a_0 = as.vector(a_0),
    a_1 = as.vector(a_1),
    c_t = as.vector(c_t),
    sse = sum(resid(res)^2),
    isConv = TRUE
  )
  attr(res, "naturalCoeffs") <- naturalCoeffs
  #  sdata <- subset(data, 
  #                  select= c( "iGDP","iEToFit","iK","iL","rho_k","rho_l"))
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  if (save.data) {
    attr(res, "data") <- sdata
  }
  attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  attr(res, "formula") <- formula
  
  class(res) <- c("LINEXmodel", class(res))
  return(res)
}