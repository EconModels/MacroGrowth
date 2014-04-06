
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
  return( fitted(object) + resid(object) )
}

#' @export
response.LINEXmodel <- function(object, ...) {
    return( attr(object, "response") )
}

#' @export
response.CDEmodel <- function(object, ...) {
    return( attr(object, "response") )
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

#residuals.CDEmodel <- function( object, ... ) {
#  e <- NextMethod()
#  return(exp(e))
#  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
#  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
#  ly <- eval( parse( text = gsub( " - .*", "", names(object$model)[1]) ), attr(object,'data'))
#  y <- exp(ly)
#  lfits <- NextMethod() + lx0 
#  e <- ly - lfits 
#  E<- exp(e)
#  return(E)
#}

#residuals.LINEXmodel <- function( object, ... ) {
#  e <- NextMethod()
#  return( exp(e) )
#  # model has form:
#  # log(iGDP) - log(iEToFit) ~ I(2 * (1 - 1/rho_k)) +  I(rho_l - 1)
#  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
#  ly <- eval( parse( text = gsub( " - .*", "", names(object$model)[1]) ), attr(object,'data'))
#  y <- exp(ly)
#  lfits <- NextMethod() + lx0 
#  e <- ly - lfits 
#  E <- exp(e)
#  return( switch(type, response=E, log=e))
#}

#' Fitting single factor models
#' 
#' @param formula a formula of the form \code{ y ~ factor + time }
#' @param data a data frame in which \code{formula} is evaluated
#' @param constrained a logical indicating whether the model parameters are constrained
#' @return an lm object with some additional attributes
#' @export
singleFactorModel2 <- function(formula, data, response, factor, time, constrained=FALSE) {
  ####################
  # Returns an nls single-factor model for the country and factor specified.
  # factor should be one of "K", "L", "Q", "X", or "U".
  # Runs a non-linear least squares fit to the data. We've replaced beta with 1-alpha for simplicity.
  # model <- iGDP ~ exp(lambda*iYear) * f^m
  # modelSF <- nls(formula=model, data=data, start = start, control=nlsControl)
  
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
    modelSF <- lm( formulas[[2]], data=data )
    m <- 1.0
    logscale <- coef(modelSF)[1]
    lambda <- coef(modelSF)[2]
  } else {
    modelSF <- lm( formulas[[1]], data=data )
    logscale <- coef(modelSF)[1]
    m <- coef(modelSF)[2]
    lambda <- as.vector(coef(modelSF)[3])
  }
  naturalCoeffs <- data.frame(
    logscale=as.vector(logscale),
    scale=exp(as.vector(logscale)),
    lambda = as.vector(lambda),
    m = as.vector(m),
    sse = sum(resid(modelSF)^2),
    isConv = TRUE
  )
  
  attr(x=modelSF, which="naturalCoeffs") <- naturalCoeffs
  
  class(modelSF) <- c("sfModel", class(modelSF))
  return(modelSF)
  
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
#' @param contrained a logical indicating whether the parameters are contrained
#' @return a CDEmodel object, which is an lm object with some additioanl attributes.
#' @export
cdModel2 <- function(formula, data, response, capital, labor, time, constrained=FALSE, ...) {
  if ( missing(formula) ) {
    formula <- substitute( response ~ capital + labor + time,
                           list( response = substitute(response),
                                 capital = substitute(capital),
                                 labor = substitute(labor),
                                 time = substitute(time)
                           )
    )
  }
  
  formulas <- list(
    log(y) - log(labor) ~ time + I(log(capital) - log(labor)),
    log(iGDP) - log(iCapStk) ~ iYear,  
    log(iGDP) - log(iLabor) ~ iYear 
  )
  
  formulas <- lapply( formulas,
                      function(x) do.call(substitute, 
                                          list(x,
                                               list( y = formula[[2]],
                                                     capital = formula[[3]][[2]][[2]],
                                                     labor = formula[[3]][[2]][[3]],
                                                     time = formula[[3]][[2]]
                                               )
                                          )
                      )
  )
  modelCD <- lm(formulas[[1]], data=data)
  # Build the additional object to add as an atrribute to the output
  # could try this, but it is a hack.
  # model$coefficients <- c(m$cofficients, 1 - m$coeffcients[3]) 
  names(modelCD$coefficients) <- c( "logscale", "lambda", "alpha")
  alpha <- coef(modelCD)["alpha"]
  if (constrained){
    if (alpha < 0.0 || alpha > 1.0){
      # Need to adjust alpha, because we are beyond 0.0 or 1.0
      if (alpha < 0.0){
        alpha <- 0.0
        modelCD <- lm( formulas[[2]], data=data )
      } else {
        alpha <- 1.0
        modelCD <- lm( formulas[[3]], data=data )
      }
      # Refit for lambda only
      names(modelCD$coefficients) <- c("logscale", "lambda")
    }
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(modelCD)["lambda"]),
                              logscale = as.vector(coef(modelCD)["logscale"]),
                              scale = exp(as.vector(coef(modelCD)["logscale"])),
                              alpha = as.vector(alpha),
                              beta = as.vector(1.0 - alpha),
                              gamma = 0.0, # Energy is not a factor for this model.
                              sse = sum(resid(modelCD)^2),
                              isConv = TRUE  # modelCD$convInfo$isConv
  )
  attr(modelCD, "naturalCoeffs") <- naturalCoeffs
  sdata <- subset(data, select= all.vars(modelCD$terms))
  attr(modelCD, "data") <- data[complete.cases(sdata),]
  
  class(modelCD) <- c("CDEmodel", class(modelCD))
  return(modelCD)
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
#' @export
# y ~ capital + labor + energy + time
cdeModel2 <- function( formula, data, response, capital, labor, energy, time, constrained=FALSE, ...){
  
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
  formulas <- list( 
    log(y) - log(energy) ~ 
      time + I(log(capital) - log(energy)) + I(log(labor) - log(energy)),  
    
    log(y) - log(energy) ~ time + I(log(capital) - log(energy)),
    log(y) - log(energy) ~ time + I(log(labor)  - log(energy)),
    log(y) - log(labor)  ~ time + I(log(capital) - log(labor)),
    
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
  #  models <- lapply( formulas, function(form)  m <- lm( form, data=data )  )
  models <- lapply( formulas, function(form)  lm( form, data=data )  )
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
  sdata <- subset(data, select = all.vars(res$terms))
  sdata <- data[complete.cases(sdata),]
  attr(res, "data") <- sdata
  attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  class(res) <- c( "CDEmodel", class(res) )
  return(res)
}


#' Fitting CES models
#' 
#' This function fits a CES model
#' 
#' @param data a data frame, if you want to use resampled data.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param nest a permutation (a,b,c,d) of the integers 1 through 4.
#' For models with 3 factors, the nesting
#' is (a + b) + c.  For 4 factors, the nesting is (a + b) + (c + d)
#' @param fittingToResampleData a logical.  If \code{FALSE}, you should also supply a 
#' value for \code{origModel}
#' because \code{origModel} will be used to obtain the starting point for a gradient search.
#' @param origModel a model
#' @param prevModel a model used to start gradient searches.
#' Use \code{NULL} if you want to use the default start locations AND do a grid search 
#' in sigma.
#' @param rho,rho1 Default values for \code{rho} and \code{rho1} are a grid upon which 
#' searches will be made.
#' Note that \code{rho = 0.25} and \code{rho1 = 0.25} are included. These are the default 
#' starting values for \code{rho} and \code{rho1}, so that we don't need to do a fit from 
#' the default values.
#' \code{rho = 0.25} corresponds to \code{sigma = 0.8}.
#' @return a list of models 
#' @export
cesModel3 <- function(formula, data,
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
  } else {
    numComponents <- length(all.vars(formula)) - 2 # subtract off response and time
  }
#   if (energyType != "none" && (nest != "(kl)")){
#     # We need to do the CES fit with the desired energyType.
#     # But, only if we asked for a nest that isn't "(kl)"
#     # To achieve the correct fit, we'll change the name of the desired column
#     # to "iEToFit" and use "iEToFit" in the nls function.
#     data <- replaceColName(data, energyType, "iEToFit")
#     # Remove rows with missing energy information
#     data <- data[ !is.na(data[,"iEToFit"]), ]
#   }
#   
  # Verify algorithm
  cesAlgorithms <- c("PORT", "L-BFGS-B") # These are the only valid algs that respect constraints
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

  # only keep data actually used to fit the model
  data <- data[ , c(yName, xNames, tName)]
  data <- data[ complete.cases(data), ]

  models <- list()
  for (algorithm in algorithms) {
    #
    # Try grid search.
    #
    if (numFactors == 2) {
      # We want a model without energy. No need for a rho1 argument.
      model <- tryCatch(
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               rho=rho, control=chooseCESControl(algorithm), multErr=multErr, ...),
        error = function(e) { print(e); list(data=data, 
                                   yName=yName, 
                                   xNames=xNames, 
                                   tName=tName, 
                                   method=algorithm, 
                                   control=chooseCESControl(algorithm), ...) }
      )
    } else {
      # We want a model with energy. Need a rho1 argument, because we are using a nesting.
      model <- tryCatch(
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               rho=rho, rho1=rho1, control=chooseCESControl(algorithm), multErr=multErr, ...),
        error = function(e) { print(e); list(data=data, 
                                   yName=yName, 
                                   xNames=xNames, 
                                   tName=tName, 
                                   method=algorithm,
                                   control=chooseCESControl(algorithm), ...) }
      )
    }
    
    hist <- paste(algorithm, "(grid)", sep="", collapse="|")  
    model <- addMetaData(model, nest=nest, nestString=nestString, history=hist)
    models[length(models)+1] <- list(model)
    
  }
  #
  # Now try gradient search starting from the best place found by the grid searches above.
  #
  bestMod <- bestModel(models, digits=digits)
  start <- coef(bestMod)
  for (algorithm in algorithms) {
    model <- tryCatch(
      cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
             control=chooseCESControl(algorithm), start=start, multErr=multErr, ...),
      error = function(e) { print(e); NULL }
    )
    hist <- paste(algorithm, "[", getHistory(bestMod), "]", collapse="|", sep="")
    model <- addMetaData(model, nest=nest, nestString=nestString, history=hist)
    models[[length(models)+1]] <- model
  }
  #
  # Now try gradient search starting from prevModel (if it is present in the argument list).
  #
  if (! is.null(prevModel)){
    start <- coef(prevModel)
    for (algorithm in algorithms) {
      model <- tryCatch(
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               control=chooseCESControl(algorithm), start=start, multErr=multErr, ...),
        error = function(e) { print(e); NULL }
      )
      hist <- paste(algorithm, "[", getHistory(prevModel), ".prev]", sep="", collapse="|")
      model <- addMetaData(model, nest=nest, nestString=nestString, history=hist)
      models[[length(models)+1]] <- model
    }
  }
  # Return everything all of the models that we calculated.
  res <- bestModel(models)
  attr(res, "model.attempts") <- models
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
    alpha <- 1.0 - delta
    beta <- delta * delta_1
    gamma <- delta * (1.0 - delta_1)
  } else if ( nestMatch(nest, c(1,3,2) ) ) { # (nest == "(ek)l"){
    alpha <- delta * (1.0 - delta_1)
    beta <- 1.0 - delta
    gamma <- delta * delta_1
  } else {
    print(nest)
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
linexModel2 <- function(formula, data, response, capital, labor, energy, time) {
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
  #                  select= c( "iGDP","iEToFit","iCapStk","iLabor","rho_k","rho_l"))
  sdata <- subset(data, select = all.vars(res$terms))
  sdata <- data[complete.cases(sdata),]
  attr(res, "data") <- sdata
  attr(res, "response") <- eval( formula[[2]], sdata, parent.frame() )
  class(res) <- c("LINEXmodel", class(res))
  return(res)
}