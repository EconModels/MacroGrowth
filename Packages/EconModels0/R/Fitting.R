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
  if ( inherits( models, "cesEst") )  {  models <- list(models) }
  o <- order(sapply( models, function(model) { round(sum(resid(model)^2), digits=digits) } ) )
  if (orderOnly) return(o)
  out  <- models[[ o[1] ]] 
  return(out) 
}

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
#  # log(iY) - log(iEToFit) ~ I(2 * (1 - 1/rho_k)) +  I(rho_l - 1)
#  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), attr(object,'data'))
#  ly <- eval( parse( text = gsub( " - .*", "", names(object$model)[1]) ), attr(object,'data'))
#  y <- exp(ly)
#  lfits <- NextMethod() + lx0 
#  e <- ly - lfits 
#  E <- exp(e)
#  return( switch(type, response=E, log=e))
#}

#' @export
respectsConstraints <- function( model ) {
  ###############
  # Tells whether we're fitting within the constraints for a Cobb-Douglas model.
  ##
  cf <- coef(model)
  if( length(cf) >=2 ) cf <- cf[-c(1,2)] 
  all( cf >= 0 ) & ( sum(cf) <=1 )
}

#' Fit CES model
#' 
#' This function fits a CES model to original or resampled data.
#' 
#' @param data a data frame, if you want to use resampled data.
#' @param baseHistorical the base directory from which to load historical data.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param nest one of \code{"(kl)"}, \code{"(le)"}, or \code{"(ek)"}.
#' Use  \code{"(kl)"} if you want a fit without energy, regardless 
#' of which energyType is specified.
#' @param energyType Use \code{"none"} to specify a CES fit without energy, but only if 
#' \code{nest != "(kl)"}.
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
cesModel2 <- function(countryAbbrev, 
                      energyType="none", 
                      baseHistorical, 
                      data = loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical), 
                      prevModel=NULL,
                      algorithms=c("PORT","L-BFGS-B"), 
                      nest="(kl)e", 
                      rho =c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      rho1=c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      digits=6,
                      ...){
  
  if (energyType != "none" && (nest != "(kl)")){
    # We need to do the CES fit with the desired energyType.
    # But, only if we asked for a nest that isn't "(kl)"
    # To achieve the correct fit, we'll change the name of the desired column
    # to "iEToFit" and use "iEToFit" in the nls function.
    data <- replaceColName(data, energyType, "iEToFit")
    # Remove rows with missing energy information
    data <- data[ !is.na(data[,"iEToFit"]), ]
  }
  
  # Verify algorithm
  cesAlgorithms <- c("PORT", "L-BFGS-B") # These are the only valid algs that respect constraints
  algorithms <- toupper(algorithms)
  badAlgorithms <- setdiff(algorithms, cesAlgorithms)
  algorithms <- intersect(algorithms, cesAlgorithms)
  for (m in badAlgorithms) {
    stop(paste("Unrecognized algorithm:", m))
  }
  # Set up xNames for the desired energy type or nesting
  if (energyType == "none" || nest == "(kl)"){
    # We don't want to include energy. So, include only k and l.
    xNames <- c("iK", "iL")
  } else {
    # We want to include energy. So, include k, l, and e.
    if (nest %in% c("(kl)e", "(lk)e")){
      xNames <- c("iK", "iL", "iEToFit")
    } else if (nest %in% c("(le)k", "(el)k")){
      xNames <- c("iL", "iEToFit", "iK")
    } else if (nest %in% c("(ek)l", "(ke)l")){
      xNames <- c("iEToFit", "iK", "iL")
    } else {
      stop(paste("Unknown nesting option", nest, "in cesModel2"))
    }
  }
  # Establish key variable names  
  tName <- "iYear"
  yName <- "iY"
  models <- list()
  for (algorithm in algorithms) {
    #
    # Try grid search.
    #
    if (energyType == "none" || nest=="(kl)"){
      # We want a model without energy. No need for a rho1 argument.
      tryCatch( {
        model <- cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
                        rho=rho, control=chooseCESControl(algorithm), multErr=TRUE, ...)
        hist <- paste(algorithm, "(grid)", sep="", collapse="|")  
        model <- addMetaData(model, nest=nest, history=hist)
        models[length(models)+1] <- list(model)
      },
      error = function(e) { warning(paste("Error in cesEst() "), print(e)) }
      )
    } else {
      # We want a model with energy. Need a rho1 argument, because we are using a nesting.
      tryCatch( {
        # If multErr=TRUE in the call to cesEst, we sometimes get models that don't work. Not sure why.
        model <- cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
                        rho=rho, rho1=rho1, control=chooseCESControl(algorithm), multErr=TRUE, ...)
        hist <- paste(algorithm, "(grid)", sep="", collapse="|")  
        model <- addMetaData(model, nest=nest, history=hist)
        models[length(models)+1] <- list(model)
      },
      error = function(e) { warning(paste("Error in cesEst() "), print(e)) }
      )
    }
  }
  #
  # Now try gradient search starting from the best place found by the grid searches above.
  #
  bestMod <- bestModel(models, digits=digits)
  start <- coef(bestMod)
  for (algorithm in algorithms) {
    model <- tryCatch(
      cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
             control=chooseCESControl(algorithm), start=start, multErr=TRUE, ...),
      error = function(e) { NULL }
    )
    hist <- paste(algorithm, "[", getHistory(bestMod), "]", collapse="|", sep="")
    model <- addMetaData(model, nest=nest, history=hist)
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
               control=chooseCESControl(algorithm), start=start, multErr=TRUE, ...),
        error = function(e) { NULL }
      )
      hist <- paste(algorithm, "[", getHistory(prevModel), ".prev]", sep="", collapse="|")
      model <- addMetaData(model, nest=nest, history=hist)
      models[[length(models)+1]] <- model
    }
  }
  # Return everything all of the models that we calculated.
  return(models)
}

#' @export
addMetaData <- function(model, nest, history=""){
  ###############
  # This function adds metadata to a model.  Currently this is only designed to
  # work with CES models. Metadata is attached as attributes (naturalCoeffs and meta)
  # to the object and the new object is returned from the function.
  ##
  if (is.null(model)){
    return(model) 
  }
  if (is.null(model$call)){
    stop("model$call is null")
  }
  if (length(model$call) < 1){
    stop("length(model) is less than 1")
  }
  call <- model$call[[1]]
  if (is.null(call)){
    stop("model$call[[1]] is null")
  }
  if ( ! as.character(call) == "cesEst" ){
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
  if (missing(nest) || is.na(nest) || nest == "(kl)" || nest == "kl"){
    alpha <- delta_1
    beta <- 1.0 - delta_1
    gamma <- 0.0
  } else if (nest == "(kl)e"){
    alpha <- delta * delta_1
    beta  <- delta * (1.0 - delta_1)
    gamma <- 1.0 - delta
  } else if (nest == "(le)k"){
    alpha <- 1.0 - delta
    beta <- delta * delta_1
    gamma <- delta * (1.0 - delta_1)
  } else if (nest == "(ek)l"){
    alpha <- delta * (1.0 - delta_1)
    beta <- 1.0 - delta
    gamma <- delta * delta_1
  } else {
    stop(paste("Unknown nest:", nest, "in addMetaData."))
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
                          history=history
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
                     history=history
  )
  
  if ( nrow(metaData) > 1 ) {
    warning( paste0("\nmeta data has ", nrow(metaData), " rows: ", paste(nest,history, sep="|")) )
    for (item in metaList) { 
      if ( length(item) > 1 ) {
        warning(paste0("\t", toString(item)))
      }
    }
  }
  attr(x=model, "naturalCoeffs") <- naturalCoeffs[1,]
  metaData$metaDataRows <- nrow(metaData)
  attr(x=model, "meta") <- metaData[1,] 
  attr(x=model, "metaList") <- metaList 
  return(model)
}

#' @export
singleFactorModel <- function(countryAbbrev, baseHistorical, data=loadData(countryAbbrev, baseHistorical), 
                              factor, respectRangeConstraints=FALSE){
  ####################
  # Returns an nls single-factor model for the country and factor specified.
  # factor should be one of "K", "L", "Q", "X", or "U".
  ##
  # We'll change the name of the desired column to "f"
  data <- replaceColName(data, factor, "f")
  # Now do the fit.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  mGuess <- 0.5 # works for almost every country
  if (respectRangeConstraints){
    m <- 1.0
    start <- list(lambda=lambdaGuess)
  } else {
    start <- list(lambda=lambdaGuess, m=mGuess)
  }
  # Runs a non-linear least squares fit to the data. We've replaced beta with 1-alpha for simplicity.
  model <- iY ~ exp(lambda*iYear) * f^m
  modelSF <- nls(formula=model, data=data, start = start, control=nlsControl)
  # Build the additional object to add as an atrribute to the output
  if (!respectRangeConstraints){
    m <- coef(modelSF)["m"]
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(modelSF)["lambda"]),
                              m = as.vector(m),
                              sse = sum(resid(modelSF)^2),
                              isConv = modelSF$convInfo$isConv
  )
  attr(x=modelSF, which="naturalCoeffs") <- naturalCoeffs
  return(modelSF)
}


#' @export
cdModel <- function(countryAbbrev, baseHistorical, data=loadData(countryAbbrev, baseHistorical), 
                    respectRangeConstraints=FALSE, ...){
  ## <<cobb-douglas functions, eval=TRUE>>=
  ####################
  # Returns an nls Cobb-Douglas model (without energy) for the country specified. 
  # No energy is included in the function to be fitted.
  # Note that the argument countryAbbrev is first so that we can use lapply 
  # with a list of country abbreviations.
  ##
  # Run the non-linear least squares fit to the data. No energy term desired in the Cobb-Douglas equation.
  # Establish guess values for alpha and lambda.
  # lambdaGuess <- 0.0 # guessing lambda = 0 means there is no technological progress.
  # alphaGuess <- 0.7 # 0.7 gives good results for all countries.  
  # start <- list(lambda=lambdaGuess, alpha=alphaGuess)
  # Runs a non-linear least squares fit to the data. We've replaced beta with 1-alpha for simplicity.
  # model <- iY ~ exp(lambda*iYear) * iK^alpha * iL^(1.0 - alpha)
  # modelCD <- nls(formula=model, data=data, start=start, control=nlsControl)
  model <- log(iY) - log(iL) ~ iYear + I(log(iK) - log(iL)) 
  modelCD <- lm(model, data=data)
  # Build the additional object to add as an atrribute to the output
  # could try this, but it is a hack.
  # model$coefficients <- c(m$cofficients, 1 - m$coeffcients[3]) 
  names(modelCD$coefficients) <- c( "logscale", "lambda", "alpha")
  alpha <- coef(modelCD)["alpha"]
  if (respectRangeConstraints){
    if (alpha < 0.0 || alpha > 1.0){
      # Need to adjust alpha, because we are beyond 0.0 or 1.0
      if (alpha < 0.0){
        alpha <- 0.0
        formula <- log(iY) - log(iL) ~ iYear  
      } else {
        alpha <- 1.0
        formula <- log(iY) - log(iK) ~ iYear  
      }
      # Refit for lambda only
      # start <- list(lambda=lambdaGuess)
      # modelCD <- nls(formula=model, data=data, start=start, control=nlsControl)
      modelCD <-lm( formula, data=data)
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

#' @export
cdeModel <- function(countryAbbrev, 
                     baseHistorical,
                     energyType="Q", 
                     data=loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical), 
                     respectRangeConstraints=FALSE, ...){
  
  ##
  # We need to do the Cobb-Douglas fit with the desired energy data.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function. 
  data <- replaceColName(data, energyType, "iEToFit")
  formulas <- list( 
    log(iY) - log(iEToFit) ~ 
      iYear + I(log(iK) - log(iEToFit)) + I(log(iL) - log(iEToFit)),  
    
    log(iY) - log(iEToFit) ~ iYear + I(log(iK) - log(iEToFit)),
    log(iY) - log(iEToFit) ~ iYear + I(log(iL)  - log(iEToFit)),
    log(iY) - log(iL)  ~ iYear + I(log(iK) - log(iL)),
    
    log(iY) - log(iK) ~ iYear,
    log(iY) - log(iL)  ~ iYear,
    log(iY) - log(iEToFit) ~ iYear 
  )
  coefNames <- list( 
    c("logscale", "lambda", "alpha", "beta"),
    
    c("logscale", "lambda", "alpha"),
    c("logscale", "lambda", "beta"),
    c("logscale", "lambda", "alpha"),
    
    c("logscale", "lambda"),
    c("logscale", "lambda"),
    c("logscale", "lambda")
  )
  models <- lapply( formulas, function(form)  m <- lm( form, data=data )  )
  sse <- sapply( models, function(m) sum( resid(m)^2 ) )
  if ( respectRangeConstraints ) {
    good <- sapply( models, respectsConstraints )
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
  attr(res, "data") <- data[complete.cases(sdata),]
  class(res) <- c( "CDEmodel", class(res) )
  return(res)
}

#' @export
linexModel <- function(countryAbbrev, energyType, baseHistorical, data=loadData(countryAbbrev, baseHistorical)){
  ####################
  # Returns an nls linex model for the country and energyType specified.
  # energyType must be one of "Q", "X", or "U".
  # 
  # If you want to supply your own data, you need to specify ALL arguments.
  # Also, be VERY SURE that countryAbbrev is appropriate for the data you are supplying,
  # because decisions about guess values for parameters and optimization methods
  # are made based upon countryAbbrev, and there is no way to verify that 
  # countryAbbrev is associated with data.    
  ##
  # We need to do the Linex fit with the desired energyType.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function.
  data <- replaceColName(data=data, factor=energyType, newName="iEToFit")
  data <- transform(data, 
                    rho_k = iK / ( (.5) * (iEToFit + iL) ),
                    rho_l = iL / iEToFit 
  )
  model <- lm( log(iY) - log(iEToFit) ~  I(2 * (1 - 1/rho_k))  + I(rho_l - 1), data=data)
  
  # Build the additional object to add as an atrribute to the output
  a_0 <- coef(model)[2]
  a_1 <- coef(model)[3]
  c_t <- a_1 / a_0
  naturalCoeffs <- data.frame(
    logscale = as.vector(coef(model)[1]),
    scale = exp(as.vector(coef(model)[1])),
    a_0 = as.vector(a_0),
    a_1 = as.vector(a_1),
    c_t = as.vector(c_t),
    sse = sum(resid(model)^2),
    isConv = TRUE
  )
  attr(model, "naturalCoeffs") <- naturalCoeffs
  sdata <- subset(data, 
                  select= c( "iY","iEToFit","iK","iL","rho_k","rho_l"))
  attr(model, "data") <- data[complete.cases(sdata),]
  
  class(model) <- c("LINEXmodel", class(model))
  return(model)
}
