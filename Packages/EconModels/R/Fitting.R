
# convert from delta_1 and delta to alpha_i in a nest aware way.

standardCoefs <- function (delta=NA, delta_1=NA, nest=NULL, method = 1L, digits=5) {
  # convert to standard coefficents taking nest order into account
  # basically we are just permuting things so that alpha_i can always
  # refer to the same quantities, even when they show up in different parts of the model.
  
  # when nest is NULL, we make nest be 1:3 
  if (is.null(nest))  nest <- 1:3
 
  # if delta is NA, set it to 1 or local calculation.  
  # This collapses the 2-factor case to the 3-factor case and handles cases where delta can't be approximated.
  ldelta <- if (is.na(delta)) 1 else delta
  ldelta_1 <- if (!is.na(delta) && round(delta, digits) == 0.0) 0.5 else delta_1
  
  # make sure we have exactly 3 slots
  nest <- head(c(nest,3), 3)
  if (length(nest) < 3) stop("bad nest")
  
  # alternative ways to handle delta = NA
  # second works well for 2-factor models
 
  res <- 
    switch(method,
           "1" =  list(delta * ldelta_1,  delta * (1.0 - ldelta_1), 1.0 - delta),    # NA, NA, NA
           "2" = list(ldelta * delta_1,  ldelta * (1.0 - delta_1), 1.0 - delta),   # delta_1, 1-delta_1, NA
           "3" = list(ldelta * delta_1,  ldelta * (1.0 - delta_1), 1.0 - ldelta)   # delta_1, 1-delta_1, 0
    )
  
  # order() is used to invert the nest permutation
  res<- res[order(nest)]
  names(res) <- c("alpha_1", "alpha_2", "alpha_3")
  return(as.data.frame(res))
}

nestMatch <- function( n1, n2 ) {
  (length(n1) == length(n2)) && all(n1==n2)
}

#' Natural coefficients of a model
#' 
#' A convenience function that returns the "natural" coefficients of a model object.
#' @param object the model object from which you want to extract the \code{meta} attribute.
#' @param ... additional arguments
#' @return the coefficients of the model on a "natural" scale.
#' @export
naturalCoef <- function(object, ...) {
  UseMethod("naturalCoef")
}

#' @export
naturalCoef.default <- function(object, ...) {
  attr(object, "naturalCoeffs") 
}

#' @export
naturalCoef.SFmodel <- function(object, ...) {
  as.data.frame(
    dplyr::data_frame(
      logscale=coef(object)[1],
      scale=exp(logscale),
      lambda = if (object$winner == 2) coef(object)[2] else coef(object)[3],
      m = if (object$winner == 2) 1.0 else coef(object)[2]
    )
  )
}

#' @export
naturalCoef.CDEmodel <- function(object, ...) {
  
  leftCoef <- c(3,2,3,3,1,2,3)[object$winner]
  cf <- coef(object)[c("alpha_1", "alpha_2", "alpha_3")]
  names(cf) <-  c("alpha_1", "alpha_2", "alpha_3")
  cf[is.na(cf)] <- 0
  cf[leftCoef] <- 1 - sum(cf)
 
  as.data.frame( 
    dplyr::data_frame(
      lambda = coef(object)["lambda"],
      logscale = coef(object)["logscale"],
      scale = exp(logscale),
      alpha_1 = cf["alpha_1"],
      alpha_2 = cf["alpha_2"],
      alpha_3 = cf["alpha_3"]
    )
  )
}


#' @export
naturalCoef.LINEXmodel <- function( object, ...) {
  as.data.frame(
    dplyr::data_frame(
      logscale = coef(object)[1],
      scale = exp(logscale),
      a_0 = coef(object)[2],
      a_1 = coef(object)[3],
      c_t = a_1 / a_0
    )
  )
}

#' @export
naturalCoef.plm <- function(object, ...) {
  makeNatCoef(object, ...)
}

#' @export
naturalCoef.cesEst <- function(object, ...) {
  makeNatCoef(object, ...)
}
  
makeNatCoef <- function(object, nest=object$nest, method = 1, ...) {
  coefList <- as.list(coef(object))
  if (inherits(object, "cesEst") && length(coefList) == 4) {
    # We disagree with the naming of the coefficients by cesEst when only two factors of production are involved.
    # cesEst calls the coefficients gamma, lambda, delta, and rho.
    # We prefer gamma, lambda, delta_1 and rho_1, though, because this case provides, essentially, the inner nest.
    # This code appends "_1" to the appropriate names and adds appropriate delta, rho, and sigma values.
    # We chose to rename the coefficients based on position. 
    # We could have renamed by name to defend against position changes in cesEst.
    # But, cesEst could also change names in the future, so there is no clear benefit to renaming by name.
    # So, we'll stick with renaming by position.
    names(coefList)[3:4] <- paste0(names(coefList)[3:4], "_1")
    coefList[["delta"]] <- 1
    coefList[["sigma"]] <- NA
    coefList[["rho"]] <- NA
  }
  gamma_coef <-  tryCatch(with(coefList, exp(logscale)), error=function(e) NA)
  if (is.na(gamma_coef)) gamma_coef <- coefList$gamma
  if (is.null(gamma_coef)) gamma_coef <- NA
  lambda <-  tryCatch(with(coefList, lambda), error=function(e) NA)
  delta_1 <-  tryCatch(with(coefList, delta_1), error=function(e) NA)
  delta <-  tryCatch(with(coefList, delta), error=function(e) NA)
  rho_1 <-  tryCatch(with(coefList, rho_1), error=function(e) NA)
  sigma_1 <-  tryCatch(with(coefList, sigma_1), error=function(e) NA)
  rho <-  tryCatch(with(coefList, rho), error=function(e) NA)
  sigma <-  tryCatch(with(coefList, sigma), error=function(e) NA)
  sse <-  sum(resid(object)^2)
  if (is.null(nest)) { nest <- 1:3 }
  sc <- standardCoefs(delta_1=delta_1, delta=delta, nest=nest, method=method)
  alpha_1 <- sc$alpha_1
  alpha_2 <- sc$alpha_2
  alpha_3 <- sc$alpha_3
  
  as.data.frame( 
    dplyr::data_frame( gamma = gamma_coef,
              lambda = lambda,
              delta = delta,
              delta_1 = delta_1,
              sigma_1 = if (is.na(sigma_1)) 1/(1 + rho_1) else sigma_1,
              rho_1 = if (is.na(rho_1)) 1/sigma_1 - 1 else rho_1,
              sigma = if (is.na(sigma)) 1/(1 + rho) else sigma,
              rho = if (is.na(rho)) 1/sigma - 1 else rho,
              alpha_1 = alpha_1,
              alpha_2 = alpha_2,
              alpha_3 = alpha_3
    )
  )
}

#' Compute SSE from a model object
#' 
#' Compute SSE from a model object and also check that certain constraints are met
#' by the coefficients.
#' 
#' @param object a model object that inherits from \code{cesModel}.
#' @return a data frame with with variables \code{sse}, \code{constrained}, \code{sse.contrained}.
#' 
sse <- function(object) {
  sse <- sum(resid(object)^2)
  coefs <- naturalCoef(object)
  #   call = Reduce(paste, gdata::trim(deparse(object$call))
  constrained <- 
    (is.na(coefs$delta) || (0 <= coefs$delta && coefs$delta <= 1)) &&
    (is.na(coefs$delta_1) || (0 <= coefs$delta_1 && coefs$delta_1 <= 1)) &&
    (is.na(coefs$rho) || coefs$rho >= -1) &&
    (is.na(coefs$rho_1) || coefs$rho_1 >= -1)
  as.data.frame(
    dplyr::data_frame(
      sse = sse,
      constrained = constrained,
      sse.constrained =
        if(constrained) sse else Inf
    )
  )
}

#' Extracts the best model (least sse) from a list of models
#' 
#' @param models the list of models
#' @param digits the number of digits of \code{sse} that is considered significant
#' @param orderOnly if \code{FALSE}, returns a reordered list of models. 
#' If \code{TRUE}, returns an integer vector representing the order of the models.
#' @note The ordering process preserves the original order in the event of ties at the desired significance level.
#' @note This function relies upon the \code{sse} value being stored 
#' in an attribute of the model called \code{naturalCoeffs}.
#' @return an integer vector representing the order of the models if \code{orderOnly} is \code{TRUE}. 
#' A reordered list of models if \code{orderOnly} is \code{FALSE} (the default). 
#' @export
bestModel <- function(models, digits=6, orderOnly=FALSE, constrained=FALSE) {
  if (constrained) {
    o <- order(sapply( models, function(model) { 
      if (is.null(model)) NA else round(sse(model)$sse.constrained, digits=digits) } ) )
  } else {
    o <- order(sapply( models, function(model) { 
      if (is.null(model)) NA else round(sse(model)$sse, digits=digits) } ) )
  }
  if (orderOnly) return(o)
  models[[ o[1] ]] 
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

# Note: fitted() returns on the natural scale for cesEst objects but on the log-scale for plm objects,
# but both return residuals on the log-scale, so this works for both of them.
#' @export
yhat.cesModel <- function(object, ...) {
  object$response / exp(resid(object))
}

#' @export
yhat.CDEmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), object$data)
  exp( fitted(object,...) + lx0[!is.na(lx0)] )
}

#' @export
yhat.LINEXmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), object$data)
  exp( fitted(object, ...) + lx0[!is.na(lx0)] )
}

#' @export
yhat.sfModel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), object$data)
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
response.LINEXmodel <- function(object, ...)
    return( object$response )

#' @export
response.CDEmodel <- function(object, ...)
    return( object$response )

#' @export
response.cesModel <- function(object, ...)
    return( object$response )

#' @export
response.default <- function(object, ...) {
  return( fitted(object) + resid(object) )
}

#' @export
predict.CDEmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), object$data)
  exp( NextMethod() + lx0[!is.na(lx0)] )
}

#' @export
predict.LINEXmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), object$data)
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
                    log(y) - log(x) ~ time)
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
  
  if (constrained){
    res <- eval( substitute(lm(f, data=data), list(f=formulas[[2]])) )
    res$winner <- 2
    logscale <- coef(res)[1]
    lambda <- coef(res)[2]
  } else {
    res <- eval( substitute(lm(f, data=data), list(f=formulas[[1]])) )
    res$winner <- 1
    logscale <- coef(res)[1]
    lambda <- as.vector(coef(res)[3])
  }
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  if (save.data) { res$data <- sdata }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  res$formula <- formula
  
  class(res) <- c("SFmodel", class(res))
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
#' @param constrained a logical indicating whether the parameters should be constrained in the fitting process.
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

CDformulas <- list( 
  log(y) - log(energy) ~ 
    I(log(capital) - log(energy)) + I(log(labor) - log(energy)) + time,  
  
  log(y) - log(labor)  ~ I(log(capital) - log(labor)) + time,
  log(y) - log(energy) ~ I(log(capital) - log(energy)) + time,
  log(y) - log(energy) ~ I(log(labor)  - log(energy)) + time,
  
  log(y) - log(capital) ~ time,
  log(y) - log(labor)  ~ time,
  log(y) - log(energy) ~ time 
)

CDcoefNames <- list( 
  c("logscale",  "alpha_1", "alpha_2", "lambda"),
  
  c("logscale", "alpha_1", "lambda"),
  c("logscale", "alpha_1", "lambda"),
  c("logscale", "alpha_2", "lambda"),
  
  c("logscale", "lambda"),
  c("logscale", "lambda"),
  c("logscale", "lambda")
)

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
  
  sdata <- subset(data, select=all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  formulas <- CDformulas[c(2,5,6)]  # just the ones involving 2 of the 3 factors
  
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
  res <- eval(substitute(lm( f, data=sdata ), list(f=formulas[[1]])))
  res$winner <- 2
  names(res$coefficients) <- CDcoefNames[[2]]
  alpha_1 <- coef(res)["alpha_1"]
  if (constrained){
    if (alpha_1 < 0.0 || alpha_1 > 1.0){
      # Need to adjust alpha_1, because we are beyond 0.0 or 1.0
      if (alpha_1 < 0.0){
        alpha_1 <- 0.0
        res <- eval(substitute(lm( f, data=sdata ), list(f=formulas[[2]])))
        res$winner <- 6
      } else {
        alpha_1 <- 1.0
        res <- eval(substitute(lm( f, data=sdata ), list(f=formulas[[3]])))
        res$winner <- 5
      }
      names(res$coefficients) <- c("logscale", "lambda")
    }
  }
  if (save.data) {
    res$data <- sdata
  }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  res$formula <- formula
  
  class(res) <- c("CDEmodel", class(res))
  return(res)
}

respectsConstraints <- function( model ) {
  # Tells whether we're fitting within the constraints for a Cobb-Douglas model.
  # This assumes that the model has coefficients named alpha_i or that 
  # the paramters of interest are all but the first (intercept) and last (time).
  cf <- coef(model)
  if (any(c("alpha_1","alpha_2","alpha_3") %in% names(cf) ) ) {
    cf <- cf[ names(cf) %in% c("alpha_1", "alpha_2", "alpha_3") ]
  } else {
    # get rid of first and last (intercept and time)
    cf <- tail(head(cf,-1), -1) 
  }
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
  
  
  formulas <- lapply(CDformulas, function(x) do.call( substitute, list( x,  list(
    time = formula[[3]][[3]],
    energy = formula[[3]][[2]][[3]],
    labor = formula[[3]][[2]][[2]][[3]],
    capital = formula[[3]][[2]][[2]][[2]],
    y = formula[[2]]  
  ) 
  ) ) )
  
  models <- lapply( formulas, function(form)
    eval(substitute(lm(f, data=sdata), list(f=form)))  
  )
  sse <- sapply( models, function(m) sum( resid(m)^2 ) )
  if ( constrained ) {
    good <- sapply( models, respectsConstraints)
    good[ !good ] <- NA
  } else { 
    good <- TRUE
  }
  winner <- which.min( sse * good )
  res <- models[[winner]]
  names( res$coefficients ) <- CDcoefNames[[winner]]
  res$winner <- winner
  res$formula <-  formula
  attr(res, "good") <-  sapply( models, respectsConstraints )
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  if (save.data) { res$data <- sdata }
  
  class(res) <- c( "CDEmodel", class(res) )
  return(res)
}


#' Fitting LINEX models
#' 
#' @param formula a formula of the form \code{response ~ capital + labor + energy + time}
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
  
  res <- eval( substitute(lm(f, data=data), list(f=formulas[[1]])) )
  
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  if (save.data) { res$data <- sdata }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  res$formula <- formula
  
  class(res) <- c("LINEXmodel", class(res))
  return(res)
}