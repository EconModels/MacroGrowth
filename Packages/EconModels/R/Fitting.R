
# convert from delta_1 and delta to alpha, beta, gamma in a nest aware way.

standardCoefs <- function (delta=NA, delta_1=NA, nest=NULL, method = 1L, digits=5) {
  # convert to standard coefficents taking nest order into account
  # basically we are just permuting things so that alpha, beta, and gamma can always
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
   
# Old code to be deleted once we have established that the new, shorter code does the same job. 
#   if (is.na(nest) || is.null(nest) || nestMatch(nest, 1:2) ) { # (nest == "(kl)"){
#     alpha <- delta_1
#     beta <- 1.0 - delta_1
#     gamma <- 0.0
#   } else if (nestMatch(nest, c(2, 1))){ # (nest == "(lk)"){
#     alpha <- 1 - delta_1
#     beta <- delta_1
#     gamma <- 0
#   } else if ( nestMatch(nest, 1:3) ) { # (nest == "(kl)e"){
#     alpha <- delta * delta_1
#     beta  <- delta * (1.0 - delta_1)
#     gamma <- 1.0 - delta
#   } else if ( nestMatch(nest, c(2,3,1) ) ) { # (nest == "(le)k"){
#     beta <- delta * delta_1
#     gamma <- delta * (1.0 - delta_1)
#     alpha <- 1.0 - delta
#   } else if ( nestMatch(nest, c(1,3,2) ) ) { # (nest == "(ke)l"){
#     alpha <- delta * delta_1
#     gamma <- delta * (1.0 - delta_1)
#     beta <- 1.0 - delta
#   } else if ( nestMatch(nest, c(2,1,3) ) ) { # (nest == "(lk)e"){
#     beta <- delta * delta_1
#     alpha <- delta * (1.0 - delta_1)
#     gamma <- 1.0 - delta
#   } else if ( nestMatch(nest, c(3,2,1) ) ) { # (nest == "(el)k"){
#     gamma <- delta * delta_1
#     beta <- delta * (1.0 - delta_1)
#     alpha <- 1.0 - delta
#   } else if ( nestMatch(nest, c(3,1,2) ) ) { # (nest == "(ek)l"){
#     gamma <- delta * delta_1
#     alpha <- delta * (1.0 - delta_1)
#     beta <- 1.0 - delta
#   } else {
#     stop(paste("Unknown nest:", deparse(nest)))
#   }
#   
#   # should this be a list, a named vector, or a data.frame?
#   return(data.frame(alpha=alpha, beta=beta, gamma=gamma))
# }

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
  
  data_frame( gamma = gamma_coef,
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
}

#' Model metadata
#' 
#' A convenience function that returns the \code{meta} attribute of a model object.
#' @param object the model object from which you want to extract the \code{meta} attribute.
#' @return the \code{meta} attribute from \code{object}.
#' @export
#' 
metaData <- function(object, ...) {
  UseMethod("metaData")
}

#' @export
metaData.default <- function(object, ...) {
  as.data.frame(matrix(nrow=1, ncol=0))
}

#' @export
metaData.cesModel <- function(object, ...) {
  attr(object, "meta") 
}

sse <- function(object) {
  sse <- sum(resid(object)^2)
  coefs <- naturalCoef(object)
#   call = Reduce(paste, gdata::trim(deparse(object$call))
  data_frame(
    sse = sse,
    constrained =
      (is.na(coefs$delta) || (0 <= coefs$delta && coefs$delta <= 1)) &&
      (is.na(coefs$delta_1) || (0 <= coefs$delta_1 && coefs$delta_1 <= 1)) &&
      (is.na(coefs$rho) || coefs$rho >= -1) &&
      (is.na(coefs$rho_1) || coefs$rho_1 >= -1) ,
    sse.constrained =
      if(constrained) sse else Inf
  )
}
#' Natural coefficients and metadata for all model attempts
#' 
#' A convenience function that returns a data frame containing both 
#' \code{naturalCoeffs} and \code{meta} attributes for all model attempts 
#' stored in the \code{model.attempts} attribute of \code{object}.
#' @param object the model object from which you want to extract
#' information from all model attempts.
#' @return a data frame containing the \code{naturalCoeffs} and \code{meta} attributes from
#' all model attempts in \code{object}.
#' @export
natmetaFrame <- function(object){
  natc <- plyr::rbind.fill(lapply(attr(object, "model.attempts"), naturalCoef))
  sse  <- plyr::rbind.fill(lapply(attr(object, "model.attempts"), sse))
  meta <- plyr::rbind.fill(lapply(attr(object, "model.attempts"), metaData))
  return(cbind(natc, sse, meta))
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
  # Build the additional object to add as an atrribute to the output
  if (constrained){
    # res <- lm( formulas[[2]], data=data )
    res <- eval( substitute(lm(f, data=data), list(f=formulas[[2]])) )
    
    m <- 1.0
    logscale <- coef(res)[1]
    lambda <- coef(res)[2]
  } else {
    # res <- lm( formulas[[1]], data=data )
    res <- eval( substitute(lm(f, data=data), list(f=formulas[[1]])) )
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
    isConv = TRUE # always, because we use lm
  )
  
  attr(res, "naturalCoeffs") <- naturalCoeffs
  
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
  
  
  formulas <- list(
    log(y) - log(labor) ~ I(log(capital) - log(labor)) + time,
    log(y) - log(labor) ~ time,
    log(y) - log(capital) ~ time  
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
  # res <- lm(formulas[[1]], data=sdata)
  res <- eval(substitute(lm( f, data=sdata ), list(f=formulas[[1]])))
  # Build the additional object to add as an atrribute to the output
  # could try this, but it is a hack.
  # model$coefficients <- c(m$cofficients, 1 - m$coeffcients[3]) 
  names(res$coefficients) <- c( "logscale",  "alpha", "lambda")
  alpha <- coef(res)["alpha"]
  if (constrained){
    if (alpha < 0.0 || alpha > 1.0){
      # Need to adjust alpha, because we are beyond 0.0 or 1.0
      if (alpha < 0.0){
        alpha <- 0.0
        res <- eval(substitute(lm( f, data=sdata ), list(f=formulas[[2]])))
      } else {
        alpha <- 1.0
        res <- eval(substitute(lm( f, data=sdata ), list(f=formulas[[3]])))
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
                              isConv = TRUE  # always, because we use lm.
  )
  res$formula <- naturalCoeffs

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
  # This assumes that the model has coefficients named alpha/beta/gamma or that 
  # the paramters of interest are all but the first (intercept) and last (time).
  cf <- coef(model)
  if (any(c("alpha","beta","gamma") %in% names(cf) ) ) {
    cf <- cf[ names(cf) %in% c("alpha", "beta", "gamma") ]
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
    c("logscale",  "alpha", "beta", "lambda"),
    
    c("logscale", "alpha", "lambda"),
    c("logscale", "beta", "lambda"),
    c("logscale", "alpha", "lambda"),
    
    c("logscale", "lambda"),
    c("logscale", "lambda"),
    c("logscale", "lambda")
  )
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
  names( res$coefficients ) <- coefNames[[winner]]
  
  # Build the additional object to add as an atrribute to the output
  cf <- coef(res)
  naturalCoeffs <- data.frame(
    logscale= cf["logscale"],
    scale = exp( cf["logscale"] ),
    sse = sum(resid(res)^2),
    constrained = respectsConstraints(res),
    sse.constrained = if (respectsConstraints(res)) sse else Inf,
    isConv = TRUE, # always, because we use lm.
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
  res$formula <-  formula
  if (save.data) { res$data <- sdata }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  
  class(res) <- c( "CDEmodel", class(res) )
  return(res)
}

#' Add meta data to model object.
#'
#' This function adds metadata (\code{naturalCoeffs} and \code{meta}) to a model.  
#' Currently this is designed to work with CES models only. 
#' @param model the model to which metadata is to be added.
#' @param formula the original CES formula. Assumed to be of the form \code{y ~ k + l + e + t}.
#' @param nest the nesting for a CES model. Assumed to be of the form \code{c(1,2,3,4)}.
#' @param naturalCoeffs the \code{naturalCoeffs} object to be added as metadata.
#' @param history the fitting history to be attached to the meta attribute.
#' @note If no \code{naturalCoeffs} argument is specified, 
#' an attempt will be made to calculate a \code{naturalCoeffs} object. 
#' The calculated \code{naturalCoeffs} object will be added to the model.
#' @return \code{model} with two additional attributes, \code{naturalCoeffs} and \code{meta}.
#' 
addMetaData <- function(model, formula, nest, naturalCoeffs=NULL, history=""){
  if (is.null(model)){
    return(model) 
  }
  
  # cesEst is from the cesEst function. cesModel comes from constrained fits to CES.
  if ( ! any( c("cesEst", "cesModel") %in% class(model) ) ){
    stop(paste0("Unsupported model class: " , class(model) , ". model must be NULL, cesEst, or cesModel"))
  }
  
  if (is.null(naturalCoeffs)){
    # Need to calculate naturalCoeffs, because they weren't supplied.
    
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
      # In the no-energy situation, we have no way of knowing the value of rho.
      # So, assign the value of rho to be NA.
      rho <- NA
    } else {
      # This is the with-energy situation. Things are more straightforward.
      # delta_1 and sigma_1 are for the inner nest.
      # delta and rho are for the outer nest.
      delta_1 <- coef(model)["delta_1"]
      delta <- coef(model)["delta"]
      rho_1 <- coef(model)["rho_1"]
      rho <- coef(model)["rho"]
    }
    naturalCoeffs <- data.frame(# Variable name collision alert: there is a gamma coefficient
                                # in the CES model (gamma_coef) and a gamma calculated 
                                # from the delta values.
                                # gamma_coef is the coefficient in the CES model. It should be near 1.0.
                                # gamma is calculated from the delta values in the model.
                                # gamma is analogous to the gamma exponent on energy in the Cobb-Douglas model.
                                # And, gamma is the required name of the variable to be plotted with the ternary 
                                # plot function standardTriPlot.  
                                # (standardTriPlot assumes that one variable 
                                # is named "gamma", and it plots that variable.)  
                                # gamma_coef is in the naturalCoeffs attribute.
                                # gamma is in the meta attribute.
                                gamma_coef = as.vector(coef(model)["gamma"]),
                                lambda = as.vector(coef(model)["lambda"]),
                                delta_1 = as.vector(delta_1),
                                delta = as.vector(delta),
                                sigma_1 = as.vector(1 / (1 + rho_1)),
                                rho_1 = as.vector(rho_1),
                                sigma = as.vector(1 / (1 + rho)),
                                rho = as.vector(rho),
                                sse = sum(resid(model)^2)
    )    
  } else{
    # naturalCoeffs was supplied. Use it for delta and delta_1. 
    # These values will be used below for calculating alpha, beta, and gamma.
    delta <- naturalCoeffs$delta
    delta_1 <- naturalCoeffs$delta_1
  }

  sc <- standardCoefs(delta_1, delta, nest=nest)
  alpha <- sc$alpha
  beta <- sc$beta
  gamma <- sc$beta
  
  # Tell whether a grid search was used.
  grid <- length( intersect(c("rho", "rho1"), names(model$call) ) ) > 0
  
  # Get the nest information
  fNames <- cesParseFormula(formula, nest)

  ##########################################
  # There is probably a better way to do this.
  # But, for now, check the class of the model and react appropriately.
  ##########################################
  # Create the list of meta information.
  if (inherits(model, "cesEst")) {
    metaList <- list(  isConv = model$convergence,
                       algorithm = as.vector(model$method),
                       # The PORT algorthm returns a number. L-BFGS-B returns a list. Need to deal with both.
                       iter = as.vector(ifelse(is.list(model$iter), model$iter["function"], model$iter)),
                       grid = grid,
                       # alpha = as.vector(alpha),
                       # beta = as.vector(beta),
                       # gamma = as.vector(gamma),
                       # start.lambda = as.vector(model$start["lambda"]),
                       # start.delta_1 = as.vector(model$start["delta_1"]),
                       # start.rho_1 = as.vector(model$start["rho_1"]),
                       # start.gamma_coef = as.vector(model$start["gamma"]),
                       # start.delta = as.vector(model$start["delta"]),
                       # start.rho = as.vector(model$start["rho"]),
                       history=as.vector(history),
                       nestStr = fNames$nestStr,
                       nestStrParen = fNames$nestStrParen
    )
  } else if ("plm" %in% class(model)){
    metaList <- list(  isConv = model$converged, 
                       algorithm = paste("plm", row.names(model$optimization), sep="/", collapse=";"),
                       iter = paste(model$optimization$niter, collapse=";"),
                       grid = NA,
                       # alpha = alpha,
                       # beta = beta,
                       # gamma = gamma,
                       history = as.vector(paste0("boundary[", model$bname, "]")),   # store type of bondary model?
                       nestStr = fNames$nestStr,
                       nestStrParen = fNames$nestStrParen
    )
  } else {
    stop(paste("Unknown model class", class(model), "in addMetaData."))
  }
  
  # Boundary models may come in here with NULL items. Convert NULL to NA before creating the data.frame.
  metaList <- replace(metaList, unlist(lapply(metaList, is.null)), NA)
  metaData <- data.frame(metaList)
  
  if ( nrow(metaData) > 1 ) {
    warning( paste0("\nmetaData has ", nrow(metaData), " rows: ", paste(metaList$nestStr,history, sep="|")) )
    for (item in metaList) { 
      if ( length(item) > 1 ) {
        warning(paste0("\t", toString(item)))
      }
    }
  }
  attr(model, "naturalCoeffs") <- naturalCoeffs[1,]
  attr(model, "meta") <- metaData[1,] 
  
  return(model)
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
  
  # Build the additional object to add as an atrribute to the output
  a_0 <- coef(res)[2]
  a_1 <- coef(res)[3]
  c_t <- a_1 / a_0
  naturalCoeffs <- data_frame(
    logscale = as.vector(coef(res)[1]),
    scale = exp(as.vector(coef(res)[1])),
    a_0 = as.vector(a_0),
    a_1 = as.vector(a_1),
    c_t = as.vector(c_t),
    sse = as.vector(sum(resid(res)^2)),
    isConv = TRUE # Always, because we're fitting with lm.
  )
  attr(res, "naturalCoeffs") <- naturalCoeffs
  #  sdata <- subset(data, 
  #                  select= c( "iGDP","iEToFit","iK","iL","rho_k","rho_l"))
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  if (save.data) {
    res$data <- sdata
  }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  res$formula <- formula
  
  class(res) <- c("LINEXmodel", class(res))
  return(res)
}