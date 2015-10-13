

standardES <- function( sigma=NA, sigma_1=NA, sigma_2=NA, nest=1:3, standardize=TRUE) {
  if (standardize) {
    nest <- c(nest, c(5,5,5,5))  # add dummy slots so nest has length >= 4
    nest[1:2] <- sort(nest[1:2])
    nest[3:4] <- sort(nest[3:4])
    nest <- nest[nest < 5]      # remove dummy slots
  }
  
  if (length(nest) == 2L) {
    res <- data.frame(a=sigma_1)
    names(res) <- paste0("sigma_", nest[1], nest[2])
    return(res)
  }
  
  if (length(nest) == 3L) {
    res <- data.frame(a = sigma_1, b = sigma)
    names(res) <- c( paste0("sigma_", nest[1], nest[2]),
                     paste0("sigma_", nest[1], nest[2], "_", nest[3]) 
    )
    return(res)
  }
    res <- data.frame(a = sigma_1, b=sigma_2, c=sigma)
    names(res) <- c( paste0("sigma_", nest[1], nest[2]),
                     paste0("sigma_", nest[3], nest[4]),
                     paste0("sigma_", nest[1], nest[2], "_", nest[3], nest[4])
    )
    return(res)
}

# This function provides alpha_i values for the CES model.
# It converts from delta_1 and delta to alpha_i in a nest-aware way.
# The standardCoefs provide the importance of each parameter
# IN THE ORDER THEY APPEAR IN THE FORMULA,
# regardless of which nest you choose
# and regardless of how the values for delta and delta_1 turn out.
# The point here is that the values of delta and delta_1 are applicable to the permuted
# variable order during the fitting process.
# But, we want coefficients that apply to the parameters in the order they appeared
# in the formula. 
# standardCoefs does just that.
# Example: A formula is specified as y ~ a + b + c + time,
# nest is given as c(3,1,2) such that the parameters (during the estimation process)
# are permuted to y ~ c + a + b + time.
# Results will be the following:
# delta    delta_1    alpha_1   alpha_2    alpha_3
# 1        1          0         0          1
# 1        0          1         0          0
# 0        0 or 1     0         1          0
standardCoefs <- function(delta=NA, delta_1=NA, nest=NULL, digits=5) {
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
  modprod <- function(a, b) {  # want 0 * NA == NA * 0 == 0
    if (!is.na(a) && a==0) return(0)
    if (!is.na(b) && b==0) return(0)
    a * b
  }
  
#  res <-  switch(method,
#           "1" = list(delta * ldelta_1,  delta * (1.0 - ldelta_1), 1.0 - delta),   # NA, NA, NA
#           "2" = list(ldelta * delta_1,  ldelta * (1.0 - delta_1), 1.0 - delta),   # delta_1, 1-delta_1, NA
#           "3" = list(ldelta * delta_1,  ldelta * (1.0 - delta_1), 1.0 - ldelta),  # delta_1, 1-delta_1, 0
#           "4" = list(
#             modprod(delta, delta_1), 
#             modprod(delta, (1.0 - delta_1)), 
#             1.0 - delta)  
#    )
  res <- list( modprod(delta, delta_1), 
                 modprod(delta, (1.0 - delta_1)), 
                 1.0 - delta)  
  
  # invert the nest permutation
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
#' @param object the model object from which you want to extract the natural coefficients.
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
  
makeNatCoef <- function(object, nest=object$nest, ...) {
  coefList <- as.list(coef(object))
  if (inherits(object, "cesEst") && length(coefList) == 4) {
    # We disagree with the naming of the coefficients by cesEst when only two factors of production are involved.
    # cesEst calls the coefficients gamma, lambda, delta, and rho.
    # Depending on nest, we sometimes prefer gamma, lambda, delta_1 and rho_1, 
    # because this case provides, essentially, the inner nest.
    # This code appends "_1" to the appropriate names and adds appropriate delta, rho, and sigma values.
    # We chose to rename the coefficients based on position. 
    # We could have renamed by name to defend against position changes in cesEst.
    # But, cesEst could also change names in the future, so there is no clear benefit to renaming by name.
    # So, we'll stick with renaming by position.
    if (all(nest <= 2)) { # nest = 1 2 or 2 1
      names(coefList)[3:4] <- paste0(names(coefList)[3:4], "_1")
      coefList[["delta"]] <- 1
      coefList[["sigma"]] <- NA
      coefList[["rho"]] <- NA
    } else {
      if (any(nest == 1)) { # nest = 1 3 or 3 1
        coefList[["delta_1"]] <- 1
        coefList[["sigma_1"]] <- NA
        coefList[["rho_1"]] <- NA
      } else { # nest = 2 3 or 3 2
        coefList[["delta_1"]] <- 0
        coefList[["sigma_1"]] <- NA
        coefList[["rho_1"]] <- NA
      }
    }
    
    # special treatment for boundary model problem child
    # in this case alpha_1 and alpha_2 don't make sense because we are creating
    # a new first factor as pmin(x1, x2)
    if (!is.null(object$boundary) && grepl("^17:", object$boundary)) {
      coefList[["delta_1"]] <- NA
      coefList[["sigma_1"]] <- 0
    }
  }
  # We also prefer the name "scale" for the pre-multiplier coefficient.
  # cesEst gives this as "gamma"
#   gamma_coef <-  tryCatch(with(coefList, exp(logscale)), error=function(e) NA)
#   if (is.na(gamma_coef)) gamma_coef <- coefList$gamma
#   if (is.null(gamma_coef)) gamma_coef <- NA
  scale <-  tryCatch(with(coefList, exp(logscale)), error=function(e) NA)
  if (is.na(scale)) scale <- coefList$gamma
  if (is.null(scale)) scale <- NA
  lambda <-  tryCatch(with(coefList, lambda), error=function(e) NA)
  delta_1 <-  tryCatch(with(coefList, delta_1), error=function(e) NA)
  delta <-  tryCatch(with(coefList, delta), error=function(e) NA)
  rho_1 <-  tryCatch(with(coefList, rho_1), error=function(e) NA)
  sigma_1 <-  tryCatch(with(coefList, sigma_1), error=function(e) NA)
  rho <-  tryCatch(with(coefList, rho), error=function(e) NA)
  sigma <-  tryCatch(with(coefList, sigma), error=function(e) NA)
  # We no longer include sse in naturalCoef, because sse is not a coefficient
  # sse <-  sum(resid(object)^2)
  if (is.null(nest)) { nest <- 1:3 }

  sc <- standardCoefs(delta_1=delta_1, delta=delta, nest=nest)
  
  res <- 
    as.data.frame( 
      dplyr::data_frame( 
        scale = scale,
        logscale = log(scale),
        lambda = lambda,
        delta = delta,
        delta_1 = delta_1,
        sigma_1 = if (is.na(sigma_1)) 1/(1 + rho_1) else sigma_1,
        rho_1 = if (is.na(rho_1)) 1/sigma_1 - 1 else rho_1,
        sigma = if (is.na(sigma)) 1/(1 + rho) else sigma,
        rho = if (is.na(rho)) 1/sigma - 1 else rho,
        alpha_1 = sc$alpha_1,
        alpha_2 = sc$alpha_2,
        alpha_3 = sc$alpha_3
      )
  )
  
 bind_cols( res, standardES(sigma = res$sigma, sigma_1 = res$sigma_1, nest=nest))
}

#' Fitting history of a model
#' 
#' An convenience function that returns the fitting history for a model.
#' @param model the model object from which you want to extract the \code{history}.
#' @return the fitting history for a model.
#' @export
getHistory <- function(model) {
  model$history
}

#' Extract data associated with an object
#' 
#' Extract data associated with an object
#' @param object an object from which you want to extract data.
#' @return data associated with \code{object}.
#' @export
getData <- function(object, ...) {
  UseMethod("getData")
}

#' @export
getData.default <- function(object, ...) {
  object$data
}

#' @export
getData.SFmodel <- function(object, ...) {
  object$data
}

#' @export
getData.CDEmodel <- function(object, ...) {
  object$data
}

#' @export
getData.LINEXmodel <- function(object, ...) {
  object$data
}

#' @export
getData.cesModel <- function(object, ...) {
  object$data
}

#' @export
getData.lm <- function(object, full = TRUE, ...) {
  if (full) {
    eval(object$call[["data"]], environment(object$call[["formula"]]))
  } else {
    model.frame(object, ...)
  }
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
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), getData(object))
  exp( fitted(object,...) + lx0[!is.na(lx0)] )
}

#' @export
yhat.LINEXmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), getData(object))
  exp( fitted(object, ...) + lx0[!is.na(lx0)] )
}

#' @export
yhat.SFmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), getData(object))
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
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), getData(object))
  exp( NextMethod() + lx0[!is.na(lx0)] )
}

#' @export
predict.LINEXmodel <- function( object, ... ) {
  # model has form log(y) - log(x_0) ~ iYear + I(log x_1 - log x_0) + ... + I(log(x_k) - log(x_0))
  lx0 <- eval( parse( text = gsub( ".* - ", "", names(object$model)[1]) ), getData(object))
  exp( NextMethod() + lx0[!is.na(lx0)] )
}


#' Fitting single factor models
#' 
#' @param formula a formula of the form \code{ y ~ factor + time }
#' @param data a data frame in which \code{formula} is evaluated
#' @param constrained a logical indicating whether the model parameters are constrained
#' @param correlation an optional \code{\link{corStruct}} object describing the 
#'   within-group correlation structure. See the documentation of \code{\link{corClasses}}
#'   for a description of the available \code{corStruct} classes. 
#'   If a grouping variable is to be used, 
#'   it must be specified in the \code{form} argument to the \code{corStruct} constructor. 
#'   Defaults to \code{NULL}, corresponding to uncorrelated errors.
#' @return an lm object with some additional attributes
#' @examples
#' US <- subset(Calvin, Country=="US")
#' sfModel(response = iGDP, factor=iK, time = iYear, data=US)
#' sfModel(iGDP ~ iK + iYear, data=US)
#' sfModel(response="iGDP", factor="iK", time="iYear", data=US)
#' 
#' @export
sfModel <- function(formula, data, response, factor, time, constrained=FALSE,
                              save.data=TRUE, correlation = NULL) {
  
  if ( missing(formula) ) {
    formula <- substitute( response ~ factor + time,
                           list( response = as_name_or_null(response),
                                 factor = as_name_or_null(factor),
                                 time = as_name_or_null(time)
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
    res <- eval( substitute(gls(f, data=data, correlation = C), 
                            list(C = correlation, f=formulas[[2]])) )
    res$winner <- 2
    logscale <- coef(res)[1]
    lambda <- coef(res)[2]
  } else {
    res <- eval( substitute(gls(f, data=data, correlation = C), 
                            list(C = correlation, f=formulas[[1]])) )
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
#' \code{response ~ x1 + x2 + time} or 
#' \code{response ~ x1 + x2 + x3 + time}
#' @param data a data frame in which \code{formula} is evaluated
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param x1 instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param x2 instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param x3 instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param constrained a logical indicating whether the parameters should be constrained in the fitting process.
#' @return a CDEmodel object, which is an lm object with some additioanl attributes.
#' @examples
#' US <- subset(Calvin, Country=="US")
#' cdModel(response = iGDP, x1 = iK, x2 = iL, time = iYear, data=US)
#' cdModel(iGDP ~ iK + iL + iYear, data=US)
#' cdModel(response = iGDP, x1 = iK, x2 = iL, x3 = iQp, time = iYear, data=US)
#' cdModel(iGDP ~ iK + iL + iQp + iYear, data=US)
#' cdModel(response="iGDP", x1="iK", x2="iL", x3="iQp", time="iYear", data=US)
#' 
#' @export
cdModel <- function(formula, data, response, x1, x2, x3, time, 
                             constrained=TRUE, save.data=TRUE, 
                             corellation = correlation, ...) {
  if (missing(formula)) {
    if (missing(x3)) {
      return( cd2Model( data=data, 
                          response=response, 
                          x1=x1, 
                          x2=x2, 
                          time=time, 
                          constrained = constrained, save.data=save.data, 
                          correlation = correlation, ...) )
    } else {
      return( cd3Model( data=data, 
                        response=response, 
                        x1=x1, 
                        x2=x2,
                        x3=x3,
                        time=time, 
                        constrained = constrained, save.data=save.data, 
                        correlation = correlation, ...) )
    }
  }
  
  if (ncol( attr(terms(formula),"factors") ) == 3 ) {
    # formula contains response, x1, and x2
    cd2Model( formula=formula, data=data, constrained = constrained, 
              save.data=save.data, correlation = correlation, ... )
  } else {
    # formula contains response, x1, x2, and x3
    cd3Model( formula=formula, data=data, constrained = constrained, save.data=save.data, 
              correlation = correlation, ... )
  }
}

# These formulas provide C-D models at all boundaries.
# Also, these formulas all assume constant returns to scale.
CDformulas <- list( 
  log(y) - log(x3) ~ 
    I(log(x1) - log(x3)) + I(log(x2) - log(x3)) + time,  # [[1]] Full model
  
  log(y) - log(x2) ~ I(log(x1) - log(x2)) + time,            # [[2]] With x3 exponent = 0
  log(y) - log(x3) ~ I(log(x1) - log(x3)) + time,            # [[3]] With x2 exponent = 0
  log(y) - log(x3) ~ I(log(x2) - log(x3)) + time,            # [[4]] With x1 exponent = 0
  
  log(y) - log(x1) ~ time,  # [[5]] With x1 exponent = 1, x2 and x3 exponents = 0
  log(y) - log(x2) ~ time,  # [[6]] With x2 exponent = 1, x1 and x3 exponents = 0
  log(y) - log(x3) ~ time   # [[7]] With x3 exponent = 1, x1 and x2 exponents = 0
)

CDcoefNames <- list( 
  c("logscale",  "alpha_1", "alpha_2", "lambda"), # [[1]] Full model
  
  c("logscale", "alpha_1", "lambda"), # [[2]] With x3 exponent = 0
  c("logscale", "alpha_1", "lambda"), # [[3]] With x2 exponent = 0
  c("logscale", "alpha_2", "lambda"), # [[4]] With x1 exponent = 0
  
  c("logscale", "lambda"), # [[5]] With x1 exponent = 1, x2 and x3 exponents = 0
  c("logscale", "lambda"), # [[6]] With x2 exponent = 1, x1 and x3 exponents = 0
  c("logscale", "lambda")  # [[7]] With x3 exponent = 1, x1 and x2 exponents = 0
)

#' Fitting Cobb-Douglas Models 
#' 
#' @param formala a formual of the form \code{response ~ x1 + x2 + time}
#' @param data a data fram in which \code{formula} is evaluated
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param x1 instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param x2 instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param constrained a logical indicating whether the parameters are constrained
#' @param correlation an optional \code{\link{corStruct}} object describing the 
#'   within-group correlation structure. See the documentation of \code{\link{corClasses}}
#'   for a description of the available \code{corStruct} classes. 
#'   If a grouping variable is to be used, 
#'   it must be specified in the \code{form} argument to the \code{corStruct} constructor. 
#'   Defaults to \code{NULL}, corresponding to uncorrelated errors.
#' @return a CDEmodel object, which is an lm object with some additional attributes.
#' @examples
#' US <- subset(Calvin, Country=="US")
#' cd2Model(response = iGDP, x1 = iK, x2 = iL, time = iYear, data=US)
#' cd2Model(iGDP ~ iK + iL + iYear, data=US)
#' cd2Model(response = "iGDP", x1="iK", x2="iL", time="iYear", data=US)
#' 
cd2Model <- function(formula, data, response, x1, x2, time, constrained=TRUE, 
                       save.data=TRUE, correlation = NULL, ...) {
  if ( missing(formula) ) {
    formula <- substitute( response ~ x1 + x2 + time,
                           list( response = as_name_or_null(response),
                                 x1 = as_name_or_null(x1),
                                 x2 = as_name_or_null(x2),
                                 time = as_name_or_null(time)
                           )
    )
  }
  
  sdata <- subset(data, select=all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
# we will only look at formulas[[c(2, 5, 6)]] below; but keep all for indexing convenience
  
  formulas <- lapply( CDformulas, 
                      function(x) do.call(substitute, 
                                          list(x,
                                               list( y = formula[[2]],
                                                     x1 = formula[[3]][[2]][[2]],
                                                     x2 = formula[[3]][[2]][[3]],
                                                     time = formula[[3]][[3]]
                                               )
                                          )
                      )
  )
  res <- eval(substitute(gls( f, correlation = C, data=sdata ), 
                         list(C = correlation, f=formulas[[2]])))
  winner <- 2
  names(res$coefficients) <- CDcoefNames[[2]]
  alpha_1 <- coef(res)["alpha_1"]
  if (constrained){
    # Adjust alpha_1 if we are outside the range 0 <= alpha_1 <= 1
    if (alpha_1 < 0.0){
      alpha_1 <- 0.0
      winner <- 6
    } else if (alpha_1 > 1) {
      alpha_1 <- 1.0
      winner <- 5
    }
    if (winner > 2) {  
      res <- eval(substitute(gls( f, data=sdata, correlation = C ), 
                             list(C = correlation, f=formulas[[winner]])))
      names(res$coefficients) <- CDcoefNames[[winner]]
    }
  }
  if (save.data) {
    res$data <- sdata
  }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  res$formula <- formula
  res$winner <- winner
  
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

#' Fitting Cobb-Douglas models with 3 Factors
#' 
#' @param formula a formula of the form \code{response ~ x1 + x2 + x3 + time}
#' @param data a data frame in which \code{formala} is evaluated
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param x1 instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param x2 instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param x3 instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually as character strings.
#' @param constrained a logical indicated whether the coefficents are constrained. See details
#' @param correlation an optional \code{\link{corStruct}} object describing the 
#'   within-group correlation structure. See the documentation of \code{\link{corClasses}}
#'   for a description of the available \code{corStruct} classes. 
#'   If a grouping variable is to be used, 
#'   it must be specified in the \code{form} argument to the \code{corStruct} constructor. 
#'   Defaults to \code{NULL}, corresponding to uncorrelated errors.
#' @param \dots additional arguments; currently unused.
#' @details More about contranints TBA.
#' @examples
#' US <- subset(Calvin, Country=="US")
#' cd3Model(response = iGDP, x1 = iK, x2 = iL, x3 = iQp, time = iYear, data=US)
#' cd3Model(iGDP ~ iK + iL + iQp + iYear, data=US)
#' cd3Model(response = "iGDP", x1="iK", x2="iL", x3="iQp", time="iYear", data=US)


# y ~ x1 + x2 + x3 + time
cd3Model <- function( formula, data, response, x1, x2, x3, time, 
                      constrained=TRUE, save.data=TRUE, 
                      correlation = NULL,
                      ...){
  
  if ( missing(formula) ) {
    formula <- substitute( response ~ x1 + x2 + x3 + time,
                           list( response = as_name_or_null(response),
                                 x1 = as_name_or_null(x1),
                                 x2 = as_name_or_null(x2),
                                 x3 = as_name_or_null(x3),
                                 time = as_name_or_null(time)
                           )
    )
  }
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  
  
  formulas <- lapply(CDformulas, function(x) do.call( substitute, list( x,  list(
    time = formula[[3]][[3]],
    x3 = formula[[3]][[2]][[3]],
    x2 = formula[[3]][[2]][[2]][[3]],
    x1 = formula[[3]][[2]][[2]][[2]],
    y = formula[[2]]  
  ) 
  ) ) )
  
  models <- lapply( formulas, function(form)
    eval(substitute(gls(f, correlation = C, data=sdata), list(f=form, C = correlation)))  
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
#' @param formula a formula of the form \code{response ~ x1 + x2 + x3 + time}
#' @param response instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param x1 instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param x2 instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param x3 instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param time instead of specifying a formula, expressions for
#' the components can be specified individually.
#' @param data a data frame in which \code{formula} is evaluated
#' @param correlation an optional \code{\link{corStruct}} object describing the 
#'   within-group correlation structure. See the documentation of \code{\link{corClasses}}
#'   for a description of the available \code{corStruct} classes. 
#'   If a grouping variable is to be used, 
#'   it must be specified in the \code{form} argument to the \code{corStruct} constructor. 
#'   Defaults to \code{NULL}, corresponding to uncorrelated errors.
#' @examples
#' US <- subset(Calvin, Country=="US")
#' linexModel(iGDP ~ iK + iL + iQp + iYear, data=US)
#' linexModel(response = "iGDP", x1="iK", x2="iL", x3="iQp", time="iYear", data=US)
#' naturalCoef(linexModel(response = "iGDP", x1="iK", x2="iL", x3="iQp", time="iYear", data=US))
#' 
#' @export
#' 
linexModel <- function(formula, data, response, x1, x2, x3, time, save.data=TRUE,
                       correlation = NULL) {
  if ( missing(formula) ) {
    formula <- substitute( response ~ x1 + x2 + x3 + time,
                           list( response = as_name_or_null(response),
                                 x1 = as_name_or_null(x1),
                                 x2 = as_name_or_null(x2),
                                 x3 = as_name_or_null(x3),
                                 time = as_name_or_null(time)
                           )
    )
  }
  formulas <- list( log(y) - log(x3) ~  
                      I(2 * (1 - 1/(x1 / ( .5 * (x3 + x2) ) )) )  + 
                      I( x2/x3 - 1 )
  )
  
  
  formulas <- lapply(formulas, function(x) do.call( substitute, list( x,  list(
    time = formula[[3]][[3]],
    x3 = formula[[3]][[2]][[3]],
    x2 = formula[[3]][[2]][[2]][[3]],
    x1 = formula[[3]][[2]][[2]][[2]],
    y = formula[[2]]  
  ) 
  ) ) )
  
  res <- eval( substitute(gls(f, data=data, correlation = C), 
                          list(C = correlation, f=formulas[[1]])) )
  
  sdata <- subset(data, select = all.vars(formula))
  sdata <- data[complete.cases(sdata), unique(c(all.vars(formula), names(data)))]
  if (save.data) { res$data <- sdata }
  res$response <- eval( formula[[2]], sdata, parent.frame() )
  res$formula <- formula
  
  class(res) <- c("LINEXmodel", class(res))
  return(res)
}
