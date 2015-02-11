#' Parameterized Linear Models
#' 
#' The optimal model is fit in a case where asside from some parameters, the model is linear.  
#' Numerical optimization is used over these parameters with \code{lm} fitting the rest.
#' @param formula a formula describing the model
#' @param data a data frame
#' @param params the parameters to be fit by numerical optimation outside of \code{lm}.
#' @param optimize a logical indicating whether \code{param} should be plugged in or used as the seed
#'   for the numerical optimization
#' @param .ocall used for recursive calling
#' @return On object of class \code{c("plm", "lm")} which is an enhanced \code{"lm"} object.
#' @export
#' @examples
#' if( require(EconData) & require(dplyr) ) {
#' plm( log(iGDP) - log(iK) ~ iYear, 
#'      data=Calvin %>% filter(Country=="US"))
#'      
#' plm( log(iGDP) - delta * iK - (1-delta) * iL ~ iYear, 
#'      data=Calvin %>% filter(Country=="US"), 
#'      p=c(delta=0.4), 
#'      optimize=FALSE ) 
#' 
#' plm( log(iGDP) - delta * iK - (1-delta) * iL ~ iYear, data=Calvin %>% filter(Country=="US"), 
#'      p=c(delta=0.4), 
#'      optimize=TRUE ) 
#' 
#' foo <- plm( log(iGDP / delta*(delta_1*iK + (1-delta_1)*iL) + (1-delta)*iQp) ~ iYear,
#'             data=Calvin %>% filter(Country=="US"), 
#'             p=c(delta=0.5, delta_1=0.5))
#' foo
#' class(foo)
#' }

plm <- function( formula, data=parent.frame(), params=c(), optimize=TRUE, .ocall=NULL ) {
  if(is.null(.ocall)) .ocall <- match.call()
 
  # if no params, just plain old lm with an extra class wrapper 
  if (length(params) < 1) { 
    res <- lm(formula, data=data)
    class(res) <- c("plm", class(res))
    res$call <- .ocall
    return(res)
  }
 
  # if we get here, we have params to deal with
  
  pnames <- names(params)
  if (! optimize) {
    np <- length(params)
    plist <- as.list(params) 
    names(plist) <- pnames
    f <- do.call(substitute, list(formula, plist))
    res <- eval(substitute(lm(f, data=data), list(f=f))) 
    res$coefficients <- c(coef(res), params)
    class(res) <- c("plm", class(res))
    res$call <- .ocall
    res
  } else {
    # find optimal params
    opt_params <- 
      nlmin( 
        function(p) {
          names(p) <- pnames
          mod <- plm(formula=formula, data=data, params=p, optimize=FALSE)
          sum(resid(mod)^2)
        },
        p = params
      )$estimate
    names(opt_params) <- pnames
    # return model fit with optimal params
    res <- plm(formula=formula, data=data, params=opt_params, optimize=FALSE, .ocall=.ocall)
    res
  }
}

