#' Parameterized Linear Models
#' 
#' The optimal model is fit in a case where aside from some parameters, the model is linear.  
#' Numerical optimization is used over these parameters with \code{lm} fitting the rest.
#' @param formula a formula describing the model
#' @param data a data frame
#' @param params a named vector of the parameters to be fit by numerical optimation outside of \code{lm}.
#' @param optimize a logical indicating whether \code{params} should be plugged in or used as the seed
#'   for the numerical optimization
#' @param .ocall used for recursive calling
#' @param ... additional arguments, currently ignored.
#' @return On object of class \code{c("plm", "lm")} which is an enhanced \code{"lm"} object.
#' @export
#' @examples
#' if( require(EconData) & require(dplyr) ) {
#' plm( log(iGDP) - log(iK) ~ iYear, 
#'      data=Calvin %>% filter(Country=="US"))
#'      
#' plm( log(iGDP) - delta * iK - (1-delta) * iL ~ iYear, 
#'      data=Calvin %>% filter(Country=="US"), 
#'      params=c(delta=0.4), 
#'      optimize=FALSE ) 
#' 
#' plm( log(iGDP) - delta * iK - (1-delta) * iL ~ iYear, data=Calvin %>% filter(Country=="US"), 
#'      params=c(delta=0.4), 
#'      optimize=TRUE ) 
#'      
#' plm( log(iGDP) - delta * iK - (1-delta) * iL ~ iYear, data=Calvin %>% filter(Country=="US"), 
#'      params=c(delta=0.4), method=c("nlm", "spg"),
#'      optimize=TRUE ) 
#' 
#' foo <- plm( log(iGDP / delta*(delta_1*iK + (1-delta_1)*iL) + (1-delta)*iQp) ~ iYear,
#'             data=Calvin %>% filter(Country=="US"), 
#'             params=c(delta=0.5, delta_1=0.5))
#' foo
#' class(foo)
#' }

plm <- function( formula, data=parent.frame(), params=c(), optimize=TRUE, .ocall=NULL, method="nlm", ..., debug=FALSE ) {
  if(is.null(.ocall)) .ocall <- match.call()
 
  orig_params <- params
  
  used_params_names <- intersect( names(params), all.vars(formula))
  unused_params_names <- setdiff( names(params), all.vars(formula))
  used_params <- params[used_params_names]
  unused_params <- params[unused_params_names]
  # print(list(used = used_params, unused=unused_params))
  
  # if no params, just plain old lm with an extra class wrapper 
  if (length(used_params) < 1) { 
    res <- lm(formula, data=data)
    class(res) <- c("plm", class(res))
    res$call <- .ocall
    res$converged <- TRUE
    res$coefficients <- c(coef(res), unused_params)
    return(res)
  }
 
  # if we get here, we have params to deal with
  
  pnames <- names(used_params)
  plist <- as.list(used_params) 
  
  if (! optimize) {
    np <- length(params)
    if (debug) print(paste(params, collapse=", "))
    res <- tryCatch({
      f <- do.call(substitute, list(formula, plist))
      res <- eval(substitute(lm(f, data=data), list(f=f, data=data)))
      res$coefficients <- c(coef(res), params)
      class(res) <- c("plm", class(res))
      res$call <- .ocall
      res
    }, error=function(e) {warning(e); NULL} 
    )
    return(res)
  } else { # find optimal params
    start.model <- plm(formula=formula, data=data, params=used_params, optimize=FALSE) 
    f <- function(p, nr = nrow(model.frame(start.model))) {
      names(p) <- pnames
      mod <- tryCatch(  
        plm(formula=formula, data=data, params=p, optimize=FALSE), 
        error = function(e) {
          warning(e); 
          NULL
        }
      )
      
      if(is.null(mod) || nrow(model.frame(mod)) < nr) {
        res <- 0.001 * .Machine$double.xmax 
      } else {
        res <- sum(resid(mod)^2)
      }
      
      if (length(res) != 1L || !is.numeric(res) || !is.finite(res)) {
        res <- 0.1 * .Machine$double.xmax
        # print(coef(mod))
        # cat("\n")
      }
      return(res)
    }
    opt_out <- optimx::optimx(par = params, fn = f, method = method, ... )
    best_opt <- which.min(opt_out$value)
    npar <- attr(opt_out[best_opt], "npar")
    opt_params <- unlist(opt_out[best_opt, seq_len(npar), drop=FALSE])
    
    if (debug) print(opt_params)
    # return model fit with optimal params
    if (opt_out$convcode[best_opt] == 0) {
      res <- plm(formula=formula, data=data, params=opt_params, optimize=FALSE, .ocall=.ocall)
      res$converged <- TRUE
    } else {
      res <- plm(formula=formula, data=data, params=orig_params, optimize=FALSE, .ocall=.ocall)
      res$converged <- FALSE
    }
    # don't think we need this since the unused_params will be among orig_params or opt_params
    # res$coefficients <- c(coef(res), unused_params)
    res$optimization <- opt_out
    res$start <- orig_params
    return(res)
  }
}


