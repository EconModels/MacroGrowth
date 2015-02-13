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
    if (TRUE || length(params) < 2 ) {
      opt_params <- 
        nlmin( 
          function(p) {
            names(p) <- pnames
            mod <- plm(formula=formula, data=data, params=p, optimize=FALSE)
            sum(resid(mod)^2)
          },
          p = params
        )$estimate
    } else {
    opt_params <- 
      optim( 
        par = params,
        function(p) {
          names(p) <- pnames
          mod <- plm(formula=formula, data=data, params=p, optimize=FALSE)
          sum(resid(mod)^2)
        },
        method = "L-BFGS-B",
        lower = rep(0, length(params)),
        upper = rep(1, length(params))
      )$par
    }
    

    names(opt_params) <- pnames
    # return model fit with optimal params
    res <- plm(formula=formula, data=data, params=opt_params, optimize=FALSE, .ocall=.ocall)
    res
  }
}

#' Apply lm to (several) formula templates
#' 
#' This automates fitting several models that make use of the same variables and data set.
#' 
#' @param formula A formula containing the (common) response on the left and all variables
#' used in any of the models on the right.
#' @param data a data frame containing the necessary variables.
#' @param formulaTemplates a list of formulas using response \code{y} and 
#' predictors 
#' \code{capital},
#' \code{labor}, 
#' \code{energy}, 
#' and 
#' \code{time}. 
#' @param coefNames a list of names for the coefficients of the fitted model.
#' @param save.data a logical, currently not used.
#' @param ... additional arguments, currently not used.
#' @examples 
#' apply_lm( iGDP ~ iK + iL + iQp + iYear, data=Calvin, 
#'   formulaTemplates = list( 
#'     log(y) - log(capital) ~ time,
#'     log(y) - log(labor) ~ time,
#'     log(y) - log(energy) ~ time),
#'  coefNames = list(
#'    c("logscale", "lambda"),
#'    c("logscale", "lambda"),
#'    c("logscale", "lambda"))
#'  )
#' 

apply_lm <- function( formula, data, formulaTemplates, 
                      save.data=TRUE, ...){
  
  formulas <- lapply(
    formulaTemplates, 
    function(ft) {
      do.call( substitute, 
               list( ft,  list(
                 time = formula[[3]][[3]],
                 energy = formula[[3]][[2]][[3]],
                 labor = formula[[3]][[2]][[2]][[3]],
                 capital = formula[[3]][[2]][[2]][[2]],
                 y = formula[[2]]  
               ) )
      ) 
    }
  )
  
  models <- 
    lapply( formulas, 
            function(form){
#              d <- subset(data, select = all.vars(form))
#              sdata <- data[complete.cases(d), unique(c(all.vars(form), names(data)))]
              eval(substitute(lm(f, data=data), list(f=form)))  
            }
    )
  
  sse <- sapply( models, function(m) sum( resid(m)^2 ) )
#  for (i in 1:length(models)) {
#    names( models[[i]]$coefficients ) <- coefNames[[i]]
#  }
  return(list(models=models, sse=sse))
} 

# require(dplyr); require(EconData)
# 
# formulaTemplates <- 
#   list( 
#     log(y) - log(capital) ~ time,     # 1
#     log(y) - log(labor) ~ time,       # 2
#     log(y) - log(energy) ~ time,      # 6
#     log(y) - log(pmin(capital, labor)) ~ time,          #3
#     log(y) - log(pmin(capital, energy)) ~ time,         #7
#     log(y) - log(pmin(labor, energy)) ~ time,           #8
#     log(y) - log(pmin(capital, labor, energy)) ~ time,  #9
#     log(y) - log(delta_1*capital + (1-delta_1)*labor) ~ time,   #4
#     log(y) - log(delta*capital + (1-delta)*energy) ~ time,  #4
#     log(y) - log(delta*labor + (1-delta)*energy) ~ time     #4
#   )

# all the same, so don't bother with this.
# coefNames <- list( 
#   c("logscale", "lambda"),    #1
#   c("logscale", "lambda"),    #2
#   c("logscale", "lambda"),    #6
#   c("logscale", "lambda"),    #3
#   c("logscale", "lambda"),    #7
#   c("logscale", "lambda"),    #8
#   c("logscale", "lambda"),    #9
#   c("logscale", "lambda"),    #4
#   c("logscale", "lambda"),    #10
#   c("logscale", "lambda")     #11
# ) 

# params <- list(
#   c(),
#   c(),
#   c(),
#   c(),
#   c(),
#   c(),
#   c(),
#   c(delta_1 = 0.5),
#   c(delta = 0.5),
#   c(delta = 0.5)
# )
#
# apply_plm(iGDP ~ iK + iL + iQp + iYear, data=Calvin %>% filter(Country == "US"), 
#           formulaTemplates, params=params) 
#
# apply_plm(iGDP ~ iK + iL + iQp + iYear, data=Calvin %>% filter(Country == "US"), 
#           formulaTemplates[8], params=params[8]) 
#
# plm(formula = log(iGDP) - log(delta_1 * iK + (1 - delta_1) *  iL) ~ iYear, 
#     data = Calvin %>% filter(Country == "US"), params = c("delta_1" = 0.5))

apply_plm <- function( formula, data, formulaTemplates, 
                       params,
                       save.data=TRUE, ...){
  fun1 <- 
    function(formulaTemplate, params) {
      form <- 
        do.call(substitute, list( formulaTemplate, 
              list(
                time = formula[[3]][[3]],
                energy = formula[[3]][[2]][[3]],
                labor = formula[[3]][[2]][[2]][[3]],
                capital = formula[[3]][[2]][[2]][[2]],
                y = formula[[2]]  
              ) ) )
      
#      d <- subset(data, select = intersect(all.vars(form), names(data)))
#      sdata <- data[complete.cases(d), unique(c(intersect(all.vars(form), names(data)), names(data)))]
      mod <- eval(substitute(plm(f, data=data, params=p), list(f=form, p=params)))  
      # names(mod$coefficients) <- c(cNames, names(params))
      sse <- sum(resid(mod)^2)
      list(model=mod, sse=sse)
    }
  
  Map (fun1, 
       formulaTemplates, params
  )
} 
