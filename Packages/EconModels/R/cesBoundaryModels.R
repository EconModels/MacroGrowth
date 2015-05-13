
#' Fits CES boundary models
#' 
#' The boundary models are given in Table 2 of 
#' Heun, et al "An Empirical Investigation of the Role of Energy in Economic Growth"
#'
#' @param formula a CES formula in the form \code{y ~ a + b + c + d + time}
#' @param data historical time series data
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
#' @return a list of boundary models.
#' @examples 
#' if (require(EconData) & require(dplyr)) {
#'   cesBoundaryModels(iGDP ~ iK + iL + iQp + iYear, data=Calvin %>% filter(Country=="US"), nest=c(1,2,3))
#'   cesBoundaryModels(iGDP ~ iK + iL + iQp + iYear, data=Calvin %>% filter(Country=="US"), nest=c(2,3,1))
#'   cesBoundaryModels(iGDP ~ iK + iL + iYear, data=Calvin %>% filter(Country=="US"), nest=c(1,2))
#'   }
#' @export

cesBoundaryModels <- function(formula, data, nest, method="nlm", subset=TRUE){
  # f <- formula  # to avoid problems while renaming is happening.
 
  # use same data for all models, even if some models could make use of more complete data. 
  d <- subset(data, select = intersect(all.vars(formula), names(data)))
  sdata <- data[complete.cases(d), unique(c(intersect(all.vars(formula), names(data)), names(data)))]
  

  numFactors <- cesParseFormula(formula, nest)$numFactors
  if ( ! numFactors %in% 2:3 ) {
    stop("model must have 2 or 3 factors.")
  }
 
  if (numFactors >=3) { 
    formulaRosetta <- list(
      y = formula[[2]],
      x1 = formula[[3]][[2]][[2]][[2]],
      x2 = formula[[3]][[2]][[2]][[3]],
      x3 = formula[[3]][[2]][[3]],
      time = formula[[3]][[3]]
    ) 
    # permute to reflect nesting
    names(formulaRosetta)[2:4] <- c("x1", "x2", "x3")[nest]
  } else {
    formulaRosetta <- list(
      y = formula[[2]],
      x1 = formula[[3]][[2]][[2]],
      x2 = formula[[3]][[2]][[3]],
      time = formula[[3]][[3]]
    ) 
    # permute to reflect nesting
    names(formulaRosetta)[2:3] <- c("x1", "x2")[nest]
  }
  
  # old names - new names 
  formulaTemplates <- 
    list( 
      "01:1-1-" = log(y) - log(x1) ~ time,
      "02:0-1-" = log(y) - log(x2) ~ time,
      "03:--0-" = log(y) - log(x3) ~ time,
      "04:-01-" = log(y) - log(pmin(x1, x2)) ~ time,
      "05:1--0" = log(y) - log(pmin(x1, x3)) ~ time,
      "06:0--0" = log(y) - log(pmin(x2, x3)) ~ time,
      "07:*N1-" = log(y) - log(delta_1*x1 + (1-delta_1)*x2) ~ time,
      "08:1-*N" = log(y) - log(delta*x1 + (1-delta)*x3) ~ time,
      "09:0-*N" = log(y) - log(delta*x2 + (1-delta)*x3) ~ time,
      "13:-0-0" = log(y) - log(pmin(x1, x2, x3)) ~ time,
      "14:-0*N" = log(y) - log( delta*pmin(x1, x2) + (1-delta)*x3 ) ~ time,
      "15:*N-0" = log(y) - log( pmin(delta_1*x1 + (1-delta_1)*x2, x3)) ~ time,
      "16:*N*N" = log(y) - log( delta*(delta_1*x1 + (1-delta_1)*x2) + (1-delta)*x3) ~ time,
      "18:**-0" = log(y) - log( pmin((delta_1*x1^(-rho_1) + (1-delta_1)*x2^(-rho_1))^(-1/rho_1), x3) ) ~ time,
      "19:***N" = log(y) - log( (delta * (delta_1*x1^(-rho_1) + (1-delta_1)*x2^(-rho_1))^(-1/rho_1) + (1-delta)*x3 ) ) ~ time,
      "20:*N**" = log(y) - log( (delta * (delta_1*x1 + (1-delta_1)*x2)^(-rho) + (1-delta)*x3^(-rho) ) ^ (-1/rho)) ~ time,
      "10:**1-" = y ~ x1 + x2 + x3 + time,
      "11:1-**" = y ~ x1 + x2 + x3 + time,
      "12:0-**" = y ~ x1 + x2 + x3 + time,
      "17:-0**" = y ~ pmin(x1, x2) + placeholder + x3 + time
    )
  # Values that don't appear in the model can be set to their fixed values and nlm()
  # will leave them alone.  
  # But nlm() doesn't like Inf even if the parameter isn't used, so when rho or rho_1 is Inf, we
  # set sigma or sigma_1, respectively, to 0 instead of setting rho or rho_1 to Inf.  
  # This will cost some effort down stream to recover the rho values.  See makeNatCoef.
  # There may be a better solution, but this will get us going for the moment.
  
  params <- list(
    "01:1-1-" = c(delta_1 = 1,   delta=1),
    "02:0-1-" = c(delta_1 = 0,   delta=1),
    "03:--0-" = c(delta =   0),
    "04:-01-" = c(sigma_1 = 0,   delta=1),
    "05:1--0" = c(sigma =   0,   delta_1 = 1.0),
    "06:0--0" = c(sigma =   0,   delta_1 = 0.0),
    "07:*N1-" = c(delta_1 = 0.5, delta =   1.0,  rho_1 = -1),
    "08:1-*N" = c(delta =   0.5, delta_1=  1.0,     rho = -1),
    "09:0-*N" = c(delta =   0.5, delta_1 = 0.0,     rho = -1),
    # 5
    # 15
    # 16
    "13:-0-0" = c(sigma =   0,   sigma_1 = 0.0),
    "14:-0*N" = c(delta =   0.5, sigma_1 = 0.0,      rho = -1),
    "15:*N-0" = c(delta_1 = 0.5, rho_1 =  -1.0,    sigma =  0),
    "16:*N*N" = c(delta =   0.5, delta_1 = 0.5,    rho_1 = -1, rho = -1),
    # 18
    "18:**-0" = c(delta_1 = 0.5, rho_1 =   0.25,   sigma = 0.0),
    "19:***N" = c(delta =   0.5, delta_1 = 0.5,    rho_1 = 0.25,   rho = -1),
    "20:*N**" = c(delta =   0.5, delta_1 = 0.5,      rho = 0.25, rho_1 = -1)
  ) 
  

  formulas <- 
    lapply(
      formulaTemplates, 
      function(ft)
        as.formula(do.call(substitute, list( ft, formulaRosetta) ) )
    )
  
  keep <- sapply(formulaTemplates, function(ft)  numFactors >=3 | !("x3" %in% all.vars(ft)))
  plmModel <- sapply(1:length(formulaTemplates), function(x) x <= length(params)) # params for plm() only
  

  # fit all the models
  plmModels <- 
    Map( 
      function(formula, param) { 
        res <- eval(substitute( plm(formula, data = sdata, param = param, method = method), 
                                list(formula=formula, param=param, method=method)))
        # for all of our models, log(gamma) and lambda are first two coefficients
        if (! is.null(res)) {
          names(res$coefficients)[1] <- "logscale"
          names(res$coefficients)[2] <- "lambda"
        }
        res
      }, 
      formulas[keep & plmModel & subset], params[keep & plmModel & subset]
    )
 
  cesModels <- 
    Map( 
      function(formula, nest) {
        eval(substitute( 
          cesModel(formula, data = sdata,  nest = nest, constrained = TRUE, fitBoundaries = FALSE), 
          list(formula=formula)
        ))
      },
      formulas[keep & !plmModel & (1:20 < 20) & subset], # remove ugly case (20)
      list(c(1,2), c(1,3), c(2,3))[any(keep & !plmModel & (1:20 < 20) & subset)]  # adjust nest to leave one out
    )
  
  # Now handle the ugly case.  Since cesEst() can only work with variables, we need
  # to compute a variable and add it to the data frame before calling cesModel.

  if(keep[20]) {
    tryCatch( { 
      formulaPmin <- formulas[[20]] 
      pminVar <- deparse(formulaPmin[[3]][[2]][[2]][[2]])
      sdata[[pminVar]] <- eval(formulaPmin[[3]][[2]][[2]][[2]], sdata)
      cesModels <- 
        c(cesModels, 
          list("17:-0**" =  
            eval(substitute(
              cesModel(f, data=sdata, nest = c(1,3), constrained=TRUE, fitBoundaries=FALSE),
              list(f = formulaPmin)
            ))
          )
        )
    }, error = function(e) warning(e)
    )
  }
 
  o <-  order( names(formulaTemplates[keep]) )
  return( 
    Map(function(model, bd) { model$boundary <- bd; model },
        model = c(plmModels, cesModels)[o],
        bd = names(c(plmModels, cesModels)[o]) 
  ) )
}
