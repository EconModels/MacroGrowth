
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

cesBoundaryModels <- function(formula, data, nest){
  # f <- formula  # to avoid problems while renaming is happening.
 
  # use same data for all models, even if some models could make use of more complete data. 
  d <- subset(data, select = intersect(all.vars(formula), names(data)))
  sdata <- data[complete.cases(d), unique(c(intersect(all.vars(formula), names(data)), names(data)))]
  
#   timeSeries <- cesTimeSeries(f=f, data=sdata, nest=nest)
#   y <- timeSeries$y
#   x1 <- timeSeries$x1
#   x2 <- timeSeries$x2
#   x3 <- timeSeries$x3
#   x4 <- timeSeries$x4
#   time <- timeSeries$time
  
  # could compute this differently to avoid cesTimeSeries()
  numFactors <- cesParseFormula(formula, nest)$numFactors
  if ( ! numFactors %in% 2:3 ) {
    stop("model must have 2 or 3 factors.")
  }
 
  if (numFactors >=3) { 
    formulaRosetta <- list(
      y = formula[[2]],
      capital = formula[[3]][[2]][[2]][[2]],
      labor = formula[[3]][[2]][[2]][[3]],
      energy = formula[[3]][[2]][[3]],
      time = formula[[3]][[3]]
    ) 
    # permute to reflect nesting
    names(formulaRosetta)[2:4] <- c("capital", "labor", "energy")[nest]
  } else {
    formulaRosetta <- list(
      y = formula[[2]],
      capital = formula[[3]][[2]][[2]],
      labor = formula[[3]][[2]][[3]],
      time = formula[[3]][[3]]
    ) 
    # permute to reflect nesting
    names(formulaRosetta)[2:3] <- c("capital", "labor")[nest]
  }
  
  # old names - new names 
  formulaTemplates <- 
    list( 
      "01-01" = log(y) - log(capital) ~ time,
      "02-02" = log(y) - log(labor) ~ time,
      "06-03" = log(y) - log(energy) ~ time,
      "03-04" = log(y) - log(pmin(capital, labor)) ~ time,
      "07-05" = log(y) - log(pmin(capital, energy)) ~ time,
      "08-06" = log(y) - log(pmin(labor, energy)) ~ time,
      "04-07" = log(y) - log(delta_1*capital + (1-delta_1)*labor) ~ time,
      "10-08" = log(y) - log(delta*capital + (1-delta)*energy) ~ time,
      "11-09" = log(y) - log(delta*labor + (1-delta)*energy) ~ time,
      "09-13" = log(y) - log(pmin(capital, labor, energy)) ~ time,
      "13-14" = log(y) - log( delta*pmin(capital, labor) + (1-delta)*energy ) ~ time,
      "14-15" = log(y) - log( pmin(delta_1*capital + (1-delta_1)*labor, energy)) ~ time,
      "12-16" = log(y) - log( delta*(delta_1*capital + (1-delta_1)*labor) + (1-delta)*energy) ~ time,
      "20-18" = log(y) - log( pmin((delta_1*capital^(-rho_1) + (1-delta_1)*labor^(-rho_1))^(-1/rho_1), energy) ) ~ time,
      "19-19" = log(y) - log( (delta * (delta_1*capital^(-rho_1) + (1-delta_1)*labor^(-rho_1))^(-1/rho_1) + (1-delta)*energy ) ) ~ time,
      "17-20" = log(y) - log( (delta * (delta_1*capital + (1-delta_1)*labor)^(-rho) + (1-delta)*energy^(-rho) ) ^ (-1/rho)) ~ time,
      "05-10" = y ~ capital + labor + time,
      "15-11" = y ~ capital + energy + time,
      "16-12" = y ~ labor + energy + time,
      "18-17" = y ~ pmin(capital, labor) + energy + time
    )
  # Values that don't appear in the model can be set to their fixed values and nlm()
  # will leave them alone.  
  # But nlm() doesn't like Inf even if the parameter isn't used, so when rho or rho_1 is Inf, we
  # set sigma or sigma_1, respectively, to 0 instead of setting rho or rho_1 to Inf.  
  # This will cost some effort down stream to recover the rho values.  See makeNatCoef.
  # There may be a better solution, but this will get us going for the moment.
  
  params <- list(
    "01-01" =  c(delta_1 = 1,   delta=1),
    "02-02" =  c(delta_1 = 0,   delta=1),
    "06-03" =  c(delta =   0),
    "03-04" =  c(sigma_1 = 0,   delta=1),
    "07-05" =  c(sigma =   0,   delta_1 = 1.0),
    "08-06" =  c(sigma =   0,   delta_1 = 0.0),
    "04-07" =  c(delta_1 = 0.5, delta =   1.0,   rho_1 = -1),
    "10-08" = c(delta =   0.5, delta_1=  1.0,     rho = -1),
    "11-09" = c(delta =   0.5, delta_1 = 0.0,     rho = -1),
    # 5
    # 15
    # 16
    "09-13" =  c(sigma =   0,   sigma_1 = 0.0),
    "13-14" = c(delta =   0.5, sigma_1 = 0.0,      rho = -1),
    "14-15" = c(delta_1 = 0.5, rho_1 =  -1.0,    sigma = 0),
    "12-16" = c(delta =   0.5, delta_1 = 0.5,   rho_1 = -1, rho = -1),
    # 18
    "20-18" = c(delta_1 = 0.5, rho_1 =   0.25,   sigma = 0.0),
    "19-19" = c(delta =   0.5, delta_1 = 0.5,    rho_1 = 0.25,   rho = -1),
    "17-20" = c(delta =   0.5, delta_1 = 0.5,    rho =   0.25, rho_1 = -1)
  ) 
  





  formulas <- 
    lapply(
      formulaTemplates, 
      function(ft)
        as.formula(do.call(substitute, list( ft, formulaRosetta) ) )
    )
  
  keep <- sapply(formulaTemplates, function(ft)  numFactors >=3 | !("energy" %in% all.vars(ft)))
  plmModel <- sapply(1:length(formulaTemplates), function(x) x <= length(params)) # params for plm() only
  

  # fit all the models
  plmModels <- 
    Map( 
      function(formula, param) { 
        res <- eval(substitute( plm(formula, data = sdata, param=param), list(formula=formula, param=param)))
        # for all of our models, log(gamma) and lambda are first two coefficients
        if (! is.null(res)) {
          names(res$coefficients)[1] <- "logscale"
          names(res$coefficients)[2] <- "lambda"
        }
        res
      }, 
      formulas[keep & plmModel], params[keep & plmModel]
    )
 
  cesModels <- 
    Map( 
      function(formula) {
        eval(substitute( 
          cesModel(formula, data = sdata,  nest = c(1,2), constrained = TRUE, fitBoundaries = FALSE), 
          list(formula=formula)
        ))
      },
      formulas[keep & !plmModel & (1:20 < 20)] # remove ugly case (20)
    )
  
  # Now handle the ugly case.  Since cesEst() can only work with variables, we need
  # to compute a variable and add it to the data frame before calling cesModel.

  if(keep[20]) {
    tryCatch( { 
      formulaPmin <- formulas[[20]] 
      pminVar <- deparse(formulaPmin[[3]][[2]][[2]])
      sdata[[pminVar]] <- eval(formulaPmin[[3]][[2]][[2]], sdata)
      cesModels <- 
        c(cesModels, 
          list("18-17" =  
            eval(substitute(
              cesModel(f, data=sdata, nest = c(1,2), constrained=TRUE, fitBoundaries=FALSE),
              list(f = formulaPmin)
            ))
          )
        )
    }, error = function(e) warning(e)
    )
  }
 
  o <-  order( substr(names(formulaTemplates[keep]), 4,5) )
  return( c(plmModels, cesModels)[o] )
}