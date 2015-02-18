
#' @export
#' 
getNatCoef <- function( model ) {
  coefList <- as.list(coef(model))
  gamma_coef = tryCatch(with(coefList, exp(logscale)), error=function(e) NA)
  if (is.na(gamma_coef)) gamma_coef <- coefList$gamma
  if (is.null(gamma_coef)) gamma_coef <- NA
  lambda = tryCatch(with(coefList, lambda), error=function(e) NA)
  delta_1 = tryCatch(with(coefList, delta_1), error=function(e) NA)
  delta = tryCatch(with(coefList, delta), error=function(e) NA)
  rho_1 = tryCatch(with(coefList, rho_1), error=function(e) NA)
  sigma_1 = tryCatch(with(coefList, sigma_1), error=function(e) NA)
  rho = tryCatch(with(coefList, rho), error=function(e) NA)
  sigma = tryCatch(with(coefList, sigma), error=function(e) NA)
  sse = sum(resid(model)^2)
  
  data_frame( gamma_coef = gamma_coef,
     lambda = lambda,
     delta = delta,
     delta_1 = delta_1,
     sigma_1 = if (is.na(sigma_1)) 1/(1 + rho_1) else sigma_1,
     rho_1 = if (is.na(rho_1)) 1/sigma_1 - 1 else rho_1,
     sigma = if (is.na(sigma)) 1/(1 + rho) else sigma,
     rho = if (is.na(rho)) 1/sigma - 1 else rho,
     sse = sse,
     constrained =
       (is.na(delta) || (0 <= delta && delta <= 1)) &&
       (is.na(delta_1) || (0 <= delta_1 && delta_1 <= 1)) &&
       (is.na(rho) || rho >= -1) &&
       (is.na(rho_1) || rho_1 >= -1) 
       ,
     sse.constrained =
       if(constrained) sse else Inf,
     call = Reduce(paste, gdata::trim(deparse(model$call)))
  )
}

    
#' Fits CES boundary models
#' 
#' The boundary models are given in Table 2 of 
#' Heun, et al "An Empirical Investigation of the Role of Energy in Economic Growth"
#'
#' @param formula a CES formula in the form \code{y ~ a + b + c + d + time}
#' @param data historical time series data
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
# #' @param id the identification number (from Table 2) for the boundary model you want to fit.
#' @return a model object with class \code{CESModel} and \code{naturalCoeffs} and \code{meta} attributes.
#' @note the \code{naturalCoeffs} attribute includes correct values of boundary parameters. 
#' \code{NA} values in \code{naturalCoeffs} indicate that the parameter is unknowable at that boundary.
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
  
  formulaTemplates <- 
    list( 
      "1"  = log(y) - log(capital) ~ time,
      "2"  = log(y) - log(labor) ~ time,
      "6"  = log(y) - log(energy) ~ time,
      "3"  = log(y) - log(pmin(capital, labor)) ~ time,
      "7"  = log(y) - log(pmin(capital, energy)) ~ time,
      "8"  = log(y) - log(pmin(labor, energy)) ~ time,
      "9"  = log(y) - log(pmin(capital, labor, energy)) ~ time,
      "4"  = log(y) - log(delta_1*capital + (1-delta_1)*labor) ~ time,
      "10" = log(y) - log(delta*capital + (1-delta)*energy) ~ time,
      "11" = log(y) - log(delta*labor + (1-delta)*energy) ~ time,
      "12" = log(y) - log( delta*(delta_1*capital + (1-delta_1)*labor) + (1-delta)*energy) ~ time,
      "13" = log(y) - log( delta*pmin(capital, labor) + (1-delta)*energy ) ~ time,
      "14" = log(y) - log( pmin(delta_1*capital + (1-delta_1)*labor, energy)) ~ time,
      "17" = log(y) - log( (delta * (delta_1*capital + (1-delta_1)*labor)^(-rho) + (1-delta)*energy^(-rho) ) ^ (-1/rho)) ~ time,
      "19" = log(y) - log( (delta * (delta_1*capital^(-rho_1) + (1-delta_1)*labor^(-rho_1))^(-1/rho_1) + (1-delta)*energy ) ) ~ time,
      "20" = log(y) - log( pmin((delta_1*capital^(-rho_1) + (1-delta_1)*labor^(-rho_1))^(-1/rho_1), energy) ) ~ time,
      "5"  = y ~ capital + labor + time,
      "15" = y ~ capital + energy + time,
      "16" = y ~ labor + energy + time,
      "18" = y ~ pmin(capital, labor) + energy + time
    )
  # Values that don't appear in the model can be set to their fixed values and nlm()
  # will leave them alone.  
  # But nlm() doesn't like Inf even if the parameter isn't used, so when rho or rho_1 is Inf, we
  # set sigma or sigma_1 to 0 instead of rho or rho_1 to Inf.  
  # This will cost some effort down stream to recover the rho values.  See getNatCoefs.
  # There may be a better solution, but this will get us going for the moment.
  
  params <- list(
    "1" = c(delta_1 = 1, delta=1),
    "2" = c(delta_1 = 0, delta=1),
    "6" = c(delta = 0),
    "3" = c(sigma_1 = 0, delta=1),
    "7" = c(sigma = 0, delta_1 = 1),
    "8" = c(sigma = 0, delta_1 = 0),
    "9" = c(sigma = 0, sigma_1 = 0),
    "4" = c(delta_1 = 0.5, delta = 1, rho_1 = -1),
    "10" = c(delta = 0.5, delta_1=1, rho = -1),
    "11" = c(delta = 0.5, delta_1 = 0, rho = -1),
    "12" = c(delta = 0.5, delta_1 = 0.5, rho_1 = -1, rho = -1),
    "13" = c(delta = 0.5, sigma_1 = 0,  sigma = 0),
    "14" = c(delta_1 = 0.5, rho_1 = -1, sigma = 0),
    "17" = c(delta = 0.5, delta_1 = 0.5, rho = 0.25, rho_1 = -1),
    "19" = c(delta = 0.5, delta_1 = 0.5, rho_1 = 0.25, rho = -1),
    "20" = c(delta_1 = 0.4, rho_1=0.35, sigma = 0)
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
        names(res$coefficients)[1] <- "logscale"
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
          list( 
            eval(substitute(
              cesModel(f, data=sdata, nest = c(1,2), constrained=TRUE, fitBoundaries=FALSE),
              list(f = formulaPmin)
            ))
          )
        )
    }, error = function(e) print(e)
    )
  }
  
  return( c(plmModels, cesModels) )
}

# 
# #' Extracts y, x1, x2, x3, x4, and time data as time series for CES boundary models
# #' 
# #' Given formula \code{f} and nest \code{nest}, this function
# #' extracts a response variable (\code{y}), 
# #' factors of production (\code{x1}, \code{x2}, \code{x3}, and \code{x4}), and 
# #' a time variable (\code{time}) from \code{data}.
# #' All extracted variables are time series vectors.
# #'
# #' @param f the CES formula for which time series data is to be extracted, 
# #' assumed to be of the form \code{y ~ x1 + x2 + x3 + x4 + time}.
# #' @param data the data frame from which time series data is to be extracted
# #' @param nest identifies the nesting of the variables in the original formula
# #' and should be a vector containing a permuation of the integers 1 through k,
# #' where k is the number of non-time explanatory variables in the model formula.
# #' @return a named list of time series'. 
# #' Names are \code{y}, \code{x1}, \code{x2}, \code{x3}, \code{x4}, and \code{time}.
# cesTimeSeries <- function(f, data, nest){
#   fNames <- cesParseFormula(f, nest)
#   # Extract variables for convenience.
#   numFactors <- fNames$numFactors
#   if (numFactors < 2 || numFactors > 4){
#     stop(paste0("numFactors = " + numFactors + " in cesTimeSeries. Should have 2 <= numFactors <= 4."))
#   }
#   cesNames <- fNames$cesNames
#   yName <- fNames$yName
#   xNames <- fNames$xNames
#   tName <- fNames$tName
#   
#   # Extract variales from data.
#   y <- eval(substitute(data$y, list(y = yName)))
#   time <- eval(substitute(data$time, list(time = tName)))
#   x1 <- eval(substitute(data$colx1, list(colx1 = xNames[[1]])))
#   x2 <- eval(substitute(data$colx2, list(colx2 = xNames[[2]])))
#   if (numFactors == 2){
#     return(list(y = y, x1=x1, x2=x2, x3=NA, x4=NA, time=time))
#   }
#   if (numFactors == 3){
#     x3 <- eval(substitute(data$colx3, list(colx3 = xNames[[3]])))
#     return(list(y = y, x1=x1, x2=x2, x3=x3, x4=NA, time=time))
#   }
#   if (numFactors == 4){
#     x4 <- eval(substitute(data$colx4, list(colx4 = xNames[[4]])))
#     return(list(y = y, x1=x1, x2=x2, x3=x3, x4=x4, time=time))
#   }
# }
# 
# #' Tells whether variables are always similarly ordered across all observations.
# #' 
# #' @param data a data frame that contains rows that are to be compared for ordering
# #' @return \code{TRUE} if all rows are ordered the same, \code{FALSE} if they are not.
# #' @note If all rows are comprised of equal numbers, \code{FALSE} is returned.
# #' @export
# rowsSameOrdered <- function(data){
#   # Look at all the rows. 
#   # Make a vector with length equal to the number of rows in data whose
#   # values are TRUE if there was at least two numbers equal on the row and FALSE if all numbers were unique.
#   somethingEqualOnRow <- apply(data, 1,
#                                function(row){
#                                  if (length(row) < 2){
#                                    stop(paste("row has length", length(row), "in rowsSameOrdered. length must be >= 2"))
#                                  }
#                                  # row is each row of data.
#                                  for (i in 1:(length(row)-1)){
#                                    for (j in (i+1):length(row)){
#                                      if (row[i] == row[j]){
#                                        return(TRUE)
#                                      }
#                                    }
#                                  }
#                                  return(FALSE)
#                                })
#   # Find the index of the first row contining unique numbers (no duplicates)
#   iCompRow <- match(FALSE, somethingEqualOnRow)
#   if (is.na(iCompRow)){
#     # No rows have differing data. Return false.
#     return(FALSE)
#   }
#   # Use the first unique row as our comparison row.
#   compRow <- data[iCompRow, ]
#   # Find the order for compRow
#   oCompRow <- order(compRow)
#   # Now, check to see if every other row has a similar order.
#   order <- apply(data, 1, 
#                  function(row){
#                    # row is each row of data.
#                    # arrange row in least-to-greatest order of compRow.
#                    reorderedRow <- row[oCompRow]
#                    # If reorderedRow is itself in least-to-greatest order, it has same ordering as compRow.
#                    for (j in 2:length(reorderedRow)){
#                      if (reorderedRow[j-1] > reorderedRow[j]){
#                        # Didn't have least-to-greatest order when arranged as compRow. No need to check any further.
#                        return(FALSE)
#                      }
#                    }
#                    # row has same ordering as compRow.
#                    return(TRUE)
#                  })
#   # All rows are same-ordered if all items in order are TRUE.
#   return(all(order))
# }
# 
