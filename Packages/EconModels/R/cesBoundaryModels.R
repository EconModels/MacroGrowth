#' Fits CES boundary models
#' 
#' The boundary models are given in Table 2 of 
#' Heun, et al "An Empirical Investigation of the Role of Energy in Economic Growth"
#'
#' @param data historical time series data
#' @param f a CES formula in the form \code{y ~ a + b + c + d + time}
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
#' @param id the identification number (from Table 2) for the boundary model you want to fit.
#' @return a model object with class \code{CESModel} and \code{naturalCoeffs} and \code{meta} attributes.
#' @note the \code{naturalCoeffs} attribute includes correct values of boundary parameters. 
#' \code{NA} values in \code{naturalCoeffs} indicate that the parameter is unknowable at that boundary.
#' @export
cesBoundaryModel <- function(data, f, nest, id){
  timeSeries <- cesTimeSeries(data, f, nest)
  y <- timeSeries$y
  x1 <- timeSeries$x1
  x2 <- timeSeries$x2
  time <- timeSeries$time
  
  if (id == 1){
    # Constraints are delta_1 = 1 and delta = 1.
    # rho_1, sigma_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma * A * x1.
    # In log transform space, ln(y/x1) = ln(gamma_coef) + lambda*time.
    mod <- lm(log(y/x1) ~ time)
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(mod$coefficients[[1]])),
      lambda = as.vector(mod$coefficients[[2]]),
      delta_1 = as.vector(1),
      delta = as.vector(1),
      sigma_1 = NA,
      rho_1 = NA, 
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
  } else if (id == 2){
    # Constraints are delta_1 = 0 and delta = 1.
    # rho_1, sigma_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma * A * x2.
    # In log transform space, ln(y/x2) = ln(gamma_coef) + lambda*time.
    mod <- lm(log(y/x2) ~ time)
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(mod$coefficients[[1]])),
      lambda = as.vector(mod$coefficients[[2]]),
      delta_1 = as.vector(0),
      delta = as.vector(1),
      sigma_1 = NA,
      rho_1 = NA,
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
  } else if (id == 3){
    # Constraints are sigma_1 = 0 and delta = 1.
    # delta_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma * A * min(x1, x2).
    # In log transform space, ln(y/min(x1, x2)) = ln(gamma_coef) + lambda*time.
    minx1x2 <- pmin(timeSeries$x1, timeSeries$x2)
    mod <- lm(log(y/minx1x2) ~ time)
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(mod$coefficients[[1]])),
      lambda = as.vector(mod$coefficients[[2]]),
      delta_1 = NA,
      delta = as.vector(1),
      sigma_1 = 0,
      rho_1 = Inf,
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
#   } else if (id == 4){
#     # Constraints are sigma_1 = Inf and delta = 1.
#     # rho and sigma are unknowable and set to NA.
#     # The model is y = gamma * A * [delta_1 * x1 + (1-delta_1) * x2].
#     # We do not log transform this model. We use nls.
#     
#   } else if (id == 5){
#     # Constraint is delta = 1.
#     # rho and sigma are unknowable and set to NA.
#     # The model is y = gamma * A * [delta_1 * x1^(-rho_1) + (1-delta_1) * x2^(-rho_1)]^(-1/rho_1).
#     # We do not log transform this model. We use nls.
    
  } else {
    stop(paste0("Unknown id = ", id, " in cesBoundaryModel"))
  }

  class(mod) <- c("CESmodel", class(mod))
  attr(mod, "bmodID") <- id
  mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
  return(mod)
}

#' Extracts y, x1, x2, x3, x4, and time data as time series for CES boundary models
#' 
#' Given formula \code{f} and nest \code{nest}, this function
#' extracts a response variable (\code{y}), 
#' factors of production (\code{x1}, \code{x2}, \code{x3}, and \code{x4}), and 
#' a time variable (\code{time}) from \code{data}.
#' All extracted variables are time series vectors.
#'
#' @param data the data frame from which time series data is to be extracted
#' @param f the CES formula for which time series data is to be extracted, 
#' assumed to be of the form \code{y ~ x1 + x2 + x3 + x4 + time}.
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
#' @return a named list of time series'. 
#' Names are \code{y}, \code{x1}, \code{x2}, \code{x3}, \code{x4}, and \code{time}.
cesTimeSeries <- function(data, f, nest){
  fNames <- cesFormulaNames(f, nest)
  # Extract variables for convenience.
  numFactors <- fNames$numFactors
  if (numFactors < 2 || numFactors > 4){
    stop(paste0("numFactors = " + numFactors + " in cesTimeSeries. Should have 2 <= numFactors <= 4."))
  }
  cesNames <- fNames$cesNames
  yName <- fNames$yName
  xNames <- fNames$xNames
  tName <- fNames$tName
  
  # Extract variales from data.
  y <- eval(substitute(data$y, list(y = yName)))
  time <- eval(substitute(data$time, list(time = tName)))
  x1 <- eval(substitute(data$colx1, list(colx1 = xNames[[1]])))
  x2 <- eval(substitute(data$colx2, list(colx2 = xNames[[2]])))
  if (numFactors == 2){
    return(list(y = y, x1=x1, x2=x2, x3=NA, x4=NA, time=time))
  }
  if (numFactors == 3){
    x3 <- eval(substitute(data$colx3, list(colx3 = xNames[[3]])))
    return(list(y = y, x1=x1, x2=x2, x3=x3, x4=NA, time=time))
  }
  if (numFactors == 4){
    x4 <- eval(substitute(data$colx4, list(colx4 = xNames[[4]])))
    return(list(y = y, x1=x1, x2=x2, x3=x3, x4=x4, time=time))
  }
}