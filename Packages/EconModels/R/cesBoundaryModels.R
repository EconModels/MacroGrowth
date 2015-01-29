#' Fits CES boundary model 1
#' 
#' The boundary model is \code{y = gamma * A * x1} in log transform space 
#' as {ln(y/x1) = ln(gamma) + lambda*time}.
#' For ces boundary model 1, the constraints are \code{delta_1 = 1}, and \code{delta = 1}.
#' \code{rho_1} (\code{sigma_1}) and \code{rho} (\code{sigma}) are unknowable and set to \code{NA}.
#'
#' @param y the response variable, assumed to be a vector
#' @param x1 the first factor of production, assumed to be a vector
#' @param time the indexed time variable, assumed to be a vector
#' @param formula the original CES formula that is being fitted
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
bmod1 <- function(y, x1, time, formula, nest){
  mod <- lm(log(y/x1) ~ time)
  attr(mod, "bmod") <- 1
  class(mod) <- c("CESmodel", class(mod))
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
  mod <- addMetaData(model=mod, formula=formula, nest=nest, naturalCoeffs=naturalCoeffs)
  return(mod)
}

#' Fits CES boundary model 2
#' 
#' The boundary model is \code{y = gamma * A * x2} in log transform space 
#' as {ln(y/x2) = ln(gamma) + lambda*time}.
#' For ces boundary model 2, the constraints are \code{delta_1 = 0}, and \code{delta = 1}.
#' \code{rho_1} (\code{sigma_1}) and \code{rho} (\code{sigma}) are unknowable and set to \code{NA}.
#'
#' @param y the response variable, assumed to be a vector
#' @param x2 the second factor of production, assumed to be a vector
#' @param time the indexed time variable, assumed to be a vector
#' @param formula the original CES formula that is being fitted
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
bmod2 <- function(y, x2, time, formula, nest){
  mod <- lm(log(y/x2) ~ time)
  attr(mod, "bmod") <- 2
  class(mod) <- c("CESmodel", class(mod))
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
  mod <- addMetaData(model=mod, formula=formula, nest=nest, naturalCoeffs=naturalCoeffs)
  return(mod)
}

#' Fits CES boundary model 3
#' 
#' The boundary model is \code{y = gamma * A * min(x1, x2)} in log transform space 
#' as {ln(y/min(x1, x2)) = ln(gamma) + lambda*time}.
#' For ces boundary model 3, the constraints are \code{sigma_1 = 0}, and \code{delta = 1}.
#' \code{delta_1} and \code{rho} (\code{sigma}) are unknowable and set to \code{NA}.
#'
#' @param y the response variable, assumed to be a vector
#' @param minx1x2 the factor of production, assumed to be a vector
#' @param time the indexed time variable, assumed to be a vector
#' @param formula the original CES formula that is being fitted
#' @param nest identifies the nesting of the variables in the original formula,
#' assumed to be of the form \code{c(1,2,3)}.
bmod3 <- function(y, minx1x2, time, formula, nest){
  mod <- lm(log(y/minx1x2) ~ time)
  attr(mod, "bmod") <- 3
  class(mod) <- c("CESmodel", class(mod))
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
  mod <- addMetaData(model=mod, formula=formula, nest=nest, naturalCoeffs=naturalCoeffs)
  return(mod)
}
