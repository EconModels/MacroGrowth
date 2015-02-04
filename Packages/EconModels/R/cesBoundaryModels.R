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
cesBoundaryModel <- function(f, data, nest, id){
  timeSeries <- cesTimeSeries(data, f, nest)
  y <- timeSeries$y
  x1 <- timeSeries$x1
  x2 <- timeSeries$x2
  x3 <- timeSeries$x3
  x4 <- timeSeries$x4
  time <- timeSeries$time
  numFactors <- cesFormulaNames(f, nest)$numFactors
  
  if (id == 1){
    # Constraints are delta_1 = 1 and delta = 1.
    # rho_1, sigma_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma_coef * A * x1.
    # In log transform space, ln(y/x1) = ln(gamma_coef) + lambda*time.
    mod <- lm(log(y/x1) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = as.vector(1),
      delta = as.vector(1),
      sigma_1 = NA,
      rho_1 = NA, 
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 2){
    # Constraints are delta_1 = 0 and delta = 1.
    # rho_1, sigma_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma_coef * A * x2.
    # In log transform space, ln(y/x2) = ln(gamma_coef) + lambda*time.
    mod <- lm(log(y/x2) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = as.vector(0),
      delta = as.vector(1),
      sigma_1 = NA,
      rho_1 = NA,
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 3){
    # When variables x1 and x2 have the same order 
    # (e.g., x_1 < x_2 (or the other way around) at every observation in the data frame),
    # there is no need to fit this model.
    # We'll get the same fit with the model y = gamma_coef * A * x_1 
    # or y = gamma_coef * A * x_2, depending on which one is always the minimum.
    if (rowsSameOrdered(data.frame(x1, x2))){
      return(NULL)
    }
    # Constraints are sigma_1 = 0 and delta = 1.
    # delta_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma_coef * A * min(x1, x2).
    # In log transform space, ln(y/min(x1, x2)) = ln(gamma_coef) + lambda*time.
    minx1x2 <- pmin(timeSeries$x1, timeSeries$x2)
    mod <- lm(log(y/minx1x2) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = NA,
      delta = as.vector(1),
      sigma_1 = as.vector(0),
      rho_1 = Inf,
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 4){
    # Constraints are sigma_1 = Inf and delta = 1.
    # rho and sigma are unknowable and set to NA.
    # The model is y = gamma_coef * A * [delta_1 * x1 + (1-delta_1) * x2].
    # We use a nested fitting approach.
    # Given a value for delta_1, the variable blendedX is calculated. 
    # We fit the log transform of the equation,
    # log(y/blendedX) ~ time
    # with lm to obtain estimates for gamma_coef and lambda.
    # Then, we use nlmin to find the best value of delta_1.
    sse4 <- function(params) {
      delta_1 <- params[[1]]
      blendedX <- delta_1*x1 + (1-delta_1)*x2
      inner.model <- lm(log(y/blendedX) ~ time)
      sse <- sum(resid(inner.model)^2)
      attr(sse, "inner.model") <- inner.model
      return(sse)
    }
    mod <- nlmin(sse4, p=c(delta_1=0.5) )
    class(mod) <- c("CESmodel", class(mod))
    innerMod <- attr(sse4(mod$estimate), "inner.model")
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(innerMod)[[1]])),
      lambda = as.vector(coef(innerMod)[[2]]),
      delta_1 = as.vector(mod$estimate[[1]]),
      delta = as.vector(1),
      sigma_1 = as.vector(Inf),
      rho_1 = as.vector(-1),
      sigma = NA,
      rho = NA,
      sse = mod$minimum
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 5){
    # Constraint is delta = 1.
    # The model is y = gamma_coef * A * [delta_1 * x1^(-rho_1) + (1-delta_1) * x2^(-rho_1)]^(-1/rho_1).
    # This is nothing more than the CES function in two variables, x1, and x2.
    # So, fit by calling cesModel unconstrained.
    # Make a data frame with the correct variables
    bmod5data <- data.frame(y, x1, x2, time)
    mod <- cesModel2(f = y ~ x1 + x2 + time, data = bmod5data, nest = c(1,2), constrained = FALSE)
    return(mod)
  } else if (id == 6){
    # Constraints is delta = 0.
    # delta_1, rho_1, sigma_1, rho, and sigma are unknowable and set to NA.
    # The model is y = gamma_coef * A * x3.
    # In log transform space, ln(y/x3) = ln(gamma_coef) + lambda*time.
    mod <- lm(log(y/x3) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = NA,
      delta = as.vector(0),
      sigma_1 = NA,
      rho_1 = NA, 
      sigma = NA,
      rho = NA,
      sse = as.vector(sum(resid(mod)^2))
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 7){
    # When variables x1 and x3 have the same order 
    # (e.g., x_1 < x_3 (or the other way around) at every observation in the data frame),
    # there is no need to fit this model.
    # We'll get the same fit with the model y = gamma_coef * A * x_1
    # or y = gamma_coef * A * x_3, depending on which one is always the minimum.
    if (rowsSameOrdered(data.frame(x1, x3))){
      return(NULL)
    }
    # Constraints are delta_1 = 1 and sigma = 0.
    # delta, sigma_1, and rho_1 are unknowable and set to NA.
    # The model is y = gamma_coef * A * min(x1, x3).
    # In log transform space, ln(y/min(x1, x3)) = ln(gamma_coef) + lambda*time.
    minx1x3 <- pmin(timeSeries$x1, timeSeries$x3)
    mod <- lm(log(y/minx1x3) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = as.vector(1),
      delta = NA,
      sigma_1 = NA,
      rho_1 = NA,
      sigma = as.vector(0),
      rho = Inf,
      sse = as.vector(sum(resid(mod)^2))
    )    
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 8){
    # When variables x2 and x3 have the same order 
    # (e.g., x_2 < x_3 (or the other way around) at every observation in the data frame),
    # there is no need to fit this model.
    # We'll get the same fit with the model y = gamma_coef * A * x_2
    # or y = gamma_coef * A * x_3, depending on which one is always the minimum.
    if (rowsSameOrdered(data.frame(x2, x3))){
      return(NULL)
    }
    # Constraints are delta_1 = 0 and sigma = 0.
    # delta, sigma_1, and rho_1 are unknowable and set to NA.
    # The model is y = gamma_coef * A * min(x2, x3).
    # In log transform space, ln(y/min(x2, x3)) = ln(gamma_coef) + lambda*time.
    minx2x3 <- pmin(timeSeries$x2, timeSeries$x3)
    mod <- lm(log(y/minx2x3) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = as.vector(0),
      delta = NA,
      sigma_1 = NA,
      rho_1 = NA,
      sigma = as.vector(0),
      rho = Inf,
      sse = as.vector(sum(resid(mod)^2))
    )    
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 9){
    # When variables x1, x2, and x3 have the same order 
    # (e.g., x_1 < x_2 < x3 (or another permutation) at every observation in the data frame),
    # there is no need to fit this model.
    # We'll get the same fit with the model y = gamma_coef * A * x_1 
    # or y = gamma_coef * A * x_2 or y = gamma_coef * A * x_3, depending on which one is always the minimum.
    if (rowsSameOrdered(data.frame(x1, x2, x3))){
      return(NULL)
    }
    # Constraints are sigma_1 = 0 and sigma = 0.
    # delta_1 and delta are unknowable and set to NA.
    # The model is y = gamma_coef * A * min(x1 x2, x3).
    # In log transform space, ln(y/min(x1, x2, x3)) = ln(gamma_coef) + lambda*time.
    minx1x2x3 <- pmin(timeSeries$x1, timeSeries$x2, timeSeries$x3)
    mod <- lm(log(y/minx1x2x3) ~ time)
    class(mod) <- c("CESmodel", class(mod))
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(mod)[[1]])),
      lambda = as.vector(coef(mod)[[2]]),
      delta_1 = NA,
      delta = NA,
      sigma_1 = as.vector(0),
      rho_1 = Inf,
      sigma = as.vector(0),
      rho = Inf,
      sse = as.vector(sum(resid(mod)^2))
    )    
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 10){
    # Constraints are delta_1 = 1 and sigma = Inf.
    # sigma_1 is unknowable and set to NA.
    # The model is y = gamma_coef * A * [delta * x1 + (1-delta) * x3].
    # We use a nested fitting approach.
    # Given a value for delta, the variable blendedX is calculated. 
    # We fit the log transform of the equation,
    # log(y/blendedX) ~ time
    # with lm to obtain estimates for gamma_coef and lambda.
    # Then, we use nlmin to find the best value of delta.
    sse10 <- function(params) {
      delta <- params[[1]]
      blendedX <- delta*x1 + (1-delta)*x3
      inner.model <- lm(log(y/blendedX) ~ time)
      sse <- sum(resid(inner.model)^2)
      attr(sse, "inner.model") <- inner.model
      return(sse)
    }
    mod <- nlmin(sse10, p=c(delta=0.5))
    class(mod) <- c("CESmodel", class(mod))
    innerMod <- attr(sse10(mod$estimate), "inner.model")
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(innerMod)[[1]])),
      lambda = as.vector(coef(innerMod)[[2]]),
      delta = as.vector(mod$estimate[[1]]),
      delta_1 = as.vector(1),
      sigma_1 = NA,
      rho_1 = NA,
      sigma = Inf,
      rho = as.vector(0),
      sse = mod$minimum
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 11){
    # Constraints are delta_1 = 0 and sigma = Inf.
    # sigma_1 is unknowable and set to NA.
    # The model is y = gamma_coef * A * [delta * x2 + (1-delta) * x3].
    # We use a nested fitting approach.
    # Given a value for delta, the variable blendedX is calculated. 
    # We fit the log transform of the equation,
    # log(y/blendedX) ~ time
    # with lm to obtain estimates for gamma_coef and lambda.
    # Then, we use nlmin to find the best value of delta.
    sse11 <- function(params) {
      delta <- params[[1]]
      blendedX <- delta*x2 + (1-delta)*x3
      inner.model <- lm(log(y/blendedX) ~ time)
      sse <- sum(resid(inner.model)^2)
      attr(sse, "inner.model") <- inner.model
      return(sse)
    }
    mod <- nlmin(sse11, p=c(delta=0.5))
    class(mod) <- c("CESmodel", class(mod))
    innerMod <- attr(sse11(mod$estimate), "inner.model")
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(innerMod)[[1]])),
      lambda = as.vector(coef(innerMod)[[2]]),
      delta = as.vector(mod$estimate[[1]]),
      delta_1 = as.vector(1),
      sigma_1 = NA,
      rho_1 = NA,
      sigma = Inf,
      rho = as.vector(0),
      sse = mod$minimum
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 12){
    # Constraints are sigma_1 = 0 and sigma = Inf.
    # The model is y = gamma_coef * A * [delta * (delta_1*x_1 + (1-delta_1)*x_2) + (1-delta)*x3].
    # We use a nested fitting approach.
    # Given values for delta_1 and delta, the variable blendedX is calculated. 
    # We fit the log transform of the equation,
    # log(y/blendedX) ~ time
    # with lm to obtain estimates for gamma_coef and lambda.
    # Then, we use nlmin to find the best value of delta.
    sse12 <- function(params) {
      delta_1 <- params[[1]]
      delta <- params[[2]]
      blendedX <- delta*(delta_1*x1 + (1-delta_1)*x2) + (1-delta)*x3
      inner.model <- lm(log(y/blendedX) ~ time)
      sse <- sum(resid(inner.model)^2)
      attr(sse, "inner.model") <- inner.model
      return(sse)
    }
    mod <- nlmin(sse12, p=c(delta_1=0.5, delta=0.5))
    class(mod) <- c("CESmodel", class(mod))
    innerMod <- attr(sse12(mod$estimate), "inner.model")
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(innerMod)[[1]])),
      lambda = as.vector(coef(innerMod)[[2]]),
      delta_1 = as.vector(mod$estimate[[1]]),
      delta = as.vector(mod$estimate[[2]]),
      sigma_1 = Inf,
      rho_1 = as.vector(0),
      sigma = Inf,
      rho = as.vector(0),
      sse = mod$minimum
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)
    return(mod)
  } else if (id == 13){
    # When variables x1 and x2 have the same order 
    # (e.g., x_1 < x_2 (or the other way around) at every observation in the data frame),
    # there is no need to fit this model.
    # We'll get the same fit with boundary model 10 or boundary model 11.
    if (rowsSameOrdered(data.frame(x1, x2))){
      return(NULL)
    }
    # Constraints are sigma_1 = 0 and sigma = Inf.
    # delta_1 is unknowable and set to NA.
    # The model is y = gamma_coef * A * (delta*min(x1, x2) + (1-delta)*x3).
    # Given a value for delta, the variable blendedX is calculated. 
    # We fit the log transform of the equation,
    # log(y/blendedX) ~ time
    # with lm to obtain estimates for gamma_coef and lambda.
    # Then, we use nlmin to find the best value of delta.
    minx1x2 <- pmin(timeSeries$x1, timeSeries$x2)
    sse13 <- function(params) {
      delta <- params[[1]]
      blendedX <- delta*minx1x2 + (1-delta)*x3
      inner.model <- lm(log(y/blendedX) ~ time)
      sse <- sum(resid(inner.model)^2)
      attr(sse, "inner.model") <- inner.model
      return(sse)
    }
    mod <- nlmin(sse13, p=c(delta=0.5))
    class(mod) <- c("CESmodel", class(mod))
    innerMod <- attr(sse13(mod$estimate), "inner.model")
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(innerMod)[[1]])),
      lambda = as.vector(coef(innerMod)[[2]]),
      delta = as.vector(mod$estimate[[1]]),
      delta_1 = NA,
      sigma_1 = as.vector(0),
      rho_1 = Inf,
      sigma = Inf,
      rho = as.vector(-1),
      sse = mod$minimum
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)  
    return(mod)
  } else if (id == 14){
    # Constraints are sigma_1 = Inf and sigma = 0.
    # delta is unknowable and set to NA.
    # The model is y = gamma_coef * A * min[delta_1*x1 + (1-delta_1)*x2, x3].
    # Given a value for delta_1, the variable blendedx1x2 is calculated as delta_1*x1 + (1-delta_1)*x2.
    # Then, blendedX is compared against x3 to find the minimum (smallest).
    # We fit the log transform of the equation,
    # log(y/smallest) ~ time
    # with lm to obtain estimates for gamma_coef and lambda.
    # Then, we use nlmin to find the best value of delta_1.
    sse14 <- function(params) {
      delta_1 <- params[[1]]
      blendedx1x2 <- delta_1*x1 + (1-delta_1)*x2
      smallest <- pmin(blendedx1x2, timeSeries$x3)
      inner.model <- lm(log(y/smallest) ~ time)
      sse <- sum(resid(inner.model)^2)
      attr(sse, "inner.model") <- inner.model
      return(sse)
    }
    mod <- nlmin(sse14, p=c(delta_1=0.5))
    class(mod) <- c("CESmodel", class(mod))
    innerMod <- attr(sse14(mod$estimate), "inner.model")
    naturalCoeffs <- data.frame(
      gamma_coef = as.vector(exp(coef(innerMod)[[1]])),
      lambda = as.vector(coef(innerMod)[[2]]),
      delta = NA,
      delta_1 = as.vector(mod$estimate[[1]]),
      sigma_1 = as.vector(Inf),
      rho_1 = -1,
      sigma = as.vector(0),
      rho = as.vector(Inf),
      sse = mod$minimum
    )
    attr(mod, "bmodID") <- id
    mod <- addMetaData(model=mod, formula=f, nest=nest, naturalCoeffs=naturalCoeffs)  
    return(mod)
  } else {
    stop(paste0("Unknown id = ", id, " in cesBoundaryModel"))
  }
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

#' Tells whether variables are always similarly ordered across all observations.
#' 
#' @param data a data frame that contains rows that are to be compared for ordering
#' @return \code{TRUE} if all rows are ordered the same, \code{FALSE} if they are not.
#' @note If all rows are comprised of equal numbers, \code{FALSE} is returned.
#' @export
rowsSameOrdered <- function(data){
  # Look at all the rows. 
  # Make a vector with length equal to the number of rows in data whose
  # values are TRUE if there was at least two numbers equal on the row and FALSE if all numbers were unique.
  somethingEqualOnRow <- apply(data, 1,
                               function(row){
                                 if (length(row) < 2){
                                   stop(paste("row has length", length(row), "in rowsSameOrdered. length must be >= 2"))
                                 }
                                 # row is each row of data.
                                 for (i in 1:(length(row)-1)){
                                   for (j in (i+1):length(row)){
                                     if (row[i] == row[j]){
                                       return(TRUE)
                                     }
                                   }
                                 }
                                 return(FALSE)
                               })
  # Find the index of the first row contining unique numbers (no duplicates)
  iCompRow <- match(FALSE, somethingEqualOnRow)
  if (is.na(iCompRow)){
    # No rows have differing data. Return false.
    return(FALSE)
  }
  # Use the first unique row as our comparison row.
  compRow <- data[iCompRow, ]
  # Find the order for compRow
  oCompRow <- order(compRow)
  # Now, check to see if every other row has a similar order.
  order <- apply(data, 1, 
                 function(row){
                   # row is each row of data.
                   # arrange row in least-to-greatest order of compRow.
                   reorderedRow <- row[oCompRow]
                   # If reorderedRow is itself in least-to-greatest order, it has same ordering as compRow.
                   for (j in 2:length(reorderedRow)){
                     if (reorderedRow[j-1] > reorderedRow[j]){
                       # Didn't have least-to-greatest order when arranged as compRow. No need to check any further.
                       return(FALSE)
                     }
                   }
                   # row has same ordering as compRow.
                   return(TRUE)
                 })
  # All rows are same-ordered if all items in order are TRUE.
  return(all(order))
}

