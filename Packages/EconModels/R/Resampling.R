#' @import foreach
#' @import doParallel

# This file contains code to resample data for economic growth
# functions. The idea is that we can resample the historical
# data and then develop statistical confidence intervals
# from the values of the model coefficients developed from 
# thousands of different resamples.
#
# We do this multiple times and save a file that contains 
# the results for later investigation

#' @export
timeFileName <- function(pre="",post="") {
  dt <- Sys.time()
  dt <- gsub(" ","-", dt)
  return(paste(pre, dt, post, sep=""))
}

# Compute resampled responses for a model.

#' @export
resampledResponse <- function( object, ...) {
  UseMethod("resampledResponse")
}

#' @export
resampledResponse.default <- function( object, method=c("residual", "wild", "debug", "parametric"),  
                                       reindex=FALSE,
                                       multErr, tol=1e-6, ... ) {
  if (missing(multErr)) {
    multErr <- all( response(object) / yhat(object) / exp(resid(object)) - 1 < tol )
  }
  method <- match.arg(method)
  n <- length(yhat(object))
  sgn <- if (method=="wild") resample( c(-1,1), n ) else 1
                             
  if (multErr) {
    if (method == "paramteric") {
      res <- rlnorm( n, 0, sd(resid(object)) *sqrt((n-1) / (n - length(coef(object)))) )
    } else {
      res <- yhat(object) * resample( exp(resid(object)) ) ^ sgn
    }
  } else {
    if (method == "paramteric") {
      res <- rnorm( n, 0, sd(resid(object)) *sqrt((n-1) / (n - length(coef(object)))) )
    } else {
      res <- yhat(object) + resample( resid(object) ) * sgn
    }
  }
  if (reindex) {
    res <- res / res[1]  # normalize so first entry is 1.
  }
  return(res)
}

#' Perform resample fits for a model
#' 
#' This function returns a list containing models and coefficients from the resample fits.
#' 
#' @param model a model returned from \code{linexModel}, \code{cdModel}, 
#' \code{cesModel}, or \code{sfModel}.  \code{model} must
#' have been fit with \code{save.data = TRUE}.
#' @param method one of \code{"resample"}, \code{"residual"}, \code{"wild"}, \code{"parametric"}, or \code{"debug"}
#' @param n the number of resamples you want to perform
#' @param save.data a logical indicating whether the data should be saved 
#' with each model.
#' @param seed a seed for \code{set.seed} to make results reproducible
#' @param id a character vector of length 1 used as an identifier that will be 
#' added to the output.  This is convenient
#' for marking data internally if you are doing resampling on many different models.
#' @param reindex a boolean that indicates whether to reindex the resampled fits.
#' @param mc.cores number of cores to use in main loop.
#' @param ... additional arguments passed to \code{fitfun}
#' @return a list of length two containting a data frame of coefficients and a list of models
#' @export
resampledFits <- function(model,
                          method=c("residual", "resample", "wild", "parametric", "debug"),
                          n,
                          save.data=FALSE,
                          seed,
                          id,
                          reindex = FALSE,
                          mc.cores = parallel::detectCores(),
                          ...) {
  
  fitfun <- switch(class(model)[1],
                   "SFmodel"    = "sfModel",
                   "LINEXmodel" = "linexModel",
                   "CDEmodel"   = "cdModel",
                   "cesEst"     = "cesModel",
                   "cesModel"   = "cesModel",
                   "plm"        = "cesModel"
  )
  
  formula <- model$formula
  
  method <- match.arg(method)   # allow multiples?
  if (!missing(seed)) set.seed(seed)
  data <- getData(model)
  
  baseFitCoeffs <- naturalCoef(model)  # extractAllMetaData(model)
  # Add a method column.
  baseFitCoeffs$method <- "orig"
  coeffs <- baseFitCoeffs
  
  # Begin accumulating a list of the models. The original model is in the first slot of the list.
  models <- list(model)
  
  # Now do the resample fits.
  if (n > 0L) {
    many_refits <-
      parallel::mclapply(
        1L:n, mc.cores = mc.cores,
        function(...) {
          newData <- resampledData(model, method=method, reindex=reindex)
          newModel <- do.call(fitfun, c(list(formula=formula, data=newData), list(...) ))
          resampleCoeffs <- naturalCoef(newModel) # extractAllMetaData(newModel)
          resampleCoeffs$method <- method
          # coeffs <- plyr::rbind.fill(coeffs, resampleCoeffs)
          # models[length(models)+1] <- list(newModel)
          list(model = newModel, coeffs = resampleCoeffs)
        }
      )
    models <- lapply(many_refits, function(x) x$model)
    coeffs <- do.call(plyr::rbind.fill, lapply(many_refits, function(x) x$coeffs))
    models <- lapply(many_refits, function(x) x$model)
    names(models) <- paste0(method, ".", 1L:n)
  }
  # At this point, both coeffs (which contains the coefficients) and 
  # models (which contains the models)
  # should be the same size. If not, we need to stop. Something has gone wrong.
  if (nrow(coeffs) != length(models)){
    # There is a problem here. Let's save the coeffs and the models in a list.
    mismatchedCoeffsModelsError <- list(coeffs=coeffs, models=models)
    save(mismatchedCoeffsModelsError, file="mismatchedCoeffsModelsError.Rdata")
    stop(paste("nrow(coeffs) =", nrow(coeffs), 
               "and length(models) =", length(models), "but they should be equal.",
               "coeffs and models have been saved in a file named mismatchedCoeffsModelsError."))
  }
  # coeffs$countryAbbrev <- countryAbbrev
  if (!missing(id)) coeffs$`.id` <- id
  out <- list(coeffs=coeffs, models=models)
  return(out)
}

#' Create resampled data
#' 
#' @param model a model returned from \code{linexModel}, \code{cdModel},
#' \code{cesModel}, or \code{sfModel}.  \code{model} must
#' have been fit with \code{save.data = TRUE}.
#' @param method One of 
#'      resample:   resample rows from data. Can result in repeated years.
#'      residual:   resamples the residuals and applies them to the data. All years are present.
#'      wild:       same as residuals but randomly select sign of resampled residuals.
#'      parametric: resample residuals from normal distribution
#' @param  reindex a logical indicating whether response values should be
#' reindexed before fitting.
#' 
#' @details
#' This relies on the fact that each of these models stores the data used to fit
#' the model with the response variable in the first column.

#' @export
resampledData <- function(model, method=c("residual", "resample", "wild", "parametric", "debug"), 
                          reindex=FALSE){
  data <- getData(model)
  if (is.null(data)) {
    stop ("'model' must be fit with 'save.data = TRUE'")
  }
  
  method <- match.arg(method)
  if(method == "resample") {
    res <- resample(data)
  } else {
    res <- data    
    res[ , 1] <- NA
    res[ , 1] <- resampledResponse(model, method=method, reindex=reindex)
  }
  return(res)
}
