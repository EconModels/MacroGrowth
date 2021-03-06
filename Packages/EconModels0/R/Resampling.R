#' @import mosaic
#' @import foreach
#' @import doParallel
#' @import plyr

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

# methods for generating a resampled response.

#' @export
resampledResponse <- function( object, ...) {
  UseMethod("resampledResponse")
}

#' @export
resampledResponse.CDEmodel <- function( object, method=c("residual", "wild", "debug"), ... ) {
  mresid <- exp(resid(object))
  method <- match.arg(method)
  if (method=="debug") {
    return( yhat(object) * mresid )
  }
  n <- length(fitted(object))
  sgn <- if (method=="wild") resample( c(-1,1), n ) else 1
  yhat(object) * resample( mresid )  ^ sgn
}

#' @export
resampledResponse.LINEXmodel <- function( object, method=c("residual", "wild", "debug"), ... ) {
  mresid <- exp(resid(object))
  method <- match.arg(method)
  if (method=="debug") {
    return( yhat(object) * mresid )
  }
  n <- length(fitted(object))
  sgn <- if (method=="wild") resample( c(-1,1), n ) else 1
  yhat(object) * resample( mresid )  ^ sgn
}

#' @export
resampledResponse.default <- function( object, method=c("residual", "wild", "debug"), ... ) {
  method <- match.arg(method)
  if (method=="debug") return( yhat(object) + resid(object) )
  n <- length(fitted(object))
  sgn <- if (method=="wild") resample( c(-1,1), n ) else 1
  yhat(object) + resample( resid(object) ) * sgn
}

#' Generates all resample data for all countries, all model types, and all energy types
#' 
#' This function generates curve fit coefficients and models
#' and stores them to disk. The data are stored in an 
#' object called "resampleData" and an object called "resampleModels". 
#' These objects will have the same name
#' when they are loaded back from disk. 
#' 
#' @param method one of \code{"resample"}, \code{"residual"}, \code{"wild"}, or \code{"debug"}
#' @param n the number of resamples you want to perform
#' @param baseResample the relative path to the directory where you want to store results
#' @param baseHistorical the relative path to the directory in which the historical data resides
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
genAllResampleData <- function(method="wild", n=numResamples(), baseResample, baseHistorical, ...) {
  #######################
  # Generates all resampling data for all models using the method specified
  ##
  # method <- match.arg(method)
  # Establish the parallel computing resources
  registerDoParallel()
  # Establish the timer
  t_0 <- proc.time()
  # Use the foreach package
  status <- foreach(ca=countryAbbrevs, .errorhandling="pass", .init=c(), .combine=c) %dopar% {
    status <- c()
    status <- c(status,
                genResampleData(modelType="sf",    countryAbbrev=ca, factor="K",     n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="sf",    countryAbbrev=ca, factor="L",     n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="sf",    countryAbbrev=ca, factor="Q",     n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="sf",    countryAbbrev=ca, factor="X",     n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cd",    countryAbbrev=ca,                 n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cde",   countryAbbrev=ca, energyType="Q", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cde",   countryAbbrev=ca, energyType="X", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="ces",   countryAbbrev=ca,                 n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(kl)e",  countryAbbrev=ca, energyType="Q", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(le)k",  countryAbbrev=ca, energyType="Q", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(ek)l",  countryAbbrev=ca, energyType="Q", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(kl)e",  countryAbbrev=ca, energyType="X", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(le)k",  countryAbbrev=ca, energyType="X", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(ek)l",  countryAbbrev=ca, energyType="X", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="linex", countryAbbrev=ca, energyType="Q", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="linex", countryAbbrev=ca, energyType="X", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status
  }  
  status2 <- foreach(ca=countryAbbrevsU, .errorhandling="pass", .combine=c, .init=c()) %dopar% {
    status <- c()
    status <- c(status,
                genResampleData(modelType="sf",    countryAbbrev=ca, factor="U",     n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cde",   countryAbbrev=ca, energyType="U", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(kl)e",  countryAbbrev=ca, energyType="U", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(le)k",  countryAbbrev=ca, energyType="U", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="cese-(ek)l",  countryAbbrev=ca, energyType="U", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status <- c(status,
                genResampleData(modelType="linex", countryAbbrev=ca, energyType="U", n=n, method=method,
                                baseHistorical=baseHistorical, baseResample=baseResample, ...))
    status
  }  
  # Report timer results
  timing <- proc.time() - t_0
  return(list(s1=status, s2=status2, timing=timing) )
}

#' Generates resample data, for the base fit and resample fits
#' 
#' This function generates curve fit coefficients and models
#' and stores them to disk. The data are stored in an 
#' object called "resampleData" and an object called "resampleModels". 
#' These objects will have the same name
#' when they are loaded back from disk. 
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev a string representing the country for which you want to load resample data.
#' @param energyType one of \code{"none"}, \code{"Q"} (for thermal energy), \code{"X"} (for exergy), 
#' or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param method one of \code{"resample"}, \code{"residual"}, \code{"wild"}, or \code{"debug"}
#' @param n the number of resamples you want to perform
#' @param clobber whether you want to overwrite any previous results
#' @param verbose whether you want lots of output
#' @param baseResample the relative path to the directory where you want to store results
#' @param baseHistorical the relative path to the directory in which the historical data resides
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
genResampleData <- function(modelType=modelTypes,
                            countryAbbrev=countryAbbrevs, 
                            energyType="Q", 
                            factor="K",
                            method="wild",
                            n,
                            clobber=TRUE,
                            verbose=FALSE,
                            baseResample,
                            baseHistorical){
  pathCoeffs <- getPathForResampleData(modelType=modelType,
                                       countryAbbrev=countryAbbrev, 
                                       energyType=energyType,
                                       factor=factor,
                                       baseResample=baseResample)
  pathModels <- getPathForResampleModels(modelType=modelType,
                                         countryAbbrev=countryAbbrev, 
                                         energyType=energyType,
                                         factor=factor,
                                         baseResample=baseResample)
  status <- "attempted"
  
  # If both files exist AND clobber==FALSE, don't do anything.
  if (file.exists(pathCoeffs) && file.exists(pathModels)) {
    if (verbose) {
      cat(paste(pathCoeffs, "exists\n"))
    }
    if (! clobber) {
      status <- "both the coeffs file and the models existed; not clobbered"
      names(status) <- paste(countryAbbrev[1], modelType[1], energyType[1], factor[1], sep=":")
      return(status)
    } else {
      status <- "both the coeffs and models files existed; going to clobber"
    }
  }
  if (verbose) cat(paste('Data will be saved in', pathCoeffs, "and", pathModels, "\n"))
  status <- "creating new files"
  # This next call returns a list that contains two named data.frames: 
  # baseFitCoeffs and resampleFitCoeffs. 
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  method <- match.arg(method)
  resampleInfo <- resampleFits(modelType=modelType,
                               countryAbbrev=countryAbbrev, 
                               energyType=energyType, 
                               factor=factor,
                               method=method,
                               n=n,
                               baseHistorical=baseHistorical)
  # Split the coefficients from the models
  resampleData <- resampleInfo$coeffs
  resampleModels <- resampleInfo$models  
  # Figure out which folder files should be saved in
  folder <- getFolderForResampleData(modelType=modelType,
                                     countryAbbrev=countryAbbrev,
                                     baseResample=baseResample)
  # Ensure that the folder exists. showWarnings=FALSE, because we don't care 
  # if the directory already exists.
  dir.create(path=folder, recursive=TRUE, showWarnings=FALSE)
  # Save the coeffs and models to disk
  save(resampleData, file=pathCoeffs)
  save(resampleModels, file=pathModels)
  status <- paste0("  ", c("coefficients", "models"), " saved in ", c(pathCoeffs, pathModels), collapse=" \n" )
  names(status) <- paste(countryAbbrev[1], modelType[1], energyType[1], factor[1], sep=":")
  paste(status)
  return(status)
}

#' Performs resample fits for a model
#' 
#' This function returns a list containing models and coefficients from the resample fits.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev the country abbreviation that you want to analyze.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param method one of \code{"resample"}, \code{"residual"}, \code{"wild"}, or \code{"debug"}
#' @param n the number of resamples you want to perform
#' @param baseHistorical the relative path to the directory containing the historical data
#' @param rho a vector of \code{rho} value over which you'll search if doing a CES analysis
#' @param rho1 a vector of \code{rho1} value over which you'll search if doing a CES analysis
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
resampleFits <- function(
  modelType=modelTypes,
  countryAbbrev=countryAbbrevs, 
  energyType="Q", 
  factor="K",
  method=resampleMethods,
  n,
  baseHistorical,
  rho =c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
  rho1=c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99)){

  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
#   energyType <- match.arg(energyType)
#   factor <- match.arg(factor)
  method <- match.arg(method)   # allow multiples?
  set.seed(getSeed()) # Provide reproducible results
  # Load the raw economic and energy data for the country of interest.
  data <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
  # If useful work (U) is desired, subset to available data only.
  if (!is.na(energyType)){
    if (factor == "U" || energyType == "U"){
      # Trim the dataset to include only those years for which U is available.
      data <- subset(data, !is.na(iU))
    }
  }
  # First do a fit without resampling and get these coefficients
  origModel <- switch(modelType,
                      "sf"          = sfModel(data=data, factor=factor, respectRangeConstraints=TRUE),
                      "cd"          = cdwoeModel(data=data, respectRangeConstraints=TRUE),
                      "cde"         = cdeModel(data=data, energyType=energyType, respectRangeConstraints=TRUE),
                      "ces"         = cesModel2(data=data),
                      "cese-(kl)e"  = cesModel2(data=data, nest="(kl)e", energyType=energyType),
                      "cese-(le)k"  = cesModel2(data=data, nest="(le)k", energyType=energyType),
                      "cese-(ek)l"  = cesModel2(data=data, nest="(ek)l", energyType=energyType),
                      "linex"       = linexModel(data=data, energyType=energyType)
  )
  baseFitCoeffs <- extractAllMetaData(origModel)
  # Add a method column.
  baseFitCoeffs$method <- "orig"
  coeffs <- baseFitCoeffs
  
  # Begin accumulating a list of the models. The original model is in the first slot of the list.
  models <- list(orig=switch(modelType, 
                             "sf"         = origModel,
                             "cd"         = origModel,
                             "cde"        = origModel,
                             "ces"        = bestModel(origModel),
                             "cese-(kl)e" = bestModel(origModel),
                             "cese-(le)k" = bestModel(origModel),
                             "cese-(ek)l" = bestModel(origModel),
                             "linex"      = origModel))  
  # Now do the resample fits.
  resampleFitCoeffs <- switch(modelType,
                              "sf"    = for (i in 1:n){
                                resampleData <- doResample(data=data, origModel=origModel, method=method)
                                model <- sfModel(data=resampleData, factor=factor, respectRangeConstraints=TRUE)
                                resampleCoeffs <- attr(x=model, which="naturalCoeffs")
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- model
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "cd"    = for (i in 1:n){
                                resampleData <- doResample(data=data, origModel=origModel, method=method)
                                model <- cdwoeModel(data=resampleData, factor=factor, respectRangeConstraints=TRUE)
                                resampleCoeffs <- attr(x=model, which="naturalCoeffs")
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- model
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "cde"   = for (i in 1:n) {
                                resampleData <- doResample(data=data, origModel=origModel, method=method)
                                model <- cdeModel(data=resampleData, energyType=energyType, respectRangeConstraints=TRUE)
                                resampleCoeffs <- attr(x=model, which="naturalCoeffs")
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- model
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "ces" = for (i in 1:n){
                                resampleData <- doResample(data=data, origModel=bestModel(origModel), method=method)
                                model <- cesModel2(countryAbbrev=countryAbbrev, data=resampleData, prevModel=bestModel(origModel))
                                resampleCoeffs <- extractAllMetaData(model)
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[length(models)+1] <- list(bestModel(model))
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "cese-(kl)e" = for (i in 1:n){
                                resampleData <- doResample(data=data, origModel=bestModel(origModel), method=method)
                                model <- cesModel2(countryAbbrev=countryAbbrev, energyType=energyType, nest="(kl)e",
                                                   data=resampleData, prevModel=bestModel(origModel))
                                resampleCoeffs <- extractAllMetaData(model)
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- bestModel(model)
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "cese-(le)k" = for (i in 1:n){
                                resampleData <- doResample(data=data, origModel=bestModel(origModel), method=method)
                                model <- cesModel2(countryAbbrev=countryAbbrev, energyType=energyType, nest="(le)k",
                                                   data=resampleData, prevModel=bestModel(origModel))
                                resampleCoeffs <- extractAllMetaData(model)
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- bestModel(model)
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "cese-(ek)l" = for (i in 1:n){
                                resampleData <- doResample(data=data, origModel=bestModel(origModel), method=method)
                                model <- cesModel2(countryAbbrev=countryAbbrev, energyType=energyType, nest="(ek)l",
                                                   data=resampleData, prevModel=bestModel(origModel))
                                resampleCoeffs <- extractAllMetaData(model)
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- bestModel(model)
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              "linex"   = for (i in 1:n) {
                                resampleData <- doResample(data=data, origModel=origModel, method=method)
                                model <- linexModel(countryAbbrev=countryAbbrev, energyType=energyType, data=resampleData)
                                resampleCoeffs <- attr(x=model, which="naturalCoeffs")
                                resampleCoeffs$method <- method
                                coeffs <- rbind.fill(coeffs, resampleCoeffs)
                                models[[length(models)+1]] <- model
                                names(models)[length(models)] <- paste(method, ".", i, sep="")
                              },
                              stop("unknown model type")
  )
  
  # At this point, both coeffs (which contains the coefficients) and models (which contains the models)
  # should be the same size. If not, we need to stop. Something has gone wrong.
  if (nrow(coeffs) != length(models)){
    # There is a problem here. Let's save the coeffs and the models in a list.
    mismatchedCoeffsModelsError <- list(coeffs=coeffs, models=models)
    save(mismatchedCoeffsModelsError, file="mismatchedCoeffsModelsError.Rdata")
    stop(paste("nrow(coeffs) =", nrow(coeffs), 
               "and length(models) =", length(models), "but they should be equal.",
               "coeffs and models have been saved in a file named mismatchedCoeffsModelsError."))
  }
  coeffs$countryAbbrev <- countryAbbrev
  out <- list(coeffs=coeffs, models=models)
  return(out)
}

#' @export
doResample <- function(data, origModel, method=resampleMethods, reindexGDP=TRUE){
  ######################
  # data: original data frame for country of interest. This data should NOT be resampled.
  #       this should contain the raw economic and energy data
  # origModel: original model returned from nls or cesEst.
  # method:
  #         resample:  resample rows from data. Can result in repeated years
  #         residual:  resamples the residuals and applies them to the data. All years are present.
  #         wild:      same as residuals but randomly select sign of resampled residuals
  # reindexGDP:
  #         TRUE (default) will rescale iGDP values so that the observation in the first year (row) is 1.0
  #         FALSE leaves GDP values untouched.
  ##
  method <- match.arg(method)
  if(method == "resample") {
    out <- resample(data)
  } else {
    out <- data    
    out[ , "iGDP"] <- NA
    out[ , "iGDP"] <- resampledResponse(origModel, method=method)
  }
  if (reindexGDP){
    # Divide all the values in the iGDP column by the first value in the column
    out[ , "iGDP"] <- out[ , "iGDP"] / out[1, "iGDP"]
  }
  return(out)
}
