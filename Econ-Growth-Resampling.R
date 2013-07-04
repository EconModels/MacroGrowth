require(mosaic)
require(foreach)
require(doParallel)
source('Econ-Growth-Functions2.R')

# This file contains code to resample data for economic growth
# functions. The idea is that we can resample the historical
# data and then develop statistical confidence intervals
# from the values of the model coefficients developed from 
# thousands of different resamples.
#
# We do this multiple times and save a file that contains 
# the results for later investigation
# 
# To generate the data, run the function genAllCDeResampleData. 
# This function will do the resampling and save data to disk 
# in appropriate folders and files.
#
# alpha 95% CIs for different amounts of resampling (with seed = 123)
# CDe with Q for the U.S.
# n      lower     upper   time [s]
# 10     0.2477    0.3151     0.62
# 100    0.2124    0.3351     5.2
# 1000   0.1987    0.3398    47.5
# 10000  0.1971    0.3356   484.7 (8 minutes)
# 30000  0.1960    0.3360
# 100000 0.1975    0.3360 14823.3 (4.2 hours)

genAllResampleData <- function(method=resampleMethods, n){
  #######################
  # Generates all resampling data for all models using the method specified
  ##
  if (missing(n)){
    n <- getNResamples()
  }
  if (missing(method)){
    method <- getResampleMethod()
  } else {
    method <- match.arg(method)
  }
  # Establish the parallel computing resources
  registerDoParallel()
  # Establish the timer
  t_0 <- proc.time()
  # Use the foreach package
  foreach(countryAbbrev=countryAbbrevs, .errorhandling="pass") %dopar% {
    genResampleData(modelType="sf",   countryAbbrev=countryAbbrev, factor="K",     n=n, method=method)
    genResampleData(modelType="sf",   countryAbbrev=countryAbbrev, factor="L",     n=n, method=method)
    genResampleData(modelType="sf",   countryAbbrev=countryAbbrev, factor="Q",     n=n, method=method)
    genResampleData(modelType="sf",   countryAbbrev=countryAbbrev, factor="X",     n=n, method=method)
    genResampleData(modelType="cd",   countryAbbrev=countryAbbrev,                 n=n, method=method)
    genResampleData(modelType="cde",  countryAbbrev=countryAbbrev, energyType="Q", n=n, method=method)
    genResampleData(modelType="cde",  countryAbbrev=countryAbbrev, energyType="X", n=n, method=method)
    genResampleData(modelType="ces",  countryAbbrev=countryAbbrev,                 n=n, method=method)
    genResampleData(modelType="cese", countryAbbrev=countryAbbrev, energyType="Q", n=n, method=method)
    genResampleData(modelType="cese", countryAbbrev=countryAbbrev, energyType="X", n=n, method=method)
    
  }  
  foreach(countryAbbrev=countryAbbrevsU, .errorhandling="pass") %dopar% {
    genResampleData(modelType="sf",   countryAbbrev=countryAbbrev, factor="U",     n=n, method=method)
    genResampleData(modelType="cde",  countryAbbrev=countryAbbrev, energyType="U", n=n, method=method)
    genResampleData(modelType="cese", countryAbbrev=countryAbbrev, energyType="U", n=n, method=method)
  }  
  # Report timer results
  out <- proc.time() - t_0
  return(out)
}

genResampleData <- function(modelType=modelTypes,
                            countryAbbrev=countryAbbrevs, 
                            energyType=energyTypes, 
                            factor=factors,
                            method=resampleMethods,
                            n
                            ){
  #########################
  # This function generates curve fits to resampled data for the Cobb-Douglas with energy 
  # production function and stores them to disk. The data are stored in an 
  # object called "resampleData". This object will have the same name
  # when it is loaded back from disk. 
  # We found that 10,000 resamples is sufficient to obtain good results
  ## 
  # This next call returns a list that contains two named data.frames: 
  # baseFitCoeffs and resampleFitCoeffs. 
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  energyType <- match.arg(energyType)
  factor <- match.arg(factor)
  method <- match.arg(method)
  resampleData <- resampleFits(modelType=modelType,
                               countryAbbrev=countryAbbrev, 
                               energyType=energyType, 
                               factor=factor,
                               method=method,
                               n=n)
  folder <- getFolderForResampleData(modelType=modelType,
                                     countryAbbrev=countryAbbrev, 
                                     energyType=energyType,
                                     factor=factor)
  # Ensure that the folder exists. showWarnings=FALSE, because we don't care 
  # if the directory already exists.
  dir.create(path=folder, recursive=TRUE, showWarnings=FALSE)
  path <- getPathForResampleData(modelType=modelType,
                                 countryAbbrev=countryAbbrev, 
                                 energyType=energyType,
                                 factor=factor)
  save(resampleData, file=path)
}

resampleFits <- function(modelType=modelTypes,
                         countryAbbrev=countryAbbrevs, 
                         energyType=energyTypes, 
                         factor=factors,
                         method=resampleMethods,
                         n
                         ){
  ##################
  # This function creates n resampled curve fits and returns them.
  # The returned object is a data frame.  The first row is the base fit to the 
  # actual historical data
  # The remaining rows are for the n resampled fits.
  # The method column identifies these and simplifies plotting of results.
  # n = number of resamples
  # countryAbbrev = the country you want to study
  # energyType = the type of energy of interest to you
  ##
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  energyType <- match.arg(energyType)
  factor <- match.arg(factor)
  method <- match.arg(method)
  set.seed(getSeed()) # Provide reproducible results
  # Load the raw economic and energy data for the country of interest.
  data <- loadData(countryAbbrev=countryAbbrev)
  # If useful work (U) is desired, subset to available data only.
  if (energyType == "U"){
    # Trim the dataset to include only those years for which U is available.
    data <- subset(data, !is.na(iU))
  }
  # First do a fit without resampling and get these coefficients
  origModel <- switch(modelType,
                      "sf"    = singleFactorModel(data=data, factor=factor),
                      "cd"    = cdModel(data=data, respectRangeConstraints=TRUE),
                      "cde"   = cdeModel(data=data, energyType=energyType, respectRangeConstraints=TRUE),
                      "ces"   = cesModelNoEnergy(data=data),
                      "cese"  = cesModel(countryAbbrev=countryAbbrev, energyType=energyType),
                      "linex" = linexModel(countryAbbrev=countryAbbrev, energyType=energyType)
                      )
  baseFitCoeffs <- attr(x = origModel, which="naturalCoeffs")
  # Now do a fit with resampling n times and get all of the coefficients
  resampleFitCoeffs <- switch(modelType,
                              "sf"    = do(n) * attr(x=singleFactorModel(data=doResample(data=data, 
                                                                                         origModel=origModel, 
                                                                                         method=method),
                                                                         factor=factor),
                                                     which="naturalCoeffs"),
                              "cd"    = do(n) * attr(x=cdModel(data=doResample(data=data, 
                                                                               origModel=origModel, 
                                                                               method=method),
                                                               respectRangeConstraints=TRUE),
                                                     which="naturalCoeffs"),
                              "cde"   = do(n) * attr(x=cdeModel(data=doResample(data=data, 
                                                                                origModel=origModel, 
                                                                                method=method),
                                                                energyType=energyType, 
                                                                respectRangeConstraints=TRUE),
                                                     which="naturalCoeffs"),
                              "ces"   = do(n) * attr(x=cesModelNoEnergy(data=doResample(data=data, 
                                                                                        origModel=origModel, 
                                                                                        method=method)),
                                                     which="naturalCoeffs"),
                              "cese"  = do(n) * attr(x=cesModel(countryAbbrev=countryAbbrev,
                                                                energyType=energyType,
                                                                data=doResample(data=data, 
                                                                                origModel=origModel, 
                                                                                method=method)),
                                                     which="naturalCoeffs"),
                              "linex" = do(n) * attr(x=linexModel(countryAbbrev=countryAbbrev,
                                                                  energyType=energyType,
                                                                  data=doResample(data=data, 
                                                                                  origModel=origModel, 
                                                                                  method=method)),
                                                     which="naturalCoeffs")
                              )
  # Combine the results and return
  baseFitCoeffsDF <- as.data.frame(matrix(baseFitCoeffs, nrow=1))
  names(baseFitCoeffsDF) <- names(baseFitCoeffs)
  resampleFitCoeffs <- transform(resampleFitCoeffs, method=method)
  baseFitCoeffsDF <- transform(baseFitCoeffsDF, method="orig")
  out <- rbind(as.data.frame(baseFitCoeffsDF), resampleFitCoeffs)
  # Make a factor column for the country
  countryAbbrev <- data.frame(rep(countryAbbrev, nrow(out)))
  colnames(countryAbbrev) <- "countryAbbrev"
  out <- cbind(out, countryAbbrev)
  return(out)
}

doResample <- function(data, origModel, method=resampleMethods){
  ######################
  # data: original data frame for country of interest. This data should NOT be resampled.
  #       this should contain the raw economic and energy data
  # origModel: original model returned from nls or cesEst.
  # method:
  #         resample:  resample rows from data. Can result in repeated years
  #         residual:  resamples the residuals and applies them to the data. All years are present.
  #         wild:      same as residuals but randomly select sign of resampled residuals
  ##
  method <- match.arg(method)
  if(method == "resample") {
    out <- resample(data)
    return(out)
  }
  data[ , "iGDP"] <- NA
  data[ , "iGDP"] <- 
    switch(method,
           "residual" = fitted(origModel) + resample(resid(origModel)),
           "wild"     = fitted(origModel) + resample(resid(origModel)) * resample(c(-1,1), length(resid(origModel))),
           "debug"    = fitted(origModel) + (resid(origModel)) 
    )
  return(data)
}

cdeFracUnconvergedResampleFitsAll <- function(){
  ###########################
  # Calculates the fraction of unconverged resamples stored on disk 
  # for all countries and all energy types
  ## 
  energyType <- "Q"
  qUnconverged <- lapply(countryAbbrevs, cdeFracUnconvergedResampleFits, energyType=energyType)
  energyType <- "X"
  xUnconverged <- lapply(countryAbbrevs, cdeFracUnconvergedResampleFits, energyType=energyType)
  energyType <- "U"
  uUnconverged <- lapply(countryAbbrevsU, cdeFracUnconvergedResampleFits, energyType=energyType)
  uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  uUnconverged <- c(uUnconverged, uNA)
  return(cbind(qUnconverged, xUnconverged, uUnconverged))
}

cdeFracUnconvergedResampleFits <- function(countryAbbrev, energyType, ...){
  ###################
  # Gives the fraction of resample fits that did not converge
  ##
  data <- loadCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  nObs <- nrow(data[["resampleFitCoeffs"]])
  tallyResults <- tally(~isConv, data=data[["resampleFitCoeffs"]], format="proportion")
  # Grabs the fraction that is converged. We can't simply gather the fraction that
  # has not converged (tallyResults[["0"]]), because there are some times when
  # all resampled fits converge, and there is no "0" item in the 
  # result from tally.
  fracConverged <- tallyResults[["1"]]
  fracUnconverged <- 1.0 - fracConverged
  return(fracUnconverged)
}
