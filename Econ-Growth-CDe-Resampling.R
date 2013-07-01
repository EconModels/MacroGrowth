require(mosaic)
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

genAllCDeResampleData <- function(){
  #############################
  # This script generates resample data for all countries
  # and saves to disk.
  ##
  t_0 <- proc.time()
  n=100 # 10,000 samples are probably sufficient
  method="wild"
  energyType <- "Q"
  lapply(countryAbbrevs, genCDeResampleData, energyType=energyType, n=n, method=method)
  energyType <- "X"
  lapply(countryAbbrevs, genCDeResampleData, energyType=energyType, n=n, method=method)
  energyType <- "U"
  lapply(countryAbbrevsU, genCDeResampleData, energyType=energyType, n=n, method=method)
  print(proc.time() - t_0)
}

genCDeResampleData <- function(countryAbbrev, energyType, n, method=c("resample","residual","wild")){
  #########################
  # This function generates curve fits to resampled data for the Cobb-Douglas with energy 
  # production function and stores them to disk. The data are stored in an 
  # object called "resampleData". This object will have the same name
  # when it is loaded back from disk. 
  # We found that 10,000 resamples is sufficient to obtain good results
  ## 
  # This next call returns a list that contains two named data.frames: 
  # baseFitCoeffs and resampleFitCoeffs. 
  resampleData <- cdeResampleFits(countryAbbrev=countryAbbrev, 
                                  energyType=energyType, 
                                  respectRangeConstraints=TRUE, 
                                  n=n, 
                                  method=method,
                                  )
  folder <- getFolderForCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  # Ensure that the folder exists. showWarnings=FALSE, because we don't care 
  # if the directory already exists.
  dir.create(path=folder, recursive=TRUE, showWarnings=FALSE)
  path <- getPathForCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  save(resampleData, file=path)
}

cdeResampleFits <- function(countryAbbrev, energyType, respectRangeConstraints=FALSE, n, 
                            method=c("resample","residual","wild", "debug"), ...){
  ##################
  # This function creates n resampled curve fits and returns them.
  # The returned object is a list with the first item being the base fit to the 
  # actual historical data
  # The second object is a a data.frame of n resampled fits.
  # n = number of resamples
  # countryAbbrev = the country you want to study
  # energyType = the type of energy of interest to you
  ##
  method = match.arg(method)
  set.seed(123) # Provide reproducible results
  # First do a fit without resampling and get these coefficients
  origModel <- cdeModel(countryAbbrev=countryAbbrev,
                        energyType=energyType,
                        respectRangeConstraints=respectRangeConstraints)
  baseFitCoeffs <- attr(x = origModel,
                        which="naturalCoeffs")
  # Now do a fit with resampling n times and get all of the coefficients
  data <- loadData(countryAbbrev=countryAbbrev)
  resampleFitCoeffs <- 
    do(n) * attr(x=cdeModel(data=doResample(data, model=origModel, 
                                            energyType=energyType, method=method),
                            energyType=energyType, 
                            respectRangeConstraints=respectRangeConstraints),
                            which="naturalCoeffs")
  # Combine the results and return
  # out <- list(baseFitCoeffs=baseFitCoeffs, resampleFitCoeffs=resampleFitCoeffs)

  baseFitCoeffsDF <- as.data.frame(matrix(baseFitCoeffs, nrow=1))
  names(baseFitCoeffsDF) <- names(baseFitCoeffs)
  resampleFitCoeffs <- transform(resampleFitCoeffs, method=method)
  baseFitCoeffsDF <- transform(baseFitCoeffsDF, method="orig")
  out <- rbind(as.data.frame(baseFitCoeffsDF), resampleFitCoeffs)
  return(out)
}

doResample <- function(data, model, energyType=c("X","U","Q"), method=c("resample", "residual", "wild", "debug")) {
  ######################
  # data: original data frame for country of interest. This data should NOT be resampled.
  # model: original model returned from nls
  # energyType: one of those listed
  # method:
  #         resample:  resample rows from data. Can result in repeated years
  #         residual:  resamples the residuals and applies them to the data. All years are present.
  #         wild:      same as residuals but randomly select sign of resampled residuals
  ##
  
  energyType <- match.arg(energyType)
  method = match.arg(method)
  energyType <- paste('i', energyType, sep="")
  # model$na.action contains indices of rows that couldn't be used due to missingness.
  keep.ind <- sort(setdiff(1:nrow(data), model$na.action))
  
  if(method == "resample") {
    return( resample( data[keep.ind,] ) )
  }
  
  data[,energyType] <- NA
  data[keep.ind, energyType] <- 
    switch(method,
           "residual" = fitted(model) + resample(resid(model)),
           "wild"     = fitted(model) + resample(resid(model)) * resample(c(-1,1), length(resid(model))),
           "debug"    = fitted(model) + (resid(model)) 
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