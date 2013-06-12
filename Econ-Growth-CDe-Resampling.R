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
  energyType <- "Q"
  lapply(countryAbbrevs, genCDeResampleData, energyType=energyType, n=n)
  energyType <- "X"
  lapply(countryAbbrevs, genCDeResampleData, energyType=energyType, n=n)
  energyType <- "U"
  lapply(countryAbbrevsU, genCDeResampleData, energyType=energyType, n=n)
  print(proc.time() - t_0)
}

genCDeResampleData <- function(countryAbbrev, energyType, n){
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
                                  n=n)
  folder <- getFolderForCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  # Ensure that the folder exists. showWarnings=FALSE, because 
  dir.create(path=folder, recursive=TRUE, showWarnings=FALSE)
  path <- getPathForCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  save(resampleData, file=path)
}

loadCDeResampleData <- function(countryAbbrev, energyType){
  #############################
  # This function loads previously-saved Cobb-Douglas with energy
  # curve fits from resampled data. The loaded object is
  # a list that contains two named data.frames: 
  # baseFitCoeffs and resampleFitCoeffs. 
  ##
  path <- getPathForCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  load(file=path)
  return(resampleData)
}

getPathForCDeResampleData <- function(countryAbbrev, energyType){
  ######################
  # Returns a string identifying the filename in which we 
  # hold Cobb-Douglas resampled data
  ## 
  filename <- paste("cdeResampleData-", countryAbbrev, "-", energyType, ".Rdata", sep="")
  path <- file.path("data_resample", "cde", countryAbbrev, energyType, filename)
  return(path)
}

getFolderForCDeResampleData <- function(countryAbbrev, energyType){
  ##################
  # Returns a string identifying a folder for resampled data.
  ##
  folder <- file.path("data_resample", "cde", countryAbbrev, energyType)
  return(folder)
}

cdeResampleCoeffProps <- function(cdeResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the cde model
  ##
  baseFitCoeffs <- cdeResampleFits$baseFitCoeffs
  resampleFitCoeffs <- cdeResampleFits$resampleFitCoeffs
  lambdaCI <- qdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  alphaCI <- qdata(p=ciVals, vals=alpha, data=resampleFitCoeffs)
  betaCI <- qdata(p=ciVals, vals=beta, data=resampleFitCoeffs)
  gammaCI <- qdata(p=ciVals, vals=gamma, data=resampleFitCoeffs)
  # Now make a data.frame that contains the information.
  lower <- data.frame(lambda=lambdaCI["2.5%"],
                      alpha=alphaCI["2.5%"],
                      beta=betaCI["2.5%"],
                      gamma=gammaCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(lambda=baseFitCoeffs["lambda"],
                    alpha=baseFitCoeffs["alpha"],
                    beta=baseFitCoeffs["beta"],
                    gamma=baseFitCoeffs["gamma"])
  row.names(mid) <- "CDe"
  upper <- data.frame(lambda=lambdaCI["97.5%"],
                      alpha=alphaCI["97.5%"],
                      beta=betaCI["97.5%"],
                      gamma=gammaCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

cdeResampleFits <- function(countryAbbrev, energyType, respectRangeConstraints=FALSE, n, ...){
  ##################
  # n = number of resamples
  # countryAbbrev = the country you want to study
  # energyType = the type of energy of interest to you
  ##
  set.seed(123) # Provide reproducible results
  # First do a fit without resampling and get these coefficients
  baseFitCoeffs <- attr(x=cdeModel(countryAbbrev=countryAbbrev,
                                   energyType=energyType,
                                   respectRangeConstraints=respectRangeConstraints),
                        which="naturalCoeffs")
  # Now do a fit with resampling n times and get all of the coefficients
  resampleFitCoeffs <- do(n) * attr(x=cdeModel(data=resample(loadData(countryAbbrev=countryAbbrev)), 
                                               energyType=energyType, 
                                               respectRangeConstraints=respectRangeConstraints),
                                    which="naturalCoeffs")
  # Combine the results and return
  out <- list(baseFitCoeffs=baseFitCoeffs, resampleFitCoeffs=resampleFitCoeffs)
  return(out)
}