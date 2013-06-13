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
  n=10000 # 10,000 samples are probably sufficient
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
  # Ensure that the folder exists. showWarnings=FALSE, because we don't care 
  # if the directory already exists.
  dir.create(path=folder, recursive=TRUE, showWarnings=FALSE)
  path <- getPathForCDeResampleData(countryAbbrev=countryAbbrev, energyType=energyType)
  save(resampleData, file=path)
}

cdeResampleFits <- function(countryAbbrev, energyType, respectRangeConstraints=FALSE, n, ...){
  ##################
  # This function creates n resampled curve fits and returns them.
  # The returned object is a list with the first item being the base fit to the 
  # actual historical data
  # The second object is a a data.frame of n resampled fits.
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