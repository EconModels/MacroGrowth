# The code below will perform two nls fits, one for each 
# reparameterization (AB and CD)
# The answers are similar for the US, but seemingly significantly
# different for, say, TZ
countryAbbrev <- "US"
energyType <- "Q"
respectRangeConstraints <- TRUE
modelAB <- cdeModelAB(countryAbbrev=countryAbbrev, 
                      energyType=energyType, 
                      respectRangeConstraints=respectRangeConstraints)
print(attr(x=modelAB, which="naturalCoeffs"))
modelCD <- cdeModelCD(countryAbbrev=countryAbbrev, 
                      energyType=energyType, 
                      respectRangeConstraints=respectRangeConstraints)
print(attr(x=modelCD, which="naturalCoeffs"))



# This file contains code to resample data for economic growth
# functions. The idea is that we can resample the historical
# data and then develop statistical confidence intervals
# from the values of the model coefficients developed from 
# thousands of different resamples.
#
# We do this multiple times and save a file that contains 
# the results for later investigation

require(mosaic)
source('Econ-Growth-Functions2.R')

cdeResampleFits <- function(countryAbbrev, energyType, respectRangeConstraints=FALSE, n, ...){
  ##################
  # n = number of resamples
  # countryAbbrev = the country you want to study
  # energyType = the type of energy of interest to you
  ##
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

set.seed(123) # Provide reproducible results

# Code for doing lots of resamples

n <- 100 # number of resamples desired

# alpha 95% CIs for different amounts of resampling (with seed = 123)
# CDe with Q for the U.S.
# n      lower     upper   time [s]
# 10     0.2477    0.3151     0.62
# 100    0.2124    0.3351     5.2
# 1000   0.1987    0.3398    47.5
# 10000  0.1971    0.3356   484.7 (8 minutes)
# 30000  0.1960    0.3360
# 100000 0.1975    0.3360 14823.3 (4.2 hours)

# ptm <- proc.time()
# data <- cdeResampleFits(countryAbbrev="ZM", energyType="Q", respectRangeConstraints=TRUE, n=n)
# statProps <- cdeResampleCoeffProps(cdeResampleFits=data)
# print(statProps)
# print(proc.time() - ptm)

# tally( ~ b == 1.0, data= sims )
# xyplot( beta ~ alpha, data= sims)
# qdata( c(0.025, 0.975), alpha, data = sims)
# 
# save(sims, n, file="someSimulations.Rda")