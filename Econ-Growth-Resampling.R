require(mosaic)
require(foreach)
require(doParallel)
require(plyr)
source('Econ-Growth-Functions2.R')

# This file contains code to resample data for economic growth
# functions. The idea is that we can resample the historical
# data and then develop statistical confidence intervals
# from the values of the model coefficients developed from 
# thousands of different resamples.
#
# We do this multiple times and save a file that contains 
# the results for later investigation

timeFileName <- function(pre="",post="") {
  dt <- Sys.time()
  dt <- gsub(" ","-", dt)
  return(paste(pre, dt, post, sep=""))
}

genAllResampleData <- function(method="wild", n=numResamples(), ...) {
  #######################
  # Generates all resampling data for all models using the method specified
  ##
  method <- match.arg(method)
  # Establish the parallel computing resources
  registerDoParallel()
  # Establish the timer
  t_0 <- proc.time()
  # Use the foreach package
  status <- list()
  foreach(ca=countryAbbrevs, .errorhandling="pass") %dopar% {
    status <- c(status,
                genResampleData(modelType="sf",    countryAbbrev=ca, factor="K",     n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="sf",    countryAbbrev=ca, factor="L",     n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="sf",    countryAbbrev=ca, factor="Q",     n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="sf",    countryAbbrev=ca, factor="X",     n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cd",    countryAbbrev=ca,                 n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cde",   countryAbbrev=ca, energyType="Q", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cde",   countryAbbrev=ca, energyType="X", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="ces",   countryAbbrev=ca,                 n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(kl)e",  countryAbbrev=ca, energyType="Q", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(le)k",  countryAbbrev=ca, energyType="Q", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(ek)l",  countryAbbrev=ca, energyType="Q", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(kl)e",  countryAbbrev=ca, energyType="X", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(le)k",  countryAbbrev=ca, energyType="X", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(ek)l",  countryAbbrev=ca, energyType="X", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="linex", countryAbbrev=ca, energyType="Q", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="linex", countryAbbrev=ca, energyType="X", n=n, method=method,...))
  }  
  foreach(ca=countryAbbrevsU, .errorhandling="pass") %dopar% {
    status <- c(status,
    genResampleData(modelType="sf",    countryAbbrev=ca, factor="U",     n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cde",   countryAbbrev=ca, energyType="U", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(kl)e",  countryAbbrev=ca, energyType="U", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(le)k",  countryAbbrev=ca, energyType="U", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="cese-(ek)l",  countryAbbrev=ca, energyType="U", n=n, method=method,...))
    status <- c(status,
    genResampleData(modelType="linex", countryAbbrev=ca, energyType="U", n=n, method=method,...))
  }  
  # Report timer results
  timing <- proc.time() - t_0
  print(timing)
  return(status)
}

genResampleData <- function(modelType=modelTypes,
                            countryAbbrev=countryAbbrevs, 
                            energyType=energyTypes, 
                            factor=factors,
                            method=resampleMethods,
                            n,
                            clobber=TRUE,
                            verbose=FALSE){
  status <- list()  
  path <- getPathForResampleData(modelType=modelType,
                                 countryAbbrev=countryAbbrev, 
                                 energyType=energyType,
                                 factor=factor)
  
  if (file.exists(path)) {
    if (verbose) {
      message(paste(path, "exists"))
    }
    if (! clobber) {
      status[[path]] <- "file existed; not clobbered"
      return(status)
    } else {
      status[[path]] <- "file existed; going to clobber"
    }
  }
  if (verbose) message(paste('Data will be saved in', path))
  status[[path]] <- "creating new file"
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
                                     countryAbbrev=countryAbbrev
                                     )
  # Ensure that the folder exists. showWarnings=FALSE, because we don't care 
  # if the directory already exists.
  dir.create(path=folder, recursive=TRUE, showWarnings=FALSE)

  save(resampleData, file=path)
  status[[path]] <- "file saved"
  return(status)
}

resampleFits <- function(
  modelType=modelTypes,
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
  if (factor == "U" || energyType == "U"){
    # Trim the dataset to include only those years for which U is available.
    data <- subset(data, !is.na(iU))
  }
  # First do a fit without resampling and get these coefficients
  origModel <- switch(modelType,
                      "sf"    = singleFactorModel(data=data, factor=factor, respectRangeConstraints=TRUE),
                      "cd"    = cdModel(data=data, respectRangeConstraints=TRUE),
                      "cde"   = cdeModel(data=data, energyType=energyType, respectRangeConstraints=TRUE),
                      "ces"   = cesModelNoEnergy(data=data),
                      "cese"  = cesModel(countryAbbrev=countryAbbrev, energyType=energyType),
                      "cese-(kl)e"  = fitCES(countryAbbrev=countryAbbrev, nest="(kl)e", energyType=energyType),
                      "cese-(le)k"  = fitCES(countryAbbrev=countryAbbrev, nest="(le)k", energyType=energyType),
                      "cese-(ek)l"  = fitCES(countryAbbrev=countryAbbrev, nest="(ek)l", energyType=energyType),
                      "linex" = linexModel(countryAbbrev=countryAbbrev, energyType=energyType)
                      )
  baseFitCoeffs <- naturalCoef(origModel)

  # Now do a fit with resampling n times and get all of the coefficients
  safeCES <- function(data,origModel,method) {
    myData <- doResample(data=data, origModel=origModel, method=method)
    tryCatch(attr(cesModelNoEnergy(data=myData), "naturalCoeffs"),
             error=function(e) { saveRDS(myData, file=timeFileName("data_failures/CESfail-",".Rds")); return(NULL) }
    )
  }
  safefitCES <- function(countryAbbrev, energyType="Q", nest="(kl)e", algorithm=c("PORT","L-BFGS-B"), data) {
    myData <- doResample(data=data, origModel=origModel, method=method)
    nC <- tryCatch( naturalCoef(fitCES(countryAbbrev=countryAbbrev,
                                energyType=energyType,
                                nest="(kl)e",
                                data=myData)),
             error=function(e) { message(e); saveRDS(myData, file=timeFileName("data_failures/CESEfail-",".Rds")); return(safeDF(NULL)) }
    )
    return( rbind.fill(naturalCoef(origModel), nC)[-1,] )
  }
  resampleFitCoeffs <- switch(modelType,
                              "sf"    = do(n) * attr(x=singleFactorModel(data=doResample(data=data, 
                                                                                         origModel=origModel, 
                                                                                         method=method),
                                                                         factor=factor,
                                                                         respectRangeConstraints=TRUE),
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
                              "ces"   = do(n) * safeCES(data=data, origModel=origModel, method=method),
                              "cese"  =  do(n) * naturalCoef(cesModel(countryAbbrev=countryAbbrev,
                                                                energyType=energyType,
                                                                data=doResample(data=data, 
                                                                                origModel=origModel, 
                                                                                method=method))),
                              "cese-(kl)e" = do(n) * safefitCES(countryAbbrev=countryAbbrev,
                                                                energyType=energyType,
                                                                nest="(kl)e",
                                                                data=doResample(data=data, 
                                                                                origModel=origModel, 
                                                                                method=method)),
                              "cese-(le)k" =  do(n) * safefitCES(countryAbbrev=countryAbbrev,
                                                                energyType=energyType,
                                                                nest="(le)k",
                                                                data=doResample(data=data, 
                                                                                origModel=origModel, 
                                                                                method=method)),
                              "cese-(ek)l" =  do(n) * safefitCES(countryAbbrev=countryAbbrev,
                                                                energyType=energyType,
                                                                nest="(ek)l",
                                                                data=doResample(data=data, 
                                                                                origModel=origModel, 
                                                                                method=method)),
                              "linex" = do(n) * attr(x=linexModel(countryAbbrev=countryAbbrev,
                                                                  energyType=energyType,
                                                                  data=doResample(data=data, 
                                                                                  origModel=origModel, 
                                                                                  method=method)),
                                                     which="naturalCoeffs"),
                              stop("unknown model type")
                              )
  # Combine the results and return
  # baseFitCoeffsDF <- as.data.frame(matrix(baseFitCoeffs, nrow=1))
  # names(baseFitCoeffsDF) <- names(baseFitCoeffs)

#  names(baseFitCoeffs) <- gsub("-",".", names(baseFitCoeffs))
#  print(setdiff(names(resampleFitCoeffs), names(baseFitCoeffs)))
#  print(setdiff(names(baseFitCoeffs), names(resampleFitCoeffs)))
  baseFitCoeffs <- transform(baseFitCoeffs, method="orig")
#  print(str(resampleFitCoeffs))
  resampleFitCoeffs <- transform(resampleFitCoeffs, method=method)
  out <- rbind(baseFitCoeffs, resampleFitCoeffs)
  out <- transform(out, countryAbbrev=countryAbbrev)
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