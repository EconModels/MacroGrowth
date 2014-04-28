#' @export
singleFactorPredictions <- function(countryAbbrev, factor, baseHistorical, baseResample, archive=NULL){
  #########################
  # Takes the single-factor fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (!(haveDataSF(countryAbbrev, factor))){
    #If we don't have data for this combination of countryAbbrev and energyType, 
    # return a column of NAs if the above conditions have been met.
    nRows <- 21 # All of these countries need 21 rows.
    df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
    colnames(df) <- "pred"
    return(df)
  }
#   model <- sfModel(countryAbbrev=countryAbbrev, factor=factor)
  model <- loadResampleModelsBaseModelOnly(modelType="sf", countryAbbrev=countryAbbrev, factor=factor, 
                                           baseResample=baseResample, archive=archive)
  pred <- predict(model) #See http://stackoverflow.com/questions/9918807/how-get-plot-from-nls-in-r
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev=countryAbbrev, df=df, baseHistorical=baseHistorical)
  return(df)
}

#' @export
singleFactorPredictionsColumn <- function(factor){
  #########################
  # Takes the single-factor fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, singleFactorPredictions, factor=factor))
  colnames(out) <- c(paste("predGDP", factor, sep=""))
  return(out)
}

#' @export
sfResampleCoeffProps <- function(sfResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the sf models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- sfResampleFits[sfResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- sfResampleFits[sfResampleFits[["method"]] != "orig", ]
  lambdaCI <- myqdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  mCI <- myqdata(p=ciVals, vals=m, data=resampleFitCoeffs)
  # Now make a data.frame that contains the information.
  lower <- data.frame(lambda=lambdaCI["2.5%"],
                      m=mCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(lambda=baseFitCoeffs["lambda"],
                    m=baseFitCoeffs["m"])
  row.names(mid) <- "SF"
  upper <- data.frame(lambda=lambdaCI["97.5%"],
                      m=mCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

#' @export
singleFactorData <- function(countryAbbrev, factor, archive=NULL, baseResample){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the single factor production function for a given a country.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # factor is a string, one of "K", "L", "Q", "X", or "U"
  #
  # returns a data.frame of data for the Cobb-Douglas model. 
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: lambda and m corresponding to the parameters in the model.
  ##
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataSF(countryAbbrev, factor)){
    #Return a column of NAs if we don't have data for this factor
    nRows <- 3 # +95% CI, SF, and -95% CI.
    nCols <- 2 # lambda, m
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("lambda", "m")
    rownames(df) <- c("+95% CI", "SF", "-95% CI")
    return(df)
  }
  resampledData <- loadResampleData(modelType="sf", countryAbbrev=countryAbbrev, factor=factor,
                                    archive=archive, baseResample=baseResample)
  statisticalProperties <- sfResampleCoeffProps(resampledData)
  return(statisticalProperties)
}

#' @export
singleFactorCountryRow <- function(countryAbbrev, factor, baseResample){
  ############
  # Creates a row for the single factor parameters table for the given country (2-letter code) and factor.
  ##
  dataSF <- singleFactorData(countryAbbrev=countryAbbrev, factor=factor, baseResample=baseResample)
  out <- cbind(dataSF["-95% CI", "lambda"], dataSF["SF", "lambda"], dataSF["+95% CI", "lambda"],
               dataSF["-95% CI", "m"],  dataSF["SF", "m"],  dataSF["+95% CI", "m"])
  return(out)
}

#' @export
singleFactorParamsDF <- function(factor){
  ########################
  # Aggregates the single-factor results intoa a big data frame for the given factor
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataSF <- do.call("rbind", lapply(countryAbbrevs, singleFactorCountryRow, factor=factor))
  #Add names to the rows and columns
  colnames(dataSF) <- c("lowerCI_lambda", "lambda", "upperCI_lambda", "lowerCI_m", "m", "upperCI_m")
  rownames(dataSF) <- countryAbbrevs
  dataSF <- data.frame(dataSF)
  return(dataSF)
}
