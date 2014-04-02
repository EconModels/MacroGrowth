
#' @export
cobbDouglasModel <- function(countryAbbrev, energyType="none", data=loadData(countryAbbrev), ...){
  ####################
  # Returns an nls Cobb-Douglas model for the country specified
  # This function dispatches to cdModel or cdeModel based on which energy type is specified.
  # Give an energyType ("Q", "X", or "U") if you want to include an energy term. Supply energyType="none"
  # for a model without energy.
  ##
  if (energyType == "none"){
    # Fit the Cobb-Douglas model without energy.
    return(cdModel(data=data, ...))
  }
  # Fit the Cobb-Douglas model with energy
  return(cdeModel(data=data, energyType=energyType, ...))
}

#' @export
cdResampleCoeffProps <- function(cdResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the cd and cde models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- cdResampleFits[cdResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- cdResampleFits[cdResampleFits[["method"]] != "orig", ]
  lambdaCI <- myqdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  alphaCI <- myqdata(p=ciVals, vals=alpha, data=resampleFitCoeffs)
  betaCI <- myqdata(p=ciVals, vals=beta, data=resampleFitCoeffs)
  gammaCI <- myqdata(p=ciVals, vals=gamma, data=resampleFitCoeffs)
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

#' Cobb-Douglas Predictions
#' 
#' @export
#' @return a data frame with one variable named \code{pred} containing fitted values on the 
#' natural scale.
 
cobbDouglasPredictions <- function(countryAbbrev, energyType){
  #########################
  # Takes the Cobb-Douglas fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (energyType != "none"){
    # Consider this replacement only if energyType has been specified.
    if (!(haveDataCD(countryAbbrev, energyType))){
      # If we don't have data for this combination of countryAbbrev and energyType, 
      # return a column of NAs when the above conditions have been met.
      nRows <- 21 # All of these countries need 21 rows.
      df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
      colnames(df) <- "pred"
      return(df)
    }
  }
  model <- cobbDouglasModel(countryAbbrev, energyType)
  pred <- predict(model) # dont See http://stackoverflow.com/questions/9918807/how-get-plot-from-nls-in-r
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

#' @export
#' @export
cobbDouglasPredictionsColumn <- function(energyType="none"){
  #########################
  # Takes the Cobb-Douglas fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasPredictions, energyType=energyType))
  if (energyType == "none"){
    colnames(out) <- c("predGDP")
  } else {
    colnames(out) <- c(paste("predGDP", energyType, sep=""))
  }
  return(out)
}

#' @export
#' @export
cobbDouglasData <- function(countryAbbrev, energyType="none", archive=NULL, base="../", ...){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the Cobb-Douglas production function given a country.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", "U", or NA. NA means you want a CD model without energy.
  #
  # returns a data.frame of data for the Cobb-Douglas model. 
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: lambda, alpha, beta, gamma, corresponding to the parameters in the model.
  ##
  # First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCD(countryAbbrev, energyType)){
    #Return a column of NAs if the above conditions have been met.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 4 # lambda, alpha, beta, and gamma
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("lambda", "alpha", "beta", "gamma")
    rownames(df) <- c("+95% CI", "CDe", "-95% CI")
    return(df)
  } else if (energyType == "none"){
    # We want Cobb-Douglas without energy
    resampledData <- loadResampleData(modelType="cd", countryAbbrev=countryAbbrev, energyType="none",
                                      archive=archive, base=base)
  } else {
    # We want Cobb-Douglas with energy
    resampledData <- loadResampleData(modelType="cde", countryAbbrev=countryAbbrev, 
                                      energyType=energyType,
                                      archive=archive, base=base)
  }
  statisticalProperties <- cdResampleCoeffProps(resampledData)
  # Set the correct label in the row that shows the base values.
  if (energyType == "none"){
    rownames(statisticalProperties) <- c("+95% CI", "CD", "-95% CI")
  } else {
    rownames(statisticalProperties) <- c("+95% CI", "CDe", "-95% CI")
  }
  return(statisticalProperties)
}



#' @export
cobbDouglasCountryRow <- function(countryAbbrev, energyType="none"){
  ############
  # Creates a row for the Cobb Douglas parameters table for the given country (2-letter code) and energyType (Q, X, or U)
  ##
  dataCD <- cobbDouglasData(countryAbbrev, energyType)
  if (energyType == "none"){
    out <- cbind(dataCD["-95% CI", "lambda"], dataCD["CD", "lambda"], dataCD["+95% CI", "lambda"],
                 dataCD["-95% CI", "alpha"],  dataCD["CD", "alpha"],  dataCD["+95% CI", "alpha"],
                 dataCD["-95% CI", "beta"],   dataCD["CD", "beta"],   dataCD["+95% CI", "beta"])
  } else {
    out <- cbind(dataCD["-95% CI", "lambda"], dataCD["CDe", "lambda"], dataCD["+95% CI", "lambda"],
                 dataCD["-95% CI", "alpha"],  dataCD["CDe", "alpha"],  dataCD["+95% CI", "alpha"],
                 dataCD["-95% CI", "beta"],   dataCD["CDe", "beta"],   dataCD["+95% CI", "beta"],
                 dataCD["-95% CI", "gamma"],  dataCD["CDe", "gamma"],  dataCD["+95% CI", "gamma"])
  }
  return(out)
}


