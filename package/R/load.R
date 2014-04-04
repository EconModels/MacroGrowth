#' @export
loadCESResampleData <- function(nest, energyType="none", archive=NULL, baseResample){
  #################################
  # Loads and binds data for a CES resample ternary plot.
  # If the energyType argument is missing or NA, you'll get data for the CES model without energy.
  # If you specify nest="all", you'll get data for all nests. You'll need to specify energyType if you use nest="all"
  ##
  if (energyType == "none" || nest=="(kl)"){
    # Desire CES without energy.
    data <- loadAllResampleData(modelType="ces", countryAbbrevsOrder=countryAbbrevs, energyType="none", 
                                archive=archive, baseResample=baseResample)
    data$nest <- "(kl)"
    return(data)
  }
  # We have an energyType
  if (nest == "all"){
    # Data for all nest options is desired.
    # Recursively call this function and rbind.fill the results together.
    allNests <- lapply( cesNests, loadCESResampleData, energyType=energyType, archive=archive, baseResample=baseResample )
    outgoing <- do.call(rbind.fill, allNests)
    # Now set the order for the factors of the nests.
    # Doing so sets the order of appearance on graphs.
    outgoing$nest <- factor(outgoing$nest, levels=cesNests)
    return(outgoing)
  }
  modelType <- paste("cese-", nest, sep="")
  if (energyType == "U"){
    data <- loadAllResampleData(modelType=modelType, 
                                energyType=energyType,
                                countryAbbrevsOrder=countryAbbrevsForGraphU,
                                archive=archive,
                                baseResample=baseResample)
  } else {
    data <- loadAllResampleData(modelType=modelType, energyType=energyType,
                                countryAbbrevsOrder=countryAbbrevs, 
                                archive=archive, 
                                baseResample=baseResample)
  }
  # Add the nest argument to the data.
  data$nest <- nest
  return(data)
}

#' @export
loadCESSpaghettiGraphData <- function(nest="(kl)", energyType="none", archive=NULL, baseHistorical, baseResample){
  ################################
  # Creates a data frame containing historical data, the fit to historical data, and 
  # resample predictions.
  ## 
  # We want all nests.
  if (nest == "all"){
    # Data for all nest options is desired.
    # Ensure that we have an energyType
    if (energyType == "none"){
      stop('Need to include an energy type if nest = "all"')
    }
    # Recursively call this function and rbind.fill the results together.
    allNests <- lapply( cesNests, loadCESSpaghettiGraphData, energyType=energyType, 
                        archive=archive, baseHistorical=baseHistorical, baseResample=baseResample )
    outgoing <- do.call(rbind.fill, allNests)
    # Now set the order for the factors of the nests
    outgoing$nest <- factor(outgoing$nest, levels=cesNests)
    return(outgoing)
  }
  # We don't want all of the nests. Do the nest that is desired.
  # Put the historical data in a data.frame. 
  # We apply the nest argument as given to the data frame. 
  # Doing so assists with graphing later.
  actual <- loadData(baseHistorical=baseHistorical)
  actual <- actual[c("Year", "iGDP", "Country")]
  actual$ResampleNumber <- NA
  actual$Type <- "actual"
  actual$Resampled <- FALSE
  actual$Energy <- NA
  actual$nest <- nest
  
  # Put the fits to historical data in a data.frame
  # Note that if we get nest="kl",the cesPredictionsColumn function 
  # gives the CES model without energy, 
  # regardless of which energy type is passed in here.
  prediction <- cesPredictionsColumn(energyType=energyType, nest=nest, baseHistorical=baseHistorical, baseResample=baseResample)
  pred <- actual
  # Replace the historical GDP data with the predicted GDP data, which is in column 1.
  pred$iGDP <- prediction[,1]
  pred$ResampleNumber <- NA
  pred$Type <- "fitted"
  pred$Resampled <- FALSE
  pred$Energy <- energyType
  pred$nest <- nest
  
  # Remove rows where predicted GDP is NA, i.e., those rows where we don't have a prediction.
  pred <- subset(pred, !is.na(iGDP))
  
  # Remove rows where we don't need historical data or predictions, 
  # specifically those times when we won't have a prediction.
  if (!missing(energyType)){
    if (energyType == "U" && nest != "(kl)"){
      actual <- subset(actual, Country %in% countryAbbrevsU)
      pred <- subset(pred, Country %in% countryAbbrevsU)
    }
  }  
  
  if (energyType == "none" || nest=="(kl)"){
    modelType <- "ces"
    # May need to ensure that the nest is set to "(kl)" when there is no energy involved.
    # We may have got here with a missing or NA nest.
    nest <- "(kl)"
  } else {
    modelType <- paste("cese-", nest, sep="")
  }
  
  # Figure out which countries we need to loop over.
  if (energyType == "none" || energyType == "Q" || energyType == "X" || nest == "(kl)") {
    countryAbbrevs <- countryAbbrevs
  } else if (energyType == "U"){
    countryAbbrevs <- countryAbbrevsForGraphU
  } else {
    warning(paste("Unknown energyType", energyType))
    return(NULL)
  }
  # Put all of the resamples in a list that will be converted to a data.frame
  dfList <- list()
  for (countryAbbrev in countryAbbrevs){
    # Get the raw data for this country
    historical <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
    if (! missing(energyType) && energyType != "none"){
      # Don't do this test if we are missing energy.
      if (energyType == "U" && nest != "(kl)"){
        # subset historical to include only years for which U is available.
        # But, only if we are using U and if we are not using the (kl) nest.
        # If we have the (kl) nest, we are not actually using U, even if we specified it.
        # We might say both (kl) and U if we are looping over nests with U involved.
        historical <- subset(historical, !is.na(iU))
      }
    }
    years <- data.frame(Year = historical$Year)
    # Get the list of resample models for this country.
    resampleModels <- loadResampleModelsRefitsOnly(countryAbbrev=countryAbbrev, 
                                                   modelType=modelType, 
                                                   energyType=energyType, 
                                                   archive=archive, baseResample=baseResample)
    # Add each model's prediction to the data.frame    
    nResamples <- length(resampleModels)
    # Get the number of years from fitted(resampleModels[[1]]), because not
    # all models cover all the years.
    nYears <- length(fitted(resampleModels[[1]]))
    dfList[[countryAbbrev]] <- data.frame(
      Year = rep(historical$Year, nResamples),
      iGDP = unlist(lapply( resampleModels, fitted )),
      Country = countryAbbrev,
      ResampleNumber = rep( 1:nResamples, each=nYears ),
      Type = "fitted",
      Resampled = TRUE,
      Energy = energyType,
      nest = nest
    )
  }
  
  # Now rbind everything together and return.  
  outgoing <- do.call("rbind", c(list(actual,pred), dfList) )
  # Ensure that the country factor is in the right order
  outgoing$Country <- factor(outgoing$Country, levels=countryAbbrevs)
  return(outgoing)
}

#' Loads all resample data, for the base fit and resample fits
#' 
#' This function returns a data frame that contains all of the resample model coefficients
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev a string representing the country for which you want to load resample data.
#' @param energyType one of \code{"none"}, \code{"Q"} (for thermal energy), \code{"X"} (for exergy), 
#' or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param archive the relative path to a .zip archive containing resample model data, 
#' if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, 
#' if the data are to be loaded from a directory or an archive.
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadResampleData <- function(modelType, countryAbbrev, energyType="none", factor=NA, 
                             archive=NULL, baseResample){
  path <- getPathForResampleData(modelType=modelType, countryAbbrev=countryAbbrev, 
                                 energyType=energyType, factor=factor, baseResample=baseResample)
  # The name of the object loaded by this call is resampleData.
  if (is.null(archive)) {
    load(file=path) 
  } else {
    f <- unz(archive, path)
    load(f)
    close(f)
  }
  if ("sigma" %in% names(resampleData) ){
    sigmaTrans <- ifelse(resampleData$sigma < 2, resampleData$sigma, 1.5 - resampleData$rho )
    resampleData$sigmaTrans <- sigmaTrans
  }  
  if ("sigma_1" %in% names(resampleData) ){
    sigmaTrans_1 <- ifelse(resampleData$sigma_1 < 2, resampleData$sigma_1, 1.5 - resampleData$rho_1 )
    resampleData$sigmaTrans_1 <- sigmaTrans_1
  }
  # Ensure that countryAbbrev comes in as a factor (not a string)
  resampleData$countryAbbrev <- factor(resampleData$countryAbbrev)
  resampleData$model <- modelType
  resampleData$energy <- energyType
  resampleData$factor <- factor
  return(resampleData)
}

#' Loads all resample models, for the base fit and resample fits
#' 
#' This function returns a list that contains all of the resample models and the base model
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev a string representing the country for which you want to load resample models.
#' @param energyType one of \code{"none"}, \code{"Q"} (for thermal energy), \code{"X"} (for exergy), 
#' or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param archive the relative path to a .zip archive containing resample model data, 
#' if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, 
#' if the data are to be loaded from a directory or an archive.
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadResampleModels <- function(modelType, countryAbbrev, energyType="none", factor=NA, 
                               archive=NULL, baseResample){
  path <- getPathForResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, 
                                   energyType=energyType, factor=factor,
                                   baseResample=baseResample)
  # The name of the object loaded by this call is resampleModels.
  if (is.null(archive)) {
    load(file=path) 
  } else {
    f <- unz(archive, path)
    load(f)
    close(f)
  }
  return(resampleModels)
}

#' Loads all resample data, for the base fit and resample fits
#' 
#' This function returns a data frame that contains all of the resample model coefficients
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param countryAbbrevsOrder a vector of country abbreviations in the order you want them to appear in the data frame.
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param base the relative path of the top-level directory containing the resample data, if the data are to be loaded from 
#' a directory or an archive.
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadAllResampleData <- function(modelType, energyType="none", factor, 
                                countryAbbrevsOrder=countryAbbrevs,
                                archive=NULL, baseResample=NULL){
  if (!missing(energyType) && !missing(factor)){
    stop(paste("energyType =", energyType, "and factor =", factor, 
               "in loadAllResampleData. Didn't expect both to be specified. Can't proceed."))
  }
  if (!missing(energyType)){
    if (energyType == "none" || energyType != "U"){
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder, loadResampleData, modelType=modelType, 
                                           energyType=energyType, archive=archive, baseResample=baseResample))
    }
    else {
      # energyType is "U"
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder[1:3], loadResampleData, modelType=modelType, 
                                           energyType=energyType, archive=archive, baseResample=baseResample))
    }
  } else if (!missing(factor)){
    if (factor == "U"){
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder[1:3], loadResampleData, 
                                           modelType=modelType, factor=factor, 
                                           archive=archive, baseResample=baseResample))
    } else {
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder, loadResampleData, modelType=modelType, 
                                           factor=factor, archive=archive, baseResample=baseResample))
    }
  } else {
    # Neither energyType nor factor were specified
    data <- do.call("rbind.fill", lapply(countryAbbrevsOrder, loadResampleData, 
                                         modelType=modelType, archive=archive, baseResample=baseResample))
  }
  return(data)
}

#' Loads resample data, for the resample fits only (ignoring the base fit)
#' 
#' This function returns a data frame that contains all of the resample model coefficients
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev the country for which you want to load resample data
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, 
#' if the data are to be loaded from a directory or an archive.
#' @return a data frame containing resample data only (not the base fit data) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadResampleDataRefitsOnly <- function(modelType, countryAbbrev, energyType="none", 
                                       factor="K", archive=NULL, baseResample){
  data <- loadResampleData(modelType=modelType, countryAbbrev=countryAbbrev, 
                           energyType=energyType, factor=factor,
                           archive = archive, baseResample=baseResample)
  # Select only those rows that aren't the original curve fit
  data <- data[data[["method"]]!="orig", ]
  return(data)
}

#' Loads resample models, for the resample fits only (ignoring the base fit)
#' 
#' This function returns a list of resample models (ignoring the model for the base fit)
#' 
#' @param countryAbbrev the country for which you want to load resample data
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, 
#' if the data are to be loaded from the directory.
#' @return a list containing resample models only (not the base fit data) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadResampleModelsRefitsOnly <- function(countryAbbrev, modelType, energyType="none", factor="K", 
                                         archive=NULL, baseResample){
  ####################
  # Loads models for resampled data only from a previously-run set of resample curve fits
  ##
  models <- loadResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, 
                               energyType=energyType, factor=factor, 
                               archive=archive, baseResample=baseResample)
  # Select only those models that aren't from the curve fit to historical data (which is in position 1)
  len <- length(models)
  # Return everything but the first element (which is the fit to historical data).
  return(models[-1])
}

#' Loads resample data, for the base fit only (ignoring the resamples)
#' 
#' This function returns a data frame that contains base fit coefficients
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev the country for which you want to load resample data
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, if the data are to be loaded from 
#' the directory.
#' @return coefficients for the base fit (not the resample fits) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadResampleDataBaseFitOnly <- function(modelType, countryAbbrev, energyType="none", 
                                        factor="K", archive=NULL, baseResample){
  data <- loadResampleData(modelType=modelType, countryAbbrev=countryAbbrev, 
                           energyType=energyType, factor=factor,
                           archive=archive, baseResample=baseResample) 
  # Select the row containing the original curve fit
  data <- data[data[["method"]]=="orig", ]
  return(data)
}

#' Loads the base resample model (ignoring the resample models)
#' 
#' This function returns the base fit model (ignoring the resample models)
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev the country for which you want to load resample data
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, if the data are to be loaded from 
#' the directory.
#' @return a list containing resample models only (not the base fit data) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadResampleModelsBaseModelOnly <- function(modelType, countryAbbrev, energyType="none", factor, 
                                            archive=NULL, baseResample){
  models <- loadResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, 
                               energyType=energyType, factor=factor, 
                               archive=archive, baseResample=baseResample) 
  # Select the first model, which is the model for the fit to historical data  
  return(models[[1]])
}

#' Load historical economic data
#' 
#' This function returns a data frame containing historical economic data
#' 
#' @param countryAbbrev the country/countries for which you want to load data
#' @param baseHistorical the relative path of the directory containing the historical data.
#' @return a data frame containing historical economic data
#' @export
loadData <-
  function(countryAbbrev, baseHistorical){
    # Read the data file as a table with a header.  
    path <- file.path(baseHistorical, "AllData.txt")
    data <- read.table(file=path, header=TRUE)
    
    if (missing(countryAbbrev)){
      return(data)
    }
    return(subset(data, Country %in% countryAbbrev))
  }
