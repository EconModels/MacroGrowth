
#' Creates an string representation of the factors of production
#' 
#' @param formula the formula used for fitting. 
#' The energy variable is extracted from the formula.
#' @param nest an integer vector containing 2 or 3 values. 
#' For 2-value vectors, integers must be 1 or 2.
#' For 3-value vectors, integers must be 1, 2, or 3.
#' @details Factors of production are ordered according to the nest, if present.
#' In the nest argument, the integer 1 indicates the capital stock variable ("iK"),
#' the integer 2 indicates the labor variable ("iL"), and
#' the integer 3 indicates the energy variable (one of "iQ", "iX", or "iU")
#' Nesting positions are given by location in the \code{nest} vector, if present.
#' c(3,1,2) is interpreted as energy and capital stock nested together. 
#' For example, (iX + iK) + (iL).
#' If the \code{nest} vector is not present, factors of production 
#' are returned in the order they appear in the \code{formula}.
#' @return a string representing the nesting of the form "iQ+iK+iL", etc.
#' An empty string if a nest is not involved.
factorString <- function( formula, nest, Kvar=factors$K, Lvar=factors$L, sep="+" ) {
  if (class(formula) == "character"){
    formula <- eval(parse(text=formula))
  }
  matches <- na.omit(match(x=all.vars(formula), table=factors))
  factorsPresent <- factors[matches]
  if (missing(nest) || is.null(nest) || is.na(nest) || (nest=="") || ((class(nest) == "list") && (length(nest)==0))){
    # Simply return the factors of production in the order they appear in the formula.
    return(paste(factorsPresent, collapse=sep))
  }
  # We have a nest.
  if (class(nest) == "list"){
    # nest is a list. Grab the "nest" item and see if it has class integer
    if (class(nest[["nest"]]) == "integer"){
      nest <- nest[["nest"]]
    } else {
      # give up
      stop(paste("Unknown nest =", nest, "in factorString"))
    }
  }
  if (length(nest) != length(factorsPresent)){
    # This is a problem. Need to have as many nest items as factors of production.
    stop(paste("length(nest) =", length(nest), 
               "and length(factorsPresent) =", length(factorsPresent), 
               ": They should be equal."))
  }
  if (length(nest) < 2){
    # Only one factor of production. Return it.
    return(factorsPresent[[1]])
  }
  # Rearrange the factors in the nested order.
  orderedFactors <- factorsPresent[nest]
  return(paste(orderedFactors, collapse=sep))
}

#' Creates an id for this run of resampling
#' 
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A 2- or 3-vector of integers.
#' @param nestStr if used, the string for the nesting
#' @param n the number of resamples being attempted
#' @param sep the separator used to create the id string. Default is " : ".
#' @return a string to be used as the id for this resample
#' @export
fittingID <- function(fun, countryAbbrev, formula, nest=NULL, n, sep=" : "){
  id <- paste(countryAbbrev, fun, formula, factorString(formula=formula, nest=nest), n, sep=sep)
  return(id)
}

#' File name for resample coefficients for the given parameters
#' 
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A vector of 2 or 3 integers.
#' @param sep the separator used to create the id string. Default is "_".
#' @return a string representing the file name for these resample coefficients.
resampleCoeffsFileName <- function(fun, countryAbbrev, formula, nest=NULL, sep="_"){
  # Strip "Model" off the end of fun, if present
  modelType <- sub(pattern="Model", replacement="", x=fun)
  f <- paste(countryAbbrev, modelType, factorString(formula=formula, nest=nest), sep=sep)
  f <- paste0(f, ".Rdata")
  return(f)
}

#' Path to resample coefficients for the given parameters
#' 
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A vector of 2 or 3 integers.
#' @param sep the separator used to create the id string. Default is "_".
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return a string representing the file name for these resample coefficients.
#' @export
resampleCoeffsPath <- function(fun, countryAbbrev, formula, nest=NULL, baseResample, sep="_"){
  f <- resampleCoeffsFileName(fun=fun, countryAbbrev=countryAbbrev, formula=formula, nest=nest, sep=sep)
  path <- file.path(baseResample, f)
  return(path)
}

#' Calculate a resample models file name for the given parameters
#' 
#' @param fun the function used for fitting
#' @param countryAbbrev the country being fitted
#' @param formula the formula used for the fitting
#' @param nest if used, the nest employed for this fit. A vector of 2 or 3 integers.
#' @param sep the separator used to create the id string. Default is "_".
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return a string representing the file name for these resample coefficients.
#' @export
resampleModelsPath <- function(fun, countryAbbrev, formula, nest=NULL, baseResample, sep="_"){
  f <- paste("models", resampleCoeffsFileName(fun=fun, countryAbbrev=countryAbbrev, 
                                              formula=formula, nest=nest, sep=sep),
             sep=sep)
  path <- file.path(baseResample, f)
  return(path)
}

#' Extracts an energyType from a formula
#' 
#' @param formula the formula that was fitted
#' @return a string representing the energyType that was used for the fitting. 
#' NA if the sfModel was used.
energyTypeFromFormula <- function(formula){
  if (class(formula) == "character"){
    formula <- eval(parse(text=formula))
  }
  vars <- all.vars(formula)
  if (length(vars) == 3){
    # for the single-factor model, we have only 3 variables in the formula, response, factor, and time.
    # All other models have longer formulas!
    energyType <- NA
  } else {
    # We have some other type of model, so we can find an energyType
    matches <- na.omit(match(x=vars, table=energyTypes))
    if (length(matches) <= 0){
      energyType <- NA
    } else {
      energyType <- energyTypes[[matches]]
    }
  }
  return(energyType)
}

#' Parses the id created by resampling
#' 
#' @param id an \code{id} as a string
#' @param sep the separator used to create the id string. Default is " : ".
#' @return a named list containing information about the resampling that was carried out
# parseID <- function(id, sep=" : "){
#   info <- strsplit(x=id, split=sep)[[1]]
#   func <- info[1]
#   countryAbbrev <- info[2]
#   formula <- eval(parse(text=info[3]))
#   vars <- all.vars(formula)
#   nestStr <- info[4]
#   n <- info[5]
#   # Now add some additional useful information
#   factor <- factorFromFormula(formula=formula)
#   energyType <- energyTypeFromFormula(formula=formula)
#   if (func == "sfModel"){
#     modelType <- "sf"
#     nest <- NA
#   } else if (func == "cdModel"){
#     if (is.na(energyType)){
#       modelType <- "cd"
#     } else {
#       modelType <- "cde"
#     }
#     nest <- NA
#   } else if (func == "cesModel"){
#     if (is.na(energyType)){
#       modelType <- "ces"
#     } else {
#       modelType <- "cese"
#     }
#     nest <- nestFromFormula(formula=formula)
#   } else if (func == "linexModel"){
#     modelType <- "linex"
#     nest <- NA
#   } else {
#     stop(paste("Unknown func", func, "in parseID."))
#   }
#   out <- list(func=func, countryAbbrev=countryAbbrev, formula=formula, nest=nest, n=n, 
#               modelType=modelType, factor=factor, energyType=energyType)
#   return(out)
# }

#' Path to resample data file
#' 
#' This function returns a string representing the relative file path for the resample data being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ".
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
# getPathForResampleData <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
#   return(doGetPath(prefix="resampleData", modelType=modelType, countryAbbrev=countryAbbrev, 
#                    energyType=energyType, factor=factor, baseResample=baseResample))
# }

#' Path to resample models file
#' 
#' This function returns a string representing the relative file path for the resample models being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ".
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
# getPathForResampleModels <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
#   return(doGetPath(prefix="resampleModels", modelType=modelType, countryAbbrev=countryAbbrev, 
#                    energyType=energyType, factor=factor, baseResample=baseResample))
# }

#' Generates paths to resample coefficients or models in String format
#' 
#' This function returns a string representing the relative file path for the resample coefficients or models being requested.
#' 
#' @param prefix the prefix for the model file names. 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ".
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
# doGetPath <- function(prefix, modelType, countryAbbrev, energyType="iQ", factor="iK", baseResample){
#   if (missing(energyType) || is.na(energyType) || (energyType == "none")){
#     energyType <- "NA"
#   }
#   if (missing(factor) || is.na(factor) || (factor == "none")){
#     factor <- "NA"
#   }
#   folder <- getFolderForResampleData(modelType=modelType, countryAbbrev=countryAbbrev, baseResample=baseResample)   
#   rdat <- ".Rdata"
#   filename <- switch(modelType,
#                      "sf"         = paste(prefix, "-", modelType, "-", countryAbbrev, "-", factor,     rdat, sep=""),
#                      "cd"         = paste(prefix, "-", modelType, "-", countryAbbrev, "-", "NA",       rdat, sep=""),
#                      "cde"        = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
#                      "ces"        = paste(prefix, "-", modelType, "-", countryAbbrev, "-", "NA",       rdat, sep=""),
#                      "cese-(kl)"  = paste(prefix, "-", "ces",     "-", countryAbbrev, "-", "NA",       rdat, sep=""),
#                      "cese-(kl)e" = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
#                      "cese-(le)k" = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
#                      "cese-(ek)l" = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
#                      "linex"      = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
#                      stop(paste("Unknown modelType", modelType, "in doGetPath."))
#   )
#   path <- file.path(folder, filename)
#   return(path)
# }

#' Directory for resample data
#' 
#' This function returns a string representing the relative directory path containing the resample data being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path to the directory containing the data for the requested resample data.
# getFolderForResampleData <- function(modelType=modelTypes, countryAbbrev=countryAbbrevs, baseResample){
#   dr <- baseResample
#   #   countryAbbrev <- match.arg(countryAbbrev)
#   folder <- switch(modelType,
#                    "cde"       = file.path(dr, "cd",      countryAbbrev),
#                    "cese-(kl)" = file.path(dr, "ces",     countryAbbrev),
#                    file.path(dr, modelType, countryAbbrev)
#   )
#   return(folder)
# }

#' Loads and binds data for a CES resample ternary plot.
#'
#' @param nest the desired nest. One of \code{"(kl)"}, \code{"(kl)e"}, \code{"(le)k"}, or \code{"(ek)l"}.
#' If you specify nest=\code{"all"}, you'll get data for all nests. You'll need to specify energyType 
#' if you use nest=\code{"all"}.
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param archive path to a zip file archive
#' @param baseResample the parent directory of all resample data, usually \code{"data_resample"}.
#' @return a data frame containing CES resample data for the given arguments.
loadCESResampleData <- function(nest, energyType="none", archive=NULL, baseResample){
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
  if (energyType == "iU"){
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

#' Creates a data frame containing historical data, the fit to historical data, and resample predictions.
#'
#' @param nest the desired nest. One of \code{"(kl)"}, \code{"(kl)e"}, \code{"(le)k"}, or \code{"(ek)l"}.
#' If you specify nest=\code{"all"}, you'll get data for all nests. You'll need to specify energyType 
#' if you use nest=\code{"all"}.
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param archive path to a zip file archive
#' @param baseResample the parent directory of all resample data, usually \code{"data_resample"}.
#' @return a data frame containing CES data for a "spaghetti" graph.
loadCESSpaghettiGraphData <- function(nest="(kl)", energyType="none", archive=NULL, baseHistorical, baseResample){
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
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
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
  if (is.null(archive)) {
    resampleData <- readRDS(file=path)   
  } else {
    f <- gzcon(unz(archive, path))
    resampleData <- readRDS(f)
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
  # Add several relevant columns to the data frame.
  # Get the unique values in the .id column of the data frame
  id <- unique(resampleData$.id)
  if (length(id) != 1){
    stop(paste("Found more than one unique .id in resampledData:", id))
  }
  # Parse the id into pieces of relevant information
  info <- parseID(id)
  # Add the relevant information to the data frame.
  resampleData$countryAbbrev <- factor(info$countryAbbrev)
  resampleData$modelType <- factor(info$modelType)
  resampleData$factor <- factor(info$factor)
  resampleData$energyType <- factor(info$energyType)
  resampleData$nestStr <- factor(info$nest)
  return(resampleData)
}

#' Loads all resample models, for the base fit and resample fits
#' 
#' This function returns a list that contains all of the resample models and the base model
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev a string representing the country for which you want to load resample models.
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
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
  if (is.null(archive)) {
    resampleModels <- readRDS(file=path)   
  } else {
    f <- gzcon(unz(archive, path))
    resampleModels <- readRDS(f)
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
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param countryAbbrevs a vector of country abbreviations that you want in the resulting data frame
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return a data frame containing resample data for specified countries for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
#' @export
loadAllResampleData <- function(modelType, energyType="none", factor, 
                                countryAbbrevs=countryAbbrevs,
                                archive=NULL, baseResample=NULL){
  if (!missing(energyType) && !missing(factor)){
    stop(paste("energyType =", energyType, "and factor =", factor, 
               "in loadAllResampleData. Didn't expect both to be specified. Can't proceed."))
  }
  if (!missing(energyType)){
    if (energyType == "none" || energyType != "iU"){
      data <- do.call("rbind.fill", lapply(countryAbbrevs, loadResampleData, modelType=modelType, 
                                           energyType=energyType, archive=archive, baseResample=baseResample))
    }
    else {
      # energyType is "iU"
      data <- do.call("rbind.fill", lapply(countryAbbrevsU, loadResampleData, modelType=modelType, 
                                           energyType=energyType, archive=archive, baseResample=baseResample))
    }
  } else if (!missing(factor)){
    if (factor == "iU"){
      data <- do.call("rbind.fill", lapply(countryAbbrevsU, loadResampleData, 
                                           modelType=modelType, factor=factor, 
                                           archive=archive, baseResample=baseResample))
    } else {
      data <- do.call("rbind.fill", lapply(countryAbbrevs, loadResampleData, modelType=modelType, 
                                           factor=factor, archive=archive, baseResample=baseResample))
    }
  } else {
    # Neither energyType nor factor were specified
    data <- do.call("rbind.fill", lapply(countryAbbrevs, loadResampleData, 
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
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, 
#' if the data are to be loaded from a directory or an archive.
#' @return a data frame containing resample data only (not the base fit data) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
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
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, 
#' if the data are to be loaded from the directory.
#' @return a list containing resample models only (not the base fit data) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
loadResampleModelsRefitsOnly <- function(countryAbbrev, modelType, energyType="none", factor="K", 
                                         archive=NULL, baseResample){
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
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, if the data are to be loaded from 
#' the directory.
#' @return coefficients for the base fit (not the resample fits) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
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
#' @param energyType the name of the energy type as it appeared in the original data file.
#' For example, "iQ". If the energyType argument is missing or NA, you'll get data for the CES model without energy.
#' @param factor the name of the factor as it appeared in the original data file.
#' For example, "iK".
#' @param archive path to a .zip archive containing resample model data, if resample data are to be loaded from the archive
#' @param baseResample the relative path of the top-level directory containing the resample data, if the data are to be loaded from 
#' the directory.
#' @return a list containing resample models only (not the base fit data) for the specified country 
#' for the given \code{modelType}, \code{energyType},
#' or \code{factor}.
loadResampleModelsBaseModelOnly <- function(modelType, countryAbbrev, energyType="none", factor, 
                                            archive=NULL, baseResample){
  models <- loadResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, 
                               energyType=energyType, factor=factor, 
                               archive=archive, baseResample=baseResample) 
  # Select the first model, which is the model for the fit to historical data  
  return(models[[1]])
}
