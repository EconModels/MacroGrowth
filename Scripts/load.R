#' Path to resample data file
#' 
#' This function returns a string representing the relative file path for the resample data being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
getPathForResampleData <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
  return(doGetPath(prefix="resampleData", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, baseResample=baseResample))
}

#' Path to resample models file
#' 
#' This function returns a string representing the relative file path for the resample models being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
getPathForResampleModels <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
  return(doGetPath(prefix="resampleModels", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, baseResample=baseResample))
}

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
doGetPath <- function(prefix, modelType, countryAbbrev, energyType="iQ", factor="iK", baseResample){
  if (missing(energyType) || is.na(energyType) || (energyType == "none")){
    energyType <- "NA"
  }
  if (missing(factor) || is.na(factor) || (factor == "none")){
    factor <- "NA"
  }
  folder <- getFolderForResampleData(modelType=modelType, countryAbbrev=countryAbbrev, baseResample=baseResample)   
  rdat <- ".Rdata"
  filename <- switch(modelType,
                     "sf"         = paste(prefix, "-", modelType, "-", countryAbbrev, "-", factor,     rdat, sep=""),
                     "cd"         = paste(prefix, "-", modelType, "-", countryAbbrev, "-", "NA",       rdat, sep=""),
                     "cde"        = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
                     "ces"        = paste(prefix, "-", modelType, "-", countryAbbrev, "-", "NA",       rdat, sep=""),
                     "cese-(kl)"  = paste(prefix, "-", "ces",     "-", countryAbbrev, "-", "NA",       rdat, sep=""),
                     "cese-(kl)e" = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
                     "cese-(le)k" = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
                     "cese-(ek)l" = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
                     "linex"      = paste(prefix, "-", modelType, "-", countryAbbrev, "-", energyType, rdat, sep=""),
                     stop(paste("Unknown modelType", modelType, "in doGetPath."))
  )
  path <- file.path(folder, filename)
  return(path)
}

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
getFolderForResampleData <- function(modelType=modelTypes, countryAbbrev=countryAbbrevs, baseResample){
  dr <- baseResample
  #   countryAbbrev <- match.arg(countryAbbrev)
  folder <- switch(modelType,
                   "cde"       = file.path(dr, "cd",      countryAbbrev),
                   "cese-(kl)" = file.path(dr, "ces",     countryAbbrev),
                   file.path(dr, modelType, countryAbbrev)
  )
  return(folder)
}