
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
#' @export
getPathForResampleData <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
  ######################
  # Returns a string identifying the entire file path in which we 
  # hold resampled data
  ##
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
#' @export
getPathForResampleModels <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
  return(doGetPath(prefix="resampleModels", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, baseResample=baseResample))
}

#' @export
doGetPath <- function(prefix, modelType, countryAbbrev, energyType="Q", factor="K", baseResample){
  if (energyType == "none"){
    energyType="NA"
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
                     stop(paste("Unknown modelType", modelType, "in getPathForResampleModels."))
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
#' @export
getFolderForResampleData <- function(modelType=modelTypes, countryAbbrev=countryAbbrevs, baseResample){
  dr <- baseResample
  countryAbbrev <- match.arg(countryAbbrev)
  folder <- switch(modelType,
                   "cde"       = file.path(dr, "cd",      countryAbbrev),
                   "cese-(kl)" = file.path(dr, "ces",     countryAbbrev),
                   file.path(dr, modelType, countryAbbrev)
  )
  return(folder)
}

