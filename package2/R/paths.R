
#' @export
getPathForResampleData <- function(modelType, countryAbbrev, energyType="none", factor, base="../"){
  ######################
  # Returns a string identifying the entire file path in which we 
  # hold resampled data
  ##
  return(doGetPath(prefix="resampleData", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, base=base))
}

#' @export
getPathForResampleModels <- function(modelType, countryAbbrev, energyType="Q", factor="K", base="../"){
  ######################
  # Returns a string identifying the entire file path in which we 
  # hold resampled models
  ##
  return(doGetPath(prefix="resampleModels", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, base=base))
}

#' @export
doGetPath <- function(prefix, modelType, countryAbbrev, energyType="Q", factor="K", base="../"){
  if (energyType == "none"){
    energyType="NA"
  }
  folder <- getFolderForResampleData(modelType=modelType, countryAbbrev=countryAbbrev, base=base)   
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

#' @export
getFolderForResampleData <- function(modelType=modelTypes, countryAbbrev=countryAbbrevs,
                                     base = getOption('heun_data_resample')
                                     ){
  ##################
  # Returns a string identifying a folder for resampled data.
  ##
  dr <- base
  if (is.null(dr)) dr <- "../data_resample"
  countryAbbrev <- match.arg(countryAbbrev)
  folder <- switch(modelType,
                   "cde"       = file.path(dr, "cd",      countryAbbrev),
                   "cese-(kl)" = file.path(dr, "ces",     countryAbbrev),
                   file.path(dr, modelType, countryAbbrev)
                   )
  return(folder)
}

