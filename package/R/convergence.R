#' @export
fracUnconvergedResampleFitsAll <- function(baseResample){
  ###########################
  # Calculates the fraction of unconverged resamples stored on disk 
  # for all countries and all energy types
  ## 
  out <- data.frame()
  modelType <- "sf"
  for (factor in factors){
    if (factor == "U"){
      unconverged <- lapply(countryAbbrevsU, fracUnconvergedResampleFits, modelType=modelType, 
                            factor=factor, baseResample=baseResample)
      uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
      unconverged <- c(unconverged, uNA)
    } else {
      unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, 
                            factor=factor, baseResample=baseResample)
    }
    unconverged <- c(modelType=modelType, energyType="none", factor=factor, unconverged)
    out <- rbind(out, as.data.frame(unconverged))
  }
  
  modelType <- "cd"
  unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, factor=NULL,
                        baseResample=baseResample)
  unconverged <- c(modelType=modelType, energyType="none", factor=NA, unconverged)
  out <- rbind(out, as.data.frame(unconverged))    
  
  modelType <- "cde"
  for (energyType in energyTypes){
    if (energyType == "U"){
      unconverged <- lapply(countryAbbrevsU, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType,
                            baseResample=baseResample)
      uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
      unconverged <- c(unconverged, uNA)
    } else {
      unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType,
                            baseResample=baseResample)
    }
    unconverged <- c(modelType=modelType, energyType=energyType, factor=NA, unconverged)
    out <- rbind(out, as.data.frame(unconverged))
  }
  
  modelType <- "ces"
  unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType,
                        baseResample=baseResample)
  unconverged <- c(modelType=modelType, energyType="none", factor=NA, unconverged)
  out <- rbind(out, as.data.frame(unconverged))    
  
  for (modelType in c("cese-(kl)e", "cese-(le)k", "cese-(ek)l", "linex")){
    for (energyType in energyTypes){
      if (energyType == "U"){
        unconverged <- lapply(countryAbbrevsU, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType,
                              baseResample=baseResample)
        uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
        unconverged <- c(unconverged, uNA)
      } else {
        unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType,
                              baseResample=baseResample)
      }
      unconverged <- c(modelType=modelType, energyType=energyType, factor=NA, unconverged)
      out <- rbind(out, as.data.frame(unconverged))
    }
  }
  colnames(out) <- c("Model", "Energy", "Factor", countryAbbrevs)
  return(out)
}

#' @export
fracUnconvergedResampleFits <- function(modelType=modelTypes, 
                                        countryAbbrev=countryAbbrevs, 
                                        energyType=energyTypes, 
                                        factor=factors, 
                                        archive=NULL,
                                        baseResample,
                                        ...){
  ###################
  # Gives the fraction of resample fits that did not converge for the 
  # given parameters.
  ##
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  energyType <- match.arg(energyType)
  factor <- match.arg(factor)
  data <- loadResampleDataRefitsOnly(modelType=modelType, countryAbbrev=countryAbbrev, 
                                     energyType=energyType, factor=factor,
                                     archive=archive, baseResample=baseResample)
  nObs <- nrow(data)
  tallyResults <- tally(~isConv, data=data, format="proportion")
  # Grabs the fraction that is converged. We can't simply gather the fraction that
  # has not converged (tallyResults[["0"]]), because there are some times when
  # all resampled fits converge, and there is no "0" item in the 
  # result from tally.
  fracConverged <- tallyResults[["TRUE"]]
  fracUnconverged <- 1.0 - fracConverged
  return(fracUnconverged)
}
