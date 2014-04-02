
#' @export
cesPredictions <- function(countryAbbrev, energyType="none", nest, forceRun=FALSE,
                           archive=NULL, base="../"){
  #########################
  # Takes the CES fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  # If energyType="none", the CES model without energy will be used.
  # If nest="(kl)", the CES model without energy will be used.
  # forceRun = TRUE will cause the full analysis to be run, which might take FOR-E-VER!
  # forceRun = FALSE will load previously-saved data from disk.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (!(haveDataCES(countryAbbrev, energyType)) && (nest != "(kl)")){
    # If we don't have data for this combination of countryAbbrev and energyType, 
    # return a column of NAs if the above conditions have been met.
    nRows <- 21 # All of these countries need 21 rows.
    df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
    colnames(df) <- "pred"
    return(df)
  }
  if (forceRun){
    model <- bestModel(cesModel2(countryAbbrev=countryAbbrev, energyType=energyType, nest=nest))
  } else {
    if (energyType == "none" || nest=="(kl)"){
      modelType <- "ces"
    } else {
      modelType <- paste("cese-", nest, sep="")
    }
    model <- loadResampleModelsBaseModelOnly(modelType=modelType, countryAbbrev=countryAbbrev, 
                                             energyType=energyType, archive=archive, base=base)
  }
  pred <- fitted(model)
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

#' @export
cesPredictionsColumn <- function(energyType="none", nest){
  #########################
  # Takes the CES fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  # If energyType="none" is specified, the CES model without energy will be used for the predictions.
  # Or, if nest="(kl)" is supplied, the CES model without energy will be used for the predictions,
  # regardless of the type of energy requested.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, cesPredictions, energyType=energyType, nest=nest))  
  if (energyType == "none"){
    colnames(out) <- "predGDP"
  } else {
    colnames(out) <- c(paste("predGDP", energyType, sep=""))
  }
  return(out)
}

