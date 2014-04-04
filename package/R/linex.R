#' @export
linexPredictions <- function(countryAbbrev, energyType, baseHistorical){
  #########################
  # Takes the LINEX fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (!(haveDataSF(countryAbbrev, energyType))){
    # If we don't have data for this combination of countryAbbrev and energyType, 
    # return a column of NAs if the above conditions have been met.
    nRows <- 21 # All of these countries need 21 rows.
    df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
    colnames(df) <- "pred"
    return(df)
  }
  model <- linexModel(countryAbbrev=countryAbbrev, energyType=energyType, baseHistorical=baseHistorical)
  pred <- predict(model) #See http://stackoverflow.com/questions/9918807/how-get-plot-from-nls-in-r
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev=countryAbbrev, df=df, baseHistorical=baseHistorical)
  return(df)
}

#' @export
linexPredictionsColumn <- function(energyType, baseHistorical){
  #########################
  # Takes the LINEX fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, linexPredictions, energyType=energyType, baseHistorical=baseHistorical))
  colnames(out) <- c(paste("predGDP", energyType, sep=""))
  return(out)
}
