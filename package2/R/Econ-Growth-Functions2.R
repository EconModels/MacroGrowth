
#' @export
factorsForModel <- function(modelType) {
  if (modelType %in% c('sf')) return(factors)
  if (modelType %in% c('cd')) return("K")
  return(energyTypes)
}




#' @export
getHistory <- function(model) {
  #####################
  # Extracts history from a model
  ##
  out <- metaData(model)$history
  return(out)
}


#' @export
extractAllMetaData <- function(model, digits=6, ...) {
  ###########################
  # This function extracts metadata from a model.
  # It works with both CES models (in which case model is actually a list of 
  # all the models that were tried) and other models.
  ## 
  if (is.list(model) && all( sapply( model, function(x) inherits(x, "cesEst") ) ) ) { 
    # We have a CES model. Want to extract both the coeffs and the sse values.
    # Get sse values.
    sseVals <- safeDF(NULL, nrow=1) # We'll fill this data.frame as we go.
    for (mod in model){
      # Loop over all of the models in the incoming list
      hist <- as.character(attr(mod, "meta")$"history")
      # Create the column name that we'll use. Form is "sse.hist"
      colName <- paste("sse.", hist, sep="")
      # Create a data.frame with the sse value
      sseDF <- safeDF(attr(mod, "naturalCoeffs")["sse"])
      # Give it a unique column name
      colnames(sseDF) <- colName
      # Add to the sseVals data.frame.
      sseVals <- cbind(sseVals, sseDF)
    }
    # Get coefficients from the best model
    bestMod <- bestModel(model, digits=digits)
    out <- cbind( safeDF(naturalCoef(bestMod)), safeDF(metaData(bestMod)), sseVals )
  } else {
    # We have a generic model
    out <- cbind( safeDF(naturalCoef(model)), safeDF(metaData(model)) )
  }
  return(out)
}

#' @export
chooseCESControl <- function(algorithm){
  ####################
  # This function chooses the CES control parameter
  # based on whether we want PORT or L-BRGS-B.
  ##
  control <- switch(algorithm,
                    "PORT" = list(iter.max=2000, eval.max=2000),
                    "L-BFGS-B" = list(maxit=5000),
                    list()
  )
  return(control)
}





#' @export
getSeed <- function(){
  ######################
  # Returns the seed that we'll use for all resampling. I'm putting 
  # the seed into a function so that it is accessible from 
  # many places (including the paper, should we choose to include it there).
  ##
  return(123)
}


#' @export
nResamples <- function(modelType=modelTypes, 
                       countryAbbrev=countryAbbrevs, 
                       energyType=energyTypes, 
                       factor=factors, 
                       archive=NULL, base=base,
                       ...){
  ###################
  # Gives the number of resample fits for the 
  # given parameters.
  ##
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  energyType <- match.arg(energyType)
  factor <- match.arg(factor)
  data <- loadResampleDataRefitsOnly(modelType=modelType, countryAbbrev=countryAbbrev, 
                                     energyType=energyType, factor=factor,
                                     archive=archive, base=base)
  nObs <- nrow(data)
  return(nObs)
}


#' @export
numResamples <- function(){
  ####################
  # Gives the desired number of resamples to be performed.
  ##
  return(1000)
}
