
#' @export
singleFactorCountryRowsForParamsGraph <- function(countryAbbrev, factor, baseResample){
  ###########################################
  # Creates a number of rows in a data.frame that contain information 
  # about the coefficients of a single factor model for countryAbbrev.
  # Each parameter has its own row with confidence intervals.
  # The country name is in a column. Which parameter is involved is
  # also in a column.
  # 
  # The return type is a data.frame.
  ##
  #Create three rows, one for each parameter. Each row is a data.frame so that it is plottable!
  dataSF <- singleFactorData(countryAbbrev=countryAbbrev, factor=factor, baseResample=baseResample)
  valueRow <- "SF"
  # Create rows for each parameter
  lambdaRow <- data.frame(country = countryAbbrev, 
                          parameter = "lambda", 
                          lowerCI = dataSF["-95% CI", "lambda"], 
                          value = dataSF[valueRow, "lambda"], 
                          upperCI = dataSF["+95% CI", "lambda"])
  mRow <- data.frame(country = countryAbbrev, 
                     parameter = "m",
                     lowerCI = dataSF["-95% CI", "m"],
                     value = dataSF[valueRow, "m"],  
                     upperCI = dataSF["+95% CI", "m"])
  table <- rbind(lambdaRow, mRow)
  return(table)
}

#' @export
createSFParamsGraph <- function(factor){
  #############################
  # Creates a graph with confidene intervals for the single-factor model for the given factor
  ##
  # Figure out the exponent for the desired factor
  if (factor == "K"){
    exponentParameter <- "$\\alpha$"
  } else if (factor == "L"){
    exponentParameter <- "$\\beta$"
  } else if ((factor == "Q") || (factor == "X") || (factor == "U")){
    exponentParameter <- "$\\gamma$"
  }
  # Create a data table with the following columns:
  # country abbrev, parameter (lambda or m), -95% CI, value, +95% CI
  data <- do.call("rbind", lapply(countryAbbrevs, singleFactorCountryRowsForParamsGraph, factor=factor))
  graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                   data = data,
                   centers = value, #identifies where the dots should be placed
                   draw.bands = FALSE, #provides nicer error bars
                   horizontal = FALSE, #makes error bars vertical and puts the countries in the x axis
                   layout = c(2,1), #2 column, 1 row
                   col = "black", #Sets line color to black
                   lwd = 1, #Sets line width to 1.0
                   strip = strip.custom(bg="transparent", factor.levels=c("$\\lambda$ [1/year]", exponentParameter)),
                   ylim = list(c(-0.05, 0.1), c(-0.5, 1.5)), #y axis limits
                   scales = list(cex=scaleTextSize, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 x=list(cex=0.75), #reduces text size so that country abbrevs are legible
                                 y=list(rot=0, relation="free", #allow each axis to be different
                                        at=list(c(0.0, 0.05, 0.1),   #y tick mark for lambda
                                                c(0.0, 0.5, 1.0)
                                        )
                                 )
                   )
  )
  
  return(graph)
}

#' @export
wireCloudPanel <- function(x, y, z, points, showPoints=TRUE, ...) {
  ###############################
  # Creates a panel for use with a mixed wireframe and cloud.
  # See http://stackoverflow.com/questions/1406202/plotting-a-wireframe-and-a-cloud-with-lattice-in-r
  # 
  # x: x variable to be used in the wireframe plot. This will be the x variable from the wireframe plot.
  # y: y variable to be used in the wireframe plot. This will be the y variable from the wireframe plot.
  # z: z variable to be used in the wireframe plot. This will be the z variable from the wireframe plot.
  # points: a data.frame containing columns named x, y, and z.
  # showPoints: a boolean saying whether you want the points to be shown.
  ##
  panel.wireframe(x, y, z, ...)
  if (showPoints){
    #pch=16 gives filled circle plot symbol, cex!=1 makes it a different size.
    panel.cloud(points$x, points$y, points$z, col="red", pch=16, cex=1.5, ...)
  }
}

#' @export
sf3DSSEGraph <- function(countryAbbrev, factor, showOpt=TRUE, baseHistorical, baseResample, archive=NULL){
  #########################
  # Creates a 3-D wireframe graph with SSE on the vertical axis and 
  # lambda and m on the horizontal axes.
  # params: 
  #   countryAbbrev: 2-letter country code (upper case)
  #   factor: the factor of production you want to use in the single-factor model. One of K, L, Q, X, or U
  #   showOpt: tells whether or not to show a dot at the optimium point.
  ##
  # First, make a data.frame with which we want to make predictions
  lambdaSeq <- seq(0.0, 0.05, by=0.01)
  mSeq <- seq(0.0, 1.0, by=0.1)
  newX <- data.frame(expand.grid(lambdaSeq, mSeq))
  colnames(newX) <- c("lambda", "m")
  # Get the actual GDP data
  data <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
  y_act <- data[ ,"iGDP"] # Pick off the GDP column
  y_act <- data.frame(y_act)
  # Need the model to make predictions
#   model <- sfModel(countryAbbrev=countryAbbrev, factor=factor, baseHistorical=baseHistorical)
  model <- loadResampleModelsBaseModelOnly(modelType="sf", countryAbbrev=countryAbbrev, factor=factor, 
                                           baseResample=baseResample, archive=archive)
  # Get information about the optimum point, the point where SSE is minimized
  coefs <- coef(model)
  lambda_opt <- coefs["lambda"]
  gamma_opt <- coefs["m"]
  xOpt <- data.frame(lambda=lambda_opt, m=gamma_opt)
  y_opt_pred <- predict(model, xOpt)
  y_opt_pred <- data.frame(y_opt_pred)
  compareDF_opt <- cbind(y_opt_pred, y_act)
  se_opt <- with(compareDF_opt, (y_opt_pred-y_act)^2)
  compareDF_opt <- cbind(compareDF_opt, se_opt)
  sums_opt <- colSums(compareDF_opt)
  sse_opt <- sums_opt[3]
  # Need to name these x, y, and z so that the custom panel will pick them up.
  optPoint <- data.frame(x=lambda_opt, y=gamma_opt, z=sse_opt)
  rownames(optPoint) <- NULL
  
  # Now gather all the points we need for the surface.
  nObs <- nrow(newX)
  sse <- c(1:nObs) # Build a bogus vector at the start
  # Loop over all the different combinations of lambda and m that are of interest for us.
  for (i in 1:nObs){
    y_pred <- predict(model, newX[i,]) # Get lambda and m for this particular row.
    y_pred <- data.frame(y_pred)
    compareDF <- cbind(y_pred, y_act)
    se <- with(compareDF, (y_pred-y_act)^2) # Makes a column of squared errors at each year
    compareDF <- cbind(compareDF, se) # Bind it to the data frame as an additional column
    # Now get the sse
    sums <- colSums(compareDF)
    sse[i] <- sums[3] # squared error (se) is the 3rd column
  }
  ssedf <- data.frame(sse)
  colnames(ssedf) <- "sse"
  # Now merge the sse column and the newX data to form a data.frame fit for 3-D plotting
  dataFor3DSurface <- cbind(newX, ssedf)
  # Create the figure
  fig <- wireframe(sse ~ lambda * m,
                   data = dataFor3DSurface,
                   panel = wireCloudPanel,
                   points = optPoint,
                   showPoints = showOpt,
                   screen = list(z = -20, x = -55),
                   xlab = "$\\lambda$ [1/year]",
                   ylab = "$\\gamma$",
                   zlab = list(label="$SSE_y$", rot=90),
                   scales = list(arrows=FALSE, 
                                 cex=0.45, 
                                 col="black", 
                                 tck=0.8,
                                 x=list(at=c(0.00, 0.01, 0.02, 0.03)),
                                 y=list(at=c(0.0, 0.5, 1.0)),
                                 z=list(at=c(10, 20, 30))
                   ),
                   xlim = c(0.0, 0.04), # lambda axis
                   ylim = c(0.0, 1.0), # gamma axis
                   zlim = c(0, 30) # sse axis
  )
  return(fig)
}

#' @export
twoVarCloudPlot <- function(data, xCoef, yCoef, xLabel, yLabel, textScaling = 1.0, ...){
  ########################
  # This function makes a cloud plot from 
  # data. The data object should be a
  # data.frame containing columns named xCoef and yCoef and 
  # a column named countryAbbrev.
  # xCoef and yCoef should be vectors containing information to be plotted, 
  # typically data$something.
  # This function constructs a lattice plot from the information.
  ##
  # Calculate x and y for the plot
  # Identify the factor levels.
  nFactors <- length(unique(data$countryAbbrev))
  if (nFactors == 3){
    # We have U data
    factorLevels <- countryNamesAlphU
    countryOrder <- countryOrderForGraphsU
    layoutSpec <- threePanelLayoutSpec
  } else if (nFactors == 9){
    # We have any other factor
    factorLevels <- countryNamesAlph
    countryOrder <- countryOrderForGraphs
    layoutSpec <- ninePanelLayoutSpec
  } else {
    stop(paste("Found", nFactors, "countries in twoVarCloudPlot.",
               "Specifically:", unique(data$countryAbbrev), 
               "Expected 3 or 9 countries. Don't know how to continue."))
  }
  # Attach the xy data to the original resample data so that we retain the country information
  graph <- xyplot(yCoef ~ xCoef | countryAbbrev, data=data, 
                  pch=16, 
                  alpha=0.1, 
                  cex=1,
                  col.symbol = "black", #Controls symbol parameters
                  as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                  index.cond = list(countryOrder), #orders the panels.
                  layout=layoutSpec,
                  scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                              tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                              alternating=FALSE # eliminates left-right, top-bot alternating of axes
                  ), 
                  strip=strip.custom(factor.levels=factorLevels, # Sets text for factor levels
                                     bg="transparent", # Sets background transparent to match the graph itself.
                                     par.strip.text=list(cex=textScaling) # Scales text in the strip.
                  ),
                  xlab=list(label=xLabel, cex=textScaling), 
                  ylab=list(label=yLabel, cex=textScaling)
  )
  return(graph)
}

#' @export
sfResamplePlot <- function(factor, ...){
  ##################
  # A wrapper function for twoVarCloudPlot that binds data for all countries
  # and sends to the graphing function.
  ##
  data <- loadAllResampleData(modelType="sf", factor=factor)
  xLabel <- switch(factor,
                   "K" = "$\\alpha$",
                   "L" = "$\\beta$",
                   "Q" = "$\\gamma$",
                   "X" = "$\\gamma$",
                   "U" = "$\\gamma$"
  )
  yLabel <- "$\\lambda$"
  graph <- standardScatterPlot(loadAllResampleData(model="sf", factor=factor, 
                                                   countryAbbrevsOrder=countryAbbrevsFor3x3Graph), 
                               aes(m, lambda)) + 
    labs(x=xLabel, y=expression(lambda))
  
  return(graph)
}

#' @export
loadCDSpaghettiGraphData <- function(energyType="none", archive=NULL, baseHistorical, baseResample){
  ################################
  # Creates a data frame containing historical data, the fit to historical data, and 
  # resample predictions for Cobb-Douglas models.
  # Call with energyType = "none" for the CD model without energy.
  # Call with energyType = "all" for all energy types
  ##
  if (energyType == "all"){
    energyTypeList <- c("none", "Q", "X", "U")
    # Data for all energy types is desired.
    # Recursively call this function and rbind.fill the results together.
    allEnergies <- lapply( energyTypeList, loadCDSpaghettiGraphData, archive=archive )
    outgoing <- do.call(rbind, allEnergies)
    # Now set the order for the factors of the energy types
    outgoing$Energy <- factor(outgoing$Energy, levels=energyTypeList)
    return(outgoing)
  }
  # Set modelType based on energyType. At the same time, check that energyType is OK.
  if (energyType == "none"){
    modelType <- "cd"
  } else if (energyType == "Q" || energyType == "X" || energyType == "U") {
    modelType <- "cde"
  } else {
    warning(paste("Unknown energyType", energyType))
    return(NULL)
  }
  
  # Put the historical data in a data.frame. 
  actual <- loadData(baseHistorical=baseHistorical)
  actual <- actual[c("Year", "iGDP", "Country")]
  actual$ResampleNumber <- NA
  actual$Type <- "actual"
  actual$Resampled <- FALSE
  actual$Energy <- energyType
  
  # Put the fits to historical data in a data.frame
  prediction <- cobbDouglasPredictionsColumn(energyType=energyType, baseHistorical=baseHistorical)
  pred <- actual
  # Replace the historical GDP data with the predicted GDP data, which is in column 1.
  pred$iGDP <- prediction[,1]
  pred$ResampleNumber <- NA
  pred$Type <- "fitted"
  pred$Resampled <- FALSE
  pred$Energy <- energyType
  
  # Remove rows where predicted GDP is NA, i.e., those rows where we don't have a prediction.
  pred <- subset(pred, !is.na(iGDP))
  
  # Figure out which countries we need to loop over.
  if (energyType == "U"){
    countryAbbrevs <- countryAbbrevsForGraphU
  } else {
    countryAbbrevs <- countryAbbrevs
  }
  # Remove rows where we don't need historical data or predictions, 
  # specifically those times when we won't have a prediction.
  actual <- subset(actual, Country %in% countryAbbrevs)
  pred <- subset(pred, Country %in% countryAbbrevs)
  
  # Put all of the resamples in a list that will be converted to a data.frame
  dfList <- list()
  for (countryAbbrev in countryAbbrevs){
    # Get the raw data for this country
    historical <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
    if (energyType == "U"){
      # subset historical to include only years for which U is available.
      historical <- subset(historical, !is.na(iU))
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
      iGDP = unlist(lapply( resampleModels, yhat)),
      Country = countryAbbrev,
      ResampleNumber = rep( 1:nResamples, each=nYears ),
      Type = "fitted",
      Resampled = TRUE,
      Energy = energyType
    )
  }
  
  # Now rbind everything together and return.  
  outgoing <- do.call("rbind", c(list(actual,pred), dfList) )
  # Ensure that the country factor is in the right order
  outgoing$Country <- factor(outgoing$Country, levels=countryAbbrevs)
  return(outgoing)
}

#' @export
cobbDouglasCountryRowsForParamsGraph <- function(countryAbbrev, energyType="none", baseResample){
  ###########################################
  # Creates a number of rows in a data.frame that contain information 
  # about the coefficients of a Cobb-Douglas model for countryAbbrev.
  # Each parameter has its own row with confidence intervals.
  # The country name is in a column. Which parameter is involved is
  # also in a column.
  # 
  # The return type is a data.frame.
  ##
  #Create three rows, one for each parameter. Each row is a data.frame so that it is plottable!
  if (energyType == "none"){
    valueRow <- "CD"
    dataCD <- cobbDouglasData(countryAbbrev=countryAbbrev, energyType="none", baseResample=baseResample)
  } else {
    valueRow <- "CDe"
    dataCD <- cobbDouglasData(countryAbbrev=countryAbbrev, energyType=energyType, baseResample=baseResample)
  }
  # These rows are common to the "with" and "without" energy cases.
  lambdaRow <- data.frame(country = countryAbbrev, 
                          parameter = "lambda", 
                          lowerCI = dataCD["-95% CI", "lambda"], 
                          value = dataCD[valueRow, "lambda"], 
                          upperCI = dataCD["+95% CI", "lambda"])
  alphaRow <- data.frame(country = countryAbbrev, 
                         parameter = "alpha",
                         lowerCI = dataCD["-95% CI", "alpha"],
                         value = dataCD[valueRow, "alpha"],  
                         upperCI = dataCD["+95% CI", "alpha"])
  betaRow <- data.frame(country = countryAbbrev, 
                        parameter = "beta",
                        lowerCI = dataCD["-95% CI", "beta"],
                        value = dataCD[valueRow, "beta"], 
                        upperCI = dataCD["+95% CI", "beta"])
  if (energyType == "none"){
    table <- rbind(lambdaRow, alphaRow, betaRow)
  } else {
    gammaRow <- data.frame(country = countryAbbrev, 
                           parameter = "gamma",
                           lowerCI = dataCD["-95% CI", "gamma"],
                           value = dataCD[valueRow, "gamma"], 
                           upperCI = dataCD["+95% CI", "gamma"])
    table <- rbind(lambdaRow, alphaRow, betaRow, gammaRow)
  }
  return(table)
}

#' @export
createCDParamsGraph <- function(energyType="none", baseResample){
  #############################
  # Creates a graph with confidence intervals for the Cobb-Douglas model for the given energyType. If you 
  # want the Cobb-Douglas model without energy, supply energyType="none".
  ##
  if (energyType == "none"){
    # Create a data table with the following columns:
    # country abbrev, parameter (lambda, alpha, or beta), -95% CI, value, +95% CI
    data <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRowsForParamsGraph, energyType="none", 
                                    baseResample=baseResample))
    graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                     data = data, 
                     centers = value, #identifies where the dots should be placed
                     draw.bands = FALSE, #provides nicer error bars
                     horizontal = FALSE, #makes error bars vertical and puts the countries in the x axis
                     layout = c(2,2), #2 column, 2 row
                     index.cond = list(c(2,3,1)), #orders the panels as alpha, beta, lambda, gamma
                     #sets labels in strip and background color.
                     strip = strip.custom(factor.levels=c("$\\lambda$ [1/year]", "$\\alpha$", "$\\beta$"), 
                                          bg="transparent"),
                     col = "black", #Sets line color to black
                     lwd = 1, #Sets line width to 1.0
                     ylim = list(c(-0.05, 0.1), c(-0.5, 1.5), c(-0.5, 1.5)), #y axis limits
                     scales = list(cex=scaleTextSize, #controls text size on scales.
                                   tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                   x=list(cex=0.75), #reduces text size so that country abbrevs are legible
                                   y=list(rot=0, relation="free", #allow each axis to be different
                                          at=list(c(0.0, 0.05, 0.1),   #y tick mark for lambda
                                                  c(0.0, 0.5, 1.0), 
                                                  c(0.0, 0.5, 1.0)
                                          )
                                   )
                     )
    )
  } else {
    # Create a data table with the following columns:
    # country abbrev, parameter (lambda, alpha, or beta), -95% CI, value, +95% CI
    data <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRowsForParamsGraph, energyType=energyType,
                                    baseResample=baseResample))
    graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                     data = data, 
                     centers = value, #identifies where the dots should be placed
                     draw.bands = FALSE, #provides nicer error bars
                     horizontal = FALSE, #makes error bars vertical and puts the countries in the x axis
                     layout = c(2,2), #2 column, 2 row
                     index.cond = list(c(2,3,1,4)), #orders the panels as alpha, beta, lambda, gamma
                     #set labels and bg color in strip
                     strip = strip.custom(factor.levels=c("$\\lambda$ [1/year]", "$\\alpha$", "$\\beta$", "$\\gamma$"),  
                                          bg="transparent"),
                     col = "black", #Sets line color to black
                     lwd = 1, #Sets line width to 1.0
                     ylim = list(c(-0.05, 0.1), c(-0.5, 1.5), c(-0.5, 1.5), c(-0.5, 1.5)), #y axis limits
                     scales = list(cex=scaleTextSize, #controls text size on scales.
                                   tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                   x=list(cex=0.75), #reduces text size so that country abbrevs are legible
                                   y=list(rot=0, relation="free", #allow each axis to be different
                                          at=list(c(0.0, 0.05, 0.1),   #y tick mark for lambda
                                                  c(0.0, 0.5, 1.0), 
                                                  c(0.0, 0.5, 1.0),
                                                  c(0.0, 0.5, 1.0)
                                          )
                                   )
                     )
    )
  }
  return(graph)
}

#' @export
cesCountryRowsForParamsGraph <- function(countryAbbrev, energyType="none", nest="(kl)e", baseResample){
  ###########################################
  # Creates a number of rows in a data.frame that contain information 
  # about the coefficients of a CES model for countryAbbrev and energyType
  # Each parameter has its own row with confidence intervals.
  # The country name is in a column. Which parameter is involved is
  # also in a column.
  # Set energyType="none" for the CES model without energy.
  # 
  # The return type is a data.frame.
  ##
  #Create six rows, one for each parameter. Each row is a data.frame so that it is plottable!
  dataCES <- cesData(countryAbbrev=countryAbbrev, energyType=energyType, nest=nest, baseResample=baseResample)
  gammaRow <- data.frame(country = countryAbbrev, 
                         parameter = "gamma", 
                         lowerCI = dataCES["-95% CI", "gamma"], 
                         value = dataCES["CES", "gamma"], 
                         upperCI = dataCES["+95% CI", "gamma"])
  lambdaRow <- data.frame(country = countryAbbrev, 
                          parameter = "lambda",
                          lowerCI = dataCES["-95% CI", "lambda"],
                          value = dataCES["CES", "lambda"],  
                          upperCI = dataCES["+95% CI", "lambda"])
  delta_1Row <- data.frame(country = countryAbbrev, 
                           parameter = "delta_1",
                           lowerCI = dataCES["-95% CI", "delta_1"],
                           value = dataCES["CES", "delta_1"],  
                           upperCI = dataCES["+95% CI", "delta_1"])
  deltaRow <- data.frame(country = countryAbbrev, 
                         parameter = "delta",
                         lowerCI = dataCES["-95% CI", "delta"],
                         value = dataCES["CES", "delta"],  
                         upperCI = dataCES["+95% CI", "delta"])
  sigma_1Row <- data.frame(country = countryAbbrev, 
                           parameter = "sigma_1",
                           lowerCI = dataCES["-95% CI", "sigma_1"],
                           value = dataCES["CES", "sigma_1"],  
                           upperCI = dataCES["+95% CI", "sigma_1"])
  sigmaRow <- data.frame(country = countryAbbrev, 
                         parameter = "sigma",
                         lowerCI = dataCES["-95% CI", "sigma"],
                         value = dataCES["CES", "sigma"],  
                         upperCI = dataCES["+95% CI", "sigma"])
  table <- rbind(gammaRow, lambdaRow, delta_1Row, deltaRow, sigma_1Row, sigmaRow)
  return(table)
}

#' @export
createCESParamsGraph <- function(energyType="none", nest="(kl)e", baseResample){
  #############################
  # Creates a graph with confidence intervals for the CES model for the given energyType and nesting.
  ##
  # Create a data table with the following columns:
  # country abbrev, parameter (gamma, lambda, delta_1, delta, sigma_1, sigma), -95% CI, value, +95% CI
  data <- do.call("rbind", lapply(countryAbbrevs, cesCountryRowsForParamsGraph, energyType=energyType, nest=nest,
                                  baseResample=baseResample))
  graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                   data = data, 
                   centers = value, #identifies where the dots should be placed
                   as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                   draw.bands = FALSE, #provides nicer error bars
                   horizontal = FALSE, #makes error bars vertical and puts the countries in the x axis
                   layout = c(2,3), #2 column, 3 row
                   # Orders the panels as gamma, lambda, delta_1, delta, sigma_1, sigma
                   index.cond = list(c(1,2,3,4,5,6)), 
                   #set labels and bg color in strip
                   strip = strip.custom(factor.levels=c("$\\gamma$", "$\\lambda$ [1/year]", "$\\delta_1$",
                                                        "$\\delta$", "$\\sigma_1$", "$\\sigma$"), bg="transparent"),
                   col = "black", #Sets line color to black
                   lwd = 1, #Sets line width to 1.0
                   ylim = list(c(0.75, 1.25), c(-0.05, 0.1), c(0.0, 1.0),
                               c(0.0, 1.0), c(0.0, 2.0), c(0.0, 2.0)), #y axis limits
                   scales = list(cex=scaleTextSize, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 x=list(cex=0.75), #reduces text size so that country abbrevs are legible
                                 y=list(rot=0, relation="free", #allow each axis to be different
                                        at=list(c(0.75, 1.0, 1.25), c(-0.05, 0.0, 0.05, 0.10),
                                                c(0.0, 0.5, 1.0), c(0.0, 0.5, 1.0), 
                                                c(0.0, 1.0, 2.0), c(0.0, 1.0, 2.0))   #y tick marks
                                 )
                   )
  )
  return(graph)
}



#' @export
loadLinexSpaghettiGraphData <- function(energyType="Q", archive=NULL, baseHistorical, baseResample){
  ################################
  # Creates a data frame containing historical data, the fit to historical data, and 
  # resample predictions for LINEX models.
  # Call with energyType = "all" for all energy types
  ##
  if (energyType == "all"){
    energyTypeList <- c("Q", "X", "U")
    # Data for all energy types is desired.
    # Recursively call this function and rbind the results together.
    allEnergies <- lapply( energyTypeList, loadLINEXSpaghettiGraphData, archive=archive )
    outgoing <- do.call(rbind, allEnergies)
    # Now set the order for the factors of the energy types
    outgoing$Energy <- factor(outgoing$Energy, levels=energyTypeList)
    return(outgoing)
  }
  
  modelType <- "linex"
  
  # Put the historical data in a data.frame. 
  actual <- loadData(baseHistorical=baseHistorical)
  actual <- actual[c("Year", "iGDP", "Country")]
  actual$ResampleNumber <- NA
  actual$Type <- "actual"
  actual$Resampled <- FALSE
  actual$Energy <- energyType
  
  # Put the fits to historical data in a data.frame
  prediction <- linexPredictionsColumn(energyType=energyType, baseHistorical=baseHistorical)
  pred <- actual
  # Replace the historical GDP data with the predicted GDP data, which is in column 1.
  pred$iGDP <- prediction[,1]
  pred$ResampleNumber <- NA
  pred$Type <- "fitted"
  pred$Resampled <- FALSE
  pred$Energy <- energyType
  
  # Remove rows where predicted GDP is NA, i.e., those rows where we don't have a prediction.
  pred <- subset(pred, !is.na(iGDP))
  
  # Figure out which countries we need to loop over.
  if (energyType == "U"){
    countryAbbrevs <- countryAbbrevsForGraphU
  } else {
    countryAbbrevs <- countryAbbrevs
  }
  # Remove rows where we don't need historical data or predictions, 
  # specifically those times when we won't have a prediction.
  actual <- subset(actual, Country %in% countryAbbrevs)
  pred <- subset(pred, Country %in% countryAbbrevs)
  
  # Put all of the resamples in a list that will be converted to a data.frame
  dfList <- list()
  for (countryAbbrev in countryAbbrevs){
    # Get the raw data for this country
    historical <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
    if (energyType == "U"){
      # subset historical to include only years for which U is available.
      historical <- subset(historical, !is.na(iU))
    }
    years <- data.frame(Year = historical$Year)
    # Get the list of resample models for this country.
    resampleModels <- loadResampleModelsRefitsOnly(countryAbbrev=countryAbbrev, 
                                                   modelType=modelType, 
                                                   energyType=energyType, 
                                                   archive=archive,
                                                   baseResample=baseResample)
    # Add each model's prediction to the data.frame    
    nResamples <- length(resampleModels)
    # Get the number of years from fitted(resampleModels[[1]]), because not
    # all models cover all the years.
    nYears <- length(fitted(resampleModels[[1]]))
    dfList[[countryAbbrev]] <- data.frame(
      Year = rep(historical$Year, nResamples),
      iGDP = unlist(lapply( resampleModels, yhat)),
      Country = countryAbbrev,
      ResampleNumber = rep( 1:nResamples, each=nYears ),
      Type = "fitted",
      Resampled = TRUE,
      Energy = energyType
    )
  }
  
  # Now rbind everything together and return.  
  outgoing <- do.call("rbind", c(list(actual,pred), dfList) )
  # Ensure that the country factor is in the right order
  outgoing$Country <- factor(outgoing$Country, levels=countryAbbrevs)
  return(outgoing)
}


#' @export
linexCountryRowsForParamsGraph <- function(countryAbbrev, energyType, baseResample){
  ###########################################
  # Creates a number of rows in a data.frame that contain information 
  # about the coefficients of a LINEX model for countryAbbrev and energyType
  # Each parameter has its own row with confidence intervals.
  # The country name is in a column. Which parameter is involved is
  # also in a column.
  # 
  # The return type is a data.frame.
  ##
  #Create three rows, one for each parameter. Each row is a data.frame so that it is plottable!
  dataLINEX <- linexData(countryAbbrev=countryAbbrev, energyType=energyType, baseResample=baseResample)
  a_0Row <- data.frame(country = countryAbbrev, 
                       parameter = "a_0", 
                       lowerCI = dataLINEX["-95% CI", "a_0"], 
                       value = dataLINEX["LINEX", "a_0"], 
                       upperCI = dataLINEX["+95% CI", "a_0"])
  c_tRow <- data.frame(country = countryAbbrev, 
                       parameter = "c_t",
                       lowerCI = dataLINEX["-95% CI", "c_t"],
                       value = dataLINEX["LINEX", "c_t"],  
                       upperCI = dataLINEX["+95% CI", "c_t"])
  table <- rbind(a_0Row, c_tRow)
  return(table)
}

#' @export
createLINEXParamsGraph <- function(energyType, baseResample){
  #############################
  # Creates a graph with confidence intervals for the LINEX model for the given energyType
  ##
  # Create a data table with the following columns:
  # country abbrev, parameter (a_0, c_t), -95% CI, value, +95% CI
  data <- do.call("rbind", lapply(countryAbbrevs, linexCountryRowsForParamsGraph, energyType=energyType, 
                                  baseResample=baseResample))
  graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                   data = data, 
                   centers = value, #identifies where the dots should be placed
                   draw.bands = FALSE, #provides nicer error bars
                   horizontal = FALSE, #makes error bars vertical and puts the countries in the x axis
                   layout = c(2,1), #2 column, 2 row
                   index.cond = list(c(1,2)), #orders the panels as alpha, beta, lambda, gamma
                   #set labels and bg color in strip
                   strip = strip.custom(factor.levels=c("$a_0$", "$c_t$"), bg="transparent"),
                   col = "black", #Sets line color to black
                   lwd = 1, #Sets line width to 1.0
                   ylim = list(c(-0.05, 1.0), c(-0.5, 5)), #y axis limits
                   scales = list(cex=scaleTextSize, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 x=list(cex=0.75), #reduces text size so that country abbrevs are legible
                                 y=list(rot=0, relation="free", #allow each axis to be different
                                        at=list(c(0.0, 0.5, 1.0), c(0, 1, 2, 3, 4, 5))   #y tick marks
                                 )
                   )
  )
  return(graph)
}

#' @export
CIvsParamPlot <- function(model, param, energyType="none", factor=NA, textScaling=1.0){
  #######################
  # Creates a plot of confidence interval vs. value of a parameter with symbols being the 2-letter abbreviation
  # for each country.
  # 
  # model the model function you want to use. One of SF, CD, CDe, CES, or LINEX.
  # param the parameter you want to plot. The options depend upon the model you want:
  #       SF: lambda or m
  #       CD: lambda, alpha, or beta
  #      CDe: lambda, alpha, beta, or gamma
  #      CES: gamma, lambda, delta_1, sigma_1, or delta
  #     CESe: gamma, lambda, delta_1, sigma_1, delta, or sigma
  #    LINEX: a_0 or c_t
  # energyType the energy type you want. One of Q, X, or U
  # factor the factor you want to use in a single-factor model, if needed. One of K, L, Q, X, or U.
  # textScaling the scale factor for labels on the graph
  ##
  # Get the data from the requested model
  data <- CIvsParamDF(model=model, param=param, energyType=energyType, factor=factor)
  # Also, set limits on the x and y axes if needed
  if (param == "lambda"){
    xLabel <- "$\\lambda$ [1/year]"
    xLimits <- c(0.0, 0.1); atX=c(0.0, 0.05, 0.1)
    yLimits <- c(0.0, 0.2); atY-c(0.0, 0.1, 0.2)
  } else if (param == "m"){
    if (factor == "K"){xLabel <- "$\\alpha$"}
    else if (factor == "L"){xLabel <- "$\\beta$"}
    else if ((factor == "Q") || (factor == "X") || (factor == "U")){xLabel <- "$\\gamma$"}
    xLimits <- c(-0.1, 1.5); atX <- c(0.0, 0.5, 1.0, 1.5)
    yLimits <- c(0.0, 1.5); atY <- c(0.0, 0.5, 1.0, 1.5)
  } else if (param == "alpha"){
    xLabel <- "$\\alpha$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(0.0, 1.5); atY <- c(0.0, 0.5, 1.0, 1.5)
  } else if (param == "beta"){
    xLabel <- "$\\beta$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(-0.1, 1.5); atY <- c(0.0, 0.5, 1.0, 1.5)
  } else if (param == "gamma"){
    xLabel <- "$\\gamma$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(-0.1, 2.0); atY <- c(0.0, 1.0, 2.0)
  } else if (param == "delta_1"){
    xLabel <- "$\\delta_1$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(0.0, 1.5); atY <- c(0.0, 0.5, 1.0, 1.5)
  } else if (param == "sigma_1"){
    xLabel <- "$\\sigma_1$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(0.0, 2.0); atY <- c(0.0, 1.0, 2.0)
  } else if (param == "delta"){
    xLabel <- "$\\delta$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(-0.1, 2.0); atY <- c(0.0, 1.0, 2.0)
  } else if (param == "sigma"){
    xLabel <- "$\\sigma$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(-0.1, 2.0); atY <- c(0.0, 1.0, 2.0)
  } else if (param == "a_0"){
    xLabel <- "$a_0$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(0.0, 1.5); atY <- c(0.0, 0.5, 1.0, 1.5)
  } else if (param == "c_t"){
    xLabel <- "$c_t$"
    xLimits <- c(-0.1, 1.1); atX <- c(0.0, 0.5, 1.0)
    yLimits <- c(0.0, 1.5); atY <- c(0.0, 0.5, 1.0, 1.5)
  }
  yLabel <- paste("Width of 95\\% CI on", xLabel)
  
  # Need to use the indices in the following code, because the column labels will be different for each type
  # of model. In other words, if we're doing CESe with Q and plotting sigma, the data.frame will come 
  # back with column names of "sigma" and "CI" on the first and second column, respectively. 
  # plot <-  xyplot(data$CI ~ data$sigma, ...)
  # would break if we, instead, wanted CESe with Q and plotting delta.
  # So, instead, we use the column indices to pick off the correct variables.
  plot <- xyplot(data[ , 2] ~ data[ , 1], # data[ , 1] contains the the parameter; data[ , 2] contains the CI width
                 xlab=list(label=xLabel),
                 ylab=list(label=yLabel),
                 scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                             tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                             x=list(at=atX),
                             y=list(at=atY)
                 ), 
                 xlim=xLimits, #x axis limits
                 ylim=yLimits, #y axis limits
                 panel = function(x, y){
                   ltext(x=x, y=y, labels=data[ , 4]) # Picks up the 4th column containing the country abbreviations.
                 })
  return(plot)
}

#' @export
CIvsParamLattice <- function(textScaling=1.0, countryAbbrevScaling=1.0){
  ########################
  # Creates a lattice plot that has the size of confidence intervals on the vertical axis and parameter
  # values on the horizontal axis.
  # At this point, this function is NOT a general-purpose function, as the number of permutations
  # of model, parameter, energy type, and factor is too varied to be able to turn into a general-purpose 
  # graph with little time investment at this point.
  # This function makes a lattice plot with the following panels (in the arrangement shown)
  #
  # gammaCDeq   gammaCDex    gammaCDeu
  # deltaCESeq  deltaCESex   deltaCESeu
  # sigmaCESeq  sigmaCESex   sigmaCESeu
  #
  # Parameters and models (gamma for CDe and delta and sigma for CESe) are in rows and energyTypes (q, x, and u) 
  # are in columns.
  #
  # textScaling: the amount by which you want to scale the text on the graph.
  ##
  # Set up plotting parameters that are common to both graphs.
  xTics <- c(0.0, 0.5, 1.0) #x axis tic locations
  yTics <- c(0.0, 1.0, 2.0) #y axis tic locations
  xLimits <- c(-0.2, 1.2) #x axis limits
  yLimits <- c(-0.4, 2.4) #y axis limits
  xLabels <- c("$\\gamma$", "$\\delta$", "$\\sigma$") #x axis labels
  yLabel <- "Width of 95\\% Confidence Interval"
  stripLabels <- c("Cobb-Douglas with $q$", "CES with $q$", "CES with $q$",
                   "Cobb-Douglas with $x$", "CES with $x$", "CES with $x$", 
                   "Cobb-Douglas with $u$", "CES with $u$", "CES with $u$")
  stripBG <- "transparent"
  
  # Build a data.frame that contains coordinates for diagonal lines on the various graphs.
  # Columns are "param", "CI", and "factor" to match the column names later in this function.
  # Diagonal line for the gamma-CD-Q panel
  param <- c(0.0, 1.0)
  CI <- c(0.0, 2.0)
  factor <- c("gammaCDeQ", "gammaCDeQ")
  diagLineGammaCDeQDF <- data.frame(param, CI, factor)
  # Diagonal line for the gamma-CD-X panel
  factor <- c("gammaCDeX", "gammaCDeX")
  diagLineGammaCDeXDF <- data.frame(param, CI, factor)
  # Diagonal line for the gamma-CD-U panel
  factor <- c("gammaCDeU", "gammaCDeU")
  diagLineGammaCDeUDF <- data.frame(param, CI, factor)
  
  # Diagonal line for the delta-CES-Q panel  
  param <- c(1.0, 0.0)
  CI <- c(0.0, 2.0)
  factor <- c("deltaCESeQ", "deltaCESeQ")
  diagLineDeltaCESeQDF <- data.frame(param, CI, factor)
  # Diagonal line for the delta-CES-X panel  
  factor <- c("deltaCESeX", "deltaCESeX")
  diagLineDeltaCESeXDF <- data.frame(param, CI, factor)
  # Diagonal line for the delta-CES-U panel  
  factor <- c("deltaCESeU", "deltaCESeU")
  diagLineDeltaCESeUDF <- data.frame(param, CI, factor)
  
  # Diagonal lines for the sigma-CES-Q panel  
  param <- c(0.0, 1.0, 2.0)
  CI <- c(2.0, 0.0, 2.0)
  factor <- c("sigmaCESeQ", "sigmaCESeQ", "sigmaCESeQ")
  diagLineSigmaCESeQDF <- data.frame(param, CI, factor)
  # Diagonal line for the sigma-CES-X panel  
  factor <- c("sigmaCESeX", "sigmaCESeX", "sigmaCESeX")
  diagLineSigmaCESeXDF <- data.frame(param, CI, factor)
  # Diagonal line for the sigma-CES-U panel  
  factor <- c("sigmaCESeU", "sigmaCESeU", "sigmaCESeU")
  diagLineSigmaCESeUDF <- data.frame(param, CI, factor)
  
  # Now combine all data.frames in the correct order.
  diagLinesDF <- rbind(diagLineGammaCDeQDF, diagLineDeltaCESeQDF, diagLineSigmaCESeQDF,
                       diagLineGammaCDeXDF, diagLineDeltaCESeXDF, diagLineSigmaCESeXDF,
                       diagLineGammaCDeUDF, diagLineDeltaCESeUDF, diagLineSigmaCESeUDF)
  
  # Make an xyplot that contains the diagonal lines.
  diagLinesLattice <- xyplot(CI ~ param | factor, data=diagLinesDF,
                             type="l", col = "black", lty=2, #controls line parameters
                             scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                                         tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                         alternating=FALSE, # eliminates left-right, top-bot alternating of axes
                                         x=list(at=xTics),
                                         y=list(at=yTics)
                             ), 
                             xlim=xLimits, 
                             ylim=yLimits, 
                             xlab=list(label=xLabels),
                             ylab=list(label=yLabel),
                             strip = strip.custom(factor.levels=stripLabels, # Set text for factor levels
                                                  bg=stripBG,
                                                  par.strip.text=list(cex=textScaling) # Scales text in the strip.
                             ),
                             as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
  )
  
  # Build a data.frame that has all the necessary data from the models.
  gammaCDeqDF  <- CIvsParamDF(model="CDe",  param="gamma", energyType="Q")
  deltaCESeqDF <- CIvsParamDF(model="CESe", param="delta", energyType="Q")
  sigmaCESeqDF <- CIvsParamDF(model="CESe", param="sigma", energyType="Q")
  gammaCDexDF  <- CIvsParamDF(model="CDe",  param="gamma", energyType="X")
  deltaCESexDF <- CIvsParamDF(model="CESe", param="delta", energyType="X")
  sigmaCESexDF <- CIvsParamDF(model="CESe", param="sigma", energyType="X")
  gammaCDeuDF  <- CIvsParamDF(model="CDe",  param="gamma", energyType="U")
  deltaCESeuDF <- CIvsParamDF(model="CESe", param="delta", energyType="U")
  sigmaCESeuDF <- CIvsParamDF(model="CESe", param="sigma", energyType="U")
  # Change the name of the first columns (the ones that contain the parameter of interest to us) to "param".
  # Doing this allows us to combine the individual data frames into one large data frame.
  colnames(gammaCDeqDF)[1]  <- "param"
  colnames(deltaCESeqDF)[1] <- "param"
  colnames(sigmaCESeqDF)[1] <- "param"
  colnames(gammaCDexDF)[1]  <- "param"
  colnames(deltaCESexDF)[1] <- "param"
  colnames(sigmaCESexDF)[1] <- "param"
  colnames(gammaCDeuDF)[1]  <- "param"
  colnames(deltaCESeuDF)[1] <- "param"
  colnames(sigmaCESeuDF)[1] <- "param"
  # Create one big data.frame with all of the data.
  data <- rbind(gammaCDeqDF, deltaCESeqDF, sigmaCESeqDF, 
                gammaCDexDF, deltaCESexDF, sigmaCESexDF,
                gammaCDeuDF, deltaCESeuDF, sigmaCESeuDF)
  # Now make the graph.
  dataPlot <- xyplot(CI ~ param | factor, data=data,
                     scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 alternating=FALSE, # eliminates left-right, top-bot alternating of axes
                                 x=list(at=xTics),
                                 y=list(at=yTics)
                     ), 
                     xlim=xLimits, 
                     ylim=yLimits, 
                     xlab=list(label=xLabels),
                     ylab=list(label=yLabel),
                     # Set text for factor levels
                     strip = strip.custom(factor.levels=stripLabels, 
                                          bg=stripBG, # Sets background transparent to match the graph itself.
                                          par.strip.text=list(cex=textScaling) # Scales text in the strip.
                     ),
                     as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                     panel = function(x, y){
                       # Picks up the 4th column containing the country abbreviations.
                       ltext(x=x, y=y, labels=data[ , 4], cex=countryAbbrevScaling) 
                     })
  doubleYPlot <- doubleYScale(diagLinesLattice, dataPlot, add.ylab2=FALSE, use.style=FALSE)
  #return(plot)
  #return(diagLinesLattice)
  return(doubleYPlot)
}

## <<PartialResidualPlots, eval=TRUE>>=
#' @export
createDataForPartialResidualPlot <- function(countryAbbrev, modelType, energyType, nest,
                                             baseHistorical, baseResample, archive=NULL){
  #############################
  # Creates a data.frame containing raw data and residuals for given arguments.
  # The residuals are for a model that does not use energy.
  # countryAbbrev: the country of interest to you
  # modelType: one of "CD" or "CES"
  # energyType: the energy type you want to use. Is required.
  # The name of the column of energyType for countryAbbrev is changed to 
  # "iEToFit"
  # nest: the nest you want to use. Required for modelType="ces". Not used for modelType="cd".
  # returns: a data.frame containing data for countryAbbrev with an additional
  #          column containing the residual for each year. The name of the 
  #          column for energyType is changed to "iEToFit"
  ##
  # Get the model
  if (modelType == "cd" || modelType == "ces"){
    if (modelType == "ces"){
      modelType <- paste("cese-", nest, sep="")
    }
    model <- loadResampleModelsBaseModelOnly(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, 
                                             baseResample=baseResample, archive=archive)
  } else {
    stop(paste("Unknown modelType:", modelType, "in partialResidualPlot. Only 'cd' and 'ces' are supported."))
  }
  # Get the residuals
  resid <- resid(model)
  resid <- data.frame(resid)
  resid <- padRows(countryAbbrev=countryAbbrev, df=resid, baseHistorical=baseHistorical)
  # Load data
  data <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
  data <- cbind(data, resid)
  data <- replaceColName(data=data, factor=energyType, newName="iEToFit")
  return(data)
}

#' @export
createPartialResidualPlot <- function(modelType, countryAbbrev=NA, textScaling=1.0, energyType, nest, 
                                      baseHistorical, baseResample, archive=NULL){
  #################
  # Creates a plot with y residuals vs. energy (e). The y residuals are from the specified modelType without energy.
  # modelType: one of "CD" or "CES"
  # energyType: one of "Q", "X", or "U" to serve as the ordinate of the graph
  # countryAbbrev: set to NA (the default) if you want a lattice graph with all countries shown. Set to one of
  #                US, UK, JP, CN, ZA, SA, IR, TZ, ZM for an individual graph.
  ## 
  xLabelString <- paste("$", tolower(energyType), "$", sep="")
  yLabelString <- paste("$y$ residuals (", modelType, " without energy)", sep="")
  if (! is.na(countryAbbrev)){
    # An individual graph for a single country is desired.
    data <- createDataForPartialResidualPlot(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType,
                                             nest=nest, baseHistorical=baseHistorical, baseResample=baseResample, archive=archive)
    plot <- xyplot(resid ~ iEToFit, data=data,
                   type=c("p"),
                   col="black",
                   scales=list(cex=scaleTextSize*textScaling, #controls text size on scales.
                               tck=scaleTickSize), #controls tick mark length. < 0 for inside the graph.
                   xlab=list(label=xLabelString, cex=textScaling),
                   ylab=list(label=yLabelString, cex=textScaling)
    )
  } else {
    # Lattice graph with all countries is desired.
    data <- do.call("rbind", lapply(X=countryAbbrevsAlph, FUN=createDataForPartialResidualPlot, modelType=modelType, energyType=energyType))
    # If useful work is desired, need to trim the data set.
    if (energyType == "U"){
      # Use data from only those countries that have U defined.
      data <- subset(data, !is.na(iEToFit))
      layout <- threePanelLayoutSpec
      factorLevels <- countryNamesAlphU # Want only those countries for which U is known
      indexCond <- list(countryOrderForGraphsU) # Orders as US, UK, JP
    } else {
      # Use data from all countries, because all countries have Q and X data.
      factorLevels <- countryNamesAlph # We want all countries shown
      layout <- ninePanelLayoutSpec
      indexCond <- list(countryOrderForGraphs) # Orders as US, UK, JP, CN, ZA, SA, IR, TZ, ZM
    }
    xLimits <- c(0.5, 4.5) # Maintain consistent x-axis limits across all graphs
    yLimits <- c(-0.2, 0.2) # Maintain consistent y-axis limits across all graphs
    plot <- xyplot(resid ~ iEToFit | Country, data=data,
                   type = c("p"),
                   index.cond = indexCond, #orders the panels.
                   layout = layout,
                   strip = strip.custom(factor.levels=factorLevels, 
                                        bg="transparent", # Sets background transparent to match the graph itself.
                                        par.strip.text=list(cex=textScaling) # Scales text in the strip.
                   ),
                   as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                   col = "black",
                   xlim=xLimits, 
                   ylim=yLimits, 
                   scales = list(cex=scaleTextSize*textScaling, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 alternating=FALSE), # prevents left/right, top/bot alternating
                   xlab = list(label=xLabelString, cex=textScaling),
                   ylab = list(label=yLabelString, cex=textScaling)
    )
  }
  return(plot)
}
