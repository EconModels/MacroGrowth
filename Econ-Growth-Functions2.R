require(lattice)
require(latticeExtra)
require(ggplot2)
require(plyr)    # for rbind.fill
require(car)
require(mosaic)
require(xtable)
require(nlmrt)
require(micEconCES)
require(reshape2) # Provides access to data melting. See http://cran.r-project.org/web/packages/reshape2/reshape2.pdf
# tikz allows use of LaTeX formatting and font in graphs. Allows for a consistent look across the paper.
# See http://r-forge.r-project.org/R/?group_id=440 for instructions on installing tikzDevice.
# require(tikzDevice) 
source("Graphics.R")

# Statistical significance levels. We'll work with 95% CIs
ciLevel <- 0.95
ciHalfLevel <- ciLevel + (1.0-ciLevel)/2.0
ciVals <- c(lower=1-ciHalfLevel, upper=ciHalfLevel)
# List of countries
countryAbbrevs <- c(US="US", UK="UK", JP="JP", CN="CN", ZA="ZA", SA="SA", IR="IR", TZ="TZ", ZM="ZM")
countryAbbrevsAlph <- sort(countryAbbrevs)
countryAbbrevsForGraph <- c(US="US", UK="UK", JP="JP", CN="CN", ZA="ZA", TZ="TZ", SA="SA", IR="IR", ZM="ZM")
countryAbbrevsU <- c(US="US", UK="UK", JP="JP") #Only these countries have useful work data
countryAbbrevsAlphU <- sort(countryAbbrevsU)
countryAbbrevsForGraphU <- c(US="US", UK="UK", JP="JP")
countryNamesAlph <- c(CN="China", IR="Iran", JP="Japan", SA="Saudi Arabia", TZ="Tanzania", UK="United Kingdom", US="USA", ZA="South Africa", ZM="Zambia") #In alphabetical order.
countryNamesAlphU <- c(JP="Japan", UK="United Kingdom", US="USA") #In alphabetical order.
yLimitsForGDPGraphs <- list(c(1,10), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4)) # Alph order
modelTypes <- c('sf', 'cd', 'cde', 'ces', 'cese-(kl)e', 'cese-(le)k', 'cese-(ek)l', 'linex')
cesNests <- c(kl="(kl)", kle="(kl)e", lek="(le)k", ekl="(ek)l")
energyTypes <- c(Q="Q", X="X", U="U") # List of energy types
factors <- c(K="K", L="L", Q="Q", X="X", U="U") # List of factors of production
resampleMethods <- c("resample", "residual", "wild", "debug")

factorsForModel <- function(modelType) {
  if (modelType %in% c('sf')) return(factors)
  if (modelType %in% c('cd')) return("K")
  return(energyTypes)
}

safeMatchArg <- function(arg, choices, several.ok=FALSE) {
  return( tryCatch( match.arg( arg, choices, several.ok=several.ok), 
            error=function(e) { choices[1] } )
  )
}

########### Several global parameters for graphs. Set here and use below to ensure consistent appearance of graphs.
# Set the order for presenting countries in 3x3 lattice graphs. Default is alphabetical. 
# "1" means first alphabetically.
countryOrderForGraphs <- c(7,6,3,1,8,5,4,2,9) # Sets the order as US, UK, JP, CN, ZA, TZ, SA, IR, ZM.
countryOrderForGraphsU <- c(3, 2, 1) # Sets the order as US, UK, JP when we're looking only at energyType=U.
timeTics <- c(1980, 1990, 2000, 2010)
yTicsForIndexedGraphs <- c(1,2,3,4,5,6,7,8,9,10) #y tic mark locations.


# Full page lattice plot sizes
ninePanelLayoutSpec <- c(3,3) #indicates a 3x3 arrangement of panels.
threePanelLayoutSpec <- c(3,1) #indicates a 1 row x 3 column arangement of panels
onePanelLayoutSpec <- c(1,1) #indicates a 1x1 arrangement of panels.

# Graph Heights
maxHeight <- 8.5 #Inches
ninePanelScatterGraphHeight <- maxHeight #Inches
ninePanelTernaryGraphHeight <- 7 #Inches
ninePanelSpaghettiGraphHeight <- 7 #Inches

# Graph Widths
maxWidth <- 6.5 #Inches
fourPanelScatterGraphWidth <- maxWidth #Inches
threePanelScatterGraphWidth <- maxWidth*0.75 #Inches
onePanelScatterGraphWidth <- maxWidth*0.25
fourPanelTernaryGraphWidth <- maxWidth
twoPanelTernaryGraphWidth <- maxWidth*0.45
fourPanelSpaghettiGraphWidth <- maxWidth
twoPanelSpaghettiGraphWidth <- maxWidth*0.55
onePanelSpaghettiGraphWidth <- maxWidth*0.4

presentationGraph1RowHeight <- 2.8 #Inches
presentationGraph2ColWidth <- 2 #Inches
presentationGraph1ColWidth <- 2*presentationGraph2ColWidth
keyTextSize <- 0.85 #85% of normal size
keyColumns <- 1 #Want only 1 column in the key for lattice graphs
defaultKeyXLoc <- 0.01 #x position of the key. This default is good for 9-panel graphs.
defaultKeyYLoc <- 0.95 #y position of the key. This default is good for 9-panel graphs.
# Parameter graphs are tricky to ensure same size. I've found that with height set to 5 in for the 
# cd graphs and 2.93 in for the sf graphs, I get a nearly-exact match with graph size. From these settings I 
# can set up 2 equations:
#    5 in = 2*hgraph + haxis
#    2.93 in = hgraph + haxis
# to estimate that h_axis = 0.86 in and hgraph = 2.05 in.
h_param_graph <- 2.07 # Inches for a single panel
h_param_axis <- 0.86 # Inches (also includes whatever margin is applied)
sfParameterGraphHeight <- h_param_graph + h_param_axis
cdParameterGraphHeight <- 2*h_param_graph + h_param_axis
cesParameterGraphHeight <- 3*h_param_graph + h_param_axis
linexParameterGraphHeight <- h_param_graph + h_param_axis
sfParameterGraphWidth <- maxWidth
cdParameterGraphWidth <- maxWidth
cdTernaryGraphWidth <- maxWidth/2.2
cdSpaghettiGraphWidth <- maxWidth/2.2
cesParameterGraphWidth <- maxWidth
linexParameterGraphWidth <- maxWidth
# Other graph parameters that apply to all graphs
scaleTextSize <- 0.8  #Multiple of normal size
scaleTickSize <- -0.5 #50% of normal size and pointing INWARD!
# Controls nls curve fits. Important thing is warnOnly=TRUE allows resamples to continue, 
# even if there is an error.
nlsControl <- nls.control(maxiter=200, 
                          tol=1e-05, 
                          minFactor=1/1024,
                          printEval=FALSE, #Tells whether to print details of curve fit process.
                          warnOnly=TRUE)
spaghettiGraphLevel <- 0.95 # Controls how much data spread will be shown by the gray band on spaghetti graphs.

loadData <- function(countryAbbrev){
  #################################
  # This function loads data given a country abbreviation.
  # The file name from which data will be loaded is assumed to be of the form 
  # "<countryAbbrev>Data.txt". The file is assumed to exist in a subfolder of this project called "data".
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  #
  # returns a data.frame with the data that has been loaded
  ##
  # Read the data file as a table with a header.  
  fileName <- paste("data/", countryAbbrev, "Data.txt", sep="")
  data <- read.table(file=fileName, header=TRUE)
  return(data)
}

haveDataSF <- function(countryAbbrev, factor){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a single-factor curve fit.
  #
  # returns logical value (TRUE or FALSE)
  ##
  # The design here is to go through several cases where data is unavailable and
  # return FALSE. If we make it through all of those cases, we'll return TRUE.
  if (!(countryAbbrev %in% countryAbbrevs)){
    # If we don't know the country, we don't have data
    return(FALSE)
  }
  if (missing(factor) || is.na(factor)){
    # If we don't have a factor, we don't have data.
    return(FALSE)
  }
  if (!(factor %in% factors)){
    # If we don't know the factor, we don't have data
    return(FALSE)
  }
  if (factor == "U" && (!(countryAbbrev %in% countryAbbrevsU))){
    # We have U data for only a few countries.
    return(FALSE)
  }
  return(TRUE)
}

haveDataCD <- function(countryAbbrev, energyType="none"){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a Cobb-Douglas curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  #
  # returns logical value (TRUE or FALSE)
  ##
  # The design here is to go through several cases where data is unavailable and
  # return FALSE. If we make it through all of those cases, we'll return TRUE.
  if (!(countryAbbrev %in% countryAbbrevs)){
    # If we don't know the country, we don't have data
    return(FALSE)
  }
  if (energyType == "none"){
    # If we don't want energy included, we have data for all known countries. We need to check this case
    # first, BEFORE doing various checks on the energyType.
    return(TRUE)
  }
  if (!(energyType %in% energyTypes)){
    # If we want to include energy, we know only a few types. So, return false if we don't know the energy type.
    return(FALSE)
  }
  if (energyType == "U" && (!(countryAbbrev %in% countryAbbrevsU))){
    # We have U data for only a few countries.
    return(FALSE)
  }
  return(TRUE)
}

haveDataCES <- function(countryAbbrev, energyType="none"){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a CES curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  # This function re-routes to haveDataCD, because the logic is identical.
  #
  # returns logical value (TRUE or FALSE)
  ##
  return(haveDataCD(countryAbbrev, energyType))
}

haveDataLINEX <- function(countryAbbrev, energyType="none"){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a LINEX curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  # This function re-routes to haveDataCD, because the logic is identical.
  #
  # returns logical value (TRUE or FALSE)
  ##
  return(haveDataCD(countryAbbrev, energyType))
}

doPadRows <- function(dataToBePadded, dataThatSuppliesRowCount) {
  #####################
  # This function adds NA rows to the bottom of data.frame dataToBePadded
  # to ensure that it has the the number of observations (rows) as 
  # dataThatSuppliesRowCount. The number of columns comes from dataToBePadded
  # Execution halts if nrow(dataThatSuppliesRowCount) < nrow(dataToBePadded)
  # Returns dataToBePadded with NA rows at the bottom.
  ##
  nRowsToAdd <- nrow(dataThatSuppliesRowCount) - nrow(dataToBePadded)
  if (nRowsToAdd < 0){
    stop(paste("Model data frame has", abs(nRowsToAdd), "fewer rows than target."))
  }
  dfToAppend <- as.data.frame(matrix(NA, ncol=ncol(dataToBePadded), nrow=nRowsToAdd))
  colnames(dfToAppend) <- colnames(dataToBePadded)
  return(rbind(dataToBePadded, dfToAppend))
}

padRows <- function(countryAbbrev, df){
#####################
# This function adds NA rows to the bottom of data.frame df to ensure that it has the 
# the number of observations (rows) as the country data set for countryAbbrev
# This function is a convenience wrapper function that simply loads 
# the data.frame for countryAbbrev and then calls doPadRows
# returns a modified version of df that includes the padded rows filled with "NA".
##
  countryData <- loadData(countryAbbrev)
  return(doPadRows(dataToBePadded=df, dataThatSuppliesRowCount=countryData))
}

columnIndex <- function(data, factor){
  ##############################
  # Returns an integer representing the column index for some data
  # data the data.frame in which you want to change column names
  # factor should be a string and one of Year, Y, K, L, Q, X, or U
  ##
  if (factor == "Year"){
    colName <- "iYear"
  } else if (factor == "Y"){
    colName <- "iGDP"
  } else if (factor == "K"){
    colName <- "iCapStk"
  } else if (factor == "L"){
    colName <- "iLabor"
  } else if (factor == "Q"){
    colName <- "iQ"
  } else if (factor == "X"){
    colName <- "iX"
  } else if (factor == "U"){
    colName <- "iU"
  } else {
    print(paste("Unknown factor:", factor, "in colIndex. Terminating execution."))
    quit()
  }
  # Get the desired column index.
  colIndex <- which(names(data) %in% colName) #Find index of desired column
  return(colIndex)  
}

replaceColName <- function(data, factor, newName){
  ##############################
  # Replaces a column name with the given string
  # data the data.frame that you're working with
  # factor should be a string and one of Year, Y, K, L, Q, X, or U
  # newName should be a string and the desired new name of the column
  # returns data.frame with a new name for one of its factor column.
  ##
  colIndex <- columnIndex(data=data, factor=factor)
  # colnames(data)[colIndex] <- newName #Change desired column name to newName
  data[,newName] <- data[,colIndex]
  return(data)
}

covarianceTable <- function(countryAbbrev){
  ##############################
  # Returns an xtable of covariance information for the specified country.
  ##
  data <- loadData(countryAbbrev)
  if (countryAbbrev %in% countryAbbrevsU){
    # Calculate covariances among the typical variables.
    dataForCovar <- cbind(data$iGDP, data$iCapStk, data$iLabor, data$iQ, data$iX, data$iU)
    # Calculates the covariances among y, k. l, q, x, and u using all available data, namely,
    # 1980-2011 for covariances among y, k, l, q, and x for developed economies
    # 1980-2000 for covariances involving u for developed economies
    # 1991-2011 for covariances among y, k, l, q, and x for developing economies
    # The key here is the use="pairwise.complete.obs" argument.
    covarResults <- data.frame(cor(x=dataForCovar, use="pairwise.complete.obs"))
    names <- c("$y$", "$k$", "$l$", "$q$", "$x$", "$u$")
  } else {
    dataForCovar <- cbind(data$iGDP, data$iCapStk, data$iLabor, data$iQ, data$iX)
    covarResults <- cor(dataForCovar)
    names <- c("$y$", "$k$", "$l$", "$q$", "$x$")
  }
  colnames(covarResults) <- names
  rownames(covarResults) <- names
  return(covarResults)
}

printCovarTable <- function(countryAbbrev){
  ###############################
  # Prints a covariance table for countryAbbrev
  ##
  # Now make an xtable so it displays nicely
  covarResults <- covarianceTable(countryAbbrev)
  covarXtable <- xtable(x=covarResults, 
                        caption=paste(countryAbbrev, "covariance table."), 
                        label=paste("tab:Covariance_", countryAbbrev, sep=""))
  print(covarXtable, 
        caption.placement="top", 
        sanitize.rownames.function = identity,
        sanitize.colnames.function = identity,
        size="\\tiny",
        table.placement="H")
}

createHistoricalLatticeGraph <- function(countryAbbrev, textScaling = 1.0, keyXLoc = defaultKeyXLoc, keyYLoc = defaultKeyYLoc){
  ####################################
  # Creates a graph that displays all of the factors of production for all countries (if you leave off countryAbbrev)
  # or a specific country (if you supply a 2-letter abbreviation for a country that we know).
  # Provide a value for textScaling to scale the size of the text on the graph. This is expecially useful
  # for converting between graphs in a paper and graphs in a beamer presentation.
  # textScaling = 1.0 is good for a paper. textScaling = 0.6 is good for a beamer presentation.
  # keyXLoc and keyYLoc are locations for the graph's legend.
  ##
  # Code for all graphs, regardless of whether we want to focus on a specific country
  graphType <- "b" #b is for both line and symbol
  lineTypes <- c(0, 1, 5, 2) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(0, 2, 2, 1) #line widths. 0 means no line.
  colors <- c("black", "black", "black", "black") #line and symbol colors
  symbols <- c(1, NA, NA, NA)  #NA gives no symbol.
  # Code that deals with items that are specific to whether we want all countries or a specific country.
  if (missing(countryAbbrev)){
    # We want a graph with panels for all countries
    data <- loadData("All")
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
    yLimits <- yLimitsForGDPGraphs
  } else {
    # We want only a specific country
    data <- loadData(countryAbbrev)
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    yLimits <- yLimitsForGDPGraphs[index:index]       # Pick limits for the country we want.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
  }
  graph <- xyplot(iGDP+iCapStk+iLabor+iQ ~ Year | Country, data=data,
                  type = graphType,
                  index.cond = indexCond, #orders the panels.
                  layout = layout, 
                  # Sets strip parameters
                  strip = strip.custom(factor.levels=factorLevels, # Sets text for factor levels
                                       bg="transparent", # Sets background transparent to match the graph itself.
                                       par.strip.text=list(cex=textScaling) # Scales text in the strip.
                  ),
                  as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                  lty = lineTypes, lwd = lineWidths, col = colors, #Controls line parameters
                  pch = symbols, col.symbol = colors, #Controls symbol parameters
                  key=list(text=list(c("$y$", "$k$", "$l$", "$q$")),
                           type=graphType,
                           cex=keyTextSize * textScaling, #controls size of text in the key
                           lines=list(lty=lineTypes, lwd=lineWidths), #controls line types
                           pch=symbols, col=colors, #controls symbol (plot characters) types
                           columns=keyColumns, x=keyXLoc, y=keyYLoc), #controls columns and position of the key
                  scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                              tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                              alternating=FALSE, # eliminates left-right, top-bot alternating of axes
                              x=list(at=timeTics),
                              y=list(relation="free",  #allows each axis to be different
                                     at=yTicsForIndexedGraphs)), #specifies location for tics
                  ylim=yLimits, #y axis limits
                  #axis labels and scaling
                  xlab=list(label="", cex=textScaling), 
                  ylab=list(label="Indexed (1980=1 or 1991=1)", cex=textScaling)
  ) 
  return(graph)
}

naturalCoef <- function(object) {
  if (! "naturalCoeffs" %in% names(attributes(object)) ) return(as.data.frame(matrix(nrow=1, ncol=0)))
  return( attr(object, "naturalCoeffs") )
}

metaData <- function(object) {
  if (! "meta" %in% names(attributes(object)) ) return(as.data.frame(matrix(nrow=1, ncol=0)))
  return( attr(object, "meta") )
}

safeDF <- function(object, nrow=1, ncol=0){
  if (is.null(object)){
    return (data.frame(matrix(nrow=nrow, ncol=ncol)))    
  }
  emptyDataFrame <- FALSE
  if (inherits(object,"data.frame") ) {
    if (nrow(object) + ncol(object) > 0){
      # We're already a data.frame.
      return (object)
    }
    emptyDataFrame <- TRUE
  }
  if ( emptyDataFrame ) {
    return (data.frame(matrix(nrow=nrow, ncol=ncol)))
  }
  return(as.data.frame(object))
}

## <<single-factor functions, eval=TRUE>>=
singleFactorModel <- function(countryAbbrev, data=loadData(countryAbbrev), factor, respectRangeConstraints=FALSE){
  ####################
  # Returns an nls single-factor model for the country and factor specified.
  # factor should be one of "K", "L", "Q", "X", or "U".
  ##
  # We'll change the name of the desired column to "f"
  data <- replaceColName(data, factor, "f")
  # Now do the fit.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  mGuess <- 0.5 # works for almost every country
  if (respectRangeConstraints){
    m <- 1.0
    start <- list(lambda=lambdaGuess)
  } else {
    start <- list(lambda=lambdaGuess, m=mGuess)
  }
  # Runs a non-linear least squares fit to the data. We've replaced beta with 1-alpha for simplicity.
  model <- iGDP ~ exp(lambda*iYear) * f^m
  modelSF <- nls(formula=model, data=data, start = start, control=nlsControl)
  # Build the additional object to add as an atrribute to the output
  if (!respectRangeConstraints){
    m <- coef(modelSF)["m"]
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(modelSF)["lambda"]),
                     m = as.vector(m),
                     sse = sum(resid(modelSF)^2),
                     isConv = modelSF$convInfo$isConv
                     )
  attr(x=modelSF, which="naturalCoeffs") <- naturalCoeffs
  return(modelSF)
}

singleFactorPredictions <- function(countryAbbrev, factor){
  #########################
  # Takes the single-factor fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (!(haveDataSF(countryAbbrev, factor))){
    #If we don't have data for this combination of countryAbbrev and energyType, 
    # return a column of NAs if the above conditions have been met.
    nRows <- 21 # All of these countries need 21 rows.
    df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
    colnames(df) <- "pred"
    return(df)
  }
  model <- singleFactorModel(countryAbbrev=countryAbbrev, factor=factor)
  pred <- predict(model) #See http://stackoverflow.com/questions/9918807/how-get-plot-from-nls-in-r
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

singleFactorPredictionsColumn <- function(factor){
  #########################
  # Takes the single-factor fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, singleFactorPredictions, factor=factor))
  colnames(out) <- c(paste("predGDP", factor, sep=""))
  return(out)
}

sfResampleCoeffProps <- function(sfResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the sf models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- sfResampleFits[sfResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- sfResampleFits[sfResampleFits[["method"]] != "orig", ]
  lambdaCI <- qdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  mCI <- qdata(p=ciVals, vals=m, data=resampleFitCoeffs)
  # Now make a data.frame that contains the information.
  lower <- data.frame(lambda=lambdaCI["2.5%"],
                      m=mCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(lambda=baseFitCoeffs["lambda"],
                    m=baseFitCoeffs["m"])
  row.names(mid) <- "SF"
  upper <- data.frame(lambda=lambdaCI["97.5%"],
                      m=mCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

singleFactorData <- function(countryAbbrev, factor){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the single factor production function for a given a country.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # factor is a string, one of "K", "L", "Q", "X", or "U"
  #
  # returns a data.frame of data for the Cobb-Douglas model. 
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: lambda and m corresponding to the parameters in the model.
  ##
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataSF(countryAbbrev, factor)){
    #Return a column of NAs if we don't have data for this factor
    nRows <- 3 # +95% CI, SF, and -95% CI.
    nCols <- 2 # lambda, m
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("lambda", "m")
    rownames(df) <- c("+95% CI", "SF", "-95% CI")
    return(df)
  }
  resampledData <- loadResampleData(modelType="sf", countryAbbrev=countryAbbrev, factor=factor)
  statisticalProperties <- sfResampleCoeffProps(resampledData)
  return(statisticalProperties)
}

singleFactorCountryRow <- function(countryAbbrev, factor){
  ############
  # Creates a row for the single factor parameters table for the given country (2-letter code) and factor.
  ##
  dataSF <- singleFactorData(countryAbbrev, factor)
  out <- cbind(dataSF["-95% CI", "lambda"], dataSF["SF", "lambda"], dataSF["+95% CI", "lambda"],
               dataSF["-95% CI", "m"],  dataSF["SF", "m"],  dataSF["+95% CI", "m"])
  return(out)
}

singleFactorParamsDF <- function(factor){
  ########################
  # Aggregates the single-factor results intoa a big data frame for the given factor
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataSF <- do.call("rbind", lapply(countryAbbrevs, singleFactorCountryRow, factor=factor))
  #Add names to the rows and columns
  colnames(dataSF) <- c("lowerCI_lambda", "lambda", "upperCI_lambda", "lowerCI_m", "m", "upperCI_m")
  rownames(dataSF) <- countryAbbrevs
  dataSF <- data.frame(dataSF)
  return(dataSF)
}

singleFactorParamsTable <- function(factor){
  ############################
  # Aggregates the single-factor results into a big data table for the given factor.
  ##
  dataSF <- singleFactorParamsDF(factor)
  if (factor == "K"){
    factorString <- "$\\alpha$"
  } else if (factor == "L"){
    factorString <- "$\\beta$"
  } else {
    factorString <- "$\\gamma$"
  }
  colnames(dataSF) <- c(" ", "$\\lambda$", " ", 
                        " ", factorString,  " ")
  rownames(dataSF) <- countryAbbrevs
  tableSF <- xtable(dataSF, 
                    caption=paste("Single-factor model (with $", tolower(factor), 
                                  "$) parameters for 1980--2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
                                  sep=""), 
                    label=paste("tab:SF_Parameters_With_", factor, sep=""),
                    digits = c(1, 4,4,4, 2,2,2),
                    align = "r|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableSF)
}

singleFactorCountryRowsForParamsGraph <- function(countryAbbrev, factor){
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
  dataSF <- singleFactorData(countryAbbrev, factor)
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

printSFParamsTable <- function(factor){
  ###############################
  # Prints a covariance table for the given factor of production
  ##
  print(singleFactorParamsTable(factor),
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny", 
        table.placement="H")
}

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

sf3DSSEGraph <- function(countryAbbrev, factor, showOpt=TRUE){
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
  data <- loadData(countryAbbrev)
  y_act <- data[ ,"iGDP"] # Pick off the GDP column
  y_act <- data.frame(y_act)
  # Need the model to make predictions
  model <- singleFactorModel(countryAbbrev=countryAbbrev, factor=factor)
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
                                                   countryAbbrevsOrder=countryAbbrevsForGraph), 
                               aes(m, lambda)) + 
    labs(x=xLabel, y=expression(lambda))
  
  return(graph)
}

cdModel <- function(countryAbbrev, data=loadData(countryAbbrev), respectRangeConstraints=FALSE, ...){
  ## <<cobb-douglas functions, eval=TRUE>>=
  ####################
  # Returns an nls Cobb-Douglas model (without energy) for the country specified. 
  # No energy is included in the function to be fitted.
  # Note that the argument countryAbbrev is first so that we can use lapply 
  # with a list of country abbreviations.
  ##
  # Run the non-linear least squares fit to the data. No energy term desired in the Cobb-Douglas equation.
  # Establish guess values for alpha and lambda.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  alphaGuess <- 0.7 #0.7 gives good results for all countries.  
  start <- list(lambda=lambdaGuess, alpha=alphaGuess)
  # Runs a non-linear least squares fit to the data. We've replaced beta with 1-alpha for simplicity.
  model <- iGDP ~ exp(lambda*iYear) * iCapStk^alpha * iLabor^(1.0 - alpha)
  modelCD <- nls(formula=model, data=data, start=start, control=nlsControl)
  # Build the additional object to add as an atrribute to the output
  alpha <- coef(modelCD)["alpha"]
  if (respectRangeConstraints){
    if (alpha < 0.0 || alpha > 1.0){
      # Need to adjust alpha, because we are beyond 0.0 or 1.0
      if (alpha < 0.0){
        alpha <- 0.0
      } else {
        alpha <- 1.0
      }
      # Refit for lambda only
      start <- list(lambda=lambdaGuess)
      modelCD <- nls(formula=model, data=data, start=start, control=nlsControl)
    }
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(modelCD)["lambda"]),
                     alpha = as.vector(alpha),
                     beta = as.vector(1.0 - alpha),
                     gamma = 0.0, # Energy is not a factor for this model.
                     sse = sum(resid(modelCD)^2),
                     isConv = modelCD$convInfo$isConv
                     )
  attr(x=modelCD, which="naturalCoeffs") <- naturalCoeffs
  return(modelCD)
}

cdeModelAB <- function(countryAbbrev, 
                       energyType="none", 
                       data=loadData(countryAbbrev), 
                       respectRangeConstraints=FALSE, ...){
  #################
  # This function does a Cobb-Douglas fit for the 
  # given data using the "ab" reparameterization, namely
  # * alpha + beta + gamma = 1.0.
  # * alpha, beta, and gamma are all between 0.0 and 1.0.
  # To do this, we reparameterize as
  # * 0 < a < 1
  # * 0 < b < 1
  # * alpha = min(a, b)
  # * beta = abs(b - a)
  # * gamma = 1 - max(a, b)
  ##
  # We need to do the Cobb-Douglas fit with the desired energy data.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function. 
  data <- replaceColName(data, energyType, "iEToFit")
  # Establish guess values for lambda, alpha, and beta.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  alphaGuess <- 0.899
  betaGuess <- 1.0 - alphaGuess
  gammaGuess <- 0.001
  aGuess <- alphaGuess
  bGuess <- alphaGuess + betaGuess
  formula <- iGDP ~ exp(lambda*iYear) * iCapStk^min(a,b) * iLabor^abs(b-a) * iEToFit^(1.0-max(a,b))
  start <- list(lambda=lambdaGuess, a=aGuess, b=bGuess)
  modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
  lambda <- coef(modelCDe)["lambda"]
  a <- coef(modelCDe)["a"]
  b <- coef(modelCDe)["b"]
  isConv <- modelCDe$convInfo$isConv
  if (respectRangeConstraints){
    # Need to do a bit more work to finish the fit.
    hitABoundary <- FALSE; hitBBoundary <- FALSE
    if (a<0 || a>1){
      hitABoundary <- TRUE
      a <- ifelse (a<0, 0, 1)
    }
    if (b<0 || b>1){
      hitBBoundary <- TRUE
      b <- ifelse (b<0, 0, 1)
    }
    if (hitABoundary && hitBBoundary){
      start <- list(lambda=lambda)
      # Now re-fit. a an b have been set. Get a new value for lambda.
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      lambda <- coef(modelCDe)["lambda"]
      isConv <- modelCDe$convInfo$isConv
    } else if (hitABoundary){
      start <- list(lambda=lambda, b=b)
      # Now re-fit
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      # a has been set. Grab a new value for b
      lambda <- coef(modelCDe)["lambda"]
      b <- coef(modelCDe)["b"]
      isConv <- modelCDe$convInfo$isConv
      # Test to see if b has been pushed out of range and re-fit if needed.
      if (b<0 || b>1){
        b <- ifelse (b<0, 0, 1)
        start <- list(lambda=lambda)
        modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
        lambda <- coef(modelCDe)["lambda"]
        isConv <- modelCDe$convInfo$isConv
      }
    } else if (hitBBoundary){
      start <- list(lambda=lambda, a=a)
      # Now re-fit
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      # b has been set. Grab a new value for a.
      lambda <- coef(modelCDe)["lambda"]
      a <- coef(modelCDe)["a"]
      isConv <- modelCDe$convInfo$isConv
      # Test to see if a has been pushed out of range and re-fit if needed.
      if (a<0 || a>1){
        a <- ifelse (a<0, 0, 1)
        start <- list(lambda=lambda)
        modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
        lambda <- coef(modelCDe)["lambda"]
        isConv <- modelCDe$convInfo$isConv
      }
    }
  }
  # Build the additional object to add as an atrribute to the output
  naturalCoeffs <- data.frame(a = as.vector(a), 
                     b = as.vector(b),
                     c = NA,
                     d = NA,
                     e = NA,
                     f = NA,
                     lambda = as.vector(lambda),
                     alpha = min(a,b),
                     beta = as.vector(abs(b-a)),
                     gamma = 1 - max(a,b),
                     sse = sum(resid(modelCDe)^2),
                     isConv = isConv
                     )
  attr(x=modelCDe, which="naturalCoeffs") <- naturalCoeffs
  return(modelCDe)
}

cdeModelCD <- function(countryAbbrev, 
                       energyType="none", 
                       data=loadData(countryAbbrev), 
                       respectRangeConstraints=FALSE, ...){
  #################
  # This function does a Cobb-Douglas fit for the 
  # given data using the "cd" reparameterization, namely
  # * alpha + beta + gamma = 1.0.
  # * alpha, beta, and gamma are all between 0.0 and 1.0.
  # * 0 < c < 1
  # * 0 < d < 1
  # * beta = min(c, d)
  # * gamma = abs(d-c)
  # * alpha = 1 - max(c, d)
  ##
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function. 
  data <- replaceColName(data, energyType, "iEToFit")
  
  # Establish guess values for lambda, alpha, and beta.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  alphaGuess <- 0.899
  betaGuess <- 1.0 - alphaGuess
  gammaGuess <- 0.001
  cGuess <- betaGuess
  dGuess <- betaGuess + gammaGuess
  formula <- iGDP ~ exp(lambda*iYear) * iCapStk^(1.0-max(c,d)) * iLabor^min(c,d) * iEToFit^abs(d-c)
  start <- list(lambda=lambdaGuess, c=cGuess, d=dGuess)
  modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
  lambda <- coef(modelCDe)["lambda"]
  c <- coef(modelCDe)["c"]
  d <- coef(modelCDe)["d"]
  isConv <- modelCDe$convInfo$isConv
  if (respectRangeConstraints){
    # Need to do a bit more work to finish the fit.
    hitCBoundary <- FALSE; hitDBoundary <- FALSE
    if (c<0 || c>1){
      hitCBoundary <- TRUE
      c <- ifelse (c<0, 0, 1)
    }
    if (d<0 || d>1){
      hitDBoundary <- TRUE
      d <- ifelse (d<0, 0, 1)
    }
    if (hitCBoundary && hitDBoundary){
      start <- list(lambda=lambda)
      # Now re-fit. c and d both hit the boundary and have been reset.
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      lambda <- coef(modelCDe)["lambda"]
      isConv <- modelCDe$convInfo$isConv
    } else if (hitCBoundary){
      start <- list(lambda=lambda, d=d)
      # Now re-fit with c at its boundary.
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      # c has been reset. Grab the new value for d
      lambda <- coef(modelCDe)["lambda"]
      d <- coef(modelCDe)["d"]
      isConv <- modelCDe$convInfo$isConv
      # Test to see if d has been pushed out of range and re-fit if needed.
      if (d<0 || d>1){
        d <- ifelse (d<0, 0, 1)
        start <- list(lambda=lambda)
        modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
        lambda <- coef(modelCDe)["lambda"]
        isConv <- modelCDe$convInfo$isConv
      }
    } else if (hitDBoundary){
      start <- list(lambda=lambda, c=c)
      # Now re-fit
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      # d has been reset. Grab the new value for c
      lambda <- coef(modelCDe)["lambda"]
      c <- coef(modelCDe)["c"]
      isConv <- modelCDe$convInfo$isConv
      # Test to see if c has been pushed out of range and re-fit if needed.
      if (c<0 || c>1){
        c <- ifelse(c<0, 0, 1)
        start <- list(lambda=lambda)
        modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
        lambda <- coef(modelCDe)["lambda"]
        isConv <- modelCDe$convInfo$isConv
      }
    }
  }
  # Build the additional object to add as an atrribute to the output
  naturalCoeffs <- data.frame(a = NA, 
                     b = NA,
                     c = as.vector(c),
                     d = as.vector(d),
                     e = NA,
                     f = NA,
                     lambda = as.vector(lambda),
                     alpha = 1 - max(c,d),
                     beta = min(c,d),
                     gamma = as.vector(abs(d-c)),
                     sse = sum(resid(modelCDe)^2),
                     isConv = isConv
                     )
  attr(x=modelCDe, which="naturalCoeffs") <- naturalCoeffs
  return(modelCDe)
}

cdeModelEF <- function(countryAbbrev, 
                       energyType="none", 
                       data=loadData(countryAbbrev), 
                       respectRangeConstraints=FALSE, ...){
  #################
  # This function does a Cobb-Douglas fit for the 
  # given data using the "ef" reparameterization, namely
  # * alpha + beta + gamma = 1.0.
  # * alpha, beta, and gamma are all between 0.0 and 1.0.
  # * 0 < e < 1
  # * 0 < f < 1
  # * gamma = min(e, f)
  # * alpha = abs(f-e)
  # * beta = 1 - max(e, f)
  ##
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function. 
  data <- replaceColName(data, energyType, "iEToFit")
  
  # Establish guess values for lambda, alpha, and beta.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  alphaGuess <- 0.899
  betaGuess <- 1.0 - alphaGuess
  gammaGuess <- 0.001
  eGuess <- gammaGuess
  fGuess <- gammaGuess + alphaGuess
  formula <- iGDP ~ exp(lambda*iYear) * iCapStk^abs(f - e) * iLabor^(1 - max(e, f)) * iEToFit^min(e, f)
  start <- list(lambda=lambdaGuess, e=eGuess, f=fGuess)
  modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
  lambda <- coef(modelCDe)["lambda"]
  e <- coef(modelCDe)["e"]
  f <- coef(modelCDe)["f"]
  isConv <- modelCDe$convInfo$isConv
  if (respectRangeConstraints){
    # Need to do a bit more work to finish the fit.
    hitEBoundary <- FALSE; hitFBoundary <- FALSE
    if (e<0 || e>1){
      hitEBoundary <- TRUE
      e <- ifelse (e<0, 0, 1)
    }
    if (f<0 || f>1){
      hitFBoundary <- TRUE
      f <- ifelse (f<0, 0, 1)
    }
    if (hitEBoundary && hitFBoundary){
      start <- list(lambda=lambda)
      # Now re-fit. c and d both hit the boundary and have been reset.
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      lambda <- coef(modelCDe)["lambda"]
      isConv <- modelCDe$convInfo$isConv
    } else if (hitEBoundary){
      start <- list(lambda=lambda, f=f)
      # Now re-fit with e at its boundary.
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      # e has been reset. Grab the new value for f
      lambda <- coef(modelCDe)["lambda"]
      f <- coef(modelCDe)["f"]
      isConv <- modelCDe$convInfo$isConv
      # Test to see if f has been pushed out of range and re-fit if needed.
      if (f<0 || f>1){
        f <- ifelse (f<0, 0, 1)
        start <- list(lambda=lambda)
        modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
        lambda <- coef(modelCDe)["lambda"]
        isConv <- modelCDe$convInfo$isConv
      }
    } else if (hitFBoundary){
      start <- list(lambda=lambda, e=e)
      # Now re-fit
      modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
      # f has been reset. Grab the new value for e
      lambda <- coef(modelCDe)["lambda"]
      e <- coef(modelCDe)["e"]
      isConv <- modelCDe$convInfo$isConv
      # Test to see if e has been pushed out of range and re-fit if needed.
      if (e<0 || e>1){
        e <- ifelse(e<0, 0, 1)
        start <- list(lambda=lambda)
        modelCDe <- nls(formula=formula, data=data, start=start, control=nlsControl)
        lambda <- coef(modelCDe)["lambda"]
        isConv <- modelCDe$convInfo$isConv
      }
    }
  }
  # Build the additional object to add as an atrribute to the output
  naturalCoeffs <- data.frame(a = NA, 
                     b = NA,
                     c = NA,
                     d = NA,
                     e = as.vector(e),
                     f = as.vector(f),
                     lambda = as.vector(lambda),
                     alpha = as.vector(abs(f-e)),
                     beta = 1 - max(e,f),
                     gamma = min(e,f),
                     sse = sum(resid(modelCDe)^2),
                     isConv = isConv
                     )
  attr(x=modelCDe, which="naturalCoeffs") <- naturalCoeffs
  return(modelCDe)
}

cdeModel <- function(countryAbbrev, 
                     energyType="none", 
                     data=loadData(countryAbbrev), 
                     respectRangeConstraints=FALSE, ...){
  ###################
  # This function fits the CDe model to historical data.
  # If the ab reparameterization doesn't converge, we try the 
  # cd reparameterization.
  # If neither converge, we stop execution.
  ##
  cdeModelAB <- cdeModelAB(countryAbbrev=countryAbbrev,
                           energyType=energyType,
                           data=data,
                           respectRangeConstraints=respectRangeConstraints, ...)
  if (cdeModelAB$convInfo$isConv){
    # We obtained a good model that converged.
    return(cdeModelAB)
  }
  # Well, the model didn't converge. Try again with a different reparameterization
  cdeModelCD <- cdeModelCD(countryAbbrev=countryAbbrev,
                           energyType=energyType,
                           data=data,
                           respectRangeConstraints=respectRangeConstraints, ...)
  if (cdeModelCD$convInfo$isConv){
    # OK, that one worked. Return it.
    return(cdeModelCD)
  }
  # Try one more time with the ef reparameterization.
  cdeModelEF <- cdeModelEF(countryAbbrev=countryAbbrev,
                           energyType=energyType,
                           data=data,
                           respectRangeConstraints=respectRangeConstraints, ...)
  if (cdeModelEF$convInfo$isConv){
    return (cdeModelEF)
  }    
  # If we get to this point, neither reparameterization converged. Print some
  # information about both.
  warning(paste("None of the ab, cd, or ef reparameterizations converged for CDe. countryAbbrev =", 
                ifelse(missing(countryAbbrev), "missing", countryAbbrev), 
                "energyType =", energyType,
                "This should happen rarely. Returning the reparameterization with smallest SSE."))
#   print("data")
#   print(data)
#   print("cdeModelAB")
#   print(summary(cdeModelAB))
#   print(attr(x=cdeModelAB, which="naturalCoeffs"))
#   print("cdeModelCD")
#   print(summary(cdeModelCD))
#   print(attr(x=cdeModelCD, which="naturalCoeffs"))
#   print("cdeModelEF")
#   print(summary(cdeModelEF))
#   print(attr(x=cdeModelEF, which="naturalCoeffs"))
  sseAB <- attr(x=cdeModelAB, which="naturalCoeffs")["sse"]
  sseCD <- attr(x=cdeModelCD, which="naturalCoeffs")["sse"]
  sseEF <- attr(x=cdeModelEF, which="naturalCoeffs")["sse"]
  # Return the reparameterization with least sse
  if (sseAB < sseCD){
    bestsse <- sseAB
    out <- cdeModelAB
  } else {
    bestsse <- sseCD
    out <- cdeModelCD
  }
  if (sseEF < bestsse){
    out <- cdeModelEF
  }
  return(out)
}

cdeFixedGammaModel <- function(countryAbbrev, energyType="none", gamma, data=loadData(countryAbbrev), ...){
  ##############################
  # Returns a Cobb-Douglas model where gamma (the exponent on the energy term) has been fixed
  ##
  # We need to do the Cobb-Douglas fit with the desired energy data.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function.
  data <- replaceColName(data, energyType, "iEToFit")
  # We're fixing the value of gamma.
  # Calculate a new column iEGamma = iEToFit^gamma
  iEGamma <- with(data, iEToFit^gamma)
  data <- cbind(data, iEGamma)
  # We already know the value of gamma. We don't want to estimate it.
  # Establish guess values for lambda and alpha.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  alphaGuess <- 0.7*(1-gamma) #ensure a consistent starting point.
  start <- list(lambda=lambdaGuess, alpha=alphaGuess)
  model <- iGDP ~ exp(lambda*iYear) * iCapStk^alpha * iLabor^((1-gamma) - alpha) * iEGamma
  modelCDe <- nls(formula=model, data = data, start = start,
                  control = nlsControl,
                  #Include the next 3 lines to fit with constraints.
                  algorithm = "port",
                  lower = list(lambda=-Inf, alpha=0),
                  upper = list(lambda=Inf, alpha=1.0-gamma)
  )
  return(modelCDe)
}

cobbDouglasModel <- function(countryAbbrev, energyType="none", gamma, data=loadData(countryAbbrev), ...){
  ####################
  # Returns an nls Cobb-Douglas model for the country specified
  # Give an energyType ("Q", "X", or "U") if you want to include an energy term. Supply energyType="none"
  # for a model without energy.
  # If you supply a value for the gamma argument, a fit with fixed gamma will be provided.
  # This function dispatches to cdModel, cdeModel, or cdFixedGammaModel based on which arguments are specified.
  ##
  if (energyType == "none"){
    # Fit the Cobb-Douglas model without energy.
    return(cdModel(data=data, ...))
  }
  if (!missing(gamma)){
    # Fit the Cobb-Douglas model with fixed value of gamma
    return(cdeFixedGammaModel(data=data, energyType=energyType, gamma=gamma, ...))
  }
  # Fit the Cobb-Douglas model with gamma as a free parameter
  return(cdeModel(data=data, energyType=energyType, ...))
}

cdResampleCoeffProps <- function(cdResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the cd and cde models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- cdResampleFits[cdResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- cdResampleFits[cdResampleFits[["method"]] != "orig", ]
  lambdaCI <- qdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  alphaCI <- qdata(p=ciVals, vals=alpha, data=resampleFitCoeffs)
  betaCI <- qdata(p=ciVals, vals=beta, data=resampleFitCoeffs)
  gammaCI <- qdata(p=ciVals, vals=gamma, data=resampleFitCoeffs)
  # Now make a data.frame that contains the information.
  lower <- data.frame(lambda=lambdaCI["2.5%"],
                      alpha=alphaCI["2.5%"],
                      beta=betaCI["2.5%"],
                      gamma=gammaCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(lambda=baseFitCoeffs["lambda"],
                    alpha=baseFitCoeffs["alpha"],
                    beta=baseFitCoeffs["beta"],
                    gamma=baseFitCoeffs["gamma"])
  row.names(mid) <- "CDe"
  upper <- data.frame(lambda=lambdaCI["97.5%"],
                      alpha=alphaCI["97.5%"],
                      beta=betaCI["97.5%"],
                      gamma=gammaCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

cdeGridData <- function(countryAbbrev, energyType="none", gammaGrid){
  ###############################
  # Does a grid search over values of gamma for a CD with energy production function.
  # Results are returned as a data.frame with columns of gamma, SSE, lambda, alpha, and beta
  ##
  gamma <- gammaGrid
  models <- lapply(gamma, cobbDouglasModel, energyType=energyType, countryAbbrev=countryAbbrev)
  SSE <- unlist(lapply(models, function(model){summary(model)$sigma}))
  lambda <- unlist(lapply(models, function(model){coef(model)["lambda"]}))
  alpha <- unlist(lapply(models, function(model){coef(model)["alpha"]}))
  data <- as.data.frame(cbind(gammaGrid, lambda, alpha, SSE))
  row.names(data) <- NULL #eliminates row names, leaving only numbered rows
  colnames(data) <- c("gamma", "lambda", "alpha", "SSE")
  data <- transform(data, beta = 1.0 - alpha - gamma)
  # Now make a column that has the country abbreviation
  nRowsData <- nrow(data) #Gets number of rows in data set
  dfOfCountryAbbrevs <- as.data.frame(matrix(countryAbbrev, ncol=1, nrow=nRowsData))
  colnames(dfOfCountryAbbrevs) <- "Country"
  data <- cbind(data, dfOfCountryAbbrevs) 
  return(data)
}

cobbDouglasPredictions <- function(countryAbbrev, energyType){
  #########################
  # Takes the Cobb-Douglas fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (energyType != "none"){
    # Consider this replacement only if energyType has been specified.
    if (!(haveDataCD(countryAbbrev, energyType))){
      # If we don't have data for this combination of countryAbbrev and energyType, 
      # return a column of NAs when the above conditions have been met.
      nRows <- 21 # All of these countries need 21 rows.
      df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
      colnames(df) <- "pred"
      return(df)
    }
  }
  model <- cobbDouglasModel(countryAbbrev, energyType)
  pred <- predict(model) #See http://stackoverflow.com/questions/9918807/how-get-plot-from-nls-in-r
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

cobbDouglasPredictionsColumn <- function(energyType="none"){
  #########################
  # Takes the Cobb-Douglas fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasPredictions, energyType=energyType))
  if (energyType == "none"){
    colnames(out) <- c("predGDP")
  } else {
    colnames(out) <- c(paste("predGDP", energyType, sep=""))
  }
  return(out)
}

cobbDouglasData <- function(countryAbbrev, energyType="none", ...){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the Cobb-Douglas production function given a country.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", "U", or NA. NA means you want a CD model without energy.
  #
  # returns a data.frame of data for the Cobb-Douglas model. 
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: lambda, alpha, beta, gamma, corresponding to the parameters in the model.
  ##
  # First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCD(countryAbbrev, energyType)){
    #Return a column of NAs if the above conditions have been met.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 4 # lambda, alpha, beta, and gamma
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("lambda", "alpha", "beta", "gamma")
    rownames(df) <- c("+95% CI", "CDe", "-95% CI")
    return(df)
  } else if (energyType == "none"){
    # We want Cobb-Douglas without energy
    resampledData <- loadResampleData(modelType="cd", countryAbbrev=countryAbbrev, energyType="none")
  } else {
    # We want Cobb-Douglas with energy
    resampledData <- loadResampleData(modelType="cde", countryAbbrev=countryAbbrev, energyType=energyType)
  }
  statisticalProperties <- cdResampleCoeffProps(resampledData)
  # Set the correct label in the row that shows the base values.
  if (energyType == "none"){
    rownames(statisticalProperties) <- c("+95% CI", "CD", "-95% CI")
  } else {
    rownames(statisticalProperties) <- c("+95% CI", "CDe", "-95% CI")
  }
  return(statisticalProperties)
}

loadCDSpaghettiGraphData <- function(energyType="none", archive=NULL){
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
  actual <- loadData(countryAbbrev="all")
  actual <- actual[c("Year", "iGDP", "Country")]
  actual$ResampleNumber <- NA
  actual$Type <- "actual"
  actual$Resampled <- FALSE
  actual$Energy <- energyType

  # Put the fits to historical data in a data.frame
  prediction <- cobbDouglasPredictionsColumn(energyType=energyType)
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
    countryAbbrevs <- countryAbbrevsForGraph
  }
  # Remove rows where we don't need historical data or predictions, 
  # specifically those times when we won't have a prediction.
  actual <- subset(actual, Country %in% countryAbbrevs)
  pred <- subset(pred, Country %in% countryAbbrevs)

  # Put all of the resamples in a list that will be converted to a data.frame
  dfList <- list()
  for (countryAbbrev in countryAbbrevs){
    # Get the raw data for this country
    historical <- loadData(countryAbbrev=countryAbbrev)
    if (energyType == "U"){
      # subset historical to include only years for which U is available.
      historical <- subset(historical, !is.na(iU))
    }
    years <- data.frame(Year = historical$Year)
    # Get the list of resample models for this country.
    resampleModels <- loadResampleModelsRefitsOnly(countryAbbrev=countryAbbrev, 
                                                   modelType=modelType, 
                                                   energyType=energyType, 
                                                   archive=archive)
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
      Energy = energyType
    )
  }
  
  # Now rbind everything together and return.  
  outgoing <- do.call("rbind", c(list(actual,pred), dfList) )
  # Ensure that the country factor is in the right order
  outgoing$Country <- factor(outgoing$Country, levels=countryAbbrevs)
  return(outgoing)
}

cobbDouglasCountryRow <- function(countryAbbrev, energyType="none"){
  ############
  # Creates a row for the Cobb Douglas parameters table for the given country (2-letter code) and energyType (Q, X, or U)
  ##
  dataCD <- cobbDouglasData(countryAbbrev, energyType)
  if (energyType == "none"){
    out <- cbind(dataCD["-95% CI", "lambda"], dataCD["CD", "lambda"], dataCD["+95% CI", "lambda"],
                 dataCD["-95% CI", "alpha"],  dataCD["CD", "alpha"],  dataCD["+95% CI", "alpha"],
                 dataCD["-95% CI", "beta"],   dataCD["CD", "beta"],   dataCD["+95% CI", "beta"])
  } else {
    out <- cbind(dataCD["-95% CI", "lambda"], dataCD["CDe", "lambda"], dataCD["+95% CI", "lambda"],
                 dataCD["-95% CI", "alpha"],  dataCD["CDe", "alpha"],  dataCD["+95% CI", "alpha"],
                 dataCD["-95% CI", "beta"],   dataCD["CDe", "beta"],   dataCD["+95% CI", "beta"],
                 dataCD["-95% CI", "gamma"],  dataCD["CDe", "gamma"],  dataCD["+95% CI", "gamma"])
  }
  return(out)
}

cobbDouglasCountryRowsForParamsGraph <- function(countryAbbrev, energyType="none"){
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
    dataCD <- cobbDouglasData(countryAbbrev=countryAbbrev, energyType="none")
  } else {
    valueRow <- "CDe"
    dataCD <- cobbDouglasData(countryAbbrev=countryAbbrev, energyType=energyType)
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

cobbDouglasParamsTableNoEnergyDF <- function(){
  ######################
  # Makes a data.frame with the parameters for the Cobb-Douglas model without energy.
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataCD <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRow, energyType="none"))
  rownames(dataCD) <- countryAbbrevs
  colnames(dataCD) <- c("lowerCI_lambda", "lambda", "upperCI_lambda", 
                        "lowerCI_alpha", "alpha", "upperCI_alpha",
                        "lowerCI_beta", "beta", "upperCI_beta")
  dataCD <- data.frame(dataCD)
  return(dataCD)
}

cobbDouglasParamsTableNoEnergy <- function(){
  ############################
  # Aggregates the Cobb-Douglas results into a big data table. No energy.
  ##
  dataCD <- cobbDouglasParamsTableNoEnergyDF()
  colnames(dataCD) <- c(" ", "$\\lambda$", " ", 
                        " ", "$\\alpha$",  " ",
                        " ", "$\\beta$",   " ")
  rownames(dataCD) <- countryAbbrevs
  tableCD <- xtable(dataCD, 
                    caption="Cobb-Douglas model (without energy) parameters for 1980-2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)",
                    label="tab:CD_Parameters_No_Energy",
                    digits = c(1, 4,4,4, 2,2,2, 2,2,2),
                    align = "r|ccc|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableCD)
}

cobbDouglasParamsTableWithEnergyDF <- function(energyType){
  ######################
  # Makes a data.frame with the parameters for the Cobb-Douglas model with energy.
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataCD <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRow, energyType=energyType))
  rownames(dataCD) <- countryAbbrevs
  colnames(dataCD) <- c("lowerCI_lambda", "lambda", "upperCI_lambda", 
                        "lowerCI_alpha", "alpha", "upperCI_alpha",
                        "lowerCI_beta", "beta", "upperCI_beta",
                        "lowerCI_gamma", "gamma", "upperCI_gamma")
  dataCD <- data.frame(dataCD)
  return(dataCD)
}

cobbDouglasParamsTableWithEnergy <- function(energyType){
  ############################
  # Aggregates the Cobb-Douglas results into a big data table for the given energyType.
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataCD <- cobbDouglasParamsTableWithEnergyDF(energyType)
  colnames(dataCD) <- c(" ", "$\\lambda$", " ", 
                        " ", "$\\alpha$",  " ",
                        " ", "$\\beta$",   " ",
                        " ", "$\\gamma$",  " ")
  rownames(dataCD) <- countryAbbrevs
  tableCD <- xtable(dataCD, 
                    caption=paste("Cobb-Douglas model (with $", 
                                  tolower(energyType), 
                                  "$) parameters for 1980-2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
                                  sep=""), 
                    label=paste("tab:CD_Parameters_With_", energyType, sep=""),
                    digits = c(1, 4,4,4, 2,2,2, 2,2,2, 2,2,2),
                    align = "r|ccc|ccc|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableCD)
}

printCDParamsTable <- function(energyType="none"){
  ############################
  # Prints a table with parameters from a Cobb-Douglas model for the given energyType. 
  # Set energyType="none" to print a table for Cobb-Douglas without energy.
  ##
  if (energyType == "none"){
    print(cobbDouglasParamsTableNoEnergy(), 
          caption.placement="top", 
          sanitize.colnames.function = identity, 
          size="\\tiny", 
          table.placement="H")
  } else {
    print(cobbDouglasParamsTableWithEnergy(energyType), 
          caption.placement="top", 
          sanitize.colnames.function = identity, 
          size="\\tiny",
          table.placement="H")
  }
}

createCDParamsGraph <- function(energyType="none"){
  #############################
  # Creates a graph with confidence intervals for the Cobb-Douglas model for the given energyType. If you 
  # want the Cobb-Douglas model without energy, supply energyType="none".
  ##
  if (energyType == "none"){
    # Create a data table with the following columns:
    # country abbrev, parameter (lambda, alpha, or beta), -95% CI, value, +95% CI
    data <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRowsForParamsGraph, energyType="none"))
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
    data <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRowsForParamsGraph, energyType=energyType))
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

cesModel2 <- function(countryAbbrev, 
                      energyType="none", 
                      data = loadData(countryAbbrev=countryAbbrev), 
                      prevModel=NULL,
                      algorithms=c("PORT","L-BFGS-B"), 
                      nest="(kl)e", 
                      rho =c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      rho1=c(9, 2, 1, 0.43, 0.25, 0.1, -0.1, -0.5, -0.75, -0.9, -0.99),
                      digits=6,
                      ...){
  
  ###################
  # This function fits a CES model to original or resampled data.
  # Pass in data if you want to use resampled data.
  # Pass in a countryAbbrev if you want to use original data.
  # Pass in nest="(kl)" if you want a fit without energy, regardless of which energyType is specified.
  # If energyType="none", a CES fit without energy will be attempted, but only if nest != "(kl)".
  # If you set fittingToResampleData=FALSE, you should also supply a value for the origModel argument,
  # because origModel will be used to obtain the starting point for a gradient search.
  # Pass in a prevModel if you want to start from its location using gradient searches only.
  # Pass in NULL for prevModel if you want to use the default start locations AND do a grid search in sigma
  # Default values for rho and rho1 are a grid upon which searches will be made.
  # Note that rho = 0.25 and rho1 = 0.25 are included. These are the default starting
  # values for rho and rho1, so that we don't need to do a fit from the default values.
  # rho = 0.25 corresponds to sigma = 0.8.
  #
  # Returns a list of models that were generated within this function.
  ##
  if (energyType != "none" && (nest != "(kl)")){
    # We need to do the CES fit with the desired energyType.
    # But, only if we asked for a nest that isn't "(kl)"
    # To achieve the correct fit, we'll change the name of the desired column
    # to "iEToFit" and use "iEToFit" in the nls function.
    data <- replaceColName(data, energyType, "iEToFit")
    # Remove rows with missing energy information
    data <- data[ !is.na(data[,"iEToFit"]), ]
  }
  
  # Verify algorithm
  cesAlgorithms <- c("PORT", "L-BFGS-B") # These are the only valid algs that respect constraints
  algorithms <- toupper(algorithms)
  badAlgorithms <- setdiff(algorithms, cesAlgorithms)
  algorithms <- intersect(algorithms, cesAlgorithms)
  for (m in badAlgorithms) {
    stop(paste("Unrecognized algorithm:", m))
  }
  # Set up xNames for the desired energy type or nesting
  if (energyType == "none" || nest == "(kl)"){
    # We don't want to include energy. So, include only k and l.
    xNames <- c("iCapStk", "iLabor")
  } else {
    # We want to include energy. So, include k, l, and e.
    if (nest %in% c("(kl)e", "(lk)e")){
      xNames <- c("iCapStk", "iLabor", "iEToFit")
    } else if (nest %in% c("(le)k", "(el)k")){
      xNames <- c("iLabor", "iEToFit", "iCapStk")
    } else if (nest %in% c("(ek)l", "(ke)l")){
      xNames <- c("iEToFit", "iCapStk", "iLabor")
    } else {
      stop(paste("Unknown nesting option", nest, "in cesModel2"))
    }
  }
  # Establish key variable names  
  tName <- "iYear"
  yName <- "iGDP"
  models <- list()
  for (algorithm in algorithms) {
    #
    # Try grid search.
    #
    if (energyType == "none" || nest=="(kl)"){
      # We want a model without energy. No need for a rho1 argument.
      model <- tryCatch(
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               rho=rho, control=chooseCESControl(algorithm), ...),
        error = function(e) { NULL }
      )
    } else {
      # We want a model with energy. Need a rho1 argument, because we are using a nesting.
      model <- tryCatch(
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               rho=rho, rho1=rho1, control=chooseCESControl(algorithm), ...),
        error = function(e) { NULL }
      )
    }
    
    hist <- paste(algorithm, "(grid)", sep="", collapse="|")  
    model <- addMetaData(model, nest=nest, history=hist)
    models[[length(models)+1]] <- model
    
  }
  #
  # Now try gradient search starting from the best place found by the grid searches above.
  #
  bestMod <- bestModel(models, digits=digits)
  start <- coef(bestMod)
  for (algorithm in algorithms) {
    model <- tryCatch(
      cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
             control=chooseCESControl(algorithm), start=start, ...),
      error = function(e) { NULL }
    )
    hist <- paste(algorithm, "[", getHistory(bestMod), "]", collapse="|", sep="")
    model <- addMetaData(model, nest=nest, history=hist)
    models[[length(models)+1]] <- model
  }
  #
  # Now try gradient search starting from prevModel (if it is present in the argument list).
  #
  if (! is.null(prevModel)){
    start <- coef(prevModel)
    for (algorithm in algorithms) {
      model <- tryCatch(
        cesEst(data=data, yName=yName, xNames=xNames, tName=tName, method=algorithm, 
               control=chooseCESControl(algorithm), start=start, ...),
        error = function(e) { NULL }
      )
      hist <- paste(algorithm, "[", getHistory(prevModel), ".prev]", sep="", collapse="|")
      model <- addMetaData(model, nest=nest, history=hist)
      models[[length(models)+1]] <- model
    }
  }
  # Return everything all of the models that we calculated.
  return(models)
}

addMetaData <- function(model, nest, history=""){
  ###############
  # This function adds metadata to a model.  Currently this is only designed to
  # work with CES models. Metadata is attached as attributes (naturalCoeffs and meta)
  # to the object and the new object is returned from the function.
  ##
  if (is.null(model)){
    return(model) 
  }
  if ( ! as.character(model$call[[1]]) == "cesEst" ){
    stop("Unsupported model type.  Must be NULL or the result of calling cesEst()")
  }
  
  grid <- length( intersect(c("rho", "rho1"), names(model$call) ) ) > 0
  
  # We may be arriving here with a model that was estimated wihthout energy.
  # If that is the case, we will have only rho and sigma parameters, not
  # rho_1 and delta_1 parameters. 
  # Test for the without energy model.
  withoutEnergy <- is.na(coef(model)["rho_1"]) || is.na(coef(model)["delta_1"])

  if (withoutEnergy){
    # The coefficient representing the split between k and l 
    # is given by delta in the model. But, in our calculations, 
    # we're defining the split between k and l and delta_1.
    # So, reassign here.
    delta_1 <- coef(model)["delta"]
    # The without-energy model has delta <- 1
    delta <- 1
    # The coefficient representing the substitutability between k and l
    # is given by rho in the model argument. But, in our calculations,
    # we're defining that substitutability as rho_1.
    # So, reassign here.
    rho_1 <- coef(model)["rho"]
    # The no-energy situation is tantamount to saying that there is
    # infinite substitutability between (kl) and e. 
    # So, assign the value of rho to be -1 (sigma = Inf).
    rho <- -1
  } else {
    # This is the no-energy situation. Things are more straightforward.
    delta_1 <- coef(model)["delta_1"]
    delta <- coef(model)["delta"]
    rho_1 <- coef(model)["rho_1"]
    rho <- coef(model)["rho"]
  }
  naturalCoeffs <- data.frame(lambda = as.vector(coef(model)["lambda"]),
                              delta_1 = as.vector(delta_1),
                              rho_1 = as.vector(rho_1),
                              sigma_1 = as.vector(1 / (1 + rho_1)),
                              # Variable name collision alert: there is a gamma coefficient
                              # in the CES model (gamma_coef) and a gamma calculated 
                              # from the delta values.
                              # gamma_coef is the coefficient in the CES model. It should be near 1.0.
                              # gamma is calculated from the delta values in the model.
                              # gamma is analogous to the gamma exponent on energy in the Cobb-Douglas model.
                              # And, gamma is the required name of the variable to be plotted with the ternary 
                              # plot function standardTriPlot.  (standardTriPlot assumes that one variable 
                              # is named "gamma", and it plots that variable.)  
                              # gamma_coef is in the naturalCoeffs attribute.
                              # gamma is in the meta attribute.
                              gamma_coef = as.vector(coef(model)["gamma"]),
                              delta = as.vector(delta),
                              rho = as.vector(rho),
                              sigma = as.vector(1 / (1 + rho)),
                              sse = sum(resid(model)^2)
  )
  # Calculate some metadata, including gamma. See comments above.
  if (missing(nest) || is.na(nest) || nest == "(kl)" || nest == "kl"){
    alpha <- delta_1
    beta <- 1.0 - delta_1
    gamma <- 0.0
  } else if (nest == "(kl)e"){
    alpha <- delta * delta_1
    beta  <- delta * (1.0 - delta_1)
    gamma <- 1.0 - delta
  } else if (nest == "(le)k"){
    alpha <- 1.0 - delta
    beta <- delta * delta_1
    gamma <- delta * (1.0 - delta_1)
  } else if (nest == "(ek)l"){
    alpha <- delta * (1.0 - delta_1)
    beta <- 1.0 - delta
    gamma <- delta * delta_1
  } else {
    stop(paste("Unknown nest:", nest, "in addMetaData."))
  }
  metaData <- data.frame( isConv = model$convergence,
                          algorithm = model$method,
#                          iter = as.vector(model["iter"]),
                          grid = grid,
                          alpha = as.vector(alpha),
                          beta = as.vector(beta),
                          gamma = as.vector(gamma),
                          start.lambda = as.vector(model$start["lambda"]),
                          start.delta_1 = as.vector(model$start["delta_1"]),
                          start.rho_1 = as.vector(model$start["rho_1"]),
                          start.gamma_coef = as.vector(model$start["gamma"]),
                          start.delta = as.vector(model$start["delta"]),
                          start.rho = as.vector(model$start["rho"]),
                          history=history
  )
  
  metaList <- list(  isConv = model$convergence,
                     algorithm = model$method,
                     iter = as.vector(model$iter),
                     grid = grid,
                     alpha = as.vector(alpha),
                     beta = as.vector(beta),
                     gamma = as.vector(gamma),
                     start.lambda = as.vector(model$start["lambda"]),
                     start.delta_1 = as.vector(model$start["delta_1"]),
                     start.rho_1 = as.vector(model$start["rho_1"]),
                     start.gamma_coef = as.vector(model$start["gamma"]),
                     start.delta = as.vector(model$start["delta"]),
                     start.rho = as.vector(model$start["rho"]),
                     history=history
  )
  
  if ( nrow(metaData) > 1 ) {
    warning( paste0("\nmeta data has ", nrow(metaData), " rows: ", paste(nest,history, sep="|")) )
    for (item in metaList) { 
      if ( length(item) > 1 ) {
        warning(paste0("\t", toString(item)))
      }
    }
  }
  attr(x=model, "naturalCoeffs") <- naturalCoeffs[1,]
  metaData$metaDataRows <- nrow(metaData)
  attr(x=model, "meta") <- metaData[1,] 
  attr(x=model, "metaList") <- metaList 
  return(model)
}

getHistory <- function(model) {
  #####################
  # Extracts history from a model
  ##
  out <- metaData(model)$history
  return(out)
}

bestModel <- function(models, digits=6, orderOnly=FALSE) {
  ###################
  # Extracts the best model (least sse) from a list of models
  ##
  # Note that the order function below preserves the original order in the event of ties.
  o <- order(sapply( models, function(model) { round(sum(resid(model)^2), digits=digits) } ) )
  if (orderOnly) return(o)
  out  <- models[[ o[1] ]] 
  return(out) 
}

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

loadCESResampleData <- function(nest, energyType="none", archive=NULL){
  #################################
  # Loads and binds data for a CES resample ternary plot.
  # If the energyType argument is missing or NA, you'll get data for the CES model without energy.
  # If you specify nest="all", you'll get data for all nests. You'll need to specify energyType if you use nest="all"
  ##
  if (energyType == "none" || nest=="(kl)"){
    # Desire CES without energy.
    data <- loadAllResampleData(modelType="ces", countryAbbrevsOrder=countryAbbrevsForGraph, 
                                energyType="none", archive=archive)
    data$nest <- "(kl)"
    return(data)
  }
  # We have an energyType
  if (nest == "all"){
    # Data for all nest options is desired.
    # Recursively call this function and rbind.fill the results together.
    allNests <- lapply( cesNests, loadCESResampleData, energyType=energyType, archive=archive )
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
                                archive=archive)
  } else {
    data <- loadAllResampleData(modelType=modelType, energyType=energyType,
                                countryAbbrevsOrder=countryAbbrevsForGraph, 
                                archive=archive)
  }
  # Add the nest argument to the data.
  data$nest <- nest
  return(data)
}

cesResampleCoeffProps <- function(cesResampleFits, ...){
  #######
  # This function creates a table of confidence intervals for the ces and cese models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- cesResampleFits[cesResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- cesResampleFits[cesResampleFits[["method"]] != "orig", ]
  gammaCI <- qdata(p=ciVals, vals=gamma, data=resampleFitCoeffs)
  lambdaCI <- qdata(p=ciVals, vals=lambda, data=resampleFitCoeffs)
  delta_1CI <- qdata(p=ciVals, vals=delta_1, data=resampleFitCoeffs)  
  rho_1CI <- qdata(p=ciVals, vals=rho_1, data=resampleFitCoeffs)  
  sigma_1CI <- qdata(p=ciVals, vals=sigma_1, data=resampleFitCoeffs)  
  deltaCI <- qdata(p=ciVals, vals=delta, data=resampleFitCoeffs)  
  rhoCI <- qdata(p=ciVals, vals=rho, data=resampleFitCoeffs)  
  sigmaCI <- qdata(p=ciVals, vals=sigma, data=resampleFitCoeffs)  
  # Now make a data.frame that contains the information.
  lower <- data.frame(gamma=gammaCI["2.5%"],
                      lambda=lambdaCI["2.5%"],
                      delta_1=delta_1CI["2.5%"],
                      rho_1=rho_1CI["2.5%"],
                      sigma_1=sigma_1CI["2.5%"],
                      delta=deltaCI["2.5%"],
                      rho=rhoCI["2.5%"],
                      sigma=sigmaCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(gamma=baseFitCoeffs["gamma"],
                    lambda=baseFitCoeffs["lambda"],
                    delta_1=baseFitCoeffs["delta_1"],
                    rho_1=baseFitCoeffs["rho_1"],
                    sigma_1=baseFitCoeffs["sigma_1"],
                    delta=baseFitCoeffs["delta"],
                    rho=baseFitCoeffs["rho"],
                    sigma=baseFitCoeffs["sigma"])
  row.names(mid) <- "CESe"
  upper <- data.frame(gamma=gammaCI["97.5%"],
                      lambda=lambdaCI["97.5%"],
                      delta_1=delta_1CI["97.5%"],
                      rho_1=rho_1CI["97.5%"],
                      sigma_1=sigma_1CI["97.5%"],
                      delta=deltaCI["97.5%"],
                      rho=rhoCI["97.5%"],
                      sigma=sigmaCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

cesPredictions <- function(countryAbbrev, energyType="none", nest, archive=NULL, forceRun=FALSE){
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
    model <- loadResampleModelsBaseModelOnly(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, archive=archive)
  }
  pred <- fitted(model)
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

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

loadCESSpaghettiGraphData <- function(nest="(kl)", energyType="none", archive=NULL){
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
    allNests <- lapply( cesNests, loadCESSpaghettiGraphData, energyType=energyType, archive=archive )
    outgoing <- do.call(rbind.fill, allNests)
    # Now set the order for the factors of the nests
    outgoing$nest <- factor(outgoing$nest, levels=cesNests)
    return(outgoing)
  }
  # We don't want all of the nests. Do the nest that is desired.
  # Put the historical data in a data.frame. 
  # We apply the nest argument as given to the data frame. 
  # Doing so assists with graphing later.
  actual <- loadData(countryAbbrev="all")
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
  prediction <- cesPredictionsColumn(energyType=energyType, nest=nest)
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
    countryAbbrevs <- countryAbbrevsForGraph
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
    historical <- loadData(countryAbbrev=countryAbbrev)
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
                                                   archive=archive)
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

cesData <- function(countryAbbrev, energyType="none", nest="(kl)e"){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the CES production function given a country and an energyType.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", "U", or "none".
  # energyType="none" means we're interested in a CES fit without energy.
  #
  # returns a data.frame of data for the CES model.
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: gamma, lambda, delta_1, delta, rho_1, rho
  ##
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCES(countryAbbrev, energyType)){
    #Return a column of NAs if we don't have data available for this combination of country and energy type.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 8 # gamma, lambda, delta_1, delta, sigma_1, and sigma
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("gamma", "lambda", "delta_1", "rho_1", "sigma_1", "delta", "rho", "sigma")
    rownames(df) <- c("+95% CI", "CES", "-95% CI")
    return(df)
  } else if (energyType == "none"){
    # We want CES without energy
    resampleData <- loadResampleData(modelType="ces", countryAbbrev=countryAbbrev, energyType="none")
  } else {
    # We want CES with energy  -- might want all three for this later.
    modelType <- paste("cese-", nest, sep="")
    resampleData <- loadResampleData(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType)
  }
  statisticalProperties <- cesResampleCoeffProps(resampleData)
  # Set the correct label in the row that shows the base values.
  if (energyType == "none"){
    rownames(statisticalProperties) <- c("+95% CI", "CES", "-95% CI")
  } else {
    rownames(statisticalProperties) <- c("+95% CI", "CESe", "-95% CI")
  }
  return(statisticalProperties)
}

cesCountryRow <- function(countryAbbrev, energyType="none", nest="(kl)e"){
  ############
  # Creates a row for the CES parameters table for the given country (2-letter code),
  # energyType (Q, X, U, or NA), and nest.
  ##
  dataCES <- cesData(countryAbbrev=countryAbbrev, energyType=energyType, nest=nest)
  out <- cbind(dataCES["-95% CI", "gamma"], dataCES["CES", "gamma"], dataCES["+95% CI", "gamma"],
               dataCES["-95% CI", "lambda"], dataCES["CES", "lambda"], dataCES["+95% CI", "lambda"],
               dataCES["-95% CI", "delta_1"], dataCES["CES", "delta_1"], dataCES["+95% CI", "delta_1"],
               dataCES["-95% CI", "delta"], dataCES["CES", "delta"], dataCES["+95% CI", "delta"],
               dataCES["-95% CI", "sigma_1"], dataCES["CES", "sigma_1"], dataCES["+95% CI", "sigma_1"],
               dataCES["-95% CI", "sigma"], dataCES["CES", "sigma"], dataCES["+95% CI", "sigma"]
  )
  return(out)
}

cesCountryRowsForParamsGraph <- function(countryAbbrev, energyType="none", nest="(kl)e"){
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
  dataCES <- cesData(countryAbbrev=countryAbbrev, energyType=energyType, nest=nest)
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

cesParamsTableDF <- function(energyType){
  ######################
  # Creates a data.frame for CES parameters
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType))
  colnames(dataCES) <- c("lowerCI_gamma", "gamma", "upperCI_gamma",
                         "lowerCI_lambda", "lambda", "upperCI_lambda",
                         "lowerCI_delta_1", "delta_1", "upperCI_delta_1",
                         "lowerCI_delta", "delta", "upperCI_delta",
                         "lowerCI_sigma_1", "sigma_1", "upperCI_sigma_1",
                         "lowerCI_sigma", "sigma", "upperCI_sigma")
  rownames(dataCES) <- countryAbbrevs
  return(dataCES)
}

cesParamsTableA <- function(energyType="none", nest="(kl)e"){
  ############################
  # Aggregates the CES results for lambda, delta, and sigma into a table for the given energyType.
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType, nest=nest))
  colnames(dataCES) <- c(" ", "$\\gamma$",    " ", 
                         " ", "$\\lambda$",   " ",
                         " ", "$\\delta_1$",  " ",
                         " ", "$\\delta$",    " ",
                         " ", "$\\sigma_1$",  " ",
                         " ", "$\\sigma$",    " ")
  rownames(dataCES) <- countryAbbrevs
  if (energyType == "none"){
    energyStringCaption <- "(without energy, ($kl$) nesting)"
    energyStringLabel <- ""
  } else {
    if (nest == "(kl)e"){
      energyStringCaption <- "(with energy, ($kl$)$e$ nesting)"
    } else if (nest == "(le)k"){
      energyStringCaption <- "(with energy, ($le$)$k$ nesting)"
    } else if (nest == "(ek)l"){
      energyStringCaption <- "(with energy, ($ek$)$l$ nesting)"
    } else {
      stop(paste("Unknown nesting", nest, "in cesParamsTableA."))
    }
    energyStringLabel <- paste("_With_", energyType, "_", nest, sep="")
  }
  tableCESa <- xtable(dataCES[,c(4,5,6, 10,11,12, 16,17,18)], #Picks up lambda, delta, sigma
                      caption=paste("CES model parameters ", 
                                    energyStringCaption, 
                                    ". $\\lambda$, $\\delta$, and $\\sigma$ parameters for 1980-2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
                                    sep=""), 
                      label=paste("tab:CES_Parameters_A", energyStringLabel, sep=""),
                      digits = c(1, 4,4,4, 2,2,2, 2,2,2),
                      align = "r|ccc|ccc|ccc" #Sets alignment of the numbers in the columns
  ) 
  return(tableCESa)
}

cesParamsTableB <- function(energyType="none", nest="(kl)e"){
  ############################
  # Aggregates the CES results for gamma, delta_1, and sigma_1 into a table for the given energyType.
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType, nest=nest))
  colnames(dataCES) <- c(" ", "$\\gamma$",    " ", 
                         " ", "$\\lambda$",   " ",
                         " ", "$\\delta_1$",  " ",
                         " ", "$\\delta$",    " ",
                         " ", "$\\sigma_1$",  " ",
                         " ", "$\\sigma$",    " ")
  rownames(dataCES) <- countryAbbrevs
  if (energyType == "none"){
    energyStringCaption <- "(without energy, ($kl$) nesting)"
    energyStringLabel <- ""
  } else {
    if (nest == "(kl)e"){
      energyStringCaption <- "(with energy, ($kl$)$e$ nesting)"
    } else if (nest == "(le)k"){
      energyStringCaption <- "(with energy, ($le$)$k$ nesting)"
    } else if (nest == "(ek)l"){
      energyStringCaption <- "(with energy, ($ek$)$l$ nesting)"
    } else {
      stop(paste("Unknown nesting", nest, "in cesParamsTableA."))
    }
    energyStringLabel <- paste("_With_", energyType, "_", nest, sep="")
  }
  tableCESb <- xtable(dataCES[,c(1,2,3, 7,8,9, 13,14,15)], #Picks up gamma, delta_1, and sigma_1
                      caption=paste("CES model parameters ", energyStringCaption, ". $\\gamma$, $\\delta_1$, and $\\sigma_1$ parameters for 1980--2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                      label=paste("tab:CES_Parameters_B", energyStringLabel, sep=""),
                      digits = c(1, 2,2,2, 2,2,2, 2,2,2),
                      align = "r|ccc|ccc|ccc"  #Sets alignment of the numbers in the columns
  )
  return(tableCESb)
}

createCESParamsGraph <- function(energyType="none", nest="(kl)e"){
  #############################
  # Creates a graph with confidence intervals for the CES model for the given energyType and nesting.
  ##
  # Create a data table with the following columns:
  # country abbrev, parameter (gamma, lambda, delta_1, delta, sigma_1, sigma), -95% CI, value, +95% CI
  data <- do.call("rbind", lapply(countryAbbrevs, cesCountryRowsForParamsGraph, energyType=energyType, nest=nest))
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

printCESParamsTableA <- function(energyType="none", nest="(kl)e"){
  ############################
  # Prints a table with lambda, delta, and sigma parameters from a CES model for the given energyType. 
  ##
  print(cesParamsTableA(energyType=energyType, nest=nest), 
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny",
        table.placement="H")
}

printCESParamsTableB <- function(energyType="none", nest="(kl)e"){
  ############################
  # Prints a table with gamma, delta_1, and sigma_1 parameters from a CES model for the given energyType. 
  ##
  print(cesParamsTableB(energyType=energyType, nest=nest), 
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny",
        table.placement="H")
}

## <<LINEX functions, eval=TRUE>>=
linexModel <- function(countryAbbrev, energyType="none", data){
  ####################
  # Returns an nls linex model for the country and energyType specified.
  # energyType should be one of Q", "X", or "U".
  # 
  # If you want to supply your own data, you need to specify ALL arguments.
  # Also, be VERY SURE that countryAbbrev is appropriate for the data you are supplying,
  # because decisions about guess values for parameters and optimization methods
  # are made based upon countryAbbrev, and there is no way to verify that 
  # countryAbbrev is associated with data.    
  ##
  # We need to do the Linex fit with the desired energyType.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function.
  if (missing(data)){
    data <- loadData(countryAbbrev=countryAbbrev)    
  }
  data <- replaceColName(data=data, factor=energyType, newName="iEToFit")
  
  if (countryAbbrev == "SA"){
    # Need adjusted guess values, becasue k and l are above GDP for SA.
    start <- list(a_0=-1.0, a_1=6.0)
  } else if (countryAbbrev == "ZM"){
    # Set up the initial guess for ZM to be the coefficients for the 
    # base model
    # start <- list(a_0=0.35, c_t=2.85)
    start <- list(a_0=0.35, a_1=1.0)
  } else {
    start <- list(a_0=0.5, a_1=0.5)
  }
  # Runs a non-linear least squares fit to the data with constraints
  modelLINEX <- nls(iGDP ~ iEToFit * exp(a_0*(2.0 - (iLabor+iEToFit)/iCapStk) + a_1*(iLabor/iEToFit - 1.0)), 
                    data=data,
                    start=start,
                    control=nlsControl
  )
  # Build the additional object to add as an atrribute to the output
  a_0 <- coef(modelLINEX)["a_0"]
  a_1 <- coef(modelLINEX)["a_1"]
  c_t <- a_1 / a_0
  naturalCoeffs <- data.frame(a_0 = as.vector(a_0),
                     a_1 = as.vector(a_1),
                     c_t = as.vector(a_1 / a_0),
                     sse = sum(resid(modelLINEX)^2),
                     isConv = modelLINEX$convInfo$isConv
  )
  attr(x=modelLINEX, which="naturalCoeffs") <- naturalCoeffs
  return(modelLINEX)
}

linexPredictions <- function(countryAbbrev, energyType){
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
  model <- linexModel(countryAbbrev, energyType)
  pred <- predict(model) #See http://stackoverflow.com/questions/9918807/how-get-plot-from-nls-in-r
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

linexPredictionsColumn <- function(energyType){
  #########################
  # Takes the LINEX fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, linexPredictions, energyType=energyType))
  colnames(out) <- c(paste("predGDP", energyType, sep=""))
  return(out)
}

loadLinexSpaghettiGraphData <- function(energyType="Q", archive=NULL){
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
  actual <- loadData(countryAbbrev="all")
  actual <- actual[c("Year", "iGDP", "Country")]
  actual$ResampleNumber <- NA
  actual$Type <- "actual"
  actual$Resampled <- FALSE
  actual$Energy <- energyType
  
  # Put the fits to historical data in a data.frame
  prediction <- linexPredictionsColumn(energyType=energyType)
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
    countryAbbrevs <- countryAbbrevsForGraph
  }
  # Remove rows where we don't need historical data or predictions, 
  # specifically those times when we won't have a prediction.
  actual <- subset(actual, Country %in% countryAbbrevs)
  pred <- subset(pred, Country %in% countryAbbrevs)
  
  # Put all of the resamples in a list that will be converted to a data.frame
  dfList <- list()
  for (countryAbbrev in countryAbbrevs){
    # Get the raw data for this country
    historical <- loadData(countryAbbrev=countryAbbrev)
    if (energyType == "U"){
      # subset historical to include only years for which U is available.
      historical <- subset(historical, !is.na(iU))
    }
    years <- data.frame(Year = historical$Year)
    # Get the list of resample models for this country.
    resampleModels <- loadResampleModelsRefitsOnly(countryAbbrev=countryAbbrev, 
                                                   modelType=modelType, 
                                                   energyType=energyType, 
                                                   archive=archive)
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
      Energy = energyType
    )
  }
  
  # Now rbind everything together and return.  
  outgoing <- do.call("rbind", c(list(actual,pred), dfList) )
  # Ensure that the country factor is in the right order
  outgoing$Country <- factor(outgoing$Country, levels=countryAbbrevs)
  return(outgoing)
}

linexResampleCoeffProps <- function(linexResampleFits, ...){
  ####### 
  # This function creates a table of confidence intervals for the LINEX models
  # from the data supplied
  ##
  # Grab the original curve fit
  baseFitCoeffs <- linexResampleFits[linexResampleFits[["method"]]=="orig", ]
  # Grab the resample curve fits
  resampleFitCoeffs <- linexResampleFits[linexResampleFits[["method"]] != "orig", ]
  a_0CI <- qdata(p=ciVals, vals=a_0, data=resampleFitCoeffs)
  c_tCI <- qdata(p=ciVals, vals=c_t, data=resampleFitCoeffs)
  # Now make a data.frame that contains the information.
  lower <- data.frame(a_0=a_0CI["2.5%"],
                      c_t=c_tCI["2.5%"])
  row.names(lower) <- "-95% CI"
  mid <- data.frame(a_0=baseFitCoeffs["a_0"],
                    c_t=baseFitCoeffs["c_t"])
  row.names(mid) <- "LINEX"
  upper <- data.frame(a_0=a_0CI["97.5%"],
                      c_t=c_tCI["97.5%"])
  row.names(upper) <- "+95% CI"
  dataCD <- rbind(upper, mid, lower)
  return(dataCD)
}

linexData <- function(countryAbbrev, energyType){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the LINEX production function given a country and an energyType.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", or "U"
  #
  # returns a data.frame of data for the LINEX model.
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: a_0 and c_t, corresponding to the parameters in the model.
  ##
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCD(countryAbbrev, energyType)){
    #Return a column of NAs if the above conditions have been met.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 2 # a_0 and c_t
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("a_0", "c_t")
    rownames(df) <- c("+95% CI", "LINEX", "-95% CI")
    return(df)
  }
  resampledData <- loadResampleData(modelType="linex", countryAbbrev=countryAbbrev, energyType=energyType)
  statisticalProperties <- linexResampleCoeffProps(resampledData)
  return(statisticalProperties)
}

linexCountryRow <- function(countryAbbrev, energyType){
  ############
  # Creates a row for the LINEX parameters table for the given country (2-letter code) and energyType (Q, X, or U)
  ##
  dataLINEX <- linexData(countryAbbrev, energyType)
  out <- cbind(dataLINEX["-95% CI", "a_0"], dataLINEX["LINEX", "a_0"], dataLINEX["+95% CI", "a_0"],
               dataLINEX["-95% CI", "c_t"], dataLINEX["LINEX", "c_t"], dataLINEX["+95% CI", "c_t"])
  return(out)
}

linexCountryRowsForParamsGraph <- function(countryAbbrev, energyType){
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
  dataLINEX <- linexData(countryAbbrev, energyType)
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

createLINEXParamsGraph <- function(energyType){
  #############################
  # Creates a graph with confidence intervals for the LINEX model for the given energyType
  ##
  # Create a data table with the following columns:
  # country abbrev, parameter (a_0, c_t), -95% CI, value, +95% CI
  data <- do.call("rbind", lapply(countryAbbrevs, linexCountryRowsForParamsGraph, energyType=energyType))
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

linexParamsTableDF <- function(energyType){
  #####################
  # Creates a data.frame containing all parameters and their confidence intervals 
  ##
  #Do rbind on the results of creating a row in the table for every country abbreviation that we know.
  dataLINEX <- do.call("rbind", lapply(countryAbbrevs, linexCountryRow, energyType=energyType))
  colnames(dataLINEX) <- c("lowerCI_a_0", "a_0", "upperCI_a_0",
                           "lowerCI_c_t", "c_t", "upperCI_c_t")
  rownames(dataLINEX) <- countryAbbrevs
  return(dataLINEX)
}

linexParamsTable <- function(energyType){
  ############################
  # Aggregates the LINEX results into a big data table for the given energyType.
  ##
  dataLINEX <- linexParamsTableDF(energyType)
  colnames(dataLINEX) <- c(" ", "$a_0$", " ", " ", "$c_t$",  " ")
  rownames(dataLINEX) <- countryAbbrevs
  tableLINEX <- xtable(dataLINEX, 
                       caption=paste("LINEX model (with $", tolower(energyType), "$) parameters for 1980--2011 (US, UK, JP) and 1991--2011 (CN, ZA, SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                       label=paste("tab:LINEX_Parameters_With_", energyType, sep=""),
                       digits = c(1, 2,2,2, 2,2,2),
                       align = "r|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableLINEX)
}

printLINEXParamsTable <- function(energyType){
  ############################
  # Prints a table with parameters from a LINEX model for the given energyType. 
  ##
  print(linexParamsTable(energyType), 
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny",
        table.placement="H")
}

## <<AIC_Table_Functions, eval=TRUE>>=
createAICTable <- function(){
  ###############################
  # Creates an xtable object that holds the AIC values for each parameter estimation that we include.
  ##
  ######################
  # Single-factor models
  ######################
  # Single-factor with K
  sfKModels <- lapply(countryAbbrevs, singleFactorModel, factor="K", respectRangeConstraints=TRUE)
  aicSFk <- data.frame(lapply(sfKModels, AIC))
  rownames(aicSFk) <- "SF$k$"
  # Single-factor with L
  sfLModels <- lapply(countryAbbrevs, singleFactorModel, factor="L", respectRangeConstraints=TRUE)
  aicSFl <- data.frame(lapply(sfLModels, AIC))
  rownames(aicSFl) <- "SF$l$"
  # Single-factor with Q
  sfQModels <- lapply(countryAbbrevs, singleFactorModel, factor="Q", respectRangeConstraints=TRUE)
  aicSFq <- data.frame(lapply(sfQModels, AIC))
  rownames(aicSFq) <- "SF$q$"
  # Single-factor with X
  sfXModels <- lapply(countryAbbrevs, singleFactorModel, factor="X", respectRangeConstraints=TRUE)
  aicSFx <- data.frame(lapply(sfXModels, AIC))
  rownames(aicSFx) <- "SF$x$"
  # Single-factor with U
  aicSFu <- cbind(US=AIC(singleFactorModel(countryAbbrev="US", factor="U", respectRangeConstraints=TRUE)), 
                  UK=AIC(singleFactorModel(countryAbbrev="UK", factor="U", respectRangeConstraints=TRUE)), 
                  JP=AIC(singleFactorModel(countryAbbrev="JP", factor="U", respectRangeConstraints=TRUE)),
                  CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicSFu) <- "SF$u$"
  ######################
  # Cobb-Douglas models
  ######################
  # Cobb-Douglas without energy
  cdModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="none", respectRangeConstraints=TRUE)
  aicCD <- data.frame(lapply(cdModels, AIC))
  rownames(aicCD) <- "CD"
  # Cobb-Douglas with Q
  cdQModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="Q", respectRangeConstraints=TRUE)
  aicCDq <- data.frame(lapply(cdQModels, AIC))
  rownames(aicCDq) <- "CD$q$"
  # Cobb-Douglas with X
  cdXModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="X", respectRangeConstraints=TRUE)
  aicCDx <- data.frame(lapply(cdXModels, AIC))
  rownames(aicCDx) <- "CD$x$"
  # Cobb-Douglas with U
  aicCDu <- cbind(US=AIC(cobbDouglasModel(countryAbbrev="US", energyType="U", respectRangeConstraints=TRUE)), 
                  UK=AIC(cobbDouglasModel(countryAbbrev="UK", energyType="U", respectRangeConstraints=TRUE)), 
                  JP=AIC(cobbDouglasModel(countryAbbrev="JP", energyType="U", respectRangeConstraints=TRUE)),
                  CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicCDu) <- "CD$u$"
  ######################
  # CES models
  ######################
  # At present, this AIC for CES code is not working. Perhaps because the CES model from the cesEst function
  # in the micEcon package does not provide its data in the correct format for the AIC function?
  # --Matthew Kuperus Heun, 10 April 2013.
  #   # CES with Q
  #   cesQModels <- lapply(countryAbbrevs, cesModel2, energyType="Q")
  #   aicCESq <- data.frame(lapply(cesQModels, AIC))
  #   rownames(aicCESq) <- "CES$q$"
  #   # CES with X
  #   cesXModels <- lapply(countryAbbrevs, cesModel2, energyType="X")
  #   aicDEXx <- data.frame(lapply(cesXModels, AIC))
  #   rownames(aicCESx) <- "CES$x$"
  #   # CES with U
  #   aicCESu <- cbind(US=AIC(cesModel2("US", "U")), 
  #                   UK=AIC(cesModel2("UK", "U")), 
  #                   JP=AIC(cesModel2("JP", "U")),
  #                   CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  #   rownames(aicCESu) <- "CES$u$"
  aicCES  <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCES) <- "CES"
  aicCESq <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCESq) <- "CES$q$"
  aicCESx <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCESx) <- "CES$x$"
  aicCESu <- data.frame(US=NA, UK=NA, JP=NA, CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
  rownames(aicCESu) <- "CES$u$"
  ######################
  # LINEX models
  ######################
  # LINEX with Q
  linexQModels <- lapply(countryAbbrevs, linexModel, energyType="Q")
  aicLINEXq <- data.frame(lapply(linexQModels, AIC))
  rownames(aicLINEXq) <- "LINEX$q$"
  # LINEX with X
  linexXModels <- lapply(countryAbbrevs, linexModel, energyType="X")
  aicLINEXx <- data.frame(lapply(linexXModels, AIC))
  rownames(aicLINEXx) <- "LINEX$x$"  
  # LINEX with U
  aicLINEXu <- cbind(US=AIC(linexModel(countryAbbrev="US", energyType="U")), 
                     UK=AIC(linexModel(countryAbbrev="UK", energyType="U")), 
                     JP=AIC(linexModel(countryAbbrev="JP", energyType="U")),
                     CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicLINEXu) <- "LINEX$u$"
  
  # Create the data.frame table
  out <- rbind(aicSFk, aicSFl, aicSFq, aicSFx, aicSFu, 
               aicCD, aicCDq, aicCDx, aicCDu, 
               aicCES, aicCESq, aicCESx, aicCESu, 
               aicLINEXq, aicLINEXx, aicLINEXu) 
  # Create the xtable with caption and label
  out <- xtable(out, caption="AIC values for all models.", label="tab:AICTable", digits=1)
  return(out)
}

## <<CIvsParam_Graph, eval=TRUE>>=
CIvsParamDF <- function(model, param, energyType="none", factor=NA){
  ############################
  # Creates a data.frame that contains the following information:
  # row name: 2-letter country abbreviation
  # 1st col: parameter value. The name of this column is the name of the parameter, "sigma", "delta", etc. 
  #          as appropriate for the model generating the data
  # 2nd col: width of the confidence interval. The name of this column is "CI"
  # 3rd col: an identifier for parameter, model, and (factor or energy type). 
  #          For example, if you want the sigma parameter for the CES model with Q, you would get "sigmaCESeq".
  #          If you want the m parameter for the single-factor mdoel with k, you would get "mSFk".
  #          If you want the alpha parameter for the Cobb-Douglas model without energy, you would get "alphaCD".
  #          The name of the column is "factor"
  ##
  # Get the data from the requested model
  if (model == "SF"){data <- singleFactorParamsDF(factor)} 
  else if (model == "CD"){data <- cobbDouglasParamsTableNoEnergyDF()}
  else if (model == "CDe"){data <- cobbDouglasParamsTableWithEnergyDF(energyType=energyType)}
  else if (model == "CES"){data <- cesParamsTableDF(energyType="none")}
  else if (model == "CESe"){data <- cesParamsTableDF(energyType=energyType)}
  else if (model == "LINEX"){data <- linexParamsTableDF(energyType)}
  # Now get the data for the parameter requested
  # Also, set limits on the x and y axes if needed
  if (param == "lambda"){
    x <- subset(data, select=lambda) # The column of lambdas
    y <- subset(data, select=upperCI_lambda) - subset(data, select=lowerCI_lambda) # A column of CI values
  } else if (param == "m"){
    x <- subset(data, select=m) # The column of m's
    colnames(x) <- "m"
    y <- subset(data, select=upperCI_m) - subset(data, select=lowerCI_m) # A column of CI values
  } else if (param == "alpha"){
    x <- subset(data, select=alpha) # The column of alphas
    y <- subset(data, select=upperCI_alpha) - subset(data, select=lowerCI_alpha) # A column of CI values
  } else if (param == "beta"){
    x <- subset(data, select=beta) # The column of betas
    y <- subset(data, select=upperCI_beta) - subset(data, select=lowerCI_beta) # A column of CI values
  } else if (param == "gamma"){
    x <- subset(data, select=gamma) # The column of gammas
    y <- subset(data, select=upperCI_gamma) - subset(data, select=lowerCI_gamma) # A column of CI values
  } else if (param == "delta_1"){
    x <- subset(data, select=delta_1) # The column of delta_1's
    y <- subset(data, select=upperCI_delta_1) - subset(data, select=lowerCI_delta_1) # A column of CI values
  } else if (param == "sigma_1"){
    x <- subset(data, select=sigma_1) # The column of sigma_1's
    y <- subset(data, select=upperCI_sigma_1) - subset(data, select=lowerCI_sigma_1) # A column of CI values
  } else if (param == "delta"){
    x <- subset(data, select=delta) # The column of deltas
    y <- subset(data, select=upperCI_delta) - subset(data, select=lowerCI_delta) # A column of CI values
  } else if (param == "sigma"){
    x <- subset(data, select=sigma) # The column of sigmas
    y <- subset(data, select=upperCI_sigma) - subset(data, select=lowerCI_sigma) # A column of CI values
  } else if (param == "a_0"){
    x <- subset(data, select=a_0) # The column of a_0's
    y <- subset(data, select=upperCI_a_0) - subset(data, select=lowerCI_a_0) # A column of CI values
  } else if (param == "c_t"){
    x <- subset(data, select=c_t) # The column of c_t's
    y <- subset(data, select=upperCI_c_t) - subset(data, select=lowerCI_c_t) # A column of CI values
  }
  colnames(y) <- "CI"
  # Create the factor string. We are guaranteed to have a value for the param and model arguments.
  # However, we might have NA for either energyType or factor. But, we will not have values for both 
  # energyType and factor at the same time! So, we can simply paste everything together here and 
  # obtain the desired result.
  if (is.na(factor)){
    factorString <- paste(param, model, energyType, sep="")
  } else if (energyType == "none"){
    factorString <- paste(param, model, factor, sep="")
  } else {
    print("Neither energyType nor factor were NA in CIvsParamDF. But one should be!")
    return(NULL)
  }
  nRows <- nrow(x)
  factor <- as.data.frame(matrix(factorString, ncol=1, nrow=nRows))
  colnames(factor) <- "factor"
  # Now make a column containing the country abbreviations.
  Country <- data.frame(countryAbbrevs)
  data <- cbind(x, y, factor, Country)
  return(data)
}

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
createDataForPartialResidualPlot <- function(countryAbbrev, modelType, energyType){
  #############################
  # Creates a data.frame containing raw data and residuals for given arguments.
  # The residuals are for a model that does not use energy.
  # The name of the column of energyType for countryAbbrev is changed to 
  # "iEToFit"
  # modelType: one of "CD" or "CES"
  # countryAbbrev: the country of interest to you
  # energyType: one of "Q", "X", or "U"
  # returns: a data.frame containing data for countryAbbrev with an additional
  #          column containing the residual for each year. The name of the 
  #          column for energyType is changed to "iEToFit"
  ##
  # Get the model
  if (modelType == "CD"){
    model <- cdModel(countryAbbrev)  
  } else if (modelType == "CES"){
    model <- bestModel(cesModel2(countryAbbrev=countryAbbrev))
  } else {
    stop(paste("Unknown modelType:", modelType, "in partialResidualPlot."))
    return(NULL)
  }
  # Get the residuals
  resid <- resid(model)
  resid <- data.frame(resid)
  resid <- padRows(countryAbbrev=countryAbbrev, df=resid)
  # Load data
  data <- loadData(countryAbbrev=countryAbbrev)
  data <- cbind(data, resid)
  # Replace name of column we want with the name "iEToFit"
  data <- replaceColName(data=data, factor=energyType, newName="iEToFit")
  
  return(data)
}

createPartialResidualPlot <- function(modelType, energyType="none", countryAbbrev=NA, textScaling=1.0){
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
    data <- createDataForPartialResidualPlot(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType)
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

getSeed <- function(){
  ######################
  # Returns the seed that we'll use for all resampling. I'm putting 
  # the seed into a function so that it is accessible from 
  # many places (including the paper, should we choose to include it there).
  ##
  return(123)
}

loadResampleData <- function(modelType, countryAbbrev, energyType="none", factor=NA, archive=NULL){
  #############################
  # This function loads previously-saved Cobb-Douglas with energy
  # curve fits from resampled data. The loaded object is
  # a list that contains two named data.frames: 
  # baseFitCoeffs and resampleFitCoeffs. 
  ##
  path <- getPathForResampleData(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor)
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

loadResampleModels <- function(modelType, countryAbbrev, energyType="none", factor=NA, archive=NULL){
  #############################
  # This function loads previously-saved models
  # from resampled data. The loaded object is
  # a list that contains (in the first slot) the model for 
  # the fit to historical data and all models
  # for the fits to resampled data.
  ##
  path <- getPathForResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor)
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

loadAllResampleData <- function(modelType, energyType="none", factor, 
                                countryAbbrevsOrder=countryAbbrevs,
                                archive=NULL){
  ##################
  # Loads resample data for all countries for the given modelType and energyType or factor
  ##
  if (!missing(energyType) && !missing(factor)){
    stop(paste("energyType =", energyType, "and factor =", factor, 
               "in loadAllResampleData. Didn't expect both to be specified. Can't proceed."))
  }
  if (!missing(energyType)){
    if (energyType == "none" || energyType != "U"){
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder, loadResampleData, modelType=modelType, 
                                           energyType=energyType, archive=archive))
    }
    else {
      # energyType is "U"
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder[1:3], loadResampleData, modelType=modelType, 
                                           energyType=energyType, archive=archive))
    }
  } else if (!missing(factor)){
    if (factor == "U"){
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder[1:3], loadResampleData, 
                                           modelType=modelType, factor=factor, 
                                           archive=archive))
    } else {
      data <- do.call("rbind.fill", lapply(countryAbbrevsOrder, loadResampleData, modelType=modelType, 
                                           factor=factor, archive=archive))
    }
  } else {
    # Neither energyType nor factor were specified
    data <- do.call("rbind.fill", lapply(countryAbbrevsOrder, loadResampleData, 
                                         modelType=modelType, archive=archive))
  }
  return(data)
}

loadResampleData2 <- function(modelType = c("cese-(kl)e", "cese-(le)k", "cese-(ek)l"), 
                              energyType="Q", factor=NULL, 
                              countryAbbrev=countryAbbrevs,
                              archive=NULL){
  ##################
  # Loads resample data for all countries for the given modelType and energyType or factor
  ##
  if (!is.null(energyType) && !is.null(factor)){
    warning(paste("Both 'energyType' and 'factor' given in loadAllResampleData2()", 
                   "  Ignoring 'factor'."))
  }
  
  if (!is.null(energyType)){
    grid <- expand.grid(country=countryAbbrev, model=modelType, energy=energyType, 
                        stringsAsFactors=FALSE)
    
    grid$n <- 1:nrow(grid)
    return( ddply( grid, .(n), function(x){
      loadResampleData(modelType=x$model[1], countryAbbrev = x$country[1],
                       energyType=x$energy[1], archive=archive) }
    ))
  } 
  if (! is.null(factor) ) {
    grid <- expand.grid(country=countryAbbrev, model=modelType, factor=factor, 
                        stringsAsFactors=FALSE)
    grid$n <- 1:nrow(grid)
    return( ddply( grid, .(n), 
            loadResampleData(modelType=model, countryAbbrev=country, 
                             factor=factor, archive=archive)))
  }
    
  # Neither energyType nor factor were specified
  grid <- expand.grid(country=countryAbbrev, model=modelType, stringsAsFactors=FALSE)
  grid$n <- 1:nrow(grid)
  return(ddply( grid, .(n), 
                loadResampleData(country=country, modelType=model, archive=archive)))
}

loadResampleDataRefitsOnly <- function(modelType, countryAbbrev, energyType="none", factor){
  ####################
  # Loads coefficients for resampled data only from a previously-run set of resample curve fits
  ##
  data <- loadResampleData(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor)
  # Select only those rows that aren't the original curve fit
  data <- data[data[["method"]]!="orig", ]
  return(data)
}

loadResampleModelsRefitsOnly <- function(countryAbbrev, modelType, energyType="none", factor, archive=NULL){
  ####################
  # Loads models for resampled data only from a previously-run set of resample curve fits
  ##
  models <- loadResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor, archive=archive)
  # Select only those models that aren't from the curve fit to historical data (which is in position 1)
  len <- length(models)
  # Return everything but the first element (which is the fit to historical data).
  return(models[-1])
}

loadResampleDataBaseFitOnly <- function(modelType, countryAbbrev, energyType="none", factor){
  ####################
  # Loads the base fit coefficients only from a previously-run curve fit
  ##
  data <- loadResampleData(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor) 
  # Select the row containing the original curve fit
  data <- data[data[["method"]]=="orig", ]
  return(data)
}

loadResampleModelsBaseModelOnly <- function(modelType, countryAbbrev, energyType="none", factor, archive=NULL){
  ####################
  # Loads the model for a fit to historical data
  ##
  models <- loadResampleModels(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor, archive=archive) 
  # Select the first model, which is the model for the fit to historical data  
  return(models[[1]])
}

getPathForResampleData <- function(modelType, countryAbbrev, energyType="none", factor){
  ######################
  # Returns a string identifying the entire file path in which we 
  # hold resampled data
  ##
  return(doGetPath(prefix="resampleData", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor))
}

getPathForResampleModels <- function(modelType, countryAbbrev, energyType="Q", factor="K"){
  ######################
  # Returns a string identifying the entire file path in which we 
  # hold resampled models
  ##
  return(doGetPath(prefix="resampleModels", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor))
}

doGetPath <- function(prefix, modelType, countryAbbrev, energyType="Q", factor="K"){
  if (energyType == "none"){
    energyType="NA"
  }
  folder <- getFolderForResampleData(modelType=modelType, countryAbbrev=countryAbbrev)  
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

getFolderForResampleData <- function(modelType=modelTypes, countryAbbrev=countryAbbrevs){
  ##################
  # Returns a string identifying a folder for resampled data.
  ##
  dr <- getOption('heun_data_resample')
  if (is.null(dr)) dr <- "data_resample"
#   modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  folder <- switch(modelType,
                   "cde"       = file.path(dr, "cd",      countryAbbrev),
                   "cese-(kl)" = file.path(dr, "ces",     countryAbbrev),
                   file.path(dr, modelType, countryAbbrev)
                   )
  return(folder)
}

fracUnconvergedResampleFitsAll <- function(){
  ###########################
  # Calculates the fraction of unconverged resamples stored on disk 
  # for all countries and all energy types
  ## 
  out <- data.frame()
  modelType <- "sf"
  for (factor in factors){
    if (factor == "U"){
      unconverged <- lapply(countryAbbrevsU, fracUnconvergedResampleFits, modelType=modelType, factor=factor)
      uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
      unconverged <- c(unconverged, uNA)
    } else {
      unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, factor=factor)
    }
    unconverged <- c(modelType=modelType, energyType="none", factor=factor, unconverged)
    out <- rbind(out, as.data.frame(unconverged))
  }
  
  modelType <- "cd"
  unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, factor=NULL)
  unconverged <- c(modelType=modelType, energyType="none", factor=NA, unconverged)
  out <- rbind(out, as.data.frame(unconverged))    
  
  modelType <- "cde"
  for (energyType in energyTypes){
    if (energyType == "U"){
      unconverged <- lapply(countryAbbrevsU, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType)
      uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
      unconverged <- c(unconverged, uNA)
    } else {
      unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType)
    }
    unconverged <- c(modelType=modelType, energyType=energyType, factor=NA, unconverged)
    out <- rbind(out, as.data.frame(unconverged))
  }
  
  modelType <- "ces"
  unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType)
  unconverged <- c(modelType=modelType, energyType="none", factor=NA, unconverged)
  out <- rbind(out, as.data.frame(unconverged))    
  
  for (modelType in c("cese-(kl)e", "cese-(le)k", "cese-(ek)l", "linex")){
    for (energyType in energyTypes){
      if (energyType == "U"){
        unconverged <- lapply(countryAbbrevsU, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType)
        uNA <- c(CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA)
        unconverged <- c(unconverged, uNA)
      } else {
        unconverged <- lapply(countryAbbrevs, fracUnconvergedResampleFits, modelType=modelType, energyType=energyType)
      }
      unconverged <- c(modelType=modelType, energyType=energyType, factor=NA, unconverged)
      out <- rbind(out, as.data.frame(unconverged))
    }
  }
  colnames(out) <- c("Model", "Energy", "Factor", countryAbbrevs)
  return(out)
}

fracUnconvergedResampleFits <- function(modelType=modelTypes, 
                                        countryAbbrev=countryAbbrevs, 
                                        energyType=energyTypes, 
                                        factor=factors, ...){
  ###################
  # Gives the fraction of resample fits that did not converge for the 
  # given parameters.
  ##
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  energyType <- match.arg(energyType)
  factor <- match.arg(factor)
  data <- loadResampleDataRefitsOnly(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor)
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

nResamples <- function(modelType=modelTypes, 
                       countryAbbrev=countryAbbrevs, 
                       energyType=energyTypes, 
                       factor=factors, ...){
  ###################
  # Gives the number of resample fits for the 
  # given parameters.
  ##
  modelType <- match.arg(modelType)
  countryAbbrev <- match.arg(countryAbbrev)
  energyType <- match.arg(energyType)
  factor <- match.arg(factor)
  data <- loadResampleDataRefitsOnly(modelType=modelType, countryAbbrev=countryAbbrev, energyType=energyType, factor=factor)
  nObs <- nrow(data)
  return(nObs)
}

printFracUnconvergedXtable <- function(){
  #####################################
  # This function prints a table containing the fraction of unconverged
  # resample models.
  ##
  data <- fracUnconvergedResampleFitsAll()
  dataXtable <- xtable(x=data, 
                       caption="Fraction of unconverged resample models", 
                       label="tab:frac_unconverged_models",
                       digits = 3,
                       align = "rl|cc|ccccccccc") #Sets alignment of the numbers in the columns)
  print(dataXtable, 
        caption.placement="top", 
        size="\\tiny",
        table.placement="H",
        include.rownames=FALSE)
}

numResamples <- function(){
  ####################
  # Gives the desired number of resamples to be performed.
  ##
  return(1000)
}