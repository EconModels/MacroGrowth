## <<setup_parent, echo=FALSE, message=FALSE, eval=TRUE>>=
require(lattice)
require(latticeExtra)
require(ggplot2)
require(car)
require(mosaic)
require(xtable)
require(nlmrt)
require(micEconCES)
require(reshape2) # Provides access to data melting. See http://cran.r-project.org/web/packages/reshape2/reshape2.pdf
# tikz allows use of LaTeX formatting and font in graphs. Allows for a consistent look across the paper.
# See http://r-forge.r-project.org/R/?group_id=440 for instructions on installing tikzDevice.
# require(tikzDevice) 

# Statistical significance levels. We'll work with 95% CIs
ciLevel <- 0.95
ciHalfLevel <- ciLevel + (1.0-ciLevel)/2.0
# List of countries
countryAbbrevs <- c(US="US", UK="UK", JP="JP", CN="CN", ZA="ZA", SA="SA", IR="IR", TZ="TZ", ZM="ZM")
countryAbbrevsAlph <- sort(countryAbbrevs)
countryAbbrevsU <- c(US="US", UK="UK", JP="JP") #Only these countries have useful work data
countryAbbrevsAlphU <- sort(countryAbbrevsU)
countryNamesAlph <- c(CN="China", IR="Iran", JP="Japan", SA="Saudi Arabia", TZ="Tanzania", UK="United Kingdom", US="USA", ZA="South Africa", ZM="Zambia") #In alphabetical order.
countryNamesAlphU <- c(JP="Japan", UK="United Kingdom", US="USA") #In alphabetical order.
yLimitsForGDPGraphs <- list(c(1,10), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4), c(1,4)) # Alph order
energyTypes <- c(Q="Q", X="X", U="U") # List of energy types
factors <- c(K="K", L="L", Q="Q", X="X", U="U") # List of factors of production
########### Several global parameters for graphs. Set here and use below to ensure consistent appearance of graphs.
# Set the order for presenting countries in 3x3 lattice graphs. Default is alphabetical. 
# "1" means first alphabetically.
countryOrderForGraphs <- c(7,6,3,1,8,5,4,2,9) # Sets the order as US, UK, JP, CN, ZA, TZ, SA, IR, ZM.
countryOrderForGraphsU <- c(3, 2, 1) # Sets the order as US, UK, JP when we're looking only at energyType=U.
timeTics <- c(1980, 1990, 2000, 2010)
yTicsForIndexedGraphs <- c(1,2,3,4,5,6,7,8,9,10) #y tic mark locations.

maxWidth <- 6.5 #Inches
# Full page lattice plot sizes
ninePanelLayoutSpec <- c(3,3) #indicates a 3x3 arrangement of panels.
threePanelLayoutSpec <- c(3,1) #indicates a 1 row x 3 column arangement of panels
onePanelLayoutSpec <- c(1,1) #indicates a 1x1 arrangement of panels.
threePanelGraphWidth <- maxWidth
threePanelGraphHeight <- 3.17 #Inches
ninePanelGraphWidth <- maxWidth     
ninePanelGraphHeight <- 7.5  #Inches
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
cesParameterGraphWidth <- maxWidth
linexParameterGraphWidth <- maxWidth
# Other graph parameters that apply to all graphs
scaleTextSize <- 0.8  #Multiple of normal size
scaleTickSize <- -0.5 #50% of normal size and pointing INWARD!

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
  dataTable <- read.table(file=fileName, header=TRUE)
  return(dataTable)
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

haveDataCD <- function(countryAbbrev, energyType){
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
  if (is.na(energyType)){
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

haveDataCES <- function(countryAbbrev, energyType){
  ###############################
  # Tells whether we have data for this combination of country and energy type for a CES curve fit.
  # a missing energyType or energyType == NA means that we want to develop a model without energy, if possible.
  # This function re-routes to haveDataCD, because the logic is identical.
  #
  # returns logical value (TRUE or FALSE)
  ##
  return(haveDataCD(countryAbbrev, energyType))
}

haveDataLINEX <- function(countryAbbrev, energyType){
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

columnIndex <- function(dataTable, factor){
  ##############################
  # Returns an integer representing the column index for some data
  # dataTable the data.frame in which you want to change column names
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
  colIndex <- which(names(dataTable) %in% colName) #Find index of desired column
  return(colIndex)  
}

replaceColName <- function(dataTable, factor, newName){
  ##############################
  # Replaces a column name with the given string
  # dataTable the data.frame that you're working with
  # factor should be a string and one of Year, Y, K, L, Q, X, or U
  # newName should be a string and the desired new name of the column
  # returns dataTable with a new name for one of its factor column.
  ##
  colIndex <- columnIndex(dataTable=dataTable, factor=factor)
  colnames(dataTable)[colIndex] <- newName #Change desired column name to newName
  return(dataTable)
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
  lineTypes <- c(0, 1, 5, 2, 4, 1) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(0, 2, 2, 1, 1, 1) #line widths. 0 means no line.
  colors <- c("black", "black", "black", "red", "blue", "darkorange") #line and symbol colors
  symbols <- c(1, NA, NA, NA, NA, NA)  #NA gives no symbol.
  # Code that deals with items that are specific to whether we want all countries or a specific country.
  if (missing(countryAbbrev)){
    # We want a graph with panels for all countries
    dataTable <- loadData("All")
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
    yLimits <- yLimitsForGDPGraphs
  } else {
    # We want only a specific country
    dataTable <- loadData(countryAbbrev)
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    yLimits <- yLimitsForGDPGraphs[index:index]       # Pick limits for the country we want.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
  }
  graph <- xyplot(iGDP+iCapStk+iLabor+iQ+iX+iU ~ Year | Country, data=dataTable,
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
                  key=list(text=list(c("$y$", "$k$", "$l$", "$q$", "$x$", "$u$")),
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

## <<single-factor functions, eval=TRUE>>=
singleFactorModel <- function(data=loadData(countryAbbrev), countryAbbrev, factor){
  ####################
  # Returns an nls single-factor model for the country and factor specified.
  # factor should be one of "K", "L", "Q", "X", or "U".
  ##
  # We'll change the name of the desired column to "f"
  data <- replaceColName(data, factor, "f")
  # Now do the fit.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  mGuess <- 0.5 # works for almost every country
  # Some economies need different guesses
  # I'm commenting the following code for now. The goal here is to make it 
  # possible to supply a data.frame to this method.
#   if (countryAbbrev == "IR" && factor == "X"){
#     mGuess <- 0.7
#   }
  start <- list(lambda=lambdaGuess, m=mGuess)
  # Runs a non-linear least squares fit to the data. We've replaced beta with 1-alpha for simplicity.
  model <- iGDP ~ exp(lambda*iYear) * f^m
  modelSF <- nls(formula=model, data=data, start = start)
  #   model <- iGDP ~ exp(lambda*iYear) * f^m
  #   modelSF <- wrapnls(formula=model, data=dataTable, start=list(lambda=lambdaGuess, m=mGuess))
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

createSFLatticeGraph <- function(countryAbbrev, textScaling = 1.0, keyXLoc = defaultKeyXLoc, keyYLoc = defaultKeyYLoc){
  ##############################
  # Creates a graph that plots predicted GDP as lines, one for each single factor, and historical GDP 
  # data as open circles.
  ##
  dataTable <- loadData("All") #Grab the raw data
  predictionsK <- singleFactorPredictionsColumn("K") #Predictions from SF with K
  predictionsL <- singleFactorPredictionsColumn("L") #Predictions from SF with L
  predictionsQ <- singleFactorPredictionsColumn("Q") #Predictions from SF with Q
  predictionsX <- singleFactorPredictionsColumn("X") #Predictions from SF with X
  predictionsU <- singleFactorPredictionsColumn("U") #Predictions from SF with U
  #Now add the predictions columns to the data.
  dataTable <- cbind(dataTable, predictionsK, predictionsL, predictionsQ, predictionsX, predictionsU)
  # Code for all graphs, regardless of whether we want to focus on a specific country
  graphType <- "b" #b is for both line and symbol
  lineTypes <- c(0, 1, 5, 2, 4, 1) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(0, 2, 2, 1, 1, 1) #line widths. 0 means no line.
  colors <- c("black", "black", "black", "red", "blue", "darkorange") #line and symbol colors
  symbols <- c(1, NA, NA, NA, NA, NA)  #NA gives no symbol.
  # Code that deals with items that are specific to whether we want all countries or a specific country.  
  if (missing (countryAbbrev)){
    # We want a graph with panels for all countries
    yLimits <- yLimitsForGDPGraphs
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
  } else {
    # We want only a specific country
    dataTable <- subset(dataTable, Country == countryAbbrev)    
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    yLimits <- yLimitsForGDPGraphs[index:index]       # Pick limits for the country we want.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
  }  
  graph <- xyplot(iGDP+predGDPK+predGDPL+predGDPQ+predGDPX+predGDPU ~ Year | Country, data=dataTable,
                  type=graphType,
                  index.cond=indexCond, #orders the panels.
                  layout=layout, #indicates a 3x3 arrangement of panels.
                  strip = strip.custom(factor.levels=factorLevels, 
                                       bg="transparent", # Sets background transparent to match the graph itself.
                                       par.strip.text=list(cex=textScaling) # Scales text in the strip.
                  ),
                  as.table=TRUE, #indexing of panels starts in upper left and goes across rows.
                  lty = lineTypes, lwd = lineWidths, col = colors, #Controls line parameters
                  pch = symbols, col.symbol = colors, #Controls symbol parameters
                  key=list(text=list(c("Actual", "With $k$", "With $l$", "With $q$", "With $x$", "With $u$")),
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
  # We have a combination of country and factor for which we have data.
  modelSF <- singleFactorModel(countryAbbrev=countryAbbrev, factor=factor)
  summarySF <- summary(modelSF) # Gives the nls summary table.
  ciSF <- confint(modelSF, level = ciLevel)  # Calculates confidence intervals for the SF model.
  dofSF <- summarySF$df[2] # Gives the degrees of freedom for the model.
  tvalSF <- qt(ciHalfLevel, df = dofSF)
  #Single factor model
  lambda <- as.numeric(coef(modelSF)["lambda"])
  m <- as.numeric(coef(modelSF)["m"])
  # Combine all estimates and their confidence intervals into data frames with intelligent row names
  estSF <- data.frame(lambda = lambda, m = m)
  row.names(estSF) <- "SF"
  # The [1] subscripts pick off the lower confidence interval
  lowerSF <- data.frame(lambda=ciSF["lambda","2.5%"], 
                        m=ciSF["m", "2.5%"])
  row.names(lowerSF) <- "-95% CI"
  # The [2] subscripts pick off the upper confidence interval
  upperSF <- data.frame(lambda=ciSF["lambda","97.5%"], 
                        m=ciSF["m", "97.5%"])
  row.names(upperSF) <- "+95% CI"
  # Now create the data for a table and return it
  dataSF <- rbind(upperSF, estSF, lowerSF)
  return(dataSF)
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
                    caption=paste("Single-factor model (with $", tolower(factor), "$) parameters for 1980-2011 (US, UK, JP), 1991-2010 (CN and ZA), and 1991-2011 (SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
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
  dataTable <- do.call("rbind", lapply(countryAbbrevs, singleFactorCountryRowsForParamsGraph, factor=factor))
  graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                   data = dataTable,
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
                                 y=list(relation="free", #allow each axis to be different
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

cdModel <- function(countryAbbrev, data=loadData(countryAbbrev), ...){
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
  model <- iGDP ~ exp(lambda*iYear) * iCapStk^alpha * iLabor^(1 - alpha)
  modelCD <- nls(formula=model, data=data, start=start,
                 #Include the next 3 lines to fit with constraints.
                 #algorithm = "port",
                 #lower = list(lambda=-Inf, alpha=0),
                 #upper = list(lambda=Inf, alpha=1)
  )
  return(modelCD)
}

cdeModel <- function(countryAbbrev, energyType, data=loadData(countryAbbrev), ...){
  ####################
  # Returns an nls Cobb-Douglas model for the country specified
  # energyType is one of "Q", "X", or "U".
  ##
  # We need to do the Cobb-Douglas fit with the desired energy data.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function. 
  data <- replaceColName(data, energyType, "iEToFit")
  # Establish guess values for lambda, alpha, and beta.
  lambdaGuess <- 0.0 #guessing lambda = 0 means there is no technological progress.
  alphaGuess <- 0.9
  betaGuess <- 1.0 - alphaGuess
  aGuess <- alphaGuess
  bGuess <- alphaGuess + betaGuess
  start <- list(lambda=lambdaGuess, a=aGuess, b=bGuess)
  # Now actually do the fit, using the column name "iEToFit".
  # gamma is a free parameter.
  # Reparameterize to ensure that we meet the constraints:
  # * alpha + beta + gamma = 1.0.
  # * alpha, beta, and gamma are all between 0.0 and 1.0.
  # To do this, we reparameterize as
  # * 0 < a < 1
  # * 0 < b < 1
  # * alpha = min(a, b)
  # * beta = b - a
  # * gamma = 1 - max(a, b)
  model <- iGDP ~ exp(lambda*iYear) * iCapStk^min(a,b) * iLabor^abs(b-a) * iEToFit^(1.0 - max(a,b))
  modelCDe <- nls(formula=model, data = data, start = start,
                  control = nls.control(maxiter = 200, 
                                        tol = 1e-05, 
                                        minFactor = 1/1024, 
                                        printEval=FALSE, #Tells whether to print details of curve fit process.
                                        warnOnly=TRUE),
                  #Include the next 3 lines to fit with constraints.
                  #                   algorithm = "port",
                  #                   lower = list(lambda=-Inf, a=0, b=0),
                  #                   upper = list(lambda= Inf, a=1, b=1)
  )
  return(modelCDe)
}

cdeFixedGammaModel <- function(countryAbbrev, energyType, gamma, data=loadData(countryAbbrev), ...){
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
                  control = nls.control(maxiter = 200, 
                                        tol = 1e-05, 
                                        minFactor = 1/1024, 
                                        printEval=FALSE, #Tells whether to print details of curve fit process.
                                        warnOnly=TRUE),
                  #Include the next 3 lines to fit with constraints.
                  algorithm = "port",
                  lower = list(lambda=-Inf, alpha=0),
                  upper = list(lambda=Inf, alpha=1.0-gamma)
  )
  return(modelCDe)
}

cobbDouglasModel <- function(countryAbbrev, energyType=NA, gamma, data=loadData(countryAbbrev), ...){
  ####################
  # Returns an nls Cobb-Douglas model for the country specified
  # Give an energyType ("Q", "X", or "U") if you want to include an energy term. Supply energyType=NA 
  # for a model without energy.
  # If you supply a value for the gamma argument, a fit with fixed gamma will be provided.
  # This function dispatches to cdModel, cdeModel, or cdFixedGammaModel based on which arguments are specified.
  ##
  if (is.na(energyType)){
    # Fit the Cobb-Douglas model without energy.
    #return(cdModel(countryAbbrev=countryAbbrev, ...))
    return(cdModel(data=data, ...))
  }
  if (!missing(gamma)){
    # Fit the Cobb-Douglas model with fixed value of gamma
    #return(cdeFixedGammaModel(countryAbbrev=countryAbbrev, energyType=energyType, gamma=gamma, ...))
    return(cdeFixedGammaModel(data=data, energyType=energyType, gamma=gamma, ...))
  }
  # Fit the Cobb-Douglas model with gamma as a free parameter
  #return(cdeModel(countryAbbrev=countryAbbrev, energyType=energyType, ...))
  return(cdeModel(data=data, energyType=energyType, ...))
}

cdeGridData <- function(countryAbbrev, energyType, gammaGrid){
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

cdeGridLattice <- function(energyType, countryAbbrev=NA, keyXLoc = defaultKeyXLoc, keyYLoc = defaultKeyYLoc, textScaling = 1.0){
  #######################
  # Creates a single graph or a multi-panel lattice plot showing grid search over gamma for Cobb-Douglas models with 
  # the given quantification for energy.
  # countryAbbrev: one of "US", "UK", "JP", "CN", "ZA", "SA", "IR", "TZ", or "ZM". Set to NA if you want a lattice
  # plot for all countries
  # energyType: one of "Q", "X", or "U"
  ##
  gammaGrid <- seq(0.0, 0.95, by=0.05) # Establishes the grid in which we're interested.
  alphaBetaGammaRange <- c(-0.1, 1.5)
  alphaBetaGammaTics <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
  lambdaRange <- c(0.0, 0.004)
  lambdaTics <- c(0.0, 0.001, 0.002, 0.003, 0.004)
  sseRange <- c(0.0, 0.1)
  sseTics <- c(0.02, 0.04, 0.06, 0.08, 0.10)
  gammaRange <- c(-0.1, 1.1)
  gammaTics <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
  # Create a lattice graph that shows the four variables (lambda, alpha, beta, and SSE) vs. gamma.
  graphType <- "b" #b is for both symbls and lines. Necessary so that the key looks right.
  lineTypes <- c(2, 1, 3, 0) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(1, 3, 1, 0) #line widths. 0 means no line.
  colors <- c("black", "black", "black", "black") #line and symbol colors  
  symbolSize <- 0.5
  symbolType <- 0 #for an open square
  symbols <- c(NA, NA, NA, symbolType)
  if (is.na(countryAbbrev)){
    # All countries are wanted.
    data <- do.call("rbind", lapply(countryAbbrevsAlph, cdeGridData, energyType=energyType, gammaGrid=gammaGrid))
    # Establish some formatting parameters
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
    stripSpec <- strip.custom(factor.levels=factorLevels, # Sets text for factor levels
                              bg="transparent", # Sets background transparent to match the graph itself.
                              par.strip.text=list(cex=textScaling) # Scales text in the strip.
    )
  } else {
    # Only one specific country is wanted.
    data <- cdeGridData(countryAbbrev=countryAbbrev, energyType=energyType, gammaGrid=gammaGrid)
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
    stripSpec <- FALSE # No strip needed.
  }
  abgGraph <- xyplot(alpha+beta+gamma ~ gamma | Country, data=data,
                     type=graphType,
                     lty = lineTypes, lwd = lineWidths, col = colors, #Controls line parameters
                     pch = symbols, col.symbol = colors, #Controls symbol parameters
                     index.cond = indexCond, #orders the panels.
                     layout = layout,                     
                     strip = stripSpec,
                     as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                     key=list(text=list(c("$\\alpha$", "$\\beta$", "$\\gamma$", "$SSE_y$")),
                              type=graphType,
                              cex=keyTextSize * textScaling, #controls size of text in the key
                              lines=list(lty=lineTypes, lwd=lineWidths, pch=symbols, cex=symbolSize), #controls lines
                              col=colors,
                              columns=keyColumns, x=keyXLoc, y=keyYLoc), #controls columns and position of the key
                     scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 alternating=FALSE, # eliminates left-right, top-bot alternating of axes
                                 x=list(at=gammaTics), #location for x tics
                                 y=list(at=alphaBetaGammaTics)  #location for y tics
                     ), 
                     xlim=gammaRange, #x axis limits
                     ylim=alphaBetaGammaRange, #y axis limits
                     #axis labels and scaling
                     xlab=list(label="$\\gamma$", cex=textScaling), 
                     ylab=list(label="$\\alpha$, $\\beta$, $\\gamma$", cex=textScaling)
  )
  sseGraph <- xyplot(SSE ~ gamma | Country, data=data,
                     pch = c(symbolType), col.symbol = c("black"), cex = symbolSize, #Controls symbol parameters
                     index.cond = indexCond, #orders the panels.
                     layout = layout,                     
                     strip = stripSpec,
                     as.table = TRUE, #indexing of panels starts in upper left and goes across rows.
                     scales=list(cex=scaleTextSize * textScaling, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 y=list(at=sseTics) #location for y tics                                          
                     ), 
                     xlim=gammaRange,
                     ylim=sseRange,
                     xlab=list(label="$\\gamma$", cex=textScaling), #Not used by the double-Y graph.
                     ylab=list(label="$SSE$", cex=textScaling)
  )
  doubleYGraph <- doubleYScale(abgGraph, sseGraph, add.ylab2=TRUE, use.style=FALSE)
  return(doubleYGraph)
}

cobbDouglasPredictions <- function(countryAbbrev, energyType){
  #########################
  # Takes the Cobb-Douglas fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (!is.na(energyType)){
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

cobbDouglasPredictionsColumn <- function(energyType){
  #########################
  # Takes the Cobb-Douglas fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasPredictions, energyType=energyType))
  if (is.na(energyType)){
    colnames(out) <- c("predGDP")
  } else {
    colnames(out) <- c(paste("predGDP", energyType, sep=""))
  }
  return(out)
}

cobbDouglasData <- function(countryAbbrev, energyType, ...){
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
  }
  # We have a combination of country and energy type for which we have data.
  modelCD <- cobbDouglasModel(countryAbbrev=countryAbbrev, energyType=energyType)
  #print(str(modelCD))
  summaryCD <- summary(modelCD) # Gives the nls summary table.
  #print(summaryCD)
  #print("Before CI calculation")
  # Calculates confidence intervals for the CD model.
  
  ciCD <- tryCatch( confint( profile(modelCD, ...) , level=ciLevel,...), error=function(e) { modelCD } )
  if (inherits( ciCD, "nls") ) {
    warning("Early exit from CobbDouglassData");
    return(ciCD)
  }
  #print("After CI calculation")
  
  #print(ciCD)
  dofCD <- summaryCD$df[2] # Gives the degrees of freedom for the model.
  tvalCD <- qt(ciHalfLevel, df = dofCD)
  if (is.na(energyType)){
    #Cobb-Douglas without energy
    alpha <- as.numeric(coef(modelCD)["alpha"])
    beta <- 1.0 - alpha
    beta.est <- deltaMethod(modelCD, "1 - alpha") # Estimates beta and its standard error (SE).
    # Calculate beta and its confidence interval and report it.
    # Now calculate a confidence interval on beta
    betaCICD <- with(beta.est, Estimate + c(-1.0, 1.0) * tvalCD * SE) # Gives the confidence interval on beta.  
    # Combine all estimates and their confidence intervals into data frames with intelligent row names
    estCD <- data.frame(lambda=coef(modelCD)["lambda"], alpha=coef(modelCD)["alpha"], beta=beta, gamma=0)
    row.names(estCD) <- "CD"
    # The [1] subscripts pick off the lower confidence interval
    lowerCD <- data.frame(lambda=ciCD["lambda","2.5%"], alpha=ciCD["alpha", "2.5%"], beta=betaCICD[1], gamma=NA) 
    row.names(lowerCD) <- "-95% CI"
    # The [2] subscripts pick off the lower confidence interval
    upperCD <- data.frame(lambda=ciCD["lambda","97.5%"], alpha=ciCD["alpha", "97.5%"], beta=betaCICD[2], gamma=NA)
    row.names(upperCD) <- "+95% CI"
  } else {
    #Cobb-Douglas with energy
    a <- as.numeric(coef(modelCD)["a"])
    b <- as.numeric(coef(modelCD)["b"])
    lambda <- as.numeric(coef(modelCD)["lambda"])
    alpha <- a
    beta <- b - a
    gamma <- 1.0 - b
    # Report results with SE
    beta.est <- deltaMethod(modelCD, "b-a") # Reports results for beta, because beta = b - a.
    gamma.est <- deltaMethod(modelCD, "1-b") # Reports results for gamma, because gamma = 1 - b.
    # Now calculate confidence intervals.
    betaCICD <- with(beta.est, Estimate + c(-1.0, 1.0) * tvalCD * SE) # Gives the confidence interval on beta.
    gammaCICD <- with(gamma.est, Estimate + c(-1.0, 1.0) * tvalCD * SE) # Gives the confidence interval on gamma.
    # Combine all estimates and their confidence intervals into data frames with intelligent row names
    estCD <- data.frame(lambda = lambda, alpha = alpha, beta = beta, gamma = gamma)
    row.names(estCD) <- "CDe"
    # The [1] subscripts pick off the lower confidence interval
    lowerCD <- data.frame(lambda=ciCD["lambda","2.5%"], 
                          alpha=ciCD["a", "2.5%"], 
                          beta=betaCICD[1], 
                          gamma=gammaCICD[1])
    row.names(lowerCD) <- "-95% CI"
    # The [2] subscripts pick off the upper confidence interval
    upperCD <- data.frame(lambda=ciCD["lambda","97.5%"], 
                          alpha=ciCD["a", "97.5%"], 
                          beta=betaCICD[2], 
                          gamma=gammaCICD[2])
    row.names(upperCD) <- "+95% CI"
  }
  # Now create the data for a table and return it
  dataCD <- rbind(upperCD, estCD, lowerCD)
  return(dataCD)
}

cobbDouglasCountryRow <- function(countryAbbrev, energyType){
  ############
  # Creates a row for the Cobb Douglas parameters table for the given country (2-letter code) and energyType (Q, X, or U)
  ##
  dataCD <- cobbDouglasData(countryAbbrev, energyType)
  if (is.na(energyType)){
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

cobbDouglasCountryRowsForParamsGraph <- function(countryAbbrev, energyType){
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
  if (is.na(energyType)){
    valueRow <- "CD"
    dataCD <- cobbDouglasData(countryAbbrev=countryAbbrev, energyType=NA)
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
  if (is.na(energyType)){
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
  dataCD <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRow, energyType=NA))
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
                    caption="Cobb-Douglas model (without energy) parameters for 1980-2011 (US, UK, JP), 1991-2010 (CN and ZA), and 1991-2011 (SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", 
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
                    caption=paste("Cobb-Douglas model (with $", tolower(energyType), "$) parameters for 1980-2011 (US, UK, JP), 1991-2010 (CN and ZA), and 1991-2011 (SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                    label=paste("tab:CD_Parameters_With_", energyType, sep=""),
                    digits = c(1, 4,4,4, 2,2,2, 2,2,2, 2,2,2),
                    align = "r|ccc|ccc|ccc|ccc") #Sets alignment of the numbers in the columns
  return(tableCD)
}

printCDParamsTable <- function(energyType){
  ############################
  # Prints a table with parameters from a Cobb-Douglas model for the given energyType. 
  # Set energyType=NA to print a table for Cobb-Douglas without energy.
  ##
  if (is.na(energyType)){
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

createCDParamsGraph <- function(energyType){
  #############################
  # Creates a graph with confidence intervals for the Cobb-Douglas model for the given energyType. If you 
  # want the Cobb-Douglas model without energy, supply energyType=NA.
  ##
  if (is.na(energyType)){
    # Create a data table with the following columns:
    # country abbrev, parameter (lambda, alpha, or beta), -95% CI, value, +95% CI
    dataTable <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRowsForParamsGraph, energyType=NA))
    graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                     data = dataTable, 
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
                                   y=list(relation="free", #allow each axis to be different
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
    dataTable <- do.call("rbind", lapply(countryAbbrevs, cobbDouglasCountryRowsForParamsGraph, energyType=energyType))
    graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                     data = dataTable, 
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
                                   y=list(relation="free", #allow each axis to be different
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

createCDLatticeGraph <- function(countryAbbrev, textScaling = 1.0, keyXLoc = defaultKeyXLoc, keyYLoc = defaultKeyYLoc){
  ##############################
  # Creates a graph that plots predicted GDP as lines and GDP data as open circles.
  ##
  dataTable <- loadData("All") #Grab the raw data
  predictions  <- cobbDouglasPredictionsColumn(energyType=NA)  #Predictions from CD without energy
  predictionsQ <- cobbDouglasPredictionsColumn(energyType="Q") #Predictions from CD with Q
  predictionsX <- cobbDouglasPredictionsColumn(energyType="X") #Predictions from CD with X
  predictionsU <- cobbDouglasPredictionsColumn(energyType="U") #Predictions from CD with U
  #Now add the predictions columns to the data.
  dataTable <- cbind(dataTable, predictions, predictionsQ, predictionsX, predictionsU) 
  graphType <- "b" #b is for both line and symbol
  lineTypes <- c(0, 1, 2, 4, 1) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(0, 2, 1, 1, 1) #line widths. 0 means no line.
  colors <- c("black", "black", "red", "blue", "darkorange") #line and symbol colors
  symbols <- c(1, NA, NA, NA, NA)  #NA gives no symbol.
  # Code that deals with items that are specific to whether we want all countries or a specific country.  
  if (missing (countryAbbrev)){
    # We want a graph with panels for all countries
    yLimits <- yLimitsForGDPGraphs
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
  } else {
    # We want only a specific country
    dataTable <- subset(dataTable, Country == countryAbbrev)    
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    yLimits <- yLimitsForGDPGraphs[index:index]       # Pick limits for the country we want.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
  }
  graph <- xyplot(iGDP+predGDP+predGDPQ+predGDPX+predGDPU ~ Year | Country, data=dataTable,
                  type=graphType,
                  index.cond=indexCond, #orders the panels.
                  layout=layout, #indicates a 3x3 arrangement of panels.
                  strip = strip.custom(factor.levels=factorLevels, 
                                       bg="transparent", # Sets background transparent to match the graph itself.
                                       par.strip.text=list(cex=textScaling) # Scales text in the strip.
                  ),
                  as.table=TRUE, #indexing of panels starts in upper left and goes across rows.
                  lty = lineTypes, lwd = lineWidths, col = colors, #Controls line parameters
                  pch = symbols, col.symbol = colors, #Controls symbol parameters
                  key=list(text=list(c("Actual", "No energy", "With $q$", "With $x$", "With $u$")),
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

## <<ces functions, eval=TRUE>>=
# Set some common names for CES variables.
xNamesWithoutEnergy = c("iCapStk", "iLabor")
xNamesWithEnergy = c("iCapStk", "iLabor", "iEToFit")
tName = "iYear"
yName = "iGDP"

loadAndPrepDataForCES <- function(countryAbbrev, energyType=NA, data=loadData(countryAbbrev)){
  ######################################
  # Loads data and trims rows where necessary.
  # Also, replaces the name of the desired energy column with "iEToFit".
  # If you set energyType=NA, "iEToFit" will not appear in the column names, because
  # it is assumed that you want a CES fit without an energy term.
  # Returns a data.frame with the loaded data.
  ##
  # Trim data from China and South Africa that is missing labor information for 2011.
  data <- subset(data, !is.na(iLabor))
  if (is.na(energyType)){
    # We're done. Don't have to deal with special issues with energyType.
    return(data)
  }
  # We have an energyType argument. Do some additional checking.
  if (energyType == "U"){
    # Trim the dataset to include only those years for which U is available.
    data <- subset(data, !is.na(iU))
  }
  # We need to do the CES fit with the desired energyType.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function.
  data <- replaceColName(data, energyType, "iEToFit")
  return(data)
}

cesModelNoEnergy <- function(countryAbbrev){
  ########################
  # Returns a cesEst model (without energy) for the country specified.
  ##
  # Load the data that we need.
  dataTable <- loadAndPrepDataForCES(countryAbbrev, energyType=NA)
  control=nls.lm.control(maxiter=1000, maxfev=2000)
  modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesWithoutEnergy, tName=tName, control=control)
  return(modelCES)
}

cesModel <- function(countryAbbrev, energyType){
  ####################
  # Returns a cesEst model for the country and energyType specified.
  # energyType should be one of Q", "X", "U", or NA.
  # If energyType=NA, this method dispatches to the function cesModelNoEnergy(countryAbbrev)
  ##
  if (is.na(energyType)){
    return(cesModelNoEnergy(countryAbbrev))
  }
  # We need to include energy in the production function.
  # Load the data that we need.
  dataTable <- loadAndPrepDataForCES(countryAbbrev, energyType)
  control=nls.lm.control(maxiter=1000, maxfev=2000)
  # Decide which independent variables we want to use
  xNamesToUse <- xNamesWithEnergy    
  # Now estimate the parameters for the CES production function.
  if (countryAbbrev == "US") {
    # With Q, The unconstrained optimization leads to rho1 = -0.72 and sigma1 = 3.57, which is not 
    # economically meaningful.  So, a grid search was performed, and it confirmed that low values of 
    # rho1 yield a better fit. In fact, the lower the value of rho1, the better. So, we'll set it 
    # here at rho1=0.0, which makes the elasticity of substitution between k and l sigma1 = 1.0, 
    # giving the Cobb-Douglas form for the k l portion of the CES function. Similar results are found
    # for X and U.
    rho1 = 0.0
    # Unconstrained optimization leads to rho = 25.7 and sigma = 0.0374, indicating that (kl) and (e) are 
    # complimentary.  We'll let the estCES function solve for the best value.
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM",
                       rho1=rho1
    )
  } else if (countryAbbrev == "UK") {
    # The unconstrained optimization leads to rho1 = -2.4 and sigma1 = -0.71, which is not economically meaningful.
    # So, a grid search was performed, and it confirmed that low values of rho1 yield a better fit. In fact, the 
    # lower the value of rho1, the better. So, we'll set it here at rho1=0.0, which makes the elasticity of 
    # substitution between k and l sigma1 = 1.0, giving the Cobb-Douglas form for the k l portion of the CES function.
    # rho1 = c(0.00, 0.01, 0.02)
    rho1 = 0.0
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM",
                       rho1=rho1
    )
  } else if (countryAbbrev == "JP") {
    # The unconstrained optimization leads to rho1 = 7.99 and sigma1 = 0.11, indicating complementarity between k & l,
    # but different from the results for the US and UK (where k & l were found to be substitutes).  However, the
    # p value for rho1 is very high (0.3), indicating that rho1 is not making a significant contribution to the model. 
    # The unconstrained fit gives R^ = 0.996 and sigma = 0.103, indicating complementarity between (kl) and (e).
    # 
    # In fact, we could force rho1 = 0.0 and achieve rho = 280 and sigma = 0.00356 with R^2 = 0.988, a very small
    # decrease, indeed.  Note that both the constrained and unconstrained fits give small sigma, indicating
    # that (kl) and (e) are complimentary.
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM"
    )
  } else if (countryAbbrev == "CN"){
    # Unconstrained optimization: rho1 < -0.2290, rho = 14.1651, R^2 = 0.9984. This corresponds to sigma1 = 1.297
    # and sigma = 0.0659, indicating that (kl) and (e) are complements and k & l are substitutes.  However, none of the
    # rho or sigma values are statistically significant.
    # Now, sigma1 > 1 is not economically meaningful, so we'll set rho1 = 0 to force sigma1 = 1.0. When we do that, 
    # we obtain sigma1 = 1.0 (so perfect substitution between k and l) and sigma = 7.19 (again, not economically
    # meaningful) with R^2 = 0.9983.  Again, none of the rho or sigma values are statistically significant.
    # Furthermore, the only significant varluable is gamma = 1.0731.
    # So, for the final fit, I'll put rho1 = 0.0 and rho = 0.0. The final R^2 = 0.9983.
    rho1 = 0.0
    rho = 0.0
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM",
                       rho1=rho1,
                       rho=rho                     
    )
  } else if (countryAbbrev == "ZA"){
    # An unconstrained fit works for South Africa.  Results are sigma1 = 0.05, signalling complementarity
    # between k and l.  sigma = 0.03, indicating complementarity between (kl) and (e).
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM",
    )
  } else if (countryAbbrev == "SA"){
    # An unconstrained fit gives rho = -6.33, rho1 = 9.36 (sigma1 = 0.0965) and R^2 = 0.9892.  
    # This result is not economically meaningful.
    # Constraining rho = 0 gives rho1 = -582, again not economically meaningful.
    # So, do a grid search over meaningful regions to see what is best.
    # The result (to with +/- 0.1) is rho = 0.0 and rho1 = 5.7, with R^2 = 0.9885.
    # However, neither the the rho or nor the sigma values are statistically significant.
    rho1 = 5.7
    rho = 0.0
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM",
                       rho1 = rho1,
                       rho = rho
    )
  } else if (countryAbbrev == "IR"){
    # Iran works with an unconstrained fit.
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="LM",
    )
  } else if (countryAbbrev == "TZ"){
    # Using the PORT algorithm yields convergence and R^2 = 0.9974 with economically menaingful results.
    # However, none of the rho or sigma values are statistically significant.
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="PORT",
    )
  } else if (countryAbbrev == "ZM"){
    # ZM works with the PORT algorithm, but not the LM algorithm.
    modelCES <- cesEst(data=dataTable, yName=yName, xNames=xNamesToUse, tName=tName, control=control,
                       method="PORT",
                       rho=0.0
    )
  }
  return(modelCES)
}

cesPredictions <- function(countryAbbrev, energyType){
  #########################
  # Takes the CES fitted models and creates per-country predictions for them.
  # Returns a data.frame with the predictions.
  # If energyType=NA, the CES model without energy will be used.
  ##
  # Can't make predictions for any of CN, ZA, SA, IR, TZ, or ZM if we're interested in U
  if (!(haveDataCES(countryAbbrev, energyType))){
    # If we don't have data for this combination of countryAbbrev and energyType, 
    # return a column of NAs if the above conditions have been met.
    nRows <- 21 # All of these countries need 21 rows.
    df <- as.data.frame(matrix(NA, ncol = 1, nrow = nRows))
    colnames(df) <- "pred"
    return(df)
  }
  model <- cesModel(countryAbbrev=countryAbbrev, energyType=energyType)
  pred <- fitted(model)
  df <- data.frame(pred)
  # Pad with rows as necessary
  df <- padRows(countryAbbrev, df)
  return(df)
}

cesPredictionsColumn <- function(energyType){
  #########################
  # Takes the CES fitted models and creates a single column of predicted GDP values
  # that corresponds, row for row, with the AllData.txt file.
  # If energyType=NA is specified, the CES model without energy will be used for the predictions.
  ##
  out <- do.call("rbind", lapply(countryAbbrevs, cesPredictions, energyType=energyType))  
  if (is.na(energyType)){
    colnames(out) <- "predGDP"
  } else {
    colnames(out) <- c(paste("predGDP", energyType, sep=""))
  }
  return(out)
}

createCESLatticeGraph <- function(countryAbbrev, textScaling=1.0, keyXLoc=defaultKeyXLoc, keyYLoc=defaultKeyYLoc){
  ##############################
  # Creates a graph that plots predicted GDP as lines, one for each single factor, and historical GDP 
  # data as open circles.
  ##
  dataTable <- loadData("All") #Grab the raw data
  predictions  <- cesPredictionsColumn(energyType=NA)  #Predictions from CES without energy
  predictionsQ <- cesPredictionsColumn(energyType="Q") #Predictions from CES with Q
  predictionsX <- cesPredictionsColumn(energyType="X") #Predictions from CES with X
  predictionsU <- cesPredictionsColumn(energyType="U") #Predictions from CES with U
  #Now add the predictions columns to the data.
  dataTable <- cbind(dataTable, predictions, predictionsQ, predictionsX, predictionsU) 
  graphType <- "b" #b is for both line and symbol
  lineTypes <- c(0, 1, 2, 4, 1) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(0, 2, 1, 1, 1) #line widths. 0 means no line.
  colors <- c("black", "black", "red", "blue", "darkorange") #line and symbol colors
  symbols <- c(1, NA, NA, NA, NA)  #NA gives no symbol.
  # Code that deals with items that are specific to whether we want all countries or a specific country.  
  if (missing(countryAbbrev)){
    # We want a graph with panels for all countries
    yLimits <- yLimitsForGDPGraphs
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
  } else {
    # We want only a specific country
    dataTable <- subset(dataTable, Country == countryAbbrev)    
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    yLimits <- yLimitsForGDPGraphs[index:index]       # Pick limits for the country we want.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
  }
  graph <- xyplot(iGDP+predGDP+predGDPQ+predGDPX+predGDPU ~ Year | Country, data=dataTable,
                  type=graphType,
                  index.cond=indexCond, #orders the panels.
                  layout=layout,
                  strip = strip.custom(factor.levels=factorLevels, 
                                       bg="transparent", # Sets background transparent to match the graph itself.
                                       par.strip.text=list(cex=textScaling) # Scales text in the strip.
                  ),
                  as.table=TRUE, #indexing of panels starts in upper left and goes across rows.
                  lty = lineTypes, lwd = lineWidths, col = colors, #Controls line parameters
                  pch = symbols, col.symbol = colors, #Controls symbol parameters
                  key=list(text=list(c("Actual", "No energy", "With $q$", "With $x$", "With $u$")),
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

cesData <- function(countryAbbrev, energyType){
  #################################################
  # Calculates parameter estimates and confidence intervals
  # for the CES production function given a country and an energyType.
  #
  # countryAbbrev is a string containing the 2-letter abbreviation for the country, e.g. "US" or "CN"
  # energyType is a string, one of "Q", "X", "U", or NA. energyType=NA means we're interested
  # in a CES function without energy.
  #
  # returns a data.frame of data for the CES model.
  # First row is the +95% CI on all parameters
  # Second row contains the parameter estimates
  # Third row is the -95% CI on all parameters
  # Each column has names: gamma, lambda, delta_1, delta, rho_1, rho
  ##
  # Determine whether we want an energy term or not
  if (is.na(energyType)){
    wantEnergyTerm <- FALSE
  } else {
    wantEnergyTerm <- TRUE
  }
  #First, check to see if we want useful work (U) AND one of the countries for which we don't have data.
  if (!haveDataCES(countryAbbrev, energyType)){
    #Return a column of NAs if we don't have data available for this combination of country and energy type.
    nRows <- 3 # +95% CI, CDe, and -95% CI.
    nCols <- 6 # gamma, lambda, delta_1, delta, sigma_1, and sigma
    df <- as.data.frame(matrix(NA, ncol = nCols, nrow = nRows))
    colnames(df) <- c("gamma", "lambda", "delta_1", "delta", "sigma_1", "sigma")
    rownames(df) <- c("+95% CI", "CES", "-95% CI")
    return(df)
  }
  # We have a combination of country and energy type for which we have data.
  modelCES <- cesModel(countryAbbrev, energyType)
  summaryCES <- summary(modelCES) # Gives a summary table.
  # Calculate the degrees of freedom as N_obs - N_params
  nRows <- nrow(loadAndPrepDataForCES(countryAbbrev, energyType))
  if (wantEnergyTerm){
    # For a CES model with energy, we have 6 parameters: gamma, lambda, delta, rho, delta_1, and rho_1
    dofCES <- nRows - 6 # Gives the degrees of freedom for the model.
  } else {
    # For a CES model without energy, we have 4 parameters: gamma, lambda, delta, and rho
    dofCES <- nRows - 4 # Gives the degrees of freedom for the model.   
  }
  tvalCES <- qt(ciHalfLevel, df = dofCES)
  # Calculate confidence intervals for the parameters of the model
  ciCES <- confint(modelCES, level = ciLevel)  # Calculates confidence intervals for the CES model.
  # Get confidence intervals for the elasticities of substitution.
  #######################
  # Note: I would rather get the value of sigma and its standard error directly from the cesModel object.
  # The information is all in there. You can see it as the E terms at the bottom of the
  # print(summaryCES) results.
  #######################
  sigma.est <- deltaMethod(modelCES, "1.0 / (1.0 + rho)") # Estimates sigma and its standard error (SE)
  sigmaCI <- with(sigma.est, Estimate + c(-1.0, 1.0) * tvalCES * SE)
  if (wantEnergyTerm){
    sigma_1.est <- deltaMethod(modelCES, "1.0 / (1.0 + rho_1)") # Estimates sigma_1 and its standard error (SE)
    sigma_1CI <- with(sigma_1.est, Estimate + c(-1.0, 1.0) * tvalCES * SE)
  }
  if (!wantEnergyTerm){
    # We're not using energy in this fit.
    gamma <- coef(modelCES)["gamma"]
    lambda <- coef(modelCES)["lambda"]
    # Without energy, we get the delta coefficient that means the split between 
    # capital and labor.
    # With energy, delta_1 means the split between energy and labor.
    # To be consistent, we map the delta from the "without energy" model to variable name "delta_1"
    delta_1 <- coef(modelCES)["delta"]
    # The without energy model can be obtained with delta = 1.0. So, we set it here.
    delta <- 1.0
    # The without energy model has a sigma (elasticity of substitution between k and l) that is 
    # equivalent to the sigma_1 parameter from the "with energy" model. So, we map the names 
    # here.
    sigma_1 <- sigma.est$Estimate
    # With delta = 1.0, rho has no meaning (there is no energy term). Therefore, sigma has no meaning.
    # So, set sigma to NA
    sigma <- NA
    # Combine all estimates and their confidence intervals into data frames with intelligent row names
    estCES <- data.frame(gamma=gamma, lambda=lambda, delta_1=delta_1, delta=delta, sigma_1=sigma_1, sigma=sigma)
    row.names(estCES) <- "CES"
    # The [1] subscripts pick off the lower confidence interval
    # The size of the CI on delta is 0.0 for the CES model without energy, so we set the upper and lower bounds to 1.0
    # The CI on delta_1 shows up with the variable delta for the CES model without energy.
    lowerCES <- data.frame(gamma=ciCES["gamma","2.5 %"], lambda=ciCES["lambda", "2.5 %"],
                           delta_1=ciCES["delta", "2.5 %"], delta=1.0,
                           sigma_1=sigmaCI[1], sigma=NA)
    row.names(lowerCES) <- "-95% CI"
    # The [2] subscripts pick off the upper confidence interval
    upperCES <- data.frame(gamma=ciCES["gamma","97.5 %"], lambda=ciCES["lambda", "97.5 %"],
                           delta_1=ciCES["delta", "97.5 %"], delta=1.0,
                           sigma_1=sigmaCI[2], sigma=NA)
    row.names(upperCES) <- "+95% CI"
  } else {
    # We're using energy in this fit.
    gamma <- coef(modelCES)["gamma"]
    lambda <- coef(modelCES)["lambda"]
    delta_1 <- coef(modelCES)["delta_1"]
    delta <- coef(modelCES)["delta"]
    sigma_1 <- sigma_1.est$Estimate
    sigma <- sigma.est$Estimate
    # Combine all estimates and their confidence intervals into data frames with intelligent row names
    estCES <- data.frame(gamma=gamma, lambda=lambda, delta_1=delta_1, delta=delta, sigma_1=sigma_1, sigma=sigma)
    row.names(estCES) <- "CES"
    # The [1] subscripts pick off the lower confidence interval
    lowerCES <- data.frame(gamma=ciCES["gamma","2.5 %"], lambda=ciCES["lambda", "2.5 %"],
                           delta_1=ciCES["delta_1", "2.5 %"], delta=ciCES["delta", "2.5 %"],
                           sigma_1=sigma_1CI[1], sigma=sigmaCI[1])
    row.names(lowerCES) <- "-95% CI"
    # The [2] subscripts pick off the upper confidence interval
    upperCES <- data.frame(gamma=ciCES["gamma","97.5 %"], lambda=ciCES["lambda", "97.5 %"],
                           delta_1=ciCES["delta_1", "97.5 %"], delta=ciCES["delta", "97.5 %"],
                           sigma_1=sigma_1CI[2], sigma=sigmaCI[2])
    row.names(upperCES) <- "+95% CI"
  }
  # Now create the data for a table and return it
  dataCES <- rbind(upperCES, estCES, lowerCES)
  return(dataCES)
}

cesCountryRow <- function(countryAbbrev, energyType){
  ############
  # Creates a row for the CES parameters table for the given country (2-letter code) and energyType (Q, X, U, or NA)
  ##
  dataCES <- cesData(countryAbbrev, energyType)
  out <- cbind(dataCES["-95% CI", "gamma"], dataCES["CES", "gamma"], dataCES["+95% CI", "gamma"],
               dataCES["-95% CI", "lambda"], dataCES["CES", "lambda"], dataCES["+95% CI", "lambda"],
               dataCES["-95% CI", "delta_1"], dataCES["CES", "delta_1"], dataCES["+95% CI", "delta_1"],
               dataCES["-95% CI", "delta"], dataCES["CES", "delta"], dataCES["+95% CI", "delta"],
               dataCES["-95% CI", "sigma_1"], dataCES["CES", "sigma_1"], dataCES["+95% CI", "sigma_1"],
               dataCES["-95% CI", "sigma"], dataCES["CES", "sigma"], dataCES["+95% CI", "sigma"]
  )
  return(out)
}

cesCountryRowsForParamsGraph <- function(countryAbbrev, energyType){
  ###########################################
  # Creates a number of rows in a data.frame that contain information 
  # about the coefficients of a CES model for countryAbbrev and energyType
  # Each parameter has its own row with confidence intervals.
  # The country name is in a column. Which parameter is involved is
  # also in a column.
  # Set energyType=NA for the CES model without energy.
  # 
  # The return type is a data.frame.
  ##
  #Create six rows, one for each parameter. Each row is a data.frame so that it is plottable!
  dataCES <- cesData(countryAbbrev, energyType)
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

cesParamsTableA <- function(energyType){
  ############################
  # Aggregates the CES results for lambda, delta, and sigma into a table for the given energyType.
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType))
  colnames(dataCES) <- c(" ", "$\\gamma$",    " ", 
                         " ", "$\\lambda$",   " ",
                         " ", "$\\delta_1$",  " ",
                         " ", "$\\delta$",    " ",
                         " ", "$\\sigma_1$",  " ",
                         " ", "$\\sigma$",    " ")
  rownames(dataCES) <- countryAbbrevs
  if (is.na(energyType)){
    energyStringCaption <- "(without energy)"
    energyStringLabel <- ""
  } else {
    energyStringCaption <- paste("(with $", tolower(energyType), "$)")
    energyStringLabel <- paste("_With_", energyType, sep="")
  }
  tableCESa <- xtable(dataCES[,c(4,5,6, 10,11,12, 16,17,18)], #Picks up lambda, delta, sigma
                      caption=paste("CES model ", energyStringCaption, ". $\\lambda$, $\\delta$, and $\\sigma$ parameters for 1980-2011 (US, UK, JP), 1991-2010 (CN and ZA), and 1991-2011 (SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                      label=paste("tab:CES_Parameters_A", energyStringLabel, sep=""),
                      digits = c(1, 4,4,4, 2,2,2, 2,2,2),
                      align = "r|ccc|ccc|ccc" #Sets alignment of the numbers in the columns
  ) 
  return(tableCESa)
}

cesParamsTableB <- function(energyType){
  ############################
  # Aggregates the CES results for gamma, delta_1, and sigma_1 into a table for the given energyType.
  ##
  dataCES <- do.call("rbind", lapply(countryAbbrevs, cesCountryRow, energyType=energyType))
  colnames(dataCES) <- c(" ", "$\\gamma$",    " ", 
                         " ", "$\\lambda$",   " ",
                         " ", "$\\delta_1$",  " ",
                         " ", "$\\delta$",    " ",
                         " ", "$\\sigma_1$",  " ",
                         " ", "$\\sigma$",    " ")
  rownames(dataCES) <- countryAbbrevs
  if (is.na(energyType)){
    energyStringCaption <- "(without energy)"
    energyStringLabel <- ""
  } else {
    energyStringCaption <- paste("(with $", tolower(energyType), "$)")
    energyStringLabel <- paste("_With_", energyType, sep="")
  }
  tableCESb <- xtable(dataCES[,c(1,2,3, 7,8,9, 13,14,15)], #Picks up gamma, delta_1, and sigma_1
                      caption=paste("CES model ", energyStringCaption, ". $\\gamma$, $\\delta_1$, and $\\sigma_1$ parameters for 1980-2011 (US, UK, JP), 1991-2010 (CN and ZA), and 1991-2011 (SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
                      label=paste("tab:CES_Parameters_B", energyStringLabel, sep=""),
                      digits = c(1, 2,2,2, 2,2,2, 2,2,2),
                      align = "r|ccc|ccc|ccc"  #Sets alignment of the numbers in the columns
  )
  return(tableCESb)
}

createCESParamsGraph <- function(energyType){
  #############################
  # Creates a graph with confidence intervals for the CES model for the given energyType
  ##
  # Create a data table with the following columns:
  # country abbrev, parameter (gamma, lambda, delta_1, delta, sigma_1, sigma), -95% CI, value, +95% CI
  dataTable <- do.call("rbind", lapply(countryAbbrevs, cesCountryRowsForParamsGraph, energyType=energyType))
  graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                   data = dataTable, 
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
                   ylim = list(c(0.5, 1.5), c(-0.05, 0.05), c(0.0, 1.0),
                               c(0.0, 1.0), c(0.0, 1.0), c(0.0, 1.0)), #y axis limits
                   scales = list(cex=scaleTextSize, #controls text size on scales.
                                 tck=scaleTickSize, #controls tick mark length. < 0 for inside the graph.
                                 x=list(cex=0.75), #reduces text size so that country abbrevs are legible
                                 y=list(relation="free", #allow each axis to be different
                                        at=list(c(0.5, 1.0, 1.5), c(-0.05, 0.0, 0.05),
                                                c(0.0, 0.5, 1.0), c(0.0, 0.5, 1.0), 
                                                c(0.0, 0.5, 1.0), c(0.0, 0.5, 1.0))   #y tick marks
                                 )
                   )
  )
  return(graph)
}

printCESParamsTableA <- function(energyType){
  ############################
  # Prints a table with lambda, delta, and sigma parameters from a CES model for the given energyType. 
  ##
  print(cesParamsTableA(energyType=energyType), 
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny",
        table.placement="H")
}

printCESParamsTableB <- function(energyType){
  ############################
  # Prints a table with gamma, delta_1, and sigma_1 parameters from a CES model for the given energyType. 
  ##
  print(cesParamsTableB(energyType=energyType), 
        caption.placement="top", 
        sanitize.colnames.function = identity, 
        size="\\tiny",
        table.placement="H")
}

## <<LINEX functions, eval=TRUE>>=
linexModel <- function(countryAbbrev, energyType){
  ####################
  # Returns an nls linex model for the country and energyType specified.
  # energyType should be one of Q", "X", or "U".
  ##
  # Load the data that we need.
  dataTable <- loadData(countryAbbrev)
  # We need to do the CES fit with the desired energyType.
  # To achieve the correct fit, we'll change the name of the desired column
  # to "iEToFit" and use "iEToFit" in the nls function.
  dataTable <- replaceColName(dataTable, energyType, "iEToFit")
  a_0Guess <- 0.5
  c_tGuess <- 1.0
  if (countryAbbrev == "SA"){
    # Need adjusted guess values, becasue k and l are above GDP for SA.
    a_0Guess <- -1
    c_tGuess <- -6
    
  }
  # Runs a non-linear least squares fit to the data with constraints
  modelLINEX <- nls(iGDP ~ iEToFit * exp(a_0*(2.0 - (iLabor+iEToFit)/iCapStk) + a_0 * c_t *(iLabor/iEToFit - 1.0)), 
                    data=dataTable,
                    start=list(a_0=a_0Guess, c_t=c_tGuess),
                    control=nls.control(maxiter=50, tol=1e-06, minFactor=1/1024, printEval=FALSE, warnOnly=TRUE),
                    #                   algorithm = "port",
                    #                   lower = list(a_0=-INF, c_t=-INF),
                    #                   upper = list(a_0= INF, c_t= INF)
  )
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

createLINEXLatticeGraph <- function(countryAbbrev, textScaling = 1.0, keyXLoc = defaultKeyXLoc, keyYLoc = defaultKeyYLoc){
  ##############################
  # Creates a graph that plots predicted GDP as lines, one for each single factor, and historical GDP 
  # data as open circles.
  ##
  dataTable <- loadData("All") #Grab the raw data
  predictionsQ <- linexPredictionsColumn("Q") #Predictions from LINEX with Q
  predictionsX <- linexPredictionsColumn("X") #Predictions from LINEX with X
  predictionsU <- linexPredictionsColumn("U") #Predictions from LINEX with U
  #Now add the predictions columns to the data.
  dataTable <- cbind(dataTable, predictionsQ, predictionsX, predictionsU) 
  graphType <- "b" #b is for both line and symbol
  lineTypes <- c(0, 2, 4, 1) #line types. See http://en.wikibooks.org/wiki/R_Programming/Graphics
  lineWidths <- c(0, 1, 1, 1) #line widths. 0 means no line.
  colors <- c("black", "red", "blue", "darkorange") #line and symbol colors
  symbols <- c(1, NA, NA, NA)  #NA gives no symbol.
  # Code that deals with items that are specific to whether we want all countries or a specific country.  
  if (missing (countryAbbrev)){
    # We want a graph with panels for all countries
    yLimits <- yLimitsForGDPGraphs
    factorLevels <- countryNamesAlph # We want all countries shown
    indexCond <- list(countryOrderForGraphs) # We want all countries in this order
    layout <- ninePanelLayoutSpec # Show all countries
  } else {
    # We want only a specific country
    dataTable <- subset(dataTable, Country == countryAbbrev)    
    # Select the correct y limits
    index <- which(countryAbbrevsAlph %in% countryAbbrev)
    # The following lines use [index:index] as a convenient way of subsetting.
    # This has the added benefit of maintaining the correct classes for things.
    yLimits <- yLimitsForGDPGraphs[index:index]       # Pick limits for the country we want.
    factorLevels <- countryNamesAlph[index:index]     # Only show the country we have chosen.
    indexCond <- list(c(1))                           # We want only one country.
    layout <- onePanelLayoutSpec                      # We want only one panel in the graph.
  }  
  graph <- xyplot(iGDP+predGDPQ+predGDPX+predGDPU ~ Year | Country, data=dataTable,
                  type=graphType,
                  index.cond=indexCond, #orders the panels.
                  layout=layout,
                  strip = strip.custom(factor.levels=factorLevels, 
                                       bg="transparent", # Sets background transparent to match the graph itself.
                                       par.strip.text=list(cex=textScaling) # Scales text in the strip.
                  ),
                  as.table=TRUE, #indexing of panels starts in upper left and goes across rows.
                  lty = lineTypes, lwd = lineWidths, col = colors, #Controls line parameters
                  pch = symbols, col.symbol = colors, #Controls symbol parameters
                  key=list(text=list(c("Actual", "With $q$", "With $x$", "With $u$")),
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
  # We have a combination of country and energy type for which we have data.
  modelLINEX <- linexModel(countryAbbrev, energyType)
  summaryLINEX <- summary(modelLINEX) # Gives the nls summary table.
  dofLINEX <- summaryLINEX$df[2] # Gives the degrees of freedom for the model.
  tvalLINEX <- qt(ciHalfLevel, df = dofLINEX)
  ciLINEX <- confint(modelLINEX, level = ciLevel)  # Calculates confidence intervals for the LINEX model.
  # Report results with SE
  a_0 <- as.numeric(coef(modelLINEX)["a_0"])
  c_t <- as.numeric(coef(modelLINEX)["c_t"])
  # Combine all estimates and their confidence intervals into data frames with intelligent row names
  estLINEX <- data.frame(a_0 = a_0, c_t = c_t)
  row.names(estLINEX) <- "LINEX"
  # The [1] subscripts pick off the lower confidence interval
  lowerLINEX <- data.frame(a_0=ciLINEX["a_0","2.5%"], c_t=ciLINEX["c_t", "2.5%"])
  row.names(lowerLINEX) <- "-95% CI"
  # The [2] subscripts pick off the upper confidence interval
  upperLINEX <- data.frame(a_0=ciLINEX["a_0","97.5%"], c_t=ciLINEX["c_t", "97.5%"])
  row.names(upperLINEX) <- "+95% CI"
  # Now create the data for a table and return it
  dataLINEX <- rbind(upperLINEX, estLINEX, lowerLINEX)
  return(dataLINEX)
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
  dataTable <- do.call("rbind", lapply(countryAbbrevs, linexCountryRowsForParamsGraph, energyType=energyType))
  graph <- segplot(country ~ upperCI + lowerCI | parameter, 
                   data = dataTable, 
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
                                 y=list(relation="free", #allow each axis to be different
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
                       caption=paste("LINEX model (with $", tolower(energyType), "$) parameters for 1980-2011 (US, UK, JP), 1991-2010 (CN and ZA), and 1991-2011 (SA, IR, TZ, and ZM). (Parameter estimates beneath symbol. 95\\% confidence interval bounds to left and right.)", sep=""), 
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
  sfKModels <- lapply(countryAbbrevs, singleFactorModel, factor="K")
  aicSFk <- data.frame(lapply(sfKModels, AIC))
  rownames(aicSFk) <- "SF$k$"
  # Single-factor with L
  sfLModels <- lapply(countryAbbrevs, singleFactorModel, factor="L")
  aicSFl <- data.frame(lapply(sfLModels, AIC))
  rownames(aicSFl) <- "SF$l$"
  # Single-factor with Q
  sfQModels <- lapply(countryAbbrevs, singleFactorModel, factor="Q")
  aicSFq <- data.frame(lapply(sfQModels, AIC))
  rownames(aicSFq) <- "SF$q$"
  # Single-factor with X
  sfXModels <- lapply(countryAbbrevs, singleFactorModel, factor="X")
  aicSFx <- data.frame(lapply(sfXModels, AIC))
  rownames(aicSFx) <- "SF$x$"
  # Single-factor with U
  aicSFu <- cbind(US=AIC(singleFactorModel(countryAbbrev="US", factor="U")), 
                  UK=AIC(singleFactorModel(countryAbbrev="UK", factor="U")), 
                  JP=AIC(singleFactorModel(countryAbbrev="JP", factor="U")),
                  CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicSFu) <- "SF$u$"
  ######################
  # Cobb-Douglas models
  ######################
  # Cobb-Douglas without energy
  cdModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType=NA)
  aicCD <- data.frame(lapply(cdModels, AIC))
  rownames(aicCD) <- "CD"
  # Cobb-Douglas with Q
  cdQModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="Q")
  aicCDq <- data.frame(lapply(cdQModels, AIC))
  rownames(aicCDq) <- "CD$q$"
  # Cobb-Douglas with X
  cdXModels <- lapply(countryAbbrevs, cobbDouglasModel, energyType="X")
  aicCDx <- data.frame(lapply(cdXModels, AIC))
  rownames(aicCDx) <- "CD$x$"
  # Cobb-Douglas with U
  aicCDu <- cbind(US=AIC(cobbDouglasModel("US", "U")), 
                  UK=AIC(cobbDouglasModel("UK", "U")), 
                  JP=AIC(cobbDouglasModel("JP", "U")),
                  CN=NA, ZA=NA, SA=NA, IR=NA, TZ=NA, ZM=NA) #No U data for these countries.
  rownames(aicCDu) <- "CD$u$"
  ######################
  # CES models
  ######################
  # At present, this AIC for CES code is not working. Perhaps because the CES model from the cesEst function
  # in the micEcon package does not provide its data in the correct format for the AIC function?
  # --Matthew Kuperus Heun, 10 April 2013.
  #   # CES with Q
  #   cesQModels <- lapply(countryAbbrevs, cesModel, energyType="Q")
  #   aicCESq <- data.frame(lapply(cesQModels, AIC))
  #   rownames(aicCESq) <- "CES$q$"
  #   # CES with X
  #   cesXModels <- lapply(countryAbbrevs, cesModel, energyType="X")
  #   aicDEXx <- data.frame(lapply(cesXModels, AIC))
  #   rownames(aicCESx) <- "CES$x$"
  #   # CES with U
  #   aicCESu <- cbind(US=AIC(cesModel("US", "U")), 
  #                   UK=AIC(cesModel("UK", "U")), 
  #                   JP=AIC(cesModel("JP", "U")),
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
  aicLINEXu <- cbind(US=AIC(linexModel("US", "U")), 
                     UK=AIC(linexModel("UK", "U")), 
                     JP=AIC(linexModel("JP", "U")),
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
CIvsParamDF <- function(model, param, energyType=NA, factor=NA){
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
  else if (model == "CES"){data <- cesParamsTableDF(energyType=NA)}
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
  } else if (is.na(energyType)){
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

CIvsParamPlot <- function(model, param, energyType=NA, factor=NA, textScaling=1.0){
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
    model <- cesModelNoEnergy(countryAbbrev)
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
  data <- replaceColName(dataTable=data, factor=energyType, newName="iEToFit")
  return(data)
}

createPartialResidualPlot <- function(modelType, energyType, countryAbbrev=NA, textScaling=1.0){
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
    # Lattice graph will all countries is desired.
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
