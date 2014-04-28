#' @export
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

#' @export
padRows <- function(countryAbbrev, df, baseHistorical){
  #####################
  # This function adds NA rows to the bottom of data.frame df to ensure that it has the 
  # the number of observations (rows) as the country data set for countryAbbrev
  # This function is a convenience wrapper function that simply loads 
  # the data.frame for countryAbbrev and then calls doPadRows
  # returns a modified version of df that includes the padded rows filled with "NA".
  ##
  countryData <- loadData(countryAbbrev=countryAbbrev, baseHistorical=baseHistorical)
  return(doPadRows(dataToBePadded=df, dataThatSuppliesRowCount=countryData))
}

#' @export
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
    colName <- "iK"
  } else if (factor == "L"){
    colName <- "iL"
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

#' @export
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
