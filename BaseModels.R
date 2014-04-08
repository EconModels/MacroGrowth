# Script to generate all base models (i.e., fits to historical data)

require(EconModels2)
require(foreach)
require(doParallel)

#' Load historical economic data
#' 
#' This function returns a data frame containing historical economic data
#' 
#' @param countryAbbrev the country/countries for which you want to load data
#' @param baseHistorical the relative path of the directory containing the historical data.
#' @return a data frame containing historical economic data
#' @export
loadData <- function(countryAbbrev, baseHistorical){
  # Read the data file as a table with a header.  
  path <- file.path(baseHistorical, "AllData.txt")
  data <- read.table(file=path, header=TRUE)
  
  if (missing(countryAbbrev)){
    return(data)
  }
  return(subset(data, Country %in% countryAbbrev))
}

# CES models take a long time. Set FALSE to skip over CES models
doCES <- FALSE

# 
# Load our data
#
data <- loadData(baseHistorical="data") # grabs the historical data
countryAbbrevs <- as.character(unique(data$Country)) # tells which countries we have
names(countryAbbrevs) <- countryAbbrevs # set the names to the abbreviations
dataU <- subset(data, subset=!is.na(data$iU)) # data where we have U available
countryAbbrevsU <- as.character(unique(dataU$Country))
names(countryAbbrevsU) <- countryAbbrevsU

#
# Single Factor models
#
modelsSFK <- lapply(countryAbbrevs, function(countryAbbrev){
  singleFactorModel(formula=iGDP ~ iCapStk + iYear, data=subset(data, Country==countryAbbrev))
})
modelsSFL <- lapply(countryAbbrevs, function(countryAbbrev){
  singleFactorModel(formula=iGDP ~ iLabor + iYear, data=subset(data, Country==countryAbbrev))
})
modelsSFQ <- lapply(countryAbbrevs, function(countryAbbrev){
  singleFactorModel(formula=iGDP ~ iQ + iYear, data=subset(data, Country==countryAbbrev))
})
modelsSFX <- lapply(countryAbbrevs, function(countryAbbrev){
  singleFactorModel(formula=iGDP ~ iX + iYear, data=subset(data, Country==countryAbbrev))
})
modelsSFU <- lapply(countryAbbrevsU, function(countryAbbrev){
  singleFactorModel(formula=iGDP ~ iU + iYear, data=subset(data, Country==countryAbbrev))
})

#
# Cobb-Douglas models
#
modelsCD <- lapply(countryAbbrevs, function(countryAbbrev){
  cdModel(formula=iGDP ~ iCapStk + iLabor + iYear, data=subset(data, Country==countryAbbrev))
})
modelsCDQ <- lapply(countryAbbrevs, function(countryAbbrev){
  cdModel(formula=iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(data, Country==countryAbbrev))
})
modelsCDX <- lapply(countryAbbrevs, function(countryAbbrev){
  cdModel(formula=iGDP ~ iCapStk + iLabor + iX + iYear, data=subset(data, Country==countryAbbrev))
})
modelsCDU <- lapply(countryAbbrevsU, function(countryAbbrev){
  cdModel(formula=iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(data, Country==countryAbbrev))
})

#
# CES models
#
if (doCES == TRUE){
  # Without energy
  modelsCESKL <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=iGDP ~ iCapStk + iLabor + iYear, data=subset(data, Country==countryAbbrev))
  })
  
  # With Q
  formQ <- iGDP ~ iCapStk + iLabor + iQ + iYear
  modelsCESKLQ <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=formQ, nest=c(1,2,3), data=subset(data, Country==countryAbbrev))
  })
  modelsCESLQK <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=formQ, nest=c(2,3,1), data=subset(data, Country==countryAbbrev))
  })
  modelsCESKQL <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=formQ, nest=c(1,3,2), data=subset(data, Country==countryAbbrev))
  })
  
  # With X
  formX <- iGDP ~ iCapStk + iLabor + iX + iYear
  modelsCESKLX <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=formX, nest=c(1,2,3), data=subset(data, Country==countryAbbrev))
  })
  modelsCESLXK <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=formX, nest=c(2,3,1), data=subset(data, Country==countryAbbrev))
  })
  modelsCESKXL <- lapply(countryAbbrevs, function(countryAbbrev){
    cesModel(formula=formX, nest=c(3,1,2), data=subset(data, Country==countryAbbrev))
  })
  
  # With U
  formU <- iGDP ~ iCapStk + iLabor + iU + iYear
  modelsCESKLU <- lapply(countryAbbrevsU, function(countryAbbrev){
    cesModel(formula=formU, nest=c(1,2,3), data=subset(data, Country==countryAbbrev))
  })
  modelsCESLUK <- lapply(countryAbbrevsU, function(countryAbbrev){
    cesModel(formula=formU, nest=c(2,3,1), data=subset(data, Country==countryAbbrev))
  })
  modelsCESKUL <- lapply(countryAbbrevsU, function(countryAbbrev){
    cesModel(formula=formU, nest=c(3,1,2), data=subset(data, Country==countryAbbrev))
  })
}

#
# Linex models
#
modelsLinexQ <- lapply(countryAbbrevs, function(countryAbbrev){
  linexModel(formula=iGDP ~ iCapStk + iLabor + iQ + iYear, data=subset(data, Country==countryAbbrev))
})
modelsLinexX <- lapply(countryAbbrevs, function(countryAbbrev){
  linexModel(formula=iGDP ~ iCapStk + iLabor + iX + iYear, data=subset(data, Country==countryAbbrev))
})
modelsLinexU <- lapply(countryAbbrevsU, function(countryAbbrev){
  linexModel(formula=iGDP ~ iCapStk + iLabor + iU + iYear, data=subset(data, Country==countryAbbrev))
})