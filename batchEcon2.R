#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales

# Example usage:

# Move into the top level directory that contains this file
# for US, thermal energy, single factor model, 10 resamples, clobbering previous results, and wild resampling:
# ./batchEcon.R -c US -f Q -m sf -n 10 -C -M wild -H data -R data_resample
# for US, exergy, linex model, 10 resamples, clobbering previous results, and wild resampling:
# ./batchEcon.R -c US -e X -m linex -n 10 -C -M wild -H data -R data_resample
# for all countries, all energy types, all models, 1000 resamples, clobber previous results, wild resampling:
# ./batchEcon.R -c all -e all -m all -n 1000 -C -M wild -H data -R data_resample


nestString <- function( formula, nest ) {
  return(nest)
  xNames <- all.vars( terms(formula) )
  xNames <- tail(xNames, -1)
  xNames <- tail(xNames, -1)
  paste0(
    "(", 
    paste(head(xNames,2), collapse=" + "),  
    ") + (", 
    paste(tail(xNames, -2), collapse=" + "),
    ")"
  )
}
# print(sort(.packages()))
require(EconModels2)
suppressPackageStartupMessages(library("optparse"))

# Some specifics for our data sets and analyses
countryAbbrevs <- c(US="US", UK="UK", JP="JP", CN="CN", ZA="ZA", SA="SA", IR="IR", TZ="TZ", ZM="ZM")
modelTypes <- c('sf', 'cd', 'cde', 'ces', 'cese-(kl)e', 'cese-(le)k', 'cese-(ek)l', 'linex')
energyTypes <- c(Q="iQ", X="iX", U="iU") 
factors <- c(K="iCapStk", L="iLabor", energyTypes) 


#' Path to resample data file
#' 
#' This function returns a string representing the relative file path for the resample data being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
getPathForResampleData <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
  return(doGetPath(prefix="resampleData", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, baseResample=baseResample))
}

#' Path to resample models file
#' 
#' This function returns a string representing the relative file path for the resample models being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
getPathForResampleModels <- function(modelType, countryAbbrev, energyType="none", factor="K", baseResample){
  return(doGetPath(prefix="resampleModels", modelType=modelType, countryAbbrev=countryAbbrev, 
                   energyType=energyType, factor=factor, baseResample=baseResample))
}

#' Generates paths to resample coefficients or models in String format
#' 
#' This function returns a string representing the relative file path for the resample coefficients or models being requested.
#' 
#' @param prefix the prefix for the model file names. 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param energyType one of \code{"Q"} (for thermal energy), \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param factor one of \code{"K"} (for capital stock), \code{"L"} (for labor), \code{"Q"} (for thermal energy), 
#' \code{"X"} (for exergy), or \code{"U"} (for useful work).
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path of the file containing the data for the requested resample data.
doGetPath <- function(prefix, modelType, countryAbbrev, energyType="Q", factor="K", baseResample){
  if (energyType == "none"){
    energyType="NA"
  }
  folder <- getFolderForResampleData(modelType=modelType, countryAbbrev=countryAbbrev, baseResample=baseResample)   
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
                     stop(paste("Unknown modelType", modelType, "in doGetPath."))
  )
  path <- file.path(folder, filename)
  return(path)
}

#' Directory for resample data
#' 
#' This function returns a string representing the relative directory path containing the resample data being requested.
#' 
#' @param modelType one of \code{"sf"}, \code{"cd"}, \code{"cde"}, \code{"cese-(kl)"}, 
#' \code{"cese-(kl)e"}, \code{"cese-(le)k"}, \code{"cese-(kl)e"}, or \code{"linex"}.
#' @param countryAbbrev, a character string naming the country, 
#' if you want to use original data.
#' @param baseResample the relative path of the top-level directory containing the resample data.
#' @return the relative path to the directory containing the data for the requested resample data.
getFolderForResampleData <- function(modelType=modelTypes, countryAbbrev=countryAbbrevs, baseResample){
  dr <- baseResample
#   countryAbbrev <- match.arg(countryAbbrev)
  folder <- switch(modelType,
                   "cde"       = file.path(dr, "cd",      countryAbbrev),
                   "cese-(kl)" = file.path(dr, "ces",     countryAbbrev),
                   file.path(dr, modelType, countryAbbrev)
  )
  return(folder)
}
  
# 
# Start the script
# 

option_list <- list(
  make_option(c("-c", "--country"), default="all",
              help="country [default=%default]"),
  make_option(c("-e", "--energy"), default="iQ",
              help="energy [default=%default]"),
  make_option(c("-f", "--factor"), default="iCapStk",
              help="factor [default=%default]"),
  make_option(c("-m", "--model"), default="all",
              help="model [default=%default]"),
  make_option(c("-n", "--resamples"), default=10L, type="integer",
              help="number of resamples [default=%default]"),
  make_option(c("-C", "--clobber"), default=FALSE, action="store_true",
              help="Clobber all previous files [default=%default]"),
  make_option(c("-d", "--debug"), default=FALSE, action="store_true",
              help="runs without executing the resampling [default=%default]"),
  make_option(c("-M", "--method"), default="wild", 
              help="resampling method [default=%default]"),
  make_option(c("-H", "--baseHistorical"), # default="data", 
              help="relative path to directory for historical data"),
  make_option(c("-R", "--baseResample"), # default="data_resample", 
              help="relative path to directory for resample data")
)

opts <- parse_args(OptionParser(option_list=option_list))
# print(opts)

if(opts$model == "all") {
  opts$model <- modelTypes
} else if (opts$model == "fast") {
  opts$model <- setdiff(modelTypes, c("cese-(kl)e", "cese-(le)k", "cese-(ek)l"))
  } else {
  opts$model <- strsplit(opts$model,",")[[1]]
#   opts$model <- match.arg(opts$model,choices=modelTypes,several.ok=TRUE)
}

if(opts$country== "all") {
  opts$country <- countryAbbrevs
} else {
  opts$country <- strsplit(opts$country,",")[[1]]
#   opts$country <- match.arg(opts$country,choices=countryAbbrevs,several.ok=TRUE)
}

if(opts$energy== "all") {
  opts$energy <- energyTypes
} else {
  opts$energy <- strsplit(opts$energy,",")[[1]]
#   opts$energy <- match.arg(opts$energy,choices=energyTypes,several.ok=TRUE)
}

if(opts$factor== "all") {
  opts$factor <- factors
} else {
  opts$factor <- strsplit(opts$factor,",")[[1]]
#   opts$factor <- match.arg(opts$factor,choices=factors,several.ok=TRUE)
}

print(str(opts))

#
# Convert the options to a ModelInfos object. ModelInfos contains the 
# recipe for which fits to execute.
#

ModelInfos <- list()

for(model in opts$model){
  if (model == "sf"){
    # Build a formula for each factor
    formulas <- c()
    for(factor in opts$factor){
      formulas[length(formulas)+1] <- paste("iGDP ~", factor, "+ iYear")
    }
    ModelInfos[[length(ModelInfos)+1]] <- list(formulaStr = formulas,
                                               fun = "singleFactorModel",
                                               dots = list())
  } else if (model == "cd") {
    ModelInfos[[length(ModelInfos)+1]] <- list(formulaStr = "iGDP ~ iCapStk + iLabor + iYear",
                                               fun = "cobbDouglasModel",
                                               dots = list())
  } else if (model == "cde"){
    # Build a formula for each energy type
    formulas <- c()
    for(energy in opts$energy){
      formulas[length(formulas)+1] <- paste("iGDP ~ iCapStk + iLabor +", energy, "+ iYear")
    }
    ModelInfos[[length(ModelInfos)+1]] <- list(formulaStr = formulas,
                                               fun = "cobbDouglasModel",
                                               dots = list())
  } else if (model == "ces"){
    # Want a CES model without energy
    ModelInfos[[length(ModelInfos)+1]] <- list(formulaStr = "iGDP ~ iCapStk + iLabor + iYear",
                                               fun = "cesModel",
                                               dots = list(nest=1:2))
  } else if (grepl(pattern="cese", x=model, fixed=TRUE)){
    # Want a CES model with energy
    # Build a formula for each energy type
    formulas <- c()
    for(energy in opts$energy){
      formulas[length(formulas)+1] <- paste("iGDP ~ iCapStk + iLabor +", energy, "+ iYear")
    }
    # Figure out the desired nesting for the factors of production
    if (grepl(pattern="(kl)e", x=model, fixed=TRUE)){
      dots <- list(nest=1:3)
    } else if (grepl(pattern="(le)k", x=model, fixed=TRUE)){
      dots <- list(nest=c(2,3,1))
    } else if (grepl(pattern="(ek)l", x=model, fixed=TRUE)){
      dots <- list(nest=c(3,1,2))
    } else {
      stop(paste("Unknown nest in formula", formula, "in batchEcon.R"))
    }
    ModelInfos[[length(ModelInfos)+1]] <- list(formulaStr = formulas,
                                               fun = "cesModel",
                                               dots = dots)
  } else if (model == "linex"){
    # Build a formula for each energy type
    formulas <- c()
    for(energy in opts$energy){
      formulas[length(formulas)+1] <- paste("iGDP ~ iCapStk + iLabor +", energy, "+ iYear")
    }
    ModelInfos[[length(ModelInfos)+1]] <- list(formulaStr = formulas,
                                               fun = "linexModel",
                                               dots = list())
  } else {
    stop(paste("Unknown model type", model, "in batchEcon.R."))
  }
}

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')


#genResampleData(modelType = "cese-(kl)e", countryAbbrev="CN", energyType="Q", n=2, method="wild", clobber=TRUE)
#cat('half way')

# load historical data
All <- read.table(file="data/AllData.txt", header=TRUE)

registerDoParallel()

for (m in ModelInfos) {
  for (f in m$formulaStr) {
    # Add parallelization on countrires here!
    for (country in opts$country) {
      cdata <- subset(All, Country==country)
      formula <- eval( parse( text=f ) )
      cat ( paste(country, f, m$fun, m$dots, nestString(eval(parse(text=m$f)), m$n), 
                  sep=" : ") )
      cat ("\n")
      
      tryCatch({
        id <- paste(country, f, m$fun, sep=":")
        if (! opts$debug) {
          oModel <- do.call( m$fun, c( list( formula, data=cdata ), m$dots) )
          if (m$fun == "cesModel") {
            # Want to set prevModel to oModel in the call to cesModel. It will be passed in the ... argument.
            rFits <- resampledFits( oModel, "wild", n=opts$n, id=id, prevModel=oModel )
          } else {
            # No need for a prevModel argument, because none of the model functions (except cesModel) use it.
            rFits <- resampledFits( oModel, "wild", n=opts$n, id=id )
          }
          rFits <- resampledFits( oModel, "wild", n=opts$n, id=id )
          rModels[[length(rModels) + 1]] <- rFits[["models"]]
          coefs[[length(coefs) + 1]] <- rFits[["coeffs"]]
        }
      }, 
      error=function(e) {
        cat(paste0("  *** Skipping ", id, "\n"))
        print(e)
      }
      )
      # Now save the data to disk in the appropriate places with the appropriate file names.
      
    }
  }
}

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')
