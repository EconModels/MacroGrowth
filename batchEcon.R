#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales

# Example usage:

# Move into the top level directory that contains this file
# for US, thermal energy, single factor model, 10 resamples, clobbering previous results, and wild resampling:
# ./batchEcon.R -c US -f iQ -m sf -n 10 -C -M wild -R data_resample

# for US, exergy, linex model, 10 resamples, clobbering previous results, and wild resampling:
# ./batchEcon.R -c US -e iX -m linex -n 10 -C -M wild -R data_resample

# for US, all energy types, fast models (everything except CES with energy), 
#     10 resamples, clobber previous results, wild resampling:
# ./batchEcon.R -c US -e all -f all -m fast -n 2 -C -M wild -R data_resample

# for all countries, all energy types, all models, 1000 resamples, clobber previous results, wild resampling:
# ./batchEcon.R -c all -e all -m all -n 1000 -C -M wild -R data_resample

require(EconModels)
require(EconData)
suppressPackageStartupMessages(library("optparse"))


baseResample <- file.path("data_resample")
modelTypes <- c('sf', 'cd', 'cde', 'ces', 'cese-(kl)e', 'cese-(le)k', 'cese-(ek)l', 'linex')

option_list <- list(
  make_option(c("-c", "--country"), default="all",
              help="country [default=%default]"),
  make_option(c("-e", "--energy"), default="iQ",
              help="energy [default=%default]"),
  make_option(c("-f", "--factor"), default="iK",
              help="factor [default=%default]"),
  make_option(c("-m", "--model"), default="all",
              help="model [default=%default]"),
  make_option(c("-n", "--resamples"), default=10L, type="integer",
              help="number of resamples [default=%default]"),
  make_option(c("-F", "--constraintFree"), default=FALSE, action="store_true",
              help="disregard constraints on model parameters [default=%default]"),
  make_option(c("-C", "--clobber"), default=FALSE, action="store_true",
              help="Clobber all previous files [default=%default]"),
  make_option(c("-d", "--debug"), default=FALSE, action="store_true",
              help="runs without executing the resampling [default=%default]"),
  make_option(c("-M", "--method"), default="wild", 
              help="resampling method [default=%default]"),
#  make_option(c("-H", "--baseHistorical"), # default="data", 
#              help="relative path to directory for historical data"),
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
      ModelInfos[[length(ModelInfos)+1]] <- list(modelType=model,
                                                 fun = "sfModel",
                                                 formulaStr = paste("iGDP ~", factor, "+ iYear"),
                                                 dots = list(constrained= ! opts$constraintFree))
    }
  } else if (model == "cd") {
    ModelInfos[[length(ModelInfos)+1]] <- list(modelType=model,
                                               fun = "cdModel",
                                               formulaStr = "iGDP ~ iK + iL + iYear",
                                               dots = list(constrained= ! opts$constraintFree))
  } else if (model == "cde"){
    # Build a formula for each energy type
    formulas <- c()
    for(energy in opts$energy){
      formulas[length(formulas)+1] <- paste("iGDP ~ iK + iL +", energy, "+ iYear")
    }
    ModelInfos[[length(ModelInfos)+1]] <- list(modelType=model,
                                               fun = "cdModel",
                                               formulaStr = formulas,
                                               dots = list(constrained= ! opts$constraintFree))
  } else if (model == "ces"){
    # Want a CES model without energy
    ModelInfos[[length(ModelInfos)+1]] <- list(modelType=model,
                                               fun = "cesModel",
                                               formulaStr = "iGDP ~ iK + iL + iYear",
                                               dots = list(nest=1:2))
  } else if (grepl(pattern="cese", x=model, fixed=TRUE)){
    # Want a CES model with energy
    # Build a formula for each energy type
    formulas <- c()
    for(energy in opts$energy){
      formulas[length(formulas)+1] <- paste("iGDP ~ iK + iL +", energy, "+ iYear")
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
    ModelInfos[[length(ModelInfos)+1]] <- list(modelType=model,
                                               fun = "cesModel",
                                               formulaStr = formulas,
                                               dots = dots)
  } else if (model == "linex"){
    # Build a formula for each energy type
    formulas <- c()
    for(energy in opts$energy){
      formulas[length(formulas)+1] <- paste("iGDP ~ iK + iL +", energy, "+ iYear")
    }
    ModelInfos[[length(ModelInfos)+1]] <- list(modelType=model,
                                               fun = "linexModel",
                                               formulaStr = formulas,
                                               dots = list())
  } else {
    stop(paste("Unknown model type", model, "in batchEcon.R."))
  }
}

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

# load historical data
# All <- read.table(file="data/AllData.txt", header=TRUE)
All <- Econ2011

registerDoParallel()
# print(ModelInfos)

for (m in ModelInfos) {
  for (f in m$formulaStr) {
    # Add parallelization on countrires here!
    foreach(country=opts$country, .errorhandling="pass", .init=c(), .combine=c) %dopar% {
      tryCatch({
        cdata <- subset(All, Country==country)
        formula <- eval( parse( text=f ) )
        id <- fittingID(fun=m$fun, countryAbbrev=country, formula=f, nest=m$dots$nest, n=opts$resamples)
        cat(id); cat("\n")
        if (! opts$debug) {
          oModel <- do.call( m$fun, c( list( formula, data=cdata ), m$dots) )
          if (m$fun == "cesModel") {
            # Want to set prevModel to oModel in the call to cesModel. It will be passed in the ... argument.  
            rFits <- do.call(resampledFits, c( list( oModel, "wild", n=opts$resamples, id=id, prevModel=oModel), m$dots ) )
          } else {
            # No need for a prevModel argument, because none of the model functions (except cesModel) use it.
            rFits <- do.call(resampledFits, c( list( oModel, "wild", n=opts$resamples, id=id), m$dots) ) 
          }
          rModels <- rFits[["models"]]
          rCoeffs <- rFits[["coeffs"]]
          # Save the data to disk in the appropriate places with the appropriate file names.
          # First step, get the factor and energy type.
          terms <- all.vars( terms(formula) )
          if (m$fun == "sfModel"){
            energyType <- NA
            # We're dealing with factors. Find the factor we're using.
            matches <- na.omit(match(x=terms, table=factors))
            if (length(matches) <= 0){
              factor <- NA
            } else {
              factor <- factors[[matches]]
            }
          } else {
            factor <- NA
            # We're dealing with energy types. Find the energy type we're using.
            matches <- na.omit(match(x=terms, table=energyTypes))
            if (length(matches) <= 0){
              energyType <- NA
            } else {
              energyType <- energyTypes[[matches]]
            }
          }
          # Get the paths for the coefficients and models files.
          coeffsPath <- resampleFilePath(prefix="coeffs", fun=m$fun, countryAbbrev=country, formula=f, 
                                         nest=m$dots$nest, baseResample=baseResample)
          modelsPath <- resampleFilePath(prefix="models", fun=m$fun, countryAbbrev=country, formula=f, 
                                         nest=m$dots$nest, baseResample=baseResample)
          # Ensure that the directories exist.
          dir.create(dirname(coeffsPath), recursive=TRUE, showWarnings=FALSE)
          dir.create(dirname(modelsPath), recursive=TRUE, showWarnings=FALSE)
          # Now save the files.
          saveRDS(rCoeffs, file=coeffsPath)
          saveRDS(rModels, file=modelsPath)
        }
      }, 
      error=function(e) {
        cat(paste0("  *** Skipping ", id, "\n"))
        print(e)
        # print(list( m$fun, c( list( formula, data=cdata ), m$dots) ) )
      }
      )
    }
  }
}

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')
