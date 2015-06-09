#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales
#
# This script fits all of the base models for each combination of 
# source, country, model, factor, energy type, and nesting.
# 
# This script should be run from the top directory (Econ-Growth-R-Analysis) with the command
# "Scripts/OrigModels.R -S <Sources>"
#

# Results are stored in the file "outputDir/<Source>/Models.Rdata"
# To load this data back in, do 
# 
# oModels <- readRDS(file="outputDir/<Source>/Models.Rdata")
#
# To extract a model, do, for example
# mod <- oModels$REXS$US$cd$`iK+iL+iQ`

suppressPackageStartupMessages( require(EconModels) )
suppressPackageStartupMessages( require(EconData) )
suppressPackageStartupMessages( require(optparse) )
suppressPackageStartupMessages( require(parallel) )

nestStr <- function(nest){
  paste(nest, collapse="")
}

defaultOutputDir <- "data_resample"
filename_oModels <- "oModels.rds"
filename_models <- "models.rds"

# Provide a way to specify data sources in a comma-separated list
option_list <- list(
  make_option(c("-S", "--Sources"), default="Calvin",
              help="Comma-separated list of sources of data [default=%default]"),
  make_option(c("-f", "--fast"), default=FALSE, action="store_true",
              help="Runs fast models (skips CES with energy) [default=%default]"),
  make_option(c("-v", "--veryfast"), default=FALSE, action="store_true",
              help="Runs very fast models (skips CES altogether). Overrides -f. [default=%default]"),
  make_option(c("-V", "--verbose"), default=FALSE, action="store_true",
              help="More verbose output [default=%default]"),
  make_option(c("-O", "--outputDir"), default=defaultOutputDir,
              help=paste0("relative path to directory ",
                          "in which original models are saved ",
                          "(one level above the <Source> directories) [default=",
                          defaultOutputDir)),
  make_option(c("-M", "--maxRuns"), default=Inf, type="double", 
              help="Maximum number of models to fit [default=%default]"),
  make_option(c("-d", "--debug"), default=FALSE, action="store_true",
              help="Debug mode. No work is performed. Reports what would have been done. [default=%default]")
)

# Using positional_arguments=TRUE splits the arguments into two lists, options and args.
# Options contains the arguments given in option_list.  Args contains all unknown arguments.
opts <- parse_args(OptionParser(option_list=option_list), positional_arguments=TRUE)
opts <- opts$options
print(opts)

# Split the sources at the comma delimiters
Sources <- strsplit(opts$Sources,",")[[1]]

# Create the list of models
ModelInfos <- list(
  list( formulaStr = c("iGDP ~ iK + iYear", 
                       "iGDP ~ iL + iYear",
                       "iGDP ~ energy + iYear"),
        fun = "sfModel",
        dots = list()),
  list( formulaStr = c("iGDP ~ iK + iL + iYear",
                       "iGDP ~ iK + iL + energy + iYear"),
        fun = "cdModel",
        dots = list()),
  list( formulaStr = "iGDP ~ iK + iL + energy + iYear",
        fun = "linexModel",
        dots = list()),
  list( formulaStr = "iGDP ~ iK + iL + iYear",
        fun = "cesModel",
        dots = list(nest=1:2)),
  list( formulaStr = "iGDP ~ iK + iL + energy + iYear",
        fun = "cesModel",
        dots = list(nest=1:3)),
  list( formulaStr = "iGDP ~ iK + iL + energy + iYear",
        fun = "cesModel",
        dots = list(nest=c(2,3,1))),
  list( formulaStr = "iGDP ~ iK + iL + energy + iYear",
        fun = "cesModel",
        dots = list(nest=c(1,3,2)))
)

# Adjust for faster operation
if (opts$veryfast){
  ModelInfos <- head(ModelInfos, -4)  # skip all ces models
} else if (opts$fast){
  ModelInfos <- head(ModelInfos, -3)  # skip ces models with energy
}
# ModelInfos <- tail( ModelInfos,2)

source_list <- list()
country_list <- list()
formulaStr_list <- list()
countryData_list <- list()
model_info_list <- list()
energy_list <- list()

for (src in Sources){
  historicalData <- eval(parse(text=src))
  Countries <- countryAbbrevs[countryAbbrevs %in% levels(historicalData$Country)]
  Energies <- energyTypes[energyTypes %in% names(historicalData)]
  
  for (country in Countries) {
    countryData <- subset(historicalData, subset=Country==country)
    for (m in ModelInfos) {
      for (f in m$formulaStr) {
        for (energy in if (grepl("energy", f))  Energies else 'noEnergy') {
          formulaStr <- sub( "energy", energy, f ) 
          if (opts$verbose) {
            cat ( paste(src, country, m$fun, formulaStr, m$dots, sep=" : ") ) 
            cat ("\n")
          }
          source_list <- c(source_list, src)
          country_list <- c(country_list, country)
          formulaStr_list <- c(formulaStr_list, formulaStr)
          countryData_list[[length(countryData_list) + 1]] <- countryData
          model_info_list[[length(model_info_list) + 1]] <- m
          energy_list <- c(energy_list, energy)
        } # energy
      } # f
    } # m
  } # country
} # src


Process <- 
  function( 
    src,
    country,
    m,
    formulaStr,
    countryData,
    debug = opts$debug,
    ...
  )
  { 
    formula <- eval( parse( text= formulaStr ) )
    if (opts$verbose)
      cat ( paste(src, country, m$fun, formulaStr, m$dots, sep=" : ") )
    if (! opts$debug){
      # If we're not in debug mode, do the calculations.
      fs <- factorString(formula=formula, nest=m$dots$nest)
      res <- tryCatch({
        oModel <- do.call( m$fun, c( list( formula, data=countryData ), m$dots) )
        mod <- sub("Model", "", x=m$fun)
        attr(oModel, "id") <- 
          list(src = src, country=country, mod=mod, fs=fs)
        oModel
      }, 
      error=function(e) {
        cat(paste0("  *** Skipping ", energy, " for ", country, "\n"))
        print(e)
        oModel <- list() 
        attr(oModel, "id") <- 
          list(src = src, country=country, mod=mod, fs=fs)
        attr(oModel, "error") <- e
        oModel
      }
      )
    } else {  # debugging mode -- dont actually do the model fitting
      res <- list()
      attr(res, "id") <- 
        list(src = src, country=country, mod=mod, fs=fs)
    }
    res
  }

models <- 
  mcMap(Process, 
        src = head(source_list, opts$maxRuns),
        country = head(country_list, opts$maxRuns),
        m = head(model_info_list, opts$maxRuns),
        formula = head(formulaStr_list, opts$maxRuns),
        countryData = head(countryData_list, opts$maxRuns),
        mc.cores = parallel::detectCores() # based on minimal testing, -1 seems to slow this down.
  )

if (! opts$debug){
  # Change from flat list to a tree
  oModels <- list()
  for( i in 1:length(models) ) {
    if (!is.null(attr(models[[i]], "id"))) {
      id <- attr(models[[i]], "id")
      oModels[[id$src]][[id$country]][[id$mod]][id$fs] <- 
        if (isTRUE(all.equal(models[[i]], list(), check.attributes = FALSE)) ){
          list(NULL)
        } else {
          list(models[[i]])
        }
    } 
  }
}

#
# Save oModels object to outputDir.
#
output_dir <- file.path(opts$outputDir, src)
oModels_path <- file.path(output_dir, filename_oModels)
models_path <- file.path(output_dir, filename_models)
if (opts$debug){
  cat(paste("Would have saved", output_path)) 
  cat("\n")
} else {
  cat(paste("Saving", output_path, "...")) 
  cat("\n")
  dir.create(output_dir, showWarnings=FALSE)
  saveRDS(oModels, file=oModels_path)
  saveRDS( models, file=models_path)
} 

cat(paste("Working Directory:", getwd()))
cat("\n")
cat(paste("Number of Models Attempted:", length(models)))
cat("\n")
cat(paste("Number of Models Saved:", 
          length(leaf_apply(oModels, f=function(...) 1, strict.lists=TRUE))))
cat("\n\nDone!\n")
