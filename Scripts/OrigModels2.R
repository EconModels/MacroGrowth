#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales
#
# This script fits all of the base models for each combination of 
# source, country, model, factor, energy type, and nesting.
# 
# This script should be run from the top directory (Econ-Growth-R-Analysis) with the command
# "Scripts/OrigModels.R -S <Sources>"
#

# Results are stored in the file "data_resample/<Source>/Models.Rdata"
# To load this data back in, do 
# 
# oModels <- readRDS(file="data_resample/<Source>/Models.Rdata")
#
# To extract a model, do, for example
# mod <- oModels$REXS$US$cd$`iK+iL+iQ`
# 
# Note, too, that <Source>_Models is an object in the EconData package.

require(EconModels)
require(EconData)
require(optparse)

nestStr <- function(nest){
  paste(nest, collapse="")
}

defaultOutputDir <- "data_resample"
filename_Rdata <- "oModels.Rdata"

# Provide a way to specify data sources in a comma-separated list
option_list <- list(
  make_option(c("-S", "--Sources"), default="Calvin",
              help="Comma-separated list of sources of data [default=%default]"),
  make_option(c("-f", "--fast"), default=FALSE, action="store_true",
              help="Runs fast models (skips CES with energy) [default=%default]"),
  make_option(c("-v", "--veryfast"), default=FALSE, action="store_true",
              help="Runs very fast models (skips CES altogether). Overrides -f. [default=%default]"),
  make_option(c("-O", "--outputDir"), default=defaultOutputDir,
              help=paste0("relative path to directory ",
                          "in which original models are saved ",
                          "(one level above the <Source> directories) [default=",
                          defaultOutputDir)),
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
          cat ( paste(src, country, m$fun, formulaStr, m$dots, sep=" : ") ) ; cat ("\n")
          source_list <- c(source_list, src)
          country_list <- c(country_list, country)
          formulaStr_list <- c(formulaStr_list, formulaStr)
          countryData_list <- c(countryData_list, countryData)
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
    cat ( paste(src, country, m$fun, formulaStr, m$dots, sep=" : ") )
    if (! opts$debug){
      # If we're not in debug mode, do the calculations.
      res <- tryCatch({
        oModel <- do.call( m$fun, c( list( formula, data=countryData ), m$dots) )
        mod <- sub(pattern="Model", replacement="", x=m$fun)
        fs <- factorString(formula=formula, nest=m$dots$nest)
        attr(oModel, "id") <- 
          list(src = src, country=country, mod=mod, fs=fs)
        oModel
      }, 
      error=function(e) {
        cat(paste0("  *** Skipping ", energy, " for ", country, "\n"))
        print(e)
        NULL
      }
      )
    } else { 
      res <- NULL 
    }
    res
  }

models <- 
  mcMap(Process, 
        src = source_list,
        country = country_list,
        m = model_info_list,
        formula = formulaStr_list,
        countryData = countryData_list,
        mc.cores = parallel::detectCores() - 1
  )

if (! opts$debug){
  # Change from flat list to a tree
  oModels <- list()
  for( i in 1:length(models) ) {
    id <- attr(models[[i]], "id")
    oModels[[id$src]][[id$country]][[id$mod]][[id$fs]] <- models[[i]]
  }
}

#
# Save oModels object to outputDir.
#
output_dir <- file.path(opts$outputDir, src)
output_path <- file.path(output_dir, filename_Rdata)
if (opts$debug){
  cat(paste("Would have saved", output_path)); cat("\n")
} else {
  cat(paste("Saving", output_path, "...")); cat("\n")
  dir.create(output_dir, showWarnings=FALSE)
  saveRDS(oModels, file=output_path)  
} 

cat("\n\nDone!\n")
