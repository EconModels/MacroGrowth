#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales
#
# This script fits all of the base models for each combination of 
# source, country, model, factor, energy type, and nesting.
# 
# This script should be run from the top directory with the command
# "Scripts/OrigModels.R"
#

# Results are stored in the file "data_resample/oModels.Rdata"
# To load this data back in, do 
# 
# oModels <- readRDS(file="data_resample/oModels.Rdata")
#
# To extract a model, do, for example
# mod <- OrigModels[["Warr2000"]][["US"]][["cd"]][["iK+iL+iQ"]]
# OrigModels is an object in the EconData package.
# So, be sure to build that package first.

require(plyr)  # for rbind.fill()
require(EconModels)
require(EconData)
require(optparse)

nestStr <- function(nest){
  paste(nest, collapse="")
}

# Provide a way to specify data source
option_list <- list(
  make_option(c("-S", "--Source"), default=dataSources[1],
              help="Source of data [default=%default]")
)

opts <- parse_args(OptionParser(option_list=option_list))
# print(opts)

historicalData <- eval(parse(text=opts$Source))
Countries <- countryAbbrevs[countryAbbrevs %in% levels(historicalData$Country)]
Energies <- energyTypes[energyTypes %in% names(historicalData)]

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

# ModelInfos <- head(ModelInfos, -3)  # skip ces models with energy
# ModelInfos <- head(ModelInfos, -4)  # skip all ces models
# ModelInfos <- tail( ModelInfos,2)

oModels <- list()
for (country in Countries) {
  countryData <- subset(historicalData, subset=Country==country)
  for (m in ModelInfos) {
    for (f in m$formulaStr) {
      for (energy in if (grepl("energy", f))  Energies else 'noEnergy') {
        formulaStr <- sub( "energy", energy, f ) 
        formula <- eval( parse( text= formulaStr ) )
        # formula <- substitute( iGDP ~ iK + iL + e + iYear, list(e = energy))
        # tryCatch to skip over country/energy combos that don't exist.
        cat ( paste(opts$Source, country, m$fun, formulaStr, m$dots, sep=" : ") )
        cat ("\n")
        
        tryCatch({
          oModel <- do.call( m$fun, c( list( formula, data=countryData ), m$dots) )
          mod <- sub(pattern="Model", replacement="", x=m$fun)
          fs <- factorString(formula=formula, nest=m$dots$nest)
          oModels[[opts$Source]][[country]][[mod]][[fs]] <- oModel
        }, 
        error=function(e) {
          cat(paste0("  *** Skipping ", energy, " for ", country, "\n"))
          print(e)
        }
        )
      }
    }
  }
}
#
# Save object to data_resample for inclusion in a future zipped version of all of the results.
#
data_resample_dir <- file.path("data_resample", opts$Source)
dir.create(data_resample_dir, showWarnings=FALSE)
filename_Rdata <- paste0("OrigModels.Rdata")
data_resample_path <- file.path(data_resample_dir, filename_Rdata)
cat(paste("Saving", data_resample_path, "...")); cat("\n")
saveRDS(oModels, file=data_resample_path)
#
# Save object so that it is available to the EconData package
# (after EconData is built, of course).
#
# First, put the oModels object into the environment with the name by which it will be available from the package.
varname <- paste0(opts$Source, "_OrigModels")
assign(varname, oModels)
# Now, save the object
package_dir <- file.path("Packages", "EconData", "data")
dir.create(package_dir, showWarnings=FALSE)
filename_rda <- paste0(varname, ".rda")
package_path <- file.path(package_dir, filename_rda)
cat(paste("Saving", package_path, "...")); cat("\n")
save(list=varname, file=package_path)