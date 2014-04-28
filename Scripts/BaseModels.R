#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales
#
# This script fits all of the base models for each combination of 
# model, country, factor, energy type, and nesting.
# 

# Results are stored in the file "data_resample/oModels.Rdata"
# To load this data back in, do 
# 
# readRDS(file="data_resample/oModels.Rdata")
#

require(EconModels)
require(EconData)
require(plyr)  # for rbind.fill()

nestStr <- function(nest) paste(nest, collapse="")

All <- Econ2011
Countries <- levels(All$Country)
Energies <- c("iQ", "iX", "iU")

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
coefs <- list()

for (country in Countries) {
  cdata <- subset(All, Country==country)
  for (m in ModelInfos) {
    for (f in m$formulaStr) {
      for (energy in if (grepl("energy", f))  Energies else 'noEnergy') {
        formulaStr <- sub( "energy", energy, f ) 
        formula <- eval( parse( text= formulaStr ) )
        # formula <- substitute( iGDP ~ iK + iL + e + iYear, list(e = energy))
        # tryCatch to skip over country/energy combos that don't exist.
        cat ( paste(country, m$fun, formulaStr, m$dots, sep=" : ") )
        cat ("\n")
        
        tryCatch({
          oModel <- do.call( m$fun, c( list( formula, data=cdata ), m$dots) )
          if (is.null(m$dots$nest)) {
            oModels[[country]][[m$fun]][[formulaStr]] <- oModel
          } else {
            oModels[[country]][[m$fun]][[formulaStr]][[nestStr(m$dots$nest)]] <- oModel
          }
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

# coefs2 <- do.call(rbind.fill, coefs)

saveRDS(oModels, file="data_resample/oModels.Rdata")

