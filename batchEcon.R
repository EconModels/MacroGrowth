#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales
source('Econ-Growth-Resampling.R',echo=FALSE,verbose=FALSE)
suppressPackageStartupMessages(library("optparse"))


option_list <- list(
  make_option(c("-c", "--country"), default="all",
              help="country [default=%default]"),
  make_option(c("-e", "--energy"), default="Q",
              help="energy [default=%default]"),
  make_option(c("-m", "--model"), default="all",
              help="model [default=%default]"),
  make_option(c("-n", "--resamples"), default=10L, type="integer",
              help="number of resamples [default=%default]"),
  make_option(c("-C", "--clobber"), default=FALSE, action="store_true",
              help="number of resamples [default=%default]"),
  make_option(c("-d", "--debug"), default=FALSE, action="store_true",
              help="number of resamples [default=%default]"),
  make_option(c("-M", "--method"), default="wild", 
              help="resampling method [default=%default]")
  )

opts <- parse_args(OptionParser(option_list=option_list))

if(opts$model == "all") {
  opts$model <- modelTypes
} else {
  opts$model <- strsplit(opts$model,",")[[1]]
  opts$model <- match.arg(opts$model,choices=modelTypes,several.ok=TRUE)
}

if(opts$country== "all") {
  opts$country <- countryAbbrevs
} else {
  opts$country <- strsplit(opts$country,",")[[1]]
  opts$country <- match.arg(opts$country,choices=countryAbbrevs,several.ok=TRUE)
}

if(opts$energy== "all") {
  opts$energy <- energyTypes
} else {
  opts$energy <- toupper(strsplit(opts$energy,",")[[1]])
  opts$energy <- match.arg(opts$energy,choices=energyTypes,several.ok=TRUE)
}


print(str(opts))

if( ! opts$debug) {
  
  
  
  #genResampleData(modelType = "cese-(kl)e", countryAbbrev="CN", energyType="Q", n=2, method="wild", clobber=TRUE)
  #cat('half way')

  for (country in opts$country)
    for (model in opts$model)
      for (energy in opts$energy) {
        cat(paste0("\nFitting ", country, ":", model, ":",energy))
        genResampleData(modelType=model, 
                        countryAbbrev=country,  
                        energyType=energy, 
                        n=opts$resamples, 
                        method=opts$method, 
                        clobber=opts$clobber)
      }
  cat("\nDone @ ")
  cat(date())
  cat('\n\n')
}

  
