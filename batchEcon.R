#!/usr/bin/Rscript  --default-packages=utils,stats,lattice,grid,mosaic,methods,graphics,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales

source('Econ-Growth-Resampling.R',echo=FALSE,verbose=FALSE)
suppressPackageStartupMessages(library("optparse"))


option_list <- list(
  make_option(c("-c", "--country"), default="all",
              help="country [default=%default]"),
  make_option(c("-e", "--energy"), default="Q",
              help="energy [default=%default]"),
  make_option(c("-f", "--factor"), default="K",
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
  make_option(c("-p", "--parallelize"), default=FALSE, action="store_true",
              help="run countries in parallel [default=%default]")
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

if(opts$factor== "all") {
  opts$factor <- factors
} else {
  opts$factor <- toupper(strsplit(opts$factor,",")[[1]])
  opts$factor <- match.arg(opts$factor,choices=factors,several.ok=TRUE)
}

print(str(opts))

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

if( ! opts$debug) {
  
  #genResampleData(modelType = "cese-(kl)e", countryAbbrev="CN", energyType="Q", n=2, method="wild", clobber=TRUE)
  #cat('half way')
  
  # Do this work in parallel based on the countries desired. 
  # Usually, we'll want all countries, so this makes sense.
  registerDoParallel()
  for (model in opts$model) {
    for (energy in opts$energy) {
      for (factor in opts$factor){
        if (opts$parallelize){
          # Do countries in parallel
          foreach(country=opts$country, .errorhandling="pass", .init=c(), .combine=c) %dopar% {
            cat(paste0("\nFitting ", model, ":", country, ":", energy, ":", factor))
            genResampleData(modelType=model, 
                            countryAbbrev=country,  
                            energyType=energy, 
                            factor=factor,
                            n=opts$resamples, 
                            method=opts$method, 
                            clobber=opts$clobber)
          }
          
        } else {
          # Do countries sequentially
          for (country in opts$country) {
            cat(paste0("\nFitting ", model, ":", country, ":", energy, ":", factor))
            genResampleData(modelType=model, 
                            countryAbbrev=country,  
                            energyType=energy, 
                            factor=factor,
                            n=opts$resamples, 
                            method=opts$method, 
                            clobber=opts$clobber)
          }
        }        
      }        
    }
  }
}

# Do the work in series. I'm leaving this code in here in case we want to 
# switch back to it later.
#   for (model in opts$model) {
#     for (country in country) {
#       for (energy in opts$energy) {
#         for (factor in opts$factor){
#       }
#     }
#   }

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')
