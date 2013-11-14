#!/usr/bin/Rscript --vanilla --default-packages=utils,stats,lattice,grid,mosaic,foreach,doParallel,plyr,xtable,nlmrt,micEconCES,systemfit,Matrix,lmtest,zoo,miscTools,micEcon,minpack.lm,DEoptim,iterators,parallel,latticeExtra,RColorBrewer,ggplot2,reshape2,scales

suppressPackageStartupMessages(library("optparse"))

option_list <- list(
  make_option(c("-c", "--country"), default="US",
              help="country [default=%default]"),
  make_option(c("-e", "--energy"), default="Q",
              help="energy [default=%default]"),
  make_option(c("-m", "--model"), default="cese-(kl)e",
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

print(opts)

if( ! opts$debug) {
  
  source('Econ-Growth-Resampling.R')
  
  #genResampleData(modelType = "cese-(kl)e", countryAbbrev="CN", energyType="Q", n=2, method="wild", clobber=TRUE)
  #cat('half way')
  
  genResampleData(modelType=opts$model, countryAbbrev=opts$country, 
                  energyType=opts$energy, n=opts$resamples, method=opts$method, clobber=opts$clobber)
}

  
