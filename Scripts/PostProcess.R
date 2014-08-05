#!/usr/bin/Rscript
#
# This script creates all objects and moves them into place for
# the EconData package.
# You'll probably use the command "Scripts/PostProcess.R -R data_resample/<Source>" 
# where <Source> is the name of a data source, such as "Calvin" or "REXS".
#
# Run this script after you have done all analyses, 
# probably by executing a <Source>_batch.bash script.

require(EconData)
require(EconModels)
require(optparse)
require(doParallel)
registerDoParallel()

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

# Provide a way to specify directory of resampled data
option_list <- list(
  make_option(c("-S", "--Source"), default="Calvin", 
              help="data source [default=%default]"),
  make_option(c("-R", "--resamplePath"),
              help="relative path to directory in which resampled data are stored. [default=data_resample/<Source>]")
)
# Parse the option list
opts <- parse_args(OptionParser(option_list=option_list))
print(opts)

if (is.null(opts$resamplePath)){
  # Didn't specify a resample path. 
  # Use data_resample/<Source>
  opts$resamplePath <- file.path("data_resample", opts$Source)
}

# Get the data Source from the resamplePath.
dir_pieces <- strsplit(opts$resamplePath, split=.Platform$file.sep)[[1]]
Source <- dir_pieces[length(dir_pieces)]

# Directory into which objects should be saved for the EconData pacakge
outputdir <- file.path("data_postprocessed")

#
# Load all coefficients. Do this task in parallel for a (minor) speed gain.
#
cat(paste("Loading and saving", Source, "coefficients...")); cat("\n")
# .errorhandling="remove" skips missing countries.
Coeffs <- foreach(country=countryAbbrevs, .combine=rbind, .errorhandling="remove") %dopar% {
  loadResampledData(path=opts$resamplePath, country=country, kind="coeffs")
}
# This next code can be used to ensure that the results are identical. 
# But, you should change countryAbbrevs to sort(countryAbbrevs) above
# coeffs2 <- loadResampledData(path=opts$resamplePath, kind="coeffs")
# print(identical(coeffs, coeffs2))

# Add the Source to the data frame.
Coeffs$Source <- Source
# Relevel the country abbreviations, nestStr, nestStrParen, and energy in coeffs
Coeffs$country <- relevelFactor(as.factor(Coeffs$country), levs=countryAbbrevs)
Coeffs$nestStr <- relevelFactor(as.factor(Coeffs$nestStr), levs=nestStrLevels)
Coeffs$nestStrParen <- relevelFactor(as.factor(Coeffs$nestStrParen), levs=nestStrParenLevels)
Coeffs$energy <- replace(Coeffs$energy, which(is.na(Coeffs$energy)), "none")
Coeffs$energy <- relevelFactor(as.factor(Coeffs$energy), levs=energyLevels)

# Save all coefficients in one data frame
# objectname <- paste0(Source, "_Coeffs")
# outpath <- file.path(outputdir, paste0(objectname, ".rda"))
# assign(objectname, Coeffs)
# save(list=objectname, file=outpath, compress="gzip")
outpath <- file.path(outputdir, paste0(Source, "_Coeffs.Rdata"))
saveRDS(Coeffs, file = outpath, compress = TRUE)

#
# Load all fitted models
#
cat(paste("Loading and saving", Source, "fitted models...")); cat("\n")
# .errorhandling="remove" skips missing countries.
Fitted <- foreach(country=countryAbbrevs, .combine=rbind, .errorhandling="remove") %dopar% {
  loadResampledData(path=opts$resamplePath, country=country, kind="fitted")
}
# Relevel the country abbreviations, nestStr, nestStrParen, and energy in Fitted
Fitted$Country <- relevelFactor(as.factor(Fitted$Country), levs=countryAbbrevs)
Fitted$nestStr <- relevelFactor(as.factor(Fitted$nestStr), levs=nestStrLevels)
Fitted$nestStrParen <- relevelFactor(as.factor(Fitted$nestStrParen), levs=nestStrParenLevels)
Fitted$energy <- replace(Fitted$energy, which(is.na(Fitted$energy)), "none")
Fitted$energy <- relevelFactor(as.factor(Fitted$energy), levs=energyLevels)

# Save all fitted models in one data frame
# objectname <- paste0(Source, "_Fitted")
# outpath <- file.path(outputdir, paste0(objectname, ".rda"))
# assign(objectname, Fitted)
# save(list=objectname, file=outpath, compress="gzip")
outpath <- file.path(outputdir, paste0(Source, "_Fitted.Rdata"))
saveRDS(Fitted, file = outpath, compress = TRUE)


#
# Create an archive of the results
#
cat(paste0("Creating archive for ", Source, "...")); cat("\n")
zip(zipfile=file.path("data_resample", paste0(Source, "_data_resample.zip")), files=paste0("data_resample/", Source), flags="-r9Xj")

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')