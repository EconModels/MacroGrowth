#!/usr/bin/Rscript
#
# This script creates all objects and moves them into place for
# the EconData package.
# Execute this script from the directory that contains "data_resample."
# You'll probably use the command "Scripts/PostProcess.R"
#
# Run this script after you have done all analyses, 
# possibly by executing the "batch.bash" script.
# This script assumes that models and data
# live in the directory called "data_resample." ("batch.bash" does this by default.)

require(EconData)
require(EconModels)
require(doParallel)
registerDoParallel()

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

# Directory into which objects should be saved
datadir <- file.path("Packages", "EconData", "data")

#
# Load all coefficients. Do this task in parallel for a (minor) speed gain.
#
cat("Loading and saving all coefficients..."); cat("\n")
# .errorhandling="remove" skips missing countries.
AllCoef <- foreach(country=countryAbbrevs, .combine=rbind, .errorhandling="remove") %dopar% {
  loadResampledData(path="data_resample", country=country, kind="coeffs")
}
# This next code can be used to ensure that the results are identical. 
# But, you should change countryAbbrevs to sort(countryAbbrevs) above
# AllCoef2 <- loadResampledData(path="data_resample", kind="coeffs")
# print(identical(AllCoef, AllCoef2))

# Relevel the country abbreviations, nestStr, nestStrParen, and energy in AllCoef
AllCoef$country <- relevelFactor(as.factor(AllCoef$country), countryAbbrevs)
AllCoef$nestStr <- relevelFactor(as.factor(AllCoef$nestStr), nestStrLevels)
AllCoef$nestStrParen <- relevelFactor(as.factor(AllCoef$nestStrParen), nestStrParenLevels)
AllCoef$energy <- replace(AllCoef$energy, which(is.na(AllCoef$energy)), "none")
AllCoef$energy <- relevelFactor(as.factor(AllCoef$energy), energyLevels)

# Save all coefficients in one data frame
save(AllCoef, file=file.path(datadir, "AllCoef.rda"), compress="gzip")

#
# Load all fitted models
#
cat("Loading and saving all fitted models..."); cat("\n")
# .errorhandling="remove" skips missing countries.
AllFitted <- foreach(country=countryAbbrevs, .combine=rbind, .errorhandling="remove") %dopar% {
  loadResampledData(path="data_resample", country=country, kind="fitted")
}
# system.time(AllFitted2 <- loadResampledData(path="data_resample", kind="fitted"))
# Relevel the country abbreviations, nestStr, nestStrParen, and energy in AllFitted
AllFitted$Country <- relevelFactor(as.factor(AllFitted$Country), countryAbbrevs)
AllFitted$nestStr <- relevelFactor(as.factor(AllFitted$nestStr), nestStrLevels)
AllFitted$nestStrParen <- relevelFactor(as.factor(AllFitted$nestStrParen), nestStrParenLevels)
AllFitted$energy <- replace(AllFitted$energy, which(is.na(AllFitted$energy)), "none")
AllFitted$energy <- relevelFactor(as.factor(AllFitted$energy), energyLevels)

# Save all fitted models in one data frame
save(AllFitted, file=file.path(datadir, "AllFitted.rda"), compress="gzip")

#
# Create an archive of the results
#
cat("Creating archive..."); cat("\n")
zip(zipfile="data_resample.zip", files="data_resample", flags="-r9Xj")

# Build EconData package here?

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')