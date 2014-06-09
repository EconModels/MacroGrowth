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
# Load historical data and save the data frame.
#
cat("Loading and saving historical data..."); cat("\n")
Econ2011 <- read.table(file.path("data", "Econ2011.txt"), header=TRUE)
# Relevel country factor.
for (lev in rev(countryAbbrevs)) { Econ2011$Country <- relevel(Econ2011$Country, ref=lev) }
save(Econ2011, file=file.path(datadir, "Econ2011.rda"), compress="gzip")

#
# Copy oModels.Rdata into the correct position
#
cat("Copying original models file..."); cat("\n")
OrigModels <- readRDS(file.path("data_resample", "oModels.Rdata"))
save(OrigModels, file=file.path(datadir, "OrigModels.rda"), compress="gzip")

#
# Load all coefficients. Do this task in parallel for a (minor) speed gain.
#
cat("Loading and saving all coefficients..."); cat("\n")
AllCoef <- foreach(country=countryAbbrevs, .combine=rbind) %dopar% {
  loadResampledData(path="data_resample", country=country, kind="coeffs")
}
# This next code can be used to ensure that the results are identical. 
# But, you should change countryAbbrevs to sort(countryAbbrevs) above
# AllCoef2 <- loadResampledData(path="data_resample", kind="coeffs")
# print(identical(AllCoef, AllCoef2))

# Relevel the country abbreviations
AllCoef$country <- as.factor(AllCoef$country)
for (lev in rev(countryAbbrevs)) { AllCoef$country <- relevel(AllCoef$country, ref=lev) }
# Relevel nestStr
AllCoef$nestStr <- as.factor(AllCoef$nestStr)
for (lev in rev(nestStrLevels)) { AllCoef$nestStr <- relevel(AllCoef$nestStr, ref=lev) }
# Relevel nestStrParen
AllCoef$nestStrParen <- as.factor(AllCoef$nestStrParen)
for (lev in rev(nestStrParenLevels)) { AllCoef$nestStrParen <- relevel(AllCoef$nestStrParen, ref=lev) }
# Replace energy NA with "none" and set levels
AllCoef$energy <- replace(AllCoef$energy, which(is.na(AllCoef$energy)), "none")
AllCoef$energy <- as.factor(AllCoef$energy)
for (lev in rev(energyLevels)) { AllCoef$energy <- relevel(AllCoef$energy, ref=lev) }

# Save all coefficients in one data frame
save(AllCoef, file=file.path(datadir, "AllCoef.rda"), compress="gzip")

#
# Load all fitted models
#
cat("Loading and saving all fitted models..."); cat("\n")
AllFitted <- foreach(country=countryAbbrevs, .combine=rbind) %dopar% {
  loadResampledData(path="data_resample", country=country, kind="fitted")
}
# system.time(AllFitted2 <- loadResampledData(path="data_resample", kind="fitted"))
# Relevel the country abbreviations
AllFitted$Country <- as.factor(AllFitted$Country)
for (lev in rev(countryAbbrevs)) { AllFitted$Country <- relevel(AllFitted$Country, ref=lev) }
# Relevel nest
AllFitted$nestStr <- as.factor(AllFitted$nestStr)
for (lev in rev(nestStrLevels)) { AllFitted$nestStr <- relevel(AllFitted$nestStr, ref=lev) }
# Relevel nestStrParen
AllCoef$nestStrParen <- as.factor(AllCoef$nestStrParen)
for (lev in rev(nestStrParenLevels)) { AllCoef$nestStrParen <- relevel(AllCoef$nestStrParen, ref=lev) }
# Replace energy NA with "none" and set levels
AllFitted$energy <- replace(AllFitted$energy, which(is.na(AllFitted$energy)), "none")
AllFitted$energy <- as.factor(AllFitted$energy)
for (lev in rev(energyLevels)) { AllFitted$energy <- relevel(AllFitted$energy, ref=lev) }


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