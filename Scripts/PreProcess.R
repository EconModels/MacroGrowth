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

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

# Directory into which objects should be saved
datadir <- file.path("Packages", "EconData", "data")

#
# Load historical data sets and save the data frames.
#
cat("Loading and saving historical data..."); cat("\n")

Calvin2011 <- read.table(file.path("data", "Calvin2011.txt"), header=TRUE)
Warr2000 <- read.table(file.path("data", "Warr2000.txt"), header=TRUE)

# Relevel the countries in the data frames.
sources <- list(Calvin2011, Warr2000)
for (data in sources){
  CountryLevels <- countryAbbrevs[countryAbbrevs %in% levels(data$Country)]
  for (lev in rev(CountryLevels)) { data$Country <- relevel(data$Country, ref=lev) }
}
AllHistData <- rbind(Calvin2011, Warr2000)
save(AllHistData, file=file.path(datadir, "AllHistData.rda"), compress="gzip")

#
# Rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')