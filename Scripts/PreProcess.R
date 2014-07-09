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
databases <- c("Econ2011", "Warr2000")
for (file in databases){
  cat("  "); cat(file); cat("\n")
  infile <- paste0(file, ".txt")
  outfile <- paste0(file, ".rda")
  data <- read.table(file.path("data", infile), header=TRUE)
  # Get the countries that are in this database, in the order we'd like the levels
  CountryLevels <- countryAbbrevs[countryAbbrevs %in% levels(data$Country)]
  # Releve the country column
  for (lev in rev(CountryLevels)) { data$Country <- relevel(data$Country, ref=lev) }
  save(data, file=file.path(datadir, outfile), compress="gzip")
}

#
# Rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')