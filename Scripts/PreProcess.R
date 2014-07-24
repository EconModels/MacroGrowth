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

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

# Directory into which objects should be saved
outputdir <- file.path("Packages", "EconData", "data")

#
# Load historical data sets and save the data frames for inclusion in the EconData package.
#
for (src in dataSources){
  cat(paste("Loading and saving", src, "historical data ...")); cat("\n")
  data <- read.table(file.path("data", paste0(src, ".txt")), header=TRUE)
  data$Country <- relevelFactor(data$Country, countryAbbrevs)
  assign(src, data)
  save(list=src, file=file.path(outputdir, paste0(src, ".rda")))
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