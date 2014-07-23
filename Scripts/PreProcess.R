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
cat("Loading and saving Calvin historical data..."); cat("\n")
Calvin <- read.table(file.path("data", "Calvin.txt"), header=TRUE)
Calvin$Country <- relevelFactor(Calvin$Country, countryAbbrevs)
save(Calvin, file=file.path(outputdir, "Calvin.rda"), compress="gzip")

# See https://sites.google.com/site/benjaminwarr/the-economic-growth-engine/rexs-database
cat("Loading and saving REXS historical data..."); cat("\n")
REXS <- read.table(file.path("data", "REXS.txt"), header=TRUE)
REXS$Country <- relevelFactor(REXS$Country, countryAbbrevs)
save(REXS, file=file.path(outputdir, "REXS.rda"),   compress="gzip")

#
# Rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')