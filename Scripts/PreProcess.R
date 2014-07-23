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
datadir <- file.path("Packages", "EconData", "data")

#
# Load historical data sets and save the data frames for inclusion in the EconData package.
#
cat("Loading and saving Calvin historical data..."); cat("\n")
Calvin <- read.table(file.path("data", "Calvin.txt"), header=TRUE)
Calvin$Country <- relevelFactor(Calvin$Country, countryAbbrevs)
save(Calvin, file=file.path(datadir, "Calvin.rda"), compress="gzip")

cat("Loading and saving Warr historical data..."); cat("\n")
Warr <- read.table(file.path("data", "Warr.txt"), header=TRUE)
Warr$Country <- relevelFactor(Warr$Country, countryAbbrevs)
save(Warr,   file=file.path(datadir, "Warr.rda"),   compress="gzip")

#
# Rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')