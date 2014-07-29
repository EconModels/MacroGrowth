#!/usr/bin/Rscript
#
# This script creates a data frame for the specified <Source> and moves it into place for
# the EconData package.
# Execute this script from top level of the repository (Econ-Growth-R-Analysis).
# You'll probably use the command "Scripts/PreProcess.R"
#
# Run this script before you do resampling analyses.

require(EconData)
require(optparse)

startTime <- proc.time()
cat("\n\nStart @ ")
cat(date())
cat('\n')

# Provide a way to specify data source
option_list <- list(
  make_option(c("-S", "--Source"), default="Calvin",
              help="Source of data [default=%default]")
)

opts <- parse_args(OptionParser(option_list=option_list))


# Directory into which objects should be saved
outputdir <- file.path("Packages", "EconData", "data")

#
# Load historical data sets and save the data frames for inclusion in the EconData package.
#
cat(paste("Loading and saving", opts$Source, "historical data ...")); cat("\n")
inputpath <- file.path("data", paste0(opts$Source, ".txt"))
data <- read.table(inputpath, header=TRUE)
data$Country <- relevelFactor(data$Country, levs=countryAbbrevs)
assign(opts$Source, data)
save(list=opts$Source, file=file.path(outputdir, paste0(opts$Source, ".rda")))

#
# Rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')