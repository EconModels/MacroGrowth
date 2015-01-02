#!/usr/bin/Rscript
#
# This script creates a data frame for the specified <Sources> and moves it into place for
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

# Provide a way to specify data sources
option_list <- list(
  make_option(c("-S", "--Sources"), default="Calvin",
              help="Comma-delimited sources of data [default=%default]")
)

opts <- parse_args(OptionParser(option_list=option_list))

# Split the sources at the comma delimiters
Sources <- strsplit(opts$Sources,",")[[1]]


# Directory into which objects should be saved
outputdir <- file.path("Packages", "EconData", "data")

#
# Load historical data sets and save the data frames for inclusion in the EconData package.
#
for (src in Sources){
  cat(paste("Loading and saving", src, "historical data ...")); cat("\n")
  inputpath <- file.path("data", paste0(src, ".txt"))
  data <- read.table(inputpath, header=TRUE)
  data$Country <- relevelFactor(data$Country, levs=countryAbbrevs)
  assign(src, data)
  save(list=src, file=file.path(outputdir, paste0(src, ".rda")))
}

#
# Remember to rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')