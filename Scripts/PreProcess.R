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
# I would like to do the following. 
# But, the data frames get stored with the name "data", and, 
# thus, collide with each other.
# databases <- c("Econ2011", "Warr2000")
# for (file in databases){
#   cat("  "); cat(file); cat("\n")
#   infile <- paste0(file, ".txt")
#   outfile <- paste0(file, ".rda")
#   data <- read.table(file.path("data", infile), header=TRUE)
#   # Get the countries that are in this database, in the order we'd like the levels
#   CountryLevels <- countryAbbrevs[countryAbbrevs %in% levels(data$Country)]
#   # Releve the country column
#   for (lev in rev(CountryLevels)) { data$Country <- relevel(data$Country, ref=lev) }
#   save(data, file=file.path(datadir, outfile), compress="gzip")
# }

# Econ2011 Data Set
cat("  Econ2011..."); cat("\n")
Econ2011 <- read.table(file.path("data", "Econ2011.txt"), header=TRUE)
Econ2011CountryLevels <- countryAbbrevs[countryAbbrevs %in% levels(Econ2011$Country)]
for (lev in rev(Econ2011CountryLevels)) { Econ2011$Country <- relevel(Econ2011$Country, ref=lev) }
save(Econ2011, file=file.path(datadir, "Econ2011.rda"), compress="gzip")
# Warr2000 Data Set
cat("  Warr2000..."); cat("\n")
Warr2000 <- read.table(file.path("data", "Warr2000.txt"), header=TRUE)
Warr2000CountryLevels <- countryAbbrevs[countryAbbrevs %in% levels(Warr2000$Country)]
for (lev in rev(Warr2000CountryLevels)) { Warr2000$Country <- relevel(Warr2000$Country, ref=lev) }
save(Warr2000, file=file.path(datadir, "Warr2000.rda"), compress="gzip")


#
# Rebuild the EconData package
#

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')