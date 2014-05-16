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

# Load historical data and save the data frame.
cat("Loading and saving historical data..."); cat("\n")
Econ2011 <- read.table(file.path("data", "Econ2011.txt"), header=TRUE)
save(Econ2011, file=file.path(datadir, "Econ2011.rda"), compress="gzip")

# Copy oModels.Rdata into the correct position
cat("Copying original models file..."); cat("\n")
OrigModels <- readRDS(file.path("data_resample", "oModels.Rdata"))
save(OrigModels, file=file.path(datadir, "OrigModels.rda"), compress="gzip")

# Load all coefficients
cat("Loading and saving all coefficients..."); cat("\n")
AllCoef <- loadResampledData(path="data_resample", kind="coeffs")
# Save all coefficients in one data frame
save(AllCoef, file=file.path(datadir, "AllCoef.rda"), compress="gzip")

# Load all fitted models
cat("Loading and saving all fitted models..."); cat("\n")
AllFitted <- loadResampledData(path="data_resample", kind="fitted")
# Save all fitted models in one data frame
save(AllFitted, file=file.path(datadir, "AllFitted.rda"), compress="gzip")

# Create an archive of the results
cat("Creating archive..."); cat("\n")
zip(zipfile="data_resample.zip", files="data_resample", flags="-r9Xj")

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')