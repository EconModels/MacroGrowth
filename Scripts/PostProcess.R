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

# Copy oModels.Rdata into the correct position
print("Copying original models file...")
file.copy(from=file.path("data_resample", "oModels.Rdata"), 
          to=file.path(datadir, "OrigModels.rda"),
          overwrite=TRUE)

# Load all coefficients
cat("Loading and saving all coefficients...")
AllCoef <- loadResampledData(path="data_resample", kind="coeffs")

# Save all coefficients in one data frame
saveRDS(AllCoeffs, file.path(datadir, "AllCoef.rda"))

# Load all fitted models
cat("Loading and saving all fitted models...")
AllFitted <- loadResampledData(path="data_resample", kind="fitted")
# Save all fitted models in one data frame
saveRDS(AllFitted, file.path(datadir, "AllFitted.rda"))

# Create an archive of the results
cat("Creating archive...")
zip(zipfile="data_resample.zip", files="data_resample", flags="-r9Xj")

cat("\n\nDone @ ")
cat(date())
cat('\n\n')
cat("duration:\n")
print(proc.time() - startTime)
cat('\n\n')