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
#
require(EconData)
#
# Copy oModels.Rdata into the correct position
file.copy(from=file.path("data_resample", "oModels.Rdata"), 
          to=file.path("Packages", "EconData", "data", "OrigModels.rda"),
          overwrite=TRUE)

# Load all coefficients

# Save all coefficients in one data frame


# Load all fitted models

# Save all fitted models in one data frame



# Create an archive of the results
zip("data_resample.zip", "data_resample")