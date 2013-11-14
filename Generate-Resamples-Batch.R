#
# This file is an R file that generates all of the resampling data required to build the paper.
# To execute it, do the following.
# 
# 1) At the command line, cd into the directory that contains this file.
# 2) Type "R CMD BATCH Generate-Resamples-Batch.R &"
#
# All of the processing will take about 1 week on a 2010 2 GHz Core i7 MacBook Pro.
# So, you'll want to run this on a machine that you're confident will
# stay up for a long time.
# 
# You'll want to record the process ID number that is returned from this call.
# To check on the process from a shell login session, you can type
# ps -deaf | grep <username>
#
# The output will be recorded in a text file named "Generate-Resamples-Batch.R",
# which you can periodically check to assess the progress.
# 
source("Econ-Growth-Resampling.R")
genAllResampleData(n=1, method="wild", clobber=TRUE, verbose=TRUE)