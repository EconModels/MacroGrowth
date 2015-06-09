#!/usr/bin/Rscript
# 
# This script rsyncs post-processed resampling results files 
# from the directory specified by -p on dahl (or the default)
# to the Packages/EconData/tmp_big_data directory on a local machine.
# Files that are rsync-ed match the pattern "<SOURCE>_*.rda"
# 
# IMPORTANT:
# If you want to include the files in the EconData package for an analysis, 
# move them manually to the Packages/EconData/data directory and build and reload the EconData package.
# 
# Usage: in the top-level directory of this repository (Econ-Growth-R-Analysis) on the destination machine, say
# Scripts/DownloadResults.bash -u mkh2 -S REXS
#   -u indicates the user you want to be on dahl
#   -S identifies the data <Source> for which you want to rsync results.
#   -p identifies the directory on dahl from which you want to rsync results.
#
# You'll be prompted for the password for the user specified by -u on dahl.

suppressPackageStartupMessages(library("optparse"))

option_list <- list(
  make_option(c("-u", "--username"),
              help="Sets the username for dahl."),
  make_option(c("-S", "--Source"), default="Calvin",
              help="Comma-separated list of <Source>s to be copied. No spaces allowed. [default=%default]"),
  make_option(c("-d", "--debug"), default=FALSE, action="store_true",
              help="Debug mode. No files transferred. Reports what would have been done. [default=%default]"),
  make_option(c("-p", "--path"), default="/home/mkh2/github/Econ-Growth-R-Analysis/data_postprocessed",
              help=paste("Sets path of postprocessed data directory on dahl from which you want to copy. \n",
                         "               Do not include tralining file separator. \n",
                         "               [default=%default]")),
  make_option(c("-l", "--lpath"), default="data_postprocessed",
              help=paste("Sets path of postprocessed data directory on local machine to which you want to copy. \n",
                         "               This path must be relative to the Econ-Growth-R-Analysis directory. \n",
                         "               Do not include trailing file separator. \n",
                         "               [default=%default]"))
)

opts <- parse_args(OptionParser(
  description = paste0("Copies post-processed resampling results from dahl to a local machine. \n",
                       "Uses rsync over ssh, so running this script will require a password for login to dahl. \n",
                       "Files that are rsync-ed match the pattern <SOURCE>_*.rda \n",
                       "Run this script from the Econ-Growth-R-Analysis directory on a local machine."),
  option_list=option_list))

# Ensure that -u <username> is specified
if (is.null(opts$username)){
  stop("Need to specify -u USERNAME. Try ./DownloadPostProcessed.R -h")
}

# Split <Source>s at commas
opts$Source <- strsplit(opts$Source,",")[[1]]

print(opts)

# Build paths for remote files to be trasnferred to the local machine.
remote_files <- unlist(lapply(opts$Source, 
                              function(Source, dir) {
                                file.path(dir, paste0(Source, "_*"))
                                }, 
                              dir=opts$path))
remote_files <- paste(remote_files, collapse = " ")

# Build local file path
opts$lpath <- file.path(getwd(), opts$lpath)

# Set options for rsync.
# -v gives verbose rsync output so that status can be monitored. Use -vv or -vvv for more verbosity.
# -t preserves time stamps on files.
# -e ssh indicates ssh protocol should be used.
rsync_opts <- "-v -t -e ssh"

if (opts$debug){
  # Add a -n option to the rsync command.
  rsync_opts <- paste("-n", rsync_opts)
}

# Build the rsync command
cmd <- paste0("rsync ", rsync_opts, " ", opts$user, "@dahl.calvin.edu:'", remote_files, "' ", opts$lpath)

# Give the rsync command
system(cmd)