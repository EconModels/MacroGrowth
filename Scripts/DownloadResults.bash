#! /bin/bash
# 
# This script rsyncs post-processed resampling results files 
# from the directory specified by -p on dahl (or the default)
# to the Packages/EconData/tmp_big_data directory on a local machine.
# Files that are rsync-ed match the pattern "<SOURCE>_*.rda"
# The destination (TMP_BIG_DATA) is a staging directory for including the files in the EconData package.
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
#
# Inspried by http://tuxtweaks.com/2014/05/bash-getopts/
#

# Set fonts
NORM=`tput sgr0`
BOLD=`tput bold`
REV=`tput smso`

# Set Script Name variable
SCRIPT=`basename ${BASH_SOURCE[0]}`

# Help function
function HELP {
  echo -e "\\n"
  echo "${BOLD}${SCRIPT}${NORM} copies post-processed resampling results from dahl to a"
  echo "staging area on the local machine."
  echo "IMPORTANT: If you want to include these large data files in the ${BOLD}EconData${NORM} package,"
  echo "manually copy them to the ${BOLD}EconData/data${NORM} directory."
  echo "Uses rsync over ssh, so running this script will require a password for login to dahl."
  echo "Files that are rsync-ed match the pattern <SOURCE>_*.rda"
  echo "Run from the Econ-Growth-R-Analysis directory on a local machine."
  echo "Usage: ${BOLD}Scripts/$SCRIPT -u username -S Source${NORM} [-d] [-h] [-p path_to_data_on_dahl]"
  echo "Command line options:"
  echo "${BOLD}-u${NORM}  --Sets the username for dahl."
  echo "${BOLD}-S${NORM}  --Sets the data Source to be copied."
  echo "${BOLD}-d${NORM}  --Debug mode. No files transferred. Reports what would have been done."
  echo "${BOLD}-h${NORM}  --Prints help."
  echo "${BOLD}-p${NORM}  --Sets path of data directory on dahl."
  echo "           Default is appropriate for matt's account on dahl."
  echo "           Do not include trailing file separator."
  echo "Example: ${BOLD}$SCRIPT -u abc -S Calvin${NORM}"
  echo "           Downloads Calvin results from dahl as user abc."
  echo "Example: ${BOLD}$SCRIPT -u abc -S Calvin -d${NORM}"
  echo "           Same as above, except only prints which files would have been transfered from dahl."
  echo "           No files will be transfered. Useful for debugging purposes."
  echo -e "\\n"
  exit 1
}

# Check the number of arguments. If none are passed, print help and exit.
NUMARGS=$#
if [ $NUMARGS -eq 0 ]; then
  echo -e \\n"Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
  exit 2
fi

# Set defaults
USER=""
SOURCE=""
DEBUG=false
DAHL_PATH="/home/mkh2/github/Econ-Growth-R-Analysis/Packages/EconData/data"

# Parse command line flags
while getopts :u:S:p:dh FLAG; do
  case $FLAG in
    u)  #set option "u"
      USER=$OPTARG
      ;;
    S)  #set option "S"
      SOURCE=$OPTARG
      ;;
    d) #set debug mode
      DEBUG=true
      ;;
    h)  #show help
      HELP
      ;;
    p) #set path on dahl
      DAHL_PATH=$OPTARG
      ;;
    \?) # unknown option - show help
      echo -e \\n"Option ${BOLD}-$OPTARG${NORM} not allowed."
      echo -e "Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
      exit 2
      ;;
  esac
done

# Check whether -u was specified. It is required.
if [ -z "$USER" ]; then
  echo -e \\n"Option ${BOLD}-u${NORM} required."
  echo -e "Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
  exit 2
fi 

# Check whether -S was specified. It is required.
if [ -z "$SOURCE" ]; then
  echo -e \\n"Option ${BOLD}-S${NORM} required."
  echo -e "Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
  exit 2
fi

# Specify the files on dahl that we want to copy.
# Note that the '*' wildcard will match several files, all with the same $SOURCE.
REMOTE_FILES="$DAHL_PATH"/"$SOURCE"_*.rda
# Specify the local directory into which we want to save the files.
# The assumption is that the user has set "Econ-Growth-R-Analysis"" as the working directory.
LOCAL_DIR="$PWD/Packages/EconData/tmp_big_data"

# Set the options for rsync.
# -v gives verbose rsync output so that status can be monitored. Use -vv or -vvv for more verbosity.
# -t preserves time stamps on files.
# -e ssh indicates ssh protocol should be used.
RSYNC_OPTS="-v -t -e ssh"
if [ "$DEBUG" = true ] ; then
  # Add the -n option. -n prevents file transfers from occurring, but prints would would have been done.
  RSYNC_OPTS=-n" $RSYNC_OPTS"
fi

# Perform the rsync copy from dahl to the local machine.
rsync $RSYNC_OPTS $USER@fs1.calvin.edu:$REMOTE_FILES $LOCAL_DIR