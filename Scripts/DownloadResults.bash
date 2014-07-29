#! /bin/bash
# 
# This script rsyncs results files from dahl to the Packages/EconData/data directory on a local machine.
# The destination is appropriate for including the files in the EconData package.
# 
# In the top-level directory of this repository (Econ-Growth-R-Analysis) on the destination machine, say
# Scripts/DownloadResults.bash -u mkh2 -S REXS
#   -u indicates the user you want to be on dahl
#   -S identifies the data <Source> for which you want to rsync results.
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
  echo -e \\n
  echo "${BOLD}${SCRIPT}${NORM} copies resampling results from dahl to the"
  echo "correct location on a local machine for building the ${BOLD}EconData${NORM} package."
  echo "Run from the Econ-Growth-R-Analysis directory on a local machine."
  echo "Uses rsync over ssh, so running this script will require a password for login to dahl."
  echo "Usage: ${BOLD}Scripts/$SCRIPT [-d] -u username -S Source${NORM}"
  echo "Command line options:"
  echo "${BOLD}-u${NORM}  --Sets the username for dahl."
  echo "${BOLD}-S${NORM}  --Sets the data Source to be copied."
  echo "${BOLD}-d${NORM}  --Debug mode. No files transferred. Reports what would have been done."
  echo "${BOLD}-h${NORM}  --Prints help."
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

# Parse command line flags
# If an option should be followed by an argument, it should be followed by a ":".
# Notice there is no ":" after "h". The leading ":" suppresses error messages from
# getopts. This is required to get my unrecognized option code to work.

while getopts :u:S:dh FLAG; do
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
    \?) # unknown option - show help
      echo -e \\n"Option ${BOLD}-$OPTARG${NORM} not allowed."
      echo -e "Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
      exit 2
      ;;
  esac
done

# Check to see if -u was specified
if [ -z "$USER" ]; then
  echo -e \\n"Option ${BOLD}-u${NORM} required."
  echo -e "Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
  exit 2
  echo "USER empty."
fi 

# Check to see if -S was specified
if [ -z "$SOURCE" ]; then
  echo -e \\n"Option ${BOLD}-S${NORM} required."
  echo -e "Use ${BOLD}$SCRIPT -h${NORM} for help."\\n
  exit 2
fi

# Specify the files on dahl that we want to copy.
# Note that the '*' wildcard will match several files, all with the same $SOURCE.
REMOTE_FILES="/home/mkh2/github/Econ-Growth-R-Analysis/Packages/EconData/data/$SOURCE'_'*.rda"
# Specify the local directory into which we want to save the files.
# The assumption is that the user has set "Econ-Growth-R-Analysis"" as the working directory.
LOCAL_DIR="$PWD/Packages/EconData/data"

# Set the options for rsync.
# -v gives verbose rsync output so that status can be monitored. Use -vv or -vvv for more verbosity.
# -r gives recursive behavior to copy contents of directories.
# -e ssh indicates ssh protocol should be used.
RSYNC_OPTS="-v -r -e ssh"
if [ "$DEBUG" = true ] ; then
  # Add the -n option. -n prevents file transfers from occurring, but prints would would have been done.
  RSYNC_OPTS="$RSYNC_OPTS -n"
fi

# Perform the rsync copy from dahl to the local machine.
rsync $RSYNC_OPTS $USER@fs1.calvin.edu:$REMOTE_FILES $LOCAL_DIR