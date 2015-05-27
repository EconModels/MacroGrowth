#! /bin/bash
# This is a shell script that is specific to the dahl supercomputer at Calvin College.
# (See http://dahl.calvin.edu)
# 
# Ths script uses torque to schedule jobs on nodes of the supercomputer
# and runs combinations of countries and models until all resample data has been generated.
#
# The CES models with energy take the longest to run, so we put each of these
# runs onto their own job. The -p option runs the countries in parallel on each node.

# One bash commandline argument is used for things like "-n <number> -C -d"  These should be provided 
# as a single quoted string.

# The command to run everything for 1000 resamples while clobbering all previous results without 
# massive amounts of debug information printed to the screen is

# Scripts/Calvin_batch.bash "-n 1000 -C"

###############################
# Basic job submission: 1 Nodes, and use 1 Cores.  Maximum runtime: 1hr
#PBS -l nodes=1:ppn=4,walltime=1:00:00

# Set the notification of your processes:
#    -m  accepts up to all three control flags 'a','b','e', where:
#        a = mail is sent when the job is aborted
#        b = mail is sent when the job begins execution
#        e = mail is sent when the job finishes execution
#PBS -m a -M rpruim@gmail.com

# Set the name of job
#PBS -N Calvin-Torque-Test


LOC_PATH=$PWD # Assuming that we're running from the top directory of the repository for this script
echo $LOC_PATH

EXEC="Scripts/batchEcon.R"

SRC="Calvin"

OUTDIR="$LOC_PATH/data_resample/$SRC"

# These models take hardly any time, so we'll run them all on their own node. 
# The next line runs all countires, all energy types, and all factors
# for all models except CES with energy.

cd $LOC_PATH

qsub -N Job-fast  $EXEC -F "-c all -e all -f all -m fast -S $SRC $1 

# The various "cese" models take a long time. Spread them out across many nodes.
# dahl's 42 "compute nodes" each has 2 4-core processors. However, the R code that we're using
# can, apparently, access only one of those processors. So, at most, we can run only
# 4 analyses in parallel. Our code is parallelized on countries. We have 9 countries to 
# analyze for each model, so we'll put 3 countries on each compute node.
#
# This first batch of cese models uses only primary thermal energy (iQp).

qsub -N Job1  $EXEC -F "-c US,UK,JP -e iQp -m cese-\(kl\)e -S $SRC $1 "
qsub -N Job2  $EXEC -F "-c CN,ZA,SA -e iQp -m cese-\(kl\)e -S $SRC $1 "
qsub -N Job3  $EXEC -F "-c IR,TZ,ZM -e iQp -m cese-\(kl\)e -S $SRC $1 "

qsub -N Job4  $EXEC -F "-c US,UK,JP -e iQp -m cese-\(le\)k -S $SRC $1 "
qsub -N Job5  $EXEC -F "-c CN,ZA,SA -e iQp -m cese-\(le\)k -S $SRC $1 "
qsub -N Job6 $EXEC -F "-c IR,TZ,ZM -e iQp -m cese-\(le\)k -S $SRC $1 "

qsub -N Job7  $EXEC -F "-c US,UK,JP -e iQp -m cese-\(ek\)l -S $SRC $1 "
qsub -N Job8  $EXEC -F "-c CN,ZA,SA -e iQp -m cese-\(ek\)l -S $SRC $1 "
qsub -N Job9 $EXEC -F "-c IR,TZ,ZM -e iQp -m cese-\(ek\)l -S $SRC $1 "

# The next lines run the cese analyses for primary exergy (iXp)

qsub -N Job10  $EXEC -F "-c US,UK,JP -e iXp -m cese-\(kl\)e -S $SRC $1 "
qsub -N Job11  $EXEC -F "-c CN,ZA,SA -e iXp -m cese-\(kl\)e -S $SRC $1 "
qsub -N Job12  $EXEC -F "-c IR,TZ,ZM -e iXp -m cese-\(kl\)e -S $SRC $1 "

qsub -N Job13  $EXEC -F "-c US,UK,JP -e iXp -m cese-\(le\)k -S $SRC $1 "
qsub -N Job14  $EXEC -F "-c CN,ZA,SA -e iXp -m cese-\(le\)k -S $SRC $1 "
qsub -N Job15  $EXEC -F "-c IR,TZ,ZM -e iXp -m cese-\(le\)k -S $SRC $1 "

qsub -N Job16  $EXEC -F "-c US,UK,JP -e iXp -m cese-\(ek\)l -S $SRC $1 "
qsub -N Job17  $EXEC -F "-c CN,ZA,SA -e iXp -m cese-\(ek\)l -S $SRC $1 "
qsub -N Job18  $EXEC -F "-c IR,TZ,ZM -e iXp -m cese-\(ek\)l -S $SRC $1 "

# We have useful work (iU) data for US, UK, and JP only.

qsub -N Job19  $EXEC -F "-c US,UK,JP -e iU -m cese-\(kl\)e -S $SRC $1 "
qsub -N Job20  $EXEC -F "-c US,UK,JP -e iU -m cese-\(le\)k -S $SRC $1 "
qsub -N Job21  $EXEC -F "-c US,UK,JP -e iU -m cese-\(ek\)l -S $SRC $1 "

# Run the script to generate all orig fits and models on a node.  

qsub -N Job22  Scripts/OrigModels.R -F "-S $SRC $1 "
