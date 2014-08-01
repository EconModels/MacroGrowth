#! /bin/bash
# This is a shell script that is specific to the dahl supercomputer at Calvin College.
# (See http://dahl.calvin.edu)
# 
# Ths script moves (via ssh) to each node of the supercomputer 
# and runs combinations of countries and models until all resample data has been generated.
#
# The CES models with energy take the longest to run, so we put each of these
# runs onto their own node. The -p option runs the countries in parallel on each node.

# One bash commandline argument is used for things like "-n <number> -C -d"  These should be provided 
# as a single quoted string.

# The command to run everything for 1000 resamples while clobbering all previous results without 
# massive amounts of debug information printed to the screen is

# Scripts/REXS_batch.bash "-n 1000 -C"

LOC_PATH=$PWD # Assuming that we're running from the top directory of the repository for this script
echo $LOC_PATH

EXEC="Scripts/batchEcon.R"

SRC="REXS1960"

OUTDIR="$LOC_PATH/data_resample/$SRC"

# These models take hardly any time, so we'll run them all on their own node. 
# The next line runs all countires, all energy types, and all factors
# for all models except CES with energy.

ssh node-01 "cd $LOC_PATH; $EXEC -c all -e all -f all -m sf,cd,cde,ces,linex -R $OUTDIR $1 &> $OUTDIR/node-01.txt" &

# The various "cese" models take a long time. Spread them out across many nodes.
# dahl's 42 "compute nodes" each has 2 4-core processors. However, the R code that we're using
# can, apparently, access only one of those processors. So, at most, we can run only
# 4 analyses in parallel. Our code is parallelized on countries. We have 9 countries to 
# analyze for each model, so we'll put 3 countries on each compute node.
#
# This first batch of cese models uses only primary exergy (iXp).

ssh node-02 "cd $LOC_PATH; $EXEC -c UK -e iXp -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-02.txt" &
ssh node-03 "cd $LOC_PATH; $EXEC -c UK -e iXp -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-03.txt" &
ssh node-04 "cd $LOC_PATH; $EXEC -c UK -e iXp -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-04.txt" &

# This next batch of cese models uses only useful work (iU).

ssh node-05 "cd $LOC_PATH; $EXEC -c UK -e iU -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-05.txt" &
# node-06 has only 4 working processors.  We can access only 2 of those.  So, best to avoid node-06 for now.
ssh node-07 "cd $LOC_PATH; $EXEC -c UK -e iU -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-07.txt" &
# Gary Draving suggested avoiding node-08, because of I/O issues.
ssh node-09 "cd $LOC_PATH; $EXEC -c UK -e iU -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-09.txt" &

ssh node-10 "cd $LOC_PATH; Scripts/OrigModels.R -S $SRC &> $OUTDIR/node-10.txt" &