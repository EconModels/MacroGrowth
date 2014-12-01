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

# Scripts/Calvin_batch.bash "-n 1000 -C"

LOC_PATH=$PWD # Assuming that we're running from the top directory of the repository for this script
echo $LOC_PATH

EXEC="Scripts/batchEcon.R"

SRC="Calvin"

OUTDIR="$LOC_PATH/data_resample/$SRC"

# These models take hardly any time, so we'll run them all on their own node. 
# The next line runs all countires, all energy types, and all factors
# for all models except CES with energy.

ssh node-14 "cd $LOC_PATH; $EXEC -c all -e all -f all -m fast -S $SRC $1 &> $OUTDIR/node-14.txt" &

# The various "cese" models take a long time. Spread them out across many nodes.
# dahl's 42 "compute nodes" each has 2 4-core processors. However, the R code that we're using
# can, apparently, access only one of those processors. So, at most, we can run only
# 4 analyses in parallel. Our code is parallelized on countries. We have 9 countries to 
# analyze for each model, so we'll put 3 countries on each compute node.
#
# This first batch of cese models uses only thermal energy (Q).

ssh node-15 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iQp -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-15.txt" &
ssh node-16 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iQp -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-16.txt" &
ssh node-17 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iQp -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-17.txt" &

ssh node-18 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iQp -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-18.txt" &
ssh node-19 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iQp -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-19.txt" &
ssh node-20 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iQp -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-20.txt" &

ssh node-21 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iQp -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-21.txt" &
ssh node-22 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iQp -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-22.txt" & 
ssh node-23 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iQp -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-23.txt" &

# The next lines run the cese analyses for primary exergy (X)

ssh node-25 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iXp -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-25.txt" &
ssh node-26 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iXp -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-26.txt" &
ssh node-28 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iXp -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-28.txt" &

ssh node-29 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iXp -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-29.txt" &
ssh node-30 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iXp -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-30.txt" &
ssh node-32 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iXp -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-32.txt" &

ssh node-33 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iXp -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-33.txt" &
ssh node-34 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iXp -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-34.txt" &
ssh node-35 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iXp -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-35.txt" &

# We have useful work (U) data for US, UK, and JP only.

ssh node-37 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iU -m cese-\(kl\)e -S $SRC $1 &> $OUTDIR/node-37.txt" &
ssh node-38 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iU -m cese-\(le\)k -S $SRC $1 &> $OUTDIR/node-38.txt" &
ssh node-39 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iU -m cese-\(ek\)l -S $SRC $1 &> $OUTDIR/node-39.txt" &

# Run the script to generate all orig fits and models on a node.  

ssh node-40 "cd $LOC_PATH; Scripts/OrigModels.R -S $SRC &> $OUTDIR/node-40.txt" &
