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

ssh node-01 "cd $LOC_PATH; $EXEC -c all -e all -f all -m sf,cd,cde,ces,linex -R $OUTDIR $1 &> $OUTDIR/node-01.txt" &

# The various "cese" models take a long time. Spread them out across many nodes.
# dahl's 42 "compute nodes" each has 2 4-core processors. However, the R code that we're using
# can, apparently, access only one of those processors. So, at most, we can run only
# 4 analyses in parallel. Our code is parallelized on countries. We have 9 countries to 
# analyze for each model, so we'll put 3 countries on each compute node.
#
# This first batch of cese models uses only thermal energy (Q).

ssh node-02 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iQ -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-02.txt" &
ssh node-03 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iQ -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-03.txt" &
ssh node-04 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iQ -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-04.txt" &
ssh node-05 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iQ -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-05.txt" &
# node-06 has only 4 working processors.  We can access only 2 of those.  So, best to avoid node-06 for now.
ssh node-07 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iQ -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-07.txt" &
# Gary Draving suggested avoiding node-08, because of I/O issues.
ssh node-09 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iQ -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-09.txt" &
ssh node-10 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iQ -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-10.txt" &
# node-11 is not functional.
ssh node-12 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iQ -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-12.txt" & 
ssh node-13 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iQ -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-13.txt" &

# The next lines run the cese analyses for exergy (X)

ssh node-14 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iX -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-14.txt" &
# node-15 doesn't work. Can't find the object "Calvin" for some reason.
ssh node-16 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iX -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-16.txt" &
# node-17 is missing
ssh node-18 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iX -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-18.txt" &
ssh node-19 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iX -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-19.txt" &
ssh node-20 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iX -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-20.txt" &
# Faulty R installations on nodes 21, 22, 23, and 24.
# node-25 is missing.
# Faulty R installation on node 26.
ssh node-27 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iX -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-27.txt" &
ssh node-28 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iX -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-28.txt" &
ssh node-29 "cd $LOC_PATH; $EXEC -c CN,ZA,SA -e iX -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-29.txt" &
ssh node-30 "cd $LOC_PATH; $EXEC -c IR,TZ,ZM -e iX -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-30.txt" &

# We have useful work (U) data for US, UK, and JP only.

# node-31 appears to be down.
ssh node-32 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iU -m cese-\(kl\)e -R $OUTDIR $1 &> $OUTDIR/node-32.txt" &
# node-33 is down.
# node-34 is down.
ssh node-35 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iU -m cese-\(le\)k -R $OUTDIR $1 &> $OUTDIR/node-35.txt" &
# node-36 has a bad R install
ssh node-37 "cd $LOC_PATH; $EXEC -c US,UK,JP -e iU -m cese-\(ek\)l -R $OUTDIR $1 &> $OUTDIR/node-37.txt" &

# Run the script to generate all orig fits and models on a node.  

ssh node-38 "cd $LOC_PATH; Scripts/OrigModels.R -S $SRC &> $OUTDIR/node-38.txt" &
