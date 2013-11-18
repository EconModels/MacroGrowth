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

LOC_PATH=`pwd`
echo $LOC_PATH

# These models take hardly any time, so we'll run them all on their own node. 
# The next line runs all countires, all energy types, and all factors
# for all models except CES with energy.

ssh node-01 "cd $LOC_PATH; ./batchEcon.R -c all -e all -f all -m sf,cd,cde,ces,linex $1 > node-01.txt" &

# The various "cese" models take a long time. Spread them out across many nodes.
# dahl's 42 "compute nodes" each has 2 4-core processors. However, the R code that we're using
# can, apparently, access only one of those processors. So, at most, we can run only
# 4 analyses in parallel. Our code is parallelized on countries. We have 9 countries to 
# analyze for each model, so we'll put 3 countries on each compute node.
#
# This first batch of cese models uses only thermal energy (Q).

ssh node-02 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e Q -m cese-\(kl\)e $1 > node-02.txt" &
ssh node-03 "cd $LOC_PATH; ./batchEcon.R -c CN,ZA,SA -e Q -m cese-\(kl\)e $1 > node-03.txt" &
ssh node-04 "cd $LOC_PATH; ./batchEcon.R -c IR,TZ,ZM -e Q -m cese-\(kl\)e $1 > node-04.txt" &
ssh node-05 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e Q -m cese-\(le\)k $1 > node-05.txt" &
ssh node-06 "cd $LOC_PATH; ./batchEcon.R -c CN,ZA,SA -e Q -m cese-\(le\)k $1 > node-06.txt" &
ssh node-07 "cd $LOC_PATH; ./batchEcon.R -c IR,TZ,ZM -e Q -m cese-\(le\)k $1 > node-07.txt" &
# Gary Draving suggested avoiding node-08, because of I/O issues.
ssh node-09 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e Q -m cese-\(ek\)l $1 > node-09.txt" &
ssh node-10 "cd $LOC_PATH; ./batchEcon.R -c CN,ZA,SA -e Q -m cese-\(ek\)l $1 > node-10.txt" & 
# node-11 is not functional.
ssh node-12 "cd $LOC_PATH; ./batchEcon.R -c IR,TZ,ZM -e Q -m cese-\(ek\)l $1 > node-12.txt" &

# The next lines run the cese analyses for exergy (X)

ssh node-20 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e X -m cese-\(kl\)e $1 > node-20.txt" &
ssh node-21 "cd $LOC_PATH; ./batchEcon.R -c CN,ZA,SA -e X -m cese-\(kl\)e $1 > node-21.txt" &
ssh node-22 "cd $LOC_PATH; ./batchEcon.R -c IR,TZ,ZM -e X -m cese-\(kl\)e $1 > node-22.txt" &
ssh node-23 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e X -m cese-\(le\)k $1 > node-23.txt" &
ssh node-24 "cd $LOC_PATH; ./batchEcon.R -c CN,ZA,SA -e X -m cese-\(le\)k $1 > node-24.txt" &
ssh node-25 "cd $LOC_PATH; ./batchEcon.R -c IR,TZ,ZM -e X -m cese-\(le\)k $1 > node-25.txt" &
ssh node-26 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e X -m cese-\(ek\)l $1 > node-26.txt" &
ssh node-27 "cd $LOC_PATH; ./batchEcon.R -c CN,ZA,SA -e X -m cese-\(ek\)l $1 > node-27.txt" &
ssh node-28 "cd $LOC_PATH; ./batchEcon.R -c IR,TZ,ZM -e X -m cese-\(ek\)l $1 > node-28.txt" &

# We have useful work (U) data for US, UK, and JP only.

ssh node-30 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e U -m cese-\(kl\)e $1 > node-30.txt" &
ssh node-31 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e U -m cese-\(le\)k $1 > node-31.txt" &
ssh node-32 "cd $LOC_PATH; ./batchEcon.R -c US,UK,JP -e U -m cese-\(ek\)l $1 > node-32.txt" &
