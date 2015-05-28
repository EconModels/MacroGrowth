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


LOC_PATH=$PWD # Assuming that we're running from the top directory of the repository for this script
echo $LOC_PATH

EXEC="Scripts/batchEcon.R"

SRC="Calvin"

OUTDIR="$LOC_PATH/data_resample/$SRC"

# cd $LOC_PATH

echo "$EXEC -c all -e all -f all -m fast -S $SRC $1 "

# generate all orig fits and models 
qsub -N Job-Orig2  Scripts/OrigModels2.R -F "-S $SRC $1 "


# slow models

for country in US UK JP CN ZA SA IR TZ ZM
do
  for model in cese-\\\(kl\\\)e cese-\\\(ek\\\)l cese-\\\(le\\\)k
  do
    for energy in iQp iXp
    do
      qsub -N JobS-$country-$energy  $EXEC -F "-c $country -e $energy -m $model -S $SRC $1 "
    done
  done
done

# More slow models
# We have useful work (iU) data for US, UK, and JP only.

for country in US UK JP 
do
  for model in cese-\\\(kl\\\)e cese-\\\(ek\\\)l cese-\\\(le\\\)k
  do
    for energy in iU
    do
      qsub -N JobU-$country-$energy  $EXEC -F "-c $country -e $energy -m $model -S $SRC $1 "
    done
  done
done

# Fast models

for country in US UK JP CN ZA SA IR TZ ZM
do
  for energy in iQp iXp iU
  do
#    qsub -N Job-$country-$energy  $EXEC -F "-c $country -e $energy -m $model -S $SRC $1 "
    qsub -N JobF-$country-$energy  $EXEC -F "-c $country -e $energy -f all -m fast -S $SRC $1 "
  done
done