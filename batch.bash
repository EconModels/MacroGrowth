#! /bin/bash
# This is a shell script that is specific to the dahl supercomputer at Calvin College.
# (See http://dahl.calvin.edu)
# Not only that, it is specific to Matt's directory structure on dahl.
# 
# Ths script moves (via ssh) to each node of the supercomputer 
# and runs combinations of countries and models until all resample data has been generated.
#
# The CES models with energy take the longest to run, so we put each of these
# runs onto their own node. The -p option runs the countries in parallel on each node.
ssh node-01 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK -e Q -m cese-\(kl\)e -n 1000 -C > node-01.txt" &
ssh node-02 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c JP,CN -e Q -m cese-\(kl\)e -n 1000 -C > node-02.txt" &
ssh node-03 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZA,SA -e Q -m cese-\(kl\)e -n 1000 -C > node-03.txt" &
ssh node-04 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ -e Q -m cese-\(kl\)e -n 1000 -C > node-04.txt" &
ssh node-05 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZM    -e Q -m cese-\(kl\)e -n 1000 -C > node-05.txt" &
ssh node-06 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK -e Q -m cese-\(le\)k -n 1000 -C > node-06.txt" &
ssh node-07 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c JP,CN -e Q -m cese-\(le\)k -n 1000 -C > node-07.txt" &
ssh node-09 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZA,SA -e Q -m cese-\(le\)k -n 1000 -C > node-09.txt" &
ssh node-10 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ -e Q -m cese-\(le\)k -n 1000 -C > node-10.txt" &
ssh node-12 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZM    -e Q -m cese-\(le\)k -n 1000 -C > node-12.txt" &
ssh node-13 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK -e Q -m cese-\(ek\)l -n 1000 -C > node-13.txt" &
ssh node-14 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c JP,CN -e Q -m cese-\(ek\)l -n 1000 -C > node-14.txt" & 
ssh node-15 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZA,SA -e Q -m cese-\(ek\)l -n 1000 -C > node-15.txt" &
ssh node-16 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ -e Q -m cese-\(ek\)l -n 1000 -C > node-16.txt" &
ssh node-17 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c ZM    -e Q -m cese-\(ek\)l -n 1000 -C > node-17.txt" &

# These models take hardly any time, so we'll run them all on their own node. 
# The next line runs all countires and all energy types for all models except CES with energy.
ssh node-18 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c all -e all -f all -m sf,cd,cde,ces,linex -n 1000 -C > node-18.txt" &

# The next lines run the analyses for exergy (X) and useful work (U) energy types 
# for the CES models with energy.
ssh node-19 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK,JP -e X -m cese-\(kl\)e -n 1000 -C > node-19.txt" &
ssh node-20 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c CN,ZA,SA -e X -m cese-\(kl\)e -n 1000 -C > node-20.txt" &
ssh node-21 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ,ZM -e X -m cese-\(kl\)e -n 1000 -C > node-21.txt" &
ssh node-22 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK,JP -e X -m cese-\(le\)k -n 1000 -C > node-22.txt" &
ssh node-23 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c CN,ZA,SA -e X -m cese-\(le\)k -n 1000 -C > node-23.txt" &
ssh node-24 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ,ZM -e X -m cese-\(le\)k -n 1000 -C > node-24.txt" &
ssh node-25 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK,JP -e X -m cese-\(ek\)l -n 1000 -C > node-25.txt" &
ssh node-26 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c CN,ZA,SA -e X -m cese-\(ek\)l -n 1000 -C > node-26.txt" &
ssh node-27 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c IR,TZ,ZM -e X -m cese-\(ek\)l -n 1000 -C > node-27.txt" &
# We have useful work data for US, UK, and JP only.
ssh node-28 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK,JP -e U -m cese-\(kl\)e -n 1000 -C > node-28.txt" &
ssh node-29 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK,JP -e U -m cese-\(le\)k -n 1000 -C > node-29.txt" &
ssh node-30 "cd github/Econ-Growth-R-Analysis; ./batchEcon.R -c US,UK,JP -e U -m cese-\(ek\)l -n 1000 -C > node-30.txt" &
