#! /bin/bash
# This is a shell script that is specific to the dahl supercomputer at Calvin College.
# (See http://dahl.calvin.edu)
# 
# Ths script executes a number of sub-scripts that are designed to run together at the same time
# on dahl. 
# The subscripts are IST, Leeds, SUN, and REXS1960.
#
# Invoke with this command
#
# Scripts/ISTLeedsSUNREXS1960_batch.bash "-n 1000 -C"
#
# or similar. The single argument is passed to all subscripts.

Scripts/IST_batch.bash "$1"
Scripts/Leeds_batch.bash "$1"
Scripts/SUN_batch.bash "$1"
Scripts/REXS1960_batch.bash "$1"