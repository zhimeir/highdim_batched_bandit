#!/bin/bash

###############
# Parse input #
###############
T=$1
M=$2
SEED=$3

##############
# Run script #
##############
ml R/3.5

Rscript --vanilla simulation.R $T $M $SEED
Rscript --vanilla lasso.R $T $M $SEED
