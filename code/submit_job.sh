#!/bin/bash
# Parameters
T=("6000")
M_LIST=("3")


# Slurm parameters
PART=candes,hns,stat,pilanci      # Partition names
MEMO=10G                         # Memory required (50GB)
TIME=00-05:00:00                  # Time required (1d)
CORE=10                           # Cores required (10)

# Assemble order prefix
ORDP="sbatch --mem="$MEMO" -n 1 -c "$CORE" -p "$PART" --time="$TIME

# Create directory for log files
LOGS="logs"
mkdir -p $LOGS

# Script to be run
for M in "${M_LIST[@]}"; do 
  for SEED in {1..30}; do
  SCRIPT="job.sh $T $M $SEED"
  
  # Define job name for this chromosome
  JOBN="sim-"$T"-"$M"-"$SEED
  OUTF=$LOGS"/"$JOBN".out"
  ERRF=$LOGS"/"$JOBN".err"

  # Assemble slurm order for this job
  ORD=$ORDP" -J "$JOBN" -o "$OUTF" -e "$ERRF" "$SCRIPT

  #Print order
  echo $ORD

  # Submit order
  $ORD
  done
done
 
