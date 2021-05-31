#!/bin/bash
for SEED in {1..10}; do
  Rscript wafarin.R 3 $SEED
done
