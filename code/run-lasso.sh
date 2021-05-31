#!/bin/bash
for SEED in {1..10}; do
  Rscript ols_wafarin.R $SEED
done
