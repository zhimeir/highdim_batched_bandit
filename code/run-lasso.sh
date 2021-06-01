#!/bin/bash
for SEED in {1..10}; do
  Rscript lasso_wafarin.R $SEED
done
