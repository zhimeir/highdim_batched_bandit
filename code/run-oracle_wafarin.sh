#!/bin/bash
for SEED in {1..10}; do
  Rscript doctor_wafarin.R $SEED
done
