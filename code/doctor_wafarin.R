#!/usr/bin/env Rscript

## Process the input argument 
args <- commandArgs(trailingOnly = TRUE)
seed<- as.integer(args[1])

## Default arguments for debug
if(is.na(seed)) seed <- 1

K <- 3

## Load libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glmnet))

## Read the dataset
data <- read_csv("../datasets/warfarin.txt", col_names = FALSE)
T <- nrow(data)
d <- ncol(data) - 1

## Randomly permute the data
set.seed(seed)
new_order <- sample(1:T, T, replace = FALSE)
data <- data[new_order,]
names(data) <- c(paste0("X",1:d), "Y")


## Extract X and the optimal reward
X <- as.matrix(data[, 1:d])
max_reward <- data$Y


## Initialization
reward <- c()


  ## Initialize the training data
  X_train <- c()
  Y_train <- c()
  ## Loop through the t steps
  for(t in 1:T){
    
      ## Randomly choose an arm
      a <- 2

      ## Record the regret
      ins_reward <- -(a - 1 != max_reward[t])
      reward <- c(reward, ins_reward)

  }



regret <- -reward
cum_regret <- cumsum(regret) / (1:T)
plot(cum_regret)

output <- data.frame(max_reward = max_reward, reward = reward, cum_regret = cum_regret)
out_file <- sprintf("../results/warfarin_doctor_seed%d.txt", seed)
write_delim(output, out_file)






