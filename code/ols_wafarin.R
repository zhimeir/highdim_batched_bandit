#!/usr/bin/env Rscript

## Process the input argument 
args <- commandArgs(trailingOnly = TRUE)
seed<- as.integer(args[1])

## Default arguments for debug
if(is.na(seed)) seed <- 1

K <- 3
## Hyper-parameters for the algorithm
q <- 1              
h <- 5

## Load libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(MASS))

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

## Determine the force-samping set
force_id <- rep(0,T)
for(k in 1:K){
  for(n in 0:ceiling(log2(T/k/q))){
    for(j in (q * (k-1) + 1): (q * k)){
      
      new_id <- (2^n - 1) * K * q + j
      if(new_id > T) break
      
      ## assign the id for force-sampling
      force_id[new_id] <- k

    }
  }
}

## The lasso bandit
## Initialization
reward <- c()
X_all <- c()
Y_all <- c()
a_record <- c()
theta_forced <- rep(list(rep(0, d)), K)
theta_all <- rep(list(rep(0, d)), K)



for(t in 1:T){
  if(force_id[t] != 0){
    a <- force_id[t]
    
    ## Record the regret
    ins_reward <- -(a-1 != max_reward[t])
    reward <- c(reward, ins_reward)

    ## Record the all-sample set
    a_record <- c(a_record, a)
    X_all <- rbind(X_all, X[t,])
    Y_all <- c(Y_all, ins_reward)
    
  }else{
    ## Use the samples in the forced-sampling set to obtain an estimate 
    hat_theta_list <- rep(c(), length(K))
    est_reward_list <- c()
    for(k in 1:K){
      est_id <- which(force_id == k)
      est_id <- est_id[est_id <= t-1]
      xs <- X_all[est_id,]
      ys <- Y_all[est_id]
      if(rankMatrix(xs) >=  d){
          mdl <- lm(ys~xs-1)
          theta_forced[[k]] <- coef(mdl)
      }else{
        ##         theta_forced[[k]] <- solve(1 * diag(rep(1,d)) + t(xs) %*% xs) %*% t(xs) %*% ys
        theta_forced[[k]] <- ginv(xs) %*% ys
      }
    
      est_reward_list[k] <- as.numeric(X[t,] %*% theta_forced[[k]])
    }

    ## Determine the pre-selected set
    pre_set <- which(est_reward_list >= max(est_reward_list) - h / 2)

    ##  fit again with the all-sample data
    est_reward_list <- rep(0, K)
    for(k in pre_set){
      est_id <- which(a_record == k)
      xs <- X_all[est_id,]
      ys <- Y_all[est_id]
      if(rankMatrix(xs)[1] >= d){
          mdl <- lm(ys~xs-1)
          theta_all[[k]] <- coef(mdl)
      }else{
        ##         theta_all[[k]] <- solve(1 * diag(rep(1,d)) + t(xs) %*% xs) %*% t(xs) %*% ys
        theta_all[[k]] <- ginv(xs) %*% ys
      }
          est_reward_list[k] <- as.numeric(X[t,] %*% theta_all[[k]])
    }
    a <- pre_set[which.max(est_reward_list[pre_set])]
    
    ## Record the regret
    ins_reward <- -(a-1 != max_reward[t])
    reward <- c(reward, ins_reward)


    ## Record the all-sample set
    a_record <- c(a_record, a)
    X_all <- rbind(X_all, X[t,])
    Y_all <- c(Y_all, ins_reward)
    
  }

}


regret <- -reward
cum_regret <- cumsum(regret) / (1:T)
plot(cum_regret)

output <- data.frame(max_reward = max_reward, reward = reward, cum_regret = cum_regret)
out_file <- sprintf("../results/warfarin_ols_seed%d.txt", seed)
write_delim(output, out_file)






