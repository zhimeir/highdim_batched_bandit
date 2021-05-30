#!/usr/bin/env Rscript

## Process the input argument 
args <- commandArgs(trailingOnly = TRUE)
T <- as.integer(args[1])
M <- as.integer(args[2])
seed<- as.integer(args[3])

## Default arguments for debug
if(is.na(T)) T <- 1000
if(is.na(M)) M <- 3
if(is.na(seed)) seed <- 1

## Load libraries
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(glmnet))

## Parameters for the problems
d <- 6000
s <- 50
K <- 2
amp <- 1 / sqrt(s)
sigma <- 0.5

## Hyper-parameters for the algorithm
q <- 1              
h <- 5
lambda1 <- 0.05
lambda2_0 <- 0.05

## The data-generating model
set.seed(24601)
nonzero <- sample(d, s, replace = FALSE)
theta <- 1:d %in% nonzero * amp
y.sample <- function(X) X %*%  theta + sigma * rnorm(1)

## Generate the covariates
X.sample <- function(i){
  x <- matrix(rnorm(K * d), K, d)
  return(x)
}

## Generate the model
set.seed(seed)
X <- lapply(1:T, X.sample)
max_reward <- lapply(X, function(xx) max(xx %*% theta)) %>% unlist()

## Determine the force-samping set
force_id <- rep(0,T)
for(k in 1:K){
  for(n in 1:ceiling(log2(T))){
    for(j in (q * (k-1) + 1): (q * k)){
      
      new_id <- (2^n - 1) * K * q + j
      if(new_id > T) break
      
      ## assign the id for force-sampling
      force_id[new_id] <- k

    }
  }
}

## Transform the problem
theta_tmp <- rep(0, K * d)
theta_list <- rep(list(theta_tmp), length = K)
for (k in 1:K){
  theta_list[[k]][((k-1) * d + 1):(k * d)] <- theta
}

## The lasso bandit
## Initialization
reward <- c()
X_all <- c()
Y_all <- c()
a_record <- c()

for(t in 1:T){
  if(force_id[t] != 0){
    a <- force_id[t]
    
    ## Record the regret
    ins_reward <- X[[t]][a,] %*% theta
    reward <- c(reward, ins_reward)


    ## Record the all-sample set
    a_record <- c(a_record, a)
    X_all <- rbind(X_all, as.vector(t(X[[t]])))
    Y_all <- c(Y_all, y.sample(X[[t]][a,]))
    
    ## Update the penalty factor
    lambda2 <- lambda2_0 * sqrt((log(t) + log(d)) / t)
  }else{
    ## Use the samples in the forced-sampling set to obtain an estimate 
    hat_theta_list <- rep(c(), length(K))
    est_reward_list <- c()
    for(k in 1:K){
      est_id <- which(force_id == k)
      est_id <- est_id[est_id <= t-1]
      if(length(est_id) > 1){
        lasso_mdl <- glmnet(X_all[est_id,], Y_all[est_id], standardize = FALSE, lambda = lambda1 / 2)
        est_reward_list[k] <- as.numeric(as.vector(t(X[[t]])) %*% lasso_mdl$beta)
      }else{
        est_reward_list[k] <- 0
      }
    }

    ## Determine the pre-selected set
    pre_set <- which(est_reward_list >= max(est_reward_list) - h / 2)

    ##  fit again with the pre-selected data
    est_reward_list <- rep(0, K)
    for(k in pre_set){
      est_id <- which(a_record == k)
      if(length(est_id) > 1){
        lasso_mdl <- glmnet(X_all[est_id,], Y_all[est_id], standardize = FALSE, lambda = lambda2 / 2)
        est_reward_list[k] <- as.numeric(as.vector(t(X[[t]])) %*% lasso_mdl$beta)
      }else{
        est_reward_list[k] <- 0
      }
    }
    a <- pre_set[which.max(est_reward_list[pre_set])]
    
    ## Record the regret
    ins_reward <- X[[t]][a,] %*% theta
    reward <- c(reward, ins_reward)


    ## Record the all-sample set
    a_record <- c(a_record, a)
    X_all <- rbind(X_all, as.vector(t(X[[t]])))
    Y_all <- c(Y_all, y.sample(X[[t]][a,]))
    
    ## Update the penalty factor
    lambda2 <- lambda2_0 * sqrt((log(t) + log(d))/t)



  }

}


regret <- max_reward - reward 
cum_regret <- cumsum(regret)


## Store the results
output <- data.frame(max_reward = max_reward, reward = reward, cum_regret = cum_regret)
out_file <- sprintf("../results/K%d_sigma%.2f_T%d_M%d_seed%d_lasso.txt", K, sigma, T, M, seed)
write_delim(output, out_file)
