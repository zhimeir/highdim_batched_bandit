#!/usr/bin/env Rscript

## Process the input argument 
args <- commandArgs(trailingOnly = TRUE)
M<- as.integer(args[1])
seed<- as.integer(args[2])

## Default arguments for debug
if(is.na(M)) M <- 3
if(is.na(seed)) seed <- 1

## Parameters
gamma <- 0.005
s <- 20
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
X <- rep(list(c()), length = T)
for(t in 1:T){
  xt <- as.matrix(data[t,-(d+1)])
  dt <- matrix(0, nrow = 3, ncol = 3 * d)
  dt[1,1:d] <- xt
  dt[2,(d+1):(2 * d)] <- xt
  dt[3,(2*d + 1):(3 * d)] <- xt
  X[[t]] <- dt
}
max_reward <- as.matrix(data[,d + 1])
d <- 3 * d

## Initialization
reward <- c()

if(M < T){
  ## Initialize b
  b <- sqrt(T) * (T / s)^(1 / 2 / (2^M-1)) 

  ## Determine the grids (t_1,t_2,...,t_M)
  grids <- c() 
  t_tmp <- s
  for(m in 1:M){
    grids <- c(grids, ceiling(sqrt(t_tmp) * b))
    t_tmp <- grids[m] 
  }
  grids <- pmin(grids, T)

  ## Determin the batch id
  id <- c()
  batch_id <- rep(list(id), length = M-1)
  for(m in 1:(M-1)){
    ## The start and the end of the batch
    grid_end <- grids[m]
    if(m == 1){
      grid_start <- 0
    }else{
      grid_start <- grids[m-1]
    }
    incre <- floor((grid_end - grid_start) / (M - m))

    for (l in m:(M-1)){
      batch_id[[l]] <- c(batch_id[[l]], (grid_start + incre * (l-m) + 1):(grid_start + incre  * (l - m + 1)) )
    }
  }

  ## The bandit part
  ## Loop through the grids
  X_train <- c()
  Y_train <- c()
  for (m in 1:M){
  
    if(m == 1){
      for (t in 1:grids[1]){
        ## Randomly sample an arm
        a <- sample(1:K, 1)
        
        ## Record the regret
        ins_reward <- -(a-1 != max_reward[t])
        reward <- c(reward, ins_reward)

        ## Record the realized data
        X_train <- rbind(X_train, X[[t]][a,])
        Y_train <- c(Y_train, ins_reward) 
      }
    }else{
      id <- batch_id[[m-1]]
      ## Fit lasso w/ the realized data
      lambda <- gamma * sqrt(2 * log(K) *(log(d) + 2 * log(T))/length(id))
      lasso_mdl <- glmnet(X_train[id,], Y_train[id], standardize = FALSE, lambda = lambda)
      hat_theta <- lasso_mdl$beta
      
      for (t in (grids[m-1] + 1):grids[m]){
        ## Select an arm that maximizes X * hat_theta
        random_order <- sample(1:K, K, replace = FALSE)
        est_reward <- X[[t]] %*% hat_theta
        a <- which.max(est_reward[random_order])
        a <- random_order[a]

        ## Record the regret
        ins_reward <- -(a-1 != max_reward[t])
        reward <- c(reward, ins_reward)

        ## Record the realized data
        X_train <- rbind(X_train, X[[t]][a,])
        Y_train <- c(Y_train, ins_reward) 
      }
    }

  }
}else{
  
  ## Initialize the training data
  X_train <- c()
  Y_train <- c()
  ## Loop through the t steps
  for(t in 1:T){
    
    if(t <= 20){
      ## Randomly choose an arm
      a <- sample(1:K, 1)
      
      ## Record the regret
      ins_reward <- -(a-1 != max_reward[t])
      reward <- c(reward, ins_reward)

      ## Record the realized data
      X_train <- rbind(X_train, X[[t]][a,])
      Y_train <- c(Y_train, ins_reward) 

    }else{
      ## Fit lasso w/ the realized data
      lambda <- gamma * sqrt(2 * log(K) *(log(d) + 2 * log(T))/t)
      lasso_mdl <- glmnet(X_train, Y_train, standardize = FALSE, lambda = lambda)
      ##       lasso_mdl <- cv.glmnet(X_train, Y_train, standardize = FALSE)
      hat_theta <- lasso_mdl$beta
      
      ## Select an arm that maximizes X * hat_theta
      random_order <- sample(1:K, K, replace = FALSE)
      est_reward <- X[[t]] %*% hat_theta
      a <- which.max(est_reward[random_order])
      a <- random_order[a]

      ## Record the regret
      ins_reward <- -(a-1 != max_reward[t])
      reward <- c(reward, ins_reward)
      
      ## Record the realized data
      X_train <- rbind(X_train, X[[t]][a,])
      Y_train <- c(Y_train, ins_reward) 

    }    

  }

}


regret <- -reward
cum_regret <- cumsum(regret) / (1:T)
plot(cum_regret)

output <- data.frame(max_reward = max_reward, reward = reward, cum_regret = cum_regret)
out_file <- sprintf("../results/warfarin_M%d_seed%d_gamma%.2f.txt", M, seed, gamma)
write_delim(output, out_file)






