##Problem 1

library(rcdd)

isValidLogistic <- function(response,explanatory,data)
{
  if(0==length(explanatory))
  {
    return(TRUE);
  }
  logisticreg = suppressWarnings(glm(data[,response] ~ as.matrix(data[,as.numeric(explanatory)]),family=binomial(link=logit),x=TRUE));
  tanv = logisticreg$x;
  tanv[data[,response] == 1, ] <- (-tanv[data[,response] == 1, ]);
  vrep = cbind(0, 0, tanv);
  lout = linearity(vrep, rep = "V");
  return(length(lout)==nrow(data));
}

getLogisticAIC <- function(response, explanatory, data) {
  if (0 == length(explanatory)) {
    deviance = glm(data[, response] ~ 1,
                   family = binomial(link = "logit"))$deviance
  } 
  else {
    deviance = glm(data[, response] ~ as.matrix(data[, as.numeric(explanatory)]),
                   family = binomial(link = "logit"))$deviance
  }
  return(deviance + 2 * (1 + length(explanatory)))
}

forwardSearchAIC <- function(V, response, data, last_B) {
  cand <- setdiff(V, last_B)
  curAIC <- getLogisticAIC(response, last_B, data)
  
  if (length(cand) == 0) return(last_B)
  
  bestAIC      <- curAIC
  bestAdd      <- NULL
  for (i in cand) {
    newA     <- c(last_B, i)
    newAIC   <- getLogisticAIC(response, newA, data)
    if (newAIC < bestAIC) {
      bestAIC  <- newAIC
      bestAdd  <- i
    }
  }
  if (!is.null(bestAdd)) {
    return(c(last_B, bestAdd))
  } 
  else {
    return(last_B)
  }
}

get_valid_nbd_set <- function(V, explanatory, data, response) {
  add_one <- lapply(setdiff(V, explanatory), function(i) sort(c(explanatory, i)))
  delete_one <- lapply(explanatory, function(i) setdiff(explanatory, i))
  nbd <- c(add_one, delete_one)
  mask <- sapply(nbd, function(A) {
    isValidLogistic(response, A, data)
  })
  return(nbd[mask])
}

current_model_update <- function(A_prime, A_cur, B_cur, data, response) {
  V <- setdiff(seq_len(ncol(data)), response)
  B_greedy <- forwardSearchAIC(V, response, data, B_cur)
  
  AIC_A_prime <- getLogisticAIC(response, A_prime, data)
  AIC_B <- getLogisticAIC(response, B_greedy, data)
  
  if (AIC_A_prime < AIC_B) {
    B_next <- A_prime
  } 
  else {
    B_next <- B_greedy
  }
  return(list(A = A_prime, B = B_next))
}

MC3_iter <- function(V, A_cur, B_cur, data, response) {
  nbd_valid <- get_valid_nbd_set(V, A_cur, data, response)
  index <- sample.int(length(nbd_valid), 1)
  
  A_prime <- nbd_valid[[index]]
  nbd_prime <- get_valid_nbd_set(V, A_prime, data, response)
  p_A_prime <- -getLogisticAIC(response, A_prime, data) - log(length(nbd_prime))
  p_A <- -getLogisticAIC(response, A_cur, data) - log(length(nbd_valid))
  
  if (p_A_prime > p_A || log(runif(1)) < (p_A_prime - p_A)) {
    
    res <- current_model_update(A_prime, A_cur, B_cur, data, response)
  } 
  else {
    res <- list(A = A_cur, B = B_cur)
  }
  return(res)
}

MC3_search <- function(data, response, n_iter) {
  V <- setdiff(seq_len(ncol(data)), response)
  repeat {
    k    <- sample(length(V), 1)
    A0   <- sample(V, k)
    if (isValidLogistic(response, A0, data)) break
  }
  A_cur <- A0
  B_cur <- A0
  for (r in n_iter) {
    res    <- MC3_iter(V, A_cur, B_cur, data, response)
    A_cur  <- res$A
    B_cur  <- res$B
  }
  return(list(bestAICvars = sort(B_cur),
              bestAIC = getLogisticAIC(response, B_cur, data)))
}

##Problem 2
path = "534binarydata.txt"
data = as.matrix(read.table(path, header = FALSE))
set.seed(2427348)
response <- 61
n_iter <- 25
n_run <- 10
for (i in seq_len(n_run)) {
  cat("Chain", i, "\n")
  print(MC3_search(data, response, n_iter))
}
