det <- function(R){
  eigenvalues <- eigen(R, only.values = TRUE)$values
  det <- prod(abs(eigenvalues))
  return(det)
}

logdet <- function(R){
  return(log(det(R)))
}

logmarglik <- function(data, A){
  n = nrow(data)
  a = length(A)
  
  D_1 = data[, 1]
  D_A = data[, A]
  I_A <- diag(a)
  M_A = I_A + t(D_A) %*% D_A
  
  first_term <- lgamma((n + a + 2) / 2) - lgamma((a + 2) / 2)
  second_term <- (-1 / 2) * logdet(M_A)
  third_term <- -(n + a + 2) / 2 * log((1 + t(D_1) %*% D_1 - t(D_1) %*% D_A %*% solve(M_A) %*% t(D_A) %*% D_1))
  
  ans <- first_term + second_term + third_term
  return (ans)
}

data <- as.matrix(read.table("erdata.txt", header = FALSE))
cat("The dimension of the data:", dim(data), "\n")
cat("Result:", logmarglik(data, c(2,5,10)), "\n")
