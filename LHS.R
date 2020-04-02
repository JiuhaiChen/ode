integerLHS <- function(n, intGroups) 
{ 
  stopifnot(all(lapply(intGroups, function(X) length(X) %% n) == 0)) 
  stopifnot(require(lhs)) 
  stopifnot(is.list(intGroups)) 
  ranges <- lapply(intGroups, function(X) max(X) - min(X)) 
  A <- matrix(nrow = n, ncol = length(intGroups)) 
  for (j in 1:length(ranges)) 
  { 
    sequ <- order(runif(n)) 
    if (length(intGroups[[1]]) > 1) 
    { 
      spacing <- intGroups[[j]][2] - intGroups[[j]][1] 
    } else stop("must have more than 1 intGroup") 
    for (k in 1:n) 
    { 
      i <- sequ[k] 
      a <- min(intGroups[[j]]) + (i - 1)*(ranges[[j]] + spacing)/n 
      b <- min(intGroups[[j]]) + i*(ranges[[j]] + spacing)/n - 1 
      if (a < b) 
      { 
        A[k,j] <- sample(seq(a,b,spacing), 1) 
      } else if (a == b) 
      { 
        A[k,j] <- a 
      } else stop("error") 
    } 
  } 
  return(A) 
} 

library(lhs)
chosen_index<-integerLHS(16, list(1:32, 1:32)) 
