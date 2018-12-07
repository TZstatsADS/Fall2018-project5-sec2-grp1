########################
### Cross Validation for adaboost
########################
library(JOUSBoost)

adaboostcv.function <- function(X, Y, d, K){
  
  n <- dim(Y)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X[s != i,]
    train.label <- Y[s != i,]
    test.data <- X[s == i,]
    test.label <- Y[s == i,]
    
    par <- list(depth=d)
    cat("k=", i, "\n")
    fit <- adaboost(train.data, train.label,tree_depth=par,n_rounds=100)
    cat(i, "Train finish","\n")
    pred <- predict(fit, test.data,type="response")  
    cv.error[i] <- mean(pred != test.label)
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
}