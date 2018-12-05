cv.function <- function(X.train, y.train, par, K, s){
  cv.recall <- rep(NA, K)
  for (k in 1:K){
    train.data <- X.train[s != k, ]
    train.label <- y.train[s != k]
    test.data <- X.train[s == k, ]
    test.label <- y.train[s == k]
    
    fit <- ksvm(train.data, train.label, kernel = "rbfdot", type = "C-svc", kpar = list(sigma = par$sigma))
    pred <- predict(fit, test.data)
    pred <- ifelse(pred==2,TRUE,FALSE)
    cv.recall[k] <- sum(!pred & !test.label)/sum(!test.label)
  }
  return(mean(cv.recall))
}

perform_cv <- function(input, labels, sigmas, K){
  cv_result <- rep(NA, length(sigmas))
  n_tokens <- length(labels)
  n.fold <- floor(n_tokens/K)
  folds <- sample(rep(1:K, c(rep(n.fold, K-1), n_tokens-(K-1)*n.fold)))
  
  for(i in 1:length(sigmas)){
    cat("sigma = ", sigmas[i], "\n")
    pars = list("sigma" = sigmas[i])
    tm_cv <- system.time(cv_result[i] <- cv.function(input, labels, pars, K, folds))
    cat("time for current cv session = ", tm_cv[3], "\n")
  }
  best_sigma <- sigmas[which.max(cv_result)]
  return(list("best_sigma" = best_sigma, "cv_result" = cv_result))
}
