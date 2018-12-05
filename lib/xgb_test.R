xgb_test <- function(model, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model list using training data
  ###  - processed score features from correction candidates
  ### Output: training model specification
  
  ### load libraries
  library("xgboost")
  
  predArr <- array(NA, dim(dat_test)[1])
  
  for (i in 1:12){
    fit_train <- modelList[[i]]
    ### calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_test[, , c2]
    ### make predictions
    predArr[, c1, c2] <- predict(fit_train$fit, newdata=featMat)
  }
  return(as.numeric(predArr))
}